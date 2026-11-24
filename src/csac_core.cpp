// COSMO-SAC-dsp (Hsieh, Lin & Vrabec 2014; 2010 NHB/OH/OT hydrogen-bond split)
// Native Rcpp port of the NIST cCOSMO "COSMO3" engine (include/COSMO_SAC/COSMO.hpp).
// Residual segment-activity model + Staverman-Guggenheim combinatorial + dispersion.
// Profiles: 153 area-per-bin values (NHB|OH|OT, each 51 bins) on the sigma grid
// sigma_m = -0.025 + 0.001*m, m = 0..50  [e/A^2]; areas in A^2.
//
// Reference constants (NIST COSMO3Constants):
//   AEFFPRIME = 7.25 A^2; c_OH_OH = 4013.78; c_OT_OT = 932.31; c_OH_OT = 3016.43
//   A_ES = 6525.69; B_ES = 1.4859e8;  c_ES = A_ES + B_ES/T^2
//   R = k_B*N_A/4184 kcal/(mol K);  q0 = 79.53 A^2; r0 = 66.69 A^3; z = 10
// Dispersion classes (int): 0=WATER 1=COOH 2=NHB 3=ONLY_ACCEPTOR 4=DONOR_ACCEPTOR

#include <Rcpp.h>
#include <vector>
#include <cmath>
#include <algorithm>
using namespace Rcpp;

static const int NB = 51;       // bins per profile
static const int NP = 3;        // profiles (NHB, OH, OT)
static const int NT = NB * NP;  // 153
static const double DSIG = 0.001;
static const double SIG0 = -0.025;

static const double AEFFPRIME = 7.25;
static const double C_OH_OH = 4013.78, C_OT_OT = 932.31, C_OH_OT = 3016.43;
static const double A_ES = 6525.69, B_ES = 1.4859e8;
static const double Q0 = 79.53, R0 = 66.69, ZCOORD = 10.0;
// R = k_B * N_A / 4184  (2018 redefinition, matches NIST)
static const double RGAS = 1.380649e-23 * 6.02214076e23 / 4184.0;

// HB constant for a profile-type pair (t in {0=NHB,1=OH,2=OT})
static inline double c_hb_pair(int a, int b) {
  if (a == b) {
    if (a == 1) return C_OH_OH;
    if (a == 2) return C_OT_OT;
    return 0.0;            // NHB-NHB
  }
  if ((a == 1 && b == 2) || (a == 2 && b == 1)) return C_OH_OT;
  return 0.0;             // any pair involving NHB
}

// Build exp(-DELTAW/(R T)) for the full 153x153 interaction matrix (depends only on T)
static void build_EW(double T, std::vector<double>& EW) {
  EW.assign((size_t)NT * NT, 0.0);
  const double c_ES = A_ES + B_ES / (T * T);
  const double RT = RGAS * T;
  for (int ti = 0; ti < NP; ++ti) {
    for (int sj = 0; sj < NP; ++sj) {
      const double cc = c_hb_pair(ti, sj);
      for (int m = 0; m < NB; ++m) {
        const double sm = SIG0 + DSIG * m;
        const int row = ti * NB + m;
        for (int n = 0; n < NB; ++n) {
          const double sn = SIG0 + DSIG * n;
          const double chb = (sm * sn >= 0.0) ? 0.0 : cc;
          const double dw = c_ES * (sm + sn) * (sm + sn) - chb * (sm - sn) * (sm - sn);
          EW[(size_t)row * NT + (sj * NB + n)] = std::exp(-dw / RT);
        }
      }
    }
  }
}

// Segment activity coefficients Gamma[153] for an area-fraction profile psigma[153].
// Iterate Gnew[m] = 1 / sum_n EW[m][n]*psigma[n]*Gamma[n]; damp 0.5; until converged.
static void solve_Gamma(const std::vector<double>& EW, const double* psigma,
                        std::vector<double>& Gamma) {
  Gamma.assign(NT, 1.0);
  std::vector<double> Gnew(NT, 1.0);
  for (int iter = 0; iter < 500; ++iter) {
    double maxdiff = 0.0;
    for (int m = 0; m < NT; ++m) {
      const double* erow = &EW[(size_t)m * NT];
      double s = 0.0;
      for (int n = 0; n < NT; ++n) s += erow[n] * psigma[n] * Gamma[n];
      Gnew[m] = (s > 0.0) ? 1.0 / s : 1.0;
    }
    for (int m = 0; m < NT; ++m) {
      double g = 0.5 * (Gamma[m] + Gnew[m]);
      double d = (g != 0.0) ? std::fabs((g - Gnew[m]) / g) : 0.0;
      if (d > maxdiff) maxdiff = d;
      Gamma[m] = g;
    }
    if (maxdiff < 1e-8) break;
  }
}

// [[Rcpp::export]]
NumericVector csac_lngamma_cpp(NumericMatrix psA, NumericVector area, NumericVector vol,
                               NumericVector x, double T,
                               NumericVector ekB, IntegerVector dclass, bool use_dsp) {
  const int nc = psA.nrow();
  if (psA.ncol() != NT) stop("psA must have 153 columns (NHB|OH|OT x 51)");
  NumericVector lng(nc);

  std::vector<double> EW;
  build_EW(T, EW);

  // ---- residual: mixture segment profile + per-component pure profiles ----
  std::vector<double> psig_mix(NT, 0.0);
  double denom = 0.0;
  for (int i = 0; i < nc; ++i) denom += x[i] * area[i];
  for (int k = 0; k < NT; ++k) {
    double s = 0.0;
    for (int i = 0; i < nc; ++i) s += x[i] * psA(i, k);
    psig_mix[k] = (denom > 0.0) ? s / denom : 0.0;
  }
  std::vector<double> Gmix, lnGmix(NT), Gi, psig_i(NT);
  solve_Gamma(EW, psig_mix.data(), Gmix);
  for (int k = 0; k < NT; ++k) lnGmix[k] = std::log(Gmix[k]);

  for (int i = 0; i < nc; ++i) {
    const double Ai = area[i];
    for (int k = 0; k < NT; ++k) psig_i[k] = psA(i, k) / Ai;
    solve_Gamma(EW, psig_i.data(), Gi);
    double resid = 0.0;
    for (int k = 0; k < NT; ++k) resid += psig_i[k] * (lnGmix[k] - std::log(Gi[k]));
    resid *= Ai / AEFFPRIME;

    // ---- Staverman-Guggenheim combinatorial ----
    double sumxq = 0.0, sumxr = 0.0;
    for (int j = 0; j < nc; ++j) { sumxq += x[j] * area[j] / Q0; sumxr += x[j] * vol[j] / R0; }
    const double qi = area[i] / Q0, ri = vol[i] / R0;
    double sumxl = 0.0;
    for (int j = 0; j < nc; ++j) {
      double qj = area[j] / Q0, rj = vol[j] / R0;
      sumxl += x[j] * (ZCOORD / 2.0 * (rj - qj) - (rj - 1.0));
    }
    const double li = ZCOORD / 2.0 * (ri - qi) - (ri - 1.0);
    const double phi_over_x = ri / sumxr;
    const double theta_over_phi = (qi / sumxq) / (ri / sumxr);
    const double comb = std::log(phi_over_x) + ZCOORD / 2.0 * qi * std::log(theta_over_phi)
                        + li - phi_over_x * sumxl;

    lng[i] = resid + comb;
  }

  // ---- dispersion (binary only, both e/kB finite) ----
  if (use_dsp && nc == 2 &&
      R_finite(ekB[0]) && R_finite(ekB[1]) && dclass[0] >= 0 && dclass[1] >= 0) {
    const int W=0, COOH=1, NHB=2, ACC=3, DA=4;
    double w = 0.27027;
    int a = dclass[0], b = dclass[1];
    if ((a == W && b == ACC) || (b == W && a == ACC)) w = -0.27027;
    else if ((a == W && b == COOH) || (b == W && a == COOH)) w = -0.27027;
    else if ((a == COOH && (b == NHB || b == DA)) || (b == COOH && (a == NHB || a == DA))) w = -0.27027;
    const double e0 = ekB[0], e1 = ekB[1];
    const double A = w * (0.5 * (e0 + e1) - std::sqrt(e0 * e1));
    lng[0] += A * x[1] * x[1];
    lng[1] += A * x[0] * x[0];
  }

  return lng;
}

// ===========================================================================================
// Native two-liquid-phase flash (Michelsen stability + isoactivity Rachford-Rice + successive
// substitution) built directly on the COSMO-SAC-dsp engine above. Mirrors R/csac_lle_flash.R's
// .csac_flash_core, but builds the T-only interaction matrix EW and the pure-component segment
// gammas ONCE (the R path rebuilt them on every gamma call), so a flash - and the thousands of
// flashes a blend optimization runs - are far faster with no R<->C++ marshaling per iteration.
// ===========================================================================================

// ln(gamma) for composition x, reusing a prebuilt EW and precomputed pure-component segment ln-gammas.
static void lle_lng(const std::vector<double>& EW, const NumericMatrix& psA,
                    const NumericVector& area, const NumericVector& vol,
                    const std::vector<std::vector<double> >& lnGi_pure,
                    const NumericVector& ekB, const IntegerVector& dclass, bool use_dsp,
                    const std::vector<double>& x, std::vector<double>& lng) {
  const int nc = psA.nrow();
  std::vector<double> psig_mix(NT, 0.0);
  double denom = 0.0;
  for (int i = 0; i < nc; ++i) denom += x[i] * area[i];
  for (int k = 0; k < NT; ++k) {
    double s = 0.0;
    for (int i = 0; i < nc; ++i) s += x[i] * psA(i, k);
    psig_mix[k] = (denom > 0.0) ? s / denom : 0.0;
  }
  std::vector<double> Gmix, lnGmix(NT);
  solve_Gamma(EW, psig_mix.data(), Gmix);
  for (int k = 0; k < NT; ++k) lnGmix[k] = std::log(Gmix[k]);
  lng.assign(nc, 0.0);
  double sumxq = 0.0, sumxr = 0.0;
  for (int j = 0; j < nc; ++j) { sumxq += x[j] * area[j] / Q0; sumxr += x[j] * vol[j] / R0; }
  double sumxl = 0.0;
  for (int j = 0; j < nc; ++j) {
    double qj = area[j] / Q0, rj = vol[j] / R0;
    sumxl += x[j] * (ZCOORD / 2.0 * (rj - qj) - (rj - 1.0));
  }
  for (int i = 0; i < nc; ++i) {
    const double Ai = area[i];
    double resid = 0.0;
    for (int k = 0; k < NT; ++k) { double p = psA(i, k) / Ai; resid += p * (lnGmix[k] - lnGi_pure[i][k]); }
    resid *= Ai / AEFFPRIME;
    const double qi = area[i] / Q0, ri = vol[i] / R0;
    const double li = ZCOORD / 2.0 * (ri - qi) - (ri - 1.0);
    const double phi_over_x = ri / sumxr;
    const double theta_over_phi = (qi / sumxq) / (ri / sumxr);
    lng[i] = resid + std::log(phi_over_x) + ZCOORD / 2.0 * qi * std::log(theta_over_phi)
             + li - phi_over_x * sumxl;
  }
  if (use_dsp && nc == 2 &&
      R_finite(ekB[0]) && R_finite(ekB[1]) && dclass[0] >= 0 && dclass[1] >= 0) {
    const int W=0, COOH=1, NHB=2, ACC=3, DA=4;
    double w = 0.27027; int a = dclass[0], b = dclass[1];
    if ((a==W&&b==ACC)||(b==W&&a==ACC)) w = -0.27027;
    else if ((a==W&&b==COOH)||(b==W&&a==COOH)) w = -0.27027;
    else if ((a==COOH&&(b==NHB||b==DA))||(b==COOH&&(a==NHB||a==DA))) w = -0.27027;
    const double A = w * (0.5 * (ekB[0] + ekB[1]) - std::sqrt(ekB[0] * ekB[1]));
    lng[0] += A * x[1] * x[1]; lng[1] += A * x[0] * x[0];
  }
}

// Gibbs energy of mixing / RT for x (mirrors .csac_gmix).
static double lle_gmix(const std::vector<double>& EW, const NumericMatrix& psA,
                       const NumericVector& area, const NumericVector& vol,
                       const std::vector<std::vector<double> >& lnGi_pure,
                       const NumericVector& ekB, const IntegerVector& dclass, bool use_dsp,
                       const std::vector<double>& x) {
  const int n = x.size(); double s = 0.0;
  for (int i = 0; i < n; ++i) s += x[i];
  std::vector<double> xn(n); for (int i = 0; i < n; ++i) xn[i] = x[i] / s;
  std::vector<double> lng; lle_lng(EW, psA, area, vol, lnGi_pure, ekB, dclass, use_dsp, xn, lng);
  double g = 0.0; for (int i = 0; i < n; ++i) if (xn[i] > 1e-14) g += xn[i] * (std::log(xn[i]) + lng[i]);
  return g;
}

// g(beta) for Rachford-Rice.
static double lle_rr_g(const std::vector<double>& z, const std::vector<double>& Km1, double b) {
  double s = 0.0; const int n = z.size();
  for (int i = 0; i < n; ++i) s += z[i] * Km1[i] / (1.0 + b * Km1[i]);
  return s;
}

// Rachford-Rice for phase-II fraction beta given K = x2/x1 (mirrors .csac_rr). NA_REAL if no interior root.
static double lle_rr(const std::vector<double>& z, const std::vector<double>& K) {
  const int n = z.size();
  std::vector<double> Km1(n); bool allpos = true, allneg = true;
  double Kmax = -1e300, Kmin = 1e300;
  for (int i = 0; i < n; ++i) {
    Km1[i] = K[i] - 1.0; if (Km1[i] < 0) allpos = false; if (Km1[i] > 0) allneg = false;
    if (K[i] > Kmax) Kmax = K[i]; if (K[i] < Kmin) Kmin = K[i];
  }
  if (allpos || allneg) return NA_REAL;
  double lo = 1.0 / (1.0 - Kmax) + 1e-10, hi = 1.0 / (1.0 - Kmin) - 1e-10;
  double glo = lle_rr_g(z, Km1, lo), ghi = lle_rr_g(z, Km1, hi);
  if (glo * ghi > 0) return NA_REAL;
  double b = 0.5 * (lo + hi);
  for (int it = 0; it < 100; ++it) {
    double gb = lle_rr_g(z, Km1, b), dg = 0.0;
    for (int i = 0; i < n; ++i) { double d = 1.0 + b * Km1[i]; dg -= z[i] * Km1[i] * Km1[i] / (d * d); }
    if (gb > 0) lo = b; else hi = b;
    double step = (std::isfinite(dg) && dg != 0.0) ? gb / dg : 1e300;
    double bn = b - step;
    if (!std::isfinite(bn) || bn <= lo || bn >= hi) bn = 0.5 * (lo + hi);
    if (std::fabs(bn - b) < 1e-12) return bn;
    b = bn;
  }
  return b;
}

// Michelsen tangent-plane stability (mirrors .csac_stability). Returns true if a second phase can form
// and fills `wout` with the incipient composition to seed the flash.
static bool lle_stability(const std::vector<double>& EW, const NumericMatrix& psA,
                          const NumericVector& area, const NumericVector& vol,
                          const std::vector<std::vector<double> >& lnGi_pure,
                          const NumericVector& ekB, const IntegerVector& dclass, bool use_dsp,
                          const std::vector<double>& z, std::vector<double>& wout, int max_iter) {
  const int n = z.size();
  std::vector<double> lngz; lle_lng(EW, psA, area, vol, lnGi_pure, ekB, dclass, use_dsp, z, lngz);
  std::vector<double> di(n); for (int i = 0; i < n; ++i) di[i] = std::log(z[i]) + lngz[i];
  double best_tm = 0.0; bool unstable = false; std::vector<double> best_w = z;
  std::vector<double> w(n), lnW(n), W(n), lng(n), lnWnew(n);
  for (int k = 0; k < n; ++k) {
    double sw = 0.0;
    for (int i = 0; i < n; ++i) { w[i] = (i == k) ? 1.0 : 1e-3; sw += w[i]; }
    for (int i = 0; i < n; ++i) { w[i] /= sw; lnW[i] = std::log(w[i]); W[i] = std::exp(lnW[i]); }
    for (int it = 0; it < max_iter; ++it) {
      double sW = 0.0; for (int i = 0; i < n; ++i) sW += W[i];
      for (int i = 0; i < n; ++i) w[i] = W[i] / sW;
      lle_lng(EW, psA, area, vol, lnGi_pure, ekB, dclass, use_dsp, w, lng);
      double maxd = 0.0;
      for (int i = 0; i < n; ++i) { lnWnew[i] = di[i] - lng[i]; double d = std::fabs(lnWnew[i] - lnW[i]); if (d > maxd) maxd = d; }
      for (int i = 0; i < n; ++i) { lnW[i] = lnWnew[i]; W[i] = std::exp(lnW[i]); }
      if (maxd < 1e-9) break;
    }
    double sW = 0.0; for (int i = 0; i < n; ++i) sW += W[i];
    double tm = 1.0 - sW, wdiff = 0.0;
    for (int i = 0; i < n; ++i) { double d = std::fabs(W[i] / sW - z[i]); if (d > wdiff) wdiff = d; }
    if (tm < best_tm && wdiff > 1e-4) { best_tm = tm; unstable = true; for (int i = 0; i < n; ++i) best_w[i] = W[i] / sW; }
  }
  if (best_tm < -1e-7) unstable = true;
  wout = best_w;
  return unstable;
}

//' Two-liquid-phase COSMO-SAC-dsp flash (C++ core)
//'
//' Internal fast path for \code{csac_lle_flash} / \code{csac_lle_optimize_blend}.
//' @keywords internal
// [[Rcpp::export]]
List csac_lle_flash_cpp(NumericMatrix psA, NumericVector area, NumericVector vol,
                        NumericVector z_, double T, NumericVector ekB, IntegerVector dclass,
                        bool use_dsp, Nullable<NumericVector> K_init,
                        int max_iter, double tol, int stab_maxit) {
  const int n = psA.nrow();
  std::vector<double> z(n); double zs = 0.0;
  for (int i = 0; i < n; ++i) { z[i] = std::max((double)z_[i], 1e-12); zs += z[i]; }
  for (int i = 0; i < n; ++i) z[i] /= zs;

  std::vector<double> EW; build_EW(T, EW);
  std::vector<std::vector<double> > lnGi_pure(n, std::vector<double>(NT));
  { std::vector<double> psig_i(NT), Gi;
    for (int i = 0; i < n; ++i) { double Ai = area[i];
      for (int k = 0; k < NT; ++k) psig_i[k] = psA(i, k) / Ai;
      solve_Gamma(EW, psig_i.data(), Gi);
      for (int k = 0; k < NT; ++k) lnGi_pure[i][k] = std::log(Gi[k]); } }

  std::vector<double> K(n);
  if (K_init.isNotNull()) {
    NumericVector Ki(K_init); for (int i = 0; i < n; ++i) K[i] = std::max((double)Ki[i], 1e-300);
  } else {
    std::vector<double> wst;
    if (!lle_stability(EW, psA, area, vol, lnGi_pure, ekB, dclass, use_dsp, z, wst, stab_maxit))
      return List::create(_["split"] = false);
    for (int i = 0; i < n; ++i) K[i] = std::max(wst[i], 1e-12) / z[i];
  }

  std::vector<double> x1(z), x2(z), lng1, lng2; double beta = NA_REAL;
  for (int it = 0; it < max_iter; ++it) {
    beta = lle_rr(z, K); if (ISNA(beta)) break;
    double s1 = 0.0, s2 = 0.0;
    for (int i = 0; i < n; ++i) { x1[i] = z[i] / (1.0 + beta * (K[i] - 1.0)); x2[i] = K[i] * x1[i]; }
    for (int i = 0; i < n; ++i) { s1 += x1[i]; s2 += x2[i]; }
    for (int i = 0; i < n; ++i) { x1[i] /= s1; x2[i] /= s2; }
    lle_lng(EW, psA, area, vol, lnGi_pure, ekB, dclass, use_dsp, x1, lng1);
    lle_lng(EW, psA, area, vol, lnGi_pure, ekB, dclass, use_dsp, x2, lng2);
    double maxd = 0.0; std::vector<double> Knew(n);
    for (int i = 0; i < n; ++i) { Knew[i] = std::exp(lng1[i] - lng2[i]);
      double d = std::fabs(std::log(Knew[i]) - std::log(K[i])); if (d > maxd) maxd = d; }
    if (maxd < tol) { K = Knew; break; }
    K = Knew;
  }
  double xdiff = 0.0; for (int i = 0; i < n; ++i) { double d = std::fabs(x1[i] - x2[i]); if (d > xdiff) xdiff = d; }
  if (ISNA(beta) || xdiff < 1e-4) return List::create(_["split"] = false);

  lle_lng(EW, psA, area, vol, lnGi_pure, ekB, dclass, use_dsp, x1, lng1);
  lle_lng(EW, psA, area, vol, lnGi_pure, ekB, dclass, use_dsp, x2, lng2);
  double dG = beta * lle_gmix(EW, psA, area, vol, lnGi_pure, ekB, dclass, use_dsp, x2)
            + (1.0 - beta) * lle_gmix(EW, psA, area, vol, lnGi_pure, ekB, dclass, use_dsp, x1)
            - lle_gmix(EW, psA, area, vol, lnGi_pure, ekB, dclass, use_dsp, z);
  if (!(dG < 1e-8)) return List::create(_["split"] = false);

  if (x2[0] > x1[0]) { std::swap(x1, x2); std::swap(lng1, lng2); beta = 1.0 - beta;
    for (int i = 0; i < n; ++i) K[i] = 1.0 / K[i]; }
  NumericVector X1(n), X2(n), G1(n), G2(n), KK(n), Z(n);
  for (int i = 0; i < n; ++i) { X1[i]=x1[i]; X2[i]=x2[i]; G1[i]=std::exp(lng1[i]); G2[i]=std::exp(lng2[i]); KK[i]=K[i]; Z[i]=z[i]; }
  return List::create(_["split"]=true, _["beta"]=beta, _["dG_mix"]=dG,
                      _["x1"]=X1, _["x2"]=X2, _["gamma1"]=G1, _["gamma2"]=G2, _["K"]=KK, _["z"]=Z);
}
