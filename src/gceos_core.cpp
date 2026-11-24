// GC-EOS thermodynamic core (group-contribution equation of state).
//
// Residual Helmholtz energy A^R/(RT) at (T, V, n) per Skjold-Jorgensen (1988):
//   free-volume (Mansoori-Leland hard spheres) + a density-dependent
//   group-contribution NRTL attractive term. Pressure and fugacity coefficients
//   are obtained by numerical differentiation of this exact A^R (the analytic
//   derivatives of the supplement can replace these later as a pure speed
//   optimisation, validated against the numerical results).
//
// All temperature-dependent quantities (component diameters d_i(T), group
// energies g_kj(T), Delta g_kj = g_kj - g_jj) are precomputed on the R side and
// passed in, so this hot path is a cheap function of (V, n).
//
// Units: pressure in atm, V in cm^3, R = 82.06 cm^3 atm / mol / K (matching the
// published g* parameter units, cm^6 atm mol^-1 / segment^2).

#include <Rcpp.h>
#include <vector>
#include <cmath>
using namespace Rcpp;

// forward decl: analytic Z (+ optional ln phi) for the GC-EOS dispersive part
static double gceos_props_core(double T, double V, const NumericVector& n,
    const NumericVector& d, const NumericVector& q, const NumericMatrix& nu,
    const NumericMatrix& g, const NumericMatrix& dg, const NumericMatrix& alpha,
    double R, std::vector<double>* lnphi);

// Association (Wertheim/SAFT) contribution to A^R/(RT) for the GCA-EOS.
// numu(na,NC) = count of association site 'a' on molecule m; Delta0(na,na) =
// kappa_ab*(exp(eps_ab/RT)-1) precomputed on the R side at T. Returns 0 when no
// association sites are supplied (na=0), so pure GC-EOS is unaffected.
static double gceos_a_assoc(double V, const NumericVector& n,
                            const Nullable<NumericMatrix>& numu_,
                            const Nullable<NumericMatrix>& Delta0_) {
  if (numu_.isNull() || Delta0_.isNull()) return 0.0;
  NumericMatrix NU(numu_), DL(Delta0_);
  const int na = NU.nrow(), NC = n.size();
  if (na == 0) return 0.0;
  std::vector<double> nstar(na, 0.0);
  for (int a=0;a<na;a++){ double s=0; for(int mm=0; mm<NC; mm++) s += NU(a,mm)*n[mm]; nstar[a]=s; }
  std::vector<double> X(na, 1.0), Xn(na);
  for (int it=0; it<500; it++){
    double err=0;
    for (int a=0;a<na;a++){
      double s=0; for(int b=0;b<na;b++) s += nstar[b]*X[b]*DL(a,b)/V;
      Xn[a]=1.0/(1.0+s);
    }
    for (int a=0;a<na;a++){ err+=std::fabs(Xn[a]-X[a]); X[a]=0.5*X[a]+0.5*Xn[a]; }  // damped successive subst.
    if (err<1e-13) break;
  }
  double A=0;
  for (int a=0;a<na;a++) if (nstar[a]>0 && X[a]>0) A += nstar[a]*(std::log(X[a]) - 0.5*X[a] + 0.5);
  return A;
}

//' GC(A)-EOS residual Helmholtz energy A^R/(RT). Optional association inputs
//' \code{numu} (na x NC site counts) and \code{Delta0} (na x na strengths) add
//' the Wertheim association term; omit them for plain GC-EOS.
//' @keywords internal
// [[Rcpp::export]]
double gceos_ar_rt(double T, double V, NumericVector n, NumericVector d,
                   NumericVector q, NumericMatrix nu, NumericMatrix g,
                   NumericMatrix dg, NumericMatrix alpha, double R,
                   Nullable<NumericMatrix> numu = R_NilValue,
                   Nullable<NumericMatrix> Delta0 = R_NilValue) {
  const int NC = n.size();
  const int NG = q.size();
  const double z = 10.0;

  // free-volume term
  double lam1=0, lam2=0, lam3=0, ntot=0;
  for (int i=0;i<NC;i++){ double di=d[i]; lam1+=n[i]*di; lam2+=n[i]*di*di; lam3+=n[i]*di*di*di; ntot+=n[i]; }
  double Y = 1.0/(1.0 - M_PI*lam3/(6.0*V));
  double Afv = 3.0*(lam1*lam2/lam3)*(Y-1.0)
             + (lam2*lam2*lam2/(lam3*lam3))*(Y*Y - Y - std::log(Y))
             + ntot*std::log(Y);

  // attractive term
  std::vector<double> ng(NG,0.0);
  for (int j=0;j<NG;j++){ double s=0; for(int i=0;i<NC;i++) s+=nu(i,j)*n[i]; ng[j]=s; }
  double qtil=0; for(int j=0;j<NG;j++) qtil+=ng[j]*q[j];
  std::vector<double> theta(NG,0.0);
  for(int j=0;j<NG;j++) theta[j]= (qtil>0)? ng[j]*q[j]/qtil : 0.0;
  double RTV = R*T*V;

  double Aatt=0;
  for(int j=0;j<NG;j++){
    if (ng[j]==0.0) continue;
    double num=0, den=0;
    for(int k=0;k<NG;k++){
      if (theta[k]==0.0) continue;
      double tau = std::exp(alpha(k,j)*dg(k,j)*qtil/RTV);
      num += theta[k]*g(k,j)*tau;
      den += theta[k]*tau;
    }
    num *= qtil/RTV;
    Aatt += ng[j]*q[j]*num/den;
  }
  Aatt *= -(z/2.0);

  return Afv + Aatt + gceos_a_assoc(V, n, numu, Delta0);
}

//' GC-EOS pressure (atm): P = nRT/V - dA^R/dV
//' @keywords internal
// [[Rcpp::export]]
double gceos_pressure(double T, double V, NumericVector n, NumericVector d,
                      NumericVector q, NumericMatrix nu, NumericMatrix g,
                      NumericMatrix dg, NumericMatrix alpha, double R,
                      Nullable<NumericMatrix> numu = R_NilValue,
                      Nullable<NumericMatrix> Delta0 = R_NilValue) {
  double ntot=0; for (int i=0;i<n.size();i++) ntot+=n[i];
  if (numu.isNull() && Delta0.isNull()) {            // analytic (A1-A3) -- fast path
    double Z = gceos_props_core(T,V,n,d,q,nu,g,dg,alpha,R,nullptr);
    return Z*ntot*R*T/V;
  }
  double h = V*1e-6;                                  // numerical (association present)
  double fp = gceos_ar_rt(T, V+h, n, d,q,nu,g,dg,alpha,R, numu, Delta0);
  double fm = gceos_ar_rt(T, V-h, n, d,q,nu,g,dg,alpha,R, numu, Delta0);
  double dAR_dV = R*T*(fp-fm)/(2.0*h);
  return ntot*R*T/V - dAR_dV;
}

//' GC-EOS log fugacity coefficients ln(phi_i) at (T, V, n)
//' ln phi_i = d(A^R/RT)/dn_i |_(T,V) - ln Z
//' @keywords internal
// [[Rcpp::export]]
NumericVector gceos_lnphi(double T, double V, NumericVector n, NumericVector d,
                          NumericVector q, NumericMatrix nu, NumericMatrix g,
                          NumericMatrix dg, NumericMatrix alpha, double R,
                          Nullable<NumericMatrix> numu = R_NilValue,
                          Nullable<NumericMatrix> Delta0 = R_NilValue) {
  const int NC = n.size();
  if (numu.isNull() && Delta0.isNull()) {            // analytic (A4-A6) -- fast path
    std::vector<double> lp; gceos_props_core(T,V,n,d,q,nu,g,dg,alpha,R,&lp);
    return NumericVector(lp.begin(), lp.end());
  }
  double ntot=0; for (int i=0;i<NC;i++) ntot+=n[i];   // numerical (association present)
  double P = gceos_pressure(T,V,n,d,q,nu,g,dg,alpha,R, numu, Delta0);
  double Z = P*V/(ntot*R*T);
  NumericVector lnphi(NC);
  for (int i=0;i<NC;i++){
    double h = std::max(n[i]*1e-6, 1e-9);
    NumericVector np = clone(n), nm = clone(n);
    np[i]+=h; nm[i]-=h;
    double fp = gceos_ar_rt(T,V,np,d,q,nu,g,dg,alpha,R, numu, Delta0);
    double fm = gceos_ar_rt(T,V,nm,d,q,nu,g,dg,alpha,R, numu, Delta0);
    lnphi[i] = (fp-fm)/(2.0*h) - std::log(Z);
  }
  return lnphi;
}

// Analytic compressibility factor Z and log-fugacity coefficients for the GC-EOS
// dispersive part (free-volume + group-NRTL attractive), Skjold-Jorgensen 1988
// eqs A1-A6 + auxiliaries. Returns Z; if lnphi!=nullptr fills it (NC). Far faster
// and noise-free vs numerical differentiation. Association is NOT included here.
static double gceos_props_core(double T, double V, const NumericVector& n,
    const NumericVector& d, const NumericVector& q, const NumericMatrix& nu,
    const NumericMatrix& g, const NumericMatrix& dg, const NumericMatrix& alpha,
    double R, std::vector<double>* lnphi) {
  const int NC=n.size(), NG=q.size();
  const double zc=10.0, PI=M_PI;
  double lam1=0,lam2=0,lam3=0,ntot=0;
  for(int i=0;i<NC;i++){ double di=d[i]; lam1+=n[i]*di; lam2+=n[i]*di*di; lam3+=n[i]*di*di*di; ntot+=n[i]; }
  double Y = 1.0/(1.0 - PI*lam3/(6.0*V));
  std::vector<double> ng(NG,0.0);
  for(int j=0;j<NG;j++){ double s=0; for(int i=0;i<NC;i++) s+=nu(i,j)*n[i]; ng[j]=s; }
  double qtil=0; for(int j=0;j<NG;j++) qtil+=ng[j]*q[j];
  std::vector<double> theta(NG,0.0);
  for(int j=0;j<NG;j++) theta[j]=(qtil>0)?ng[j]*q[j]/qtil:0.0;
  double RTV=R*T*V;
  std::vector<double> H4(NG,0.0),H2(NG,0.0),H5(NG,0.0),H6(NG,0.0),TAU(NG*NG,0.0),GF(NG*NG,0.0);
  for(int j=0;j<NG;j++){
    double s4=0,s2=0,s5=0,s6=0;
    for(int k=0;k<NG;k++){
      double e=alpha(k,j)*dg(k,j)*qtil/RTV, tau=std::exp(e), gf=g(k,j)*qtil/RTV;
      TAU[k*NG+j]=tau; GF[k*NG+j]=gf; double th=theta[k];
      s4+=th*tau; s2+=th*tau*gf; s5+=th*tau*gf*e; s6+=th*tau*e;
    }
    H4[j]=s4; H2[j]=(s4>0)?s2/s4:0; H5[j]=(s4>0)?s5/s4:0; H6[j]=(s4>0)?s6/s4:0;
  }
  double lam23=lam2*lam2*lam2/(lam3*lam3);
  double brk=3.0*(lam1*lam2/lam3)+lam23*(2*Y-1-1.0/Y)+ntot/Y;
  double Zfv=brk*Y*Y*PI*lam3/(6.0*V);
  double Zatt_sum=0; for(int j=0;j<NG;j++) Zatt_sum+=ng[j]*q[j]*(H5[j]+H2[j]-H2[j]*H6[j]);
  double Zatt=-(zc/(2.0*ntot))*Zatt_sum;
  double Z=Zfv+Zatt+1.0;
  if (lnphi) {
    lnphi->assign(NC,0.0);
    double lnY=std::log(Y), l1l2_l3=lam1*lam2/lam3;
    for(int i=0;i<NC;i++){
      double di=d[i];
      double lp1=di/lam1, lp2=di*di/lam2, lp3=di*di*di/lam3;
      double Yp=Y*Y*PI*di*di*di/(6.0*V);
      double fv=3.0*(Y-1)*l1l2_l3*(lp1+lp2-lp3+Yp/(Y-1))
               +lam23*(Y*Y-Y-lnY)*(3*lp2-2*lp3)
               +lam23*(2*Y-1-1.0/Y)*Yp + lnY + (ntot/Y)*Yp;
      double MSi=0; for(int j=0;j<NG;j++) MSi+=nu(i,j)*q[j];
      double att=0;
      for(int j=0;j<NG;j++){
        double s3=0,s7=0;
        for(int k=0;k<NG;k++){ double PSik=nu(i,k)*q[k]; s3+=PSik*TAU[k*NG+j]*GF[k*NG+j]; s7+=PSik*TAU[k*NG+j]; }
        double H3=(H4[j]>0)?s3/H4[j]:0, H7=(H4[j]>0)?s7/H4[j]:0, PSij=nu(i,j)*q[j];
        att += PSij*H2[j] + theta[j]*(H3+MSi*H5[j]) - theta[j]*H2[j]*(H7 - MSi + H6[j]*MSi);
      }
      att *= -(zc/2.0);
      (*lnphi)[i]=fv+att-std::log(Z);
    }
  }
  return Z;
}

//' Analytic GC-EOS properties (Z and ln phi) via Skjold-Jorgensen A1-A6.
//' Dispersive part only (no association). For validation against the numerical
//' gceos_pressure / gceos_lnphi.
//' @keywords internal
// [[Rcpp::export]]
List gceos_props_an(double T, double V, NumericVector n, NumericVector d,
                    NumericVector q, NumericMatrix nu, NumericMatrix g,
                    NumericMatrix dg, NumericMatrix alpha, double R) {
  std::vector<double> lp;
  double Z = gceos_props_core(T,V,n,d,q,nu,g,dg,alpha,R,&lp);
  return List::create(_["Z"]=Z, _["lnphi"]=NumericVector(lp.begin(),lp.end()));
}

//' GC-EOS volume root of P(T,V,n)=Ptarget. want=0 liquid (smallest root),
//' want=1 vapor (largest). Scans above the hard-sphere covolume for sign
//' changes and refines by bisection - all in C++ (no per-point R overhead).
//' @keywords internal
// [[Rcpp::export]]
double gceos_vroot(double T, double Ptarget, NumericVector n, NumericVector d,
                   NumericVector q, NumericMatrix nu, NumericMatrix g,
                   NumericMatrix dg, NumericMatrix alpha, double R, int want,
                   Nullable<NumericMatrix> numu = R_NilValue,
                   Nullable<NumericMatrix> Delta0 = R_NilValue, double V0 = -1.0) {
  int NC = n.size();
  double b = 0; for (int i=0;i<NC;i++) b += n[i]*d[i]*d[i]*d[i];
  b *= M_PI/6.0;
  const double Vlo = 1.02*b, Vhi = 5e4;
  // warm start: Newton from the hint V0 (the same root moves little between
  // iterations). Accept only if it converges NEAR V0 (so we keep the same
  // liquid/vapor branch); otherwise fall through to the robust scan.
  if (V0 > Vlo && V0 < Vhi) {
    double V = V0, fc = gceos_pressure(T,V,n,d,q,nu,g,dg,alpha,R,numu,Delta0) - Ptarget;
    bool conv = false;
    for (int it=0; it<25; it++) {
      double h = V*1e-6;
      double fp = gceos_pressure(T,V+h,n,d,q,nu,g,dg,alpha,R,numu,Delta0) - Ptarget;
      double dfdv = (fp-fc)/h;
      if (std::fabs(dfdv) < 1e-300) break;
      double Vn = V - fc/dfdv;
      if (Vn <= Vlo) Vn = 0.5*(V+Vlo);
      if (Vn >= Vhi) break;
      double fn = gceos_pressure(T,Vn,n,d,q,nu,g,dg,alpha,R,numu,Delta0) - Ptarget;
      if (std::fabs(Vn-V) < 1e-9*V) { V=Vn; fc=fn; conv=true; break; }
      V=Vn; fc=fn;
    }
    if (conv && std::fabs(fc) < 1e-6*Ptarget && V > 0.2*V0 && V < 5.0*V0) return V;
  }
  const int npts = 60;   // P(V) has <=3 well-separated roots on log-V; 60 brackets them
  const double llo = std::log(Vlo), lhi = std::log(Vhi), step = (lhi-llo)/(npts-1);
  double Vprev = Vlo;
  double fprev = gceos_pressure(T,Vlo,n,d,q,nu,g,dg,alpha,R, numu, Delta0) - Ptarget;
  double root = NA_REAL; bool found = false;
  for (int k=1;k<npts;k++) {
    double V = std::exp(llo + step*k);
    double fc = gceos_pressure(T,V,n,d,q,nu,g,dg,alpha,R, numu, Delta0) - Ptarget;
    if (fprev*fc < 0) {                       // bracket -> bisection refine
      double a=Vprev, c=V, fa=fprev;
      for (int it=0; it<60; it++) {
        double m=0.5*(a+c);
        double fm = gceos_pressure(T,m,n,d,q,nu,g,dg,alpha,R, numu, Delta0) - Ptarget;
        if (fa*fm<=0) c=m; else { a=m; fa=fm; }
        if (c-a < 1e-9*m) break;
      }
      root = 0.5*(a+c);
      if (want==0) { found=true; break; }      // liquid: first (smallest) root
      found=true;                              // vapor: keep last
    }
    Vprev=V; fprev=fc;
  }
  return found ? root : NA_REAL;
}

//' GC-EOS stage K-values K_i = phi_i^L(x)/phi_i^V(y) at (T,P). Does the liquid
//' and vapor volume-root solves and both fugacity evaluations in one C++ call -
//' the column/flash hot path. Returns NA vector if a root fails.
//' @keywords internal
// [[Rcpp::export]]
NumericVector gceos_stage_K(double T, double P, NumericVector x, NumericVector y,
                            NumericVector d, NumericVector q, NumericMatrix nu,
                            NumericMatrix g, NumericMatrix dg, NumericMatrix alpha, double R,
                            Nullable<NumericMatrix> numu = R_NilValue,
                            Nullable<NumericMatrix> Delta0 = R_NilValue) {
  int NC = x.size();
  double VL = gceos_vroot(T,P,x,d,q,nu,g,dg,alpha,R,0, numu, Delta0);
  double VV = gceos_vroot(T,P,y,d,q,nu,g,dg,alpha,R,1, numu, Delta0);
  NumericVector K(NC);
  if (NumericVector::is_na(VL) || NumericVector::is_na(VV)) {
    for (int i=0;i<NC;i++) K[i]=NA_REAL;
    return K;
  }
  NumericVector lpL = gceos_lnphi(T,VL,x,d,q,nu,g,dg,alpha,R, numu, Delta0);
  NumericVector lpV = gceos_lnphi(T,VV,y,d,q,nu,g,dg,alpha,R, numu, Delta0);
  for (int i=0;i<NC;i++) K[i] = std::exp(lpL[i]-lpV[i]);
  return K;
}

// ============================================================================
// Saturation (bubble / dew) pressure, fully in C++.
//
// Replaces the R scan+uniroot+SS-loop wrapper around gceos_stage_K (which paid
// ~1000 R->C++ crossings per point). mode 0 = bubble (spec = liquid x, find P
// with sum K_i x_i = 1); mode 1 = dew (spec = vapor y, find P with sum y_i/K_i =
// 1). Inner successive substitution on K is warm-started across the pressure
// scan; returns the saturation pressure in the same units as [Plo,Phi], or NA.
// ============================================================================
// [[Rcpp::export]]
double gceos_satP(double T, NumericVector spec, int mode,
                  NumericVector d, NumericVector q, NumericMatrix nu,
                  NumericMatrix g, NumericMatrix dg, NumericMatrix alpha, double R,
                  double Plo, double Phi, int maxit = 40, double tol = 1e-9,
                  int nscan = 14,
                  Nullable<NumericMatrix> numu = R_NilValue,
                  Nullable<NumericMatrix> Delta0 = R_NilValue) {
  int NC = spec.size();
  // physical K init: lightest group (smallest d) gets the high K
  double dmn = d[0], dmx = d[0];
  for (int i = 1; i < NC; i++) { if (d[i] < dmn) dmn = d[i]; if (d[i] > dmx) dmx = d[i]; }
  double rg = dmx - dmn; if (rg < 1e-12) rg = 1.0;
  NumericVector K0(NC);
  for (int i = 0; i < NC; i++) { double fr = (d[i]-dmn)/rg;
    K0[i] = std::exp(std::log(4.0)*(1.0-fr) + std::log(0.01)*fr); }

  // converged (S-1) at pressure P, warm-starting K (Kw updated in place on a
  // non-trivial converge). Returns -0.5 for a trivial (single-phase) collapse,
  // NA on a failed K.
  auto fP = [&](double P, NumericVector& Kw) -> double {
    NumericVector K = clone(Kw), other(NC), xl(NC), yv(NC);
    for (int it = 0; it < maxit; it++) {
      double s = 0;
      for (int i = 0; i < NC; i++) other[i] = (mode == 0) ? K[i]*spec[i] : spec[i]/K[i];
      for (int i = 0; i < NC; i++) s += other[i];
      if (!(s > 0)) return NA_REAL;
      for (int i = 0; i < NC; i++) other[i] /= s;
      if (mode == 0) { xl = spec; yv = other; } else { xl = other; yv = spec; }
      NumericVector Kn = gceos_stage_K(T, P, xl, yv, d, q, nu, g, dg, alpha, R, numu, Delta0);
      double dm = 0;
      for (int i = 0; i < NC; i++) {
        if (NumericVector::is_na(Kn[i]) || !R_finite(Kn[i]) || Kn[i] <= 0) return NA_REAL;
        double l = std::fabs(std::log(Kn[i]/K[i])); if (l > dm) dm = l;
      }
      K = Kn;
      if (dm < tol) break;
    }
    double S = 0, mlog = 0;
    for (int i = 0; i < NC; i++) { S += (mode == 0) ? K[i]*spec[i] : spec[i]/K[i];
      double l = std::fabs(std::log(K[i])); if (l > mlog) mlog = l; }
    if (mlog < 0.02) return -0.5;                 // trivial: x==y (single phase)
    Kw = K;                                       // carry warm start only when non-trivial
    return S - 1.0;
  };

  // log scan for the first +/- crossing (non-trivial S=1 boundary)
  double llo = std::log(Plo), lhi = std::log(Phi);
  std::vector<double> Ps(nscan), Fs(nscan);
  NumericVector Kw = clone(K0);
  for (int k = 0; k < nscan; k++) {
    Ps[k] = std::exp(llo + (lhi-llo)*k/(nscan-1));
    Fs[k] = fP(Ps[k], Kw);
  }
  double Pa = NA_REAL, Pb = NA_REAL, fa = NA_REAL;
  for (int k = 1; k < nscan; k++) {
    if (R_finite(Fs[k-1]) && R_finite(Fs[k]) && Fs[k-1] > 0 && Fs[k] < 0) {
      Pa = Ps[k-1]; Pb = Ps[k]; fa = Fs[k-1]; break;
    }
  }
  if (!R_finite(Pa)) return NA_REAL;
  Kw = clone(K0);
  for (int it = 0; it < 80; it++) {               // bisection on the bracket
    double Pm = 0.5*(Pa+Pb);
    NumericVector Kt = clone(Kw);
    double fm = fP(Pm, Kt);
    if (!R_finite(fm)) { Pb = Pm; continue; }
    if (fa*fm <= 0) Pb = Pm; else { Pa = Pm; fa = fm; Kw = Kt; }
    if ((Pb - Pa) < 1e-7*Pm) break;
  }
  return 0.5*(Pa+Pb);
}

// ============================================================================
// Phase-stability-tested two-phase flash (GCFLASH / GCEXTRAC-style core).
//
// The column hot path. Unlike a bare phi^L/phi^V ratio at the current stage
// compositions (which has the trivial x=y, K=1 state as a valid fixed point and
// collapses to it near the mixture critical point), this builds x and y as a
// Rachford-Rice material-balance split of the stage bulk z. They therefore stay
// a genuine two-phase split unless the flash genuinely converges to one phase --
// which is detected (no interior RR root) or rejected (trivial K) explicitly.
// ============================================================================

// Rachford-Rice vapor fraction for feed z and K. Sets twophase=false (returns a
// single-phase boundary beta) when all K lie on one side of unity.
static double rr_beta(const NumericVector& z, const NumericVector& K, bool& twophase) {
  int NC = z.size();
  double g0 = 0, g1 = 0;
  for (int i=0;i<NC;i++){ g0 += z[i]*(K[i]-1.0); g1 += z[i]*(1.0 - 1.0/K[i]); }
  if (g0 <= 0) { twophase=false; return 0.0; }   // bubble / subcooled liquid
  if (g1 >= 0) { twophase=false; return 1.0; }   // dew / superheated vapor
  twophase = true;
  double a=0.0, b=1.0, beta=0.5;
  for (int it=0; it<100; it++) {
    beta = 0.5*(a+b); double gm=0;
    for (int i=0;i<NC;i++){ double t=K[i]-1.0; gm += z[i]*t/(1.0+beta*t); }
    if (gm > 0) a=beta; else b=beta;
    if (b-a < 1e-13) break;
  }
  return beta;
}

// Successive-substitution flash from an initial K (in/out). Returns 1 two-phase,
// 0 single-phase (RR boundary or trivial), -1 root failure. Fills x,y,beta.
static int flash_core(double T, double P, const NumericVector& z,
    const NumericVector& d, const NumericVector& q, const NumericMatrix& nu,
    const NumericMatrix& g, const NumericMatrix& dg, const NumericMatrix& alpha, double R,
    NumericVector& K, NumericVector& x, NumericVector& y, double& beta,
    int maxit, double trivtol,
    const Nullable<NumericMatrix>& numu = R_NilValue,
    const Nullable<NumericMatrix>& Delta0 = R_NilValue) {
  int NC = z.size();
  for (int it=0; it<maxit; it++) {
    bool twophase; beta = rr_beta(z, K, twophase);
    if (!twophase) return 0;
    double sx=0, sy=0;
    for (int i=0;i<NC;i++){ double t=K[i]-1.0; x[i]=z[i]/(1.0+beta*t); y[i]=K[i]*x[i]; sx+=x[i]; sy+=y[i]; }
    for (int i=0;i<NC;i++){ x[i]/=sx; y[i]/=sy; }
    NumericVector Kn = gceos_stage_K(T,P,x,y,d,q,nu,g,dg,alpha,R, numu, Delta0);  // liquid root x, vapor root y
    if (NumericVector::is_na(Kn[0])) return -1;
    double err=0, triv=0;
    for (int i=0;i<NC;i++){ double lnKn=std::log(Kn[i]); err+=std::fabs(lnKn-std::log(K[i])); triv+=std::fabs(lnKn); }
    for (int i=0;i<NC;i++) K[i]=Kn[i];
    if (triv < trivtol) return 0;   // trivial solution -> single phase
    if (err  < 1e-11)   return 1;   // converged two-phase
  }
  return 1;
}

// K initialised from the feed's own liquid/vapor volume roots (EOS-native, no
// Wilson/Tc-Pc needed). Returns true if a distinct-root estimate was found; on
// false K is left untouched (caller supplies a fallback split).
static bool kinit_feed(double T, double P, const NumericVector& z,
    const NumericVector& d, const NumericVector& q, const NumericMatrix& nu,
    const NumericMatrix& g, const NumericMatrix& dg, const NumericMatrix& alpha, double R,
    NumericVector& K,
    const Nullable<NumericMatrix>& numu = R_NilValue,
    const Nullable<NumericMatrix>& Delta0 = R_NilValue) {
  int NC = z.size();
  double VL = gceos_vroot(T,P,z,d,q,nu,g,dg,alpha,R,0, numu, Delta0);
  double VV = gceos_vroot(T,P,z,d,q,nu,g,dg,alpha,R,1, numu, Delta0);
  if (!NumericVector::is_na(VL) && !NumericVector::is_na(VV) &&
      std::fabs(VL-VV) > 1e-6*std::max(VL,VV)) {
    NumericVector lpL = gceos_lnphi(T,VL,z,d,q,nu,g,dg,alpha,R, numu, Delta0);
    NumericVector lpV = gceos_lnphi(T,VV,z,d,q,nu,g,dg,alpha,R, numu, Delta0);
    for (int i=0;i<NC;i++) K[i] = std::exp(lpL[i]-lpV[i]);
    return true;
  }
  return false;
}

// Molecular-size spread K-guess: a smaller hard-sphere diameter d_i (a lighter,
// more volatile species, e.g. the solvent) gets a larger K. Gives the correct
// split DIRECTION generically from d alone when the feed bulk is single-root
// (a supercritical fluid that nonetheless splits into solvent-rich/solute-rich
// phases). `s` scales the spread; trying a few s brackets the real split.
static void kinit_size(const NumericVector& d, double s, NumericVector& K) {
  int NC = d.size(); double dbar=0; for (int i=0;i<NC;i++) dbar+=d[i]; dbar/=NC;
  for (int i=0;i<NC;i++) K[i] = std::exp(s*(dbar - d[i])/dbar);
}

// Physical K-spread (the proven gceos_flash guesses, generalised): the lightest
// species (smallest hard-sphere diameter) gets highK, the heaviest gets lowK,
// interpolated in log-K by d-VALUE. With (lowK,highK)=(0.01,4) this reproduces
// the R c(0.03,0.015,4)-style guesses from d alone, reaching K~0.01 for solutes
// even when the feed bulk is a single supercritical volume root (high solvent).
static void kinit_phys(const NumericVector& d, double lowK, double highK, NumericVector& K) {
  int NC=d.size(); double dmin=d[0],dmax=d[0];
  for(int i=1;i<NC;i++){ dmin=std::min(dmin,(double)d[i]); dmax=std::max(dmax,(double)d[i]); }
  double range=dmax-dmin; if(range<1e-12) range=1.0;
  double ll=std::log(lowK), lh=std::log(highK);
  for(int i=0;i<NC;i++){ double frac=(d[i]-dmin)/range; K[i]=std::exp(lh*(1.0-frac)+ll*frac); }
}

//' GC-EOS isothermal two-phase flash. status: 1 two-phase, 0 single-phase,
//' -1 root failure. Warm-starts from Kguess when supplied (the column passes the
//' previous stage K); otherwise initialises K from the feed's own volume roots
//' and, if that looks single-phase, probes once with an amplified split.
//' @keywords internal
// [[Rcpp::export]]
List gceos_flash_cpp(double T, double P, NumericVector z,
    NumericVector d, NumericVector q, NumericMatrix nu, NumericMatrix g,
    NumericMatrix dg, NumericMatrix alpha, double R,
    Nullable<NumericVector> Kguess = R_NilValue, int maxit = 300, double trivtol = 1e-4,
    Nullable<NumericMatrix> numu = R_NilValue, Nullable<NumericMatrix> Delta0 = R_NilValue) {
  int NC = z.size();
  NumericVector zz = clone(z); double zs=0;
  for (int i=0;i<NC;i++) zs+=zz[i];
  for (int i=0;i<NC;i++) zz[i]/=zs;
  NumericVector K(NC), x(NC), y(NC); double beta=0; int status;
  if (Kguess.isNotNull()) {
    K = clone(NumericVector(Kguess));
    status = flash_core(T,P,zz,d,q,nu,g,dg,alpha,R,K,x,y,beta,maxit,trivtol, numu, Delta0);
  } else {
    // multistart: feed two-root estimate, then the proven physical (lowK,highK)
    // spreads (reach K~0.01 for solutes at high solvent dilution), then size
    // spreads as a fallback. Accept the first that converges to two real phases.
    std::vector<NumericVector> trials;
    NumericVector Kf(NC);
    if (kinit_feed(T,P,zz,d,q,nu,g,dg,alpha,R,Kf, numu, Delta0)) trials.push_back(clone(Kf));
    double pairs[4][2] = {{0.01,4.0},{0.005,6.0},{0.05,3.0},{0.002,10.0}};
    for (int p=0;p<4;p++){ NumericVector Kp(NC); kinit_phys(d,pairs[p][0],pairs[p][1],Kp); trials.push_back(Kp); }
    for (double s : {3.0, 8.0}) { NumericVector Ks(NC); kinit_size(d,s,Ks); trials.push_back(Ks); }
    status = 0;
    for (size_t t=0; t<trials.size(); t++) {
      NumericVector Kt=clone(trials[t]), xt(NC), yt(NC); double bt=0;
      int st = flash_core(T,P,zz,d,q,nu,g,dg,alpha,R,Kt,xt,yt,bt,maxit,trivtol, numu, Delta0);
      if (st == 1) { status=1; K=Kt; x=xt; y=yt; beta=bt; break; }
      if (st == -1 && status == 0) status = -1;
    }
  }
  if (status != 1)
    return List::create(_["status"]=status, _["beta"]=NA_REAL, _["x"]=zz, _["y"]=zz, _["K"]=K);
  return List::create(_["status"]=status, _["beta"]=beta, _["x"]=x, _["y"]=y, _["K"]=K);
}

//' GC-EOS robust per-stage K via a warm-started flash of the stage bulk z.
//' Returns the two-phase K, or an NA vector when the stage is single-phase or a
//' root fails (the caller then holds the previous stage K). This is the column's
//' per-stage engine: it cannot return the trivial K=1 of a collapsed x=y stage.
//' @keywords internal
// [[Rcpp::export]]
NumericVector gceos_stage_flashK(double T, double P, NumericVector z, NumericVector Kseed,
    NumericVector d, NumericVector q, NumericMatrix nu, NumericMatrix g,
    NumericMatrix dg, NumericMatrix alpha, double R, int maxss = 60, double trivtol = 1e-4,
    Nullable<NumericMatrix> numu = R_NilValue, Nullable<NumericMatrix> Delta0 = R_NilValue) {
  int NC = z.size();
  NumericVector zz = clone(z); double zs=0;
  for (int i=0;i<NC;i++) zs+=zz[i];
  for (int i=0;i<NC;i++) zz[i]/=zs;
  NumericVector K = clone(Kseed), x(NC), y(NC); double beta=0;
  int status = flash_core(T,P,zz,d,q,nu,g,dg,alpha,R,K,x,y,beta,maxss,trivtol, numu, Delta0);
  if (status == 1) return K;
  NumericVector na(NC); for (int i=0;i<NC;i++) na[i]=NA_REAL; return na;
}

// ============================================================================
// Three-phase (vapor-liquid-liquid, VLLE) flash.
//
// For associating systems (fatty acids, water, alcohols + CO2) a liquid can
// split into two liquids. This adds: (1) ln phi at the lower-Gibbs root,
// (2) a Michelsen tangent-plane stability test to detect a split and seed the
// incipient phase, and (3) a multiphase Rachford-Rice + successive-substitution
// flash for three phases (reference = liquid L1; unknowns beta_V, beta_L2).
// ============================================================================

// ln phi of composition w at its lower-Gibbs (stable) volume root.
static bool lnphi_stable(double T, double P, const NumericVector& w,
    const NumericVector& d, const NumericVector& q, const NumericMatrix& nu,
    const NumericMatrix& g, const NumericMatrix& dg, const NumericMatrix& alpha, double R,
    std::vector<double>& lp, const Nullable<NumericMatrix>& numu = R_NilValue,
    const Nullable<NumericMatrix>& Delta0 = R_NilValue) {
  int NC=w.size();
  double VL=gceos_vroot(T,P,w,d,q,nu,g,dg,alpha,R,0,numu,Delta0);
  double VV=gceos_vroot(T,P,w,d,q,nu,g,dg,alpha,R,1,numu,Delta0);
  bool okL=!NumericVector::is_na(VL), okV=!NumericVector::is_na(VV);
  if(!okL&&!okV) return false;
  if(okL&&okV&&std::fabs(VL-VV)>1e-6*std::max(VL,VV)){
    NumericVector lpL=gceos_lnphi(T,VL,w,d,q,nu,g,dg,alpha,R,numu,Delta0);
    NumericVector lpV=gceos_lnphi(T,VV,w,d,q,nu,g,dg,alpha,R,numu,Delta0);
    double GL=0,GV=0; for(int i=0;i<NC;i++){ if(w[i]>0){ GL+=w[i]*(std::log(w[i])+lpL[i]); GV+=w[i]*(std::log(w[i])+lpV[i]); } }
    lp.assign(NC,0.0); for(int i=0;i<NC;i++) lp[i]=(GL<=GV)?lpL[i]:lpV[i];
  } else {
    double V=okL?VL:VV; NumericVector l=gceos_lnphi(T,V,w,d,q,nu,g,dg,alpha,R,numu,Delta0);
    lp.assign(NC,0.0); for(int i=0;i<NC;i++) lp[i]=l[i];
  }
  return true;
}

// Michelsen TPD stability test of composition w0. Returns true if UNSTABLE and
// fills wtrial with the incipient-phase composition (to seed a split).
static bool tpd_test(double T, double P, const NumericVector& w0,
    const NumericVector& d, const NumericVector& q, const NumericMatrix& nu,
    const NumericMatrix& g, const NumericMatrix& dg, const NumericMatrix& alpha, double R,
    NumericVector& wtrial, const Nullable<NumericMatrix>& numu = R_NilValue,
    const Nullable<NumericMatrix>& Delta0 = R_NilValue) {
  int NC=w0.size();
  std::vector<double> lp0; if(!lnphi_stable(T,P,w0,d,q,nu,g,dg,alpha,R,lp0,numu,Delta0)) return false;
  std::vector<double> di(NC); for(int i=0;i<NC;i++) di[i]=std::log(std::max(w0[i],1e-300))+lp0[i];
  bool unstable=false; double best=0;
  // build trial-phase initialisations: each component-biased, PLUS the feed's
  // own liquid/vapor two-root K (probes a VLE split, which the lower-Gibbs trials
  // can miss for a single-root supercritical bulk).
  std::vector< std::vector<double> > inits;
  for(int t=0;t<NC;t++){ std::vector<double> W(NC); for(int i=0;i<NC;i++) W[i]=(i==t)?0.9:0.1/std::max(NC-1,1); inits.push_back(W); }
  {
    double VL=gceos_vroot(T,P,w0,d,q,nu,g,dg,alpha,R,0,numu,Delta0);
    double VV=gceos_vroot(T,P,w0,d,q,nu,g,dg,alpha,R,1,numu,Delta0);
    if(!NumericVector::is_na(VL) && !NumericVector::is_na(VV)){
      NumericVector lpL=gceos_lnphi(T,VL,w0,d,q,nu,g,dg,alpha,R,numu,Delta0);
      NumericVector lpV=gceos_lnphi(T,VV,w0,d,q,nu,g,dg,alpha,R,numu,Delta0);
      std::vector<double> Wu(NC),Wd(NC);
      for(int i=0;i<NC;i++){ double K=std::exp(lpL[i]-lpV[i]); Wu[i]=w0[i]*K; Wd[i]=w0[i]/K; }
      inits.push_back(Wu); inits.push_back(Wd);
    }
  }
  for(size_t ti=0; ti<inits.size(); ti++){
    std::vector<double> W = inits[ti];
    for(int it=0; it<80; it++){
      double sw=0; for(int i=0;i<NC;i++) sw+=W[i];
      NumericVector w(NC); for(int i=0;i<NC;i++) w[i]=W[i]/sw;
      std::vector<double> lpw; if(!lnphi_stable(T,P,w,d,q,nu,g,dg,alpha,R,lpw,numu,Delta0)) break;
      double err=0; for(int i=0;i<NC;i++){ double Wn=std::exp(di[i]-lpw[i]); err+=std::fabs(Wn-W[i]); W[i]=Wn; }
      if(err<1e-11) break;
    }
    double sw=0; for(int i=0;i<NC;i++) sw+=W[i];
    if(sw>1.0+1e-5 && sw-1.0>best){ best=sw-1.0; unstable=true; for(int i=0;i<NC;i++) wtrial[i]=W[i]/sw; }
  }
  return unstable;
}

// General 2-phase split using lower-Gibbs roots for BOTH phases (so it finds a
// liquid-liquid split, which the fixed liquid/vapor-root VLE flash cannot).
// Successive substitution from an initial K. Returns 1 two-phase, 0 single/trivial.
static int gen2phase(double T, double P, const NumericVector& z,
    const NumericVector& d, const NumericVector& q, const NumericMatrix& nu,
    const NumericMatrix& g, const NumericMatrix& dg, const NumericMatrix& alpha, double R,
    NumericVector& K, NumericVector& xa, NumericVector& xb, double& beta, int maxit,
    const Nullable<NumericMatrix>& numu, const Nullable<NumericMatrix>& Delta0) {
  int NC=z.size();
  for(int it=0; it<maxit; it++){
    bool tp; beta=rr_beta(z,K,tp); if(!tp) return 0;
    double sa=0,sb=0;
    for(int i=0;i<NC;i++){ double t=K[i]-1.0; xa[i]=z[i]/(1.0+beta*t); xb[i]=K[i]*xa[i]; sa+=xa[i]; sb+=xb[i]; }
    for(int i=0;i<NC;i++){ xa[i]/=sa; xb[i]/=sb; }
    std::vector<double> lpa,lpb;
    if(!lnphi_stable(T,P,xa,d,q,nu,g,dg,alpha,R,lpa,numu,Delta0)) return -1;
    if(!lnphi_stable(T,P,xb,d,q,nu,g,dg,alpha,R,lpb,numu,Delta0)) return -1;
    double err=0,triv=0;
    for(int i=0;i<NC;i++){ double lnKn=lpa[i]-lpb[i]; err+=std::fabs(lnKn-std::log(K[i])); triv+=std::fabs(lnKn); K[i]=std::exp(lnKn); }
    if(triv<1e-4) return 0;
    if(err<1e-11) return 1;
  }
  return 1;
}

// Multiphase Rachford-Rice for 3 phases (ref = L1). 2D Newton for beta_V, beta_L2
// given KV (=y/x1) and KL2 (=x2/x1). Fills x1; returns false on singular system.
static bool rr3(const NumericVector& z, const NumericVector& KV, const NumericVector& KL2,
                double& bV, double& bL2, NumericVector& x1) {
  int NC=z.size(); bV=0.34; bL2=0.33;
  for(int it=0; it<200; it++){
    double gV=0,gL2=0,JVV=0,JVL=0,JLL=0;
    for(int i=0;i<NC;i++){
      double aV=KV[i]-1.0, aL=KL2[i]-1.0;
      double D=1.0+bV*aV+bL2*aL; if(D<1e-12) D=1e-12;
      double xi=z[i]/D; gV+=aV*xi; gL2+=aL*xi;
      double f=xi/D; JVV-=aV*aV*f; JVL-=aV*aL*f; JLL-=aL*aL*f;
    }
    double det=JVV*JLL-JVL*JVL; if(std::fabs(det)<1e-300) return false;
    double dbV=(-gV*JLL+gL2*JVL)/det, dbL2=(-gL2*JVV+gV*JVL)/det;
    double lam=1.0;                                  // keep all three phase fractions feasible
    // limit step so betas stay >= -0.05 and bV+bL2 <= 1.05
    if(bV+lam*dbV<-0.05) lam=std::min(lam,(-0.05-bV)/dbV);
    if(bL2+lam*dbL2<-0.05) lam=std::min(lam,(-0.05-bL2)/dbL2);
    bV+=lam*dbV; bL2+=lam*dbL2;
    if(std::fabs(dbV)+std::fabs(dbL2)<1e-12) break;
  }
  for(int i=0;i<NC;i++){ double D=1.0+bV*(KV[i]-1.0)+bL2*(KL2[i]-1.0); x1[i]=z[i]/std::max(D,1e-12); }
  return true;
}

//' GC(A)-EOS three-phase (vapor-liquid-liquid) flash. Runs a 2-phase flash, tests
//' the liquid for a split via the tangent-plane criterion, and if unstable solves
//' the three-phase equilibrium. status: 3 three-phase, 2 two-phase, 1 single, -1 fail.
//' @keywords internal
// [[Rcpp::export]]
List gceos_flash3(double T, double P, NumericVector z,
    NumericVector d, NumericVector q, NumericMatrix nu, NumericMatrix g,
    NumericMatrix dg, NumericMatrix alpha, double R,
    int maxit = 200, Nullable<NumericMatrix> numu = R_NilValue,
    Nullable<NumericMatrix> Delta0 = R_NilValue) {
  int NC=z.size();
  NumericVector zz=clone(z); double zs=0; for(int i=0;i<NC;i++) zs+=zz[i]; for(int i=0;i<NC;i++) zz[i]/=zs;
  // 1) robust 2-phase VLE flash (its own complete algorithm).
  List f2 = gceos_flash_cpp(T,P,zz,d,q,nu,g,dg,alpha,R,R_NilValue,maxit,1e-4,numu,Delta0);
  int st2=f2["status"];
  NumericVector x1(NC), y(NC); double beta=0.5;
  if(st2==1){
    x1=clone(as<NumericVector>(f2["x"])); y=clone(as<NumericVector>(f2["y"])); beta=f2["beta"];
  } else {
    // VLE found nothing; check for a liquid-liquid split the VLE flash can't see.
    NumericVector wtr(NC);
    if(!tpd_test(T,P,zz,d,q,nu,g,dg,alpha,R,wtr,numu,Delta0))
      return List::create(_["status"]=1,_["nphase"]=1);          // genuinely single phase
    // K = (wtr/z)^2: the bare ratio gives Rachford-Rice g(0)=0 (sum wtr = sum z),
    // squaring pushes into the unstable region so a real split is found.
    NumericVector Kg(NC); for(int i=0;i<NC;i++){ double r=wtr[i]/std::max((double)zz[i],1e-300); Kg[i]=r*r; }
    int s2=gen2phase(T,P,zz,d,q,nu,g,dg,alpha,R,Kg,x1,y,beta,maxit,numu,Delta0);
    if(s2!=1) return List::create(_["status"]=1,_["nphase"]=1);
  }
  // 2) stability test on the liquid x1
  NumericVector wtr(NC);
  bool split = tpd_test(T,P,x1,d,q,nu,g,dg,alpha,R,wtr,numu,Delta0);
  if(!split)
    return List::create(_["status"]=2,_["nphase"]=2,_["beta"]=beta,_["x"]=x1,_["y"]=y);
  // 3) three-phase SS flash. ref=L1=x1; L2 seeded from the incipient phase wtr; V from y.
  std::vector<double> lp1,lpV,lp2;
  NumericVector KV(NC), KL2(NC), x2=clone(wtr);
  // initial K from the current phase estimates
  if(!lnphi_stable(T,P,x1,d,q,nu,g,dg,alpha,R,lp1,numu,Delta0)) return List::create(_["status"]=-1);
  if(!lnphi_stable(T,P,y, d,q,nu,g,dg,alpha,R,lpV,numu,Delta0)) return List::create(_["status"]=-1);
  if(!lnphi_stable(T,P,x2,d,q,nu,g,dg,alpha,R,lp2,numu,Delta0)) return List::create(_["status"]=-1);
  for(int i=0;i<NC;i++){ KV[i]=std::exp(lp1[i]-lpV[i]); KL2[i]=std::exp(lp1[i]-lp2[i]); }
  double bV=beta, bL2=0.1; int status=3;
  for(int it=0; it<maxit; it++){
    if(!rr3(zz,KV,KL2,bV,bL2,x1)){ status=-1; break; }
    double bL1=1.0-bV-bL2;
    if(bV< -1e-3 || bL2< -1e-3 || bL1< -1e-3){ status=2; break; }   // not a real 3-phase split
    // phase compositions
    double sx1=0,sy=0,sx2=0;
    for(int i=0;i<NC;i++){ y[i]=KV[i]*x1[i]; x2[i]=KL2[i]*x1[i]; sx1+=x1[i]; sy+=y[i]; sx2+=x2[i]; }
    for(int i=0;i<NC;i++){ x1[i]/=sx1; y[i]/=sy; x2[i]/=sx2; }
    if(!lnphi_stable(T,P,x1,d,q,nu,g,dg,alpha,R,lp1,numu,Delta0)){status=-1;break;}
    if(!lnphi_stable(T,P,y, d,q,nu,g,dg,alpha,R,lpV,numu,Delta0)){status=-1;break;}
    if(!lnphi_stable(T,P,x2,d,q,nu,g,dg,alpha,R,lp2,numu,Delta0)){status=-1;break;}
    double err=0, trivL=0;
    for(int i=0;i<NC;i++){
      double KVn=std::exp(lp1[i]-lpV[i]), KL2n=std::exp(lp1[i]-lp2[i]);
      err+=std::fabs(std::log(KVn/KV[i]))+std::fabs(std::log(KL2n/KL2[i]));
      trivL+=std::fabs(std::log(KL2n)); KV[i]=KVn; KL2[i]=KL2n;
    }
    if(trivL<1e-4){ status=2; break; }               // L2 == L1 -> only two phases
    if(err<1e-10) break;
  }
  if(status!=3)
    return List::create(_["status"]=2,_["nphase"]=2,_["beta"]=beta,_["x"]=x1,_["y"]=y);
  double bL1=1.0-bV-bL2;
  return List::create(_["status"]=3,_["nphase"]=3,_["beta_V"]=bV,_["beta_L1"]=bL1,_["beta_L2"]=bL2,
                      _["y"]=y,_["x1"]=x1,_["x2"]=x2);
}

// ============================================================================
// Naphtali-Sandholm simultaneous-correction column solver.
//
// Isothermal/isobaric equilibrium-stage cascade (no energy balance). Unknowns
// are the component liquid/vapor flows l[j,i], v[j,i]; equations per (j,i) are
// the component material balance M and the equilibrium relation E. The full
// system is solved by a block-tridiagonal Newton (blocks 2*nc, block-Thomas),
// so the flows and equilibrium converge TOGETHER and cannot diverge the way the
// sequential sum-rates cascade does at large stripping factors. K is frozen
// within each Newton step and refreshed each iteration from the robust flash;
// reflux is a damped recycle l0 = refl_frac * v_1 (organics only).
// ============================================================================

// Dense solve A(n x n) X = B(n x k), Gauss-Jordan w/ partial pivot, in place
// (solution returned in B). Row-major. Returns false if (near-)singular.
static bool gesv(int n, int k, std::vector<double>& A, std::vector<double>& B) {
  for (int col=0; col<n; col++) {
    int piv=col; double mx=std::fabs(A[col*n+col]);
    for (int r=col+1;r<n;r++){ double v=std::fabs(A[r*n+col]); if(v>mx){mx=v;piv=r;} }
    if (mx < 1e-300) return false;
    if (piv!=col){ for(int c=0;c<n;c++) std::swap(A[col*n+c],A[piv*n+c]);
                   for(int c=0;c<k;c++) std::swap(B[col*k+c],B[piv*k+c]); }
    double dgl=A[col*n+col];
    for (int r=0;r<n;r++){ if(r==col) continue; double fac=A[r*n+col]/dgl;
      if (fac==0.0) continue;
      for(int c=col;c<n;c++) A[r*n+c]-=fac*A[col*n+c];
      for(int c=0;c<k;c++) B[r*k+c]-=fac*B[col*k+c];
    }
  }
  for (int col=0; col<n; col++){ double dgl=A[col*n+col]; for(int c=0;c<k;c++) B[col*k+c]/=dgl; }
  return true;
}

//' GC-EOS Naphtali-Sandholm column solver (isothermal, isobaric). Inputs are the
//' external component feed flows f(j,i), the initial component flows l_in/v_in,
//' the solvent index sidx0 (0-based, -1 if none) and reflux ratio. Returns the
//' converged l, v, K matrices plus status/iters/residual.
//' @noRd
// [[Rcpp::export]]
List gceos_column_ns(int N, NumericVector Tvec, double P, NumericMatrix f,
    NumericMatrix l_in, NumericMatrix v_in,
    NumericMatrix dmat, NumericVector q, NumericMatrix nu, List glist,
    List dglist, NumericMatrix alpha, double R, int sidx0, double reflux,
    int max_iter = 200, double tol = 1e-8, double refl_damp = 0.1) {
  const int nc = f.ncol();
  const int m  = 2*nc;
  // state component flows
  std::vector< std::vector<double> > l(N, std::vector<double>(nc)),
                                     v(N, std::vector<double>(nc)),
                                     K(N, std::vector<double>(nc,1.0));
  std::vector<double> l0(nc, 0.0);                 // reflux liquid into stage 1
  double Fref = 0.0;
  for (int j=0;j<N;j++) for (int i=0;i<nc;i++){ l[j][i]=l_in(j,i); v[j][i]=v_in(j,i); Fref+=f(j,i); }
  if (Fref <= 0) Fref = 1.0;
  const double refl_frac = reflux/(1.0+reflux);
  const double eps = 1e-30;

  // block storage
  std::vector< std::vector<double> > A(N, std::vector<double>(m*m,0.0)),
                                     B(N, std::vector<double>(m*m,0.0)),
                                     C(N, std::vector<double>(m*m,0.0)),
                                     Gam(N, std::vector<double>(m*m,0.0));
  std::vector< std::vector<double> > rho(N, std::vector<double>(m,0.0)),
                                     F(N, std::vector<double>(m,0.0));
  // composition derivatives of K: dKL[j][i*nc+k] = d ln(phi^L_i)/d l_k,
  //                               dKV[j][i*nc+k] = d ln(phi^V_i)/d v_k.
  std::vector< std::vector<double> > dKL(N, std::vector<double>(nc*nc,0.0)),
                                     dKV(N, std::vector<double>(nc*nc,0.0));
  std::vector<double> VLc(N,-1.0), VVc(N,-1.0);   // warm-start volume cache per stage
  std::string status = "max_iter"; int it_done=0; double resid=0;

  for (int iter=0; iter<max_iter; iter++) {
    // --- stage compositions, K (robust flash, frozen in the step) ---
    const double fl_floor = 1e-12*(Fref);            // keep 1/V, K/L finite (no singular blocks)
    std::vector<double> L(N), Vt(N);
    for (int j=0;j<N;j++){
      double Ls=0,Vs=0; for(int i=0;i<nc;i++){ Ls+=l[j][i]; Vs+=v[j][i]; }
      L[j]=std::max(Ls,fl_floor); Vt[j]=std::max(Vs,fl_floor);
    }
    for (int j=0;j<N;j++){
      // per-stage T-dependent inputs (supports a column temperature profile)
      double Tj = Tvec[j];
      NumericVector dj = dmat(j,_);
      NumericMatrix gj  = as<NumericMatrix>(glist[j]);
      NumericMatrix dgj = as<NumericMatrix>(dglist[j]);
      NumericVector xj(nc), yj(nc);
      for(int i=0;i<nc;i++){ xj[i]=l[j][i]/L[j]; yj[i]=v[j][i]/Vt[j]; }
      std::fill(dKL[j].begin(),dKL[j].end(),0.0);
      std::fill(dKV[j].begin(),dKV[j].end(),0.0);
      double VL = gceos_vroot(Tj,P,xj,dj,q,nu,gj,dgj,alpha,R,0, R_NilValue,R_NilValue, VLc[j]);
      double VV = gceos_vroot(Tj,P,yj,dj,q,nu,gj,dgj,alpha,R,1, R_NilValue,R_NilValue, VVc[j]);
      if(!NumericVector::is_na(VL)) VLc[j]=VL;
      if(!NumericVector::is_na(VV)) VVc[j]=VV;
      bool okroots = !NumericVector::is_na(VL) && !NumericVector::is_na(VV);
      bool triv = false;
      if (okroots) {
        NumericVector lpL0 = gceos_lnphi(Tj,VL,xj,dj,q,nu,gj,dgj,alpha,R);
        NumericVector lpV0 = gceos_lnphi(Tj,VV,yj,dj,q,nu,gj,dgj,alpha,R);
        double tr=0;
        for(int i=0;i<nc;i++){ double Ki=std::exp(lpL0[i]-lpV0[i]); if(std::isfinite(Ki)&&Ki>0) K[j][i]=Ki; tr+=std::fabs(lpL0[i]-lpV0[i]); }
        if (tr < 1e-2) triv = true;
        // d ln(phi^L_i)/d l_k  (forward difference; perturb a liquid flow)
        for(int k=0;k<nc;k++){
          double h=1e-6*L[j]+1e-13, Lp=0; NumericVector xp(nc);
          for(int i=0;i<nc;i++) xp[i]=l[j][i];
          xp[k]+=h; for(int i=0;i<nc;i++) Lp+=xp[i];
          for(int i=0;i<nc;i++) xp[i]/=Lp;
          double VLp=gceos_vroot(Tj,P,xp,dj,q,nu,gj,dgj,alpha,R,0, R_NilValue,R_NilValue, VL);
          if(!NumericVector::is_na(VLp)){ NumericVector lpp=gceos_lnphi(Tj,VLp,xp,dj,q,nu,gj,dgj,alpha,R);
            for(int i=0;i<nc;i++) dKL[j][i*nc+k]=(lpp[i]-lpL0[i])/h; }
        }
        // d ln(phi^V_i)/d v_k  (perturb a vapor flow)
        for(int k=0;k<nc;k++){
          double h=1e-6*Vt[j]+1e-13, Vp=0; NumericVector yp(nc);
          for(int i=0;i<nc;i++) yp[i]=v[j][i];
          yp[k]+=h; for(int i=0;i<nc;i++) Vp+=yp[i];
          for(int i=0;i<nc;i++) yp[i]/=Vp;
          double VVp=gceos_vroot(Tj,P,yp,dj,q,nu,gj,dgj,alpha,R,1, R_NilValue,R_NilValue, VV);
          if(!NumericVector::is_na(VVp)){ NumericVector lpp=gceos_lnphi(Tj,VVp,yp,dj,q,nu,gj,dgj,alpha,R);
            for(int i=0;i<nc;i++) dKV[j][i*nc+k]=(lpp[i]-lpV0[i])/h; }
        }
      }
      if (!okroots || triv) {                 // root failure / trivial -> robust flash, K frozen (dK=0)
        NumericVector zj(nc), Kprev(nc);
        for(int i=0;i<nc;i++){ zj[i]=l[j][i]+v[j][i]; Kprev[i]=K[j][i]; }
        NumericVector Kfl = gceos_stage_flashK(Tj,P,zj,Kprev,dj,q,nu,gj,dgj,alpha,R,4,1e-4);
        if (!NumericVector::is_na(Kfl[0])) for(int i=0;i<nc;i++) if(std::isfinite(Kfl[i])&&Kfl[i]>0) K[j][i]=Kfl[i];
        std::fill(dKL[j].begin(),dKL[j].end(),0.0);
        std::fill(dKV[j].begin(),dKV[j].end(),0.0);
      }
    }

    // --- reflux is folded into the Newton via the stage-1 balance below
    //     (l0 = refl_frac * v_1 for organics); kept synced here for reporting ---
    if (reflux > 0) for (int i=0;i<nc;i++) l0[i] = (i==sidx0) ? 0.0 : refl_frac * v[0][i];

    // --- assemble residual F and blocks A,B,C ---
    resid = 0;
    for (int j=0;j<N;j++){
      std::fill(A[j].begin(),A[j].end(),0.0);
      std::fill(B[j].begin(),B[j].end(),0.0);
      std::fill(C[j].begin(),C[j].end(),0.0);
      double Lj=L[j], Vj=Vt[j];
      for (int i=0;i<nc;i++){
        // liquid from above: at the top stage it is the reflux refl_frac*v_1
        // (organics), making the reflux IMPLICIT in the Newton (no lagging recycle)
        bool top_refl = (j==0 && reflux>0 && i!=sidx0);
        double lup  = (j==0) ? (top_refl ? refl_frac*v[0][i] : 0.0) : l[j-1][i];
        double vdn  = (j==N-1) ? 0.0        : v[j+1][i];   // vapor from stage below
        double Mres = (l[j][i] + v[j][i] - lup - vdn - f(j,i)) / Fref;
        double xji = l[j][i]/Lj, yji = v[j][i]/Vj;
        double Eres = K[j][i]*xji - yji;
        F[j][i]    = Mres;
        F[j][nc+i] = Eres;
        if (std::fabs(Mres)>resid) resid=std::fabs(Mres);
        if (std::fabs(Eres)>resid) resid=std::fabs(Eres);
        // B block (d F_j / d u_j)
        B[j][(i)*m + (i)]       += 1.0/Fref;          // dM_i/dl_i
        B[j][(i)*m + (nc+i)]    += (top_refl ? (1.0-refl_frac) : 1.0)/Fref;  // dM_i/dv_i (reflux folded in at top)
        double Ki = K[j][i];
        for (int k2=0;k2<nc;k2++){
          double dik = (i==k2)?1.0:0.0;
          // E_i = K_i x_i - y_i, with K_i = K_i(x,y):
          //  dE_i/dl_k = K_i (dik - x_i)/L + x_i K_i  dlnphiL_i/dl_k
          //  dE_i/dv_k = -(dik - y_i)/V   - x_i K_i  dlnphiV_i/dv_k
          B[j][(nc+i)*m + (k2)]    += Ki*(dik - xji)/Lj + xji*Ki*dKL[j][i*nc+k2];
          B[j][(nc+i)*m + (nc+k2)] += -(dik - yji)/Vj   - xji*Ki*dKV[j][i*nc+k2];
        }
        // A block (d F_j / d u_{j-1}): only M wrt l_{j-1,i}
        if (j>0) A[j][(i)*m + (i)] = -1.0/Fref;
        // C block (d F_j / d u_{j+1}): only M wrt v_{j+1,i}
        if (j<N-1) C[j][(i)*m + (nc+i)] = -1.0/Fref;
      }
      // mild Levenberg regularization: keep blocks invertible when an E-row
      // degenerates (a stage vapor goes ~pure solvent, y->1) or flows collapse.
      for (int r=0;r<m;r++){ double s=std::fabs(B[j][r*m+r]); B[j][r*m+r] += 1e-6*(s + 1.0/Fref); }
    }

    if (resid < tol) { status="converged"; it_done=iter; break; }

    // --- block-Thomas solve  J du = -F  ---
    // forward sweep
    bool ok=true;
    {
      // Gam_0 = B_0^{-1} C_0 ; rho_0 = B_0^{-1} (-F_0)
      std::vector<double> Bb=B[0], RHS(m*m + m);
      // pack [C_0 | -F_0] as m x (m+1)
      std::vector<double> aug(m*(m+1));
      for(int r=0;r<m;r++){ for(int c=0;c<m;c++) aug[r*(m+1)+c]=C[0][r*m+c]; aug[r*(m+1)+m]=-F[0][r]; }
      if(!gesv(m, m+1, Bb, aug)){ ok=false; }
      else { for(int r=0;r<m;r++){ for(int c=0;c<m;c++) Gam[0][r*m+c]=aug[r*(m+1)+c]; rho[0][r]=aug[r*(m+1)+m]; } }
      for (int j=1; j<N && ok; j++){
        // Mj = B_j - A_j Gam_{j-1}
        std::vector<double> Mj=B[j];
        for(int r=0;r<m;r++) for(int c=0;c<m;c++){ double s=0; for(int t=0;t<m;t++) s+=A[j][r*m+t]*Gam[j-1][t*m+c]; Mj[r*m+c]-=s; }
        // rhs = [C_j | (-F_j - A_j rho_{j-1})]
        std::vector<double> aug2(m*(m+1));
        for(int r=0;r<m;r++){ double s=0; for(int t=0;t<m;t++) s+=A[j][r*m+t]*rho[j-1][t];
          for(int c=0;c<m;c++) aug2[r*(m+1)+c]= (j<N-1)? C[j][r*m+c] : 0.0;
          aug2[r*(m+1)+m]= -F[j][r] - s; }
        if(!gesv(m, m+1, Mj, aug2)){ ok=false; break; }
        for(int r=0;r<m;r++){ for(int c=0;c<m;c++) Gam[j][r*m+c]=aug2[r*(m+1)+c]; rho[j][r]=aug2[r*(m+1)+m]; }
      }
    }
    if(!ok){ status="singular"; it_done=iter; break; }
    // back substitution -> du stored in rho
    std::vector< std::vector<double> > du(N, std::vector<double>(m));
    for(int r=0;r<m;r++) du[N-1][r]=rho[N-1][r];
    for(int j=N-2;j>=0;j--) for(int r=0;r<m;r++){ double s=0; for(int c=0;c<m;c++) s+=Gam[j][r*m+c]*du[j+1][c]; du[j][r]=rho[j][r]-s; }

    // --- fraction-to-boundary damped update (keep flows > 0) ---
    double lam=1.0;
    for(int j=0;j<N;j++) for(int i=0;i<nc;i++){
      double dl=du[j][i], dv=du[j][nc+i];
      if(dl<0) lam=std::min(lam, -0.9*l[j][i]/dl);
      if(dv<0) lam=std::min(lam, -0.9*v[j][i]/dv);
    }
    if(lam<1e-6) lam=1e-6;
    for(int j=0;j<N;j++) for(int i=0;i<nc;i++){
      l[j][i]=std::max(l[j][i]+lam*du[j][i],   eps);
      v[j][i]=std::max(v[j][i]+lam*du[j][nc+i],eps);
    }
    it_done=iter+1;
  }

  // outputs
  NumericMatrix lo(N,nc), vo(N,nc), Ko(N,nc);
  for(int j=0;j<N;j++) for(int i=0;i<nc;i++){ lo(j,i)=l[j][i]; vo(j,i)=v[j][i]; Ko(j,i)=K[j][i]; }
  return List::create(_["l"]=lo, _["v"]=vo, _["K"]=Ko, _["l0"]=NumericVector(l0.begin(),l0.end()),
                      _["status"]=status, _["iters"]=it_done, _["resid"]=resid);
}
