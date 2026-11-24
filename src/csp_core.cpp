// COSMO-SAC-Phi (CSP) lattice-fluid EoS core — Rcpp port of the validated Python
// type-aware segment engine (scratch_tq/csphi_hb.py), faithful to JCOSMO COSMOSAC2_GHB.
//
//   P = KB_PV*T/(v-b) - mu_h*KCAL2PV/bh        (repulsive hard-sphere + COSMO attractive via holes)
//   lnphi_i = [-ln(1-b/v)+b_i/(v-B)] + (mu_i - mu_h*b_i/bh)/RT
//             - (mu_h/RT)*n_h*(bh_i/bh - 1) - lnZ
//
// Solid solubility via de Souza (2022) Eq.16 fusion route:
//   y2 = exp[fus + poy] / gamma,   fus=(dHfus/R)(1/Tm-1/T),  poy=(v_l - v_s)(Patm - P)/RT
// or the optional sublimation route:
//   y2 = (Psub/P) exp[Vs(P-Psub)/RT] / phi2.
//
// Segments are concatenated on the R side; a single hole segment is appended here. The psi
// (exchange) matrix depends only on (segments, T) so it is built once per context and the
// pressure P(v) is then a cheap function of the volume — used by the root solvers.
// Units: per-molecule; b,v in A^3; energies kcal/mol; P in bar.
// Validated vs JCOSMO: naphthalene/2-naphthol/benzoic/cholesterol lnGamma to 0.3-0.6%.

#include <Rcpp.h>
#include <vector>
#include <cmath>
using namespace Rcpp;

static const double A0        = 8437.408436894386;
static const double R_KCAL    = 0.001987204;
static const double R_J       = 8.314462;
static const double PATM      = 1.01325;            // bar
static const double CC2M3     = 1e-6;
static const double KB_PV     = 138.0649;
static const double KCAL2PV   = 69476.95457055375;
static const double HOLE_AREA = 12.566370614359172;
static const double A_EFF_MU  = 4.203091223272629;
static const double SIGMAHB   = 0.0077004;
static const double CHB[8] = {15020.482268714331,14171.015505397998,9327.21165335225,
                              14171.015505397998,14171.015505397998,6866.667164857075,
                              4642.642078563898,14171.015505397998};
static inline double E_of(int idx){ return (idx>=0 && idx<8) ? CHB[idx] : 0.0; }
static inline int getCHBType(int n2,int n3){
  n2&=0xFF; n3&=0xFF;
  if(n2==0 || n3==0) return -1;
  if(n2==20) return 12;
  if(n2==6){ switch(n3){case 6:return 0;case 3:return 1;case 4:return 2;case 5:return 6;
                        case 7:return 8;case 8:return 8;case 9:return 8;case 10:return 10;default:return -1;} }
  if(n2==1){ switch(n3){case 6:return 3;case 3:return 4;case 4:return 5;case 5:return 7;
                        case 7:return 9;case 8:return 9;case 9:return 9;case 10:return 11;default:return -1;} }
  return -1;
}

// [[Rcpp::export]]
double csp_disp_of(NumericVector par, double T){
  double d0=par[2], dT=par[3];
  if(par.size()>4) return d0*std::exp(1.0-std::pow(T/dT,par[4]));
  return d0*(1.0-std::exp(-dT/T));
}

// COSMOSPACE segment-activity solve (damped successive substitution). `lnG` is IN/OUT: pass the
// converged lnG from a nearby volume/composition as a WARM START to cut the iteration count; pass an
// empty (or wrong-size) vector for a cold start. The fixed point is a contraction, so the converged
// result is independent of the seed -- warm-starting changes only the speed, not the answer.
static void seg_gamma(const std::vector<double>& area, const std::vector<double>& psi, int n,
                      std::vector<double>& lnG, int maxit=500, double tol=1e-10){
  double asum=0.0; for(int i=0;i<n;++i) asum+=area[i];
  std::vector<double> p(n), nw(n), w(n);
  for(int i=0;i<n;++i) p[i]=area[i]/asum;
  if((int)lnG.size()!=n) lnG.assign(n,0.0);
  for(int it=0;it<maxit;++it){
    for(int j=0;j<n;++j) w[j]=p[j]*std::exp(lnG[j]);
    double mx=0.0;
    for(int i=0;i<n;++i){
      const double* prow=&psi[(size_t)i*n];
      double s=0.0; for(int j=0;j<n;++j) s+=prow[j]*w[j];
      nw[i]=-std::log(s); double d=std::fabs(nw[i]-lnG[i]); if(d>mx) mx=d;
    }
    if(mx<tol){ lnG=nw; break; }
    for(int i=0;i<n;++i) lnG[i]=0.5*(nw[i]+lnG[i]);
  }
}

// Reusable EoS context: segments (incl hole at index N-1) + psi matrix, built once per (T,profile,par).
struct Ctx {
  int N, nc;
  std::vector<double> sg, dv, ax;   // sigma, dispersion, x-scaled base area (hole ax = HOLE_AREA placeholder)
  std::vector<int> ow;
  std::vector<double> psi;          // N*N
  std::vector<double> x, bi, bhi;
  double b, bh;
  // Persistent COSMOSPACE warm starts (mixture + ideal-gas reference), carried across the many eval()
  // calls of one volume-root solve so each seg_gamma resumes from the last converged segment activities
  // instead of restarting cold. v changes smoothly along the root scan/bisection, so this collapses the
  // inner iteration count. Reset (cleared) whenever a cold start is wanted.
  std::vector<double> wG, wG0;

  // Build from concatenated compound segments (no hole) + per-compound params.
  Ctx(const NumericVector& sig, const NumericVector& area0, const IntegerVector& hbt,
      const IntegerVector& own, const NumericVector& x_, const NumericVector& bi_,
      const NumericVector& bhi_, const NumericVector& dseg, double T){
    int ns=sig.size(); nc=x_.size(); N=ns+1;
    x.assign(x_.begin(),x_.end()); bi.assign(bi_.begin(),bi_.end()); bhi.assign(bhi_.begin(),bhi_.end());
    b=0; bh=0; for(int i=0;i<nc;++i){ b+=x[i]*bi[i]; bh+=x[i]*bhi[i]; }
    sg.resize(N); dv.resize(N); ax.resize(N); ow.resize(N);
    std::vector<int> hb(N);
    for(int k=0;k<ns;++k){ sg[k]=sig[k]; hb[k]=hbt[k]; ow[k]=own[k]; dv[k]=dseg[own[k]]; ax[k]=area0[k]*x[own[k]]; }
    sg[N-1]=0; hb[N-1]=0; ow[N-1]=-1; dv[N-1]=0; ax[N-1]=HOLE_AREA;   // hole base; scaled by n_h at eval
    psi.assign((size_t)N*N,0.0); double RT=R_KCAL*T;
    for(int i=0;i<N;++i) for(int j=i;j<N;++j){
      double dW=A0*sg[i]*sg[j]-std::sqrt(std::max(dv[i]*dv[j],0.0))/2.0;
      double sa=std::max(sg[i],sg[j]), sd=std::min(sg[i],sg[j]);
      double d2=std::max(0.0,sa-SIGMAHB)*std::min(0.0,sd+SIGMAHB);
      if(d2!=0.0){ int ts,tl; if(sg[i]<sg[j]){ts=hb[i];tl=hb[j];}else{ts=hb[j];tl=hb[i];}
        int ct=getCHBType(ts,tl); if(ct>=0) dW+=E_of(ct)*d2; }
      double pv=std::exp(-dW/RT); psi[(size_t)i*N+j]=pv; psi[(size_t)j*N+i]=pv;
    }
  }

  // Pressure (and optional lnphi) at volume v and temperature T. ok=false if invalid root.
  double eval(double v, double T, bool& ok, std::vector<double>* lp){
    double n_h=(v-b)/bh; ok=true;
    if(n_h<=0){ ok=false; return NA_REAL; }
    std::vector<double> area(ax); area[N-1]=HOLE_AREA*n_h;
    seg_gamma(area,psi,N,wG);   const std::vector<double>& lnG=wG;    // warm-started mixture activities
    std::vector<double> aIG(area); for(int k=0;k<N;++k) if(ow[k]>=0) aIG[k]=0.0;
    seg_gamma(aIG,psi,N,wG0);   const std::vector<double>& lnG0=wG0;  // warm-started reference activities
    std::vector<double> mu(nc,0.0); double mu_h=0.0, RT=R_KCAL*T;
    for(int k=0;k<N;++k){ double g=lnG[k]-lnG0[k];
      if(ow[k]>=0) mu[ow[k]]+=area[k]/x[ow[k]]/A_EFF_MU*g; else mu_h+=area[k]/n_h/A_EFF_MU*g; }
    for(int i=0;i<nc;++i){ mu[i]*=RT; } mu_h*=RT;
    double P=KB_PV*T/(v-b)-mu_h*KCAL2PV/bh, Z=P*v/(KB_PV*T);
    if(lp){ lp->assign(nc,NA_REAL);
      if(Z>0) for(int i=0;i<nc;++i) (*lp)[i]=(-std::log(1.0-b/v)+bi[i]/(v-b))+(mu[i]-mu_h*bi[i]/bh)/RT
                                            -(mu_h/RT)*n_h*(bhi[i]/bh-1.0)-std::log(Z); }
    return P;
  }
};

// Volume root: find v in [lo,hi] with P(v)=Pt. last=true -> largest-v crossing (fluid/vapor),
// last=false -> smallest-v crossing (dense liquid). Geometric scan + bisection. (Both the wide
// fluid window [b*1.02,b*5e5] and the narrow liquid window [b*1.02,b*3] use a geometric grid; vs
// a linear liquid grid this shifts y2 by <=1.4e-9 across the 99-compound de Souza set.)
static double vroot(Ctx& ctx,double T,double lo,double hi,double Pt,bool last,int n,double tol,double seed=-1.0){
  bool ok;
  auto bisect=[&](double a,double bb)->double{
    for(int it=0;it<80;++it){ double m=0.5*(a+bb), Pm=ctx.eval(m,T,ok,nullptr)-Pt;
      if(std::fabs(Pm)<tol) return m;
      double Pa=ctx.eval(a,T,ok,nullptr)-Pt; if(Pa*Pm<0) bb=m; else a=m; }
    return 0.5*(a+bb); };
  auto scan=[&](double a0,double b0,int nn)->double{
    std::vector<double> vs(nn), Pv(nn);
    double lr=std::log(a0), step=(std::log(b0)-lr)/(nn-1);
    for(int i=0;i<nn;++i){ vs[i]=std::exp(lr+step*i);
      Pv[i]=ctx.eval(vs[i],T,ok,nullptr); if(!ok) Pv[i]=R_PosInf; }
    int hit=-1;
    if(last){ for(int i=nn-2;i>=0;--i) if((Pv[i]-Pt)*(Pv[i+1]-Pt)<0){ hit=i; break; } }
    else    { for(int i=0;i<nn-1;++i)  if((Pv[i]-Pt)*(Pv[i+1]-Pt)<0){ hit=i; break; } }
    if(hit<0) return NA_REAL;
    return bisect(vs[hit], vs[hit+1]); };
  // WARM start: a tight geometric bracket (18 pts) around a nearby previous root usually brackets the
  // crossing immediately -- the dense/fluid volume root moves smoothly across the T,P grid. On a miss
  // (root moved out of the window) fall back to the full wide scan, so correctness is preserved.
  if(seed>lo && seed<hi){
    double a0=std::max(lo,seed/1.5), b0=std::min(hi,seed*1.5);
    if(b0>a0){ double r=scan(a0,b0,18); if(!ISNA(r)) return r; }
  }
  return scan(lo,hi,n);
}

// lnphi of a pure compound at volume v (single-component Ctx).
static double lnphi_pure(Ctx& ctx,double v,double T){
  bool ok; std::vector<double> lp; ctx.eval(v,T,ok,&lp);
  return (ok && !ISNA(lp[0])) ? lp[0] : NA_REAL;
}
// bisect P(v)=Pt on [a,b] (monotone branch); returns v or NA.
static double bisect_v(Ctx& ctx,double T,double a,double b,double Pt,double tol){
  bool ok; double fa=ctx.eval(a,T,ok,nullptr)-Pt;
  for(int it=0;it<55;++it){ double m=0.5*(a+b), fm=ctx.eval(m,T,ok,nullptr)-Pt;
    if(std::fabs(fm)<tol || (b-a)<tol*std::fabs(m)+1e-12) return m;
    if(fa*fm<0) b=m; else { a=m; fa=fm; } }
  return 0.5*(a+b);
}

// Pure-compound saturation: Psat[bar] (and saturated vL,vV in A^3) at T, where the liquid and
// vapor volume roots have equal fugacity. Port of csphi.py saturate(). Returns c(Psat,vL,vV);
// NA if no two-phase (equal-fugacity) root exists at T (e.g. supercritical / vanished vdW loop).
// [[Rcpp::export]]
NumericVector csp_psat_cpp(NumericVector sig, NumericVector area0, IntegerVector hbt,
                           double bi, double bh, double dseg, double T){
  IntegerVector own(sig.size(), 0);
  NumericVector x=NumericVector::create(1.0), biv=NumericVector::create(bi),
                bhv=NumericVector::create(bh), dsegv=NumericVector::create(dseg);
  Ctx ctx(sig,area0,hbt,own,x,biv,bhv,dsegv,T);
  double b=ctx.b;
  const int n1=60, n2=140; std::vector<double> vs; vs.reserve(n1+n2);
  for(int i=0;i<n1;++i) vs.push_back(b*1.02+(b*2.5-b*1.02)*i/(n1-1));      // linear liquid side
  double lr=std::log(b*2.5), st=(std::log(b*2e5)-lr)/(n2-1);
  for(int i=0;i<n2;++i) vs.push_back(std::exp(lr+st*i));                    // geometric vapor side
  int n=vs.size(); std::vector<double> Pv(n); bool ok;
  for(int i=0;i<n;++i){ Pv[i]=ctx.eval(vs[i],T,ok,nullptr); if(!ok) Pv[i]=R_PosInf; }
  int imin=0; for(int i=1;i<n;++i) if(Pv[i]<Pv[imin]) imin=i;               // loop minimum
  double Phi=0.5; for(int i=imin+1;i<n;++i) if(Pv[i]>Phi) Phi=Pv[i];        // loop maximum
  double Plo=Pv[n-1]*1.05; if(Plo<1e-5) Plo=1e-5;                            // keep vapor root on-grid
  // g(Ptry) = lnphi(vL) - lnphi(vV) ; vL = liquid (first) crossing, vV = vapor (last) crossing
  auto roots=[&](double Pt,double& vL,double& vV)->bool{
    if(!((Pv[0]-Pt)*(Pv[imin]-Pt)<0)) return false;
    vL=bisect_v(ctx,T,vs[0],vs[imin],Pt,1e-7);
    int k=-1; for(int i=imin;i<n-1;++i) if((Pv[i]-Pt)*(Pv[i+1]-Pt)<0) k=i;  // last crossing
    if(k<0) return false;
    vV=bisect_v(ctx,T,vs[k],vs[k+1],Pt,1e-7); return true;
  };
  auto g=[&](double Pt)->double{ double vL,vV; if(!roots(Pt,vL,vV)) return NA_REAL;
    return lnphi_pure(ctx,vL,T)-lnphi_pure(ctx,vV,T); };
  // outer bisection on Psat in [Plo, Phi*0.97]
  double a=Plo, bb=Phi*0.97, ga=g(a);
  if(ISNA(ga)) return NumericVector::create(NA_REAL,NA_REAL,NA_REAL);
  double Psat=NA_REAL;
  for(int it=0;it<60;++it){ double m=0.5*(a+bb), gm=g(m);
    if(ISNA(gm)){ bb=m; continue; }
    if(std::fabs(gm)<1e-9 || (bb-a)<1e-10*m+1e-12){ Psat=m; break; }
    if(ga*gm<0) bb=m; else { a=m; ga=gm; } }
  if(ISNA(Psat)) Psat=0.5*(a+bb);
  double vL,vV; if(!roots(Psat,vL,vV)) return NumericVector::create(NA_REAL,NA_REAL,NA_REAL);
  return NumericVector::create(Psat,vL,vV);
}

// Mixture ln-fugacity coefficients at the liquid (or fluid) volume root for (T, P, x).
// liquid=true -> dense-liquid root (solvent / solid-liquid equilibrium); false -> fluid/vapor root.
// Returns a length-nc vector (NA if the requested root does not exist at these conditions).
// [[Rcpp::export]]
NumericVector csp_lnphi_cpp(NumericVector sig, NumericVector area0, IntegerVector hbt, IntegerVector own,
                            NumericVector x, NumericVector bi, NumericVector bhi, NumericVector dseg,
                            double T, double P, bool liquid){
  Ctx ctx(sig,area0,hbt,own,x,bi,bhi,dseg,T);
  double b=ctx.b; int nc=x.size();
  double v = liquid ? vroot(ctx,T,b*1.02,b*3.0,P,false,80,1e-7)
                    : vroot(ctx,T,b*1.02,b*5e5,P,true,96,1e-9);
  if(ISNA(v)) return NumericVector(nc, NA_REAL);
  bool ok; std::vector<double> lp; ctx.eval(v,T,ok,&lp);
  if(!ok) return NumericVector(nc, NA_REAL);
  return NumericVector(lp.begin(), lp.end());
}

// thin wrapper kept for the engine test (pressure + lnphi at given v)
// [[Rcpp::export]]
List csp_mix_cpp(NumericVector sig, NumericVector area0, IntegerVector hbt, IntegerVector own,
                 NumericVector x, NumericVector bi, NumericVector bhi, NumericVector dseg,
                 double T, double v){
  Ctx ctx(sig,area0,hbt,own,x,bi,bhi,dseg,T);
  bool ok; std::vector<double> lp; double P=ctx.eval(v,T,ok,&lp);
  if(!ok) return List::create(_["P"]=NA_REAL,_["lnphi"]=R_NilValue);
  bool good=true; for(double q:lp) if(ISNA(q)) good=false;
  if(!good) return List::create(_["P"]=P,_["lnphi"]=R_NilValue);
  return List::create(_["P"]=P,_["lnphi"]=NumericVector(lp.begin(),lp.end()));
}

// Solid solubility y2 of `solute` in a CO2-rich fluid at (T[K], P[bar]). The mixture inputs are the
// SOLVENT components (CO2, plus optional co-solvent(s)) followed by the solute, concatenated (own
// 0..nsolv-1 = solvents, nsolv = solute); xsolv holds the solvent-only mole fractions (sum 1), and the
// solute is added at infinite dilution. solute_* are the pure-solute segments. dseg_mix/dseg_sol =
// dispersion d(T) per compound. Fusion route unless sublim is supplied (c(A,B,C) Antoine,
// ln(Psub[Pa])=A-B/(T+C)); dCp=NA_real_ -> Poynting fusion, else the deltaCp SLE. With a single solvent
// (xsolv = 1, own = CO2/solute) this is exactly the original CO2+solute binary.
// [[Rcpp::export]]
NumericVector csp_solubility_cpp(NumericVector sig_m, NumericVector area_m, IntegerVector hbt_m, IntegerVector own_m,
                          NumericVector bi_m, NumericVector bhi_m, NumericVector dseg_m,
                          NumericVector sig_s, NumericVector area_s, IntegerVector hbt_s,
                          NumericVector bi_s, NumericVector bhi_s, NumericVector dseg_s,
                          NumericVector xsolv,
                          double T, double P, double Tm, double dHfus, double vs_solid,
                          double dCp, Nullable<NumericVector> sublim,
                          double v_guess_F = -1.0, double v_guess_L = -1.0){
  // Returns c(y2, vF, vL): the solubility plus the fluid + liquid molar-volume roots, so the caller can
  // thread them back as warm-start seeds (v_guess_F/v_guess_L) for the next grid point (roots move
  // smoothly with T,P), collapsing the wide volume-root scans.
  auto RES=[](double y2,double vF,double vL){ return NumericVector::create(y2,vF,vL); };
  double y=1e-7;
  // --- fluid-phase fugacity coefficient of the solute at infinite dilution in the (CO2 + co-solvent)
  //     solvent. The solvent internal composition xsolv is held fixed; the solute is the last component. ---
  int nsolv = xsolv.size();
  NumericVector xm(nsolv + 1);
  for(int k=0;k<nsolv;++k) xm[k]=xsolv[k]*(1.0-y);
  xm[nsolv]=y;
  IntegerVector own_s_only(sig_s.size(), 0);
  Ctx mix(sig_m,area_m,hbt_m,own_m,xm,bi_m,bhi_m,dseg_m,T);
  double bM=mix.b;
  double vF=vroot(mix,T,bM*1.02,bM*5e5,P,true,96,1e-9,v_guess_F);
  if(ISNA(vF)) return RES(NA_REAL,NA_REAL,NA_REAL);
  bool ok; std::vector<double> lpm; mix.eval(vF,T,ok,&lpm);
  if(ISNA(lpm[nsolv])) return RES(NA_REAL,vF,NA_REAL);
  double phi2=std::exp(lpm[nsolv]);

  // --- sublimation route ---
  if(sublim.isNotNull()){
    NumericVector ab(sublim); double Psub=std::exp(ab[0]-ab[1]/(T+ab[2]));  // Pa
    double Ppa=P*1e5, Vsm=vs_solid*CC2M3;
    return RES((Psub/Ppa)*std::exp(Vsm*(Ppa-Psub)/(R_J*T))/phi2, vF, NA_REAL);
  }
  // --- fusion route (de Souza Eq.16) ---
  Ctx sol(sig_s,area_s,hbt_s,own_s_only,NumericVector::create(1.0),bi_s,bhi_s,dseg_s,T);
  double bS=sol.b;
  double vL=vroot(sol,T,bS*1.02,bS*3.0,P,false,80,1e-7,v_guess_L);
  if(ISNA(vL)) return RES(NA_REAL,vF,NA_REAL);
  std::vector<double> lps; sol.eval(vL,T,ok,&lps);
  if(ISNA(lps[0])) return RES(NA_REAL,vF,vL);
  double f_l=std::exp(lps[0])*P;
  double fus=(dHfus/(R_J*Tm))*(1.0-Tm/T), f_s;
  if(ISNA(dCp)){
    double poy=(vL*0.6022-vs_solid)*CC2M3*(PATM-P)*1e5/(R_J*T);
    f_s=f_l*std::exp(fus+poy);
  } else {
    f_s=f_l*std::exp(fus-(dCp/R_J)*(std::log(Tm/T)-Tm/T+1.0));
  }
  return RES(f_s/(P*phi2), vF, vL);
}
