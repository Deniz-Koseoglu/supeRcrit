// openCOSMO-RS-Phi (Markgraf 2026) — Rcpp binding for supeRcrit.
// The ~500 KB numerical core lives untouched in src/ocrsphi_core/*.hpp (0 pybind refs).
// This file replaces the upstream pybind11 boundary (bindings_forPython.cpp) with an
// Rcpp/RcppEigen one: it mirrors the marshalling using C++-owned buffers Eigen::Map'd into
// the core's calculation structs. No Python. Validated bit-identical to the pybind build
// (methanol Psat 280/298.15/320 K = 6051.9/16774.5/48376.1 Pa).
//
// [[Rcpp::depends(RcppEigen)]]
#include <Rcpp.h>

#define USE_DOUBLE
// FORCE the SCALAR SIMD backend for the openCOSMO-RS-Phi core. The vectorized (AVX2/FMA) segment-
// activity solver in calculateLnGammaResidual has an out-of-bounds tail read that SEGFAULTS for
// molecules/mixtures whose segment count is not a multiple of the SIMD width (e.g. 74 segments for a
// water/dichloromethane liquid) - it only stayed hidden because scCO2 solubility exercised CO2-dilute
// compositions/segment counts that happened to align. The scalar path is bounds-safe and numerically
// identical (<0.1 Pa on the methanol Psat validation). Defined BEFORE general.hpp so its arch-macro
// auto-select (defined(__FMA__) etc.) is skipped for THIS translation unit only; the rest of the
// package (csac/csp/gceos) keeps -mavx2/-mfma. Remove once the upstream SIMD tail is padded/masked.
#define OPENCOMSORS_SIMD_SCALAR 1
#include "ocrsphi_core/general.hpp"
#include "ocrsphi_core/core_functions.hpp"

using namespace Rcpp;
using OcrsMapMat = Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>>;
using OcrsMapVec = Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, 1>>;

// The dangling-Map-after-copy problem (loadSubcalculation's maps point at the temporary's owned
// _data; `calculation` has no copy ctor) is fixed centrally by calculation::fixupMapsAfterCopy()
// in ocrsphi_core/types.hpp, called at every copy site in the core and here after building subcalcs.

// Set the global model parameters (the fixed openCOSMO-RS-Phi parametrization, Markgraf 2026).
// Mirrors loadParametersOnPython for a pure-component Phi run (sw_combTerm==3, sw_misfit==0).
static void ocrsphi_set_global_parameters() {
  param.Aeff = 4.857011;
  param.alpha = std::exp(0.3121634);
  param.CHB = std::exp(0.1482344);
  param.CHBT = 0.0; param.SigmaHB = 0.009443451; param.Rav = 0.5;
  param.comb_SG_A_std = 35.31765; param.comb_SG_z_coord = 0.0;
  param.comb_modSG_exp = 0.6666666666666666;
  param.hole_area = 12.57;
  param.exp_param["dG_eta"] = -4.211934;
  param.exp_param["dG_omega_ring"] = 0.2882878;
  param.exp_param["dG_vdw_1"]  = 0.03761312;
  param.exp_param["dG_vdw_6"]  = 0.01778966;
  param.exp_param["dG_vdw_7"]  = 0.0006285596;
  param.exp_param["dG_vdw_8"]  = 0.005266507;
  param.exp_param["dG_vdw_9"]  = 0.00537074;
  param.exp_param["dG_vdw_14"] = 0.00331679;
  param.exp_param["dG_vdw_15"] = 0.00397561;
  param.exp_param["dG_vdw_16"] = 0.03259574;
  param.exp_param["dG_vdw_17"] = 0.03610742;
  param.exp_param["dG_vdw_35"] = 0.044093;
  param.exp_param["dG_vdw_53"] = 0.2044762;
  param.useGivenInitialK = false; param.noTracing = false;
}

// Standard pure-component / Phi option block shared by all entry points (single-phase, no flash).
static void ocrs_setup_pure_opts() {
  param.sw_phi = 1; param.sw_PT_Flash = 0; param.sw_checkAndCalcBinaryLLE = 0;
  param.sw_lnGamma = 0; param.sw_excess = 0; param.sw_isotherm = 0;
  param.sw_Pbub = 0; param.sw_Pdew = 0; param.sw_Tbub = 0; param.sw_Tdew = 0;
  param.sw_criticalPoint = 0; param.sw_azeotropicPoint = 0; param.sw_criticalPoint_specifiedx = 0;
  param.sw_combTerm = 3;
  param.sw_alwaysCalculateSizeRelatedParameters = 0; param.sw_alwaysReloadSigmaProfiles = 0;
  param.sw_useSegmentReferenceStateForInteractionMatrix = 0;
  param.sw_calculateContactStatisticsAndAdditionalProperties = 0;
  param.sw_calculateResidualProperties = 1;
  param.sw_differentiateHydrogens = 0; param.sw_differentiateMoleculeGroups = 0;
  param.sw_COSMOfiles_type = "ORCA_COSMO_TZVPD";
  param.numberOfPartialInteractionMatrices = 0;
  param.sw_atomicNumber = 0; param.sw_misfit = 0; param.sw_skip_COSMOSPACE_errors = 0;
  ocrsphi_set_global_parameters();
}

// Build a molecule DIRECTLY from a stored clustered profile (skips the .orcacosmo disk parse +
// re-cluster). The calc reads only molecule.segments (the 6 clustered arrays) + Area/Volume/charge/
// group; resizeMonoatomicCations touches atoms only for moleculeGroup==3, so neutral molecules need
// no atom data. This is what makes fitting fast and stores profiles the CSP way (in the bundle).
static molecule ocrs_build_mol_from_prof(const List& prof, int index) {
  molecule m;
  m.name = "stored";
  m.index = index;
  m.Area = as<double>(prof["Area"]);
  m.Volume = as<double>(prof["Volume"]);
  m.moleculeCharge = (signed char) as<int>(prof["charge"]);
  m.moleculeGroup = (unsigned short) as<int>(prof["mgroup"]);
  IntegerVector grp = prof["group"], hbt = prof["hbtype"], an = prof["atomicNum"];
  NumericVector sig = prof["sigma"], sigc = prof["sigmaCorr"], area = prof["area"];
  int ns = grp.size();
  m.segments = segmentTypeCollection(1);
  m.segments.reserve(ns);
  for (int k = 0; k < ns; k++) {
    m.segments.SegmentTypeGroup.push_back((unsigned short) grp[k]);
    m.segments.SegmentTypeSigma.push_back(sig[k]);
    m.segments.SegmentTypeSigmaCorr.push_back(sigc[k]);
    m.segments.SegmentTypeHBtype.push_back((unsigned short) hbt[k]);
    m.segments.SegmentTypeAtomicNumber.push_back((unsigned short) an[k]);
    m.segments.SegmentTypeAreas.push_back(std::vector<double>{ (double) area[k] });
    m.segments.SegmentTypeMoleculeIndex.push_back(index);
  }
  return m;
}

//' Export the clustered openCOSMO-RS-Phi sigma profile from an .orcacosmo file
//'
//' Parses + clusters a profile ONCE and returns the compact clustered representation
//' (the 6 segment-type arrays + Area/Volume/charge/group). Store this in the bundle and feed it
//' back via the *_prof compute functions to avoid re-parsing on every call. Mirrors how CSP stores
//' its sigma profiles (sig/area/hbt).
// [[Rcpp::export]]
List ocrsphi_export_profile_cpp(std::string profile_path, bool verbose = false) {
  display = [verbose](std::string s){ if (verbose) Rcpp::Rcout << s; };
  displayTime = [](std::string, unsigned long){};
  initialize(param, true, true, true, verbose);
  ocrs_setup_pure_opts();
  molecule m = loadNewMolecule(param, profile_path);
  int ns = (int) m.segments.size();
  IntegerVector grp(ns), hbt(ns), an(ns);
  NumericVector sig(ns), sigc(ns), area(ns);
  for (int k = 0; k < ns; k++) {
    grp[k]  = m.segments.SegmentTypeGroup[k];
    sig[k]  = m.segments.SegmentTypeSigma[k];
    sigc[k] = m.segments.SegmentTypeSigmaCorr[k];
    hbt[k]  = m.segments.SegmentTypeHBtype[k];
    an[k]   = m.segments.SegmentTypeAtomicNumber[k];
    area[k] = m.segments.SegmentTypeAreas[k][0];
  }
  return List::create(_["group"] = grp, _["sigma"] = sig, _["sigmaCorr"] = sigc,
                      _["hbtype"] = hbt, _["atomicNum"] = an, _["area"] = area,
                      _["Area"] = m.Area, _["Volume"] = m.Volume,
                      _["charge"] = (int) m.moleculeCharge, _["mgroup"] = (int) m.moleculeGroup,
                      _["nseg"] = ns);
}

// Shared pure-component (1 comp + hole) LL saturation core. `molecules` must already hold
// [component, hole]; returns Psat + saturated molar volumes. Used by both entry points below.
static List ocrs_psat_core(NumericVector pc_params, NumericVector temps, double pressure_guess) {
  const int nT = temps.size();
  const int nConc = 2 * nT;
  const int numberOfComponents = 1;
  const int nCompHole = 2;

  const int npar = (int)pc_params.size();                 // 4 (old dispersion) or 5 (Zini: adds k)
  std::vector<double> phibuf(npar);
  for (int i = 0; i < npar; i++) phibuf[i] = pc_params[i];
  new (&param.phi_param) OcrsMapMat(phibuf.data(), numberOfComponents, npar);

  // ---- loadCalculations (n_ex -> 2) ----
  n_ex += 1;
  calculations.reserve(1);
  calculation nc = calculation(nCompHole);
  nc.calculationType = "LL";

  for (int j = 0; j < nCompHole; j++) {
    int molIdx = (j < numberOfComponents) ? 0 : (int)molecules.size() - 1;
    std::shared_ptr<molecule> m = molecules[molIdx];
    for (int k = 0; k < (int)m->segments.size(); k++)
      nc.segments.add((unsigned short)j, m->segments.SegmentTypeGroup[k],
                      m->segments.SegmentTypeSigma[k], m->segments.SegmentTypeSigmaCorr[k],
                      m->segments.SegmentTypeHBtype[k], m->segments.SegmentTypeAtomicNumber[k],
                      m->segments.SegmentTypeAreas[k][0], m->index);
    nc.components.push_back(m);
    nc.moleculeIndices[j] = molIdx;
  }
  nc.segments.sort(); nc.segments.shrink_to_fit();

  std::vector<double> Tvec(nConc);
  for (int i = 0; i < nT; i++) { Tvec[i] = temps[i]; Tvec[i + nT] = temps[i]; }

  std::vector<double> concBuf(nConc * numberOfComponents, 1.0);
  for (int j = 0; j < nConc; j++) {
    std::vector<double> row(nCompHole, 0.0);
    row[0] = concBuf[j];
    nc.temperatures.push_back(Tvec[j]);
    nc.concentrations.push_back(row);
  }
  nc.originalNumberOfCalculations = (unsigned short)nConc;

  for (int j = 0; j < nConc; j++) {
    nc.referenceStateType.push_back((unsigned short)5);
    std::vector<int> idxs;
    for (int k = 0; k < nCompHole; k++) {
      std::vector<double> rsc(nCompHole, 0.0); rsc[nCompHole - 1] = 1.0;
      int ci = (int)nc.addOrFindArrayIndexForConcentration(rsc, Tvec[j]);
      idxs.push_back(ci);
    }
    nc.referenceStateCalculationIndices.push_back(idxs);
  }

  new (&nc.originalConcentrations) OcrsMapMat(concBuf.data(), nConc, numberOfComponents);
  std::vector<double> lnGRes(nConc * nCompHole, 0.0), lnGComb(nConc * nCompHole, 0.0), lnGTot(nConc * nCompHole, 0.0);
  new (&nc.lnGammaResidual)      OcrsMapMat(lnGRes.data(),  nConc, nCompHole);
  new (&nc.lnGammaCombinatorial) OcrsMapMat(lnGComb.data(), nConc, nCompHole);
  new (&nc.lnGammaTotal)         OcrsMapMat(lnGTot.data(),  nConc, nCompHole);
  nc.number = 0;
  finishCalculationInitiation(nc, param);

  std::vector<double> repP(nConc, 0.0), attP(nConc, 0.0), totP(nConc, pressure_guess), tgtP(nConc, pressure_guess);
  std::vector<double> lnPhiT(nConc, 0.0), lnPhiR(nConc, 0.0), lnPhiA(nConc, 0.0);
  std::vector<double> molV(nConc, 0.0), hres(nConc, 0.0), cpres(nConc, 0.0);
  new (&nc.repulsivePressure)  OcrsMapVec(repP.data(), nConc);
  new (&nc.attractivePressure) OcrsMapVec(attP.data(), nConc);
  new (&nc.totalPressure)      OcrsMapVec(totP.data(), nConc);
  new (&nc.targetPressure)     OcrsMapVec(tgtP.data(), nConc);
  new (&nc.lnPhiTotal)         OcrsMapMat(lnPhiT.data(), nConc, numberOfComponents);
  new (&nc.lnPhiRepulsive)     OcrsMapMat(lnPhiR.data(), nConc, numberOfComponents);
  new (&nc.lnPhiAttractive)    OcrsMapMat(lnPhiA.data(), nConc, numberOfComponents);
  new (&nc.molarVolume)        OcrsMapVec(molV.data(), nConc);
  new (&nc.molarResidualEnthalpy) OcrsMapVec(hres.data(), nConc);
  new (&nc.molarResidualHeatCapacityAtConstantPressure) OcrsMapVec(cpres.data(), nConc);
  nc.lnPhiTotalPDETAtConstantPx = MatrixCalcType::Zero(nConc, (int)nc.components.size());
  nc.lnGammaResidualPDETatConstantvx = MatrixCalcType::Zero(nConc, nCompHole);
  nc.lnGammaResidualSecondPDETatConstantvx = MatrixCalcType::Zero(nConc, nCompHole);
  nc.lnGammaResidualPDEvatConstantTx = MatrixCalcType::Zero(nConc, nCompHole);
  nc.recalculateConcentrationsForPhi = true;

  calculations.push_back(nc);

  // ---- calculate (n_ex -> 3) ----
  n_ex += 1;
  if (param.sw_alwaysCalculateSizeRelatedParameters == 0 && n_ex == 3)
    resizeMonoatomicCations(param, molecules);
  std::vector<int> calcIdx = { 0 };
  calculate(calcIdx);

  NumericVector Psat(nT), vv(nT), lv(nT);
  for (int i = 0; i < nT; i++) {
    Psat[i] = calculations[0].totalPressure(i + nT);
    vv[i]   = calculations[0].molarVolume(i);
    lv[i]   = calculations[0].molarVolume(i + nT);
  }
  return List::create(_["T"] = temps, _["Psat_Pa"] = Psat,
                      _["v_vapor_m3mol"] = vv, _["v_liquid_m3mol"] = lv);
}

//' openCOSMO-RS-Phi pure-component vapor pressure from an .orcacosmo file path
// [[Rcpp::export]]
List ocrsphi_psat_cpp(std::string profile_path, NumericVector pc_params,
                      NumericVector temps, double pressure_guess = 101325.0, bool verbose = false) {
  display = [verbose](std::string s){ if (verbose) Rcpp::Rcout << s; };
  displayTime = [](std::string, unsigned long){};
  initialize(param, true, true, true, verbose);
  ocrs_setup_pure_opts();
  n_ex += 1;  // loadMolecules
  molecule newMol = loadNewMolecule(param, profile_path);
  newMol.index = (int)molecules.size();
  molecules.push_back(std::make_shared<molecule>(newMol));
  molecules.push_back(std::make_shared<molecule>(createHoleMolecule(param)));
  return ocrs_psat_core(pc_params, temps, pressure_guess);
}

//' openCOSMO-RS-Phi pure-component vapor pressure from a STORED clustered profile (fast; no re-parse)
// [[Rcpp::export]]
List ocrsphi_psat_prof_cpp(List profile, NumericVector pc_params,
                           NumericVector temps, double pressure_guess = 101325.0, bool verbose = false) {
  display = [verbose](std::string s){ if (verbose) Rcpp::Rcout << s; };
  displayTime = [](std::string, unsigned long){};
  initialize(param, true, true, true, verbose);
  ocrs_setup_pure_opts();
  n_ex += 1;
  molecule newMol = ocrs_build_mol_from_prof(profile, (int)molecules.size());
  molecules.push_back(std::make_shared<molecule>(newMol));
  molecules.push_back(std::make_shared<molecule>(createHoleMolecule(param)));
  return ocrs_psat_core(pc_params, temps, pressure_guess);
}


//' openCOSMO-RS-Phi binary pT-flash
//'
//' Native (no-Python) binary pT-flash. Given two pure-component ORCA sigma
//' profiles, their 4-parameter EoS parameters, a temperature, pressure and
//' overall composition, returns the equilibrium phase compositions
//' (row 1 = vapor/light phase, row 2 = liquid/heavy phase). An all-zero
//' second row indicates a single stable phase (no split).
//'
//' @param prof1,prof2 paths to the two `.orcacosmo` profiles
//' @param pc_params 2x4 matrix, each row c(b_i, delta_m0, delta_mT, b_h)
//' @param temperature temperature (K)
//' @param pressure total pressure (engine units, consistent with ocrsphi_psat_cpp)
//' @param z1 overall mole fraction of component 1
//' @param calctype "VL" or "LL"
//' @param verbose print engine/convergence messages
//' @return list: phase_comps (2x2 matrix), T, P, z1
// [[Rcpp::export]]
List ocrsphi_flash_binary_cpp(std::string prof1, std::string prof2, NumericMatrix pc_params,
                              double temperature, double pressure, double z1,
                              std::string calctype = "VL", bool verbose = false) {
  const int numberOfComponents = 2;
  const int nCompHole = 3;
  const int nConc = 2;   // one condition: row 0 vapor/light, row 1 liquid/heavy

  display = [verbose](std::string s){ if (verbose) Rcpp::Rcout << s; };
  displayTime = [](std::string, unsigned long){};
  initialize(param, true, true, true, verbose);

  param.sw_phi = 1; param.sw_PT_Flash = 1; param.sw_checkAndCalcBinaryLLE = 0;
  param.sw_lnGamma = 0; param.sw_excess = 0; param.sw_isotherm = 0;
  param.sw_Pbub = 0; param.sw_Pdew = 0; param.sw_Tbub = 0; param.sw_Tdew = 0;
  param.sw_criticalPoint = 0; param.sw_azeotropicPoint = 0; param.sw_criticalPoint_specifiedx = 0;
  param.sw_combTerm = 3;
  param.sw_alwaysCalculateSizeRelatedParameters = 0; param.sw_alwaysReloadSigmaProfiles = 0;
  param.sw_useSegmentReferenceStateForInteractionMatrix = 0;
  param.sw_calculateContactStatisticsAndAdditionalProperties = 0;
  param.sw_calculateResidualProperties = 1;
  param.sw_differentiateHydrogens = 0; param.sw_differentiateMoleculeGroups = 0;
  param.sw_COSMOfiles_type = "ORCA_COSMO_TZVPD";
  param.numberOfPartialInteractionMatrices = 0;
  param.sw_atomicNumber = 0; param.sw_misfit = 0; param.sw_skip_COSMOSPACE_errors = 0;

  ocrsphi_set_global_parameters();
  #define OCRS_CK(msg) do { if (verbose) { fprintf(stderr, "[ocrs] " msg "\n"); fflush(stderr); } } while(0)
  OCRS_CK("params set");

  // ---- loadMolecules (n_ex -> 1) ----
  n_ex += 1;
  std::vector<std::string> paths = { prof1, prof2 };
  for (size_t p = 0; p < paths.size(); p++) {
    molecule nm = loadNewMolecule(param, paths[p]);
    nm.index = (int)molecules.size();
    molecules.push_back(std::make_shared<molecule>(nm));
  }
  molecule holeMol = createHoleMolecule(param);
  molecules.push_back(std::make_shared<molecule>(holeMol));

  const int npar = (int)pc_params.ncol();                 // 4 (old dispersion) or 5 (Zini: adds per-cpd k)
  std::vector<double> phibuf(numberOfComponents * npar);
  for (int i = 0; i < numberOfComponents; i++)
    for (int j = 0; j < npar; j++) phibuf[i * npar + j] = pc_params(i, j);
  new (&param.phi_param) OcrsMapMat(phibuf.data(), numberOfComponents, npar);
  OCRS_CK("molecules loaded");

  // ---- loadCalculations (n_ex -> 2) ----
  n_ex += 1;
  calculations.reserve(1);
  calculation nc = calculation(nCompHole);
  nc.calculationType = calctype;

  for (int j = 0; j < nCompHole; j++) {
    int molIdx = (j < numberOfComponents) ? j : (int)molecules.size() - 1;
    std::shared_ptr<molecule> m = molecules[molIdx];
    for (int k = 0; k < (int)m->segments.size(); k++)
      nc.segments.add((unsigned short)j, m->segments.SegmentTypeGroup[k],
                      m->segments.SegmentTypeSigma[k], m->segments.SegmentTypeSigmaCorr[k],
                      m->segments.SegmentTypeHBtype[k], m->segments.SegmentTypeAtomicNumber[k],
                      m->segments.SegmentTypeAreas[k][0], m->index);
    nc.components.push_back(m);
    nc.moleculeIndices[j] = molIdx;
  }
  nc.segments.sort(); nc.segments.shrink_to_fit();

  std::vector<double> Tvec = { temperature, temperature };
  std::vector<double> concBuf = { z1, 1.0 - z1, z1, 1.0 - z1 };   // 2 rows x 2 comps (feed)
  for (int j = 0; j < nConc; j++) {
    std::vector<double> row(nCompHole, 0.0);
    row[0] = concBuf[j * 2 + 0]; row[1] = concBuf[j * 2 + 1];
    nc.temperatures.push_back(Tvec[j]);
    nc.concentrations.push_back(row);
  }
  nc.originalNumberOfCalculations = (unsigned short)nConc;

  for (int j = 0; j < nConc; j++) {
    nc.referenceStateType.push_back((unsigned short)5);
    std::vector<int> idxs;
    for (int k = 0; k < nCompHole; k++) {
      std::vector<double> rsc(nCompHole, 0.0); rsc[nCompHole - 1] = 1.0;
      int ci = (int)nc.addOrFindArrayIndexForConcentration(rsc, Tvec[j]);
      idxs.push_back(ci);
    }
    nc.referenceStateCalculationIndices.push_back(idxs);
  }

  new (&nc.originalConcentrations) OcrsMapMat(concBuf.data(), nConc, numberOfComponents);
  std::vector<double> lnGRes(nConc * nCompHole, 0.0), lnGComb(nConc * nCompHole, 0.0), lnGTot(nConc * nCompHole, 0.0);
  new (&nc.lnGammaResidual)      OcrsMapMat(lnGRes.data(),  nConc, nCompHole);
  new (&nc.lnGammaCombinatorial) OcrsMapMat(lnGComb.data(), nConc, nCompHole);
  new (&nc.lnGammaTotal)         OcrsMapMat(lnGTot.data(),  nConc, nCompHole);
  nc.number = 0;
  finishCalculationInitiation(nc, param);

  std::vector<double> repP(nConc, 0.0), attP(nConc, 0.0), totP(nConc, pressure), tgtP(nConc, pressure);
  std::vector<double> lnPhiT(nConc * numberOfComponents, 0.0), lnPhiR(nConc * numberOfComponents, 0.0), lnPhiA(nConc * numberOfComponents, 0.0);
  std::vector<double> molV(nConc, 0.0), hres(nConc, 0.0), cpres(nConc, 0.0);
  new (&nc.repulsivePressure)  OcrsMapVec(repP.data(), nConc);
  new (&nc.attractivePressure) OcrsMapVec(attP.data(), nConc);
  new (&nc.totalPressure)      OcrsMapVec(totP.data(), nConc);
  new (&nc.targetPressure)     OcrsMapVec(tgtP.data(), nConc);
  new (&nc.lnPhiTotal)         OcrsMapMat(lnPhiT.data(), nConc, numberOfComponents);
  new (&nc.lnPhiRepulsive)     OcrsMapMat(lnPhiR.data(), nConc, numberOfComponents);
  new (&nc.lnPhiAttractive)    OcrsMapMat(lnPhiA.data(), nConc, numberOfComponents);
  new (&nc.molarVolume)        OcrsMapVec(molV.data(), nConc);
  new (&nc.molarResidualEnthalpy) OcrsMapVec(hres.data(), nConc);
  new (&nc.molarResidualHeatCapacityAtConstantPressure) OcrsMapVec(cpres.data(), nConc);
  nc.lnPhiTotalPDETAtConstantPx = MatrixCalcType::Zero(nConc, (int)nc.components.size());
  nc.lnGammaResidualPDETatConstantvx = MatrixCalcType::Zero(nConc, nCompHole);
  nc.lnGammaResidualSecondPDETatConstantvx = MatrixCalcType::Zero(nConc, nCompHole);
  nc.lnGammaResidualPDEvatConstantTx = MatrixCalcType::Zero(nConc, nCompHole);
  nc.recalculateConcentrationsForPhi = true;

  calculations.push_back(nc);
  OCRS_CK("calc loaded + pushed");

  // ---- subcalculation setup (pure-component references; mirror binding VL/PT_Flash path) ----
  {
    calculation& C = calculations[0];
    C.uniqueTemperatures = C.temperatures;
    std::sort(C.uniqueTemperatures.begin(), C.uniqueTemperatures.end());
    C.uniqueTemperatures.erase(std::unique(C.uniqueTemperatures.begin(), C.uniqueTemperatures.end()),
                               C.uniqueTemperatures.end());
    C.IndicesForPureComponentCalculations = Eigen::MatrixXi((int)C.uniqueTemperatures.size(), numberOfComponents);
    for (int j = 0; j < numberOfComponents; j++) {
      std::vector<std::shared_ptr<molecule>> actualMolecules;
      actualMolecules.push_back(C.components[j]);
      actualMolecules.push_back(C.components[nCompHole - 1]);
      for (int k = 0; k < (int)C.uniqueTemperatures.size(); k++) {
        std::vector<double> tvec = { C.uniqueTemperatures[k] };
        int subIdx = C.addOrFindArrayIndexInSubcalculations(subcalculations, actualMolecules, C.uniqueTemperatures[k]);
        if (subIdx == -1) {
          subcalculations.push_back(loadSubcalculation(actualMolecules, tvec, { {1.0, 0.0} }));
          subIdx = (int)subcalculations.size() - 1;
        }
        C.IndicesForPureComponentCalculations(k, j) = subIdx;
      }
    }
  }
  // Re-point every _data-backed Eigen::Map of each subcalc to its own now-stable storage
  // (loadSubcalculation's maps dangle after push_back; see calculation::fixupMapsAfterCopy).
  for (int k = 0; k < (int)subcalculations.size(); k++)
    subcalculations[k].fixupMapsAfterCopy();

  OCRS_CK("subcalc setup done");

  // ---- calculate (n_ex -> 3) ----
  n_ex += 1;
  ocrsphi_set_global_parameters();   // pybind re-loads parameters at the top of calculate()
  if (param.sw_alwaysCalculateSizeRelatedParameters == 0 && n_ex == 3)
    resizeMonoatomicCations(param, molecules);
  if (param.sw_phi == 1 && (int)subcalculations.size() > 0) {
    std::vector<int> subIdx((int)subcalculations.size());
    for (int i = 0; i < (int)subcalculations.size(); i++) subIdx[i] = i;
    calculateSubcalculations(subIdx);
  }
  OCRS_CK("subcalcs computed");
  std::vector<int> calcIdx = { 0 };
  calculate(calcIdx);
  OCRS_CK("calculate() returned");

  // ---- extract phase compositions (row 0 vapor/light, row 1 liquid/heavy) ----
  NumericMatrix phases(2, numberOfComponents);
  for (int r = 0; r < 2; r++)
    for (int c = 0; c < numberOfComponents; c++)
      phases(r, c) = calculations[0].originalConcentrations(r, c);
  return List::create(_["phase_comps"] = phases, _["T"] = temperature,
                      _["P"] = pressure, _["z1"] = z1);
}

// Shared single-phase phi core (N comps + hole) at fixed (T, P, composition). `molecules` must already
// hold [comp1..compN, hole]; `comp` is the length-N mole-fraction vector; `pc_params` is N x 4.
// Dense/liquid root by default (root=1 -> vapor). Used by the binary + n-component entry points.
static List ocrs_lnphi_core(NumericMatrix pc_params, double temperature, double pressure,
                            std::vector<double> comp, int root, double v_guess = -1.0) {
  const int numberOfComponents = (int)pc_params.nrow();   // N real components (+ 1 hole)
  const int nCompHole = numberOfComponents + 1;
  const int nConc = 1;
  const int npar = (int)pc_params.ncol();                 // 4 (old dispersion) or 5 (Zini: adds per-cpd k)
  std::vector<double> phibuf(numberOfComponents * npar);
  for (int i = 0; i < numberOfComponents; i++)
    for (int j = 0; j < npar; j++) phibuf[i * npar + j] = pc_params(i, j);
  new (&param.phi_param) OcrsMapMat(phibuf.data(), numberOfComponents, npar);

  // LOCAL calculation (never push_back -> no dangling-Map copy)
  calculation nc = calculation(nCompHole);
  nc.calculationType = "LL";
  for (int j = 0; j < nCompHole; j++) {
    int molIdx = (j < numberOfComponents) ? j : (int)molecules.size() - 1;
    std::shared_ptr<molecule> m = molecules[molIdx];
    for (int k = 0; k < (int)m->segments.size(); k++)
      nc.segments.add((unsigned short)j, m->segments.SegmentTypeGroup[k],
                      m->segments.SegmentTypeSigma[k], m->segments.SegmentTypeSigmaCorr[k],
                      m->segments.SegmentTypeHBtype[k], m->segments.SegmentTypeAtomicNumber[k],
                      m->segments.SegmentTypeAreas[k][0], m->index);
    nc.components.push_back(m);
    nc.moleculeIndices[j] = molIdx;
  }
  nc.segments.sort(); nc.segments.shrink_to_fit();

  std::vector<double> concBuf = comp;   // length numberOfComponents
  {
    std::vector<double> row(nCompHole, 0.0);
    for (int i = 0; i < numberOfComponents; i++) row[i] = comp[i];
    nc.temperatures.push_back(temperature);
    nc.concentrations.push_back(row);
  }
  nc.originalNumberOfCalculations = (unsigned short)nConc;
  nc.referenceStateType.push_back((unsigned short)5);
  {
    std::vector<int> idxs;
    for (int k = 0; k < nCompHole; k++) {
      std::vector<double> rsc(nCompHole, 0.0); rsc[nCompHole - 1] = 1.0;
      idxs.push_back((int)nc.addOrFindArrayIndexForConcentration(rsc, temperature));
    }
    nc.referenceStateCalculationIndices.push_back(idxs);
  }

  new (&nc.originalConcentrations) OcrsMapMat(concBuf.data(), nConc, numberOfComponents);
  std::vector<double> lnGRes(nConc * nCompHole, 0.0), lnGComb(nConc * nCompHole, 0.0), lnGTot(nConc * nCompHole, 0.0);
  new (&nc.lnGammaResidual)      OcrsMapMat(lnGRes.data(),  nConc, nCompHole);
  new (&nc.lnGammaCombinatorial) OcrsMapMat(lnGComb.data(), nConc, nCompHole);
  new (&nc.lnGammaTotal)         OcrsMapMat(lnGTot.data(),  nConc, nCompHole);
  nc.number = 0;
  finishCalculationInitiation(nc, param);

  std::vector<double> repP(nConc, 0.0), attP(nConc, 0.0), totP(nConc, pressure), tgtP(nConc, pressure);
  std::vector<double> lnPhiT(nConc * numberOfComponents, 0.0), lnPhiR(nConc * numberOfComponents, 0.0), lnPhiA(nConc * numberOfComponents, 0.0);
  std::vector<double> molV(nConc, 0.0), hres(nConc, 0.0), cpres(nConc, 0.0);
  new (&nc.repulsivePressure)  OcrsMapVec(repP.data(), nConc);
  new (&nc.attractivePressure) OcrsMapVec(attP.data(), nConc);
  new (&nc.totalPressure)      OcrsMapVec(totP.data(), nConc);
  new (&nc.targetPressure)     OcrsMapVec(tgtP.data(), nConc);
  new (&nc.lnPhiTotal)         OcrsMapMat(lnPhiT.data(), nConc, numberOfComponents);
  new (&nc.lnPhiRepulsive)     OcrsMapMat(lnPhiR.data(), nConc, numberOfComponents);
  new (&nc.lnPhiAttractive)    OcrsMapMat(lnPhiA.data(), nConc, numberOfComponents);
  new (&nc.molarVolume)        OcrsMapVec(molV.data(), nConc);
  new (&nc.molarResidualEnthalpy) OcrsMapVec(hres.data(), nConc);
  new (&nc.molarResidualHeatCapacityAtConstantPressure) OcrsMapVec(cpres.data(), nConc);
  nc.lnPhiTotalPDETAtConstantPx = MatrixCalcType::Zero(nConc, (int)nc.components.size());
  nc.lnGammaResidualPDETatConstantvx = MatrixCalcType::Zero(nConc, nCompHole);
  nc.lnGammaResidualSecondPDETatConstantvx = MatrixCalcType::Zero(nConc, nCompHole);
  nc.lnGammaResidualPDEvatConstantTx = MatrixCalcType::Zero(nConc, nCompHole);
  nc.recalculateConcentrationsForPhi = true;

  // segment + cavity-volume setup (mirrors calculate() lines 2685-2732)
  const int nsegRU = RoundUpToNextMultipleOfEight((int)nc.segments.size());
  const int nconcAll = (int)nc.concentrations.size();
  nc.segmentGammas               = MatrixCalcType::Constant(nsegRU, nconcAll, 1.0);
  nc.segmentGammasPDETemperature = MatrixCalcType::Constant(nsegRU, nconcAll, 1.0);
  nc.segmentGammasPDEVolume      = MatrixCalcType::Constant(nsegRU, nconcAll, 1.0);
  nc.segmentConcentrations               = MatrixCalcType::Zero(nsegRU, nconcAll);
  nc.PartialsegmentConcentrationsPartialv = MatrixCalcType::Zero(nsegRU, nconcAll);
  n_ex = 3;
  resizeMonoatomicCations(param, molecules);
  rescaleSegments(param, nc);
  calculateSegmentConcentrations(nc);

  nc.componentCavityVolume = Eigen::VectorXd::Zero(numberOfComponents);
  nc.componentHoleVolume   = Eigen::VectorXd::Zero(numberOfComponents);
  for (int i = 0; i < numberOfComponents; i++) {
    int index = nc.components[i]->index;
    nc.componentCavityVolume(i) = double(param.phi_param(index, 0) * VMOL_V);
    nc.componentHoleVolume(i)   = double(param.phi_param(index, 3) * VMOL_V);
  }
  nc.mixtureCavityVolume = nc.originalConcentrations.cast<double>() * nc.componentCavityVolume;
  nc.mixtureHoleVolume   = nc.originalConcentrations.cast<double>() * nc.componentHoleVolume;

  // volume root-find at target pressure
  auto evalP = [&](double v) -> double {
    nc.molarVolume(0) = v;
    nc.recalculateConcentrationsForPhi = true;
    calculateEOS(param, nc, 0);
    return nc.totalPressure(0);
  };
  const double vcav = nc.mixtureCavityVolume(0);
  double vroot = -1.0; int status = -1;

  // Illinois (modified regula-falsi) bracket refine: superlinear + keeps the bracket, so it converges to
  // the same P(v)=pressure root as the old fixed 200-step bisection but in far fewer evalP (calculateEOS/
  // COSMOSPACE) calls. r(v)=P(v)-pressure is monotone on the bracket [a,b].
  auto refine = [&](double a, double b) -> double {
    double fa = evalP(a) - pressure, fb = evalP(b) - pressure;
    if (fa == fb) return 0.5 * (a + b);
    double v = 0.5 * (a + b);
    for (int it = 0; it < 80; it++) {
      v = (a * fb - b * fa) / (fb - fa);                        // false-position estimate
      if (!(v > std::min(a, b) && v < std::max(a, b))) v = 0.5 * (a + b);
      double fv = evalP(v) - pressure;
      if (std::fabs(b - a) < 1e-14 * std::fabs(v) || std::fabs(fv) < 1e-3) break;
      if ((fv < 0) == (fa < 0)) { a = v; fa = fv; fb *= 0.5; } // Illinois down-weight of the stale endpoint
      else                      { b = v; fb = fv; fa *= 0.5; }
    }
    return v;
  };

  if (root == 0) {                          // dense/liquid root (scCO2): march up from just above vcav
    double v0 = 1.0005 * vcav;
    if (v_guess > vcav * 1.05) {            // warm start: fast-forward toward a nearby previous root, but
      double vff = v_guess * 0.90;          // only if that point is verified still denser than the root
      if (vff > v0 && (evalP(vff) - pressure) > 0.0) v0 = vff;
    }
    double vprev = v0, Pprev = evalP(vprev);
    for (int it = 0; it < 4000; it++) {
      double v = vprev * 1.01, Pv = evalP(v);
      if (Pprev > pressure && Pv <= pressure) { vroot = refine(vprev, v); status = 1; break; }
      if (Pv > Pprev) break;
      vprev = v; Pprev = Pv;
    }
  } else {                                  // vapor-like root: march down from a large volume
    double vprev = 1.0e2, Pprev = evalP(vprev);
    for (int it = 0; it < 4000; it++) {
      double v = vprev / 1.02;
      if (v <= 1.05 * vcav) break;
      double Pv = evalP(v);
      if (Pprev < pressure && Pv >= pressure) { vroot = refine(v, vprev); status = 1; break; }
      vprev = v; Pprev = Pv;
    }
  }

  NumericVector lnphi(numberOfComponents), phi(numberOfComponents);
  double vout = NA_REAL, dens = NA_REAL, Psolved = NA_REAL;
  if (status == 1) {
    double Pcheck = evalP(vroot);
    for (int i = 0; i < numberOfComponents; i++) {
      lnphi[i] = nc.lnPhiTotal(0, i);
      phi[i]   = std::exp(nc.lnPhiTotal(0, i));
    }
    vout = vroot; dens = 1.0 / vroot; Psolved = Pcheck;
  } else {
    for (int i = 0; i < numberOfComponents; i++) { lnphi[i] = NA_REAL; phi[i] = NA_REAL; }
  }
  return List::create(_["lnphi"] = lnphi, _["phi"] = phi,
                      _["molar_volume_m3mol"] = vout, _["density_mol_m3"] = dens,
                      _["pressure_solved_Pa"] = Psolved, _["status"] = status,
                      _["T"] = temperature, _["P"] = pressure,
                      _["x1"] = (comp.size() ? comp[0] : NA_REAL));
}

//' openCOSMO-RS-Phi scCO2 solubility over a (T, P) grid, HOISTED into one call.
//'
//' Builds the molecule set (CO2, then co-solvent(s), then the solute last) ONCE and then, per grid point,
//' evaluates the pure-solute fugacity coefficient and runs the solid-solubility fixed point (or the
//' infinite-dilution liquid limit) entirely in C++. This removes the per-phi-call R<->C++ marshalling +
//' `initialize()` + molecule rebuild that dominated the old path (the R loop called `ocrsphi_lnphi_n`
//' thousands of times), and it warm-starts BOTH the composition fixed point (from the previous grid
//' point) AND the molar-volume root (v_guess threaded across calls), so the volume-root march collapses.
//' Returns the solute mole-fraction solubility y2 per grid point (NA where no valid root/fixed point).
//'
//' @param profiles length-N list of clustered profiles, ordered CO2, then co-solvent(s), then solute.
//' @param pc_params N x 4 matrix, rows c(b_i, delta_m0, delta_mT, b_h) in the same order.
//' @param Tvec,Pbar length-n grid temperatures (K) and pressures (bar).
//' @param xc length-(N-2) solute-free co-solvent mole fractions (empty for a bare CO2+solute binary).
//' @param Tm,dHfus solute melting point (K) and enthalpy of fusion (J/mol); NA -> treated as liquid.
//' @param Rg gas constant (J/mol/K). @param max_it,tol fixed-point cap and relative tolerance.
// [[Rcpp::export]]
NumericVector ocrsphi_solubility_cpp(List profiles, NumericMatrix pc_params,
                                     NumericVector Tvec, NumericVector Pbar, NumericVector xc,
                                     double Tm, double dHfus, double Rg,
                                     int max_it = 100, double tol = 1e-7, bool verbose = false) {
  display = [verbose](std::string s){ if (verbose) Rcpp::Rcout << s; };
  displayTime = [](std::string, unsigned long){};
  const int N = profiles.size();          // [CO2, co-solvent(s)..., solute]; solute is LAST
  const int si = N - 1;
  const int n = Tvec.size();
  initialize(param, true, true, true, verbose);
  ocrs_setup_pure_opts();
  for (int i = 0; i < N; i++) {
    molecule m = ocrs_build_mol_from_prof(as<List>(profiles[i]), (int)molecules.size());
    molecules.push_back(std::make_shared<molecule>(m));
  }
  molecules.push_back(std::make_shared<molecule>(createHoleMolecule(param)));

  std::vector<double> ecomp(N, 0.0); ecomp[si] = 1.0;         // pure-solute composition
  double sumxc = 0.0; for (int j = 0; j < xc.size(); j++) sumxc += xc[j];
  NumericVector y2(n);
  double vg_pure = -1.0, vg_mix = -1.0, y_prev = NA_REAL;

  for (int i = 0; i < n; i++) {
    double Tk = Tvec[i], Pa = Pbar[i] * 1e5;
    double gf;
    if (!R_finite(Tm) || Tk >= Tm) gf = 1.0;                  // liquid solute -> no fusion term
    else { if (!R_finite(dHfus)) { y2[i] = NA_REAL; continue; }
           gf = std::exp(-(dHfus / (Rg * Tm)) * (Tm / Tk - 1.0)); }
    // pure-solute fugacity coefficient (liquid reference): solute at x=1, others at 0
    List pl = ocrs_lnphi_core(pc_params, Tk, Pa, ecomp, 0, vg_pure);
    if (as<int>(pl["status"]) != 1) { y2[i] = NA_REAL; continue; }
    double phiL = as<NumericVector>(pl["phi"])[si];
    double vpl = pl["molar_volume_m3mol"]; if (R_finite(vpl)) vg_pure = vpl;

    if (gf < 1.0) {                                           // SOLID: warm-started fixed point
      double y = (R_finite(y_prev) && y_prev > 1e-12 && y_prev < 0.5) ? y_prev : 1e-4;
      bool bad = false;
      for (int it = 0; it < max_it; it++) {
        std::vector<double> comp(N, 0.0);
        comp[0] = (1.0 - y) * (1.0 - sumxc);
        for (int j = 0; j < xc.size(); j++) comp[1 + j] = (1.0 - y) * xc[j];
        comp[si] = y;
        List mx = ocrs_lnphi_core(pc_params, Tk, Pa, comp, 0, vg_mix);
        if (as<int>(mx["status"]) != 1) { bad = true; break; }
        double vm = mx["molar_volume_m3mol"]; if (R_finite(vm)) vg_mix = vm;
        double pm = as<NumericVector>(mx["phi"])[si];
        double yn = (phiL / pm) * gf;
        if (!R_finite(yn) || yn <= 0.0 || yn >= 1.0) { bad = true; break; }
        bool conv = std::fabs(yn - y) < tol * std::max(yn, 1e-12);
        y = yn; if (conv) break;
      }
      y2[i] = bad ? NA_REAL : y;
      if (!bad) y_prev = y;
    } else {                                                  // LIQUID: infinite-dilution (Henry limit)
      double yd = 1e-3;
      std::vector<double> comp(N, 0.0);
      comp[0] = (1.0 - yd) * (1.0 - sumxc);
      for (int j = 0; j < xc.size(); j++) comp[1 + j] = (1.0 - yd) * xc[j];
      comp[si] = yd;
      List mx = ocrs_lnphi_core(pc_params, Tk, Pa, comp, 0, vg_mix);
      if (as<int>(mx["status"]) != 1) { y2[i] = NA_REAL; continue; }
      double vm = mx["molar_volume_m3mol"]; if (R_finite(vm)) vg_mix = vm;
      double pm = as<NumericVector>(mx["phi"])[si];
      double yv = (phiL / pm) * gf;
      y2[i] = (R_finite(yv) && yv > 0.0) ? yv : NA_REAL;
    }
  }
  return y2;
}

//' openCOSMO-RS-Phi single-phase fugacity coefficients at fixed (T, P, x)
//'
//' Evaluates the EoS for ONE fluid phase of a binary mixture at a specified
//' temperature, pressure and composition: finds the molar-volume root at the
//' target pressure (dense/liquid-like root by default) and returns the component
//' fugacity coefficients ln(phi_i). Primitive for solid scCO2 solubility (equate
//' f_solid = y2 * phi2 * P and root-find y2). Unlike the pT-flash it does not
//' solve for phase splitting and needs no subcalcs (calc kept local, no push_back).
//'
//' @param prof1,prof2 paths to the two .orcacosmo profiles
//' @param pc_params 2x4 matrix, each row c(b_i, delta_m0, delta_mT, b_h)
//' @param temperature K
//' @param pressure Pa (target pressure at which phi is evaluated)
//' @param x1 mole fraction of component 1
//' @param root 0 = dense/liquid-like root (default, scCO2), 1 = vapor-like root
//' @param verbose print diagnostics
// [[Rcpp::export]]
List ocrsphi_lnphi_cpp(std::string prof1, std::string prof2, NumericMatrix pc_params,
                       double temperature, double pressure, double x1, int root = 0, bool verbose = false) {
  display = [verbose](std::string s){ if (verbose) Rcpp::Rcout << s; };
  displayTime = [](std::string, unsigned long){};
  initialize(param, true, true, true, verbose);
  ocrs_setup_pure_opts();
  std::vector<std::string> paths = { prof1, prof2 };
  for (size_t p = 0; p < paths.size(); p++) {
    molecule nm = loadNewMolecule(param, paths[p]);
    nm.index = (int)molecules.size();
    molecules.push_back(std::make_shared<molecule>(nm));
  }
  molecules.push_back(std::make_shared<molecule>(createHoleMolecule(param)));
  return ocrs_lnphi_core(pc_params, temperature, pressure, std::vector<double>{x1, 1.0 - x1}, root);
}

//' openCOSMO-RS-Phi single-phase fugacity coefficients from STORED clustered profiles (fast; no re-parse)
// [[Rcpp::export]]
List ocrsphi_lnphi_prof_cpp(List profile1, List profile2, NumericMatrix pc_params,
                            double temperature, double pressure, double x1, int root = 0, bool verbose = false) {
  display = [verbose](std::string s){ if (verbose) Rcpp::Rcout << s; };
  displayTime = [](std::string, unsigned long){};
  initialize(param, true, true, true, verbose);
  ocrs_setup_pure_opts();
  molecule m1 = ocrs_build_mol_from_prof(profile1, (int)molecules.size());
  molecules.push_back(std::make_shared<molecule>(m1));
  molecule m2 = ocrs_build_mol_from_prof(profile2, (int)molecules.size());
  molecules.push_back(std::make_shared<molecule>(m2));
  molecules.push_back(std::make_shared<molecule>(createHoleMolecule(param)));
  return ocrs_lnphi_core(pc_params, temperature, pressure, std::vector<double>{x1, 1.0 - x1}, root);
}

//' openCOSMO-RS-Phi N-component single-phase fugacity coefficients from STORED clustered profiles.
//' `profiles` is a list of N clustered profiles, `pc_params` an N x 4 matrix, `x` a length-N mole-
//' fraction vector. Returns ln(phi_i)/phi_i for all N components. Generalises the binary path so the
//' cascade + co-solvent modules can model solute + CO2 + one-or-more co-solvents (n-component).
// [[Rcpp::export]]
List ocrsphi_lnphi_n_prof_cpp(List profiles, NumericMatrix pc_params,
                              double temperature, double pressure, NumericVector x,
                              int root = 0, bool verbose = false) {
  display = [verbose](std::string s){ if (verbose) Rcpp::Rcout << s; };
  displayTime = [](std::string, unsigned long){};
  initialize(param, true, true, true, verbose);
  ocrs_setup_pure_opts();
  int N = profiles.size();
  if (pc_params.nrow() != N || x.size() != N)
    stop("ocrsphi_lnphi_n: profiles, pc_params rows, and x must all have length N");
  for (int i = 0; i < N; i++) {
    molecule m = ocrs_build_mol_from_prof(as<List>(profiles[i]), (int)molecules.size());
    molecules.push_back(std::make_shared<molecule>(m));
  }
  molecules.push_back(std::make_shared<molecule>(createHoleMolecule(param)));
  std::vector<double> comp(x.begin(), x.end());
  return ocrs_lnphi_core(pc_params, temperature, pressure, comp, root);
}


