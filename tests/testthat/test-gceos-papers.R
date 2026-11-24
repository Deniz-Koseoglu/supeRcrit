# =============================================================================
# Validation of the GC(A)-EOS engine + the three Process-Simulation modules
# against the experimental literature (the "Condensed Lit" archive).
#
# Data points are transcribed digit-exact from the source PDFs (pdftools):
#   Iwai 1996, Chang & Chen 1999, Raeissi & Peters 2001, de Melo 1999  (terpene VLE)
#   Catchpole 1998 (squalene), Pereira 1993 / Fang 2004 (tocopherol, methyl oleate)
#   Diaz 2003/2005 (deterpenation column + selectivity + sizing)
#   Ortega 2025 / Sanchez 2011 / Ferreira 2005 (GCA-EOS FFA association)
#   Espinosa 2002 (fatty-acid-ester van-der-Waals volumes / d_c)
#
# Tolerances bracket the validated predictive accuracy (zero fitted binary
# params unless the molecule-specific Diaz set is used for comparison). The
# heavy solves (bubble curves, critical points, column) are skip_on_cran().
# =============================================================================

core_ok <- function() tryCatch({
  gceos_ar_rt(300, 1e7, 1, 3.3, 1.261, matrix(1, 1, 1), matrix(5e5, 1, 1),
              matrix(0, 1, 1), matrix(0, 1, 1), 82.06); TRUE
}, error = function(e) FALSE)

ATM <- 101325
mpa2atm <- function(p) p * 1e6 / ATM
atm2mpa <- function(p) p * ATM / 1e6
aad <- function(pred, exp) mean(abs((pred - exp) / exp), na.rm = TRUE) * 100
.co2 <- list(groups = c(CO2 = 1), dc = 3.1289, Tc = 304.2)
# decompose, stripping the informational unmatched-atoms attribute for exact compares
.dec <- function(smiles, model) { g <- gceos_decompose(smiles, model); attr(g, "unmatched_atoms") <- NULL; g }

# general (shipped) gceos terpene comps, d_c anchored to the normal boiling point
.terp_gceos <- function() suppressWarnings({
  g_lim <- gceos_decompose("CC1=CCC(CC1)C(=C)C", "gceos")
  g_lin <- gceos_decompose("CC(C)=CCCC(C)(O)C=C", "gceos")
  list(lim = list(groups = g_lim,
                  dc = gceos_fit_dc(g_lim, 660,   449.65, 1, "gceos", c(2, 12)), Tc = 660),
       lin = list(groups = g_lin,
                  dc = gceos_fit_dc(g_lin, 630.5, 471.15, 1, "gceos", c(2, 12)), Tc = 630.5))
})
# Diaz 2005 molecule-specific set
.terp_diaz <- function() {
  d <- gceos_params("diaz2005")
  mk <- function(nm) { g <- d$decomp[nm, ]; g <- g[g > 0]
    list(groups = g, dc = d$components[nm, "dc"], Tc = d$components[nm, "Tc"]) }
  list(diaz = d, lim = mk("limonene"), lin = mk("linalool"), co2 = mk("CO2"))
}
.bubMPa <- function(T, xco2, co2, solute, model)  {
  s <- tryCatch(gceos_saturation(T, c(xco2, 1 - xco2), list(CO2 = co2, S = solute),
                model = model, type = "bubble", Prange = c(1, 400)), error = function(e) NULL)
  if (is.null(s)) NA_real_ else atm2mpa(s$P)
}

# -----------------------------------------------------------------------------
# MODULE 1 -- COMPONENTS & PARAMETERS
# -----------------------------------------------------------------------------
test_that("Module 1: decomposition + d_c regression reproduce literature values", {
  skip_if_not(core_ok(), "compiled core not available")
  # decompositions are deterministic and exact
  expect_equal(.dec("CC1=CCC(CC1)C(=C)C", "gceos"),
               c(`CH2=C` = 1, `CH=C` = 1, cyCH2 = 3, cyCH = 1, CH3 = 2))
  lin <- gceos_decompose("CC(C)=CCCC(C)(O)C=C", "gceos")
  expect_equal(attr(lin, "unmatched_atoms"), 0L)
  expect_equal(unname(lin["COH"]), 1L)
  expect_equal(.dec("CCCCCCCCCC", "gceos"), c(CH3 = 2, CH2 = 8))

  # d_c fitted from the normal boiling point matches Diaz 2005 within rounding
  dc_lim <- suppressWarnings(gceos_fit_dc(.dec("CC1=CCC(CC1)C(=C)C", "gceos"),
                            660, 449.65, 1, "gceos", c(2, 12)))
  dc_lin <- suppressWarnings(gceos_fit_dc(lin, 630.5, 471.15, 1, "gceos", c(2, 12)))
  dc_dec <- suppressWarnings(gceos_fit_dc(c(CH3 = 2, CH2 = 8), 617.7, 447.27, 1, "gceos", c(2, 12)))
  expect_equal(dc_lim, 5.613, tolerance = 0.02)   # Diaz 5.603
  expect_equal(dc_lin, 6.218, tolerance = 0.02)   # Diaz 6.171
  expect_equal(dc_dec, 6.024, tolerance = 0.02)
  # psat at Tb returns to 1 atm by construction
  expect_equal(suppressWarnings(gceos_psat(449.65, list(groups = .dec("CC1=CCC(CC1)C(=C)C","gceos"),
                          dc = dc_lim, Tc = 660), "gceos")), 1, tolerance = 0.01)
})

test_that("Module 1: fatty-acid / ester decompositions are correct (gceos & gcaeos)", {
  skip_if_not(core_ok(), "compiled core not available")
  # GC-EOS splits acids into COOH; esters into CH2COO. 0 unmatched atoms.
  expect_equal(.dec("CCCCCCCCCCCC(=O)O", "gceos"),       c(COOH = 1, CH3 = 1, CH2 = 10))      # lauric
  expect_equal(.dec("CCCCCCCCCCCCCCCC(=O)O", "gceos"),   c(COOH = 1, CH3 = 1, CH2 = 14))      # palmitic
  mo <- gceos_decompose("CCCCCCCC/C=C\\CCCCCCCC(=O)OC", "gceos")                               # methyl oleate
  expect_equal(unname(mo["CH2COO"]), 1L); expect_equal(unname(mo["CH=CH"]), 1L)
  expect_equal(attr(mo, "unmatched_atoms"), 0L)
  # GCA-EOS uses the whole CH2COOH acid group (deacidification set)
  expect_equal(.dec("CCCCCCCCCCCC(=O)O", "gcaeos"),      c(CH2COOH = 1, CH3 = 1, CH2 = 9))     # lauric
  expect_equal(unname(gceos_decompose("CCCCC/C=C\\C/C=C\\CCCCCCCC(=O)O","gcaeos")["CH=CH"]), 2L)  # linoleic
})

# -----------------------------------------------------------------------------
# MODULE 2 -- PHASE BEHAVIOUR : binary VLE, criticals, solubility
# -----------------------------------------------------------------------------
test_that("Module 2: CO2+limonene bubble pressure vs Iwai 1996 & Chang & Chen 1999", {
  skip_if_not(core_ok(), "compiled core not available"); skip_on_cran()
  G <- .terp_gceos(); D <- .terp_diaz()
  iwai <- data.frame(
    T = rep(c(313.2, 323.2, 333.2), each = 5),
    x = c(0.3847,0.5805,0.7004,0.8025,0.8871, 0.2873,0.4541,0.6650,0.7744,0.8338,
          0.3401,0.4812,0.6415,0.7109,0.7696),
    P = c(3.94,5.97,6.93,7.53,7.87, 3.94,6.02,7.97,8.94,9.27, 4.99,6.97,8.82,9.73,10.26))
  pg <- mapply(function(T,x) .bubMPa(T, x, .co2,  G$lim, "gceos"),  iwai$T, iwai$x)
  pd <- mapply(function(T,x) .bubMPa(T, x, D$co2, D$lim, D$diaz),   iwai$T, iwai$x)
  expect_lt(aad(pg, iwai$P), 16)     # shipped general set, fully predictive
  expect_lt(aad(pd, iwai$P), 11)     # molecule-specific Diaz set is better

  cc <- data.frame(x = c(0.0749,0.1713,0.2726,0.3451,0.4433,0.5314,0.6659,0.7395,0.8115,0.8875,0.9084),
                   P = c(1.13,2.16,3.16,4.18,5.18,6.25,7.28,8.27,8.93,9.69,9.83))
  pg <- vapply(cc$x, function(x) .bubMPa(323.20, x, .co2, G$lim, "gceos"), 0)
  expect_lt(aad(pg, cc$P), 18)
})

test_that("Module 2: CO2+linalool bubble pressure vs Raeissi & Peters 2001", {
  skip_if_not(core_ok(), "compiled core not available"); skip_on_cran()
  G <- .terp_gceos(); D <- .terp_diaz()
  rae <- data.frame(x = c(0.9501,0.9302,0.8011,0.7014,0.6096,0.4003,0.2009),
                    P = c(10.043,9.959,9.237,8.426,7.392,4.634,2.240))
  pg <- vapply(rae$x, function(x) .bubMPa(323.15, x, .co2,  G$lin, "gceos"), 0)
  pd <- vapply(rae$x, function(x) .bubMPa(323.15, x, D$co2, D$lin, D$diaz), 0)
  expect_lt(aad(pg, rae$P), 22)      # non-associating GC-EOS under-predicts (missing -OH H-bond)
  expect_lt(aad(pd, rae$P), 8)       # Diaz molecule-specific set is best
  # (gceos-hmw and Diaz both beat plain gceos here -- see test-gceos-crossval.R)
})

test_that("Module 2: CO2+linalool mixture critical points vs Raeissi & Peters 2001", {
  skip_if_not(core_ok(), "compiled core not available"); skip_on_cran()
  G <- .terp_gceos()
  crit <- data.frame(Tc = c(311.41,318.11,320.96,326.43,343.27),
                     Pc = c(8.218,9.186,9.660,10.676,13.689),
                     xc = c(0.9928,0.9801,0.9701,0.9501,0.9302))
  pcp <- xcp <- rep(NA_real_, nrow(crit))
  for (i in seq_len(nrow(crit))) {
    cc <- tryCatch(gceos_critical(crit$Tc[i], list(CO2 = .co2, S = G$lin), "gceos",
                   xrange = c(0.80, 0.998), Prange = c(20, 400)), error = function(e) NULL)
    if (!is.null(cc)) { pcp[i] <- atm2mpa(cc$P_c); xcp[i] <- cc$x_c }
  }
  expect_lt(aad(pcp, crit$Pc), 9)      # P_c overshoots, growing with T (known near-critical bias)
  expect_lt(aad(xcp, crit$xc), 2)      # critical COMPOSITION is essentially nailed
  expect_true(all(pcp >= crit$Pc))     # the bias is a consistent overshoot
})

test_that("Module 2: de Melo 1999 ternary -- selectivity is physical (GC-EOS under-predicts)", {
  skip_if_not(core_ok(), "compiled core not available"); skip_on_cran()
  D <- .terp_diaz()
  comps <- list(limonene = D$lim, linalool = D$lin, CO2 = D$co2)
  fl <- gceos_flash(323.15, mpa2atm(8.0), c(0.123, 0.027, 0.85), comps, model = D$diaz)
  alpha <- unname(fl$K[1] / fl$K[2])
  expect_gt(alpha, 1.5); expect_lt(alpha, 2.3)   # de Melo measured beta 2.1-3.75 (GC-EOS under-predicts)
})

test_that("Module 2: squalene solubility in CO2 vs Catchpole 1998 (gceos-hmw)", {
  skip_if_not(core_ok(), "compiled core not available"); skip_on_cran()
  sq <- list(groups = c(CH3 = 8, CH2 = 10, `CH=C` = 6), dc = 9.244, Tc = 838.1)  # Fornari manual decomp
  solub <- function(T, Pmpa) {
    fl <- tryCatch(gceos_flash(T, mpa2atm(Pmpa), c(0.97, 0.03), list(CO2 = .co2, S = sq),
                   model = "gceos-hmw", Kinit = c(3, 5e-4)), error = function(e) NULL)
    if (is.null(fl)) return(NA_real_)
    y <- if (fl$y[1] >= fl$x[1]) fl$y else fl$x
    unname(1000 * y[2] * 410.7 / (y[1] * 44.01))                 # g squalene / kg CO2
  }
  P323 <- c(15, 17.5, 20, 22.5); S323 <- c(10.63, 17.38, 23.84, 30.12)
  pred <- vapply(P323, function(p) solub(323, p), 0)
  expect_lt(aad(pred, S323), 12)        # ~6% over the operating window at 323 K
  expect_true(all(diff(pred) > 0))      # solubility rises monotonically with pressure
})

test_that("Module 2: alpha-tocopherol solubility vs Pereira 1993 (predictive boundary)", {
  skip_if_not(core_ok(), "compiled core not available"); skip_on_cran()
  toc <- list(groups = c(CH3 = 4, CH2 = 9, CH = 2, cyCH2 = 3, AC = 1,
                         ACCH3 = 3, ACOCH3 = 1, ACOH = 1), dc = 8.858, Tc = 857.4)
  P <- c(15.10, 18.30, 20.60, 22.90, 26.05); yexp <- c(0.00041,0.00077,0.00085,0.00133,0.00138)
  yp <- vapply(P, function(Pm) {
    fl <- tryCatch(gceos_flash(323.1, mpa2atm(Pm), c(0.995, 0.005), list(CO2 = .co2, S = toc),
                   model = "gceos", Kinit = c(2, 1e-4)), error = function(e) NULL)
    if (is.null(fl)) NA_real_ else { y <- if (fl$y[1] >= fl$x[1]) fl$y else fl$x; unname(y[2]) }
  }, 0)
  expect_true(all(is.finite(yp)))
  expect_true(all(diff(yp) > 0))        # correct qualitative trend with pressure
  expect_lt(aad(yp, yexp), 70)          # high-MW + polar chromanol = the predictive limit (~49%)
})

test_that("Module 2: CO2+tocopherol bubble pressure vs Chen 2000 (predictive boundary)", {
  skip_if_not(core_ok(), "compiled core not available"); skip_on_cran()
  # Chen, Chang & Yang, Fluid Phase Equilib. 175 (2000) 107, Table 2: CO2 + a-tocopherol,
  # 313/333 K, liquid x_CO2 ~0.70-0.79.  A clean 20-point set, but - like Pereira's vapor
  # side - tocopherol is the predictive LIMIT: the model over-predicts CO2 uptake, so the
  # bubble pressure runs ~40% low.  d_c 8.858 (Fornari 2008 CO2-VLE-optimized d_c=9.015
  # does not help).  This documents the boundary on a second, larger dataset.
  toc <- list(groups = c(CH3 = 4, CH2 = 9, CH = 2, cyCH2 = 3, AC = 1,
                         ACCH3 = 3, ACOCH3 = 1, ACOH = 1), dc = 8.858, Tc = 857.4)
  chen <- rbind(
    data.frame(T = 313, P = c(9.88,12.05,13.69,16.42,17.77,19.28,20.70,21.95,23.20,24.09),
               xCO2 = c(0.7055,0.7138,0.7140,0.7185,0.7245,0.7252,0.7380,0.7398,0.7391,0.7481)),
    data.frame(T = 333, P = c(10.33,12.41,14.07,16.02,17.68,19.46,20.78,22.03,23.26,24.16),
               xCO2 = c(0.7258,0.7294,0.7294,0.7356,0.7519,0.7544,0.7681,0.7745,0.7779,0.7873)))
  pc <- vapply(seq_len(nrow(chen)), function(i) {
    s <- tryCatch(gceos_saturation(chen$T[i], c(chen$xCO2[i], 1-chen$xCO2[i]), list(CO2 = .co2, S = toc),
                  "gceos", "bubble", Prange = c(1, 400)), error = function(e) NULL)
    if (is.null(s)) NA_real_ else s$P * ATM / 1e6
  }, numeric(1))
  expect_true(all(is.finite(pc)))             # the model runs at every point (physical bubble P)
  expect_lt(aad(pc, chen$P), 55)              # ~38% -- tocopherol predictive limit (cf. Pereira ~49%)
})

test_that("Module 2: CO2+alpha-tocopherol k-values vs Fang 2004 (high-density regime)", {
  skip_if_not(core_ok(), "compiled core not available"); skip_on_cran()
  # Fang et al., J. Supercrit. Fluids 30 (2004) 1, Table 4: CO2 + a-tocopherol VLE
  # (BOTH phases, mol%).  This is the EXACT dataset Fornari (2008) fit, reporting
  # 12.8% AAD in the weight-based equilibrium constant k = w_vapour / w_liquid.
  # Our CO2-aromatic binaries are image-verified IDENTICAL to Fornari's
  # (CO2-ACH/AC=1.06, ACCH3=0.938, ACOCH3=1.047, ACOH=1.094 -- Fornari 2007
  # Table 2); the decomposition, Tc=857.4 and the d(T) law (gceos_engine.R) all
  # match too, and tocopherol is non-associating in Fornari (plain GC-EOS).
  # d_c=8.9 is our fit to this CO2-VLE (Fornari's 8.858 is within fitting noise).
  # In the high-CO2-density supercritical-extraction regime (313 K, >=18 MPa) the
  # weight-k AAD is ~7% (i.e. >= Fornari's 12.8% overall).  See the boundary note
  # below: at LOW solvent density / higher T the model under-predicts vapour
  # tocopherol, which is what lifts the all-isotherm average.
  toc <- list(groups = c(CH3 = 4, CH2 = 9, CH = 2, cyCH2 = 3, AC = 1,
                         ACCH3 = 3, ACOCH3 = 1, ACOH = 1), dc = 8.9, Tc = 857.4)
  MWt <- 430.7; MWc <- 44.01
  wf <- function(xc) (1 - xc) * MWt / ((1 - xc) * MWt + xc * MWc)   # wt frac tocopherol
  # high-density 313.15 K points (P >= 18 MPa); x1, y1 = CO2 mole fraction (mol%)
  d <- data.frame(P  = c(18.72, 21.42, 24.71, 29.11),
                  x1 = c(76.858, 77.031, 77.256, 77.828),
                  y1 = c(99.907, 99.882, 99.815, 99.767))
  kexp <- wf(d$y1 / 100) / wf(d$x1 / 100)
  kcal <- vapply(seq_len(nrow(d)), function(i) {
    xc <- d$x1[i] / 100; yc <- d$y1[i] / 100
    fl <- tryCatch(gceos_flash(313.15, mpa2atm(d$P[i]), c(0.95, 0.05), list(CO2 = .co2, S = toc),
                   model = "gceos", Kinit = c(yc / xc, (1 - yc) / (1 - xc))), error = function(e) NULL)
    if (is.null(fl)) return(NA_real_)
    v <- if (fl$y[1] >= fl$x[1]) fl$y else fl$x; l <- if (fl$y[1] >= fl$x[1]) fl$x else fl$y
    unname((v[2] * MWt / (v[2] * MWt + v[1] * MWc)) / (l[2] * MWt / (l[2] * MWt + l[1] * MWc)))
  }, numeric(1))
  expect_true(all(is.finite(kcal)))     # flash converges at every high-density point
  expect_true(all(diff(kcal) > 0))      # k rises with pressure (correct trend)
  expect_lt(aad(kcal, kexp), 20)        # ~7% in the extraction regime (cf. Fornari 12.8% overall)
})

test_that("Module 2: alpha-tocopherol k-values - LOW-density predictive boundary (Fang 2004)", {
  skip_if_not(core_ok(), "compiled core not available"); skip_on_cran()
  # Companion to the high-density test: at LOW CO2 density (low P) and higher T the
  # model under-predicts vapour tocopherol (k_cal -> 0), so the all-isotherm
  # weight-k AAD is ~50-55% even with the verified-correct (=Fornari) parameters.
  # This is the genuine residual vs Fornari's 12.8% overall - NOT a parameter,
  # decomposition or association error (all verified) but the engine's known
  # under-prediction of heavy polar solute volatility at low solvent density.
  toc <- list(groups = c(CH3 = 4, CH2 = 9, CH = 2, cyCH2 = 3, AC = 1,
                         ACCH3 = 3, ACOCH3 = 1, ACOH = 1), dc = 8.9, Tc = 857.4)
  MWt <- 430.7; MWc <- 44.01
  wf <- function(xc) (1 - xc) * MWt / ((1 - xc) * MWt + xc * MWc)
  d <- data.frame(T  = c(313.15, 313.15, 333.15, 353.15),
                  P  = c(11.16, 12.32, 10.63, 10.30),
                  x1 = c(73.952, 74.497, 73.028, 73.312),
                  y1 = c(99.983, 99.956, 99.987, 99.986))
  kexp <- wf(d$y1 / 100) / wf(d$x1 / 100)
  kcal <- vapply(seq_len(nrow(d)), function(i) {
    xc <- d$x1[i] / 100; yc <- d$y1[i] / 100
    fl <- tryCatch(gceos_flash(d$T[i], mpa2atm(d$P[i]), c(0.95, 0.05), list(CO2 = .co2, S = toc),
                   model = "gceos", Kinit = c(yc / xc, (1 - yc) / (1 - xc))), error = function(e) NULL)
    if (is.null(fl)) return(NA_real_)
    v <- if (fl$y[1] >= fl$x[1]) fl$y else fl$x; l <- if (fl$y[1] >= fl$x[1]) fl$x else fl$y
    unname((v[2] * MWt / (v[2] * MWt + v[1] * MWc)) / (l[2] * MWt / (l[2] * MWt + l[1] * MWc)))
  }, numeric(1))
  expect_true(all(is.finite(kcal)))     # flash still converges - it just under-predicts here
  expect_true(all(kcal < kexp))         # systematic under-prediction at low density (documents the boundary)
})

test_that("Module 2: triglycerides decompose to the TG group; CO2+triolein vs Chen 2000", {
  skip_if_not(core_ok(), "compiled core not available")
  # the decomposer must emit the dedicated TG group for triacylglycerides (Fornari/
  # Vasquez), NOT 3x the generic ester CH2COO; mono-esters/glycerides are unchanged.
  trio <- gceos_decompose("CCCCCCCC/C=C\\CCCCCCCC(=O)OCC(OC(=O)CCCCCCC/C=C\\CCCCCCCC)COC(=O)CCCCCCC/C=C\\CCCCCCCC")
  expect_equal(unname(trio["TG"]), 1L); expect_equal(unname(trio["CH=CH"]), 3L)
  expect_equal(attr(trio, "unmatched_atoms"), 0L)
  expect_true(is.na(trio["CH2COO"]))                          # NOT the generic ester
  expect_equal(unname(gceos_decompose("CCCCCCCCCCCCCCCC(=O)OC")["CH2COO"]), 1L)  # mono-ester unchanged
  mono <- gceos_decompose("CCCCCCCC/C=C\\CCCCCCCC(=O)OCC(O)CO")                   # monoolein: 1 ester
  expect_equal(unname(mono["CH2COO"]), 1L); expect_true(is.na(mono["TG"]))

  skip_on_cran()
  # CO2 + triolein bubble pressure vs Chen 2000 (Table 1) -- now ~14% (was 42% with the
  # generic ester decomposition; Fornari 2008 reports 26.2%).
  to <- list(groups = trio, dc = 11.839, Tc = 1043.3)        # Vasquez 2009 d_c
  tr <- rbind(data.frame(T=313, P=c(10.24,12.37,14.16,16.05,17.74,19.40,20.83,21.88,23.14,23.99),
                         x=c(0.8996,0.9043,0.9048,0.9094,0.9131,0.9175,0.9186,0.9223,0.9244,0.9265)),
              data.frame(T=333, P=c(10.12,12.36,14.10,15.96,17.95,20.02,21.17,22.30,23.09,24.21),
                         x=c(0.8755,0.8784,0.8858,0.8948,0.9022,0.9103,0.9145,0.9155,0.9181,0.9213)))
  pc <- vapply(seq_len(nrow(tr)), function(i) {
    s <- tryCatch(gceos_saturation(tr$T[i], c(tr$x[i], 1-tr$x[i]), list(CO2 = .co2, S = to),
                  "gceos-hmw", "bubble", Prange = c(1, 500)), error = function(e) NULL)
    if (is.null(s)) NA_real_ else s$P * ATM / 1e6
  }, numeric(1))
  expect_gt(sum(is.finite(pc)), 14)
  expect_lt(aad(pc, tr$P), 20)                                # ~14%, beats Fornari 2008 (26.2%)
})

test_that("Module 2: CO2+methyl oleate bubble pressure vs Fang 2004 (operating region)", {
  skip_if_not(core_ok(), "compiled core not available"); skip_on_cran()
  g_mo <- gceos_decompose("CCCCCCCC/C=C\\CCCCCCCC(=O)OC", "gceos")
  dc_mo <- suppressWarnings(gceos_fit_dc(g_mo, 770.9, 604.8, 1, "gceos", c(5, 12)))
  mo <- list(groups = g_mo, dc = dc_mo, Tc = 770.9)
  x <- c(0.75857, 0.79513, 0.85923); P <- c(6.20, 6.69, 8.09)   # 313.15 K, x_CO2 below the critical
  pm <- vapply(x, function(xx) .bubMPa(313.15, xx, .co2, mo, "gceos"), 0)
  expect_lt(aad(pm, P), 12)
})

# -----------------------------------------------------------------------------
# MODULE 3 -- FRACTIONATION COLUMN
# -----------------------------------------------------------------------------
test_that("Module 3: relative volatility vs Diaz 2005 Table 1", {
  skip_if_not(core_ok(), "compiled core not available"); skip_on_cran()
  D <- .terp_diaz()
  comps <- list(limonene = D$lim, linalool = D$lin, CO2 = D$co2)
  t1 <- data.frame(T = c(323,333,343,353,333), P = c(8.8,8.8,8.8,8.8,9.8),
                   gceos_paper = c(1.97,1.95,1.85,1.75,1.77))
  for (i in seq_len(nrow(t1))) {
    fl <- gceos_flash(t1$T[i], mpa2atm(t1$P[i]), c(0.123, 0.027, 0.85), comps, model = D$diaz)
    expect_equal(unname(fl$K[1]/fl$K[2]), t1$gceos_paper[i], tolerance = 0.12)
  }
})

test_that("Module 3: NS column reproduces Diaz 2005 Table 6 (all separation metrics)", {
  skip_if_not(core_ok(), "compiled core not available"); skip_on_cran()
  D <- .terp_diaz()
  comps <- list(limonene = D$lim, linalool = D$lin, CO2 = D$co2)
  zoil <- c(0.9823, 0.0177, 0); P <- mpa2atm(10); MW <- c(136.24, 154.25)
  prev <- NULL
  for (SF in c(seq(20, 560, by = 40), 567)) {
    r <- gceos_column_ns_solve(N = 28, T = 334.06, P = P, comps = comps, FL = 1, zL = zoil,
                               FV = SF, zV = c(0, 0, 1), feedL = 11, reflux = 2.27,
                               solvent = "CO2", model = D$diaz, max_iter = 600, tol = 1e-8, init = prev)
    mb <- (r$raffinate$x[2]*r$raffinate$L + r$extract$comp[2]*r$extract$E) / zoil[2]
    if (r$status == "converged" && abs(mb - 1) < 0.01) prev <- r
  }
  r <- prev
  w <- r$raffinate$x[1:2] * MW; we <- r$extract$comp[1:2] * MW
  raffLi  <- 100 * w[2] / sum(w)
  extLim  <- 100 * we[1] / sum(we)
  linaRec <- 100 * (r$raffinate$x[2] * r$raffinate$L) / zoil[2]   # linalool product = raffinate
  mb <- (r$raffinate$x[2]*r$raffinate$L + r$extract$comp[2]*r$extract$E) / zoil[2]
  expect_equal(r$status, "converged")
  expect_equal(unname(raffLi),  98.18, tolerance = 2.0)            # Diaz raffinate linalool wt%
  expect_equal(unname(linaRec), 94.89, tolerance = 5.0)           # Diaz linalool recovery
  expect_equal(unname(extLim),  99.90, tolerance = 0.5)           # Diaz extract limonene wt%
  expect_equal(unname(100 * r$recovery["limonene"]), 99.96, tolerance = 0.5)
  expect_equal(unname(mb), 1, tolerance = 1e-3)                    # mass conservation

  # hydraulic sizing vs Diaz Table 6 (D = 0.28 m, H = 19.14 m)
  sc <- comps; sc$limonene$MW <- 136.24; sc$linalool$MW <- 154.25; sc$CO2$MW <- 44.01
  sz <- gceos_column_sizing(r, feed_rate = 25, feed_z = zoil, T = 334.06, P = P,
                            comps = sc, model = D$diaz, solvent = "CO2",
                            density = "budich", HETP = 19.14/28, frac_flood = 0.8)
  expect_equal(sz$diameter, 0.28, tolerance = 0.08)   # 0.295 m via Budich densities (~5%)
  expect_equal(sz$height, 19.14, tolerance = 0.05)
})

# -----------------------------------------------------------------------------
# GCA-EOS / FREE FATTY ACIDS : association (Ortega 2025, Sanchez 2011, Ferreira 2005)
# -----------------------------------------------------------------------------
test_that("GCA-EOS: shipped association parameters match Ortega/Sanchez", {
  skip_if_not(core_ok(), "compiled core not available")
  a <- gceos_params("gcaeos")$assoc
  co2_cooh <- a[a$group_i == "CO2"  & a$group_j == "COOH", ]
  cooh     <- a[a$group_i == "COOH" & a$group_j == "COOH", ]
  oh       <- a[a$group_i == "OH"   & a$group_j == "OH", ]
  expect_equal(co2_cooh$eps_R, 1908.0); expect_equal(co2_cooh$kappa, 1.0620)   # Ortega 2025 Table 3
  expect_equal(cooh$eps_R, 6517.1);     expect_equal(cooh$kappa, 0.0238)       # COOH self-association
  expect_equal(oh$eps_R, 2759.0)                                               # Sanchez 2011 Table 6
})

test_that("GCA-EOS: carboxylic-acid association holds the acid in the liquid (lowers K)", {
  skip_if_not(core_ok(), "compiled core not available")
  gca <- gceos_params("gcaeos")
  comps <- list(CO2 = .co2, lauric = list(groups = gceos_decompose("CCCCCCCCCCCC(=O)O","gcaeos"),
                                          dc = 6.3945, Tc = 743))
  x <- .gceos_inputs(333, comps, gca); z <- c(0.9, 0.1); P <- mpa2atm(5)
  f0 <- gceos_flash_cpp(333, P, z, x$d, x$q, x$nu, x$g, x$dg, x$alpha, 82.06, NULL, 300L, 1e-4)
  fA <- gceos_flash_cpp(333, P, z, x$d, x$q, x$nu, x$g, x$dg, x$alpha, 82.06, NULL, 300L, 1e-4, x$numu, x$Delta0)
  expect_equal(f0$status, 1); expect_equal(fA$status, 1)
  expect_lt(fA$K[2], f0$K[2])               # COOH dimerization keeps the acid in the liquid
})

test_that("GCA-EOS: a CO2+fatty-acid binary gives a physical, CO2-rich vapor", {
  skip_if_not(core_ok(), "compiled core not available"); skip_on_cran()
  pal <- list(groups = gceos_decompose("CCCCCCCCCCCCCCCC(=O)O", "gcaeos"), dc = 7.1104, Tc = 776)
  Ps <- vapply(c(0.3, 0.5, 0.7), function(xc) {
    s <- gceos_saturation(333, c(xc, 1 - xc), list(CO2 = .co2, S = pal), "gcaeos", "bubble", Prange = c(1, 400))
    if (is.null(s)) c(P = NA, y = NA) else c(P = atm2mpa(s$P), y = s$incipient[1]) }, c(P = 0, y = 0))
  expect_true(all(Ps["y", ] > 0.99))        # vapor is essentially pure CO2
  expect_true(all(diff(Ps["P", ]) > 0))     # bubble pressure rises with CO2 loading
})

# -----------------------------------------------------------------------------
# ESTERS / FATTY-OIL DERIVATIVES : van der Waals volumes (Espinosa 2002)
# -----------------------------------------------------------------------------
test_that("Esters: van der Waals volumes & d_c vs Espinosa 2002 Table 3", {
  skip_if_not(core_ok(), "compiled core not available")
  g18 <- gceos_decompose("CCCCCCCCCCCCCCCCCC(=O)OCC", "gceos")   # ethyl stearate C18:0
  g14 <- gceos_decompose("CCCCCCCCCCCCCC(=O)OC", "gceos")        # methyl myristate C14:0
  expect_equal(gceos_vdw_volume(g18), 14.269, tolerance = 1e-3)  # Espinosa exact
  expect_equal(gceos_vdw_volume(g14), 10.897, tolerance = 1e-3)
  expect_gt(gceos_dc_ester(g18), 7); expect_lt(gceos_dc_ester(g18), 9)
})

# -----------------------------------------------------------------------------
# DECOMPOSITION: quaternary-carbon group (branched / bicyclic terpenes)
# -----------------------------------------------------------------------------
test_that("Quaternary-C group lets branched/bicyclic molecules auto-decompose", {
  skip_if_not(core_ok(), "compiled core not available")
  expect_equal(gceos_params("gceos")$groups["C", "q"], 0)        # zero attractive surface
  np <- .dec("CC(C)(C)C", "gceos")                                # neopentane
  expect_equal(unname(np[c("CH3(B)", "C")]), c(4L, 1L))
  expect_equal(attr(gceos_decompose("CC(C)(C)C", "gceos"), "unmatched_atoms"), 0L)
  pin <- gceos_decompose("CC1=CCC2CC1C2(C)C", "gceos")            # alpha-pinene (bicyclic, quaternary C)
  expect_equal(unname(pin["C"]), 1L)
  expect_equal(attr(pin, "unmatched_atoms"), 0L)
  # the quaternary C (q=0) runs in the engine: neopentane psat at Tb returns to 1 atm
  dc <- suppressWarnings(gceos_fit_dc(np, 433.8, 282.65, 1, "gceos", c(2, 8)))
  expect_equal(suppressWarnings(gceos_psat(282.65, list(groups = np, dc = dc, Tc = 433.8), "gceos")),
               1, tolerance = 0.01)
})

# -----------------------------------------------------------------------------
# TERPENE COLUMNS beyond Diaz 2005 : Diaz 2003 (citral) + Ganan 2011/2013 set
# -----------------------------------------------------------------------------
test_that("Diaz 2003 lemon-oil (limonene/citral) column reproduces the paper", {
  skip_if_not(core_ok(), "compiled core not available"); skip_on_cran()
  lim <- list(groups = gceos_decompose("CC1=CCC(CC1)C(=C)C"),
              dc = suppressWarnings(gceos_fit_dc(gceos_decompose("CC1=CCC(CC1)C(=C)C"),660,449.65,1,"gceos",c(2,12))), Tc=660)
  cit <- list(groups = gceos_decompose("O=CC=C(C)CCC=C(C)C"),
              dc = suppressWarnings(gceos_fit_dc(gceos_decompose("O=CC=C(C)CCC=C(C)C"),704,502.15,1,"gceos",c(3,9))), Tc=704)
  A <- list(limonene = lim, citral = cit, CO2 = list(groups=c(CO2=1),dc=3.1289,Tc=304.2))
  zL <- c(0.770, 0.230, 0); SFmol <- 125.31*(zL[1]*136.24+zL[2]*152.23)/44.01; prev <- NULL
  for (SF in unique(round(c(seq(20, SFmol, length.out=10), SFmol)))) {
    r <- tryCatch(gceos_column_ns_solve(N=20,T=333.14,P=mpa2atm(9.5),comps=A,FL=1,zL=zL,FV=SF,zV=c(0,0,1),
         feedL=6,reflux=0.586,solvent="CO2",model="gceos",max_iter=600,tol=1e-8,init=prev), error=function(e) NULL)
    if (is.null(r)) next
    mb <- (r$raffinate$x[2]*r$raffinate$L + r$extract$comp[2]*r$extract$E)/zL[2]
    if (r$status=="converged" && is.finite(mb) && abs(mb-1)<0.02) prev <- r
  }
  r <- prev; expect_false(is.null(r)); expect_equal(r$status, "converged")
  citRec <- 100*(r$raffinate$x[2]*r$raffinate$L)/zL[2]
  limExt <- 100*r$extract$comp[1]/sum(r$extract$comp[1:2])
  expect_equal(unname(citRec), 96.03, tolerance = 6)            # Diaz 2003 citral recovery
  expect_gt(unname(limExt), 97)                                 # Diaz 2003 limonene extract 98.72 mol%
})

test_that("Ganan 2011 set builds and converges the T. minuta column", {
  skip_if_not(core_ok(), "compiled core not available")
  G <- gceos_params("ganan2011")
  expect_equal(G$groups["CH2C=O", "q"], 0.88)                   # ocimenone-specific ketone surface
  expect_equal(unname(G$decomp["ocimenone", "CH2C=O"]), 1)
  expect_equal(unname(G$decomp["ocimene", "C=CH"]), 2)
  skip_on_cran()
  mk <- function(nm){g<-G$decomp[nm,];g<-g[g>0];list(groups=g,dc=G$components[nm,"dc"],Tc=G$components[nm,"Tc"])}
  B <- list(ocimene=mk("ocimene"), ocimenone=mk("ocimenone"), CO2=mk("CO2"))
  r <- gceos_column_ns_solve(N=10,T=323,P=mpa2atm(9),comps=B,FL=1,zL=c(0.55,0.45,0),
       FV=200,zV=c(0,0,1),feedL=5,reflux=1.0,solvent="CO2",model=G,max_iter=600,tol=1e-8)
  expect_equal(r$status, "converged")
  mtExt <- 100*r$extract$comp[1]/sum(r$extract$comp[1:2])
  expect_gt(unname(mtExt), 95)                                  # MT (ocimene) extract purity (Ganan 96.0)
})

# -----------------------------------------------------------------------------
# VLLE COLUMN speedup : near-miscible system resolves quickly to single_phase
# -----------------------------------------------------------------------------
test_that("VLLE column resolves a near-miscible squalene column quickly", {
  skip_if_not(core_ok(), "compiled core not available"); skip_on_cran()
  sq <- list(groups = c(CH3=8, CH2=10, `CH=C`=6), dc = 9.244, Tc = 838.1)
  g_to <- gceos_decompose("CCCCCCCC/C=C\\CCCCCCCC(=O)OCC(OC(=O)CCCCCCC/C=C\\CCCCCCCC)COC(=O)CCCCCCC/C=C\\CCCCCCCC")
  to <- list(groups = g_to, dc = gceos_dc_ester(g_to, w = 3), Tc = 950)
  comps <- list(squalene = sq, triolein = to, CO2 = list(groups=c(CO2=1),dc=3.1289,Tc=304.2))
  t0 <- Sys.time()
  r <- gceos_column_vlle(N=8, T=333, P=mpa2atm(25), comps=comps, FL=1, zL=c(0.706,0.294,0),
       FV=80, zV=c(0,0,1), feedL=4, reflux=1, solvent="CO2", model="gceos-hmw",
       max_iter=60, force=TRUE, n_homotopy=3)
  dt <- as.numeric(Sys.time() - t0, units = "secs")
  expect_equal(r$status, "single_phase")   # squalene/triglyceride is miscible at 25 MPa/333 K
  expect_lt(dt, 60)                         # early-exit: seconds, not minutes
})
