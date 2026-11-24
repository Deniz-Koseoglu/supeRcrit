# COSMO-SAC-Phi (CSP) solid-solubility port — regression + API tests.
# Golden values are the Rcpp port output (src/csp_core.cpp), which reproduces the validated
# Python prototype to <= 1.4e-9 relative across all 99 de Souza (2022) compounds.

test_that("csp_compounds() lists the full LVPP CSP-parametrized set", {
  cc <- csp_compounds()
  expect_s3_class(cc, "data.frame")
  expect_gte(nrow(cc), 256L)                           # >= full LVPP CSP set (grows via chemcad expansion; was 99 de Souza, 256 LVPP)
  expect_true(all(c("jcosmo", "s2", "Tm", "dHfus", "vs", "has_csp21") %in% names(cc)))
  expect_gte(sum(cc$has_csp21), 250L)                  # nearly all carry CSP(2021) params
  expect_gte(sum(!is.na(cc$Tm)), 99L)                  # at least the 99 de Souza compounds carry fusion data
})

test_that("csp_solubility reproduces validated golden values", {
  expect_equal(csp_solubility("NAPHTHALENE", 308.15, 200, model = "2021"),
               0.0105695117621, tolerance = 1e-8)
  expect_equal(csp_solubility("NAPHTHALENE", 308.15, 200, model = "2019"),
               0.0116735194255, tolerance = 1e-8)
  expect_equal(csp_solubility("CHOLESTEROL", 313.15, 130, model = "2021"),
               0.000112994012116, tolerance = 1e-8)
  expect_equal(csp_solubility("BENZOIC ACID", 318.15, 150, model = "2021"),
               0.000612380193768, tolerance = 1e-8)
})

test_that("sublimation route uses Antoine Psub", {
  expect_equal(csp_solubility("NAPHTHALENE", 308.15, 200,
                              route = "sublimation", sublim = c(31.0, 8550.0, 0.0)),
               0.00365855798495, tolerance = 1e-8)
  expect_error(csp_solubility("NAPHTHALENE", 308.15, 200, route = "sublimation"),
               "requires Antoine")
})

test_that("compound name matching and recycling", {
  # common-name (s2) match, case-insensitive
  expect_equal(csp_solubility("naphthalene", 308.15, 200, model = "2021"),
               csp_solubility("NAPHTHALENE", 308.15, 200, model = "2021"))
  # T/P recycled to common length; solubility rises with pressure
  y <- csp_solubility("NAPHTHALENE", T = 308.15, P = c(100, 150, 200))
  expect_length(y, 3L)
  expect_true(all(diff(y) > 0))
  expect_error(csp_solubility("not-a-real-compound", 308.15, 200), "Unknown compound")
})

test_that("csp_psat solves pure-compound saturation (equal fugacity)", {
  d <- supeRcrit:::csp_dataset()
  ch4 <- d$profiles[["METHANE"]]
  par <- c(49.70, 11.63, 0.526, 305.0)
  s <- csp_psat(ch4, par, c(130, 150))
  expect_equal(s$Psat[1], 3.5803, tolerance = 1e-3)   # bar
  expect_true(s$Psat[2] > s$Psat[1])                  # Psat rises with T
  expect_true(all(s$vl > 0))
  # equal fugacity holds at the returned Psat (liquid vs vapor root)
  pL <- supeRcrit:::csp_point(ch4, 130, s$vL[1], par)
  expect_equal(pL$P, s$Psat[1], tolerance = 1e-2)
})

test_that("csp_fit_params recovers generating parameters (round-trip)", {
  d <- supeRcrit:::csp_dataset()
  ch4 <- d$profiles[["METHANE"]]
  true <- c(49.70, 11.63, 0.526, 305.0)
  Tg <- seq(120, 165, by = 5)
  ref <- csp_psat(ch4, true, Tg)
  ok <- !is.na(ref$Psat)
  # deliberately poor start; grid-seed must still recover d0/dT
  f <- csp_fit_params(ch4, Tg[ok], ref$Psat[ok], vl = ref$vl[ok], model = "2019",
                      start = c(49.70, 11.63, 0.30, 200.0), fit = c("d0", "dT"))
  expect_equal(f$par[3], 0.526, tolerance = 0.02)     # d0
  expect_equal(f$par[4], 305.0, tolerance = 5)        # dT
  expect_lt(f$objective, 1e-4)
  expect_equal(f$n_feasible, sum(ok))
})

test_that("csp_sle_solvent solves solid-liquid solubility in organic solvents", {
  Tm <- 353.4; dH <- 18990; R <- 8.314462
  x_ideal <- exp((dH / (R * Tm)) * (1 - Tm / 298.15))
  # self-consistency: solute in itself -> ideal solubility (gamma2 = 1) to machine precision
  expect_equal(csp_sle_solvent("NAPHTHALENE", "NAPHTHALENE", 298.15, fusCp = NA)$x2,
               x_ideal, tolerance = 1e-4)
  # benzene is near-ideal for naphthalene (classic data ~0.296)
  expect_equal(csp_sle_solvent("NAPHTHALENE", "BENZENE", 298.15, fusCp = NA)$x2,
               0.296, tolerance = 0.02)
  # polarity ordering: alcohols << aromatic; methanol < ethanol < benzene
  xb <- csp_sle_solvent("NAPHTHALENE", "BENZENE",  298.15, fusCp = NA)$x2
  xe <- csp_sle_solvent("NAPHTHALENE", "ETHANOL",  298.15, fusCp = NA)$x2
  xm <- csp_sle_solvent("NAPHTHALENE", "METHANOL", 298.15, fusCp = NA)$x2
  expect_true(xm < xe && xe < xb)
  expect_error(csp_sle_solvent("NAPHTHALENE", "not-a-solvent", 298.15), "Unknown solvent")
})

test_that("GCM->CSP parameter pipeline produces usable parameters", {
  # Ambrose-Walton corresponding-states Psat: monotone increasing, < Pc
  ps <- csp_psat_corrstates(c(400, 500, 600), Tc = 718, Pc = 39.7, omega = 0.33)
  expect_true(all(diff(ps) > 0) && all(ps < 39.7))
  # full predictive parametrization of naphthalene from GCM critical properties
  d <- supeRcrit:::csp_dataset(); prof <- d$profiles[["NAPHTHALENE"]]
  r <- csp_params_from_gcm(prof, Tc = 717.8, Pc = 39.7, Vc = 403, Tb = 474.1, model = "2019")
  # bi/bh come from the COSMO-volume correlation -> close to LVPP geometry
  lv <- d$par19[["NAPHTHALENE"]]
  expect_equal(r$par[1], lv[1], tolerance = 0.05)     # bi within 5%
  expect_equal(r$par[2], lv[2], tolerance = 0.15)     # bh within 15%
  # resulting SC-CO2 solubility tracks the LVPP-parametrized baseline (within ~0.3 log)
  yL <- supeRcrit:::csp_solubility_raw(d$co2, prof, d$co2_par19, lv,     308.15, 150, 353.4, 18990, 111.26, dCp = NA)
  yG <- supeRcrit:::csp_solubility_raw(d$co2, prof, d$co2_par19, r$par,  308.15, 150, 353.4, 18990, 111.26, dCp = NA)
  expect_lt(abs(log10(yG / yL)), 0.3)
})
