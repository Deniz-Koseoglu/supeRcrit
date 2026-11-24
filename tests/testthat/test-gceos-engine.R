# GC-EOS thermodynamic engine (compiled Rcpp core + R glue)

# the C++ core must be loaded (package namespace in R CMD check; skip otherwise)
core_ok <- function() tryCatch({
  gceos_ar_rt(300, 1e7, 1, 3.3, 1.261, matrix(1,1,1), matrix(5e5,1,1),
              matrix(0,1,1), matrix(0,1,1), 82.06); TRUE
}, error = function(e) FALSE)

#FUNCTION 1: pure-component vapor pressure vs reference data
test_that("GC-EOS reproduces CO2 vapor pressure...", {
  skip_if_not(core_ok(), "compiled GC-EOS core not available")
  co2 <- list(groups = c(CO2 = 1), dc = 3.1289, Tc = 304.2)
  # d_c was fitted to CO2 vapor pressure, so the EoS reproduces it to ~1.5%
  expect_equal(gceos_psat(280, co2) * 1.01325, 41.6, tolerance = 0.03)  # bar, NIST
  expect_equal(gceos_psat(290, co2) * 1.01325, 53.2, tolerance = 0.03)
  expect_equal(gceos_psat(300, co2) * 1.01325, 67.1, tolerance = 0.03)
})

#FUNCTION 2: deterpenation target molecules vs experimental boiling points
test_that("GC-EOS reproduces limonene/linalool boiling points (Diaz 2005 set)...", {
  skip_if_not(core_ok(), "compiled GC-EOS core not available")
  diaz <- gceos_params("diaz2005")
  mk <- function(nm) { gc <- diaz$decomp[nm, ]; gc <- gc[gc > 0]
    list(groups = gc, dc = diaz$components[nm, "dc"], Tc = diaz$components[nm, "Tc"]) }
  # Psat at the experimental normal boiling point must be ~1 atm (<0.5%)
  expect_equal(gceos_psat(449.65, mk("limonene"), diaz), 1, tolerance = 0.005)  # limonene
  expect_equal(gceos_psat(471.15, mk("linalool"), diaz), 1, tolerance = 0.005)  # linalool
})

#FUNCTION 3: ternary VLE selectivity vs Diaz 2005 published GC-EOS
test_that("GC-EOS flash reproduces Diaz 2005 limonene/linalool selectivity...", {
  skip_if_not(core_ok(), "compiled GC-EOS core not available")
  diaz <- gceos_params("diaz2005")
  mk <- function(nm) { g <- diaz$decomp[nm, ]; g <- g[g > 0]
    list(groups = g, dc = diaz$components[nm, "dc"], Tc = diaz$components[nm, "Tc"]) }
  cps <- list(limonene = mk("limonene"), linalool = mk("linalool"), CO2 = mk("CO2"))
  z <- c(0.123, 0.027, 0.85)                       # CO2-rich feed, terpenes 82/18 mol
  sel <- function(T, Pmpa) { fl <- gceos_flash(T, Pmpa*1e6/101325, z, cps, diaz); unname(fl$K[1]/fl$K[2]) }
  # match Diaz Table 1 GC-EOS column (relative volatility) to within ~0.05
  expect_equal(sel(333, 8.8), 1.96, tolerance = 0.04)
  expect_equal(sel(353, 8.8), 1.85, tolerance = 0.04)
  expect_equal(sel(333, 9.8), 1.79, tolerance = 0.04)
})

#FUNCTION 4: critical-diameter regression
test_that("gceos_fit_dc recovers a known critical diameter...", {
  skip_if_not(core_ok(), "compiled GC-EOS core not available")
  diaz <- gceos_params("diaz2005")
  g <- diaz$decomp["limonene", ]; g <- g[g > 0]
  # fitting d_c to limonene's boiling point recovers the Diaz value (5.6027)
  dc <- gceos_fit_dc(g, Tc = 660, Tsat = 449.65, Psat = 1, model = diaz, dc_range = c(5, 6.5))
  expect_equal(dc, 5.6027, tolerance = 0.001)
})

#FUNCTION 5: counter-current column — physical deterpenation direction
test_that("gceos_column gives physically correct deterpenation...", {
  skip_if_not(core_ok(), "compiled GC-EOS core not available")
  skip_on_cran()                                  # iterative multistage solve is slow
  diaz <- gceos_params("diaz2005")
  mk <- function(nm) { g <- diaz$decomp[nm, ]; g <- g[g > 0]
    list(groups = g, dc = diaz$components[nm, "dc"], Tc = diaz$components[nm, "Tc"]) }
  cps <- list(limonene = mk("limonene"), linalool = mk("linalool"), CO2 = mk("CO2"))
  r <- gceos_column(N = 4, T = 333, P = 10e6/101325, comps = cps,
                    FL = 1, zL = c(0.82, 0.18, 0), FV = 60, zV = c(0, 0, 1),
                    model = diaz, max_iter = 250)
  expect_true(r$converged)
  org <- r$raffinate$x[c("limonene", "linalool")]; org <- org/sum(org)
  expect_gt(org[["linalool"]], 0.18)              # raffinate enriched in linalool (the aroma)
  expect_gt(r$recovery[["limonene"]], r$recovery[["linalool"]])  # limonene preferentially extracted
})

#FUNCTION 4b: critical-diameter regression against a binary VLE point
test_that("gceos_fit_dc_vle anchors d_c to a binary VLE bubble point...", {
  skip_if_not(core_ok(), "compiled GC-EOS core not available")
  skip_on_cran()                                      # nested bubble-point root-finds are slow
  co2 <- list(groups = c(CO2 = 1), dc = 3.1289, Tc = 304.2)
  lin <- gceos_decompose("CC(C)=CCCC(C)(O)C=C", "gceos")
  # Raeissi & Peters 2001: 323.15 K, x_CO2 = 0.6096, P = 7.392 MPa
  Panc <- 7.392e6 / 101325; xco2 <- 0.6096
  dc <- gceos_fit_dc_vle(lin, Tc = 630.5, T = 323.15, Pbub = Panc,
                         x_solute = 1 - xco2, solvent = co2,
                         model = "gceos", dc_range = c(4, 9))
  expect_true(dc > 4 && dc < 9)
  expect_true(abs(dc - 6.218) > 0.05)                 # differs from the boiling-point fit (~6.218)
  # the fitted d_c must reproduce the anchor bubble pressure it was fit to
  s <- gceos_saturation(323.15, c(xco2, 1 - xco2),
         list(CO2 = co2, S = list(groups = lin, dc = dc, Tc = 630.5)),
         model = "gceos", type = "bubble", Prange = c(1, 400))
  expect_equal(s$P * 101325 / 1e6, 7.392, tolerance = 0.02)   # within 2%
})

#FUNCTION 5d: high-MW set ("gceos-hmw") — Fornari Table 4; squalene solubility vs Catchpole
test_that("gceos-hmw applies Fornari Table 4 and predicts squalene + CO2 solubility...", {
  skip_if_not(core_ok(), "compiled GC-EOS core not available")
  ps <- gceos_params("gceos-hmw")
  expect_equal(ps$groups["CH=C", "gstar"], 421650)       # revised CH=C group param
  expect_equal(ps$kstar["CO2", "CH2"], 0.874)            # revised CO2-paraffin interaction
  expect_equal(ps$kstar["CO2", "CH=C"], 0.882)           # new CO2-olefin interaction
  # light hydrocarbons are unchanged ONLY where Table 4 did not touch them; CO2-CH2
  # IS changed, so gceos-hmw differs from gceos for any CO2 mixture (by design).
  expect_false(isTRUE(all.equal(ps$kstar["CO2", "CH2"], gceos_params("gceos")$kstar["CO2", "CH2"])))

  skip_on_cran()                                          # flash + saturation are slow
  atm <- 101325; co2 <- list(groups = c(CO2 = 1), dc = 3.1289, Tc = 304.2)
  sq <- list(groups = c(CH3 = 8, CH2 = 10, `CH=C` = 6), dc = 9.244, Tc = 838.1)
  # squalene solubility in CO2 at 323 K, 20 MPa: Catchpole 1998 ~ 23.8 g/kg
  fl <- gceos_flash(323, 20e6/atm, c(0.97, 0.03), list(CO2 = co2, S = sq),
                    model = "gceos-hmw", Kinit = c(3, 5e-4))
  expect_false(is.null(fl))
  y <- if (fl$y[1] >= fl$x[1]) fl$y else fl$x
  S <- unname(1000 * y[2] * 410.7 / (y[1] * 44.01))       # g squalene / kg CO2
  expect_equal(S, 23.8, tolerance = 0.20)                 # within 20% of Catchpole 1998
})

#FUNCTION 6: ideal-gas limit and physical mixture behaviour
test_that("GC-EOS gives the correct ideal-gas limit and a physical mixture...", {
  skip_if_not(core_ok(), "compiled GC-EOS core not available")
  co2 <- list(groups = c(CO2 = 1), dc = 3.1289, Tc = 304.2, n = 1)
  e <- gceos_eval(310, 1e7, list(co2))          # very large volume -> ideal gas
  expect_lt(abs(e$Z - 1), 1e-3)
  expect_lt(abs(e$ar_rt), 1e-3)
  # binary mixture evaluates to a finite, positive pressure in the vapor region
  m <- gceos_eval(330, 2000, list(
    list(groups = c(CO2 = 1), dc = 3.1289, Tc = 304.2, n = 0.7),
    list(groups = c(CH3 = 1, cyCH2 = 3, cyCH = 1, `CH=C` = 1, `CH2=C` = 1), dc = 5.6027, Tc = 660, n = 0.3)))
  expect_true(is.finite(m$P) && m$P > 0)
  expect_length(m$lnphi, 2)
})
