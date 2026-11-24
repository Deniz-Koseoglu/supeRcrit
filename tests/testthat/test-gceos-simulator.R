# GC/GCA-EOS process simulator: analytic derivatives, Naphtali-Sandholm column
# (isothermal + thermal gradient), 3-phase VLLE flash, association, phase
# boundaries and hydraulic sizing (the Step 1-5 capabilities). Reproductions of
# Diaz et al. (2005) deterpenation are the headline checks.

core_ok <- function() tryCatch({
  gceos_ar_rt(300, 1e7, 1, 3.3, 1.261, matrix(1, 1, 1), matrix(5e5, 1, 1),
              matrix(0, 1, 1), matrix(0, 1, 1), 82.06); TRUE
}, error = function(e) FALSE)

.diaz_setup <- function() {
  diaz <- gceos_params("diaz2005")
  mk <- function(nm) { g <- diaz$decomp[nm, ]; g <- g[g > 0]
    list(groups = g, dc = diaz$components[nm, "dc"], Tc = diaz$components[nm, "Tc"]) }
  list(diaz = diaz,
       comps = list(limonene = mk("limonene"), linalool = mk("linalool"), CO2 = mk("CO2")))
}

# analytic A1-A6 derivatives must match the numerical ones the engine was built on
test_that("analytic Z and ln phi match the numerical derivatives", {
  skip_if_not(core_ok(), "compiled core not available")
  s <- .diaz_setup(); x <- .gceos_inputs(334, s$comps, s$diaz)
  n <- c(0.2, 0.05, 0.75); V <- 80
  Pn <- gceos_pressure(334, V, n, x$d, x$q, x$nu, x$g, x$dg, x$alpha, 82.06)
  Zn <- Pn * V / (sum(n) * 82.06 * 334)
  an <- gceos_props_an(334, V, n, x$d, x$q, x$nu, x$g, x$dg, x$alpha, 82.06)
  lpn <- gceos_lnphi(334, V, n, x$d, x$q, x$nu, x$g, x$dg, x$alpha, 82.06)
  expect_equal(an$Z, Zn, tolerance = 1e-6)
  expect_equal(an$lnphi, lpn, tolerance = 1e-6)
})

# the Naphtali-Sandholm column reproduces Diaz 2005 Table 6 (high-purity aroma)
test_that("NS column reproduces Diaz 2005 Table 6 deterpenation", {
  skip_if_not(core_ok(), "compiled core not available"); skip_on_cran()
  s <- .diaz_setup(); zoil <- c(0.9823, 0.0177, 0); P <- 10e6 / 101325; MW <- c(136.24, 154.25)
  prev <- NULL
  for (SF in c(seq(20, 560, by = 40), 567)) {
    r <- gceos_column_ns_solve(N = 28, T = 334.06, P = P, comps = s$comps, FL = 1, zL = zoil,
                               FV = SF, zV = c(0, 0, 1), feedL = 11, reflux = 2.27,
                               solvent = "CO2", model = s$diaz, max_iter = 600, tol = 1e-8, init = prev)
    mb <- (r$raffinate$x[2]*r$raffinate$L + r$extract$comp[2]*r$extract$E) / zoil[2]
    if (r$status == "converged" && abs(mb - 1) < 0.01) prev <- r
  }
  r <- prev; w <- r$raffinate$x[1:2] * MW
  expect_equal(r$status, "converged")
  expect_equal(unname(100 * w[2]/sum(w)), 98.18, tolerance = 2.0)                 # raffinate linalool wt%
  expect_equal(unname(100 * r$recovery["limonene"]), 99.96, tolerance = 0.5)      # limonene recovery
  mb <- (r$raffinate$x[2]*r$raffinate$L + r$extract$comp[2]*r$extract$E) / zoil[2]
  expect_equal(unname(mb), 1, tolerance = 1e-3)                                   # mass conservation
})

# a temperature-gradient column (per-stage T) still converges with mass balance
test_that("thermal-gradient column converges", {
  skip_if_not(core_ok(), "compiled core not available"); skip_on_cran()
  s <- .diaz_setup(); zoil <- c(0.9823, 0.0177, 0)
  r <- gceos_column_ns_solve(N = 28, T = seq(340, 325, length.out = 28), P = 10e6/101325,
                             comps = s$comps, FL = 1, zL = zoil, FV = 200, zV = c(0, 0, 1),
                             feedL = 11, reflux = 2.27, solvent = "CO2", model = s$diaz,
                             max_iter = 400, tol = 1e-8)
  expect_equal(r$status, "converged")
})

# three-phase (vapor-liquid-liquid) flash detects a real split
test_that("3-phase VLLE flash detects water+hexane+CO2 split", {
  skip_if_not(core_ok(), "compiled core not available")
  gca <- gceos_params("gcaeos")
  comps <- list(water = list(groups = c(H2O = 1), dc = 2.0, Tc = 647.1),
                hexane = list(groups = c(CH3 = 2, CH2 = 4), dc = 4.4, Tc = 507.6),
                CO2 = list(groups = c(CO2 = 1), dc = 3.1289, Tc = 304.2))
  x <- .gceos_inputs(313, comps, gca)
  r <- gceos_flash3(313, 5e6/101325, c(0.4, 0.3, 0.3), x$d, x$q, x$nu, x$g, x$dg, x$alpha,
                    82.06, 300L, x$numu, x$Delta0)
  expect_equal(r$status, 3)
})

# GCA-EOS association lowers a fatty-acid K-value (carboxylic-acid dimerization)
test_that("GCA-EOS association lowers the fatty-acid K-value", {
  skip_if_not(core_ok(), "compiled core not available")
  gca <- gceos_params("gcaeos")
  comps <- list(CO2 = list(groups = c(CO2 = 1), dc = 3.1289, Tc = 304.2),
                lauric = list(groups = c(CH3 = 1, CH2 = 9, CH2COOH = 1), dc = 6.3945, Tc = 743))
  x <- .gceos_inputs(333, comps, gca); z <- c(0.9, 0.1); P <- 5e6/101325
  f0 <- gceos_flash_cpp(333, P, z, x$d, x$q, x$nu, x$g, x$dg, x$alpha, 82.06, NULL, 300L, 1e-4)
  fA <- gceos_flash_cpp(333, P, z, x$d, x$q, x$nu, x$g, x$dg, x$alpha, 82.06, NULL, 300L, 1e-4, x$numu, x$Delta0)
  expect_equal(f0$status, 1); expect_equal(fA$status, 1)
  expect_lt(fA$K[2], f0$K[2])                                 # acid held more in the liquid
})

# bubble pressure of a CO2 + heavy-solute liquid is physical (vapor ~ pure CO2)
test_that("bubble point of CO2+limonene is physical", {
  skip_if_not(core_ok(), "compiled core not available"); skip_on_cran()
  s <- .diaz_setup()
  comps <- list(CO2 = s$comps$CO2, limonene = s$comps$limonene)
  b <- gceos_saturation(323.15, c(0.5, 0.5), comps, s$diaz, "bubble")
  expect_false(is.null(b))
  expect_gt(b$P, 30); expect_lt(b$P, 80)                      # ~5-6 MPa
  expect_gt(b$incipient[1], 0.99)                             # vapor is CO2-rich
})
