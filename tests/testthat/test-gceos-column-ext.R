# Advanced column solvers and process tools (gceos_column_ext.R) + dc tools (gceos_dc.R)

core_ok <- function() tryCatch({
  gceos_ar_rt(300, 1e7, 1, 3.3, 1.261, matrix(1,1,1), matrix(5e5,1,1),
              matrix(0,1,1), matrix(0,1,1), 82.06); TRUE
}, error = function(e) FALSE)

diaz_cps <- function() {
  diaz <- gceos_params("diaz2005")
  mk <- function(nm) { g <- diaz$decomp[nm, ]; g <- g[g > 0]
    list(groups = g, dc = diaz$components[nm, "dc"], Tc = diaz$components[nm, "Tc"]) }
  list(diaz = diaz, cps = list(limonene = mk("limonene"), linalool = mk("linalool"), CO2 = mk("CO2")))
}

#FUNCTION A: dc-from-structure correlation + lumping (#8)
test_that("UNIFAC vdW volume and ester d_c correlation match Espinosa 2002...", {
  skip_if_not(core_ok(), "core not available")
  g18 <- gceos_decompose("CCCCCCCCCCCCCCCCCC(=O)OCC", "gceos")   # ethyl stearate C18:0
  expect_equal(gceos_vdw_volume(g18), 14.269, tolerance = 1e-3)  # Espinosa Table 3
  g14 <- gceos_decompose("CCCCCCCCCCCCCC(=O)OC", "gceos")        # methyl myristate
  expect_equal(gceos_vdw_volume(g14), 10.897, tolerance = 1e-3)
  dc <- gceos_dc_ester(g18)
  expect_true(dc > 7.5 && dc < 8.5)                              # Espinosa Fig.1 ~8.2
  # lumping: two C18 esters merge to one pseudo-component, z sums, dc averages
  c1 <- list(groups = g18, dc = 8.0, Tc = 776); c2 <- list(groups = g18, dc = 8.2, Tc = 784)
  L <- gceos_lump(list(a = c1, b = c2), c(0.6, 0.4), key = c("C18", "C18"))
  expect_equal(unname(L$z["C18"]), 1.0)
  expect_equal(unname(L$comps[["C18"]]$dc), 0.6*8.0 + 0.4*8.2, tolerance = 1e-9)
})

#FUNCTION B: S/F continuation reaches a stiff target (#7)
test_that("gceos_column_continuation walks S/F to a target via warm-starting...", {
  skip_if_not(core_ok(), "core not available"); skip_on_cran()
  d <- diaz_cps()
  r <- gceos_column_continuation(N = 24, T = 334.06, P = 10e6/101325, comps = d$cps,
         FL = 1, zL = c(0.9823, 0.0177, 0), zV = c(0, 0, 1), target = 300,
         reflux = 2.27, var = "SF", feedL = 10, model = d$diaz, n_steps = 10)
  expect_true(r$converged)
  expect_equal(unname(tail(r$path[, "step"], 1)), 300, tolerance = 1e-6)
  expect_gt(r$recovery[["limonene"]], r$recovery[["linalool"]])  # physical separation
})

#FUNCTION C: VLLE check + fast path reduces to two-phase (#5)
test_that("VLLE column reduces to the two-phase answer when no second liquid...", {
  skip_if_not(core_ok(), "core not available"); skip_on_cran()
  d <- diaz_cps()
  v <- gceos_column_vlle(N = 20, T = 334.06, P = 10e6/101325, comps = d$cps,
         FL = 1, zL = c(0.9823, 0.0177, 0), FV = 120, zV = c(0, 0, 1),
         feedL = 10, reflux = 2.27, model = d$diaz)
  expect_true(v$converged)
  expect_true(all(v$nphase[!is.na(v$nphase)] <= 2L))             # CO2/limonene/linalool: no LLE
  expect_gt(v$recovery[["limonene"]], 0)
})

#FUNCTION C2: stability check detects a genuine three-phase (V-L-L) stage (#5)
test_that("VLLE check flags a real water+hexane+CO2 three-phase stage...", {
  skip_if_not(core_ok(), "core not available"); skip_on_cran()
  wc <- list(water  = list(groups = c(H2O = 1), dc = 2.0, Tc = 647.1),
             hexane = list(groups = c(CH3 = 2, CH2 = 4), dc = 4.4, Tc = 507.6),
             CO2    = list(groups = c(CO2 = 1), dc = 3.1289, Tc = 304.2))
  fl <- gceos_flash3_solve(313, 5e6/101325, c(0.4, 0.3, 0.3), wc, "gcaeos")
  expect_equal(fl$nphase, 3L)                       # genuine V-L-L
  # a minimal one-stage column at that bulk composition -> flagged three-phase
  col <- list(x = matrix(c(0.4, 0.3, 0.3), 1, 3), y = matrix(c(0.4, 0.3, 0.3), 1, 3),
              L = 1, V = 1)
  chk <- gceos_column_vlle_check(col, 313, 5e6/101325, wc, "gcaeos")
  expect_true(chk$vlle[1])
})

#FUNCTION D: energy-balance column produces a physical T gradient (#4)
test_that("gceos_column_energy solves a non-isothermal temperature profile...", {
  skip_if_not(core_ok(), "core not available"); skip_on_cran()
  d <- diaz_cps()
  e <- gceos_column_energy(N = 16, P = 10e6/101325, comps = d$cps, FL = 1,
         zL = c(0.9823, 0.0177, 0), FV = 100, zV = c(0, 0, 1), T_init = 334.06,
         feedL = 8, reflux = 2.27, model = d$diaz, max_outer = 12)
  expect_length(e$T, 16)
  expect_gt(max(e$T) - min(e$T), 0.5)        # an adiabatic gradient develops
  expect_true(all(is.finite(e$T)) && all(e$T > 250 & e$T < 400))
})

#FUNCTION E: process optimization improves the objective (#9)
test_that("gceos_column_optimize tunes S/F to improve recovery...", {
  skip_if_not(core_ok(), "core not available"); skip_on_cran()
  d <- diaz_cps()
  obj <- function(r) r$recovery[["limonene"]]
  op <- gceos_column_optimize(N = 18, comps = d$cps, FL = 1, zL = c(0.9823, 0.0177, 0),
          zV = c(0, 0, 1), objective = obj, vars = list(sf = c(60, 250)),
          fixed = list(pres_bar = 100, temp = 334.06, reflux = 2.27), feedL = 8,
          model = d$diaz, control = list(maxit = 10))
  expect_true(op$value > 0 && op$value <= 1)
  # more solvent -> at least as good as the low-S/F end
  lowSF <- gceos_column_continuation(18, 334.06, 100e5/101325, d$cps, FL = 1,
             zL = c(0.9823, 0.0177, 0), zV = c(0, 0, 1), target = 60, reflux = 2.27,
             feedL = 8, model = d$diaz, n_steps = 6)
  expect_gte(op$value + 1e-6, lowSF$recovery[["limonene"]])
})
