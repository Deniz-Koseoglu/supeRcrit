# GCM-style melting-point predictor (xgboost on Bradley ~24k). Errors ~ model RMSE (42 K).
test_that("gcm_mp predicts melting point from SMILES", {
  p <- gcm_mp(c("c1ccc2ccccc2c1", "Cn1cnc2c1c(=O)n(C)c(=O)n2C"))   # naphthalene, caffeine
  expect_length(p, 2L)
  expect_true(all(p > 250 & p < 600))                # plausible Tm (K)
  expect_lt(abs(p[1] - 353), 40)                     # naphthalene exp 353 K
  expect_lt(abs(p[2] - 509), 40)                     # caffeine exp 509 K
  expect_equal(gcm_mp("c1ccc2ccccc2c1", units = "C"), gcm_mp("c1ccc2ccccc2c1") - 273.15, tolerance = 1e-6)
  expect_true(is.na(gcm_mp("not-a-smiles")))
  expect_match(attr(p, "model"), "xgboost")
})

test_that("gcm_hvap predicts enthalpy of vaporization from SMILES", {
  v <- gcm_hvap(c("CCCCCC", "Cc1ccccc1"))          # hexane, toluene
  expect_length(v, 2L)
  expect_true(all(v > 15 & v < 90))                # plausible dHvap (kJ/mol)
  expect_lt(abs(v[1] - 31.6), 12)                  # hexane ~ 31.6 kJ/mol
  expect_true(is.na(gcm_hvap("not-a-smiles")))
  expect_match(attr(v, "model"), "kJ/mol")
})
