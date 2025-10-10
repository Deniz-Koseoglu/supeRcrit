#FUNCTION 1
test_that("H2O density estimation in the subcritical region works...", {
  res <- h2o_dens(200, 40)
  expect_equal(round(res,1), 866.5)
})


#FUNCTION 2
test_that("Predictions using built KTS models work...", {
  sink(nullfile())
  model <- ktsmod(oec = swex[["duba1"]][["data"]],
                  oec_vars = c(x = "Time_min", y = "Yield_100C"),
                  pars = c(pres = 15, temp = 100, flow = 2, c0 = 77, m_in = 2, f = 0.24),
                  opt_est = "default",
                  units = c(flow = "mL/min", resp = "permille"),
                  plot_units = c(x = "q", y = "abs"), #"time" "q" "abs" "cc0"
                  cumulative = TRUE,
                  mass_flow = FALSE,
                  flowpar = rep(NA,2),
                  ro_h2o = NA,
                  tmax = NA,
                  qmax = NA,
                  optmet = "nlopt")

  res <- predict_kts(model, c(25, 50, 84, 90, 106), moisture = 5)
  sink()

  expect_named(res, c("predictions", "input", "description"))
})
