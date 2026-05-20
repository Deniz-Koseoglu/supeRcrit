#FUNCTION 1
test_that("Estimation of suitable OEC breakpoint works...", {
  sink(nullfile())
  res <- oec_bp(input = sfex[[1]][["data"]], x = "time_min", y = "yield_g")
  sink()

  expect_true(is.numeric(res))
})

#FUNCTION 2
test_that("Estimation of CO2 density works...", {
  res <- c(bendens(300, 45), bendens(800, 70, "g/mL"))
  expect_true(length(res)==4 & is.numeric(res))
})

#FUNCTION 3
test_that("Estimation of EtOH density works...", {
  res <- c(etoh_dens(55), etoh_dens(55, 0.90, 880))
  expect_true(length(res)==4 & is.numeric(res))
})

#FUNCTION 4
test_that("Prediction of responses from built BIC models works...", {
  sink(nullfile())
  model <- bicmod(oec = sfex[[2]][["data"]],
                  oec_vars = c(x = "Time_min", y = "Yield_g", slv = "Solvent_mL"),
                  pars = c(pres = 300,
                           cu = 0.165,
                           temp = 45,
                           flow = NA,
                           mass_in = 0.5125,
                           moisture = 8.6,
                           D = 0.015,
                           L = 0.015,
                           etoh = 0.5,
                           dr = 1554,
                           dp = 0.0004,
                           n = 2),
                  opt_est = "default",
                  flowpar = c(1.01325, 25),
                  etoh_frac = 0.06, #Required when CO2 flow is not provided but 'etoh' is non-zero
                  ro_co2 = NA,
                  tmax = NA,
                  qmax = NA,
                  cumulative = FALSE,
                  mass_flow = FALSE,
                  draw = TRUE,
                  units = c(flow = "none", resp = "g"),
                  modtype = "all")

  res <- predict_bic(model, c(13, 15, 40, 63, 90, 105), units = "time", get_yields = TRUE)
  sink()

  expect_named(res, c("predictions", "unit_chart", "description"))
})
