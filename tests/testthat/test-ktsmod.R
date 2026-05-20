test_that("Building two-site kinetic desorption models works...", {
  res <- ktsmod(oec = swex[["duba1"]][["data"]],
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
                draw = FALSE,
                optmet = "nlopt")
  expect_named(res, c("tws", "plots", "data", "input", "units", "call"))
})
