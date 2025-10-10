test_that("Kinetic BIC modeling works...", {
  sink(nullfile())
  bic_res <- bicmod(oec = sfex[[2]][["data"]],
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
                    draw = FALSE,
                    units = c(flow = "none", resp = "g"),
                    modtype = "all")
  sink()
  expect_named(bic_res, c("sim", "ct", "cmp", "plots", "data", "input", "call"))
})
