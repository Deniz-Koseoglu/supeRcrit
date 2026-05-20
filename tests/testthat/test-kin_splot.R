test_that("multiplication works", {

  sink(nullfile())

  #Prepare input data
  bicdt <- list(sfex[["rizza1"]][["data"]],
                  sfex[["rizza2"]][["data"]],
                  sfex[["rizza3"]][["data"]])

                  #Estimates of extractable solute fraction
                  cuvals <- c(0.165, 0.21, 0.08)

                  #Generate several BIC models
                  biclst <- list()
                  for(i in seq_along(bicdt)) {
                    biclst[[i]] <- bicmod(oec = bicdt[[i]],
                                          oec_vars = c(x = "Time_min", y = "Yield_g", slv = "Solvent_mL"),
                                          pars = c(pres = 300,
                                                   cu = cuvals[i],
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
                                          etoh_frac = 0.06, #For when CO2 flow not provided but 'etoh' is >0
                                          ro_co2 = NA,
                                          tmax = NA,
                                          qmax = NA,
                                          cumulative = FALSE,
                                          mass_flow = FALSE,
                                          draw = FALSE,
                                          units = c(flow = "none", resp = "g"),
                                          modtype = "all") #"cu"
                  }

                  #Summarize the results
                  bic_summary <- kin_splot(m = biclst,
                                           which_mod = "cmp3",
                                           mvars = c(x = "x", y = "y"), #c("x", "y")
                                           leglab = c("S. obliquus", "N. salina", "C. protothecoides"),
                                           axlab = c(title = "BIC models", x = "q (kg/kg)", y = "e (kg/kg)"))
                  sink()

                  expect_named(bic_summary, c("plot", "summary", "model_type"))
})
