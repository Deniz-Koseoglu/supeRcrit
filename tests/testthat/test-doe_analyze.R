test_that("Analysis and modeling of DOE data works...", {
  sink(nullfile())
  #Maximizing extraction yield of spearmint (SFE)
  doe_optres <- doe_analyze(doe = doex[["ccd3"]][["data"]],
                            uc_facs = c("P_bar", "T_degC", "EtOH_gmin"),
                            cent_id = NA,
                            resp_var = "ExtYield",
                            time_var = "Actual_Order",
                            mod_order = 2,
                            canon_thres = "auto",
                            p_cutoff = 0.10,
                            trim_method = "both",
                            which_facs = "coded",
                            export = "none",
                            verbose = TRUE)
   sink()
   expect_named(doe_optres, c("models", "results", "plots", "statements", "call"))
})
