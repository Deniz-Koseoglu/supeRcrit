#FUNCTION 1
test_that("Adding star points to DOE works...", {
  sink(nullfile())
  #Load data
   doe_base <- load_internal("doe_base")

   #Get experimental design data.frame
   doe_df <- doe_base[["FD_3"]]

   #Add center points
   res <- add_stars(doe_df, 3, "CCC")
   sink()

   expect_true(nrow(res)==33)
})

#FUNCTION 2
test_that("Adding center points to DOE works...", {
  sink(nullfile())
   #Load data
   doe_base <- load_internal("doe_base")

   #Get experimental design data.frame
   doe_df <- doe_base[["FD_3"]]

   #Add center points
   res <- add_cpts(doe_df, 3)
   sink()

   expect_true(nrow(res)==30)
})

#FUNCTION 3
test_that("Predicting values using built RSM models works...", {
  sink(nullfile())
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

  #Predict response for new factor values
  new_preds <- predict_doe(doe_optres, newdata = c(A = -0.67, B = 0, C = 0.67), coded = TRUE)
  sink()

  expect_named(new_preds, c("summary", "predictions"))
})
