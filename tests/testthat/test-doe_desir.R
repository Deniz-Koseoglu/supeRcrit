test_that("Calculation of desirability function works...", {
  sink(nullfile())
  #Calculate overall desirability among 3 responses
   doe_lst1 <- load_internal("doe_lst1")

   desires <- doe_desir(mods = doe_lst1,
                        dsrng = list(CarnosicAcid_mgg = c(0,150),
                        Carnosol_mgg = c(0,65), ExtYield = c(1,7)),
                        frng = list(B = c(40,60), A = c(10,30), C = c(1,3)),
                        obj = c("max", "max", "max"),
                        dtype = "uncoded",
                        wts = rep(1,3),
                        spts = c(10,10),
                        modbase = "final",
                        optmet = "nlopt",
                        kmed = "auto",
                        export = "none",
                        silent = FALSE)
   sink()
   expect_named(desires, c("factor_lims", "response_lims", "mod_sums", "orig_data",
                           "output_data", "unique_solutions", "call"))
})
