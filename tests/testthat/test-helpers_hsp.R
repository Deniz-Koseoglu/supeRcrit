#FUNCTION 1
test_that("Overlaying SMARTS substructures on chemical structures works...", {
  sink(nullfile())
   smarts <- c("[CX4H3]", "[CX3H2]", "[$([!R;#6X3H0]);!$([!R;#6X3H0]=[#8])]", "[R;CX4H2]", "[R;CX4H]",
   "[R;CX3H1,cX3H1]", "[$([R;#6X3H0]);!$([R;#6X3H0]=[#8])]")
   mol <- mol_find(c("CC1=CCC(CC1)C(=C)C", "5989-54-8", "Limonene"))
   res <- plot_gcm(mol, smarts, draw_plot = TRUE)
   sink()

   expect_true(inherits(res, "array"))
})

#FUNCTION 2
test_that("GCM-based estimation of boiling point and other parameters works...", {

  sink(nullfile())
  #Define solute data
   mol <- c("CC1=C(C(CCC1)(C)C)C=CC(=CC=CC(=CC=CC=C(C)C=CC=C(C)C=CC2=C(CCCC2(C)C)C)C)C",
   "7235-40-7", "Beta-carotene")

   #Estimate parameters by GCMs
   estres <- est_gcm(mol, "JR", "JR", "SP08")
   sink()

   expect_named(estres, c("solute_data", "pares", "visres"))
})

#FUNCTION 3
test_that("Standalone HSP optimization works...", {
  sink(nullfile())
   #Using limonene as an example
   lim_cp <- 628.4626
   lim_hsp <- c(dD = 14.00539, dP = 0, dHB = 1.7012, dP_low = 2.0448, dHB_low = 1.2838)
   modif <- c("Methanol", "Ethanol", "Hexane")
   res <- hsp_optim(lim_cp, lim_hsp, modif)
   sink()

   expect_named(res, c("Modifiers", "Volume_Fraction", "SoluteHSP_vs_Temp", "SolventBlend_HSPs",
                       "Ra", "Miscib_Enhancement", "Best_Modifier", "Modifier_Ranking"))
})

#FUNCTION 4
test_that("Estimation of binary and ternary mixture critical parameters works...", {
  sink(nullfile())
  res <- iscrit_gen(solv = c("CO2", "Hexane", "Methanol"),
                    fracs = c(0.216, 0.196, 0.588),
                    pres = 400,
                    temp = 45,
                    units = "mol")

  res2 <- iscrit_etoh(0.8, "mol", pres = 400, temp = 45, "redlich")
  res3 <- iscrit_demo()
  sink()

  expect_named(res, c("fractions", "results", "results_df", "global_means", "statement"))
  expect_named(res2, c("results", "statement"))
  expect_named(res3, c("data", "plots"))
})
