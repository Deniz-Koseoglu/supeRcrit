#FUNCTION 1
test_that("Estimation of boiling point, critical parameters, HSP, and distance Ra works...", {
  sink(nullfile())
  #Retrieve molecule (beta-carotene)
  mol <- c("CC1=C(C(CCC1)(C)C)C=CC(=CC=CC(=CC=CC=C(C)C=CC=C(C)C=CC2=C(CCCC2(C)C)C)C)C",
           "7235-40-7", "Beta-carotene")

  #Assess SFE miscibility enhancement with various co-solvents
  optres <- sfe_mod(solute = mol,
                    tb = "SB_corr",
                    crit = "NL07_robust",
                    hsp = "SP12")
  sink()
  expect_named(optres, c("solute_ids", "parameters", "sfe", "gcm_vis", "call"))
})

#FUNCTION 2
test_that("Inter-solute comparison of HSP-based miscibility enhancement works...", {
  sink(nullfile())
  #Get information about solute 1 (beta-carotene)
  mol1 <- c("CC1=C(C(CCC1)(C)C)C=CC(=CC=CC(=CC=CC=C(C)C=CC=C(C)C=CC2=C(CCCC2(C)C)C)C)C",
            "7235-40-7", "Beta-carotene")

  #Get information about solute 2 (xanthohumol)
  mol2 <- c(hspex[24,c("SMILES", "CAS", "Name")])

  #Compare miscibility enhancement with ethanol as a co-solvent
  gcm_comp <- miscomp(sols = list(mol1, mol2))
  gcm_comp2 <- miscomp(sols = list(mol1, mol2), modif = c("Ethanol", "Water"), modfracs = 50)
  sink()

  expect_named(gcm_comp, c("data", "plots"))
  expect_named(gcm_comp2, c("data", "plots"))
})

test_that("Comparing GCM-based parameter estimation methods works...", {

  sink(nullfile())
  #Linoleic acid
  mol2 <- c("CCCCCC=CCC=CCCCCCCCC(=O)O", "60-33-3", "Linoleic Acid")
  res <- compare_gcm(mol2)
  sink()

  expect_true(is.data.frame(res))
})
