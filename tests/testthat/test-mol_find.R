test_that("Finding molecular descriptors works...", {
  expect_named(mol_find(c("CC1=CCC(CC1)C(=C)C", "5989-54-8", "Limonene")), c("IDs", "Molfile"))
})
