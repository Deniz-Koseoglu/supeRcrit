test_that("Finding molecular descriptors works...", {
  result <- mol_find(c("CC1=CCC(CC1)C(=C)C", "5989-54-8", "Limonene"))
  expect_true(all(c("IDs", "Molfile", "Synonyms") %in% names(result)))
  expect_true(is.character(result$Synonyms))
})
