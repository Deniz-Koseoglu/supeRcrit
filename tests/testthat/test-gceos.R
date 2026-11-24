# GC-EOS / GCA-EOS parameter sets and molecular decomposition

#FUNCTION 1: parameter-set retrieval
test_that("gceos_params returns the consolidated GC-EOS set...", {
  ps <- gceos_params("gceos")
  expect_true(is.list(ps) && nrow(ps$groups) == 60L)   # 59 Fornari groups + quaternary C
  expect_equal(dim(ps$kstar), c(60L, 60L))
  expect_equal(ps$groups["C", "q"], 0)                 # quaternary carbon: zero attractive surface
  # verified parameter spot-checks (vs Fornari 2007 rendered tables)
  expect_equal(ps$groups["COOH", "gstar"], 2933000)
  expect_equal(ps$kstar["CH3", "COH"], 0.715)
  expect_equal(ps$kstar["CH2", "COH"], 0.682)
  expect_true(isTRUE(ps$kprime["CH3", "ACH"] == 0.094))
})

#FUNCTION 2: GCA-EOS set + association layer
test_that("gceos_params returns the GCA-EOS set with association params...", {
  ga <- gceos_params("gcaeos")
  expect_identical(ga$meta$model, "GCA-EOS")
  # associating groups are RE-FITTED (differ from GC-EOS)
  expect_equal(ga$groups["H2O", "gstar"], 964720)
  # CO2-COOH cross- and COOH self-association (FFA target)
  expect_equal(ga$assoc$eps_R[ga$assoc$group_i == "CO2"  & ga$assoc$group_j == "COOH"], 1908)
  expect_equal(ga$assoc$kappa[ga$assoc$group_i == "CO2"  & ga$assoc$group_j == "COOH"], 1.0620)
  expect_equal(ga$assoc$eps_R[ga$assoc$group_i == "COOH" & ga$assoc$group_j == "COOH"], 6517.1)
  expect_identical(ga$schemes$scheme[ga$schemes$group == "COOH"], "1A")
})

#FUNCTION 3: GC-EOS molecular decomposition
test_that("gceos_decompose fragments molecules into GC-EOS groups...", {
  skip_if_not_installed("rcdk")
  cnt <- function(smi) suppressWarnings(gceos_decompose(smi, "gceos"))  # quaternary C now parameterized (q=0); decomposition clean
  expect_equal(unname(cnt("CCCCCC")[c("CH3","CH2")]), c(2L, 4L))             # n-hexane
  pa <- cnt("CCCCCCCCCCCCCCCC(=O)O")                                        # palmitic acid
  expect_equal(unname(pa[c("CH3","CH2","COOH")]), c(1L, 14L, 1L))
  expect_equal(attr(pa, "unmatched_atoms"), 0L)
  expect_equal(unname(cnt("Cc1ccccc1")[c("ACH","ACCH3")]), c(5L, 1L))       # toluene
  expect_equal(unname(cnt("CC(C)(C)C")[c("CH3(B)","C")]), c(4L, 1L))        # neopentane (bulky)
})

#FUNCTION 4: GCA-EOS molecular decomposition (model-specific acid/alcohol groups)
test_that("gceos_decompose uses GCA-EOS whole/compound groups...", {
  skip_if_not_installed("rcdk")
  cnt <- function(smi) gceos_decompose(smi, "gcaeos")
  expect_equal(unname(cnt("CC(=O)O")["CH3COOH"]), 1L)                       # acetic = 1 group
  expect_equal(unname(cnt("CCC(=O)O")[c("CH3","CH2COOH")]), c(1L, 1L))      # propanoic
  expect_equal(unname(cnt("CCO")["C2H5OH"]), 1L)                            # ethanol = 1 group
  expect_equal(unname(cnt("O=C=O")["CO2"]), 1L)
})
