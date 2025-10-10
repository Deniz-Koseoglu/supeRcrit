#FUNCTION 1
test_that("Full Factorial Design (FFD) generation works...", {
  sink(nullfile())
   doe_res <- doe_ffd(levels = 3,
                      factors = 3,
                      cpts = 3,
                      fnames = c("Pressure", "Temperature", "Flow"),
                      flims = list(c(100, 300), c(35, 65), c(2, 4)))
   sink()
   expect_named(doe_res, c("doe", "description"))
})

#FUNCTION 2
test_that("Fractional Factorial Design (FFD) generation works...", {
 #Fractional Factorial (var. 3, 2^5-2)
  sink(nullfile())
  doe_res <- doe_frfd(factors = 5,
                     p = 2,
                     cpts = 0,
                     aliasing = "default",
                     fnames = c("Pressure", "Temperature", "Flow", "Time", "PartSize"),
                     flims = list(c(100, 300), c(35, 65), c(2, 4), c(90, 240), c(0.2, 1.0)))
  sink()
  expect_named(doe_res, c("doe", "description"))
})

#FUNCTION 3
test_that("Central Composite Design (CCD) generation works...", {
  sink(nullfile())
  #Central Composite Circumscribed for 4 factors (standard CCD)
  doe_res <- doe_ccd(design = "CCC",
                     levels = 5,
                     factors = 3,
                     cpts = 6,
                     fnames = c("Pressure", "Temperature", "Co-Solvent"),
                     flims = list("hard"=c(100, 345), "hard"=c(35, 75), c(2, 8)))
  sink()
  expect_named(doe_res, c("doe", "description"))
})

#FUNCTION 4
test_that("Box Behnken Design (BBD) generation works...", {
  sink(nullfile())
   #Box-Behnken Design for 3 factors with 2 additional center points
   doe_res <- doe_bbd(factors = 3,
                      cpts = 2,
                      fnames = c("Pressure", "Temperature", "Flow"),
                      flims = list(c(100, 300), c(35, 65), c(2, 4)))
  sink()
  expect_named(doe_res, c("doe", "description"))
})

#FUNCTION 5
test_that("Taguchi Design (TD) generation works...", {
  sink(nullfile())
  #Taguchi Design for 3 levels and 3 factors
   doe_res <- doe_tm(levels = 3,
                     factors =3,
                     fnames = c("Pressure", "Temperature", "Flow"),
                     flims = list(c(100, 300), c(35, 65), c(2, 4)))
  sink()
  expect_named(doe_res, c("doe", "description"))
})
