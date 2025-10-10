test_that("Calculation of Cost of Manufacturing (COM) works...", {
  #Calculate COM for a single process
  #comin <- c(time = 180, yield = 7)

  sink(nullfile())
  #Calculate COM throughout an extraction curve and plot the results
  comin <- data.frame(time = c(5,9,11,15,19,21,23,25,30,40,60,80,100,120,140,160,180),
                      yield = c(0.80, 2.18, 2.79, 3.40, 3.86, 4.17, 4.47, 4.63, 4.93,
                                5.24, 5.70, 6.00, 6.31, 6.46, 6.77, 6.92, 7.00))

  comres <- calcom(input = comin,
                   gen = c(volex = 30, load = 9, pres = 300, temp = 40, flow = 3000,
                           extime = 30, csol_flow = 300, dilfac = 2, pr_sale = 90),
                   crm = c(bh = 155, id = 15.5, pr_mat = 0.5, pr_msol = 0.8, pr_csol = 2,
                           recp = 60, rect = 10, sept = 55),
                   cut = c(pw_main = 25, pr_kwh = 0.13, pw_dry = 2, pw_com = 1, pw_evap = 13.3,
                           cap_dry = 4.16, cap_com = 20, cap_evap = 60),
                   col = c(oper = 1, whr = 8, shifts = 3, wage = 940, wdays = 24),
                   fci = c(capex = 150000, maint = 250, other = 1210),
                   auxpr = c(oil = 1.52),
                   auxfr = c(oil = 1),
                   pltlab = "Time (min)")
  sink()
  expect_named(comres, c("input", "output", "simple_output", "rm_state", "plots", "extra", "call"))
})
