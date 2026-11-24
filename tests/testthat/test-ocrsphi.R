test_that("ocrsphi_psat reproduces the openCOSMO-RS-Phi methanol reference", {
  prof <- system.file("extdata", "solubility", "ocrsphi_profiles", "methanol.orcacosmo",
                      package = "supeRcrit")
  skip_if(prof == "", "bundled methanol.orcacosmo fixture not found")

  # methanol EoS parameters c(b_i, delta_m0, delta_mT, b_h) from the upstream example
  par <- c(56.08186814344738, 1.3518314651065213, 3.3225493224786162, 8.713083189565843)
  res <- ocrsphi_psat(prof, par, temperatures = c(280, 298.15, 320),
                      molar_mass = 32.04 / 1000)

  # bit-identical to the upstream pybind11 build (Markgraf 2026): 6051.9 / 16774.5 / 48376.1 Pa
  expect_equal(res$Psat_Pa, c(6051.9, 16774.5, 48376.1), tolerance = 1e-4)
  # saturated liquid density ~ real methanol (792 kg/m^3 at 298 K)
  expect_true(all(res$rho_liquid_kgm3 > 760 & res$rho_liquid_kgm3 < 810))
  # Psat monotonically increases with temperature
  expect_true(all(diff(res$Psat_Pa) > 0))
})
