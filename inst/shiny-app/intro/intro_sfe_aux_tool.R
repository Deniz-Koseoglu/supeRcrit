# SFE Auxiliary Tools Application Intro Steps

# Intro for CO2-Ethanol Critical Parameters tab
intro_steps_sfe_aux_tool_etoh <- function(ns, i18n) {
  steps <- list(
    list(
      element = paste0("#", ns("etoh_co2_frac_wrapper")),
      intro = i18n$t("Set the CO2 molar fraction in the CO2-Ethanol mixture for critical parameter calculations."),
      position = "auto"
    ),
    list(
      element = paste0("#", ns("etoh_pres_wrapper")),
      intro = i18n$t("Specify the pressure for CO2-Ethanol critical parameter calculations."),
      position = "auto"
    ),
    list(
      element = paste0("#", ns("etoh_temp_wrapper")),
      intro = i18n$t("Set the temperature for CO2-Ethanol critical parameter calculations."),
      position = "auto"
    ),
    list(
      element = paste0("#", ns("etoh_method_wrapper")),
      intro = i18n$t("Choose the calculation method: Chueh, Redlich, or both for comparison."),
      position = "auto"
    ),
    list(
      element = paste0("#", ns("calc_etoh_crit_wrapper")),
      intro = i18n$t("Calculate critical parameters for the CO2-Ethanol mixture."),
      position = "auto"
    ),
    list(
      element = paste0("#", ns("run_etoh_demo_wrapper")),
      intro = i18n$t("Display a validation plot showing CO2-Ethanol critical parameter predictions."),
      position = "auto"
    )
  )

  list(
    steps = steps,
    "nextLabel" = i18n$t("Next"),
    "prevLabel" = i18n$t("Back"),
    "doneLabel" = i18n$t("Done"),
    "skipLabel" = i18n$t("Skip"),
    "showProgress" = TRUE,
    "showBullets" = FALSE
  )
}

# Intro for General Mixture Critical Parameters tab
intro_steps_sfe_aux_tool_gen <- function(ns, i18n) {
  steps <- list(
    list(
      element = paste0("#", ns("gen_units_wrapper")),
      intro = i18n$t("Select units for general mixture critical parameter calculations (mass or mole fractions)."),
      position = "auto"
    ),
    list(
      element = paste0("#", ns("gen_pres_wrapper")),
      intro = i18n$t("Specify the pressure for general mixture critical parameter calculations."),
      position = "auto"
    ),
    list(
      element = paste0("#", ns("gen_temp_wrapper")),
      intro = i18n$t("Set the temperature for general mixture critical parameter calculations."),
      position = "auto"
    ),
    list(
      element = paste0("#", ns("gen_tc_method_wrapper")),
      intro = i18n$t("Choose the method for calculating critical temperature of the mixture."),
      position = "auto"
    ),
    list(
      element = paste0("#", ns("gen_pc_method_wrapper")),
      intro = i18n$t("Choose the method for calculating critical pressure of the mixture."),
      position = "auto"
    ),
    list(
      element = paste0("#", ns("calc_gen_crit_wrapper")),
      intro = i18n$t("Calculate critical parameters for general solvent mixtures."),
      position = "auto"
    )
  )

  list(
    steps = steps,
    "nextLabel" = i18n$t("Next"),
    "prevLabel" = i18n$t("Back"),
    "doneLabel" = i18n$t("Done"),
    "skipLabel" = i18n$t("Skip"),
    "showProgress" = TRUE,
    "showBullets" = FALSE
  )
}
