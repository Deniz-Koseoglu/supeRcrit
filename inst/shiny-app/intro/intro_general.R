# General Application Tour Intro Steps
# Triggered by the Intro (?) button in the top-right header, next to the AltraFlora logo.
# Covers: 4 main modules, Settings button, Language selector.
intro_steps_general <- function(ns, i18n) {
  list(
    steps = list(
      list(
        element = ".sidebar-menu > li:nth-child(1)",
        intro = i18n$t("Cost of Manufacturing (COM) Analysis: evaluate the economic feasibility of supercritical CO2 and subcritical water extraction processes, including raw material costs, utilities, labor, and capital investment."),
        position = "right"
      ),
      list(
        element = ".sidebar-menu > li:nth-child(2)",
        intro = i18n$t("SFE Co-Solvent Selection: characterize target solutes using Group Contribution Methods, optimize co-solvent selection via Hansen Solubility Parameters, compare miscibility across conditions, and calculate mixture critical parameters."),
        position = "right"
      ),
      list(
        element = ".sidebar-menu > li:nth-child(3)",
        intro = i18n$t("Design of Experiments (DOE): create experimental designs (Box-Behnken, CCD, Full/Fractional Factorial, Taguchi), analyze response surfaces, and perform multi-objective desirability optimization."),
        position = "right"
      ),
      list(
        element = ".sidebar-menu > li:nth-child(4)",
        intro = i18n$t("Kinetic Modeling: fit extraction kinetics using the Two-Site (TWS) model for subcritical water or the Broken and Intact Cells (BIC) model for supercritical CO2, with built-in density calculators."),
        position = "right"
      ),
      list(
        element = "#global_settings_button",
        intro = i18n$t("Settings: configure default values for process parameters, flow rates, vessel dimensions, and other module-specific defaults. Settings are saved and restored across sessions."),
        position = "bottom"
      ),
      list(
        element = "#language_picker_wrapper",
        intro = i18n$t("Language: switch the application interface between English, Russian, and Turkish."),
        position = "bottom"
      )
    ),
    "nextLabel" = i18n$t("Next"),
    "prevLabel" = i18n$t("Back"),
    "doneLabel" = i18n$t("Done"),
    "skipLabel" = i18n$t("Skip"),
    "showProgress" = TRUE,
    "showBullets" = FALSE,
    "exitOnOverlayClick" = FALSE
  )
}
