# General Application Tour Intro Steps
intro_steps_general <- function(ns, i18n) {
  list(
    steps = list(
      list(
        element = "#main_menu",
        intro = i18n$t("This is the main navigation menu for the supeRcrit application, allowing you to switch between different modules."),
        position = "right"
      ),
      list(
        element = "a[data-value='com_analysis']",
        intro = i18n$t("Access the Cost of Manufacturing (COM) Analysis module to evaluate the economic feasibility of supercritical CO2 and subcritical water processes."),
        position = "right"
      ),
      list(
        element = "a[data-value='doe_design']",
        intro = i18n$t("Navigate to the Design of Experiments (DOE) Design module to create various experimental designs for process optimization."),
        position = "right"
      ),
      list(
        element = "a[data-value='doe_analysis']",
        intro = i18n$t("Use the DOE Analysis module to analyze experimental data, build models, and optimize your processes based on statistical insights."),
        position = "right"
      ),
      list(
        element = "#selected_language",
        intro = i18n$t("Select your preferred language for the application interface. Available languages include English, Russian, and Turkish."),
        position = "bottom"
      )
    ),
    "nextLabel" = i18n$t("Next"),
    "prevLabel" = i18n$t("Back"),
    "doneLabel" = i18n$t("Done"),
    "skipLabel" = i18n$t("Skip"),
    "showProgress" = TRUE,
    "showBullets" = FALSE
  )
}
