#' Launch the supeRcrit Shiny Dashboard
#'
#' This function launches the Shiny dashboard for comprehensive analysis of supercritical CO2
#' and subcritical water processes, including Cost of Manufacturing (COM) analysis,
#' Design of Experiments (DOE) for process optimization, and experimental data analysis.
#'
#' @param app_name The name of the app to launch. Supported apps are "com_analysis", "doe_design", and "doe_analysis".
#' @param ... Additional arguments passed to \code{\link[shiny]{runApp}}.
#'
#' @return Runs the specified Shiny application.
#' @export
#'
#' @examples
#' \dontrun{
#' # Launch the Cost of Manufacturing analysis dashboard
#' run_app("com_analysis")
#'
#' # Launch the Design of Experiments design module
#' run_app("doe_design")
#'
#' # Launch the Design of Experiments analysis module
#' run_app("doe_analysis")
#'
#' # Launch with custom port
#' run_app("com_analysis", port = 3838)
#' }
run_app <- function(app_name = "com_analysis", ...) {
  # Check if required packages are available
  required_packages <- c("shiny", "shinydashboard", "shinyWidgets", "DT", "plotly", "dplyr", "ggplot2", "shinydashboardPlus", "jsonlite", "shiny.i18n", "rhandsontable", "rintrojs", "shinyjs")
  missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

  if(length(missing_packages) > 0) {
    stop("The following required packages are missing: ",
         paste(missing_packages, collapse = ", "),
         ". Please install them using: install.packages(c(\"",
         paste(missing_packages, collapse = "\", \""), "\"))")
  }

  # Determine the app path
  if (app_name %in% c("com_analysis", "doe_design", "doe_analysis", "kinetic_tws", "kinetic_bic")) {
    app_path <- system.file("shiny-app", package = "supeRcrit")
    if (app_path == "") {
      # If installed as a development package, try relative path
      app_path <- "inst/shiny-app"
    }
  } else {
    stop("App '", app_name, "' is not supported. Supported apps are 'com_analysis', 'doe_design', 'doe_analysis', 'kinetic_tws', and 'kinetic_bic'.")
  }

  # Check if app directory exists
  if (!dir.exists(app_path)) {
    stop("Shiny app directory not found at: ", app_path)
  }

  # Check if app.R exists
  app_file <- file.path(app_path, "app.R")
  if (!file.exists(app_file)) {
    stop("Main app file not found at: ", app_file)
  }

  # Run the app
  shiny::runApp(app_path, ...)
}
