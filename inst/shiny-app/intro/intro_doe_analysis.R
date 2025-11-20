# DOE Analysis Application Intro Steps - Analysis Parameters
intro_steps_analysis_params <- function(ns, i18n) {
  steps <- list(
    list(
      element = paste0("#", ns("time_var_wrapper")),
      intro = i18n$t("Select the variable representing time or run order in your experimental data."),
      position = "auto"
    ),
    list(
      element = paste0("#", ns("response_var_wrapper")),
      intro = i18n$t("Choose the response variable you want to analyze and model."),
      position = "auto"
    ),
    list(
      element = paste0("#", ns("mod_order_wrapper")),
      intro = i18n$t("Define the complexity of the model: Linear, Linear with Interactions, or Quadratic."),
      position = "auto"
    ),
    list(
      element = paste0("#", ns("p_cutoff_wrapper")),
      intro = i18n$t("Set the p-value threshold for statistical significance in model terms."),
      position = "auto"
    ),
    list(
      element = paste0("#", ns("trim_method_wrapper")),
      intro = i18n$t("Choose how to simplify the model: Stepwise regression, P-value cutoff, Both, or None."),
      position = "auto"
    ),
    list(
      element = paste0("#", ns("which_facs_wrapper")),
      intro = i18n$t("Specify whether to use coded (normalized) or uncoded (original) factor values in the analysis."),
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

# DOE Analysis Application Intro Steps - Data Source
intro_steps_data_source <- function(ns, i18n, data_source = "saved") {
  steps <- list(
    list(
      element = paste0("#", ns("data_source_wrapper")),
      intro = i18n$t("Select where your experimental data comes from: previously saved designs or a new CSV import."),
      position = "auto"
    ),
    list(
      element = paste0("#", ns("data_preview_wrapper")),
      intro = i18n$t("You can edit the desired section in the table by clicking on it."),
      position = "auto"
    )
  )

  # Conditional steps based on data source
  if (data_source == "saved") {
    steps <- c(steps, list(
      list(
        element = paste0("#", ns("saved_design_wrapper")),
        intro = i18n$t("Choose from your list of previously saved experimental designs."),
        position = "auto"
      )
    ))
  } else if (data_source == "csv") {
    steps <- c(steps, list(
      list(
        element = paste0("#", ns("import_file_csv")),
        intro = i18n$t("Upload your experimental data from a CSV file."),
        position = "auto"
      )
    ))
  }

  steps <- c(steps, list(
    list(
      element = paste0("#", ns("data_preview_wrapper")),
      intro = i18n$t("Review the loaded data here. You can also edit values directly in this table."),
      position = "auto"
    ),
    list(
      element = paste0("#", ns("reset_wrapper")),
      intro = i18n$t("Click to reset all analysis parameters to their default values."),
      position = "auto"
    ),
    list(
      element = paste0("#", ns("analyze_wrapper")),
      intro = i18n$t("Initiate the DOE analysis based on your selected parameters and data."),
      position = "auto"
    )
  ))

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
