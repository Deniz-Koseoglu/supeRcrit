# Intro.js steps for Two-Site Kinetic Modeling module
intro_steps_kinetic_tws <- function(ns, i18n) {
  steps <- list(
    list(
      element = paste0("#", ns("input_type")),
      intro = i18n$t("Select your data input method: either upload a CSV file or enter data manually."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("oec_x_var")),
      intro = i18n$t("Specify the column name for the time variable in your data."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("oec_y_var")),
      intro = i18n$t("Specify the column name for the response variable in your data."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("oec_slv_var")),
      intro = i18n$t("Optionally, specify the column name for the solvent usage variable."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("pres")),
      intro = i18n$t("Enter the extraction pressure in bar."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("temp")),
      intro = i18n$t("Enter the extraction temperature in degrees Celsius."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("m_in")),
      intro = i18n$t("Enter the mass of raw material loaded in grams."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("c0")),
      intro = i18n$t("Enter the maximum possible yield, in units consistent with your response variable."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("flow")),
      intro = i18n$t("Enter the flow rate of the solvent (e.g., mL/min or g/min)."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("f")),
      intro = i18n$t("Enter the fraction of easily desorbed solute (F)."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("cumulative")),
      intro = i18n$t("Check if your input data for response and solvent consumption is cumulative."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("mass_flow")),
      intro = i18n$t("Check if the provided flow rate is a mass flow rate."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("k1_est")),
      intro = i18n$t("Provide an initial estimate for the first-order rate constant k1."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("k2_est")),
      intro = i18n$t("Provide an initial estimate for the first-order rate constant k2."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("f_est")),
      intro = i18n$t("Provide an initial estimate for the fraction F, if it's not explicitly provided in process parameters."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("optmet")),
      intro = i18n$t("Select the optimization method for iterative curve fitting."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("flow_units")),
      intro = i18n$t("Specify the units for the flow rate."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("resp_units")),
      intro = i18n$t("Specify the units for the response variable."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("plot_x_units")),
      intro = i18n$t("Choose units for the X-axis of the plot (time or q)."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("plot_y_units")),
      intro = i18n$t("Choose units for the Y-axis of the plot (abs or cc0)."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("calculate")),
      intro = i18n$t("Click here to calculate the two-site kinetic model."),
      position = "left"
    ),
    list(
      element = paste0("#", ns("kinetic_plot")),
      intro = i18n$t("View the generated kinetic model plot here."),
      position = "left"
    ),
    list(
      element = paste0("#", ns("kinetic_summary")),
      intro = i18n$t("See a summary of the model results, including estimated parameters and fit statistics."),
      position = "left"
    ),
    list(
      element = paste0("#", ns("kinetic_data")),
      intro = i18n$t("Explore the raw and modeled data in a tabular format."),
      position = "left"
    ),
    list(
      element = paste0("#", ns("reset")),
      intro = i18n$t("Click here to reset all input parameters to their default values."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("prediction_times")),
      intro = i18n$t("Enter new time values (comma-separated) for which you want to predict the response."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("prediction_get_yields")),
      intro = i18n$t("Check this box to convert the default yield into mass and percentage yield."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("prediction_moisture")),
      intro = i18n$t("Optionally, enter the moisture content of the raw material in percent fresh weight. Used for correcting percentage yield predictions."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("generate_predictions")),
      intro = i18n$t("Click here to generate predictions based on the calculated model and your input."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("prediction_results_table")),
      intro = i18n$t("View the generated prediction results in a tabular format here."),
      position = "left"
    ),
    list(
      element = paste0("#", ns("prediction_description")),
      intro = i18n$t("This section provides a description of the model's validity and the process parameters used for prediction."),
      position = "left"
    )
  )
  return(steps)
}
# Kinetic TWS - Process Parameters Intro Steps
intro_steps_kinetic_tws_process_params <- function(ns, i18n) {
  steps <- list(
    list(
      element = paste0("#", ns("pres")),
      intro = i18n$t("Set the extraction pressure in bar. This affects the subcritical water properties and extraction efficiency."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("temp")),
      intro = i18n$t("Set the extraction temperature in degrees Celsius. Higher temperatures increase extraction rates but may affect compound stability."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("m_in")),
      intro = i18n$t("Enter the mass of raw material loaded into the extraction vessel in grams."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("flow")),
      intro = i18n$t("Set the flow rate of the extraction solvent (water) in mL/min or g/min depending on your units."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("c0")),
      intro = i18n$t("Enter the maximum possible yield of target compounds from the raw material, in the same units as your response variable."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("f")),
      intro = i18n$t("Set the fraction of easily desorbed solute (F). This represents the portion of compounds that are readily extractable (typically 0-1)."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("cumulative")),
      intro = i18n$t("Check if your response data is cumulative (total yield up to each time point) or incremental (yield at each time point)."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("mass_flow")),
      intro = i18n$t("Check if the flow rate is measured as mass flow (g/min) rather than volumetric flow (mL/min)."),
      position = "right"
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

# Kinetic TWS - Input Data & OEC Variables Intro Steps
intro_steps_kinetic_tws_input_data_oec <- function(ns, i18n) {
  steps <- list(
    list(
      element = paste0("#", ns("input_type")),
      intro = i18n$t("Select your data input method: either upload a CSV file or enter data manually in CSV format."),
      position = "right"
    ),
    # list(
    #   element = paste0("#", ns("file_upload")),
    #   intro = i18n$t("Upload a CSV file containing your extraction data. The file should have columns for time and response variables."),
    #   position = "right"
    # ),
    # list(
    #   element = paste0("#", ns("manual_data")),
    #   intro = i18n$t("Enter your time and yield data manually in CSV format (e.g., Time_min,Yield_100C\\n1,10\\n2,18)."),
    #   position = "right"
    # ),
    list(
      element = paste0("#", ns("oec_x_var_ui_wrapper")),
      intro = i18n$t("Select the column containing your time/extraction duration data (e.g., Time_min)."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("oec_y_var_ui_wrapper")),
      intro = i18n$t("Select the column containing your response/yield data (e.g., Yield_100C, permille, g)."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("oec_slv_var_ui_wrapper")),
      intro = i18n$t("Optionally select the column containing solvent consumption data. Leave as 'None' if not applicable."),
      position = "right"
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

# Kinetic TWS - Model Parameters Intro Steps
intro_steps_kinetic_tws_model_params <- function(ns, i18n) {
  steps <- list(
    list(
      element = paste0("#", ns("optmet_wrapper")),
      intro = i18n$t("Choose the optimization method for curve fitting: 'nlopt' (recommended) uses advanced algorithms, 'nlrob' is more robust to outliers."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("tmax_wrapper")),
      intro = i18n$t("Set the maximum time for model predictions in minutes. Leave blank to use 120% of your maximum experimental time."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("qmax_wrapper")),
      intro = i18n$t("Set the maximum solvent-to-feed ratio (q) for predictions. Leave blank to use 120% of your maximum experimental q."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("k1_est_wrapper")),
      intro = i18n$t("Provide an initial estimate for the fast desorption rate constant k1 (min⁻¹). Default is 0.1."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("k2_est_wrapper")),
      intro = i18n$t("Provide an initial estimate for the slow desorption rate constant k2 (min⁻¹). Default is 0.1."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("f_est_wrapper")),
      intro = i18n$t("Provide an initial estimate for fraction F if not specified in Process Parameters. Default is 0.5."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("flow_units_wrapper")),
      intro = i18n$t("Specify the units for flow rate (e.g., 'mL/min', 'g/min', 'kg/h', 'L/h')."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("resp_units_wrapper")),
      intro = i18n$t("Specify the units for response variable (e.g., 'g', 'percent', 'permille', 'ppm', 'ppb')."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("plot_x_units_wrapper")),
      intro = i18n$t("Choose x-axis units for plots: 'time' for extraction time or 'q' for solvent-to-feed ratio."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("plot_y_units_wrapper")),
      intro = i18n$t("Choose y-axis units for plots: 'abs' for absolute yield or 'cc0' for yield relative to maximum."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("flowpar_temp_wrapper")),
      intro = i18n$t("Enter the temperature (°C) at which the flow rate was measured, if different from extraction temperature."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("flowpar_pres_wrapper")),
      intro = i18n$t("Enter the pressure (bar) at which the flow rate was measured, if different from extraction pressure."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("ro_h2o_wrapper")),
      intro = i18n$t("Water density in g/L at extraction conditions. Leave blank to calculate automatically using IAPWS-95 formulation."),
      position = "right"
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
