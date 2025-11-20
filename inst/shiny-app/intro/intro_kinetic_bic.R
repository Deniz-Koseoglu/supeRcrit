# Intro.js steps for BIC Kinetic Modeling module
intro_steps_kinetic_bic <- function(ns, i18n) {
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
      element = paste0("#", ns("mass_in")),
      intro = i18n$t("Enter the mass of raw material loaded in grams."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("moisture")),
      intro = i18n$t("Enter the moisture content of the raw material in percentage."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("D")),
      intro = i18n$t("Enter the diameter of the extraction vessel in meters."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("L")),
      intro = i18n$t("Enter the length of the extraction vessel in meters."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("etoh")),
      intro = i18n$t("Enter the ethanol co-solvent fraction (between 0 and 1)."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("dr")),
      intro = i18n$t("Enter the real density of the raw material in g/L."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("dp")),
      intro = i18n$t("Enter the apparent density of the raw material in g/L."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("n")),
      intro = i18n$t("Enter the number of observations corresponding to the end of CER."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("flow")),
      intro = i18n$t("Enter the flow rate of the solvent (e.g., mL/min or g/min). Can be NA."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("cu")),
      intro = i18n$t("Enter the maximum extractable material fraction (between 0 and 1). Leave empty if estimating cu with modtype='cu'."),
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
      element = paste0("#", ns("etoh_frac")),
      intro = i18n$t("Enter the ethanol co-solvent fraction for flow calculations (0-0.99). Required when flow rate is not provided but ethanol is non-zero."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("flowpar_temp")),
      intro = i18n$t("Enter the temperature (°C) at which the flow rate was measured."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("flowpar_pres")),
      intro = i18n$t("Enter the pressure (bar) at which the flow rate was measured."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("ro_co2")),
      intro = i18n$t("Enter the CO2 density (g/L) if you want to override the calculated value."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("tmax")),
      intro = i18n$t("Maximum time (min) for CT model predictions. Leave empty for auto-calculation."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("qmax")),
      intro = i18n$t("Maximum solvent/material ratio for other model predictions. Leave empty for auto-calculation."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("modtype")),
      intro = i18n$t("Select the type(s) of BIC models to generate. Choose 'cu' to estimate the extractable fraction."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("aggreg")),
      intro = i18n$t("Choose how to aggregate optimization results: 'aard' (recommended) or 'mean'."),
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
      element = paste0("#", ns("opt_est_type")),
      intro = i18n$t("Choose whether to use default optimization parameters or specify custom values."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("r_est")),
      intro = i18n$t("Provide a custom initial estimate for the grinding efficiency r."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("ksas_est")),
      intro = i18n$t("Provide a custom initial estimate for ksas (solid phase mass transfer coefficient × specific area)."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("qc_est")),
      intro = i18n$t("Provide a custom initial estimate for qc (relative solvent at CER end)."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("thetaf_est")),
      intro = i18n$t("Provide a custom initial estimate for thetaf (external mass transfer coefficient)."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("ti_est")),
      intro = i18n$t("Provide a custom initial estimate for ti (FER extraction duration)."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("kf_est")),
      intro = i18n$t("Provide a custom initial estimate for kf (fluid phase mass transfer coefficient)."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("c3_est")),
      intro = i18n$t("Provide a custom initial estimate for c3 (related to maximum extractable fraction)."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("calculate")),
      intro = i18n$t("Click here to calculate the BIC kinetic model."),
      position = "left"
    ),
    list(
      element = paste0("#", ns("kinetic_plot")),
      intro = i18n$t("View the generated kinetic model plot(s) here."),
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
    )
  )
  return(steps)
}
intro_steps_kinetic_bic_advanced_params <- function(ns, i18n) {
  steps <- list(
    list(
      element = paste0("#", ns("etoh_frac_wrapper")),
      intro = i18n$t("Enter the ethanol co-solvent fraction for flow calculations (0-0.99). Required when flow rate is not provided but ethanol is non-zero."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("flowpar_temp_wrapper")),
      intro = i18n$t("Enter the temperature (°C) at which the flow rate was measured."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("flowpar_pres_wrapper")),
      intro = i18n$t("Enter the pressure (bar) at which the flow rate was measured."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("ro_co2_wrapper")),
      intro = i18n$t("Enter the CO2 density (g/L) if you want to override the calculated value."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("tmax_wrapper")),
      intro = i18n$t("Maximum time (min) for CT model predictions. Leave empty for auto-calculation."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("qmax_wrapper")),
      intro = i18n$t("Maximum solvent/material ratio for other model predictions. Leave empty for auto-calculation."),
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

# Kinetic BIC - Input Data & OEC Variables Intro Steps
intro_steps_kinetic_bic_input_data <- function(ns, i18n,input_type) {
  steps <- list(
    list(
      element = paste0("#", ns("input_type_wrapper")),
      intro = i18n$t("Select your data input method: either upload a CSV file or enter data manually."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("data_input_ui_wrapper")),
      intro = i18n$t("Upload a CSV file containing your extraction data or enter data manually in CSV format."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("oec_x_var_ui_wrapper")),
      intro = i18n$t("Specify the column name for the time variable in your data."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("oec_y_var_ui_wrapper")),
      intro = i18n$t("Specify the column name for the response variable in your data."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("oec_slv_var_ui_wrapper")),
      intro = i18n$t("Optionally, specify the column name for the solvent usage variable."),
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
# Kinetic BIC - Model Settings Intro Steps
intro_steps_kinetic_bic_model_settings <- function(ns, i18n) {
  steps <- list(
    list(
      element = paste0("#", ns("modtype_wrapper")),
      intro = i18n$t("Select the type(s) of BIC models to generate. Choose 'cu' to estimate the extractable fraction."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("aggreg_wrapper")),
      intro = i18n$t("Choose how to aggregate optimization results: 'aard' (recommended) or 'mean'."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("flow_units_wrapper")),
      intro = i18n$t("Specify the units for the flow rate."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("resp_units_wrapper")),
      intro = i18n$t("Specify the units for the response variable."),
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

# Kinetic BIC - Optimization Parameters Intro Steps
intro_steps_kinetic_bic_optimization_params <- function(ns, i18n,opt_est_custom) {
  steps <- list(
    list(
      element = paste0("#", ns("opt_est_type_wrapper")),
      intro = i18n$t("Choose whether to use default optimization parameters or specify custom values."),
      position = "right"
    ))
  custom_steps <- list(
    list(
      element = paste0("#", ns("r_est_wrapper")),
      intro = i18n$t("Provide a custom initial estimate for the grinding efficiency r."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("ksas_est_wrapper")),
      intro = i18n$t("Provide a custom initial estimate for ksas (solid phase mass transfer coefficient × specific area)."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("qc_est_wrapper")),
      intro = i18n$t("Provide a custom initial estimate for qc (relative solvent at CER end)."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("thetaf_est_wrapper")),
      intro = i18n$t("Provide a custom initial estimate for thetaf (external mass transfer coefficient)."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("ti_est_wrapper")),
      intro = i18n$t("Provide a custom initial estimate for ti (FER duration)."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("kf_est_wrapper")),
      intro = i18n$t("Provide a custom initial estimate for kf (fluid mass transfer coefficient)."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("c3_est_wrapper")),
      intro = i18n$t("Provide a custom initial estimate for c3 (related to maximum extractable fraction)."),
      position = "right"
    ))
 
    if (opt_est_custom == TRUE)
    {
      steps <- c(steps,custom_steps)
    }
    
  

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

# Kinetic BIC - Process Parameters Intro Steps
intro_steps_kinetic_bic_process_params <- function(ns, i18n) {
  steps <- list(
    list(
      element = paste0("#", ns("pres_wrapper")),
      intro = i18n$t("Enter the extraction pressure in bar."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("temp_wrapper")),
      intro = i18n$t("Enter the extraction temperature in degrees Celsius."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("mass_in_wrapper")),
      intro = i18n$t("Enter the mass of raw material loaded in grams."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("moisture_wrapper")),
      intro = i18n$t("Enter the moisture content of the raw material in percentage."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("D_wrapper")),
      intro = i18n$t("Enter the diameter of the extraction vessel in meters."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("L_wrapper")),
      intro = i18n$t("Enter the length of the extraction vessel in meters."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("etoh_wrapper")),
      intro = i18n$t("Enter the ethanol co-solvent fraction (between 0 and 1)."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("dr_wrapper")),
      intro = i18n$t("Enter the real density of the raw material in g/L."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("dp_wrapper")),
      intro = i18n$t("Enter the apparent density of the raw material in g/L."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("n_wrapper")),
      intro = i18n$t("Enter the number of observations corresponding to the end of CER."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("flow_wrapper")),
      intro = i18n$t("Enter the flow rate of the solvent (e.g., mL/min or g/min). Can be NA."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("cu_wrapper")),
      intro = i18n$t("Enter the maximum extractable material fraction (between 0 and 1). Leave empty if estimating cu with modtype='cu'."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("cumulative_wrapper")),
      intro = i18n$t("Check if your input data for response and solvent consumption is cumulative."),
      position = "right"
    ),
    list(
      element = paste0("#", ns("mass_flow_wrapper")),
      intro = i18n$t("Check if the provided flow rate is a mass flow rate."),
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
