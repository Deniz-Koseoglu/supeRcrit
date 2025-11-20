# Cost Analysis - Calculation Mode Accordion Intro Steps
intro_steps_com_calc_mode <- function(ns, i18n) {
  list(
    steps = list(
      list(
        element = paste0("#", ns("comode")),
        intro = i18n$t("Select the calculation mode: SFE (Supercritical Fluid Extraction) or SWE (Subcritical Water Extraction)."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("use_coefs")),
        intro = i18n$t("Check this box to use pre-defined coefficients for calculations, or uncheck to use custom values."),
        position = "auto"
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

# Cost Analysis - General Parameters Accordion Intro Steps
intro_steps_com_gen_params <- function(ns, i18n) {
  list(
    steps = list(
      list(
        element = paste0("#", ns("volex")),
        intro = i18n$t("Enter the volume of the extractor in liters."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("load")),
        intro = i18n$t("Input the load (amount of material) in kilograms."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("pres")),
        intro = i18n$t("Specify the operating pressure in bar."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("temp")),
        intro = i18n$t("Enter the operating temperature in degrees Celsius."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("flow")),
        intro = i18n$t("Input the flow rate in milliliters per minute."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("extime")),
        intro = i18n$t("Specify the total extraction time in minutes."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("dilfac")),
        intro = i18n$t("Enter the dilution factor for your extract."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("pr_sale")),
        intro = i18n$t("Input the sales price of the extracted product in USD per kilogram."),
        position = "auto"
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

# Cost Analysis - Input Data Accordion Intro Steps
intro_steps_com_input_data <- function(ns, i18n, input_type = "single") {
  steps <- list(
    list(
      element = paste0("#", ns("input_type")),
      intro = i18n$t("Select how you want to provide your time and yield data: single values, CSV upload, or manual text input."),
      position = "auto"
    )
  )

  # Conditional steps based on input type
  if (input_type == "single") {
    steps <- c(steps, list(
      list(
        element = paste0("#", ns("single_time")),
        intro = i18n$t("Enter the single time point for your analysis."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("single_yield")),
        intro = i18n$t("Enter the corresponding single yield value."),
        position = "auto"
      )
    ))
  } else if (input_type == "csv") {
    steps <- c(steps, list(
      list(
        element = paste0("#", ns("csv_file")),
        intro = i18n$t("Upload a CSV file containing your time and yield data. The file should have 'time' and 'yield' columns."),
        position = "auto"
      )
    ))
  } else if (input_type == "text") {
    steps <- c(steps, list(
      list(
        element = paste0("#", ns("text_input")),
        intro = i18n$t("Manually enter your time and yield data in CSV format (e.g., time,yield\\n5,0.80)."),
        position = "auto"
      )
    ))
  }

  steps <- c(steps, list(
    list(
      element = paste0("#", ns("input_data_preview")),
      intro = i18n$t("Review your input data here before proceeding with calculations."),
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


# Cost Analysis - Other Parameters Accordion Intro Steps
intro_steps_com_other_params <- function(ns, i18n) {
  list(
    steps = list(
      list(
        element = paste0("#", ns("aux_price")),
        intro = i18n$t("Enter the price of auxiliary material in USD per kilogram."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("aux_fraction")),
        intro = i18n$t("Specify the required fraction of auxiliary material."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("cosol_loss")),
        intro = i18n$t("Input the co-solvent loss as a fraction (e.g., 0.05 for 5%)."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("flowpar")),
        intro = i18n$t("Define the flow parameter. Use 'auto' for automatic calculation or a specific value."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("csol_flow")),
        intro = i18n$t("Enter the co-solvent flow rate in milliliters per minute."),
        position = "auto"
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

# Cost Analysis - Fixed Costs & Tax Rate Accordion Intro Steps
intro_steps_com_fci_params <- function(ns, i18n) {
  list(
    steps = list(
      list(
        element = paste0("#", ns("capex")),
        intro = i18n$t("Enter the total capital investment for the project in USD."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("maint")),
        intro = i18n$t("Input the annual maintenance cost in USD."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("other")),
        intro = i18n$t("Specify any other fixed costs not covered elsewhere, in USD."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("depr")),
        intro = i18n$t("Enter the depreciation rate as a fraction (e.g., 0.1 for 10%)."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("taxrate")),
        intro = i18n$t("Input the tax rate applicable to your profits."),
        position = "auto"
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

# Cost Analysis - Cost of Utilities Accordion Intro Steps
intro_steps_com_cut_params <- function(ns, i18n) {
  list(
    steps = list(
      list(
        element = paste0("#", ns("pw_main")),
        intro = i18n$t("Enter the main power consumption in kilowatts."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("pr_kwh")),
        intro = i18n$t("Input the energy price in USD per kilowatt-hour."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("pw_dry")),
        intro = i18n$t("Specify the power consumption for drying in kilowatts."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("cap_dry")),
        intro = i18n$t("Enter the drying capacity in kilograms per hour."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("pw_com")),
        intro = i18n$t("Input the power consumption for comminution (grinding) in kilowatts."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("cap_com")),
        intro = i18n$t("Specify the comminution capacity in kilograms per hour."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("pw_evap")),
        intro = i18n$t("Enter the power consumption for evaporation in kilowatts."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("cap_evap")),
        intro = i18n$t("Input the evaporation capacity in kilograms per hour."),
        position = "auto"
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

# Cost Analysis - Cost of Raw Materials Accordion Intro Steps
intro_steps_com_crm_params <- function(ns, i18n) {
  list(
    steps = list(
      list(
        element = paste0("#", ns("bh")),
        intro = i18n$t("Enter the bed height of the extractor in centimeters."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("id")),
        intro = i18n$t("Specify the inner diameter of the extractor in centimeters."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("pr_mat")),
        intro = i18n$t("Input the price of the raw material in USD per kilogram."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("pr_msol")),
        intro = i18n$t("Enter the price of the main solvent in USD per kilogram."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("pr_csol")),
        intro = i18n$t("Input the price of the co-solvent in USD per kilogram."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("recp")),
        intro = i18n$t("Specify the recovery pressure in bar."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("rect")),
        intro = i18n$t("Enter the recovery temperature in degrees Celsius."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("sept")),
        intro = i18n$t("Input the separator temperature in degrees Celsius."),
        position = "auto"
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



# Cost Analysis - Cost of Labor Accordion Intro Steps
intro_steps_com_col_params <- function(ns, i18n) {
  list(
    steps = list(
      list(
        element = paste0("#", ns("oper")),
        intro = i18n$t("Enter the number of operators required for the process."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("whr")),
        intro = i18n$t("Specify the duration of each working shift in hours."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("shifts")),
        intro = i18n$t("Input the total number of shifts per day or period."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("wage")),
        intro = i18n$t("Enter the monthly wage per operator in USD."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("wdays")),
        intro = i18n$t("Specify the number of working days in a month."),
        position = "auto"
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
