# BIC Kinetic Modeling Server Module
kinetic_bic_server <- function(input, output, session, defaults, i18n, tablang) {




  # Load required libraries
  library(dplyr)
  library(DT)
  library(plotly)
  library(zip) # For zipping export files
  library(supeRcrit) # Assuming bicmod is part of supeRcrit

  # Helper for creating namespaced ids inside this module
  ns <- session$ns
  i18n_r <- reactive({
    i18n
  })

  # Load Example button: rendered server-side so i18n_r()$t() returns a plain
  # string (rather than a shiny.i18n <span> wrapper) for the title attribute.
  output$load_example_data_btn <- renderUI({
    create_load_example_btn(ns, i18n_r)
  })

  # Estimate-from-data links (server-rendered for the same i18n title reason).
  output$estimate_n_link_ui <- renderUI({
    create_estimate_link(ns, i18n_r, "estimate_n_link")
  })
  output$estimate_cu_link_ui <- renderUI({
    create_estimate_link(ns, i18n_r, "estimate_cu_link")
  })

  output$kinetic_bic_HELP <- renderUI({
    create_help_modal(i18n_r, "kinetic_bic_help_en")
  })

  # Render accordion expand/collapse button with translated tooltips
  output$accordion_toggle_btn <- renderUI({
    create_accordion_toggle_btn(ns, i18n_r, "param_accordion")
  })

  # Dynamic UI for modtype selectizeInput with translated labels
  output$modtype_ui <- renderUI({
    selectizeInput(ns("modtype"),
      tags$span(i18n_r()$t("Model Type"),
        input_help(i18n_r()$t("Select which BIC model(s) to build. Simplified fits the CER region only. Complete fits CER + DC (2 regions) or CER + FER + DC (3 regions). Characteristic Times uses an alternative time-based formulation."),
                   title = i18n_r()$t("Model Type"), buttonLabel = i18n_r()$t("OK"))),
      choices = setNames(
        c("all", "sim", "ct", "cmp3"),
        c(i18n_r()$t("All"), i18n_r()$t("Simplified"), i18n_r()$t("Characteristic Times"), i18n_r()$t("Complete"))
      ),
      selected = isolate(input$modtype) %||% defaults$modtype,
      multiple = TRUE,
      options = list(placeholder = i18n_r()$t("Select model type(s)"))
    )
  })

  # Dynamic UI for aggreg selectInput
  output$aggreg_ui <- renderUI({
    selectInput(ns("aggreg"),
      tags$span(i18n_r()$t("Optimization Aggregation"),
        input_help(i18n_r()$t("How to select the best result from multiple optimization runs. AARD picks the result with the lowest Average Absolute Relative Deviation (recommended). Mean takes the arithmetic average of all results."),
                   title = i18n_r()$t("Aggregation"), buttonLabel = i18n_r()$t("OK"))),
      choices = setNames(c("aard", "mean"), i18n_r()$t(c("AARD", "Mean"))),
      selected = defaults$aggreg
    )
  })
  outputOptions(output, "aggreg_ui", suspendWhenHidden = FALSE)

  # Dynamic UI for flow_units selectInput - filters choices based on mass_flow, disabled when solvent selected
  output$flow_units_ui <- renderUI({
    mass_flow_checked <- if (is.null(input$mass_flow)) defaults$mass_flow else input$mass_flow
    solvent_mode <- solvent_selected()
    
    if (mass_flow_checked) {
      # Mass flow - only show mass-based units
      choices_values <- c("g/min", "kg/h")
      choices_labels <- i18n_r()$t(c("g/min", "kg/h"))
      current_selected <- input$flow_units
      # If current selection is volumetric, switch to g/min
      selected <- if (!is.null(current_selected) && current_selected %in% choices_values) {
        current_selected
      } else {
        "g/min"
      }
    } else {
      # Volumetric flow - show all units
      choices_values <- c("mL/min", "g/min", "kg/h", "L/h")
      choices_labels <- i18n_r()$t(c("mL/min", "g/min", "kg/h", "L/h"))
      current_selected <- input$flow_units
      selected <- if (!is.null(current_selected) && current_selected %in% choices_values) {
        current_selected
      } else {
        defaults$flow_units
      }
    }
    
    flow_units_div <- selectInput(ns("flow_units"),
      tags$span(i18n_r()$t("Flow Units"),
        input_help(i18n_r()$t("Units of the CO2 flow rate. Choose mass-based (g/min, kg/h) or volumetric (mL/min, L/h). Volumetric units require CO2 density for conversion."),
                   title = i18n_r()$t("Flow Units"), buttonLabel = i18n_r()$t("OK"))),
      choices = setNames(choices_values, choices_labels),
      selected = selected
    )
    
    if (solvent_mode) shinyjs::disabled(flow_units_div) else flow_units_div
  })
  outputOptions(output, "flow_units_ui", suspendWhenHidden = FALSE)

  # Dynamic UI for resp_units selectInput
  output$resp_units_ui <- renderUI({
    selectInput(ns("resp_units"),
      tags$span(i18n_r()$t("Response Units"),
        input_help(i18n_r()$t("Units of your yield/response data. Choose grams for absolute mass, percent or permille for yield relative to raw material mass, or ppm/ppb for trace concentrations."),
                   title = i18n_r()$t("Response Units"), buttonLabel = i18n_r()$t("OK"))),
      choices = setNames(
        c("g", "percent", "permille", "ppm", "ppb"),
        c(i18n_r()$t("grams"), "%", "\u2030", "ppm", "ppb")
      ),
      selected = defaults$resp_units
    )
  })
  outputOptions(output, "resp_units_ui", suspendWhenHidden = FALSE)

  output$pres_ui <- renderUI({
    numericInput(ns("pres"),
      tags$span(i18n_r()$t("Pressure (bar)"),
        input_help(i18n_r()$t("Extraction pressure in bar. Used to calculate supercritical CO2 density via the Bender equation of state."),
                   title = i18n_r()$t("Pressure"), buttonLabel = i18n_r()$t("OK"))),
      value = isolate(input$pres) %||% defaults$pres, min = 0)
  })

  output$temp_ui <- renderUI({
    numericInput(ns("temp"),
      tags$span(i18n_r()$t("Temperature (\u00B0C)"),
        input_help(i18n_r()$t("Extraction temperature in degrees Celsius. Used alongside pressure to calculate CO2 density."),
                   title = i18n_r()$t("Temperature"), buttonLabel = i18n_r()$t("OK"))),
      value = isolate(input$temp) %||% defaults$temp, min = 0)
  })

  output$mass_in_ui <- renderUI({
    numericInput(ns("mass_in"),
      tags$span(i18n_r()$t("Mass of Raw Material (g)"),
        input_help(i18n_r()$t("Total mass of raw material loaded into the extraction vessel (grams). Used to calculate bed porosity, solvent-to-feed ratio, and yield conversions."),
                   title = i18n_r()$t("Material Mass"), buttonLabel = i18n_r()$t("OK"))),
      value = isolate(input$mass_in) %||% defaults$mass_in, min = 0)
  })

  output$moisture_ui <- renderUI({
    tags$div(
      tags$label(
        i18n_r()$t("Moisture Content (%)"),
        input_help(i18n_r()$t("Moisture content of the raw material as a percentage (0-100). Used to calculate dry mass of material for the model."),
                   title = i18n_r()$t("Moisture"), buttonLabel = i18n_r()$t("OK")),
        class = "control-label",
        style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
        tags$span("0\u2013100",
          style = "font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: #6c757d; color: white; margin-left: auto; font-weight: normal;"
        )
      ),
      numericInput(ns("moisture"), NULL, value = isolate(input$moisture) %||% defaults$moisture, min = 0, max = 100)
    )
  })

  output$dr_ui <- renderUI({
    tags$div(
      tags$label(
        i18n_r()$t("Real Density (g/L)"),
        input_help(i18n_r()$t("True (skeletal) density of the raw material in g/L. Must be greater than the apparent density. Used together with apparent density to calculate bed porosity."),
                   title = i18n_r()$t("Real Density"), buttonLabel = i18n_r()$t("OK")),
        class = "control-label",
        style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
        tags$span("> 0",
          style = "font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: #6c757d; color: white; margin-left: auto; font-weight: normal;",
          title = i18n_r()$t("Must be greater than Apparent Density")
        )
      ),
      numericInput(ns("dr"), NULL, value = isolate(input$dr) %||% defaults$dr, min = 0)
    )
  })

  output$dp_ui <- renderUI({
    tags$div(
      tags$label(
        i18n_r()$t("Particle Diameter (mm)"),
        input_help(i18n_r()$t("Mean diameter of ground raw material particles in millimeters. Used to calculate the specific surface area of the extraction bed."),
                   title = i18n_r()$t("Particle Diameter"), buttonLabel = i18n_r()$t("OK")),
        class = "control-label",
        style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
        tags$span("> 0",
          style = "font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: #6c757d; color: white; margin-left: auto; font-weight: normal;"
        )
      ),
      numericInput(ns("dp"), NULL, value = isolate(input$dp) %||% defaults$dp, min = 0, step = 0.01)
    )
  })

  output$cu_ui <- renderUI({
    tags$div(
      tags$label(
        HTML(paste0(i18n_r()$t("Max Extractable Fraction"), "&nbsp;<em>c</em><sub>u</sub>")),
        input_help(i18n_r()$t("Maximum fraction of solute extractable by the supercritical fluid (0-1). Can be estimated using the cu model type, or set manually based on exhaustive extraction experiments."),
                   title = "c\u1d64", buttonLabel = i18n_r()$t("OK")),
        class = "control-label",
        style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
        div(style = "display: flex; align-items: center; gap: 4px; margin-left: auto;",
          tags$span("0\u20131",
            style = "font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: #6c757d; color: white; font-weight: normal;"
          ),
          uiOutput(ns("estimate_cu_link_ui"), inline = TRUE)
        )
      ),
      numericInput(ns("cu"), NULL, value = isolate(input$cu) %||% defaults$cu, min = 0, max = 1)
    )
  })

  output$cumulative_checkbox_ui <- renderUI({
    tags$span(
      checkboxInput(ns("cumulative"), i18n_r()$t("Cumulative Data"),
                    value = isolate(input$cumulative) %||% defaults$cumulative),
      title = i18n_r()$t("Enable if your response and solvent data represent cumulative totals. Disable if each row shows the amount collected during that interval only.")
    )
  })

  output$use_solvent_checkbox_ui <- renderUI({
    tags$span(
      checkboxInput(ns("use_solvent"), i18n_r()$t("Use Solvent Data"),
                    value = isolate(input$use_solvent) %||% FALSE),
      title = i18n_r()$t("Enable to use a solvent consumption column from your data instead of calculating it from flow rate. When enabled, the Flow Parameters section is disabled.")
    )
  })


  # Note: %||% operator is defined in utils/general_helpers.R

  # Reactive values for storing results
  kinetic_results <- reactiveValues(
    full_result = NULL,
    sim_result = NULL,
    ct_result = NULL,
    cmp_result = NULL,
    plots_list = NULL,
    data = NULL,
    input_params = NULL,
    available_models = c(),
    predict_result = NULL, # predict_bic() sonuçları
    predict_data = NULL, # Input verisi
    previous_modtype = NULL # Track previous modtype selection
  )

  # Dynamic UI for data input (only file upload shown in CSV mode)
  output$data_input_ui <- renderUI({
    if (input$input_type == "csv") {
      fileInput(ns("file_upload"), NULL,
        accept = c("text/csv", "text/comma-separated-values", "text/plain", ".csv"),
        buttonLabel = i18n_r()$t("Browse"),
        placeholder = i18n_r()$t("No file selected")
      )
    } else {
      # Manual input mode - no file input needed
      NULL
    }
  })
  
  # Reactive to track if solvent variable is being used
  solvent_selected <- reactive({
    use_solvent <- if (is.null(input$use_solvent)) FALSE else input$use_solvent
    use_solvent
  })

  # Observer to handle use_solvent checkbox changes
  # When unchecked, ensure the solvent variable doesn't persist
  observeEvent(input$use_solvent, {
    if (!input$use_solvent) {
      # Force re-render of dependent UI elements by triggering a small delay
      # This ensures flow input is properly enabled when solvent is unchecked
      shinyjs::delay(100, {
        shinyjs::enable("flow")
      })
    }
  }, ignoreInit = TRUE)

  # Reactive to check if data appears to be cumulative
  # Returns list with: response_likely_cumulative, solvent_likely_cumulative
  data_cumulative_check <- reactive({
    data <- oec_data()
    if (is.null(data) || nrow(data) < 2) {
      return(list(response = NULL, solvent = NULL))
    }
    
    # Get variable names
    y_var <- input$oec_y_var
    slv_var <- input$oec_slv_var
    use_solvent <- if (is.null(input$use_solvent)) FALSE else input$use_solvent
    
    result <- list(response = NULL, solvent = NULL)
    
    # Check response column
    if (!is.null(y_var) && y_var != "" && y_var %in% colnames(data)) {
      col_data <- data[[y_var]]
      if (is.numeric(col_data) && length(col_data) >= 2 && !all(is.na(col_data))) {
        # Remove NAs for diff calculation
        col_data_clean <- col_data[!is.na(col_data)]
        if (length(col_data_clean) >= 2) {
          response_diff <- diff(col_data_clean)
          # Data is likely cumulative if all differences are >= 0 (monotonically increasing)
          result$response <- all(response_diff >= 0, na.rm = TRUE)
        }
      }
    }
    
    # Check solvent column if enabled and valid selection exists
    if (use_solvent && !is.null(slv_var) && slv_var != "" && slv_var %in% colnames(data)) {
      col_data <- data[[slv_var]]
      if (is.numeric(col_data) && length(col_data) >= 2 && !all(is.na(col_data))) {
        # Remove NAs for diff calculation
        col_data_clean <- col_data[!is.na(col_data)]
        if (length(col_data_clean) >= 2) {
          solvent_diff <- diff(col_data_clean)
          result$solvent <- all(solvent_diff >= 0, na.rm = TRUE)
        }
      }
    }
    
    result
  })

  # Render Mass Flow checkbox (only when solvent is enabled)
  output$mass_flow_input_section_ui <- renderUI({
    if (solvent_selected()) {
      mass_flow_checked <- if (is.null(input$mass_flow)) defaults$mass_flow else input$mass_flow
      checkboxInput(ns("mass_flow"), i18n$t("Mass Flow Rate"), value = mass_flow_checked)
    } else {
      NULL
    }
  })

  # Render warning for duplicate column selections or empty selections
  output$duplicate_column_warning_ui <- renderUI({
    validation <- tryCatch(
      duplicate_column_validation(),
      error = function(e) list(has_duplicates = FALSE, has_empty = FALSE, duplicate_column = NULL)
    )
    
    # Check for empty selections first
    if (!is.null(validation) && 
        !is.null(validation$has_empty) && 
        !is.na(validation$has_empty) && 
        validation$has_empty == TRUE) {
      div(
        class = "alert alert-warning",
        style = "margin: 10px 0 0 0; padding: 10px 12px; font-size: 12px;",
        icon("exclamation-triangle"),
        " ", i18n$t("Variable selection cannot be empty.")
      )
    } else if (!is.null(validation) && 
        !is.null(validation$has_duplicates) && 
        !is.na(validation$has_duplicates) && 
        validation$has_duplicates == TRUE &&
        !is.null(validation$duplicate_column)) {
      div(
        class = "alert alert-danger",
        style = "margin: 10px 0 0 0; padding: 10px 12px; font-size: 12px;",
        icon("exclamation-triangle"),
        " ", tags$strong(i18n$t("Duplicate column warning:")),
        " ", sprintf(i18n$t("Column '%s' is selected in multiple fields."), validation$duplicate_column)
      )
    } else {
      NULL
    }
  })

  # Render warning if cumulative setting doesn't match data pattern
  output$cumulative_warning_ui <- renderUI({
    tryCatch({
      cumul_check <- data_cumulative_check()
      cumulative_setting <- if (is.null(input$cumulative)) defaults$cumulative else input$cumulative
      
      warnings <- c()
      has_mismatch <- FALSE
      
      # Check response data
      if (!is.null(cumul_check$response) && !is.na(cumul_check$response)) {
        if (cumulative_setting && !cumul_check$response) {
          warnings <- c(warnings, i18n$t("Response data does not appear to be cumulative (contains decreasing values)."))
          has_mismatch <- TRUE
        } else if (!cumulative_setting && cumul_check$response) {
          warnings <- c(warnings, i18n$t("Response data appears to be cumulative (monotonically increasing)."))
        }
      }
      
      # Check solvent data
      if (!is.null(cumul_check$solvent) && !is.na(cumul_check$solvent)) {
        if (cumulative_setting && !cumul_check$solvent) {
          warnings <- c(warnings, i18n$t("Solvent data does not appear to be cumulative (contains decreasing values)."))
          has_mismatch <- TRUE
        } else if (!cumulative_setting && cumul_check$solvent) {
          warnings <- c(warnings, i18n$t("Solvent data appears to be cumulative (monotonically increasing)."))
        }
      }
      
      if (length(warnings) > 0) {
        # Add note about setting being ignored when cumulative is checked but data is not cumulative
        note_msg <- if (has_mismatch) {
          tags$p(
            style = "margin-top: 8px; margin-bottom: 0; font-style: italic;",
            i18n$t("The 'Cumulative Data' setting will be ignored and the data will be treated as non-cumulative.")
          )
        } else {
          NULL
        }
        
        div(
          class = "alert alert-warning",
          style = "margin: 10px 0 0 0; padding: 10px 12px; font-size: 12px;",
          icon("exclamation-triangle"),
          " ", tags$strong(i18n$t("Data mismatch warning:")),
          tags$ul(
            style = "margin-bottom: 0; margin-top: 5px;",
            lapply(warnings, function(w) tags$li(w))
          ),
          note_msg
        )
      } else {
        NULL
      }
    }, error = function(e) {
      NULL
    })
  })

  # Render solvent units info when solvent column is selected
  output$solvent_units_info_ui <- renderUI({
    if (solvent_selected()) {
      mass_flow_checked <- if (is.null(input$mass_flow)) defaults$mass_flow else input$mass_flow
      current_units <- if (mass_flow_checked) "g" else "mL"
      units_label <- if (mass_flow_checked) i18n$t("(grams)") else i18n$t("(milliliters)")
      
      div(
        class = "alert alert-info",
        style = "margin: 10px 0 0 0; padding: 10px 12px; font-size: 12px;",
        icon("info-circle"),
        " ", tags$strong(i18n$t("Solvent Data Provided:")), " ",
        i18n$t("When a Solvent column is selected, the values must represent the amount of solvent expended (cumulative or per interval). The units are determined by the 'Mass Flow Rate' checkbox above."),
        tags$br(), tags$br(),
        tags$strong(i18n$t("Current solvent units:")), " ",
        tags$span(style = "font-weight: bold; color: #31708f;", paste0(current_units, " ", units_label))
      )
    } else {
      NULL
    }
  })

  # Dynamic UI for flow rate input with units from flow_units dropdown
  # Disabled when solvent column is selected
  output$flow_input_ui <- renderUI({
    flow_unit <- input$flow_units %||% defaults$flow_units %||% "g/min"
    unit_display <- paste0(" (", i18n$t(flow_unit), ")")
    label_text <- paste0(i18n$t("Flow Rate"), unit_display)

    current_value <- if (!is.null(input$flow)) input$flow else defaults$flow
    is_disabled <- solvent_selected()

    flow_input_div <- tags$div(
      tags$label(label_text,
        input_help(i18n_r()$t("CO2 flow rate delivered by the pump. Not needed if you provide a Solvent Data column in the Input Data section. Required for calculating the solvent-to-material ratio."),
                   title = i18n_r()$t("Flow Rate"), buttonLabel = i18n_r()$t("OK")),
        class = "control-label"),
      numericInput(ns("flow"), NULL, value = current_value)
    )
    
    if (is_disabled) {
      shinyjs::disabled(flow_input_div)
    } else {
      flow_input_div
    }
  })

  # Render message when flow rate is disabled due to solvent column selection
  output$flow_params_disabled_message <- renderUI({
    if (solvent_selected()) {
      div(
        class = "alert alert-info",
        style = "margin-bottom: 10px; padding: 8px 12px; font-size: 12px;",
        icon("info-circle"),
        " ", i18n$t("Flow Rate is disabled because solvent consumption data is provided in the input.")
      )
    } else {
      NULL
    }
  })

  # ============================================================
  # Advanced Parameters with toggles
  # ============================================================
  ro_co2_enabled <- reactiveVal(FALSE)
  tmax_enabled <- reactiveVal(FALSE)
  qmax_enabled <- reactiveVal(FALSE)

  observeEvent(input$toggle_ro_co2, { ro_co2_enabled(!ro_co2_enabled()) })
  observeEvent(input$toggle_tmax, { tmax_enabled(!tmax_enabled()) })
  observeEvent(input$toggle_qmax, { qmax_enabled(!qmax_enabled()) })

  # CO₂ Density with toggle
  output$ro_co2_ui <- renderUI({
    is_on <- ro_co2_enabled()
    div(
      class = "form-group shiny-input-container", style = "width: 100%;",
      div(
        style = "display: flex; justify-content: space-between; align-items: center;",
        tags$label(class = "control-label",
          HTML(paste0("CO", tags$sub("2"), " ", i18n_r()$t("Density"), " (g/L)")),
          input_help(i18n_r()$t("Supercritical CO2 density at extraction conditions (g/L). Leave empty to calculate automatically from pressure and temperature."),
                     title = i18n_r()$t("CO2 Density"), buttonLabel = i18n_r()$t("OK")),
          title = i18n_r()$t("Supercritical CO\u2082 density. If not specified, it is calculated automatically from pressure and temperature.")
        ),
        actionButton(ns("toggle_ro_co2"), label = NULL,
          icon = icon(if (is_on) "toggle-on" else "toggle-off"),
          class = paste("btn btn-xs", if (is_on) "btn-primary" else "btn-default"),
          style = "padding: 2px 6px; font-size: 14px;",
          title = i18n_r()$t("Enable custom value"))
      ),
      if (is_on) {
        numericInput(ns("ro_co2"), label = NULL, value = isolate(input$ro_co2) %||% defaults$ro_co2, min = 0.1)
      } else {
        div(style = "background-color: #f5f5f5; border: 1px solid #ddd; border-radius: 4px; padding: 6px 12px; color: #888; height: 34px; line-height: 22px;",
          i18n_r()$t("Auto"))
      }
    )
  })

  # Max Time with toggle
  output$tmax_ui <- renderUI({
    is_on <- tmax_enabled()
    
    exp_max <- NULL
    tryCatch({
      data <- oec_data()
      x_var <- input$oec_x_var
      if (!is.null(data) && !is.null(x_var) && x_var %in% names(data)) {
        exp_max <- max(data[[x_var]], na.rm = TRUE)
      }
    }, error = function(e) NULL)
    
    badge_text <- if (!is.null(exp_max) && is.finite(exp_max)) {
      paste0(i18n_r()$t("Exp:"), " ", round(exp_max, 1))
    } else NULL
    
    div(
      class = "form-group shiny-input-container", style = "width: 100%;",
      div(
        style = "display: flex; justify-content: space-between; align-items: center;",
        tags$label(class = "control-label",
          i18n_r()$t("Max Time (min)"),
          input_help(i18n_r()$t("Upper time limit for Characteristic Times model predictions (minutes). Leave empty to automatically use 120% of your longest experimental time point."),
                     title = i18n_r()$t("Max Time"), buttonLabel = i18n_r()$t("OK")),
          title = i18n_r()$t("Maximum extraction time for CT model predictions (minutes)")
        ),
        div(style = "display: flex; align-items: center; gap: 4px;",
          if (!is.null(badge_text)) tags$span(badge_text,
            title = paste0(i18n_r()$t("Max experimental value:"), " ", round(exp_max, 2), " ", i18n$t("min")),
            style = "font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: #6c757d; color: white; font-weight: normal; cursor: help;"),
          actionButton(ns("toggle_tmax"), label = NULL,
            icon = icon(if (is_on) "toggle-on" else "toggle-off"),
            class = paste("btn btn-xs", if (is_on) "btn-primary" else "btn-default"),
            style = "padding: 2px 6px; font-size: 14px;",
            title = i18n_r()$t("Enable custom value"))
        )
      ),
      if (is_on) {
        numericInput(ns("tmax"), label = NULL, value = isolate(input$tmax) %||% defaults$tmax, min = 0.1)
      } else {
        div(style = "background-color: #f5f5f5; border: 1px solid #ddd; border-radius: 4px; padding: 6px 12px; color: #888; height: 34px; line-height: 22px;",
          i18n_r()$t("Auto (120% of experimental max)"))
      }
    )
  })

  observeEvent(input$tmax, {
    if (tmax_enabled() && !is.null(input$tmax) && !is.na(input$tmax) && input$tmax <= 0) {
      showNotification(i18n$t("Max Time must be greater than 0."), type = "warning")
      updateNumericInput(session, "tmax", value = 1)
    }
  }, ignoreInit = TRUE)

  # Max Solvent-to-Material Ratio with toggle
  output$qmax_ui <- renderUI({
    is_on <- qmax_enabled()
    
    # Get experimental max q from model output (most accurate)
    exp_max_q <- NULL
    tryCatch({
      model_data <- kinetic_results$data
      if (!is.null(model_data) && "q" %in% names(model_data)) {
        exp_max_q <- max(model_data$q, na.rm = TRUE)
      }
    }, error = function(e) NULL)
    
    badge_text <- if (!is.null(exp_max_q) && is.finite(exp_max_q)) {
      paste0(i18n_r()$t("Exp:"), " ", round(exp_max_q, 1))
    } else NULL
    
    div(
      class = "form-group shiny-input-container", style = "width: 100%;",
      div(
        style = "display: flex; justify-content: space-between; align-items: center;",
        tags$label(class = "control-label",
          i18n_r()$t("Max S/M Ratio (kg/kg)"),
          input_help(i18n_r()$t("Upper limit for the solvent-to-material ratio in model predictions. Leave empty to automatically use 120% of your largest experimental q value."),
                     title = i18n_r()$t("Max S/M Ratio"), buttonLabel = i18n_r()$t("OK")),
          title = i18n_r()$t("Maximum solvent-to-material mass ratio for model predictions")
        ),
        div(style = "display: flex; align-items: center; gap: 4px;",
          if (!is.null(badge_text)) tags$span(badge_text,
            title = paste0(i18n_r()$t("Max experimental value:"), " ", round(exp_max_q, 2)),
            style = "font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: #6c757d; color: white; font-weight: normal; cursor: help;"),
          actionButton(ns("toggle_qmax"), label = NULL,
            icon = icon(if (is_on) "toggle-on" else "toggle-off"),
            class = paste("btn btn-xs", if (is_on) "btn-primary" else "btn-default"),
            style = "padding: 2px 6px; font-size: 14px;",
            title = i18n_r()$t("Enable custom value"))
        )
      ),
      if (is_on) {
        numericInput(ns("qmax"), label = NULL, value = isolate(input$qmax) %||% defaults$qmax, min = 0.1)
      } else {
        div(style = "background-color: #f5f5f5; border: 1px solid #ddd; border-radius: 4px; padding: 6px 12px; color: #888; height: 34px; line-height: 22px;",
          i18n_r()$t("Auto (120% of experimental max)"))
      }
    )
  })

  observeEvent(input$qmax, {
    if (qmax_enabled() && !is.null(input$qmax) && !is.na(input$qmax) && input$qmax <= 0) {
      showNotification(i18n$t("Max S/M Ratio must be greater than 0."), type = "warning")
      updateNumericInput(session, "qmax", value = 1)
    }
  }, ignoreInit = TRUE)

  # Render Data Preview section (shown in both CSV and Manual modes)
  output$data_preview_ui <- renderUI({
    data <- display_oec_data()
    calc_data <- oec_data()  # Data for calculations (NA rows filtered)
    has_valid_data <- !is.null(calc_data) && nrow(calc_data) > 0
    
    if (!is.null(data) && nrow(data) > 0) {
      # Create download button with conditional disabled state
      download_btn <- downloadButton(ns("download_current_data"), i18n$t("Download"),
        icon = icon("download"),
        class = "btn btn-outline-secondary btn-sm",
        style = "height: 34px; padding: 5px 10px;"
      )
      
      # Disable button if no valid data
      if (!has_valid_data) {
        download_btn <- shinyjs::disabled(download_btn)
      }
      
      # Create clear button
      clear_btn <- actionButton(ns("clear_data"), i18n$t("Clear"),
        icon = icon("trash-alt"),
        class = "btn btn-outline-secondary btn-sm",
        style = "height: 34px; padding: 5px 10px;",
        title = i18n$t("Clear current data")
      )
      
      # Check if first row has zeros
      zero_row_warning <- NULL
      if (has_valid_data) {
        first_row_numeric <- as.numeric(calc_data[1, sapply(calc_data, is.numeric)])
        if (length(first_row_numeric) > 0 && !any(first_row_numeric == 0, na.rm = TRUE)) {
          zero_row_warning <- div(
            class = "alert alert-warning", style = "padding: 8px 12px; margin-bottom: 8px; font-size: 13px;",
            icon("exclamation-triangle"), " ",
            i18n$t("Data does not start from zero. A zero-row (time = 0, yield = 0) is typically required for model fitting.")
          )
        }
      }

      tagList(
        # Zero-row warning (if applicable)
        zero_row_warning,
        # Controls row: Clear, Download, separator, Rename
        div(
          style = "display: flex; align-items: center; justify-content: flex-start; margin-bottom: 8px; gap: 8px;",
          # Clear and Download buttons
          clear_btn,
          download_btn,
          # Separator
          tags$span(style = "border-left: 1px solid #ccc; height: 20px; margin-left: 4px; margin-right: 4px;"),
          # Rename column controls
          uiOutput(ns("rename_col_controls_ui"))
        ),
        fluidRow(
          column(12, rhandsontable::rHandsontableOutput(ns("oec_data_preview")))
        )
      )
    } else {
      # Placeholder when no data is loaded - message depends on input mode
      placeholder_msg <- if (input$input_type == "csv") {
        i18n$t("Upload a CSV file to preview data.")
      } else {
        i18n$t("Click in the table to start entering data.")
      }
      
      div(
        style = "text-align: center; padding: 20px; color: #888; background-color: #f9f9f9; border: 1px dashed #ddd; border-radius: 4px;",
        icon("table", style = "font-size: 24px; margin-bottom: 8px; display: block;"),
        tags$span(placeholder_msg)
      )
    }
  })

  # Separate renderUI for rename controls - reacts to display_oec_data() changes
  output$rename_col_controls_ui <- renderUI({
    data <- display_oec_data()
    if (is.null(data) || ncol(data) < 1) return(NULL)
    
    current_cols <- colnames(data)
    
    # Truncate long column names for display (keep full name as value)
    max_display_len <- 12
    display_names <- sapply(current_cols, function(name) {
      if (nchar(name) > max_display_len) {
        paste0(substr(name, 1, max_display_len - 1), "…")
      } else {
        name
      }
    })
    choices <- setNames(current_cols, display_names)
    
    div(
      style = "display: flex; align-items: center; gap: 5px; flex: 1;",
      tags$span(style = "font-size: 12px; color: #666; white-space: nowrap;", i18n$t("Rename")),
      selectInput(
        ns("rename_col_select"),
        label = NULL,
        choices = choices,
        selected = current_cols[1],
        width = "130px"
      ) |> tagAppendAttributes(style = "margin-bottom: 0; flex-shrink: 0;"),
      tags$input(
        id = ns("rename_col_newname"),
        type = "text",
        class = "form-control form-control-sm",
        style = "flex: 1; min-width: 100px; height: 34px; font-size: 12px;",
        placeholder = i18n$t("New name")
      ),
      actionButton(
        ns("rename_col_btn"),
        label = NULL,
        icon = icon("pen"),
        class = "btn btn-outline-secondary btn-sm",
        style = "height: 34px; padding: 2px 8px; flex-shrink: 0;",
        title = i18n$t("Rename column")
      )
    )
  })

  # Observer for column rename
  observeEvent(input$rename_col_btn, {
    old_name <- input$rename_col_select
    new_name <- trimws(input$rename_col_newname)
    
    if (is.null(old_name) || old_name == "" || is.null(new_name) || new_name == "") {
      showNotification(i18n$t("Please select a column and enter a new name."), type = "warning")
      return()
    }
    
    # Get current display data
    data <- display_oec_data()
    
    if (is.null(data)) return()
    
    # Check if old name exists
    if (!old_name %in% colnames(data)) {
      showNotification(i18n$t("Column not found."), type = "error")
      return()
    }
    
    # Check if new name already exists
    if (new_name %in% colnames(data) && new_name != old_name) {
      showNotification(i18n$t("A column with this name already exists."), type = "error")
      return()
    }
    
    # Rename column
    col_idx <- which(colnames(data) == old_name)
    colnames(data)[col_idx] <- new_name
    edited_preview_data$data <- data
    
    # Clear the new name input
    shinyjs::runjs(sprintf("$('#%s').val('');", ns("rename_col_newname")))
    
    showNotification(sprintf(i18n$t("Column renamed: %s → %s"), old_name, new_name), type = "message")
  })

  # Reactive value to store edited preview data
  edited_preview_data <- reactiveValues(data = NULL, source = NULL)
  manual_input_data <- reactiveValues(data = NULL)  # Persists across input type changes
  csv_data <- reactiveValues(data = NULL)  # Store CSV data separately
  
  # Clear edited_preview_data when switching input types to avoid stale data
  observeEvent(input$input_type, {
    edited_preview_data$data <- NULL
    prev_colnames$names <- NULL
  }, ignoreInit = TRUE)
  
  # Load CSV data when a new file is uploaded
  observeEvent(input$file_upload, {
    req(input$file_upload)
    tryCatch({
      csv_data$data <- read.csv(input$file_upload$datapath, stringsAsFactors = FALSE)
      example_data$data <- NULL
      edited_preview_data$data <- NULL
    }, error = function(e) {
      showNotification(paste(i18n$t("Error loading CSV:"), e$message), type = "error")
      csv_data$data <- NULL
    })
  })
  
  # Track column names to detect changes
  prev_colnames <- reactiveValues(names = NULL)

  # Render rHandsontable preview for uploaded/manual data (EDITABLE)
  # Uses table_render_data() to only re-render when base data changes, not on every edit
  output$oec_data_preview <- rhandsontable::renderRHandsontable({
    create_editable_hot(table_render_data())
  })

  # Update edited data when user modifies the preview table
  observeEvent(input$oec_data_preview, {
    if (!is.null(input$oec_data_preview)) {
      tryCatch({
        new_data <- rhandsontable::hot_to_r(input$oec_data_preview)
        
        # Check for valid data
        if (is.null(new_data) || !is.data.frame(new_data)) {
          return()
        }
        
        # Fix column names - replace NA, empty, or generic names with sensible defaults
        col_names <- colnames(new_data)
        for (i in seq_along(col_names)) {
          if (is.na(col_names[i]) || col_names[i] == "" || col_names[i] == " ") {
            col_names[i] <- paste0("Column", i)
          }
        }
        # Ensure unique names
        col_names <- make.unique(col_names, sep = "_")
        colnames(new_data) <- col_names
        
        edited_preview_data$data <- new_data
        
        # Also update the underlying data source for the current mode
        # This ensures data persists and is isolated between modes
        if (!is.null(input$input_type)) {
          if (input$input_type == "manual") {
            manual_input_data$data <- new_data
          } else if (input$input_type == "csv") {
            # Update whichever CSV source is active
            if (!is.null(csv_data$data)) {
              csv_data$data <- new_data
            } else if (!is.null(example_data$data)) {
              example_data$data <- new_data
            }
          }
        }
        
        # Track column names for change detection
        prev_colnames$names <- col_names
      }, error = function(e) {
        # Silently ignore rhandsontable internal errors (genColHeaders, afterChange)
      })
    }
  }, ignoreInit = TRUE)

  # Observer to handle "all" vs specific model type selections
  observeEvent(input$modtype,
    {
      if (!is.null(input$modtype) && length(input$modtype) > 0) {
        current_selection <- input$modtype
        previous_selection <- kinetic_results$previous_modtype

        # If "all" is in the new selection along with other types
        if ("all" %in% current_selection && length(current_selection) > 1) {
          # Determine what was just added
          if (!is.null(previous_selection)) {
            # Find newly added items
            newly_added <- setdiff(current_selection, previous_selection)

            if ("all" %in% newly_added) {
              # "all" was just added, keep only "all"
              updateSelectizeInput(session, "modtype", selected = "all")
              kinetic_results$previous_modtype <- "all"
            } else {
              # Other model(s) were added, remove "all"
              new_selection <- setdiff(current_selection, "all")
              updateSelectizeInput(session, "modtype", selected = new_selection)
              kinetic_results$previous_modtype <- new_selection
            }
          } else {
            # No previous selection, default to keeping "all" only
            updateSelectizeInput(session, "modtype", selected = "all")
            kinetic_results$previous_modtype <- "all"
          }
        } else {
          # Update previous selection tracking
          kinetic_results$previous_modtype <- current_selection
        }
      }
    },
    ignoreInit = TRUE,
    ignoreNULL = FALSE
  )

  # Reactive for default OEC variable selections
  default_oec_vars <- reactive({
    data <- oec_data()
    if (is.null(data) || ncol(data) < 2) {
      return(list(x_var = NULL, y_var = NULL, slv_var = "None"))
    }

    slv_var_default <- "None"
    solvent_column <- grep("solv", colnames(data), ignore.case = TRUE, value = TRUE)
    if (length(solvent_column) > 0) {
      slv_var_default <- solvent_column[1]
    }

    list(
      x_var = colnames(data)[1],
      y_var = colnames(data)[2],
      slv_var = slv_var_default
    )
  })

  # Warning for insufficient columns
  output$insufficient_columns_warning_ui <- renderUI({
    data <- oec_data()
    if (!is.null(data) && ncol(data) < 2) {
      div(
        class = "alert alert-danger",
        style = "margin: 10px 0 0 0; padding: 10px 12px; font-size: 12px;",
        icon("exclamation-triangle"),
        " ", tags$strong(i18n$t("Insufficient columns:")),
        " ", i18n$t("Data must have at least 2 columns (Time and Response).")
      )
    } else {
      NULL
    }
  })

  # Render dynamic UI for OEC variables
  output$oec_x_var_ui <- renderUI({
    data <- oec_data()
    if (is.null(data) || ncol(data) < 2) {
      return(
        div(
          style = "color: #888; font-style: italic; padding: 8px 0; white-space: nowrap;",
          icon("info-circle"),
          " ", i18n$t("Load data to select variables")
        )
      )
    }
    all_cols <- colnames(data)
    current_selection <- isolate(input$oec_x_var)
    
    # Keep current selection if valid (including ""), otherwise default to first
    selected <- if (!is.null(current_selection) && (current_selection == "" || current_selection %in% all_cols)) {
      current_selection
    } else {
      all_cols[1]
    }
    
    tags$div(
      tags$label(
        i18n$t("Time"),
        class = "control-label",
        style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
        actionLink(ns("clear_x_var"), i18n$t("Clear"),
          style = "font-size: 11px; padding: 1px 6px; border-radius: 3px; text-decoration: none; background-color: #dc3545; color: white; margin-left: auto;",
          title = i18n$t("Clear selection")
        )
      ),
      selectInput(ns("oec_x_var"), NULL, choices = c("None" = "", setNames(all_cols, all_cols)), selected = selected)
    )
  })

  output$oec_y_var_ui <- renderUI({
    data <- oec_data()
    if (is.null(data) || ncol(data) < 2) {
      return(NULL)
    }
    all_cols <- colnames(data)
    current_selection <- isolate(input$oec_y_var)
    
    # Keep current selection if valid (including ""), otherwise default to second column
    selected <- if (!is.null(current_selection) && (current_selection == "" || current_selection %in% all_cols)) {
      current_selection
    } else if (length(all_cols) >= 2) {
      all_cols[2]
    } else if (length(all_cols) > 0) {
      all_cols[1]
    } else {
      ""
    }
    
    tags$div(
      tags$label(
        i18n$t("Response"),
        class = "control-label",
        style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
        actionLink(ns("clear_y_var"), i18n$t("Clear"),
          style = "font-size: 11px; padding: 1px 6px; border-radius: 3px; text-decoration: none; background-color: #dc3545; color: white; margin-left: auto;",
          title = i18n$t("Clear selection")
        )
      ),
      selectInput(ns("oec_y_var"), NULL, choices = c("None" = "", setNames(all_cols, all_cols)), selected = selected)
    )
  })

  output$oec_slv_var_ui <- renderUI({
    data <- oec_data()
    use_solvent <- if (is.null(input$use_solvent)) FALSE else input$use_solvent
    
    # Don't show anything if solvent not enabled or insufficient columns for basic operation
    if (!use_solvent || is.null(data) || ncol(data) < 2) {
      return(NULL)
    }
    
    # Show disabled state if not enough columns for solvent (need 3+)
    if (ncol(data) < 3) {
      return(
        div(
          tags$label(i18n$t("Solvent"), class = "control-label"),
          tags$select(
            class = "form-control",
            disabled = "disabled",
            style = "background-color: #e9ecef;",
            tags$option(i18n$t("Need 3+ columns"))
          ),
          tags$small(class = "text-muted", i18n$t("Add more columns to enable solvent selection."))
        )
      )
    }
    
    all_cols <- colnames(data)
    current_selection <- isolate(input$oec_slv_var)
    
    # Keep current selection if valid (including ""), otherwise default to third column
    selected <- if (!is.null(current_selection) && (current_selection == "" || current_selection %in% all_cols)) {
      current_selection
    } else if (length(all_cols) >= 3) {
      all_cols[3]
    } else if (length(all_cols) > 0) {
      all_cols[1]
    } else {
      ""
    }
    
    tags$div(
      tags$label(
        i18n$t("Solvent"),
        class = "control-label",
        style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
        actionLink(ns("clear_slv_var"), i18n$t("Clear"),
          style = "font-size: 11px; padding: 1px 6px; border-radius: 3px; text-decoration: none; background-color: #dc3545; color: white; margin-left: auto;",
          title = i18n$t("Clear selection")
        )
      ),
      selectInput(ns("oec_slv_var"), NULL, choices = c("None" = "", setNames(all_cols, all_cols)), selected = selected)
    )
  })

  # Clear button observers for variable selectors
  observeEvent(input$clear_x_var, {
    updateSelectInput(session, "oec_x_var", selected = "")
  })
  
  observeEvent(input$clear_y_var, {
    updateSelectInput(session, "oec_y_var", selected = "")
  })
  
  observeEvent(input$clear_slv_var, {
    updateSelectInput(session, "oec_slv_var", selected = "")
  })

  # Validation for duplicate column selections (reactive for use in warnings)
  duplicate_column_validation <- reactive({
    data <- oec_data()
    
    # Handle wrapper borders
    wrapper_ids <- c(ns("oec_x_var_ui_wrapper"), ns("oec_y_var_ui_wrapper"), ns("oec_slv_var_ui_wrapper"))
    
    # No valid data - clear all borders and return
    if (is.null(data) || nrow(data) == 0) {
      for (wrapper_id in wrapper_ids) {
        shinyjs::runjs(sprintf(
          "$('#%s').css({'border': '', 'border-radius': '', 'padding': ''});",
          wrapper_id
        ))
      }
      return(list(has_duplicates = FALSE, has_empty = FALSE, duplicate_column = NULL, message = NULL))
    }
    
    available_cols <- colnames(data)
    
    # Only validate solvent if use_solvent is checked
    use_solvent <- if (is.null(input$use_solvent)) FALSE else input$use_solvent
    
    # Get current selections
    x_var <- input$oec_x_var
    y_var <- input$oec_y_var
    slv_var <- if (use_solvent) input$oec_slv_var else NULL
    
    # Check for empty required selections (Time and Response are required)
    x_empty <- is.null(x_var) || x_var == "" || x_var == "None"
    y_empty <- is.null(y_var) || y_var == "" || y_var == "None"
    slv_empty <- use_solvent && (is.null(slv_var) || slv_var == "" || slv_var == "None")
    has_empty <- x_empty || y_empty || slv_empty
    
    # Check if same column is selected for Time and Response (most common duplicate)
    same_time_response <- !x_empty && !y_empty && x_var == y_var
    
    # Filter out invalid selections for duplicate check
    selections <- c(x_var, y_var, slv_var)
    selections <- selections[!is.null(selections) & selections != "" & selections != "None"]
    selections <- selections[selections %in% available_cols]
    
    # Check for duplicates
    has_duplicates <- length(selections) != length(unique(selections))
    duplicate_column <- NULL
    
    if (has_duplicates) {
      duplicate_column <- selections[duplicated(selections)][1]
    }
    
    select_vals <- list(x_var, y_var, slv_var)
    is_empty <- list(x_empty, y_empty, slv_empty)
    
    for (i in seq_along(wrapper_ids)) {
      wrapper_id <- wrapper_ids[i]
      current_val <- select_vals[[i]]
      empty_val <- is_empty[[i]]
      
      # Skip solvent wrapper if not using solvent
      if (i == 3 && !use_solvent) {
        shinyjs::runjs(sprintf(
          "$('#%s').css({'border': '', 'border-radius': '', 'padding': ''});",
          wrapper_id
        ))
        next
      }
      
      # Apply orange border for empty OR same column selection (for time/response)
      if (empty_val || (i <= 2 && same_time_response)) {
        shinyjs::runjs(sprintf(
          "$('#%s').css({'border': '2px solid #ffc107', 'border-radius': '4px', 'padding': '5px'});",
          wrapper_id
        ))
      } else if (!is.null(current_val) && current_val != "" && current_val != "None" && current_val %in% available_cols) {
        if (has_duplicates && !is.null(duplicate_column) && current_val == duplicate_column) {
          # Add orange border for duplicates
          shinyjs::runjs(sprintf(
            "$('#%s').css({'border': '2px solid #ffc107', 'border-radius': '4px', 'padding': '5px'});",
            wrapper_id
          ))
        } else {
          # Remove border
          shinyjs::runjs(sprintf(
            "$('#%s').css({'border': '', 'border-radius': '', 'padding': ''});",
            wrapper_id
          ))
        }
      } else {
        # Remove border for invalid/missing selections
        shinyjs::runjs(sprintf(
          "$('#%s').css({'border': '', 'border-radius': '', 'padding': ''});",
          wrapper_id
        ))
      }
    }

    list(
      has_duplicates = has_duplicates,
      has_empty = has_empty,
      duplicate_column = duplicate_column,
      message = if (has_duplicates) sprintf("Column '%s' is selected multiple times!", duplicate_column) else NULL
    )
  })

  # Validation message output (now empty - warning moved to cumulative_warning_ui area)
  output$column_validation_message <- renderUI({
    # This output is kept for backward compatibility but validation display is moved
    NULL
  })

  # Reactive for validated OEC data
  validated_oec_data <- reactive({
    data <- oec_data()
    req(data)

    # Use input values if available, otherwise use defaults
    x_var <- input$oec_x_var %||% default_oec_vars()$x_var
    y_var <- input$oec_y_var %||% default_oec_vars()$y_var
    slv_var <- input$oec_slv_var %||% default_oec_vars()$slv_var

    req(x_var, y_var) # Ensure x and y variables are selected

    if (!x_var %in% colnames(data) || !y_var %in% colnames(data)) {
      showNotification(i18n$t("Selected OEC variables not found in data."), type = "error")
      return(NULL)
    }
    if (!is.numeric(data[[x_var]]) || !is.numeric(data[[y_var]])) {
      showNotification(i18n$t("Time and response columns must be numeric."), type = "error")
      return(NULL)
    }
    data
  })

  # Reactive for base OEC data (from file upload, example, or manual input)
  base_oec_data <- reactive({
    req(input$input_type)
    data <- NULL

    if (input$input_type == "csv") {
      if (!is.null(csv_data$data)) {
        # Use stored CSV data
        data <- csv_data$data
      } else if (!is.null(example_data$data)) {
        # Example data loaded via button
        data <- example_data$data
      }
      # If no file uploaded and no example data, return NULL
    } else {
      # Manual input mode - use persistent manual data, then default empty table
      # NOTE: Do NOT fall back to example_data$data - that belongs to CSV mode
      if (!is.null(manual_input_data$data)) {
        data <- manual_input_data$data
      } else {
        # Default empty 8-column data for manual input (6 rows)
        # Extra columns allow pasting data with more columns without crashing
        data <- data.frame(
          Time = rep(NA_real_, 6),
          Yield = rep(NA_real_, 6),
          Col3 = rep(NA_real_, 6),
          Col4 = rep(NA_real_, 6),
          Col5 = rep(NA_real_, 6),
          Col6 = rep(NA_real_, 6),
          Col7 = rep(NA_real_, 6),
          Col8 = rep(NA_real_, 6),
          stringsAsFactors = FALSE
        )
      }
    }
    data
  })
  
  # Reactive for display data (includes NA rows for preview table)
  # This is used for calculations, not for rendering the table
  display_oec_data <- reactive({
    # If user has edited the preview table, use that data
    if (!is.null(edited_preview_data$data)) {
      return(edited_preview_data$data)
    }
    # Otherwise use base data
    base_oec_data()
  })
  
  # Reactive specifically for triggering table re-render
  # Uses display_oec_data() so column changes (additions/renames) show immediately
  table_render_data <- reactive({
    display_oec_data()
  })
  
  # Reactive for final OEC data (filters NA rows for calculations)
  oec_data <- reactive({
    data <- display_oec_data()
    
    # Remove rows that are completely NA for calculations
    if (!is.null(data) && nrow(data) > 0) {
      complete_rows <- apply(data, 1, function(row) !all(is.na(row)))
      data <- data[complete_rows, , drop = FALSE]
      if (nrow(data) == 0) data <- NULL
    }
    data
  })
  
  # Reactive value to store example data
  example_data <- reactiveValues(data = NULL)
  
  # Observer for loading example data
  observeEvent(input$load_example_data, {
    default_path <- system.file("extdata", "gui-kinetic_bic_oec.csv", package = "supeRcrit")
    if (file.exists(default_path)) {
      raw_data <- read.csv(default_path, stringsAsFactors = FALSE)
      
      if (input$input_type == "manual") {
        # Manual input mode - store in manual_input_data
        manual_input_data$data <- raw_data
        edited_preview_data$data <- NULL
      } else if (input$input_type == "csv") {
        # CSV mode - clear csv_data first, then set example_data
        csv_data$data <- NULL
        edited_preview_data$data <- NULL
        example_data$data <- raw_data
      }
      
      showNotification(i18n$t("Example data loaded successfully!"), type = "message")
    } else {
      showNotification(i18n$t("Example data file not found."), type = "error")
    }
  })

  # Clear data button - removes all data and resets preview
  observeEvent(input$clear_data, {
    if (input$input_type == "manual") {
      # In manual mode, revert to empty starting table
      # NOTE: Do NOT clear example_data$data here - it belongs to CSV mode
      manual_input_data$data <- NULL
      edited_preview_data$data <- data.frame(
        Time = rep(NA_real_, 6),
        Yield = rep(NA_real_, 6),
        Col3 = rep(NA_real_, 6),
        Col4 = rep(NA_real_, 6),
        Col5 = rep(NA_real_, 6),
        Col6 = rep(NA_real_, 6),
        Col7 = rep(NA_real_, 6),
        Col8 = rep(NA_real_, 6),
        stringsAsFactors = FALSE
      )
    } else {
      example_data$data <- NULL
      edited_preview_data$data <- NULL
      csv_data$data <- NULL
      shinyjs::reset("file_upload")
    }
    showNotification(i18n$t("Data cleared."), type = "message")
  })

  # Open n (CER) estimation modal when link is clicked
  observeEvent(input$estimate_n_link, {
    showModal(modalDialog(
      title = i18n$t("Estimate Number of Observations (CER)"),
      size = "m",
      easyClose = TRUE,
      tags$p(i18n$t("This will estimate the observation number at which the Constant Extraction Rate (CER) period ends using segmented regression.")),
      fluidRow(
        column(6,
          selectInput(ns("segmode"), i18n$t("Segmentation Method"),
            choices = setNames(c("seg", "step"), c(i18n$t("Segmented Regression"), i18n$t("Step Regression"))),
            selected = "seg"
          )
        ),
        column(6,
          div(style = "margin-top: 25px;",
            actionButton(ns("preview_n"), i18n$t("Preview"), 
              icon = icon("eye"),
              class = "btn btn-outline-secondary btn-sm"
            )
          )
        )
      ),
      tags$hr(),
      div(
        style = "text-align: center;",
        plotOutput(ns("n_estimation_plot"), height = "250px")
      ),
      verbatimTextOutput(ns("n_estimation_result")),
      footer = tagList(
        modalButton(i18n$t("Cancel")),
        actionButton(ns("estimate_n"), i18n$t("Apply"), 
          icon = icon("check"),
          class = "btn btn-primary"
        )
      )
    ))
  })

  # Reactive to store n estimation result
  n_estimation <- reactiveValues(value = NULL, plot_data = NULL)

  # Preview n estimation (generate plot)
  observeEvent(input$preview_n, {
    # Check if data is loaded
    if (is.null(oec_data()) || nrow(oec_data()) == 0) {
      showNotification(i18n$t("Please load data first."), type = "error")
      return()
    }
    
    # Validate the data
    data <- validated_oec_data()
    if (is.null(data)) {
      showNotification(i18n$t("Data validation failed. Please check your data and variable selections."), type = "error")
      return()
    }

    tryCatch({
      # Get variable names
      x_var_val <- input$oec_x_var %||% default_oec_vars()$x_var
      y_var_val <- input$oec_y_var %||% default_oec_vars()$y_var
      segmode_val <- input$segmode %||% "seg"
      
      # Check if data needs to be converted to cumulative
      # oec_bp expects cumulative data for proper breakpoint detection
      y_vals <- data[[y_var_val]]
      
      # Check if data is cumulative (monotonically increasing)
      is_cumulative <- all(diff(y_vals) >= 0, na.rm = TRUE)
      
      # Create a copy of data for estimation
      est_data <- data
      converted <- FALSE
      
      # If data is not cumulative, convert it by cumsum
      if (!is_cumulative) {
        est_data[[y_var_val]] <- cumsum(y_vals)
        converted <- TRUE
      }

      # Call oec_bp function
      n_result <- supeRcrit::oec_bp(
        input = est_data,
        x = x_var_val,
        y = y_var_val,
        plt = FALSE,
        segmode = segmode_val
      )

      # Store result (use converted data for plot)
      n_estimation$value <- n_result
      n_estimation$plot_data <- list(
        data = est_data,
        x_var = x_var_val,
        y_var = y_var_val,
        n = n_result,
        converted = converted
      )
      
      if (converted) {
        showNotification(i18n$t("Data was converted to cumulative for breakpoint estimation."), type = "message")
      }
    },
    error = function(e) {
      n_estimation$value <- NULL
      n_estimation$plot_data <- NULL
      showNotification(paste(i18n$t("Error:"), e$message), type = "error")
    })
  })

  # Render n estimation plot
  output$n_estimation_plot <- renderPlot({
    req(n_estimation$plot_data)
    
    pd <- n_estimation$plot_data
    x_vals <- pd$data[[pd$x_var]]
    y_vals <- pd$data[[pd$y_var]]
    n_val <- pd$n
    converted <- if (!is.null(pd$converted)) pd$converted else FALSE
    
    # Simple plot with breakpoint
    par(mar = c(4, 4, 2, 1))
    y_label <- if (converted) paste0(pd$y_var, " (", i18n$t("cumulative"), ")") else pd$y_var
    plot(x_vals, y_vals, 
      pch = 19, col = "steelblue",
      xlab = pd$x_var, ylab = y_label,
      main = ""
    )
    
    # Add vertical line at breakpoint
    if (!is.null(n_val) && n_val <= length(x_vals)) {
      abline(v = x_vals[n_val], col = "red", lwd = 2, lty = 2)
      points(x_vals[n_val], y_vals[n_val], pch = 19, col = "red", cex = 2)
    }
    
    # Add legend
    legend("bottomright", 
      legend = c(i18n$t("Data"), i18n$t("CER Endpoint")),
      col = c("steelblue", "red"),
      pch = c(19, 19),
      lty = c(NA, 2),
      bty = "n"
    )
  }, bg = "transparent")

  # Render n estimation result text
  output$n_estimation_result <- renderText({
    req(n_estimation$value)
    sprintf("%s: %d", i18n$t("Estimated observation number"), n_estimation$value)
  })

  # Apply n estimation result
  observeEvent(input$estimate_n, {
    if (!is.null(n_estimation$value) && n_estimation$value >= 2) {
      updateNumericInput(session, "n", value = n_estimation$value)
      removeModal()
      showNotification(
        sprintf(i18n$t("Estimated n: %s"), n_estimation$value),
        type = "message"
      )
    } else {
      showNotification(i18n$t("Please click Preview first to estimate n."), type = "warning")
    }
  })

  # Open cu estimation modal when link is clicked
  observeEvent(input$estimate_cu_link, {
    showModal(modalDialog(
      title = HTML(paste0(i18n$t("Estimate Max Extractable Fraction"), " <em>c</em><sub>u</sub>")),
      size = "s",
      easyClose = TRUE,
      tags$p(HTML(paste0(i18n$t("This will estimate the maximum extractable material fraction"), " (<em>c</em><sub>u</sub>) ", i18n$t("from your data.")))),
      textInput(ns("c3_est"), HTML(paste0("<em>c</em><sub>3</sub> (", i18n$t("initial estimate"), ")")), value = defaults$c3_est),
      tags$small(class = "text-muted", HTML(paste0("<em>c</em><sub>3</sub> ", i18n$t("is used as an initial value for the optimization algorithm.")))),
      footer = tagList(
        modalButton(i18n$t("Cancel")),
        actionButton(ns("estimate_cu"), i18n$t("Calculate"), 
          icon = icon("calculator"),
          class = "btn btn-primary"
        )
      )
    ))
  })

  # Estimate cu (Max Extractable Material Fraction) using bicmod with modtype="cu"
  observeEvent(input$estimate_cu, {
    # Check if data is loaded
    if (is.null(oec_data()) || nrow(oec_data()) == 0) {
      showNotification(i18n$t("Please load data before estimating cu."), type = "error")
      return()
    }
    
    # Validate the data
    data <- validated_oec_data()
    if (is.null(data)) {
      showNotification(i18n$t("Data validation failed. Please check your data and variable selections."), type = "error")
      return()
    }

    # Validate n parameter
    if (is.null(input$n) || is.na(input$n) || input$n < 2) {
      showNotification(i18n$t("Number of Observations (CER) must be at least 2."), type = "error")
      return()
    }

    # Validate flow rate is provided when solvent data is not used
    # When accordion is closed, infer use_solvent from whether a solvent column was auto-detected
    use_solvent <- input$use_solvent
    if (is.null(use_solvent)) {
      # Accordion not opened - check if data has a solvent column
      slv_default <- default_oec_vars()$slv_var
      use_solvent <- !is.null(slv_default) && slv_default != "None" && slv_default != ""
    }
    flow_empty <- is.null(input$flow) || is.na(input$flow) || input$flow == ""
    if (!use_solvent && flow_empty) {
      showNotification(i18n$t("Flow Rate is required when Solvent Data is not provided."), type = "error")
      return()
    }

    tryCatch(
      {
        withProgress(message = i18n$t("Estimating cu..."), value = 0, {
          incProgress(0.3, detail = i18n$t("Preparing parameters..."))

          # Prepare OEC variables
          x_var_val <- input$oec_x_var %||% default_oec_vars()$x_var
          y_var_val <- input$oec_y_var %||% default_oec_vars()$y_var
          slv_var_val <- input$oec_slv_var %||% default_oec_vars()$slv_var

          oec_vars_list <- c(x = x_var_val, y = y_var_val)
          # Only add solvent variable if use_solvent is checked and variable is valid
          if (use_solvent && !is.null(slv_var_val) && length(slv_var_val) > 0 && !is.na(slv_var_val) && slv_var_val != "None" && slv_var_val != "") {
            oec_vars_list <- c(oec_vars_list, slv = slv_var_val)
          }

          # Prepare parameters (without cu since we're estimating it)
          # Use %||% defaults to handle inputs that haven't been rendered yet
          D_val <- input$D %||% defaults$D
          L_val <- input$L %||% defaults$L
          D_m <- switch(dim_unit_D(), "m" = D_val, "cm" = D_val / 100, "mm" = D_val / 1000)
          L_m <- switch(dim_unit_L(), "m" = L_val, "cm" = L_val / 100, "mm" = L_val / 1000)
          
          pars_list <- c(
            pres = input$pres %||% defaults$pres,
            temp = input$temp %||% defaults$temp,
            mass_in = input$mass_in %||% defaults$mass_in,
            moisture = input$moisture %||% defaults$moisture,
            D = D_m,
            L = L_m,
            etoh = input$etoh %||% defaults$etoh,
            dr = input$dr %||% defaults$dr,
            dp = (input$dp %||% defaults$dp) / 1000,
            n = input$n %||% defaults$n,
            flow = if (is.null(input$flow) || is.na(input$flow)) NA else input$flow
          )

          # Prepare flowpar
          fp_pres <- input$flowpar_pres %||% defaults$flowpar_pres
          fp_temp <- input$flowpar_temp %||% defaults$flowpar_temp
          flowpar_vec <- if (!is.null(fp_pres) && !is.null(fp_temp) &&
            !is.na(fp_pres) && !is.na(fp_temp)) {
            c(fp_pres, fp_temp)
          } else {
            rep(NA, 2)
          }

          # Prepare optimization estimates (c3 is required for cu estimation)
          ksas_starts_val <- as.numeric(input$ksas_est %||% defaults$ksas_est)
          opt_est_val <- c(
            r = as.numeric(input$r_est %||% defaults$r_est),
            ksas = ksas_starts_val[1],
            qc = as.numeric(input$qc_est %||% defaults$qc_est),
            thetaf = as.numeric(input$thetaf_est %||% defaults$thetaf_est),
            ti = as.numeric(input$ti_est %||% defaults$ti_est),
            kf = as.numeric(input$kf_est %||% defaults$kf_est),
            c3 = as.numeric(input$c3_est %||% defaults$c3_est)
          )

          # Prepare units
          units_list <- c(flow = input$flow_units %||% defaults$flow_units, resp = input$resp_units %||% defaults$resp_units)

          incProgress(0.5, detail = i18n$t("Running estimation..."))

          # Call bicmod with modtype="cu"
          cu_result <- supeRcrit::bicmod(
            oec = validated_oec_data(),
            oec_vars = oec_vars_list,
            pars = pars_list,
            opt_est = opt_est_val,
            etoh_frac = input$etoh_frac %||% defaults$etoh_frac %||% 0,
            flowpar = flowpar_vec,
            ro_co2 = NA,
            cumulative = input$cumulative %||% defaults$cumulative,
            mass_flow = input$mass_flow %||% defaults$mass_flow,
            modtype = "cu",
            units = units_list,
            silent = TRUE,
            ksas0 = ksas_starts_val
          )

          incProgress(0.9, detail = i18n$t("Done"))

          # Extract cu value and update the input
          if (!is.null(cu_result) && !is.null(cu_result$cu)) {
            cu_value <- round(cu_result$cu, 4)
            updateNumericInput(session, "cu", value = cu_value)
            # Close the modal
            removeModal()
            showNotification(
              sprintf(i18n$t("Estimated cu: %s"), cu_value),
              type = "message"
            )
          } else {
            showNotification(i18n$t("Could not estimate cu from data."), type = "warning")
          }
        })
      },
      error = function(e) {
        showNotification(paste(i18n$t("Error estimating cu:"), e$message), type = "error")
      }
    )
  })

  # Calculate kinetic model
  observeEvent(input$calculate, {
    # Helper function to get input value or default
    # This handles the case where accordion sections haven't been opened yet
    get_input_or_default <- function(input_name) {
      val <- input[[input_name]]
      if (is.null(val)) {
        defaults[[input_name]]
      } else {
        val
      }
    }
    
    # Check if data is loaded
    if (is.null(oec_data()) || nrow(oec_data()) == 0) {
      showNotification(i18n$t("Please load data before calculating."), type = "error")
      return()
    }
    
    # Check for duplicate column selection
    x_var <- input$oec_x_var
    y_var <- input$oec_y_var
    if (!is.null(x_var) && !is.null(y_var) && 
        x_var != "" && y_var != "" && 
        x_var == y_var) {
      showNotification(i18n$t("Time and Response columns must be different."), type = "error")
      return()
    }
    
    # Validate the data
    data <- validated_oec_data()
    if (is.null(data)) {
      showNotification(i18n$t("Data validation failed. Please check your data and variable selections."), type = "error")
      return()
    }

    # Validate n parameter (Number of Observations CER)
    n_val <- get_input_or_default("n")
    if (is.null(n_val) || is.na(n_val) || n_val < 2) {
      showNotification(i18n$t("Number of Observations (CER) must be at least 2."), type = "error")
      return()
    }

    # Validate flow rate is provided when solvent data is not used
    use_solvent <- get_input_or_default("use_solvent")
    use_solvent <- if (is.null(use_solvent)) FALSE else use_solvent
    flow_val <- get_input_or_default("flow")
    flow_empty <- is.null(flow_val) || is.na(flow_val) || flow_val == ""
    if (!use_solvent && flow_empty) {
      showNotification(i18n$t("Flow Rate is required when Solvent Data is not provided."), type = "error")
      return()
    }

    tryCatch(
      {
        # Show progress
        withProgress(message = i18n$t("Calculating BIC model..."), value = 0, {
          incProgress(0.2, detail = i18n$t("Preparing parameters..."))

          # Prepare OEC variables, using input values or defaults
          x_var_val <- input$oec_x_var %||% default_oec_vars()$x_var
          y_var_val <- input$oec_y_var %||% default_oec_vars()$y_var
          slv_var_val <- input$oec_slv_var %||% default_oec_vars()$slv_var

          oec_vars_list <- c(x = x_var_val, y = y_var_val)
          # Only add solvent variable if use_solvent is checked and variable is valid
          if (use_solvent && !is.null(slv_var_val) && length(slv_var_val) > 0 && !is.na(slv_var_val) && slv_var_val != "None" && slv_var_val != "") {
            oec_vars_list <- c(oec_vars_list, slv = slv_var_val)
          }

          # Prepare parameters for bicmod
          pars_list <- c(
            pres = get_input_or_default("pres"),
            temp = get_input_or_default("temp"),
            mass_in = get_input_or_default("mass_in"),
            moisture = get_input_or_default("moisture"),
            D = switch(dim_unit_D(), "m" = get_input_or_default("D"), "cm" = get_input_or_default("D") / 100, "mm" = get_input_or_default("D") / 1000),
            L = switch(dim_unit_L(), "m" = get_input_or_default("L"), "cm" = get_input_or_default("L") / 100, "mm" = get_input_or_default("L") / 1000),
            etoh = get_input_or_default("etoh"),
            dr = get_input_or_default("dr"),
            dp = get_input_or_default("dp") / 1000,
            n = get_input_or_default("n"),
            flow = {
              flow_val <- get_input_or_default("flow")
              if (is.null(flow_val) || is.na(flow_val)) NA else flow_val
            }
          )

          # Add cu parameter if provided (not NA)
          cu_val <- get_input_or_default("cu")
          if (!is.null(cu_val) && !is.na(cu_val)) {
            pars_list <- c(pars_list, cu = cu_val)
          }

          # Prepare flowpar (temperature and pressure for flow measurement)
          flowpar_temp_val <- get_input_or_default("flowpar_temp")
          flowpar_pres_val <- get_input_or_default("flowpar_pres")
          flowpar_vec <- if (!is.null(flowpar_temp_val) && !is.null(flowpar_pres_val) &&
            !is.na(flowpar_temp_val) && !is.na(flowpar_pres_val)) {
            c(flowpar_pres_val, flowpar_temp_val)
          } else {
            rep(NA, 2)
          }

          # Prepare optimization estimates
          # Validate and convert custom estimates
          ksas_starts_val <- as.numeric(get_input_or_default("ksas_est"))
          custom_est <- c(
            r = as.numeric(get_input_or_default("r_est")),
            ksas = ksas_starts_val[1],
            qc = as.numeric(get_input_or_default("qc_est")),
            thetaf = as.numeric(get_input_or_default("thetaf_est")),
            ti = as.numeric(get_input_or_default("ti_est")),
            kf = as.numeric(get_input_or_default("kf_est")),
            c3 = as.numeric(get_input_or_default("c3_est"))
          )

          # Check for invalid values
          if (any(is.na(custom_est))) {
            stop("All optimization parameters must be valid numbers")
          }
          opt_est_val <- custom_est

          # Prepare units
          units_list <- c(flow = get_input_or_default("flow_units"), resp = get_input_or_default("resp_units"))

          incProgress(0.6, detail = i18n$t("Running model..."))

          # DEBUG: Print all parameters being sent to bicmod
          # cat("\n=== DEBUG: bicmod() Parameters ===\n")
          # cat("oec_vars:", paste(names(oec_vars_list), oec_vars_list, sep="=", collapse=", "), "\n")
          # cat("pars:", paste(names(pars_list), pars_list, sep="=", collapse=", "), "\n")
          # cat("opt_est:", paste(names(opt_est_val), opt_est_val, sep="=", collapse=", "), "\n")
          # cat("units:", paste(names(units_list), units_list, sep="=", collapse=", "), "\n")
          # cat("etoh_frac:", input$etoh_frac, "\n")
          # cat("flowpar:", paste(flowpar_vec, collapse=", "), "\n")
          # cat("ro_co2:", input$ro_co2, "\n")
          # cat("tmax:", input$tmax, "\n")
          # cat("qmax:", input$qmax, "\n")
          # cat("cumulative:", input$cumulative, "\n")
          # cat("mass_flow:", input$mass_flow, "\n")
          # cat("aggreg:", input$aggreg, "\n")
          # cat("modtype:", input$modtype, "\n")
          # cat("oec data dimensions:", nrow(validated_oec_data()), "x", ncol(validated_oec_data()), "\n")
          # cat("===================================\n\n")

          # Call bicmod function
          model_result <- supeRcrit::bicmod(
            oec = validated_oec_data(),
            oec_vars = oec_vars_list,
            pars = pars_list,
            opt_est = opt_est_val,
            etoh_frac = {
              val <- get_input_or_default("etoh_frac")
              if (is.null(val)) 0 else val
            },
            flowpar = flowpar_vec,
            ro_co2 = if (ro_co2_enabled()) get_input_or_default("ro_co2") else NA,
            tmax = if (tmax_enabled()) get_input_or_default("tmax") else NA,
            qmax = if (qmax_enabled()) get_input_or_default("qmax") else NA,
            cumulative = get_input_or_default("cumulative"),
            mass_flow = if (solvent_selected()) isTRUE(get_input_or_default("mass_flow")) else grepl("^g/|^kg/", get_input_or_default("flow_units")),
            draw = TRUE,
            aggreg = get_input_or_default("aggreg"),
            modtype = get_input_or_default("modtype"),
            units = units_list,
            ksas0 = ksas_starts_val
          )

          # Fix duplicate names in plots list
          plots_list <- model_result$plots
          if (!is.null(plots_list) && length(plots_list) > 0) {
            # Make names unique by adding suffixes
            names(plots_list) <- make.unique(names(plots_list), sep = "_")
          }

          # Store results
          kinetic_results$full_result <- model_result
          kinetic_results$sim_result <- model_result$sim
          kinetic_results$ct_result <- model_result$ct
          kinetic_results$cmp_result <- model_result$cmp
          kinetic_results$plots_list <- plots_list # Use the fixed plots list
          kinetic_results$data <- model_result$data
          kinetic_results$input_params <- model_result$input
          kinetic_results$resp_units <- input$resp_units %||% "percent"
          kinetic_results$etoh_frac_snapshot <- input$etoh_frac %||% 0
          kinetic_results$flow_units_snapshot <- input$flow_units %||% "g/min"
          kinetic_results$flow_value_snapshot <- input$flow %||% NA
          kinetic_results$flowpar_snapshot <- c(input$flowpar_pres %||% NA, input$flowpar_temp %||% NA)
          kinetic_results$solvent_mode_snapshot <- solvent_selected()


          # Determine available models based on fixed plot names
          available <- names(plots_list)
          # Clean up the names for available models (remove suffixes if needed)
          available_clean <- gsub("_\\d+$", "", available) # Remove _1, _2 etc suffixes
          # available_clean <- unique(available_clean)
          kinetic_results$available_models <- available_clean


          # Determine available models
          available <- c()
          if (!is.null(model_result$sim)) available <- c(available, "sim")
          if (!is.null(model_result$ct)) available <- c(available, "ct")
          if (!is.null(model_result$cmp)) {
            # Check for cmp2 and cmp3 plots specifically if cmp is present
            if (!is.null(model_result$plots$cmp2)) available <- c(available, "cmp2")
            if (!is.null(model_result$plots$cmp3)) available <- c(available, "cmp3")
          }
          kinetic_results$available_models <- available

          incProgress(1, detail = i18n$t("Completed!"))
        })

        showNotification(i18n$t("BIC model calculated successfully!"), type = "message")
      },
      error = function(e) {
        # Parse error message and provide user-friendly feedback
        error_msg <- e$message
        
        # Check for common error patterns and provide helpful messages
        if (grepl("f\\(\\) values at end points not of opposite sign", error_msg, ignore.case = TRUE)) {
          user_msg <- i18n$t("Model fitting failed: The solver could not find a valid solution. This often occurs when input parameters are inconsistent (e.g., incorrect Mass Flow Rate setting, mismatched units, or inappropriate parameter values). Please verify your input data and parameters.")
        } else if (grepl("mass_flow.*must be logical", error_msg, ignore.case = TRUE)) {
          user_msg <- i18n$t("Invalid Mass Flow Rate setting. Please check the checkbox value.")
        } else if (grepl("flow.*units.*must not be volumetric", error_msg, ignore.case = TRUE)) {
          user_msg <- i18n$t("When Mass Flow Rate is enabled, flow units must be mass-based (g/min or kg/h), not volumetric.")
        } else if (grepl("Pressure must be between", error_msg, ignore.case = TRUE)) {
          user_msg <- i18n$t("Pressure value is out of valid range (1-1000 bar).")
        } else if (grepl("Temperature must be between", error_msg, ignore.case = TRUE)) {
          user_msg <- i18n$t("Temperature value is out of valid range (-50 to 300 °C).")
        } else if (grepl("All optimization methods failed", error_msg, ignore.case = TRUE)) {
          user_msg <- i18n$t("All optimization methods failed. Please check your input data and ensure parameters are reasonable for the BIC model.")
        } else {
          # Default: show the original error
          user_msg <- paste(i18n$t("Error calculating BIC model:"), error_msg)
        }
        
        showNotification(user_msg, type = "error", duration = 10)

        kinetic_results$full_result <- NULL
        kinetic_results$sim_result <- NULL
        kinetic_results$ct_result <- NULL
        kinetic_results$cmp_result <- NULL
        kinetic_results$plots_list <- NULL
        kinetic_results$data <- NULL
        kinetic_results$input_params <- NULL
        kinetic_results$available_models <- c()
      }
    )
  })

  # Reactive for conditional panels
  output$has_any_results <- reactive({
    !is.null(kinetic_results$sim_result) || 
    !is.null(kinetic_results$ct_result) || 
    !is.null(kinetic_results$cmp_result)
  })
  outputOptions(output, "has_any_results", suspendWhenHidden = FALSE)

  # Observer to enable/disable result tabs based on results availability
  observe({
    has_results <- !is.null(kinetic_results$sim_result) || 
                   !is.null(kinetic_results$ct_result) || 
                   !is.null(kinetic_results$cmp_result)
    if (has_results) {
      session$sendCustomMessage("enableTabs", list(tabsetId = ns("results_tabs")))
    } else {
      session$sendCustomMessage("disableTabs", list(tabsetId = ns("results_tabs"), keepFirst = TRUE))
    }
  })

  output$has_sim <- reactive({
    !is.null(kinetic_results$sim_result)
  })
  outputOptions(output, "has_sim", suspendWhenHidden = FALSE)

  output$has_ct <- reactive({
    !is.null(kinetic_results$ct_result)
  })
  outputOptions(output, "has_ct", suspendWhenHidden = FALSE)

  output$has_cmp <- reactive({
    !is.null(kinetic_results$cmp_result)
  })
  outputOptions(output, "has_cmp", suspendWhenHidden = FALSE)

  output$has_multiple_models <- reactive({
    length(kinetic_results$available_models) > 1
  })
  outputOptions(output, "has_multiple_models", suspendWhenHidden = FALSE)

  # Plot selector UI
  output$plot_selector_ui <- renderUI({
    req(kinetic_results$plots_list)

    # Get all available plot names
    plot_names <- names(kinetic_results$plots_list)
    req(plot_names)

    # Create display choices
    choices_display <- c()
    choices_values <- c()
    plot_names <- sort(plot_names)


    for (plot_name in plot_names) {
      # Map plot names to descriptive display names
      if (plot_name == "sim") {
        display_name <- i18n$t("Simplified Model (solvent/material ratio)")
      } else if (plot_name == "sim_time") {
        display_name <- i18n$t("Simplified Model (time-based)")
      } else if (plot_name == "ct") {
        display_name <- i18n$t("Characteristic Times Model (time-based)")
      } else if (plot_name == "cmp2") {
        display_name <- i18n$t("Complete Model - 2 Regions (solvent/material ratio)")
      } else if (plot_name == "cmp3") {
        display_name <- i18n$t("Complete Model - 3 Regions (solvent/material ratio)")
      } else if (plot_name == "cmp2_time") {
        display_name <- i18n$t("Complete Model - 2 Regions (time-based)")
      } else if (plot_name == "cmp3_time") {
        display_name <- i18n$t("Complete Model - 3 Regions (time-based)")
      } else {
        # Fallback: try to make it more descriptive
        display_name <- gsub("_", " ", plot_name)
        display_name <- tools::toTitleCase(display_name)
      }

      choices_display <- c(choices_display, display_name)
      choices_values <- c(choices_values, plot_name)
    }

    # Liste ile choices oluşturma
    choices_list <- setNames(choices_values, choices_display)

    selectInput(ns("selected_plot"),
      i18n$t("Select Model Plot"),
      choices = choices_list,
      selected = choices_values[1],
      width = "100%"
    )
  })

  # Render selected model plot using unified selectors
  output$selected_model_plot <- renderPlotly({
    sel_model <- input$selected_model_detail
    sel_xaxis <- input$plot_xaxis_type %||% "time"
    req(sel_model, kinetic_results$plots_list)
    
    # Map model selector + x-axis to plot name
    plot_name <- if (sel_model == "ct") {
      "ct"  # CT only has time-based
    } else if (sel_model == "sim") {
      if (sel_xaxis == "time") "sim_time" else "sim"
    } else if (sel_model == "cmp") {
      # Use cmp2 or cmp3 based on what's available
      if (sel_xaxis == "time") {
        if ("cmp2_time" %in% names(kinetic_results$plots_list)) "cmp2_time" else "cmp3_time"
      } else {
        if ("cmp2" %in% names(kinetic_results$plots_list)) "cmp2" else "cmp3"
      }
    } else sel_model
    
    plot_obj <- kinetic_results$plots_list[[plot_name]]
    req(plot_obj)

    tryCatch(
      {
        # Check if it's a list (which might happen with certain plot combinations)
        if (is.list(plot_obj) && !("ggplot" %in% class(plot_obj)) && !("plotly" %in% class(plot_obj))) {
          # If it's a plain list, try to get the first element
          if (length(plot_obj) > 0) {
            plot_obj <- plot_obj[[1]]
          } else {
            stop("Empty plot list")
          }
        }

        # Define plot labels mapping for translation
        plot_labels <- list(
          sim = list(
            title = "Simple BIC model",
            x = "q (kg/kg)",
            y = "e (kg/kg)"
          ),
          sim_time = list(
            title = "Simple BIC model (time-based)",
            x = "Time (min)",
            y = "e (kg/kg)"
          ),
          ct = list(
            title = "Simple BIC characteristic times model",
            x = "Time (min)",
            y = "e (kg/kg)"
          ),
          cmp2 = list(
            title = "Complete BIC model",
            x = "q (kg/kg)",
            y = "e (kg/kg)"
          ),
          cmp3 = list(
            title = "Complete BIC model",
            x = "q (kg/kg)",
            y = "e (kg/kg)"
          ),
          cmp2_time = list(
            title = "Complete BIC model (time-based)",
            x = "Time (min)",
            y = "e (kg/kg)"
          ),
          cmp3_time = list(
            title = "Complete BIC model (time-based)",
            x = "Time (min)",
            y = "e (kg/kg)"
          )
        )

        # Apply translations if plot is ggplot and labels are defined
        if ("ggplot" %in% class(plot_obj) && plot_name %in% names(plot_labels)) {
          labels <- plot_labels[[plot_name]]
          plot_obj <- translate_plot_labels(
            plot_obj,
            i18n_r(),
            title = labels$title,
            x = labels$x,
            y = labels$y
          )
        }

        if ("ggplot" %in% class(plot_obj)) {
          # ggplot object - convert to plotly
          p <- ggplotly(plot_obj)

          # Fix legend labels - translate period values
          if (!is.null(p$x$data)) {
            # Period translations
            period_translations <- c(
              "cer" = toupper(i18n_r()$t("cer")),
              "dc" = toupper(i18n_r()$t("dc")),
              "fer" = toupper(i18n_r()$t("fer"))
            )

            for (i in seq_along(p$x$data)) {
              if (!is.null(p$x$data[[i]]$name)) {
                # Get lowercase version of legend name
                original_name <- tolower(p$x$data[[i]]$name)

                # Translate if it's a period value, otherwise keep uppercase
                if (original_name %in% names(period_translations)) {
                  p$x$data[[i]]$name <- period_translations[[original_name]]
                } else {
                  p$x$data[[i]]$name <- toupper(p$x$data[[i]]$name)
                }
              }
            }
          }

          # Translate legend title "period"
          p <- p %>% plotly::layout(
            legend = list(title = list(text = i18n_r()$t("period")))
          )

          p
        } else if ("plotly" %in% class(plot_obj)) {
          # Already a plotly object
          plot_obj
        } else {
          # Try direct conversion as last resort
          p <- ggplotly(plot_obj)

          # Fix legend labels - translate period values
          if (!is.null(p$x$data)) {
            # Period translations
            period_translations <- c(
              "cer" = toupper(i18n_r()$t("cer")),
              "dc" = toupper(i18n_r()$t("dc")),
              "fer" = toupper(i18n_r()$t("fer"))
            )

            for (i in seq_along(p$x$data)) {
              if (!is.null(p$x$data[[i]]$name)) {
                # Get lowercase version of legend name
                original_name <- tolower(p$x$data[[i]]$name)

                # Translate if it's a period value, otherwise keep uppercase
                if (original_name %in% names(period_translations)) {
                  p$x$data[[i]]$name <- period_translations[[original_name]]
                } else {
                  p$x$data[[i]]$name <- toupper(p$x$data[[i]]$name)
                }
              }
            }
          }

          # Translate legend title "period"
          p <- p %>% plotly::layout(
            legend = list(title = list(text = i18n_r()$t("period")))
          )

          p
        }
      },
      error = function(e) {
        print(paste("Error converting plot:", e$message))
        print(paste("Plot object class:", class(plot_obj)))

        plotly_empty() %>%
          add_annotations(
            text = paste("Error displaying plot:", e$message),
            x = 0.5, y = 0.5,
            showarrow = FALSE
          )
      }
    )
  })

  # Model Comparison Table
  output$model_comparison_table <- DT::renderDataTable({
    req(kinetic_results$available_models)
    add_prettynames <- c(
      i18n_r()$t("Model"),
      i18n_r()$t("AARD"),
      i18n_r()$t("RMSE"),
      i18n_r()$t("<em>R</em><sup>2</sup>")
    )

    comp_data <- data.frame()

    if ("sim" %in% kinetic_results$available_models && !is.null(kinetic_results$sim_result$resid)) {
      comp_data <- rbind(comp_data, data.frame(
        Model = i18n$t("Simplified"),
        AARD = sprintf("%.6f", kinetic_results$sim_result$resid["aard"]),
        RMSE = sprintf("%.10f", kinetic_results$sim_result$resid["rmse"]),
        R2 = sprintf("%.6f", kinetic_results$sim_result$resid["r2"])
      ))
    }

    if ("ct" %in% kinetic_results$available_models && !is.null(kinetic_results$ct_result$resid)) {
      comp_data <- rbind(comp_data, data.frame(
        Model = i18n$t("Characteristic Times"),
        AARD = sprintf("%.6f", kinetic_results$ct_result$resid["aard"]),
        RMSE = sprintf("%.10f", kinetic_results$ct_result$resid["rmse"]),
        R2 = sprintf("%.6f", kinetic_results$ct_result$resid["r2"])
      ))
    }

    if ("cmp2" %in% kinetic_results$available_models && !is.null(kinetic_results$cmp_result$resid)) {
      comp_data <- rbind(comp_data, data.frame(
        Model = i18n$t("Complete Model"),
        AARD = sprintf("%.6f", kinetic_results$cmp_result$resid["aard"]),
        RMSE = sprintf("%.10f", kinetic_results$cmp_result$resid["rmse"]),
        R2 = sprintf("%.6f", kinetic_results$cmp_result$resid["r2"])
      ))
    }


    if (nrow(comp_data) == 0) return(NULL)
    
    # Find best values (lowest AARD, lowest RMSE, highest R2)
    aard_vals <- as.numeric(comp_data$AARD)
    rmse_vals <- as.numeric(comp_data$RMSE)
    r2_vals <- as.numeric(comp_data$R2)
    
    best_aard <- which.min(aard_vals)
    best_rmse <- which.min(rmse_vals)
    best_r2 <- which.max(r2_vals)
    
    dt <- DT::datatable(
      comp_data,
      colnames = add_prettynames,
      extensions = "Buttons",
      options = list(pageLength = 25, dom = "Bfrtip", language = tablang(),
        columnDefs = list(list(className = "dt-center", targets = "_all")),
        buttons = create_dt_export_buttons(i18n_r, "supercrit_bic_model_comparison")),
      rownames = FALSE,
      escape = FALSE
    )
    
    # Highlight best values in green
    if (nrow(comp_data) > 1) {
      dt <- dt %>%
        DT::formatStyle("AARD", backgroundColor = DT::styleEqual(comp_data$AARD[best_aard], "#d4edda")) %>%
        DT::formatStyle("RMSE", backgroundColor = DT::styleEqual(comp_data$RMSE[best_rmse], "#d4edda")) %>%
        DT::formatStyle("R2", backgroundColor = DT::styleEqual(comp_data$R2[best_r2], "#d4edda"))
    }
    dt
  })

  # ============================================================
  # Unified Model Detail Selector
  # ============================================================
  output$model_detail_selector_ui <- renderUI({
    req(kinetic_results$available_models)
    avail <- kinetic_results$available_models
    
    choices <- c()
    labels <- c()
    if ("sim" %in% avail) { choices <- c(choices, "sim"); labels <- c(labels, i18n_r()$t("Simplified")) }
    if ("ct" %in% avail) { choices <- c(choices, "ct"); labels <- c(labels, i18n_r()$t("Characteristic Times")) }
    if (any(c("cmp2", "cmp3") %in% avail)) { choices <- c(choices, "cmp"); labels <- c(labels, i18n_r()$t("Complete")) }
    
    if (length(choices) == 0) return(NULL)
    
    radioButtons(ns("selected_model_detail"), i18n_r()$t("Select Model"),
      choiceNames = as.list(labels),
      choiceValues = as.list(choices),
      selected = {
        prev <- isolate(input$selected_model_detail)
        if (!is.null(prev) && prev %in% choices) prev else choices[1]
      },
      inline = TRUE
    )
  })

  # X-axis toggle for plot (time vs S/M ratio)
  output$plot_xaxis_toggle_ui <- renderUI({
    sel <- input$selected_model_detail
    is_ct <- !is.null(sel) && sel == "ct"
    
    selected_val <- if (is_ct) "time" else (isolate(input$plot_xaxis_type) %||% "time")
    
    radio_div <- radioButtons(ns("plot_xaxis_type"), i18n_r()$t("X-Axis"),
      choiceNames = list(i18n_r()$t("Time (min)"), i18n_r()$t("Solvent/Material Ratio (kg/kg)")),
      choiceValues = c("time", "sm"),
      selected = selected_val,
      inline = TRUE
    )
    
    if (is_ct) shinyjs::disabled(radio_div) else radio_div
  })

  # ============================================================
  # Unified Model Parameters table (with Fitted column)
  # ============================================================
  output$unified_mod_pars_table <- DT::renderDataTable({
    sel <- input$selected_model_detail
    req(sel)
    
    result <- switch(sel,
      "sim" = kinetic_results$sim_result,
      "ct" = kinetic_results$ct_result,
      "cmp" = kinetic_results$cmp_result
    )
    req(result)
    
    mod_pars <- result$mod_pars
    fit_pars <- result$fit_pars
    param_names <- names(mod_pars)
    
    param_mapping <- c(
      "beta1" = "%%\\beta_1%%", "beta" = "%%\\beta%%",
      "G0" = "%%G_0%%", "G" = "%%G%%",
      "kf" = "%%k_f%%", "kfa0" = "%%k_fa_0%%",
      "ksas" = "%%k_sa_s%%", "qm" = "%%q_m%%",
      "qn" = "%%q_n%%", "qs" = "%%q_s%%",
      "r" = "%%r%%", "ti" = "%%t_i%%",
      "thetaf" = "%%\\theta_f%%", "thetae" = "%%\\theta_e%%",
      "c1" = "%%c_1%%", "c2" = "%%c_2%%",
      "tprime" = "%%t'%%", "eprime" = "%%e'%%"
    )
    
    desc_map <- c(
      "c1" = i18n_r()$t("Constant"), "c2" = i18n_r()$t("Constant"),
      "qm" = i18n_r()$t("Solvent at end of CER"),
      "r" = i18n_r()$t("Grinding efficiency"),
      "ksas" = i18n_r()$t("Solid-phase MT coefficient \u00D7 area"),
      "G" = i18n_r()$t("Initial fraction of solute in broken cells"),
      "G0" = i18n_r()$t("Initial fraction at CER start"),
      "kf" = i18n_r()$t("Fluid-phase MT coefficient"),
      "kfa0" = i18n_r()$t("Fluid-phase MT coefficient \u00D7 area"),
      "thetaf" = i18n_r()$t("External MT resistance"),
      "thetae" = i18n_r()$t("External MT coefficient"),
      "ti" = i18n_r()$t("FER duration"),
      "qn" = i18n_r()$t("Solvent at end of FER"),
      "qs" = i18n_r()$t("Specific solubility"),
      "beta" = i18n_r()$t("Solvent velocity ratio"),
      "beta1" = i18n_r()$t("Extraction rate parameter"),
      "tprime" = i18n_r()$t("Time at end of CER"),
      "eprime" = i18n_r()$t("Yield at end of CER")
    )
    
    unit_map <- c(
      "c1" = "\u2014", "c2" = "\u2014",
      "qm" = "kg/kg", "r" = "\u2014",
      "ksas" = "m\u207B\u00B9 s\u207B\u00B9",
      "G" = "\u2014", "G0" = "\u2014",
      "kf" = "s\u207B\u00B9", "kfa0" = "s\u207B\u00B9",
      "thetaf" = "\u2014", "thetae" = "\u2014",
      "ti" = i18n$t("min"), "qn" = "kg/kg", "qs" = "kg/kg",
      "beta" = "\u2014", "beta1" = "\u2014",
      "tprime" = i18n$t("min"), "eprime" = "g/g"
    )
    
    display_names <- ifelse(param_names %in% names(param_mapping), param_mapping[param_names], param_names)
    descriptions <- ifelse(param_names %in% names(desc_map), desc_map[param_names], param_names)
    units <- ifelse(param_names %in% names(unit_map), unit_map[param_names], "\u2014")
    
    fitted_col <- vapply(param_names, function(p) {
      if (p %in% fit_pars) i18n_r()$t("Yes") else ""
    }, character(1), USE.NAMES = FALSE)
    
    dt_data <- data.frame(
      Parameter = display_names, Value = sprintf("%.6f", as.numeric(mod_pars)),
      Fitted = fitted_col, Unit = units, Description = descriptions,
      stringsAsFactors = FALSE
    )
    
    DT::datatable(dt_data,
      colnames = c(i18n_r()$t("Parameter"), i18n_r()$t("Value"), i18n_r()$t("Fitted"), i18n_r()$t("Unit"), i18n_r()$t("Description")),
      extensions = "Buttons",
      options = list(pageLength = 25, dom = "Bt", ordering = FALSE, language = tablang(),
        columnDefs = list(list(className = "dt-center", targets = "_all")),
        buttons = create_dt_export_buttons(i18n_r, "supercrit_bic_model_parameters")),
      rownames = FALSE, escape = FALSE
    )
  })

  # ============================================================
  # Unified Observed vs Predicted Data table
  # ============================================================
  output$unified_ordt_table <- DT::renderDataTable({
    sel <- input$selected_model_detail
    req(sel)
    
    result <- switch(sel,
      "sim" = kinetic_results$sim_result,
      "ct" = kinetic_results$ct_result,
      "cmp" = kinetic_results$cmp_result
    )
    req(result, result$ordt)
    
    ordt <- as.data.frame(result$ordt)
    resp_unit <- kinetic_results$resp_units %||% "percent"
    resp_display <- switch(resp_unit, "g" = "g", "percent" = "%", "permille" = "\u2030", "ppm" = "ppm", "ppb" = "ppb", resp_unit)
    
    # Build column names based on actual columns
    # SIM/CT ordt: t(optional), x(q), y(obs), mod_y(pred) 
    # CMP ordt: t(optional), x(q), y(obs), y_cmp3(pred), y_cmp2(pred)
    orig_names <- names(ordt)
    col_map <- c(
      "t" = i18n_r()$t("Time (min)"),
      "x" = i18n_r()$t("Solvent/Material (kg/kg)"),
      "y" = paste0(i18n_r()$t("Observed Yield"), " (", resp_display, ")"),
      "mod_y" = paste0(i18n_r()$t("Predicted Yield"), " (", resp_display, ")"),
      "y_cmp3" = paste0(i18n_r()$t("Predicted"), " CMP3 (", resp_display, ")"),
      "y_cmp2" = paste0(i18n_r()$t("Predicted"), " CMP2 (", resp_display, ")")
    )
    col_names <- ifelse(orig_names %in% names(col_map), col_map[orig_names], orig_names)
    
    num_cols <- which(sapply(ordt, is.numeric)) - 1  # 0-based
    
    DT::datatable(ordt,
      colnames = col_names,
      extensions = "Buttons",
      options = list(pageLength = 25, dom = "Btp", language = tablang(),
        columnDefs = c(
          list(list(className = "dt-center", targets = "_all")),
          trim_zeros_columndefs(num_cols, digits = 4)
        ),
        buttons = create_dt_export_buttons(i18n_r, "supercrit_bic_observed_vs_predicted")),
      rownames = FALSE
    )
  })

  # ============================================================
  # Unified Detailed Predictions table
  # ============================================================
  output$unified_mdt_table <- DT::renderDataTable({
    sel <- input$selected_model_detail
    req(sel)
    
    result <- switch(sel,
      "sim" = kinetic_results$sim_result,
      "ct" = kinetic_results$ct_result,
      "cmp" = kinetic_results$cmp_result
    )
    req(result, result$mdt)
    
    mdt <- as.data.frame(result$mdt)
    resp_unit <- kinetic_results$resp_units %||% "percent"
    resp_display <- switch(resp_unit, "g" = "g", "percent" = "%", "permille" = "\u2030", "ppm" = "ppm", "ppb" = "ppb", resp_unit)
    
    # Expand Period descriptors
    if ("period" %in% names(mdt)) {
      mdt$period <- my_mapvalues(as.character(mdt$period),
        c("cer", "fer", "dc"),
        c(i18n_r()$t("Constant Extraction Rate (CER)"),
          i18n_r()$t("Falling Extraction Rate (FER)"),
          i18n_r()$t("Diffusion Controlled (DC)")),
        warn_missing = FALSE
      )
    }
    
    # Fix model names (cmp2 -> Complete (2 Regions), cmp3 -> Complete (3 Regions))
    if ("model" %in% names(mdt)) {
      mdt$model <- my_mapvalues(as.character(mdt$model),
        c("sim", "ct", "cmp2", "cmp3"),
        c(i18n_r()$t("Simplified"), i18n_r()$t("Characteristic Times"),
          i18n_r()$t("Complete (2 Regions)"), i18n_r()$t("Complete (3 Regions)")),
        warn_missing = FALSE
      )
    }
    
    # Build column name mapping using actual column names
    # mdt columns: model, period, x, y, and optionally t
    orig_names <- names(mdt)
    col_map <- c(
      "model" = i18n_r()$t("Model"),
      "period" = i18n_r()$t("Period"),
      "x" = i18n_r()$t("Solvent/Material (kg/kg)"),
      "y" = paste0(i18n_r()$t("Yield"), " (", resp_display, ")"),
      "t" = i18n_r()$t("Time (min)")
    )
    col_names <- ifelse(orig_names %in% names(col_map), col_map[orig_names], orig_names)
    
    # Remove Model column unless Complete model is selected
    if (sel != "cmp" && "model" %in% orig_names) {
      model_col <- which(orig_names == "model")
      mdt <- mdt[, -model_col, drop = FALSE]
      col_names <- col_names[-model_col]
    }
    
    num_cols_mdt <- which(sapply(mdt, is.numeric)) - 1  # 0-based
    
    DT::datatable(mdt,
      colnames = col_names,
      extensions = "Buttons",
      options = list(pageLength = 50, dom = "Btp", language = tablang(),
        columnDefs = c(
          list(list(className = "dt-center", targets = "_all")),
          trim_zeros_columndefs(num_cols_mdt, digits = 4)
        ),
        buttons = create_dt_export_buttons(i18n_r, "supercrit_bic_detailed_predictions")),
      rownames = FALSE
    )
  })

  # SIM Model Outputs (legacy - kept for compatibility)
  output$sim_mod_pars_table <- DT::renderDataTable({
    req(kinetic_results$sim_result)

    # Column names translation
    add_prettynames <- c(
      i18n_r()$t("Parameter"),
      i18n_r()$t("Value"),
      i18n_r()$t("Description")
    )

    # Parameter isimlerini KaTeX formatında eşleştir (%%...%% formatı)
    param_mapping <- c(
      "beta1" = "%%beta_1%%",
      "beta" = "%%\\beta%%",
      "G0" = "%%G_0%%",
      "G" = "%%G%%",
      "kf" = "%%k_f%%",
      "kfa0" = "%%k_fa_0%%",
      "ksas" = "%%k_sa_s%%",
      "qm" = "%%q_m%%",
      "qn" = "%%q_n%%",
      "qs" = "%%q_s%%",
      "r" = "%%r%%",
      "ti" = "%%t_i%%",
      "thetaf" = "%%\\theta_f%%",
      "thetae" = "%%\\theta_e%%",
      "c1" = "%%c_1%%",
      "c2" = "%%c_1%%"
    )

    param_names <- names(kinetic_results$sim_result$mod_pars)

    # Description translations
    description_translations <- c(
      "c1" = i18n_r()$t("Constant C₁"),
      "c2" = i18n_r()$t("Constant C₂"),
      "qm" = i18n_r()$t("Relative amount of expended solvent at end of CER (kg/kg)"),
      "r" = i18n_r()$t("Grinding efficiency (fraction of broken cells)"),
      "ksas" = i18n_r()$t("Solid phase mass transfer coefficient × area (1/m/s)"),
      "G" = i18n_r()$t("Initial fraction of solute in broken cells")
    )

    # Eşleşen KaTeX formatlarını bul
    display_names <- ifelse(param_names %in% names(param_mapping),
      param_mapping[param_names],
      param_names
    )

    # Create data frame
    dt_data <- data.frame(
      Parameter = display_names,
      Value = sprintf("%.6f", as.numeric(kinetic_results$sim_result$mod_pars)),
      Description = param_names,
      stringsAsFactors = FALSE
    )

    # Apply description translations
    dt_data$Description <- my_mapvalues(dt_data$Description, names(description_translations), description_translations, warn_missing = FALSE)

    DT::datatable(
      dt_data,
      colnames = add_prettynames,
      options = list(
        pageLength = 25,
        dom = "t",
        language = tablang()
      ),
      rownames = FALSE,
      escape = FALSE
    )
  })

  output$sim_fitted_pars <- DT::renderDataTable({
    req(kinetic_results$sim_result)
    req(kinetic_results$sim_result$fit_pars)

    # Column names translation
    add_prettynames <- c(
      i18n_r()$t("Parameter"),
      i18n_r()$t("Value"),
      i18n_r()$t("Description")
    )

    param_names <- kinetic_results$sim_result$fit_pars # Already a character vector of parameter names

    param_names <- as.character(kinetic_results$sim_result$fit_pars)
    if (length(param_names) == 0) {
      return(NULL)
    }

    # Her parametre için 1 değer; yoksa NA
    fit_values <- kinetic_results$sim_result$mod_pars[param_names]

    # Parameter mapping with KaTeX formatting
    param_mapping <- c(
      "r" = "%%r%%",
      "ksas" = "%%k_sa_s%%",
      "qc" = "%%q_c%%",
      "thetaf" = "%%\\theta_f%%",
      "ti" = "%%t_i%%",
      "kf" = "%%k_f%%",
      "c3" = "%%c_3%%"
    )

    # Description translations
    description_translations <- c(
      "r" = i18n_r()$t("Grinding efficiency (fraction of broken cells)"),
      "ksas" = i18n_r()$t("Solid phase mass transfer coefficient × specific area (1/m/s)"),
      "qc" = i18n_r()$t("Relative amount of solvent expended at end of CER"),
      "thetaf" = i18n_r()$t("External mass transfer coefficient"),
      "ti" = i18n_r()$t("Extraction duration of FER (min)"),
      "kf" = i18n_r()$t("Fluid phase mass transfer coefficient"),
      "c3" = i18n_r()$t("Constant related to maximum extractable material fraction"),
      "c1" = i18n_r()$t("Constant C₁"),
      "c2" = i18n_r()$t("Constant C₂"),
      "qm" = i18n_r()$t("Relative amount of expended solvent at end of CER (kg/kg)")
    )

    # Get KaTeX formatted names
    display_names <- vapply(param_names, function(p) {
      if (p %in% names(param_mapping)) {
        param_mapping[[p]]
      } else {
        p
      }
    }, character(1), USE.NAMES = FALSE)

    # Create data frame
    dt_data <- data.frame(
      Parameter = display_names,
      Value = sprintf("%.6f", as.numeric(fit_values)),
      Description = param_names,
      stringsAsFactors = FALSE
    )

    # Apply description translations
    dt_data$Description <- my_mapvalues(dt_data$Description, names(description_translations), description_translations, warn_missing = FALSE)

    DT::datatable(
      dt_data,
      colnames = add_prettynames,
      options = list(
        pageLength = 25,
        dom = "t",
        ordering = FALSE,
        language = tablang()
      ),
      rownames = FALSE,
      escape = FALSE
    )
  })

  output$sim_statistics_table <- DT::renderDataTable({
    req(kinetic_results$sim_result)

    add_prettynames <- c(
      i18n_r()$t("Statistic"),
      i18n_r()$t("Value")
    )

    stat_display_names <- c("aard" = "AARD", "rmse" = "RMSE", "r2" = "R²")
    data.frame(
      Statistic = stat_display_names[names(kinetic_results$sim_result$resid)],
      Value = sprintf("%.6f", as.numeric(kinetic_results$sim_result$resid))
    ) %>%
      DT::datatable(
        colnames = add_prettynames,
        options = list(pageLength = 25, dom = "t", language = tablang()),
        rownames = FALSE
      )
  })

  output$sim_ordt_table <- DT::renderDataTable(
    {
      req(kinetic_results$sim_result)

      # Column names translation
      add_prettynames <- c(
        i18n_r()$t("Time"),
        i18n_r()$t("S/M Ratio"),
        i18n_r()$t("Observed Yield"),
        i18n_r()$t("Predicted Yield")
      )

      dt_output <- DT::datatable(
        kinetic_results$sim_result$ordt,
        extensions = "Buttons",
        colnames = add_prettynames,
        options = list(
          scrollX = TRUE,
          pageLength = 25,
          dom = "Bfrtip",
          language = tablang(),
          buttons = create_dt_export_buttons(i18n_r, "supercrit_bic_sim_observed_prediction_data")
        ),
        rownames = FALSE
      )
      dt_output <- dt_output %>% DT::formatRound(columns = c("x", "t", "y", "mod_y"), digits = 5)
    },
    server = FALSE
  )

  output$sim_mdt_table <- DT::renderDataTable(
    {
      req(kinetic_results$sim_result)

      # Column names translation
      add_prettynames <- c(
        i18n_r()$t("Model"),
        i18n_r()$t("Period"),
        i18n_r()$t("S/M Ratio"),
        i18n_r()$t("Predicted Yield"),
        i18n_r()$t("Time")
      )

      # Model value translations
      model_translations <- c(
        "sim" = i18n_r()$t("sim"),
        "ct" = i18n_r()$t("ct"),
        "cmp" = i18n_r()$t("cmp"),
        "cmp2" = i18n_r()$t("cmp2"),
        "cmp3" = i18n_r()$t("cmp3")
      )

      # Period value translations
      period_translations <- c(
        "cer" = i18n_r()$t("cer"),
        "dc" = i18n_r()$t("dc"),
        "fer" = i18n_r()$t("fer")
      )

      dt_data <- kinetic_results$sim_result$mdt
      dt_data$model <- my_mapvalues(dt_data$model, names(model_translations), model_translations, warn_missing = FALSE)
      dt_data$period <- my_mapvalues(dt_data$period, names(period_translations), period_translations, warn_missing = FALSE)

      dt_output <- DT::datatable(
        dt_data,
        extensions = "Buttons",
        colnames = add_prettynames,
        options = list(
          scrollX = TRUE,
          pageLength = 25,
          dom = "Bfrtip",
          language = tablang(),
          buttons = create_dt_export_buttons(i18n_r, "supercrit_bic_sim_detailed_prediction_data")
        ),
        rownames = FALSE
      )
      dt_output <- dt_output %>% DT::formatRound(columns = c("x", "y", "t"), digits = 5)
    },
    server = FALSE
  )

  # CT Model Outputs
  output$ct_mod_pars_table <- DT::renderDataTable({
    req(kinetic_results$ct_result)

    # Column names translation
    add_prettynames <- c(
      i18n_r()$t("Parameter"),
      i18n_r()$t("Value"),
      i18n_r()$t("Description")
    )

    # Parameter isimlerini KaTeX formatında eşleştir (%%...%% formatı)
    param_mapping <- c(
      "beta1" = "%%beta_1%%",
      "beta" = "%%\\beta%%",
      "G0" = "%%G_0%%",
      "G" = "%%G%%",
      "kf" = "%%k_f%%",
      "kfa0" = "%%k_fa_0%%",
      "ksas" = "%%k_sa_s%%",
      "qm" = "%%q_m%%",
      "qn" = "%%q_n%%",
      "qs" = "%%q_s%%",
      "r" = "%%r%%",
      "ti" = "%%t_i%%",
      "tc" = "%%t_c%%",
      "tf" = "%%t_f%%",
      "thetaf" = "%%\\theta_f%%",
      "thetae" = "%%\\theta_e%%",
      "tprime" = "%%t'%%",
      "eprime" = "%%e'%%"
    )

    param_names <- names(kinetic_results$ct_result$mod_pars)

    # Description translations
    description_translations <- c(
      "thetaf" = i18n_r()$t("External mass transfer resistance"),
      "ti" = i18n_r()$t("Extraction time/duration of FER"),
      "tprime" = i18n_r()$t("Extraction time at end of CER"),
      "eprime" = i18n_r()$t("Yield at end of CER (g/g total dry solid)"),
      "G" = i18n_r()$t("Initial fraction of solute in broken cells")
    )

    # Eşleşen KaTeX formatlarını bul
    display_names <- ifelse(param_names %in% names(param_mapping),
      param_mapping[param_names],
      param_names
    )

    # Create data frame
    dt_data <- data.frame(
      Parameter = display_names,
      Value = sprintf("%.6f", as.numeric(kinetic_results$ct_result$mod_pars)),
      Description = param_names,
      stringsAsFactors = FALSE
    )

    # Apply description translations
    dt_data$Description <- my_mapvalues(dt_data$Description, names(description_translations), description_translations, warn_missing = FALSE)

    DT::datatable(
      dt_data,
      colnames = add_prettynames,
      options = list(
        pageLength = 25,
        dom = "t",
        language = tablang()
      ),
      rownames = FALSE,
      escape = FALSE
    )
  })

  output$ct_fitted_pars <- DT::renderDataTable({
    req(kinetic_results$ct_result)
    req(kinetic_results$ct_result$fit_pars)

    # Column names translation
    add_prettynames <- c(
      i18n_r()$t("Parameter"),
      i18n_r()$t("Value"),
      i18n_r()$t("Description")
    )

    param_names <- kinetic_results$ct_result$fit_pars # Already a character vector of parameter names

    if (length(param_names) == 0) {
      return(NULL)
    }

    fit_values <- kinetic_results$ct_result$mod_pars[param_names] # Get actual fitted values

    # Parameter mapping with KaTeX formatting
    param_mapping <- c(
      "thetaf" = "%%\\theta_f%%",
      "ti" = "%%t_i%%",
      "tc" = "%%t_c%%",
      "tf" = "%%t_f%%",
      "thetae" = "%%\\theta_e%%",
      "G" = "%%G%%"
    )

    # Description translations
    description_translations <- c(
      "thetaf" = i18n_r()$t("External mass transfer resistance"),
      "ti" = i18n_r()$t("Extraction time/duration of FER (min)"),
      "tc" = i18n_r()$t("Characteristic time for CER (min)"),
      "tf" = i18n_r()$t("Characteristic time for DC region (min)"),
      "thetae" = i18n_r()$t("External mass transfer coefficient"),
      "G" = i18n_r()$t("Initial fraction of solute in broken cells")
    )

    # Get KaTeX formatted names
    display_names <- vapply(param_names, function(p) {
      if (p %in% names(param_mapping)) {
        param_mapping[[p]]
      } else {
        p
      }
    }, character(1), USE.NAMES = FALSE)

    # Create data frame
    dt_data <- data.frame(
      Parameter = display_names,
      Value = sprintf("%.6f", as.numeric(fit_values)),
      Description = param_names,
      stringsAsFactors = FALSE
    )

    # Apply description translations
    dt_data$Description <- my_mapvalues(dt_data$Description, names(description_translations), description_translations, warn_missing = FALSE)

    DT::datatable(
      dt_data,
      colnames = add_prettynames,
      options = list(
        pageLength = 25,
        dom = "t",
        ordering = FALSE,
        language = tablang()
      ),
      rownames = FALSE,
      escape = FALSE
    )
  })

  output$ct_statistics_table <- DT::renderDataTable({
    req(kinetic_results$ct_result)

    add_prettynames <- c(
      i18n_r()$t("Statistic"),
      i18n_r()$t("Value")
    )

    stat_display_names <- c("aard" = "AARD", "rmse" = "RMSE", "r2" = "R²")
    data.frame(
      Statistic = stat_display_names[names(kinetic_results$ct_result$resid)],
      Value = sprintf("%.6f", as.numeric(kinetic_results$ct_result$resid))
    ) %>%
      DT::datatable(
        colnames = add_prettynames,
        options = list(pageLength = 25, dom = "t", language = tablang()),
        rownames = FALSE
      )
  })

  output$ct_ordt_table <- DT::renderDataTable(
    {
      req(kinetic_results$ct_result)

      # Column names translation
      add_prettynames <- c(
        i18n_r()$t("Time"),
        i18n_r()$t("Observed Yield"),
        i18n_r()$t("Predicted Yield")
      )

      dt_output <- DT::datatable(
        kinetic_results$ct_result$ordt,
        extensions = "Buttons",
        colnames = add_prettynames,
        options = list(
          scrollX = TRUE,
          pageLength = 25,
          dom = "Bfrtip",
          language = tablang(),
          buttons = create_dt_export_buttons(i18n_r, "supercrit_bic_ct_observed_prediction_data")
        ),
        rownames = FALSE
      )
      dt_output <- dt_output %>% DT::formatRound(columns = c("x", "y", "mod_y"), digits = 5)
    },
    server = FALSE
  )

  output$ct_mdt_table <- DT::renderDataTable(
    {
      req(kinetic_results$ct_result)

      # Column names translation - CT model uses different columns
      add_prettynames <- c(
        i18n_r()$t("Model"),
        i18n_r()$t("Period"),
        i18n_r()$t("Time"),
        i18n_r()$t("Predicted Yield")
      )

      # Model value translations
      model_translations <- c(
        "sim" = i18n_r()$t("sim"),
        "ct" = i18n_r()$t("ct"),
        "cmp" = i18n_r()$t("cmp"),
        "cmp2" = i18n_r()$t("cmp2"),
        "cmp3" = i18n_r()$t("cmp3")
      )

      # Period value translations
      period_translations <- c(
        "cer" = i18n_r()$t("cer"),
        "dc" = i18n_r()$t("dc"),
        "fer" = i18n_r()$t("fer")
      )

      dt_data <- kinetic_results$ct_result$mdt
      dt_data$model <- my_mapvalues(dt_data$model, names(model_translations), model_translations, warn_missing = FALSE)
      dt_data$period <- my_mapvalues(dt_data$period, names(period_translations), period_translations, warn_missing = FALSE)

      dt_output <- DT::datatable(
        dt_data,
        extensions = "Buttons",
        colnames = add_prettynames,
        options = list(
          scrollX = TRUE,
          pageLength = 25,
          dom = "Bfrtip",
          language = tablang(),
          buttons = create_dt_export_buttons(i18n_r, "supercrit_bic_ct_detailed_prediction_data")
        ),
        rownames = FALSE
      )
      dt_output <- dt_output %>% DT::formatRound(columns = c("x", "y"), digits = 5)
    },
    server = FALSE
  )



  output$cmp_mod_pars_table <- DT::renderDataTable({
    req(kinetic_results$cmp_result)

    # Column names translation
    add_prettynames <- c(
      i18n_r()$t("Parameter"),
      i18n_r()$t("Value"),
      i18n_r()$t("Description")
    )

    # Parameter isimlerini KaTeX formatında eşleştir (%%...%% formatı)
    param_mapping <- c(
      "beta1" = "%%beta_1%%",
      "beta" = "$$\\beta$$",
      "G0" = "%%G_0%%",
      "G" = "%%G%%",
      "kf" = "%%k_f%%",
      "kfa0" = "%%k_fa_0%%",
      "ksas" = "%%k_sa_s%%",
      "qm" = "%%q_m%%",
      "qn" = "%%q_n%%",
      "qs" = "%%q_s%%",
      "r" = "%%r%%",
      "thetae" = "%%\\theta_e%%"
    )

    param_names <- names(kinetic_results$cmp_result$mod_pars)

    # Description translations
    description_translations <- c(
      "thetae" = i18n_r()$t("External mass transfer resistance"),
      "kfa0" = i18n_r()$t("Fluid phase mass transfer coefficient × area (1/m/s)"),
      "qm" = i18n_r()$t("Relative amount of expended solvent at end of CER (kg/kg)"),
      "qn" = i18n_r()$t("FER for 3-period model"),
      "qs" = i18n_r()$t("DC for 2-period model"),
      "beta" = i18n_r()$t("Coefficient β"),
      "G" = i18n_r()$t("Initial fraction of solute in broken cells"),
      "kf" = i18n_r()$t("Fluid mass transfer coefficient (1/s)"),
      "r" = i18n_r()$t("Grinding efficiency (fraction of broken cells)"),
      "ksas" = i18n_r()$t("Solid phase mass transfer coefficient × area (1/m/s)")
    )

    # Eşleşen KaTeX formatlarını bul
    display_names <- ifelse(param_names %in% names(param_mapping),
      param_mapping[param_names],
      param_names
    )

    # Create data frame
    dt_data <- data.frame(
      Parameter = display_names,
      Value = sprintf("%.6f", as.numeric(kinetic_results$cmp_result$mod_pars)),
      Description = param_names,
      stringsAsFactors = FALSE
    )

    # Apply description translations
    dt_data$Description <- my_mapvalues(dt_data$Description, names(description_translations), description_translations, warn_missing = FALSE)

    DT::datatable(
      dt_data,
      colnames = add_prettynames,
      options = list(
        pageLength = 25,
        dom = "t",
        language = tablang()
      ),
      rownames = FALSE,
      escape = FALSE
    )
  })



  output$cmp_fitted_pars <- DT::renderDataTable({
    req(kinetic_results$cmp_result)
    req(kinetic_results$cmp_result$fit_pars)

    # Column names translation
    add_prettynames <- c(
      i18n_r()$t("Parameter"),
      i18n_r()$t("Value"),
      i18n_r()$t("Description")
    )

    fit_pars <- kinetic_results$cmp_result$fit_pars

    if (length(fit_pars) == 0) {
      return(NULL)
    }

    param_names <- fit_pars # Use fit_pars directly as it contains the parameter names

    if (length(param_names) == 0) {
      return(NULL)
    }

    fit_values <- kinetic_results$cmp_result$mod_pars[param_names] # Get actual fitted values

    # Parameter mapping with KaTeX formatting
    param_mapping <- c(
      "r" = "%%r%%",
      "ksas" = "%%k_sa_s%%",
      "kf" = "%%k_f%%",
      "thetae" = "%%\\theta_e%%",
      "beta" = "%%\\beta%%",
      "G" = "%%G%%",
      "qm" = "%%q_m%%",
      "qn" = "%%q_n%%",
      "qs" = "%%q_s%%",
      "kfa0" = "%%k_fa_0%%"
    )

    # Description translations
    description_translations <- c(
      "r" = i18n_r()$t("Grinding efficiency (fraction of broken cells)"),
      "ksas" = i18n_r()$t("Solid phase mass transfer coefficient × specific area (1/m/s)"),
      "kf" = i18n_r()$t("Fluid phase mass transfer coefficient (1/s)"),
      "thetae" = i18n_r()$t("External mass transfer resistance"),
      "beta" = i18n_r()$t("Coefficient β"),
      "G" = i18n_r()$t("Initial fraction of solute in broken cells"),
      "qm" = i18n_r()$t("Relative amount of expended solvent at end of CER (kg/kg)"),
      "qn" = i18n_r()$t("Solvent consumption at end of FER for 3-period model"),
      "qs" = i18n_r()$t("Solvent consumption for 2-period DC model"),
      "kfa0" = i18n_r()$t("Fluid phase mass transfer coefficient × area (1/m/s)")
    )

    # Get KaTeX formatted names
    display_names <- vapply(param_names, function(p) {
      if (p %in% names(param_mapping)) {
        param_mapping[[p]]
      } else {
        p
      }
    }, character(1), USE.NAMES = FALSE)

    # Create data frame
    dt_data <- data.frame(
      Parameter = display_names,
      Value = sprintf("%.6f", as.numeric(fit_values)),
      Description = param_names,
      stringsAsFactors = FALSE
    )

    # Apply description translations
    dt_data$Description <- my_mapvalues(dt_data$Description, names(description_translations), description_translations, warn_missing = FALSE)

    DT::datatable(
      dt_data,
      colnames = add_prettynames,
      options = list(
        pageLength = 25,
        dom = "t",
        ordering = FALSE,
        language = tablang()
      ),
      rownames = FALSE,
      escape = FALSE
    )
  })

  output$cmp_statistics_table <- DT::renderDataTable({
    req(kinetic_results$cmp_result)

    add_prettynames <- c(
      i18n_r()$t("Statistic"),
      i18n_r()$t("Value")
    )

    stat_display_names <- c("aard" = "AARD", "rmse" = "RMSE", "r2" = "R²")
    data.frame(
      Statistic = stat_display_names[names(kinetic_results$cmp_result$resid)],
      Value = sprintf("%.6f", as.numeric(kinetic_results$cmp_result$resid))
    ) %>%
      DT::datatable(
        colnames = add_prettynames,
        options = list(pageLength = 25, dom = "t", language = tablang()),
        rownames = FALSE
      )
  })

  output$cmp_ordt_table <- DT::renderDataTable(
    {
      req(kinetic_results$cmp_result)

      # Column names translation
      add_prettynames <- c(
        i18n_r()$t("Time"),
        i18n_r()$t("S/M Ratio"),
        i18n_r()$t("Observed Yield"),
        i18n_r()$t("Predicted Yield (3-Period)"),
        i18n_r()$t("Predicted Yield (2-Period)")
      )

      dt_output <- DT::datatable(
        kinetic_results$cmp_result$ordt,
        extensions = "Buttons",
        colnames = add_prettynames,
        options = list(
          scrollX = TRUE,
          pageLength = 25,
          dom = "Bfrtip",
          language = tablang(),
          buttons = create_dt_export_buttons(i18n_r, "supercrit_bic_cmp_observed_prediction_data")
        ),
        rownames = FALSE
      )
      dt_output <- dt_output %>% DT::formatRound(columns = c("t", "x", "y", "y_cmp3", "y_cmp2"), digits = 5)
    },
    server = FALSE
  )

  output$cmp_mdt_table <- DT::renderDataTable(
    {
      req(kinetic_results$cmp_result)

      # Column names translation
      add_prettynames <- c(
        i18n_r()$t("Model"),
        i18n_r()$t("Period"),
        i18n_r()$t("S/M Ratio"),
        i18n_r()$t("Predicted Yield"),
        i18n_r()$t("Time")
      )

      # Model value translations
      model_translations <- c(
        "sim" = i18n_r()$t("sim"),
        "ct" = i18n_r()$t("ct"),
        "cmp" = i18n_r()$t("cmp"),
        "cmp2" = i18n_r()$t("cmp2"),
        "cmp3" = i18n_r()$t("cmp3")
      )

      # Period value translations
      period_translations <- c(
        "cer" = i18n_r()$t("cer"),
        "dc" = i18n_r()$t("dc"),
        "fer" = i18n_r()$t("fer")
      )

      dt_data <- kinetic_results$cmp_result$mdt
      dt_data$model <- my_mapvalues(dt_data$model, names(model_translations), model_translations, warn_missing = FALSE)
      dt_data$period <- my_mapvalues(dt_data$period, names(period_translations), period_translations, warn_missing = FALSE)

      dt_output <- DT::datatable(
        dt_data,
        extensions = "Buttons",
        colnames = add_prettynames,
        options = list(
          scrollX = TRUE,
          pageLength = 25,
          dom = "Bfrtip",
          language = tablang(),
          buttons = create_dt_export_buttons(i18n_r, "supercrit_bic_cmp_detailed_prediction_data")
        ),
        rownames = FALSE
      )
      dt_output <- dt_output %>% DT::formatRound(columns = c("x", "y", "t"), digits = 5)
    },
    server = FALSE
  )





  # Reset function
  observeEvent(input$reset, {
    # Reset all inputs to default values
    updateRadioButtons(session, "input_type", selected = "csv")
    # The dynamic selectInputs will reset automatically when oec_data() becomes NULL
    # or when new data is loaded.
    updateNumericInput(session, "pres", value = defaults$pres)
    updateNumericInput(session, "temp", value = defaults$temp)
    updateNumericInput(session, "mass_in", value = defaults$mass_in)
    updateNumericInput(session, "moisture", value = defaults$moisture)
    updateNumericInput(session, "D", value = defaults$D)
    updateNumericInput(session, "L", value = defaults$L)
    updateNumericInput(session, "etoh", value = defaults$etoh)
    updateNumericInput(session, "dr", value = defaults$dr)
    updateNumericInput(session, "dp", value = defaults$dp)
    updateNumericInput(session, "n", value = defaults$n)
    updateNumericInput(session, "flow", value = defaults$flow)
    updateNumericInput(session, "cu", value = defaults$cu)
    updateCheckboxInput(session, "cumulative", value = defaults$cumulative)
    updateCheckboxInput(session, "mass_flow", value = defaults$mass_flow)
    updateCheckboxInput(session, "use_solvent", value = FALSE)

    # Advanced parameters
    updateNumericInput(session, "etoh_frac", value = defaults$etoh_frac)
    updateNumericInput(session, "flowpar_temp", value = defaults$flowpar_temp)
    updateNumericInput(session, "flowpar_pres", value = defaults$flowpar_pres)
    updateNumericInput(session, "ro_co2", value = defaults$ro_co2)
    updateNumericInput(session, "tmax", value = defaults$tmax)
    updateNumericInput(session, "qmax", value = defaults$qmax)

    # Model settings
    updateSelectizeInput(session, "modtype", selected = defaults$modtype)
    updateSelectInput(session, "aggreg", selected = defaults$aggreg)
    updateSelectInput(session, "flow_units", selected = defaults$flow_units)
    updateSelectInput(session, "resp_units", selected = defaults$resp_units)

    # Optimization parameters
    updateTextInput(session, "r_est", value = defaults$r_est)
    updateSelectizeInput(session, "ksas_est", choices = defaults$ksas_est, selected = defaults$ksas_est)
    updateTextInput(session, "qc_est", value = defaults$qc_est)
    updateTextInput(session, "thetaf_est", value = defaults$thetaf_est)
    updateTextInput(session, "ti_est", value = defaults$ti_est)
    updateTextInput(session, "kf_est", value = defaults$kf_est)
    updateTextInput(session, "c3_est", value = defaults$c3_est)

    # Reset data inputs
    example_data$data <- NULL
    edited_preview_data$data <- NULL
    # Reset file input by using shinyjs
    shinyjs::reset("file_upload")
    
    # Clear variable selection wrapper borders
    wrapper_ids <- c(ns("oec_x_var_ui_wrapper"), ns("oec_y_var_ui_wrapper"), ns("oec_slv_var_ui_wrapper"))
    for (wrapper_id in wrapper_ids) {
      shinyjs::runjs(sprintf(
        "$('#%s').css({'border': '', 'border-radius': '', 'padding': ''});",
        wrapper_id
      ))
    }

    # Clear results
    kinetic_results$full_result <- NULL
    kinetic_results$sim_result <- NULL
    kinetic_results$ct_result <- NULL
    kinetic_results$cmp_result <- NULL
    kinetic_results$plots_list <- NULL
    kinetic_results$data <- NULL
    kinetic_results$input_params <- NULL
    kinetic_results$available_models <- c()
    kinetic_results$previous_modtype <- NULL

    showNotification(i18n$t("Parameters reset"), type = "message")
  })
  observeEvent(input$show_units, {
  showModal(
    modalDialog(
      title = i18n$t("BIC Model Parameter Units"),
      DT::dataTableOutput(ns("units_table")),
      easyClose = TRUE,
      size = "l",
      footer = tagList(
        modalButton(i18n$t("Close"))  # veya "Kapat", "Dismiss" vb.
      )
    )
  )
})


  output$units_table <- DT::renderDataTable(
    {
      # Get the data
      dt_data <- show_pars("bic")

      # Column names translation
      add_prettynames <- c(
        i18n_r()$t("Type"),
        i18n_r()$t("Parameter"),
        i18n_r()$t("Units"),
        i18n_r()$t("Description")
      )

      # Type values translation
      type_translations <- c(
        "input" = i18n_r()$t("input"),
        "adjustable" = i18n_r()$t("adjustable"),
        "output" = i18n_r()$t("output")
      )

      # Description translations
      description_translations <- c(
        "Extraction pressure (bar)" = i18n_r()$t("Extraction pressure (bar)"),
        "Extraction temperature (degC)" = i18n_r()$t("Extraction temperature (degC)"),
        "Solvent flow rate (mass or volumetric; kg/s in 'bicmod' output)" = i18n_r()$t("Solvent flow rate (mass or volumetric; kg/s in 'bicmod' output)"),
        "Bed porosity (dimensionless)" = i18n_r()$t("Bed porosity (dimensionless)"),
        "Number of experimental data points" = i18n_r()$t("Number of experimental data points"),
        "Mass of wet sample (g)" = i18n_r()$t("Mass of wet sample (g)"),
        "Moisture content (%)" = i18n_r()$t("Moisture content (%)"),
        "Total dry mass (solute + insoluble material)" = i18n_r()$t("Total dry mass (solute + insoluble material)"),
        "Extraction vessel diameter (m)" = i18n_r()$t("Extraction vessel diameter (m)"),
        "Extraction vessel height (m)" = i18n_r()$t("Extraction vessel height (m)"),
        "Apparent density of raw material (g/L)" = i18n_r()$t("Apparent density of raw material (g/L)"),
        "Real density of raw material (g/L)" = i18n_r()$t("Real density of raw material (g/L)"),
        "Average particle diameter (m)" = i18n_r()$t("Average particle diameter (m)"),
        "Specific area per unit volume of extraction bed (1/m)" = i18n_r()$t("Specific area per unit volume of extraction bed (1/m)"),
        "Solvent density under standard conditions (g/L)" = i18n_r()$t("Solvent density under standard conditions (g/L)"),
        "EtOH co-solvent flow rate (mL/min, if any)" = i18n_r()$t("EtOH co-solvent flow rate (mL/min, if any)"),
        "Asymptotic extraction yield at infinite time" = i18n_r()$t("Asymptotic extraction yield at infinite time"),
        "Period corresponding to the end of the CER" = i18n_r()$t("Period corresponding to the end of the CER"),
        "Mass of insoluble material (g)" = i18n_r()$t("Mass of insoluble material (g)"),
        "Ratio (concentration) of solute to insoluble material (kg/kg)" = i18n_r()$t("Ratio (concentration) of solute to insoluble material (kg/kg)"),
        "CO₂/insoluble solid ratio in the extraction bed" = i18n_r()$t("CO₂/insoluble solid ratio in the extraction bed"),
        "Apparent solubility in the CER (fraction)" = i18n_r()$t("Apparent solubility in the CER (fraction)"),
        "Extraction time of the FER (i.e. solid mass transfer, min)" = i18n_r()$t("Extraction time of the FER (i.e. solid mass transfer, min)"),
        "Initial fraction of solute in open (broken) cells" = i18n_r()$t("Initial fraction of solute in open (broken) cells"),
        "Extraction time at the end of the CER" = i18n_r()$t("Extraction time at the end of the CER"),
        "External material (mass) transport resistance" = i18n_r()$t("External material (mass) transport resistance"),
        "Fluid phase mass transfer coefficient (1/s)" = i18n_r()$t("Fluid phase mass transfer coefficient (1/s)"),
        "Product of kf and a0 (1/m/s)" = i18n_r()$t("Product of kf and a0 (1/m/s)"),
        "Grinding efficiency (fraction of broken cells)" = i18n_r()$t("Grinding efficiency (fraction of broken cells)"),
        "Solid phase mass transfer coefficient (1/s)" = i18n_r()$t("Solid phase mass transfer coefficient (1/s)"),
        "Specific area between intact and broken cells (1/m)" = i18n_r()$t("Specific area between intact and broken cells (1/m)"),
        "Product of ks and as (1/m/s)" = i18n_r()$t("Product of ks and as (1/m/s)"),
        "Solvent passed at the end of the CER (kg/kg of insolubles)" = i18n_r()$t("Solvent passed at the end of the CER (kg/kg of insolubles)"),
        "Internal material (mass) transport resistance" = i18n_r()$t("Internal material (mass) transport resistance"),
        "Coefficient" = i18n_r()$t("Coefficient"),
        "Solvent passed at the end of the FER (kg/kg of insolubles)" = i18n_r()$t("Solvent passed at the end of the FER (kg/kg of insolubles)"),
        "Relative amount of passed solvent (kg/kg insoluble solid)" = i18n_r()$t("Relative amount of passed solvent (kg/kg insoluble solid)"),
        "Fractional yield (g/g insoluble solid)" = i18n_r()$t("Fractional yield (g/g insoluble solid)"),
        "Fractional yield (g/g total dry solid)" = i18n_r()$t("Fractional yield (g/g total dry solid)")
      )

      # Apply translations
      dt_data$type <- my_mapvalues(dt_data$type, names(type_translations), type_translations, warn_missing = FALSE)
      dt_data$description <- my_mapvalues(dt_data$description, names(description_translations), description_translations, warn_missing = FALSE)

      DT::datatable(
        dt_data,
        extensions = "Buttons",
        colnames = add_prettynames,
        options = list(
          language = tablang(),
          dom = "Bfrtip",
          buttons = list(
            list(
              extend = "copy",
              text = i18n_r()$t("Copy"),
              titleAttr = i18n_r()$t("Copy"),
              action = copy_button_no_popup(
                copy_label = i18n_r()$t("Copy"),
                copied_label = i18n_r()$t("Copied!")
              )
            )
          )
        ),
        rownames = FALSE
      )
    },
    server = FALSE
  )

  # ============================================================================
  # PREDICTION FUNCTIONALITY
  # ============================================================================

  # Conditional panel for predictions tab
  output$has_full_result <- reactive({
    !is.null(kinetic_results$full_result)
  })
  outputOptions(output, "has_full_result", suspendWhenHidden = FALSE)

  output$has_bic_predict_results <- reactive({
    !is.null(kinetic_results$predict_result)
  })
  outputOptions(output, "has_bic_predict_results", suspendWhenHidden = FALSE)

  # Note: trim_zeros_columndefs is defined in utils/general_helpers.R
    # Calculate Yields checkbox with tooltip (aligned with selectInputs)
  output$predict_get_yields_ui <- renderUI({
    cur <- isolate(input$predict_get_yields)
    if (is.null(cur)) cur <- TRUE
    tags$div(
      title = i18n_r()$t("Convert the predicted fractional yield into mass and percentage yield using the raw material mass and response units from the model."),
      style = "display: inline-block;",
      checkboxInput(ns("predict_get_yields"), i18n_r()$t("Calculate mass and percentage yields"), value = cur)
    )
  })

  # Dynamic model selector - only show available models
  output$predict_model_selector_ui <- renderUI({
    req(kinetic_results$available_models)
    available <- kinetic_results$available_models

    choice_values <- c()
    choice_names <- c()

    if ("sim" %in% available) {
      choice_values <- c(choice_values, "sim")
      choice_names <- c(choice_names, i18n_r()$t("Simplified"))
    }
    if ("ct" %in% available) {
      choice_values <- c(choice_values, "ct")
      choice_names <- c(choice_names, i18n_r()$t("Characteristic Times"))
    }
    if ("cmp2" %in% available) {
      choice_values <- c(choice_values, "cmp2")
      choice_names <- c(choice_names, i18n_r()$t("Complete (2 Regions)"))
    }
    if ("cmp3" %in% available) {
      choice_values <- c(choice_values, "cmp3")
      choice_names <- c(choice_names, i18n_r()$t("Complete (3 Regions)"))
    }

    selectInput(ns("predict_model"),
      i18n_r()$t("Model"),
      choices = setNames(choice_values, choice_names),
      selected = isolate(input$predict_model) %||% choice_values[1]
    )
  })

  # Dynamic UI for predict_units based on model selection
  output$predict_units_ui <- renderUI({
    req(input$predict_model)
    is_ct <- input$predict_model == "ct"
    
    prev <- isolate(input$predict_units) %||% "sm"
    selected_val <- if (is_ct) "time" else prev
    
    choices_vals <- c("sm", "time", "pct_yield")
    choices_names <- c(
      i18n_r()$t("Solvent/Material Ratio (kg/kg)"),
      i18n_r()$t("Time (minutes)"),
      i18n_r()$t("Percentage of Maximum Yield")
    )
    
    units_div <- selectInput(ns("predict_units"),
      i18n_r()$t("Input Data Type"),
      choices = setNames(choices_vals, choices_names),
      selected = selected_val
    )
    
    if (is_ct && selected_val != "pct_yield") shinyjs::disabled(units_div) else units_div
  })

  # Dynamic UI for prediction data input
  # Helper to get experimental range for predictions
  predict_exp_range <- reactive({
    exp_data <- kinetic_results$data
    units <- input$predict_units %||% "sm"
    
    if (units == "pct_yield") {
      return(c(1, 100))
    } else if (units == "time") {
      if (!is.null(exp_data) && !is.null(input$oec_x_var) && input$oec_x_var %in% names(exp_data)) {
        range(exp_data[[input$oec_x_var]], na.rm = TRUE)
      } else NULL
    } else {
      if (!is.null(exp_data) && "q" %in% names(exp_data)) {
        range(exp_data$q, na.rm = TRUE)
      } else NULL
    }
  })

  output$predict_data_input_ui <- renderUI({
    input_type <- input$predict_input_type
    units <- input$predict_units %||% "sm"
    
    if (units == "time") {
      label_text <- i18n_r()$t("Time (min)")
      placeholder <- i18n_r()$t("Type time values and press Enter...")
      unit_suffix <- i18n_r()$t("min")
    } else if (units == "pct_yield") {
      label_text <- i18n_r()$t("Percentage of Maximum Yield (%)")
      placeholder <- i18n_r()$t("Type percentage values and press Enter...")
      unit_suffix <- "%"
    } else {
      label_text <- i18n_r()$t("Solvent/Material Ratio (kg/kg)")
      placeholder <- i18n_r()$t("Type S/M ratio values and press Enter...")
      unit_suffix <- i18n_r()$t("kg/kg")
    }
    
    exp_range <- predict_exp_range()
    badge_text <- if (!is.null(exp_range)) {
      paste0(round(exp_range[1], 1), "\u2013", round(exp_range[2], 1), " ", unit_suffix)
    } else paste0("> 0 ", unit_suffix)
    
    if (input_type == "csv") {
      # Show file input + preview of uploaded values
      csv_data <- predict_input_data()
      tagList(
        fileInput(ns("predict_file_upload"),
          i18n_r()$t("Upload CSV (single column)"),
          accept = c("text/csv", ".csv")
        ),
        if (!is.null(csv_data) && length(csv_data) > 0) {
          # Check for out-of-range values
          csv_badge_color <- "#6c757d"
          if (!is.null(exp_range) && any(csv_data < exp_range[1] | csv_data > exp_range[2])) {
            csv_badge_color <- "#dc3545"
          }
          div(
            style = "margin-top: -10px; margin-bottom: 10px;",
            tags$label(class = "control-label",
              style = "display: flex; align-items: center; gap: 8px;",
              tags$span(paste0(i18n_r()$t("Uploaded values"), " (", length(csv_data), ")")),
              tags$span(badge_text,
                style = paste0("font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: ", csv_badge_color, "; color: white; font-weight: normal;")
              )
            ),
            div(
              style = "background-color: #f5f5f5; border: 1px solid #ddd; border-radius: 4px; padding: 8px; max-height: 100px; overflow-y: auto; font-size: 12px; color: #555;",
              paste(round(csv_data, 4), collapse = ", ")
            )
          )
        }
      )
    } else if (input_type == "sequence") {
      # Check if sequence endpoints are out of range
      seq_badge_color <- "#6c757d"
      from_val <- input$predict_seq_from
      to_val <- input$predict_seq_to
      if (!is.null(exp_range) && !is.null(from_val) && !is.null(to_val) && !is.na(from_val) && !is.na(to_val)) {
        if (from_val < exp_range[1] || to_val > exp_range[2]) seq_badge_color <- "#dc3545"
      }
      
      tagList(
        tags$label(
          class = "control-label",
          style = "display: flex; align-items: center; gap: 8px; width: 100%;",
          tags$span(label_text),
          tags$span(badge_text,
            style = paste0("font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: ", seq_badge_color, "; color: white; font-weight: normal;"),
            title = paste0(i18n_r()$t("Experimental range:"), " ", badge_text)
          )
        ),
        div(
          style = "display: flex; gap: 8px; align-items: flex-end;",
          div(style = "flex: 1;", numericInput(ns("predict_seq_from"), i18n_r()$t("From"),
            value = isolate(input$predict_seq_from) %||% if (!is.null(exp_range)) round(exp_range[1], 1) else 0, min = 0, step = 1)),
          div(style = "flex: 1;", numericInput(ns("predict_seq_to"), i18n_r()$t("To"),
            value = isolate(input$predict_seq_to) %||% if (!is.null(exp_range)) round(exp_range[2], 1) else 100, min = 0, step = 1)),
          div(style = "flex: 1;", numericInput(ns("predict_seq_step"), i18n_r()$t("Step"),
            value = isolate(input$predict_seq_step) %||% if (!is.null(exp_range)) round((exp_range[2] - exp_range[1]) / 10, 1) else 10, min = 0.1, step = 1))
        )
      )
    } else {
      # Tag-style input with out-of-range coloring
      current_vals <- input$predict_manual_tags  # reactive (not isolate) so badge updates
      
      range_js <- if (!is.null(exp_range)) {
        sprintf("var rMin = %s; var rMax = %s; if (!isNaN(val) && (val < rMin || val > rMax)) isOutOfRange = true;", exp_range[1], exp_range[2])
      } else ""
      
      # Check if any current values are out of range for badge color
      badge_color <- "#6c757d"
      if (!is.null(exp_range) && !is.null(current_vals) && length(current_vals) > 0) {
        num_vals <- as.numeric(current_vals)
        num_vals <- num_vals[!is.na(num_vals)]
        if (length(num_vals) > 0 && any(num_vals < exp_range[1] | num_vals > exp_range[2])) {
          badge_color <- "#dc3545"
        }
      }
      
      tagList(
        tags$label(
          class = "control-label",
          style = "display: flex; align-items: center; gap: 8px; width: 100%;",
          tags$span(label_text),
          tags$span(badge_text,
            style = paste0("font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: ", badge_color, "; color: white; font-weight: normal;"),
            title = paste0(i18n_r()$t("Experimental range:"), " ", badge_text)
          )
        ),
        selectizeInput(
          ns("predict_manual_tags"),
          label = NULL,
          choices = current_vals,
          selected = current_vals,
          multiple = TRUE,
          options = list(
            create = TRUE,
            persist = FALSE,
            placeholder = placeholder,
            render = I(paste0("{
              item: function(item, escape) {
                var val = parseFloat(item.value);
                var isOutOfRange = isNaN(val) || val < 0;
                ", range_js, "
                var style = isOutOfRange ? 'background-color: #dc3545; border-color: #dc3545;' : '';
                return '<div class=\"item\" style=\"' + style + '\">' + escape(item.value) + '</div>';
              }
            }"))
          )
        )
      )
    }
  })

  # Range warning for prediction values (works for all input modes)
  output$predict_range_warning_ui <- renderUI({
    req(input$predict_input_type)
    
    # For manual mode, read tags directly
    vals <- NULL
    if (input$predict_input_type == "manual") {
      tags_val <- input$predict_manual_tags
      if (!is.null(tags_val) && length(tags_val) > 0) {
        vals <- as.numeric(tags_val)
        vals <- vals[!is.na(vals)]
      }
    } else if (input$predict_input_type == "sequence") {
      from <- input$predict_seq_from
      to <- input$predict_seq_to
      step <- input$predict_seq_step
      if (!is.null(from) && !is.null(to) && !is.null(step) && !is.na(from) && !is.na(to) && !is.na(step) && step > 0 && from < to) {
        vals <- c(from, to)  # Only need endpoints to check range
      }
    } else if (input$predict_input_type == "csv") {
      vals <- predict_input_data()
    }
    
    if (is.null(vals) || length(vals) == 0) return(NULL)
    
    if (any(vals < 0)) {
      return(div(
        style = "background-color: #fff3cd; border: 1px solid #ffc107; border-radius: 4px; padding: 8px; margin-top: 5px; margin-bottom: 10px;",
        icon("exclamation-triangle", style = "color: #856404; margin-right: 6px;"),
        tags$span(style = "color: #856404;", i18n$t("Negative values detected. These will be ignored."))
      ))
    }
    
    exp_range <- predict_exp_range()
    if (!is.null(exp_range) && any(vals < exp_range[1] | vals > exp_range[2])) {
      return(div(
        style = "background-color: #fff3cd; border: 1px solid #ffc107; border-radius: 4px; padding: 8px; margin-top: 5px; margin-bottom: 10px;",
        icon("exclamation-triangle", style = "color: #856404; margin-right: 6px;"),
        tags$span(style = "color: #856404;",
          i18n$t("One or more values are outside the experimental range. Predictions may be unreliable (extrapolation)."))
      ))
    }
    NULL
  })

  # Parse prediction input data
  predict_input_data <- reactive({
    req(input$predict_input_type)
    data <- NULL

    if (input$predict_input_type == "csv") {
      if (is.null(input$predict_file_upload)) return(NULL)
      tryCatch({
        data <- read.csv(input$predict_file_upload$datapath, header = FALSE)[, 1]
        data <- as.numeric(data)
        data <- data[!is.na(data)]
      }, error = function(e) {
        showNotification(paste(i18n$t("Error loading CSV:"), e$message), type = "error")
        return(NULL)
      })
    } else if (input$predict_input_type == "sequence") {
      req(input$predict_seq_from, input$predict_seq_to, input$predict_seq_step)
      from <- input$predict_seq_from
      to <- input$predict_seq_to
      step <- input$predict_seq_step
      if (!is.na(from) && !is.na(to) && !is.na(step) && step > 0 && from < to) {
        data <- seq(from, to, by = step)
      }
    } else {
      # Tag input
      if (!is.null(input$predict_manual_tags) && length(input$predict_manual_tags) > 0) {
        data <- as.numeric(input$predict_manual_tags)
        data <- data[!is.na(data)]
      }
    }

    data
  })

  # Auto-correct percentage values to 1-100 range
  observeEvent(input$predict_manual_tags, {
    if ((input$predict_units %||% "sm") != "pct_yield") return()
    vals <- input$predict_manual_tags
    if (is.null(vals) || length(vals) == 0) return()
    num_vals <- suppressWarnings(as.numeric(vals))
    needs_fix <- !is.na(num_vals) & (num_vals < 1 | num_vals > 100)
    if (any(needs_fix, na.rm = TRUE)) {
      corrected <- ifelse(needs_fix, pmax(1, pmin(100, num_vals)), num_vals)
      new_vals <- as.character(corrected)
      updateSelectizeInput(session, "predict_manual_tags", selected = new_vals)
      showNotification(i18n$t("Percentage values were adjusted to the valid range (1\u2013100%)."), type = "warning")
    }
  }, ignoreInit = TRUE)

  # Auto-correct sequence inputs for pct_yield
  observeEvent(list(input$predict_seq_from, input$predict_seq_to), {
    if ((input$predict_units %||% "sm") != "pct_yield") return()
    from_val <- input$predict_seq_from
    to_val <- input$predict_seq_to
    if (!is.null(from_val) && !is.na(from_val) && from_val < 1) {
      updateNumericInput(session, "predict_seq_from", value = 1)
      showNotification(i18n$t("Percentage values were adjusted to the valid range (1\u2013100%)."), type = "warning")
    }
    if (!is.null(to_val) && !is.na(to_val) && to_val > 100) {
      updateNumericInput(session, "predict_seq_to", value = 100)
      showNotification(i18n$t("Percentage values were adjusted to the valid range (1\u2013100%)."), type = "warning")
    }
  }, ignoreInit = TRUE)

  # Calculate predictions
  observeEvent(input$predict_calculate, {
    print("=== PREDICTION CALCULATE BUTTON CLICKED (BIC) ===")
    print(paste("Full model result exists:", !is.null(kinetic_results$full_result)))

    # Check if model exists
    req(kinetic_results$full_result)

    print(paste("Predict input data:", predict_input_data()))
    print(paste("Is NULL:", is.null(predict_input_data())))
    print(paste("Length:", length(predict_input_data())))

    # Validate prediction data is not empty
    if (is.null(predict_input_data()) || length(predict_input_data()) == 0) {
      print("=== SHOWING WARNING: No prediction data (BIC) ===")
      showNotification(i18n$t("Please enter prediction data."), type = "warning", session = session)
      return()
    }

    print("=== VALIDATION PASSED, PROCEEDING (BIC) ===")

    req(input$predict_model)
    req(input$predict_units)

    # Validate CT model with time units
    if (input$predict_model == "ct" && input$predict_units %in% c("sm")) {
      showNotification(i18n$t("Characteristic Times (CT) model only accepts 'Time (minutes)' units."),
        type = "warning"
      )
      return(NULL)
    }

    tryCatch(
      {
        withProgress(
          message = i18n$t("Calculating predictions..."),
          value = 0,
          {
            incProgress(0.3, detail = i18n$t("Running predict_bic..."))

            if (input$predict_units == "pct_yield") {
              # Percentage of Maximum Yield mode
              target_pcts <- predict_input_data()
              
              full_result <- kinetic_results$full_result
              inpr <- full_result$input
              
              # Get time range from model data
              model_data <- full_result$data
              x_var <- input$oec_x_var %||% colnames(model_data)[1]
              t_max_data <- if (x_var %in% names(model_data)) max(model_data[[x_var]], na.rm = TRUE) else max(model_data[[1]], na.rm = TRUE)
              t_max <- t_max_data * 1.5
              dense_times <- seq(0.001, t_max, length.out = 2000)
              
              dense_pred <- supeRcrit::predict_bic(
                input = full_result,
                newdata = dense_times,
                units = "time",
                get_yields = TRUE
              )
              
              incProgress(0.6, detail = i18n$t("Interpolating yield milestones..."))
              
              # Get the selected model's predictions
              sel_model <- input$predict_model
              available_names <- names(dense_pred$predictions)
              
              # Map UI model name to prediction names
              model_lookup <- switch(sel_model,
                "sim" = "sim",
                "ct" = "ct",
                "cmp" = c("cmp3", "cmp2"),
                sel_model
              )
              
              pred_df <- NULL
              for (nm in model_lookup) {
                if (nm %in% available_names) {
                  pred_df <- dense_pred$predictions[[nm]]
                  break
                }
              }
              
              if (is.null(pred_df)) {
                # Fallback: use first available
                if (length(available_names) > 0) {
                  pred_df <- dense_pred$predictions[[available_names[1]]]
                }
              }
              
              if (is.null(pred_df) || nrow(pred_df) == 0) {
                showNotification(i18n$t("Could not generate predictions for the selected model."), type = "error")
                return(NULL)
              }
              
              # Find max yield from dense curve
              max_yield <- max(pred_df$yield, na.rm = TRUE)
              
              # For each target %, find interpolated time
              interp_times <- numeric(length(target_pcts))
              for (i in seq_along(target_pcts)) {
                target_yield <- (target_pcts[i] / 100) * max_yield
                idx <- which(pred_df$yield >= target_yield)
                if (length(idx) == 0) {
                  interp_times[i] <- NA
                } else if (idx[1] == 1) {
                  interp_times[i] <- pred_df$t[1]
                } else {
                  j <- idx[1]
                  y1 <- pred_df$yield[j - 1]; y2 <- pred_df$yield[j]
                  t1 <- pred_df$t[j - 1]; t2 <- pred_df$t[j]
                  interp_times[i] <- if (y2 != y1) t1 + (target_yield - y1) / (y2 - y1) * (t2 - t1) else t1
                }
              }
              
              valid <- !is.na(interp_times)
              if (!any(valid)) {
                showNotification(i18n$t("Could not find extraction times for the given yield targets."), type = "warning")
                return(NULL)
              }
              
              pred_result <- supeRcrit::predict_bic(
                input = full_result,
                newdata = interp_times[valid],
                units = "time",
                get_yields = input$predict_get_yields
              )
              
              # Add target percentage column to each prediction data frame
              valid_pcts <- target_pcts[valid]
              for (nm in names(pred_result$predictions)) {
                df <- pred_result$predictions[[nm]]
                if (!is.null(df) && nrow(df) > 0) {
                  # Ensure pct_col matches df rows
                  pct_col <- if (length(valid_pcts) == nrow(df)) valid_pcts else rep(NA, nrow(df))
                  pred_result$predictions[[nm]] <- cbind(target_pct = pct_col, df)
                }
              }
              
              kinetic_results$predict_result <- pred_result
            } else {
              # Standard mode: time or S/M ratio
              pred_result <- supeRcrit::predict_bic(
                input = kinetic_results$full_result,
                newdata = predict_input_data(),
                units = input$predict_units,
                get_yields = input$predict_get_yields
              )

              # Store results
              kinetic_results$predict_result <- pred_result
            }

            incProgress(1, detail = i18n$t("Completed!"))
          }
        )

        showNotification(i18n$t("Predictions calculated successfully!"),
          type = "message"
        )
      },
      error = function(e) {
        showNotification(paste(
          i18n$t("Error calculating predictions:"),
          e$message
        ), type = "error")
        kinetic_results$predict_result <- NULL
      }
    )
  })

  # Render predictions table
  output$predict_results_table <- DT::renderDataTable(
    {
      req(kinetic_results$predict_result)
      req(input$predict_model)

      # Get selected model predictions
      preds <- kinetic_results$predict_result$predictions[[input$predict_model]]

      req(preds)

      # Column names translation - dynamically based on actual columns
      possible_colnames <- c(
        "target_pct" = i18n_r()$t("Target (%)"),
        "t" = i18n_r()$t("Time (min)"),
        "q" = i18n_r()$t("Solvent/Material Ratio (kg/kg)"),
        "sm" = i18n_r()$t("S/M Ratio"),
        "yield" = i18n_r()$t("Fractional Yield (g/g)"),
        "yield_mass" = i18n_r()$t("Yield - Mass (g)"),
        "yield_percent" = i18n_r()$t("Yield - Percent (%)")
      )

      # Get translations for columns that exist in preds, in correct order
      add_prettynames <- sapply(colnames(preds), function(col) {
        if (col %in% names(possible_colnames)) {
          possible_colnames[[col]]
        } else {
          col  # fallback to original name if translation not found
        }
      }, USE.NAMES = FALSE)

      # Build columnDefs for trimming trailing zeroes with appropriate precision
      tq_cols <- which(colnames(preds) %in% c("t", "q", "sm")) - 1
      yield_cols <- which(colnames(preds) == "yield") - 1
      mass_cols <- which(colnames(preds) == "yield_mass") - 1
      pct_cols <- which(colnames(preds) == "yield_percent") - 1
      
      DT::datatable(
        preds,
        extensions = "Buttons",
        colnames = add_prettynames,
        options = list(
          scrollX = TRUE,
          pageLength = 25,
          dom = "Bfrtip",
          language = tablang(),
          columnDefs = c(
            list(list(className = "dt-center", targets = "_all")),
            trim_zeros_columndefs(tq_cols, digits = 2),
            trim_zeros_columndefs(yield_cols, digits = 6),
            trim_zeros_columndefs(mass_cols, digits = 4),
            trim_zeros_columndefs(pct_cols, digits = 2)
          ),
          buttons = list(
            list(
              extend = "copy",
              text = i18n_r()$t("Copy"),
              titleAttr = i18n_r()$t("Copy"),
              action = copy_button_no_popup(
                copy_label = i18n_r()$t("Copy"),
                copied_label = i18n_r()$t("Copied!")
              )
            ),
            list(
              extend = "csv",
              filename = generate_filename_with_timestamp(
                paste0(
                  "supercrit_bic_predictions_",
                  input$predict_model
                )
              )
            ),
            list(
              extend = "excel",
              filename = generate_filename_with_timestamp(
                paste0(
                  "supercrit_bic_predictions_",
                  input$predict_model
                )
              )
            ),
            list(
              extend = "pdf",
              filename = generate_filename_with_timestamp(
                paste0(
                  "supercrit_bic_predictions_",
                  input$predict_model
                )
              )
            )
          )
        ),
        rownames = FALSE,
        escape = FALSE
      )
    },
    server = FALSE
  )

  # Render unit chart
  output$predict_unit_chart <- DT::renderDataTable({
    req(kinetic_results$predict_result)

    # Unit translations for all cells
    unit_translations <- c(
      "min" = i18n_r()$t("min"),
      "g" = i18n_r()$t("g"),
      "kg/kg insoluble solid" = i18n_r()$t("kg/kg insoluble solid"),
      "dimensionless (S/M ratio)" = i18n_r()$t("dimensionless (S/M ratio)"),
      "g/g insoluble solid" = i18n_r()$t("g/g insoluble solid"),
      "g/g total solid" = i18n_r()$t("g/g total solid"),
      "percent dry weight" = i18n_r()$t("percent dry weight")
    )

    dt_data <- as.data.frame(kinetic_results$predict_result$unit_chart, stringsAsFactors = FALSE)
    # The unit_chart always has 3 rows in order: sim, ct, cmp (hardcoded in predict_bic).
    # model_type column is stripped by predict_bic. Select correct row by model name.
    sel_model <- input$predict_model %||% "sim"
    row_idx <- switch(sel_model, "sim" = 1, "ct" = 2, "cmp2" = 3, "cmp3" = 3, 1)
    row_idx <- min(row_idx, nrow(dt_data))
    dt_data <- dt_data[row_idx, , drop = FALSE]
    rownames(dt_data) <- NULL

    # Apply translations to all columns
    for (col in colnames(dt_data)) {
      dt_data[[col]] <- my_mapvalues(dt_data[[col]], names(unit_translations), unit_translations, warn_missing = FALSE)
    }

    # Translate column names
    col_map <- c(
      "model_type" = i18n_r()$t("Model"),
      "t" = i18n_r()$t("Time"),
      "q" = i18n_r()$t("Solvent/Material Ratio"),
      "sm" = i18n_r()$t("S/M Ratio"),
      "yield" = i18n_r()$t("Fractional Yield"),
      "yield_mass" = i18n_r()$t("Mass Yield"),
      "yield_percent" = i18n_r()$t("Percentage Yield")
    )
    col_names <- ifelse(colnames(dt_data) %in% names(col_map), col_map[colnames(dt_data)], colnames(dt_data))
    
    DT::datatable(
      dt_data,
      colnames = col_names,
      options = list(pageLength = 10, dom = "t", language = tablang(),
        columnDefs = list(list(className = "dt-center", targets = "_all"))),
      rownames = FALSE
    )
  })

  # Render description
  output$predict_description <- renderUI({
    req(kinetic_results$predict_result)
    req(kinetic_results$full_result)
    
    inpr <- kinetic_results$full_result$input
    
    # Build properly formatted, translated description with bold values
    lines <- c(i18n_r()$t("Predictions are valid for the following process parameters:"))
    
    if (!is.null(inpr[["pres"]])) {
      lines <- c(lines, paste0(i18n_r()$t("Pressure"), " ", i18n_r()$t("of"), " <strong>", round(inpr[["pres"]], 1), " ", i18n_r()$t("bar"), "</strong>."))
    }
    if (!is.null(inpr[["temp"]])) {
      lines <- c(lines, paste0(i18n_r()$t("Temperature"), " ", i18n_r()$t("of"), " <strong>", round(inpr[["temp"]], 1), " \u00B0C</strong>."))
    }
    if (!is.null(inpr[["flow"]]) && !is.na(inpr[["flow"]])) {
      # inpr[["flow"]] is qaver in kg/s; convert to g/min (always)
      flow_gmin <- round(inpr[["flow"]] * 1000 * 60, 2)
      flow_line <- paste0(i18n_r()$t("Flow rate"), " ", i18n_r()$t("of"), " <strong>", flow_gmin, " ", i18n_r()$t("g/min"), "</strong>")
      
      # Add user's original input in brackets with measurement conditions
      flow_val <- kinetic_results$flow_value_snapshot
      flow_unit <- kinetic_results$flow_units_snapshot %||% "g/min"
      flowpar <- kinetic_results$flowpar_snapshot
      
      if (!is.null(flow_val) && !is.na(flow_val) && flow_unit != "g/min") {
        context_parts <- paste0("<strong>", round(flow_val, 2), " ", flow_unit, "</strong>")
        # flowpar is only relevant for volumetric units (mL/min, L/h)
        is_volumetric <- grepl("^mL|^L", flow_unit)
        if (is_volumetric && !is.null(flowpar) && !all(is.na(flowpar))) {
          fp_pres <- if (!is.na(flowpar[1])) paste0("<strong>", round(flowpar[1], 1), " ", i18n_r()$t("bar"), "</strong>") else NULL
          fp_temp <- if (!is.na(flowpar[2])) paste0("<strong>", round(flowpar[2], 1), " \u00B0C</strong>") else NULL
          fp_conds <- paste(c(fp_pres, fp_temp), collapse = paste0(" ", i18n_r()$t("and"), " "))
          if (nchar(fp_conds) > 0) {
            context_parts <- paste0(context_parts, " ", i18n_r()$t("at"), " ", fp_conds)
          }
        }
        flow_line <- paste0(flow_line, " (", context_parts, ")")
      }
      lines <- c(lines, paste0(flow_line, "."))
    }
    solvent_mode <- kinetic_results$solvent_mode_snapshot %||% FALSE
    if (!is.null(inpr[["etoh"]]) && inpr[["etoh"]] > 0 && !solvent_mode) {
      # Direct flow rate was provided (not solvent data mode)
      lines <- c(lines, paste0(i18n_r()$t("Co-solvent flow rate"), " ", i18n_r()$t("of"), " <strong>", round(inpr[["etoh"]], 2), " ", i18n_r()$t("mL/min"), "</strong>."))
    }
    # Co-solvent fraction (solvent data mode)
    etoh_frac_val <- kinetic_results$etoh_frac_snapshot %||% 0
    if (etoh_frac_val > 0 && solvent_mode) {
      lines <- c(lines, paste0(i18n_r()$t("Co-solvent fraction"), " ", i18n_r()$t("of"), " <strong>", round(etoh_frac_val, 4), "</strong>."))
    }
    if (!is.null(inpr[["D"]])) {
      lines <- c(lines, paste0(i18n_r()$t("Extractor diameter"), " ", i18n_r()$t("of"), " <strong>", round(inpr[["D"]], 4), " ", i18n_r()$t("m"), "</strong>."))
    }
    if (!is.null(inpr[["L"]])) {
      lines <- c(lines, paste0(i18n_r()$t("Extractor length"), " ", i18n_r()$t("of"), " <strong>", round(inpr[["L"]], 4), " ", i18n_r()$t("m"), "</strong>."))
    }
    if (!is.null(inpr[["cu"]]) && !is.na(inpr[["cu"]])) {
      lines <- c(lines, paste0(i18n_r()$t("Extractable fraction"), " ", i18n_r()$t("of"), " <strong>", round(inpr[["cu"]], 4), "</strong>."))
    }
    
    lines <- c(lines, i18n_r()$t("Refer to the unit reference table for units of input and predicted data."))
    
    HTML(paste0("<p>", paste(lines, collapse = "<br/>"), "</p>"))
  })

  # Reset predictions
  observeEvent(input$pred_reset_btn, {
    kinetic_results$predict_result <- NULL
    kinetic_results$predict_data <- NULL
    updateRadioButtons(session, "predict_input_type", selected = "manual")
    updateSelectizeInput(session, "predict_manual_tags", selected = character(0))
    updateCheckboxInput(session, "predict_get_yields", value = TRUE)
    showNotification(i18n$t("Predictions reset"), type = "message")
  })

  # Download Current Data as CSV
  output$download_current_data <- downloadHandler(
    filename = function() {
      timestamp <- format(Sys.time(), "%d-%m-%Y_%Hhrs-%Mmin-%Ssec")
      paste0("kinetic_bic_data_", timestamp, ".csv")
    },
    content = function(file) {
      data <- oec_data()
      if (!is.null(data) && nrow(data) > 0) {
        write.csv(data, file, row.names = FALSE)
      } else {
        # Write empty file to prevent error (button should be disabled anyway)
        writeLines("", file)
      }
    },
    contentType = "text/csv"
  )

  # Export kinetic model results
  output$export_kinetic <- downloadHandler(
    filename = function() {
      # Generate filename with timestamp
      timestamp <- format(Sys.time(), "%d-%m-%Y_%Hhrs-%Mmin-%Ssec")
      paste0("supercrit_kinetic_bic_export_", timestamp, ".zip")
    },
    content = function(file) {
      req(kinetic_results$full_result)

      tryCatch(
        {
          # Create temporary directory
          temp_dir <- tempdir()

          # Call kin_export with temporary directory (only modres for individual models)
          kin_export(
            modres = kinetic_results$full_result,
            expath = temp_dir,
            silent = TRUE
          )

          # Find the created directory (should start with "KIN_")
          kin_dirs <- list.dirs(temp_dir, full.names = TRUE, recursive = FALSE)
          kin_dirs <- kin_dirs[grepl("^KIN_", basename(kin_dirs))]

          if (length(kin_dirs) > 0) {
            # Use the most recent one
            kin_dir <- kin_dirs[order(basename(kin_dirs), decreasing = TRUE)][1]

            # Zip the directory
            zip::zip(zipfile = file, files = basename(kin_dir), root = temp_dir)

            showNotification(i18n$t("Kinetic model results exported successfully!"), type = "message")
          } else {
            stop("Could not find exported files directory")
          }
        },
        error = function(e) {
          showNotification(paste(i18n$t("Error exporting kinetic model results:"), e$message), type = "error")
        }
      )
    }
  )

  # Ensure UI outputs in collapsed accordions are rendered immediately
  # This prevents inputs from being NULL until accordion is opened
  outputOptions(output, "flow_input_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "flow_units_ui", suspendWhenHidden = FALSE)

  # ============================================================
  # Co-Solvent Flow Rate: disabled when solvent data is provided
  # ============================================================
  output$etoh_input_ui <- renderUI({
    is_disabled <- solvent_selected()
    
    cur <- isolate(input$etoh) %||% defaults$etoh
    
    etoh_div <- tags$div(
      tags$label(
        i18n_r()$t("Co-Solvent Flow Rate (mL/min)"),
        input_help(i18n_r()$t("Volumetric flow rate of the co-solvent (e.g. ethanol) in mL/min. Set to 0 if no co-solvent is used."),
                   title = i18n_r()$t("Co-Solvent Flow"), buttonLabel = i18n_r()$t("OK")),
        class = "control-label",
        title = i18n_r()$t("Volumetric flow rate of ethanol co-solvent. Only applicable when a direct CO\u2082 flow rate is provided.")
      ),
      numericInput(ns("etoh"), label = NULL, value = cur, min = 0)
    )
    
    if (is_disabled) shinyjs::disabled(etoh_div) else etoh_div
  })

  # Flow Measurement Pressure - disabled for mass-based flow units
  output$flowpar_pres_ui <- renderUI({
    flow_unit <- input$flow_units %||% defaults$flow_units %||% "g/min"
    is_mass <- grepl("^g/|^kg/", flow_unit)
    
    cur <- isolate(input$flowpar_pres) %||% defaults$flowpar_pres
    
    fp_div <- tags$div(
      tags$label(i18n_r()$t("Flow Measurement Pressure (bar)"),
        input_help(i18n_r()$t("Pressure at which your flow meter measured the CO2 flow rate. Only relevant when measured at conditions different from extraction."),
                   title = i18n_r()$t("Flow Meas. P"), buttonLabel = i18n_r()$t("OK")),
        class = "control-label"),
      numericInput(ns("flowpar_pres"), label = NULL, value = cur, min = 0)
    )
    
    if (is_mass) {
      shinyjs::disabled(tags$div(
        tags$label(i18n_r()$t("Flow Measurement Pressure (bar)"), class = "control-label"),
        tags$div(
          style = "background-color: #f5f5f5; border: 1px solid #ddd; border-radius: 4px; padding: 6px 12px; color: #888; height: 34px; line-height: 22px;",
          i18n_r()$t("Not needed with mass flow")
        )
      ))
    } else fp_div
  })

  # Flow Measurement Temperature - disabled for mass-based flow units
  output$flowpar_temp_ui <- renderUI({
    flow_unit <- input$flow_units %||% defaults$flow_units %||% "g/min"
    is_mass <- grepl("^g/|^kg/", flow_unit)
    
    cur <- isolate(input$flowpar_temp) %||% defaults$flowpar_temp
    
    ft_div <- tags$div(
      tags$label(i18n_r()$t("Flow Measurement Temperature (\u00B0C)"),
        input_help(i18n_r()$t("Temperature at which your flow meter measured the CO2 flow rate. Only relevant when measured at conditions different from extraction."),
                   title = i18n_r()$t("Flow Meas. T"), buttonLabel = i18n_r()$t("OK")),
        class = "control-label"),
      numericInput(ns("flowpar_temp"), label = NULL, value = cur, min = 0)
    )
    
    if (is_mass) {
      shinyjs::disabled(tags$div(
        tags$label(i18n_r()$t("Flow Measurement Temperature (\u00B0C)"), class = "control-label"),
        tags$div(
          style = "background-color: #f5f5f5; border: 1px solid #ddd; border-radius: 4px; padding: 6px 12px; color: #888; height: 34px; line-height: 22px;",
          i18n_r()$t("Not needed with mass flow")
        )
      ))
    } else ft_div
  })

  # ============================================================
  # Co-Solvent Fraction: always shown, disabled when not needed
  # ============================================================
  output$etoh_frac_ui <- renderUI({
    flow_val <- input$flow
    etoh_val <- input$etoh
    use_solvent <- solvent_selected()
    
    flow_empty <- is.null(flow_val) || is.na(flow_val) || flow_val == "" || flow_val == 0
    etoh_positive <- !is.null(etoh_val) && !is.na(etoh_val) && etoh_val > 0
    
    # Needed when: (flow is empty AND etoh > 0) OR (solvent data is used)
    is_needed <- (flow_empty && etoh_positive) || use_solvent
    
    cur <- isolate(input$etoh_frac) %||% defaults$etoh_frac
    
    if (is_needed) {
      tags$div(
        tags$label(
          i18n_r()$t("Co-Solvent Fraction"),
          input_help(i18n_r()$t("Volume fraction of co-solvent in the total solvent mixture (0-0.99). Required when CO2 flow rate is not provided but co-solvent is used, so the mixture density can be calculated."),
                     title = i18n_r()$t("Co-Solvent Fraction"), buttonLabel = i18n_r()$t("OK")),
          class = "control-label",
          style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
          title = i18n_r()$t("Volumetric fraction of ethanol in the total solvent mixture (e.g. 0.06 = 6% ethanol). Required when using solvent consumption data or when CO\u2082 flow rate is not provided."),
          tags$span("0\u20130.99",
            style = "font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: #6c757d; color: white; margin-left: auto; font-weight: normal;"
          )
        ),
        numericInput(ns("etoh_frac"), label = NULL, value = cur, min = 0, max = 0.99, step = 0.01)
      )
    } else {
      shinyjs::disabled(tags$div(
        tags$label(i18n_r()$t("Co-Solvent Fraction"), class = "control-label"),
        tags$div(
          style = "background-color: #f5f5f5; border: 1px solid #ddd; border-radius: 4px; padding: 6px 12px; color: #888; height: 34px; line-height: 22px;",
          i18n_r()$t("Calculated from flow rates")
        )
      ))
    }
  })
  outputOptions(output, "etoh_frac_ui", suspendWhenHidden = FALSE)

  observeEvent(input$etoh_frac, {
    val <- input$etoh_frac
    if (!is.null(val) && !is.na(val) && (val < 0 || val > 0.99)) {
      corrected <- max(0, min(0.99, val))
      showNotification(i18n$t("Co-Solvent Fraction was adjusted to the valid range (0\u20130.99)."), type = "warning")
      updateNumericInput(session, "etoh_frac", value = corrected)
    }
  }, ignoreInit = TRUE)

  # ============================================================
  # Range checks with auto-correct
  # ============================================================
  
  # Moisture 0-100%
  observeEvent(input$moisture, {
    val <- input$moisture
    if (!is.null(val) && !is.na(val) && (val < 0 || val > 100)) {
      corrected <- max(0, min(100, val))
      showNotification(i18n$t("Moisture Content was adjusted to the valid range (0\u2013100%)."), type = "warning")
      updateNumericInput(session, "moisture", value = corrected)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$dp, {
    val <- input$dp
    if (!is.null(val) && !is.na(val) && val <= 0) {
      showNotification(i18n$t("Particle Diameter must be greater than 0."), type = "warning")
      updateNumericInput(session, "dp", value = defaults$dp)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$dr, {
    val <- input$dr
    if (!is.null(val) && !is.na(val) && val <= 0) {
      showNotification(i18n$t("Real Density must be greater than 0."), type = "warning")
      updateNumericInput(session, "dr", value = defaults$dr)
    }
  }, ignoreInit = TRUE)

  # Number of Observations (CER): 1 to n-1
  observeEvent(input$n, {
    val <- input$n
    data <- oec_data()
    if (!is.null(val) && !is.na(val) && !is.null(data)) {
      n_data <- nrow(data)
      if (val < 1 || val >= n_data) {
        corrected <- max(1, min(n_data - 1, val))
        showNotification(
          paste0(i18n$t("Number of Observations (CER) was adjusted to the valid range (1\u2013"), n_data - 1, ")."),
          type = "warning"
        )
        updateNumericInput(session, "n", value = corrected)
      }
    }
  }, ignoreInit = TRUE)

  # Max Extractable Fraction cu: 0 < cu <= 1
  observeEvent(input$cu, {
    val <- input$cu
    if (!is.null(val) && !is.na(val) && (val <= 0 || val > 1)) {
      corrected <- max(0.01, min(1, val))
      showNotification(i18n$t("Max Extractable Fraction must be between 0 and 1. Value was adjusted."), type = "warning")
      updateNumericInput(session, "cu", value = corrected)
    }
  }, ignoreInit = TRUE)

  # Number of Observations (CER) with dynamic range badge
  output$n_input_ui <- renderUI({
    data <- oec_data()
    n_data <- if (!is.null(data)) nrow(data) else NA
    
    badge_text <- if (!is.na(n_data) && n_data > 1) paste0("1\u2013", n_data - 1) else "\u2265 1"
    
    cur_val <- isolate(input$n) %||% defaults$n
    
    tags$div(
      tags$label(
        i18n_r()$t("Number of Observations (CER)"),
        input_help(i18n_r()$t("Number of data points in your extraction curve that correspond to the end of the Constant Extraction Rate (CER) period. This boundary is used to segment the extraction curve and estimate apparent solubility."),
                   title = i18n_r()$t("CER Endpoint"), buttonLabel = i18n_r()$t("OK")),
        class = "control-label",
        style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
        div(style = "display: flex; align-items: center; gap: 4px; margin-left: auto;",
          tags$span(badge_text,
            style = "font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: #6c757d; color: white; font-weight: normal;"
          ),
          uiOutput(ns("estimate_n_link_ui"), inline = TRUE)
        )
      ),
      numericInput(ns("n"), NULL, value = cur_val, min = 1)
    )
  })

  # ============================================================
  # Extractor Diameter with unit selector badge
  # ============================================================
  dim_unit_D <- reactiveVal("m")
  dim_unit_L <- reactiveVal("m")
  
  output$D_ui <- renderUI({
    unit <- dim_unit_D()
    cur_val <- isolate(input$D) %||% defaults$D
    
    display_val <- switch(unit, "m" = cur_val, "cm" = cur_val * 100, "mm" = cur_val * 1000)
    
    tags$div(
      tags$label(
        i18n_r()$t("Extractor Diameter"),
        input_help(i18n_r()$t("Inner diameter of the extraction vessel. Click the unit badge to cycle between meters, centimeters, and millimeters. Used to calculate bed volume and porosity."),
                   title = i18n_r()$t("Extractor Diameter"), buttonLabel = i18n_r()$t("OK")),
        class = "control-label",
        style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
        actionLink(ns("toggle_dim_unit_D"), unit,
          style = "font-size: 10px; padding: 1px 5px; border-radius: 3px; text-decoration: none; background-color: #17a2b8; color: white; margin-left: auto; font-weight: normal; cursor: pointer;",
          title = i18n_r()$t("Click to cycle units: m, cm, mm")
        )
      ),
      numericInput(ns("D"), label = NULL, value = round(display_val, 6), min = 0, step = 0.001)
    )
  })
  
  output$L_ui <- renderUI({
    unit <- dim_unit_L()
    cur_val <- isolate(input$L) %||% defaults$L
    
    display_val <- switch(unit, "m" = cur_val, "cm" = cur_val * 100, "mm" = cur_val * 1000)
    
    tags$div(
      tags$label(
        i18n_r()$t("Extractor Length"),
        input_help(i18n_r()$t("Length of the extraction vessel. Click the unit badge to cycle between meters, centimeters, and millimeters. Used to calculate bed volume and porosity."),
                   title = i18n_r()$t("Extractor Length"), buttonLabel = i18n_r()$t("OK")),
        class = "control-label",
        style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
        actionLink(ns("toggle_dim_unit_L"), unit,
          style = "font-size: 10px; padding: 1px 5px; border-radius: 3px; text-decoration: none; background-color: #17a2b8; color: white; margin-left: auto; font-weight: normal; cursor: pointer;",
          title = i18n_r()$t("Click to cycle units: m, cm, mm")
        )
      ),
      numericInput(ns("L"), label = NULL, value = round(display_val, 6), min = 0, step = 0.001)
    )
  })
  
  # Cycle dimension units independently
  observeEvent(input$toggle_dim_unit_D, {
    cur <- dim_unit_D()
    D_val <- input$D %||% defaults$D
    D_m <- switch(cur, "m" = D_val, "cm" = D_val / 100, "mm" = D_val / 1000)
    new_unit <- switch(cur, "m" = "cm", "cm" = "mm", "mm" = "m")
    dim_unit_D(new_unit)
    updateNumericInput(session, "D", value = round(switch(new_unit, "m" = D_m, "cm" = D_m * 100, "mm" = D_m * 1000), 6))
  })
  
  observeEvent(input$toggle_dim_unit_L, {
    cur <- dim_unit_L()
    L_val <- input$L %||% defaults$L
    L_m <- switch(cur, "m" = L_val, "cm" = L_val / 100, "mm" = L_val / 1000)
    new_unit <- switch(cur, "m" = "cm", "cm" = "mm", "mm" = "m")
    dim_unit_L(new_unit)
    updateNumericInput(session, "L", value = round(switch(new_unit, "m" = L_m, "cm" = L_m * 100, "mm" = L_m * 1000), 6))
  })

  # ============================================================
  # Optimization Parameters with italic labels and tooltips
  # ============================================================
  opt_param_ui <- function(input_id, symbol_html, unit, tooltip, default_val, range_text = "> 0") {
    cur <- isolate(input[[input_id]]) %||% default_val
    # Extract plain text from symbol_html for the modal title
    symbol_plain <- gsub("<[^>]+>", "", symbol_html)
    tags$div(
      tags$label(
        class = "control-label",
        style = "display: flex; align-items: center; justify-content: space-between; width: 100%; cursor: help;",
        title = tooltip,
        tags$span(HTML(symbol_html)),
        tags$span(paste0("(", unit, ")"), style = "color: #666; font-weight: normal; margin-left: 2px;"),
        input_help(tooltip, title = symbol_plain, buttonLabel = i18n_r()$t("OK")),
        tags$span(range_text,
          style = "font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: #6c757d; color: white; margin-left: auto; font-weight: normal; flex-shrink: 0;"
        )
      ),
      textInput(ns(input_id), label = NULL, value = cur)
    )
  }

  # Reset Optimization Parameters to defaults
  observeEvent(input$reset_opt_params, {
    updateTextInput(session, "r_est", value = defaults$r_est)
    updateSelectizeInput(session, "ksas_est", choices = defaults$ksas_est, selected = defaults$ksas_est)
    updateTextInput(session, "qc_est", value = defaults$qc_est)
    updateTextInput(session, "thetaf_est", value = defaults$thetaf_est)
    updateTextInput(session, "ti_est", value = defaults$ti_est)
    updateTextInput(session, "kf_est", value = defaults$kf_est)
    showNotification(i18n$t("Optimization parameters reset to defaults."), type = "message")
  })

  output$r_est_ui <- renderUI({
    opt_param_ui("r_est", "<em>r</em>", "\u2014",
      i18n_r()$t("Grinding efficiency: fraction of broken cells (0\u20131)"),
      defaults$r_est, range_text = "0\u20131")
  })
  
  output$ksas_est_ui <- renderUI({
    cur <- isolate(input$ksas_est) %||% defaults$ksas_est
    # Ensure cur is a character vector
    if (is.null(cur) || length(cur) == 0) cur <- c("1e-06", "1e-05", "1e-04", "1e-03")
    tooltip <- i18n_r()$t("Product of solid-phase mass transfer coefficient and specific interfacial area. Multiple starting values enable multi-start optimization for better convergence.")
    tags$div(
      tags$label(
        class = "control-label",
        style = "display: flex; align-items: center; justify-content: space-between; width: 100%; cursor: help;",
        title = tooltip,
        tags$span(HTML("<em>k<sub>s</sub>a<sub>s</sub></em>")),
        tags$span("(s\u207B\u00B9)", style = "color: #666; font-weight: normal; margin-left: 2px;"),
        input_help(tooltip, title = "ksas", buttonLabel = i18n_r()$t("OK")),
        tags$span(paste0(i18n_r()$t("max"), " 6"),
          style = "font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: #6c757d; color: white; margin-left: auto; font-weight: normal; flex-shrink: 0;"
        )
      ),
      selectizeInput(ns("ksas_est"), label = NULL,
        choices = cur, selected = cur, multiple = TRUE,
        options = list(create = TRUE, maxItems = 6,
                       placeholder = i18n_r()$t("Enter ksas values...")))
    )
  })
  
  output$qc_est_ui <- renderUI({
    opt_param_ui("qc_est",
      HTML("<em>q<sub>m</sub></em>"), "kg/kg",
      i18n_r()$t("Relative amount of solvent at the end of the CER period"),
      defaults$qc_est)
  })
  
  output$thetaf_est_ui <- renderUI({
    opt_param_ui("thetaf_est",
      HTML("\u03B8<sub>e</sub>"), "\u2014",
      i18n_r()$t("External mass transfer coefficient"),
      defaults$thetaf_est)
  })
  
  output$ti_est_ui <- renderUI({
    opt_param_ui("ti_est",
      HTML("<em>t<sub>i</sub></em>"), "min",
      i18n_r()$t("Duration of the falling extraction rate (FER) period"),
      defaults$ti_est)
  })
  
  output$kf_est_ui <- renderUI({
    opt_param_ui("kf_est",
      HTML("<em>k<sub>f</sub></em>"), "s\u207B\u00B9",
      i18n_r()$t("Fluid-phase mass transfer coefficient"),
      defaults$kf_est)
  })

  outputOptions(output, "resp_units_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "aggreg_ui", suspendWhenHidden = FALSE)

  # suspendWhenHidden for renderUI outputs (auto-added)
  outputOptions(output, "D_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "L_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "accordion_toggle_btn", suspendWhenHidden = FALSE)
  outputOptions(output, "column_validation_message", suspendWhenHidden = FALSE)
  outputOptions(output, "cu_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "cumulative_checkbox_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "cumulative_warning_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "data_input_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "data_preview_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "dp_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "dr_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "duplicate_column_warning_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "estimate_cu_link_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "estimate_n_link_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "etoh_input_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "flow_params_disabled_message", suspendWhenHidden = FALSE)
  outputOptions(output, "flowpar_pres_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "flowpar_temp_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "insufficient_columns_warning_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "kf_est_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "kinetic_bic_HELP", suspendWhenHidden = FALSE)
  outputOptions(output, "ksas_est_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "load_example_data_btn", suspendWhenHidden = FALSE)
  outputOptions(output, "mass_flow_input_section_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "mass_in_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "model_detail_selector_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "modtype_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "moisture_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "n_input_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "oec_slv_var_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "oec_x_var_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "oec_y_var_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "plot_selector_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "plot_xaxis_toggle_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "predict_data_input_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "predict_description", suspendWhenHidden = FALSE)
  outputOptions(output, "predict_get_yields_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "predict_model_selector_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "predict_range_warning_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "predict_units_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "pres_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "qc_est_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "qmax_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "r_est_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "rename_col_controls_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "ro_co2_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "solvent_units_info_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "temp_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "thetaf_est_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "ti_est_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "tmax_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "use_solvent_checkbox_ui", suspendWhenHidden = FALSE)

}
