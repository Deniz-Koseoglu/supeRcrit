# Two-Site Kinetic Modeling Server Module
kinetic_tws_server <- function(input, output, session, defaults, i18n, tablang) {




  # Load required libraries
  library(dplyr)
  library(DT)
  library(plotly)
  library(zip) # For zipping export files
  library(supeRcrit) # Assuming ktsmod is part of supeRcrit

  # Helper for creating namespaced ids inside this module
  ns <- session$ns
  i18n_r <- reactive({
    i18n
  })

  # Note: trim_zeros_columndefs is defined in utils/general_helpers.R

  # Load Example button: rendered server-side so i18n_r()$t() returns a plain
  # string (rather than a shiny.i18n <span> wrapper) for the title attribute.
  output$load_example_data_btn <- renderUI({
    create_load_example_btn(ns, i18n_r)
  })

  output$kinetic_tws_HELP <- renderUI({
    create_help_modal(i18n_r, "kinetic_tws_help_en")
  })

  # Render accordion expand/collapse button with translated tooltips
  output$accordion_toggle_btn <- renderUI({
    create_accordion_toggle_btn(ns, i18n_r, "param_accordion")
  })

  # Note: %||% operator is defined in utils/general_helpers.R

  # Reactive values for storing results
  kinetic_results <- reactiveValues(
    model_data = NULL,
    model_summary = NULL,
    model_plot = NULL
  )

  # Reactive output to check if results are available
  output$has_results <- reactive({
    !is.null(kinetic_results$model_data)
  })
  outputOptions(output, "has_results", suspendWhenHidden = FALSE)

  # Observer to enable/disable result tabs based on results availability
  observe({
    has_results <- !is.null(kinetic_results$model_data)
    if (has_results) {
      session$sendCustomMessage("enableTabs", list(tabsetId = ns("results_tabs")))
    } else {
      session$sendCustomMessage("disableTabs", list(tabsetId = ns("results_tabs"), keepFirst = TRUE))
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

  output$cumulative_checkbox_ui <- renderUI({
    tags$span(
      checkboxInput(ns("cumulative"), i18n_r()$t("Cumulative Data"),
                    value = isolate(input$cumulative) %||% defaults$cumulative),
      title = i18n_r()$t("Enable if your response data represents the total accumulated yield up to each time point. Disable if each row shows the yield collected during that interval only.")
    )
  })

  output$use_solvent_checkbox_ui <- renderUI({
    tags$span(
      checkboxInput(ns("use_solvent"), i18n_r()$t("Use Solvent Data"),
                    value = isolate(input$use_solvent) %||% FALSE),
      title = i18n_r()$t("Enable to use a solvent consumption column from your data instead of calculating it from flow rate. When enabled, the Flow Parameters section is disabled.")
    )
  })

  # Render Mass Flow checkbox (only visible when solvent data is enabled)
  output$mass_flow_input_section_ui <- renderUI({
    if (solvent_selected()) {
      tags$span(
        checkboxInput(ns("mass_flow"), i18n_r()$t("Mass Solvent Data"), value = defaults$mass_flow %||% FALSE),
        title = i18n_r()$t("Enable if the solvent consumption column in your data is in mass units (grams). Disable if it is in volume units (mL).")
      )
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
      mass_flow_checked <- isTRUE(input$mass_flow)
      current_units <- if (mass_flow_checked) "g" else "mL"
      units_label <- if (mass_flow_checked) i18n$t("(grams)") else i18n$t("(milliliters)")
      
      div(
        class = "alert alert-info",
        style = "margin: 10px 0 0 0; padding: 10px 12px; font-size: 12px;",
        icon("info-circle"),
        " ", tags$strong(i18n$t("Solvent Data Provided:")), " ",
        i18n$t("When a Solvent column is selected, the values must represent the amount of solvent expended (cumulative or per interval)."),
        tags$br(), tags$br(),
        tags$strong(i18n$t("Current solvent units:")), " ",
        tags$span(style = "font-weight: bold; color: #31708f;", paste0(current_units, " ", units_label))
      )
    } else {
      NULL
    }
  })

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

  # Dynamic UI for flow rate input - shows selected flow unit in label
  # Disabled when solvent column is selected
  output$flow_input_ui <- renderUI({
    flow_unit <- input$flow_units %||% defaults$flow_units %||% "g/min"
    unit_display <- if (flow_unit == "none") "" else paste0(" (", i18n_r()$t(flow_unit), ")")
    label_text <- paste0(i18n_r()$t("Flow Rate"), unit_display)

    current_value <- if (!is.null(input$flow)) input$flow else defaults$flow
    is_disabled <- solvent_selected()

    flow_input_div <- tags$div(
      tags$label(label_text,
        input_help(i18n_r()$t("Volumetric or mass flow rate of the extraction solvent. Not needed if you provide a Solvent Data column in the Input Data section."),
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

  # Dynamic UI for flow_units selectInput - all options, disabled when solvent is selected
  output$flow_units_ui <- renderUI({
    is_disabled <- solvent_selected()
    
    choices_values <- c("mL/min", "g/min", "kg/h", "L/h")
    choices_labels <- c(
      i18n_r()$t("mL/min"), i18n_r()$t("g/min"), i18n_r()$t("kg/h"),
      i18n_r()$t("L/h")
    )
    
    current_selected <- input$flow_units
    selected <- if (!is.null(current_selected) && current_selected %in% choices_values) {
      current_selected
    } else {
      defaults$flow_units %||% "g/min"
    }
    
    flow_units_div <- tags$div(
      selectInput(ns("flow_units"),
        tags$span(i18n$t("Flow Units"),
          input_help(i18n_r()$t("Units of the flow rate value. Choose mass-based (g/min, kg/h) if your flow meter measures mass, or volumetric (mL/min, L/h) if it measures volume."),
                     title = i18n_r()$t("Flow Units"), buttonLabel = i18n_r()$t("OK"))),
        choices = setNames(choices_values, choices_labels),
        selected = selected
      )
    )
    
    if (is_disabled) {
      shinyjs::disabled(flow_units_div)
    } else {
      flow_units_div
    }
  })
  outputOptions(output, "flow_units_ui", suspendWhenHidden = FALSE)

  # Render resp_units selectInput with i18n
  output$resp_units_ui <- renderUI({
    selectInput(ns("resp_units"),
      tags$span(i18n_r()$t("Response Units"),
        input_help(i18n_r()$t("Units of your yield/response data. Choose grams for absolute mass, percent or permille for yield relative to raw material mass, or ppm/ppb for trace concentrations."),
                   title = i18n_r()$t("Response Units"), buttonLabel = i18n_r()$t("OK"))),
      choices = setNames(
        c("g", "percent", "permille", "ppm", "ppb"),
        c(i18n_r()$t("grams"), "%", "\u2030", "ppm", "ppb")
      ),
      selected = isolate(input$resp_units) %||% defaults$resp_units
    )
  })
  outputOptions(output, "resp_units_ui", suspendWhenHidden = FALSE)

  output$pres_ui <- renderUI({
    numericInput(ns("pres"),
      tags$span(i18n_r()$t("Pressure (bar)"),
        input_help(i18n_r()$t("Extraction pressure in bar. Affects solvent density and extraction efficiency. For subcritical water extraction, typically 10-50 bar to keep water in liquid state above 100\u00B0C."),
                   title = i18n_r()$t("Pressure"), buttonLabel = i18n_r()$t("OK"))),
      value = isolate(input$pres) %||% defaults$pres, min = 0)
  })

  output$temp_ui <- renderUI({
    numericInput(ns("temp"),
      tags$span(i18n_r()$t("Temperature (\u00B0C)"),
        input_help(i18n_r()$t("Extraction temperature in degrees Celsius. Higher temperatures increase extraction rates but may cause thermal degradation of sensitive compounds."),
                   title = i18n_r()$t("Temperature"), buttonLabel = i18n_r()$t("OK"))),
      value = isolate(input$temp) %||% defaults$temp, min = 0)
  })

  # Render Maximum Yield input with dynamic range badge
  output$c0_ui <- renderUI({
    cur_val <- input$c0 %||% defaults$c0
    input_exists <- !is.null(input$c0)
    resp_units <- input$resp_units %||% defaults$resp_units %||% "g"
    
    # Determine max value based on units
    max_val <- switch(resp_units,
      "g" = input$m_in %||% defaults$m_in %||% 100,
      "percent" = 100,
      "permille" = 1000,
      "ppm" = 1e6,
      "ppb" = 1e9,
      100
    )
    
    # Format badge text elegantly
    max_text <- switch(resp_units,
      "g" = paste0(round(max_val, 1), " g"),
      "percent" = "100%",
      "permille" = paste0("1000\u2030"),
      "ppm" = "1M",
      "ppb" = "1B",
      as.character(max_val)
    )
    badge_text <- paste0("0\u2013", max_text)
    
    # Auto-correct with warning (only when input already exists, not on initial render)
    if (input_exists && !is.null(cur_val) && !is.na(cur_val) && (cur_val < 0 || cur_val > max_val)) {
      showNotification(i18n$t("Maximum Yield was adjusted to the valid range."), type = "warning")
      cur_val <- max(0, min(max_val, cur_val))
    }
    
    badge_color <- "#6c757d"
    
    tags$div(
      tags$label(
        i18n_r()$t("Maximum Yield"),
        input_help(i18n_r()$t("Maximum possible yield of target compounds from the raw material, in the same units as your response variable. This is the asymptotic value the extraction curve approaches."),
                   title = i18n_r()$t("Maximum Yield"), buttonLabel = i18n_r()$t("OK")),
        class = "control-label",
        style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
        tags$span(
          badge_text,
          style = paste0("font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: ",
            badge_color, "; color: white; margin-left: auto; font-weight: normal;")
        )
      ),
      numericInput(ns("c0"), label = NULL, value = cur_val, min = 0)
    )
  })

  # Auto-correct c0 when user enters out-of-range value
  observeEvent(input$c0, {
    resp_units <- input$resp_units %||% defaults$resp_units %||% "g"
    max_val <- switch(resp_units,
      "g" = input$m_in %||% defaults$m_in %||% 100,
      "percent" = 100,
      "permille" = 1000,
      "ppm" = 1e6,
      "ppb" = 1e9,
      100
    )
    val <- input$c0
    if (!is.null(val) && !is.na(val) && (val < 0 || val > max_val)) {
      showNotification(i18n$t("Maximum Yield was adjusted to the valid range."), type = "warning")
      updateNumericInput(session, "c0", value = max(0, min(max_val, val)))
    }
  }, ignoreInit = TRUE)

  # Render Mass of Raw Material with conditional range check (must be >= c0 when grams)
  output$m_in_ui <- renderUI({
    cur_val <- input$m_in %||% defaults$m_in
    resp_units <- input$resp_units %||% "g"
    c0_val <- input$c0 %||% defaults$c0 %||% 0
    min_val <- if (resp_units == "g") max(0, c0_val) else 0
    badge_text <- if (resp_units == "g" && c0_val > 0) paste0("\u2265", round(c0_val, 1)) else ">0"
    badge_color <- "#6c757d"
    if (!is.null(cur_val) && !is.na(cur_val) && cur_val < min_val) badge_color <- "#dc3545"
    tags$div(
      tags$label(
        i18n_r()$t("Mass of Raw Material (g)"),
        input_help(i18n_r()$t("Mass of raw material loaded into the extraction vessel (grams). This is used to calculate the solvent-to-material ratio and to convert between yield units."),
                   title = i18n_r()$t("Material Mass"), buttonLabel = i18n_r()$t("OK")),
        class = "control-label",
        style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
        tags$span(badge_text, style = paste0("font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: ", badge_color, "; color: white; margin-left: auto; font-weight: normal;"))
      ),
      numericInput(ns("m_in"), label = NULL, value = cur_val, min = 0)
    )
  })

  # Auto-correct m_in
  observeEvent(input$m_in, {
    resp_units <- input$resp_units %||% "g"
    if (resp_units == "g") {
      c0_val <- input$c0 %||% 0
      if (!is.null(input$m_in) && !is.na(input$m_in) && input$m_in < c0_val) {
        showNotification(i18n$t("Mass of Raw Material cannot be less than Maximum Yield when units are grams."), type = "warning")
        updateNumericInput(session, "m_in", value = c0_val)
      }
    }
    if (!is.null(input$m_in) && !is.na(input$m_in) && input$m_in < 0) {
      updateNumericInput(session, "m_in", value = 0)
    }
  }, ignoreInit = TRUE)

  # Render F (Fraction of Easily Desorbed Solute) with range check and clear button
  output$f_ui <- renderUI({
    cur_val <- isolate(input$f)
    is_definite <- f_is_definite()
    badge_color <- "#6c757d"
    if (!is.null(cur_val) && !is.na(cur_val) && (cur_val <= 0 || cur_val > 1)) badge_color <- "#dc3545"
    
    div(
      class = "form-group shiny-input-container",
      style = "width: 100%;",
      div(
        style = "display: flex; justify-content: space-between; align-items: center;",
        tags$label(
          class = "control-label",
          HTML(paste0(
            if (is_definite) i18n_r()$t("Definite") else i18n_r()$t("Initial"),
            " <i>F</i> ",
            if (is_definite) i18n_r()$t("Value") else i18n_r()$t("Estimate")
          )),
          input_help(i18n_r()$t("Fraction of easily desorbed solute (0 to 1). Represents the proportion of compounds quickly released during extraction. The remainder (1-F) is released more slowly from the plant matrix. Toggle the switch to set F as a fixed value or let the model estimate it."),
                     title = i18n_r()$t("F"), buttonLabel = i18n_r()$t("OK"))
        ),
        div(
          style = "display: flex; align-items: center; gap: 4px;",
          tags$span("0\u20131", style = paste0("font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: ", badge_color, "; color: white; font-weight: normal;")),
          actionButton(
            ns("toggle_f_mode"),
            label = NULL,
            icon = icon(if (is_definite) "toggle-on" else "toggle-off"),
            class = paste("btn btn-xs", if (is_definite) "btn-primary" else "btn-default"),
            style = "padding: 2px 6px; font-size: 14px;",
            title = if (is_definite) i18n_r()$t("Definite value (will not be estimated)") else i18n_r()$t("Initial estimate (will be optimized)")
          )
        )
      ),
      numericInput(ns("f"), label = NULL, value = if (is.null(cur_val) || is.na(cur_val)) defaults$f else cur_val, step = 0.01)
    )
  })

  # Track whether F is a definite value or an initial estimate
  f_is_definite <- reactiveVal(FALSE)
  
  observeEvent(input$toggle_f_mode, {
    f_is_definite(!f_is_definite())
  })

  # Auto-correct F
  observeEvent(input$f, {
    val <- input$f
    if (!is.null(val) && !is.na(val) && (val <= 0 || val > 1)) {
      showNotification(paste0("\u1D439 ", i18n$t("was adjusted to the valid range.")), type = "warning")
      updateNumericInput(session, "f", value = max(0.001, min(1, val)))
    }
  }, ignoreInit = TRUE)

  # Render k1 estimate
  output$k1_est_ui <- renderUI({
    cur <- isolate(input$k1_est) %||% defaults$k1_est
    if (is.null(cur) || length(cur) == 0) cur <- "0.1"
    tags$div(
      tags$label(
        HTML(paste0(i18n_r()$t("Starting Values of"), " &nbsp;<i>k</i><sub>1</sub>")),
        input_help(i18n_r()$t("Starting guess(es) for the fast desorption rate constant k\u2081. Multiple values enable multi-start optimization. Higher values mean faster initial extraction."),
                   title = i18n_r()$t("k\u2081"), buttonLabel = i18n_r()$t("OK")),
        tags$span(paste0(i18n_r()$t("max"), " 6"),
          style = "font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: #6c757d; color: white; margin-left: auto; font-weight: normal; flex-shrink: 0;",
          title = i18n_r()$t("Multiple starting values enable multi-start optimization for better convergence.")),
        class = "control-label",
        style = "display: flex; align-items: center; justify-content: space-between; width: 100%;"),
      selectizeInput(ns("k1_est"), label = NULL,
        choices = cur, selected = cur, multiple = TRUE,
        options = list(create = TRUE, maxItems = 6))
    )
  })

  # Render k2 estimate
  output$k2_est_ui <- renderUI({
    cur <- isolate(input$k2_est) %||% defaults$k2_est
    if (is.null(cur) || length(cur) == 0) cur <- "0.1"
    tags$div(
      tags$label(
        HTML(paste0(i18n_r()$t("Starting Values of"), " &nbsp;<i>k</i><sub>2</sub>")),
        input_help(i18n_r()$t("Starting guess(es) for the slow desorption rate constant k\u2082. Multiple values enable multi-start optimization. Typically much smaller than k\u2081."),
                   title = i18n_r()$t("k\u2082"), buttonLabel = i18n_r()$t("OK")),
        tags$span(paste0(i18n_r()$t("max"), " 6"),
          style = "font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: #6c757d; color: white; margin-left: auto; font-weight: normal; flex-shrink: 0;",
          title = i18n_r()$t("Multiple starting values enable multi-start optimization for better convergence.")),
        class = "control-label",
        style = "display: flex; align-items: center; justify-content: space-between; width: 100%;"),
      selectizeInput(ns("k2_est"), label = NULL,
        choices = cur, selected = cur, multiple = TRUE,
        options = list(create = TRUE, maxItems = 6))
    )
  })

  # Render F estimate with toggle for definite/estimate mode
  output$f_est_ui <- renderUI({
    is_definite <- f_is_definite()
    
    div(
      class = "form-group shiny-input-container",
      style = "width: 100%;",
      div(
        style = "display: flex; justify-content: space-between; align-items: center;",
        tags$label(
          class = "control-label",
          HTML(paste0(
            if (is_definite) i18n_r()$t("Definite") else i18n_r()$t("Initial"),
            " <i>F</i> ",
            if (is_definite) i18n_r()$t("Value") else i18n_r()$t("Estimate")
          ))
        ),
        div(
          style = "display: flex; align-items: center; gap: 4px;",
          tags$span("0\u20131", style = "font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: #6c757d; color: white; font-weight: normal;"),
          actionButton(
            ns("toggle_f_mode"),
            label = NULL,
            icon = icon(if (is_definite) "toggle-on" else "toggle-off"),
            class = paste("btn btn-xs", if (is_definite) "btn-primary" else "btn-default"),
            style = "padding: 2px 6px; font-size: 14px;",
            title = if (is_definite) i18n_r()$t("Definite value (will not be estimated)") else i18n_r()$t("Initial estimate (will be optimized)")
          )
        )
      ),
      textInput(ns("f_est"), label = NULL, value = isolate(input$f_est) %||% defaults$f_est)
    )
  })

  # Toggle reactiveVals for optional fields
  tmax_enabled <- reactiveVal(FALSE)
  qmax_enabled <- reactiveVal(FALSE)
  flowpar_pres_enabled <- reactiveVal(FALSE)
  flowpar_temp_enabled <- reactiveVal(FALSE)
  
  observeEvent(input$toggle_tmax, { tmax_enabled(!tmax_enabled()) })
  observeEvent(input$toggle_qmax, { qmax_enabled(!qmax_enabled()) })
  observeEvent(input$toggle_flowpar_pres, { flowpar_pres_enabled(!flowpar_pres_enabled()) })
  observeEvent(input$toggle_flowpar_temp, { flowpar_temp_enabled(!flowpar_temp_enabled()) })

  # Render Max Time with toggle
  output$tmax_ui <- renderUI({
    is_on <- tmax_enabled()
    
    # Try to get max experimental time
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
          input_help(i18n_r()$t("Upper time limit for model predictions (minutes). Leave empty to automatically use 120% of your longest experimental time point."),
                     title = i18n_r()$t("Max Time"), buttonLabel = i18n_r()$t("OK")),
          title = "Maximum extraction time for model predictions (minutes)"
        ),
        div(style = "display: flex; align-items: center; gap: 4px;",
          if (!is.null(badge_text)) tags$span(badge_text,
            title = paste0(i18n_r()$t("Max experimental value:"), " ", round(exp_max, 2), " min"),
            style = "font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: #6c757d; color: white; font-weight: normal; cursor: help;"),
          actionButton(ns("toggle_tmax"), label = NULL,
            icon = icon(if (is_on) "toggle-on" else "toggle-off"),
            class = paste("btn btn-xs", if (is_on) "btn-primary" else "btn-default"),
            style = "padding: 2px 6px; font-size: 14px;",
            title = "Enable custom value")
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

  # Render Max Q with toggle
  output$qmax_ui <- renderUI({
    is_on <- qmax_enabled()
    
    exp_max_q <- NULL
    tryCatch({
      data <- oec_data()
      slv_var <- input$oec_slv_var
      m_in <- input$m_in %||% defaults$m_in %||% 1
      use_solvent <- isTRUE(input$use_solvent_var)
      if (!is.null(data) && use_solvent && !is.null(slv_var) && slv_var %in% names(data) && m_in > 0) {
        slv_data <- data[[slv_var]]
        if (is.numeric(slv_data)) {
          max_slv <- max(cumsum(slv_data), na.rm = TRUE)
          exp_max_q <- max_slv / (m_in / 1000)
        }
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
          title = i18n_r()$t("Maximum solvent-to-material mass ratio for model predictions (kg/kg)")
        ),
        div(style = "display: flex; align-items: center; gap: 4px;",
          if (!is.null(badge_text)) tags$span(badge_text,
            title = paste0(i18n_r()$t("Max experimental value:"), " ", round(exp_max_q, 2)),
            style = "font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: #6c757d; color: white; font-weight: normal; cursor: help;"),
          actionButton(ns("toggle_qmax"), label = NULL,
            icon = icon(if (is_on) "toggle-on" else "toggle-off"),
            class = paste("btn btn-xs", if (is_on) "btn-primary" else "btn-default"),
            style = "padding: 2px 6px; font-size: 14px;",
            title = "Enable custom value")
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
      showNotification(paste0(i18n$t("Max"), " q ", i18n$t("must be greater than 0.")), type = "warning")
      updateNumericInput(session, "qmax", value = 1)
    }
  }, ignoreInit = TRUE)

  # Render Flow Measurement Pressure with toggle
  output$flowpar_pres_ui <- renderUI({
    is_on <- flowpar_pres_enabled()
    pres_val <- input$pres %||% defaults$pres
    
    div(
      class = "form-group shiny-input-container", style = "width: 100%;",
      div(
        style = "display: flex; justify-content: space-between; align-items: center;",
        tags$label(class = "control-label",
          i18n_r()$t("Flow Meas. Pressure (bar)"),
          input_help(i18n_r()$t("Pressure at which your flow meter measured the solvent flow rate. Only relevant when measured at conditions different from extraction."),
                     title = i18n_r()$t("Flow Measurement P"), buttonLabel = i18n_r()$t("OK")),
          title = "Pressure at which the flow rate was measured. Defaults to extraction pressure."
        ),
        actionButton(ns("toggle_flowpar_pres"), label = NULL,
          icon = icon(if (is_on) "toggle-on" else "toggle-off"),
          class = paste("btn btn-xs", if (is_on) "btn-primary" else "btn-default"),
          style = "padding: 2px 6px; font-size: 14px;",
          title = "Enable custom value")
      ),
      if (is_on) {
        numericInput(ns("flowpar_pres"), label = NULL, value = isolate(input$flowpar_pres) %||% defaults$flowpar_pres, min = 0)
      } else {
        div(style = "background-color: #f5f5f5; border: 1px solid #ddd; border-radius: 4px; padding: 6px 12px; color: #888; height: 34px; line-height: 22px;",
          paste0(i18n_r()$t("Same as extraction"), " (", round(pres_val, 1), " bar)"))
      }
    )
  })

  # Render Flow Measurement Temperature with toggle
  output$flowpar_temp_ui <- renderUI({
    is_on <- flowpar_temp_enabled()
    temp_val <- input$temp %||% defaults$temp
    
    div(
      class = "form-group shiny-input-container", style = "width: 100%;",
      div(
        style = "display: flex; justify-content: space-between; align-items: center;",
        tags$label(class = "control-label",
          i18n_r()$t("Flow Meas. Temp. (\u00B0C)"),
          input_help(i18n_r()$t("Temperature at which your flow meter measured the solvent flow rate. Only relevant when measured at conditions different from extraction."),
                     title = i18n_r()$t("Flow Measurement T"), buttonLabel = i18n_r()$t("OK")),
          title = "Temperature at which the flow rate was measured. Defaults to extraction temperature."
        ),
        actionButton(ns("toggle_flowpar_temp"), label = NULL,
          icon = icon(if (is_on) "toggle-on" else "toggle-off"),
          class = paste("btn btn-xs", if (is_on) "btn-primary" else "btn-default"),
          style = "padding: 2px 6px; font-size: 14px;",
          title = "Enable custom value")
      ),
      if (is_on) {
        numericInput(ns("flowpar_temp"), label = NULL, value = isolate(input$flowpar_temp) %||% defaults$flowpar_temp, min = 0)
      } else {
        div(style = "background-color: #f5f5f5; border: 1px solid #ddd; border-radius: 4px; padding: 6px 12px; color: #888; height: 34px; line-height: 22px;",
          paste0(i18n_r()$t("Same as extraction"), " (", round(temp_val, 1), " \u00B0C)"))
      }
    )
  })

  # Render Solvent Density - disabled when mass flow units are selected
  output$ro_h2o_ui <- renderUI({
    flow_unit <- input$flow_units %||% defaults$flow_units %||% "g/min"
    is_mass_flow <- grepl("^g/|^kg/", flow_unit)
    cur_val <- isolate(input$ro_h2o) %||% defaults$ro_h2o
    
    tooltip <- i18n_r()$t("Only needed for volumetric flow. Leave empty to auto-calculate from pressure and temperature (IAPWS-95).")
    
    tags$div(
      style = if (is_mass_flow) "opacity: 0.4; pointer-events: none;" else "",
      tags$label(
        i18n_r()$t("Solvent Density (g/L)"),
        input_help(i18n_r()$t("Density of water at your extraction conditions (g/L). Only needed when the flow rate is given in volumetric units (mL/min or L/h). Leave empty to calculate automatically from pressure and temperature."),
                   title = i18n_r()$t("Solvent Density"), buttonLabel = i18n_r()$t("OK")),
        class = "control-label",
        style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
        tags$span(
          if (is_mass_flow) i18n_r()$t("Not needed") else i18n_r()$t("Optional"),
          title = tooltip,
          style = "font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: #6c757d; color: white; margin-left: auto; font-weight: normal; cursor: help;"
        )
      ),
      numericInput(ns("ro_h2o"), label = NULL, value = if (is.null(cur_val) || is.na(cur_val)) NA else cur_val)
    )
  })

  # Render optmet selectInput with i18n
  output$optmet_ui <- renderUI({
    selectInput(ns("optmet"),
      tags$span(i18n_r()$t("Optimization Method"),
        input_help(i18n_r()$t("Method for fitting the model curve to your data. Global (recommended) searches broadly for the best fit using multiple starting points. Robust is slower but handles noisy data or outliers better."),
                   title = i18n_r()$t("Optimization Method"), buttonLabel = i18n_r()$t("OK"))),
      choices = setNames(
        c("nlopt", "nlrob"),
        c(i18n_r()$t("Non-Linear (Global)"), i18n_r()$t("Robust Non-Linear (Local)"))
      ),
      selected = isolate(input$optmet) %||% defaults$optmet
    )
  })
  outputOptions(output, "optmet_ui", suspendWhenHidden = FALSE)

  # Render nfits numericInput
  output$nfits_ui <- renderUI({
    numericInput(ns("nfits"),
      tags$span(i18n_r()$t("Number of Fits"),
        input_help(i18n_r()$t("Number of times to repeat the entire fitting process. When > 1, the best fit is used for the model, and fitted parameter standard deviations are shown in the summary."),
                   title = i18n_r()$t("Number of Fits"), buttonLabel = i18n_r()$t("OK"))),
      value = isolate(input$nfits) %||% defaults$nfits %||% 1,
      min = 1, max = 20, step = 1
    )
  })
  outputOptions(output, "nfits_ui", suspendWhenHidden = FALSE)

  # Render plot_x_units selectInput with i18n
  output$plot_x_units_ui <- renderUI({
    selectInput(ns("plot_x_units"),
      tags$span(i18n_r()$t("Plot X-axis Units"),
        input_help(i18n_r()$t("Choose what to show on the horizontal axis of the kinetic plot. Time shows extraction duration in minutes. S/M Ratio (q) shows cumulative solvent consumed per kilogram of insoluble raw material."),
                   title = i18n_r()$t("Plot X-axis"), buttonLabel = i18n_r()$t("OK"))),
      choices = setNames(
        c("time", "q"),
        c(i18n_r()$t("Time"), paste0(i18n_r()$t("S/M Ratio"), " (q)"))
      ),
      selected = isolate(input$plot_x_units) %||% defaults$plot_x_units
    )
  })
  outputOptions(output, "plot_x_units_ui", suspendWhenHidden = FALSE)

  # Render plot_y_units selectInput with i18n
  output$plot_y_units_ui <- renderUI({
    selectInput(ns("plot_y_units"),
      tags$span(i18n_r()$t("Plot Y-axis Units"),
        input_help(i18n_r()$t("Choose what to show on the vertical axis. Absolute Yield shows the extract amount in your response units. Fractional Yield (c/c0) shows the fraction of the maximum possible yield already collected (0 to 1)."),
                   title = i18n_r()$t("Plot Y-axis"), buttonLabel = i18n_r()$t("OK"))),
      choices = setNames(
        c("abs", "cc0"),
        c(i18n_r()$t("Absolute Yield"), paste0(i18n_r()$t("Fractional Yield"), " (c/c", "\u2080", ")"))
      ),
      selected = isolate(input$plot_y_units) %||% defaults$plot_y_units
    )
  })
  outputOptions(output, "plot_y_units_ui", suspendWhenHidden = FALSE)

  # Reactive value to store example data
  example_data <- reactiveValues(data = NULL)

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

  # Observer for loading example data
  observeEvent(input$load_example_data, {
    default_path <- system.file("extdata", "gui-kinetic_tws_oec-1.csv", package = "supeRcrit")
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

  # Download handler for current data
  output$download_current_data <- downloadHandler(
    filename = function() {
      paste0("tws_data_", format(Sys.time(), "%d-%m-%Y_%Hhrs-%Mmin-%Ssec"), ".csv")
    },
    content = function(file) {
      data <- oec_data()
      if (!is.null(data)) {
        write.csv(data, file, row.names = FALSE)
      }
    }
  )

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
    x_var_current <- input$oec_x_var
    y_var_current <- input$oec_y_var
    current_selection <- input$oec_slv_var
    
    # Filter out x_var and y_var from choices (only if they are actual columns, not "")
    used_cols <- c(x_var_current, y_var_current)
    used_cols <- used_cols[!is.null(used_cols) & used_cols != "" & used_cols %in% all_cols]
    choices <- setdiff(all_cols, used_cols)
    
    # Keep current selection if valid (including ""), otherwise default to first available
    selected <- if (!is.null(current_selection) && (current_selection == "" || current_selection %in% choices)) {
      current_selection
    } else if (length(choices) > 0) {
      choices[1]
    } else {
      ""
    }
    
    if (length(choices) == 0) {
      # No columns left - show disabled dropdown
      return(
        div(
          tags$label(i18n$t("Solvent"), class = "control-label"),
          tags$select(
            class = "form-control",
            disabled = "disabled",
            style = "background-color: #e9ecef;",
            tags$option(i18n$t("No columns available"))
          ),
          tags$small(class = "text-muted", i18n$t("All columns are in use. Add more columns to data."))
        )
      )
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
      selectInput(ns("oec_slv_var"), NULL, choices = c("None" = "", setNames(choices, choices)), selected = selected)
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
    
    # Check if same column is selected for Time and Response
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

    tryCatch(
      {
        # Show progress
        withProgress(message = i18n$t("Calculating kinetic model..."), value = 0, {
          incProgress(0.2, detail = i18n$t("Preparing parameters..."))

          # Prepare OEC variables, using input values or defaults
          x_var_val <- input$oec_x_var %||% default_oec_vars()$x_var
          y_var_val <- input$oec_y_var %||% default_oec_vars()$y_var
          
          # Only include solvent variable if use_solvent is checked
          use_solvent <- solvent_selected()
          slv_var_val <- if (use_solvent) input$oec_slv_var else NULL

          oec_vars_list <- c(x = x_var_val, y = y_var_val)
          if (!is.null(slv_var_val) && slv_var_val != "" && slv_var_val %in% colnames(data)) {
            oec_vars_list <- c(oec_vars_list, slv = slv_var_val)
          }

          # Prepare parameters for ktsmod
          # Get flow value - if solvent is provided, flow is not needed
          flow_val <- if (use_solvent && !is.null(slv_var_val)) {
            NA
          } else {
            flow_input <- get_input_or_default("flow")
            if (is.null(flow_input) || is.na(flow_input)) NA else flow_input
          }

          # Determine F value based on toggle mode
          f_val <- get_input_or_default("f")
          f_definite <- f_is_definite()
          
          pars_list <- c(
            pres = get_input_or_default("pres"),
            temp = get_input_or_default("temp"),
            m_in = get_input_or_default("m_in"),
            c0 = get_input_or_default("c0"),
            flow = flow_val,
            f = if (f_definite && !is.null(f_val) && !is.na(f_val)) f_val else NA
          )

          # Prepare units (use defaults if renderUI hasn't loaded yet)
          units_list <- c(
            flow = get_input_or_default("flow_units"),
            resp = get_input_or_default("resp_units")
          )

          # Prepare optimization estimates
          k1_starts_val <- as.numeric(get_input_or_default("k1_est"))
          k2_starts_val <- as.numeric(get_input_or_default("k2_est"))
          opt_est_list <- c(
            k1 = k1_starts_val[1],
            k2 = k2_starts_val[1]
          )
          if (!f_definite && !is.null(f_val) && !is.na(f_val)) {
            opt_est_list <- c(opt_est_list, f = as.numeric(f_val))
          }

          # Prepare plot units (use defaults if renderUI hasn't loaded yet)
          plot_units_list <- c(
            x = get_input_or_default("plot_x_units"),
            y = get_input_or_default("plot_y_units")
          )

          # Prepare flowpar - use NA when toggles are off (defaults to extraction P/T)
          flowpar_vec <- c(
            if (flowpar_pres_enabled()) get_input_or_default("flowpar_pres") else NA,
            if (flowpar_temp_enabled()) get_input_or_default("flowpar_temp") else NA
          )

          # tmax/qmax - use NA when toggles are off (defaults to 120% of max)
          tmax_val <- if (tmax_enabled()) get_input_or_default("tmax") else NA
          qmax_val <- if (qmax_enabled()) get_input_or_default("qmax") else NA



          incProgress(0.6, detail = i18n$t("Running model..."))
          # browser()

          # DEBUG: Print all parameters being sent to ktsmod
          cat("\n=== DEBUG: ktsmod() Parameters ===\n")
          cat("oec_vars:", paste(names(oec_vars_list), oec_vars_list, sep = "=", collapse = ", "), "\n")
          cat("pars:", paste(names(pars_list), pars_list, sep = "=", collapse = ", "), "\n")
          cat("units:", paste(names(units_list), units_list, sep = "=", collapse = ", "), "\n")
          cat("opt_est:", paste(names(opt_est_list), opt_est_list, sep = "=", collapse = ", "), "\n")
          cat("plot_units:", paste(names(plot_units_list), plot_units_list, sep = "=", collapse = ", "), "\n")
          cat("flowpar:", paste(flowpar_vec, collapse = ", "), "\n")
          cat("ro_h2o:", get_input_or_default("ro_h2o"), "\n")
          cat("tmax:", tmax_val, "\n")
          cat("qmax:", qmax_val, "\n")
          cat("cumulative:", get_input_or_default("cumulative"), "\n")
          cat("mass_flow:", if (solvent_selected()) isTRUE(get_input_or_default("mass_flow")) else grepl("^g/|^kg/", get_input_or_default("flow_units")), "\n")
          cat("optmet:", get_input_or_default("optmet"), "\n")
          cat("oec data dimensions:", nrow(validated_oec_data()), "x", ncol(validated_oec_data()), "\n")
          cat("===================================\n\n")

          # Call ktsmod function
          tryCatch(
            {
              model_result <- supeRcrit::ktsmod(
                oec = validated_oec_data(),
                oec_vars = oec_vars_list,
                pars = pars_list,
                units = units_list,
                opt_est = opt_est_list,
                # opt_est = "default",
                plot_units = plot_units_list,
                flowpar = flowpar_vec,
                ro_h2o = get_input_or_default("ro_h2o"),
                tmax = tmax_val,
                qmax = qmax_val,
                cumulative = get_input_or_default("cumulative"),
                mass_flow = if (solvent_selected()) isTRUE(get_input_or_default("mass_flow")) else grepl("^g/|^kg/", get_input_or_default("flow_units")),
                draw = FALSE, # Do not draw plot directly, capture it
                optmet = get_input_or_default("optmet"),
                k1_0 = k1_starts_val,
                k2_0 = k2_starts_val,
                nfits = as.integer(get_input_or_default("nfits") %||% 1)
              )
            },
            error = function(e) {
              cat("\n!!! ERROR in ktsmod() !!!\n")
              cat("Error message:", e$message, "\n")
              cat("Error class:", class(e), "\n")
              print(traceback())
              stop(e$message)
            }
          )

          # Store results
          full_model_result(model_result) # Store the full result
          # Snapshot the user's original flow input so the Predictions tab can
          # display it verbatim in the Model Validity card.
          flow_input_snapshot(list(
            value = flow_val,
            unit  = unname(units_list[["flow"]])
          ))
          kinetic_results$model_data <- model_result$data
          kinetic_results$model_summary <- model_result$tws # Store the full tws output
          kinetic_results$model_plot <- model_result$plots$tws
          kinetic_results$call <- model_result$call

          incProgress(1, detail = i18n$t("Completed!"))
        })

        showNotification(i18n$t("Kinetic model calculated successfully!"), type = "message")
      },
      error = function(e) {
        error_msg <- e$message
        # Check if it's an nlrob-related error
        optmet_val <- input$optmet %||% defaults$optmet
        if (optmet_val == "nlrob") {
          if (grepl("differing number of rows|no non-missing", error_msg, ignore.case = TRUE)) {
            error_msg <- paste0(
              i18n$t("The 'nlrob' optimization method failed to converge with your data. "),
              i18n$t("This often happens with limited data points (< 10 rows) or poor initial estimates. "),
              "\n\n",
              i18n$t("Recommendations:"),
              "\n• ", i18n$t("Switch to 'nlopt' optimization method (recommended)"),
              "\n• ", i18n$t("Provide more data points (at least 10-15 rows)"),
              "\n• ", i18n$t("Adjust initial parameter estimates (k1, k2, f)"),
              "\n\n",
              i18n$t("Original error: "), e$message
            )
          } else if (grepl("nlrob|robustbase", error_msg, ignore.case = TRUE)) {
            error_msg <- paste0(
              i18n$t("Error with nlrob optimization method. "),
              i18n$t("Try using 'nlopt' method instead, or check if 'robustbase' package is installed. "),
              i18n$t("Original error: "), e$message
            )
          }
        }
        showNotification(paste(i18n$t("Error calculating kinetic model:"), error_msg), type = "error", duration = NULL)
        # Clear results on error
        kinetic_results$model_data <- NULL
        kinetic_results$model_summary <- NULL
        kinetic_results$model_plot <- NULL
        kinetic_results$call <- NULL

        full_model_result(NULL)
        flow_input_snapshot(NULL)
      }
    )
  })

  # Calculate kinetic model
  # This duplicate block has been removed - calculation is now handled in the above observeEvent
  # Render model plot
  output$kinetic_plot <- renderPlotly({
    req(kinetic_results$model_plot)

    tryCatch(
      {
        plot_obj <- kinetic_results$model_plot

        # Check if it's already a plotly object
        if ("plotly" %in% class(plot_obj)) {
          return(plot_obj)
        }

        # If it's a ggplot, translate labels and then convert safely
        if ("ggplot" %in% class(plot_obj)) {
          # Build dynamic y-axis label based on actual settings
          resp_unit <- isolate(input$resp_units) %||% defaults$resp_units %||% "permille"
          plot_y <- isolate(input$plot_y_units) %||% defaults$plot_y_units %||% "abs"
          
          resp_unit_display <- switch(resp_unit,
            "g" = "g",
            "percent" = "%",
            "permille" = "\u2030",
            "ppm" = "ppm",
            "ppb" = "ppb",
            resp_unit
          )
          
          y_label <- if (plot_y == "cc0") "e (kg/kg)" else paste0("Yield (", resp_unit_display, ")")
          
          # Translate plot labels
          plot_obj <- translate_plot_labels(
            plot_obj,
            i18n_r(),
            title = "Two-site kinetic desorption model",
            x = if (isolate(input$plot_x_units) %||% "time" == "q") "S/M (kg/kg)" else "Time (min)",
            y = y_label
          )

          # Try ggplotly with error handling
          tryCatch(
            {
              ggplotly(plot_obj, tooltip = "all")
            },
            error = function(e1) {
              # If ggplotly fails, try with minimal tooltip
              tryCatch(
                {
                  ggplotly(plot_obj, tooltip = c("x", "y"))
                },
                error = function(e2) {
                  # If still fails, create a simple plotly plot
                  plot_ly() %>%
                    add_annotations(
                      text = paste(i18n_r()$t("Plot display error. Original ggplot available but cannot convert to plotly.")),
                      x = 0.5, y = 0.5,
                      showarrow = FALSE,
                      font = list(size = 14)
                    ) %>%
                    layout(
                      title = "Plot Conversion Error",
                      showlegend = FALSE,
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
                    )
                }
              )
            }
          )
        } else {
          # Unknown plot type, create error message
          plot_ly() %>%
            add_annotations(
              text = i18n_r()$t("Unsupported plot format"),
              x = 0.5, y = 0.5,
              showarrow = FALSE
            )
        }
      },
      error = function(e) {
        # Final fallback - create empty plot with error message
        plot_ly() %>%
          add_annotations(
            text = paste(i18n_r()$t("Error displaying plot:"), substr(e$message, 1, 100)),
            x = 0.5, y = 0.5,
            showarrow = FALSE,
            font = list(color = "red")
          ) %>%
          layout(title = "Plot Error")
      }
    )
  })

  # Render combined model parameters table (with "Fitted" column)
  output$model_parameters_table <- DT::renderDataTable({
    req(kinetic_results$model_summary)
    
    mod_pars <- kinetic_results$model_summary$mod_pars
    fit_pars <- kinetic_results$model_summary$fit_pars
    
    if (length(mod_pars) == 0) return(NULL)
    
    param_names <- names(mod_pars)

    param_mapping <- c(
      "pres" = "%%P%%", "temp" = "%%T%%", "flow" = "%%Q%%",
      "c0" = "%%c_0%%", "m_in" = "%%m_{in}%%", "m" = "%%m%%",
      "k1" = "%%k_1%%", "k2" = "%%k_2%%", "f" = "%%F%%"
    )

    descriptions <- vapply(param_names, function(p) {
      result <- switch(p,
        "pres" = i18n$t("Pressure (bar)"),
        "temp" = i18n$t("Temperature (\u00B0C)"),
        "flow" = i18n$t("Flow rate (kg/s)"),
        "c0" = i18n$t("Maximum Yield"),
        "m_in" = i18n$t("Mass of raw material loaded (g)"),
        "m" = i18n$t("Number of experimental observations"),
        "k1" = paste0(i18n$t("First-order rate constant for fast desorption"), " (", i18n$t("min"), "<sup>\u22121</sup>)"),
        "k2" = paste0(i18n$t("First-order rate constant for slow desorption"), " (", i18n$t("min"), "<sup>\u22121</sup>)"),
        "f" = i18n$t("Fraction of easily desorbed solute"),
        NULL
      )
      if (is.null(result)) result <- p
      result
    }, character(1), USE.NAMES = FALSE)

    display_names <- vapply(param_names, function(p) {
      if (p %in% names(param_mapping)) param_mapping[[p]] else p
    }, character(1), USE.NAMES = FALSE)

    fitted_col <- vapply(param_names, function(p) {
      if (p %in% fit_pars) i18n_r()$t("Yes") else ""
    }, character(1), USE.NAMES = FALSE)

    # Get SD from fit_summary if available (nfits > 1)
    fit_summary <- kinetic_results$model_summary$fit_summary
    has_sd <- !is.null(fit_summary) && nrow(fit_summary) > 1

    sd_col <- vapply(param_names, function(p) {
      if (!has_sd || !(p %in% fit_summary$Parameter)) return("")
      sd_val <- fit_summary$SD[fit_summary$Parameter == p]
      if (is.na(sd_val) || sd_val == 0) return("")
      sprintf("%.6f", sd_val)
    }, character(1), USE.NAMES = FALSE)

    if (has_sd && any(sd_col != "")) {
      add_prettynames <- c(
        i18n_r()$t("Parameter"), i18n_r()$t("Value"), i18n_r()$t("SD"),
        i18n_r()$t("Fitted"), i18n_r()$t("Description")
      )

      tbl_df <- data.frame(
        Parameter = display_names,
        Value = sprintf("%.6f", as.numeric(mod_pars)),
        SD = sd_col,
        Fitted = fitted_col,
        Description = descriptions,
        stringsAsFactors = FALSE
      )
      center_targets <- c(1, 2, 3)
    } else {
      add_prettynames <- c(
        i18n_r()$t("Parameter"), i18n_r()$t("Value"),
        i18n_r()$t("Fitted"), i18n_r()$t("Description")
      )

      tbl_df <- data.frame(
        Parameter = display_names,
        Value = sprintf("%.6f", as.numeric(mod_pars)),
        Fitted = fitted_col,
        Description = descriptions,
        stringsAsFactors = FALSE
      )
      center_targets <- c(1, 2)
    }

    tbl_df %>%
      DT::datatable(
        colnames = add_prettynames,
        options = list(
          pageLength = 25, dom = "t", ordering = FALSE,
          language = tablang(),
          columnDefs = list(list(className = "dt-center", targets = center_targets))
        ),
        rownames = FALSE, escape = FALSE
      )
  })

  # Render model statistics as value boxes
  output$stat_aard_vb <- renderValueBox({
    req(kinetic_results$model_summary)
    aard <- kinetic_results$model_summary$resid[["aard"]]
    color <- if (!is.null(aard) && aard < 5) "green" else if (!is.null(aard) && aard < 10) "yellow" else "red"
    valueBox(
      paste0(round(aard, 2), "%"), i18n$t("AARD"),
      icon = icon("percent"), color = color
    )
  })

  output$stat_rmse_vb <- renderValueBox({
    req(kinetic_results$model_summary)
    rmse <- kinetic_results$model_summary$resid[["rmse"]]
    valueBox(
      round(rmse, 4), i18n$t("RMSE"),
      icon = icon("chart-line"), color = "blue"
    )
  })

  output$stat_r2_vb <- renderValueBox({
    req(kinetic_results$model_summary)
    r2 <- kinetic_results$model_summary$resid[["r2"]]
    color <- if (!is.null(r2) && r2 > 0.99) "green" else if (!is.null(r2) && r2 > 0.95) "yellow" else "red"
    valueBox(
      round(r2, 6), HTML("R\u00B2"),
      icon = icon("bullseye"), color = color
    )
  })

  # Render observed vs predicted data table
  output$observed_predicted_table <- DT::renderDataTable(
    {
      req(kinetic_results$model_summary)

      data_obs <- kinetic_results$model_summary$ordt
      # The S/M column is only present when plot_x_units != "q"; build headers conditionally.
      has_sm_obs <- "q" %in% colnames(data_obs)

      # Tooltip'li column isimleri
      add_prettynames_html <- c(
        sprintf(
          "<span title='%s'>%s</span>",
          i18n_r()$t("Extraction time (min)"),
          i18n_r()$t("Time")
        ),
        if (has_sm_obs) sprintf(
          "<span title='%s'>%s</span>",
          i18n_r()$t("Solvent to material ratio (kg/kg)"),
          i18n_r()$t("S/M")
        ),
        i18n_r()$t("Yield"),
        sprintf(
          "<span title='%s'>%s</span>",
          i18n_r()$t("Fractional yield relative to maximum extractable solute"),
          i18n_r()$t("c/c₀")
        ),
        sprintf(
          "<span title='%s'>%s</span>",
          i18n_r()$t("Predicted yield value"),
          i18n_r()$t("Pred. Yield")
        ),
        sprintf(
          "<span title='%s'>%s</span>",
          i18n_r()$t("Predicted fractional yield value"),
          i18n_r()$t("Pred. c/c₀")
        )
      )

      # All numeric columns: render at up to 3 dp with trailing zeroes stripped.
      num_idx_obs <- which(sapply(data_obs, is.numeric)) - 1L

      dt_output <- DT::datatable(
        data_obs,
        extensions = "Buttons",
        colnames = add_prettynames_html,
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          dom = "Bfrtip",
          language = tablang(),
          columnDefs = trim_zeros_columndefs(num_idx_obs, digits = 3),
          buttons = create_dt_export_buttons(i18n_r, "supercrit_kinetic_observed_predicted_data")
        ),
        rownames = FALSE,
        escape = FALSE
      )
      dt_output
    },
    server = FALSE,
    escape = FALSE
  )

  # Render model data table
  output$model_data_table <- DT::renderDataTable(
    {
      req(kinetic_results$model_summary)

      dt_data <- kinetic_results$model_summary$mdt
      # Drop the "model" column: it is always "tws" in this module and adds no information.
      dt_data$model <- NULL

      # Tooltip'li column isimleri (model column dropped). The S/M column is only
      # present when plot_x_units != "q"; build headers conditionally to match.
      has_sm <- "q" %in% colnames(dt_data)
      add_prettynames_html <- c(
        sprintf(
          "<span title='%s'>%s</span>",
          i18n_r()$t("Extraction time (min)"),
          i18n_r()$t("Time")
        ),
        if (has_sm) sprintf(
          "<span title='%s'>%s</span>",
          i18n_r()$t("Solvent to material ratio (kg/kg)"),
          i18n_r()$t("S/M")
        ),
        i18n_r()$t("Yield"),
        sprintf(
          "<span title='%s'>%s</span>",
          i18n_r()$t("Fractional yield relative to maximum extractable solute"),
          i18n_r()$t("c/c₀")
        )
      )

      # All numeric columns: render at up to 3 dp with trailing zeroes stripped.
      num_idx_mdt <- which(sapply(dt_data, is.numeric)) - 1L

      dt_output <- DT::datatable(
        dt_data,
        extensions = "Buttons",
        colnames = add_prettynames_html,
        options = list(
          scrollX = TRUE,
          pageLength = 25,
          language = tablang(),
          dom = "Bfrtip",
          columnDefs = trim_zeros_columndefs(num_idx_mdt, digits = 3),
          buttons = create_dt_export_buttons(i18n_r, "supercrit_kinetic_model_data")
        ),
        rownames = FALSE,
        escape = FALSE
      )
      dt_output
    },
    server = FALSE,
    escape = FALSE
  )

  # Reactive for storing the full ktsmod result for predictions
  full_model_result <- reactiveVal(NULL)

  # Snapshot of the user's original flow input (value + unit) at model-run time.
  # This is needed because ktsmod returns flow as `qaver` in kg/s -- a derived
  # average that depends on the time grid -- which is NOT equal to the original
  # input flow rate. Showing the original input avoids both the unit-label bug
  # and the qaver vs constant-flow discrepancy.
  flow_input_snapshot <- reactiveVal(NULL)



  # Reactive values for storing prediction results
  prediction_results <- reactiveValues(
    predictions_df = NULL,
    description_text = NULL
  )

  output$has_prediction_results <- reactive({
    !is.null(prediction_results$predictions_df)
  })
  outputOptions(output, "has_prediction_results", suspendWhenHidden = FALSE)

  # Dynamic UI for prediction data input
  # Helper to get experimental time range
  exp_time_range <- reactive({
    if (!is.null(kinetic_results$model_summary) && !is.null(kinetic_results$model_summary$ordt)) {
      range(kinetic_results$model_summary$ordt$x, na.rm = TRUE)
    } else {
      NULL
    }
  })

  output$predict_data_input_ui <- renderUI({
    input_type <- input$predict_input_type
    
    if (input_type == "csv") {
      fileInput(ns("predict_file_upload"),
        i18n_r()$t("Upload CSV (single column)"),
        accept = c("text/csv", ".csv"),
        buttonLabel = i18n_r()$t("Browse"),
        placeholder = i18n_r()$t("No file selected")
      )
    } else if (input_type == "sequence") {
      # Sequence input: from, to, step
      t_range <- exp_time_range()
      badge_text <- if (!is.null(t_range)) paste0(round(t_range[1], 1), "\u2013", round(t_range[2], 1), " ", i18n$t("min")) else ""
      
      tagList(
        tags$label(
          class = "control-label",
          style = "display: flex; align-items: center; gap: 8px; width: 100%;",
          tags$span(i18n_r()$t("Extraction Time (min) Sequence")),
          if (nchar(badge_text) > 0) tags$span(
            badge_text,
            style = "font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: #6c757d; color: white; font-weight: normal;",
            title = paste0(i18n_r()$t("Experimental range:"), " ", badge_text)
          )
        ),
        div(
          style = "display: flex; gap: 8px; align-items: flex-end;",
          div(style = "flex: 1;", numericInput(ns("seq_from"), i18n_r()$t("From"), value = if (!is.null(t_range)) round(t_range[1], 1) else 0, min = 0, step = 1)),
          div(style = "flex: 1;", numericInput(ns("seq_to"), i18n_r()$t("To"), value = if (!is.null(t_range)) round(t_range[2], 1) else 120, min = 0, step = 1)),
          div(style = "flex: 1;", numericInput(ns("seq_step"), i18n_r()$t("Step"), value = if (!is.null(t_range)) round((t_range[2] - t_range[1]) / 10, 1) else 10, min = 0.1, step = 1))
        )
      )
    } else {
      # Manual tag input
      t_range <- exp_time_range()
      badge_text <- if (!is.null(t_range)) paste0(round(t_range[1], 1), "\u2013", round(t_range[2], 1), " ", i18n$t("min")) else ""
      
      current_vals <- isolate(input$predict_manual_tags)
      
      # Build JS renderer for out-of-range highlighting
      range_js <- if (!is.null(t_range)) {
        sprintf("var tMin = %s; var tMax = %s; if (!isNaN(val) && (val < tMin || val > tMax)) isOutOfRange = true;", t_range[1], t_range[2])
      } else ""
      
      tagList(
        tags$label(
          i18n_r()$t("Extraction Times (min)"),
          class = "control-label",
          style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
          if (nchar(badge_text) > 0) tags$span(
            badge_text,
            style = "font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: #6c757d; color: white; margin-left: auto; font-weight: normal;",
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
            placeholder = i18n_r()$t("Type time values and press Enter..."),
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

  # Parse prediction input data
  predict_input_data <- reactive({
    req(input$predict_input_type)

    data <- NULL

    if (input$predict_input_type == "csv") {
      if (is.null(input$predict_file_upload)) {
        return(NULL)
      }
      tryCatch(
        {
          data <- read.csv(input$predict_file_upload$datapath,
            header = FALSE
          )[, 1]
          data <- as.numeric(data)
          data <- data[!is.na(data)]
        },
        error = function(e) {
          showNotification(paste(
            i18n$t("Error loading CSV:"),
            e$message
          ), type = "error")
          return(NULL)
        }
      )
    } else if (input$predict_input_type == "sequence") {
      req(input$seq_from, input$seq_to, input$seq_step)
      from <- input$seq_from
      to <- input$seq_to
      step <- input$seq_step
      if (is.na(from) || is.na(to) || is.na(step) || step <= 0 || from >= to) {
        return(NULL)
      }
      data <- seq(from, to, by = step)
    } else {
      # Tag-style input: values are in input$predict_manual_tags as character vector
      if (is.null(input$predict_manual_tags) || length(input$predict_manual_tags) == 0) {
        return(NULL)
      }
      data <- as.numeric(input$predict_manual_tags)
      data <- data[!is.na(data)]
    }

    data
  })

  # Calculate Yields checkbox with a translated native browser tooltip.
  # Must be rendered server-side (inside an active session) so i18n_r()$t()
  # returns plain text rather than a shiny.i18n <span ...> wrapper, which
  # would otherwise be HTML-escaped into the title attribute and shown
  # verbatim by the browser. Matches the working pattern used in the
  # Desirability module.
  output$calculate_yields_ui <- renderUI({
    cur <- isolate(input$prediction_get_yields)
    if (is.null(cur)) cur <- TRUE
    tags$div(
      title = "Convert the predicted fractional yield (c/c0) into mass (g) and percentage (%) yield using the raw material mass and response units from the model.",
      style = "display: inline-block;",
      checkboxInput(ns("prediction_get_yields"), i18n_r()$t("Calculate Yields"), value = cur)
    )
  })

  # Moisture Content UI with green "optional" badge and a static 0-100 range hint.
  # Out-of-range values are auto-corrected (see observer below) with a notification,
  # so no dynamic red badge or inline warning panel is needed.
  output$prediction_moisture_ui <- renderUI({
    cur <- if (!is.null(input$prediction_moisture)) input$prediction_moisture else NA
    tags$div(
      tags$label(
        class = "control-label",
        style = "display: flex; align-items: center; gap: 6px; width: 100%;",
        tags$span(i18n_r()$t("Moisture Content (%)")),
        tags$span(
          "0\u2013100",
          style = "font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: #6c757d; color: white; font-weight: normal; margin-left: auto;",
          title = paste0(i18n_r()$t("Valid range:"), " 0\u2013100%")
        ),
        tags$span(
          i18n_r()$t("optional"),
          style = "font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: #28a745; color: white; font-weight: normal;",
          title = i18n_r()$t("Moisture content of the raw material (% fresh weight). Used to correct percentage yield predictions from wet to dry weight basis. Leave empty if not applicable.")
        )
      ),
      numericInput(ns("prediction_moisture"), label = NULL, value = cur, min = 0, max = 100, step = 0.1)
    )
  })

  # Auto-correct moisture when user enters out-of-range value
  observeEvent(input$prediction_moisture, {
    val <- input$prediction_moisture
    if (!is.null(val) && !is.na(val) && (val < 0 || val > 100)) {
      showNotification(i18n$t("Moisture Content was adjusted to the valid range (0\u2013100%)."), type = "warning")
      updateNumericInput(session, "prediction_moisture", value = max(0, min(100, val)))
    }
  }, ignoreInit = TRUE)

  # Range warning for prediction time values
  output$predict_range_warning <- renderUI({
    req(input$predict_input_type)
    
    vals <- predict_input_data()
    if (is.null(vals) || length(vals) == 0) return(NULL)
    
    # Check for negative values
    if (any(vals < 0)) {
      return(div(
        style = "background-color: #fff3cd; border: 1px solid #ffc107; border-radius: 4px; padding: 8px; margin-top: 5px; margin-bottom: 10px;",
        icon("exclamation-triangle", style = "color: #856404; margin-right: 6px;"),
        tags$span(style = "color: #856404;",
          i18n$t("Negative time values detected. These will be ignored.")
        )
      ))
    }
    
    # Check if values are outside experimental range
    t_range <- exp_time_range()
    if (!is.null(t_range)) {
      if (any(vals < t_range[1] | vals > t_range[2])) {
        return(div(
          style = "background-color: #fff3cd; border: 1px solid #ffc107; border-radius: 4px; padding: 8px; margin-top: 5px; margin-bottom: 10px;",
          icon("exclamation-triangle", style = "color: #856404; margin-right: 6px;"),
          tags$span(style = "color: #856404;",
            i18n$t("One or more time values are outside the experimental range. Predictions may be unreliable (extrapolation).")
          )
        ))
      }
    }
    NULL
  })

  # Reset predictions
  observeEvent(input$pred_reset_btn, {
    prediction_results$predictions_df <- NULL
    prediction_results$description_text <- NULL
    updateRadioButtons(session, "predict_input_type", selected = "manual")
    updateSelectizeInput(session, "predict_manual_tags", selected = character(0))
    # Sequence inputs will auto-reset via renderUI when model data exists
    updateCheckboxInput(session, "prediction_get_yields", value = TRUE)
    updateNumericInput(session, "prediction_moisture", value = NA)
    showNotification(i18n$t("Predictions reset."), type = "message")
  })

  # Observe event for generating predictions
  observeEvent(input$calculate_predictions, {
    print("=== PREDICTION CALCULATE BUTTON CLICKED ===")
    print(paste("Full model result exists:", !is.null(full_model_result())))

    req(full_model_result()) # Ensure a model has been calculated

    print(paste("Predict input data:", predict_input_data()))
    print(paste("Is NULL:", is.null(predict_input_data())))
    print(paste("Length:", length(predict_input_data())))

    # Validate prediction data is not empty
    if (is.null(predict_input_data()) || length(predict_input_data()) == 0) {
      print("=== SHOWING WARNING: No prediction data ===")
      showNotification(i18n$t("Please enter prediction data."), type = "warning", session = session)
      return()
    }

    print("=== VALIDATION PASSED, PROCEEDING ===")

    # Call predict_kts function
    tryCatch(
      {
        withProgress(message = i18n$t("Generating predictions..."), value = 0, {
          incProgress(0.5)
          preds <- supeRcrit::predict_kts(
            input = full_model_result(),
            newdata = predict_input_data(),
            get_yields = input$prediction_get_yields,
            moisture = input$prediction_moisture
          )
          prediction_results$predictions_df <- preds$predictions
          prediction_results$description_text <- preds$description
          incProgress(1)
        })
        showNotification(i18n$t("Predictions generated successfully!"), type = "message")
      },
      error = function(e) {
        showNotification(paste(i18n$t("Error generating predictions:"), e$message), type = "error")
        prediction_results$predictions_df <- NULL
        prediction_results$description_text <- NULL
      }
    )
  })

  # Render prediction results table
  output$prediction_results_table <- DT::renderDataTable(
    {
      req(prediction_results$predictions_df)

      # Column isim çevirileri için mapping
      col_names_short <- c(
        "t" = i18n_r()$t("Time"),
        "sm" = i18n_r()$t("S/M"),
        "yield_permille" = i18n_r()$t("Yield (‰)"),
        "yield_cc0" = i18n_r()$t("c/c₀"),
        "yield_g" = i18n_r()$t("Yield (g)"),
        "yield_percent" = i18n_r()$t("Yield (%)")
      )

      col_names_full <- c(
        "t" = i18n_r()$t("Extraction time (min)"),
        "sm" = i18n_r()$t("Solvent to material ratio (kg/kg)"),
        "yield_permille" = i18n_r()$t("Yield value in permille"),
        "yield_cc0" = i18n_r()$t("Fractional yield relative to maximum extractable solute"),
        "yield_g" = i18n_r()$t("Yield value in grams"),
        "yield_percent" = i18n_r()$t("Yield value in percentage")
      )

      # Mevcut kolonlar için tooltip'li başlıklar oluştur
      current_cols <- colnames(prediction_results$predictions_df)
      add_prettynames_html <- sapply(current_cols, function(col) {
        if (col %in% names(col_names_short)) {
          sprintf(
            "<span title='%s'>%s</span>",
            col_names_full[[col]],
            col_names_short[[col]]
          )
        } else {
          col # Bilinmeyen kolonlar olduğu gibi kalır
        }
      }, USE.NAMES = FALSE)

      # All columns get the trim-zeros render (Time as well — integer when whole,
      # fractional values stripped of trailing zeroes), at up to 3 dp.
      pred_df <- prediction_results$predictions_df
      num_idx_pred <- which(sapply(pred_df, is.numeric)) - 1L

      dt_output <- DT::datatable(
        pred_df,
        extensions = "Buttons",
        colnames = add_prettynames_html,
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          language = tablang(),
          dom = "Bfrtip",
          columnDefs = trim_zeros_columndefs(num_idx_pred, digits = 3),
          buttons = create_dt_export_buttons(i18n_r, "supercrit_kinetic_predictions")
        ),
        rownames = FALSE,
        escape = FALSE
      )
      dt_output
    },
    server = FALSE,
    escape = FALSE
  )

  # Render prediction description
  output$prediction_description <- renderUI({
    req(prediction_results$description_text)
    req(full_model_result())
    
    fmr <- full_model_result()
    inpars <- fmr$input
    units <- fmr$units
    mod_pars <- fmr$tws$mod_pars
    
    # Build properly formatted description. Use i18n_r()$t() (not i18n$t()) so
    # this renderUI takes a reactive dependency on the active language and
    # re-fires whenever the user switches languages.
    flow_unit <- units["flow"]
    resp_unit <- units["response"]

    # Map the raw response-unit selectInput key (e.g. "permille") to the
    # symbol the user actually sees in the Response Units dropdown.
    resp_display <- switch(resp_unit,
      "g" = i18n_r()$t("grams"),
      "percent" = "%",
      "permille" = "\u2030",
      "ppm" = "ppm",
      "ppb" = "ppb",
      resp_unit
    )

    # Show the user's original flow input verbatim as the primary value (from
    # the snapshot captured at model-run time). For volumetric inputs (mL/min,
    # L/h) the model's internally-computed mass flow is shown in brackets so
    # the user can see the equivalent in g/min. For mass-flow inputs the primary
    # IS the mass flow already, so no counterpart is shown.
    #
    # Note: inpars[["flow"]] is ktsmod's `qaver` = mean(mass_slv/(t*60)) with
    # qser[1] zeroed at line 200 of ktsmod.R. For constant input flow and n
    # time points this underestimates the true rate by a factor of (n-1)/n,
    # so we never use it as the primary display value -- only as the
    # mass-equivalent counterpart for volumetric inputs.
    flow_text <- NULL
    snap <- flow_input_snapshot()
    if (!is.null(snap) && !is.null(snap$value) && !is.na(snap$value) && !is.null(snap$unit)) {
      primary_str <- paste0("<strong>", round(snap$value, 2), " ", i18n_r()$t(snap$unit), "</strong>")
      counterpart <- NULL
      if (snap$unit %in% c("mL/min", "L/h") &&
          !is.null(inpars[["flow"]]) && !is.na(inpars[["flow"]])) {
        flow_g_per_min <- inpars[["flow"]] * 1000 * 60
        counterpart <- paste0("<strong>", round(flow_g_per_min, 2), " ", i18n_r()$t("g/min"), "</strong>")
      }
      flow_text <- if (!is.null(counterpart)) paste0(primary_str, " (", counterpart, ")") else primary_str
    } else if (!is.null(inpars[["flow"]]) && !is.na(inpars[["flow"]])) {
      # Fallback for any cached/older model result without the snapshot.
      flow_text <- paste0("<strong>", round(inpars[["flow"]] * 1000 * 60, 2), " ", i18n_r()$t("g/min"), "</strong>")
    }
    
    c0_val <- round(mod_pars[["c0"]], 2)

    # Build the statement with bold values
    lines <- c(
      paste0(i18n_r()$t("Predictions are valid for the following process parameters:"))
    )
    if (!is.null(inpars[["pres"]])) lines <- c(lines, paste0(i18n_r()$t("Pressure"), " ", i18n_r()$t("of"), " <strong>", inpars[["pres"]], " ", i18n_r()$t("bar"), "</strong>."))
    if (!is.null(inpars[["temp"]])) lines <- c(lines, paste0(i18n_r()$t("Temperature"), " ", i18n_r()$t("of"), " <strong>", inpars[["temp"]], " ", i18n_r()$t("\u00b0C"), "</strong>."))
    if (!is.null(flow_text)) lines <- c(lines, paste0(i18n_r()$t("Flow rate"), " ", i18n_r()$t("of"), " ", flow_text, "."))
    lines <- c(lines, paste0(i18n_r()$t("Maximum yield"), " (c\u2080) ", i18n_r()$t("of"), " <strong>", c0_val, " ", resp_display, "</strong>."))

    lines <- c(lines, paste0(
      i18n_r()$t("The units of flow rate and response are"),
      " <strong>", i18n_r()$t(flow_unit), "</strong> ", i18n_r()$t("and"), " <strong>", resp_display, "</strong>, ", i18n_r()$t("respectively"), "."
    ))

    get_yields <- input$prediction_get_yields
    moisture <- input$prediction_moisture
    if (!is.null(get_yields) && get_yields) {
      basis <- if (is.na(moisture)) i18n_r()$t("wet") else i18n_r()$t("dry")
      lines <- c(lines, paste0(
        i18n_r()$t("The calculated percentage yield was calculated on a"),
        " <strong>", basis, "</strong> ", i18n_r()$t("weight basis"), "."
      ))
    }
    
    HTML(paste0("<p>", paste(lines, collapse = "<br/>"), "</p>"))
  })

  # Reset function
  observeEvent(input$reset, {
    # Reset all inputs to default values
    updateRadioButtons(session, "input_type", selected = defaults$input_type)
    # Reset file upload input to allow reloading default data
    shinyjs::reset(ns("file_upload"))
    
    # Clear data reactives
    example_data$data <- NULL
    edited_preview_data$data <- NULL
    
    # Clear variable selection wrapper borders
    wrapper_ids <- c(ns("oec_x_var_ui_wrapper"), ns("oec_y_var_ui_wrapper"), ns("oec_slv_var_ui_wrapper"))
    for (wrapper_id in wrapper_ids) {
      shinyjs::runjs(sprintf(
        "$('#%s').css({'border': '', 'border-radius': '', 'padding': ''});",
        wrapper_id
      ))
    }
    
    # The dynamic selectInputs will reset automatically when oec_data() becomes NULL
    # or when new data is loaded.
    updateNumericInput(session, "pres", value = defaults$pres)
    updateNumericInput(session, "temp", value = defaults$temp)
    updateNumericInput(session, "m_in", value = defaults$m_in)
    updateNumericInput(session, "c0", value = defaults$c0)
    updateNumericInput(session, "flow", value = defaults$flow)
    updateNumericInput(session, "f", value = defaults$f)
    updateCheckboxInput(session, "cumulative", value = defaults$cumulative)
    updateCheckboxInput(session, "use_solvent", value = FALSE)
    updateSelectizeInput(session, "k1_est", choices = defaults$k1_est, selected = defaults$k1_est)
    updateSelectizeInput(session, "k2_est", choices = defaults$k2_est, selected = defaults$k2_est)
    updateTextInput(session, "f_est", value = defaults$f_est)
    updateSelectInput(session, "optmet", selected = defaults$optmet)
    updateNumericInput(session, "nfits", value = defaults$nfits %||% 1)
    updateSelectInput(session, "flow_units", selected = defaults$flow_units)
    updateSelectInput(session, "resp_units", selected = defaults$resp_units)
    updateSelectInput(session, "plot_x_units", selected = defaults$plot_x_units)
    updateSelectInput(session, "plot_y_units", selected = defaults$plot_y_units)
    updateNumericInput(session, "flowpar_temp", value = NA)
    updateNumericInput(session, "flowpar_pres", value = NA)
    updateNumericInput(session, "ro_h2o", value = NA)
    updateNumericInput(session, "tmax", value = NA)
    updateNumericInput(session, "qmax", value = NA)

    # Reset prediction inputs
    updateTextInput(session, "prediction_times", value = defaults$prediction_times)
    updateCheckboxInput(session, "prediction_get_yields", value = defaults$prediction_get_yields)
    updateNumericInput(session, "prediction_moisture", value = NA)

    # Clear results
    kinetic_results$model_data <- NULL
    kinetic_results$model_summary <- NULL
    kinetic_results$model_plot <- NULL
    full_model_result(NULL) # Clear the full model result
    flow_input_snapshot(NULL)
    prediction_results$predictions_df <- NULL
    prediction_results$description_text <- NULL

    showNotification(i18n$t("Parameters reset"), type = "message")
  })

  # Download Default CSV Template
  output$download_default_csv <- downloadHandler(
    filename = function() {
      "kinetic_tws_default.csv"
    },
    content = function(file) {
      default_path <- system.file("extdata", "gui-kinetic_tws_oec-1.csv", package = "supeRcrit")
      if (file.exists(default_path)) {
        file.copy(default_path, file)
      } else {
        showNotification(i18n$t("Default template file not found."), type = "error")
      }
    },
    contentType = "text/csv"
  )

  # Export kinetic model results
  output$export_kinetic <- downloadHandler(
    filename = function() {
      # Generate filename with timestamp
      timestamp <- format(Sys.time(), "%d-%m-%Y_%Hhrs-%Mmin-%Ssec")
      paste0("supercrit_kinetic_tws_export_", timestamp, ".zip")
    },
    content = function(file) {
      req(full_model_result())

      tryCatch(
        {
          # Create temporary directory
          temp_dir <- tempdir()

          # Call kin_export with temporary directory (only modres for individual models)
          kin_export(
            modres = full_model_result(),
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
  # Help button for units
  observeEvent(input$show_units, {
    showModal(modalDialog(
      title = i18n$t("Two-Site Kinetic Model Parameter Units"),
      DT::dataTableOutput(ns("units_table")),
      easyClose = TRUE,
      size = "l",
      footer = tagList(
        modalButton(i18n$t("Close"))
      )
    ))
  })

  output$units_table <- DT::renderDataTable(
    {
      # Column isimleri
      add_prettynames <- c(
        i18n_r()$t("Type"),
        i18n_r()$t("Parameter"),
        i18n_r()$t("Units"),
        i18n_r()$t("Description")
      )

      # Type çevirileri
      type_translations <- c(
        "input" = i18n_r()$t("input"),
        "adjustable" = i18n_r()$t("adjustable")
      )

      # Units çevirileri
      units_translations <- c(
        "variable" = i18n_r()$t("variable"),
        "none" = i18n_r()$t("none")
      )

      # Description çevirileri
      description_translations <- c(
        "Initial concentration of solute in raw material (e.g. ppm)" = i18n_r()$t("Initial concentration of solute in raw material (e.g. ppm)"),
        "Recovered solute at time t (e.g. ppm of raw material)" = i18n_r()$t("Recovered solute at time t (e.g. ppm of raw material)"),
        "Fraction of quick-desorbing solute" = i18n_r()$t("Fraction of quick-desorbing solute"),
        "First-order rate constant of the quickly-desorbed solute fraction f" = i18n_r()$t("First-order rate constant of the quickly-desorbed solute fraction f"),
        "First-order rate constant of the slowly-desorbed solute fraction (1-f)" = i18n_r()$t("First-order rate constant of the slowly-desorbed solute fraction (1-f)")
      )

      dt_data <- show_pars("ts")
      dt_data$type <- my_mapvalues(dt_data$type, names(type_translations), type_translations, warn_missing = FALSE)
      dt_data$units <- my_mapvalues(dt_data$units, names(units_translations), units_translations, warn_missing = FALSE)
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

  # Ensure UI outputs in collapsed accordions are rendered immediately
  outputOptions(output, "flow_input_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "optmet_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "flow_units_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "resp_units_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "plot_x_units_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "plot_y_units_ui", suspendWhenHidden = FALSE)

  # suspendWhenHidden for renderUI outputs (auto-added)
  outputOptions(output, "accordion_toggle_btn", suspendWhenHidden = FALSE)
  outputOptions(output, "c0_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "calculate_yields_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "cumulative_checkbox_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "cumulative_warning_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "data_input_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "data_preview_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "duplicate_column_warning_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "f_est_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "f_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "flow_params_disabled_message", suspendWhenHidden = FALSE)
  outputOptions(output, "flowpar_pres_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "flowpar_temp_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "insufficient_columns_warning_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "k1_est_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "k2_est_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "kinetic_tws_HELP", suspendWhenHidden = FALSE)
  outputOptions(output, "load_example_data_btn", suspendWhenHidden = FALSE)
  outputOptions(output, "m_in_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "mass_flow_input_section_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "oec_slv_var_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "oec_x_var_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "oec_y_var_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "predict_data_input_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "predict_range_warning", suspendWhenHidden = FALSE)
  outputOptions(output, "prediction_description", suspendWhenHidden = FALSE)
  outputOptions(output, "prediction_moisture_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "pres_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "qmax_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "rename_col_controls_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "ro_h2o_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "solvent_units_info_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "temp_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "tmax_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "use_solvent_checkbox_ui", suspendWhenHidden = FALSE)

}
