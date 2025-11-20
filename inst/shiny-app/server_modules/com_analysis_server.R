# COM Analysis Server Module
com_analysis_server <- function(input, output, session, defaults, i18n) {
  # Load required libraries
  library(scales)
  library(dplyr)

  # helper for creating namespaced ids inside this module
  ns <- session$ns

  # Reactive values for storing results and input data
  results <- reactiveValues(
    com_result = NULL,
    simple_data = NULL,
    detailed_data = NULL,
    input_data = NULL,
    is_multi_value = FALSE,
    selected_simple_data = NULL
  )

  # Reactive to check if export all results button should be visible
  output$show_export_all_results <- reactive({
    !is.null(results$com_result)
  })
  outputOptions(output, "show_export_all_results", suspendWhenHidden = FALSE)

  # -- Settings modal integration ---------------------------------------------
  observeEvent(input$open_settings, {
    showModal(com_analysis_settings_modal_ui(ns("settings_modal"), i18n))
  })

  com_analysis_settings_modal_server(
    "settings_modal",
    app_name = "com_analysis",
    get_current = reactive({
      # Ensure results$input_data is up-to-date before saving
      # This will trigger parsed_input_data() and its observer
      # if any relevant inputs have changed.
      parsed_input_data()
      get_current_settings(input, results)
    }),
    apply_func = function(st) apply_settings(session, st, results),
    i18n = i18n
  )

  # Helper function to convert text input to numeric or NA
  to_numeric_or_na <- function(x) {
    if (is.null(x) || x == "" || is.na(x)) {
      return(NA_real_)
    } else {
      return(as.numeric(x))
    }
  }

  # Reactive for parsed input data
  parsed_input_data <- reactive({
    parsed_input_data_util(input, results, i18n)
  })

  # Display input data preview
  output$input_data_preview <- renderPrint({
    data <- parsed_input_data()
    if (!is.null(data)) {
      head(data)
    } else {
      i18n$t("No data to preview or invalid format.")
    }
  })

  # Observe parsed_input_data and store it
  observeEvent(parsed_input_data(), {
    results$input_data <- parsed_input_data()
  })

  # Observer to update column choices when CSV file is uploaded
  observeEvent(input$csv_file, {
    if (!is.null(input$csv_file)) {
      tryCatch(
        {
          # Read the CSV to get column names with better handling of empty columns
          data <- read.csv(input$csv_file$datapath,
            header = TRUE, stringsAsFactors = FALSE,
            na.strings = c("", "NA", "N/A"), strip.white = TRUE
          )

          # Remove completely empty columns
          data <- data[, !sapply(data, function(x) all(is.na(x) | x == ""))]
          column_names <- names(data)

          # Update time column choices (default to first column)
          updateSelectInput(session, "time_column",
            choices = column_names,
            selected = column_names[1]
          )

          # Update yield column choices (default to second column if available)
          default_yield <- if (length(column_names) >= 2) column_names[2] else column_names[1]
          updateSelectInput(session, "yield_column",
            choices = column_names,
            selected = default_yield
          )
        },
        error = function(e) {
          showNotification(paste("Error reading CSV file:", e$message), type = "error")
        }
      )
    }
  })

  # Function to prepare input data for calcom (now uses reactive input_data)
  prepare_input_data <- function() {
    return(results$input_data)
  }

  # Function to prepare parameters for calcom
  prepare_parameters <- function() {
    # Helper function to safely create named vectors and debug
    safe_named_vector_debug <- function(param_name, ...) {
      values <- list(...)
      # Filter out NULL values before creating the vector
      non_null_values <- values[!sapply(values, is.null)]

      # Check if names are present for all non-NULL values
      if (length(non_null_values) > 0 && is.null(names(non_null_values))) {
        stop(paste("DEBUG ERROR: Parameter group '", param_name, "' has values without names."))
      }

      # Check for length mismatch between values and names
      if (length(non_null_values) != length(names(non_null_values))) {
        stop(paste(
          "DEBUG ERROR: Parameter group '", param_name,
          "' has a mismatch between number of values (", length(non_null_values),
          ") and number of names (", length(names(non_null_values)), ").",
          "Values:", paste(non_null_values, collapse = ", "),
          "Names:", paste(names(non_null_values), collapse = ", ")
        ))
      }

      # Create the vector
      result <- do.call(c, non_null_values)
      return(result)
    }

    # General parameters - ensure all required parameters are included
    gen_params <- safe_named_vector_debug("gen_params",
      volex = input$volex,
      load = input$load,
      pres = input$pres,
      temp = input$temp,
      flow = input$flow,
      extime = input$extime,
      dilfac = input$dilfac,
      pr_sale = input$pr_sale,
      csol_flow = input$csol_flow
    )

    # Calculation Mode parameters
    comode_val <- input$comode
    use_coefs_val <- if (is.null(input$use_coefs)) FALSE else as.logical(input$use_coefs)

    # Cost of Raw Materials parameters
    crm_params <- safe_named_vector_debug("crm_params",
      bh = input$bh,
      id = input$id,
      pr_mat = input$pr_mat,
      pr_msol = input$pr_msol,
      pr_csol = input$pr_csol,
      recp = input$recp,
      rect = input$rect,
      sept = input$sept
    )

    # Cost of Utilities parameters
    cut_params <- safe_named_vector_debug("cut_params",
      pw_main = input$pw_main,
      pr_kwh = input$pr_kwh,
      pw_dry = input$pw_dry,
      pw_com = input$pw_com,
      pw_evap = input$pw_evap,
      cap_dry = input$cap_dry,
      cap_com = input$cap_com,
      cap_evap = input$cap_evap
    )

    # Cost of Labor parameters
    col_params <- safe_named_vector_debug("col_params",
      oper = input$oper,
      whr = input$whr,
      shifts = input$shifts,
      wage = input$wage,
      wdays = input$wdays
    )

    # Fixed Costs parameters
    fci_params <- safe_named_vector_debug("fci_params",
      capex = input$capex,
      maint = input$maint,
      other = input$other,
      depr = input$depr
    )
    taxrate_val <- to_numeric_or_na(input$taxrate)

    # Other parameters
    auxpr_params <- safe_named_vector_debug("auxpr_params", oil = to_numeric_or_na(input$aux_price))
    auxfr_params <- safe_named_vector_debug("auxfr_params", oil = to_numeric_or_na(input$aux_fraction))

    # Handle cosol_loss
    cosol_loss_val <- 0.05 # Hardcode default
    if (!is.null(input$cosol_loss) && !is.na(input$cosol_loss)) {
      cosol_loss_val <- as.numeric(input$cosol_loss)
    }

    # Handle flowpar
    flowpar_val <- "auto" # Hardcode default
    if (!is.null(input$flowpar) && input$flowpar != "") {
      flowpar_val <- input$flowpar
    }
    # Ensure flowpar is "auto" if it's not numeric and not "auto"
    if (is.na(as.numeric(flowpar_val)) && flowpar_val != "auto") {
      flowpar_val <- "auto"
    }

    return(list(
      gen = gen_params,
      crm = crm_params,
      cut = cut_params,
      col = col_params,
      fci = fci_params,
      auxpr = auxpr_params,
      auxfr = auxfr_params,
      cosol_loss = cosol_loss_val,
      flowpar = flowpar_val,
      comode = comode_val,
      use_coefs = use_coefs_val,
      taxrate = taxrate_val
    ))
  }

  # Calculate button event
  observeEvent(input$calculate, {
    tryCatch(
      {
        # Show progress
        withProgress(message = i18n$t("Calculating..."), value = 0, {
          # Prepare input data
          incProgress(0.2, detail = i18n$t("Preparing data..."))
          input_data <- prepare_input_data()

          # Prepare parameters
          incProgress(0.4, detail = i18n$t("Preparing parameters..."))
          params <- prepare_parameters()

          # Call calcom function
          incProgress(0.8, detail = i18n$t("Performing calculations..."))

          # Check if input_data is valid before proceeding
          if (is.null(input_data)) {
            stop("No input data available. Please provide valid input data before calculating.")
          }

          if (results$is_multi_value) {
            com_result <- calcom(
              input = input_data,
              invars = c("time", "yield"),
              gen = params$gen,
              crm = params$crm,
              cut = params$cut,
              col = params$col,
              fci = params$fci,
              auxpr = params$auxpr,
              auxfr = params$auxfr,
              taxrate = params$taxrate, # Add taxrate as a direct parameter
              cosol_loss = params$cosol_loss, # Revert to using params$cosol_loss
              flowpar = params$flowpar, # Use the flowpar from params
              comode = params$comode,
              use_coefs = params$use_coefs,
              pltlab = "Time (min)",
              draw = TRUE # Ensure plots are generated for multi-value input
            )
          } else {
            # For single value, input needs to be a named vector
            com_result <- calcom(
              input = c(time = input_data$time[1], yield = input_data$yield[1]),
              invars = c("time", "yield"),
              gen = params$gen,
              crm = params$crm,
              cut = params$cut,
              col = params$col,
              fci = params$fci,
              auxpr = params$auxpr,
              auxfr = params$auxfr,
              taxrate = params$taxrate, # Add taxrate as a direct parameter
              cosol_loss = params$cosol_loss, # Revert to using params$cosol_loss
              flowpar = params$flowpar, # Use the flowpar from params
              comode = params$comode,
              use_coefs = params$use_coefs,
              pltlab = "Time (min)",
              draw = FALSE # No plots for single value input
            )
          }
          # Güzel görünümde call oluştur
          pretty_call <- substitute(
            calcom(
              input = input_val, invars = c("time", "yield"),
              pltlab = "Time (min)", gen = gen_val, crm = crm_val,
              auxpr = auxpr_val, auxfr = auxfr_val, cut = cut_val,
              col = col_val, fci = fci_val
            ),
            list(
              input_val = c(time = input_data$time[1], yield = input_data$yield[1]),
              gen_val = params$gen,
              crm_val = params$crm,
              auxpr_val = params$auxpr,
              auxfr_val = params$auxfr,
              cut_val = params$cut,
              col_val = params$col,
              fci_val = params$fci
            )
          )

          results$call <- format(com_result[["call"]])
          #  results$call <- deparse(com_result[["call"]], width.cutoff = 500)
          # results$call <- pretty_call

          # Store results
          results$com_result <- com_result
          results$simple_data <- com_result$simple_output
          results$detailed_data <- com_result$output
          results$params <- params # Store params in reactive values

          incProgress(1, detail = i18n$t("Completed!"))
        })

        # Show success message
        showNotification(i18n$t("Calculation completed!"), type = "message")
      },
      error = function(e) {
        # Show error message
        showNotification(paste(i18n$t("bug-Error occurred:"), e$message), type = "error")
        # browser()
      }
    )
  })

  # Reset button event
  observeEvent(input$reset, {
    # Reset all inputs to default values
    updateNumericInput(session, "volex", value = defaults$volex)
    updateNumericInput(session, "load", value = defaults$load)
    updateNumericInput(session, "pres", value = defaults$pres)
    updateNumericInput(session, "temp", value = defaults$temp)
    updateNumericInput(session, "flow", value = defaults$flow)
    updateNumericInput(session, "extime", value = defaults$extime)
    updateNumericInput(session, "dilfac", value = defaults$dilfac)
    updateNumericInput(session, "pr_sale", value = defaults$pr_sale)

    updateRadioButtons(session, "comode", selected = defaults$comode)
    updateCheckboxInput(session, "use_coefs", value = defaults$use_coefs)

    updateNumericInput(session, "bh", value = defaults$bh)
    updateNumericInput(session, "id", value = defaults$id)
    updateNumericInput(session, "pr_mat", value = defaults$pr_mat)
    updateNumericInput(session, "pr_msol", value = defaults$pr_msol)
    updateNumericInput(session, "pr_csol", value = defaults$pr_csol)
    updateNumericInput(session, "recp", value = defaults$recp)
    updateNumericInput(session, "rect", value = defaults$rect)
    updateNumericInput(session, "sept", value = defaults$sept)

    updateNumericInput(session, "pw_main", value = defaults$pw_main)
    updateNumericInput(session, "pr_kwh", value = defaults$pr_kwh)
    updateNumericInput(session, "pw_dry", value = defaults$pw_dry)
    updateNumericInput(session, "pw_com", value = defaults$pw_com)
    updateNumericInput(session, "pw_evap", value = defaults$pw_evap)
    updateNumericInput(session, "cap_dry", value = defaults$cap_dry)
    updateNumericInput(session, "cap_com", value = defaults$cap_com)
    updateNumericInput(session, "cap_evap", value = defaults$cap_evap)

    updateNumericInput(session, "oper", value = defaults$oper)
    updateNumericInput(session, "whr", value = defaults$whr)
    updateNumericInput(session, "shifts", value = defaults$shifts)
    updateNumericInput(session, "wage", value = defaults$wage)
    updateNumericInput(session, "wdays", value = defaults$wdays)

    updateNumericInput(session, "capex", value = defaults$capex)
    updateNumericInput(session, "maint", value = defaults$maint)
    updateNumericInput(session, "other", value = defaults$other)
    updateNumericInput(session, "depr", value = defaults$depr)
    updateTextInput(session, "taxrate", value = defaults$taxrate)

    updateTextInput(session, "aux_price", value = defaults$aux_price)
    updateTextInput(session, "aux_fraction", value = defaults$aux_fraction)
    updateNumericInput(session, "cosol_loss", value = defaults$cosol_loss)
    updateTextInput(session, "flowpar", value = defaults$flowpar)
    updateNumericInput(session, "csol_flow", value = defaults$csol_flow)

    # Reset input data section
    updateRadioButtons(session, "input_type", selected = "single") # Default to single value
    updateNumericInput(session, "single_time", value = 180) # Default from UI
    updateNumericInput(session, "single_yield", value = 7) # Default from UI
    updateTextAreaInput(session, "text_input", value = "") # Clear text input

    # Reset CSV column selections
    updateSelectInput(session, "time_column", choices = NULL, selected = NULL)
    updateSelectInput(session, "yield_column", choices = NULL, selected = NULL)

    # Clear results
    results$com_result <- NULL
    results$simple_data <- NULL
    results$detailed_data <- NULL
    results$input_data <- NULL # Ensure input_data is cleared on reset
    results$is_multi_value <- FALSE # Ensure multi-value flag is reset

    showNotification(i18n$t("Parameters reset"), type = "message")
  })

  # Observe results and update time slider
  observeEvent(results$com_result, {
    if (results$is_multi_value && !is.null(results$simple_data)) {
      time_values <- results$simple_data$time
      if (length(time_values) > 0) {
        # Find the row with the maximum monthly_profit
        max_profit_row <- results$simple_data[which.max(results$simple_data$monthly_profit), ]


        updateSliderInput(session, "time_slider",
          min = min(time_values),
          max = max(time_values),
          value = max_profit_row$time, # median(time_values),
          step = min(diff(sort(unique(time_values))))
        )
      }
    }
  })

  # Reactive for selected time data
  selected_simple_data <- reactive({
    if (!is.null(results$com_result) && nrow(results$simple_data) > 0) {
      # Find the closest time value in simple_data to the slider value
      closest_time_idx <- which.min(abs(results$simple_data$time - input$time_slider))
      results$simple_data[closest_time_idx, ]
    } else {
      NULL
    }
  })

  # Reactive for selected detailed data
  selected_detailed_data <- reactive({
    req(results$detailed_data)

    if (results$is_multi_value) {
      req(input$time_slider)
      # Find the closest time column
      time_cols <- names(results$detailed_data)[grepl("_min$", names(results$detailed_data))]
      numeric_times <- as.numeric(gsub("_min$", "", time_cols))

      # Handle cases where numeric_times might be empty or contain NA/NaN
      if (length(numeric_times) == 0 || all(is.na(numeric_times))) {
        selected_col_name <- "60_min" # Fallback to default if no time columns found
      } else {
        closest_time_idx <- which.min(abs(numeric_times - input$time_slider))
        selected_col_name <- time_cols[closest_time_idx]
      }
    } else { # Single value case
      # Find the single _min column in detailed_data
      single_value_cols <- names(results$detailed_data)[grepl("_min$", names(results$detailed_data))]
      if (length(single_value_cols) == 1) {
        selected_col_name <- single_value_cols[1]
      } else {
        # Fallback if no single _min column found (should not happen with valid calcom output)
        selected_col_name <- "60_min" # Keep as a last resort fallback
      }
    }

    # Ensure the selected column exists, fallback to a safe default if not
    if (!selected_col_name %in% names(results$detailed_data)) {
      # Try to find any _min column if the selected one is missing
      available_min_cols <- names(results$detailed_data)[grepl("_min$", names(results$detailed_data))]
      if (length(available_min_cols) > 0) {
        selected_col_name <- available_min_cols[1] # Use the first available _min column
      } else {
        selected_col_name <- "60_min" # Absolute last resort fallback
      }
    }

    # Format the detailed data for display
    display_data <- results$detailed_data %>%
      select(description, !!sym(selected_col_name), units) %>% # Change order here
      rename(
        # "Parameter" = parameter,
        "Description" = description,
        "Value" = !!sym(selected_col_name), # Rename Value first
        "Unit" = units # Then rename Unit
      ) %>%
      mutate(Value = format(Value, big.mark = ",", digits = 2, scientific = FALSE)) # Format Value column
    return(display_data)
  })

  # Output to control conditionalPanel visibility for time slider
  output$has_multi_data <- reactive({
    results$is_multi_value
  })
  outputOptions(output, "has_multi_data", suspendWhenHidden = FALSE)

  # Render Info Boxes
  output$extract_box <- renderInfoBox({
    if (!is.null(selected_simple_data())) {
      extract_value <- selected_simple_data()$extract_made
      infoBox(
        i18n$t("Extract produced *"),
        paste0(format(round(extract_value), big.mark = ","), " kg"),
        icon = icon("weight-hanging"),
        color = "blue"
      )
    } else {
      infoBox(i18n$t("Extract produced *"), "0 kg", icon = icon("weight-hanging"), color = "blue")
    }
  })

  output$profit_box <- renderInfoBox({
    if (!is.null(selected_simple_data())) {
      sc_value <- selected_simple_data()$monthly_profit
      infoBox(
        i18n$t("Profit (After-tax) *"),
        paste0(format(sc_value, digits = 2), " $"),
        icon = icon("dollar-sign"),
        color = "green"
      )
    } else {
      infoBox(i18n$t("Gross profit (After-tax) *"), "$0", icon = icon("dollar-sign"), color = "green")
    }
  })

  output$margin_box <- renderInfoBox({
    if (!is.null(selected_simple_data())) {
      margin_value <- selected_simple_data()$margin_percent
      infoBox(
        i18n$t("Profit Margin *"),
        paste0(format(margin_value, digits = 2), " %"),
        icon = icon("percentage"),
        color = "purple"
      )
    } else {
      infoBox(i18n$t("Profit Margin *"), "0 %", icon = icon("percentage"), color = "purple")
    }
  })

  output$payback_box <- renderInfoBox({
    if (!is.null(selected_simple_data())) {
      payback_value <- selected_simple_data()$payback_yr
      infoBox(
        i18n$t("Payback Period"),
        paste0(format(payback_value, digits = 2), i18n$t(" years")),
        icon = icon("clock"),
        color = "orange"
      )
    } else {
      infoBox(i18n$t("Payback Period"), "0 years", icon = icon("clock"), color = "orange")
    }
  })

  # Render ROI Plot
  output$roi_plot <- renderPlotly({
    render_roi_plot(selected_simple_data(), input$capex, results$com_result, i18n)
  })

  output$roi_monthly_table <- DT::renderDataTable(
    {
      req(results$params) # Ensure params is available
      render_roi_monthly_table(results$detailed_data, results$is_multi_value, input$time_slider, input$capex, results$com_result, results$params$taxrate, i18n)
    },
    server = FALSE,
    options = list(rownames = FALSE)
  )
  # Export Monthly Data
  output$export_monthly <- download_roi_monthly_processed_data(
    prepare_roi_monthly_data(results$detailed_data, results$is_multi_value, input$time_slider, input$capex, results$com_result, results$params$taxrate, i18n)
  )

  # Render Detailed Table
  output$detailed_table <- DT::renderDataTable(
    {
      req(selected_detailed_data()) # Use the reactive detailed data

      # Use the detailed data directly (already formatted in selected_detailed_data)
      display_data <- selected_detailed_data()

      DT::datatable(
        display_data,
        extensions = "Buttons",
        options = list(
          scrollX = TRUE,
          pageLength = 32,
          dom = "Bfrtip",
          buttons = list(
            list(extend = "csv", filename = generate_filename_with_timestamp("supercrit_com_analysis_detailed_data")),
            list(extend = "excel", filename = generate_filename_with_timestamp("supercrit_com_analysis_detailed_data")),
            list(extend = "pdf", filename = generate_filename_with_timestamp("supercrit_com_analysis_detailed_data"))
          )
        )
      )
    },
    server = FALSE
  )

  # Render Payback Plot
  output$payback_plot <- renderPlotly({
    if (results$is_multi_value) {
      if (!is.null(results$com_result) && !is.null(results$com_result$plots$payback)) {
        results$com_result$plots$payback
      } else {
        plotly_empty()
      }
    } else { # Single value case
      if (!is.null(selected_simple_data())) {
        payback_value <- selected_simple_data()$payback_yr

        plot_data <- data.frame(
          x = c(0, payback_value),
          y = c(0, 0)
        )

        p <- plot_ly(plot_data, x = ~x, y = ~y, type = "scatter", mode = "lines") %>%
          add_segments(
            x = 0, xend = payback_value, y = 0, yend = 0,
            line = list(color = "orange", width = 5),
            name = i18n$t("Payback Period")
          ) %>%
          layout(
            title = paste0(i18n$t("Payback Period: "), format(payback_value, digits = 2), " years"),
            xaxis = list(title = "Years"),
            yaxis = list(title = "", showticklabels = FALSE),
            showlegend = FALSE
          )
        return(p)
      } else {
        plotly_empty()
      }
    }
  })

  # Render Specific Cost Plot
  output$sc_plot <- renderPlotly({
    if (!is.null(results$com_result) && !is.null(results$com_result$plots$sc)) {
      results$com_result$plots$sc
    } else {
      plotly_empty()
    }
  })

  # Render Server Call Output
  output$call_output <- renderPrint({
  if (!is.null(results$call)) {
    # Join the character vector into a single string for better display
    cat(results$call)
  } else {
    cat("No calculation performed yet.\n")
  }
  })



  # Download All Results
  output$download_all_results <- downloadHandler(
    filename = function() {
      paste0(generate_filename_with_timestamp("supercrit_com_analysis_export"), ".zip")
    },
    content = function(file) {
      req(results$com_result) # Ensure results are available

      # Create a temporary directory for export
      temp_dir <- file.path(tempdir(), paste0("com_analysis_export_", format(Sys.time(), "%Y%m%d%H%M%S")))
      dir.create(temp_dir, recursive = TRUE)

      # Export results using com_export
      tryCatch({
        com_export(comres = results$com_result, expath = temp_dir, silent = TRUE)

        # Get list of files to zip
        files_to_zip <- list.files(temp_dir, full.names = TRUE, recursive = TRUE)

        # Create the zip file
        zip::zip(zipfile = file, files = files_to_zip, mode = "cherry-pick")
      }, error = function(e) {
        showNotification(paste(i18n$t("Error exporting results:"), e$message), type = "error")
      }, finally = {
        # Clean up the temporary directory
        unlink(temp_dir, recursive = TRUE)
      })
    },
    contentType = "application/zip"
  )

  # Intro Help Button Observers
  observeEvent(input$input_data_help, {
    current_input_type <- input$input_type %||% "single"
    introjs(session, options = intro_steps_com_input_data(ns, i18n, current_input_type))
  })

  observeEvent(input$calc_mode_help, {
    introjs(session, options = intro_steps_com_calc_mode(ns, i18n))
  })

  observeEvent(input$gen_params_help, {
    introjs(session, options = intro_steps_com_gen_params(ns, i18n))
  })

  observeEvent(input$other_params_help, {
    introjs(session, options = intro_steps_com_other_params(ns, i18n))
  })

  observeEvent(input$crm_params_help, {
    introjs(session, options = intro_steps_com_crm_params(ns, i18n))
  })

  observeEvent(input$cut_params_help, {
    introjs(session, options = intro_steps_com_cut_params(ns, i18n))
  })

  observeEvent(input$col_params_help, {
    introjs(session, options = intro_steps_com_col_params(ns, i18n))
  })

  observeEvent(input$fci_params_help, {
    introjs(session, options = intro_steps_com_fci_params(ns, i18n))
  })

  # ============================================================================
  # UNITS MODAL
  # ============================================================================

  observeEvent(input$show_units, {
    showModal(modalDialog(
      title = i18n$t("COM Analysis Parameter Units"),
      DT::dataTableOutput(ns("units_table")),
      easyClose = TRUE,
      size = "l"
    ))
  })

  output$units_table <- DT::renderDataTable({
    com_pars <- show_pars("com")
    # show_pars("com") returns a list with $input and $output dataframes
    # Combine them for display
    if (is.list(com_pars) && length(com_pars) == 2) {
      # Add a source column to distinguish input vs output parameters
      input_df <- com_pars$input
      output_df <- com_pars$output
      if (!is.null(input_df) && nrow(input_df) > 0) {
        input_df$source <- "Input Parameters"
      }
      if (!is.null(output_df) && nrow(output_df) > 0) {
        output_df$source <- "Output Parameters"
      }
      # Combine the dataframes
      combined_df <- bind_rows(input_df, output_df)
      combined_df
    } else {
      # Fallback in case structure changes
      com_pars
    }
  })
}
