# Two-Site Kinetic Modeling Server Module
kinetic_tws_server <- function(input, output, session, defaults, i18n) {
  # Load required libraries
  library(dplyr)
  library(DT)
  library(plotly)
  library(zip) # For zipping export files
  library(supeRcrit) # Assuming ktsmod is part of supeRcrit

  # Helper for creating namespaced ids inside this module
  ns <- session$ns

  # Null coalescing operator
  `%||%` <- function(x, y) if (is.null(x)) y else x

  # Reactive values for storing results
  kinetic_results <- reactiveValues(
    model_data = NULL,
    model_summary = NULL,
    model_plot = NULL
  )

  # Dynamic UI for data input (CSV upload or manual text input)
  output$data_input_ui <- renderUI({
    if (input$input_type == "csv") {
      fileInput(ns("file_upload"), i18n$t("Upload CSV File"),
        accept = c("text/csv", "text/comma-separated-values", "text/plain", ".csv")
      )
    } else {
      textAreaInput(ns("manual_data"), i18n$t("Enter Time and Yield Data (CSV format, max 10 rows)"),
        rows = 10, placeholder = "Time_min,Yield_100C\n1,10\n2,18\n..."
      )
    }
  })

  # Reactive for parsed OEC data
  oec_data <- reactive({
    req(input$input_type)
    data <- NULL

    if (input$input_type == "csv") {
      req(input$file_upload)
      tryCatch(
        {
          data <- read.csv(input$file_upload$datapath, stringsAsFactors = FALSE)
        },
        error = function(e) {
          showNotification(paste(i18n$t("Error loading CSV:"), e$message), type = "error")
          return(NULL)
        }
      )
    } else { # Manual input
      req(input$manual_data)
      if (nchar(input$manual_data) > 0) {
        tryCatch(
          {
            data <- read.csv(text = input$manual_data, stringsAsFactors = FALSE)
          },
          error = function(e) {
            showNotification(paste(i18n$t("Error parsing manual data:"), e$message), type = "error")
            return(NULL)
          }
        )
      }
    }
    data
  })

  # Reactive for default OEC variable selections
  default_oec_vars <- reactive({
    data <- oec_data()
    if (is.null(data) || ncol(data) < 2) {
      return(list(x_var = NULL, y_var = NULL, slv_var = "None"))
    }

    list(
      x_var = colnames(data)[1],
      y_var = colnames(data)[2],
      slv_var = "None"
    )
  })

  # Render dynamic UI for OEC variables
  output$oec_x_var_ui <- renderUI({
    data <- oec_data()
    if (is.null(data) || ncol(data) < 1) {
      return(NULL)
    }
    choices <- colnames(data)
    selected <- input$oec_x_var %||% default_oec_vars()$x_var
    selectInput(ns("oec_x_var"), i18n$t("Time Variable"), choices = choices, selected = selected)
  })

  output$oec_y_var_ui <- renderUI({
    data <- oec_data()
    if (is.null(data) || ncol(data) < 2) {
      return(NULL)
    }
    choices <- colnames(data)
    selected <- input$oec_y_var %||% default_oec_vars()$y_var
    selectInput(ns("oec_y_var"), i18n$t("Response Variable"), choices = choices, selected = selected)
  })

  output$oec_slv_var_ui <- renderUI({
    data <- oec_data()
    if (is.null(data)) {
      return(NULL)
    }
    choices <- c("None", colnames(data))
    selected <- input$oec_slv_var %||% default_oec_vars()$slv_var
    selectInput(ns("oec_slv_var"), i18n$t("Solvent Variable (Optional)"), choices = choices, selected = selected)
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
    req(validated_oec_data())

    tryCatch(
      {
        # Show progress
        withProgress(message = i18n$t("Calculating kinetic model..."), value = 0, {
          incProgress(0.2, detail = i18n$t("Preparing parameters..."))

          # Prepare OEC variables, using input values or defaults
          x_var_val <- input$oec_x_var %||% default_oec_vars()$x_var
          y_var_val <- input$oec_y_var %||% default_oec_vars()$y_var
          slv_var_val <- input$oec_slv_var %||% default_oec_vars()$slv_var

          oec_vars_list <- c(x = x_var_val, y = y_var_val)
          if (slv_var_val != "None" && nchar(slv_var_val) > 0) {
            oec_vars_list <- c(oec_vars_list, slv = slv_var_val)
          }

          # Prepare parameters for ktsmod
          pars_list <- c(
            pres = input$pres,
            temp = input$temp,
            m_in = input$m_in,
            c0 = input$c0,
            flow = input$flow,
            f = input$f
          )

          # Prepare units
          units_list <- c(flow = input$flow_units, resp = input$resp_units)

          # Prepare optimization estimates
          opt_est_list <- c(
            k1 = as.numeric(input$k1_est),
            k2 = as.numeric(input$k2_est)
          )
          if (is.na(input$f)) { # Only add f_est if f is not provided in pars
            opt_est_list <- c(opt_est_list, f = as.numeric(input$f_est))
          }

          # Prepare plot units
          plot_units_list <- c(x = input$plot_x_units, y = input$plot_y_units)

          # Prepare flowpar
          flowpar_vec <- c(input$flowpar_temp, input$flowpar_pres)
          # Ensure NA values are handled correctly for ktsmod
          flowpar_vec[is.null(flowpar_vec)] <- NA



          incProgress(0.6, detail = i18n$t("Running model..."))
          #browser()

          # Call ktsmod function
          model_result <- supeRcrit::ktsmod(
            oec = validated_oec_data(),
            oec_vars = oec_vars_list,
            pars = pars_list,
            units = units_list,
            opt_est = opt_est_list,
            # opt_est = "default",
            plot_units = plot_units_list,
            flowpar = flowpar_vec,
            ro_h2o = input$ro_h2o,
            tmax = input$tmax,
            qmax = input$qmax,
            cumulative = input$cumulative,
            mass_flow = input$mass_flow,
            draw = FALSE, # Do not draw plot directly, capture it
            optmet = input$optmet
          )

          # Store results
          full_model_result(model_result) # Store the full result
          kinetic_results$model_data <- model_result$data
          kinetic_results$model_summary <- model_result$tws # Store the full tws output
          kinetic_results$model_plot <- model_result$plots$tws
          kinetic_results$call <- model_result$call

          incProgress(1, detail = i18n$t("Completed!"))
        })

        showNotification(i18n$t("Kinetic model calculated successfully!"), type = "message")
      },
      error = function(e) {
        showNotification(paste(i18n$t("Error calculating kinetic model:"), e$message), type = "error")
        # Clear results on error to hide export button
        kinetic_results$model_data <- NULL
        kinetic_results$model_summary <- NULL
        kinetic_results$model_plot <- NULL
        kinetic_results$call <- NULL
        full_model_result(NULL)
      }
    )
  })

  # Calculate kinetic model
  # This duplicate block has been removed - calculation is now handled in the above observeEvent
   # Render model plot
  output$kinetic_plot <- renderPlotly({
    req(kinetic_results$model_plot)
    
    tryCatch({
      plot_obj <- kinetic_results$model_plot
      
      # Check if it's already a plotly object
      if ("plotly" %in% class(plot_obj)) {
        return(plot_obj)
      }
      
      # If it's a ggplot, try to convert safely
      if ("ggplot" %in% class(plot_obj)) {
        # Try ggplotly with error handling
        tryCatch({
          ggplotly(plot_obj, tooltip = "all")
        }, error = function(e1) {
          # If ggplotly fails, try with minimal tooltip
          tryCatch({
            ggplotly(plot_obj, tooltip = c("x", "y"))
          }, error = function(e2) {
            # If still fails, create a simple plotly plot
            plot_ly() %>%
              add_annotations(
                text = paste("Plot display error. Original ggplot available but cannot convert to plotly."),
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
          })
        })
      } else {
        # Unknown plot type, create error message
        plot_ly() %>%
          add_annotations(
            text = "Unsupported plot format",
            x = 0.5, y = 0.5,
            showarrow = FALSE
          )
      }
    }, error = function(e) {
      # Final fallback - create empty plot with error message
      plot_ly() %>%
        add_annotations(
          text = paste("Error displaying plot:", substr(e$message, 1, 100)),
          x = 0.5, y = 0.5,
          showarrow = FALSE,
          font = list(color = "red")
        ) %>%
        layout(title = "Plot Error")
    })
  })

  # Render model parameters table
  output$model_parameters_table <- DT::renderDataTable({
    req(kinetic_results$model_summary)
    mod_pars_df <- as.data.frame(kinetic_results$model_summary$mod_pars)
    colnames(mod_pars_df) <- "Value"
    dt_output <- DT::datatable(
      mod_pars_df,
      options = list(dom = "t", scrollX = TRUE),
      rownames = TRUE
    )
    dt_output <- dt_output %>% DT::formatRound(columns = c("Value"), digits = 6)
  })

  # Render fitted parameters output
  output$fitted_parameters_output <- renderUI({
    req(kinetic_results$model_summary)
    pars <- kinetic_results$model_summary$fit_pars
    tags$div(
      style = "display: flex; flex-wrap: wrap; gap: 4px;",
      lapply(pars, function(p) {
        tags$span(
          style = "background-color: #f8f9fa; padding: 2px 6px; border-radius: 3px; font-family: monospace; font-size: 12px;",
          p
        )
      })
    )
  })

  # Render model statistics table
  output$model_statistics_table <- DT::renderDataTable({
    req(kinetic_results$model_summary)
    resid_df <- as.data.frame(kinetic_results$model_summary$resid)
    colnames(resid_df) <- "Value"
    dt_output <- DT::datatable(
      resid_df,
      options = list(dom = "t", scrollX = TRUE),
      rownames = TRUE
    )
    dt_output <- dt_output %>% DT::formatRound(columns = c("Value"), digits = 6)

  })

  # Render observed vs predicted data table
  output$observed_predicted_table <- DT::renderDataTable(
    {
      req(kinetic_results$model_summary)
      dt_output <- DT::datatable(
        kinetic_results$model_summary$ordt,
        extensions = "Buttons",
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          dom = "Bfrtip",
          buttons = list(
            list(extend = "csv", filename = generate_filename_with_timestamp("supercrit_kinetic_observed_predicted_data")),
            list(extend = "excel", filename = generate_filename_with_timestamp("supercrit_kinetic_observed_predicted_data")),
            list(extend = "pdf", filename = generate_filename_with_timestamp("supercrit_kinetic_observed_predicted_data"))
          )
        ),
        rownames = FALSE
      )

      dt_output <- dt_output %>% DT::formatRound(columns = c("q", "cc0", "pred_y", "pred_cc0"), digits = 5)
    },
    server = FALSE,
  )

  # Render model data table
  output$model_data_table <- DT::renderDataTable(
    {
      req(kinetic_results$model_summary)
      dt_output <- DT::datatable(
        kinetic_results$model_summary$mdt,
        extensions = "Buttons",
        options = list(
          scrollX = TRUE,
          pageLength = 25,
          dom = "Bfrtip",
          buttons = list(
            list(extend = "csv", filename = generate_filename_with_timestamp("supercrit_kinetic_model_data")),
            list(extend = "excel", filename = generate_filename_with_timestamp("supercrit_kinetic_model_data")),
            list(extend = "pdf", filename = generate_filename_with_timestamp("supercrit_kinetic_model_data"))
          )
        ),
        rownames = FALSE
      )

      dt_output <- dt_output %>% DT::formatRound(columns = c("x", "q", "y", "cc0"), digits = 5)
    },
    server = FALSE,
  )

  # Reactive for storing the full ktsmod result for predictions
  full_model_result <- reactiveVal(NULL)



  # Reactive values for storing prediction results
  prediction_results <- reactiveValues(
    predictions_df = NULL,
    description_text = NULL
  )

  # Dynamic UI for prediction data input
  output$predict_data_input_ui <- renderUI({
    if (input$predict_input_type == "csv") {
      fileInput(ns("predict_file_upload"), 
                i18n$t("Upload CSV (single column)"),
                accept = c("text/csv", ".csv"))
    } else {
      textAreaInput(ns("predict_manual_data"), 
                    i18n$t("Enter values (one per line)"),
                    rows = 8,
                    placeholder = "10\n20\n30\n...")
    }
  })

  # Parse prediction input data
  predict_input_data <- reactive({
    req(input$predict_input_type)
    
    data <- NULL
    
    if (input$predict_input_type == "csv") {
      req(input$predict_file_upload)
      tryCatch({
        data <- read.csv(input$predict_file_upload$datapath, 
                         header = FALSE)[,1]
        data <- as.numeric(data)
        data <- data[!is.na(data)]
      }, error = function(e) {
        showNotification(paste(i18n$t("Error loading CSV:"), 
                              e$message), type = "error")
        return(NULL)
      })
    } else {
      req(input$predict_manual_data)
      if (nchar(input$predict_manual_data) > 0) {
        tryCatch({
          data <- as.numeric(strsplit(input$predict_manual_data, 
                                     "\\n")[[1]])
          data <- data[!is.na(data)]
        }, error = function(e) {
          showNotification(paste(i18n$t("Error parsing data:"), 
                                e$message), type = "error")
          return(NULL)
        })
      }
    }
    
    data
  })

  # Observe event for generating predictions
  observeEvent(input$generate_predictions, {
    req(full_model_result()) # Ensure a model has been calculated
    req(predict_input_data())

    # Call predict_kts function
    tryCatch({
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
    }, error = function(e) {
      showNotification(paste(i18n$t("Error generating predictions:"), e$message), type = "error")
      prediction_results$predictions_df <- NULL
      prediction_results$description_text <- NULL
    })
  })

  # Render prediction results table
  output$prediction_results_table <- DT::renderDataTable(
    {
      req(prediction_results$predictions_df)
      dt_output <- DT::datatable(
        prediction_results$predictions_df,
        extensions = "Buttons",
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          dom = "Bfrtip",
          buttons = list(
            list(extend = "csv", filename = generate_filename_with_timestamp("supercrit_kinetic_predictions")),
            list(extend = "excel", filename = generate_filename_with_timestamp("supercrit_kinetic_predictions")),
            list(extend = "pdf", filename = generate_filename_with_timestamp("supercrit_kinetic_predictions"))
          )
        ),
        rownames = FALSE
      )
      dt_output <- dt_output %>% DT::formatRound(columns = colnames(prediction_results$predictions_df), digits = 5)
    },
    server = FALSE
  )

  # Render prediction description
  output$prediction_description <- renderUI({
    req(prediction_results$description_text)
    HTML(paste0("<p>", gsub("\n", "<br/>", prediction_results$description_text), "</p>"))
  })

  # ============================================================================
  # UNITS MODAL
  # ============================================================================

  observeEvent(input$show_units, {
    showModal(modalDialog(
      title = i18n$t("Two-Site Kinetic Model Parameter Units"),
      DT::dataTableOutput(ns("units_table")),
      easyClose = TRUE,
      size = "l"
    ))
  })

  output$units_table <- DT::renderDataTable({
    show_pars("ts")
  })

  # Reactive to check if export button should be visible
  output$show_export_button <- reactive({
    !is.null(kinetic_results$model_data)
  })
  outputOptions(output, "show_export_button", suspendWhenHidden = FALSE)

  # Reset function
  observeEvent(input$reset, {
    # Reset all inputs to default values
    updateRadioButtons(session, "input_type", selected = "csv")
    # The dynamic selectInputs will reset automatically when oec_data() becomes NULL
    # or when new data is loaded.
    updateNumericInput(session, "pres", value = 15)
    updateNumericInput(session, "temp", value = 100)
    updateNumericInput(session, "m_in", value = 2)
    updateNumericInput(session, "c0", value = 77)
    updateNumericInput(session, "flow", value = 2)
    updateNumericInput(session, "f", value = 0.24)
    updateCheckboxInput(session, "cumulative", value = TRUE)
    updateCheckboxInput(session, "mass_flow", value = FALSE)
    updateTextInput(session, "k1_est", value = "0.1")
    updateTextInput(session, "k2_est", value = "0.1")
    updateTextInput(session, "f_est", value = "0.5")
    updateSelectInput(session, "optmet", selected = "nlopt")
    updateTextInput(session, "flow_units", value = "mL/min")
    updateTextInput(session, "resp_units", value = "permille")
    updateTextInput(session, "plot_x_units", value = "q")
    updateTextInput(session, "plot_y_units", value = "abs")
    updateNumericInput(session, "flowpar_temp", value = NA)
    updateNumericInput(session, "flowpar_pres", value = NA)
    updateNumericInput(session, "ro_h2o", value = NA)
    updateNumericInput(session, "tmax", value = NA)
    updateNumericInput(session, "qmax", value = NA)

    # Reset prediction inputs
    updateTextInput(session, "prediction_times", value = "")
    updateCheckboxInput(session, "prediction_get_yields", value = TRUE)
    updateNumericInput(session, "prediction_moisture", value = NA)

    # Clear results
    kinetic_results$model_data <- NULL
    kinetic_results$model_summary <- NULL
    kinetic_results$model_plot <- NULL
    full_model_result(NULL) # Clear the full model result
    prediction_results$predictions_df <- NULL
    prediction_results$description_text <- NULL

    showNotification(i18n$t("Parameters reset"), type = "message")
  })

  # Export kinetic model results
  output$export_kinetic <- downloadHandler(
    filename = function() {
      # Generate filename with timestamp
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
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

  # Intro Help Button Observers
  observeEvent(input$input_data_oec_help, {
    introjs(session, options = intro_steps_kinetic_tws_input_data_oec(ns, i18n))
  })

  observeEvent(input$process_params_help, {
    introjs(session, options = intro_steps_kinetic_tws_process_params(ns, i18n))
  })

  observeEvent(input$model_params_help, {
    introjs(session, options = intro_steps_kinetic_tws_model_params(ns, i18n))
  })


}
