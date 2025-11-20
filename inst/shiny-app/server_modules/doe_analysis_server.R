doe_analysis_server <- function(input, output, session, defaults, i18n) {
  # Helper for creating namespaced ids inside this module
  ns <- session$ns

  shinyjs::disable("doe_analysis-reset")
  shinyjs::disable("save_analysis") # Disable save button initially

  # Source the save modal UI
  source(file.path("ui_modules", "doe_analysis_save_modal_ui.R"), local = TRUE)$value

  # Reactive values for storing results
  analysis_results <- reactiveValues(
    input_data = NULL,
    analysis_result = NULL,
    variable_names = NULL,
    response_var = NULL,
    time_var = NULL
  )

  # Reactive to check if export all results button should be visible
  output$show_export_all_results <- reactive({
    !is.null(analysis_results$analysis_result)
  })
  outputOptions(output, "show_export_all_results", suspendWhenHidden = FALSE)

  # Reactive value to store available settings
  saved_designs_rv <- reactiveVal(NULL)

  # Function to refresh saved designs
  refresh_saved_designs <- function() {
    designs <- list_saved_settings("doe_design")
    saved_designs_rv(designs)
  }

  # Observe changes in saved_designs_rv and update selectInput
  observeEvent(saved_designs_rv(), {
    saved_designs <- saved_designs_rv()
    if (length(saved_designs) > 0) {
      updateSelectInput(session, "saved_design",
        choices = setNames(saved_designs, saved_designs),
        selected = saved_designs[1]
      )
    } else {
      updateSelectInput(session, "saved_design",
        choices = c("No saved designs" = ""),
        selected = ""
      )
    }
  })

  # Listen for custom message to refresh designs
  observeEvent(input$refresh_designs, {
    refresh_saved_designs()
  })

  # Initial refresh when the module starts
  refresh_saved_designs()

  # Reactive for loading data based on source
  loaded_data <- reactive({
    switch(input$data_source,
      "saved" = {
        req(input$saved_design)
        design_data <- load_settings("doe_design", input$saved_design)
        if (!is.null(design_data$results$design_matrix)) {
          design_data$results$design_matrix
        } else {
          NULL
        }
      },
      "import" = {
        req(input$import_file)
        tryCatch(
          {
            json_data <- jsonlite::read_json(input$import_file$datapath, simplifyVector = TRUE)
            if (!is.null(json_data$results$design_matrix)) {
              json_data$results$design_matrix
            } else {
              json_data
            }
          },
          error = function(e) {
            showNotification(paste(i18n$t("Error loading JSON:"), e$message), type = "error")
            NULL
          }
        )
      },
      "csv" = {
        req(input$import_file_csv)
        tryCatch(
          {
            parse_design_file(input$import_file_csv$datapath)
          },
          error = function(e) {
            showNotification(paste(i18n$t("Error loading file:"), e$message), type = "error")
            NULL
          }
        )
      },
      "manual" = {
        req(input$manual_data)
        tryCatch(
          {
            # Parse CSV data
            con <- textConnection(input$manual_data)
            data <- read.csv(con, stringsAsFactors = FALSE)
            close(con)
            data
          },
          error = function(e) {
            showNotification(paste(i18n$t("Error parsing manual data:"), e$message), type = "error")
            NULL
          }
        )
      }
    )
  })

  # Update variable selections when data changes
  observeEvent(loaded_data(), {
    data <- loaded_data()
    if (!is.null(data) && is.data.frame(data)) {
      var_names <- names(data)

      updateSelectInput(session, "time_var", choices = var_names)


      response_choices <- c("None" = "", var_names)
      last_choice_value <- tail(response_choices, 1)

      updateSelectInput(
        session,
        "response_var",
        choices = response_choices,
        selected = last_choice_value
      )
      # Auto-select likely response and time variables
      if ("response" %in% var_names) {
        updateSelectInput(session, "response_var", selected = "response")
      }
      if ("Actual_Order" %in% var_names) {
        updateSelectInput(session, "time_var", selected = "Actual_Order")
      }

      analysis_results$input_data <- data
      analysis_results$variable_names <- var_names
      shinyjs::enable("save_analysis") # Enable save button when data is loaded
    } else {
      shinyjs::disable("save_analysis") # Disable save button if no data
    }
  })

  # Reactive for available uncoded factors
  available_uc_facs <- reactive({
    req(analysis_results$input_data)
    data <- analysis_results$input_data
    var_names <- names(data)

    # Identify coded factors (single uppercase letters)
    coded_cols <- grep("^[A-Z]$", var_names, value = TRUE)

    # Exclude coded factors, response variable, and time variable
    exclude_cols <- c(coded_cols, input$response_var, input$time_var)

    # Filter out any NULL or empty strings from exclude_cols
    exclude_cols <- exclude_cols[exclude_cols != "" & !is.null(exclude_cols)]

    # Get uncoded factors
    uc_facs_candidates <- setdiff(var_names, exclude_cols)

    # Ensure uc_facs_candidates are not single letters (coded factors)
    uc_facs_candidates <- uc_facs_candidates[!grepl("^[A-Z]$", uc_facs_candidates)]

    uc_facs_candidates
  })

  # Update uc_facs checkboxGroupInput when data or selections change
  observeEvent(c(loaded_data(), input$response_var, input$time_var), {
    choices <- available_uc_facs()
    updateCheckboxGroupInput(session, "uc_facs",
      choices = choices,
      selected = choices, # Select all available by default
      inline = TRUE
    )
  })

  # Observe for save analysis button click
  observeEvent(input$save_analysis, {
    req(analysis_results$input_data) # Require data to be loaded
    showModal(doe_analysis_save_modal_ui(ns("save_modal"), i18n))
  })

  # Render filename preview in modal
  output[["save_modal-design_name_preview"]] <- renderUI({
    req(input[["save_modal-analysis_name"]])
    user_input_name <- input[["save_modal-analysis_name"]]

    # Basic sanitization for filename
    sanitized_name <- gsub("[^a-zA-Z0-9_.-]", "_", user_input_name)
    if (nchar(sanitized_name) == 0) {
      sanitized_name <- "untitled"
    }

    final_filename <- generate_filename_with_timestamp(paste0(sanitized_name, "_doe_analysis"))

    div(
      class = "alert alert-info",
      icon("info-circle"),
      strong(i18n$t("Final filename:")),
      br(),
      final_filename
    )
  })

  # Confirm save analysis from modal
  observeEvent(input[["save_modal-confirm_save_analysis"]], {
    req(analysis_results$input_data, input[["save_modal-analysis_name"]])

    removeModal() # Close the modal

    tryCatch(
      {
        # Determine data source reference and original file path
        data_source_ref <- NULL
        original_file_path <- NULL

        if (input$data_source == "saved") {
          data_source_ref <- input$saved_design
        } else if (input$data_source == "csv") {
          if (!is.null(input$import_file_csv$datapath)) {
            original_file_path <- input$import_file_csv$name # Save the original filename
          }
        }

        # Create analysis data structure
        analysis_settings <- list(
          data_source = input$data_source,
          parameters = list(
            response_var = input$response_var,
            time_var = input$time_var,
            mod_order = as.numeric(input$mod_order),
            p_cutoff = input$p_cutoff,
            trim_method = input$trim_method,
            uc_facs = input$uc_facs,
            which_facs = input$which_facs,
            canon_thres = "auto",
            export = "none",
            verbose = FALSE
          ),
          data = list(
            source_reference = data_source_ref,
            original_file = original_file_path, # Add original file path for CSV
            table = analysis_results$input_data # The actual data.frame
          ),
          metadata = list(
            created_date = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            app_version = "0.9.0"
          )
        )

        # Generate filename using user input and timestamp
        user_input_name <- input[["save_modal-analysis_name"]]
        sanitized_name <- gsub("[^a-zA-Z0-9_.-]", "_", user_input_name)
        if (nchar(sanitized_name) == 0) {
          sanitized_name <- "untitled"
        }
        filename <- generate_filename_with_timestamp(paste0(sanitized_name, "_doe_analysis"))

        # Save using settings utils
        save_settings("doe_analysis", analysis_settings, filename)

        showNotification(i18n$t("Analysis settings saved successfully!"), type = "message")
      },
      error = function(e) {
        showNotification(paste(i18n$t("Error saving analysis settings:"), e$message), type = "error")
      }
    )
  })

  # Modal for column configuration
  observeEvent(input$open_col_config, {
    req(analysis_results$input_data)
    showModal(modalDialog(
      title = i18n$t("Column Configuration"),
      size = "m",
      easyClose = FALSE,
      uiOutput(ns("col_config_modal_content")),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(ns("close_modal"), "Close", class = "btn btn-default")
      )
    ), session = session)

    # Initial disable state - after UI renders
    shinyjs::delay(500, {
      apply_id <- ns("apply_single")
      add_id <- ns("add_column_btn")
      shinyjs::runjs(sprintf("console.log('Disabling apply button: %s'); $('#%s').prop('disabled', true).addClass('btn-disabled');", apply_id, apply_id))
      shinyjs::runjs(sprintf("console.log('Disabling add button: %s'); $('#%s').prop('disabled', true).addClass('btn-disabled');", add_id, add_id))
    })
  })


  # Apply button validation
  observe({
    # Only validate when modal inputs exist
    req(input$selected_col, input$operation)

    selected_col <- input$selected_col
    operation <- input$operation
    new_name <- input$new_name # May be NULL if not rendered

    # Debug
    cat("Apply validation - selected_col:", selected_col, "operation:", operation, "new_name:", new_name, "\n")

    # Check conditions
    col_selected <- !is.null(selected_col) && selected_col != "" && selected_col != "Choose..."
    name_valid <- TRUE
    if (!is.null(operation) && operation == "rename") {
      name_valid <- !is.null(new_name) && new_name != ""
    }

    apply_valid <- col_selected && name_valid
    cat("Apply button state:", apply_valid, "\n")

    # Update button state
    if (apply_valid) {
      apply_id <- ns("apply_single")
      shinyjs::runjs(sprintf("console.log('Enabling apply button: %s'); $('#%s').prop('disabled', false).removeClass('btn-disabled');", apply_id, apply_id))
    } else {
      apply_id <- ns("apply_single")
      shinyjs::runjs(sprintf("console.log('Disabling apply button: %s'); $('#%s').prop('disabled', true).addClass('btn-disabled');", apply_id, apply_id))
    }
  })

  # Add button validation
  observe({
    # Only validate when modal input exists
    req(input$add_new_col)

    add_col <- input$add_new_col

    # Debug
    cat("Add validation - add_col:", add_col, "\n")

    add_valid <- !is.null(add_col) && add_col != ""
    cat("Add button state:", add_valid, "\n")

    # Update button state - removed ns() wrapper
    if (add_valid) {
      add_id <- ns("add_column_btn")
      shinyjs::runjs(sprintf("console.log('Enabling add button: %s'); $('#%s').prop('disabled', false).removeClass('btn-disabled');", add_id, add_id))
    } else {
      add_id <- ns("add_column_btn")
      shinyjs::runjs(sprintf("console.log('Disabling add button: %s'); $('#%s').prop('disabled', true).addClass('btn-disabled');", add_id, add_id))
    }
  })

  observeEvent(input$close_modal, {
    removeModal(session)
  })

  # Modal content UI
  output$col_config_modal_content <- renderUI({
    data <- analysis_results$input_data
    if (is.null(data) || !is.data.frame(data) || ncol(data) == 0) {
      return(NULL)
    }

    col_names <- names(data)
    tagList(
      fluidRow(
        column(12, p("Select a column to modify:", style = "font-weight: bold;"))
      ),
      fluidRow(
        column(6, selectInput(ns("selected_col"), "Select Column:", choices = c("Choose..." = "", col_names))),
        column(6, radioButtons(ns("operation"), "Operation:",
          choices = c("Rename" = "rename", "Remove" = "remove"),
          selected = "rename", inline = TRUE
        ))
      ),
      fluidRow(
        column(6, conditionalPanel(
          condition = sprintf("input.%s == 'rename'", ns("operation")),
          textInput(ns("new_name"), "New Name:", value = "", placeholder = "Enter new name")
        )),
        column(6, actionButton(ns("apply_single"), "Apply", class = "btn btn-primary btn-sm transition-button", style = "margin-top: 25px;color:white !important;"))
      ),
      hr(),
      fluidRow(
        column(6, textInput(ns("add_new_col"), "Add New Column:", value = "", placeholder = "Enter column name")),
        column(6, actionButton(ns("add_column_btn"), "Add", class = "btn btn-success btn-sm transition-button", style = "margin-top: 25px;"))
      ),
      br(),
      p("Changes will be applied to the data preview immediately.", style = "font-style: italic; color: #666;")
    )
  })

  # Note: Button validation observers are handled by setup_button_validator inside modal open event
  # The actual action logic remains in the existing observeEvents below

  # CSS for modal backdrop opacity and button validation
  tags$head(
    tags$style(HTML("
      .modal-backdrop {
        opacity: 0.3 !important;
      }
      .modal-dialog {
        margin-top: 50px;
      }
    ")),
    includeCSS("./www/css/button_validation.css")
  )

  # Data preview with rhandsontable
  output$data_preview <- renderRHandsontable({
    data <- analysis_results$input_data
    if (!is.null(data)) {
      # print("Rendering rHandsontable with new data.") # Debugging line
      rhandsontable(data, height = 400) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
        hot_cols(columnSorting = TRUE)
    } else {
      # print("Rendering rHandsontable with 'No data loaded' message.") # Debugging line
      rhandsontable(data.frame(Message = i18n$t("No data loaded")), height = 400)
    }
  })

  # Helper function to rename a column
  rename_column <- function(data, old_name, new_name) {
    if (!(old_name %in% names(data))) {
      stop(i18n$t(paste0("Column '", old_name, "' not found for renaming.")))
    }
    if (new_name %in% names(data) && new_name != old_name) {
      stop(i18n$t(paste0("New column name '", new_name, "' already exists.")))
    }
    names(data)[names(data) == old_name] <- new_name
    data
  }

  # Helper function to add a column
  add_column <- function(data, new_name) {
    if (new_name %in% names(data)) {
      stop(i18n$t(paste0("Column '", new_name, "' already exists.")))
    }
    data[[new_name]] <- NA
    data
  }

  # Helper function to remove a column
  remove_column <- function(data, col_name, response_var, time_var) {
    if (!(col_name %in% names(data))) {
      stop(i18n$t(paste0("Column '", col_name, "' not found for removal.")))
    }
    if (col_name == response_var) {
      stop(i18n$t(paste0("Cannot remove response variable '", col_name, "'.")))
    }
    if (col_name == time_var) {
      stop(i18n$t(paste0("Cannot remove time variable '", col_name, "'.")))
    }
    data <- data %>% select(-!!sym(col_name))
    data
  }

  # Handle rhandsontable edits
  observeEvent(input$data_preview, {
    if (!is.null(input$data_preview)) {
      analysis_results$input_data <- hot_to_r(input$data_preview)
    }
  })

  # Observe for single column operations (rename/remove)
  observeEvent(input$apply_single, {
    req(analysis_results$input_data, input$selected_col, input$operation)
    current_data <- analysis_results$input_data
    selected_col <- input$selected_col
    operation <- input$operation
    response_var <- input$response_var
    time_var <- input$time_var

    tryCatch(
      {
        if (operation == "rename") {
          req(input$new_name)
          new_name <- input$new_name
          if (new_name == "") {
            stop("New name cannot be empty.")
          }
          current_data <- rename_column(current_data, selected_col, new_name)
          showNotification(i18n$t("Column renamed successfully."), type = "message")
        } else if (operation == "remove") {
          current_data <- remove_column(current_data, selected_col, response_var, time_var)
          showNotification(i18n$t("Column removed successfully."), type = "message")
        }

        # Update data
        analysis_results$input_data <- as.data.frame(current_data)

        # Update variable selections
        var_names <- names(current_data)
        updateSelectInput(session, "time_var", choices = var_names, selected = time_var)
        updateSelectInput(session, "response_var", choices = c("None" = "", var_names), selected = response_var)

        # Reset form
        updateSelectInput(session, "selected_col", selected = "")
        updateTextInput(session, "new_name", value = "")

        # Close modal on success
        removeModal(session)
      },
      error = function(e) {
        showNotification(paste(i18n$t("Error:"), e$message), type = "error")
      }
    )
  })

  # Observe for adding new column
  observeEvent(input$add_column_btn, {
    req(analysis_results$input_data, input$add_new_col)
    current_data <- analysis_results$input_data
    new_col_name <- input$add_new_col

    if (new_col_name == "") {
      showNotification(i18n$t("Column name cannot be empty."), type = "error")
      return()
    }

    tryCatch(
      {
        current_data <- add_column(current_data, new_col_name)
        analysis_results$input_data <- as.data.frame(current_data)

        # Update variable selections
        var_names <- names(current_data)
        updateSelectInput(session, "time_var", choices = var_names, selected = input$time_var)
        updateSelectInput(session, "response_var", choices = c("None" = "", var_names), selected = input$response_var)

        showNotification(paste(i18n$t("New column added:"), new_col_name), type = "message")

        # Reset input
        updateTextInput(session, "add_new_col", value = "")

        # Close modal on success
        removeModal(session)
      },
      error = function(e) {
        showNotification(paste(i18n$t("Error adding column:"), e$message), type = "error")
      }
    )
  })

  # Run analysis
  observeEvent(input$analyze, {
    tryCatch(
      {
        req(analysis_results$input_data, input$response_var)

        # Show progress
        withProgress(message = i18n$t("Running analysis..."), value = 0, {
          # Prepare data
          incProgress(0.2, detail = i18n$t("Preparing data..."))
          data <- analysis_results$input_data

          # Prepare uncoded factors from input
          selected_uc_facs <- input$uc_facs

          # Validation: if which_facs is "uncoded", at least one uc_facs must be selected
          if (input$which_facs == "uncoded" && length(selected_uc_facs) == 0) {
            showNotification(i18n$t("Please select at least one uncoded factor when 'Factor Type' is 'Uncoded'."), type = "error")
            return()
          }


          # Run DOE analysis
          incProgress(0.6, detail = i18n$t("Running DOE analysis..."))
          result <- doe_analyze(
            doe = data,
            uc_facs = if (length(selected_uc_facs) == 0) NA else selected_uc_facs, # Pass NA if no uncoded factors selected
            cent_id = NA,
            resp_var = input$response_var,
            time_var = if (input$time_var == "") NULL else input$time_var,
            mod_order = as.numeric(input$mod_order),
            canon_thres = "auto",
            p_cutoff = input$p_cutoff,
            trim_method = input$trim_method,
            which_facs = input$which_facs,
            export = "none",
            asprat = "default",
            verbose = FALSE
          )


          # Store results
          analysis_results$analysis_result <- result

          analysis_results$response_var <- input$response_var
          analysis_results$time_var <- input$time_var

  

          incProgress(1, detail = i18n$t("Analysis completed!"))
        })

        showNotification(i18n$t("Analysis completed successfully!"), type = "message")
      },
      error = function(e) {
        showNotification(paste(i18n$t("Analysis error:"), e$message), type = "error")
      }
    )
  })


  # Initial Model Summary
  output$initial_model_summary <- renderUI({
    req(analysis_results$analysis_result)
    result <- analysis_results$analysis_result
    init_metrics <- result$results$initial$Model_Metrics
    init_misc <- result$results$initial$Misc

    # LoF badge color - handle NaN values
    lof_color <- if (is.na(init_metrics$LoF_Pvalue) || is.nan(init_metrics$LoF_Pvalue)) {
      "warning"
    } else if (init_metrics$LoF_Pvalue > 0.05) {
      "success"
    } else {
      "danger"
    }
    lof_display <- if (is.na(init_metrics$LoF_Pvalue) || is.nan(init_metrics$LoF_Pvalue)) {
      "N/A"
    } else {
      sprintf("%.4f", init_metrics$LoF_Pvalue)
    }
    lof_badge <- sprintf('<span class="badge badge-%s">%s</span>', lof_color, lof_display)

    html_content <- paste0(
      '<div style="font-size: 14px;">',
      "<p><strong>Response Variable:</strong> ", analysis_results$response_var, "</p>",
      "<p><strong>Model Order:</strong> ", init_metrics$Order, "</p>",
      "<hr>",
      "<p><strong>R²:</strong> ", sprintf("%.4f", init_metrics$R2), "</p>",
      "<p><strong>Adjusted R²:</strong> ", sprintf("%.4f", init_metrics$Adj_R2), "</p>",
      "<p><strong>F-statistic:</strong> ", sprintf("%.2f", init_metrics$F_Statistic),
      " (df1=", init_metrics$F_DOF_1, ", df2=", init_metrics$F_DOF_2, ")</p>",
      "<p><strong>Residual Std Error:</strong> ", sprintf("%.4f", init_metrics$Residual_Stnd_Error), "</p>",
      "<p><strong>Lack-of-Fit p-value:</strong> ", lof_badge, "</p>",
      "<hr>",
      "<p><strong>Model Equation:</strong></p>",
      '<pre style="background-color: #f5f5f5; padding: 10px; border-radius: 4px; font-size: 12px;">',
      init_misc$Equation[["raw"]], "</pre>",
      "</div>"
    )

    HTML(html_content)
  })

  # Final Model Summary
  output$final_model_summary <- renderUI({
    req(analysis_results$analysis_result)
    result <- analysis_results$analysis_result
    fin_metrics <- result$results$final$Model_Metrics
    fin_misc <- result$results$final$Misc

    # LoF badge color - handle NaN values
    lof_color <- if (is.na(fin_metrics$LoF_Pvalue) || is.nan(fin_metrics$LoF_Pvalue)) {
      "warning"
    } else if (fin_metrics$LoF_Pvalue > 0.05) {
      "success"
    } else {
      "danger"
    }
    lof_display <- if (is.na(fin_metrics$LoF_Pvalue) || is.nan(fin_metrics$LoF_Pvalue)) {
      "N/A"
    } else {
      sprintf("%.4f", fin_metrics$LoF_Pvalue)
    }
    lof_badge <- sprintf('<span class="badge badge-%s">%s</span>', lof_color, lof_display)

    html_content <- paste0(
      '<div style="font-size: 14px;">',
      "<p><strong>Response Variable:</strong> ", analysis_results$response_var, "</p>",
      "<p><strong>Model Order:</strong> ", fin_metrics$Order, "</p>",
      "<hr>",
      "<p><strong>R²:</strong> ", sprintf("%.4f", fin_metrics$R2), "</p>",
      "<p><strong>Adjusted R²:</strong> ", sprintf("%.4f", fin_metrics$Adj_R2), "</p>",
      "<p><strong>F-statistic:</strong> ", sprintf("%.2f", fin_metrics$F_Statistic),
      " (df1=", fin_metrics$F_DOF_1, ", df2=", fin_metrics$F_DOF_2, ")</p>",
      "<p><strong>Residual Std Error:</strong> ", sprintf("%.4f", fin_metrics$Residual_Stnd_Error), "</p>",
      "<p><strong>Lack-of-Fit p-value:</strong> ", lof_badge, "</p>",
      "<hr>",
      "<p><strong>Model Equation:</strong></p>",
      '<pre style="background-color: #f5f5f5; padding: 10px; border-radius: 4px; font-size: 12px;">',
      fin_misc$Equation[["raw"]], "</pre>",
      "</div>"
    )

    HTML(html_content)
  })

  # Trimming Information
  output$trimming_info <- renderUI({
    req(analysis_results$analysis_result)
    result <- analysis_results$analysis_result
    statements <- result$statements

    # Collect trimming information
    trim_info <- c()
    trim_badge <- '<span class="badge badge-success">NO</span>' # Default to NO

    if ("Notrim" %in% names(statements)) {
      trim_info <- c(trim_info, paste0("<p>", statements["Notrim"], "</p>"))
    } else {
      trim_badge <- '<span class="badge badge-warning">YES</span>'
    }

    if ("p_cutoff" %in% names(statements)) {
      trim_info <- c(trim_info, paste0("<p><strong>P-value Cutoff:</strong><br>", statements["p_cutoff"], "</p>"))
    }

    if ("Stepwise" %in% names(statements)) {
      trim_info <- c(trim_info, paste0("<p><strong>Stepwise Regression:</strong><br>", statements["Stepwise"], "</p>"))
    }

    if ("Signif_Effs" %in% names(statements)) {
      trim_info <- c(trim_info, paste0("<p><strong>Effects Removed:</strong><br>", statements["Signif_Effs"], "</p>"))
    }

    if ("H_Principle" %in% names(statements)) {
      trim_info <- c(trim_info, paste0("<p><strong>Hierarchy Principle:</strong><br>", statements["H_Principle"], "</p>"))
    }

    html_content <- paste0(
      '<div style="font-size: 14px;">',
      "<p><strong>Model Was Trimmed:</strong> ", trim_badge, "</p>",
      "<hr>",
      paste(trim_info, collapse = "<hr>"),
      "</div>"
    )

    HTML(html_content)
  })

  # FAZE 2: Optimization Results Tab Outputs

  # Optimization Type
  output$optimization_type <- renderUI({
    req(analysis_results$analysis_result)
    result <- analysis_results$analysis_result

    # Check if canonical analysis exists (quadratic model)
    if (!is.null(result$results$final$Model_Metrics$Canonical_Analysis)) {
      ca <- result$results$final$Model_Metrics$Canonical_Analysis
      eigen_vals <- ca$eigen$values

      # Determine type from eigenvalues
      all_neg <- all(eigen_vals < 0)
      all_pos <- all(eigen_vals > 0)

      type <- if (all_neg) {
        "Maximum"
      } else if (all_pos) {
        "Minimum"
      } else {
        "Saddle Point"
      }

      # Badge color
      badge_color <- if (all_neg) "success" else if (all_pos) "primary" else "warning"

      html_content <- paste0(
        '<div style="text-align: center; padding: 20px;">',
        '<h2><span class="badge badge-', badge_color, '" style="font-size: 24px;">', type, "</span></h2>",
        '<p style="margin-top: 15px; font-size: 14px;">',
        "Based on eigenvalue analysis of the response surface",
        "</p>",
        "</div>"
      )
    } else {
      html_content <- paste0(
        '<div style="text-align: center; padding: 20px;">',
        '<p class="text-muted">Canonical analysis not available for this model order</p>',
        "</div>"
      )
    }

    HTML(html_content)
  })

  # Predicted Response
  output$predicted_response <- renderUI({
    req(analysis_results$analysis_result)
    result <- analysis_results$analysis_result

    if (!is.null(result$results$final$Model_Metrics$Canonical_Analysis)) {
      ca <- result$results$final$Model_Metrics$Canonical_Analysis
      predicted <- ca$predicted

      html_content <- paste0(
        '<div style="text-align: center; padding: 20px;">',
        '<h2 style="color: #3c8dbc;">', sprintf("%.4f", predicted), "</h2>",
        '<p style="margin-top: 15px; font-size: 14px;">',
        "Predicted ", analysis_results$response_var, " at stationary point",
        "</p>",
        "</div>"
      )
    } else {
      html_content <- paste0(
        '<div style="text-align: center; padding: 20px;">',
        '<p class="text-muted">Not available</p>',
        "</div>"
      )
    }

    HTML(html_content)
  })

  # Optimal Conditions Detailed (Coded & Decoded)
  output$optimal_conditions_detailed <- DT::renderDataTable(
    {
      req(analysis_results$analysis_result)
      result <- analysis_results$analysis_result
      ca <- result$results$final$Model_Metrics$Canonical_Analysis

      if (!is.null(result$results$final$Model_Metrics$Canonical_Analysis)) {
        optimal_df <- if (!is.null(ca$xs_decoded)) {
          data.frame(
            Factor = names(ca$xs),
            Coded = round(as.vector(ca$xs), 4),
            Decoded = round(as.vector(ca$xs_decoded), 4),
            stringsAsFactors = FALSE
          )
        } else {
          data.frame(
            Factor = names(ca$xs),
            Coded = round(as.vector(ca$xs), 4),
            Decoded = rep(NA, length(ca$xs)), # veya atlamak için bu kısmı çıkar
            stringsAsFactors = FALSE
          )
        }

        DT::datatable(
          optimal_df,
          options = list(
            dom = "t",
            pageLength = 20
          ),
          rownames = FALSE
        )
      } else {
        DT::datatable(
          data.frame(Message = "Canonical analysis not available for this model"),
          options = list(dom = "t"),
          rownames = FALSE
        )
      }
    },
    server = FALSE
  )

  # Min/Max Comparison
  output$minmax_comparison <- DT::renderDataTable(
    {
      req(analysis_results$analysis_result)
      result <- analysis_results$analysis_result

      if (!is.null(result$results$final$Model_Metrics$Trad_Opt)) {
        trad_opt <- result$results$final$Model_Metrics$Trad_Opt

        # Get coded values
        coded_df <- trad_opt$coded

        # Create comparison dataframe
        if (nrow(coded_df) >= 2) {
          # Get factor names (exclude response column)
          factor_cols <- setdiff(names(coded_df), analysis_results$response_var)

          # Build comparison table
          comparison_list <- list(
            Type = c("Minimum", "Maximum"),
            Response = round(coded_df[[analysis_results$response_var]], 4)
          )

          # Add factor values
          for (col in factor_cols) {
            comparison_list[[col]] <- round(coded_df[[col]], 4)
          }

          comparison_df <- as.data.frame(comparison_list, stringsAsFactors = FALSE)

          DT::datatable(
            comparison_df,
            options = list(
              dom = "t",
              pageLength = 10
            ),
            rownames = FALSE
          )
        } else {
          DT::datatable(
            data.frame(Message = "Insufficient optimization data"),
            options = list(dom = "t"),
            rownames = FALSE
          )
        }
      } else {
        DT::datatable(
          data.frame(Message = "Traditional optimization not available"),
          options = list(dom = "t"),
          rownames = FALSE
        )
      }
    },
    server = FALSE
  )

  # Eigenvalues Table
  output$eigenvalues_table <- DT::renderDataTable(
    {
      req(analysis_results$analysis_result)
      result <- analysis_results$analysis_result

      if (!is.null(result$results$final$Model_Metrics$Canonical_Analysis)) {
        ca <- result$results$final$Model_Metrics$Canonical_Analysis

        eigen_df <- data.frame(
          Factor = names(ca$xs),
          Eigenvalue = round(ca$eigen$values, 6),
          Sign = ifelse(ca$eigen$values > 0, "Positive (+)",
            ifelse(ca$eigen$values < 0, "Negative (-)", "Zero (0)")
          ),
          stringsAsFactors = FALSE
        )

        DT::datatable(
          eigen_df,
          options = list(
            dom = "t",
            pageLength = 20
          ),
          rownames = FALSE
        ) %>%
          DT::formatStyle(
            "Sign",
            backgroundColor = DT::styleEqual(
              c("Positive (+)", "Negative (-)", "Zero (0)"),
              c("#d4edda", "#f8d7da", "#fff3cd")
            )
          )
      } else {
        DT::datatable(
          data.frame(Message = "Eigenvalue analysis not available"),
          options = list(dom = "t"),
          rownames = FALSE
        )
      }
    },
    server = FALSE
  )

  # Range Check and Warnings
  output$ca_warnings <- renderUI({
    req(analysis_results$analysis_result)
    result <- analysis_results$analysis_result
    statements <- result$statements

    # Collect warnings
    warnings <- c()

    # Check for out of range warnings (Optim_3 and Optim_4)
    if ("Optim_3_final" %in% names(statements)) {
      warnings <- c(warnings, statements["Optim_3_final"])
    }

    if ("Optim_4_final" %in% names(statements)) {
      warnings <- c(warnings, statements["Optim_4_final"])
    }

    # Check eigenvalue signs (Optim_4 usually contains this)
    eigen_warning <- grep("eigen", statements, ignore.case = TRUE, value = TRUE)
    if (length(eigen_warning) > 0) {
      warnings <- c(warnings, eigen_warning)
    }

    if (length(warnings) > 0) {
      # Check if warnings indicate problems
      has_issues <- any(grepl("out of range|positive", warnings, ignore.case = TRUE))

      alert_class <- if (has_issues) "alert-warning" else "alert-info"
      icon_class <- if (has_issues) "exclamation-triangle" else "info-circle"

      html_content <- paste0(
        '<div class="alert ', alert_class, '" style="margin: 10px;">',
        '<h4><i class="fa fa-', icon_class, '"></i> Analysis Notes</h4>',
        paste(warnings, collapse = "<br><br>"),
        "</div>"
      )
    } else {
      html_content <- paste0(
        '<div class="alert alert-success" style="margin: 10px;">',
        '<h4><i class="fa fa-check-circle"></i> All Checks Passed</h4>',
        "<p>All optimal values are within the specified range.</p>",
        "<p>Eigenvalue analysis confirms the stationary point type.</p>",
        "</div>"
      )
    }

    HTML(html_content)
  })


  # Steepest Ascent Table
  output$steepest_ascent_table <- DT::renderDataTable(
    {
      req(analysis_results$analysis_result)
      result <- analysis_results$analysis_result

      if (!is.null(result$results$final$Model_Metrics$Steepest_Ascent)) {
        steep_df <- result$results$final$Model_Metrics$Steepest_Ascent
        steep_df <- steep_df %>%
          mutate(!!sym(analysis_results$response_var) := round(.data[[!!sym(analysis_results$response_var)]], 3))


        DT::datatable(
          steep_df,
          options = list(
            dom = "t",
            pageLength = 10
          ),
          rownames = FALSE
        )
      } else {
        DT::datatable(
          data.frame(Message = "Steepest Ascent not available for this model"),
          options = list(dom = "t"),
          rownames = FALSE
        )
      }
    },
    server = FALSE
  )


  # Total Result - Formatted HTML Output
  output$formatted_full_report <- renderUI({
    req(analysis_results$analysis_result)
    result <- analysis_results$analysis_result
    statements <- result$statements

    if (!is.null(statements)) {
      # Helper function to format individual statements with styling
      format_statement <- function(name, text) {
        section_class <- "general-statement" # Default class

        # Assign classes based on statement type for styling
        if (grepl("Inestimable|Mod_initial|Mod_final", name)) {
          section_class <- "model-summary"
        } else if (grepl("Eq_initial|Eq_final", name)) {
          section_class <- "model-equation"
        } else if (grepl("R2_vs_adjR2_initial|LoF_initial|R2_vs_adjR2_final|LoF_final", name)) {
          section_class <- "model-performance"
        } else if (grepl("Notrim|Signif_Effs|H_Principle", name)) {
          section_class <- "trimming-info"
        } else if (grepl("Optim_|TradOpt_", name)) {
          section_class <- "optimization-details"
        }

        # Add specific styling for warnings/notes
        if (grepl("NOTE:", text, ignore.case = TRUE) || grepl("error", text, ignore.case = TRUE)) {
          section_class <- paste(section_class, "warning-statement")
        }

        # Highlighted keywords for better readability
        text <- gsub("R\\^2", "<strong>R<sup>2</sup></strong>", text, ignore.case = TRUE)
        text <- gsub("Adj\\. R\\^2", "<strong>Adj. R<sup>2</sup></strong>", text, ignore.case = TRUE)
        text <- gsub("Lack-of-Fit", "<strong>Lack-of-Fit</strong>", text, ignore.case = TRUE)
        text <- gsub("F-statistic", "<strong>F-statistic</strong>", text, ignore.case = TRUE)
        text <- gsub("eigenvalues", "<strong>eigenvalues</strong>", text, ignore.case = TRUE)
        text <- gsub("stationary point", "<strong>stationary point</strong>", text, ignore.case = TRUE)
        text <- gsub("maximum", "<span class='highlight-max'>maximum</span>", text, ignore.case = TRUE)
        text <- gsub("minimum", "<span class='highlight-min'>minimum</span>", text, ignore.case = TRUE)
        text <- gsub("out of range", "<span class='highlight-warning'>out of range</span>", text, ignore.case = TRUE)


        return(HTML(paste0('<div class="statement-item ', section_class, '"><p>', text, "</p></div>")))
      }

      # Define logical sections and their member statement names (order matters)
      sections_config <- list(
        list(
          title = "1. Model Building Overview",
          statements = c("Inestimable", "Mod_initial", "Mod_final")
        ),
        list(
          title = "2. Model Equations",
          statements = c("Eq_initial", "Eq_final")
        ),
        list(
          title = "3. Model Performance",
          statements = c("R2_vs_adjR2_initial", "LoF_initial", "R2_vs_adjR2_final", "LoF_final")
        ),
        list(
          title = "4. Model Trimming and Simplification",
          statements = c("Notrim", "Signif_Effs", "H_Principle")
        ),
        list(
          title = "5. Canonical Analysis (Optimization)",
          statements = c(
            "Optim_1_initial", "Optim_2_initial", "Optim_3_initial", "Optim_4_initial", "Optim_5_initial",
            "Optim_1_final", "Optim_2_final", "Optim_3_final", "Optim_4_final", "Optim_5_final"
          )
        ),
        list(
          title = "6. Traditional Min/Max Optimization",
          statements = c("TradOpt_1_initial", "TradOpt_2_initial", "TradOpt_1_final", "TradOpt_2_final")
        )
      )

      full_html_parts <- list()
      # Add overarching report title
      full_html_parts[[length(full_html_parts) + 1]] <- div(class = "main-report-title", h2("Comprehensive Design of Experiments Analysis Report"))


      # Process each defined section
      for (section_cfg in sections_config) {
        section_html_content <- list()

        # Filter statements relevant to the current section
        current_section_statements <- statements[names(statements) %in% section_cfg$statements]

        if (length(current_section_statements) > 0) {
          # Add section title
          section_html_content[[length(section_html_content) + 1]] <- h3(section_cfg$title)

          # Add formatted statements for this section
          for (i in seq_along(current_section_statements)) {
            section_html_content[[length(section_html_content) + 1]] <- format_statement(names(current_section_statements)[i], current_section_statements[i])
          }
          full_html_parts[[length(full_html_parts) + 1]] <- div(class = "report-section", section_html_content)
        }
      }

      # Combine all parts into a single tagList and render
      return(tagList(full_html_parts))
    } else {
      return(p("No detailed analysis report available."))
    }
  })

  # --- Download Handlers for Full Report

  # Download Formatted Report as HTML


  # Download Formatted Report as HTML
  output$download_formatted_report <- downloadHandler(
    filename = function() {
      paste0(generate_filename_with_timestamp("supercrit_doe_analysis_report"), ".html")
    },
    content = function(file) {
      req(analysis_results$analysis_result)

      # Soluk: renderUI mantığını burada yeniden tanımla
      result <- analysis_results$analysis_result
      statements <- result$statements

      # Helper function to format individual statements with styling
      format_statement <- function(name, text) {
        section_class <- "general-statement" # Default class

        # Assign classes based on statement type for styling
        if (grepl("Inestimable|Mod_initial|Mod_final", name)) {
          section_class <- "model-summary"
        } else if (grepl("Eq_initial|Eq_final", name)) {
          section_class <- "model-equation"
        } else if (grepl("R2_vs_adjR2_initial|LoF_initial|R2_vs_adjR2_final|LoF_final", name)) {
          section_class <- "model-performance"
        } else if (grepl("Notrim|Signif_Effs|H_Principle", name)) {
          section_class <- "trimming-info"
        } else if (grepl("Optim_|TradOpt_", name)) {
          section_class <- "optimization-details"
        }

        # Add specific styling for warnings/notes
        if (grepl("NOTE:", text, ignore.case = TRUE) || grepl("error", text, ignore.case = TRUE)) {
          section_class <- paste(section_class, "warning-statement")
        }

        # Highlighted keywords for better readability
        text <- gsub("R\\^2", "<strong>R<sup>2</sup></strong>", text, ignore.case = TRUE)
        text <- gsub("Adj\\. R\\^2", "<strong>Adj. R<sup>2</sup></strong>", text, ignore.case = TRUE)
        text <- gsub("Lack-of-Fit", "<strong>Lack-of-Fit</strong>", text, ignore.case = TRUE)
        text <- gsub("F-statistic", "<strong>F-statistic</strong>", text, ignore.case = TRUE)
        text <- gsub("eigenvalues", "<strong>eigenvalues</strong>", text, ignore.case = TRUE)
        text <- gsub("stationary point", "<strong>stationary point</strong>", text, ignore.case = TRUE)
        text <- gsub("maximum", "<span class='highlight-max'>maximum</span>", text, ignore.case = TRUE)
        text <- gsub("minimum", "<span class='highlight-min'>minimum</span>", text, ignore.case = TRUE)
        text <- gsub("out of range", "<span class='highlight-warning'>out of range</span>", text, ignore.case = TRUE)

        return(tags$div(class = paste("statement-item", section_class), tags$p(HTML(text))))
      }

      # Define logical sections and their member statement names (order matters)
      sections_config <- list(
        list(
          title = "1. Model Building Overview",
          statements = c("Inestimable", "Mod_initial", "Mod_final")
        ),
        list(
          title = "2. Model Equations",
          statements = c("Eq_initial", "Eq_final")
        ),
        list(
          title = "3. Model Performance",
          statements = c("R2_vs_adjR2_initial", "LoF_initial", "R2_vs_adjR2_final", "LoF_final")
        ),
        list(
          title = "4. Model Trimming and Simplification",
          statements = c("Notrim", "Signif_Effs", "H_Principle")
        ),
        list(
          title = "5. Canonical Analysis (Optimization)",
          statements = c(
            "Optim_1_initial", "Optim_2_initial", "Optim_3_initial", "Optim_4_initial", "Optim_5_initial",
            "Optim_1_final", "Optim_2_final", "Optim_3_final", "Optim_4_final", "Optim_5_final"
          )
        ),
        list(
          title = "6. Traditional Min/Max Optimization",
          statements = c("TradOpt_1_initial", "TradOpt_2_initial", "TradOpt_1_final", "TradOpt_2_final")
        )
      )

      if (!is.null(statements) && length(statements) > 0) {
        full_html_parts <- list()
        # Add overarching report title
        full_html_parts[[length(full_html_parts) + 1]] <- tags$div(class = "main-report-title", tags$h2("Comprehensive Design of Experiments Analysis Report"))

        # Process each defined section
        for (section_cfg in sections_config) {
          section_html_content <- list()

          # Filter statements relevant to the current section
          current_section_statements <- statements[names(statements) %in% section_cfg$statements]

          if (length(current_section_statements) > 0) {
            # Add section title
            section_html_content[[length(section_html_content) + 1]] <- tags$h3(section_cfg$title)

            # Add formatted statements for this section
            for (i in seq_along(current_section_statements)) {
              section_html_content[[length(section_html_content) + 1]] <- format_statement(names(current_section_statements)[i], current_section_statements[i])
            }
            full_html_parts[[length(full_html_parts) + 1]] <- tags$div(class = "report-section", section_html_content)
          }
        }

        # Combine all parts into a single tagList
        report_content <- tagList(full_html_parts)

        # Convert tagList to HTML string safely
        tryCatch(
          {
            body_html <- htmltools::renderTags(report_content)$html
          },
          error = function(e) {
            body_html <- as.character(report_content) # Fallback
          }
        )

        # If still empty, provide a default message
        if (is.null(body_html) || body_html == "") {
          body_html <- "<p>No detailed analysis report available.This might be due to missing statements.</p>"
        }
      } else {
        body_html <- "<p>No detailed analysis report available. No statements found.</p>"
      }

      # Add basic HTML structure and embedded CSS for a standalone HTML file
      full_html <- paste0(
        "<!DOCTYPE html>",
        "<html><head><title>DOE Analysis Report</title>",
        # Embed the same CSS used in the UI for consistent styling
        "<style type='text/css'>",
        "body { font-family: 'Segoe UI', Arial, sans-serif; line-height: 1.6; margin: 20px; color: #333; background-color: #f8f9fa; }",
        ".main-report-title { text-align: center; margin-bottom: 30px; }",
        ".main-report-title h2 { color: #0056b3; font-size: 2.2em; border-bottom: 2px solid #0056b3; padding-bottom: 10px; margin-top: 20px; }",
        ".report-section { background-color: #f8f9fa; border: 1px solid #e9ecef; border-radius: 8px; padding: 20px; margin-bottom: 25px; box-shadow: 0 4px 8px rgba(0,0,0,0.05); }",
        ".report-section h3 { color: #007bff; font-size: 1.6em; border-bottom: 1px solid #dee2e6; padding-bottom: 8px; margin-top: 0; margin-bottom: 18px; }",
        ".statement-item { position: relative; padding: 12px 18px; margin-bottom: 12px; border-radius: 6px; border-left: 6px solid; box-shadow: 0 2px 4px rgba(0,0,0,0.03); background-color: #ffffff; }",
        ".statement-item p { margin: 0; line-height: 1.5; font-size: 0.95em; }",
        "strong { font-weight: 700; color: #000; }",
        ".model-summary { border-color: #007bff; background-color: #e7f3ff; }",
        ".model-equation { border-color: #343a40; background-color: #e2e6ea; }",
        ".model-performance { border-color: #28a745; background-color: #e6ffed; }",
        ".trimming-info { border-color: #ffc107; background-color: #fff8e6; }",
        ".optimization-details { border-color: #6f42c1; background-color: #f5f0fa; }",
        ".general-statement { border-color: #6c757d; background-color: #f0f2f5; }",
        ".warning-statement { border-color: #dc3545; background-color: #ffe0e4; color: #dc3545; }",
        ".highlight-max { color: #28a745; font-weight: bold; }",
        ".highlight-min { color: #007bff; font-weight: bold; }",
        ".highlight-warning { color: #dc3545; font-weight: bold; }",
        "</style>",
        "</head><body>",
        body_html,
        "</body></html>"
      )

      # Write to file
      writeLines(full_html, file)
    },
    contentType = "text/html"
  )


  # Download Raw Statements as TXT
  output$download_raw_statements <- downloadHandler(
    filename = function() {
      paste0(generate_filename_with_timestamp("supercrit_doe_analysis_raw_statements"), ".txt")
    },
    content = function(file) {
      req(analysis_results$analysis_result)
      result <- analysis_results$analysis_result
      statements <- result$statements
      if (!is.null(statements)) {
        combined_text <- paste(names(statements), statements, sep = ": ", collapse = "\n\n")
        header <- paste0(
          "DOE ANALYSIS - RAW STATEMENTS\n",
          "Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n",
          "========================================\n\n"
        )
        writeLines(paste0(header, combined_text), file)
      } else {
        writeLines("No raw statements available.", file)
      }
    },
    contentType = "text/plain"
  )

  # ... (sonraki kodlar) ...
  # Optimal conditions table canonical
  output$optimal_conditions <- DT::renderDataTable(
    {
      req(analysis_results$analysis_result)
      result <- analysis_results$analysis_result


      # Get optimal conditions from canonical analysis
      xs_decoded <- result$results$final$Model_Metrics$Canonical_Analysis$xs_decoded


      if (!is.null(xs_decoded) && length(xs_decoded) > 0) {
        optimal_df <- data.frame(
          Factor = names(xs_decoded),
          Optimal_Value = round(as.vector(xs_decoded), 4), # use as.vector()
          stringsAsFactors = FALSE
        )


        DT::datatable(
          optimal_df,
          options = list(
            pageLength = 10,
            dom = "t"
          )
        )
      } else {
        DT::datatable(
          data.frame(Factor = "No data", Optimal_Value = "No data"),
          options = list(pageLength = 10, dom = "t")
        )
      }
    },
    server = FALSE
  )

  # Canonical analysis summary (for Model Summary tab)
  output$canonical_analysis_summary <- renderUI({
    req(analysis_results$analysis_result)
    result <- analysis_results$analysis_result
    statements <- result$statements

    html_content <- ""
    if ("Optim_1_initial" %in% names(statements)) {
      html_content <- paste0(html_content, "<p><strong>Initial Model:</strong> ", statements["Optim_1_initial"], "</p>")
    }
    if ("Optim_1_final" %in% names(statements)) {
      html_content <- paste0(html_content, "<p><strong>Final Model:</strong> ", statements["Optim_1_final"], "</p>")
    }
    if (html_content == "") {
      html_content <- "<p class='text-muted'>Canonical analysis not available for this model order.</p>"
    }
    HTML(html_content)
  })

  # Diagnostic plots
  # Initial Model Plots
  output$initial_response_vs_predicted_plot <- renderPlotly({
    req(analysis_results$analysis_result)
    response <- input$response_var
    plot_name <- paste0("InitialModel_", response, "_vs_Predicted")

    p <- analysis_results$analysis_result$plots$initial[[plot_name]]

    if (!is.null(p) && inherits(p, "ggplot")) {
      plotly_p <- ggplotly(p)
      plotly_p <- add_formula_annotations(plotly_p, p)
      plotly_p
    } else {
      plotly_empty(type = "scatter", mode = "markers") %>%
        layout(title = "No plot available")
    }
  })


  output$initial_response_diagnostics_plot <- renderUI({
    req(analysis_results$analysis_result)
    response <- input$response_var
    plot_name <- paste0("Initial Model, ", response)



    grob_object <- analysis_results$analysis_result$plots$initial[[plot_name]]
    if (!is.null(grob_object)) {
      img_src <- render_grob_as_image(grob_object)
      tags$img(src = img_src, width = "100%")
    } else {
      p("No plot available.")
    }
  })

  output$initial_residual_vs_predicted_plot <- renderPlotly({
    req(analysis_results$analysis_result)

    p <- analysis_results$analysis_result$plots$initial$InitialModel_Residual_vs_Predicted
    if (!is.null(p) && inherits(p, "ggplot")) {
      ggplotly(p)
    } else {
      plotly_empty(type = "scatter", mode = "markers") %>%
        layout(title = "No plot available or not a ggplot object")
    }
  })

  output$initial_residual_diagnostics_plot <- renderUI({
    req(analysis_results$analysis_result)
    grob_object <- analysis_results$analysis_result$plots$initial$`Initial Model, Residual`
    if (!is.null(grob_object)) {
      img_src <- render_grob_as_image(grob_object)
      tags$img(src = img_src, width = "100%")
    } else {
      p("No plot available.")
    }
  })

  output$initial_cooks_plot <- renderPlotly({
    req(analysis_results$analysis_result)
    p <- analysis_results$analysis_result$plots$initial$Cooks_Distance_Plot

    if (!is.null(p) && inherits(p, "ggplot")) {
      plotly_p <- ggplotly(p)
      plotly_p <- add_layer_labels(plotly_p, p, layers_to_check = c(4, 5))
      plotly_p
    } else {
      plotly_empty(type = "scatter", mode = "markers") %>%
        layout(title = "No plot available or not a ggplot object")
    }
  })

  output$initial_pareto_plot <- renderPlotly({
    req(analysis_results$analysis_result)
    p <- analysis_results$analysis_result$plots$initial$Pareto_Plot_Initial_Model
    plotly_p <- ggplotly(p)
    plotly_p <- add_legend_and_caption(plotly_p, p)
    plotly_p
  })

  output$initial_response_vs_coded <- renderPlotly({
    req(analysis_results$analysis_result)
    response <- input$response_var
    plot_name <- paste0("InitialModel_", response, "_vs_Coded")

    p <- analysis_results$analysis_result$plots$initial[[plot_name]]

    if (!is.null(p) && inherits(p, "ggplot")) {
      plotly_p <- ggplotly(p)
    } else {
      plotly_empty(type = "scatter", mode = "markers") %>%
        layout(title = "No plot available")
    }
  })

  output$initial_response_vs_uncoded <- renderPlotly({
    req(analysis_results$analysis_result)
    response <- input$response_var
    plot_name <- paste0("InitialModel_", response, "_vs_Uncoded")

    p <- analysis_results$analysis_result$plots$initial[[plot_name]]

    if (!is.null(p) && inherits(p, "ggplot")) {
      plotly_p <- ggplotly(p)
    } else {
      plotly_empty(type = "scatter", mode = "markers") %>%
        layout(title = "No plot available")
    }
  })

  output$initial_residual_vs_coded <- renderPlotly({
    req(analysis_results$analysis_result)


    p <- analysis_results$analysis_result$plots$initial$InitialModel_Residual_vs_Coded

    if (!is.null(p) && inherits(p, "ggplot")) {
      plotly_p <- ggplotly(p)
    } else {
      plotly_empty(type = "scatter", mode = "markers") %>%
        layout(title = "No plot available")
    }
  })
  output$initial_residual_vs_uncoded <- renderPlotly({
    req(analysis_results$analysis_result)

    p <- analysis_results$analysis_result$plots$initial$InitialModel_Residual_vs_Uncoded

    if (!is.null(p) && inherits(p, "ggplot")) {
      plotly_p <- ggplotly(p)
    } else {
      plotly_empty(type = "scatter", mode = "markers") %>%
        layout(title = "No plot available")
    }
  })



  # Diagnostic plots
  # Final Model Plots
  output$final_response_vs_predicted_plot <- renderPlotly({
    req(analysis_results$analysis_result)
    response <- input$response_var
    plot_name <- paste0("FinalModel_", response, "_vs_Predicted")

    p <- analysis_results$analysis_result$plots$final[[plot_name]]

    if (!is.null(p) && inherits(p, "ggplot")) {
      plotly_p <- ggplotly(p)
      plotly_p <- add_formula_annotations(plotly_p, p)
      plotly_p
    } else {
      plotly_empty(type = "scatter", mode = "markers") %>%
        layout(title = "No plot available")
    }
  })


  output$final_response_diagnostics_plot <- renderUI({
    req(analysis_results$analysis_result)
    response <- input$response_var
    plot_name <- paste0("Final Model, ", response)



    grob_object <- analysis_results$analysis_result$plots$final[[plot_name]]
    if (!is.null(grob_object)) {
      img_src <- render_grob_as_image(grob_object)
      tags$img(src = img_src, width = "100%")
    } else {
      p("No plot available.")
    }
  })

  output$final_residual_vs_predicted_plot <- renderPlotly({
    req(analysis_results$analysis_result)

    p <- analysis_results$analysis_result$plots$final$FinalModel_Residual_vs_Predicted
    if (!is.null(p) && inherits(p, "ggplot")) {
      ggplotly(p)
    } else {
      plotly_empty(type = "scatter", mode = "markers") %>%
        layout(title = "No plot available or not a ggplot object")
    }
  })

  output$final_residual_diagnostics_plot <- renderUI({
    req(analysis_results$analysis_result)
    grob_object <- analysis_results$analysis_result$plots$final$`Final Model, Residual`
    if (!is.null(grob_object)) {
      img_src <- render_grob_as_image(grob_object)
      tags$img(src = img_src, width = "100%")
    } else {
      p("No plot available.")
    }
  })

  output$final_cooks_plot <- renderPlotly({
    req(analysis_results$analysis_result)
    p <- analysis_results$analysis_result$plots$final$Cooks_Distance_Plot

    if (!is.null(p) && inherits(p, "ggplot")) {
      plotly_p <- ggplotly(p)
      plotly_p <- add_layer_labels(plotly_p, p, layers_to_check = c(4, 5))
      plotly_p
    } else {
      plotly_empty(type = "scatter", mode = "markers") %>%
        layout(title = "No plot available or not a ggplot object")
    }
  })

  output$final_pareto_plot <- renderPlotly({
    req(analysis_results$analysis_result)
    p <- analysis_results$analysis_result$plots$final$Pareto_Plot_Final_Model
    plotly_p <- ggplotly(p)
    plotly_p <- add_legend_and_caption(plotly_p, p)
    plotly_p
  })

  output$final_response_vs_coded <- renderPlotly({
    req(analysis_results$analysis_result)
    response <- input$response_var
    plot_name <- paste0("FinalModel_", response, "_vs_Coded")

    p <- analysis_results$analysis_result$plots$final[[plot_name]]

    if (!is.null(p) && inherits(p, "ggplot")) {
      plotly_p <- ggplotly(p)
    } else {
      plotly_empty(type = "scatter", mode = "markers") %>%
        layout(title = "No plot available")
    }
  })

  output$final_response_vs_uncoded <- renderPlotly({
    req(analysis_results$analysis_result)
    response <- input$response_var
    plot_name <- paste0("FinalModel_", response, "_vs_Uncoded")

    p <- analysis_results$analysis_result$plots$final[[plot_name]]

    if (!is.null(p) && inherits(p, "ggplot")) {
      plotly_p <- ggplotly(p)
    } else {
      plotly_empty(type = "scatter", mode = "markers") %>%
        layout(title = "No plot available")
    }
  })

  output$final_residual_vs_coded <- renderPlotly({
    req(analysis_results$analysis_result)


    p <- analysis_results$analysis_result$plots$final$FinalModel_Residual_vs_Coded

    if (!is.null(p) && inherits(p, "ggplot")) {
      plotly_p <- ggplotly(p)
    } else {
      plotly_empty(type = "scatter", mode = "markers") %>%
        layout(title = "No plot available")
    }
  })
  output$final_residual_vs_uncoded <- renderPlotly({
    req(analysis_results$analysis_result)

    p <- analysis_results$analysis_result$plots$final$FinalModel_Residual_vs_Uncoded

    if (!is.null(p) && inherits(p, "ggplot")) {
      plotly_p <- ggplotly(p)
    } else {
      plotly_empty(type = "scatter", mode = "markers") %>%
        layout(title = "No plot available")
    }
  })






  # Model Equations
  output$model_equations <- renderUI({
    req(analysis_results$analysis_result)
    result <- analysis_results$analysis_result
    init_misc <- result$results$initial$Misc
    fin_misc <- result$results$final$Misc

    html_content <- paste0(
      '<div style="font-size: 14px;">',
      "<h4>Initial Model Equation:</h4>",
      '<pre style="background-color: #f5f5f5; padding: 10px; border-radius: 4px; font-size: 12px;">',
      init_misc$Equation[["raw"]], "</pre>",
      "<h4>Final Model Equation:</h4>",
      '<pre style="background-color: #f5f5f5; padding: 10px; border-radius: 4px; font-size: 12px;">',
      fin_misc$Equation[["raw"]], "</pre>",
      "</div>"
    )
    HTML(html_content)
  })

  # Coefficients table
  output$coefficients_table <- DT::renderDataTable(
    {
      req(analysis_results$analysis_result)
      result <- analysis_results$analysis_result

      coeffs <- result$results$initial$Model_Results %>%
        select(Term, Estimate, Stnd_Error, t_value, p_value, Signif_Level) %>%
        mutate(
          Estimate = round(Estimate, 4),
          Stnd_Error = round(Stnd_Error, 4),
          t_value = round(t_value, 4),
          p_value = round(p_value, 4)
        )

      DT::datatable(
        coeffs,
        options = list(
          scrollX = TRUE,
          pageLength = 20,
          dom = "Bfrtip",
          buttons = list(
            list(extend = "csv", filename = generate_filename_with_timestamp("supercrit_doe_analysis_model_coefficient")),
            list(extend = "excel", filename = generate_filename_with_timestamp("supercrit_doe_analysis_model_coefficient")),
            list(extend = "pdf", filename = generate_filename_with_timestamp("supercrit_doe_analysis_model_coefficient"))
          )
        ),
        extensions = "Buttons"
      )
    },
    server = FALSE
  )

  # Model data table
  output$model_data_table <- DT::renderDataTable(
    {
      req(analysis_results$analysis_result)
      result <- analysis_results$analysis_result

      model_data <- result$results$initial$Model_Data

      DT::datatable(
        model_data,
        options = list(
          scrollX = TRUE,
          pageLength = 25,
          dom = "Bfrtip",
          buttons = list(
            list(extend = "csv", filename = generate_filename_with_timestamp("supercrit_doe_analysis_model_data")),
            list(extend = "excel", filename = generate_filename_with_timestamp("supercrit_doe_analysis_model_data")),
            list(extend = "pdf", filename = generate_filename_with_timestamp("supercrit_doe_analysis_model_data"))
          )
        ),
        extensions = "Buttons"
      )
    },
    server = FALSE
  )

  # Download All Results
  output$export_all_results <- downloadHandler(
    filename = function() {
      paste0(generate_filename_with_timestamp("supercrit_doe_analysis_results"), ".zip")
    },
    content = function(file) {
      req(analysis_results$analysis_result) # Ensure results are available

      # Create a temporary directory for export
      temp_dir <- file.path(tempdir(), paste0("doe_analysis_export_", format(Sys.time(), "%Y%m%d%H%M%S")))
      dir.create(temp_dir, recursive = TRUE)

      # Export results using doeopt_export
      tryCatch({
        doeopt_export(input = analysis_results$analysis_result, expath = temp_dir, silent = TRUE)

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



  # Reset function
  observeEvent(input$reset, {
    # Reset inputs
    updateRadioButtons(session, "data_source", selected = "saved")
    updateSelectInput(session, "response_var", selected = "")
    updateSelectInput(session, "time_var", selected = "")
    updateSelectInput(session, "mod_order", selected = 2)
    updateNumericInput(session, "p_cutoff", value = 0.1)
    updateSelectInput(session, "trim_method", selected = "both")
    updateSelectInput(session, "which_facs", selected = "coded")

    # Clear results
    analysis_results$input_data <- NULL
    analysis_results$analysis_result <- NULL
    analysis_results$variable_names <- NULL

    showNotification(i18n$t("Analysis reset"), type = "message")
  })

  # Apply analysis settings function
  apply_analysis_settings <- function(session, settings) {
    updateRadioButtons(session, "data_source", selected = settings$data_source)
    updateSelectInput(session, "response_var", selected = settings$response_var)
    updateSelectInput(session, "time_var", selected = settings$time_var)
    updateSelectInput(session, "mod_order", selected = as.character(settings$mod_order))
    updateNumericInput(session, "p_cutoff", value = settings$p_cutoff)
    updateSelectInput(session, "trim_method", selected = settings$trim_method)
    updateSelectInput(session, "which_facs", selected = settings$which_facs)
  }

  # ============================================================================
  # PREDICTION TAB - Server Logic
  # ============================================================================

  # Reactive values for predictions
  prediction_results <- reactiveValues(
    predictions = NULL,
    multi_input_data = NULL
  )

  # Observe CSV file upload for predictions
  observeEvent(input$pred_import_csv, {
    req(input$pred_import_csv)

    tryCatch(
      {
        # Read the CSV file
        df <- read.csv(input$pred_import_csv$datapath, stringsAsFactors = FALSE)
        prediction_results$multi_input_data <- df # Directly load to multi_input_data
        showNotification(i18n$t("CSV file loaded to input table."), type = "message")
      },
      error = function(e) {
        showNotification(paste(i18n$t("Error loading CSV for prediction:"), e$message), type = "error")
        prediction_results$multi_input_data <- NULL
      }
    )
  })

  # Dynamic factor inputs for single value prediction
  output$pred_factor_inputs <- renderUI({
    req(analysis_results$analysis_result)

    result <- analysis_results$analysis_result

    # Get factor names
    if (!is.null(result$results$final$Model_Metrics$Canonical_Analysis)) {
      coded_facs <- names(result$results$final$Model_Metrics$Canonical_Analysis$xs)
    } else {
      # Fallback to model coefficients
      coded_facs <- result$models$final$codenames[nchar(result$models$final$codenames[, "data"]) == 1, "data"]
    }

    uncoded_facs <- if (!is.null(result$models$final$realnames)) {
      result$models$final$realnames
    } else {
      coded_facs
    }

    # Determine which factors to show based on coded/uncoded selection
    is_coded <- as.logical(input$pred_coded)
    fac_names <- if (is_coded) coded_facs else uncoded_facs
    fac_labels <- if (is_coded) coded_facs else paste0(uncoded_facs, " (", coded_facs, ")")

    # Get ranges from original data
    orig_df <- result$models$final$orig_df

    # Create numeric inputs for each factor
    input_list <- lapply(seq_along(fac_names), function(i) {
      fac <- fac_names[i]
      coded_fac <- coded_facs[i]

      # Get range from original data
      if (is_coded) {
        fac_range <- range(orig_df[, coded_fac], na.rm = TRUE)
      } else {
        uncoded_fac <- uncoded_facs[i]
        fac_range <- range(orig_df[, uncoded_fac], na.rm = TRUE)
      }

      # Default to midpoint
      default_val <- mean(fac_range)

      numericInput(
        ns(paste0("pred_fac_", i)),
        label = fac_labels[i],
        value = round(default_val, 2),
        min = fac_range[1],
        max = fac_range[2],
        step = (fac_range[2] - fac_range[1]) / 100
      )
    })

    do.call(tagList, input_list)
  })

  # Initialize multi-input table
  output$pred_multi_input <- renderRHandsontable({
    req(analysis_results$analysis_result)

    result <- analysis_results$analysis_result

    # Get factor names
    if (!is.null(result$results$final$Model_Metrics$Canonical_Analysis)) {
      coded_facs <- names(result$results$final$Model_Metrics$Canonical_Analysis$xs)
    } else {
      coded_facs <- result$models$final$codenames[nchar(result$models$final$codenames[, "data"]) == 1, "data"]
    }

    uncoded_facs <- if (!is.null(result$models$final$realnames)) {
      result$models$final$realnames
    } else {
      coded_facs
    }

    # Determine which factors to show
    is_coded <- as.logical(input$pred_coded_multi)
    fac_names <- if (is_coded) coded_facs else uncoded_facs

    # Create initial data frame
    if (is.null(prediction_results$multi_input_data)) {
      init_df <- as.data.frame(matrix(0, nrow = 3, ncol = length(fac_names)))
      colnames(init_df) <- fac_names
      prediction_results$multi_input_data <- init_df
    }

    rhandsontable(prediction_results$multi_input_data, height = 300) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
      hot_cols(columnSorting = TRUE)
  })

  # Update multi-input data when edited
  observeEvent(input$pred_multi_input, {
    if (!is.null(input$pred_multi_input)) {
      prediction_results$multi_input_data <- hot_to_r(input$pred_multi_input)
    }
  })

  # Perform prediction
  observeEvent(input$predict_btn, {
    req(analysis_results$analysis_result)

    tryCatch(
      {
        result <- analysis_results$analysis_result

        # Get factor names
        if (!is.null(result$results$final$Model_Metrics$Canonical_Analysis)) {
          coded_facs <- names(result$results$final$Model_Metrics$Canonical_Analysis$xs)
        } else {
          coded_facs <- result$models$final$codenames[nchar(result$models$final$codenames[, "data"]) == 1, "data"]
        }

        uncoded_facs <- if (!is.null(result$models$final$realnames)) {
          result$models$final$realnames
        } else {
          coded_facs
        }

        # Prepare newdata based on input type
        if (input$pred_input_type == "single") {
          # Single value prediction
          is_coded <- as.logical(input$pred_coded)
          fac_names <- if (is_coded) coded_facs else uncoded_facs

          # Collect values from inputs
          newdata <- sapply(seq_along(fac_names), function(i) {
            input[[paste0("pred_fac_", i)]]
          })
          names(newdata) <- fac_names
        } else {
          # Multiple value prediction
          req(prediction_results$multi_input_data)
          is_coded <- as.logical(input$pred_coded_multi)
          newdata <- prediction_results$multi_input_data
        }

        # Call predict_doe
        withProgress(message = i18n$t("Calculating predictions..."), value = 0.5, {
          preds <- predict_doe(
            input = result,
            newdata = newdata,
            coded = is_coded
          )

          prediction_results$predictions <- preds
        })

        showNotification(i18n$t("Predictions completed successfully!"), type = "message")
      },
      error = function(e) {
        showNotification(paste(i18n$t("Prediction error:"), e$message), type = "error")
      }
    )
  })

  # Display initial model predictions
  output$pred_results_initial <- DT::renderDataTable(
    {
      req(prediction_results$predictions)

      preds <- prediction_results$predictions

      if (!is.null(preds$summary$initial)) {
        DT::datatable(
          preds$summary$initial,
          options = list(
            scrollX = TRUE,
            pageLength = 10,
            dom = "t"
          ),
          rownames = FALSE
        )
      } else {
        DT::datatable(
          data.frame(Message = i18n$t("No initial model predictions available")),
          options = list(dom = "t"),
          rownames = FALSE
        )
      }
    },
    server = FALSE
  )

  # Display final model predictions
  output$pred_results_final <- DT::renderDataTable(
    {
      req(prediction_results$predictions)

      preds <- prediction_results$predictions

      if (!is.null(preds$summary$final)) {
        DT::datatable(
          preds$summary$final,
          options = list(
            scrollX = TRUE,
            pageLength = 10,
            dom = "t"
          ),
          rownames = FALSE
        )
      } else {
        DT::datatable(
          data.frame(Message = i18n$t("No final model predictions available")),
          options = list(dom = "t"),
          rownames = FALSE
        )
      }
    },
    server = FALSE
  )

  # Download predictions
  output$download_predictions <- downloadHandler(
    filename = function() {
      paste0(generate_filename_with_timestamp("supercrit_doe_predictions"), ".csv")
    },
    content = function(file) {
      req(prediction_results$predictions)

      preds <- prediction_results$predictions

      # Combine initial and final predictions
      combined_df <- NULL

      if (!is.null(preds$summary$initial)) {
        init_df <- preds$summary$initial
        init_df$Model <- "Initial"
        combined_df <- init_df
      }

      if (!is.null(preds$summary$final)) {
        final_df <- preds$summary$final
        final_df$Model <- "Final"

        if (is.null(combined_df)) {
          combined_df <- final_df
        } else {
          combined_df <- rbind(combined_df, final_df)
        }
      }

      if (!is.null(combined_df)) {
        write.csv(combined_df, file, row.names = FALSE)
      }
    }
  )

  # Intro Help Button Observer for Analysis Parameters
  observeEvent(input$doe_analysis_help, {
    introjs(session, options = intro_steps_analysis_params(ns, i18n))
  })

  # Intro Help Button Observer for Data Source
  observeEvent(input$data_source_help, {
    current_data_source <- input$data_source %||% "saved"
    introjs(session, options = intro_steps_data_source(ns, i18n, current_data_source))
  })
}

# Helper function to parse design files (CSV, TAB with header, direct TAB)
parse_design_file <- function(file_path) {
  # Read all lines
  lines <- readLines(file_path, warn = FALSE)

  # Check if it's a TAB format with DESIGN MATRIX header
  design_matrix_idx <- grep("^DESIGN MATRIX", lines, ignore.case = TRUE)

  if (length(design_matrix_idx) > 0) {
    # Format 2: TAB file with DESIGN MATRIX header
    start_idx <- design_matrix_idx[1] + 1

    # Find where table ends (empty line or "CALL" section)
    end_indices <- which(lines == "" | grepl("^CALL", lines, ignore.case = TRUE))
    end_indices <- end_indices[end_indices > start_idx]

    if (length(end_indices) > 0) {
      end_idx <- end_indices[1] - 1
    } else {
      end_idx <- length(lines)
    }

    # Extract table lines
    table_lines <- lines[start_idx:end_idx]

    # Parse as tab-delimited
    con <- textConnection(table_lines)
    data <- read.table(con, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
    close(con)
  } else {
    # Format 1 or 3: Direct CSV or direct TAB
    # Check first line for delimiter to decide between CSV and TAB
    first_line <- lines[1]

    if (grepl("\t", first_line)) {
      # Format 3: Tab-delimited, starts directly with table
      con <- textConnection(lines)
      data <- read.table(con, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
      close(con)
    } else {
      # Format 1: CSV file
      data <- read.csv(file_path, stringsAsFactors = FALSE)
    }
  }

  return(data)
}
