doe_analysis_server <- function(input, output, session, defaults, i18n, tablang, doe_rv, function_config = NULL) {






  # Helper for creating namespaced ids inside this module
  ns <- session$ns
  i18n_r <- reactive({
    i18n
  })

  # Helper function to convert equation string to LaTeX format
  # Converts: "1.234 + 0.567*A - 0.890*B + 1.23*A*B" to LaTeX
  equation_to_latex <- function(eq_str, response_var = "Y") {
    if (is.null(eq_str) || eq_str == "") return("")
    
    # Start with Y = 
    latex <- paste0(response_var, " = ")
    
    # Replace multiplication symbols with proper LaTeX
    eq_str <- gsub("\\*", " \\\\cdot ", eq_str)
    
    # Handle squared terms: A^2 formatting
    eq_str <- gsub("([A-Z])\\^2", "\\1^{2}", eq_str)
    
    # Wrap in LaTeX display
    latex <- paste0(latex, eq_str)
    
    return(latex)
  }
  
  # Helper function to create KaTeX rendered equation HTML
  render_equation_katex <- function(eq_str, response_var = "Y", element_id = NULL) {
    if (is.null(eq_str) || eq_str == "") {
      return(HTML('<p style="color: #666; font-style: italic;">No equation available</p>'))
    }
    
    latex_eq <- equation_to_latex(eq_str, response_var)
    
    # Generate unique ID if not provided
    if (is.null(element_id)) {
      element_id <- paste0("eq_", sample(100000:999999, 1))
    }
    
    # Return HTML with KaTeX rendering script
    tagList(
      div(
        id = element_id,
        class = "latex-equation-container",
        style = "background-color: #f8f9fa; padding: 15px; border-radius: 4px; overflow-x: auto; text-align: center;",
        span(
          class = "latex-equation",
          `data-equation` = latex_eq,
          style = "font-size: 16px;"
        )
      ),
      tags$script(HTML(sprintf("
        setTimeout(function() {
          var container = document.getElementById('%s');
          if (container) {
            var latexEl = container.querySelector('.latex-equation');
            if (latexEl && typeof katex !== 'undefined') {
              var equation = latexEl.getAttribute('data-equation');
              try {
                katex.render(equation, latexEl, {
                  throwOnError: false,
                  displayMode: true
                });
              } catch(e) {
                console.log('KaTeX render error:', e);
                latexEl.textContent = equation;
              }
            }
          }
        }, 100);
      ", element_id)))
    )
  }

  output$doe_analysis_HELP <- renderUI({
    create_help_modal(i18n_r, "doe_analysis_help_en")
  })

  # Render accordion expand/collapse button with translated tooltips
  output$accordion_toggle_btn <- renderUI({
    create_accordion_toggle_btn(ns, i18n_r, "doe_param_accordion")
  })

  # Render UI for CSV file import - DOE Analysis
  output$import_file_csv_div <- renderUI({
    div(
      style = "display: flex; align-items: flex-start; gap: 10px;",
      div(style = "flex: 1;",
        fileInput(ns("import_file_csv"), i18n_r()$t("Import CSV File"),
          accept = ".csv",
          buttonLabel = i18n_r()$t("Browse"),
          placeholder = i18n_r()$t("No file selected")
        )
      ),
      div(style = "margin-top: 25px; flex-shrink: 0;",
        actionButton(ns("reload_csv"), i18n_r()$t("Reload"),
          icon = icon("refresh"),
          class = "btn btn-default"
        )
      )
    )
  })
  
  # Enable/disable Reload button based on whether a CSV file is selected
  observe({
    if (!is.null(input$import_file_csv) && !is.null(input$import_file_csv$datapath)) {
      shinyjs::enable("reload_csv")
    } else {
      shinyjs::disable("reload_csv")
    }
  })
  
  # Reload CSV button handler
  observeEvent(input$reload_csv, {
    req(input$import_file_csv)
    
    tryCatch({
      data <- parse_design_file(input$import_file_csv$datapath)
      var_names <- names(data)
      
      # Identify coded factors (single uppercase letters A-Z) - these should not be selectable
      coded_cols <- grep("^[A-Z]$", var_names, value = TRUE)
      
      # Available columns for time/response selection (exclude coded factors)
      selectable_vars <- setdiff(var_names, coded_cols)
      
      # Store for later use
      analysis_results$input_data <- data
      analysis_results$variable_names <- var_names
      analysis_results$selectable_vars <- selectable_vars
      analysis_results$table_render_trigger <- analysis_results$table_render_trigger + 1  # Trigger table re-render
      
      # Determine initial selections for time and response variables
      initial_time <- if ("Actual_Order" %in% selectable_vars) "Actual_Order" else selectable_vars[1]
      initial_response <- if ("response" %in% selectable_vars) "response" else tail(selectable_vars, 1)
      
      # Ensure they're not the same
      if (initial_time == initial_response && length(selectable_vars) > 1) {
        initial_response <- setdiff(selectable_vars, initial_time)[1]
      }
      
      # Set time_var choices (exclude initial_response)
      time_choices <- setdiff(selectable_vars, initial_response)
      updateSelectInput(session, "time_var", choices = time_choices, selected = initial_time)
      
      # Set response_var choices (exclude initial_time)
      response_available <- setdiff(selectable_vars, initial_time)
      response_choices <- c(setNames("", i18n$t("None")), setNames(response_available, response_available))
      updateSelectInput(session, "response_var", choices = response_choices, selected = initial_response)
      
      showNotification(i18n$t("CSV file reloaded successfully."), type = "message")
    }, error = function(e) {
      showNotification(paste(i18n$t("Error reloading CSV:"), e$message), type = "error")
    })
  })
  output$mod_order_ui <- renderUI({
    selectInput(ns("mod_order"),
      tags$span(i18n_r()$t("Model Order"),
        input_help(i18n_r()$t("Order of the polynomial model to fit. Linear fits main effects only. Linear + Interactions adds two-factor interaction terms. Quadratic adds squared terms for response surface modeling."),
                   title = i18n_r()$t("Model Order"), buttonLabel = i18n_r()$t("OK"))),
      choices = setNames(c(1, 1.5, 2), c(
        i18n_r()$t("Linear (1)"),
        i18n_r()$t("Linear + Interactions (1.5)"),
        i18n_r()$t("Quadratic (2)")
      )),
      selected = defaults$mod_order
    )
  })
  outputOptions(output, "mod_order_ui", suspendWhenHidden = FALSE)

  # P-value cutoff with range badge
  output$p_cutoff_ui <- renderUI({
    cur <- isolate(if (!is.null(input$p_cutoff)) input$p_cutoff else 0.1)
    tags$div(
      tags$label(
        HTML(gsub("P-", "<em>p</em>-", gsub("p-", "<em>p</em>-", i18n_r()$t("P-value Cutoff")))),
        input_help(i18n_r()$t("Significance threshold for retaining model terms. Terms with p-values above this cutoff are considered insignificant and may be removed during trimming. Common values are 0.05 or 0.10."),
                   title = i18n_r()$t("P-value Cutoff"), buttonLabel = i18n_r()$t("OK")),
        class = "control-label",
        style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
        tags$span(
          "0–0.5",
          style = "font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: #6c757d; color: white; margin-left: auto; font-weight: normal;",
          title = paste0(i18n_r()$t("Valid range:"), " 0 – 0.5")
        )
      ),
      numericInput(ns("p_cutoff"), label = NULL, value = cur, step = 0.01)
    )
  })
  outputOptions(output, "p_cutoff_ui", suspendWhenHidden = FALSE)

  # Auto-correct p_cutoff when user enters out-of-range value
  observeEvent(input$p_cutoff, {
    val <- input$p_cutoff
    if (!is.null(val) && !is.na(val) && (val <= 0 || val > 0.5)) {
      corrected <- max(0.01, min(0.5, val))
      showNotification(i18n$t("P-value cutoff was adjusted to the valid range (0–0.5)."), type = "warning")
      updateNumericInput(session, "p_cutoff", value = corrected)
    }
  }, ignoreInit = TRUE)

    output$trim_method_ui <- renderUI({
    selectInput(ns("trim_method"),
      tags$span(i18n_r()$t("Trim Method"),
        input_help(i18n_r()$t("Method for removing insignificant terms from the model. Stepwise uses backward elimination based on AIC. P-value Cutoff removes terms above the threshold. Both applies both methods sequentially. None keeps the full model."),
                   title = i18n_r()$t("Trim Method"), buttonLabel = i18n_r()$t("OK"))),
      choices = setNames(
        c("stepwise", "p_cutoff", "both", "none"),
        c(i18n_r()$t("Stepwise"), i18n_r()$t("P-value Cutoff"), i18n_r()$t("Both"), i18n_r()$t("None"))
      ),
      selected = defaults$trim_method
    )
  })
  outputOptions(output, "trim_method_ui", suspendWhenHidden = FALSE)

  # Reactive value to track if custom canon_thres is enabled
  canon_thres_enabled <- reactiveVal(FALSE)
  
  output$canon_thres_ui <- renderUI({
    is_enabled <- canon_thres_enabled()
    
    div(
      class = "form-group shiny-input-container",
      style = "width: 100%;",
      # Label row with toggle button floated right
      div(
        style = "display: flex; justify-content: space-between; align-items: center;",
        tags$label(class = "control-label",
          i18n_r()$t("Canonical Threshold"),
          input_help(i18n_r()$t("Eigenvalue threshold for canonical analysis of the response surface. Eigenvalues below this threshold are treated as zero, simplifying the surface interpretation. Leave disabled for automatic selection."),
                     title = i18n_r()$t("Canonical Threshold"), buttonLabel = i18n_r()$t("OK"))
        ),
        actionButton(
          ns("toggle_canon_thres"),
          label = NULL,
          icon = icon(if (is_enabled) "toggle-on" else "toggle-off"),
          class = paste("btn btn-xs", if (is_enabled) "btn-primary" else "btn-default"),
          style = "padding: 2px 6px; font-size: 14px;",
          title = i18n$t("Enable custom threshold")
        )
      ),
      # Input field (enabled/disabled based on toggle)
      if (is_enabled) {
        tagList(
          tags$input(
            id = ns("canon_thres_value"),
            type = "number",
            class = "form-control",
            value = "0.10",
            min = "0.01",
            step = "0.01"
          ),
          tags$small(
            style = "color: #666; display: block; margin-top: 5px;",
            i18n$t("Factor of max eigenvalue")
          ),
          tags$small(
            style = "color: #856404; display: block; margin-top: 5px;",
            icon("exclamation-triangle", style = "margin-right: 4px;"),
            i18n$t("A small threshold may move the stationary point farther from the origin.")
          )
        )
      } else {
        div(
          style = "background-color: #f5f5f5; border: 1px solid #ddd; border-radius: 4px; padding: 6px 12px; color: #888; height: 34px; line-height: 22px;",
          tags$span(i18n$t("Auto (10% of max eigenvalue)"))
        )
      }
    )
  })
  outputOptions(output, "canon_thres_ui", suspendWhenHidden = FALSE)
  
  # Toggle observer for canon_thres
  observeEvent(input$toggle_canon_thres, {
    canon_thres_enabled(!canon_thres_enabled())
  })


  # Render UI for predictions CSV import
  output$pred_import_csv_div <- renderUI({
    fileInput(ns("pred_import_csv"), i18n_r()$t("Import CSV File"),
      accept = ".csv",
      buttonLabel = i18n_r()$t("Browse"),
      placeholder = i18n_r()$t("No file selected")
    )
  })

  # Dynamic UI for time variable selection
  output$time_var_ui <- renderUI({
    selectable <- analysis_results$selectable_vars
    
    # Show disabled input with placeholder when no data loaded
    if (is.null(selectable) || length(selectable) == 0) {
      return(
        tags$div(
          class = "form-group shiny-input-container",
          tags$label(class = "control-label", `for` = ns("time_var"),
            i18n_r()$t("Time Variable"),
            input_help(i18n_r()$t("Select the column representing run order or time sequence. Used for detecting time-dependent trends in exploratory data analysis plots."),
                       title = i18n_r()$t("Time Variable"), buttonLabel = i18n_r()$t("OK"))
          ),
          tags$select(
            id = ns("time_var"),
            class = "form-control",
            disabled = "disabled",
            style = "background-color: #e9ecef; color: #6c757d;",
            tags$option(i18n$t("Load data"))
          )
        )
      )
    }
    
    # Get current selections
    current_time <- input$time_var
    current_response <- input$response_var
    
    # Get uncoded factors assigned to coded factors
    assigned_uc_facs <- character(0)
    data <- analysis_results$input_data
    if (!is.null(data)) {
      var_names <- names(data)
      coded_cols <- sort(grep("^[A-Z]$", var_names, value = TRUE))
      for (cf in coded_cols) {
        input_id <- paste0("uc_for_", cf)
        val <- input[[input_id]]
        if (!is.null(val) && length(val) == 1 && val != "") {
          assigned_uc_facs <- c(assigned_uc_facs, val)
        }
      }
      assigned_uc_facs <- unique(assigned_uc_facs)
    }
    
    # Filter out: uncoded factors assigned to coded factors AND response variable (if set)
    available_for_time <- setdiff(selectable, assigned_uc_facs)
    if (!is.null(current_response) && current_response != "") {
      available_for_time <- setdiff(available_for_time, current_response)
    }
    
    # Keep current selection if valid (including ""), otherwise default
    selected <- if (!is.null(current_time) && (current_time == "" || current_time %in% available_for_time)) {
      current_time
    } else if ("Actual_Order" %in% available_for_time) {
      "Actual_Order"
    } else if (length(available_for_time) > 0) {
      available_for_time[1]
    } else {
      ""
    }
    
    tags$div(
      tags$label(
        i18n_r()$t("Time Variable"),
        input_help(i18n_r()$t("Select the column representing run order or time sequence. Used for detecting time-dependent trends in exploratory data analysis plots."),
                   title = i18n_r()$t("Time Variable"), buttonLabel = i18n_r()$t("OK")),
        class = "control-label",
        style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
        actionLink(ns("clear_time_var"), i18n$t("Clear"),
          style = "font-size: 11px; padding: 1px 6px; border-radius: 3px; text-decoration: none; background-color: #dc3545; color: white; margin-left: auto;",
          title = i18n$t("Clear selection")
        )
      ),
      selectInput(ns("time_var"), NULL,
        choices = c(setNames("", i18n$t("None")), setNames(available_for_time, available_for_time)),
        selected = selected)
    )
  })
  outputOptions(output, "time_var_ui", suspendWhenHidden = FALSE)

  # Dynamic UI for response variable selection
  output$response_var_ui <- renderUI({
    selectable <- analysis_results$selectable_vars
    
    # Show disabled input with placeholder when no data loaded
    if (is.null(selectable) || length(selectable) == 0) {
      return(
        tags$div(
          class = "form-group shiny-input-container",
          tags$label(class = "control-label", `for` = ns("response_var"),
            i18n_r()$t("Response Variable"),
            input_help(i18n_r()$t("Select the column containing the measured response (e.g. yield, concentration). This is the dependent variable that the model will fit and optimize."),
                       title = i18n_r()$t("Response Variable"), buttonLabel = i18n_r()$t("OK"))
          ),
          tags$select(
            id = ns("response_var"),
            class = "form-control",
            disabled = "disabled",
            style = "background-color: #e9ecef; color: #6c757d;",
            tags$option(i18n$t("Load data"))
          )
        )
      )
    }
    
    # Get current selections
    current_time <- input$time_var
    current_response <- input$response_var
    
    # Filter out time variable from choices (if set)
    available_for_response <- selectable
    if (!is.null(current_time) && current_time != "") {
      available_for_response <- setdiff(available_for_response, current_time)
    }
    
    # Keep current selection if valid (including ""), otherwise default
    selected <- if (!is.null(current_response) && (current_response == "" || current_response %in% available_for_response)) {
      current_response
    } else if ("response" %in% available_for_response) {
      "response"
    } else if (length(available_for_response) > 0) {
      tail(available_for_response, 1)
    } else {
      ""
    }
    
    tags$div(
      tags$label(
        i18n_r()$t("Response Variable"),
        input_help(i18n_r()$t("Select the column containing the measured response (e.g. yield, concentration). This is the dependent variable that the model will fit and optimize."),
                   title = i18n_r()$t("Response Variable"), buttonLabel = i18n_r()$t("OK")),
        class = "control-label",
        style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
        actionLink(ns("clear_response_var"), i18n$t("Clear"),
          style = "font-size: 11px; padding: 1px 6px; border-radius: 3px; text-decoration: none; background-color: #dc3545; color: white; margin-left: auto;",
          title = i18n$t("Clear selection")
        )
      ),
      selectInput(ns("response_var"), NULL,
        choices = c(setNames("", i18n$t("None")), setNames(available_for_response, available_for_response)),
        selected = selected)
    )
  })
  outputOptions(output, "response_var_ui", suspendWhenHidden = FALSE)

  # Clear button observers for variable selectors
  observeEvent(input$clear_time_var, {
    updateSelectInput(session, "time_var", selected = "")
  })
  
  observeEvent(input$clear_response_var, {
    updateSelectInput(session, "response_var", selected = "")
  })

  shinyjs::disable("doe_analysis-reset")
  shinyjs::disable("save_analysis") # Disable save button initially

  # Source the save modal UI
  source(file.path("ui_modules", "doe_analysis_save_modal_ui.R"), local = TRUE)$value

  # Source DOE analysis helper functions
  source(file.path("server_modules", "doe_analysis_helpers.R"), local = TRUE)$value

  # Reactive values for storing results
  analysis_results <- reactiveValues(
    input_data = NULL,
    analysis_result = NULL,
    variable_names = NULL,
    selectable_vars = NULL,  # Variables available for time/response selection (excludes coded factors)
    response_var = NULL,
    time_var = NULL,
    table_render_trigger = 0  # Incremented only when table should re-render (not on edits)
  )

  # Reactive output to check if results are available
  output$has_results <- reactive({
    !is.null(analysis_results$analysis_result)
  })
  outputOptions(output, "has_results", suspendWhenHidden = FALSE)

  # Observer to enable/disable result tabs based on results availability
  observe({
    has_results <- !is.null(analysis_results$analysis_result)
    if (has_results) {
      session$sendCustomMessage("enableTabs", list(tabsetId = ns("results_tabs")))
    } else {
      session$sendCustomMessage("disableTabs", list(tabsetId = ns("results_tabs"), keepFirst = TRUE))
    }
  })

  # Observe changes in doe_rv$saved_designs and update selectInput
  observeEvent(doe_rv$saved_designs,
    {
      if (length(doe_rv$saved_designs) > 0) {
        design_names <- sapply(doe_rv$saved_designs, function(x) x$name)
        design_ids <- seq_along(doe_rv$saved_designs)
        updateSelectInput(session, "saved_design",
          choices = setNames(design_ids, design_names),
          selected = design_ids[1]
        )
      } else {
        updateSelectInput(session, "saved_design",
          choices = c("No saved designs" = ""),
          selected = ""
        )
      }
    },
    ignoreNULL = FALSE
  )

  # Load saved design button handler
  observeEvent(input$load_saved_design, {
    req(input$saved_design)
    design_id <- as.numeric(input$saved_design)
    
    if (!is.na(design_id) && design_id > 0 && design_id <= length(doe_rv$saved_designs)) {
      design_data <- doe_rv$saved_designs[[design_id]]$data
      data <- NULL
      
      if (!is.null(design_data$results$design_matrix)) {
        data <- design_data$results$design_matrix
      }
      
      if (!is.null(data) && is.data.frame(data)) {
        var_names <- names(data)
        
        # Identify coded factors (single uppercase letters A-Z) - these should not be selectable
        coded_cols <- grep("^[A-Z]$", var_names, value = TRUE)
        
        # Available columns for time/response selection (exclude coded factors)
        selectable_vars <- setdiff(var_names, coded_cols)
        
        # Store for later use
        analysis_results$input_data <- data
        analysis_results$variable_names <- var_names
        analysis_results$selectable_vars <- selectable_vars
        analysis_results$table_render_trigger <- analysis_results$table_render_trigger + 1  # Trigger table re-render
        
        # Determine initial selections for time and response variables
        initial_time <- if ("Actual_Order" %in% selectable_vars) "Actual_Order" else selectable_vars[1]
        initial_response <- if ("response" %in% selectable_vars) "response" else tail(selectable_vars, 1)
        
        # Ensure they're not the same
        if (initial_time == initial_response && length(selectable_vars) > 1) {
          initial_response <- setdiff(selectable_vars, initial_time)[1]
        }
        
        # Set time_var choices (exclude initial_response)
        time_choices <- setdiff(selectable_vars, initial_response)
        updateSelectInput(session, "time_var", choices = time_choices, selected = initial_time)
        
        # Set response_var choices (exclude initial_time)
        response_available <- setdiff(selectable_vars, initial_time)
        response_choices <- c(setNames("", i18n$t("None")), setNames(response_available, response_available))
        updateSelectInput(session, "response_var", choices = response_choices, selected = initial_response)
        
        shinyjs::enable("save_analysis")
        
        showNotification(i18n$t("Design loaded successfully."), type = "message")
      } else {
        showNotification(i18n$t("Failed to load design data."), type = "error")
      }
    } else {
      showNotification(i18n$t("Please select a valid design."), type = "warning")
    }
  })

  # Reactive for loading data based on source
  # Note: For "saved" designs, data is loaded via the load_saved_design button, not automatically
  loaded_data <- reactive({
    switch(input$data_source,
      "saved" = {
        # Saved designs are loaded via dedicated button, not automatically
        NULL
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
        if (!is.null(input$import_file_csv)) {
          # User uploaded file
          tryCatch(
            {
              parse_design_file(input$import_file_csv$datapath)
            },
            error = function(e) {
              showNotification(paste(i18n$t("Error loading file:"), e$message), type = "error")
              NULL
            }
          )
        } else {
          # No file uploaded - return NULL (user can click "Load Example" to load default data)
          NULL
        }
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
      
      # Identify coded factors (single uppercase letters A-Z) - these should not be selectable
      coded_cols <- grep("^[A-Z]$", var_names, value = TRUE)
      
      # Available columns for time/response selection (exclude coded factors)
      selectable_vars <- setdiff(var_names, coded_cols)
      
      # Store both for later use
      analysis_results$input_data <- data
      analysis_results$variable_names <- var_names
      analysis_results$selectable_vars <- selectable_vars
      analysis_results$table_render_trigger <- analysis_results$table_render_trigger + 1  # Trigger table re-render
      
      # Determine initial selections for time and response variables
      initial_time <- if ("Actual_Order" %in% selectable_vars) "Actual_Order" else selectable_vars[1]
      initial_response <- if ("response" %in% selectable_vars) "response" else tail(selectable_vars, 1)
      
      # Ensure they're not the same
      if (initial_time == initial_response && length(selectable_vars) > 1) {
        initial_response <- setdiff(selectable_vars, initial_time)[1]
      }
      
      # Set time_var choices (exclude initial_response)
      time_choices <- setdiff(selectable_vars, initial_response)
      updateSelectInput(session, "time_var", choices = time_choices, selected = initial_time)
      
      # Set response_var choices (exclude initial_time)
      response_available <- setdiff(selectable_vars, initial_time)
      response_choices <- c(setNames("", i18n$t("None")), setNames(response_available, response_available))
      updateSelectInput(session, "response_var", choices = response_choices, selected = initial_response)

      shinyjs::enable("save_analysis") # Enable save button when data is loaded
    } else {
      shinyjs::disable("save_analysis") # Disable save button if no data
    }
  })

  # Helper function to get variables available for time/response selection
  # Excludes: coded factors, the other dropdown's selection, and assigned uncoded factors
  get_available_for_dropdown <- function(exclude_var = NULL) {
    selectable_vars <- analysis_results$selectable_vars
    if (is.null(selectable_vars)) return(character(0))
    
    # Get assigned uncoded factors
    assigned_uc <- get_selected_uc_facs()
    
    # Start with selectable_vars (already excludes coded factors)
    available <- selectable_vars
    
    # Exclude assigned uncoded factors
    available <- setdiff(available, assigned_uc)
    
    # Exclude the other dropdown's selection if provided
    if (!is.null(exclude_var) && exclude_var != "") {
      available <- setdiff(available, exclude_var)
    }
    
    available
  }
  
  # Function to update both dropdowns
  update_time_response_dropdowns <- function() {
    current_time <- input$time_var
    current_response <- input$response_var
    
    # Get available vars for each dropdown (excluding the other's selection)
    available_for_time <- get_available_for_dropdown(exclude_var = current_response)
    available_for_response <- get_available_for_dropdown(exclude_var = current_time)
    
    # Build choices with None option
    time_choices <- c(setNames("", i18n$t("None")), setNames(available_for_time, available_for_time))
    response_choices <- c(setNames("", i18n$t("None")), setNames(available_for_response, available_for_response))
    
    # Update time_var, keeping current selection if still valid (including "")
    if (!is.null(current_time) && (current_time == "" || current_time %in% available_for_time)) {
      updateSelectInput(session, "time_var", choices = time_choices, selected = current_time)
    } else if (length(available_for_time) > 0) {
      updateSelectInput(session, "time_var", choices = time_choices, selected = available_for_time[1])
    } else {
      updateSelectInput(session, "time_var", choices = c(setNames("", i18n$t("None"))), selected = "")
    }
    
    # Update response_var, keeping current selection if still valid (including "")
    if (!is.null(current_response) && (current_response == "" || current_response %in% available_for_response)) {
      updateSelectInput(session, "response_var", choices = response_choices, selected = current_response)
    } else if (length(available_for_response) > 0) {
      updateSelectInput(session, "response_var", choices = response_choices, selected = tail(available_for_response, 1))
    } else {
      updateSelectInput(session, "response_var", choices = c(setNames("", i18n$t("None"))), selected = "")
    }
  }

  # Note: Mutual filtering is handled by renderUI for time_var_ui and response_var_ui
  # No separate observeEvent handlers needed - they cause timing issues with "" selections

  # Validation message for empty variable selections
  output$column_validation_message <- renderUI({
    selectable <- analysis_results$selectable_vars
    
    time_var <- input$time_var
    response_var <- input$response_var
    
    # Check if any selection is empty
    time_empty <- is.null(time_var) || time_var == ""
    response_empty <- is.null(response_var) || response_var == ""
    
    # Handle wrapper borders for empty selections
    time_wrapper_id <- ns("time_var_wrapper")
    response_wrapper_id <- ns("response_var_wrapper")
    
    if (is.null(selectable) || length(selectable) == 0) {
      # Clear borders when no data
      shinyjs::runjs(sprintf(
        "$('#%s').css({'border': '', 'border-radius': '', 'padding': ''});",
        time_wrapper_id
      ))
      shinyjs::runjs(sprintf(
        "$('#%s').css({'border': '', 'border-radius': '', 'padding': ''});",
        response_wrapper_id
      ))
      return(NULL)
    }
    
    # Check for duplicate selection (same column for both)
    has_duplicate <- !time_empty && !response_empty && time_var == response_var
    
    # Apply orange border for empty time variable, red for duplicate
    if (time_empty) {
      shinyjs::runjs(sprintf(
        "$('#%s').css({'border': '2px solid #ffc107', 'border-radius': '4px', 'padding': '5px'});",
        time_wrapper_id
      ))
    } else if (has_duplicate) {
      shinyjs::runjs(sprintf(
        "$('#%s').css({'border': '2px solid #dc3545', 'border-radius': '4px', 'padding': '5px'});",
        time_wrapper_id
      ))
    } else {
      shinyjs::runjs(sprintf(
        "$('#%s').css({'border': '', 'border-radius': '', 'padding': ''});",
        time_wrapper_id
      ))
    }
    
    # Apply orange border for empty response variable, red for duplicate
    if (response_empty) {
      shinyjs::runjs(sprintf(
        "$('#%s').css({'border': '2px solid #ffc107', 'border-radius': '4px', 'padding': '5px'});",
        response_wrapper_id
      ))
    } else if (has_duplicate) {
      shinyjs::runjs(sprintf(
        "$('#%s').css({'border': '2px solid #dc3545', 'border-radius': '4px', 'padding': '5px'});",
        response_wrapper_id
      ))
    } else {
      shinyjs::runjs(sprintf(
        "$('#%s').css({'border': '', 'border-radius': '', 'padding': ''});",
        response_wrapper_id
      ))
    }
    
    if (time_empty || response_empty) {
      div(
        class = "alert alert-warning",
        style = "margin: 10px 0; padding: 10px;",
        icon("exclamation-triangle"),
        i18n$t(" Variable selection cannot be empty.")
      )
    } else if (has_duplicate) {
      div(
        class = "alert alert-warning",
        style = "margin: 10px 0; padding: 10px;",
        icon("exclamation-triangle"),
        sprintf(i18n$t(" Warning: Column '%s' is selected for both Time and Response!"), time_var)
      )
    } else {
      NULL
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

  # Helper function to get selected uncoded factors from dropdowns
  get_selected_uc_facs <- function() {
    data <- analysis_results$input_data
    if (is.null(data)) return(character(0))
    
    var_names <- names(data)
    coded_cols <- sort(grep("^[A-Z]$", var_names, value = TRUE))
    
    if (length(coded_cols) == 0) return(character(0))
    
    selected <- character(0)
    for (cf in coded_cols) {
      input_id <- paste0("uc_for_", cf)
      val <- input[[input_id]]
      if (!is.null(val)) {
        if (length(val) == 1) {
          if (val != "") {
            selected <- c(selected, val)
          }
        }
      }
    }
    unique(selected)
  }

  # Render coded factors info and uncoded factor selectors
  output$coded_factors_info_ui <- renderUI({
    # Only render when we have data
    data <- analysis_results$input_data
    if (is.null(data)) {
      return(NULL)
    }
    
    var_names <- names(data)
    
    # Identify coded factors (single uppercase letters A-Z)
    coded_cols <- sort(grep("^[A-Z]$", var_names, value = TRUE))
    
    if (length(coded_cols) == 0) {
      # Warning when no coded factors detected
      return(
        div(
          style = "background-color: #fff3cd; border: 1px solid #ffc107; border-radius: 4px; padding: 10px; margin-bottom: 10px;",
          icon("exclamation-triangle", style = "color: #856404; margin-right: 8px;"),
          tags$span(style = "color: #856404;",
            i18n$t("No coded factors detected. DOE analysis requires columns named A, B, C, etc. (single uppercase letters) for coded factors.")
          )
        )
      )
    }
    
    # For uncoded factor selection, exclude response, time, AND coded factors
    resp_var <- input$response_var
    time_var <- input$time_var
    exclude_cols <- coded_cols  # Start with coded factors excluded
    if (!is.null(resp_var)) {
      if (resp_var != "") {
        exclude_cols <- c(exclude_cols, resp_var)
      }
    }
    if (!is.null(time_var)) {
      if (time_var != "") {
        exclude_cols <- c(exclude_cols, time_var)
      }
    }
    
    # Get all potential uncoded factor candidates
    uc_candidates <- setdiff(var_names, exclude_cols)
    
    # Get currently selected uncoded factors for each coded factor
    selected_uc <- list()
    for (cf in coded_cols) {
      input_id <- paste0("uc_for_", cf)
      val <- input[[input_id]]
      if (!is.null(val)) {
        if (length(val) == 1) {
          if (val != "") {
            selected_uc[[cf]] <- val
          }
        }
      }
    }
    
    # Create a selector row for each coded factor (using flexbox for better alignment)
    factor_selectors <- lapply(coded_cols, function(cf) {
      # Get uncoded factors selected by OTHER coded factors
      other_selected <- unlist(selected_uc[names(selected_uc) != cf])
      
      # Filter out already-selected uncoded factors
      available_uc <- setdiff(uc_candidates, other_selected)
      
      # Current selection for this coded factor (keep it available even if selected)
      current_sel <- selected_uc[[cf]]
      if (!is.null(current_sel)) {
        available_uc <- union(available_uc, current_sel)
      }
      
      # Add "None" option
      uc_choices <- c(setNames("", i18n$t("None")), setNames(available_uc, available_uc))
      
      # Each factor gets its own row with label, dropdown, and clear button
      div(
        style = "display: flex; align-items: center; gap: 8px; margin-bottom: 5px;",
        tags$span(
          style = "font-weight: bold; min-width: 35px; text-align: right;",
          paste0(cf, " \u2192")
        ),
        div(
          style = "flex: 1;",
          selectInput(
            ns(paste0("uc_for_", cf)),
            label = NULL,
            choices = uc_choices,
            selected = if (!is.null(current_sel)) current_sel else "",
            width = "100%"
          ) |> tagAppendAttributes(style = "margin-bottom: 0;")
        ),
        actionButton(
          ns(paste0("clear_uc_for_", cf)),
          label = i18n$t("Clear"),
          icon = icon("times"),
          class = "btn btn-default btn-sm",
          style = "flex-shrink: 0;"
        )
      )
    })
    
    tagList(
      # Uncoded factor associations title (styled like other parameter titles)
      tags$label(class = "control-label", i18n$t("Uncoded Factor Associations")),
      # Instruction text
      tags$p(style = "font-size: 12px; color: #666; margin-bottom: 8px; margin-top: 2px;",
        i18n$t("Select the uncoded (real-world) variable that corresponds to each coded factor:")
      ),
      # Info about detected coded factors (moved below instruction)
      div(
        style = "background-color: #e7f3ff; border: 1px solid #b8daff; border-radius: 4px; padding: 10px; margin-bottom: 10px;",
        icon("info-circle", style = "color: #004085; margin-right: 8px;"),
        tags$span(style = "color: #004085;",
          sprintf(i18n$t("Detected coded factors: %s"), paste(coded_cols, collapse = ", "))
        )
      ),
      # Vertical list of factor selectors
      div(
        factor_selectors
      )
    )
  })
  
 # Clear button observers for each possible coded factor (A-Z)
  # These observers are created once and will only fire when the corresponding button exists
  lapply(LETTERS, function(cf) {
    observeEvent(input[[paste0("clear_uc_for_", cf)]], {
      updateSelectInput(session, paste0("uc_for_", cf), selected = "")
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
  })

  # Observers for uncoded factor assignment changes (A-Z)
  # When an uncoded factor is assigned/unassigned, update time/response dropdowns
  lapply(LETTERS, function(cf) {
    observeEvent(input[[paste0("uc_for_", cf)]], {
      req(analysis_results$selectable_vars)
      update_time_response_dropdowns()
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
  })

  # Load Example Data button handler
  observeEvent(input$load_example_data, {
    default_path <- system.file("extdata", "gui-doe-analysis-default.csv", package = "supeRcrit")
    if (file.exists(default_path)) {
      tryCatch({
        data <- parse_design_file(default_path)
        
        var_names <- names(data)
        
        # Identify coded factors (single uppercase letters A-Z) - these should not be selectable
        coded_cols <- grep("^[A-Z]$", var_names, value = TRUE)
        
        # Available columns for time/response selection (exclude coded factors)
        selectable_vars <- setdiff(var_names, coded_cols)
        
        # Store for later use
        analysis_results$input_data <- data
        analysis_results$variable_names <- var_names
        analysis_results$selectable_vars <- selectable_vars
        analysis_results$table_render_trigger <- analysis_results$table_render_trigger + 1  # Trigger table re-render
        
        # Determine initial selections for time and response variables
        initial_time <- if ("Actual_Order" %in% selectable_vars) "Actual_Order" else selectable_vars[1]
        initial_response <- if ("response" %in% selectable_vars) "response" else tail(selectable_vars, 1)
        
        # Ensure they're not the same
        if (initial_time == initial_response && length(selectable_vars) > 1) {
          initial_response <- setdiff(selectable_vars, initial_time)[1]
        }
        
        # Set time_var choices (exclude initial_response)
        time_choices <- setdiff(selectable_vars, initial_response)
        updateSelectInput(session, "time_var", choices = time_choices, selected = initial_time)
        
        # Set response_var choices (exclude initial_time)
        response_available <- setdiff(selectable_vars, initial_time)
        response_choices <- c(setNames("", i18n$t("None")), setNames(response_available, response_available))
        updateSelectInput(session, "response_var", choices = response_choices, selected = initial_response)
        
        showNotification(i18n$t("Example data loaded successfully."), type = "message")
      }, error = function(e) {
        showNotification(paste(i18n$t("Error loading example data:"), e$message), type = "error")
      })
    } else {
      showNotification(i18n$t("Example data file not found."), type = "error")
    }
  })

  # Observe for save analysis button click
  observeEvent(input$save_analysis, {
    req(analysis_results$input_data) # Require data to be loaded
    # Get default directory for save dialog
    default_dir <- file.path(
      system.file(package = "supeRcrit"), 
      "shiny-app", "config", "user-settings", "doe_analysis"
    )
    showModal(doe_analysis_save_modal_ui(ns("save_modal"), i18n, default_directory = default_dir))
  })
  
  # Set up shinyFiles directory chooser with system volumes
  volumes <- c(
    Home = Sys.getenv("HOME"),
    shinyFiles::getVolumes()()
  )
  shinyFiles::shinyDirChoose(
    input, 
    "save_modal-browse_directory", 
    roots = volumes,
    session = session
  )
  
  # Update text input when directory is selected via browser
  observeEvent(input[["save_modal-browse_directory"]], {
    req(input[["save_modal-browse_directory"]])
    dir_path <- shinyFiles::parseDirPath(volumes, input[["save_modal-browse_directory"]])
    if (length(dir_path) > 0 && nchar(dir_path) > 0) {
      updateTextInput(session, "save_modal-save_directory", value = dir_path)
    }
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
    req(analysis_results$analysis_result, input[["save_modal-analysis_name"]])

    removeModal() # Close the modal

    tryCatch(
      {
        # Get user choices from modal
        save_to_disk <- isTRUE(input[["save_modal-save_to_disk"]])
        custom_directory <- input[["save_modal-save_directory"]]
        
        print("DEBUG: ===== STARTING SAVE OPERATION =====")
        print(paste("DEBUG: Save to disk checkbox:", save_to_disk))
        print(paste("DEBUG: Custom directory:", custom_directory))

        # Get analysis result object
        analysis_obj <- analysis_results$analysis_result

        # Add metadata to analysis object
        analysis_obj$metadata <- list(
          name = input[["save_modal-analysis_name"]],
          timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          app_version = "0.9.0",
          response_var = input$response_var,
          parameters = list(
            response_var = input$response_var,
            time_var = input$time_var,
            mod_order = as.numeric(input$mod_order),
            p_cutoff = input$p_cutoff,
            trim_method = input$trim_method,
            uc_facs = get_selected_uc_facs(),
            which_facs = input$which_facs
          ),
          # Include input data for desirability analysis re-runs
          input_data = analysis_results$input_data
        )

        # Generate sanitized name using user input
        user_input_name <- input[["save_modal-analysis_name"]]
        sanitized_name <- gsub("[^a-zA-Z0-9_.-]", "_", user_input_name)
        if (nchar(sanitized_name) == 0) {
          sanitized_name <- "untitled"
        }
        print(paste("DEBUG: Sanitized name:", sanitized_name))

        # Save to .RData file if user checked the option
        filepath <- NULL
        if (save_to_disk) {
          print("DEBUG: Attempting to save to directory...")
          
          # Use custom directory if provided, otherwise use default
          if (!is.null(custom_directory) && nchar(trimws(custom_directory)) > 0) {
            # Custom directory specified
            dir_path <- trimws(custom_directory)
            if (!dir.exists(dir_path)) {
              dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
            }
            # Generate filename with timestamp
            timestamp_str <- format(Sys.time(), "%d-%m-%Y_%Hhrs-%Mmin-%Ssec")
            filename <- paste0(sanitized_name, "_", timestamp_str, ".RData")
            filepath <- file.path(dir_path, filename)
            
            # Save the file
            tryCatch({
              save(analysis_obj, file = filepath)
              print(paste("DEBUG: Saved to custom directory:", filepath))
            }, error = function(e) {
              showNotification(
                paste(i18n$t("Error saving to custom directory:"), e$message),
                type = "error"
              )
              filepath <<- NULL
            })
          } else {
            # Use default package directory
            filepath <- save_doe_analysis(
              app_name = "doe_analysis",
              analysis_obj = analysis_obj,
              analysis_name = sanitized_name
            )
          }

          print(paste("DEBUG: Returned filepath:", filepath))

          if (is.null(filepath)) {
            showNotification(i18n$t("Failed to save analysis to disk."), type = "warning")
          }
        } else {
          print("DEBUG: Save to disk not selected - saving to session only")
        }

        # ===== ALWAYS SAVE TO SESSION STORAGE =====
        print("DEBUG: ===== SAVING TO SESSION STORAGE =====")

        # FIFO limit: Maximum 6 analyses in session storage
        # If we already have 6, remove the oldest one before adding new
        print(paste("DEBUG: Current session storage count:", length(doe_rv$saved_analyses)))
        if (length(doe_rv$saved_analyses) >= 6) {
          # Find oldest analysis based on timestamp
          timestamps <- sapply(doe_rv$saved_analyses, function(x) as.POSIXct(x$timestamp))
          oldest_idx <- which.min(timestamps)

          # Remove oldest and compact list
          doe_rv$saved_analyses[[oldest_idx]] <- NULL
          doe_rv$saved_analyses <- Filter(Negate(is.null), doe_rv$saved_analyses)
          print("DEBUG: Removed oldest analysis due to FIFO limit")
        }

        # Generate unique ID (timestamp-based)
        current_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        new_id <- length(doe_rv$saved_analyses) + 1

        print(paste("DEBUG: Creating new session entry with ID:", new_id))

        # Save metadata to session-based storage (not the full analysis object)
        new_entry <- list(
          id = new_id,
          name = sanitized_name,
          timestamp = current_timestamp,
          filepath = filepath, # Reference to .RData file (can be NULL)
          response_var = input$response_var,
          analysis_obj = analysis_obj # Store full object in session for doe_desir
        )

        print(paste("DEBUG: New entry created with name:", new_entry$name))

        # Add to session storage
        doe_rv$saved_analyses[[new_id]] <- new_entry
        print(paste("DEBUG: Added to session storage. New count:", length(doe_rv$saved_analyses)))

        # Sort analyses by timestamp (newest first) for dropdown display
        timestamps <- sapply(doe_rv$saved_analyses, function(x) as.POSIXct(x$timestamp))
        sorted_indices <- order(timestamps, decreasing = TRUE)
        doe_rv$saved_analyses <- doe_rv$saved_analyses[sorted_indices]
        print("DEBUG: Session storage sorted by timestamp")
        print(paste("DEBUG: Final session storage count:", length(doe_rv$saved_analyses)))

        # Show appropriate notification based on where data was saved
        if (!is.null(filepath)) {
          showNotification(
            paste(i18n$t("Analysis saved successfully to session and"), basename(filepath)),
            type = "message",
            duration = 5
          )
        } else {
          showNotification(
            i18n$t("Analysis saved successfully to session"),
            type = "message",
            duration = 5
          )
        }
      },
      error = function(e) {
        print(paste("DEBUG: ERROR in save operation:", e$message))
        print(paste("DEBUG: Error traceback:", paste(traceback(), collapse = "\n")))
        showNotification(paste(i18n$t("Error saving analysis:"), e$message), type = "error")
      }
    )
  })

  # Modal for column configuration
  # Render Data Preview section with inline controls
  output$data_preview_ui <- renderUI({
    data <- analysis_results$input_data
    
    # Check if data is valid
    if (is.null(data)) {
      # Placeholder when no data is loaded
      return(
        div(
          style = "text-align: center; padding: 20px; color: #888; background-color: #f9f9f9; border: 1px dashed #ddd; border-radius: 4px;",
          icon("table", style = "font-size: 24px; margin-bottom: 8px; display: block;"),
          tags$span(i18n$t("Load a saved design or import a CSV file to preview data."))
        )
      )
    }
    
    # Create clear button
    clear_btn <- actionButton(ns("clear_data"), i18n$t("Clear"),
      icon = icon("trash-alt"),
      class = "btn btn-outline-secondary btn-sm",
      style = "height: 34px; padding: 5px 10px;"
    )
    
    # Create download button
    download_btn <- downloadButton(ns("download_current_data"), i18n$t("Download"),
      icon = icon("download"),
      class = "btn btn-outline-secondary btn-sm",
      style = "height: 34px; padding: 5px 10px;"
    )
    
    # Create rename controls with flexible layout
    current_cols <- colnames(data)
    rename_controls <- div(
      style = "display: flex; align-items: center; gap: 5px; flex: 1;",
      tags$span(style = "font-size: 12px; color: #666; white-space: nowrap;", i18n$t("Rename")),
      tags$select(
        id = ns("rename_col_select"),
        class = "form-control form-control-sm",
        style = "width: 130px; height: 34px; flex-shrink: 0;",
        lapply(current_cols, function(col) tags$option(value = col, col))
      ),
      tags$input(
        id = ns("rename_col_newname"),
        type = "text",
        class = "form-control form-control-sm",
        style = "flex: 1; min-width: 80px; height: 34px; font-size: 12px;",
        placeholder = i18n$t("New name")
      ),
      actionButton(
        ns("rename_col_btn"),
        label = NULL,
        icon = icon("pen"),
        class = "btn btn-outline-secondary btn-sm",
        style = "height: 34px; padding: 2px 8px; flex-shrink: 0;"
      )
    )
    
    tagList(
      # Controls row
      div(
        style = "display: flex; align-items: center; justify-content: flex-start; margin-bottom: 8px; gap: 8px;",
        clear_btn,
        download_btn,
        tags$span(style = "border-left: 1px solid #ccc; height: 20px; margin-left: 4px; margin-right: 4px;"),
        rename_controls
      ),
      fluidRow(
        column(12, rHandsontableOutput(ns("data_preview")))
      )
    )
  })
  
  # Observer for column rename
  observeEvent(input$rename_col_btn, {
    old_name <- input$rename_col_select
    new_name <- trimws(input$rename_col_newname)
    
    # Validate inputs
    if (is.null(old_name)) {
      showNotification(i18n$t("Please select a column."), type = "warning")
      return()
    }
    if (old_name == "") {
      showNotification(i18n$t("Please select a column."), type = "warning")
      return()
    }
    if (is.null(new_name)) {
      showNotification(i18n$t("Please enter a new name."), type = "warning")
      return()
    }
    if (new_name == "") {
      showNotification(i18n$t("Please enter a new name."), type = "warning")
      return()
    }
    
    data <- analysis_results$input_data
    if (is.null(data)) return()
    
    if (!(old_name %in% colnames(data))) {
      showNotification(i18n$t("Column not found."), type = "error")
      return()
    }
    
    # Check if new name already exists (and is different from old name)
    if (new_name %in% colnames(data)) {
      if (new_name != old_name) {
        showNotification(i18n$t("A column with this name already exists."), type = "error")
        return()
      }
    }
    
    colnames(data)[colnames(data) == old_name] <- new_name
    analysis_results$input_data <- data
    analysis_results$table_render_trigger <- analysis_results$table_render_trigger + 1  # Trigger table re-render for column rename
    
    # Update variable selections if needed
    if (!is.null(input$time_var)) {
      if (input$time_var == old_name) {
        updateSelectInput(session, "time_var", selected = new_name)
      }
    }
    if (!is.null(input$response_var)) {
      if (input$response_var == old_name) {
        updateSelectInput(session, "response_var", selected = new_name)
      }
    }
    
    shinyjs::runjs(sprintf("document.getElementById('%s').value = '';", ns("rename_col_newname")))
    showNotification(sprintf(i18n$t("Column renamed: %s -> %s"), old_name, new_name), type = "message")
  })
  
  # Observer for clear data button
  observeEvent(input$clear_data, {
    analysis_results$input_data <- NULL
    analysis_results$analysis_result <- NULL
    analysis_results$variable_names <- NULL
    analysis_results$selectable_vars <- NULL
    prediction_results$predictions <- NULL
    analysis_results$table_render_trigger <- analysis_results$table_render_trigger + 1

    # Clear cached uncoded factor selections via JS (updateSelectInput won't work on destroyed UI)
    lapply(LETTERS, function(cf) {
      shinyjs::runjs(sprintf("Shiny.setInputValue('%s', '', {priority: 'event'})", ns(paste0("uc_for_", cf))))
    })

    shinyjs::disable("save_analysis")
    showNotification(i18n$t("Data cleared."), type = "message")
  })
  
  # Download current data handler
  output$download_current_data <- downloadHandler(
    filename = function() {
      paste0("doe_data_", format(Sys.time(), "%d-%m-%Y_%Hhrs-%Mmin-%Ssec"), ".csv")
    },
    content = function(file) {
      data <- analysis_results$input_data
      if (!is.null(data)) {
        write.csv(data, file, row.names = FALSE)
      }
    }
  )

  # Data preview with rhandsontable
  # Uses table_render_trigger to only re-render when data is loaded/cleared, not on every edit
  output$data_preview <- renderRHandsontable({
    # Depend on trigger for re-rendering
    analysis_results$table_render_trigger
    # Isolate the data read so edits don't cause re-render
    data <- isolate(analysis_results$input_data)
    if (!is.null(data)) {
      rhandsontable(data, height = 300, useTypes = FALSE) %>%
        hot_table(
          highlightCol = TRUE, 
          highlightRow = TRUE,
          contextMenu = TRUE
        ) %>%
        hot_cols(columnSorting = FALSE)
    } else {
      NULL
    }
  })

  # Handle rhandsontable edits
  observeEvent(input$data_preview, {
    if (!is.null(input$data_preview)) {
      tryCatch({
      data <- hot_to_r(input$data_preview)
      
      if (is.null(data) || !is.data.frame(data)) return()
      
      # Fix column names
      col_names <- colnames(data)
      for (i in seq_along(col_names)) {
        if (is.na(col_names[i]) || col_names[i] == "" || col_names[i] == " ") {
          col_names[i] <- paste0("Column", i)
        }
      }
      col_names <- make.unique(col_names, sep = "_")
      colnames(data) <- col_names
      
      var_names <- names(data)
      
      # Recalculate selectable_vars when data changes
      coded_cols <- grep("^[A-Z]$", var_names, value = TRUE)
      selectable_vars <- setdiff(var_names, coded_cols)
      
      analysis_results$input_data <- data
      analysis_results$variable_names <- var_names
      analysis_results$selectable_vars <- selectable_vars
      
      # Update dropdown choices to reflect any column changes
      current_time <- input$time_var
      current_response <- input$response_var
      
      # Update time_var choices (exclude current response if valid)
      if (!is.null(current_response) && current_response != "" && current_response %in% selectable_vars) {
        time_choices <- setdiff(selectable_vars, current_response)
      } else {
        time_choices <- selectable_vars
      }
      
      # Update response_var choices (exclude current time if valid)
      if (!is.null(current_time) && current_time %in% selectable_vars) {
        response_available <- setdiff(selectable_vars, current_time)
      } else {
        response_available <- selectable_vars
      }
      response_choices <- c(setNames("", i18n$t("None")), setNames(response_available, response_available))
      
      # Update choices, keeping current selection if still valid
      if (!is.null(current_time) && current_time %in% time_choices) {
        updateSelectInput(session, "time_var", choices = time_choices, selected = current_time)
      } else {
        updateSelectInput(session, "time_var", choices = time_choices, selected = time_choices[1])
      }
      
      if (!is.null(current_response) && current_response %in% c("", response_available)) {
        updateSelectInput(session, "response_var", choices = response_choices, selected = current_response)
      } else {
        updateSelectInput(session, "response_var", choices = response_choices, selected = tail(response_available, 1))
      }
      }, error = function(e) {
        # Silently ignore rhandsontable internal errors (genColHeaders, afterChange)
      })
    }
  })

  # Run analysis
  observeEvent(input$analyze, {
    # Validate inputs before running analysis
    if (is.null(analysis_results$input_data)) {
      showNotification(i18n$t("Please load data before running analysis."), type = "error")
      return()
    }
    
    if (is.null(input$response_var) || input$response_var == "") {
      showNotification(i18n$t("Please select a response variable."), type = "error")
      return()
    }
    
    # Determine uncoded factors: must be all specified or none
    selected_uc_facs <- get_selected_uc_facs()
    coded_cols_for_analysis <- sort(grep("^[A-Z]$", names(analysis_results$input_data), value = TRUE))
    n_coded <- length(coded_cols_for_analysis)
    
    if (length(selected_uc_facs) > 0 && length(selected_uc_facs) < n_coded) {
      showNotification(
        i18n$t("Either all or none of the coded factors must be associated with uncoded factors. Please complete the assignments or clear them all."),
        type = "error", duration = 8
      )
      return()
    }
    
    tryCatch(
      {
        # Show progress
        withProgress(message = i18n$t("Running analysis..."), value = 0, {
          # Prepare data
          incProgress(0.2, detail = i18n$t("Preparing data..."))
          data <- analysis_results$input_data

          # Prepare uncoded factors from dropdown selections
          selected_uc_facs <- get_selected_uc_facs()

          # Determine canon_thres value
          canon_thres_val <- if (canon_thres_enabled() && !is.null(input$canon_thres_value)) {
            as.numeric(input$canon_thres_value)
          } else {
            "auto"
          }

          # Run DOE analysis
          incProgress(0.6, detail = i18n$t("Running DOE analysis..."))
          
          uc_facs_val <- if (length(selected_uc_facs) == n_coded) selected_uc_facs else NA
          
          result <- doe_analyze(
            doe = data,
            uc_facs = uc_facs_val,
            cent_id = NA,
            resp_var = input$response_var,
            time_var = if (input$time_var == "") NULL else input$time_var,
            mod_order = as.numeric(input$mod_order),
            canon_thres = canon_thres_val,
            p_cutoff = input$p_cutoff,
            trim_method = input$trim_method,
            which_facs = "coded",
            export = "none",
            asprat = "default",
            verbose = FALSE
          )

          # Validate that the model produced meaningful results
          # Check if final model has any terms besides intercept
          final_results <- result$results$final$Model_Results
          if (!is.null(final_results)) {
            # Get terms excluding intercept
            terms <- final_results$Term
            non_intercept_terms <- terms[!grepl("^\\(Intercept\\)$|^Intercept$", terms, ignore.case = TRUE)]
            
            if (length(non_intercept_terms) == 0) {
              stop(i18n$t("Model fitting failed: All factors were removed during model trimming. The final model contains only the intercept. Try adjusting the P-value cutoff or trim method."))
            }
            
            # Check if all non-intercept terms have non-significant p-values (using fixed 0.10 cutoff)
            if ("p_value" %in% names(final_results)) {
              non_intercept_idx <- !grepl("^\\(Intercept\\)$|^Intercept$", final_results$Term, ignore.case = TRUE)
              p_values <- final_results$p_value[non_intercept_idx]
              
              if (length(p_values) > 0 && all(p_values > 0.10, na.rm = TRUE)) {
                # This is a warning, not an error - model can still be shown
                showNotification(
                  i18n$t("Warning: No significant factors found at the specified p-value cutoff."),
                  type = "warning",
                  duration = 8
                )
              }
            }
          }

          # Store results
          analysis_results$analysis_result <- result

          analysis_results$response_var <- input$response_var
          analysis_results$time_var <- input$time_var

          # Enable save button now that analysis is complete
          shinyjs::enable("save_analysis")

          # Check if initial and final models are identical
          initial_eq <- result$results$initial$Misc$Equation[["raw"]]
          final_eq <- result$results$final$Misc$Equation[["raw"]]

          if (!is.null(initial_eq) && !is.null(final_eq) && identical(initial_eq, final_eq)) {
            # Models are identical - could hide initial tab or show a message
            analysis_results$models_identical <- TRUE
          } else {
            analysis_results$models_identical <- FALSE
          }

          incProgress(1, detail = i18n$t("Analysis completed!"))
        })

        showNotification(i18n$t("Analysis completed successfully!"), type = "message")
      },
      error = function(e) {
        showNotification(paste(i18n$t("Analysis error"), e$message), type = "error")
      }
    )
  })


  # Initial Model Summary
  output$initial_model_summary <- renderUI({
    req(analysis_results$analysis_result)
    result <- analysis_results$analysis_result
    init_metrics <- result$results$initial$Model_Metrics
    init_misc <- result$results$initial$Misc

    # ===== TRANSLATION DEFINITIONS =====
    txt <- list(
      response_var = i18n_r()$t("Response Variable"),
      model_order = i18n_r()$t("Model Order"),
      r2 = i18n_r()$t("R²:"),
      adj_r2 = i18n_r()$t("Adjusted R²:"),
      f_stat = i18n_r()$t("F-statistic:"),
      resid_err = i18n_r()$t("Residual Std Error:"),
      lof_pval = i18n_r()$t("Lack-of-Fit p-value:"),
      model_eq = i18n_r()$t("Model Equation:"),
      na = i18n_r()$t("N/A")
    )

    # LoF badge color - handle NULL, NaN, NA values
    lof_val <- init_metrics$LoF_Pvalue
    
    # Determine display based on value
    if (is.null(lof_val) || length(lof_val) == 0) {
      lof_color <- "secondary"
      lof_display <- i18n_r()$t("Test failed")
    } else if (is.nan(lof_val)) {
      lof_color <- "secondary"
      lof_display <- i18n_r()$t("Test failed")
    } else if (is.na(lof_val)) {
      lof_color <- "secondary"
      lof_display <- i18n_r()$t("Test failed")
    } else if (lof_val > 0.05) {
      lof_color <- "success"
      lof_display <- sprintf("%.4f", lof_val)
    } else {
      lof_color <- "danger"
      lof_display <- sprintf("%.4f", lof_val)
    }
    lof_badge <- sprintf('<span class="badge badge-%s">%s</span>', lof_color, lof_display)

    # Build the summary HTML (without equation)
    summary_html <- paste0(
      '<div style="font-size: 14px;">',
      "<p><strong>", txt$response_var, "</strong> ", analysis_results$response_var, "</p>",
      "<p><strong>", txt$model_order, "</strong> ", init_metrics$Order, "</p>",
      "<hr>",
      "<p><strong>", txt$r2, "</strong> ", sprintf("%.4f", init_metrics$R2), "</p>",
      "<p><strong>", txt$adj_r2, "</strong> ", sprintf("%.4f", init_metrics$Adj_R2), "</p>",
      "<p><strong>", txt$f_stat, "</strong> ", sprintf("%.2f", init_metrics$F_Statistic),
      " (df1=", init_metrics$F_DOF_1, ", df2=", init_metrics$F_DOF_2, ")</p>",
      "<p><strong>", txt$resid_err, "</strong> ", sprintf("%.4f", init_metrics$Residual_Stnd_Error), "</p>",
      "<p><strong>", txt$lof_pval, "</strong> ", lof_badge, "</p>",
      "<hr>",
      "<p><strong>", txt$model_eq, "</strong></p>",
      "</div>"
    )

    # Return tagList with summary and KaTeX equation
    tagList(
      HTML(summary_html),
      render_equation_katex(
        init_misc$Equation[["raw"]], 
        response_var = analysis_results$response_var,
        element_id = ns("init_model_eq")
      )
    )
  })

  # Final Model Summary
  output$final_model_summary <- renderUI({
    req(analysis_results$analysis_result)
    result <- analysis_results$analysis_result
    fin_metrics <- result$results$final$Model_Metrics
    fin_misc <- result$results$final$Misc

    # ===== TRANSLATION DEFINITIONS =====
    txt <- list(
      response_var = i18n_r()$t("Response Variable"),
      model_order = i18n_r()$t("Model Order"),
      r2 = i18n_r()$t("R²:"),
      adj_r2 = i18n_r()$t("Adjusted R²:"),
      f_stat = i18n_r()$t("F-statistic:"),
      resid_err = i18n_r()$t("Residual Std Error:"),
      lof_pval = i18n_r()$t("Lack-of-Fit p-value:"),
      model_eq = i18n_r()$t("Model Equation:"),
      na = i18n_r()$t("N/A")
    )

    # LoF badge color - handle NULL, NaN, NA values
    lof_val <- fin_metrics$LoF_Pvalue
    
    # Determine display based on value
    if (is.null(lof_val) || length(lof_val) == 0) {
      lof_color <- "secondary"
      lof_display <- i18n_r()$t("Test failed")
    } else if (is.nan(lof_val)) {
      lof_color <- "secondary"
      lof_display <- i18n_r()$t("Test failed")
    } else if (is.na(lof_val)) {
      lof_color <- "secondary"
      lof_display <- i18n_r()$t("Test failed")
    } else if (lof_val > 0.05) {
      lof_color <- "success"
      lof_display <- sprintf("%.4f", lof_val)
    } else {
      lof_color <- "danger"
      lof_display <- sprintf("%.4f", lof_val)
    }
    lof_badge <- sprintf('<span class="badge badge-%s">%s</span>', lof_color, lof_display)

    # Build the summary HTML (without equation)
    summary_html <- paste0(
      '<div style="font-size: 14px;">',
      "<p><strong>", txt$response_var, "</strong> ", analysis_results$response_var, "</p>",
      "<p><strong>", txt$model_order, "</strong> ", fin_metrics$Order, "</p>",
      "<hr>",
      "<p><strong>", txt$r2, "</strong> ", sprintf("%.4f", fin_metrics$R2), "</p>",
      "<p><strong>", txt$adj_r2, "</strong> ", sprintf("%.4f", fin_metrics$Adj_R2), "</p>",
      "<p><strong>", txt$f_stat, "</strong> ", sprintf("%.2f", fin_metrics$F_Statistic),
      " (df1=", fin_metrics$F_DOF_1, ", df2=", fin_metrics$F_DOF_2, ")</p>",
      "<p><strong>", txt$resid_err, "</strong> ", sprintf("%.4f", fin_metrics$Residual_Stnd_Error), "</p>",
      "<p><strong>", txt$lof_pval, "</strong> ", lof_badge, "</p>",
      "<hr>",
      "<p><strong>", txt$model_eq, "</strong></p>",
      "</div>"
    )

    # Return tagList with summary and KaTeX equation
    tagList(
      HTML(summary_html),
      render_equation_katex(
        fin_misc$Equation[["raw"]], 
        response_var = analysis_results$response_var,
        element_id = ns("fin_model_eq")
      )
    )
  })

  # Trimming Information
  output$trimming_info <- renderUI({
    req(analysis_results$analysis_result)
    result <- analysis_results$analysis_result
    statements <- result$statements

    # ===== TRANSLATION DEFINITIONS =====
    txt <- list(
      model_trimmed = i18n_r()$t("Model Was Trimmed:"),
      badge_no = i18n_r()$t("NO"),
      badge_yes = i18n_r()$t("YES"),
      p_cutoff = i18n_r()$t("P-value Cutoff:"),
      stepwise = i18n_r()$t("Stepwise Regression:"),
      remaining_effs = i18n_r()$t("Remaining Effects:"),
      hierarchy = i18n_r()$t("Hierarchy Principle:")
    )

    # Helper to translate source-code generated statements
    translate_trim_stmt <- function(text) {
      # Static statements
      text <- sub("^The initial model was not truncated via either a p-value cutoff or stepwise regression\\.$",
                  i18n_r()$t("The initial model was not truncated via either a p-value cutoff or stepwise regression."), text)
      text <- sub("^No effects were removed from the model\\.$",
                  i18n_r()$t("No effects were removed from the model."), text)
      text <- sub("^No effects were removed from the model since all were significant\\.$",
                  i18n_r()$t("No effects were removed from the model since all were significant."), text)
      text <- sub("^No effects were added to the model upon applying the Hierarchy Principle\\.$",
                  i18n_r()$t("No effects were added to the model upon applying the Hierarchy Principle."), text)
      text <- sub("^No effects were removed from the initial model since Lack-of-Fit testing failed.*$",
                  i18n_r()$t("No effects were removed from the initial model since Lack-of-Fit testing failed and/or the model became too simple after trimming!"), text)
      # Dynamic statements with numbers and factor lists
      text <- sub("^Stepwise regression based on AIC removed (\\d+) factors from the initial model:",
                  paste0(i18n_r()$t("Stepwise regression based on AIC removed"), " \\1 ", i18n_r()$t("factors from the initial model"), ":"), text)
      text <- sub("^A p-value cutoff removed (\\d+) factors from the initial model:",
                  paste0(i18n_r()$t("A p-value cutoff removed"), " \\1 ", i18n_r()$t("factors from the initial model"), ":"), text)
      text <- sub("^After removing insignificant effects via (.+) method\\(s\\), the following (\\d+) were left in the model:",
                  paste0(i18n_r()$t("After removing insignificant effects via"), " \\1 ", i18n_r()$t("method(s), the following"), " \\2 ", i18n_r()$t("were left in the model"), ":"), text)
      text <- sub("^The following main effects were added to the model upon applying the Hierarchy Principle:",
                  paste0(i18n_r()$t("The following main effects were added to the model upon applying the Hierarchy Principle"), ":"), text)
      text <- sub("The full list of terms included is now:",
                  paste0(i18n_r()$t("The full list of terms included is now"), ":"), text)
      text
    }

    # Collect trimming information
    trim_info <- c()
    trim_badge <- paste0('<span class="badge badge-success">', txt$badge_no, "</span>") # Default to NO

    if ("Notrim" %in% names(statements)) {
      trim_info <- c(trim_info, paste0("<p>", translate_trim_stmt(statements["Notrim"]), "</p>"))
    } else {
      trim_badge <- paste0('<span class="badge badge-warning">', txt$badge_yes, "</span>")
    }

    if ("p_cutoff" %in% names(statements)) {
      trim_info <- c(trim_info, paste0("<p><strong>", txt$p_cutoff, "</strong><br>", translate_trim_stmt(statements["p_cutoff"]), "</p>"))
    }

    if ("Stepwise" %in% names(statements)) {
      trim_info <- c(trim_info, paste0("<p><strong>", txt$stepwise, "</strong><br>", translate_trim_stmt(statements["Stepwise"]), "</p>"))
    }

    if ("Signif_Effs" %in% names(statements)) {
      trim_info <- c(trim_info, paste0("<p><strong>", txt$remaining_effs, "</strong><br>", translate_trim_stmt(statements["Signif_Effs"]), "</p>"))
    }

    if ("H_Principle" %in% names(statements)) {
      trim_info <- c(trim_info, paste0("<p><strong>", txt$hierarchy, "</strong><br>", translate_trim_stmt(statements["H_Principle"]), "</p>"))
    }

    html_content <- paste0(
      '<div style="font-size: 14px;">',
      "<p><strong>", txt$model_trimmed, "</strong> ", trim_badge, "</p>",
      "<hr>",
      paste(trim_info, collapse = "<hr>"),
      "</div>"
    )

    HTML(html_content)
  })

  # FAZE 2: Optimization Results Tab Outputs

  # Helper function to create optimization summary for any model type (initial/final)
  create_optimization_summary <- function(model_type = "final") {
    req(analysis_results$analysis_result)
    result <- analysis_results$analysis_result

    # ===== TRANSLATION DEFINITIONS =====
    txt <- list(
      # Table headers
      table_method = i18n_r()$t("Method"),
      table_description = i18n_r()$t("Description & Results"),

      # Canonical Analysis
      canonical_title = i18n_r()$t("Canonical Analysis"),
      canonical_subtitle = i18n_r()$t("RSM Method"),
      canonical_purpose = i18n_r()$t("Finds stationary point via eigenvalue decomposition"),
      canonical_bestfor = i18n_r()$t("Quadratic models (order = 2)"),
      canonical_unavailable = i18n_r()$t("Not available (requires quadratic model)"),

      # Traditional Optimization
      traditional_title = i18n_r()$t("Traditional Optimization"),
      traditional_subtitle = i18n_r()$t("Numerical Method"),
      traditional_purpose = i18n_r()$t("Finds min/max via numerical optimization (L-BFGS-B)"),
      traditional_bestfor = i18n_r()$t("All model types (linear, interaction, quadratic)"),
      traditional_unavailable = i18n_r()$t("Not available"),

      # Common labels
      label_purpose = i18n_r()$t("Purpose:"),
      label_bestfor = i18n_r()$t("Best for:"),
      label_result = i18n_r()$t("Result:"),
      label_results = i18n_r()$t("Results:"),
      label_at_response = i18n_r()$t("at response ="),
      label_minimum = i18n_r()$t("Minimum"),
      label_maximum = i18n_r()$t("Maximum"),

      # Stationary point types
      type_maximum = i18n_r()$t("Maximum"),
      type_minimum = i18n_r()$t("Minimum"),
      type_saddle = i18n_r()$t("Saddle Point"),

      # Note
      note_label = i18n_r()$t("Note:"),
      note_text = i18n_r()$t("Canonical Analysis provides the theoretical optimum for quadratic models. Traditional Optimization gives practical min/max bounds for all model types.")
    )

    # ===== HTML CONSTRUCTION =====
    ca <- result$results[[model_type]]$Model_Metrics$Canonical_Analysis
    trad_opt <- result$results[[model_type]]$Model_Metrics$Trad_Opt

    html_parts <- list()

    # Clean, minimal table-based layout
    html_parts <- c(html_parts, paste0(
      '<div style="padding: 20px; background-color: #ffffff; border: 1px solid #e0e0e0; border-radius: 4px;">',
      '<table style="width: 100%; border-collapse: collapse; font-size: 14px; line-height: 1.6;">',
      "<thead>",
      '<tr style="border-bottom: 2px solid #333;">',
      '<th style="padding: 12px 8px; text-align: left; width: 180px; font-weight: 600;">', txt$table_method, "</th>",
      '<th style="padding: 12px 8px; text-align: left; font-weight: 600;">', txt$table_description, "</th>",
      "</tr>",
      "</thead>",
      "<tbody>"
    ))

    # Method 1: Canonical Analysis
    if (!is.null(ca)) {
      eigen_vals <- ca$eigen$values
      all_neg <- all(eigen_vals < 0)
      all_pos <- all(eigen_vals > 0)
      type <- if (all_neg) txt$type_maximum else if (all_pos) txt$type_minimum else txt$type_saddle

      html_parts <- c(html_parts, paste0(
        '<tr style="border-bottom: 1px solid #e0e0e0;">',
        '<td style="padding: 16px 8px; vertical-align: top;">',
        '<strong style="color: #333;">', txt$canonical_title, "</strong><br>",
        '<span style="font-size: 12px; color: #666;">(', txt$canonical_subtitle, ")</span>",
        "</td>",
        '<td style="padding: 16px 8px;">',
        '<p style="margin: 0 0 8px 0;"><strong>', txt$label_purpose, "</strong> ", txt$canonical_purpose, "</p>",
        '<p style="margin: 0 0 8px 0;"><strong>', txt$label_bestfor, "</strong> ", txt$canonical_bestfor, "</p>",
        '<p style="margin: 0;"><strong>', txt$label_result, "</strong> ", type, " ", txt$label_at_response, " ", sprintf("%.4f", ca$predicted), "</p>",
        "</td>",
        "</tr>"
      ))
    } else {
      html_parts <- c(html_parts, paste0(
        '<tr style="border-bottom: 1px solid #e0e0e0;">',
        '<td style="padding: 16px 8px; vertical-align: top;">',
        '<strong style="color: #333;">', txt$canonical_title, "</strong><br>",
        '<span style="font-size: 12px; color: #666;">(', txt$canonical_subtitle, ")</span>",
        "</td>",
        '<td style="padding: 16px 8px; color: #999;">',
        txt$canonical_unavailable,
        "</td>",
        "</tr>"
      ))
    }

    # Method 2: Traditional Optimization
    if (!is.null(trad_opt) && !is.null(trad_opt$coded)) {
      coded_df <- trad_opt$coded
      min_val <- coded_df[[analysis_results$response_var]][1]
      max_val <- coded_df[[analysis_results$response_var]][2]

      html_parts <- c(html_parts, paste0(
        "<tr>",
        '<td style="padding: 16px 8px; vertical-align: top;">',
        '<strong style="color: #333;">', txt$traditional_title, "</strong><br>",
        '<span style="font-size: 12px; color: #666;">(', txt$traditional_subtitle, ")</span>",
        "</td>",
        '<td style="padding: 16px 8px;">',
        '<p style="margin: 0 0 8px 0;"><strong>', txt$label_purpose, "</strong> ", txt$traditional_purpose, "</p>",
        '<p style="margin: 0 0 8px 0;"><strong>', txt$label_bestfor, "</strong> ", txt$traditional_bestfor, "</p>",
        '<p style="margin: 0;">',
        "<strong>", txt$label_results, "</strong><br>",
        "&nbsp;&nbsp;• ", txt$label_minimum, " = ", sprintf("%.4f", min_val), "<br>",
        "&nbsp;&nbsp;• ", txt$label_maximum, " = ", sprintf("%.4f", max_val),
        "</p>",
        "</td>",
        "</tr>"
      ))
    } else {
      html_parts <- c(html_parts, paste0(
        "<tr>",
        '<td style="padding: 16px 8px; vertical-align: top;">',
        '<strong style="color: #333;">', txt$traditional_title, "</strong><br>",
        '<span style="font-size: 12px; color: #666;">(', txt$traditional_subtitle, ")</span>",
        "</td>",
        '<td style="padding: 16px 8px; color: #999;">',
        txt$traditional_unavailable,
        "</td>",
        "</tr>"
      ))
    }

    html_parts <- c(html_parts, "</tbody></table>")

    # Minimal recommendation (only if both methods available)
    if (!is.null(ca) && !is.null(trad_opt)) {
      html_parts <- c(html_parts, paste0(
        '<div style="margin-top: 16px; padding: 12px; background-color: #f8f9fa; border-left: 3px solid #666; font-size: 13px; color: #555;">',
        "<strong>", txt$note_label, "</strong> ", txt$note_text,
        "</div>"
      ))
    }

    html_parts <- c(html_parts, "</div>")

    return(HTML(paste(html_parts, collapse = "")))
  }

  # Optimization Methods Summary - Final Model
  output$optimization_methods_summary_final <- renderUI({
    create_optimization_summary("final")
  })

  # Optimization Methods Summary - Initial Model
  output$optimization_methods_summary_initial <- renderUI({
    create_optimization_summary("initial")
  })

  # Optimization Type - OLD (keeping for backwards compatibility, remove later)
  output$optimization_methods_summary <- renderUI({
    req(analysis_results$analysis_result)
    result <- analysis_results$analysis_result

    ca <- result$results$final$Model_Metrics$Canonical_Analysis
    trad_opt <- result$results$final$Model_Metrics$Trad_Opt

    html_parts <- list()

    # Introduction
    html_parts <- c(html_parts, paste0(
      '<div style="padding: 15px; background-color: #f8f9fa; border-radius: 5px; margin-bottom: 20px;">',
      '<h4 style="color: #0056b3; margin-top: 0;">Two Optimization Approaches</h4>',
      '<p style="font-size: 14px; line-height: 1.6;">',
      "This analysis uses <strong>two complementary methods</strong> to find optimal factor settings:",
      "</p>",
      "</div>"
    ))

    # Method 1: Canonical Analysis
    if (!is.null(ca)) {
      eigen_vals <- ca$eigen$values
      all_neg <- all(eigen_vals < 0)
      all_pos <- all(eigen_vals > 0)
      type <- if (all_neg) "Maximum" else if (all_pos) "Minimum" else "Saddle Point"
      badge_color <- if (all_neg) "success" else if (all_pos) "primary" else "warning"

      html_parts <- c(html_parts, paste0(
        '<div style="padding: 15px; background-color: #e7f3ff; border-left: 4px solid #007bff; margin-bottom: 15px;">',
        '<h5 style="color: #007bff; margin-top: 0;"><i class="fa fa-flask"></i> Method 1: Canonical Analysis (RSM)</h5>',
        '<p style="font-size: 13px; margin-bottom: 10px;">',
        "<strong>Purpose:</strong> Finds the stationary point of the response surface using eigenvalue decomposition.<br>",
        "<strong>Best for:</strong> Quadratic models (order = 2) with curvature.<br>",
        '<strong>Result:</strong> <span class="badge badge-', badge_color, '">', type, "</span> at predicted response = <strong>",
        sprintf("%.4f", ca$predicted), "</strong>",
        "</p>",
        "</div>"
      ))
    } else {
      html_parts <- c(html_parts, paste0(
        '<div style="padding: 15px; background-color: #fff3cd; border-left: 4px solid #ffc107; margin-bottom: 15px;">',
        '<h5 style="color: #856404; margin-top: 0;"><i class="fa fa-info-circle"></i> Method 1: Canonical Analysis</h5>',
        '<p style="font-size: 13px; color: #856404;">',
        "<strong>Status:</strong> Not available (requires quadratic model with order = 2)",
        "</p>",
        "</div>"
      ))
    }

    # Method 2: Traditional Optimization
    if (!is.null(trad_opt) && !is.null(trad_opt$coded)) {
      coded_df <- trad_opt$coded
      min_val <- coded_df[[analysis_results$response_var]][1]
      max_val <- coded_df[[analysis_results$response_var]][2]

      html_parts <- c(html_parts, paste0(
        '<div style="padding: 15px; background-color: #e6ffed; border-left: 4px solid #28a745; margin-bottom: 15px;">',
        paste0('<h5 style="color: #28a745; margin-top: 0;"><i class="fa fa-calculator"></i> ', i18n_r()$t("Method 2: Traditional Optimization"), '</h5>'),
        '<p style="font-size: 13px; margin-bottom: 10px;">',
        "<strong>Purpose:</strong> Uses numerical optimization (L-BFGS-B) to find min/max predictions within design space.<br>",
        "<strong>Best for:</strong> All model types (linear, interaction, quadratic).<br>",
        "<strong>Results:</strong><br>",
        '&nbsp;&nbsp;• <span style="color: #007bff;">Minimum response</span> = <strong>', sprintf("%.4f", min_val), "</strong><br>",
        '&nbsp;&nbsp;• <span style="color: #28a745;">Maximum response</span> = <strong>', sprintf("%.4f", max_val), "</strong>",
        "</p>",
        "</div>"
      ))
    } else {
      html_parts <- c(html_parts, paste0(
        '<div style="padding: 15px; background-color: #f8d7da; border-left: 4px solid #dc3545; margin-bottom: 15px;">',
        paste0('<h5 style="color: #721c24; margin-top: 0;"><i class="fa fa-exclamation-triangle"></i> ', i18n_r()$t("Method 2: Traditional Optimization"), '</h5>'),
        '<p style="font-size: 13px; color: #721c24;">',
        "<strong>Status:</strong> Not available (optimization failed or data missing)",
        "</p>",
        "</div>"
      ))
    }

    # Summary recommendation
    if (!is.null(ca) && !is.null(trad_opt)) {
      html_parts <- c(html_parts, paste0(
        '<div style="padding: 15px; background-color: #f0f2f5; border-radius: 5px; margin-top: 20px;">',
        '<p style="font-size: 13px; margin: 0; color: #555;">',
        '<strong><i class="fa fa-lightbulb-o"></i> Recommendation:</strong> ',
        "For quadratic models, <strong>Canonical Analysis</strong> provides the theoretical optimum. ",
        "<strong>Traditional Optimization</strong> gives practical min/max bounds. ",
        "Compare both methods to understand your design space thoroughly.",
        "</p>",
        "</div>"
      ))
    }

    HTML(paste(html_parts, collapse = ""))
  })

  # Helper: Create Optimization Type output
  create_optimization_type <- function(model_type = "final") {
    req(analysis_results$analysis_result)
    result <- analysis_results$analysis_result

    # Check if canonical analysis exists (quadratic model)
    if (!is.null(result$results[[model_type]]$Model_Metrics$Canonical_Analysis)) {
      ca <- result$results[[model_type]]$Model_Metrics$Canonical_Analysis
      eigen_vals <- ca$eigen$values

      # Determine type from eigenvalues
      all_neg <- all(eigen_vals < 0)
      all_pos <- all(eigen_vals > 0)

      type <- if (all_neg) {
        i18n_r()$t("Maximum")
      } else if (all_pos) {
        i18n_r()$t("Minimum")
      } else {
        i18n_r()$t("Saddle Point")
      }

      # Badge color
      badge_color <- if (all_neg) "success" else if (all_pos) "primary" else "warning"

      html_content <- paste0(
        '<div style="text-align: center; padding: 20px;">',
        '<h2><span class="badge badge-', badge_color, '" style="font-size: 24px;">', type, "</span></h2>",
        '<p style="margin-top: 15px; font-size: 14px;">',
        i18n_r()$t("Based on eigenvalue analysis of the response surface"),
        "</p>",
        "</div>"
      )
    } else {
      html_content <- paste0(
        '<div style="text-align: center; padding: 20px;">',
        '<p class="text-muted">', i18n_r()$t("Canonical analysis not available for this model order"), '</p>',
        "</div>"
      )
    }

    return(HTML(html_content))
  }

  # Optimization Type - Final
  output$optimization_type_final <- renderUI({
    create_optimization_type("final")
  })

  # Optimization Type - Initial
  output$optimization_type_initial <- renderUI({
    create_optimization_type("initial")
  })

  # Optimization Type - OLD (backwards compatibility)
  output$optimization_type <- renderUI({
    create_optimization_type("final")
  })

  # Predicted Response - Final
  output$predicted_response_final <- renderUI({
    create_predicted_response_output(analysis_results, "final", i18n_r())
  })

  # Predicted Response - Initial
  output$predicted_response_initial <- renderUI({
    create_predicted_response_output(analysis_results, "initial", i18n_r())
  })

  # Predicted Response - OLD (backwards compatibility)
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
        i18n_r()$t("Predicted"), " ", analysis_results$response_var, " ", i18n_r()$t("at stationary point"),
        "</p>",
        "</div>"
      )
    } else {
      html_content <- paste0(
        '<div style="text-align: center; padding: 20px;">',
        '<p class="text-muted">', i18n_r()$t("Not available"), "</p>",
        "</div>"
      )
    }

    HTML(html_content)
  })

  # ============================================================================
  # ALL OPTIMIZATION OUTPUTS - Initial and Final Models
  # ============================================================================

  # Optimal Conditions Detailed - Final
  output$optimal_conditions_detailed_final <- DT::renderDataTable(
    {
      create_optimal_conditions_detailed(analysis_results, "final")
    },
    server = FALSE
  )

  # Optimal Conditions Detailed - Initial
  output$optimal_conditions_detailed_initial <- DT::renderDataTable(
    {
      create_optimal_conditions_detailed(analysis_results, "initial")
    },
    server = FALSE
  )

  # Min/Max Comparison - Final
  output$minmax_comparison_final <- DT::renderDataTable(
    {
      create_minmax_comparison(analysis_results, "final")
    },
    server = FALSE
  )

  # Min/Max Comparison - Initial
  output$minmax_comparison_initial <- DT::renderDataTable(
    {
      create_minmax_comparison(analysis_results, "initial")
    },
    server = FALSE
  )

  # Eigenvalues Table - Final
  output$eigenvalues_table_final <- DT::renderDataTable(
    {
      create_eigenvalues_table(analysis_results, "final")
    },
    server = FALSE
  )

  # Eigenvalues Table - Initial
  output$eigenvalues_table_initial <- DT::renderDataTable(
    {
      create_eigenvalues_table(analysis_results, "initial")
    },
    server = FALSE
  )

  # CA Warnings - Final
  output$ca_warnings_final <- renderUI({
    create_ca_warnings(analysis_results, "final")
  })

  # CA Warnings - Initial
  output$ca_warnings_initial <- renderUI({
    create_ca_warnings(analysis_results, "initial")
  })

  # Steepest Ascent Table - Final
  output$steepest_ascent_table_final <- DT::renderDataTable(
    {
      create_steepest_ascent_table(analysis_results, "final")
    },
    server = FALSE
  )

  # Steepest Ascent Table - Initial
  output$steepest_ascent_table_initial <- DT::renderDataTable(
    {
      create_steepest_ascent_table(analysis_results, "initial")
    },
    server = FALSE
  )

  # ============================================================================
  # OLD OUTPUTS - For backwards compatibility (these use final model)
  # ============================================================================

  # Optimal Conditions Detailed (Coded & Decoded) - CANONICAL ANALYSIS - OLD
  output$optimal_conditions_detailed <- DT::renderDataTable(
    {
      req(analysis_results$analysis_result)
      result <- analysis_results$analysis_result
      ca <- result$results$final$Model_Metrics$Canonical_Analysis

      if (!is.null(ca)) {
        # Build the optimal conditions data frame
        optimal_df <- data.frame(
          Factor = names(ca$xs),
          Coded = round(as.vector(ca$xs), 4),
          stringsAsFactors = FALSE
        )

        # Add decoded values if available
        if (!is.null(ca$xs_decoded)) {
          optimal_df$Decoded <- round(as.vector(ca$xs_decoded), 4)
        }

        DT::datatable(
          optimal_df,
          options = list(
            dom = "t",
            pageLength = 20,
            language = tablang()
          ),
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = "caption-side: top; text-align: center; color: #666; font-size: 14px; padding: 10px;",
            "Stationary point from Canonical Analysis (for quadratic models)"
          )
        )
      } else {
        DT::datatable(
          data.frame(Message = i18n$t("Canonical analysis not available (requires quadratic model)")),
          options = list(dom = "t", language = tablang()),
          rownames = FALSE
        )
      }
    },
    server = FALSE
  )

  # Min/Max Comparison - TRADITIONAL OPTIMIZATION
  output$minmax_comparison <- DT::renderDataTable(
    {
      req(analysis_results$analysis_result)
      result <- analysis_results$analysis_result

      if (!is.null(result$results$final$Model_Metrics$Trad_Opt)) {
        trad_opt <- result$results$final$Model_Metrics$Trad_Opt

        # Determine if we should show coded, decoded, or both
        has_coded <- !is.null(trad_opt$coded)
        has_decoded <- !is.null(trad_opt$decoded)

        if (has_coded && has_decoded) {
          # Show both coded and decoded factor values
          coded_df <- trad_opt$coded
          decoded_df <- trad_opt$decoded

          if (nrow(coded_df) >= 2 && nrow(decoded_df) >= 2) {
            coded_factor_cols <- setdiff(names(coded_df), analysis_results$response_var)
            decoded_factor_cols <- setdiff(names(decoded_df), analysis_results$response_var)

            comparison_list <- list(
              Type = c(i18n$t("Minimum"), i18n$t("Maximum"))
            )

            # Coded factors (A, B, C)
            for (col in coded_factor_cols) {
              comparison_list[[col]] <- round(coded_df[[col]], 4)
            }

            # Decoded factors (actual names like Pressure, Temperature)
            for (col in decoded_factor_cols) {
              comparison_list[[col]] <- round(decoded_df[[col]], 4)
            }

            # Response
            comparison_list[[i18n$t("Response")]] <- round(coded_df[[analysis_results$response_var]], 4)

            comparison_df <- as.data.frame(comparison_list, stringsAsFactors = FALSE, check.names = FALSE)
            colnames(comparison_df)[1] <- i18n$t("Type")

            DT::datatable(
              comparison_df,
              options = list(
                dom = "t",
                pageLength = 10,
                scrollX = TRUE,
                language = tablang()
              ),
              rownames = FALSE,
              caption = htmltools::tags$caption(
                style = "caption-side: top; text-align: center; color: #666; font-size: 14px; padding: 10px;",
                i18n$t("Min/Max predictions from Traditional Optimization")
              )
            )
          } else {
            DT::datatable(
              data.frame(Message = i18n$t("Insufficient optimization data")),
              options = list(dom = "t", language = tablang()),
              rownames = FALSE
            )
          }
        } else if (has_coded) {
          # Show only coded values with decoded factor names if available
          coded_df <- trad_opt$coded

          if (nrow(coded_df) >= 2) {
            factor_cols <- setdiff(names(coded_df), analysis_results$response_var)
            realnames <- result$models$final$realnames
            # Build named mapping
            codenames_vec <- factor_cols
            rn_map <- if (!is.null(realnames) && length(realnames) == length(codenames_vec)) {
              setNames(realnames, codenames_vec)
            } else NULL

            comparison_list <- list(
              Type = c(i18n$t("Minimum"), i18n$t("Maximum"))
            )

            for (col in factor_cols) {
              col_label <- if (!is.null(rn_map) && col %in% names(rn_map)) rn_map[[col]] else col
              comparison_list[[col_label]] <- round(coded_df[[col]], 4)
            }

            comparison_list[[i18n$t("Response")]] <- round(coded_df[[analysis_results$response_var]], 4)

            comparison_df <- as.data.frame(comparison_list, stringsAsFactors = FALSE, check.names = FALSE)
            colnames(comparison_df)[1] <- i18n$t("Type")

            DT::datatable(
              comparison_df,
              colnames = curr_cols,
              options = list(
                dom = "t",
                pageLength = 10,
                language = tablang()
              ),
              rownames = FALSE,
              caption = htmltools::tags$caption(
                style = "caption-side: top; text-align: center; color: #666; font-size: 14px; padding: 10px;",
                i18n$t("Min/Max predictions (coded values only)")
              )
            )
          } else {
            DT::datatable(
              data.frame(Message = i18n$t("Insufficient optimization data")),
              options = list(dom = "t", language = tablang()),
              rownames = FALSE
            )
          }
        } else {
          DT::datatable(
            data.frame(Message = i18n$t("Traditional optimization data format issue")),
            options = list(dom = "t", language = tablang()),
            rownames = FALSE
          )
        }
      } else {
        DT::datatable(
          data.frame(Message = i18n$t("Traditional optimization not available")),
          options = list(dom = "t", language = tablang()),
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
            pageLength = 20,
            language = tablang()
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
          options = list(dom = "t", language = tablang()),
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
            pageLength = 10,
            language = tablang()
          ),
          rownames = FALSE
        )
      } else {
        DT::datatable(
          data.frame(Message = "Steepest Ascent not available for this model"),
          options = list(dom = "t", language = tablang()),
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
        # Translate dynamic source-code statements
        if (grepl("Optim_1_", name)) {
          text <- sub("^The (initial|final) model yielded the following stationary point",
                      i18n_r()$t("The model yielded the following stationary point"), text)
          text <- sub("\\(uncoded values in brackets if available\\)",
                      paste0("(", i18n_r()$t("uncoded values in brackets if available"), ")"), text)
          text <- sub(", with eigenvalues:", paste0(", ", i18n_r()$t("with eigenvalues"), ":"), text)
        } else if (grepl("Optim_4_", name)) {
          text <- sub("The stationary point found by canonical analysis is a ",
                      paste0(i18n_r()$t("The stationary point found by canonical analysis is a"), " "), text)
          text <- sub("maximum\\.", paste0(i18n_r()$t("maximum"), "."), text)
          text <- sub("minimum\\.", paste0(i18n_r()$t("minimum"), "."), text)
          text <- sub("saddle point\\.", paste0(i18n_r()$t("saddle point"), "."), text)
        } else if (grepl("Optim_5_", name)) {
          text <- sub("Model prediction at the stationary point yields the response value:",
                      paste0(i18n_r()$t("Model prediction at the stationary point yields the response value"), ":"), text)
        } else if (grepl("Optim_2_", name)) {
          text <- sub("^NOTE:", paste0(i18n_r()$t("NOTE"), ":"), text)
          text <- sub("optimum decoded CA values are out of range!",
                      i18n_r()$t("optimum decoded CA values are out of range!"), text)
        } else if (grepl("Optim_3_", name)) {
          text <- sub("^NOTE:", paste0(i18n_r()$t("NOTE"), ":"), text)
          text <- sub("CA eigen values are positive!",
                      i18n_r()$t("CA eigen values are positive!"), text)
        } else if (grepl("TradOpt_", name)) {
          text <- sub("^The (initial|final) model yielded the following",
                      i18n_r()$t("The model yielded the following"), text)
          text <- sub("MINIMUM response value:", paste0(i18n_r()$t("MINIMUM"), " ", i18n_r()$t("response value"), ":"), text)
          text <- sub("MAXIMUM response value:", paste0(i18n_r()$t("MAXIMUM"), " ", i18n_r()$t("response value"), ":"), text)
          text <- sub("obtained via the following parameter values \\(uncoded values in brackets if available\\)",
                      paste0(i18n_r()$t("obtained via the following parameter values"),
                             " (", i18n_r()$t("uncoded values in brackets if available"), ")"), text)
        }

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
        # Translate dynamic source-code statements
        if (grepl("Optim_1_", name)) {
          text <- sub("^The (initial|final) model yielded the following stationary point",
                      i18n_r()$t("The model yielded the following stationary point"), text)
          text <- sub("\\(uncoded values in brackets if available\\)",
                      paste0("(", i18n_r()$t("uncoded values in brackets if available"), ")"), text)
          text <- sub(", with eigenvalues:", paste0(", ", i18n_r()$t("with eigenvalues"), ":"), text)
        } else if (grepl("Optim_4_", name)) {
          text <- sub("The stationary point found by canonical analysis is a ",
                      paste0(i18n_r()$t("The stationary point found by canonical analysis is a"), " "), text)
          text <- sub("maximum\\.", paste0(i18n_r()$t("maximum"), "."), text)
          text <- sub("minimum\\.", paste0(i18n_r()$t("minimum"), "."), text)
          text <- sub("saddle point\\.", paste0(i18n_r()$t("saddle point"), "."), text)
        } else if (grepl("Optim_5_", name)) {
          text <- sub("Model prediction at the stationary point yields the response value:",
                      paste0(i18n_r()$t("Model prediction at the stationary point yields the response value"), ":"), text)
        } else if (grepl("Optim_2_", name)) {
          text <- sub("^NOTE:", paste0(i18n_r()$t("NOTE"), ":"), text)
          text <- sub("optimum decoded CA values are out of range!",
                      i18n_r()$t("optimum decoded CA values are out of range!"), text)
        } else if (grepl("Optim_3_", name)) {
          text <- sub("^NOTE:", paste0(i18n_r()$t("NOTE"), ":"), text)
          text <- sub("CA eigen values are positive!",
                      i18n_r()$t("CA eigen values are positive!"), text)
        } else if (grepl("TradOpt_", name)) {
          text <- sub("^The (initial|final) model yielded the following",
                      i18n_r()$t("The model yielded the following"), text)
          text <- sub("MINIMUM response value:", paste0(i18n_r()$t("MINIMUM"), " ", i18n_r()$t("response value"), ":"), text)
          text <- sub("MAXIMUM response value:", paste0(i18n_r()$t("MAXIMUM"), " ", i18n_r()$t("response value"), ":"), text)
          text <- sub("obtained via the following parameter values \\(uncoded values in brackets if available\\)",
                      paste0(i18n_r()$t("obtained via the following parameter values"),
                             " (", i18n_r()$t("uncoded values in brackets if available"), ")"), text)
        }

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

  # Optimal conditions table - Shows decoded Canonical Analysis results in Model Summary tab
  output$optimal_conditions <- DT::renderDataTable(
    {
      req(analysis_results$analysis_result)
      result <- analysis_results$analysis_result

      # Try to get canonical analysis decoded values first
      ca <- result$results$final$Model_Metrics$Canonical_Analysis

      if (!is.null(ca)) {
        # Canonical analysis available - show stationary point
        if (!is.null(ca$xs_decoded) && length(ca$xs_decoded) > 0 &&
            !is.null(ca$xs) && length(ca$xs) > 0) {
          # Show both coded and decoded values with factor names
          realnames <- result$models$final$realnames
          coded_names <- names(ca$xs)
          factor_labels <- if (!is.null(realnames) && length(realnames) == length(coded_names)) {
            realnames
          } else {
            coded_names
          }

          optimal_df <- data.frame(
            Factor = coded_names,
            Name = factor_labels,
            Coded = round(as.vector(ca$xs), 4),
            Decoded = round(as.vector(ca$xs_decoded), 4),
            stringsAsFactors = FALSE
          )

          add_prettynames <- c(
            i18n_r()$t("Factor"),
            i18n_r()$t("Factor Name"),
            i18n_r()$t("Coded Value"),
            i18n_r()$t("Decoded Value")
          )

          DT::datatable(
            optimal_df,
            colnames = add_prettynames,
            options = list(
              pageLength = 10,
              dom = "t",
              language = tablang()
            ),
            rownames = FALSE,
            caption = htmltools::tags$caption(
              style = "caption-side: top; text-align: center; color: #666; font-size: 16px; padding: 8px;",
              i18n_r()$t("Stationary point from Canonical Analysis")
            )
          )
        } else if (!is.null(ca$xs) && length(ca$xs) > 0) {
          # Show coded values only if decoded not available
          optimal_df <- data.frame(
            Factor = names(ca$xs),
            `Coded Value` = round(as.vector(ca$xs), 4),
            stringsAsFactors = FALSE,
            check.names = FALSE
          )

          DT::datatable(
            optimal_df,
            colnames = c(i18n_r()$t("Factor"), i18n_r()$t("Coded Value")),
            options = list(
              pageLength = 10,
              dom = "t",
              language = tablang()
            ),
            rownames = FALSE,
            caption = htmltools::tags$caption(
              style = "caption-side: top; text-align: center; color: #666; font-size: 16px; padding: 8px;",
              i18n_r()$t("Stationary point (coded) from Canonical Analysis")
            )
          )
        } else {
          DT::datatable(
            data.frame(Message = i18n$t("Canonical analysis data incomplete")),
            options = list(dom = "t", language = tablang()),
            rownames = FALSE
          )
        }
      } else {
        # No canonical analysis - this is normal for linear/interaction models
        DT::datatable(
          data.frame(Message = i18n$t("Canonical analysis not available (requires quadratic model)")),
          options = list(dom = "t", language = tablang()),
          rownames = FALSE
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

    # ===== TRANSLATION DEFINITIONS =====
    txt <- list(
      initial_model = i18n_r()$t("Initial Model:"),
      final_model = i18n_r()$t("Final Model:"),
      not_available = i18n_r()$t("Canonical analysis not available for this model order.")
    )

    # Helper to translate dynamic canonical analysis statements from source code
    translate_ca_statement <- function(stmt) {
      if (is.null(stmt) || nchar(stmt) == 0) return(stmt)
      s <- stmt
      s <- sub("^The initial model yielded the following stationary point",
               i18n_r()$t("The model yielded the following stationary point"), s)
      s <- sub("^The final model yielded the following stationary point",
               i18n_r()$t("The model yielded the following stationary point"), s)
      s <- sub("\\(uncoded values in brackets if available\\)",
               paste0("(", i18n_r()$t("uncoded values in brackets if available"), ")"), s)
      s <- sub(", with eigenvalues:",
               paste0(", ", i18n_r()$t("with eigenvalues"), ":"), s)
      s
    }

    html_content <- ""
    if ("Optim_1_initial" %in% names(statements)) {
      html_content <- paste0(html_content, "<p><strong>", txt$initial_model, "</strong> ", translate_ca_statement(statements["Optim_1_initial"]), "</p>")
    }
    if ("Optim_1_final" %in% names(statements)) {
      html_content <- paste0(html_content, "<p><strong>", txt$final_model, "</strong> ", translate_ca_statement(statements["Optim_1_final"]), "</p>")
    }
    if (html_content == "") {
      html_content <- paste0("<p class='text-muted'>", txt$not_available, "</p>")
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
      p <- translate_plot_labels(
        p,
        i18n_r(),
        title = "Initial Model Predicted Response vs Actual Response",
        x = "Predicted Response",
        y = "Actual Response"
      )
      plotly_p <- ggplotly(p)
      plotly_p <- add_formula_annotations(plotly_p, p)
      plotly_p
    } else {
      plotly_empty(type = "scatter", mode = "markers") %>%
        layout(title = list(text = i18n$t("No plot available")))
    }
  })


  output$initial_response_diagnostics_plot <- renderUI({
    req(analysis_results$analysis_result)
    current_i18n <- i18n_r()  # Force reactive dependency
    response <- input$response_var
    plot_name <- paste0("Initial Model, ", response)



    grob_object <- analysis_results$analysis_result$plots$initial[[plot_name]]
    if (!is.null(grob_object)) {
      # Translate grob text elements
      translated_grob <- translate_grob_text(grob_object, current_i18n)
      img_src <- render_grob_as_image(translated_grob)
      tags$img(src = img_src, width = "100%")
    } else {
      p(current_i18n$t("No plot available."))
    }
  })

  output$initial_residual_vs_predicted_plot <- renderPlotly({
    req(analysis_results$analysis_result)

    p <- analysis_results$analysis_result$plots$initial$InitialModel_Residual_vs_Predicted
    if (!is.null(p) && inherits(p, "ggplot")) {
      p <- translate_plot_labels(
        p,
        i18n_r(),
        title = "Initial Model: Predicted Response vs Residuals",
        x = "Predicted Response",
        y = "Residuals"
      )
      ggplotly(p)
    } else {
      plotly_empty(type = "scatter", mode = "markers") %>%
        layout(title = list(text = i18n$t("No plot available or not a ggplot object")))
    }
  })

  output$initial_residual_diagnostics_plot <- renderUI({
    req(analysis_results$analysis_result)
    current_i18n <- i18n_r()  # Force reactive dependency
    grob_object <- analysis_results$analysis_result$plots$initial$`Initial Model, Residual`
    if (!is.null(grob_object)) {
      # Translate grob text elements
      translated_grob <- translate_grob_text(grob_object, current_i18n)
      img_src <- render_grob_as_image(translated_grob)
      tags$img(src = img_src, width = "100%")
    } else {
      p(current_i18n$t("No plot available."))
    }
  })

  output$initial_cooks_plot <- renderPlotly({
    req(analysis_results$analysis_result)
    p <- analysis_results$analysis_result$plots$initial$Cooks_Distance_Plot

    if (!is.null(p) && inherits(p, "ggplot")) {
      p <- translate_plot_labels(
        p,
        i18n_r(),
        title = "Cook's Distance Plot - Initial Model",
        x = "Observation",
        y = "Cook's Distance"
      )
      plotly_p <- ggplotly(p)
      plotly_p <- add_layer_labels(plotly_p, p, layers_to_check = c(4, 5))
      plotly_p
    } else {
      plotly_empty(type = "scatter", mode = "markers") %>%
        layout(title = list(text = i18n$t("No plot available or not a ggplot object")))
    }
  })

  output$initial_pareto_plot <- renderPlotly({
    req(analysis_results$analysis_result)
    p <- analysis_results$analysis_result$plots$initial$Pareto_Plot_Initial_Model
    p <- translate_plot_labels(
      p,
      i18n_r(),
      title = "Pareto Plot - Initial Model",
      y = "Absolute effects"
    )
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
      p <- translate_plot_labels(
        p,
        i18n_r(),
        title = "Initial Model: Response vs Coded Factors",
        x = "Factor Level",
        y = "Response"
      )
      plotly_p <- ggplotly(p)
    } else {
      plotly_empty(type = "scatter", mode = "markers") %>%
        layout(title = list(text = i18n$t("No plot available")))
    }
  })

  output$initial_response_vs_uncoded <- renderPlotly({
    req(analysis_results$analysis_result)
    response <- input$response_var
    plot_name <- paste0("InitialModel_", response, "_vs_Uncoded")

    p <- analysis_results$analysis_result$plots$initial[[plot_name]]

    if (!is.null(p) && inherits(p, "ggplot")) {
      p <- translate_plot_labels(
        p,
        i18n_r(),
        title = "Initial Model: Response vs Uncoded Factors",
        x = "Factor Level",
        y = "Response"
      )
      plotly_p <- ggplotly(p)
    } else {
      plotly_empty(type = "scatter", mode = "markers") %>%
        layout(title = list(text = i18n$t("No plot available")))
    }
  })

  output$initial_residual_vs_coded <- renderPlotly({
    req(analysis_results$analysis_result)


    p <- analysis_results$analysis_result$plots$initial$InitialModel_Residual_vs_Coded

    if (!is.null(p) && inherits(p, "ggplot")) {
      p <- translate_plot_labels(
        p,
        i18n_r(),
        title = "Initial Model: Residual vs Coded Factors",
        x = "Factor Level",
        y = "Residual"
      )
      plotly_p <- ggplotly(p)
    } else {
      plotly_empty(type = "scatter", mode = "markers") %>%
        layout(title = list(text = i18n$t("No plot available")))
    }
  })
  output$initial_residual_vs_uncoded <- renderPlotly({
    req(analysis_results$analysis_result)

    p <- analysis_results$analysis_result$plots$initial$InitialModel_Residual_vs_Uncoded

    if (!is.null(p) && inherits(p, "ggplot")) {
      p <- translate_plot_labels(
        p,
        i18n_r(),
        title = "Initial Model: Residual vs Uncoded Factors",
        x = "Factor Level",
        y = "Residual"
      )
      plotly_p <- ggplotly(p)
    } else {
      plotly_empty(type = "scatter", mode = "markers") %>%
        layout(title = list(text = i18n$t("No plot available")))
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
      p <- translate_plot_labels(
        p,
        i18n_r(),
        title = "Final Model Predicted Response vs Actual Response",
        x = "Predicted Response",
        y = "Actual Response"
      )
      plotly_p <- ggplotly(p)
      plotly_p <- add_formula_annotations(plotly_p, p)
      plotly_p
    } else {
      plotly_empty(type = "scatter", mode = "markers") %>%
        layout(title = list(text = i18n$t("No plot available")))
    }
  })


  output$final_response_diagnostics_plot <- renderUI({
    req(analysis_results$analysis_result)
    current_i18n <- i18n_r()  # Force reactive dependency
    response <- input$response_var
    plot_name <- paste0("Final Model, ", response)



    grob_object <- analysis_results$analysis_result$plots$final[[plot_name]]
    if (!is.null(grob_object)) {
      # Translate grob text elements
      translated_grob <- translate_grob_text(grob_object, current_i18n)
      img_src <- render_grob_as_image(translated_grob)
      tags$img(src = img_src, width = "100%")
    } else {
      p(current_i18n$t("No plot available."))
    }
  })

  output$final_residual_vs_predicted_plot <- renderPlotly({
    req(analysis_results$analysis_result)

    p <- analysis_results$analysis_result$plots$final$FinalModel_Residual_vs_Predicted
    if (!is.null(p) && inherits(p, "ggplot")) {
      p <- translate_plot_labels(
        p,
        i18n_r(),
        title = "Final Model: Predicted Response vs Residuals",
        x = "Predicted Response",
        y = "Residuals"
      )
      ggplotly(p)
    } else {
      plotly_empty(type = "scatter", mode = "markers") %>%
        layout(title = list(text = i18n$t("No plot available or not a ggplot object")))
    }
  })

  output$final_residual_diagnostics_plot <- renderUI({
    req(analysis_results$analysis_result)
    current_i18n <- i18n_r()  # Force reactive dependency
    grob_object <- analysis_results$analysis_result$plots$final$`Final Model, Residual`
    if (!is.null(grob_object)) {
      # Translate grob text elements
      translated_grob <- translate_grob_text(grob_object, current_i18n)
      img_src <- render_grob_as_image(translated_grob)
      tags$img(src = img_src, width = "100%")
    } else {
      p(current_i18n$t("No plot available."))
    }
  })

  output$final_cooks_plot <- renderPlotly({
    req(analysis_results$analysis_result)
    p <- analysis_results$analysis_result$plots$final$Cooks_Distance_Plot

    if (!is.null(p) && inherits(p, "ggplot")) {
      p <- translate_plot_labels(
        p,
        i18n_r(),
        title = "Cook's Distance Plot - Final Model",
        x = "Observation",
        y = "Cook's Distance"
      )
      plotly_p <- ggplotly(p)
      plotly_p <- add_layer_labels(plotly_p, p, layers_to_check = c(4, 5))
      plotly_p
    } else {
      plotly_empty(type = "scatter", mode = "markers") %>%
        layout(title = list(text = i18n$t("No plot available or not a ggplot object")))
    }
  })

  output$final_pareto_plot <- renderPlotly({
    req(analysis_results$analysis_result)
    p <- analysis_results$analysis_result$plots$final$Pareto_Plot_Final_Model
    p <- translate_plot_labels(
      p,
      i18n_r(),
      title = "Pareto Plot - Final Model",
      y = "Absolute effects"
    )
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
      p <- translate_plot_labels(
        p,
        i18n_r(),
        title = "Final Model: Response vs Coded Factors",
        x = "Factor Level",
        y = "Response"
      )
      plotly_p <- ggplotly(p)
    } else {
      plotly_empty(type = "scatter", mode = "markers") %>%
        layout(title = list(text = i18n$t("No plot available")))
    }
  })

  output$final_response_vs_uncoded <- renderPlotly({
    req(analysis_results$analysis_result)
    response <- input$response_var
    plot_name <- paste0("FinalModel_", response, "_vs_Uncoded")

    p <- analysis_results$analysis_result$plots$final[[plot_name]]

    if (!is.null(p) && inherits(p, "ggplot")) {
      p <- translate_plot_labels(
        p,
        i18n_r(),
        title = "Final Model: Response vs Uncoded Factors",
        x = "Factor Level",
        y = "Response"
      )
      plotly_p <- ggplotly(p)
    } else {
      plotly_empty(type = "scatter", mode = "markers") %>%
        layout(title = list(text = i18n$t("No plot available")))
    }
  })

  output$final_residual_vs_coded <- renderPlotly({
    req(analysis_results$analysis_result)


    p <- analysis_results$analysis_result$plots$final$FinalModel_Residual_vs_Coded

    if (!is.null(p) && inherits(p, "ggplot")) {
      p <- translate_plot_labels(
        p,
        i18n_r(),
        title = "Final Model: Residual vs Coded Factors",
        x = "Factor Level",
        y = "Residual"
      )
      plotly_p <- ggplotly(p)
    } else {
      plotly_empty(type = "scatter", mode = "markers") %>%
        layout(title = list(text = i18n$t("No plot available")))
    }
  })
  output$final_residual_vs_uncoded <- renderPlotly({
    req(analysis_results$analysis_result)

    p <- analysis_results$analysis_result$plots$final$FinalModel_Residual_vs_Uncoded

    if (!is.null(p) && inherits(p, "ggplot")) {
      p <- translate_plot_labels(
        p,
        i18n_r(),
        title = "Final Model: Residual vs Uncoded Factors",
        x = "Factor Level",
        y = "Residual"
      )
      plotly_p <- ggplotly(p)
    } else {
      plotly_empty(type = "scatter", mode = "markers") %>%
        layout(title = list(text = i18n$t("No plot available")))
    }
  })






  # ============================================================================
  # MODEL DETAILS OUTPUTS - Initial and Final Models
  # ============================================================================

  # Helper: Create model equation output with KaTeX
  create_model_equation <- function(model_type = "final") {
    req(analysis_results$analysis_result)
    result <- analysis_results$analysis_result
    misc <- result$results[[model_type]]$Misc

    render_equation_katex(
      misc$Equation[["raw"]], 
      response_var = analysis_results$response_var,
      element_id = ns(paste0("details_eq_", model_type))
    )
  }

  # Model Equation - Final
  output$model_equation_final <- renderUI({
    create_model_equation("final")
  })

  # Model Equation - Initial
  output$model_equation_initial <- renderUI({
    create_model_equation("initial")
  })

  # Coefficients Table - Final
  output$coefficients_table_final <- DT::renderDataTable(
    {
      req(analysis_results$analysis_result)
      create_coefficients_table(analysis_results, "final")
    },
    server = FALSE
  )

  # Coefficients Table - Initial
  output$coefficients_table_initial <- DT::renderDataTable(
    {
      req(analysis_results$analysis_result)
      create_coefficients_table(analysis_results, "initial")
    },
    server = FALSE
  )

  # Model Data Table - Final
  output$model_data_table_final <- DT::renderDataTable(
    {
      req(analysis_results$analysis_result)
      create_model_data_table(analysis_results, "final")
    },
    server = FALSE
  )

  # Model Data Table - Initial
  output$model_data_table_initial <- DT::renderDataTable(
    {
      req(analysis_results$analysis_result)
      create_model_data_table(analysis_results, "initial")
    },
    server = FALSE
  )

  # ============================================================================
  # OLD OUTPUTS - For backwards compatibility
  # ============================================================================

  # Model Equations - OLD (shows both)
  output$model_equations <- renderUI({
    req(analysis_results$analysis_result)
    result <- analysis_results$analysis_result
    init_misc <- result$results$initial$Misc
    fin_misc <- result$results$final$Misc

    tagList(
      tags$h4(i18n_r()$t("Initial Model Equation:")),
      render_equation_katex(
        init_misc$Equation[["raw"]], 
        response_var = analysis_results$response_var,
        element_id = ns("old_init_eq")
      ),
      tags$h4(i18n_r()$t("Final Model Equation:"), style = "margin-top: 20px;"),
      render_equation_katex(
        fin_misc$Equation[["raw"]], 
        response_var = analysis_results$response_var,
        element_id = ns("old_fin_eq")
      )
    )
  })

  # Coefficients table - OLD (initial)
  output$coefficients_table <- DT::renderDataTable(
    {
      req(analysis_results$analysis_result)
      create_coefficients_table(analysis_results, "initial")
    },
    server = FALSE
  )

  # Model data table - OLD (initial)
  output$model_data_table <- DT::renderDataTable(
    {
      req(analysis_results$analysis_result)
      create_model_data_table(analysis_results, "initial")
    },
    server = FALSE
  )

  # Download Default CSV Template
  output$download_default_csv <- downloadHandler(
    filename = function() {
      "doe_analysis_default.csv"
    },
    content = function(file) {
      default_path <- system.file("extdata", "gui-doe-analysis-default.csv", package = "supeRcrit")
      if (file.exists(default_path)) {
        file.copy(default_path, file)
      } else {
        showNotification(i18n$t("Default template file not found."), type = "error")
      }
    },
    contentType = "text/csv"
  )

  # Download All Results
  output$export_all_results <- downloadHandler(
    filename = function() {
      paste0(generate_filename_with_timestamp("supercrit_doe_analysis_results"), ".zip")
    },
    content = function(file) {
      req(analysis_results$analysis_result) # Ensure results are available

      # Show progress
      withProgress(message = i18n$t("Preparing export..."), value = 0, {
        # Create a temporary directory for export
        incProgress(0.1, detail = i18n$t("Creating temporary directory..."))
        temp_dir <- file.path(tempdir(), paste0("doe_analysis_export_", format(Sys.time(), "%d-%m-%Y_%Hhrs-%Mmin-%Ssec")))
        dir.create(temp_dir, recursive = TRUE)

        # Export results using doeopt_export
        tryCatch({
          incProgress(0.3, detail = i18n$t("Exporting analysis results..."))
          doeopt_export(input = analysis_results$analysis_result, expath = temp_dir, silent = TRUE)

          # Get list of files to zip
          incProgress(0.7, detail = i18n$t("Collecting exported files..."))
          files_to_zip <- list.files(temp_dir, full.names = TRUE, recursive = TRUE)

          # Create the zip file
          incProgress(0.9, detail = i18n$t("Creating ZIP archive..."))
          zip::zip(zipfile = file, files = files_to_zip, mode = "cherry-pick")

          incProgress(1, detail = i18n$t("Export completed!"))
        }, error = function(e) {
          showNotification(paste(i18n$t("Error exporting results:"), e$message), type = "error")
        }, finally = {
          # Clean up the temporary directory
          unlink(temp_dir, recursive = TRUE)
        })
      })
    },
    contentType = "application/zip"
  )



  # Reset function
  observeEvent(input$reset, {
    # Clear results first
    analysis_results$input_data <- NULL
    analysis_results$analysis_result <- NULL
    analysis_results$variable_names <- NULL
    analysis_results$selectable_vars <- NULL
    prediction_results$predictions <- NULL
    analysis_results$table_render_trigger <- analysis_results$table_render_trigger + 1

    # Disable save button
    shinyjs::disable("save_analysis")

    # Clear variable selection wrapper borders
    shinyjs::runjs(sprintf(
      "$('#%s').css({'border': '', 'border-radius': '', 'padding': ''});",
      ns("time_var_wrapper")
    ))
    shinyjs::runjs(sprintf(
      "$('#%s').css({'border': '', 'border-radius': '', 'padding': ''});",
      ns("response_var_wrapper")
    ))

    # Reset data source to csv without the saved→csv flip that reloads data
    updateRadioButtons(session, "data_source", selected = "csv")

    # Reset other inputs
    updateSelectInput(session, "mod_order", selected = 2)
    updateNumericInput(session, "p_cutoff", value = 0.1)
    updateSelectInput(session, "trim_method", selected = "both")
    updateSelectInput(session, "which_facs", selected = "coded")

    # Clear cached uncoded factor selections via JS
    lapply(LETTERS, function(cf) {
      shinyjs::runjs(sprintf("Shiny.setInputValue('%s', '', {priority: 'event'})", ns(paste0("uc_for_", cf))))
    })

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
    multi_input_data = NULL,
    table_render_trigger = 0  # Incremented only when table should re-render
  )

  # Reactive output to check if predictions are available
  output$has_predictions <- reactive({
    !is.null(prediction_results$predictions)
  })
  outputOptions(output, "has_predictions", suspendWhenHidden = FALSE)

  # Observe CSV file upload for predictions
  observeEvent(input$pred_import_csv, {
    req(input$pred_import_csv)

    tryCatch(
      {
        # Read the CSV file
        df <- read.csv(input$pred_import_csv$datapath, stringsAsFactors = FALSE)
        prediction_results$multi_input_data <- df # Directly load to multi_input_data
        prediction_results$table_render_trigger <- prediction_results$table_render_trigger + 1  # Trigger table re-render
        showNotification(i18n$t("CSV file loaded to input table."), type = "message")
      },
      error = function(e) {
        showNotification(paste(i18n$t("Error loading CSV for prediction:"), e$message), type = "error")
        prediction_results$multi_input_data <- NULL
      }
    )
  })

  # Check if uncoded factors are available
  pred_has_uncoded <- reactive({
    req(analysis_results$analysis_result)
    result <- analysis_results$analysis_result
    rn <- result$models$final$realnames
    !is.null(rn) && !any(is.na(rn))
  })

  # Disable/enable Uncoded radio option based on availability
  observe({
    has_uc <- pred_has_uncoded()
    if (!has_uc) {
      updateRadioButtons(session, "pred_coded", selected = "TRUE")
      shinyjs::disable("pred_coded")
    } else {
      shinyjs::enable("pred_coded")
    }
  })

  # Track the last pred_coded mode to detect switches
  pred_last_coded <- reactiveVal(NULL)
  pred_inputs_generation <- reactiveVal(0)
  pred_force_rebuild <- reactiveVal(0)

  # Dynamic factor inputs for single value prediction
  output$pred_factor_inputs <- renderUI({
    req(analysis_results$analysis_result)
    cur_coded <- input$pred_coded  # Re-render when coded/uncoded changes
    pred_force_rebuild()  # Re-render when reset is triggered

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

    # Determine which factors to show based on coded/uncoded selection
    is_coded <- as.logical(cur_coded)
    fac_names <- if (is_coded) coded_facs else uncoded_facs
    fac_labels <- if (is_coded) coded_facs else paste0(uncoded_facs, " (", coded_facs, ")")

    # Detect mode switch vs reset
    prev_mode <- isolate(pred_last_coded())
    mode_switched <- !identical(prev_mode, cur_coded)
    was_reset <- is.null(prev_mode)
    pred_last_coded(cur_coded)

    # Get ranges from original data
    orig_df <- result$models$final$orig_df

    # Create numeric inputs for each factor with range badge
    input_list <- lapply(seq_along(fac_names), function(i) {
      fac <- fac_names[i]
      coded_fac <- coded_facs[i]

      # Get both ranges for conversion
      coded_range <- range(orig_df[, coded_fac], na.rm = TRUE)
      uncoded_range <- if (!identical(uncoded_facs, coded_facs)) {
        range(orig_df[, uncoded_facs[i]], na.rm = TRUE)
      } else coded_range
      
      # Current range for this mode
      fac_range <- if (is_coded) coded_range else uncoded_range
      default_val <- round(mean(fac_range), 2)
      
      # Get current input value
      cur_val <- input[[paste0("pred_fac_", i)]]
      
      if (was_reset) {
        # Reset: use default midpoint
        val <- default_val
      } else if (mode_switched && !is.null(cur_val) && !identical(coded_range, uncoded_range)) {
        # Convert value from old mode to new mode via linear interpolation
        if (is_coded) {
          # Was uncoded, convert to coded
          old_range <- uncoded_range; new_range <- coded_range
        } else {
          # Was coded, convert to uncoded
          old_range <- coded_range; new_range <- uncoded_range
        }
        old_span <- old_range[2] - old_range[1]
        if (old_span != 0) {
          val <- round(((cur_val - old_range[1]) * (new_range[2] - new_range[1]) / old_span) + new_range[1], 2)
        } else {
          val <- default_val
        }
      } else if (!is.null(cur_val)) {
        val <- cur_val
      } else {
        val <- default_val
      }

      # Range badge color based on actual value
      badge_color <- "#6c757d"
      if (!is.null(val) && !is.na(val) && (val < fac_range[1] || val > fac_range[2])) {
        badge_color <- "#dc3545"
      }
      badge_text <- paste0(round(fac_range[1], 2), "\u2013", round(fac_range[2], 2))

      tags$div(
        tags$label(
          fac_labels[i],
          class = "control-label",
          style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
          tags$span(
            badge_text,
            style = paste0("font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: ",
              badge_color, "; color: white; margin-left: auto; font-weight: normal;"),
            title = paste0(i18n_r()$t("Modeled range:"), " ", round(fac_range[1], 2), " \u2013 ", round(fac_range[2], 2))
          )
        ),
        numericInput(
          ns(paste0("pred_fac_", i)),
          label = NULL,
          value = val,
          step = round((fac_range[2] - fac_range[1]) / 100, 4)
        )
      )
    })

    pred_inputs_generation(isolate(pred_inputs_generation()) + 1)
    do.call(tagList, input_list)
  })

  # Range warning for single-value prediction inputs
  output$pred_range_warning <- renderUI({
    req(analysis_results$analysis_result)
    req(input$pred_input_type == "single")
    
    # Depend on inputs generation — only fires AFTER pred_factor_inputs completes
    gen <- pred_inputs_generation()
    req(gen > 0)
    
    # Use isolate on pred_coded — we don't want to react to mode change directly
    cur_mode <- isolate(input$pred_coded)
    req(cur_mode)
    
    result <- analysis_results$analysis_result
    orig_df <- result$models$final$orig_df

    if (!is.null(result$results$final$Model_Metrics$Canonical_Analysis)) {
      coded_facs <- names(result$results$final$Model_Metrics$Canonical_Analysis$xs)
    } else {
      coded_facs <- result$models$final$codenames[nchar(result$models$final$codenames[, "data"]) == 1, "data"]
    }
    uncoded_facs <- result$models$final$realnames %||% coded_facs

    is_coded <- as.logical(cur_mode)
    fac_names <- if (is_coded) coded_facs else uncoded_facs

    # Wait for all factor inputs to exist
    for (i in seq_along(fac_names)) {
      val <- input[[paste0("pred_fac_", i)]]
      if (is.null(val)) return(NULL)
    }

    out_of_range <- FALSE
    for (i in seq_along(fac_names)) {
      val <- input[[paste0("pred_fac_", i)]]
      if (is.na(val)) next
      fac_col <- if (is_coded) coded_facs[i] else uncoded_facs[i]
      fac_range <- range(orig_df[, fac_col], na.rm = TRUE)
      if (val < fac_range[1] || val > fac_range[2]) {
        out_of_range <- TRUE
        break
      }
    }

    if (out_of_range) {
      div(
        style = "background-color: #fff3cd; border: 1px solid #ffc107; border-radius: 4px; padding: 8px; margin-top: 5px; margin-bottom: 10px;",
        icon("exclamation-triangle", style = "color: #856404; margin-right: 6px;"),
        tags$span(style = "color: #856404;",
          i18n$t("One or more factor values are outside the modeled range. Predictions may be unreliable (extrapolation).")
        )
      )
    }
  })

  # Convert multi-input data when coded/uncoded mode changes
  observeEvent(input$pred_coded, {
    req(analysis_results$analysis_result)
    # Only convert if we're in multiple mode and have data
    if (is.null(input$pred_input_type) || input$pred_input_type != "multiple") return()
    result <- analysis_results$analysis_result
    orig_df <- result$models$final$orig_df
    
    if (!is.null(result$results$final$Model_Metrics$Canonical_Analysis)) {
      coded_facs <- names(result$results$final$Model_Metrics$Canonical_Analysis$xs)
    } else {
      coded_facs <- result$models$final$codenames[nchar(result$models$final$codenames[, "data"]) == 1, "data"]
    }
    uncoded_facs <- result$models$final$realnames %||% coded_facs
    
    is_coded <- as.logical(input$pred_coded)
    new_fac_names <- if (is_coded) coded_facs else uncoded_facs
    
    old_data <- prediction_results$multi_input_data
    
    if (!is.null(old_data) && ncol(old_data) == length(coded_facs) && !identical(coded_facs, uncoded_facs)) {
      # Convert each column
      new_data <- old_data
      for (i in seq_along(coded_facs)) {
        coded_range <- range(orig_df[, coded_facs[i]], na.rm = TRUE)
        uncoded_range <- range(orig_df[, uncoded_facs[i]], na.rm = TRUE)
        old_span <- if (is_coded) (uncoded_range[2] - uncoded_range[1]) else (coded_range[2] - coded_range[1])
        
        if (old_span != 0) {
          if (is_coded) {
            # Was uncoded, convert to coded
            new_data[, i] <- round(((old_data[, i] - uncoded_range[1]) * (coded_range[2] - coded_range[1]) / old_span) + coded_range[1], 2)
          } else {
            # Was coded, convert to uncoded
            new_data[, i] <- round(((old_data[, i] - coded_range[1]) * (uncoded_range[2] - uncoded_range[1]) / old_span) + uncoded_range[1], 2)
          }
        }
      }
      colnames(new_data) <- new_fac_names
      prediction_results$multi_input_data <- new_data
    } else {
      prediction_results$multi_input_data <- NULL
    }
    
    prediction_results$table_render_trigger <- prediction_results$table_render_trigger + 1
  }, ignoreInit = TRUE)

  # Initialize multi-input table
  # Uses table_render_trigger to only re-render when data is loaded, not on every edit
  output$pred_multi_input <- renderRHandsontable({
    req(analysis_results$analysis_result)
    # Depend on trigger for re-rendering
    prediction_results$table_render_trigger

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
    is_coded <- as.logical(input$pred_coded)
    fac_names <- if (is_coded) coded_facs else uncoded_facs

    # Create or convert data frame based on current mode
    multi_data <- isolate(prediction_results$multi_input_data)
    orig_df <- result$models$final$orig_df
    
    needs_init <- is.null(multi_data) || !identical(sort(colnames(multi_data)), sort(fac_names))
    
    if (needs_init && !is.null(multi_data) && ncol(multi_data) == length(fac_names) && !identical(coded_facs, uncoded_facs)) {
      # Existing data with wrong column names — convert values
      new_data <- multi_data
      for (i in seq_along(coded_facs)) {
        coded_range <- range(orig_df[, coded_facs[i]], na.rm = TRUE)
        uncoded_range <- range(orig_df[, uncoded_facs[i]], na.rm = TRUE)
        if (is_coded) {
          old_span <- uncoded_range[2] - uncoded_range[1]
          if (old_span != 0) new_data[, i] <- round(((multi_data[, i] - uncoded_range[1]) * (coded_range[2] - coded_range[1]) / old_span) + coded_range[1], 2)
        } else {
          old_span <- coded_range[2] - coded_range[1]
          if (old_span != 0) new_data[, i] <- round(((multi_data[, i] - coded_range[1]) * (uncoded_range[2] - uncoded_range[1]) / old_span) + uncoded_range[1], 2)
        }
      }
      colnames(new_data) <- fac_names
      prediction_results$multi_input_data <- new_data
      multi_data <- new_data
    } else if (is.null(multi_data)) {
      # Fresh init — use coded 0 converted to uncoded if needed
      init_vals <- lapply(seq_along(fac_names), function(i) {
        if (is_coded) {
          rep(0, 3)
        } else {
          coded_range <- range(orig_df[, coded_facs[i]], na.rm = TRUE)
          uncoded_range <- range(orig_df[, uncoded_facs[i]], na.rm = TRUE)
          coded_span <- coded_range[2] - coded_range[1]
          if (coded_span != 0) {
            rep(round(((0 - coded_range[1]) * (uncoded_range[2] - uncoded_range[1]) / coded_span) + uncoded_range[1], 2), 3)
          } else {
            rep(round(mean(uncoded_range), 2), 3)
          }
        }
      })
      init_df <- as.data.frame(init_vals)
      colnames(init_df) <- fac_names
      prediction_results$multi_input_data <- init_df
      multi_data <- init_df
    }

    rhandsontable(multi_data, contextMenu = TRUE, stretchH = "all") %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
      hot_cols(columnSorting = TRUE)
  })

  # Update multi-input data when edited
  observeEvent(input$pred_multi_input, {
    if (!is.null(input$pred_multi_input)) {
      prediction_results$multi_input_data <- hot_to_r(input$pred_multi_input)
    }
  })

  # Range badges for multi-value prediction inputs (one per factor, turns red individually)
  output$pred_multi_range_warning <- renderUI({
    req(analysis_results$analysis_result)
    
    result <- analysis_results$analysis_result
    orig_df <- result$models$final$orig_df
    
    if (!is.null(result$results$final$Model_Metrics$Canonical_Analysis)) {
      coded_facs <- names(result$results$final$Model_Metrics$Canonical_Analysis$xs)
    } else {
      coded_facs <- result$models$final$codenames[nchar(result$models$final$codenames[, "data"]) == 1, "data"]
    }
    uncoded_facs <- result$models$final$realnames %||% coded_facs
    
    is_coded <- as.logical(input$pred_coded)
    fac_names <- if (is_coded) coded_facs else uncoded_facs
    data <- prediction_results$multi_input_data
    
    badges <- lapply(seq_along(fac_names), function(i) {
      fac_col <- if (is_coded) coded_facs[i] else uncoded_facs[i]
      fac_range <- range(orig_df[, fac_col], na.rm = TRUE)
      
      # Check if any values in this column are out of range
      badge_color <- "#6c757d"
      if (!is.null(data) && ncol(data) >= i) {
        vals <- data[, i]
        vals <- vals[!is.na(vals)]
        if (length(vals) > 0 && any(vals < fac_range[1] | vals > fac_range[2])) {
          badge_color <- "#dc3545"
        }
      }
      
      tags$span(
        paste0(fac_names[i], ": ", round(fac_range[1], 2), "\u2013", round(fac_range[2], 2)),
        style = paste0(
          "font-size: 11px; padding: 2px 7px; border-radius: 3px; background-color: ",
          badge_color, "; color: white; margin-right: 5px; display: inline-block; margin-bottom: 3px;"
        ),
        title = paste0(i18n_r()$t("Modeled range:"), " ", round(fac_range[1], 2), " \u2013 ", round(fac_range[2], 2))
      )
    })
    
    # Check if any factor has out-of-range values
    any_out_of_range <- any(sapply(badges, function(b) grepl("#dc3545", as.character(b))))
    
    warning_banner <- if (any_out_of_range) {
      div(
        style = "background-color: #fff3cd; border: 1px solid #ffc107; border-radius: 4px; padding: 8px; margin-top: 5px; margin-bottom: 10px;",
        icon("exclamation-triangle", style = "color: #856404; margin-right: 6px;"),
        tags$span(style = "color: #856404;",
          i18n$t("One or more factor values are outside the modeled range. Predictions may be unreliable (extrapolation).")
        )
      )
    }
    
    tagList(
      div(
        style = "margin-bottom: 8px; margin-top: 5px;",
        tags$span(tags$strong(i18n_r()$t("Ranges: ")), style = "margin-right: 4px;"),
        do.call(tagList, badges)
      ),
      warning_banner
    )
  })

  # Reset predictions and factor values
  observeEvent(input$pred_reset_btn, {
    # Clear predictions
    prediction_results$predictions <- NULL
    
    # Reset Input Type and Value Type
    updateRadioButtons(session, "pred_input_type", selected = "single")
    updateRadioButtons(session, "pred_coded", selected = "TRUE")
    
    # Clear multi-input data so it reinitializes with zeros
    prediction_results$multi_input_data <- NULL
    prediction_results$table_render_trigger <- prediction_results$table_render_trigger + 1
    
    # Force single-value factor inputs to rebuild with default values (coded 0)
    pred_last_coded(NULL)
    pred_force_rebuild(pred_force_rebuild() + 1)
    
    showNotification(i18n$t("Predictions reset."), type = "message")
  })

  # Enable/disable prediction Reset button
  observe({
    is_default <- TRUE
    if (!is.null(input$pred_input_type) && input$pred_input_type != "single") is_default <- FALSE
    if (!is.null(input$pred_coded) && input$pred_coded != "TRUE") is_default <- FALSE
    if (!is.null(prediction_results$predictions)) is_default <- FALSE
    
    # Check if any single-value factor inputs differ from default (0 in coded mode)
    if (is_default && !is.null(analysis_results$analysis_result)) {
      result <- analysis_results$analysis_result
      if (!is.null(result$results$final$Model_Metrics$Canonical_Analysis)) {
        n_facs <- length(result$results$final$Model_Metrics$Canonical_Analysis$xs)
      } else {
        n_facs <- sum(nchar(result$models$final$codenames[, "data"]) == 1)
      }
      for (i in seq_len(n_facs)) {
        val <- input[[paste0("pred_fac_", i)]]
        if (!is.null(val) && !is.na(val) && val != 0) {
          is_default <- FALSE
          break
        }
      }
    }
    
    # Check if multi-input data has been modified
    if (is_default && !is.null(prediction_results$multi_input_data)) {
      if (any(prediction_results$multi_input_data != 0, na.rm = TRUE)) {
        is_default <- FALSE
      }
    }
    
    shinyjs::toggleState("pred_reset_btn", condition = !is_default)
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

        # Validate that uncoded factors are fully specified (predict_doe needs them for decoding)
        if (!is.null(result$models$final$realnames) && any(is.na(result$models$final$realnames))) {
          showNotification(
            i18n$t("All uncoded factor associations must be specified before using predictions. Please assign uncoded factors in the Data tab."),
            type = "error", duration = 8
          )
          return()
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
          is_coded <- as.logical(input$pred_coded)
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

        # Show download button

        showNotification(i18n$t("Predictions completed successfully!"), type = "message")
      },
      error = function(e) {
        showNotification(paste(i18n$t("Prediction error"), e$message), type = "error")
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
            dom = "t",
            language = tablang()
          ),
          rownames = FALSE
        ) %>%
          formatRound("response", 3)
      } else {
        DT::datatable(
          data.frame(Message = i18n$t("No initial model predictions available")),
          options = list(dom = "t", language = tablang()),
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
            dom = "t",
            language = tablang()
          ),
          rownames = FALSE
        ) %>%
          formatRound("response", 3)
      } else {
        DT::datatable(
          data.frame(Message = i18n$t("No final model predictions available")),
          options = list(dom = "t", language = tablang()),
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

  # suspendWhenHidden for renderUI outputs (auto-added)
  outputOptions(output, "accordion_toggle_btn", suspendWhenHidden = FALSE)
  outputOptions(output, "ca_warnings", suspendWhenHidden = FALSE)
  outputOptions(output, "ca_warnings_final", suspendWhenHidden = FALSE)
  outputOptions(output, "ca_warnings_initial", suspendWhenHidden = FALSE)
  outputOptions(output, "canonical_analysis_summary", suspendWhenHidden = FALSE)
  outputOptions(output, "coded_factors_info_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "column_validation_message", suspendWhenHidden = FALSE)
  outputOptions(output, "data_preview_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "doe_analysis_HELP", suspendWhenHidden = FALSE)
  outputOptions(output, "final_model_summary", suspendWhenHidden = FALSE)
  outputOptions(output, "final_residual_diagnostics_plot", suspendWhenHidden = FALSE)
  outputOptions(output, "final_response_diagnostics_plot", suspendWhenHidden = FALSE)
  outputOptions(output, "formatted_full_report", suspendWhenHidden = FALSE)
  outputOptions(output, "import_file_csv_div", suspendWhenHidden = FALSE)
  outputOptions(output, "initial_model_summary", suspendWhenHidden = FALSE)
  outputOptions(output, "initial_residual_diagnostics_plot", suspendWhenHidden = FALSE)
  outputOptions(output, "initial_response_diagnostics_plot", suspendWhenHidden = FALSE)
  outputOptions(output, "model_equation_final", suspendWhenHidden = FALSE)
  outputOptions(output, "model_equation_initial", suspendWhenHidden = FALSE)
  outputOptions(output, "model_equations", suspendWhenHidden = FALSE)
  outputOptions(output, "optimization_methods_summary", suspendWhenHidden = FALSE)
  outputOptions(output, "optimization_methods_summary_final", suspendWhenHidden = FALSE)
  outputOptions(output, "optimization_methods_summary_initial", suspendWhenHidden = FALSE)
  outputOptions(output, "optimization_type", suspendWhenHidden = FALSE)
  outputOptions(output, "optimization_type_final", suspendWhenHidden = FALSE)
  outputOptions(output, "optimization_type_initial", suspendWhenHidden = FALSE)
  outputOptions(output, "pred_factor_inputs", suspendWhenHidden = FALSE)
  outputOptions(output, "pred_import_csv_div", suspendWhenHidden = FALSE)
  outputOptions(output, "pred_multi_range_warning", suspendWhenHidden = FALSE)
  outputOptions(output, "pred_range_warning", suspendWhenHidden = FALSE)
  outputOptions(output, "predicted_response", suspendWhenHidden = FALSE)
  outputOptions(output, "predicted_response_final", suspendWhenHidden = FALSE)
  outputOptions(output, "predicted_response_initial", suspendWhenHidden = FALSE)
  outputOptions(output, "trimming_info", suspendWhenHidden = FALSE)

}
