# DOE Design Server Module
doe_design_server <- function(input, output, session, defaults, i18n) {
  # Load required libraries
  library(dplyr)
  library(DT)
  library(shinyWidgets)

  # Helper for creating namespaced ids inside this module
  ns <- session$ns

  # Null coalescing operator
  `%||%` <- function(x, y) if (is.null(x)) y else x

  # Helper function to generate timestamped filenames
  generate_filename_with_timestamp <- function(base_name) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    paste0(base_name, "__", timestamp)
  }

  # Function to get design requirements and validation
  get_design_requirements <- function(design_type, n_factors, design_specific = NULL) {
    switch(design_type,
      "bbd" = {
        if (n_factors < 3 || n_factors > 4) {
          list(
            valid = FALSE,
            error = "Box-Behnken Design requires 3-4 factors only!"
          )
        } else {
          list(
            valid = TRUE,
            info = "BBD designs include 3 center points by default. Additional center points can be added."
          )
        }
      },
      "ccd" = {
        if (n_factors < 2 || n_factors > 4) {
          list(
            valid = FALSE,
            error = "Central Composite Design requires 2-4 factors only!"
          )
        } else {
          recommended_cpts <- if (n_factors == 2) 4 else 6
          star_points <- 2 * n_factors
          list(
            valid = TRUE,
            info = sprintf("CCD includes star points automatically. Recommended center points: %d", recommended_cpts)
          )
        }
      },
      "ffd" = {
        # Get the levels from design_specific if available
        levels <- if (!is.null(design_specific$levels)) design_specific$levels else 2

        # Validate levels
        if (levels < 2 || levels > 3) {
          list(
            valid = FALSE,
            error = "Only 2- and 3-level FFDs are available at present!"
          )
        } else if (levels == 3) {
          # 3-level FFD: only 2-3 factors allowed
          if (n_factors < 2 || n_factors > 3) {
            list(
              valid = FALSE,
              error = "Only FFDs for 2 to 3 factors are available for 3-level FFD!"
            )
          } else {
            list(
              valid = TRUE,
              info = "Note that 3-level FFD designs must contain 3 center points by default! Only ADDITIONAL center points are provided in 'cpts'."
            )
          }
        } else {
          # 2-level FFD: 2-5 factors allowed
          if (n_factors < 2 || n_factors > 5) {
            list(
              valid = FALSE,
              error = "Only FFDs for 2 to 5 factors are available for 2-level FFD!"
            )
          } else {
            list(
              valid = TRUE,
              info = "2-level FFD designs do not include default center points."
            )
          }
        }
      },
      "frfd" = {
        if (n_factors < 2 || n_factors > 5) {
          list(
            valid = FALSE,
            error = "Fractional Factorial Design requires 2-5 factors!"
          )
        } else {
          p_required <- if (n_factors %in% 2:4) 1 else "1-2"
          list(
            valid = TRUE,
            info = sprintf("Fraction (p) must be %s for %d factors.", p_required, n_factors)
          )
        }
      },
      "tm" = {
        if (n_factors < 3 || n_factors > 5) {
          list(
            valid = FALSE,
            error = "Taguchi Method requires 3-5 factors!"
          )
        } else if (design_specific$levels < 2 | design_specific$levels > 4) {
          list(
            valid = FALSE,
            error = "For Taguchi designs, 2-4 factor 'levels' are currently supported!"
          )
        } else {
          list(
            valid = TRUE,
            info = "Taguchi designs do not support additional center points."
          )
        }
      },
      list(valid = TRUE, info = NULL, warning = NULL, error = NULL)
    )
  }

  # Reactive values for storing results
  design_results <- reactiveValues(
    full_design_result = NULL, # Stores the complete output from doe_* functions
    design_data = NULL, # Stores the data.frame for display
    design_description = NULL,      # YENİ
    design_matrix_name = NULL,       # YENİ
    confounding_pattern = NULL,      # YENİ (sadece frfd için)
    design_type = NULL,
    factors = NULL,
    factor_names = NULL,
    factor_limits = NULL
  )

  # -- Settings modal integration ---------------------------------------------
  observeEvent(input$open_settings, {
    showModal(doe_design_settings_modal_ui(ns("settings_modal"), i18n))
  })

  # Center points input (conditional for Taguchi Method)
  output$center_points_input <- renderUI({
    req(input$design_type)

    # Hide center points input for Taguchi Method
    if (input$design_type == "tm") {
      return(NULL)
    }

    numericInput(ns("cpts"), i18n$t("Center Points"), value = 0, min = 0)
  })

  # Design requirements and validation
  output$design_requirements <- renderUI({
    req(input$design_type, input$factors)

    design_type <- input$design_type
    n_factors <- input$factors
    design_specific <- get_design_specific_params(input)

    requirements <- get_design_requirements(design_type, n_factors, design_specific)

    if (!is.null(requirements$warning)) {
      div(
        class = "alert alert-warning",
        icon("exclamation-triangle"),
        strong(i18n$t("Requirements:")),
        br(),
        requirements$warning
      )
    } else if (!is.null(requirements$info)) {
      div(
        class = "alert alert-info",
        icon("info-circle"),
        strong(i18n$t("Information:")),
        br(),
        requirements$info
      )
    }
  })

  # Dynamic UI for factor inputs
  output$factor_inputs <- renderUI({
    req(input$factors, input$design_type)

    n_factors <- input$factors
    design_type <- input$design_type
    design_specific <- get_design_specific_params(input)

    # Validate factor count for design type
    requirements <- get_design_requirements(design_type, n_factors, design_specific)
    if (!requirements$valid) {
      return(div(
        class = "alert alert-danger",
        icon("exclamation-triangle"),
        strong(i18n$t("Invalid Configuration:")),
        br(),
        requirements$error
      ))
    }

    factor_ui <- lapply(1:n_factors, function(i) {
      fluidRow(
        column(4, textInput(ns(paste0("factor_name_", i)),
          paste0(i18n$t("Factor"), " ", i, " ", i18n$t("Name")),
          value = paste0("Factor", i)
        )),
        column(4, numericInput(ns(paste0("factor_min_", i)),
          i18n$t("Min"),
          value = if (i == 1) 100 else if (i == 2) 35 else if (i == 3) 2 else 0
        )),
        column(4, numericInput(ns(paste0("factor_max_", i)),
          i18n$t("Max"),
          value = if (i == 1) 300 else if (i == 2) 65 else if (i == 3) 4 else 10
        ))
      )
    })

    do.call(tagList, factor_ui)
  })

  # Dynamic UI for design-specific parameters
  output$design_specific_params <- renderUI({
    req(input$design_type, input$factors)

    n_factors <- input$factors

    switch(input$design_type,
      "ccd" = {
        # CCD automatically sets levels based on design type
        ccd_type <- input$ccd_design
        if (!is.null(ccd_type)) {
          auto_levels <- if (ccd_type == "CCC") 5 else 3
          ccd_description <- if (ccd_type == "CCC") {
            "Star points extend beyond factor range (±1.414)"
          } else {
            "Star points are at factor faces (±1.0)"
          }

          fluidRow(
            column(6, selectInput(ns("ccd_design"), i18n$t("CCD Type:"),
              choices = c("Circumscribed (CCC)" = "CCC", "Face-Centered (CCF)" = "CCF"),
              selected = ccd_type
            )),
            column(6, div(
              style = "margin-top: 25px;",
              strong(i18n$t("Levels:")), paste(auto_levels, "(auto-set)"),
              br(),
              em(style = "font-size: 0.9em; color: #666;", ccd_description)
            ))
          )
        } else {
          fluidRow(
            column(6, selectInput(ns("ccd_design"), i18n$t("CCD Type:"),
              choices = c("Circumscribed (CCC)" = "CCC", "Face-Centered (CCF)" = "CCF"),
              selected = "CCC"
            )),
            column(6, div(
              style = "margin-top: 25px;",
              strong(i18n$t("Levels:")), "5 (auto-set)",
              br(),
              em(style = "font-size: 0.9em; color: #666;", "Star points extend beyond factor range (±1.414)")
            ))
          )
        }
      },
      "ffd" = fluidRow(
        column(6, numericInput(ns("ffd_levels"), i18n$t("Levels"), value = 3, min = 2, max = 3)),
        column(6, checkboxInput(ns("ffd_center_default"), i18n$t("Use Default Center Points"), value = TRUE))
      ),
      "frfd" = {
        # Validate p value based on factors
        max_p <- if (n_factors == 5) 2 else 1
        current_p <- input$frfd_p
        if (!is.null(current_p) && current_p > max_p) {
          current_p <- max_p
        }

        fluidRow(
          column(6, numericInput(ns("frfd_p"), i18n$t("Fraction (p)"),
            value = current_p %||% 1, min = 1, max = max_p
          )),
          column(6, selectInput(ns("frfd_aliasing"), i18n$t("Aliasing:"),
            choices = c("Default" = "default", "Custom" = "custom"),
            selected = "default"
          ))
        )
      },
      "tm" = fluidRow(
        column(6, numericInput(ns("tm_levels"), i18n$t("Levels"), value = 3, min = 2, max = 4))
      ),
      # BBD has no additional parameters
      fluidRow()
    )
  })

  # Calculate design
  observeEvent(input$calculate, {
    tryCatch(
      {
        # Show progress
        withProgress(message = i18n$t("Generating design..."), value = 0, {
          # Prepare parameters
          incProgress(0.2, detail = i18n$t("Preparing parameters..."))
          params <- prepare_design_parameters(input)

          # Generate design based on type
          incProgress(0.6, detail = i18n$t("Generating design matrix..."))
          design_result_full <- generate_design(input$design_type, params)

          # Store results
          design_results$full_design_result <- design_result_full$full_result # Store the complete result
          design_results$design_data <- design_result_full$data # Store the data.frame for display
          design_results$design_description <- design_result_full$full_result$description # Store description
          design_results$design_matrix_name <- names(design_result_full$full_result$doe)[1] # Store matrix name
          design_results$confounding_pattern <- if (input$design_type == "frfd" && !is.null(design_result_full$full_result$doe[["CONFOUNDING PATTERN"]])) {
            design_result_full$full_result$doe[["CONFOUNDING PATTERN"]]
          } else {
            NULL
          }
          design_results$design_type <- input$design_type
          design_results$factors <- input$factors
          design_results$factor_names <- params$fnames
          design_results$factor_limits <- params$flims

          incProgress(1, detail = i18n$t("Completed!"))
        })

        showNotification(i18n$t("Design generated successfully!"), type = "message")
      },
      error = function(e) {
        showNotification(paste(i18n$t("Error:"), e$message), type = "error")
      }
    )
  })

  # Prepare design parameters
  prepare_design_parameters <- function(input) {
    n_factors <- input$factors

    # Get factor names
    fnames <- sapply(1:n_factors, function(i) {
      name <- input[[paste0("factor_name_", i)]]
      if (is.null(name) || name == "") paste0("Factor", i) else name
    })

    # Get factor limits
    flims <- lapply(1:n_factors, function(i) {
      min_val <- input[[paste0("factor_min_", i)]]
      max_val <- input[[paste0("factor_max_", i)]]
      c(min_val, max_val)
    })

    # Handle center points (NULL for Taguchi Method)
    cpts <- if (input$design_type == "tm") 0 else (input$cpts %||% 0)

    # Get randomize option
    randomize <- input$randomize %||% TRUE

    list(
      factors = n_factors,
      cpts = cpts,
      fnames = fnames,
      flims = flims,
      randomize = randomize,
      design_specific = get_design_specific_params(input)
    )
  }

  # Get design-specific parameters
  get_design_specific_params <- function(input) {
    switch(input$design_type,
      "ccd" = list(
        design = input$ccd_design %||% "CCC",
        levels = if (input$ccd_design %||% "CCC" == "CCC") 5 else 3
      ),
      "ffd" = list(
        levels = input$ffd_levels %||% 3,
        center_default = input$ffd_center_default %||% TRUE
      ),
      "frfd" = list(
        p = input$frfd_p %||% 1,
        aliasing = input$frfd_aliasing %||% "default"
      ),
      "tm" = list(
        levels = input$tm_levels %||% 3
      ),
      "bbd" = list(), # No additional parameters
      list()
    )
  }

  # Generate design based on type
 generate_design <- function(design_type, params) {
  switch(design_type,
    "bbd" = {
      result <- doe_bbd(
        factors = params$factors,
        cpts = params$cpts,
        fnames = params$fnames,
        flims = params$flims,
        randomize = params$randomize
      )

      
      data_df <- as.data.frame(result$doe[[1]])
   
      
      list(
        full_result = result, # Store the complete result
        data = data_df
      )
    },
    "ccd" = {
      result <- doe_ccd(
        design = params$design_specific$design,
        levels = params$design_specific$levels,
        factors = params$factors,
        cpts = params$cpts,
        fnames = params$fnames,
        flims = params$flims,
        randomize = params$randomize
      )
      # Extract and clean the data frame
      data_df <- as.data.frame(result$doe[[1]])
      
      list(
        full_result = result, # Store the complete result
        data = data_df
      )
    },
    "ffd" = {
      default_cpts <- if (params$design_specific$levels == 3) 3 else 0
      total_cpts <- default_cpts + params$cpts
      
      result <- doe_ffd(
        levels = params$design_specific$levels,
        factors = params$factors,
        cpts = params$cpts,
        fnames = params$fnames,
        flims = params$flims,
        randomize = params$randomize
      )
      
      # Extract and clean the data frame
      data_df <- as.data.frame(result$doe[[1]])
      
      # Ensure proper column names and structure
      if (!is.null(params$fnames) && length(params$fnames) > 0) {
        # Check if we need to rename columns
        expected_cols <- c("Standard_Order", "A", "B", "C", params$fnames)
        if (ncol(data_df) >= length(params$fnames)) {
          # Keep existing structure but ensure it's a proper data frame
          data_df <- data_df[, 1:min(ncol(data_df), length(expected_cols)), drop = FALSE]
        }
      }
  
      
      list(
        full_result = result, # Store the complete result
        data = data_df
      )
    },
    "frfd" = {
      result <- doe_frfd(
        factors = params$factors,
        p = params$design_specific$p,
        cpts = params$cpts,
        aliasing = params$design_specific$aliasing,
        fnames = params$fnames,
        flims = params$flims,
        randomize = params$randomize
      )
      # Extract and clean the data frame
      data_df <- as.data.frame(result$doe[[1]])
     
      
      list(
        full_result = result, # Store the complete result
        data = data_df
      )
    },
    "tm" = {
      result <- doe_tm(
        levels = params$design_specific$levels,
        factors = params$factors,
        fnames = params$fnames,
        flims = params$flims,
        randomize = params$randomize
      )
      # Extract and clean the data frame
      data_df <- as.data.frame(result$doe[[1]])
 
      
      list(
        full_result = result, # Store the complete result
        data = data_df
      )
    }
  )
}

  # Render design table
  output$design_table <- DT::renderDataTable(
    {
      req(design_results$design_data)
      data <- design_results$design_data
      total_data_cols <- ncol(data)


      format_cols_index <- 2:total_data_cols # (1+((total_data_cols-1)/2))



      dt_output <- DT::datatable(
        data,
        rownames = FALSE,
        options = list(
          scrollX = TRUE,
           #columnDefs = list(defaultContent = "-",targets = "_all"),
          pageLength = 10,
          dom = "Bfrtip",
          buttons = list(
            list(extend = "csv", filename = generate_filename_with_timestamp("supercrit_doe_design_matrix")),
            list(extend = "excel", filename = generate_filename_with_timestamp("supercrit_doe_design_matrix")),
            list(extend = "pdf", filename = generate_filename_with_timestamp("supercrit_doe_design_matrix"))
          )
        ),
        extensions = "Buttons"
      )

      if (length(format_cols_index) > 0) {
        dt_output <- dt_output %>% DT::formatRound(columns = format_cols_index, digits = 2)
      }

      dt_output
    },
    server = FALSE
  )

  # Design description output
output$design_description <- renderText({
  req(design_results$design_description)
  HTML(paste0("<br>", design_results$design_description,"<br>"))
})

  # Design matrix name output
  output$design_matrix_name <- renderUI({
    req(design_results$design_matrix_name)
    tags$h5(style="color:#ffffff;",paste0("(", design_results$design_matrix_name, ")"))
  })

  # Confounding pattern table output
  output$confounding_table <- DT::renderDataTable(
    {
      req(design_results$confounding_pattern)
      DT::datatable(
        design_results$confounding_pattern,
        rownames = FALSE,
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          dom = "Bfrtip",
          buttons = list(
            list(extend = "csv", filename = generate_filename_with_timestamp("supercrit_doe_confounding_pattern")),
            list(extend = "excel", filename = generate_filename_with_timestamp("supercrit_doe_confounding_pattern")),
            list(extend = "pdf", filename = generate_filename_with_timestamp("supercrit_doe_confounding_pattern"))
          )
        ),
        extensions = "Buttons"
      )
    },
    server = FALSE
  )

  # Reactive to check if confounding pattern exists for conditional panel
  output$has_confounding_pattern <- reactive({
    !is.null(design_results$confounding_pattern)
  })
  outputOptions(output, "has_confounding_pattern", suspendWhenHidden = FALSE)

  # Reactive to check if export button should be visible
  output$show_export_button <- reactive({
    !is.null(design_results$design_data)
  })
  outputOptions(output, "show_export_button", suspendWhenHidden = FALSE)

  # Save design - show modal
  observeEvent(input$save_design, {
    req(design_results$design_data)
    showModal(doe_design_save_modal_ui(ns("save_modal"), i18n))
  })

  # Render filename preview in modal
  output[["save_modal-design_name_preview"]] <- renderUI({
    req(input[["save_modal-design_name"]])
    user_input_name <- input[["save_modal-design_name"]]

    # Basic sanitization for filename
    sanitized_name <- gsub("[^a-zA-Z0-9_.-]", "_", user_input_name)
    if (nchar(sanitized_name) == 0) {
      sanitized_name <- "untitled"
    }

    # timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    final_filename <- generate_filename_with_timestamp(paste0(sanitized_name,"_",input$design_type))

    div(
      class = "alert alert-info",
      icon("info-circle"),
      strong(i18n$t("Final filename:")),
      br(),
      final_filename
    )
  })

  # Confirm save design from modal
  observeEvent(input[["save_modal-confirm_save_design"]], {
    req(design_results$design_data, input[["save_modal-design_name"]])

    removeModal() # Close the modal

    tryCatch(
      {
        # Create design data structure
        design_data <- list(
          design_type = design_results$design_type,
          parameters = list(
            factors = design_results$factors,
            factor_names = design_results$factor_names,
            factor_limits = design_results$factor_limits
          ),
          results = list(
            design_matrix = design_results$design_data
          ),
          metadata = list(
            created_date = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            app_version = "0.9.0"
          )
        )

        # Generate filename using user input and timestamp
        user_input_name <- input[["save_modal-design_name"]]
        sanitized_name <- gsub("[^a-zA-Z0-9_.-]", "_", user_input_name)
        if (nchar(sanitized_name) == 0) {
          sanitized_name <- "untitled"
        }
        # timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        filename <- generate_filename_with_timestamp(paste0(sanitized_name,"_",input$design_type))

        # Save using settings utils
        save_settings("doe_design", design_data, filename)

        showNotification(i18n$t("Design saved successfully!"), type = "message")

        # Send custom message to refresh saved designs in other modules
        session$sendCustomMessage("refreshSavedDesigns", list(app = "doe_design"))
      },
      error = function(e) {
        showNotification(paste(i18n$t("Error saving design:"), e$message), type = "error")
      }
    )
  })

  # Export design
  output$export_design <- downloadHandler(
    filename = function() {
      # Use the helper function to generate the filename
      paste0(generate_filename_with_timestamp(paste0("supecrit_doe_design_export_", design_results$design_type)),".tab")
    },
    content = function(file) {
      req(design_results$full_design_result)

      tryCatch(
        {
          # Use the doe_export function
          # The doe_export function writes directly to the file path
          doe_export(
            input = design_results$full_design_result,
            export_name = paste0("# Experimental Design: ", design_results$design_type),
            expath = dirname(file), # doe_export expects a directory, not a full path
            silent = TRUE # Suppress console output from doe_export
          )
          
          temp_files <- list.files(dirname(file), pattern = "doe_.*\\.tab", full.names = TRUE)
          
          latest_file <- NULL
          if (length(temp_files) > 0) {
            file_times <- file.info(temp_files)$mtime
            latest_file <- temp_files[which.max(file_times)]
          }

          if (!is.null(latest_file) && file.exists(latest_file)) {
            # The 'file' argument in content() is the full path to the desired output file
            file.rename(latest_file, file)
            showNotification(i18n$t("Design exported successfully!"), type = "message")
          } else {
            stop("Error: Could not find the exported file to rename.")
          }
        },
        error = function(e) {
          showNotification(paste(i18n$t("Error exporting design:"), e$message), type = "error")
        }
      )
    }
  )

  # Reset function
  observeEvent(input$reset, {
    # Reset all inputs
    updateRadioButtons(session, "design_type", selected = "bbd")
    updateNumericInput(session, "factors", value = 3)
    updateNumericInput(session, "cpts", value = 0)
    updateCheckboxInput(session, "randomize", value = TRUE)

    # Clear results
    design_results$full_design_result <- NULL
    design_results$design_data <- NULL
    design_results$design_description <- NULL
    design_results$design_matrix_name <- NULL
    design_results$confounding_pattern <- NULL
    design_results$design_type <- NULL

    showNotification(i18n$t("Parameters reset"), type = "message")
  })

  # Apply design settings function
  apply_design_settings <- function(session, settings) {
    updateRadioButtons(session, "design_type", selected = settings$design_type)
    updateNumericInput(session, "factors", value = settings$factors)
    updateNumericInput(session, "cpts", value = settings$cpts)

    # Update factor inputs after a short delay to allow UI to render

    if (!is.null(settings$factor_names)) {
      for (i in 1:length(settings$factor_names)) {
        updateTextInput(session, paste0("factor_name_", i), value = settings$factor_names[i])
      }
    }
    if (!is.null(settings$factor_limits)) {
      for (i in 1:length(settings$factor_limits)) {
        updateNumericInput(session, paste0("factor_min_", i), value = settings$factor_limits[[i]][1])
        updateNumericInput(session, paste0("factor_max_", i), value = settings$factor_limits[[i]][2])
      }
    }
  }

  # Intro Help Button Observer (Adaptive)
  observeEvent(input$doe_design_help, {
    # Get current design parameters for adaptive intro
    current_design_type <- input$design_type %||% "bbd"
    current_factors <- input$factors %||% 3

    # Generate adaptive intro steps based on current selections
    introjs(session, options = intro_steps_doe_design(ns, i18n, current_design_type, current_factors))
  })
}
