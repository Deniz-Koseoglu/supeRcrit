doe_desir_server <- function(input, output, session, defaults, i18n) {
  ns <- session$ns

  # Source the save modal UI
  source(file.path("ui_modules", "doe_desir_ui.R"), local = TRUE)$value

  # Reactive values for storing data
  desir_data <- reactiveValues(
    available_setups = NULL,
    selected_setups = NULL,
    loaded_setups_data = list(),
    analysis_results = list(),
    desir_result = NULL,
    response_info = list()
  )

  # Reactive to check if export results button should be visible
  output$show_export_results <- reactive({
    !is.null(desir_data$desir_result)
  })
  outputOptions(output, "show_export_results", suspendWhenHidden = FALSE)

  # ============================================================================
  # 1. LOAD AVAILABLE SETUPS
  # ============================================================================

  # Function to refresh available setups
  refresh_available_setups <- function() {
    setups <- list_saved_settings("doe_analysis")
    desir_data$available_setups <- setups
  }

  # Initial load
  refresh_available_setups()

  # Render analysis selection UI
  output$analysis_selection_ui <- renderUI({
    setups <- desir_data$available_setups

    if (length(setups) == 0) {
      return(
        div(
          class = "alert alert-warning",
          icon("exclamation-triangle"),
          strong(i18n$t("No saved DOE analyses found.")),
          br(),
          i18n$t("Please create and save DOE analyses first in the DOE Analysis tab.")
        )
      )
    }

    # Create choices with metadata
    choices <- setNames(setups, setups)

    checkboxGroupInput(
      ns("selected_analyses"),
      i18n$t("Available Analyses:"),
      choices = choices,
      selected = NULL
    )
  })

  # ============================================================================
  # 2. LOAD SELECTED ANALYSES
  # ============================================================================

  observeEvent(input$load_analyses, {
    req(input$selected_analyses)

    # Validate selection count
    n_selected <- length(input$selected_analyses)
    if (n_selected < 2) {
      showNotification(
        i18n$t("Please select at least 2 analyses."),
        type = "error"
      )
      return()
    }

    if (n_selected > 6) {
      showNotification(
        i18n$t("Please select maximum 6 analyses."),
        type = "warning"
      )
      return()
    }

    # Load selected setups
    withProgress(message = i18n$t("Loading analyses..."), value = 0, {
      loaded_data <- list()

      for (i in seq_along(input$selected_analyses)) {
        setup_name <- input$selected_analyses[i]
        incProgress(1 / length(input$selected_analyses),
          detail = paste(i18n$t("Loading"), setup_name)
        )

        tryCatch(
          {
            setup_data <- load_settings("doe_analysis", setup_name)
            loaded_data[[setup_name]] <- setup_data
          },
          error = function(e) {
            showNotification(
              paste(i18n$t("Error loading"), setup_name, ":", e$message),
              type = "error"
            )
          }
        )
      }

      desir_data$loaded_setups_data <- loaded_data
      desir_data$selected_setups <- input$selected_analyses

      # Calculate response ranges across all loaded setups
      response_ranges <- list()
      for (setup_name in names(loaded_data)) {
        setup <- loaded_data[[setup_name]]
        resp_var <- setup$parameters$response_var
        data <- setup$data$table

        # Ensure resp_var exists in data
        if (!is.null(resp_var) && resp_var %in% names(data) && length(data[[resp_var]]) > 0) {
          resp_values <- data[[resp_var]]

          # Handle cases where response might be numeric or factor/character
          if (is.numeric(resp_values)) {
            resp_values <- as.numeric(resp_values)
          } else {
            # Try to convert to numeric, skip if not possible
            resp_values_num <- suppressWarnings(as.numeric(resp_values))
            if (any(is.na(resp_values_num))) {
              warning(paste("Non-numeric response values found for", resp_var, "in", setup_name))
              next
            }
            resp_values <- resp_values_num
          }

          # Calculate range only if we have valid numeric values
          valid_values <- resp_values[!is.na(resp_values)]
          if (length(valid_values) > 0) {
            if (!resp_var %in% names(response_ranges)) {
              response_ranges[[resp_var]] <- c(min = Inf, max = -Inf)
            }
            response_ranges[[resp_var]]["min"] <- min(response_ranges[[resp_var]]["min"], min(valid_values))
            response_ranges[[resp_var]]["max"] <- max(response_ranges[[resp_var]]["max"], max(valid_values))
          }
        }
      }

      desir_data$response_info <- response_ranges
      print("Calculated response ranges:")
      print(str(response_ranges))
    })

    showNotification(
      paste(i18n$t("Loaded"), length(desir_data$loaded_setups_data), i18n$t("analyses successfully.")),
      type = "message"
    )
  })

  # ============================================================================
  # 3. SELECTED ANALYSES PREVIEW
  # ============================================================================

  output$selected_analyses_preview <- renderUI({
    req(desir_data$loaded_setups_data)

    if (length(desir_data$loaded_setups_data) == 0) {
      return(NULL)
    }

    # Create preview cards
    cards <- lapply(names(desir_data$loaded_setups_data), function(setup_name) {
      setup <- desir_data$loaded_setups_data[[setup_name]]
      params <- setup$parameters

      div(
        class = "alert alert-info",
        style = "margin-bottom: 10px; padding: 10px;",
        strong(setup_name),
        br(),
        tags$small(
          icon("flask"), " ", i18n$t("Response:"), " ", params$response_var, br(),
          icon("cog"), " ", i18n$t("Model Order:"), " ", params$mod_order, br(),
          icon("calendar"), " ", setup$metadata$created_date
        )
      )
    })

    tagList(
      h5(i18n$t("Selected Analyses:")),
      cards
    )
  })

  # ============================================================================
  # 4. DYNAMIC DESIRABILITY SETTINGS UI
  # ============================================================================

  output$desirability_settings_ui <- renderUI({
    req(desir_data$loaded_setups_data)

    # Extract unique responses
    responses <- unique(sapply(desir_data$loaded_setups_data, function(x) {
      x$parameters$response_var
    }))

    print(paste("Creating UI for responses:", paste(responses, collapse = ", ")))

    if (length(responses) == 0) {
      return(div(class = "alert alert-warning", i18n$t("No responses found in loaded analyses.")))
    }



    # Create settings for each response
    response_settings <- lapply(seq_along(responses), function(i) {
      resp <- responses[i]

      tagList(
        h4(paste(i18n$t("Response"), i, ":", resp)),
        hr(),
        fluidRow(
          column(
            4,
            radioButtons(
              inputId = ns(paste0("obj_", i)), # Keep ns() here
              label = i18n$t("Objective:"),
              choices = c(
                "Maximize" = "max",
                "Minimize" = "min",
                "Target" = "trg"
              ),
              selected = "max",
              inline = FALSE
            )
          ),
          column(
            4,
            numericInput(
              inputId = ns(paste0("lower_", i)), # Keep ns() here
              label = i18n$t("Lower Limit:"),
              value = if (!is.null(desir_data$response_info[[resp]][["min"]])) {
                ceiling(desir_data$response_info[[resp]][["min"]])
              } else {
                0
              },
              step = 0.1
            ),
            numericInput(
              inputId = ns(paste0("upper_", i)), # Keep ns() here
              label = i18n$t("Upper Limit:"),
              value = if (!is.null(desir_data$response_info[[resp]][["max"]])) {
                floor(desir_data$response_info[[resp]][["max"]])
              } else {
                100
              },
              step = 0.1
            )
          ),
          column(
            4,
            conditionalPanel(
              condition = sprintf("input['%s'] == 'trg'", ns(paste0("obj_", i))), # Use ns() here too
              numericInput(
                inputId = ns(paste0("target_", i)), # Keep ns() here
                label = i18n$t("Target Value:"),
                value = 50,
                step = 0.1
              )
            ),
            numericInput(
              inputId = ns(paste0("weight1_", i)), # Keep ns() here
              label = i18n$t("Weight (Lower):"),
              value = 1,
              min = 0.1,
              max = 10,
              step = 0.1
            ),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'trg'", ns(paste0("obj_", i))), # Use ns() here too
              numericInput(
                inputId = ns(paste0("weight2_", i)), # Keep ns() here
                label = i18n$t("Weight (Upper):"),
                value = 1,
                min = 0.1,
                max = 10,
                step = 0.1
              )
            )
          )
        ),
        if (i < length(responses)) hr() else NULL
      )
    })

    tagList(response_settings)
  })
  # ============================================================================
  # 5. DYNAMIC FACTOR RANGE UI
  # ============================================================================

  output$factor_range_ui <- renderUI({
    req(desir_data$loaded_setups_data)

    # Get factors from first setup (assuming all have same factors)
    first_setup <- desir_data$loaded_setups_data[[1]]
    data <- first_setup$data$table

    # Identify coded factors
    coded_factors <- grep("^[A-Z]$", names(data), value = TRUE)

    # Create range inputs for each factor
    factor_inputs <- lapply(coded_factors, function(fac) {
      fluidRow(
        column(4, h5(fac)),
        column(
          4,
          numericInput(
            ns(paste0("fac_min_", fac)),
            i18n$t("Min:"),
            value = -1,
            step = 0.1
          )
        ),
        column(
          4,
          numericInput(
            ns(paste0("fac_max_", fac)),
            i18n$t("Max:"),
            value = 1,
            step = 0.1
          )
        )
      )
    })

    tagList(factor_inputs)
  })

  # ============================================================================
  # 6. CALCULATE DESIRABILITY
  # ============================================================================

  observeEvent(input$calculate, {
    req(desir_data$loaded_setups_data)


    # Debug: Print all available inputs
    all_inputs <- names(reactiveValuesToList(input))
    print("All available inputs:")
    print(all_inputs)

    # Debug: Look specifically for our inputs
    desir_inputs <- all_inputs[grepl("^(obj_|lower_|upper_|target_|weight)", all_inputs)]
    print("Desirability-related inputs:")
    print(desir_inputs)


    tryCatch(
      {
        withProgress(message = i18n$t("Calculating desirability..."), value = 0, {
          # STEP 1: Run analyses for each setup
          incProgress(0.1, detail = i18n$t("Running DOE analyses..."))

          mods <- list()
          n_setups <- length(desir_data$loaded_setups_data)

          for (i in seq_along(desir_data$loaded_setups_data)) {
            setup_name <- names(desir_data$loaded_setups_data)[i]
            setup <- desir_data$loaded_setups_data[[setup_name]]

            incProgress(0.4 / n_setups, detail = paste(i18n$t("Analyzing"), setup_name))

            # Extract parameters
            params <- setup$parameters
            data <- setup$data$table

            # --- Add these print statements for debugging ---
            print(paste("Processing setup:", setup_name))
            print(paste("Response variable for this setup:", params$response_var))
            if (is.null(params$response_var) || params$response_var == "") {
              warning(paste("Response variable is NULL or empty for setup:", setup_name))
              next # Skip this setup if response_var is invalid
            }
            # --- End of added debugging ---


            # Run doe_analyze
            analysis_result <- doe_analyze(
              doe = data,
              uc_facs = if (length(params$uc_facs) == 0) NA else params$uc_facs,
              cent_id = NA,
              resp_var = params$response_var,
              time_var = if (is.null(params$time_var) || params$time_var == "") NULL else params$time_var,
              mod_order = params$mod_order,
              canon_thres = params$canon_thres,
              p_cutoff = params$p_cutoff,
              trim_method = params$trim_method,
              which_facs = params$which_facs,
              export = "none",
              asprat = "default",
              verbose = FALSE
            )


            mods[[params$response_var]] <- analysis_result
          }

          desir_data$analysis_results <- mods
          print(names(mods))
          # browser()




          # STEP 2: Prepare desirability parameters

          incProgress(0.2, detail = i18n$t("Preparing parameters..."))


          # STEP 2: Prepare desirability parameters
          incProgress(0.2, detail = i18n$t("Preparing parameters..."))

          responses <- names(mods)
          n_resp <- length(responses)

          # Debug: Print available inputs
          print("Available inputs:")
          print(names(input))
          # ... existing code ...

# Build frng
if (input$use_default_ranges) {
  frng <- "default"
  print("Using default factor ranges")
} else {
  # Get coded factors
  first_setup <- desir_data$loaded_setups_data[[1]]
  data <- first_setup$data$table
  coded_factors <- grep("^[A-Z]$", names(data), value = TRUE)

  frng <- list()
  print("Building custom factor ranges")
  for (fac in coded_factors) {
    frng[[fac]] <- c(
      input[[paste0("fac_min_", fac)]],
      input[[paste0("fac_max_", fac)]]
    )
  }
}

print("frng structure:")
str(frng)

# ... rest of the code ...

          # Build dsrng
          dsrng <- list()
          obj <- character(n_resp)
          wts <- c() # Initialize as empty vector

          for (i in seq_along(responses)) {
            resp <- responses[i]

            # Debug: Check what we're looking for
            lower_id <- paste0("lower_", i)
            upper_id <- paste0("upper_", i)
            obj_id <- paste0("obj_", i)

            print(paste("Looking for inputs:", lower_id, upper_id, obj_id))

            lower <- input[[lower_id]]
            upper <- input[[upper_id]]
            objective <- input[[obj_id]]

            # Validation: Check if inputs exist
            if (is.null(lower) || is.null(upper) || is.null(objective)) {
              stop(paste(
                "Missing input values for response", i, ":", resp,
                "- lower:", is.null(lower),
                "- upper:", is.null(upper),
                "- objective:", is.null(objective)
              ))
            }

            print(paste("Response", i, "values - lower:", lower, "upper:", upper, "objective:", objective))

            if (objective == "trg") {
              target_id <- paste0("target_", i)
              target <- input[[target_id]]

              if (is.null(target)) {
                stop(paste("Missing target value for response", i, ":", resp))
              }

              dsrng[[resp]] <- c(lower, upper, target)

              # Get weights with proper defaults
              weight1 <- input[[paste0("weight1_", i)]]
              weight2 <- input[[paste0("weight2_", i)]]

              # Ensure weights are not NULL
              if (is.null(weight1)) weight1 <- 1
              if (is.null(weight2)) weight2 <- 1

              wts <- c(wts, weight1, weight2)
            } else {
              dsrng[[resp]] <- c(lower, upper)

              # For max/min: get single weight
              weight1 <- input[[paste0("weight1_", i)]]

              # Ensure weight is not NULL
              if (is.null(weight1)) weight1 <- 1

              wts <- c(wts, weight1)
            }

            obj[i] <- objective
          }

          # Additional validation for wts
          if (length(wts) == 0) {
            stop("No weights were collected. Check that weight inputs are properly defined.")
          }

          print("Final weights vector:")
          print(wts)
          print(paste("Length of wts:", length(wts)))




          # STEP 3: Run doe_desir
          incProgress(0.2, detail = i18n$t("Optimizing desirability..."))


          kmed_val <- if (input$kmed == "NA") NA else if (input$kmed == "auto") "auto" else as.numeric(input$kmed)

          cat("\n=== DEBUG: doe_desir parameters ===\n")

          cat("mods:\n")
          # print(names(mods))
          cat("mods class:", class(mods), "\n")
          cat("mods names:", names(mods), "\n\n")

          cat("dsrng:\n")
          print(str(dsrng))
          cat("dsrng class:", class(dsrng), "\n")
          cat("dsrng names:", names(dsrng), "\n\n")

          cat("frng:\n")
          print(str(frng))
          cat("frng class:", class(frng), "\n")
          if (is.list(frng)) cat("frng names:", names(frng), "\n")
          cat("frng value:", frng, "\n\n")

          cat("obj:\n")
          print(obj)
          cat("obj class:", class(obj), "\n")
          cat("obj length:", length(obj), "\n\n")

          cat("dtype:\n")
          print(input$dtype)
          cat("dtype class:", class(input$dtype), "\n\n")

          cat("wts:\n")
          print(wts)
          cat("wts class:", class(wts), "\n")
          cat("wts length:", length(wts), "\n\n")

          cat("spts:\n")
          spts_val <- c(input$spts_random, input$spts_data)
          print(spts_val)
          cat("spts class:", class(spts_val), "\n")
          cat("spts length:", length(spts_val), "\n\n")

          cat("modbase:\n")
          print(input$modbase)
          cat("modbase class:", class(input$modbase), "\n\n")

          cat("optmet:\n")
          print(input$optmet)
          cat("optmet class:", class(input$optmet), "\n\n")

          cat("kmed:\n")
          print(kmed_val)
          cat("kmed class:", class(kmed_val), "\n\n")

          cat("export: 'none'\n")
          cat("silent: TRUE\n")
          cat("=== END DEBUG ===\n\n")
          #browser()


          desir_result <- doe_desir(
            mods = mods,
            dsrng = dsrng,
            frng = frng,
            obj = obj,
            dtype = input$dtype,
            wts = wts,
            spts = c(input$spts_random, input$spts_data),
            modbase = input$modbase,
            optmet = input$optmet,
            kmed = kmed_val,
            export = "none",
            silent = FALSE
          )
          #           doe_lst1 <- load_internal("doe_lst1")

          # desir_result <- doe_desir(mods = doe_lst1,
          #                      dsrng = list(CarnosicAcid_mgg = c(0,150),
          #                      Carnosol_mgg = c(0,65), ExtYield = c(1,7)),
          #                      frng = list(B = c(40,60), A = c(10,30), C = c(1,3)),
          #                      obj = c("max", "max", "max"),
          #                      dtype = "uncoded",
          #                      wts = rep(1,3),
          #                      spts = c(100,10),
          #                      modbase = "final",
          #                      optmet = "nlopt",
          #                      kmed = "auto",
          #                      export = "none",
          #                      silent = FALSE)

          desir_data$desir_result <- desir_result

          incProgress(0.1, detail = i18n$t("Complete!"))
        })

        showNotification(
          i18n$t("Desirability analysis completed successfully!"),
          type = "message"
        )
      },
      error = function(e) {
        showNotification(
          paste(i18n$t("Error in desirability calculation:"), e$message),
          type = "error"
        )
      }
    )
  })

  # ============================================================================
  # 7. RENDER RESULTS - SUMMARY TAB
  # ============================================================================

  output$factor_limits_table <- DT::renderDataTable(
    {
      req(desir_data$desir_result)

      DT::datatable(
        desir_data$desir_result$factor_lims,
        options = list(
          dom = "t",
          pageLength = 20
        ),
        rownames = FALSE
      )
    },
    server = FALSE
  )

  output$response_limits_table <- DT::renderDataTable(
    {
      req(desir_data$desir_result)

      DT::datatable(
        desir_data$desir_result$response_lims,
        options = list(
          dom = "t",
          pageLength = 20
        ),
        rownames = FALSE
      )
    },
    server = FALSE
  )

  output$model_summaries_table <- DT::renderDataTable(
    {
      req(desir_data$desir_result)

      DT::datatable(
        desir_data$desir_result$mod_sums,
        options = list(
          scrollX = TRUE,
          pageLength = 20,
          dom = "t"
        ),
        rownames = FALSE
      )
    },
    server = FALSE
  )

  # ============================================================================
  # 8. RENDER RESULTS - OPTIMIZATION TAB
  # ============================================================================

  output$unique_solutions_table <- DT::renderDataTable(
    {
      req(desir_data$desir_result)

      DT::datatable(
        desir_data$desir_result$unique_solutions,
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          dom = "Bfrtip",
          buttons = list(
            list(extend = "csv", filename = "desirability_unique_solutions"),
            list(extend = "excel", filename = "desirability_unique_solutions")
          )
        ),
        extensions = "Buttons",
        rownames = FALSE
      )
    },
    server = FALSE
  )

  output$output_data_table <- DT::renderDataTable(
    {
      req(desir_data$desir_result)

      DT::datatable(
        desir_data$desir_result$output_data,
        options = list(
          scrollX = TRUE,
          pageLength = 25,
          dom = "Bfrtip",
          buttons = list(
            list(extend = "csv", filename = "desirability_output_data"),
            list(extend = "excel", filename = "desirability_output_data")
          )
        ),
        extensions = "Buttons",
        rownames = FALSE
      )
    },
    server = FALSE
  )

  output$orig_data_table <- DT::renderDataTable(
    {
      req(desir_data$desir_result)

      DT::datatable(
        desir_data$desir_result$orig_data,
        options = list(
          scrollX = TRUE,
          pageLength = 25,
          dom = "Bfrtip",
          buttons = list(
            list(extend = "csv", filename = "desirability_original_data"),
            list(extend = "excel", filename = "desirability_original_data")
          )
        ),
        extensions = "Buttons",
        rownames = FALSE
      )
    },
    server = FALSE
  )

  # ============================================================================
  # 9. DOWNLOAD HANDLERS
  # ============================================================================

  output$download_solutions <- downloadHandler(
    filename = function() {
      paste0(generate_filename_with_timestamp("desirability_solutions"), ".csv")
    },
    content = function(file) {
      req(desir_data$desir_result)
      write.csv(desir_data$desir_result$unique_solutions, file, row.names = FALSE)
    }
  )

  output$download_factor_limits <- downloadHandler(
    filename = function() {
      paste0(generate_filename_with_timestamp("desirability_factor_limits"), ".csv")
    },
    content = function(file) {
      req(desir_data$desir_result)
      write.csv(desir_data$desir_result$factor_lims, file, row.names = FALSE)
    }
  )

  output$download_response_limits <- downloadHandler(
    filename = function() {
      paste0(generate_filename_with_timestamp("desirability_response_limits"), ".csv")
    },
    content = function(file) {
      req(desir_data$desir_result)
      write.csv(desir_data$desir_result$response_lims, file, row.names = FALSE)
    }
  )

  output$download_model_summaries <- downloadHandler(
    filename = function() {
      paste0(generate_filename_with_timestamp("desirability_model_summaries"), ".csv")
    },
    content = function(file) {
      req(desir_data$desir_result)
      write.csv(desir_data$desir_result$mod_sums, file, row.names = FALSE)
    }
  )

  output$export_all <- downloadHandler(
    filename = function() {
      paste0(generate_filename_with_timestamp("desirability_results"), ".zip")
    },
    content = function(file) {
      req(desir_data$desir_result)

      temp_dir <- file.path(tempdir(), paste0("desir_export_", format(Sys.time(), "%Y%m%d%H%M%S")))
      dir.create(temp_dir, recursive = TRUE)

      tryCatch({
        desir_export(input = desir_data$desir_result, expath = temp_dir, silent = TRUE)

        files_to_zip <- list.files(temp_dir, full.names = TRUE, recursive = TRUE)
        zip::zip(zipfile = file, files = files_to_zip, mode = "cherry-pick")
      }, error = function(e) {
        showNotification(paste(i18n$t("Error exporting results:"), e$message), type = "error")
      }, finally = {
        unlink(temp_dir, recursive = TRUE)
      })
    },
    contentType = "application/zip"
  )

  # ============================================================================
  # 10. RESET FUNCTION
  # ============================================================================

  observeEvent(input$reset, {
    desir_data$loaded_setups_data <- list()
    desir_data$selected_setups <- NULL
    desir_data$analysis_results <- list()
    desir_data$desir_result <- NULL
    desir_data$response_info <- list()

    updateCheckboxGroupInput(session, "selected_analyses", selected = character(0))

    showNotification(i18n$t("Reset complete"), type = "message")
  })

 
}
