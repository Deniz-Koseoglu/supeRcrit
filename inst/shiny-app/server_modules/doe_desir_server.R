doe_desir_server <- function(input, output, session, defaults, i18n, tablang, doe_rv) {



  ns <- session$ns
  i18n_r <- reactive({
    i18n
  })

  # Helper function to get numeric columns excluding integers for formatting
  get_numeric_cols_for_formatting <- function(data) {
    names(data)[sapply(data, function(x) {
      is.numeric(x) && !all(x == floor(x), na.rm = TRUE)
    })]
  }

 # Helper function to convert raw equation to LaTeX format
  convert_equation_to_latex <- function(eq) {
    if (is.null(eq) || is.na(eq) || eq == "") return("")
    
    # Start with the equation
    latex_eq <- eq
    
    # Replace multiplication asterisks with proper LaTeX notation
    # First handle coefficients multiplied by terms: "3.1*A" -> "3.1A"
    latex_eq <- gsub("([0-9.]+)\\*([A-Za-z])", "\\1 \\2", latex_eq)
    
    # Handle squared terms: "A^2" -> "A^{2}"
    latex_eq <- gsub("\\^([0-9]+)", "^{\\1}", latex_eq)
    
    # Handle interaction terms: "A*B" -> "A \\cdot B" or just "AB"
    latex_eq <- gsub("([A-Za-z])\\*([A-Za-z])", "\\1 \\\\cdot \\2", latex_eq)
    
    # Replace remaining asterisks
    latex_eq <- gsub("\\*", " \\\\cdot ", latex_eq)
    
    # Add Y = at the beginning
    latex_eq <- paste0("Y = ", latex_eq)
    
    return(latex_eq)
  }

  output$doe_desir_HELP <- renderUI({
    create_help_modal(i18n_r, "doe_desir_help_en")
  })

  # Render accordion expand/collapse button with translated tooltips
  output$accordion_toggle_btn <- renderUI({
    create_accordion_toggle_btn(ns, i18n_r, "desir_accordion")
  })

  # Source the save modal UI
  source(file.path("ui_modules", "doe_desir_ui.R"), local = TRUE)$value

  # Source intro steps
  # Reactive values for storing data
  desir_data <- reactiveValues(
    available_setups = NULL,
    available_setups_full = NULL, # Cache full analysis objects for comparison
    selected_setups = NULL,
    loaded_setups_data = list(),
    last_dtype = "coded",
    analysis_results = list(),
    desir_result = NULL,
    response_info = list(),
    refresh_trigger = NULL # Trigger to refresh analysis list after deletion
  )

  # Reactive output to check if results are available
  output$has_results <- reactive({
    !is.null(desir_data$desir_result)
  })
  outputOptions(output, "has_results", suspendWhenHidden = FALSE)

  # Observer to enable/disable result tabs based on results availability
  observe({
    has_results <- !is.null(desir_data$desir_result)
    if (has_results) {
      session$sendCustomMessage("enableTabs", list(tabsetId = ns("results_tabs")))
    } else {
      session$sendCustomMessage("disableTabs", list(tabsetId = ns("results_tabs"), keepFirst = TRUE))
    }
  })

  # ============================================================================
  # 1. LOAD AVAILABLE SETUPS (from .RData files)
  # ============================================================================

  # Helper function to refresh available analyses (hybrid: session + file system)
  refresh_available_analyses <- function() {
    all_analyses_info <- list() # Collect analysis info with timestamps

    # 1. Get from file system (.RData files) with timestamps
    file_analyses <- list_doe_analyses("doe_analysis")
    # print(paste("DEBUG: Found", length(file_analyses), "analyses from file system"))
    if (length(file_analyses) > 0) {
      for (i in seq_along(file_analyses)) {
        filepath <- file_analyses[i]
        name <- names(file_analyses)[i]

        # Load analysis to get timestamp (temporary load to get metadata)
        analysis_obj <- NULL
        timestamp <- NA
        tryCatch(
          {
            analysis_obj <- load_doe_analysis(filepath)
            timestamp <- if (!is.null(analysis_obj$metadata$timestamp)) {
              as.POSIXct(analysis_obj$metadata$timestamp)
            } else {
              # Fallback to file modification time
              file.info(filepath)$mtime
            }
          },
          error = function(e) {
            warning(paste("Could not load timestamp for", name, ":", e$message))
            timestamp <- file.info(filepath)$mtime # fallback to file mod time
          }
        )

        all_analyses_info[[length(all_analyses_info) + 1]] <- list(
          path = filepath,
          name = name,
          timestamp = timestamp,
          source = "file"
        )
      }
      # print(paste("DEBUG: File analyses:", paste(names(file_analyses), collapse = ", ")))
    }

    # 2. Get from session storage (may include just-saved analyses) with timestamps
    if (!is.null(doe_rv$saved_analyses) && length(doe_rv$saved_analyses) > 0) {
      # print(paste("DEBUG: Found", length(doe_rv$saved_analyses), "analyses in session"))
      for (saved_analysis in doe_rv$saved_analyses) {
        # Check if this is a valid analysis entry
        has_filepath <- !is.null(saved_analysis$filepath) && file.exists(saved_analysis$filepath)
        has_analysis_obj <- !is.null(saved_analysis$analysis_obj)
        
        if (!has_filepath && !has_analysis_obj) {
          next # Skip invalid entries
        }
        
        # Determine path
        if (has_filepath) {
          path <- saved_analysis$filepath
        } else {
          path <- paste0("session://", saved_analysis$id)
        }

        # Get timestamp
        timestamp <- if (!is.null(saved_analysis$analysis_obj$metadata$timestamp)) {
          as.POSIXct(saved_analysis$analysis_obj$metadata$timestamp)
        } else {
          Sys.time() # Current time as fallback for newly created
        }

        all_analyses_info[[length(all_analyses_info) + 1]] <- list(
          path = path,
          name = saved_analysis$name,
          timestamp = timestamp,
          source = "session"
        )
      }
      # print(paste("DEBUG: Session analyses added:", paste(sapply(all_analyses_info[all_analyses_info$source == "session"], function(x) x$name), collapse = ", ")))
    } else {
      # print("DEBUG: No analyses in session storage")
    }

    # Sort by timestamp descending (newest first), then prioritize session over file
    if (length(all_analyses_info) > 0) {
      sorted_analyses <- all_analyses_info[order(
        sapply(all_analyses_info, function(x) x$timestamp),
        sapply(all_analyses_info, function(x) if (x$source == "file") 1 else 0), # file = 1, session = 0, so session sorts first
        decreasing = c(TRUE, FALSE) # timestamp desc, source asc (session before file)
      )]

      # Take only the first 6 (most recent)
      limited_analyses <- head(sorted_analyses, 6)

      # Create the final named vector (filepath with name labels)
      final_analyses <- sapply(limited_analyses, function(x) {
        setNames(x$path, x$name)
      })
      names(final_analyses) <- sapply(limited_analyses, function(x) x$name)

      # print(paste("DEBUG: Total unique analyses:", length(all_analyses_info)))
      # print(paste("DEBUG: Limited to", length(limited_analyses), "most recent analyses"))
      desir_data$available_setups <- final_analyses

      # Cache full analysis objects for comparison
      cached_objects <- list()
      for (item in limited_analyses) {
        filepath <- item$path
        tryCatch(
          {
            # Load the full analysis object
            if (grepl("^session://", filepath)) {
              # From session storage
              session_id <- as.numeric(sub("^session://", "", filepath))
              for (saved_analysis in doe_rv$saved_analyses) {
                if (!is.null(saved_analysis$id) && saved_analysis$id == session_id) {
                  cached_objects[[filepath]] <- saved_analysis$analysis_obj
                  break
                }
              }
            } else {
              # From file system
              cached_objects[[filepath]] <- load_doe_analysis(filepath)
            }
          },
          error = function(e) {
            warning(paste("Could not cache analysis object for", item$name, ":", e$message))
          }
        )
      }
      desir_data$available_setups_full <- cached_objects
      # print(paste("DEBUG: Cached", length(cached_objects), "full analysis objects"))
    } else {
      # print("DEBUG: No analyses found from any source")
      desir_data$available_setups <- NULL
      desir_data$available_setups_full <- NULL
    }
  }

  # Initialize available setups on module load
  observe({
    refresh_available_analyses()
  })

  # Also refresh when session storage changes
  observeEvent(doe_rv$saved_analyses,
    {
      refresh_available_analyses()
    },
    ignoreNULL = FALSE
  )

  # Load Example Data button handler
  observeEvent(input$load_example_data, {
    seed_data <- doe_rv$seed_analyses
    if (length(seed_data) == 0) {
      showNotification(i18n$t("No example data available."), type = "warning")
      return()
    }
    
    # Check if seed data is already loaded (avoid duplicates)
    existing_names <- sapply(doe_rv$saved_analyses, function(x) x$name)
    seed_names <- sapply(seed_data, function(x) x$name)
    
    new_seeds <- seed_data[!seed_names %in% existing_names]
    
    if (length(new_seeds) == 0) {
      showNotification(i18n$t("Example data already loaded."), type = "message")
      return()
    }
    
    # Append seed data to saved_analyses (already has correct structure from helper)
    current_len <- length(doe_rv$saved_analyses)
    for (i in seq_along(new_seeds)) {
      seed <- new_seeds[[i]]
      # Update ID to avoid conflicts
      seed$id <- current_len + i
      doe_rv$saved_analyses[[current_len + i]] <- seed
    }
    
    showNotification(
      sprintf(i18n$t("Loaded %d example analysis(es)."), length(new_seeds)), 
      type = "message"
    )
  })

  # Refresh when triggered by deletion
  observeEvent(desir_data$refresh_trigger, {
    refresh_available_analyses()
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # Listen for refresh button click
  observeEvent(input$refresh_analyses, {
    refresh_available_analyses()

    # Clear all selections
    updateCheckboxGroupButtons(
      session,
      "selected_analyses",
      selected = character(0),
      disabledChoices = character(0) # Enable all choices
    )

    # Enable the refresh button itself
    shinyjs::enable("refresh_analyses")

    showNotification(i18n$t("Analysis list refreshed!"), type = "message", duration = 2)
  })

  # Render dtype selectInput with i18n
  output$dtype_ui <- renderUI({
    selectInput(ns("dtype"),
      tags$span(i18n_r()$t("Factor Type"),
        input_help(i18n_r()$t("Whether to express factor ranges in coded (-1 to +1) or uncoded (actual) units. Coded units are standardized; uncoded use the original measurement scales."),
                   title = i18n_r()$t("Factor Type"), buttonLabel = i18n_r()$t("OK"))),
      choices = setNames(c("coded", "uncoded"), c(i18n_r()$t("Coded"), i18n_r()$t("Uncoded"))),
      selected = input$dtype %||% defaults$dtype %||% "coded"
    )
  })
  outputOptions(output, "dtype_ui", suspendWhenHidden = FALSE)

  # Render modbase selectInput with i18n
  output$modbase_ui <- renderUI({
    selectInput(ns("modbase"),
      tags$span(i18n_r()$t("Model Base"),
        input_help(i18n_r()$t("Which model from the DOE Analysis to use for predictions. Initial uses the full model before trimming. Final uses the simplified model after removing insignificant terms."),
                   title = i18n_r()$t("Model Base"), buttonLabel = i18n_r()$t("OK"))),
      choices = setNames(c("initial", "final"), c(i18n_r()$t("Initial"), i18n_r()$t("Final"))),
      selected = input$modbase %||% defaults$modbase %||% "final"
    )
  })
  outputOptions(output, "modbase_ui", suspendWhenHidden = FALSE)

  # Render optmet selectInput with i18n
  output$optmet_ui <- renderUI({
    selectInput(ns("optmet"),
      tags$span(i18n_r()$t("Optimization Method"),
        input_help(i18n_r()$t("Optimization algorithm for finding factor settings that maximize overall desirability. Global searches broadly using multiple starting points (recommended). Local uses a single-start Nelder-Mead simplex method."),
                   title = i18n_r()$t("Optimization Method"), buttonLabel = i18n_r()$t("OK"))),
      choices = setNames(c("nlopt", "optim"), c(
        i18n_r()$t("Non-Linear (Global)"),
        i18n_r()$t("Nelder-Mead (Local)")
      )),
      selected = defaults$optmet %||% "nlopt"
    )
  })
  outputOptions(output, "optmet_ui", suspendWhenHidden = FALSE)

  # Render kmed selectInput with i18n
  output$kmed_ui <- renderUI({
    selectInput(ns("kmed"),
      tags$span(i18n_r()$t("Clustering"),
        input_help(i18n_r()$t("Apply k-medoids clustering to group similar optimal solutions. Auto detects the best number of clusters. None disables clustering. Useful when multiple local optima exist."),
                   title = i18n_r()$t("Clustering"), buttonLabel = i18n_r()$t("OK"))),
      choices = setNames(
        c("NA", "auto", "2", "3", "4", "5"),
        c(i18n_r()$t("None"), i18n_r()$t("Auto"), "2", "3", "4", "5")
      ),
      selected = "NA"
    )
  })
  outputOptions(output, "kmed_ui", suspendWhenHidden = FALSE)

  output$use_default_ranges_ui <- renderUI({
    tags$span(
      checkboxInput(ns("use_default_ranges"), i18n_r()$t("Use Default Factor Ranges"),
                    value = isolate(input$use_default_ranges) %||% defaults$use_default_ranges),
      title = i18n_r()$t("When enabled, factor ranges are set to the coded design limits (-1 to +1) or the corresponding uncoded values. Disable to specify custom ranges.")
    )
  })

  output$spts_random_ui <- renderUI({
    numericInput(ns("spts_random"),
      tags$span(i18n_r()$t("Random Starting Points"),
        input_help(i18n_r()$t("Number of randomly generated starting points for the optimization search. More starting points increase the chance of finding the global optimum but take longer to compute."),
                   title = i18n_r()$t("Random Starting Points"), buttonLabel = i18n_r()$t("OK"))),
      value = isolate(input$spts_random) %||% defaults$spts_random, min = 10, max = 1000, step = 10
    )
  })

  output$spts_data_ui <- renderUI({
    numericInput(ns("spts_data"),
      tags$span(i18n_r()$t("Data Starting Points"),
        input_help(i18n_r()$t("Number of starting points drawn from the experimental data itself. These complement random starting points by seeding the search near observed conditions."),
                   title = i18n_r()$t("Data Starting Points"), buttonLabel = i18n_r()$t("OK"))),
      value = isolate(input$spts_data) %||% defaults$spts_data, min = 1, max = 100, step = 1
    )
  })

  # Render analysis selection UI
  output$analysis_selection_ui <- renderUI({
    setups <- desir_data$available_setups

    if (is.null(setups) || length(setups) == 0) {
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

    # setups is a named vector: names = display names, values = filepaths
    tagList(
      # Header row with label and delete button
      div(
        style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 5px;",
        tags$label(i18n$t("Available Analyses"), class = "control-label", style = "margin-bottom: 0;"),
        actionButton(
          ns("delete_selected_analyses"),
          label = i18n$t("Delete Selected"),
          icon = icon("trash-alt"),
          class = "btn-xs btn-danger",
          style = "color: white;",
          disabled = "disabled"
        )
      ),
      div(
        style = "max-height: 240px; overflow-y: auto;",
        checkboxGroupButtons(
          inputId = ns("selected_analyses"),
          label = NULL,
          choices = setups,
          selected = NULL,
          direction = "vertical",
          individual = TRUE,
          width = "100%",
          checkIcon = list(
            yes = icon("ok", lib = "glyphicon"),
            no = icon("unchecked", lib = "glyphicon")
          )
        )
      )
    )
  })

  # Enable/disable Delete Selected and Load Selected buttons based on checkbox selection
  observe({
    selected <- input$selected_analyses
    has_selection <- !is.null(selected) && length(selected) > 0
    shinyjs::toggleState("delete_selected_analyses", condition = has_selection)
    shinyjs::toggleState("load_analyses", condition = has_selection)
  })

  # Observer for delete button
  observeEvent(input$delete_selected_analyses, {
    req(input$selected_analyses)
    req(length(input$selected_analyses) > 0)
    
    # Get display names for selected analyses
    setups <- desir_data$available_setups
    selected_names <- names(setups)[setups %in% input$selected_analyses]
    
    # Show confirmation dialog
    showModal(modalDialog(
      title = i18n$t("Confirm Deletion"),
      p(i18n$t("Are you sure you want to delete the selected analysis/analyses?")),
      tags$ul(lapply(selected_names, function(n) tags$li(tags$strong(n)))),
      div(
        style = "background-color: #f8d7da; border: 1px solid #f5c6cb; border-radius: 4px; padding: 10px; margin-top: 10px;",
        tags$span(
          style = "color: #721c24; font-weight: bold;",
          icon("exclamation-triangle"),
          i18n$t("This will permanently delete the file(s) from disk.")
        )
      ),
      footer = tagList(
        modalButton(i18n$t("Cancel")),
        actionButton(ns("confirm_delete_analyses"), i18n$t("Delete"),
          class = "btn-danger", style = "color: white;")
      )
    ))
  })

  # Observer for confirm delete
  observeEvent(input$confirm_delete_analyses, {
    removeModal()
    
    req(input$selected_analyses)
    
    deleted_count <- 0
    for (filepath in input$selected_analyses) {
      # Delete the file if it exists on disk (not session-based)
      if (!grepl("^session://", filepath) && file.exists(filepath)) {
        tryCatch({
          file.remove(filepath)
          deleted_count <- deleted_count + 1
        }, error = function(e) {
          showNotification(
            paste0(i18n$t("Error deleting file: "), e$message),
            type = "error"
          )
        })
      } else if (grepl("^session://", filepath)) {
        # Remove from session storage
        session_id <- as.numeric(sub("^session://", "", filepath))
        doe_rv$saved_analyses <- Filter(function(x) {
          is.null(x$id) || x$id != session_id
        }, doe_rv$saved_analyses)
        deleted_count <- deleted_count + 1
      }
    }
    
    if (deleted_count > 0) {
      showNotification(
        paste0(i18n$t("Deleted "), deleted_count, i18n$t(" analysis/analyses")),
        type = "warning",
        duration = 3
      )
    }

    # Trigger refresh of the analysis list
    desir_data$refresh_trigger <- Sys.time()
  })

  # ============================================================================
  # 1B. MONITOR SELECTIONS AND DISABLE UNRELATED ANALYSES
  # ============================================================================

  # Observe when user selects/deselects analyses
  observeEvent(input$selected_analyses,
    {
      req(desir_data$available_setups)
      req(desir_data$available_setups_full)

      if (length(input$selected_analyses) == 0) {
        # No selections - enable everything
        updateCheckboxGroupButtons(
          session,
          "selected_analyses",
          disabledChoices = character(0)
        )
        shinyjs::enable("refresh_analyses")
        # print("DEBUG: No selections - all analyses enabled")
      } else {
        # At least one analysis selected - implement relatedness logic
        shinyjs::disable("refresh_analyses")

        # Get the reference data from the first selected analysis
        first_path <- input$selected_analyses[1]

        if (!first_path %in% names(desir_data$available_setups_full)) {
          warning(paste("Selected analysis not found in cache:", first_path))
          return()
        }

        reference_obj <- desir_data$available_setups_full[[first_path]]

        # Get Orig_Data from results$initial
        if (is.null(reference_obj$results) ||
          is.null(reference_obj$results$initial) ||
          is.null(reference_obj$results$initial$Orig_Data)) {
          warning(paste("Could not find Orig_Data in reference analysis:", first_path))
          return()
        }

        reference_data <- reference_obj$results$initial$Orig_Data
        # print(paste("DEBUG: Reference analysis:", first_path))
        # print(paste("DEBUG: Reference Orig_Data dimensions:", nrow(reference_data), "x", ncol(reference_data)))

        # Check all available analyses for compatibility
        unrelated_choices <- c()

        for (analysis_path in names(desir_data$available_setups_full)) {
          # Skip if it's the currently selected one
          if (analysis_path %in% input$selected_analyses) {
            next
          }

          current_obj <- desir_data$available_setups_full[[analysis_path]]

          # Safety check
          if (is.null(current_obj$results) ||
            is.null(current_obj$results$initial) ||
            is.null(current_obj$results$initial$Orig_Data)) {
            unrelated_choices <- c(unrelated_choices, analysis_path)
            next
          }

          current_data <- current_obj$results$initial$Orig_Data

          # Compare using identical()
          if (!identical(reference_data, current_data)) {
            unrelated_choices <- c(unrelated_choices, analysis_path)
            print(paste(
              "DEBUG: Unrelated analysis:", analysis_path,
              "- dimensions:", nrow(current_data), "x", ncol(current_data)
            ))
          } else {
            print(paste("DEBUG: Related analysis:", analysis_path))
          }
        }

        # Update UI to disable unrelated choices
        updateCheckboxGroupButtons(
          session,
          "selected_analyses",
          disabledChoices = unrelated_choices
        )

        print(paste("DEBUG: Disabled", length(unrelated_choices), "unrelated analyses"))
      }
    },
    ignoreNULL = FALSE
  )

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

    # Load selected setups from .RData files OR session storage
    withProgress(message = i18n$t("Loading analyses..."), value = 0, {
      loaded_data <- list()

      for (i in seq_along(input$selected_analyses)) {
        filepath <- input$selected_analyses[i]
        incProgress(1 / length(input$selected_analyses),
          detail = paste(i18n$t("Loading"), basename(filepath))
        )

        tryCatch(
          {
            # Initialize variables
            analysis_obj <- NULL
            setup_name <- NULL

            # Check if this is a session-only analysis
            if (grepl("^session://", filepath)) {
              # Extract session ID
              session_id <- as.numeric(sub("^session://", "", filepath))
              print(paste("DEBUG: Loading from session, ID:", session_id))

              # Find in session storage
              for (saved_analysis in doe_rv$saved_analyses) {
                if (!is.null(saved_analysis$id) && saved_analysis$id == session_id) {
                  analysis_obj <- saved_analysis$analysis_obj
                  setup_name <- saved_analysis$name
                  print(paste("DEBUG: Found session analysis:", setup_name))
                  break
                }
              }

              if (is.null(analysis_obj)) {
                stop(paste("Session analysis with ID", session_id, "not found"))
              }
            } else {
              # Load analysis object from .RData file
              analysis_obj <- load_doe_analysis(filepath)

              # Extract setup name from metadata
              setup_name <- if (!is.null(analysis_obj$metadata$name)) {
                analysis_obj$metadata$name
              } else {
                # Fallback: use filename without extension and timestamp
                basename(tools::file_path_sans_ext(filepath))
              }
            }

            # Store the FULL analysis object (already contains doe_analyze result)
            # No need to transform - we'll use it directly
            loaded_data[[setup_name]] <- analysis_obj
            print(paste("DEBUG: Loaded setup:", setup_name))
          },
          error = function(e) {
            showNotification(
              paste(i18n$t("Error loading"), basename(filepath), ":", e$message),
              type = "error"
            )
          }
        )
      }

      desir_data$loaded_setups_data <- loaded_data
      desir_data$selected_setups <- names(loaded_data)

      # Calculate response ranges across all loaded setups
      response_ranges <- list()
      for (setup_name in names(loaded_data)) {
        analysis_obj <- loaded_data[[setup_name]]
        resp_var <- analysis_obj$metadata$parameters$response_var
        data <- analysis_obj$metadata$input_data

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
      analysis_obj <- desir_data$loaded_setups_data[[setup_name]]
      params <- analysis_obj$metadata$parameters

      div(
        class = "alert alert-info",
        style = "margin-bottom: 10px; padding: 10px;",
        strong(setup_name),
        br(),
        tags$small(
          icon("flask"), " ", i18n$t("Response"), " ", params$response_var, br(),
          icon("cog"), " ", i18n$t("Model Order"), " ", params$mod_order, br(),
          icon("calendar"), " ", analysis_obj$metadata$timestamp
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
    # Check if any analyses have been loaded
    if (length(desir_data$loaded_setups_data) == 0) {
      return(div(
        class = "text-muted", 
        style = "font-style: italic; padding: 10px 0;",
        i18n$t("Load DOE analyses above to configure desirability settings.")
      ))
    }

    # Extract unique responses
    responses <- unique(sapply(desir_data$loaded_setups_data, function(x) {
      x$metadata$parameters$response_var
    }))

    # Touch objective inputs to re-render when they change (for greyed-out state)
    for (ri in seq_along(responses)) input[[paste0("obj_", ri)]]

    if (length(responses) == 0) {
      return(div(class = "alert alert-info", i18n$t("No responses found in loaded analyses.")))
    }



    # Create settings for each response
    response_settings <- lapply(seq_along(responses), function(i) {
      resp <- responses[i]
      
      # Get response data range for range badges
      resp_min <- desir_data$response_info[[resp]][["min"]] %||% 0
      resp_max <- desir_data$response_info[[resp]][["max"]] %||% 100
      resp_badge <- paste0(round(resp_min, 2), "\u2013", round(resp_max, 2))
      
      # Auto-correct helper (suppress notification when input hasn't been set by user yet)
      clamp_val <- function(val, lo, hi, field_name, input_exists = TRUE) {
        if (!is.null(val) && !is.na(val) && (val < lo || val > hi)) {
          if (input_exists) {
            showNotification(paste0(resp, " ", field_name, " ", i18n$t("was adjusted to the valid range.")), type = "warning")
          }
          max(lo, min(hi, val))
        } else val
      }
      
      lower_input <- input[[paste0("lower_", i)]]
      upper_input <- input[[paste0("upper_", i)]]
      target_input <- input[[paste0("target_", i)]]

      lower_val <- clamp_val(
        lower_input %||% ceiling(resp_min),
        resp_min, resp_max, i18n$t("Lower Limit"), !is.null(lower_input)
      )
      upper_val <- clamp_val(
        upper_input %||% floor(resp_max),
        resp_min, resp_max, i18n$t("Upper Limit"), !is.null(upper_input)
      )
      target_val <- clamp_val(
        target_input %||% round(mean(c(resp_min, resp_max)), 1),
        resp_min, resp_max, i18n$t("Target Value"), !is.null(target_input)
      )
      
      is_target <- identical(input[[paste0("obj_", i)]], "trg")

      tagList(
        h4(paste(i18n$t("Response"), i, ":", resp)),
        # Row 1: Objective + Target Value
        div(
          style = "display: flex; align-items: center; gap: 8px; margin-bottom: 8px;",
          div(
            style = "flex: 1;",
            selectInput(
              inputId = ns(paste0("obj_", i)),
              label = tags$span(i18n$t("Objective"),
                if (i == 1) input_help(i18n_r()$t("Desirability objective for this response. Maximize drives the response toward its upper limit. Minimize drives it toward its lower limit. Target aims for a specific value between the limits."),
                                       title = i18n_r()$t("Objective"), buttonLabel = i18n_r()$t("OK"))),
              choices = setNames(
                c("max", "min", "trg"),
                c(i18n$t("Maximize"), i18n$t("Minimize"), i18n$t("Target"))
              ),
              selected = input[[paste0("obj_", i)]] %||% "max",
              width = "100%"
            )
          ),
          div(
            style = paste0("flex: 1;", if (!is_target) " opacity: 0.4; pointer-events: none;" else ""),
            tags$div(
              tags$label(
                i18n$t("Target Value"),
                if (i == 1) input_help(i18n_r()$t("Desired target value for this response when the objective is set to Target. The desirability is highest at this value and decreases toward the limits. Only active when Objective is set to Target."),
                                       title = i18n_r()$t("Target Value"), buttonLabel = i18n_r()$t("OK")),
                class = "control-label",
                style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
                tags$span(resp_badge,
                  style = "font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: #6c757d; color: white; margin-left: auto; font-weight: normal;"
                )
              ),
              numericInput(ns(paste0("target_", i)), label = NULL, value = round(target_val, 2), step = 0.1)
            )
          )
        ),
        # Row 2: Lower Limit + Upper Limit
        div(
          style = "display: flex; align-items: center; gap: 8px; margin-bottom: 8px;",
          div(
            style = "flex: 1;",
            tags$div(
              tags$label(
                i18n$t("Lower Limit"),
                if (i == 1) input_help(i18n_r()$t("Lower boundary of the acceptable range for this response. For Maximize, the desirability is zero at or below this value. For Target, it defines the lower end of the desirability window."),
                                       title = i18n_r()$t("Lower Limit"), buttonLabel = i18n_r()$t("OK")),
                class = "control-label",
                style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
                tags$span(resp_badge,
                  style = "font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: #6c757d; color: white; margin-left: auto; font-weight: normal;"
                )
              ),
              numericInput(ns(paste0("lower_", i)), label = NULL, value = round(lower_val, 2), step = 0.1)
            )
          ),
          div(
            style = "flex: 1;",
            tags$div(
              tags$label(
                i18n$t("Upper Limit"),
                if (i == 1) input_help(i18n_r()$t("Upper boundary of the acceptable range for this response. For Minimize, the desirability is zero at or above this value. For Target, it defines the upper end of the desirability window."),
                                       title = i18n_r()$t("Upper Limit"), buttonLabel = i18n_r()$t("OK")),
                class = "control-label",
                style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
                tags$span(resp_badge,
                  style = "font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: #6c757d; color: white; margin-left: auto; font-weight: normal;"
                )
              ),
              numericInput(ns(paste0("upper_", i)), label = NULL, value = round(upper_val, 2), step = 0.1)
            )
          )
        ),
        # Row 3: Weight (Lower) + Weight (Upper)
        div(
          style = "display: flex; align-items: center; gap: 8px; margin-bottom: 8px;",
          div(
            style = "flex: 1;",
            tags$div(
              tags$label(
                i18n$t("Weight (Lower)"),
                if (i == 1) input_help(i18n_r()$t("Shape parameter for the desirability function below the target (or for the entire range when maximizing/minimizing). Values less than 1 give a convex shape (lenient), 1 is linear, and values greater than 1 give a concave shape (strict)."),
                                       title = i18n_r()$t("Weight"), buttonLabel = i18n_r()$t("OK")),
                class = "control-label",
                style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
                tags$span("0.1\u201310",
                  style = "font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: #6c757d; color: white; margin-left: auto; font-weight: normal;"
                )
              ),
              numericInput(ns(paste0("weight1_", i)), label = NULL, value = input[[paste0("weight1_", i)]] %||% 1, min = 0.1, max = 10, step = 0.1)
            )
          ),
          div(
            style = paste0("flex: 1;", if (!is_target) " opacity: 0.4; pointer-events: none;" else ""),
            tags$div(
              tags$label(
                i18n$t("Weight (Upper)"),
                class = "control-label",
                style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
                tags$span("0.1\u201310",
                  style = "font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: #6c757d; color: white; margin-left: auto; font-weight: normal;"
                )
              ),
              numericInput(ns(paste0("weight2_", i)), label = NULL, value = input[[paste0("weight2_", i)]] %||% 1, min = 0.1, max = 10, step = 0.1)
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
    # Re-render when factor type changes
    input$dtype
    
    # Check if any analyses have been loaded
    if (length(desir_data$loaded_setups_data) == 0) {
      return(div(
        class = "text-muted", 
        style = "font-style: italic; padding: 10px 0;",
        i18n$t("Load DOE analyses above to configure factor ranges.")
      ))
    }

    # Get factors from first setup (assuming all have same factors)
    first_analysis <- desir_data$loaded_setups_data[[1]]
    data <- first_analysis$metadata$input_data

    # Identify coded factors
    coded_factors <- grep("^[A-Z]$", names(data), value = TRUE)
    
    # Get uncoded factor names if available
    realnames <- first_analysis$models$final$realnames
    
    # Determine factor type
    factor_type <- input$dtype %||% "coded"
    is_coded <- factor_type == "coded"

    # Detect if factor type just changed (to suppress auto-correct and reset values)
    dtype_changed <- !identical(isolate(desir_data$last_dtype), factor_type)
    desir_data$last_dtype <- factor_type

    # Compute label width dynamically based on longest factor name
    label_width <- if (!is_coded && !is.null(realnames)) {
      max_label <- max(nchar(paste0(coded_factors, " (", realnames, ")")), na.rm = TRUE)
      max(50, min(200, max_label * 8))
    } else {
      15
    }

    # Create range inputs for each factor
    factor_inputs <- lapply(seq_along(coded_factors), function(i) {
      fac <- coded_factors[i]
      
      # Build label
      has_uncoded <- !is.null(realnames) && length(realnames) >= i && !is.na(realnames[i])
      fac_label <- if (!is_coded && has_uncoded) {
        paste0(fac, " (", realnames[i], ")")
      } else {
        fac
      }
      
      # Get data range - try multiple sources
      fac_range <- c(-1, 1)  # default fallback
      if (is_coded) {
        if (fac %in% names(data)) fac_range <- range(data[, fac], na.rm = TRUE)
      } else if (has_uncoded) {
        # Try input_data first, then orig_df from the model
        if (realnames[i] %in% names(data)) {
          fac_range <- range(data[, realnames[i]], na.rm = TRUE)
        } else {
          # Compute uncoded range from coded range via linear interpolation
          # coded range is always in data
          coded_range <- range(data[, fac], na.rm = TRUE)
          # Use doe_decode to get uncoded equivalents
          tryCatch({
            orig_df <- first_analysis$models$final$orig_df
            if (!is.null(orig_df) && realnames[i] %in% names(orig_df)) {
              fac_range <- range(orig_df[, realnames[i]], na.rm = TRUE)
            }
          }, error = function(e) NULL)
        }
      }
      
      # Current values - reset on dtype change, preserve otherwise
      if (dtype_changed) {
        cur_min <- fac_range[1]
        cur_max <- fac_range[2]
      } else {
        cur_min <- isolate(input[[paste0("fac_min_", fac)]])
        cur_max <- isolate(input[[paste0("fac_max_", fac)]])
        if (is.null(cur_min) || is.na(cur_min)) cur_min <- fac_range[1]
        if (is.null(cur_max) || is.na(cur_max)) cur_max <- fac_range[2]
      }
      
      badge_text <- paste0(round(fac_range[1], 2), "\u2013", round(fac_range[2], 2))
      badge_style <- "font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: #6c757d; color: white; margin-left: auto; font-weight: normal; white-space: nowrap;"
      
      div(
        style = paste0("display: flex; align-items: flex-end; gap: 8px; margin-bottom: 5px;"),
        tags$span(
          style = paste0("font-weight: bold; min-width: ", label_width, "px; text-align: right; padding-bottom: 7px;"),
          fac_label
        ),
        div(
          style = "flex: 1;",
          tags$label(
            i18n$t("Min"),
            if (i == 1) input_help(i18n_r()$t("Minimum value of this factor for the optimization search space. The optimizer will not explore below this value."),
                                    title = i18n_r()$t("Min"), buttonLabel = i18n_r()$t("OK")),
            class = "control-label",
            style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
            tags$span(badge_text, style = badge_style)
          ),
          numericInput(ns(paste0("fac_min_", fac)), label = NULL, value = round(cur_min, 2), step = 0.1) |>
            tagAppendAttributes(style = "margin-bottom: 0;")
        ),
        div(
          style = "flex: 1;",
          tags$label(
            i18n$t("Max"),
            if (i == 1) input_help(i18n_r()$t("Maximum value of this factor for the optimization search space. The optimizer will not explore above this value."),
                                    title = i18n_r()$t("Max"), buttonLabel = i18n_r()$t("OK")),
            class = "control-label",
            style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
            tags$span(badge_text, style = badge_style)
          ),
          numericInput(ns(paste0("fac_max_", fac)), label = NULL, value = round(cur_max, 2), step = 0.1) |>
            tagAppendAttributes(style = "margin-bottom: 0;")
        )
      )
    })

    tagList(factor_inputs)
  })

  # Auto-correct factor range values when user edits them
  # Use a single observer per possible factor letter (A-Z)
  lapply(LETTERS, function(fac) {
    observeEvent(input[[paste0("fac_min_", fac)]], {
      req(desir_data$loaded_setups_data)
      req(length(desir_data$loaded_setups_data) > 0)
      
      fac_range <- get_factor_range(fac)
      if (is.null(fac_range)) return()
      
      val <- input[[paste0("fac_min_", fac)]]
      if (!is.null(val) && !is.na(val) && (val < fac_range[1] || val > fac_range[2])) {
        clamped <- max(fac_range[1], min(fac_range[2], val))
        fac_label <- get_factor_label(fac)
        showNotification(paste0(fac_label, " Min ", i18n$t("was adjusted to the valid range.")), type = "warning")
        updateNumericInput(session, paste0("fac_min_", fac), value = round(clamped, 2))
      }
    }, ignoreInit = TRUE)
    
    observeEvent(input[[paste0("fac_max_", fac)]], {
      req(desir_data$loaded_setups_data)
      req(length(desir_data$loaded_setups_data) > 0)
      
      fac_range <- get_factor_range(fac)
      if (is.null(fac_range)) return()
      
      val <- input[[paste0("fac_max_", fac)]]
      if (!is.null(val) && !is.na(val) && (val < fac_range[1] || val > fac_range[2])) {
        clamped <- max(fac_range[1], min(fac_range[2], val))
        fac_label <- get_factor_label(fac)
        showNotification(paste0(fac_label, " Max ", i18n$t("was adjusted to the valid range.")), type = "warning")
        updateNumericInput(session, paste0("fac_max_", fac), value = round(clamped, 2))
      }
    }, ignoreInit = TRUE)
  })
  
  # Helper: get current factor range based on dtype
  get_factor_range <- function(fac) {
    tryCatch({
      first_analysis <- desir_data$loaded_setups_data[[1]]
      data <- first_analysis$metadata$input_data
      coded_factors <- grep("^[A-Z]$", names(data), value = TRUE)
      if (!(fac %in% coded_factors)) return(NULL)
      
      idx <- which(coded_factors == fac)
      realnames <- tryCatch(first_analysis$models$final$realnames, error = function(e) NULL)
      factor_type <- input$dtype %||% "coded"
      is_coded <- factor_type == "coded"
      has_uncoded <- !is.null(realnames) && length(realnames) >= idx && !is.na(realnames[idx])
      
      if (is_coded && fac %in% names(data)) {
        return(range(data[, fac], na.rm = TRUE))
      } else if (!is_coded && has_uncoded) {
        if (realnames[idx] %in% names(data)) {
          return(range(data[, realnames[idx]], na.rm = TRUE))
        }
        orig_df <- tryCatch(first_analysis$models$final$orig_df, error = function(e) NULL)
        if (!is.null(orig_df) && realnames[idx] %in% names(orig_df)) {
          return(range(orig_df[, realnames[idx]], na.rm = TRUE))
        }
      }
      return(c(-1, 1))
    }, error = function(e) NULL)
  }
  
  # Helper: get factor label based on dtype
  get_factor_label <- function(fac) {
    tryCatch({
      first_analysis <- desir_data$loaded_setups_data[[1]]
      data <- first_analysis$metadata$input_data
      coded_factors <- grep("^[A-Z]$", names(data), value = TRUE)
      idx <- which(coded_factors == fac)
      realnames <- tryCatch(first_analysis$models$final$realnames, error = function(e) NULL)
      factor_type <- input$dtype %||% "coded"
      has_uncoded <- !is.null(realnames) && length(realnames) >= idx && !is.na(realnames[idx])
      
      if (factor_type != "coded" && has_uncoded) {
        paste0(fac, " (", realnames[idx], ")")
      } else {
        fac
      }
    }, error = function(e) fac)
  }

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
          # STEP 1: Extract saved analysis results (no need to re-run doe_analyze!)
          incProgress(0.1, detail = i18n$t("Loading saved analyses..."))

          mods <- list()
          n_setups <- length(desir_data$loaded_setups_data)

          for (i in seq_along(desir_data$loaded_setups_data)) {
            setup_name <- names(desir_data$loaded_setups_data)[i]
            analysis_obj <- desir_data$loaded_setups_data[[setup_name]]

            incProgress(0.4 / n_setups, detail = paste(i18n$t("Processing"), setup_name))

            # Get response variable name
            resp_var <- analysis_obj$metadata$parameters$response_var

            # Validate response variable
            if (is.null(resp_var) || resp_var == "") {
              warning(paste("Response variable is NULL or empty for setup:", setup_name))
              next
            }

            # The analysis_obj structure is:
            # - Top level: doe_analyze result + metadata we added
            # - We need to extract just the doe_analyze result parts

            # Create a clean doe_analyze result by removing our added metadata
            analysis_result <- analysis_obj
            analysis_result$metadata <- NULL # Remove our added metadata

            # CRITICAL: Ensure plots exists (validation requirement)
            # Old saved files might have plots = NULL, but validation requires it to exist
            if (is.null(analysis_result$plots)) {
              analysis_result$plots <- list()
              print(paste("WARNING: plots was NULL for", setup_name, "- set to empty list"))
            }

            # Debug: Check structure
            print(paste("Analysis result for", setup_name, "has elements:", paste(names(analysis_result), collapse = ", ")))
            print(paste("Class:", paste(class(analysis_result), collapse = ", ")))

            # Validate required elements for doe_desir
            required_elements <- c("models", "results", "plots", "statements", "call")
            missing_elements <- setdiff(required_elements, names(analysis_result))
            if (length(missing_elements) > 0) {
              warning(paste("Missing required elements for", setup_name, ":", paste(missing_elements, collapse = ", ")))
              next
            }

            mods[[resp_var]] <- analysis_result
          }

          desir_data$analysis_results <- mods
          print(paste("Loaded", length(mods), "analysis results"))
          print(paste("Response variables:", paste(names(mods), collapse = ", ")))

          # Validate that we have analyses
          if (length(mods) == 0) {
            stop(i18n$t("No valid analyses were loaded. Please check your saved analysis files."))
          }

          # STEP 2: Prepare desirability parameters
          incProgress(0.2, detail = i18n$t("Preparing parameters..."))

          responses <- names(mods)
          n_resp <- length(responses)

          print(paste("Number of responses for desirability:", n_resp))

          # Additional validation
          if (n_resp < 2) {
            stop(paste(i18n$t("Desirability function requires at least 2 responses. Only found:"), n_resp))
          }

          # Build frng
          if (input$use_default_ranges) {
            frng <- "default"
            print("Using default factor ranges")
          } else {
            # Get coded factors
            first_analysis <- desir_data$loaded_setups_data[[1]]
            data <- first_analysis$metadata$input_data
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

          # Build dsrng
          dsrng <- list()
          obj <- character(n_resp)
          wts <- list() # Initialize as list (one element per response)

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

              dsrng[[resp]] <- c(lower, target, upper)

              # Get weights with proper defaults
              weight1 <- input[[paste0("weight1_", i)]]
              weight2 <- input[[paste0("weight2_", i)]]

              # Ensure weights are not NULL
              if (is.null(weight1)) weight1 <- 1
              if (is.null(weight2)) weight2 <- 1

              # Both weights as a length-2 vector for target objective
              wts[[i]] <- c(weight1, weight2)
            } else {
              dsrng[[resp]] <- c(lower, upper)

              # For max/min: get single weight
              weight1 <- input[[paste0("weight1_", i)]]

              # Ensure weight is not NULL
              if (is.null(weight1)) weight1 <- 1

              # Single weight for max/min objective
              wts[[i]] <- weight1
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

          # cat("\n=== DEBUG: doe_desir parameters ===\n")

          # cat("mods:\n")
          # # print(names(mods))
          # cat("mods class:", class(mods), "\n")
          # cat("mods names:", names(mods), "\n\n")

          # cat("dsrng:\n")
          # print(str(dsrng))
          # cat("dsrng class:", class(dsrng), "\n")
          # cat("dsrng names:", names(dsrng), "\n\n")

          # cat("frng:\n")
          # print(str(frng))
          # cat("frng class:", class(frng), "\n")
          # if (is.list(frng)) cat("frng names:", names(frng), "\n")
          # cat("frng value:", frng, "\n\n")

          # cat("obj:\n")
          # print(obj)
          # cat("obj class:", class(obj), "\n")
          # cat("obj length:", length(obj), "\n\n")

          # cat("dtype:\n")
          # print(input$dtype)
          # cat("dtype class:", class(input$dtype), "\n\n")

          # cat("wts:\n")
          # print(wts)
          # cat("wts class:", class(wts), "\n")
          # cat("wts length:", length(wts), "\n\n")

          # cat("spts:\n")
          spts_val <- c(input$spts_random, input$spts_data)
          # print(spts_val)
          # cat("spts class:", class(spts_val), "\n")
          # cat("spts length:", length(spts_val), "\n\n")

          # cat("modbase:\n")
          # print(input$modbase)
          # cat("modbase class:", class(input$modbase), "\n\n")

          # cat("optmet:\n")
          # print(input$optmet)
          # cat("optmet class:", class(input$optmet), "\n\n")

          # cat("kmed:\n")
          # print(kmed_val)
          # cat("kmed class:", class(kmed_val), "\n\n")

          # cat("export: 'none'\n")
          # cat("silent: TRUE\n")
          # cat("=== END DEBUG ===\n\n")
          # browser()


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

      # Prepare pretty column names
      add_prettynames <- c(
        i18n_r()$t("Code Name"),
        i18n_r()$t("Name"),
        i18n_r()$t("Goal"),
        i18n_r()$t("Lower Limit"),
        i18n_r()$t("Upper Limit"),
        i18n_r()$t("Lower Limit (Uncoded)"),
        i18n_r()$t("Upper Limit (Uncoded)")
      )

      # Prepare translations for goal column
      goal_translations <- c(
        "in_range" = i18n_r()$t("In Range")
      )

      # Transform data
      dt_data <- desir_data$desir_result$factor_lims
      dt_data$goal <- my_mapvalues(dt_data$goal, names(goal_translations), goal_translations, warn_missing = FALSE)

      # Get numeric columns for formatting
      numeric_cols <- get_numeric_cols_for_formatting(dt_data)

      DT::datatable(
        dt_data,
        colnames = add_prettynames,
        options = list(
          dom = "t",
          pageLength = 20,
          language = tablang()
        ),
        rownames = FALSE,
        escape = FALSE
      ) %>% DT::formatRound(columns = numeric_cols, digits = 5)
    },
    server = FALSE
  )

  output$response_limits_table <- DT::renderDataTable(
    {
      req(desir_data$desir_result)

      # Transform data
      dt_data <- desir_data$desir_result$response_lims
      
      # Prepare translations for goal column
      goal_translations <- c(
        "max" = i18n_r()$t("Maximize"),
        "min" = i18n_r()$t("Minimize"),
        "trg" = i18n_r()$t("Target")
      )
      dt_data$goal <- my_mapvalues(dt_data$goal, names(goal_translations), goal_translations, warn_missing = FALSE)

      # Build pretty names dynamically based on actual columns
      col_name_map <- c(
        name = i18n_r()$t("Name"),
        goal = i18n_r()$t("Goal"),
        lower_lim = i18n_r()$t("Lower Limit"),
        target = i18n_r()$t("Target Value"),
        upper_lim = i18n_r()$t("Upper Limit"),
        lower_wt = i18n_r()$t("Lower Weight"),
        upper_wt = i18n_r()$t("Upper Weight")
      )
      add_prettynames <- unname(col_name_map[colnames(dt_data)])

      # Get numeric columns for formatting
      numeric_cols <- get_numeric_cols_for_formatting(dt_data)

      DT::datatable(
        dt_data,
        colnames = add_prettynames,
        options = list(
          dom = "t",
          pageLength = 20,
          language = tablang()
        ),
        rownames = FALSE,
        escape = FALSE
      ) %>% DT::formatRound(columns = numeric_cols, digits = 2)
    },
    server = FALSE
  )

  output$model_summaries_table <- DT::renderDataTable(
    {
      req(desir_data$desir_result)

      # Prepare pretty column names
      add_prettynames <- c(
        i18n_r()$t("Response"),
        i18n_r()$t("Model Type"),
        i18n_r()$t("Order"),
        i18n_r()$t("R²"),
        i18n_r()$t("Adjusted R²"),
        i18n_r()$t("Standard Error"),
        i18n_r()$t("F"),
        i18n_r()$t("Degrees of Freedom 1"),
        i18n_r()$t("Degrees of Freedom 2"),
        i18n_r()$t("Lack of Fit p-value"),
        i18n_r()$t("Model Equation")
      )

      # Prepare translations for model_type column
      model_type_translations <- c(
        "initial" = i18n_r()$t("Initial"),
        "final" = i18n_r()$t("Final")
      )

      # Transform data
      dt_data <- desir_data$desir_result$mod_sums
      dt_data$model_type <- my_mapvalues(dt_data$model_type, names(model_type_translations), model_type_translations, warn_missing = FALSE)

      # Store original equations for modal display
      original_equations <- dt_data$model_equation
      
      # Replace equation column with just a view button
      dt_data$model_equation <- sapply(seq_along(original_equations), function(i) {
        eq <- original_equations[i]
        # Create a button that triggers the modal
        paste0(
          '<button class="btn btn-xs btn-info view-equation-btn" ',
          'data-equation="', htmltools::htmlEscape(eq), '" ',
          'data-response="', htmltools::htmlEscape(dt_data$response[i]), '" ',
          'data-model-type="', htmltools::htmlEscape(dt_data$model_type[i]), '" ',
          'style="padding: 4px 10px;" ',
          'title="', i18n_r()$t("View full equation"), '">',
          '<i class="fa fa-eye"></i> ', i18n_r()$t("View"),
          '</button>'
        )
      })

      # Get numeric columns for formatting (excluding the equation column now)
      numeric_cols <- get_numeric_cols_for_formatting(dt_data[, !names(dt_data) %in% "model_equation"])

      DT::datatable(
        dt_data,
        extensions = "Buttons",
        colnames = add_prettynames,
        options = list(
          scrollX = TRUE,
          pageLength = 20,
          dom = "Bfrtip",
          language = tablang(),
          columnDefs = list(
            list(width = '100px', targets = ncol(dt_data) - 1, className = 'dt-center')  # Narrow centered column for button
          ),
          buttons = create_dt_export_buttons(i18n_r, "supercrit_doe_desir_model_summaries")
        ),
        rownames = FALSE,
        escape = FALSE  # Important: allow HTML in cells
      ) %>% DT::formatRound(columns = numeric_cols, digits = 5)
    },
    server = FALSE
  )
  
  # Observer for equation view button clicks (using JavaScript callback)
  observeEvent(input$equation_modal_data, {
    req(input$equation_modal_data)
    
    eq_data <- input$equation_modal_data
    raw_equation <- eq_data$equation
    response_name <- eq_data$response
    model_type <- eq_data$model_type
    
    # Convert to LaTeX
    latex_equation <- convert_equation_to_latex(raw_equation)
    
    showModal(modalDialog(
      title = paste0(i18n_r()$t("Model Equation"), " - ", response_name, " (", model_type, ")"),
      size = "l",
      easyClose = TRUE,
      
      # Mathematical notation section
      div(
        style = "margin-bottom: 20px;",
        tags$h5(i18n_r()$t("Mathematical Notation:"), style = "margin-bottom: 10px; color: #333;"),
        div(
          id = ns("latex_equation_display"),
          style = "background-color: #f8f9fa; padding: 20px; border-radius: 8px; text-align: center; font-size: 1.2em; overflow-x: auto;",
          # KaTeX will render here
          tags$span(class = "latex-equation", `data-equation` = latex_equation)
        )
      ),
      
      # Raw equation section
      div(
        tags$h5(i18n_r()$t("Raw Equation:"), style = "margin-bottom: 10px; color: #333;"),
        div(
          style = "background-color: #f0f0f0; padding: 15px; border-radius: 8px; font-family: 'Courier New', monospace; font-size: 0.9em; word-wrap: break-word; overflow-x: auto;",
          raw_equation
        )
      ),
      
      # JavaScript to render KaTeX
      tags$script(HTML(sprintf("
        setTimeout(function() {
          var latexEl = document.querySelector('#%s .latex-equation');
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
        }, 100);
      ", ns("latex_equation_display")))),
      
      footer = modalButton(i18n_r()$t("Close"))
    ))
  })

  # ============================================================================
  # 8. RENDER RESULTS - OPTIMIZATION TAB
  # ============================================================================

  # Helper: build grouped header DT container for desirability output tables
  build_desir_sketch <- function(col_names, c_facs, uc_facs, resps) {
    # Categorize columns
    str_coded <- paste0("str_", c_facs)
    str_uncoded <- paste0("str_", uc_facs)
    di_cols <- paste0("di_", resps)
    
    # Build display names, group assignments, and tooltips
    groups <- character(length(col_names))
    display <- character(length(col_names))
    tooltips <- character(length(col_names))
    
    for (j in seq_along(col_names)) {
      cn <- col_names[j]
      if (cn == "type") {
        groups[j] <- ""
        display[j] <- i18n_r()$t("Type")
        tooltips[j] <- i18n_r()$t("Starting point source: random or from design data")
      } else if (cn %in% str_coded) {
        groups[j] <- i18n_r()$t("Starting Points (Coded)")
        display[j] <- gsub("str_", "", cn)
        tooltips[j] <- paste0(i18n_r()$t("Starting point for coded factor"), " ", gsub("str_", "", cn))
      } else if (cn %in% str_uncoded) {
        groups[j] <- i18n_r()$t("Starting Points (Uncoded)")
        display[j] <- gsub("str_", "", cn)
        tooltips[j] <- paste0(i18n_r()$t("Starting point for uncoded factor"), " ", gsub("str_", "", cn))
      } else if (cn %in% c_facs) {
        groups[j] <- i18n_r()$t("Optimal Values (Coded)")
        display[j] <- cn
        tooltips[j] <- paste0(i18n_r()$t("Optimized coded factor value for"), " ", cn)
      } else if (cn %in% uc_facs) {
        groups[j] <- i18n_r()$t("Optimal Values (Uncoded)")
        display[j] <- cn
        tooltips[j] <- paste0(i18n_r()$t("Optimized uncoded factor value for"), " ", cn)
      } else if (cn %in% resps) {
        groups[j] <- i18n_r()$t("Predicted Responses")
        display[j] <- cn
        tooltips[j] <- paste0(i18n_r()$t("Predicted response value for"), " ", cn)
      } else if (cn %in% di_cols) {
        groups[j] <- i18n_r()$t("Individual Desirability")
        resp_name <- gsub("di_", "", cn)
        display[j] <- resp_name
        tooltips[j] <- paste0(i18n_r()$t("Individual desirability score for"), " ", resp_name, " (0\u20131)")
      } else if (cn == "DO") {
        groups[j] <- i18n_r()$t("Individual Desirability")
        display[j] <- "DO"
        tooltips[j] <- i18n_r()$t("Overall desirability (geometric mean of individual desirabilities)")
      } else if (cn == "Clustering") {
        groups[j] <- ""
        display[j] <- i18n_r()$t("Cluster")
        tooltips[j] <- i18n_r()$t("K-medoids cluster assignment")
      } else {
        groups[j] <- ""
        display[j] <- cn
        tooltips[j] <- cn
      }
    }
    
    # Build the two-row header
    unique_groups <- rle(groups)
    
    header_row1 <- tags$tr(
      lapply(seq_along(unique_groups$lengths), function(k) {
        grp <- unique_groups$values[k]
        span <- unique_groups$lengths[k]
        if (grp == "") {
          idx <- sum(unique_groups$lengths[seq_len(k - 1)]) + seq_len(span)
          lapply(idx, function(ii) tags$th(rowspan = 2, display[ii], title = tooltips[ii],
            style = "vertical-align: bottom; text-align: center; border-left: 2px solid #ccc; cursor: help;"))
        } else {
          list(tags$th(colspan = span, grp, style = "text-align: center; border-bottom: 2px solid #ddd; border-left: 2px solid #ccc;"))
        }
      })
    )
    
    # Second row: only grouped columns
    header_row2_items <- list()
    for (k in seq_along(unique_groups$lengths)) {
      grp <- unique_groups$values[k]
      span <- unique_groups$lengths[k]
      if (grp != "") {
        idx <- sum(unique_groups$lengths[seq_len(k - 1)]) + seq_len(span)
        for (ii in idx) {
          is_first_in_group <- (ii == idx[1])
          border_style <- if (is_first_in_group) "text-align: center; border-left: 2px solid #ccc; cursor: help;" else "text-align: center; cursor: help;"
          header_row2_items <- c(header_row2_items, list(tags$th(display[ii], title = tooltips[ii], style = border_style)))
        }
      }
    }
    header_row2 <- tags$tr(header_row2_items)
    
    htmltools::withTags(table(
      class = "display",
      thead(header_row1, header_row2)
    ))
    
    # Find group boundary column indices (0-based for JS)
    boundary_cols <- c()
    unique_groups <- rle(groups)
    pos <- 0
    for (k in seq_along(unique_groups$lengths)) {
      if (k > 1) boundary_cols <- c(boundary_cols, pos)
      pos <- pos + unique_groups$lengths[k]
    }
    
    list(
      sketch = htmltools::withTags(table(class = "display", thead(header_row1, header_row2))),
      boundary_cols = as.list(boundary_cols)
    )
  }

  # Helper: extract factor/response info from desir result
  get_desir_col_info <- function() {
    first_analysis <- desir_data$loaded_setups_data[[1]]
    data <- first_analysis$metadata$input_data
    c_facs <- grep("^[A-Z]$", names(data), value = TRUE)
    realnames <- tryCatch(first_analysis$models$final$realnames, error = function(e) NULL)
    uc_facs <- if (!is.null(realnames) && !any(is.na(realnames))) realnames else c_facs
    resps <- unique(sapply(desir_data$loaded_setups_data, function(x) x$metadata$parameters$response_var))
    list(c_facs = c_facs, uc_facs = uc_facs, resps = resps)
  }

  output$unique_solutions_table <- DT::renderDataTable(
    {
      req(desir_data$desir_result)

      my_data <- desir_data$desir_result$unique_solutions
      col_info <- get_desir_col_info()
      
      # Build grouped header
      sketch_info <- build_desir_sketch(colnames(my_data), col_info$c_facs, col_info$uc_facs, col_info$resps)

      # Identify column groups for smart rounding
      di_cols <- grep("^di_|^DO$", colnames(my_data), value = TRUE)
      coded_cols <- colnames(my_data)[colnames(my_data) %in% c(col_info$c_facs, paste0("str_", col_info$c_facs))]
      other_numeric <- setdiff(get_numeric_cols_for_formatting(my_data), c(di_cols, coded_cols))

      # Smart format coded factors: remove trailing zeros
      for (cc in coded_cols) {
        if (cc %in% colnames(my_data) && is.numeric(my_data[[cc]])) {
          my_data[[cc]] <- sapply(my_data[[cc]], function(v) if (is.na(v)) NA else sub("\\.$", "", sub("0+$", "", formatC(v, digits = 4, format = "f"))))
        }
      }

      dt <- DT::datatable(
        my_data,
        container = sketch_info$sketch,
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          columnDefs = list(
            list(className = "dt-center", targets = "_all"),
            list(className = "dt-group-border", targets = sketch_info$boundary_cols)
          ),
          initComplete = htmlwidgets::JS("function(settings, json) { $(this.api().table().node()).find('td.dt-group-border, th.dt-group-border').css('border-left', '2px solid #ccc'); }"),
          dom = "Bfrtip",
          language = tablang(),
          buttons = create_dt_export_buttons(i18n_r, "desirability_unique_solutions")
        ),
        extensions = "Buttons",
        rownames = FALSE
      )
      if (length(di_cols) > 0) dt <- dt %>% DT::formatRound(columns = di_cols, digits = 3)
      if (length(other_numeric) > 0) dt <- dt %>% DT::formatRound(columns = other_numeric, digits = 2)
      dt
    },
    server = FALSE
  )

  output$output_data_table <- DT::renderDataTable(
    {
      req(desir_data$desir_result)

      my_data <- desir_data$desir_result$output_data
      col_info <- get_desir_col_info()
      
      # Build grouped header
      sketch_info <- build_desir_sketch(colnames(my_data), col_info$c_facs, col_info$uc_facs, col_info$resps)

      # Identify column groups for smart rounding
      di_cols <- grep("^di_|^DO$", colnames(my_data), value = TRUE)
      coded_cols <- colnames(my_data)[colnames(my_data) %in% c(col_info$c_facs, paste0("str_", col_info$c_facs))]
      other_numeric <- setdiff(get_numeric_cols_for_formatting(my_data), c(di_cols, coded_cols))

      # Smart format coded factors: remove trailing zeros
      for (cc in coded_cols) {
        if (cc %in% colnames(my_data) && is.numeric(my_data[[cc]])) {
          my_data[[cc]] <- sapply(my_data[[cc]], function(v) if (is.na(v)) NA else sub("\\.$", "", sub("0+$", "", formatC(v, digits = 4, format = "f"))))
        }
      }

      dt <- DT::datatable(
        my_data,
        container = sketch_info$sketch,
        options = list(
          scrollX = TRUE,
          pageLength = 20,
          columnDefs = list(
            list(className = "dt-center", targets = "_all"),
            list(className = "dt-group-border", targets = sketch_info$boundary_cols)
          ),
          initComplete = htmlwidgets::JS("function(settings, json) { $(this.api().table().node()).find('td.dt-group-border, th.dt-group-border').css('border-left', '2px solid #ccc'); }"),
          dom = "Bfrtip",
          language = tablang(),
          buttons = create_dt_export_buttons(i18n_r, "desirability_output_data")
        ),
        extensions = "Buttons",
        rownames = FALSE
      )
      if (length(di_cols) > 0) dt <- dt %>% DT::formatRound(columns = di_cols, digits = 3)
      if (length(other_numeric) > 0) dt <- dt %>% DT::formatRound(columns = other_numeric, digits = 2)
      dt
    },
    server = FALSE
  )

  output$orig_data_table <- DT::renderDataTable(
    {
      req(desir_data$desir_result)

      my_data <- desir_data$desir_result$orig_data
      col_names <- colnames(my_data)
      
      # Categorize columns into two groups
      di_and_do <- grep("^di_|^DO$", col_names, value = TRUE)
      orig_cols <- setdiff(col_names, di_and_do)
      
      # Build display names and tooltips
      display <- sapply(col_names, function(cn) {
        if (grepl("^di_", cn)) gsub("di_", "", cn)
        else if (cn == "DO") "DO"
        else cn
      })
      
      tooltips <- sapply(col_names, function(cn) {
        if (grepl("^di_", cn)) paste0(i18n_r()$t("Individual desirability score for"), " ", gsub("di_", "", cn), " (0\u20131)")
        else if (cn == "DO") i18n_r()$t("Overall desirability (geometric mean of individual desirabilities)")
        else cn
      })
      
      # Build two-row header
      n_orig <- length(orig_cols)
      n_desir <- length(di_and_do)
      
      header_row1 <- tags$tr(
        tags$th(colspan = n_orig, i18n_r()$t("Original Data"),
          style = "text-align: center; border-bottom: 2px solid #ddd;"),
        tags$th(colspan = n_desir, i18n_r()$t("Desirabilities"),
          style = "text-align: center; border-bottom: 2px solid #ddd; border-left: 2px solid #ccc;")
      )
      
      header_row2 <- tags$tr(
        lapply(seq_along(col_names), function(j) {
          is_first_desir <- (col_names[j] == di_and_do[1])
          border_style <- if (is_first_desir) "text-align: center; border-left: 2px solid #ccc; cursor: help;" else "text-align: center; cursor: help;"
          tags$th(display[j], title = tooltips[j], style = border_style)
        })
      )
      
      sketch <- htmltools::withTags(table(
        class = "display",
        thead(header_row1, header_row2)
      ))
      
      # Boundary column (first desirability column, 0-based)
      boundary_idx <- which(col_names == di_and_do[1]) - 1
      
      # Identify column groups for rounding
      other_numeric <- setdiff(get_numeric_cols_for_formatting(my_data), di_and_do)

      dt <- DT::datatable(
        my_data,
        container = sketch,
        options = list(
          scrollX = TRUE,
          pageLength = 20,
          columnDefs = list(
            list(className = "dt-center", targets = "_all"),
            list(className = "dt-group-border", targets = list(boundary_idx))
          ),
          initComplete = htmlwidgets::JS("function(settings, json) { $(this.api().table().node()).find('td.dt-group-border, th.dt-group-border').css('border-left', '2px solid #ccc'); }"),
          dom = "Bfrtip",
          language = tablang(),
          buttons = create_dt_export_buttons(i18n_r, "desirability_original_data")
        ),
        extensions = "Buttons",
        rownames = FALSE
      )
      if (length(di_and_do) > 0) dt <- dt %>% DT::formatRound(columns = di_and_do, digits = 3)
      if (length(other_numeric) > 0) dt <- dt %>% DT::formatRound(columns = other_numeric, digits = 2)
      dt
    },
    server = FALSE
  )

  # ============================================================================
  # 9. DOWNLOAD HANDLERS
  # ============================================================================

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

      temp_dir <- file.path(tempdir(), paste0("desir_export_", format(Sys.time(), "%d-%m-%Y_%Hhrs-%Mmin-%Ssec")))
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

    # Reset checkboxGroupButtons (use correct function for shinyWidgets)
    updateCheckboxGroupButtons(
      session,
      "selected_analyses",
      selected = character(0),
      disabledChoices = character(0)
    )

    # Re-enable refresh button if it was disabled
    shinyjs::enable("refresh_analyses")

    # Reset Global Parameters to defaults
    updateSelectInput(session, "dtype", selected = defaults$dtype %||% "coded")
    updateSelectInput(session, "modbase", selected = defaults$modbase %||% "final")
    updateSelectInput(session, "optmet", selected = defaults$optmet %||% "nlopt")
    updateSelectInput(session, "kmed", selected = defaults$kmed %||% "NA")
    updateNumericInput(session, "spts_random", value = defaults$spts_random)
    updateNumericInput(session, "spts_data", value = defaults$spts_data)

    # Reset Factor Range Settings
    updateCheckboxInput(session, "use_default_ranges", value = defaults$use_default_ranges)

    showNotification(i18n$t("Reset complete"), type = "message")
  })

  # ============================================================================
  # 11. HELP BUTTON OBSERVERS
  # ============================================================================
  # Ensure UI outputs in collapsed accordions are rendered immediately

  # suspendWhenHidden for renderUI outputs (auto-added)
  outputOptions(output, "accordion_toggle_btn", suspendWhenHidden = FALSE)
  outputOptions(output, "analysis_selection_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "doe_desir_HELP", suspendWhenHidden = FALSE)
  outputOptions(output, "selected_analyses_preview", suspendWhenHidden = FALSE)
  outputOptions(output, "spts_data_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "spts_random_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "use_default_ranges_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "desirability_settings_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "factor_range_ui", suspendWhenHidden = FALSE)


}
