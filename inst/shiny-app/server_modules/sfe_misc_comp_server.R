library(shinyjs)

miscomp_server <- function(input, output, session, defaults, i18n, tablang, sfe_rv) {






  ns <- session$ns
  i18n_r <- reactive({
    i18n
  })
  rv <- reactiveValues(
    miscomp_results = NULL,
    selected_saved_calc_ids = character(0), # IDs of selected saved calculations
    current_blend = list() # Current blend being built (solvents with fractions)
  )

  # Modifier values (programmatic names) and display keys (human-readable for translation)
  modifier_values <- c("Acetone", "Benzene", "Cyclohexane", "DiethylEther",
    "Ethanol", "Heptane", "Hexane", "Methanol", "MethylOleate",
    "Toluene", "PXylene", "OXylene", "Water")
  modifier_display_keys <- c("Acetone", "Benzene", "Cyclohexane", "Diethyl Ether",
    "Ethanol", "Heptane", "Hexane", "Methanol", "Methyl Oleate",
    "Toluene", "p-Xylene", "o-Xylene", "Water")

  # Helper: translate a single solvent token (full name or abbreviation)
  translate_solvent_token <- function(token, i18n_fn) {
    token <- trimws(token)
    key_map <- setNames(modifier_display_keys, modifier_values)
    if (token %in% names(key_map)) return(i18n_fn$t(key_map[[token]]))
    if (token == "CO2") return("CO2")
    return(token)
  }

  # Local wrapper for range_badge_input that uses module's ns and i18n_r
  # Calls the global version from utils/general_helpers.R with include_minmax = TRUE
  local_range_badge_input <- function(input_id, label_text, value, min_val, max_val,
                                      step = NA, range_text = NULL, tooltip = NULL,
                                      help_content = NULL, help_title = NULL) {
    range_badge_input(ns, i18n_r, input_id, label_text, value, min_val, max_val,
                      step = step, range_text = range_text, tooltip = tooltip,
                      help_content = help_content, help_title = help_title,
                      include_minmax = TRUE)
  }

  # --- Process Conditions renderUI outputs ---
  output$pres_input_ui <- renderUI({
    # Parse defaults (comma-separated string to numeric vector)
    default_pres <- as.numeric(strsplit(defaults$pres_input, ",")[[1]])
    default_pres <- default_pres[!is.na(default_pres)]
    # Keep current selection if available
    current_val <- if (!is.null(input$pres_input)) input$pres_input else as.character(default_pres)
    
    tagList(
      tags$label(
        i18n_r()$t("Pressures (bar)"),
        input_help(i18n_r()$t("Select one or more pressures at which to compare co-solvent miscibility. The comparison is evaluated at each pressure–temperature combination."),
                   title = i18n_r()$t("Pressures"), buttonLabel = i18n_r()$t("OK")),
        class = "control-label",
        style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
        tags$span(
          paste0("75\u20131000 ", i18n_r()$t("bar"), ", ", i18n_r()$t("max"), " 6"),
          style = "font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: #6c757d; color: white; margin-left: auto; font-weight: normal;",
          title = paste0("75 \u2013 1000 ", i18n_r()$t("bar"), ", ", i18n_r()$t("max"), " 6 ", i18n_r()$t("values"))
        )
      ),
      selectizeInput(
        ns("pres_input"),
        label = NULL,
        choices = current_val,
        selected = current_val,
        multiple = TRUE,
        options = list(
          create = TRUE,
          persist = FALSE,
          maxItems = 6,
          placeholder = i18n_r()$t("Type pressure values..."),
          render = I("{
            item: function(item, escape) {
              var val = parseFloat(item.value);
              var isOutOfRange = isNaN(val) || val < 75 || val > 1000;
              var style = isOutOfRange ? 'background-color: #dc3545; border-color: #dc3545;' : '';
              return '<div class=\"item\" style=\"' + style + '\">' + escape(item.value) + '</div>';
            }
          }")
        )
      )
    )
  })
  outputOptions(output, "pres_input_ui", suspendWhenHidden = FALSE)

  output$temp_ui <- renderUI({
    cur <- if (is.null(input$temp)) defaults$temp else input$temp
    local_range_badge_input("temp", i18n_r()$t("Temperature (\u00B0C)"),
      value = cur, min_val = 31, max_val = 200,
      tooltip = "31 \u2013 200 \u00B0C",
      help_content = i18n_r()$t("Extraction temperature for the comparison. Must be above 31\u00B0C (the critical temperature of CO2)."),
      help_title = i18n_r()$t("Temperature"))
  })
  outputOptions(output, "temp_ui", suspendWhenHidden = FALSE)

  output$vfrac_input_ui <- renderUI({
    # Parse defaults (comma-separated string to numeric vector)
    default_vfrac <- as.numeric(strsplit(defaults$vfrac_input, ",")[[1]])
    default_vfrac <- default_vfrac[!is.na(default_vfrac)]
    # Keep current selection if available
    current_val <- if (!is.null(input$vfrac_input)) input$vfrac_input else as.character(default_vfrac)
    
    tagList(
      tags$label(
        i18n_r()$t("Volume Fractions"),
        input_help(i18n_r()$t("Select one or more co-solvent volume fractions (%) at which to compare miscibility. Each fraction is evaluated separately."),
                   title = i18n_r()$t("Volume Fractions"), buttonLabel = i18n_r()$t("OK")),
        class = "control-label",
        style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
        tags$span(
          "0.01\u20130.99",
          style = "font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: #6c757d; color: white; margin-left: auto; font-weight: normal;",
          title = i18n_r()$t("0.01 - 0.99 (1% - 99%)")
        )
      ),
      selectizeInput(
        ns("vfrac_input"),
        label = NULL,
        choices = current_val,
        selected = current_val,
        multiple = TRUE,
        options = list(
          create = TRUE,
          persist = FALSE,
          placeholder = i18n_r()$t("Type volume fractions..."),
          render = I("{
            item: function(item, escape) {
              var val = parseFloat(item.value);
              var isOutOfRange = isNaN(val) || val < 0.01 || val > 0.99;
              var style = isOutOfRange ? 'background-color: #dc3545; border-color: #dc3545;' : '';
              return '<div class=\"item\" style=\"' + style + '\">' + escape(item.value) + '</div>';
            }
          }")
        )
      )
    )
  })
  outputOptions(output, "vfrac_input_ui", suspendWhenHidden = FALSE)

  # Rendered radioButtons with translated modifier type choices
  output$modifier_type_ui <- renderUI({
    radioButtons(
      inputId = ns("modifier_type"),
      label = tags$span(i18n_r()$t("Modifier Type"),
        input_help(i18n_r()$t("Choose between a single pure co-solvent or a custom blend of multiple solvents at specified volume ratios."),
                   title = i18n_r()$t("Modifier Type"), buttonLabel = i18n_r()$t("OK"))),
      choices = setNames(
        c("pure", "blend"),
        c(i18n_r()$t("Pure Modifier"), i18n_r()$t("Custom Blend"))
      ),
      selected = if (!is.null(input$modifier_type)) input$modifier_type else defaults$modifier_type,
      inline = TRUE
    )
  })
  outputOptions(output, "modifier_type_ui", suspendWhenHidden = FALSE)

  # Rendered pickerInput with translated modifier names for pure modifier selection
  output$modifier_selection_ui <- renderUI({
    translated_names <- sapply(modifier_display_keys, function(k) i18n_r()$t(k))
    choices <- setNames(modifier_values, translated_names)
    pickerInput(
      inputId = ns("modifier_selection"),
      label = tags$span(i18n_r()$t("Select Modifier"),
        input_help(i18n_r()$t("Choose the co-solvent to evaluate. The HSP distance and miscibility enhancement will be compared against pure CO2 for each selected solute."),
                   title = i18n_r()$t("Modifier"), buttonLabel = i18n_r()$t("OK"))),
      choices = choices,
      selected = defaults$modifier_selection,
      multiple = FALSE
    )
  })
  outputOptions(output, "modifier_selection_ui", suspendWhenHidden = FALSE)

  # Rendered Custom Blend box with all translated labels
  output$custom_blend_box_ui <- renderUI({
    box(
      title = i18n_r()$t("Custom Blend"),
      status = "primary",
      collapsible = FALSE,
      solidHeader = TRUE,
      width = NULL,
      actionButton(ns("sfe_misc_comp_blend_intro"), "",
        icon = icon("info-circle"),
        class = "btn-light btn-xs", style = "float: right; margin-top:-2px;"
      ),

      # Blend Builder
      h5(i18n_r()$t("Build a Blend")),
      div(id = ns("blend_solvent_selector_wrapper"), uiOutput(ns("blend_solvent_selector_ui"))),
      div(
        style = "display: flex; gap: 6px;",
        div(style = "flex: 1;", actionButton(ns("add_to_blend"), i18n_r()$t("Add to Blend"), class = "btn btn-primary btn-block", style = "color: white;")),
        div(style = "flex: 1;", actionButton(ns("equalize_blend"), i18n_r()$t("Equalize"), class = "btn btn-success btn-block", style = "color: white;", disabled = "disabled"))
      ),
      hr(),

      # Current Blend Being Built
      h5(i18n_r()$t("Current Blend Composition:")),
      uiOutput(ns("current_blend_ui")), # Dynamic UI for fraction inputs

      fluidRow(
        column(
          12,
          div(id = ns("clear_blend_wrapper"), actionButton(ns("clear_blend"), i18n_r()$t("Clear Blend"), class = "btn btn-primary btn-block", style = "color: white;"))
        )
      )
    )
  })
  outputOptions(output, "custom_blend_box_ui", suspendWhenHidden = FALSE)

  # Reactive output to check if results are available
  output$has_results <- reactive({
    !is.null(rv$miscomp_results)
  })
  outputOptions(output, "has_results", suspendWhenHidden = FALSE)

  # Observer to enable/disable result tabs based on results availability
  observe({
    has_results <- !is.null(rv$miscomp_results)
    if (has_results) {
      session$sendCustomMessage("enableTabs", list(tabsetId = ns("results_tabs")))
    } else {
      session$sendCustomMessage("disableTabs", list(tabsetId = ns("results_tabs"), keepFirst = TRUE))
    }
  })

  # --- Solute Selection Logic ---

  # Load Example Data button handler
  observeEvent(input$load_example_data, {
    seed_data <- sfe_rv$seed_calculations
    if (length(seed_data) == 0) {
      showNotification(i18n$t("No example data available."), type = "warning")
      return()
    }
    
    # Check if seed data is already loaded (avoid duplicates)
    existing_names <- sapply(sfe_rv$saved_calculations, function(x) x$display_name)
    seed_names <- sapply(seed_data, function(x) x$display_name)
    
    new_seeds <- seed_data[!seed_names %in% existing_names]
    
    if (length(new_seeds) == 0) {
      showNotification(i18n$t("Example data already loaded."), type = "message")
      return()
    }
    
    # Append seed data to saved_calculations
    current_len <- length(sfe_rv$saved_calculations)
    for (i in seq_along(new_seeds)) {
      new_seeds[[i]]$id <- current_len + i
      sfe_rv$saved_calculations[[current_len + i]] <- new_seeds[[i]]
    }
    
    showNotification(
      sprintf(i18n$t("Loaded %d example calculation(s)."), length(new_seeds)), 
      type = "message"
    )
  })

  # Load from Disk button handler
  observeEvent(input$load_from_disk, {
    showModal(modalDialog(
      title = i18n$t("Load Calculation from Disk"),
      size = "m",
      fileInput(
        ns("load_file_input"),
        i18n$t("Select JSON file(s)"),
        multiple = TRUE,
        accept = c(".json", "application/json"),
        buttonLabel = i18n$t("Browse..."),
        placeholder = i18n$t("No file selected")
      ),
      tags$small(
        class = "text-muted",
        i18n$t("Select one or more .json files exported from Solute Characterization.")
      ),
      footer = tagList(
        modalButton(i18n$t("Cancel")),
        actionButton(ns("confirm_load_from_disk"), i18n$t("Load"), class = "btn btn-primary", style = "color: white;")
      )
    ))
  })
  
  # Confirm load from disk
  observeEvent(input$confirm_load_from_disk, {
    req(input$load_file_input)
    
    removeModal()
    
    files <- input$load_file_input
    loaded_count <- 0
    
    for (i in seq_len(nrow(files))) {
      tryCatch({
        json_data <- jsonlite::fromJSON(files$datapath[i], simplifyVector = FALSE)
        
        # Validate structure
        if (is.null(json_data$parameters) || is.null(json_data$display_name)) {
          showNotification(
            sprintf(i18n$t("Invalid file format: %s"), files$name[i]),
            type = "warning"
          )
          next
        }
        
        # Check for duplicates
        existing_names <- sapply(sfe_rv$saved_calculations, function(x) x$display_name)
        if (json_data$display_name %in% existing_names) {
          showNotification(
            sprintf(i18n$t("Calculation '%s' already loaded."), json_data$display_name),
            type = "warning"
          )
          next
        }
        
        # Add to saved_calculations
        current_len <- length(sfe_rv$saved_calculations)
        new_id <- current_len + 1
        
        sfe_rv$saved_calculations[[new_id]] <- list(
          id = new_id,
          display_name = json_data$display_name,
          timestamp = json_data$timestamp %||% format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          source_module = json_data$source_module %||% "Loaded from Disk",
          parameters = json_data$parameters
        )
        
        loaded_count <- loaded_count + 1
        
      }, error = function(e) {
        showNotification(
          sprintf(i18n$t("Error loading %s: %s"), files$name[i], e$message),
          type = "error"
        )
      })
    }
    
    if (loaded_count > 0) {
      showNotification(
        sprintf(i18n$t("Loaded %d calculation(s) from disk."), loaded_count),
        type = "message"
      )
    }
  })

  # Render the list of saved calculations as checkboxes (limited to first 6)
  output$saved_calculations_list_ui <- renderUI({
    # Create reactive dependency on saved_calculations
    calcs <- sfe_rv$saved_calculations

    if (length(calcs) == 0) {
      return(tagList(
        p(i18n$t("No saved GCM calculations found.")),
        p(style = "font-size: 0.9em; color: #888; margin-top: 10px;", 
          sprintf(i18n$t("Calculations from %s will appear here."), i18n$t("Solute Characterization")))
      ))
    }

    # Show all calculations (scrollable if many)
    total_calcs <- length(calcs)

    # Create choices from saved calculations (already sorted newest first)
    choices <- setNames(
      seq_along(calcs),
      sapply(calcs, function(x) x$display_name)
    )

    has_selection <- length(rv$selected_saved_calc_ids) > 0

    delete_btn <- actionButton(
      ns("delete_selected_calcs"),
      label = i18n$t("Remove Selected from Session"),
      icon = icon("trash-alt"),
      class = "btn-xs btn-danger",
      style = "color: white;"
    )
    if (!has_selection) {
      delete_btn <- shinyjs::disabled(delete_btn)
    }

    tagList(
      div(
        style = if (total_calcs > 6) "margin-bottom: 10px; max-height: 300px; overflow-y: auto;" else "margin-bottom: 10px;",
        checkboxGroupInput(
          ns("saved_calculation_checkboxes"),
          label = i18n$t("Select saved calculations"),
          choices = choices,
          selected = rv$selected_saved_calc_ids
        )
      ),
      div(
        style = "margin-top: -5px;",
        delete_btn
      ),
      p(style = "font-size: 0.9em; color: #888; margin-top: 10px;", 
        sprintf(i18n$t("Showing %d calculation(s) from %s."), total_calcs, i18n$t("Solute Characterization"))
      )
    )
  })

  # Update selected_saved_calc_ids when checkboxes change
  observeEvent(input$saved_calculation_checkboxes, {
    rv$selected_saved_calc_ids <- input$saved_calculation_checkboxes
  }, ignoreNULL = FALSE)

  # Observer for delete button
  observeEvent(input$delete_selected_calcs, {
    req(input$saved_calculation_checkboxes)
    req(length(input$saved_calculation_checkboxes) > 0)
    
    # Get names of selected calculations
    selected_indices <- as.numeric(input$saved_calculation_checkboxes)
    selected_names <- sapply(selected_indices, function(idx) {
      if (idx <= length(sfe_rv$saved_calculations)) {
        sfe_rv$saved_calculations[[idx]]$display_name
      } else {
        NA
      }
    })
    selected_names <- na.omit(selected_names)
    
    # Show confirmation dialog
    showModal(modalDialog(
      title = i18n$t("Confirm Deletion"),
      p(i18n$t("Are you sure you want to delete the selected calculation(s)?")),
      tags$ul(lapply(selected_names, function(n) tags$li(tags$strong(n)))),
      footer = tagList(
        modalButton(i18n$t("Cancel")),
        actionButton(ns("confirm_delete_calcs"), i18n$t("Delete"),
          class = "btn-danger", style = "color: white;")
      )
    ))
  })

  # Observer for confirm delete
  observeEvent(input$confirm_delete_calcs, {
    removeModal()
    
    req(input$saved_calculation_checkboxes)
    selected_indices <- sort(as.numeric(input$saved_calculation_checkboxes), decreasing = TRUE)
    
    deleted_count <- 0
    for (idx in selected_indices) {
      if (idx <= length(sfe_rv$saved_calculations)) {
        sfe_rv$saved_calculations[[idx]] <- NULL
        deleted_count <- deleted_count + 1
      }
    }
    
    # Clean up NULLs
    sfe_rv$saved_calculations <- Filter(Negate(is.null), sfe_rv$saved_calculations)
    
    # Reset selection
    rv$selected_saved_calc_ids <- character(0)
    
    showNotification(
      paste0(i18n$t("Deleted"), " ", deleted_count, " ", i18n$t("calculation(s)")),
      type = "warning",
      duration = 3
    )
  })

  # Render selected solutes from saved calculations
  output$selected_solutes_ui <- renderUI({
    if (length(rv$selected_saved_calc_ids) == 0) {
      return(p(i18n$t("No solutes selected.")))
    }

    items <- lapply(rv$selected_saved_calc_ids, function(id) {
      # Get calculation from session storage
      calc <- if (as.numeric(id) <= length(sfe_rv$saved_calculations)) {
        sfe_rv$saved_calculations[[as.numeric(id)]]
      } else {
        NULL
      }

      if (!is.null(calc)) {
        # Build display string conditionally based on SMILES availability
        smiles <- calc$parameters$smiles_input
        has_smiles <- !is.null(smiles) && nchar(trimws(smiles)) > 0
        
        if (has_smiles) {
          display_text <- tagList(
            strong(calc$parameters$name_input), 
            " (", calc$parameters$cas_input, ") - ", smiles
          )
        } else {
          display_text <- tagList(
            strong(calc$parameters$name_input), 
            " (", calc$parameters$cas_input, ")"
          )
        }
        
        fluidRow(
          column(12, p(display_text))
        )
      }
    })

    tagList(items)
  })

  # Render blend solvent selector with reactive translations
  output$blend_solvent_selector_ui <- renderUI({
    translated_names <- sapply(modifier_display_keys, function(k) i18n_r()$t(k))
    choices <- setNames(modifier_values, translated_names)
    selectInput(
      ns("blend_solvent_selector"),
      tags$span(i18n_r()$t("Select Solvent to Add"),
        input_help(i18n_r()$t("Choose a solvent to add to the custom blend. Multiple solvents can be combined with specified volume fractions."),
                   title = i18n_r()$t("Blend Solvent"), buttonLabel = i18n_r()$t("OK"))),
      choices = choices,
      selected = NULL
    )
  })
  outputOptions(output, "blend_solvent_selector_ui", suspendWhenHidden = FALSE)

  # Get selected solutes from saved calculations
  combined_solutes <- reactive({
    sols_list <- list()

    if (length(rv$selected_saved_calc_ids) > 0) {
      for (id in rv$selected_saved_calc_ids) {
        # Get calculation from session storage
        calc <- if (as.numeric(id) <= length(sfe_rv$saved_calculations)) {
          sfe_rv$saved_calculations[[as.numeric(id)]]
        } else {
          NULL
        }

        if (!is.null(calc)) {
          # Always create a 3-element NAMED vector: [SMILES, CAS, Name]
          sols_list[[length(sols_list) + 1]] <- c(
            SMILES = if (!is.null(calc$parameters$smiles_input)) calc$parameters$smiles_input else "",
            CAS = if (!is.null(calc$parameters$cas_input)) calc$parameters$cas_input else "",
            Name = if (!is.null(calc$parameters$name_input)) calc$parameters$name_input else ""
          )
        }
      }
    }
    sols_list
  })

  # --- Blend Management Logic (Copied from sfe_mod) ---
  # Global list to keep track of fraction observers for cleanup
  fraction_observers <- list()

  # 1. Add solvent to current blend
  observeEvent(input$add_to_blend, {
    req(input$blend_solvent_selector)
    solvent <- input$blend_solvent_selector

    # Check if already added
    if (solvent %in% names(rv$current_blend)) {
      showNotification(i18n$t("Solvent already in blend!"), type = "warning")
      return()
    }

    # Add with default fraction 0
    rv$current_blend[[solvent]] <- 0

    # Ensure the input for the newly added solvent is reset
    session$sendCustomMessage("resetInput", list(id = ns(paste0("frac_", solvent)), value = 0))
  })

  # Enable/disable Equalize button based on blend size
  observe({
    n <- length(rv$current_blend)
    shinyjs::toggleState("equalize_blend", condition = n >= 2)
  })

  # Equalize blend fractions
  observeEvent(input$equalize_blend, {
    solvents <- names(rv$current_blend)
    req(length(solvents) >= 2)
    equal_frac <- round(100 / length(solvents), 1)
    for (s in solvents) {
      rv$current_blend[[s]] <- equal_frac
      updateNumericInput(session, paste0("frac_", s), value = equal_frac)
    }
  })

  # 2. Render current blend UI (dynamic fraction inputs)
  output$current_blend_ui <- renderUI({
    if (length(rv$current_blend) == 0) {
      return(p(i18n$t("No solvents added yet.")))
    }

    # Create fraction inputs for each solvent (flexbox layout)
    inputs <- lapply(names(rv$current_blend), function(solvent) {
      translated_name <- translate_solvent_token(solvent, i18n_r())
      div(
        style = "display: flex; align-items: center; gap: 8px; margin-bottom: 5px;",
        tags$span(
          style = "font-weight: bold; min-width: 100px;",
          translated_name
        ),
        div(
          style = "flex: 1;",
          numericInput(ns(paste0("frac_", solvent)), NULL,
            value = rv$current_blend[[solvent]],
            min = 1, max = 100, step = 0.1, width = "100%"
          ) |> tagAppendAttributes(style = "margin-bottom: 0;")
        ),
        actionButton(ns(paste0("remove_", solvent)), label = NULL,
          icon = icon("times"),
          class = "btn btn-default btn-sm",
          style = "flex-shrink: 0;"
        )
      )
    })

    tagList(
      inputs,
      hr(),
      uiOutput(ns("blend_total_display"))
    )
  })
  outputOptions(output, "current_blend_ui", suspendWhenHidden = FALSE)

  # Separate reactive for total display that updates when inputs change
  output$blend_total_display <- renderUI({
    # Read current solvent names from rv$current_blend
    current_solvents <- names(rv$current_blend)
    
    if (length(current_solvents) == 0) {
      return(NULL)
    }
    
    # Calculate total from current input values
    total <- sum(sapply(current_solvents, function(s) {
      val <- input[[paste0("frac_", s)]]
      if (is.null(val) || is.na(val)) 0 else as.numeric(val)
    }), na.rm = TRUE)
    
    total_bg <- if (total == 100) "#28a745" else "#dc3545"
    
    p(
      strong(i18n$t("Total:")), " ",
      span(
        paste0(round(total, 1), "%"),
        style = paste0(
          "background-color:", total_bg, "; color: white; font-weight: bold; ",
          "padding: 2px 8px; border-radius: 4px;"
        )
      )
    )
  })
  outputOptions(output, "blend_total_display", suspendWhenHidden = FALSE)

  # 3. Update fractions when changed (with observer cleanup)
  observe({
    current_solvents <- names(rv$current_blend)

    # Clean up old observers for solvents that are no longer in the blend
    old_solvents <- setdiff(names(fraction_observers), current_solvents)
    for (old_s in old_solvents) {
      if (!is.null(fraction_observers[[old_s]])) {
        fraction_observers[[old_s]]$destroy() # Destroy the observer
        fraction_observers[[old_s]] <- NULL
      }
    }

    # Create or update observers for current solvents
    for (solvent in current_solvents) {
      if (is.null(fraction_observers[[solvent]])) { # Only create if it doesn't exist
        local({
          s <- solvent
          fraction_observers[[s]] <<- observeEvent(input[[paste0("frac_", s)]],
            {
              val <- input[[paste0("frac_", s)]]
              if (!is.null(val) && !is.na(val)) {
                # Update rv$current_blend directly from input value
                rv$current_blend[[s]] <- as.numeric(val)
              }
            },
            ignoreInit = TRUE,
            ignoreNULL = TRUE
          )
        })
      }
    }
  })

  # 4. Clear current blend
  observeEvent(input$clear_blend, {
    old_solvents <- names(rv$current_blend)

    # Clear reactive value
    rv$current_blend <- list()

    # Clear associated numeric inputs in UI
    for (s in old_solvents) {
      updateNumericInput(session, paste0("frac_", s), value = NULL)
      session$sendCustomMessage("resetInput", list(id = ns(paste0("frac_", s)), value = 0))
    }
  })

  # 5. Observe for removing solvents from current blend
  observe({
    req(length(rv$current_blend) > 0)
    for (solvent in names(rv$current_blend)) {
      local({
        s <- solvent
        observeEvent(input[[paste0("remove_", s)]],
          {
            # Clear the input value in the UI
            updateNumericInput(session, paste0("frac_", s), value = NULL)
            session$sendCustomMessage("resetInput", list(id = ns(paste0("frac_", s)), value = 0))

            # Remove from reactive value
            rv$current_blend[[s]] <- NULL
          },
          ignoreInit = TRUE,
          ignoreNULL = TRUE
        )
      })
    }
  })

  # --- Main miscomp execution logic ---
  observeEvent(input$run_miscomp, {
    # 1. Validate minimum solutes
    if (length(combined_solutes()) < 2) {
      showNotification(i18n$t("Please select at least 2 solutes from saved calculations."), type = "error")
      return(NULL)
    }

    # 2. Get GCM parameters from selected calculations
    # Geçici kontrol - kütüphane hazır olunca TRUE
    allow_multi_gcm <- TRUE

    if (allow_multi_gcm) {
      # Her solute için ayrı parametreler (vektör)
      tb_param <- sapply(rv$selected_saved_calc_ids, function(id) {
        calc <- sfe_rv$saved_calculations[[as.numeric(id)]]
        calc$parameters$gcm_tb
      })
      crit_param <- sapply(rv$selected_saved_calc_ids, function(id) {
        calc <- sfe_rv$saved_calculations[[as.numeric(id)]]
        calc$parameters$gcm_crit
      })
      hsp_param <- sapply(rv$selected_saved_calc_ids, function(id) {
        calc <- sfe_rv$saved_calculations[[as.numeric(id)]]
        calc$parameters$gcm_hsp
      })
      simplicity_param <- sapply(rv$selected_saved_calc_ids, function(id) { # YENİ!
        calc <- sfe_rv$saved_calculations[[as.numeric(id)]]
        calc$parameters$gcm_simplicity
      })
    } else {
      # Mevcut: ilk hesaplamadan tek değer
      first_calc_id <- rv$selected_saved_calc_ids[1]
      first_calc <- if (as.numeric(first_calc_id) <= length(sfe_rv$saved_calculations)) {
        sfe_rv$saved_calculations[[as.numeric(first_calc_id)]]
      } else {
        NULL
      }
      if (is.null(first_calc)) {
        showNotification(i18n$t("Error: Could not load GCM parameters from selected calculation."), type = "error")
        return(NULL)
      }
      tb_param <- first_calc$parameters$gcm_tb
      crit_param <- first_calc$parameters$gcm_crit
      hsp_param <- first_calc$parameters$gcm_hsp
      simplicity_param <- first_calc$parameters$gcm_simplicity
    }



    # 3. Validate GCM methods
    if (all(tb_param == "none") && all(crit_param == "none") && all(hsp_param == "none")) {
      showNotification(i18n$t("Selected calculation does not have valid GCM methods."), type = "error")
      return(NULL)
    }

    # 4. Validate temperature range
    if (is.null(input$temp) || is.na(input$temp) || input$temp < 31 || input$temp > 200) {
      showNotification(
        paste(i18n$t("Out of range:"), i18n$t("Temperature (\u00B0C)"), "(31 \u2013 200)"),
        type = "error"
      )
      return(NULL)
    }

    # 5. Parse and validate pressure inputs (selectizeInput returns character vector)
    pres_values <- as.numeric(input$pres_input)
    pres_values <- pres_values[!is.na(pres_values)]
    if (length(pres_values) == 0) {
      showNotification(i18n$t("Please enter valid pressure values."), type = "error")
      return(NULL)
    }
    if (length(pres_values) > 6) {
      showNotification(i18n$t("Maximum 6 pressure values are allowed."), type = "error")
      return(NULL)
    }
    # Range check for pressures
    pres_out_of_range <- pres_values[pres_values < 75 | pres_values > 1000]
    if (length(pres_out_of_range) > 0) {
      showNotification(
        paste(i18n$t("Pressure values out of range (75-1000 bar):"), paste(pres_out_of_range, collapse = ", ")),
        type = "error"
      )
      return(NULL)
    }

    # 6. Parse and validate vfrac inputs (selectizeInput returns character vector)
    vfrac_values <- as.numeric(input$vfrac_input)
    vfrac_values <- vfrac_values[!is.na(vfrac_values)]
    if (length(vfrac_values) == 0) {
      showNotification(i18n$t("Please enter valid volume fraction values."), type = "error")
      return(NULL)
    }
    # Range check for volume fractions
    vfrac_out_of_range <- vfrac_values[vfrac_values < 0.01 | vfrac_values > 0.99]
    if (length(vfrac_out_of_range) > 0) {
      showNotification(
        paste(i18n$t("Volume fraction values out of range (0.01-0.99):"), paste(vfrac_out_of_range, collapse = ", ")),
        type = "error"
      )
      return(NULL)
    }

    # 7. Prepare modifier and modfracs based on modifier_type selection
    modifier_param <- NULL
    modfracs_param <- NA

    if (input$modifier_type == "pure") {
      # Use pure modifier selection
      modifier_param <- input$modifier_selection
      if (is.null(modifier_param) || modifier_param == "") {
        showNotification(i18n$t("Please select a co-solvent."), type = "error")
        return(NULL)
      }
    } else if (input$modifier_type == "blend") {
      # Use current blend
      if (length(rv$current_blend) < 2) {
        showNotification(i18n$t("Please add at least 2 solvents to the blend."), type = "error")
        return(NULL)
      }
      
      # Get current fractions from input values
      current_fractions <- sapply(names(rv$current_blend), function(s) {
        input_val <- input[[paste0("frac_", s)]]
        if (is.null(input_val) || is.na(input_val)) 0 else as.numeric(input_val)
      })
      total <- sum(current_fractions, na.rm = TRUE)
      
      if (total != 100) {
        showNotification(i18n$t("Blend percentages must total exactly 100%."), type = "error")
        return(NULL)
      }
      
      modifier_param <- names(rv$current_blend)
      modfracs_param <- as.numeric(current_fractions)
    }

    if (is.null(modifier_param) || length(modifier_param) == 0) {
      showNotification(i18n$t("Please select a co-solvent or define a blend."), type = "error")
      return(NULL)
    }

    # 6. Prepare colors
    cols_param <- "default"
    if (input$use_custom_colors) {
      custom_colors <- c(
        input$col_one, input$col_two, input$col_three,
        input$col_four, input$col_five, input$col_six
      )
      # Filter out NULLs and ensure valid colors
      custom_colors <- custom_colors[!sapply(custom_colors, is.null)]
      if (length(custom_colors) > 0) {
        cols_param <- setNames(custom_colors, c("one", "two", "three", "four", "five", "six")[1:length(custom_colors)])
      }
    }

    withProgress(message = i18n$t("Running Miscibility Comparison..."), {
      miscomp_output <- tryCatch(
        {
          miscomp(
            sols = combined_solutes(),
            tb = tb_param,
            crit = crit_param,
            hsp = hsp_param,
            modif = modifier_param,
            modfracs = modfracs_param,
            pres = pres_values,
            pres_comp = pres_values,  # Use all pressures for comparison plots
            cols = cols_param,
            plt_title = input$plt_title,
            temp = input$temp,
            vfrac = vfrac_values,
            simplicity = simplicity_param,
            draw = FALSE, # Important for Shiny apps
            silent = TRUE
          )
        },
        error = function(e) {
          showNotification(paste(i18n$t("Error during miscibility comparison:"), e$message), type = "error")
          return(NULL)
        }
      )

      if (!is.null(miscomp_output)) {
        rv$miscomp_results <- miscomp_output
        
        # Store modifier label info at computation time (so subtitles don't change with live edits)
        if (input$modifier_type == "pure" && !is.null(input$modifier_selection)) {
          rv$miscomp_results$modifier_label <- translate_solvent_token(input$modifier_selection, i18n_r())
          rv$miscomp_results$modifier_subtitle <- NULL
        } else if (input$modifier_type == "blend" && length(rv$current_blend) >= 2) {
          current_fractions <- sapply(names(rv$current_blend), function(s) {
            val <- input[[paste0("frac_", s)]]
            if (is.null(val) || is.na(val)) 0 else as.numeric(val)
          })
          translated_solvents <- sapply(names(rv$current_blend), function(s) translate_solvent_token(s, i18n_r()))
          fractions_str <- paste(round(current_fractions, 0), collapse = ":")
          solvents_str <- paste(translated_solvents, collapse = ":")
          rv$miscomp_results$modifier_label <- paste0(i18n_r()$t("solvent blend"), "*")
          rv$miscomp_results$modifier_subtitle <- paste0("* ", fractions_str, " v/v ", solvents_str)
        } else {
          rv$miscomp_results$modifier_label <- i18n_r()$t("co-solvent")
          rv$miscomp_results$modifier_subtitle <- NULL
        }

        # Render data table
        output$miscibility_data_table <- DT::renderDataTable({
          req(rv$miscomp_results$data)

          add_prettynames <- c(
            i18n_r()$t("Solute Name"),
            i18n_r()$t("Volume Fraction"),
            i18n_r()$t("Pressure (bar)"),
            i18n_r()$t("Miscibility Enhancement (%)")
          )
          
          DT::datatable(
            rv$miscomp_results$data,
            colnames = add_prettynames,
            extensions = "Buttons",
            options = list(
              scrollX = TRUE, 
              paging = FALSE, 
              language = tablang(),
              dom = "Bfrtip",
              buttons = create_dt_export_buttons(i18n_r, "supercrit_miscibility_comparison")
            ),
            rownames = FALSE,
            filter = "top"
          ) %>% formatRound("miscib", 2)
        })

        # Render dynamic plot tabs (plotly olarak güncellendi)
        output$plots_tabs_ui <- renderUI({
          req(rv$miscomp_results$plots)
          plot_names <- names(rv$miscomp_results$plots)
          
          # Separate solute plots from pressure comparison plots
          pres_plots <- plot_names[grepl("^pres_\\d+$", plot_names)]
          solute_plots <- plot_names[!grepl("^pres_\\d+$", plot_names)]
          
          all_tabs <- list()
          
          # Single tab for all analyte comparisons with dropdown selector
          if (length(solute_plots) > 0) {
            solute_choices <- setNames(solute_plots, sapply(solute_plots, function(s) i18n$t(s)))
            
            analyte_tab <- tabPanel(
              i18n$t("Analyte Comparisons"),
              div(
                style = "margin-bottom: 15px;",
                selectInput(
                  ns("analyte_comparison_selector"),
                  i18n$t("Select Analyte"),
                  choices = solute_choices,
                  selected = solute_choices[1],
                  width = "300px"
                )
              ),
              plotlyOutput(ns("analyte_comparison_plot"), height = "600px")
            )
            all_tabs <- c(all_tabs, list(analyte_tab))
          }
          
          # Single tab for pressure comparisons with dropdown selector
          if (length(pres_plots) > 0) {
            pres_choices <- sapply(pres_plots, function(p) {
              sub("^pres_(\\d+)$", "\\1", p)
            })
            names(pres_choices) <- sapply(pres_choices, function(p) {
              paste0(p, " ", i18n$t("bar"))
            })
            
            pres_comparison_tab <- tabPanel(
              i18n$t("Pressure Comparisons"),
              div(
                style = "margin-bottom: 15px;",
                selectInput(
                  ns("pres_comparison_selector"),
                  i18n$t("Select Pressure"),
                  choices = pres_choices,
                  selected = pres_choices[1],
                  width = "300px"
                )
              ),
              plotlyOutput(ns("pres_comparison_plot"), height = "600px")
            )
            all_tabs <- c(all_tabs, list(pres_comparison_tab))
          }
          
          do.call(tabsetPanel, c(list(id = ns("plots_active_tab")), all_tabs))
        })
        
        # Render the pressure comparison plot based on selector
        output$pres_comparison_plot <- renderPlotly({
          req(rv$miscomp_results$plots)
          req(input$pres_comparison_selector)
          
          plot_name <- paste0("pres_", input$pres_comparison_selector)
          req(plot_name %in% names(rv$miscomp_results$plots))
          
          plot_obj <- rv$miscomp_results$plots[[plot_name]]
          
          # Use stored modifier label info (frozen at computation time)
          modifier <- rv$miscomp_results$modifier_label %||% i18n_r()$t("co-solvent")
          modifier_subtitle <- rv$miscomp_results$modifier_subtitle
          
          pressure <- input$pres_comparison_selector
          plot_title <- if (isTRUE(input$plt_title)) {
            sprintf(
              i18n_r()$t("Miscibility enhancement of various solutes in %1$s at %2$s bar"),
              modifier,
              pressure
            )
          } else {
            NULL
          }
          
          x_label <- i18n_r()$t("Volume fraction")
          y_label <- i18n_r()$t("Miscibility enhancement (%)")
          
          plot_obj <- plot_obj + ggplot2::labs(
            title = plot_title,
            subtitle = if (isTRUE(input$plt_title)) modifier_subtitle else NULL,
            x = x_label,
            y = y_label
          )
          
          p <- ggplotly(plot_obj)
          
          if (!is.null(modifier_subtitle) && isTRUE(input$plt_title)) {
            p <- p %>% plotly::layout(
              annotations = list(
                list(
                  x = 0,
                  y = 1.06,
                  xref = "paper",
                  yref = "paper",
                  text = modifier_subtitle,
                  showarrow = FALSE,
                  font = list(size = 11, color = "gray40"),
                  xanchor = "left"
                )
              ),
              margin = list(t = 80)
            )
          }
          
          legend_title <- i18n_r()$t("Solute")
          p <- p %>% plotly::layout(
            legend = list(title = list(text = legend_title))
          )
          
          p
        })

        # Render the analyte comparison plot based on dropdown selector
        output$analyte_comparison_plot <- renderPlotly({
          req(rv$miscomp_results$plots)
          req(input$analyte_comparison_selector)
          
          plot_name <- input$analyte_comparison_selector
          req(plot_name %in% names(rv$miscomp_results$plots))
          
          plot_obj <- rv$miscomp_results$plots[[plot_name]]

          # Use stored modifier label info (frozen at computation time)
          modifier <- rv$miscomp_results$modifier_label %||% i18n_r()$t("co-solvent")
          modifier_subtitle <- rv$miscomp_results$modifier_subtitle

          # Parse pressure values (selectizeInput returns character vector)
          pres_values <- as.numeric(input$pres_input)
          pres_values <- pres_values[!is.na(pres_values)]
          pres_min <- if (length(pres_values) > 0) min(pres_values) else 100
          pres_max <- if (length(pres_values) > 0) max(pres_values) else 600

          # Solute-specific plot title
          plot_title <- if (isTRUE(input$plt_title)) {
            sprintf(
              i18n_r()$t("Miscibility enhancement of %1$s (%2$s-%3$s bar) in %4$s"),
              plot_name,
              pres_min,
              pres_max,
              modifier
            )
          } else {
            NULL
          }

          # Translate axis labels
          x_label <- i18n_r()$t("Volume fraction")
          y_label <- i18n_r()$t("Miscibility enhancement (%)")

          # Apply translations to ggplot object BEFORE ggplotly conversion
          plot_obj <- plot_obj + ggplot2::labs(
            title = plot_title,
            subtitle = if (isTRUE(input$plt_title)) modifier_subtitle else NULL,
            x = x_label,
            y = y_label
          )

          # Convert to plotly
          p <- ggplotly(plot_obj)
          
          # If there's a subtitle, add it via plotly annotations for proper rendering
          if (!is.null(modifier_subtitle) && isTRUE(input$plt_title)) {
            p <- p %>% plotly::layout(
              annotations = list(
                list(
                  x = 0,
                  y = 1.06,
                  xref = "paper",
                  yref = "paper",
                  text = modifier_subtitle,
                  showarrow = FALSE,
                  font = list(size = 11, color = "gray40"),
                  xanchor = "left"
                )
              ),
              margin = list(t = 80)
            )
          }

          # Solute plot: translate "pres" to "Pressure (bar)"
          legend_title <- i18n_r()$t("Pressure (bar)")
          p <- p %>% plotly::layout(
            legend = list(title = list(text = legend_title))
          )

          p
        })
      }
    })
  })

  # Reset function
  observeEvent(input$reset, {
    # Clear reactive values
    rv$miscomp_results <- NULL
    rv$selected_saved_calc_ids <- character(0)

    # Reset saved calculation checkboxes
    updateCheckboxGroupInput(session, "saved_calculation_checkboxes", selected = character(0))

    # Reset modifier type to default
    updateRadioButtons(session, "modifier_type", selected = defaults$modifier_type)

    # Reset modifier selection to default
    updatePickerInput(session, "modifier_selection", selected = defaults$modifier_selection)

    # Reset process conditions to defaults
    default_pres <- as.numeric(strsplit(defaults$pres_input, ",")[[1]])
    default_pres <- as.character(default_pres[!is.na(default_pres)])
    updateSelectizeInput(session, "pres_input", selected = default_pres)
    updateNumericInput(session, "temp", value = defaults$temp)
    default_vfrac <- as.numeric(strsplit(defaults$vfrac_input, ",")[[1]])
    default_vfrac <- as.character(default_vfrac[!is.na(default_vfrac)])
    updateSelectizeInput(session, "vfrac_input", selected = default_vfrac)

    # Reset advanced plot options
    updateCheckboxInput(session, "plt_title", value = defaults$plt_title)
    updateCheckboxInput(session, "use_custom_colors", value = defaults$use_custom_colors)

    # Reset custom colors to defaults
    updateColourInput(session, "col_one", value = defaults$col_one)
    updateColourInput(session, "col_two", value = defaults$col_two)
    updateColourInput(session, "col_three", value = defaults$col_three)
    updateColourInput(session, "col_four", value = defaults$col_four)
    updateColourInput(session, "col_five", value = defaults$col_five)
    updateColourInput(session, "col_six", value = defaults$col_six)

    # Clear current blend being built
    old_solvents <- names(rv$current_blend)
    rv$current_blend <- list()

    # Clear associated numeric inputs in UI
    for (s in old_solvents) {
      updateNumericInput(session, paste0("frac_", s), value = NULL)
      session$sendCustomMessage("resetInput", list(id = ns(paste0("frac_", s)), value = 0))
    }

    # Clear any fraction observers
    for (s in names(fraction_observers)) {
      if (!is.null(fraction_observers[[s]])) {
        fraction_observers[[s]]$destroy()
        fraction_observers[[s]] <- NULL
      }
    }

    showNotification(i18n$t("Miscibility comparison reset to defaults"), type = "message")
  })

  # Render HELP output
  output$sfe_misc_comp_HELP <- renderUI({
    create_help_modal(i18n_r, "sfe_misc_comp_help_en")
  })

  # Render accordion expand/collapse button with translated tooltips
  output$accordion_toggle_btn <- renderUI({
    create_accordion_toggle_btn(ns, i18n_r, "comp_accordion")
  })
  # Track currently visible plot tab
  rv$current_plot_tab <- NULL
  
  # Helper function to get translated plot for export (applies same translations as renderPlotly)
  get_translated_plot <- function(plot_name) {
    req(rv$miscomp_results$plots)
    req(plot_name %in% names(rv$miscomp_results$plots))
    
    plot_obj <- rv$miscomp_results$plots[[plot_name]]
    
    # Apply same translations as in renderPlotly
    # Determine if this is a pressure comparison plot or solute plot
    is_pres_plot <- grepl("^pres_\\d+$", plot_name)
    
    if (is_pres_plot) {
      pressure <- sub("^pres_(\\d+)$", "\\1", plot_name)
      # Use stored modifier label info (frozen at computation time)
      modifier <- rv$miscomp_results$modifier_label %||% i18n_r()$t("co-solvent")
      modifier_subtitle <- rv$miscomp_results$modifier_subtitle
      
      plot_title <- if (isTRUE(input$plt_title)) {
        paste0(
          i18n_r()$t("Miscibility Enhancement with"), " ", modifier, "\n",
          i18n_r()$t("at"), " ", pressure, " ", i18n_r()$t("bar"), ", ",
          input$temp, " °C"
        )
      } else NULL
      
      plot_obj <- plot_obj + ggplot2::labs(
        title = plot_title,
        subtitle = if (!is.null(modifier_subtitle)) modifier_subtitle else NULL,
        x = i18n_r()$t("Volume Fraction of Co-Solvent"),
        y = i18n_r()$t("Miscibility Enhancement (%)")
      )
    } else {
      # Solute-specific plot — use stored modifier label info
      modifier <- rv$miscomp_results$modifier_label %||% i18n_r()$t("co-solvent")
      modifier_subtitle <- rv$miscomp_results$modifier_subtitle
      
      plot_title <- if (isTRUE(input$plt_title)) {
        paste0(
          plot_name, ": ", i18n_r()$t("Miscibility Enhancement with"), " ", modifier
        )
      } else NULL
      
      plot_obj <- plot_obj + ggplot2::labs(
        title = plot_title,
        subtitle = if (!is.null(modifier_subtitle)) modifier_subtitle else NULL,
        x = i18n_r()$t("Volume Fraction of Co-Solvent"),
        y = i18n_r()$t("Miscibility Enhancement (%)"),
        colour = i18n_r()$t("Pressure (bar)"),
        shape = i18n_r()$t("Pressure (bar)")
      ) +
        ggplot2::theme(legend.title = ggplot2::element_text(size = 11))
    }
    
    return(plot_obj)
  }

  # Helper to determine the currently displayed plot name
  get_current_plot_name <- function() {
    plot_names <- names(rv$miscomp_results$plots)
    pres_plots <- plot_names[grepl("^pres_\\d+$", plot_names)]
    solute_plots <- plot_names[!grepl("^pres_\\d+$", plot_names)]
    
    active_tab <- input$plots_active_tab
    
    if (!is.null(active_tab)) {
      # Analyte Comparisons tab: use the analyte selector
      if (identical(active_tab, i18n$t("Analyte Comparisons")) && !is.null(input$analyte_comparison_selector)) {
        sel <- input$analyte_comparison_selector
        if (sel %in% plot_names) return(sel)
      }
      
      # Pressure Comparisons tab: use the pressure selector
      if (identical(active_tab, i18n$t("Pressure Comparisons")) && !is.null(input$pres_comparison_selector)) {
        pres_name <- paste0("pres_", input$pres_comparison_selector)
        if (pres_name %in% plot_names) return(pres_name)
      }
    }
    
    # Fallback: first available plot
    if (length(solute_plots) > 0) solute_plots[1] else pres_plots[1]
  }

  # Export current plot
  output$export_current_plot <- downloadHandler(
    filename = function() {
      current_name <- get_current_plot_name()
      paste0("supercrit_miscomp_", gsub("[^a-zA-Z0-9]", "_", current_name), "_", format(Sys.time(), "%d-%m-%Y_%Hhrs-%Mmin-%Ssec"), ".pdf")
    },
    content = function(file) {
      req(rv$miscomp_results$plots)
      
      current_name <- get_current_plot_name()
      plot_obj <- get_translated_plot(current_name)
      
      ggplot2::ggsave(file, plot = plot_obj, device = "pdf", width = 10, height = 8, units = "in", dpi = 300)
      
      showNotification(i18n$t("Done!"), type = "message", duration = 3)
    },
    contentType = "application/pdf"
  )

  # Export all plots as ZIP
  output$export_all_plots <- downloadHandler(
    filename = function() {
      paste0("supercrit_miscomp_all_plots_", format(Sys.time(), "%d-%m-%Y_%Hhrs-%Mmin-%Ssec"), ".zip")
    },
    content = function(file) {
      req(rv$miscomp_results$plots)
      
      prep_notif <- showNotification(i18n$t("Preparing export..."), type = "message", duration = NULL)
      on.exit(removeNotification(prep_notif), add = TRUE)
      
      # Create temp directory
      temp_dir <- file.path(tempdir(), paste0("miscomp_plots_", format(Sys.time(), "%d-%m-%Y_%Hhrs-%Mmin-%Ssec")))
      dir.create(temp_dir, showWarnings = FALSE)
      
      tryCatch({
        plot_names <- names(rv$miscomp_results$plots)
        
        for (pname in plot_names) {
          plot_obj <- get_translated_plot(pname)
          
          # Create safe filename
          safe_name <- gsub("[^a-zA-Z0-9]", "_", pname)
          plot_file <- file.path(temp_dir, paste0(safe_name, ".pdf"))
          
          ggplot2::ggsave(plot_file, plot = plot_obj, device = "pdf", width = 10, height = 8, units = "in", dpi = 300)
        }
        
        # Create ZIP
        files_to_zip <- list.files(temp_dir, full.names = TRUE)
        zip::zip(zipfile = file, files = files_to_zip, mode = "cherry-pick")
        
        showNotification(i18n$t("Done!"), type = "message", duration = 3)
      }, error = function(e) {
        showNotification(paste(i18n$t("Error exporting plots:"), e$message), type = "error")
      }, finally = {
        unlink(temp_dir, recursive = TRUE)
      })
    },
    contentType = "application/zip"
  )

  # suspendWhenHidden for renderUI outputs (auto-added)
  outputOptions(output, "accordion_toggle_btn", suspendWhenHidden = FALSE)
  outputOptions(output, "saved_calculations_list_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "selected_solutes_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "sfe_misc_comp_HELP", suspendWhenHidden = FALSE)

}
