library(shinyjs)

miscibility_optimization_server <- function(input, output, session, defaults, i18n, tablang, sfe_rv) {






  # Load the helper function from general_helpers.R

  ns <- session$ns
  i18n_r <- reactive({
    i18n
  })
  rv <- reactiveValues(
    miscibility_results = NULL,
    modifier_ranking = NULL,
    solute_data = NULL, # Keep for comparison list, but not for direct sfe_mod input
    gcm_results = NULL, # Keep for comparison list, but not for direct sfe_mod input
    sfe_mod_output = NULL,
    current_blend = list(), # list(solvents, fractions)
    saved_blends = list(), # list of lists
    me_solvent_list = NULL, # Raw modifier names for ME dropdown
    best_table_view = "solvent", # "solvent" or "me" for Best Modifier Table view
    # Loaded calculation parameters from saved GCM calculation
    loaded_calculation = NULL,
    loaded_cas = NULL,
    loaded_name = NULL,
    loaded_smiles = NULL,
    loaded_specify_smiles = NULL,
    loaded_tb = NULL,
    loaded_tb_manual_input = NULL,
    loaded_crit = NULL,
    loaded_hsp = NULL,
    loaded_simplicity = NULL,
    loaded_gorder = NULL
  )

  # Reactive output to check if results are available
  output$has_results <- reactive({
    !is.null(rv$sfe_mod_output)
  })
  outputOptions(output, "has_results", suspendWhenHidden = FALSE)

  # Observer to enable/disable result tabs based on results availability
  observe({
    has_results <- !is.null(rv$sfe_mod_output)
    if (has_results) {
      session$sendCustomMessage("enableTabs", list(tabsetId = ns("results_tabs")))
      shinyjs::show("best_modifier_table_wrapper")
    } else {
      session$sendCustomMessage("disableTabs", list(tabsetId = ns("results_tabs"), keepFirst = TRUE))
      shinyjs::hide("best_modifier_table_wrapper")
    }
  })


  # Reactive value to store the currently selected calculation ID from the list
  rv_selected_calc_id <- reactiveVal(NULL)

  # Modifier names: internal values -> translated display names
  modifier_values <- c("Acetone", "Benzene", "Cyclohexane", "DiethylEther",
    "Ethanol", "Heptane", "Hexane", "Methanol", "MethylOleate",
    "Toluene", "PXylene", "OXylene", "Water")
  modifier_display_keys <- c("Acetone", "Benzene", "Cyclohexane", "Diethyl Ether",
    "Ethanol", "Heptane", "Hexane", "Methanol", "Methyl Oleate",
    "Toluene", "p-Xylene", "o-Xylene", "Water")

  # Abbreviation -> display key mapping (from solv_db)
  modifier_abbrevs <- c("Ac", "C6H6", "CHex", "DEEt",
    "EtOH", "Hep", "Hex", "MeOH", "MeOl",
    "Tol", "p-X", "o-X", "H2O")
  abbrev_display_keys <- c("Acetone", "Benzene", "Cyclohexane", "Diethyl Ether",
    "Ethanol", "Heptane", "Hexane", "Methanol", "Methyl Oleate",
    "Toluene", "p-Xylene", "o-Xylene", "Water")

  # Helper: translate a single solvent token (full name or abbreviation)
  translate_solvent_token <- function(token, i18n_fn) {
    token <- trimws(token)
    key_map <- setNames(modifier_display_keys, modifier_values)
    if (token %in% names(key_map)) return(i18n_fn$t(key_map[[token]]))
    abb_map <- setNames(abbrev_display_keys, modifier_abbrevs)
    if (token %in% names(abb_map)) return(i18n_fn$t(abb_map[[token]]))
    if (token == "CO2") return("CO2")
    return(token)
  }

  # Helper: translate a modifier name (single, blend, or with CO2 prefix)
  translate_modifier <- function(name, i18n_fn = i18n_r()) {
    if (length(name) != 1 || is.na(name) || !nzchar(name)) return(as.character(name))
    co2_prefix <- ""
    if (grepl("^CO2 with ", name)) {
      co2_prefix <- "CO2 + "
      name <- sub("^CO2 with ", "", name)
    }
    if (grepl(":", name)) {
      parts <- strsplit(name, " \\(")[[1]]
      solvent_part <- parts[1]
      frac_part <- if (length(parts) > 1) paste0(" (", parts[2]) else ""
      tokens <- strsplit(solvent_part, ":")[[1]]
      translated <- sapply(tokens, function(s) translate_solvent_token(s, i18n_fn), USE.NAMES = FALSE)
      return(paste0(co2_prefix, paste(translated, collapse = ":"), frac_part))
    }
    return(paste0(co2_prefix, translate_solvent_token(name, i18n_fn)))
  }

  # Rendered pickerInput with translated modifier names and Select All/Deselect All
  output$modifier_selection_ui <- renderUI({
    translated_names <- sapply(modifier_display_keys, function(k) i18n_r()$t(k))
    choices <- setNames(modifier_values, translated_names)
    pickerInput(
      inputId = ns("modifier_selection"),
      label = tags$span(i18n_r()$t("Select Modifier(s)"),
        input_help(i18n_r()$t("Choose one or more co-solvents to evaluate alongside pure CO2. The miscibility enhancement of each co-solvent is calculated across the specified pressure and temperature ranges."),
                   title = i18n_r()$t("Co-Solvent Selection"), buttonLabel = i18n_r()$t("OK"))),
      choices = choices,
      selected = c("Ethanol", "Water"),
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `live-search` = TRUE,
        `select-all-text` = i18n_r()$t("Select All"),
        `deselect-all-text` = i18n_r()$t("Deselect All")
      )
    )
  })

  # Rendered radioButtons with translated display mode choices
  output$table_display_mode_ui <- renderUI({
    radioButtons(ns("table_display_mode"),
      label = NULL,
      choices = setNames(
        c("fit", "scroll"),
        c(i18n_r()$t("Fit to page"), i18n_r()$t("Show all (scrollable)"))
      ),
      selected = defaults$table_display_mode,
      inline = TRUE
    )
  })

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

  # Dynamic Process Conditions inputs with range badges
  # suspendWhenHidden = FALSE ensures inputs are available even when the accordion is collapsed
  output$vfrac_input_ui <- renderUI({
    cur <- if (is.null(input$vfrac_input)) defaults$vfrac * 100 else input$vfrac_input
    local_range_badge_input("vfrac_input", i18n_r()$t("Co-Solvent Volume Fraction (%)"),
      value = cur, min_val = 0.1, max_val = 99.9, step = 0.1,
      range_text = "0.1\u201399.9",
      tooltip = "0.1 \u2013 99.9%",
      help_content = i18n_r()$t("Volume percentage of co-solvent in the total solvent mixture. Typically 5-15% for SFE applications. Higher fractions increase solvent polarity but may reduce selectivity."),
      help_title = i18n_r()$t("Volume Fraction"))
  })
  outputOptions(output, "vfrac_input_ui", suspendWhenHidden = FALSE)

  output$pressure_min_ui <- renderUI({
    cur <- if (is.null(input$pressure_min)) defaults$pres_min else input$pressure_min
    local_range_badge_input("pressure_min", i18n_r()$t("Min. Pressure (bar)"),
      value = cur, min_val = 75, max_val = 1000,
      tooltip = paste0("75 \u2013 1000 ", i18n_r()$t("bar")),
      help_content = i18n_r()$t("Pressure range and step size for the miscibility evaluation grid. The calculation is repeated at each pressure-temperature combination within this range."),
      help_title = i18n_r()$t("Pressure Range"))
  })
  outputOptions(output, "pressure_min_ui", suspendWhenHidden = FALSE)

  output$temperature_min_ui <- renderUI({
    cur <- if (is.null(input$temperature_min)) defaults$temp_min else input$temperature_min
    local_range_badge_input("temperature_min", i18n_r()$t("Min. Temperature (\u00B0C)"),
      value = cur, min_val = 31, max_val = 200,
      tooltip = "31 \u2013 200 \u00B0C",
      help_content = i18n_r()$t("Temperature range and step size for the miscibility evaluation grid. Must be above 31\u00B0C (the critical temperature of CO2)."),
      help_title = i18n_r()$t("Temperature Range"))
  })
  outputOptions(output, "temperature_min_ui", suspendWhenHidden = FALSE)

  output$pressure_max_ui <- renderUI({
    cur <- if (is.null(input$pressure_max)) defaults$pres_max else input$pressure_max
    local_range_badge_input("pressure_max", i18n_r()$t("Max. Pressure (bar)"),
      value = cur, min_val = 75, max_val = 1000,
      tooltip = paste0("75 \u2013 1000 ", i18n_r()$t("bar")))
  })
  outputOptions(output, "pressure_max_ui", suspendWhenHidden = FALSE)

  output$temperature_max_ui <- renderUI({
    cur <- if (is.null(input$temperature_max)) defaults$temp_max else input$temperature_max
    local_range_badge_input("temperature_max", i18n_r()$t("Max. Temperature (\u00B0C)"),
      value = cur, min_val = 31, max_val = 200,
      tooltip = "31 \u2013 200 \u00B0C")
  })
  outputOptions(output, "temperature_max_ui", suspendWhenHidden = FALSE)

  output$pressure_step_ui <- renderUI({
    cur <- if (is.null(input$pressure_step)) defaults$pres_step else input$pressure_step
    local_range_badge_input("pressure_step", i18n_r()$t("Pressure Step (bar)"),
      value = cur, min_val = 1, max_val = 100,
      tooltip = paste0("1 \u2013 100 ", i18n_r()$t("bar")))
  })
  outputOptions(output, "pressure_step_ui", suspendWhenHidden = FALSE)

  output$temperature_step_ui <- renderUI({
    cur <- if (is.null(input$temperature_step)) defaults$temp_step else input$temperature_step
    local_range_badge_input("temperature_step", i18n_r()$t("Temperature Step (\u00B0C)"),
      value = cur, min_val = 1, max_val = 50,
      tooltip = "1 \u2013 50 \u00B0C")
  })
  outputOptions(output, "temperature_step_ui", suspendWhenHidden = FALSE)

  # Toggle button for Best Modifier Table view (solvent names vs ME values)
  output$best_table_view_toggle_ui <- renderUI({
    label <- if (rv$best_table_view == "solvent") {
      i18n_r()$t("Show ME Values")
    } else {
      i18n_r()$t("Show Solvent Names")
    }
    actionButton(ns("toggle_best_table_view"), label,
      icon = icon("exchange-alt"), class = "btn-sm btn-default"
    )
  })

  observeEvent(input$toggle_best_table_view, {
    rv$best_table_view <- if (rv$best_table_view == "solvent") "me" else "solvent"
  })

  # Shared color palette for Best Modifier Table and legend
  palette_base <- c(
    "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
    "#FF7F00", "#FFFF33", "#A65628", "#F781BF",
    "#999999", "#66C2A5"
  )

  modifier_color_map <- reactive({
    req(rv$sfe_mod_output)
    best_mod_data <- rv$sfe_mod_output$sfe$Best_Modifier
    if (is.null(best_mod_data) || length(best_mod_data) == 0) return(NULL)

    methods <- sort(unique(unlist(best_mod_data)))
    methods <- methods[methods != "CO2"]
    if (length(methods) == 0) return(NULL)

    n <- length(methods)
    pal <- if (n > length(palette_base)) rep(palette_base, ceiling(n / length(palette_base))) else palette_base
    base_cols <- pal[1:n]
    bg_cols <- sapply(base_cols, function(col) {
      rv <- col2rgb(col)
      rgb(rv[1] / 255, rv[2] / 255, rv[3] / 255, alpha = 0.5)
    })
    list(methods = methods, base_colors = base_cols, bg_colors = bg_cols)
  })

  # Legend for colour-coded solvents (shown in both views)
  output$best_table_legend_ui <- renderUI({
    cmap <- modifier_color_map()
    if (is.null(cmap)) return(NULL)

    methods <- cmap$methods
    base_colors <- cmap$base_colors
    bg_colors <- cmap$bg_colors

    # Deduplicate by translated name
    translated <- sapply(methods, translate_modifier)
    seen <- character(0)
    legend_items <- list()
    for (i in seq_along(methods)) {
      if (!translated[i] %in% seen) {
        seen <- c(seen, translated[i])
        legend_items[[length(legend_items) + 1]] <- tags$span(
          style = "display:inline-flex; align-items:center; margin-right:10px;",
          tags$span(style = paste0(
            "width:14px; height:14px; border-radius:3px; margin-right:4px; ",
            "background-color:", bg_colors[i], "; border:1px solid ", base_colors[i], ";"
          )),
          tags$span(translated[i], style = "font-size:12px;")
        )
      }
    }

    tags$div(
      style = "display:flex; flex-wrap:wrap; align-items:center; gap:2px;",
      tags$strong(paste0(i18n_r()$t("Legend"), ": "), style = "font-size:12px; margin-right:4px;"),
      tagList(legend_items)
    )
  })

  # Ensure renderUI outputs inside collapsed accordions render eagerly
  outputOptions(output, "modifier_selection_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "table_display_mode_ui", suspendWhenHidden = FALSE)

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

  # Render the list of saved calculations
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
    display_calcs <- calcs

    # Create choices for radio buttons
    choices <- setNames(
      seq_along(display_calcs),
      sapply(display_calcs, function(x) x$display_name)
    )

    has_selection <- !is.null(rv_selected_calc_id()) && rv_selected_calc_id() <= length(display_calcs)

    delete_btn <- actionButton(
      ns("delete_selected_calc"),
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
        radioButtons(
          ns("saved_calculation_radio"),
          label = i18n$t("Select a saved calculation"),
          choices = choices,
          selected = if (has_selection) rv_selected_calc_id() else character(0)
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

  # Observe selection of radio buttons and auto-load calculation
  observeEvent(input$saved_calculation_radio, {
    rv_selected_calc_id(as.numeric(input$saved_calculation_radio))

    # Auto-load the selected calculation
    req(rv_selected_calc_id())

    # Get calculation from session storage
    calc <- if (rv_selected_calc_id() <= length(sfe_rv$saved_calculations)) {
      sfe_rv$saved_calculations[[rv_selected_calc_id()]]
    } else {
      NULL
    }

    if (is.null(calc)) {
      showNotification(i18n$t("Selected calculation not found."), type = "error")
      return()
    }

    # Store parameters in reactive values for use in optimize_miscibility
    rv$loaded_calculation <- calc$display_name
    rv$loaded_cas <- calc$parameters$cas_input
    rv$loaded_name <- calc$parameters$name_input
    rv$loaded_smiles <- calc$parameters$smiles_input
    rv$loaded_specify_smiles <- calc$parameters$specify_smiles
    rv$loaded_tb <- calc$parameters$gcm_tb
    rv$loaded_tb_manual_input <- calc$parameters$tb_manual_input
    rv$loaded_crit <- calc$parameters$gcm_crit
    rv$loaded_hsp <- calc$parameters$gcm_hsp
    rv$loaded_simplicity <- calc$parameters$gcm_simplicity
    rv$loaded_gorder <- calc$parameters$gcm_gorder

    # Also store results for compatibility
    rv$sfe_mod_input <- calc$results
    rv$loaded_calc_id <- rv_selected_calc_id()

    # Show feedback about loaded calculation
    showNotification(
      paste0(i18n$t("Loaded: "), calc$display_name),
      type = "message",
      duration = 3
    )
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # Observer for delete button
  observeEvent(input$delete_selected_calc, {
    req(input$saved_calculation_radio)
    
    idx <- as.numeric(input$saved_calculation_radio)
    req(idx <= length(sfe_rv$saved_calculations))
    
    calc_name <- sfe_rv$saved_calculations[[idx]]$display_name
    
    # Show confirmation dialog
    showModal(modalDialog(
      title = i18n$t("Confirm Deletion"),
      p(i18n$t("Are you sure you want to delete this calculation?")),
      p(tags$strong(calc_name)),
      footer = tagList(
        modalButton(i18n$t("Cancel")),
        actionButton(ns("confirm_delete_calc"), i18n$t("Delete"),
          class = "btn-danger", style = "color: white;")
      )
    ))
  })

  # Observer for confirm delete
  observeEvent(input$confirm_delete_calc, {
    removeModal()
    
    req(input$saved_calculation_radio)
    idx <- as.numeric(input$saved_calculation_radio)
    req(idx <= length(sfe_rv$saved_calculations))

    # Delete the calculation
    calc_name <- sfe_rv$saved_calculations[[idx]]$display_name
    sfe_rv$saved_calculations[[idx]] <- NULL
    sfe_rv$saved_calculations <- Filter(Negate(is.null), sfe_rv$saved_calculations)

    # Reset selection
    rv_selected_calc_id(NULL)
    rv$sfe_mod_input <- NULL
    rv$loaded_calc_id <- NULL

    showNotification(
      paste0(i18n$t("Deleted: "), calc_name),
      type = "warning",
      duration = 3
    )
  })

  # Render blend solvent selector with reactive translations
  output$blend_solvent_selector_ui <- renderUI({
    translated_names <- sapply(modifier_display_keys, function(k) i18n_r()$t(k))
    selectInput(
      ns("blend_solvent_selector"),
      tags$span(i18n_r()$t("Select Solvent to Add"),
        input_help(i18n_r()$t("Choose a solvent to add to the custom blend. Multiple solvents can be combined with specified volume fractions."),
                   title = i18n_r()$t("Blend Solvent"), buttonLabel = i18n_r()$t("OK"))),
      choices = setNames(modifier_values, translated_names),
      selected = NULL
    )
  })
  outputOptions(output, "blend_solvent_selector_ui", suspendWhenHidden = FALSE)

  # Toggle SMILES input visibility
  observeEvent(input$specify_smiles, {
    shinyjs::toggle("smiles_div", condition = input$specify_smiles)
  })

  # Toggle manual Tb input visibility
  observeEvent(input$gcm_tb,
    {
      if (input$gcm_tb == "manual") {
        shinyjs::show("tb_manual_div")
      } else {
        shinyjs::hide("tb_manual_div")
      }
    },
    ignoreInit = FALSE
  )



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
      div(
        style = "display: flex; align-items: center; gap: 8px; margin-bottom: 5px;",
        tags$span(
          style = "font-weight: bold; min-width: 100px;",
          translate_modifier(solvent)
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

  # 5. Add Blend
  observeEvent(input$save_blend, {
    if (length(rv$current_blend) < 2) {
      showNotification(i18n$t("A blend must contain at least 2 solvents."), type = "error")
      return()
    }

    # Recalculate total based on current input values, not rv$current_blend directly
    # This ensures we use the most up-to-date values from the UI
    current_fractions <- sapply(names(rv$current_blend), function(s) {
      input_val <- input[[paste0("frac_", s)]]
      if (is.null(input_val) || is.na(input_val)) 0 else as.numeric(input_val)
    })
    total <- sum(current_fractions, na.rm = TRUE)



    if (total != 100) {
      showNotification(i18n$t("Total percentage must be exactly 100% to save the blend."), type = "error")
      return()
    }

    # Create blend object (deep copy) using the current_fractions
    blend <- list(
      solvents = as.character(names(rv$current_blend)),
      fractions = as.numeric(current_fractions),
      preview = paste0(
        paste(names(rv$current_blend), collapse = ":"),
        " (", paste(sprintf("%.0f", current_fractions), collapse = ":"), ")"
      )
    )

    # Add to Added Blends
    rv$saved_blends[[length(rv$saved_blends) + 1]] <- blend

    # Reset fractions to 0 for new blend with same solvents
    for (s in names(rv$current_blend)) {
      rv$current_blend[[s]] <- 0
    }

    showNotification(i18n$t("Blend saved!"), type = "message")
  })

  # 6. Render Added Blends
  output$saved_blends_ui <- renderUI({
    if (length(rv$saved_blends) == 0) {
      return(p(i18n$t("No blends saved yet.")))
    }

    blend_items <- lapply(seq_along(rv$saved_blends), function(i) {
      blend <- rv$saved_blends[[i]]
      # Translate solvent names in preview
      translated_preview <- paste0(
        paste(sapply(blend$solvents, translate_modifier), collapse = ":"),
        " (", paste(sprintf("%.0f", blend$fractions), collapse = ":"), ")"
      )
      fluidRow(
        column(9, p(paste0(i, ". ", translated_preview))),
        column(3, actionButton(ns(paste0("del_blend_", i)), "×",
          class = "btn-xs btn-danger",
          onclick = paste0("Shiny.setInputValue('", ns("last_deleted_blend_id"), "', 'del_blend_", i, "', {priority: 'event'})")
        ))
      )
    })

    tagList(
      blend_items,
      # Hidden input to capture the ID of the clicked delete button
      shinyjs::hidden(actionButton(ns("last_deleted_blend_id"), "", style = "display: none;"))
    )
  })

  # 7. Observe for removing solvents from current blend
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

  # 8. Observe for deleting Added Blends (Revised approach)
  # This observer will listen for any button click that matches the pattern "del_blend_X"
  observeEvent(input$last_deleted_blend_id,
    {
      req(input$last_deleted_blend_id)

      # Extract the index from the input ID
      # The input ID will be something like "del_blend_1", "del_blend_2", etc.
      deleted_idx_str <- sub("del_blend_", "", input$last_deleted_blend_id)
      deleted_idx <- as.numeric(deleted_idx_str)

      # Ensure the index is valid and within the bounds of rv$saved_blends
      if (!is.na(deleted_idx) && deleted_idx > 0 && deleted_idx <= length(rv$saved_blends)) {
        isolate({
          rv$saved_blends <- rv$saved_blends[-deleted_idx]
        })
      }

      # Reset the input value to allow for repeated clicks on the same button ID
      # (though the button itself will be re-rendered with a new ID if the list changes)
      session$sendCustomMessage("resetInput", list(id = ns(input$last_deleted_blend_id), value = 0))
    },
    ignoreInit = TRUE,
    ignoreNULL = TRUE
  )

  # Validate Process Conditions ranges when Optimize is clicked
  # Returns NULL if valid, or a translated error string if not
  validate_ranges <- function() {
    i18n_fn <- i18n_r()
    checks <- list(
      list(input$vfrac_input, 0.1, 99.9, "Co-Solvent Volume Fraction (%)"),
      list(input$pressure_min, 75, 1000, "Min. Pressure (bar)"),
      list(input$pressure_max, 75, 1000, "Max. Pressure (bar)"),
      list(input$temperature_min, 31, 200, "Min. Temperature (\u00B0C)"),
      list(input$temperature_max, 31, 200, "Max. Temperature (\u00B0C)"),
      list(input$pressure_step, 1, 100, "Pressure Step (bar)"),
      list(input$temperature_step, 1, 50, "Temperature Step (\u00B0C)")
    )
    invalid_labels <- c()
    for (ch in checks) {
      v <- ch[[1]]
      if (is.null(v)) next
      if (is.na(v) || v < ch[[2]] || v > ch[[3]]) {
        invalid_labels <- c(invalid_labels, i18n_fn$t(ch[[4]]))
      }
    }
    if (length(invalid_labels) == 0) return(NULL)
    paste(i18n_fn$t("Out of range:"), paste(invalid_labels, collapse = ", "))
  }

  observeEvent(input$optimize_miscibility, {
    # Validate Process Conditions ranges
    range_error <- validate_ranges()
    if (!is.null(range_error)) {
      showNotification(range_error, type = "error")
      return(NULL)
    }

    # Check if a calculation is loaded and show notification if not
    if (is.null(rv$loaded_calculation) || is.null(rv$loaded_tb) || is.null(rv$loaded_crit) || is.null(rv$loaded_hsp)) {
      showNotification(i18n$t("Please load a saved calculation first."), type = "error")
      return(NULL)
    }

    # Require loaded calculation - cannot optimize without loading one
    req(rv$loaded_tb, rv$loaded_crit, rv$loaded_hsp, rv$loaded_calculation)

    # 1. Build solute vector from LOADED calculation parameters
    # Always create a 3-element NAMED vector: [SMILES, CAS, Name]
    solute_input_vector <- c(
      SMILES = if (!is.null(rv$loaded_smiles)) rv$loaded_smiles else "",
      CAS = if (!is.null(rv$loaded_cas)) rv$loaded_cas else "",
      Name = if (!is.null(rv$loaded_name)) rv$loaded_name else ""
    )

    # Input validation for GCM methods from loaded calculation
    if (rv$loaded_tb == "none" && rv$loaded_crit == "none" && rv$loaded_hsp == "none") {
      showNotification(i18n$t("Loaded calculation has no GCM methods selected. Please load a different calculation."), type = "error")
      return(NULL)
    }

    # Input validation for pressure and temperature ranges
    req(input$pressure_min, input$pressure_max, input$temperature_min, input$temperature_max)
    if (input$pressure_min >= input$pressure_max) {
      showNotification(i18n$t("Minimum pressure must be less than maximum pressure."), type = "error")
      return(NULL)
    }
    if (input$temperature_min >= input$temperature_max) {
      showNotification(i18n$t("Minimum temperature must be less than maximum temperature."), type = "error")
      return(NULL)
    }

    # Check for modifier selection and Added Blends
    if (length(input$modifier_selection) == 0 && length(rv$saved_blends) == 0) {
      showNotification(i18n$t("Please select at least one co-solvent or define a blend for miscibility optimization."), type = "error")
      return(NULL)
    }

    withProgress(message = i18n$t("Optimizing Miscibility..."), {
      # Build modif and modfracs parameters
      modif_list <- list()
      modfracs_list <- list()

      # 1. Add individual modifiers
      if (!is.null(input$modifier_selection) && length(input$modifier_selection) > 0) {
        for (mod in input$modifier_selection) {
          modif_list[[length(modif_list) + 1]] <- mod
          # modfracs_list[[length(modfracs_list) + 1]] <- NA # No fractions for single modifiers
        }
      }

      # 2. Add Added Blends
      if (length(rv$saved_blends) > 0) {
        for (blend in rv$saved_blends) {
          modif_list[[length(modif_list) + 1]] <- blend$solvents

          # Prepare modfracs
          if (length(blend$solvents) == 2 &&
            blend$fractions[1] == blend$fractions[2] &&
            sum(blend$fractions) == 100) {
            modfracs_list[[length(modfracs_list) + 1]] <- blend$fractions[1]
          } else {
            modfracs_list[[length(modfracs_list) + 1]] <- blend$fractions
          }
        }
      }

      # Prepare modfracs parameter (NA if no blends, otherwise the list)
      modfracs_param <- if (length(modfracs_list) > 0) modfracs_list else NA

      # Generate pressure and temperature sequences using step values
      pres_step <- if (!is.null(input$pressure_step) && input$pressure_step > 0) input$pressure_step else 5
      temps_step <- if (!is.null(input$temperature_step) && input$temperature_step > 0) input$temperature_step else 3
      
      pres_seq <- seq(input$pressure_min, input$pressure_max, by = pres_step)
      # Ensure max value is included if not already
      if (tail(pres_seq, 1) != input$pressure_max) {
        pres_seq <- c(pres_seq, input$pressure_max)
      }
      
      temps_seq <- seq(input$temperature_min, input$temperature_max, by = temps_step)
      # Ensure max value is included if not already
      if (tail(temps_seq, 1) != input$temperature_max) {
        temps_seq <- c(temps_seq, input$temperature_max)
      }

      vfrac_val <- input$vfrac_input / 100

      # Prepare Tb parameter from loaded calculation
      tb_param <- if (rv$loaded_tb == "manual") {
        req(rv$loaded_tb_manual_input)
        as.numeric(rv$loaded_tb_manual_input)
      } else {
        rv$loaded_tb
      }

      # Then proceed with the tryCatch (unchanged)
      sfe_mod_output <- tryCatch(
        {
          sfe_mod(
            solute = solute_input_vector, # Use the directly provided input vector
            tb = tb_param,
            crit = rv$loaded_crit,
            hsp = rv$loaded_hsp,
            gorder = rv$loaded_gorder,
            hlight = TRUE,
            simplicity = rv$loaded_simplicity,
            modif = modif_list,
            modfracs = modfracs_param,
            pres = pres_seq, # Use the generated sequence
            temps = temps_seq, # Use the generated sequence
            vfrac = vfrac_val, # Use the converted fraction
            silent = TRUE
          )
        },
        error = function(e) {
          showNotification(paste(i18n$t("Error during miscibility optimization:"), e$message), type = "error")
          rv$sfe_mod_output <- NULL # Clear results on error
          return(NULL)
        }
      )

      if (!is.null(sfe_mod_output)) {
        # Store the full output for export
        rv$sfe_mod_output <- sfe_mod_output

        # Populate rv$solute_data and rv$gcm_results from sfe_mod_output for comparison list
        rv$solute_data <- sfe_mod_output$solute_ids
        rv$gcm_results <- list(
          pares = sfe_mod_output$parameters,
          methods = list(tb = rv$loaded_tb, crit = rv$loaded_crit, hsp = rv$loaded_hsp)
        )

        # Process Miscibility Enhancement results for plotting
        miscib_enh_list <- sfe_mod_output$sfe$Miscib_Enhancement
        miscib_enh_df_list <- lapply(names(miscib_enh_list), function(modifier_name) {
          df <- as.data.frame(miscib_enh_list[[modifier_name]])
          df$Pressure_bar <- as.numeric(rownames(df))
          df_long <- tidyr::gather(df, key = "Temperature_C", value = "ME_percent", -Pressure_bar)
          df_long$Temperature_C <- as.numeric(gsub("X", "", df_long$Temperature_C))
          df_long$Modifier <- modifier_name
          df_long$Pressure_MPa <- df_long$Pressure_bar / 10 # Convert bar to MPa
          df_long$Temperature_K <- df_long$Temperature_C + 273.15 # Convert Celsius to Kelvin
          df_long
        })
        rv$miscibility_results <- do.call(rbind, miscib_enh_df_list)

        # Handle Modifier_Ranking carefully for single or multiple modifiers
        if (!is.null(sfe_mod_output$sfe$Modifier_Ranking) && length(sfe_mod_output$sfe$Modifier_Ranking) > 0) {
          # Modifier_Ranking is a table object (named numeric vector) from sfe_mod
          # Convert it to a data.frame with proper column names
          ranking_data <- sfe_mod_output$sfe$Modifier_Ranking
          rv$modifier_ranking <- data.frame(
            Modifier = names(ranking_data),
            Occurrence = as.numeric(ranking_data),
            check.names = FALSE
          )
          # Sort by occurrence descending
          rv$modifier_ranking <- rv$modifier_ranking[order(rv$modifier_ranking[[2]], decreasing = TRUE), ]
        } else if (length(modif_list) == 1) {
          # Case: Exactly one modifier or one blend selected. It's 100% "ranked"
          # Use the key from Miscib_Enhancement if available, otherwise construct from modif_list
          me_keys <- names(sfe_mod_output$sfe$Miscib_Enhancement)
          mod_name <- if (length(me_keys) > 0) {
            me_keys[1]
          } else if (is.character(modif_list[[1]]) && length(modif_list[[1]]) > 1) {
            # Blend: construct name from components
            paste(modif_list[[1]], collapse = ":")
          } else {
            as.character(modif_list[[1]])[1]
          }
          rv$modifier_ranking <- data.frame(
            Modifier = mod_name,
            Occurrence = 100
          )
        } else {
          # Fallback for unexpected cases or no modifiers to rank meaningfully
          rv$modifier_ranking <- data.frame(
            Modifier = character(0),
            Occurrence = numeric(0)
          )
        }

        # Render Value Boxes
        output$best_modifier_vb <- renderValueBox({
          req(rv$modifier_ranking)
          best_mod <- rv$modifier_ranking[1, 1]
          display_text <- translate_modifier(best_mod)
          # Scale font size based on text length to prevent overflow
          font_size <- if (nchar(display_text) > 30) "14px"
            else if (nchar(display_text) > 20) "18px"
            else if (nchar(display_text) > 14) "22px"
            else "28px"
          valueBox(
            tags$span(display_text, style = paste0("font-size:", font_size)),
            i18n$t("Best Co-Solvent"), icon = icon("star"), color = "green"
          )
        })

        output$max_me_vb <- renderValueBox({
          req(rv$miscibility_results)
          min_me <- min(rv$miscibility_results$ME_percent, na.rm = TRUE)
          max_me <- max(rv$miscibility_results$ME_percent, na.rm = TRUE)
          valueBox(
            paste0(round(min_me, 2), " – ", round(max_me, 2), " %"),
            i18n$t("ME Range (%)"),
            icon = icon("percent"), color = "blue"
          )
        })

        output$num_modifiers_vb <- renderValueBox({
          req(rv$miscibility_results)
          total_configs <- length(unique(rv$miscibility_results$Modifier))
          valueBox(total_configs, i18n$t("Solvents Tested"), icon = icon("flask"), color = "yellow")
        })


        output$best_modifier_table <- renderDT({
          req(rv$miscibility_results)

          # Reactive dependencies
          view_mode <- rv$best_table_view  # "solvent" or "me"
          display_mode <- if (!is.null(input$table_display_mode)) input$table_display_mode else "fit"
          bar_label <- i18n_r()$t("bar")

          max_fit_rows <- 10
          max_fit_cols <- 10

          best_mod_data <- sfe_mod_output$sfe$Best_Modifier

          if (!is.null(best_mod_data) && length(best_mod_data) > 0) {
            # Multiple modifiers case
            best_mod_matrix <- best_mod_data

            # Fit-to-page sampling
            if (display_mode == "fit") {
              n_rows <- nrow(best_mod_matrix)
              n_cols <- ncol(best_mod_matrix)
              if (n_rows > max_fit_rows) {
                row_indices <- round(seq(1, n_rows, length.out = max_fit_rows))
                best_mod_matrix <- best_mod_matrix[row_indices, , drop = FALSE]
              }
              if (n_cols > max_fit_cols) {
                col_indices <- round(seq(1, n_cols, length.out = max_fit_cols))
                best_mod_matrix <- best_mod_matrix[, col_indices, drop = FALSE]
              }
            }

            # Get color map from shared reactive
            cmap <- modifier_color_map()
            methods <- if (!is.null(cmap)) cmap$methods else character(0)
            bg_colors <- if (!is.null(cmap)) cmap$bg_colors else character(0)

            if (view_mode == "me") {
              # ME values view: show ME%, color-coded by best modifier
              me_list <- sfe_mod_output$sfe$Miscib_Enhancement
              pressures <- as.numeric(rownames(best_mod_matrix))
              temperatures <- as.numeric(colnames(best_mod_matrix))

              # Build color lookup from abbreviation to bg color
              color_lookup <- setNames(bg_colors, methods)

              # Build display matrix with inline HTML for colored cells
              display_matrix <- matrix("", nrow = length(pressures), ncol = length(temperatures))

              for (ri in seq_along(pressures)) {
                for (ci in seq_along(temperatures)) {
                  best_abbr <- best_mod_matrix[ri, ci]
                  me_key <- paste0("CO2 with ", best_abbr)
                  p_row <- as.character(pressures[ri])
                  t_col <- as.character(temperatures[ci])
                  me_val <- NA

                  if (!is.null(me_list[[me_key]])) {
                    me_data <- me_list[[me_key]]
                    if (p_row %in% rownames(me_data) && t_col %in% colnames(me_data)) {
                      me_val <- me_data[p_row, t_col]
                    }
                  }
                  # Fallback: search all ME entries for the best value
                  if (is.na(me_val)) {
                    for (me_name in names(me_list)) {
                      if (p_row %in% rownames(me_list[[me_name]]) && t_col %in% colnames(me_list[[me_name]])) {
                        val <- me_list[[me_name]][p_row, t_col]
                        if (!is.na(val) && (is.na(me_val) || val > me_val)) {
                          me_val <- val
                        }
                      }
                    }
                  }

                  val_text <- if (is.na(me_val)) "" else round(me_val, 2)
                  bg <- color_lookup[best_abbr]
                  if (!is.na(bg) && nchar(bg) > 0) {
                    display_matrix[ri, ci] <- paste0(
                      '<div style="background-color:', bg,
                      ';font-weight:bold;padding:4px;margin:-4px;text-align:center;">',
                      val_text, '</div>')
                  } else {
                    display_matrix[ri, ci] <- as.character(val_text)
                  }
                }
              }

              df <- as.data.frame(display_matrix, stringsAsFactors = FALSE)
              colnames(df) <- paste0(temperatures, " °C")
              rownames(df) <- paste0("<b>", pressures, " ", bar_label, "</b>")

              table_options <- if (display_mode == "fit") {
                list(
                  dom = "t", pageLength = nrow(df), paging = FALSE,
                  searching = FALSE, scrollX = FALSE, scrollY = FALSE, autoWidth = TRUE
                )
              } else {
                list(
                  dom = "t", pageLength = nrow(df), paging = FALSE,
                  searching = FALSE, scrollX = TRUE, scrollY = "400px", autoWidth = TRUE,
                  initComplete = htmlwidgets::JS(
                    "function(settings, json) {",
                    "  $(this.api().table().container()).find('td, th').css('white-space', 'nowrap');",
                    "}")
                )
              }

              datatable(df, options = table_options, rownames = TRUE, escape = FALSE)

            } else {
              # Solvent names view (original)
              df <- as.data.frame(best_mod_matrix)
              for (col_i in seq_len(ncol(df))) {
                df[[col_i]] <- sapply(df[[col_i]], translate_modifier)
              }

              translated_methods <- sapply(methods, translate_modifier)
              colnames(df) <- paste0(colnames(df), " °C")
              rownames(df) <- paste0("<b>", rownames(best_mod_matrix), " ", bar_label, "</b>")

              table_options <- if (display_mode == "fit") {
                list(
                  dom = "t", pageLength = nrow(df), paging = FALSE,
                  searching = FALSE, scrollX = FALSE, scrollY = FALSE, autoWidth = TRUE
                )
              } else {
                list(
                  dom = "t", pageLength = nrow(df), paging = FALSE,
                  searching = FALSE, scrollX = TRUE, scrollY = "400px", autoWidth = TRUE,
                  initComplete = htmlwidgets::JS(
                    "function(settings, json) {",
                    "  $(this.api().table().container()).find('td, th').css('white-space', 'nowrap');",
                    "}")
                )
              }

              datatable(
                df,
                options = table_options,
                rownames = TRUE,
                escape = FALSE
              ) %>%
                formatStyle(
                  columns = colnames(df),
                  backgroundColor = styleEqual(translated_methods, bg_colors),
                  color = "black", fontWeight = "bold"
                )
            }
          } else if (length(modif_list) == 1) {
            # Single modifier case (pure or blend)
            single_modifier_name <- if (length(modif_list[[1]]) > 1) {
              # Blend: vector of solvent names
              paste(sapply(modif_list[[1]], translate_modifier), collapse = ":")
            } else {
              translate_modifier(as.character(modif_list[[1]]))
            }

            pressures <- sort(unique(rv$miscibility_results$Pressure_bar))
            temperatures <- sort(unique(rv$miscibility_results$Temperature_C))

            if (display_mode == "fit") {
              if (length(pressures) > max_fit_rows) {
                pres_indices <- round(seq(1, length(pressures), length.out = max_fit_rows))
                pressures <- pressures[pres_indices]
              }
              if (length(temperatures) > max_fit_cols) {
                temp_indices <- round(seq(1, length(temperatures), length.out = max_fit_cols))
                temperatures <- temperatures[temp_indices]
              }
            }

            if (view_mode == "me") {
              # ME values for single modifier
              me_list <- sfe_mod_output$sfe$Miscib_Enhancement
              me_data <- me_list[[1]]  # Only one entry
              me_matrix <- matrix(NA, nrow = length(pressures), ncol = length(temperatures))
              for (ri in seq_along(pressures)) {
                for (ci in seq_along(temperatures)) {
                  p_row <- as.character(pressures[ri])
                  t_col <- as.character(temperatures[ci])
                  if (p_row %in% rownames(me_data) && t_col %in% colnames(me_data)) {
                    me_matrix[ri, ci] <- round(me_data[p_row, t_col], 2)
                  }
                }
              }
              df <- as.data.frame(me_matrix)
              colnames(df) <- paste0(temperatures, " °C")
              rownames(df) <- paste0("<b>", pressures, " ", bar_label, "</b>")

              table_options <- if (display_mode == "fit") {
                list(dom = "t", pageLength = nrow(df), paging = FALSE,
                  searching = FALSE, scrollX = FALSE, scrollY = FALSE, autoWidth = TRUE)
              } else {
                list(dom = "t", pageLength = nrow(df), paging = FALSE,
                  searching = FALSE, scrollX = TRUE, scrollY = "400px", autoWidth = TRUE,
                  initComplete = htmlwidgets::JS(
                    "function(settings, json) {",
                    "  $(this.api().table().container()).find('td, th').css('white-space', 'nowrap');",
                    "}"))
              }

              datatable(df, options = table_options, rownames = TRUE, escape = FALSE) %>%
                formatStyle(
                  columns = colnames(df),
                  backgroundColor = "#E41A1C80",
                  fontWeight = "bold"
                )
            } else {
              # Solvent names for single modifier
              single_mod_matrix <- matrix(single_modifier_name,
                nrow = length(pressures), ncol = length(temperatures)
              )
              df <- as.data.frame(single_mod_matrix)
              colnames(df) <- paste0(temperatures, " °C")
              rownames(df) <- paste0("<b>", pressures, " ", bar_label, "</b>")

              table_options <- if (display_mode == "fit") {
                list(dom = "t", pageLength = nrow(df), paging = FALSE,
                  searching = FALSE, scrollX = FALSE, scrollY = FALSE, autoWidth = TRUE)
              } else {
                list(dom = "t", pageLength = nrow(df), paging = FALSE,
                  searching = FALSE, scrollX = TRUE, scrollY = "400px", autoWidth = TRUE,
                  initComplete = htmlwidgets::JS(
                    "function(settings, json) {",
                    "  $(this.api().table().container()).find('td, th').css('white-space', 'nowrap');",
                    "}"))
              }

              datatable(df, options = table_options, rownames = TRUE, escape = FALSE) %>%
                formatStyle(
                  columns = colnames(df),
                  backgroundColor = "#E41A1C80",
                  color = "black", fontWeight = "bold"
                )
            }
          } else {
            DT::datatable(
              data.frame(Message = i18n$t("No Best Modifier data available.")),
              options = list(dom = "t", paging = FALSE, searching = FALSE, language = tablang())
            )
          }
        })



        output$miscibility_results_table <- DT::renderDataTable({
          req(rv$miscibility_results)

          # Create display copy with translated modifier names
          display_data <- rv$miscibility_results
          display_data$Modifier <- sapply(display_data$Modifier, translate_modifier)

          add_prettynames <- c(
            i18n_r()$t("Pressure (bar)"),
            i18n_r()$t("Temperature (°C)"),
            i18n_r()$t("Miscibility Enhancement (%)"),
            i18n_r()$t("Modifier"),
            i18n_r()$t("Pressure (MPa)"),
            i18n_r()$t("Temperature (K)")
          )

          DT::datatable(
            display_data,
            colnames = add_prettynames,
            extensions = "Buttons",
            options = list(
              scrollX = TRUE, paging = FALSE, language = tablang(),
              dom = "Bfrtip",
              buttons = create_dt_export_buttons(i18n_r, "supercrit_miscibility_results")
            ),
            rownames = FALSE,
            filter = "top"
          ) %>%
            DT::formatRound(columns = "ME_percent", digits = 2)
        })

        output$modifier_ranking_table <- DT::renderDataTable({
          req(rv$modifier_ranking)

          display_data <- rv$modifier_ranking
          display_data[[1]] <- sapply(display_data[[1]], translate_modifier)

          add_prettynames <- c(
            i18n_r()$t("Modifier"),
            i18n_r()$t("Occurrence (%)")
          )

          DT::datatable(
            display_data,
            colnames = add_prettynames,
            extensions = "Buttons",
            options = list(
              dom = "Bt", scrollX = TRUE, language = tablang(),
              buttons = create_dt_export_buttons(i18n_r, "supercrit_modifier_ranking")
            ),
            rownames = FALSE
          ) %>%
            DT::formatRound(columns = "Occurrence", digits = 2)
        })

        # Prepare data for Co-Solvent Comparison Table (store raw names)
        solv_list <- unique(rv$miscibility_results$Modifier)
        reslist <- list()
        for (i in solv_list) {
          min_val <- min(rv$miscibility_results$ME_percent[rv$miscibility_results$Modifier == i], na.rm = TRUE)
          max_val <- max(rv$miscibility_results$ME_percent[rv$miscibility_results$Modifier == i], na.rm = TRUE)
          reslist[[i]] <- c(i, round(min_val, 2), round(max_val, 2))
        }
        rv$miscibility_comparison_data <- setNames(
          do.call(rbind.data.frame, reslist),
          c("Co_solvent", "Min", "Max")
        )

        # Update co-solvent selection for Miscibility Enhancement tab
        rv$me_solvent_list <- solv_list
        updateSelectInput(session, "miscibility_enhancement_solvent_selection",
          choices = setNames(solv_list, sapply(solv_list, translate_modifier)),
          selected = solv_list[1]
        )
      }
    })
  })

  # Render Co-Solvent Comparison Table
  output$miscibility_comparison_table <- DT::renderDataTable({
    req(rv$miscibility_comparison_data)

    display_data <- rv$miscibility_comparison_data
    display_data[[1]] <- sapply(display_data[[1]], translate_modifier)

    add_prettynames <- c(
      i18n_r()$t("Co-solvent"),
      i18n_r()$t("Min"),
      i18n_r()$t("Max")
    )

    DT::datatable(
      display_data,
      colnames = add_prettynames,
      extensions = "Buttons",
      options = list(
        dom = "Bt", scrollX = TRUE, language = tablang(),
        buttons = create_dt_export_buttons(i18n_r, "supercrit_cosolvent_comparison")
      ),
      rownames = FALSE
    )
  })

  # Re-translate ME solvent dropdown when language changes
  observe({
    i18n_r()  # reactive dependency on language
    solv_list <- rv$me_solvent_list
    req(solv_list)
    current_sel <- input$miscibility_enhancement_solvent_selection
    updateSelectInput(session, "miscibility_enhancement_solvent_selection",
      choices = setNames(solv_list, sapply(solv_list, translate_modifier)),
      selected = if (!is.null(current_sel) && current_sel %in% solv_list) current_sel else solv_list[1]
    )
  })

  # Render Miscibility Enhancement Table for selected co-solvent
  output$miscibility_enhancement_table <- DT::renderDataTable({
    req(input$miscibility_enhancement_solvent_selection, rv$miscibility_results)
    bar_label <- i18n_r()$t("bar")

    selected_modifier <- input$miscibility_enhancement_solvent_selection

    # Filter data for the selected modifier
    filtered_data <- rv$miscibility_results[rv$miscibility_results$Modifier == selected_modifier, ]

    # Get unique pressures and temperatures
    pressures <- sort(unique(filtered_data$Pressure_bar))
    temperatures <- sort(unique(filtered_data$Temperature_C))

    # Create an empty matrix
    me_matrix <- matrix(NA,
      nrow = length(pressures), ncol = length(temperatures),
      dimnames = list(Pressure = pressures, Temperature = temperatures)
    )

    # Populate the matrix
    for (i in 1:nrow(filtered_data)) {
      p_idx <- which(pressures == filtered_data$Pressure_bar[i])
      t_idx <- which(temperatures == filtered_data$Temperature_C[i])
      me_matrix[p_idx, t_idx] <- round(filtered_data$ME_percent[i], 2)
    }

    df <- as.data.frame(me_matrix)
    colnames(df) <- paste0(temperatures, " °C")
    rownames(df) <- paste0("<b>", pressures, " ", bar_label, "</b>")

    DT::datatable(
      df,
      extensions = "Buttons",
      options = list(
        scrollX = TRUE, paging = FALSE, language = tablang(),
        dom = "Bfrtip",
        buttons = create_dt_export_buttons(i18n_r, "supercrit_miscibility_enhancement")
      ),
      rownames = TRUE,
      escape = FALSE
    )
  })

  # Observe Add to Comparison button
  observeEvent(input$add_to_comparison, {
    req(rv$solute_data, rv$miscibility_results, rv$modifier_ranking)

    # Store the current optimization result for multi-comparison
    comparison_entry <- list(
      solute_name = rv$solute_data$IDs$Name,
      miscibility_data = rv$miscibility_results,
      modifier_ranking = rv$modifier_ranking,
      timestamp = Sys.time()
    )

    # Add to a global reactiveValue or save to a temporary file for multi-comparison
    # Assuming sfe_rv$comparison_list is defined in app.R or a global scope
    isolate({
      if (is.null(sfe_rv$comparison_list)) {
        sfe_rv$comparison_list <- list()
      }
      sfe_rv$comparison_list[[length(sfe_rv$comparison_list) + 1]] <- comparison_entry
    })

    showNotification(i18n$t("Optimization results added to comparison list."), type = "message")
  })

  # Reactive to check if export button should be visible
  output$show_export_button <- reactive({
    !is.null(rv$sfe_mod_output)
  })
  outputOptions(output, "show_export_button", suspendWhenHidden = FALSE)

  # Download handler for export button
  output$export_results <- downloadHandler(
    filename = function() {
      req(rv$sfe_mod_output)
      solute_name <- rv$sfe_mod_output$solute_ids["Name"]
      solute_cas <- rv$sfe_mod_output$solute_ids["CAS"]
      base_name <- paste0("HSP_", solute_name, "_", solute_cas)
      paste0(generate_filename_with_timestamp(base_name), ".zip")
    },
    content = function(file) {
      req(rv$sfe_mod_output)

      # Show progress notification
      progress_id <- showNotification(
        i18n$t("Preparing export... Please wait."),
        type = "message",
        duration = NULL,
        closeButton = FALSE
      )
      on.exit(removeNotification(progress_id), add = TRUE)

      tryCatch({
        # Create temporary directory for export
        temp_dir <- file.path(tempdir(), paste0("hsp_export_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S")))
        dir.create(temp_dir, recursive = TRUE)

        # Call hsp_export function (writes .tab file with tab separator)
        hsp_export(
          input = rv$sfe_mod_output,
          addres = NA,
          plotpars = "default",
          plot_format = "png",
          expath = temp_dir,
          silent = TRUE
        )

        # Find the created folder (it has timestamp in name)
        exported_folders <- list.dirs(temp_dir, full.names = TRUE, recursive = FALSE)
        hsp_folder <- exported_folders[grepl("^HSP_", basename(exported_folders))]

        if (length(hsp_folder) == 0) stop("Export folder not created")
        latest_folder <- hsp_folder[which.max(file.info(hsp_folder)$mtime)]

        # Post-process .tab files: convert to .csv, translate headers, add BOM
        tab_files <- list.files(latest_folder, pattern = "\\.tab$", full.names = TRUE)
        for (tab_file in tab_files) {
          original_bytes <- readBin(tab_file, "raw", file.info(tab_file)$size)
          content <- rawToChar(original_bytes)
          Encoding(content) <- "UTF-8"

          # Replace tab delimiters with commas; quote fields that contain commas
          lines <- strsplit(content, "\n", fixed = TRUE)[[1]]
          csv_lines <- vapply(lines, function(line) {
            if (!grepl("\t", line, fixed = TRUE)) return(line)
            fields <- strsplit(line, "\t", fixed = TRUE)[[1]]
            fields <- vapply(fields, function(f) {
              if (grepl(",", f, fixed = TRUE)) paste0('"', f, '"') else f
            }, character(1))
            paste(fields, collapse = ",")
          }, character(1), USE.NAMES = FALSE)
          content <- paste(csv_lines, collapse = "\n")

          # --- Contrib section name translations ---
          # Pattern: "Tb GCM contibutions (method: SB_corr)" etc.
          # Target parameter name translations
          contrib_param_translations <- list(
            "Tb" = i18n$t("GCM contributions to boiling point"),
            "Critical" = i18n$t("GCM contributions to critical parameters"),
            "HSP" = i18n$t("GCM contributions to Hansen Solubility Parameters"),
            "VDW" = i18n$t("GCM contributions to Van der Waals volume")
          )
          # Method code -> descriptive name translations (longer codes first to prevent partial matches)
          method_name_translations <- list(
            "JR_corr" = i18n$t("Joback-Reid corrected"),
            "JR" = i18n$t("Joback-Reid"),
            "SB_corr" = i18n$t("Stein-Brown corrected"),
            "SB" = i18n$t("Stein-Brown"),
            "NL07_robust" = i18n$t("Nannoolal 2007 robust"),
            "NL07" = i18n$t("Nannoolal 2007"),
            "NL04" = i18n$t("Nannoolal 2004"),
            "HKR_STW" = i18n$t("Hukkerikar-Stepwise"),
            "HKR_SIM" = i18n$t("Hukkerikar-Simultaneous"),
            "SP08" = i18n$t("Stefanis-Panayiotou 2008"),
            "SP12" = i18n$t("Stefanis-Panayiotou 2012"),
            "ZHAO" = i18n$t("Zhao 2003"),
            "BND" = i18n$t("Bondi 1964"),
            "SLON" = i18n$t("Slonimskii 1970")
          )
          # Replace each "ParamName GCM contibutions (method: CODE)" pattern
          for (param_en in names(contrib_param_translations)) {
            for (code in names(method_name_translations)) {
              old_str <- paste0(param_en, " GCM contibutions (method: ", code, ")")
              new_str <- paste0(contrib_param_translations[[param_en]],
                                " (", i18n$t("method"), ": ", method_name_translations[[code]], ")")
              content <- gsub(old_str, new_str, content, fixed = TRUE)
            }
          }

          # --- GCM METHODS UTILIZED section: translate param names and method codes ---
          # Structure: "GCM METHODS UTILIZED\nTb,Critical,...\nJR_corr,SP12,...\n"
          # Line 2 = parameter names (Tb, Critical, HSP, VDW), Line 3 = method codes
          # NOTE: header translations haven't been applied yet, so search for English original
          methods_pos <- regexpr("GCM METHODS UTILIZED", content, fixed = TRUE)
          if (methods_pos > 0) {
            after_header <- substring(content, methods_pos + nchar("GCM METHODS UTILIZED"))
            method_lines <- strsplit(after_header, "\n", fixed = TRUE)[[1]]
            if (length(method_lines) >= 3) {
              # Translate parameter names row (Tb, Critical, HSP, VDW)
              param_names_line <- method_lines[2]
              param_short_translations <- list(
                "Critical" = i18n$t("Critical Parameters"),
                "HSP" = i18n$t("Hansen Solubility Parameters"),
                "VDW" = i18n$t("Van der Waals Volume"),
                "Tb" = i18n$t("Boiling Point")
              )
              translated_params <- param_names_line
              for (pn in names(param_short_translations)) {
                translated_params <- gsub(
                  paste0("(?<![A-Za-z0-9_])", pn, "(?![A-Za-z0-9_])"),
                  param_short_translations[[pn]],
                  translated_params, perl = TRUE
                )
              }
              if (param_names_line != translated_params) {
                content <- sub(param_names_line, translated_params, content, fixed = TRUE)
              }
              # Translate method codes row (JR_corr, SP12, etc.)
              values_line <- method_lines[3]
              translated_line <- values_line
              for (code in names(method_name_translations)) {
                translated_line <- gsub(
                  paste0("(?<![A-Za-z0-9_])", code, "(?![A-Za-z0-9_])"),
                  method_name_translations[[code]],
                  translated_line, perl = TRUE
                )
              }
              if (values_line != translated_line) {
                content <- sub(values_line, translated_line, content, fixed = TRUE)
              }
            }
          }

          # --- Column name translations for contrib tables and parameter headers ---
          # Plain-text scientific notation matching Solute Characterization UI
          # ORDERING IS CRITICAL: longest names first to prevent partial matches
          # NOTE: commas avoided in replacements since this is CSV data
          col_translations <- list(
            # Common columns
            "Group" = i18n$t("Group"),
            "Type" = i18n$t("Type"),
            "SMARTS" = "SMARTS",
            "Occurrences" = i18n$t("Occurrences"),
            # Sum_*_low_cont (longest first)
            "Sum_dP_low_cont" = "\u03a3\u03b4P (low)",
            "Sum_dHB_low_cont" = "\u03a3\u03b4HB (low)",
            # Sum_*_cont
            "Sum_Tb_cont" = "\u03a3Tb",
            "Sum_Tc_cont" = "\u03a3Tc",
            "Sum_Pc_cont" = "\u03a3Pc",
            "Sum_Vc_cont" = "\u03a3Vc",
            "Sum_dD_cont" = "\u03a3\u03b4D",
            "Sum_dP_cont" = "\u03a3\u03b4P",
            "Sum_dHB_cont" = "\u03a3\u03b4HB",
            "Sum_VDW_cont" = "\u03a3VDW",
            # *_low_cont
            "dP_low_cont" = "\u03b4P (low)",
            "dHB_low_cont" = "\u03b4HB (low)",
            # *_cont
            "Tb_cont" = "Tb",
            "Tc_cont" = "Tc",
            "Pc_cont" = "Pc",
            "Vc_cont" = "Vc",
            "dD_cont" = "\u03b4D",
            "dP_cont" = "\u03b4P",
            "dHB_cont" = "\u03b4HB",
            "VDW_cont" = "VDW",
            # Raw parameter names (from crit_vec/hsp_vec headers): *_low before bare
            "dP_low" = "\u03b4P (low)",
            "dHB_low" = "\u03b4HB (low)",
            "dD" = "\u03b4D",
            "dP" = "\u03b4P",
            "dHB" = "\u03b4HB"
          )
          # Always replace — these are scientific symbols, not language-dependent
          # Use word-boundary lookaround to match column names at any position
          for (col_en in names(col_translations)) {
            col_tr <- col_translations[[col_en]]
            content <- gsub(
              paste0("(?<![A-Za-z0-9_])", col_en, "(?![A-Za-z0-9_])"),
              col_tr,
              content, perl = TRUE
            )
          }

          # --- Solvent name translations ---
          # Build complete translation map: full names, abbreviations
          # All maps use modifier_values/modifier_abbrevs defined at module level

          # 1. Replace "CO2 with " prefix → "CO2 + " (simple, no solvent name involved)
          content <- gsub("CO2 with ", "CO2 + ", content, fixed = TRUE)

          # 2. Bare full names (e.g. "Acetone", "DiethylEther") and abbreviations (e.g. "EtOH", "Tol")
          # Sort by length descending to prevent partial matches
          bare_map <- list()
          for (i in seq_along(modifier_values)) {
            full <- modifier_values[i]
            tr <- translate_solvent_token(full, i18n)
            if (full != tr) bare_map[[full]] <- tr
          }
          for (i in seq_along(modifier_abbrevs)) {
            abbr <- modifier_abbrevs[i]
            tr <- translate_solvent_token(abbr, i18n)
            if (abbr != tr) bare_map[[abbr]] <- tr
          }
          # Sort by key length descending
          bare_map <- bare_map[order(nchar(names(bare_map)), decreasing = TRUE)]

          # Apply bare names using word-boundary lookaround (not consuming delimiters)
          for (en_text in names(bare_map)) {
            content <- gsub(
              paste0("(?<![A-Za-z])", en_text, "(?![A-Za-z])"),
              bare_map[[en_text]],
              content, perl = TRUE
            )
          }

          # --- Section header translations ---
          header_translations <- list(
            "SOLUTE: " = paste0(i18n$t("SOLUTE"), ": "),
            "GCM FRAGMENTATION RESULTS" = i18n$t("GCM FRAGMENTATION RESULTS"),
            "BP AND CRITICAL PARAMETER CALCULATION RESULTS FROM GCM" = i18n$t("BP AND CRITICAL PARAMETER CALCULATION RESULTS FROM GCM"),
            "HANSEN SOLUBILITY PARAMETER (HSP) CALCULATION RESULTS FROM GCM" = i18n$t("HANSEN SOLUBILITY PARAMETER (HSP) CALCULATION RESULTS FROM GCM"),
            "GCM METHODS UTILIZED" = i18n$t("GCM METHODS UTILIZED"),
            "GCM-BASED ESTIMATION OF BOILING POINT AND OTHER PARAMETERS WAS NOT CARRIED OUT." = i18n$t("GCM-BASED ESTIMATION OF BOILING POINT AND OTHER PARAMETERS WAS NOT CARRIED OUT."),
            "SFE MODIFIER OPTIMIZATION RESULTS" = i18n$t("SFE MODIFIER OPTIMIZATION RESULTS"),
            "SELECTION OF THE BEST scCO2 SFE MODIFIER AMONGST: " = paste0(i18n$t("SELECTION OF THE BEST scCO2 SFE MODIFIER AMONGST"), ": "),
            "SOLUTE HSP VERSUS TEMPERATURE" = i18n$t("SOLUTE HSP VERSUS TEMPERATURE"),
            "CO2-MODIFIER BLEND HSP VERSUS TEMPERATURE AND PRESSURE (MODIFIER VOL. FRACTION: " = paste0(i18n$t("CO2-MODIFIER BLEND HSP VERSUS TEMPERATURE AND PRESSURE (MODIFIER VOL. FRACTION"), ": "),
            "HSP DISTANCE Ra BETWEEN SOLUTE AND SOLVENT BLENDS (LOWER = BETTER MISCIBILITY)" = i18n$t("HSP DISTANCE Ra BETWEEN SOLUTE AND SOLVENT BLENDS (LOWER = BETTER MISCIBILITY)"),
            "MISCIBILITY ENHANCEMENT (%) WITH DIFFERENT SOLVENT BLENDS (HIGHER = BETTER)" = i18n$t("MISCIBILITY ENHANCEMENT (%) WITH DIFFERENT SOLVENT BLENDS (HIGHER = BETTER)"),
            "BEST SOLVENT OUT OF SELECTION: " = paste0(i18n$t("BEST SOLVENT OUT OF SELECTION"), ": "),
            "SOLVENT RANKING (HIGHER = BETTER)" = i18n$t("SOLVENT RANKING (HIGHER = BETTER)"),
            "SFE MODIFIER OPTIMIZATION WAS NOT CARRIED OUT." = i18n$t("SFE MODIFIER OPTIMIZATION WAS NOT CARRIED OUT.")
          )

          # --- Data-level translations ---
          data_translations <- list(
            " bar" = paste0(" ", i18n$t("bar"))
          )

          for (en_text in names(header_translations)) {
            content <- gsub(en_text, header_translations[[en_text]], content, fixed = TRUE)
          }
          for (en_text in names(data_translations)) {
            content <- gsub(en_text, data_translations[[en_text]], content, fixed = TRUE)
          }

          # Write as .csv with UTF-8 BOM
          csv_file <- sub("\\.tab$", ".csv", tab_file)
          con <- file(csv_file, "wb")
          writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con)
          writeBin(charToRaw(content), con)
          close(con)

          # Remove original .tab file
          file.remove(tab_file)
        }

        # Create zip from the export folder
        files_to_zip <- list.files(latest_folder, full.names = TRUE, recursive = TRUE)
        zip::zip(zipfile = file, files = files_to_zip, mode = "cherry-pick")

        showNotification(i18n$t("Export completed successfully!"), type = "message")
      }, error = function(e) {
        showNotification(paste(i18n$t("Error exporting results:"), e$message), type = "error")
      }, finally = {
        unlink(temp_dir, recursive = TRUE)
      })
    },
    contentType = "application/zip"
  )

  # Render HELP output
  output$sfe_misc_opt_HELP <- renderUI({
    create_help_modal(i18n_r, "sfe_misc_opt_help_en")
  })

  # Render accordion expand/collapse button with translated tooltips
  output$accordion_toggle_btn <- renderUI({
    create_accordion_toggle_btn(ns, i18n_r, "opt_accordion")
  })
  # Reset function: Clear results and hide export button
  observeEvent(input$reset, {
    rv$miscibility_results <- NULL
    rv$modifier_ranking <- NULL
    rv$solute_data <- NULL
    rv$gcm_results <- NULL
    rv$sfe_mod_output <- NULL

    # Clear loaded calculation parameters
    rv$loaded_calculation <- NULL
    rv$loaded_cas <- NULL
    rv$loaded_name <- NULL
    rv$loaded_smiles <- NULL
    rv$loaded_specify_smiles <- NULL
    rv$loaded_tb <- NULL
    rv$loaded_tb_manual_input <- NULL
    rv$loaded_crit <- NULL
    rv$loaded_hsp <- NULL
    rv$loaded_simplicity <- NULL
    rv$loaded_gorder <- NULL

    # Clear radio button selection
    rv_selected_calc_id(NULL)
    updateRadioButtons(session, "saved_calculation_radio", selected = character(0))

    # Reset modifier selection
    updatePickerInput(session, "modifier_selection", selected = character(0))

    # Reset pressure, temperature, and vfrac inputs to defaults
    updateNumericInput(session, "vfrac_input", value = defaults$vfrac * 100)
    updateNumericInput(session, "pressure_min", value = defaults$pres_min)
    updateNumericInput(session, "pressure_max", value = defaults$pres_max)
    updateNumericInput(session, "pressure_step", value = defaults$pres_step)
    updateNumericInput(session, "temperature_min", value = defaults$temp_min)
    updateNumericInput(session, "temperature_max", value = defaults$temp_max)
    updateNumericInput(session, "temperature_step", value = defaults$temp_step)
    updateRadioButtons(session, "table_display_mode", selected = defaults$table_display_mode)

    # Clear saved blends
    rv$saved_blends <- list()

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

    showNotification(i18n$t("Miscibility optimization reset to defaults"), type = "message")
  })

  # return(rv) # Return reactive values to app.R for inter-module communication

  # suspendWhenHidden for renderUI outputs (auto-added)
  outputOptions(output, "accordion_toggle_btn", suspendWhenHidden = FALSE)
  outputOptions(output, "best_table_legend_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "best_table_view_toggle_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "blend_total_display", suspendWhenHidden = FALSE)
  outputOptions(output, "current_blend_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "saved_blends_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "saved_calculations_list_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "sfe_misc_opt_HELP", suspendWhenHidden = FALSE)

}
