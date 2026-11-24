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

  # Extra co-solvents available ONLY to the COSMO-SAC-Phi EoS engines (CSP / openCOSMO-RS-Phi), which resolve
  # solvents BY NAME against their own databases (not the Hansen swex set HSP mode is limited to). Mirrors the
  # Co-Solvent Optimization module.
  eos_extra_solvents <- data.frame(
    value  = c("Isopropanol","Ethyl acetate","Tetrahydrofuran","Acetonitrile","Dimethyl sulfoxide",
               "Methyl ethyl ketone","Dichloromethane","Chloroform","Methyl acetate","1-Octanol"),
    smiles = c("CC(C)O","CCOC(C)=O","C1CCOC1","CC#N","CS(C)=O","CCC(C)=O","C(Cl)Cl","C(Cl)(Cl)Cl","COC(C)=O","CCCCCCCCO"),
    stringsAsFactors = FALSE)
  # Hansen modifiers that ALSO resolve in CSP (Heptane/Hexane/MethylOleate absent from CSP -> excluded).
  eos_hansen_keep <- c("Acetone","Benzene","Cyclohexane","DiethylEther","Ethanol","Methanol",
                       "Toluene","PXylene","OXylene","Water")
  # Active co-solvent set (value/label/smiles): HSP + Makarov use the Hansen swex solvents; the EoS engines
  # additionally offer the curated CSP-resolvable co-solvents. Mode = comp_method + sol_engine.
  active_cosolv <- reactive({
    sv <- show_solv(); smi_of <- setNames(sv$SMILES, sv$Solvent)
    is_eos <- identical(input$comp_method %||% "hsp", "solubility") &&
              (input$sol_engine %||% "csp") %in% c("csp", "ocrsphi")
    if (is_eos) {
      keep <- modifier_values %in% eos_hansen_keep
      rbind(
        data.frame(value = modifier_values[keep], label = modifier_display_keys[keep],
                   smiles = unname(smi_of[modifier_values[keep]]), stringsAsFactors = FALSE),
        data.frame(value = eos_extra_solvents$value, label = eos_extra_solvents$value,
                   smiles = eos_extra_solvents$smiles, stringsAsFactors = FALSE))
    } else {
      data.frame(value = modifier_values, label = modifier_display_keys,
                 smiles = unname(smi_of[modifier_values]), stringsAsFactors = FALSE)
    }
  })
  # Translated (value -> label) choices for a co-solvent picker from the active, mode-aware set.
  cosolv_choices <- function() { ac <- active_cosolv()
    setNames(ac$value, vapply(ac$label, function(k) i18n_r()$t(k), character(1))) }

  # Helper: translate a single solvent token (full name or abbreviation)
  translate_solvent_token <- function(token, i18n_fn) {
    token <- trimws(token)
    key_map <- setNames(modifier_display_keys, modifier_values)
    if (token %in% names(key_map)) return(i18n_fn$t(key_map[[token]]))
    if (token == "CO2") return("CO2")
    return(token)
  }

  # Warn (but still compute) when a manual blend's components are not fully miscible (Clavilier 2025 +
  # curated screen). Same wording as the Co-Solvent Optimization module. `solvents` = untranslated keys.
  warn_blend_misc <- function(solvents) {
    mm <- tryCatch(blend_miscibility(solvents), error = function(e) NULL)
    if (is.null(mm) || isTRUE(mm$miscible)) return(invisible())
    nm <- paste(solvents, collapse = " + ")
    showNotification(
      if (length(mm$reaction_pairs)) sprintf(i18n$t("Blend '%s': its solvents react or interact on contact; the result is shown but treat it with caution."), nm)
      else if (length(mm$immiscible_pairs)) sprintf(i18n$t("Blend '%s': its solvents are immiscible (phase-separating); the predicted blend may not form a single phase."), nm)
      else sprintf(i18n$t("Blend '%s': its solvents are only partially miscible; the blend may not form a single phase at all ratios."), nm),
      type = "warning", duration = 8)
    invisible()
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
    default_pres <- as.numeric(strsplit(defaults$pres_input, ",")[[1]]); default_pres <- default_pres[!is.na(default_pres)]
    current_val <- if (!is.null(input$pres_input)) input$pres_input else as.character(default_pres)
    # Shared range-badge input so the BADGE (not just the tags) turns red when a value is out of range.
    range_badge_multi_input(ns, i18n_r, "pres_input", i18n_r()$t("Pressures (bar)"), current_val,
      min_val = 75, max_val = 1000, max_items = 6,
      range_text = paste0("75–1000 ", i18n_r()$t("bar"), ", ", i18n_r()$t("max"), " 6"),
      tooltip = paste0("75 – 1000 ", i18n_r()$t("bar"), ", ", i18n_r()$t("max"), " 6 ", i18n_r()$t("values")),
      placeholder = i18n_r()$t("Type pressure values..."),
      help_content = i18n_r()$t("Select one or more pressures at which to compare co-solvent miscibility. The comparison is evaluated at each pressure–temperature combination."),
      help_title = i18n_r()$t("Pressures"))
  })
  outputOptions(output, "pres_input_ui", suspendWhenHidden = FALSE)

  # Up to 6 temperatures in BOTH modes (HSP miscibility and ML solubility both gain a temperature
  # dimension). Defaults to a single value; add more to compare across temperatures.
  output$temp_ui <- renderUI({
    cur <- if (!is.null(input$temp)) input$temp else as.character(defaults$temp %||% 40)
    range_badge_multi_input(ns, i18n_r, "temp", i18n_r()$t("Temperatures (\u00B0C)"), cur,
      min_val = 31, max_val = 200, max_items = 6,
      range_text = paste0("31\u2013200 \u00B0C, ", i18n_r()$t("max"), " 6"),
      tooltip = paste0("31 \u2013 200 \u00B0C, ", i18n_r()$t("max"), " 6 ", i18n_r()$t("values")),
      placeholder = i18n_r()$t("Type temperature values..."),
      help_content = i18n_r()$t("One or more extraction temperatures for the comparison. Each must be above 31\u00B0C (the critical temperature of CO2)."),
      help_title = i18n_r()$t("Temperature"))
  })
  outputOptions(output, "temp_ui", suspendWhenHidden = FALSE)

  output$vfrac_input_ui <- renderUI({
    default_vfrac <- as.numeric(strsplit(defaults$vfrac_input, ",")[[1]])
    default_vfrac <- default_vfrac[!is.na(default_vfrac)]
    cur <- if (!is.null(input$vfrac_input)) input$vfrac_input else as.character(default_vfrac)
    # Shared range-badge input so the BADGE (not just the tags) turns red when a value is out of range.
    range_badge_multi_input(ns, i18n_r, "vfrac_input", i18n_r()$t("Volume Fractions"), cur,
      min_val = 0.01, max_val = 0.99, tooltip = i18n_r()$t("0.01 - 0.99 (1% - 99%)"),
      placeholder = i18n_r()$t("Type volume fractions..."),
      help_content = i18n_r()$t("Select one or more co-solvent volume fractions (%) at which to compare miscibility. Each fraction is evaluated separately."),
      help_title = i18n_r()$t("Volume Fractions"))
  })
  outputOptions(output, "vfrac_input_ui", suspendWhenHidden = FALSE)

  # Method selector (HSP miscibility vs ML solubility) and the Solubility-mode mole-fraction input.
  output$comp_method_ui <- renderUI({
    selectInput(ns("comp_method"),
      label = tags$span(i18n_r()$t("Comparison Method"),
        input_help(i18n_r()$t("HSP (Miscibility) compares solutes by Hansen solubility-parameter distance. Solubility compares them by the predicted enhancement of each solute's scCO₂ solubility (and reports the absolute solubility). Solubility mode needs each solute to be in the selected engine's database (CSP-2021 / openCOSMO-RS-Phi)."),
                   title = i18n_r()$t("Comparison Method"), buttonLabel = i18n_r()$t("OK"))),
      choices = setNames(c("hsp", "solubility"),
        c(i18n_r()$t("HSP (Miscibility)"), i18n_r()$t("Solubility"))),
      selected = if (!is.null(input$comp_method)) input$comp_method else "hsp", width = "100%")
  })
  outputOptions(output, "comp_method_ui", suspendWhenHidden = FALSE)

  # Solubility engines (csp, ocrsphi) that can compute EVERY currently-selected solute. Because ineligible
  # solutes are disabled in the picker, the ACTIVE engine always covers the selection, so it is never
  # removed; the OTHER engine is offered only when it too covers every selected solute. With nothing
  # selected, both are offered. (A comparison is single-engine — the two disagree ~6x on absolute
  # solubility — so per-solute mixing is never allowed; this just hides an engine that can't do the set.)
  comp_engine_keys <- reactive({
    all_keys <- c("csp", "ocrsphi")
    if (!identical(input$comp_method %||% "hsp", "solubility")) return(all_keys)
    calcs <- sfe_rv$saved_calculations
    sel <- rv$selected_saved_calc_ids
    if (!length(calcs) || !length(sel)) return(all_keys)
    idx <- suppressWarnings(as.integer(sel)); idx <- idx[!is.na(idx) & idx >= 1 & idx <= length(calcs)]
    if (!length(idx)) return(all_keys)
    nms <- vapply(calcs[idx], function(c) c$parameters$name_input %||% "", character(1))
    smi <- vapply(calcs[idx], function(c) c$parameters$smiles_input %||% "", character(1))
    keys <- all_keys[c(all(nzchar(nms) & db_engine_eligible(nms, "csp", smi)),
                       all(nzchar(nms) & db_engine_eligible(nms, "ocrsphi", smi)))]
    if (!length(keys)) all_keys else keys
  })

  # Engine selector — choices restricted to comp_engine_keys() so an engine that cannot compute every
  # selected solute disappears. Selection preserved across re-renders (isolate) and never points at a
  # removed key. Separate from the mole-fraction input so selection changes don't re-render the badge.
  output$comp_engine_ui <- renderUI({
    keys <- comp_engine_keys()
    labs <- c(csp = i18n_r()$t("CSP-2021 (in database)"), ocrsphi = i18n_r()$t("openCOSMO-RS-Phi (in database)"))
    sel <- isolate(input$sol_engine); if (is.null(sel) || !sel %in% keys) sel <- keys[[1]]
    selectInput(ns("sol_engine"),
      label = tags$span(i18n_r()$t("Solubility Engine"),
        input_help(i18n_r()$t("CSP-2021 and openCOSMO-RS-Phi are predictive COSMO-SAC-Phi equations of state; each needs the solute(s) AND every co-solvent to be in its database and has no hard T/P domain. Being genuine n-component models, they capture true co-solvent enhancement and can also compare blends. (The Makarov ML model is not offered for co-solvent comparison — its co-solvent sensitivity is too weak to rank solvents reliably; it remains available for pure-CO₂ solubility in the Separator Cascade module.)"),
                   title = i18n_r()$t("Solubility Engine"), buttonLabel = i18n_r()$t("OK"))),
      choices = setNames(keys, unname(labs[keys])), selected = sel, width = "100%")
  })
  outputOptions(output, "comp_engine_ui", suspendWhenHidden = FALSE)

  output$comp_molfrac_ui <- renderUI({
    # Multiple mole fractions (mirrors HSP's Volume Fractions) — expressed as FRACTIONS (0.01-0.99),
    # not percentages, to match the volume-fraction convention. Range badge (and tags) turn red out of
    # range. Defaults 0.05..0.40.
    cur <- if (!is.null(input$comp_molfrac)) input$comp_molfrac
           else c("0.05", "0.10", "0.15", "0.20", "0.25", "0.30", "0.35", "0.40")
    range_badge_multi_input(ns, i18n_r, "comp_molfrac", i18n_r()$t("Co-Solvent Mole Fractions"), cur,
      min_val = 0.01, max_val = 0.99, placeholder = i18n_r()$t("Type mole fractions..."),
      help_content = i18n_r()$t("One or more co-solvent mole fractions (0.01-0.99) at which to compare solubility. Each fraction is evaluated separately. Typical loadings are 0.01-0.10; the model was trained up to ~0.50."),
      help_title = i18n_r()$t("Mole Fractions"))
  })
  outputOptions(output, "comp_molfrac_ui", suspendWhenHidden = FALSE)

  output$comp_unit_ui <- renderUI({
    radioButtons(ns("comp_unit"),
      label = tags$span(i18n_r()$t("Solubility Unit"),
        input_help(i18n_r()$t("Unit for the absolute solubility shown in the results table and plot: mole fraction (mol/mol) or grams of solute per kilogram of CO2 (g/kg). The enhancement factor is unitless."),
                   title = i18n_r()$t("Solubility Unit"), buttonLabel = i18n_r()$t("OK"))),
      choices = setNames(c("molfrac", "gkg"), c(i18n_r()$t("mol/mol"), i18n_r()$t("g/kg"))),
      selected = "molfrac", inline = TRUE)
  })
  outputOptions(output, "comp_unit_ui", suspendWhenHidden = FALSE)

  # Engine-coverage helper (Solubility mode only): how many of the loaded solutes each COSMO-SAC-Phi engine
  # can compute, so the user picks the engine that keeps the most solutes comparable. A comparison uses ONE
  # engine (the two disagree by ~6x on absolute solubility, so per-solute mixing would be misleading).
  output$engine_coverage_ui <- renderUI({
    if (!identical(input$comp_method %||% "hsp", "solubility")) return(NULL)
    calcs <- sfe_rv$saved_calculations
    if (!length(calcs)) return(NULL)
    nms_v <- vapply(calcs, function(c) c$parameters$name_input %||% "", character(1))
    smi_v <- vapply(calcs, function(c) c$parameters$smiles_input %||% "", character(1))
    keep <- nzchar(nms_v); n <- sum(keep)
    if (!n) return(NULL)
    n_csp <- sum(keep & db_engine_eligible(nms_v, "csp", smi_v))
    n_ocr <- sum(keep & db_engine_eligible(nms_v, "ocrsphi", smi_v))
    tags$small(class = "text-muted", style = "display:block; margin-top:2px; margin-bottom:8px;",
      sprintf(i18n_r()$t("Engine coverage of %d loaded solute(s): CSP-2021 %d, openCOSMO-RS-Phi %d. Each comparison uses a single engine."),
              n, n_csp, n_ocr))
  })
  outputOptions(output, "engine_coverage_ui", suspendWhenHidden = FALSE)

  # Parallel-evaluation toggle (Solubility mode only; on by default). Spreads the solute x fraction
  # solub_optim jobs across cores. Shown in the "Advanced Options" accordion via uiOutput(sol_parallel_ui).
  output$sol_parallel_ui <- renderUI({
    if (!identical(input$comp_method, "solubility")) return(NULL)
    checkboxInput(ns("sol_parallel"),
      label = tags$span(i18n_r()$t("Parallel evaluation"),
        input_help(i18n_r()$t("Spreads the (independent) solubility calculations across multiple CPU cores to speed up the slow COSMO-SAC-Phi (CSP-2021 / openCOSMO-RS-Phi) comparison: the solute × co-solvent-fraction evaluations are split across cores. Each worker is a separate process, so results are numerically identical to a single-core run; only the wall-clock time changes. Turn this off to run on a single core (e.g. to keep CPU free for other work)."),
                   title = i18n_r()$t("Parallel evaluation"), buttonLabel = i18n_r()$t("OK"))),
      value = isolate(input$sol_parallel) %||% TRUE)
  })
  outputOptions(output, "sol_parallel_ui", suspendWhenHidden = FALSE)

  # Rendered radioButtons with translated modifier type choices. Blends are available in BOTH modes:
  # HSP mixes Hansen parameters by volume fraction; Solubility (ML) uses mole-fraction feature
  # superposition in predict_solubility (an extrapolation of a single-co-solvent-trained model — reliable
  # for ranking, indicative in absolute value; a note flags this in ML mode).
  output$modifier_type_ui <- renderUI({
    ml_note <- if (identical(input$comp_method, "solubility"))
      tags$small(class = "text-muted", style = "display:block; margin-top:4px;",
        i18n_r()$t("Blends are compared with the selected COSMO-SAC-Phi EoS engine (CSP-2021 / openCOSMO-RS-Phi) as genuine n-component mixtures.")) else NULL
    tagList(
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
      ),
      ml_note
    )
  })
  outputOptions(output, "modifier_type_ui", suspendWhenHidden = FALSE)

  # Rendered pickerInput with translated modifier names for pure modifier selection
  output$modifier_selection_ui <- renderUI({
    choices <- cosolv_choices()   # mode-aware: base Hansen set, or the extended CSP set for the EoS engines
    pickerInput(
      inputId = ns("modifier_selection"),
      label = tags$span(i18n_r()$t("Select Modifier"),
        input_help(i18n_r()$t("Choose the co-solvent to evaluate. The HSP distance and miscibility enhancement will be compared against pure CO2 for each selected solute."),
                   title = i18n_r()$t("Modifier"), buttonLabel = i18n_r()$t("OK"))),
      choices = choices,
      selected = if (isTRUE(defaults$modifier_selection %in% choices)) defaults$modifier_selection else unname(choices)[1],
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
  # "Load from Database" button — inline with Load Examples / Load from Disk. Offered for HSP only
  # (ML mode needs just a SMILES). Each load adds a checkbox below; select the ones to compare.
  output$db_load_btn <- renderUI({
    # Available in BOTH modes — the DB modal filters compounds by method (HSP needs δD/δP/δHB + Tc;
    # Solubility (ML) needs a SMILES), so ML-suitable compounds are offered in Solubility mode too.
    actionButton(ns("open_db_modal"), i18n_r()$t("Load from Database"), icon = icon("database"),
      class = "btn btn-success btn-sm", style = "color:white; margin-left: 5px;")
  })
  outputOptions(output, "db_load_btn", suspendWhenHidden = FALSE)

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

    # Database-limited EoS engines (csp/ocrsphi) in Solubility mode: grey out + disable solutes not in that
    # engine's database (HSP / Makarov keep every calc selectable, as before).
    method <- input$comp_method %||% "hsp"
    if (identical(method, "solubility") && (input$sol_engine %||% "csp") %in% c("csp", "ocrsphi")) method <- input$sol_engine
    eos <- method %in% c("csp", "ocrsphi")
    nms_v <- vapply(calcs, function(c) c$parameters$name_input %||% "", character(1))
    smi_v <- vapply(calcs, function(c) c$parameters$smiles_input %||% "", character(1))
    ok <- if (eos) (nzchar(nms_v) & db_engine_eligible(nms_v, method, smi_v)) else rep(TRUE, length(calcs))
    # For greyed-out solutes, distinguish "available in the OTHER engine — switch to use it" from "in
    # neither COSMO-SAC-Phi database". A comparison must stay on ONE engine (CSP-2021 and openCOSMO-RS-Phi
    # disagree by ~6x on absolute solubility, so mixing them per-solute would be misleading), so the hint
    # points the user to the engine that covers the solute rather than silently dropping it.
    other      <- if (identical(method, "csp")) "ocrsphi" else "csp"
    other_disp <- if (identical(other, "csp")) "CSP-2021" else "openCOSMO-RS-Phi"
    other_ok   <- if (eos) (nzchar(nms_v) & db_engine_eligible(nms_v, other, smi_v)) else rep(FALSE, length(calcs))
    hint <- function(i) if (other_ok[i]) sprintf(i18n$t("available in %s — switch engine"), other_disp)
                        else i18n$t("in neither COSMO database")
    choice_names <- lapply(seq_along(calcs), function(i) {
      nm <- htmltools::htmlEscape(calcs[[i]]$display_name)
      if (ok[i]) HTML(nm)
      else HTML(sprintf("<span style='color:#bbb; font-style:italic;'>%s <small>(%s)</small></span>", nm, htmltools::htmlEscape(hint(i))))
    })
    unsuit_idx <- which(!ok)
    sel_keep <- rv$selected_saved_calc_ids[rv$selected_saved_calc_ids %in% as.character(which(ok))]

    has_selection <- length(sel_keep) > 0

    delete_btn <- actionButton(
      ns("delete_selected_calcs"),
      label = i18n$t("Remove Selected from Session"),
      icon = icon("trash-alt"),
      class = "btn-xs btn-danger",
      style = "color: white;"
    )
    save_btn <- actionButton(
      ns("save_selected_calcs"),
      label = i18n$t("Save Selected to Disk"),
      icon = icon("save"),
      class = "btn-xs btn-primary",
      style = "color: white;"
    )
    if (!has_selection) {
      delete_btn <- shinyjs::disabled(delete_btn)
      save_btn <- shinyjs::disabled(save_btn)
    }

    disable_js <- if (length(unsuit_idx))
      paste(sprintf("$(\"#%s input[value='%d']\").prop('disabled', true).closest('.checkbox').css('opacity','0.5');",
                    ns("saved_calculation_checkboxes"), unsuit_idx), collapse = " ")
    else ""
    disable_script <- if (nzchar(disable_js)) tags$script(HTML(sprintf("setTimeout(function(){%s}, 0);", disable_js))) else NULL

    tagList(
      div(
        style = if (total_calcs > 6) "margin-bottom: 10px; max-height: 300px; overflow-y: auto;" else "margin-bottom: 10px;",
        checkboxGroupInput(
          ns("saved_calculation_checkboxes"),
          label = i18n$t("Select saved calculations"),
          choiceNames = choice_names,
          choiceValues = as.character(seq_along(calcs)),
          selected = sel_keep
        ),
        disable_script
      ),
      div(
        style = "margin-top: -5px; display:flex; gap:6px; flex-wrap:wrap;",
        delete_btn, save_btn
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

  # ---- Load a compound from the experimental database (method-filtered) -> synthetic saved-calc ----
  db_refresh_comp <- reactiveVal(0)
  # Active EoS engine in Solubility mode -> enables the modal's "Engine compounds" source (pick ANY compound
  # the engine predicts, not just the measured-property subset).
  eng_now <- reactive(if (identical(input$comp_method %||% "hsp", "solubility") &&
    (input$sol_engine %||% "csp") %in% c("csp", "ocrsphi")) input$sol_engine else NULL)
  observeEvent(input$open_db_modal, showModal(exp_db_modal(ns, i18n, engine = eng_now())))
  db_filtered_comp <- reactive({ db_refresh_comp()
    eng <- eng_now(); d <- exp_db_source(input, eng); if (is.null(d) || !nrow(d)) return(NULL)
    # "Engine compounds" source = the engine's FULL list (already the eligible set); skip category/method/SFE.
    if (isTRUE(input$db_source == "engine") && !is.null(eng)) return(d)
    # Experimental DB: in Solubility mode the EoS engines (csp/ocrsphi) restrict Load-from-DB to compounds
    # in their own database; Makarov keeps the SMILES filter.
    meth <- input$comp_method %||% "hsp"
    if (identical(meth, "solubility") && (input$sol_engine %||% "csp") %in% c("csp", "ocrsphi")) meth <- input$sol_engine
    d <- d[exp_db_method_keep(d, meth), , drop = FALSE]
    exp_db_filtered(d, input$db_category, isTRUE(input$db_sfe_only)) })
  output$db_table <- DT::renderDataTable({ exp_db_dt(i18n_r, db_filtered_comp()) }, server = TRUE)  # server-side: only the visible page is sent (fast); full-DB export via exp_db_wire_download
  exp_db_wire_download(output, session, db_filtered_comp)
  exp_db_wire_category(input, session)
  observeEvent(input$db_add_save, {
    if (isTRUE(exp_db_add_save(input, session, i18n, close = FALSE))) db_refresh_comp(db_refresh_comp() + 1) })
  exp_db_wire_edit_delete(input, output, session, i18n,
    refresh = function() db_refresh_comp(db_refresh_comp() + 1), db_fn = db_filtered_comp)
  observeEvent(input$db_load_selected, {
    sel <- input$db_table_rows_selected
    if (is.null(sel) || !length(sel)) { showNotification(i18n$t("Select a compound first."), type = "warning"); return() }
    db <- db_filtered_comp(); req(db); row <- db[sel, ]
    meth <- input$comp_method %||% "hsp"
    exp_hsp <- suppressWarnings(as.numeric(c(row$dD, row$dP, row$dHB))); Tc <- suppressWarnings(as.numeric(row$Tc))
    has_exp_hsp <- all(is.finite(exp_hsp)) && is.finite(Tc)
    idx <- length(sfe_rv$saved_calculations) + 1
    sfe_rv$saved_calculations[[idx]] <- list(
      id = idx,
      display_name = paste0(row$name, " | ", if (identical(meth, "hsp")) i18n$t("DB (exp HSP)") else i18n$t("DB (ML)"),
                            " | ", format(Sys.time(), "%H:%M:%S")),
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), source_module = "Database",
      parameters = list(
        cas_input = if (!is.na(row$cas)) sub("\\|.*$", "", row$cas) else "",
        name_input = row$name, smiles_input = row$smiles %||% "", specify_smiles = TRUE,
        gcm_tb = "none", tb_manual_input = NA, gcm_crit = "none", gcm_hsp = "none",
        gcm_simplicity = NULL, gcm_gorder = 0,
        melting_point = suppressWarnings(as.numeric(row$Tm)), enthalpy_vap = suppressWarnings(as.numeric(row$Hvap)),
        db_experimental = has_exp_hsp, exp_crit_value = if (has_exp_hsp) Tc else NULL,
        exp_hsp_values = if (has_exp_hsp) exp_hsp else NULL, chrastil = NULL))
    rv$selected_saved_calc_ids <- unique(c(rv$selected_saved_calc_ids, as.character(idx)))  # auto-check it
    removeModal()
    showNotification(sprintf(i18n$t("Loaded '%s' from the database."), row$name), type = "message", duration = 3)
  })

  # Save the checkbox-selected saved calculations to disk (one JSON per calc; round-trips via Load from Disk).
  setup_save_selected_to_disk(input, session, i18n_r, list(
    trigger = "save_selected_calcs", confirm = "save_sel_confirm", dir_input = "save_sel_dir", browse = "save_sel_browse",
    default_dir = function() supercrit_user_dir(file.path("user-settings", "sfe_gcm_calculations")),
    collect = function() {
      sel <- input$saved_calculation_checkboxes
      if (is.null(sel) || !length(sel)) return(NULL)
      lapply(sel, function(id) {
        cc <- sfe_rv$saved_calculations[[as.integer(id)]]
        list(name = cc$parameters$name_input %||% cc$display_name %||% "calculation",
             data = list(display_name = cc$display_name, timestamp = cc$timestamp,
                         source_module = cc$source_module, parameters = cc$parameters,
                         metadata = list(created_date = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), app_version = "0.9.0")))
      })
    }))

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
        # Uniform format: bold name + CAS in parentheses (CAS omitted when absent). SMILES is never shown.
        display_text <- tagList(
          strong(calc$parameters$name_input),
          solute_cas_suffix(calc$parameters$cas_input)
        )
        fluidRow(
          column(12, p(display_text))
        )
      }
    })

    tagList(items)
  })

  # Render blend solvent selector with reactive translations
  output$blend_solvent_selector_ui <- renderUI({
    choices <- cosolv_choices()   # mode-aware co-solvent set (extended for the EoS engines)
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
          class = "btn btn-danger btn-sm",
          style = "flex-shrink: 0; color: white;"
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
    # Composition unit for this blend: volume % in HSP mode, mole % for the n-component EoS (Solubility).
    unit_lab <- if (identical(input$comp_method %||% "hsp", "hsp")) i18n_r()$t("vol %") else i18n_r()$t("mol %")
    div(
      style = "display:flex; align-items:center; gap:10px; flex-wrap:wrap;",
      tags$span(strong(i18n$t("Total:")), " ",
        span(paste0(round(total, 1), "%"),
          style = paste0("background-color:", total_bg, "; color: white; font-weight: bold; padding: 2px 8px; border-radius: 4px;"))),
      tags$span(style = "border-left:1px solid #ccc; align-self:stretch;"),
      tags$span(strong(i18n$t("Units:")), " ",
        span(unit_lab, style = "background:#6c757d; color:white; font-weight:700; font-size:11px; padding:2px 8px; border-radius:11px;"))
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

  # ---- Solubility (ML) plots: built to EXACTLY match the HSP-mode miscomp() plots (same colours,
  #      shapes, geoms, theme, grid, fonts, line thickness) — only the y-axis differs (log10 when the
  #      solubilities span a wide range). Same defaults as R/hsp_main.R so the two modes are identical.
  .comp_defcols <- c("black", "blue", "darkgreen", "darkorange", "darkred", "purple")
  comp_ml_style <- function(d, xlab, ylab, title, log_y) {
    d <- d[is.finite(d$Y), , drop = FALSE]
    lv <- levels(d$grp); n <- length(lv)
    cols <- rep(.comp_defcols, length.out = n); shp <- rep(c(0:2, 5:7), length.out = n)
    p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[["Fraction"]], y = .data[["Y"]],
           colour = .data[["grp"]], shape = .data[["grp"]], group = .data[["grp"]])) +
      ggplot2::geom_line() + ggplot2::geom_point(size = 2) +
      ggplot2::labs(title = title, x = xlab, y = ylab) +
      ggplot2::scale_x_continuous(breaks = scales::breaks_pretty(n = 6)) +
      ggplot2::scale_colour_manual(values = cols, breaks = lv, labels = lv) +
      ggplot2::scale_shape_manual(values = shp, breaks = lv, labels = lv) +
      ggplot2::theme(aspect.ratio = 1, panel.background = ggplot2::element_blank(),
        axis.text = ggplot2::element_text(colour = "black", size = 12),
        axis.title = ggplot2::element_text(colour = "black", size = 13),
        axis.line = ggplot2::element_line(colour = "black"),
        legend.title = ggplot2::element_blank(), legend.position = "right",
        legend.text = ggplot2::element_text(size = 11))
    if (log_y) p + ggplot2::scale_y_log10() else p + ggplot2::scale_y_continuous(breaks = scales::breaks_pretty(n = 6))
  }

  # Encode/parse an ML plot key that carries the temperature dimension. Three kinds, each fixing two of
  # {solute, pressure, temperature} via the key and varying the third as lines (x = co-solvent fraction):
  #   "an" analyte:      a = solute,   b = temperature  -> lines per pressure
  #   "pr" pressure:     a = pressure, b = temperature  -> lines per solute
  #   "tm" temperature:  a = solute,   b = pressure     -> lines per temperature
  comp_ml_key <- function(kind, a, b) paste(kind, a, b, sep = "@@")

  # Build the comparison ggplot for a given key (see comp_ml_key), for BOTH modes. HSP plots miscibility
  # enhancement (%); Solubility (ML) plots absolute solubility (unit toggle, log10 for wide ranges). Both
  # share the exact same style (comp_ml_style) and the temperature-aware key scheme. Used by the on-screen
  # renderers AND the export.
  comp_plot_for <- function(plot_name) {
    d0 <- rv$miscomp_results$data; req(d0)
    sol_mode <- isTRUE(rv$miscomp_results$sol_mode)
    if (sol_mode) {
      gkg <- identical(input$comp_unit %||% "molfrac", "gkg")
      d0$Y <- if (gkg) solub_gkg(d0$Solubility_molfrac, d0$mw) else d0$Solubility_molfrac
      ylab <- if (gkg) i18n_r()$t("Solubility (g/kg)") else i18n_r()$t("Solubility (mol/mol)")
      xlab <- i18n_r()$t("Co-solvent mole fraction")
    } else {
      d0$Y <- d0$Miscib
      ylab <- i18n_r()$t("Miscibility Enhancement (%)")
      xlab <- i18n_r()$t("Volume Fraction of Co-Solvent")
    }
    modlab <- rv$miscomp_results$modifier_label %||% i18n_r()$t("co-solvent")
    subt <- rv$miscomp_results$modifier_subtitle
    # For a blend, fold the composition straight INTO the title (e.g. "50:50 Acetone:Ethanol", or
    # "33:33:33 A:B:C" for more components) instead of repeating it as a separate subtitle. A single
    # co-solvent has no subtitle, so this is just its name.
    mod_for_title <- if (!is.null(subt) && nzchar(subt)) sub("^\\*\\s*", "", subt) else modlab
    bar <- i18n_r()$t("bar")
    y_at <- if (sol_mode) i18n_r()$t("Solubility at") else i18n_r()$t("Miscibility enhancement at")
    parts <- strsplit(plot_name, "@@", fixed = TRUE)[[1]]
    kind <- parts[1]
    if (kind == "pr") {
      P <- suppressWarnings(as.numeric(parts[2])); tt <- suppressWarnings(as.numeric(parts[3]))
      dd <- d0[d0$Pressure == P & d0$Temperature == tt, , drop = FALSE]
      grp <- factor(dd$Solute, levels = sort(unique(dd$Solute)))
      title <- if (isTRUE(input$plt_title)) sprintf("%s %s %s, %s °C — %s", y_at, P, bar, tt, mod_for_title) else NULL
    } else if (kind == "tm") {
      sol <- parts[2]; P <- suppressWarnings(as.numeric(parts[3]))
      dd <- d0[d0$Solute == sol & d0$Pressure == P, , drop = FALSE]
      grp <- factor(dd$Temperature, levels = sort(unique(dd$Temperature)))
      title <- if (isTRUE(input$plt_title)) sprintf("%s — %s %s, %s", sol, P, bar, mod_for_title) else NULL
    } else {   # "an" (default)
      sol <- parts[2]; tt <- suppressWarnings(as.numeric(parts[3]))
      dd <- d0[d0$Solute == sol & d0$Temperature == tt, , drop = FALSE]
      grp <- factor(dd$Pressure, levels = sort(unique(dd$Pressure)))
      title <- if (isTRUE(input$plt_title)) sprintf("%s — %s, %s °C", sol, mod_for_title, tt) else NULL
    }
    dd <- data.frame(Fraction = dd$Fraction, Y = dd$Y, grp = grp)
    yy <- dd$Y[is.finite(dd$Y) & dd$Y > 0]
    log_y <- sol_mode && length(yy) > 1 && (max(yy) / min(yy) > 100)   # log axis only for ML solubility
    comp_ml_style(dd, xlab, ylab, title, log_y)
  }

  # Every plot key (for the exporter and get_current_plot_name), for both modes. Temperature-Comparisons
  # keys are emitted only when more than one temperature was evaluated (that tab is contextual on it).
  comp_plot_names <- function() {
    d <- rv$miscomp_results$data; if (is.null(d)) return(character(0))
    sols <- sort(unique(d$Solute)); press <- sort(unique(d$Pressure)); temps <- sort(unique(d$Temperature))
    keys <- c(as.vector(outer(sols, temps, function(s, t) comp_ml_key("an", s, t))),
              as.vector(outer(press, temps, function(p, t) comp_ml_key("pr", p, t))))
    if (length(temps) > 1)
      keys <- c(keys, as.vector(outer(sols, press, function(s, p) comp_ml_key("tm", s, p))))
    keys
  }

  # ---- Shared (mode-aware) result renderers, defined once. Both run branches only set rv$miscomp_results;
  #      these react to it. ----
  # Miscibility Data table: HSP shows the ME (%) columns, ML shows the enhancement + solubility columns.
  output$miscibility_data_table <- DT::renderDataTable({
    req(rv$miscomp_results$data)
    d <- rv$miscomp_results$data
    if (isTRUE(rv$miscomp_results$sol_mode)) {
      gkg <- identical(input$comp_unit %||% "molfrac", "gkg")
      yv <- if (gkg) solub_gkg(d$Solubility_molfrac, d$mw) else d$Solubility_molfrac
      sol_hdr <- if (gkg) i18n_r()$t("Solubility (g/kg)") else i18n_r()$t("Solubility (mol/mol)")
      out <- data.frame(Solute = d$Solute, Fraction = d$Fraction, Temperature = d$Temperature,
        Pressure = d$Pressure, Enhancement = d$Enhancement, Solubility = signif(yv, 4), stringsAsFactors = FALSE)
      DT::datatable(out,
        colnames = c(i18n_r()$t("Solute Name"), i18n_r()$t("Mole Fraction"), i18n_r()$t("Temperature (°C)"),
                     i18n_r()$t("Pressure (bar)"), i18n_r()$t("Enhancement (×)"), sol_hdr),
        caption = htmltools::tags$caption(style = "caption-side: bottom; font-size:0.82em; color:#777;",
          i18n_r()$t("Absolute-solubility scale in pure CO2 — extremely insoluble ~1e-7 mol/mol (~1e-4 g/kg, e.g. disperse dyes); typical ~1e-4 (~0.7 g/kg); highly soluble ~0.1 (~300 g/kg, e.g. naphthalene).")),
        extensions = "Buttons", rownames = FALSE, filter = "top",
        options = list(scrollX = TRUE, paging = FALSE, language = tablang(), dom = "Bfrtip",
          buttons = create_dt_export_buttons(i18n_r, "supercrit_cosolvent_solubility_comparison")))
    } else {
      out <- data.frame(Solute = d$Solute, Fraction = d$Fraction, Temperature = d$Temperature,
        Pressure = d$Pressure, Miscib = round(d$Miscib, 2), stringsAsFactors = FALSE)
      DT::datatable(out,
        colnames = c(i18n_r()$t("Solute Name"), i18n_r()$t("Volume Fraction"), i18n_r()$t("Temperature (°C)"),
                     i18n_r()$t("Pressure (bar)"), i18n_r()$t("Miscibility Enhancement (%)")),
        extensions = "Buttons", rownames = FALSE, filter = "top",
        options = list(scrollX = TRUE, paging = FALSE, language = tablang(), dom = "Bfrtip",
          buttons = create_dt_export_buttons(i18n_r, "supercrit_miscibility_comparison")))
    }
  })

  # Plot tabs: Analyte + Pressure Comparisons, plus a contextual Temperature Comparisons tab (only when
  # >1 temperature). Analyte/Pressure gain a temperature dropdown when >1 temperature. Shared by both modes.
  output$plots_tabs_ui <- renderUI({
    req(rv$miscomp_results$data)
    d <- rv$miscomp_results$data
    solutes <- sort(unique(d$Solute)); pressures <- sort(unique(d$Pressure)); temps <- sort(unique(d$Temperature))
    multi_t <- length(temps) > 1
    temp_choices <- setNames(as.character(temps), paste0(temps, " °C"))
    pres_choices <- setNames(as.character(pressures), paste0(pressures, " ", i18n$t("bar")))
    # Export buttons for a tab: same exact formatting as the Co-Solvent Selection "Export Results" button
    # (btn-success btn-sm, white text). Unique IDs per tab (all wired to the same handlers) so the pair can
    # sit INLINE on each tab's selector row without duplicate output IDs.
    exp_btns <- function(sfx) div(style = "display:flex; gap:8px; align-items:flex-end; margin-left:auto;",
      downloadButton(ns(paste0("export_current_plot_", sfx)), i18n$t("Export Current Plot"),
        class = "btn btn-success btn-sm", style = "color:white;"),
      downloadButton(ns(paste0("export_all_plots_", sfx)), i18n$t("Export All Plots"),
        class = "btn btn-success btn-sm", style = "color:white;"))
    # Dropdowns on the left; the Export buttons pushed to the right of the same row (margin-left:auto).
    # The .comp-sel-row style below zeroes each selectInput's form-group bottom-margin so flex-end aligns
    # the buttons to the INPUT box bottom (not 15px below it), keeping them level with the dropdown.
    sel_row <- function(sfx, ...) div(class = "comp-sel-row",
      style = "margin-bottom: 15px; display:flex; gap:12px; flex-wrap:wrap; align-items:flex-end;",
      ..., exp_btns(sfx))

    analyte_tab <- tabPanel(i18n$t("Analyte Comparisons"),
      sel_row("an",
        selectInput(ns("analyte_comparison_selector"), i18n$t("Select Analyte"),
          choices = setNames(solutes, solutes), selected = solutes[1], width = "260px"),
        if (multi_t) selectInput(ns("analyte_temp_selector"), i18n$t("Temperature (°C)"),
          choices = temp_choices, selected = temp_choices[1], width = "180px")),
      plotly::plotlyOutput(ns("analyte_comparison_plot"), height = "600px"))

    pres_tab <- tabPanel(i18n$t("Pressure Comparisons"),
      sel_row("pr",
        selectInput(ns("pres_comparison_selector"), i18n$t("Select Pressure"),
          choices = pres_choices, selected = as.character(pressures[1]), width = "260px"),
        if (multi_t) selectInput(ns("pres_temp_selector"), i18n$t("Temperature (°C)"),
          choices = temp_choices, selected = temp_choices[1], width = "180px")),
      plotly::plotlyOutput(ns("pres_comparison_plot"), height = "600px"))

    tabs <- list(analyte_tab, pres_tab)
    if (multi_t) {
      temp_tab <- tabPanel(i18n$t("Temperature Comparisons"),
        sel_row("tm",
          selectInput(ns("temp_comp_solute_selector"), i18n$t("Select Analyte"),
            choices = setNames(solutes, solutes), selected = solutes[1], width = "260px"),
          selectInput(ns("temp_comp_pres_selector"), i18n$t("Select Pressure"),
            choices = pres_choices, selected = as.character(pressures[1]), width = "260px")),
        plotly::plotlyOutput(ns("temp_comparison_plot"), height = "600px"))
      tabs <- c(tabs, list(temp_tab))
    }
    tagList(
      tags$style(HTML(".comp-sel-row .form-group { margin-bottom: 0 !important; }")),
      do.call(tabsetPanel, c(list(id = ns("plots_active_tab")), tabs)))
  })

  .first_temp <- function() as.character(sort(unique(rv$miscomp_results$data$Temperature))[1])
  # Analyte tab: one solute (+ temperature), one line per pressure.
  output$analyte_comparison_plot <- plotly::renderPlotly({
    req(rv$miscomp_results$data, input$analyte_comparison_selector)
    tt <- if (!is.null(input$analyte_temp_selector)) input$analyte_temp_selector else .first_temp()
    plotly::layout(quiet_ggplotly(comp_plot_for(comp_ml_key("an", input$analyte_comparison_selector, tt))),
      legend = list(title = list(text = i18n_r()$t("Pressure (bar)"))))
  })
  # Pressure tab: one pressure (+ temperature), one line per solute.
  output$pres_comparison_plot <- plotly::renderPlotly({
    req(rv$miscomp_results$data, input$pres_comparison_selector)
    tt <- if (!is.null(input$pres_temp_selector)) input$pres_temp_selector else .first_temp()
    plotly::layout(quiet_ggplotly(comp_plot_for(comp_ml_key("pr", input$pres_comparison_selector, tt))),
      legend = list(title = list(text = i18n_r()$t("Solute"))))
  })
  # Temperature tab: one solute + one pressure, one line per temperature.
  output$temp_comparison_plot <- plotly::renderPlotly({
    req(rv$miscomp_results$data, input$temp_comp_solute_selector, input$temp_comp_pres_selector)
    plotly::layout(quiet_ggplotly(comp_plot_for(comp_ml_key("tm", input$temp_comp_solute_selector, input$temp_comp_pres_selector))),
      legend = list(title = list(text = i18n_r()$t("Temperature (°C)"))))
  })

  # --- Main miscomp execution logic ---
  observeEvent(input$run_miscomp, {
    # 1. Validate minimum solutes
    if (length(combined_solutes()) < 2) {
      showNotification(i18n$t("Please select at least 2 solutes from saved calculations."), type = "error")
      return(NULL)
    }

    # ---- Solubility (ML) mode: compare solutes by predicted scCO₂ solubility enhancement ----
    # Loops solub_optim over each selected solute (its saved SMILES + melting_point) against the single
    # chosen co-solvent, across the pressure list at the fixed temperature. HSP path below is untouched.
    if (identical(input$comp_method, "solubility")) {
      # Up to 6 temperatures (Solubility/ML mode). The comparison plots gain a temperature dimension.
      temp_values <- sort(unique(suppressWarnings(as.numeric(input$temp))))
      temp_values <- temp_values[is.finite(temp_values)]
      if (!length(temp_values)) { showNotification(i18n$t("Please enter valid temperature values."), type = "error"); return(NULL) }
      t_bad <- temp_values[temp_values < 31 | temp_values > 200]
      if (length(t_bad)) { showNotification(paste(i18n$t("Out of range:"), i18n$t("Temperature (°C)"), "(31 – 200)"), type = "error"); return(NULL) }
      if (length(temp_values) > 6) { showNotification(i18n$t("Maximum 6 temperature values are allowed."), type = "error"); return(NULL) }
      pres_values <- as.numeric(input$pres_input); pres_values <- pres_values[!is.na(pres_values)]
      if (!length(pres_values)) { showNotification(i18n$t("Please enter valid pressure values."), type = "error"); return(NULL) }
      # Single co-solvent OR a blend (mole-fraction feature superposition in predict_solubility/solubcomp).
      ac_cs <- active_cosolv(); solv_map <- setNames(ac_cs$smiles, ac_cs$value)   # includes EoS-only extras
      blend_ratio <- NULL; cos_subtitle <- NULL
      if (identical(input$modifier_type, "blend")) {
        if (length(rv$current_blend) < 2) {
          showNotification(i18n$t("Please add at least 2 solvents to the blend."), type = "error"); return(NULL) }
        blend_solv <- names(rv$current_blend)
        blend_fr <- vapply(blend_solv, function(s) { v <- input[[paste0("frac_", s)]]
          if (is.null(v) || is.na(v)) 0 else as.numeric(v) }, numeric(1))
        if (sum(blend_fr) <= 0) { showNotification(i18n$t("Blend percentages must be positive."), type = "error"); return(NULL) }
        blend_disp <- vapply(blend_solv, function(s) translate_solvent_token(s, i18n_r()), character(1))
        # Name the co-solvents by their UNTRANSLATED tokens so the EoS engines (which resolve by database
        # name) can find them; the translated names are used only for display (cos_name / subtitle).
        cos_arg <- setNames(unname(solv_map[blend_solv]), blend_solv)
        blend_ratio <- blend_fr / sum(blend_fr)
        cos_name <- paste(blend_disp, collapse = " + ")
        cos_subtitle <- paste0("* ", paste(round(100 * blend_ratio), collapse = ":"), " ", paste(blend_disp, collapse = ":"))
        warn_blend_misc(blend_solv)
      } else {
        cos_sel <- input$modifier_selection
        if (is.null(cos_sel) || !nzchar(cos_sel)) { showNotification(i18n$t("Please select a co-solvent."), type = "error"); return(NULL) }
        cos_arg <- setNames(unname(solv_map[cos_sel]), cos_sel)
        cos_name <- translate_solvent_token(cos_sel, i18n_r())
      }
      # Multiple co-solvent mole FRACTIONS (0.01-0.99). Mirrors HSP's Volume Fractions.
      fr <- sort(unique(suppressWarnings(as.numeric(input$comp_molfrac))))
      fr <- fr[is.finite(fr) & fr > 0 & fr < 1]; if (!length(fr)) fr <- 0.05

      # Build the solute table (name + SMILES + saved melting point) for the solubcomp() source function,
      # the ML counterpart to miscomp(). solubcomp loops solub_optim over solutes x fractions x the
      # pressure/temperature grid and returns the long data frame + out-of-domain count.
      sol_rows <- lapply(rv$selected_saved_calc_ids, function(id) {
        calc <- if (as.numeric(id) <= length(sfe_rv$saved_calculations)) sfe_rv$saved_calculations[[as.numeric(id)]] else NULL
        if (is.null(calc)) return(NULL)
        smi <- calc$parameters$smiles_input
        if (is.null(smi) || !nzchar(smi)) return(NULL)
        tmv <- suppressWarnings(as.numeric(calc$parameters$melting_point))
        nm <- if (!is.null(calc$parameters$name_input) && nzchar(calc$parameters$name_input)) calc$parameters$name_input else smi
        data.frame(name = nm, smiles = smi, Tm = if (length(tmv) == 1 && is.finite(tmv)) tmv else NA_real_,
                   stringsAsFactors = FALSE)
      })
      sol_df <- do.call(rbind, Filter(Negate(is.null), sol_rows))
      if (is.null(sol_df) || !nrow(sol_df)) {
        showNotification(i18n$t("No solubility results were produced. Ensure the selected solutes have SMILES."), type = "warning"); return(NULL) }

      eng <- input$sol_engine %||% "csp"; is_eos <- eng != "makarov"
      # Blends are evaluated n-component by the COSMO-SAC-Phi EoS engines (CSP-2021 / openCOSMO-RS-Phi).
      # The Makarov ML model has no blend-mixing rule, so a blend needs an EoS engine (or HSP mode).
      if (!is_eos && identical(input$modifier_type, "blend")) {
        showNotification(i18n$t("The Makarov ML model has no blend-mixing rule; choose CSP-2021 or openCOSMO-RS-Phi to compare a blend (or use HSP mode)."), type = "error"); return(NULL) }
      # Parallel evaluation (checkbox, on by default): reserve ~25% of cores (at least one) so the app and
      # the user's other work keep responsive. solubcomp spreads the solute x fraction jobs across cores.
      n_workers <- 1L
      if (is_eos && isTRUE(input$sol_parallel %||% TRUE)) {
        nc <- suppressWarnings(parallel::detectCores()); if (is.na(nc) || nc < 1L) nc <- 1L
        n_workers <- max(1L, nc - max(1L, as.integer(ceiling(nc / 4))))
      }
      cmp <- tryCatch(
        withProgress(message = i18n$t("Predicting co-solvent solubility..."),
          detail = if (n_workers > 1L) i18n$t("Running in parallel; this can take a while for the EoS engines.") else NULL,
          solubcomp(sol_df, cosolvent = cos_arg, temp_C = temp_values, pres_bar = pres_values,
                    x = fr, blend_ratio = blend_ratio, draw = FALSE, silent = TRUE, engine = eng,
                    workers = n_workers)),
        error = function(e) { showNotification(paste(i18n$t("Error during solubility prediction:"), e$message), type = "error"); NULL })
      if (is.null(cmp) || is.null(cmp$data) || !nrow(cmp$data)) {
        showNotification(if (is_eos) i18n$t("No results: the selected EoS engine needs the solute(s) and co-solvent in its database (see the model help).") else i18n$t("No solubility results were produced. Ensure the selected solutes have SMILES."), type = "warning"); return(NULL) }

      rv$miscomp_results <- list(data = cmp$data, sol_mode = TRUE,
        modifier_label = cos_name, modifier_subtitle = cos_subtitle,
        fractions = fr, temp_C = temp_values)
      if (isTRUE(cmp$n_out > 0))
        showNotification(i18n$t("Some pressure/temperature points are outside the solubility model's reliable range (308-373 K, 80-360 bar)."), type = "warning", duration = 6)
      # (Result table + plot tabs are rendered by the shared mode-aware outputs defined at module scope.)
      return(NULL)
    }

    # 2. Get GCM parameters from selected calculations
    # Geçici kontrol - kütüphane hazır olunca TRUE
    allow_multi_gcm <- TRUE

    if (allow_multi_gcm) {
      # Per-solute GCM args. A NON-GCM method marker ("manual"/"none"/"experimental") must never reach
      # est_gcm (which only accepts a real GCM method string or a numeric value), so — mirroring the
      # Optimization module — Tb "manual" resolves to its numeric value (any other non-GCM marker -> NA,
      # which est_gcm excludes); crit/HSP pass a valid PLACEHOLDER method, with the measured value applied
      # through the crit_values/hsp_values override for DB-experimental solutes. Manual crit/HSP that were
      # not saved fall back to the GCM estimate (warned below). Each is a per-solute list for miscomp().
      calcs <- lapply(rv$selected_saved_calc_ids, function(id) sfe_rv$saved_calculations[[as.numeric(id)]]$parameters)
      .is_gcm <- function(m) !is.null(m) && length(m) == 1 && !is.na(m) && nzchar(m) && !(m %in% c("manual", "none", "experimental"))
      .num1   <- function(x) { v <- suppressWarnings(as.numeric(x)[1]); if (isTRUE(is.finite(v))) v else NA_real_ }
      tb_param <- lapply(calcs, function(pp)
        if (identical(pp$gcm_tb, "manual") && is.finite(.num1(pp$tb_manual_input))) .num1(pp$tb_manual_input)
        else if (.is_gcm(pp$gcm_tb)) pp$gcm_tb else NA)
      crit_param <- lapply(calcs, function(pp) if (.is_gcm(pp$gcm_crit)) pp$gcm_crit else "JR")
      hsp_param  <- lapply(calcs, function(pp) if (.is_gcm(pp$gcm_hsp))  pp$gcm_hsp  else "SP12")
      simplicity_param <- vapply(calcs, function(pp) pp$gcm_simplicity %||% "auto", character(1))
      # Per-solute experimental override (DB-loaded compounds carry measured Tc + HSP; NULL otherwise).
      crit_values_param <- lapply(calcs, function(pp)
        if (isTRUE(pp$db_experimental)) .num1(pp$exp_crit_value) else NULL)
      hsp_values_param <- lapply(calcs, function(pp)
        if (isTRUE(pp$db_experimental)) suppressWarnings(as.numeric(pp$exp_hsp_values)) else NULL)
      # Warn when a manual/experimental crit or HSP could not be recovered -> GCM fallback for that solute.
      .have_ov <- function(v, n) length(suppressWarnings(as.numeric(v))) >= n &&
                                 all(is.finite(suppressWarnings(as.numeric(v))[seq_len(n)]))
      if (any(vapply(calcs, function(pp)
            (!.is_gcm(pp$gcm_crit) && !(isTRUE(pp$db_experimental) && .have_ov(pp$exp_crit_value, 1))) ||
            (!.is_gcm(pp$gcm_hsp)  && !(isTRUE(pp$db_experimental) && .have_ov(pp$exp_hsp_values, 3))), logical(1))))
        showNotification(i18n$t("A selected solute used manual/experimental critical-T or HSP values that were not saved with it; the comparison fell back to the group-contribution estimate for that property. Re-characterize and save the solute to use the exact values."), type = "warning", duration = 8)
    } else {
      crit_values_param <- hsp_values_param <- NULL
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



    # 3. Validate GCM methods (DB-experimental compounds supply their own measured Tc + HSP, so they
    #    are exempt from needing a GCM method).
    any_db_exp <- any(vapply(rv$selected_saved_calc_ids,
      function(id) isTRUE(sfe_rv$saved_calculations[[as.numeric(id)]]$parameters$db_experimental), logical(1)))
    # (Check the RAW stored methods, not the resolved per-solute lists above.) Only block when every solute
    # has no GCM method for any property AND none is DB-experimental — otherwise the GCM placeholders below
    # can still estimate Tc/HSP from the SMILES.
    raw_none <- function(k) all(vapply(calcs, function(pp) identical(pp[[k]] %||% "none", "none"), logical(1)))
    if (!any_db_exp && raw_none("gcm_tb") && raw_none("gcm_crit") && raw_none("gcm_hsp")) {
      showNotification(i18n$t("Selected calculation does not have valid GCM methods."), type = "error")
      return(NULL)
    }

    # 4. Validate temperatures (up to 6, 31-200 \u00B0C). HSP mode now also supports multiple temperatures.
    temp_hsp <- sort(unique(suppressWarnings(as.numeric(input$temp))))
    temp_hsp <- temp_hsp[is.finite(temp_hsp)]
    if (!length(temp_hsp)) { showNotification(i18n$t("Please enter valid temperature values."), type = "error"); return(NULL) }
    if (any(temp_hsp < 31 | temp_hsp > 200)) {
      showNotification(paste(i18n$t("Out of range:"), i18n$t("Temperature (\u00B0C)"), "(31 \u2013 200)"), type = "error"); return(NULL) }
    if (length(temp_hsp) > 6) { showNotification(i18n$t("Maximum 6 temperature values are allowed."), type = "error"); return(NULL) }

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
      warn_blend_misc(names(rv$current_blend))

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
            temp = temp_hsp,
            vfrac = vfrac_values,

            simplicity = simplicity_param,
            draw = FALSE, # Important for Shiny apps
            silent = TRUE,
            # Per-solute experimental override for DB-loaded compounds (measured Tc + HSP, skip GCM).
            crit_values = crit_values_param,
            hsp_values = hsp_values_param
          )
        },
        error = function(e) {
          showNotification(paste(i18n$t("Error during miscibility comparison:"), e$message), type = "error")
          return(NULL)
        }
      )

      if (!is.null(miscomp_output)) {
        # Normalise miscomp()'s long data (name/vfrac/pres/temp/miscib) into the shared, capitalised schema
        # used by the mode-aware renderers (Solute/Fraction/Temperature/Pressure/Miscib). The shared
        # outputs at module scope then render the table + temperature-aware plot tabs for BOTH modes.
        raw <- miscomp_output$data
        dat <- data.frame(Solute = raw$name, Fraction = raw$vfrac, Temperature = raw$temp,
                          Pressure = as.numeric(raw$pres), Miscib = raw$miscib, stringsAsFactors = FALSE)

        # Modifier label/subtitle frozen at computation time (so live edits don't change plot titles).
        if (input$modifier_type == "pure" && !is.null(input$modifier_selection)) {
          mod_label <- translate_solvent_token(input$modifier_selection, i18n_r()); mod_subtitle <- NULL
        } else if (input$modifier_type == "blend" && length(rv$current_blend) >= 2) {
          current_fractions <- sapply(names(rv$current_blend), function(s) {
            val <- input[[paste0("frac_", s)]]; if (is.null(val) || is.na(val)) 0 else as.numeric(val) })
          translated_solvents <- sapply(names(rv$current_blend), function(s) translate_solvent_token(s, i18n_r()))
          mod_label <- paste0(i18n_r()$t("solvent blend"), "*")
          mod_subtitle <- paste0("* ", paste(round(current_fractions, 0), collapse = ":"), " v/v ",
                                 paste(translated_solvents, collapse = ":"))
        } else {
          mod_label <- i18n_r()$t("co-solvent"); mod_subtitle <- NULL
        }

        rv$miscomp_results <- list(data = dat, sol_mode = FALSE, modifier_label = mod_label,
          modifier_subtitle = mod_subtitle, temp_C = temp_hsp)
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

  # Determine the currently displayed plot's key, for BOTH modes, from the active tab + its selectors
  # (the temperature-aware key scheme is shared).
  get_current_plot_name <- function() {
    d <- rv$miscomp_results$data; req(d)
    t1 <- as.character(sort(unique(d$Temperature))[1]); p1 <- as.character(sort(unique(d$Pressure))[1])
    active_tab <- input$plots_active_tab
    if (identical(active_tab, i18n$t("Pressure Comparisons")) && !is.null(input$pres_comparison_selector)) {
      tt <- if (!is.null(input$pres_temp_selector)) input$pres_temp_selector else t1
      return(comp_ml_key("pr", input$pres_comparison_selector, tt))
    }
    if (identical(active_tab, i18n$t("Temperature Comparisons")) && !is.null(input$temp_comp_solute_selector)) {
      pp <- if (!is.null(input$temp_comp_pres_selector)) input$temp_comp_pres_selector else p1
      return(comp_ml_key("tm", input$temp_comp_solute_selector, pp))
    }
    sol <- if (!is.null(input$analyte_comparison_selector)) input$analyte_comparison_selector else as.character(sort(unique(d$Solute))[1])
    tt <- if (!is.null(input$analyte_temp_selector)) input$analyte_temp_selector else t1
    comp_ml_key("an", sol, tt)
  }

  # Plot export handlers. The per-tab button IDs (an/pr/tm) all share these: "current plot" exports the
  # ACTIVE tab's plot, "all plots" zips every plot. Both go through comp_plot_for(), so the exported PDFs
  # reproduce the on-screen plots EXACTLY (same builder, same title logic).
  .cur_plot_dl <- function() downloadHandler(
    filename = function() {
      current_name <- get_current_plot_name()
      paste0("supercrit_miscomp_", gsub("[^a-zA-Z0-9]", "_", current_name), "_", format(Sys.time(), "%d-%m-%Y_%Hhrs-%Mmin-%Ssec"), ".pdf")
    },
    content = function(file) {
      req(rv$miscomp_results)
      current_name <- get_current_plot_name(); req(current_name)
      plot_obj <- comp_plot_for(current_name)
      ggplot2::ggsave(file, plot = plot_obj, device = "pdf", width = 10, height = 8, units = "in", dpi = 300)
      showNotification(i18n$t("Done!"), type = "message", duration = 3)
    },
    contentType = "application/pdf"
  )
  .all_plots_dl <- function() downloadHandler(
    filename = function() paste0("supercrit_miscomp_all_plots_", format(Sys.time(), "%d-%m-%Y_%Hhrs-%Mmin-%Ssec"), ".zip"),
    content = function(file) {
      req(rv$miscomp_results, rv$miscomp_results$data)
      prep_notif <- showNotification(i18n$t("Preparing export..."), type = "message", duration = NULL)
      on.exit(removeNotification(prep_notif), add = TRUE)
      temp_dir <- file.path(tempdir(), paste0("miscomp_plots_", format(Sys.time(), "%d-%m-%Y_%Hhrs-%Mmin-%Ssec")))
      dir.create(temp_dir, showWarnings = FALSE)
      tryCatch({
        for (pname in comp_plot_names()) {
          plot_obj <- comp_plot_for(pname)
          plot_file <- file.path(temp_dir, paste0(gsub("[^a-zA-Z0-9]", "_", pname), ".pdf"))
          ggplot2::ggsave(plot_file, plot = plot_obj, device = "pdf", width = 10, height = 8, units = "in", dpi = 300)
        }
        zip::zip(zipfile = file, files = list.files(temp_dir, full.names = TRUE), mode = "cherry-pick")
        showNotification(i18n$t("Done!"), type = "message", duration = 3)
      }, error = function(e) {
        showNotification(paste(i18n$t("Error exporting plots:"), e$message), type = "error")
      }, finally = { unlink(temp_dir, recursive = TRUE) })
    },
    contentType = "application/zip"
  )
  # One handler per rendered button (the three tabs' buttons share the same logic).
  for (.sfx in c("an", "pr", "tm")) {
    local({
      s <- .sfx
      output[[paste0("export_current_plot_", s)]] <- .cur_plot_dl()
      output[[paste0("export_all_plots_", s)]] <- .all_plots_dl()
    })
  }

  # ==== Full-report export (top Export dropdown): PDF = ALL graphs, CSV = the full data ================
  # Comparison setup, as localised key-value pairs for the PDF header block. Everything is derived from the
  # stored results (+ the current engine / unit toggles) so it always matches what produced the graphs.
  comp_report_setup <- function() {
    d <- rv$miscomp_results$data; if (is.null(d) || !nrow(d)) return(list())
    sol_mode <- isTRUE(rv$miscomp_results$sol_mode)
    modlab <- rv$miscomp_results$modifier_label %||% i18n$t("co-solvent")
    subt <- rv$miscomp_results$modifier_subtitle
    modtxt <- if (!is.null(subt) && nzchar(subt)) paste0(modlab, "  (", sub("^\\*\\s*", "", subt), ")") else modlab
    sols <- sort(unique(d$Solute)); press <- sort(unique(d$Pressure))
    temps <- sort(unique(d$Temperature)); fracs <- sort(unique(d$Fraction))
    mode_val <- if (sol_mode) sprintf("%s (%s)", i18n$t("Solubility"),
                                      switch(input$sol_engine %||% "csp", csp = "CSP-2021",
                                             ocrsphi = "openCOSMO-RS-Phi", input$sol_engine %||% "csp"))
                else i18n$t("HSP (Miscibility)")
    Filter(Negate(is.null), list(
      list(label = i18n$t("Mode"), value = mode_val),
      list(label = i18n$t("Co-solvent"), value = modtxt),
      list(label = i18n$t("Solutes"), value = paste(sols, collapse = ", ")),
      list(label = i18n$t("Temperature(s)"), value = paste0(paste(temps, collapse = ", "), " °C")),
      list(label = i18n$t("Pressure(s)"), value = paste0(paste(press, collapse = ", "), " ", i18n$t("bar"))),
      list(label = if (sol_mode) i18n$t("Co-Solvent Mole Fractions") else i18n$t("Co-Solvent Volume Fractions"),
           value = paste(fracs, collapse = ", ")),
      if (sol_mode) list(label = i18n$t("Solubility Unit"),
                         value = if (identical(input$comp_unit %||% "molfrac", "gkg")) i18n$t("g/kg") else i18n$t("mol/mol")) else NULL))
  }

  # All comparison graphs for the PDF, grouped by family, reusing comp_plot_for() so they are IDENTICAL to
  # the on-screen plots. The per-graph title is embedded (ggtitle) and the legend moved to the BOTTOM so,
  # when two graphs are placed side-by-side (2-up grid in the PDF builder), each square panel fills its
  # cell width instead of surrendering ~40% to a right-hand legend. A compact theme suits the smaller cells.
  comp_report_groups <- function() {
    d <- rv$miscomp_results$data; if (is.null(d) || !nrow(d)) return(list())
    bar <- i18n$t("bar")
    # Fold the co-solvent (or blend composition) into each graph title, exactly like the on-screen plots
    # (comp_plot_for), so the PDF titles match the app. A single co-solvent is just its name; a blend is
    # its composition string (leading "* " stripped).
    modlab <- rv$miscomp_results$modifier_label %||% i18n$t("co-solvent")
    subt <- rv$miscomp_results$modifier_subtitle
    modt <- if (!is.null(subt) && nzchar(subt)) sub("^\\*\\s*", "", subt) else modlab
    pdf_theme <- ggplot2::theme(
      plot.title = ggplot2::element_text(size = 9.5, face = "bold", hjust = 0),
      axis.text = ggplot2::element_text(size = 7.5), axis.title = ggplot2::element_text(size = 8.5),
      legend.position = "bottom", legend.direction = "horizontal",
      legend.text = ggplot2::element_text(size = 7.5), legend.title = ggplot2::element_text(size = 8),
      legend.key.size = grid::unit(9, "pt"), legend.margin = ggplot2::margin(0, 0, 0, 0),
      plot.margin = ggplot2::margin(4, 8, 4, 6))
    mk <- function(key, title, leg) {
      p <- tryCatch(comp_plot_for(key), error = function(e) NULL)
      if (is.null(p) || !inherits(p, "ggplot")) return(NULL)
      p <- p + ggplot2::labs(title = title, colour = leg, shape = leg) +
           ggplot2::guides(colour = ggplot2::guide_legend(nrow = 1), shape = ggplot2::guide_legend(nrow = 1)) +
           pdf_theme
      list(title = title, plot = p)
    }
    sols <- sort(unique(d$Solute)); press <- sort(unique(d$Pressure)); temps <- sort(unique(d$Temperature))
    an <- list(); for (s in sols) for (tt in temps)
      an[[length(an) + 1]] <- mk(comp_ml_key("an", s, tt), sprintf("%s — %s, %s °C", s, modt, tt), i18n$t("Pressure (bar)"))
    pr <- list(); for (p in press) for (tt in temps)
      pr[[length(pr) + 1]] <- mk(comp_ml_key("pr", p, tt), sprintf("%s %s, %s °C — %s", p, bar, tt, modt), i18n$t("Solute"))
    groups <- list(
      list(title = i18n$t("Analyte Comparisons"), plots = Filter(Negate(is.null), an)),
      list(title = i18n$t("Pressure Comparisons"), plots = Filter(Negate(is.null), pr)))
    if (length(temps) > 1) {
      tm <- list(); for (s in sols) for (p in press)
        tm[[length(tm) + 1]] <- mk(comp_ml_key("tm", s, p), sprintf("%s — %s %s, %s", s, p, bar, modt), i18n$t("Temperature (°C)"))
      groups <- c(groups, list(list(title = i18n$t("Temperature Comparisons"), plots = Filter(Negate(is.null), tm))))
    }
    groups
  }

  # PDF: branded report with every graph (reuses report_pdf.R via build_comp_pdf_report).
  output$export_comp_pdf <- downloadHandler(
    filename = function() paste0(generate_filename_with_timestamp("supercrit_cosolvent_comparison"), ".pdf"),
    content = function(file) {
      req(rv$miscomp_results)
      logo <- if (file.exists("www/altraflora-15-min.png")) "www/altraflora-15-min.png" else NULL
      appver <- tryCatch(paste0("supeRcrit v", as.character(utils::packageVersion("supeRcrit"))),
                         error = function(e) "supeRcrit")
      pid <- showNotification(i18n$t("Preparing export..."), type = "message", duration = NULL)
      on.exit(removeNotification(pid), add = TRUE)
      tryCatch({
        export <- list(setup = comp_report_setup(), groups = comp_report_groups())
        build_comp_pdf_report(export, i18n, file, company = appver, logo = logo)
        showNotification(i18n$t("Done!"), type = "message", duration = 3)
      }, error = function(e) {
        showNotification(paste(i18n$t("Error exporting plots:"), e$message), type = "error", duration = 8)
        grDevices::cairo_pdf(file); grid::grid.text(i18n$t("Report generation failed.")); grDevices::dev.off()
      })
    },
    contentType = "application/pdf"
  )

  # CSV: the full long-form comparison data (mode-aware columns), localised, UTF-8 with a BOM so Excel
  # renders the ° / × symbols correctly.
  output$export_comp_csv <- downloadHandler(
    filename = function() paste0(generate_filename_with_timestamp("supercrit_cosolvent_comparison_data"), ".csv"),
    content = function(file) {
      req(rv$miscomp_results$data)
      d <- rv$miscomp_results$data
      if (isTRUE(rv$miscomp_results$sol_mode)) {
        gkg <- identical(input$comp_unit %||% "molfrac", "gkg")
        yv <- if (gkg) solub_gkg(d$Solubility_molfrac, d$mw) else d$Solubility_molfrac
        out <- data.frame(d$Solute, d$Fraction, d$Temperature, d$Pressure, d$Enhancement, signif(yv, 6),
                          stringsAsFactors = FALSE, check.names = FALSE)
        names(out) <- c(i18n$t("Solute Name"), i18n$t("Mole Fraction"), i18n$t("Temperature (°C)"),
                        i18n$t("Pressure (bar)"), i18n$t("Enhancement (×)"),
                        if (gkg) i18n$t("Solubility (g/kg)") else i18n$t("Solubility (mol/mol)"))
      } else {
        out <- data.frame(d$Solute, d$Fraction, d$Temperature, d$Pressure, round(d$Miscib, 2),
                          stringsAsFactors = FALSE, check.names = FALSE)
        names(out) <- c(i18n$t("Solute Name"), i18n$t("Volume Fraction"), i18n$t("Temperature (°C)"),
                        i18n$t("Pressure (bar)"), i18n$t("Miscibility Enhancement (%)"))
      }
      tmp <- tempfile(fileext = ".csv")
      utils::write.csv(out, tmp, row.names = FALSE, na = "", fileEncoding = "UTF-8")
      body <- readBin(tmp, "raw", n = file.info(tmp)$size); unlink(tmp)
      con <- file(file, "wb"); on.exit(close(con), add = TRUE)
      writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con)   # UTF-8 BOM
      writeBin(body, con)
    },
    contentType = "text/csv"
  )

  # suspendWhenHidden for renderUI outputs (auto-added)
  outputOptions(output, "accordion_toggle_btn", suspendWhenHidden = FALSE)
  outputOptions(output, "saved_calculations_list_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "selected_solutes_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "sfe_misc_comp_HELP", suspendWhenHidden = FALSE)

}
