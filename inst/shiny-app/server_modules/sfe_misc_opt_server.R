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
    sol_scan = NULL, # Solubility-mode results (cosolvent_solubility_scan output + ranking)
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
    loaded_tm = NULL,   # resolved solute melting point (K) from the loaded calculation, for Solubility mode
    loaded_crit = NULL,
    loaded_hsp = NULL,
    loaded_simplicity = NULL,
    loaded_gorder = NULL
  )

  # Background-run state for the slow EoS/COSMO solubility scan (callr worker + progress polling + kill).
  sol_async <- reactiveValues(proc = NULL, prog = NULL, progdir = NULL, ctx = NULL)

  # Flag driving the contextual Run/Kill button swap (see the two conditionalPanels in the UI). Using a
  # visibility toggle over two STATIC buttons (rather than re-rendering one) avoids the re-rendered-
  # actionButton value reset that would spuriously re-fire the Run observer.
  output$sol_running <- reactive({ !is.null(sol_async$proc) })
  outputOptions(output, "sol_running", suspendWhenHidden = FALSE)

  # Poll the background solubility scan: stream stage-by-stage progress while alive, finalise once it exits.
  observe({
    proc <- sol_async$proc
    if (is.null(proc)) return()
    invalidateLater(500, session)
    isolate({
      pd <- sol_async$progdir
      if (!is.null(pd) && dir.exists(pd) && !is.null(sol_async$prog)) {
        files <- list.files(pd)
        ncos <- { m <- grep("^ncos_", files, value = TRUE)
          if (length(m)) suppressWarnings(as.integer(sub("^ncos_", "", m[1]))) else (sol_async$ctx$ncos %||% NA_integer_) }
        njobs <- { m <- grep("^njobs_", files, value = TRUE)
          if (length(m)) suppressWarnings(as.integer(sub("^njobs_", "", m[1]))) else NA_integer_ }
        done_cs <- sum(grepl("^cs_", files)); done_jobs <- sum(grepl("^job_", files))
        if ("stage_done" %in% files)          { frac <- 0.98; detail <- i18n$t("Finalising results…") }
        else if ("stage_rank" %in% files)     { frac <- 0.92; detail <- i18n$t("Ranking co-solvents…") }
        else if ("stage_blend" %in% files)    { frac <- 0.88; detail <- i18n$t("Evaluating solvent blends…") }
        else if (isTRUE(is.finite(njobs)) && njobs > 0) {   # grid-chunked parallel run: progress by jobs done
          frac <- 0.10 + 0.72 * (min(done_jobs, njobs) / njobs)
          detail <- if (isTRUE(is.finite(ncos)) && ncos > 0) sprintf(i18n$t("Screening co-solvents (%d in parallel)…"), ncos)
                    else i18n$t("Screening co-solvents…") }
        else if (isTRUE(is.finite(ncos)) && ncos > 0 && done_cs > 0) {   # per-co-solvent markers
          k <- min(done_cs, ncos); frac <- 0.12 + 0.70 * (k / ncos)
          detail <- sprintf(i18n$t("Screening co-solvents (%d/%d)…"), k, ncos) }
        else if ("stage_baseline" %in% files) { frac <- 0.85; detail <- i18n$t("Ranking co-solvents…") }
        else                                  { frac <- 0.06; detail <- i18n$t("Preparing solute…") }
        try(sol_async$prog$set(value = frac, detail = detail), silent = TRUE)
      }
      if (!proc$is_alive()) {
        scan <- tryCatch(proc$get_result(), error = function(e) {
          showNotification(paste(i18n$t("Error during solubility prediction:"), conditionMessage(e)), type = "error"); NULL })
        fin <- sol_async$ctx$finalize
        if (!is.null(sol_async$prog)) { try(sol_async$prog$set(value = 1, detail = i18n$t("done")), silent = TRUE)
          try(sol_async$prog$close(), silent = TRUE) }
        if (!is.null(sol_async$progdir)) try(unlink(sol_async$progdir, recursive = TRUE), silent = TRUE)
        sol_async$proc <- NULL; sol_async$prog <- NULL; sol_async$progdir <- NULL; sol_async$ctx <- NULL
        if (!is.null(scan) && is.function(fin)) fin(scan)
      }
    })
  })

  # Kill button -> terminate the background worker and clean up.
  observeEvent(input$kill_run, {
    proc <- sol_async$proc
    if (!is.null(proc)) try(proc$kill(), silent = TRUE)
    if (!is.null(sol_async$prog)) try(sol_async$prog$close(), silent = TRUE)
    if (!is.null(sol_async$progdir)) try(unlink(sol_async$progdir, recursive = TRUE), silent = TRUE)
    sol_async$proc <- NULL; sol_async$prog <- NULL; sol_async$progdir <- NULL; sol_async$ctx <- NULL
    showNotification(i18n$t("Evaluation cancelled."), type = "message", duration = 3)
  })

  # Reactive output to check if results are available (either HSP or Solubility mode)
  output$has_results <- reactive({
    !is.null(rv$sfe_mod_output) || !is.null(rv$sol_scan)
  })
  outputOptions(output, "has_results", suspendWhenHidden = FALSE)

  # Recommended-Enhancing-Blends tab is populated only when the Automatic Solvent Finder produced blends.
  output$has_finder_blends <- reactive({ !is.null(rv$finder_result) })
  outputOptions(output, "has_finder_blends", suspendWhenHidden = FALSE)

  # True while Solubility mode is selected.
  output$is_sol_mode <- reactive({ identical(input$opt_method %||% "hsp", "solubility") })
  outputOptions(output, "is_sol_mode", suspendWhenHidden = FALSE)

  # Enhancing BLENDS are supported for HSP (Hansen miscibility-distance blend rule) and for the COSMO-SAC-Phi
  # EoS engines (CSP / openCOSMO-RS-Phi are genuine n-component models). NOT for Makarov ML (its "blends"
  # were an unreliable extrapolation and were removed).
  output$blends_supported <- reactive({
    if (!identical(input$opt_method %||% "hsp", "solubility")) return(TRUE)      # HSP
    (input$sol_engine %||% "ocrsphi") %in% c("csp", "ocrsphi")                    # Solubility: EoS only
  })
  outputOptions(output, "blends_supported", suspendWhenHidden = FALSE)
  # Whether an EoS blend screen produced results (drives the dedicated blends tab in Solubility/EoS mode).
  output$has_eos_blends <- reactive({ !is.null(rv$sol_scan$blend_ranking) && nrow(rv$sol_scan$blend_ranking) > 0 })
  outputOptions(output, "has_eos_blends", suspendWhenHidden = FALSE)

  # Automatic Solvent Finder works in BOTH modes: HSP screens all solvents by miscibility and suggests
  # binary blends; Solubility (ML) screens all co-solvents by predicted solubility (optionally including
  # experimental binary blends via the ml_blends checkbox). When on, the manual solvent/blend inputs are
  # greyed out.
  observe({
    af  <- isTRUE(input$auto_finder)
    # Manual co-solvent selection and the manual blends box are greyed only while the Auto Finder screens
    # everything. (Solubility mode now always uses an n-component EoS engine — CSP / openCOSMO-RS-Phi —
    # both of which are blend-capable, so there is no non-blend engine to grey the blends box for.)
    if (af) {
      shinyjs::addClass(id = "modifier_selection_wrapper", class = "disabled-section")
      shinyjs::addClass(id = "blends_box_wrapper", class = "disabled-section")
    } else {
      shinyjs::removeClass(id = "modifier_selection_wrapper", class = "disabled-section")
      shinyjs::removeClass(id = "blends_box_wrapper", class = "disabled-section")
    }
  })

  # Auto-reset the enhancing-blend checkbox whenever it is hidden (Auto Finder off), so a stale checked
  # value never lingers out of view. Both solubility engines are blend-capable, so mode alone gates it.
  observe({
    hidden <- !isTRUE(input$auto_finder)
    if (hidden && isTRUE(input$screen_blends)) updateCheckboxInput(session, "screen_blends", value = FALSE)
  })

  # Observer to enable/disable result tabs based on results availability
  observe({
    has_results <- !is.null(rv$sfe_mod_output) || !is.null(rv$sol_scan)
    if (has_results) {
      session$sendCustomMessage("enableTabs", list(tabsetId = ns("results_tabs")))
      # Best Modifier grid applies to both modes (HSP miscibility grid / ML solubility-enhancement grid).
      if (!is.null(rv$sfe_mod_output) || !is.null(rv$sol_scan)) shinyjs::show("best_modifier_table_wrapper")
      else shinyjs::hide("best_modifier_table_wrapper")
    } else {
      session$sendCustomMessage("disableTabs", list(tabsetId = ns("results_tabs"), keepFirst = TRUE))
      shinyjs::hide("best_modifier_table_wrapper")
    }
  })

  # Method selector (HSP miscibility vs ML solubility) and the Solubility-mode mole-fraction input.
  output$opt_method_ui <- renderUI({
    selectInput(ns("opt_method"),
      label = tags$span(i18n_r()$t("Selection Method"),
        input_help(i18n_r()$t("HSP (Miscibility) ranks co-solvents by Hansen solubility-parameter distance. Solubility ranks them by the predicted enhancement of the solute's scCO₂ solubility (and reports the absolute solubility), using a COSMO-SAC-Phi equation of state (CSP-2021 / openCOSMO-RS-Phi). Solubility mode needs the solute to be in the selected engine's database."),
                   title = i18n_r()$t("Selection Method"), buttonLabel = i18n_r()$t("OK"))),
      choices = setNames(c("hsp", "solubility"),
        c(i18n_r()$t("HSP (Miscibility)"), i18n_r()$t("Solubility"))),
      selected = "hsp", width = "100%")
  })
  outputOptions(output, "opt_method_ui", suspendWhenHidden = FALSE)

  # ---- Solubility-engine eligibility for the loaded solute -------------------------------------------
  # A COSMO-SAC-Phi engine can only be used if the solute is actually parameterised in its database
  # (db_engine_eligible resolves names exactly as the engines do, and is has_csp21 / fitted-only aware).
  # Returns a named logical c(ocrsphi=, csp=); NA when no solute is loaded (→ both engines offered).
  sol_engine_avail <- function(nm, smi) {
    nm <- nm %||% ""
    if (!nzchar(nm)) return(c(ocrsphi = NA, csp = NA))
    c(ocrsphi = isTRUE(db_engine_eligible(nm, "ocrsphi", smi)),
      csp     = isTRUE(db_engine_eligible(nm, "csp")))
  }
  # Eligible engine keys (ordered ocrsphi, csp). Offer BOTH when no solute is loaded yet, or when the
  # solute resolves in neither database (so the dropdown is never empty; the run then warns out-of-DB).
  sol_engine_keys <- function(nm, smi) {
    av <- sol_engine_avail(nm, smi)
    if (any(is.na(av))) return(c("ocrsphi", "csp"))
    keys <- names(av)[av]
    if (!length(keys)) c("ocrsphi", "csp") else keys
  }

  output$sol_molfrac_ui <- renderUI({
    cur <- if (is.null(input$sol_molfrac)) 5 else input$sol_molfrac
    # Restrict the engine choices to the engine(s) the loaded solute is parameterised in; preserve the
    # current selection across re-renders (mole-fraction / language changes) unless it became ineligible.
    eng_keys <- sol_engine_keys(rv$loaded_name, rv$loaded_smiles)
    eng_labels <- c(ocrsphi = i18n_r()$t("openCOSMO-RS-Phi (in database)"),
                    csp     = i18n_r()$t("CSP-2021 (in database)"))
    eng_sel <- isolate(input$sol_engine); if (is.null(eng_sel) || !eng_sel %in% eng_keys) eng_sel <- eng_keys[[1]]
    tagList(
      # Vertically centre the unit radios against the engine dropdown box (34px) so they line up.
      tags$style(HTML(".sol-unit-align .shiny-options-group{display:flex;align-items:center;min-height:34px;margin-top:0;}")),
      # Solubility-engine selector (Makarov ML / CSP-2021 / openCOSMO-RS-Phi) with the display-unit
      # radio inline to its right. The engine gets the wider column so its longer option labels don't wrap.
      fluidRow(
        column(8,
          selectInput(ns("sol_engine"),
            label = tags$span(i18n_r()$t("Solubility Engine"),
              input_help(i18n_r()$t("CSP-2021 and openCOSMO-RS-Phi are predictive COSMO-SAC-Phi equations of state; each needs the solute AND every co-solvent to be in its database and has no hard T/P domain. Being genuine n-component models, they capture true co-solvent enhancement and can also screen enhancing blends. (The Makarov ML model is not offered for co-solvent screening — its co-solvent sensitivity is too weak to rank solvents reliably; it remains available for pure-CO₂ solubility in the Separator Cascade module.)"),
                         title = i18n_r()$t("Solubility Engine"), buttonLabel = i18n_r()$t("OK"))),
            choices = setNames(eng_keys, unname(eng_labels[eng_keys])),
            selected = eng_sel, width = "100%")),
        column(4,
          div(class = "sol-unit-align",
            radioButtons(ns("sol_unit"),
              label = tags$span(i18n_r()$t("Solubility Unit"),
                input_help(i18n_r()$t("Unit for the absolute solubility shown in the results: mole fraction (mol/mol) or grams of solute per kilogram of CO2 (g/kg). The enhancement factor is unitless."),
                           title = i18n_r()$t("Solubility Unit"), buttonLabel = i18n_r()$t("OK"))),
              choices = setNames(c("molfrac", "gkg"), c(i18n_r()$t("mol/mol"), i18n_r()$t("g/kg"))),
              selected = "molfrac", inline = TRUE)))
      ),
      local_range_badge_input("sol_molfrac", i18n_r()$t("Co-Solvent Mole Fraction (%)"),
        value = cur, min_val = 0.1, max_val = 50, step = 0.1,
        range_text = "0.1–50 mol%",
        tooltip = "0.1 – 50 mol%",
        help_content = i18n_r()$t("Mole percentage of co-solvent in the CO2 stream used by the solubility model. Typical co-solvent loadings are 1-10 mol%; the model was trained up to ~50 mol%."),
        help_title = i18n_r()$t("Mole Fraction")),
      # Parallel-evaluation toggle (on by default): multi-core speed-up for the slow COSMO/EoS screening.
      checkboxInput(ns("sol_parallel"),
        label = tags$span(i18n_r()$t("Parallel evaluation"),
          input_help(i18n_r()$t("Spreads the (independent) solubility calculations across multiple CPU cores to speed up the slow COSMO-SAC-Phi (CSP-2021 / openCOSMO-RS-Phi) screening. Both the per-co-solvent scans and — when only a few co-solvents are selected — the pressure/temperature grid are split across cores, so all cores stay busy. Each worker is a separate process, so results are numerically identical to a single-core run; only the wall-clock time changes. Turn this off to run on a single core (e.g. to keep CPU free for other work)."),
                     title = i18n_r()$t("Parallel evaluation"), buttonLabel = i18n_r()$t("OK"))),
        value = TRUE),
      # Ranking metric: how co-solvents are scored and ranked (switchable; also drives the value boxes and
      # the Best-Modifier value view). Absolute gain (Δ) is the robust default; enhancement uses the median.
      radioButtons(ns("sol_metric"),
        label = tags$span(i18n_r()$t("Rank co-solvents by"),
          input_help(i18n_r()$t("How co-solvents are scored and ranked. 'Absolute gain (Δ)' — how much MORE solute dissolves thanks to the co-solvent (in the chosen unit); robust and process-relevant, and the recommended default. 'Enhancement (×)' — the relative boost over pure CO2 (y with co-solvent / y in pure CO2), aggregated by the median so a single near-critical spike no longer dominates. 'Absolute solubility' — the achieved solubility with the co-solvent. The winning co-solvent at each pressure/temperature point is the same for all three (whichever gives the highest solubility there); only the ranking, the value boxes and the value view change."),
                     title = i18n_r()$t("Rank co-solvents by"), buttonLabel = i18n_r()$t("OK"))),
        choices = setNames(c("gain", "enh", "abs"),
          c(i18n_r()$t("Absolute gain (Δ)"), i18n_r()$t("Enhancement (×)"), i18n_r()$t("Absolute solubility"))),
        selected = "gain"))
  })
  outputOptions(output, "sol_molfrac_ui", suspendWhenHidden = FALSE)

  # Notify the user when the loaded solute is parameterised in only one Solubility engine, so the engine
  # selector (which auto-restricts to eligible engines above) had to switch away from the current choice.
  # Fires only in Solubility mode and only on an actual narrowing (both-available keeps both, silently).
  observeEvent(list(rv$loaded_name, rv$loaded_smiles, input$opt_method), {
    if (!identical(input$opt_method %||% "hsp", "solubility")) return()
    nm <- rv$loaded_name %||% ""; if (!nzchar(nm)) return()
    av <- sol_engine_avail(nm, rv$loaded_smiles)
    if (any(is.na(av))) return()
    keys <- names(av)[av]
    disp <- c(ocrsphi = "openCOSMO-RS-Phi", csp = "CSP-2021")
    cur  <- isolate(input$sol_engine) %||% "ocrsphi"
    if (length(keys) == 1L && !identical(cur, keys)) {
      showNotification(sprintf(
        i18n$t("'%1$s' is not parameterised in %2$s; the Solubility Engine was switched to %3$s."),
        nm, disp[[setdiff(c("ocrsphi", "csp"), keys)]], disp[[keys]]), type = "warning", duration = 8)
    } else if (length(keys) == 0L) {
      showNotification(sprintf(
        i18n$t("'%s' is not in the CSP-2021 or openCOSMO-RS-Phi database — Solubility mode may not run; try HSP (Miscibility) mode."),
        nm), type = "warning", duration = 9)
    }
  }, ignoreInit = TRUE)

  # Solubility Unit now lives inline with the engine selector (above); keep the output as a no-op so the
  # UI's sol_unit_ui placeholder renders nothing.
  output$sol_unit_ui <- renderUI(NULL)
  outputOptions(output, "sol_unit_ui", suspendWhenHidden = FALSE)

  # ---- GSK Solvent Sustainability Guide lookup (same source as the LLE module) ----------------------
  # Matches each candidate co-solvent to the GSK guide by normalised name; returns sustainability band
  # (gsk 1-4), ICH residual-solvent class + ppm limit, food flag and key hazard, or NULL if unmatched.
  .gsk_tbl <- local({
    L <- tryCatch(supeRcrit::lle_solvents(), error = function(e) NULL)
    if (is.null(L)) NULL else { rownames(L) <- gsub("[^a-z0-9]", "", tolower(L$name)); L }
  })
  # normalized misc_opt name -> normalized GSK-guide name for the few that differ (n-alkane prefixes).
  .gsk_alias <- c(heptane = "nheptane", hexane = "nhexane")
  gsk_info <- function(solvent_raw) {
    if (is.null(.gsk_tbl) || is.null(solvent_raw) || is.na(solvent_raw)) return(NULL)
    # Try several name forms so the GSK match works whether the caller passes the raw name, an
    # abbreviation (resolved via translate_modifier -> full name), or a "CO2 + X" / "CO2 with X" prefix
    # (the GSK guide rates the SOLVENT itself, not the CO2 + solvent mixture).
    cands <- unique(c(solvent_raw, tryCatch(translate_modifier(solvent_raw), error = function(e) NULL)))
    cands <- unique(c(cands, sub("^\\s*co2\\s*(\\+|with)\\s*", "", cands, ignore.case = TRUE)))
    for (nm in cands) {
      key <- gsub("[^a-z0-9]", "", tolower(nm))
      if (!(key %in% rownames(.gsk_tbl)) && key %in% names(.gsk_alias)) key <- unname(.gsk_alias[key])
      if (key %in% rownames(.gsk_tbl)) return(.gsk_tbl[key, , drop = FALSE])
    }
    NULL
  }
  # Coloured GSK band cell (HTML) — filled dot + band label; a neutral "not rated" for solvents that
  # are not in the GSK guide (e.g. methyl oleate), rather than defaulting to a band.
  gsk_band_html <- function(gsk) {
    if (is.na(gsk)) return(sprintf("<span style='color:#999;'>&#9679; %s</span>", i18n_r()$t("not rated")))
    b <- supeRcrit::lle_solvent_band(gsk)
    sprintf("<span style='color:%s;font-weight:600;'>&#9679; %s</span>", b$colour, b$label)
  }
  # ICH class + ppm limit + key hazard, as one Notes string (blank when unmatched). The hazard phrase is
  # stored lower-case without terminal punctuation, so capitalise its first letter and make sure the whole
  # note ends in a full stop (e.g. "ICH 2. toxic if swallowed" -> "ICH 2. Toxic if swallowed.").
  gsk_notes <- function(gi) {
    if (is.null(gi)) return("")
    ich <- if (!is.na(gi$ich)) (if (is.na(gi$ppm)) sprintf("ICH %d. ", gi$ich)
                                else sprintf("ICH %d (≤%s ppm). ", gi$ich, format(gi$ppm, big.mark = ",", trim = TRUE))) else ""
    haz <- if (!is.na(gi$hazard) && nzchar(gi$hazard)) gi$hazard else ""
    if (nzchar(haz)) haz <- paste0(toupper(substr(haz, 1, 1)), substr(haz, 2, nchar(haz)))
    out <- trimws(paste0(ich, haz))
    if (nzchar(out) && !grepl("[.!?]$", out)) out <- paste0(out, ".")
    out
  }


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

  # Extra co-solvents available ONLY to the COSMO-SAC-Phi EoS engines (CSP / openCOSMO-RS-Phi), which
  # resolve solvents BY NAME against their own databases (not the Hansen `swex` set that HSP mode is
  # limited to). Curated set (present in CSP; user choice = drop dioxane/pyridine/nitromethane/formic acid,
  # keep DCM + chloroform). Names are CSP-resolvable via its normalised matching.
  eos_extra_solvents <- data.frame(
    value  = c("Isopropanol","Ethyl acetate","Tetrahydrofuran","Acetonitrile","Dimethyl sulfoxide",
               "Methyl ethyl ketone","Dichloromethane","Chloroform","Methyl acetate",
               "1-Octanol"),
    smiles = c("CC(C)O","CCOC(C)=O","C1CCOC1","CC#N","CS(C)=O","CCC(C)=O","C(Cl)Cl",
               "C(Cl)(Cl)Cl","COC(C)=O","CCCCCCCCO"),
    stringsAsFactors = FALSE)
  # Hansen modifiers that ALSO resolve in CSP (Heptane/Hexane/MethylOleate are absent from CSP -> excluded
  # from the EoS co-solvent list).
  eos_hansen_keep <- c("Acetone","Benzene","Cyclohexane","DiethylEther","Ethanol","Methanol",
                       "Toluene","PXylene","OXylene","Water")

  # Active co-solvent set (data.frame value/label/smiles). HSP + Makarov use the Hansen swex solvents;
  # the EoS engines additionally offer the curated CSP-resolvable co-solvents (and drop the 3 not in CSP).
  active_cosolv <- reactive({
    sv <- show_solv(); smi_of <- setNames(sv$SMILES, sv$Solvent)
    is_eos <- identical(input$opt_method %||% "hsp", "solubility") &&
              (input$sol_engine %||% "ocrsphi") %in% c("csp", "ocrsphi")
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

  # Helper: translate a modifier name (single, blend, or with CO2 prefix).
  # `co2_prefix`: prepend "CO2 + " so the entry reads as the CO2 + co-solvent mixture. Kept ON where it adds
  # clarity (rankings, CSV exports, run titles); turned OFF for the "best" summaries (Best Co-Solvent box,
  # Best Modifier grid) and the enhancing-blend tables, where every entry is obviously a co-solvent added to
  # CO2 and the prefix is only clutter.
  translate_modifier <- function(name, i18n_fn = i18n_r(), co2_prefix = TRUE) {
    if (length(name) != 1 || is.na(name) || !nzchar(name)) return(as.character(name))
    if (name == "CO2") return("CO2")                       # pure CO2 (callers usually map to "None")
    if (grepl("^CO2 with ", name)) name <- sub("^CO2 with ", "", name)
    pfx <- if (isTRUE(co2_prefix)) "CO2 + " else ""
    if (grepl(":", name)) {
      parts <- strsplit(name, " \\(")[[1]]
      solvent_part <- parts[1]
      frac_part <- if (length(parts) > 1) paste0(" (", parts[2]) else ""
      tokens <- strsplit(solvent_part, ":")[[1]]
      translated <- sapply(tokens, function(s) translate_solvent_token(s, i18n_fn), USE.NAMES = FALSE)
      return(paste0(pfx, paste(translated, collapse = " + "), frac_part))
    }
    return(paste0(pfx, translate_solvent_token(name, i18n_fn)))
  }

  # Inline miscibility badge appended after a blend name: partially-miscible components -> amber "partly
  # miscible"; reacting components -> red "reactive". Tooltip built the app's proven way (as in exp_db_dt):
  # i18n_r()$t() returns a real translated STRING (not a client-side <span>), attribute-escaped so quotes/<>
  # can't break the title="". Returns "" for a fully-miscible blend or a single solvent. Needs escape=FALSE
  # on the rendering table.
  misc_badge <- function(components) {
    m <- tryCatch(blend_miscibility(components), error = function(e) NULL)
    if (is.null(m)) return("")
    esc <- function(s) htmltools::htmlEscape(s, attribute = TRUE)
    chip <- function(bg, tip, lbl) sprintf(
      ' <span title="%s" style="background:%s;color:#fff;font-size:9px;font-weight:700;border-radius:3px;padding:1px 5px;margin-left:5px;vertical-align:middle;">%s</span>',
      esc(tip), bg, lbl)
    if (length(m$reaction_pairs))
      return(chip("#d9534f", i18n_r()$t("These solvents react or interact exothermically on contact (Clavilier et al. 2025); treat the predicted blend with caution."), i18n_r()$t("reactive")))
    if (length(m$partial_pairs))
      return(chip("#f0ad4e", i18n_r()$t("Partially miscible: these solvents mix only over a limited composition range (Clavilier et al. 2025), so the blend may not form a single phase at all ratios."), i18n_r()$t("partly miscible")))
    ""
  }

  # Rendered pickerInput with translated modifier names and Select All/Deselect All
  output$modifier_selection_ui <- renderUI({
    ac <- active_cosolv()   # engine-aware: extended CSP list for the EoS engines
    translated_names <- vapply(ac$label, function(k) i18n_r()$t(k), character(1))
    choices <- setNames(ac$value, translated_names)
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
    cur <- if (is.null(input$vfrac_input)) defaults$vfrac_input else input$vfrac_input
    local_range_badge_input("vfrac_input", i18n_r()$t("Co-Solvent Volume Fraction (%)"),
      value = cur, min_val = 0.1, max_val = 99.9, step = 0.1,
      range_text = "0.1\u201399.9%",
      tooltip = "0.1 \u2013 99.9%",
      help_content = i18n_r()$t("Volume percentage of co-solvent in the total solvent mixture. Typically 5-15% for SFE applications. Higher fractions increase solvent polarity but may reduce selectivity."),
      help_title = i18n_r()$t("Volume Fraction"))
  })
  outputOptions(output, "vfrac_input_ui", suspendWhenHidden = FALSE)

  output$pressure_min_ui <- renderUI({
    cur <- if (is.null(input$pressure_min)) defaults$pressure_min else input$pressure_min
    local_range_badge_input("pressure_min", HTML(paste0(i18n_r()$t("Min."), "&nbsp;<i>P</i>&nbsp;(bar)")),
      value = cur, min_val = 75, max_val = 1000,
      range_text = paste0("75\u20131000 ", i18n_r()$t("bar")),
      tooltip = paste0("75 \u2013 1000 ", i18n_r()$t("bar")),
      help_content = i18n_r()$t("Pressure range and step size for the miscibility evaluation grid. The calculation is repeated at each pressure-temperature combination within this range."),
      help_title = i18n_r()$t("Pressure Range"))
  })
  outputOptions(output, "pressure_min_ui", suspendWhenHidden = FALSE)

  output$temperature_min_ui <- renderUI({
    cur <- if (is.null(input$temperature_min)) defaults$temperature_min else input$temperature_min
    local_range_badge_input("temperature_min", HTML(paste0(i18n_r()$t("Min."), "&nbsp;<i>T</i>&nbsp;(\u00B0C)")),
      value = cur, min_val = 31, max_val = 200,
      range_text = "31\u2013200 \u00B0C",
      tooltip = "31 \u2013 200 \u00B0C",
      help_content = i18n_r()$t("Temperature range and step size for the miscibility evaluation grid. Must be above 31\u00B0C (the critical temperature of CO2)."),
      help_title = i18n_r()$t("Temperature Range"))
  })
  outputOptions(output, "temperature_min_ui", suspendWhenHidden = FALSE)

  output$pressure_max_ui <- renderUI({
    cur <- if (is.null(input$pressure_max)) defaults$pressure_max else input$pressure_max
    local_range_badge_input("pressure_max", HTML(paste0(i18n_r()$t("Max."), "&nbsp;<i>P</i>&nbsp;(bar)")),
      value = cur, min_val = 75, max_val = 1000,
      range_text = paste0("75\u20131000 ", i18n_r()$t("bar")),
      tooltip = paste0("75 \u2013 1000 ", i18n_r()$t("bar")))
  })
  outputOptions(output, "pressure_max_ui", suspendWhenHidden = FALSE)

  output$temperature_max_ui <- renderUI({
    cur <- if (is.null(input$temperature_max)) defaults$temperature_max else input$temperature_max
    local_range_badge_input("temperature_max", HTML(paste0(i18n_r()$t("Max."), "&nbsp;<i>T</i>&nbsp;(\u00B0C)")),
      value = cur, min_val = 31, max_val = 200,
      range_text = "31\u2013200 \u00B0C",
      tooltip = "31 \u2013 200 \u00B0C")
  })
  outputOptions(output, "temperature_max_ui", suspendWhenHidden = FALSE)

  # Grid step. The COSMO-SAC-Phi EoS engines (CSP / openCOSMO-RS-Phi) solve iteratively per (T,P) point
  # (~100x slower than the ML model), so they get their OWN coarser default step via separate inputs
  # (pressure_step_eos / temperature_step_eos) shown only in that mode; HSP / Makarov keep the fine
  # pressure_step / temperature_step. Both sets are saved independently, so the grid is what the user sees
  # (no run-time "grid coarsened" surprise).
  .step_is_eos <- function() identical(input$opt_method %||% "hsp", "solubility") &&
                              (input$sol_engine %||% "ocrsphi") %in% c("csp", "ocrsphi")
  output$pressure_step_ui <- renderUI({
    lab <- HTML(paste0("<i>P</i>&nbsp;", i18n_r()$t("Step"), "&nbsp;(bar)"))
    id  <- if (.step_is_eos()) "pressure_step_eos" else "pressure_step"
    dfl <- if (.step_is_eos()) defaults$pressure_step_eos else defaults$pressure_step
    cur <- if (is.null(input[[id]])) dfl else input[[id]]
    local_range_badge_input(id, lab, value = cur, min_val = 1, max_val = 100,
      range_text = paste0("1\u2013100 ", i18n_r()$t("bar")), tooltip = paste0("1 \u2013 100 ", i18n_r()$t("bar")))
  })
  outputOptions(output, "pressure_step_ui", suspendWhenHidden = FALSE)

  output$temperature_step_ui <- renderUI({
    lab <- HTML(paste0("<i>T</i>&nbsp;", i18n_r()$t("Step"), "&nbsp;(\u00B0C)"))
    id  <- if (.step_is_eos()) "temperature_step_eos" else "temperature_step"
    dfl <- if (.step_is_eos()) defaults$temperature_step_eos else defaults$temperature_step
    cur <- if (is.null(input[[id]])) dfl else input[[id]]
    local_range_badge_input(id, lab, value = cur, min_val = 1, max_val = 50,
      range_text = "1\u201350 \u00B0C", tooltip = "1 \u2013 50 \u00B0C")
  })
  outputOptions(output, "temperature_step_ui", suspendWhenHidden = FALSE)

  # Toggle button for Best Modifier Table view (solvent names vs ME values)
  output$best_table_view_toggle_ui <- renderUI({
    label <- if (rv$best_table_view == "solvent") {
      # Showing solvent names -> offer to switch to the per-cell VALUES. In Solubility mode the label names
      # the active metric; in HSP mode it stays the miscibility-enhancement ("ME") values.
      if (!is.null(rv$sol_scan)) {
        paste0(i18n_r()$t("Show"), " ", sol_metric_label(unit = input$sol_unit %||% "molfrac", mw = rv$sol_scan$scan$mw))
      } else {
        i18n_r()$t("Show ME Values")
      }
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

  # ---- Solubility-enhancement metric (switchable) -------------------------------------------------
  # The ranking, value boxes and Best-Modifier value view all follow it. "gain" (absolute Δ solubility,
  # robust to near-critical ratio spikes) is the default; "enh" is the enhancement ratio, aggregated by
  # MEDIAN so a lone near-critical spike no longer dominates; "abs" is the achieved absolute solubility.
  sol_metric <- reactive({ m <- input$sol_metric %||% "gain"; if (m %in% c("enh", "gain", "abs")) m else "gain" })

  # Unit-aware label for the current metric (table headers, toggle button, value boxes).
  sol_metric_label <- function(metric = sol_metric(), unit = input$sol_unit %||% "molfrac", mw = NA_real_) {
    gkg <- identical(unit, "gkg") && isTRUE(is.finite(mw)); u <- if (gkg) i18n_r()$t("g/kg") else i18n_r()$t("mol/mol")
    switch(metric, enh = i18n_r()$t("Enhancement (×)"),
      gain = paste0(i18n_r()$t("Solubility gain"), " (Δ ", u, ")"),
      abs  = paste0(i18n_r()$t("Solubility"), " (", u, ")")) }

  # Metric-aware ranking: median aggregates over in-domain points (in the display unit), sorted by the
  # chosen metric; plus the global peak-solubility cell used for the Best-Modifier highlight. Reactive on
  # metric/unit so switching is instant (no re-scan). `nms` are the single co-solvents (blends excluded).
  sol_ranked <- reactive({
    req(rv$sol_scan); scan <- rv$sol_scan$scan
    metric <- sol_metric(); unit <- input$sol_unit %||% "molfrac"; mw <- scan$mw
    gkg <- identical(unit, "gkg") && isTRUE(is.finite(mw)); dom <- scan$domain
    nms <- rv$sol_scan$ranking$Modifier
    conv <- function(y) if (gkg) solub_gkg(y, mw) else y
    rows <- lapply(nms, function(nm) {
      enh <- scan$enhancement[[nm]]; sol <- scan$solubility[[nm]]; pur <- scan$pure; inm <- dom & is.finite(enh)
      md <- function(v) if (any(inm)) stats::median(v[inm], na.rm = TRUE) else NA_real_
      data.frame(Modifier = nm, Enh = md(enh), Gain = md(conv(sol) - conv(pur)), Sol = md(conv(sol)),
                 stringsAsFactors = FALSE) })
    df <- do.call(rbind, rows)
    key <- switch(metric, enh = "Enh", gain = "Gain", abs = "Sol")
    df <- df[order(df[[key]], decreasing = TRUE), , drop = FALSE]
    peak <- NULL; best_y <- -Inf                          # global argmax achieved solubility (in-domain)
    for (nm in nms) { s <- scan$solubility[[nm]]; s[!dom] <- NA
      if (any(is.finite(s))) { wi <- which.max(s)
        if (isTRUE(s[wi] > best_y)) { best_y <- s[wi]
          peak <- list(nm = nm, r = rownames(s)[((wi - 1) %% nrow(s)) + 1], c = colnames(s)[((wi - 1) %/% nrow(s)) + 1]) } } }
    list(df = df, metric = metric, key = key, unit = unit, gkg = gkg, mw = mw, peak = peak) })

  # Per-point value string for the Best-Modifier value view (`b` = the winning co-solvent at that cell).
  sol_point_value <- function(scan, b, rn, cn, metric, gkg, mw) {
    yc <- tryCatch(scan$solubility[[b]][rn, cn], error = function(e) NA_real_); yp <- scan$pure[rn, cn]
    if (!isTRUE(is.finite(yc))) return(NA_character_)
    if (metric == "enh") return(paste0(round(yc / yp, 2), "×"))
    cvt <- function(y) if (gkg) solub_gkg(y, mw) else y
    v <- if (metric == "gain") cvt(yc) - cvt(yp) else cvt(yc)
    formatC(v, format = if (gkg) "fg" else "e", digits = if (gkg) 3 else 2) }

  # Shared colour scheme for the Best Modifier table / grid / legend. Each co-solvent gets a FIXED colour
  # that is a pure function of its name, so a given solvent is always shown in the same colour across runs
  # and across solutes (previously colours were assigned by the solvent's sorted position among whichever
  # solvents happened to win in that run, so e.g. ethanol could be red in one run and blue in the next).
  # A distinct colour is hand-assigned to the common co-solvents (keyed on a normalised name); any other
  # solvent gets a stable hash of its name into the same palette.
  palette_base <- c(
    "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#A65628", "#F781BF",
    "#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#8DA0CB",
    "#1F78B4", "#B15928", "#66C2A5", "#FB8072", "#BC80BD", "#FDB462"
  )
  .solvent_norm <- function(x) gsub("[^a-z0-9]", "", tolower(x))
  solvent_fixed_colors <- c(
    ethanol = "#377EB8", methanol = "#4DAF4A", water = "#1F78B4", acetone = "#984EA3",
    isopropanol = "#FF7F00", `2propanol` = "#FF7F00", propan2ol = "#FF7F00",
    `1propanol` = "#A6761D", propan1ol = "#A6761D", `1butanol` = "#BF812D", `2butanol` = "#D9A441",
    toluene = "#E41A1C", benzene = "#E7298A", oxylene = "#FB8072", pxylene = "#80B1D3", xylene = "#FDB462",
    hexane = "#A65628", heptane = "#8C510A", cyclohexane = "#8DA0CB",
    ethylacetate = "#1B9E77", acetonitrile = "#D95F02", chloroform = "#7570B3",
    dichloromethane = "#66A61E", dcm = "#66A61E", diethylether = "#E6AB02",
    `1octanol` = "#F781BF", octanol = "#F781BF", methyloleate = "#66C2A5",
    thf = "#B2DF8A", tetrahydrofuran = "#B2DF8A", dmso = "#BC80BD", dmf = "#CAB2D6",
    limonene = "#33A02C", ethyllactate = "#6A3D9A", carbondioxide = "#9E9E9E"
  )
  solvent_color <- function(name) {
    key <- .solvent_norm(name)
    if (!is.na(key) && nzchar(key) && key %in% names(solvent_fixed_colors)) return(unname(solvent_fixed_colors[[key]]))
    if (is.na(key) || !nzchar(key)) return(palette_base[1])
    idx <- (sum(utf8ToInt(key)) %% length(palette_base)) + 1L   # stable per-name hash into the palette
    palette_base[idx]
  }

  modifier_color_map <- reactive({
    # Mode-aware: HSP reads sfe_mod's Best_Modifier; Solubility (ML) reads the solub_optim best grid.
    best_mod_data <- if (!is.null(rv$sol_scan)) rv$sol_scan$scan$best
      else { req(rv$sfe_mod_output); rv$sfe_mod_output$sfe$Best_Modifier }
    if (is.null(best_mod_data) || length(best_mod_data) == 0) return(NULL)

    methods <- sort(unique(unlist(best_mod_data)))
    methods <- methods[methods != "CO2" & !is.na(methods)]
    if (length(methods) == 0) return(NULL)

    base_cols <- vapply(methods, solvent_color, character(1))   # fixed colour per solvent name
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

    # Deduplicate by translated name (Best Modifier table + legend: no "CO2 +" prefix)
    translated <- sapply(methods, function(m) translate_modifier(m, co2_prefix = FALSE))
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

  # Curated, non-overlapping example solutes per Selection Method. Every seed technically works in
  # both modes (all carry SMILES + GCM params), but HSP (Miscibility) and Solubility (ML) get distinct
  # example sets so switching modes loads a fresh, mode-appropriate set rather than reporting the same
  # examples as "already loaded".
  opt_example_names <- function(mode) {
    if (identical(mode, "solubility"))
      c("Naphthalene", "Anthracene", "Caffeine", "Vanillin", "Thymoquinone")   # scCO2-solubility examples
    else
      c("Limonene", "Pinene", "Carotene", "Capsaicin", "Cholesterol")          # miscibility (HSP) examples
  }

  # Load Example Data button handler
  observeEvent(input$load_example_data, {
    seed_data <- sfe_rv$seed_calculations
    if (length(seed_data) == 0) {
      showNotification(i18n$t("No example data available."), type = "warning")
      return()
    }

    # Only the examples appropriate to the currently-selected method (HSP vs ML).
    want <- opt_example_names(if (is.null(input$opt_method)) "hsp" else input$opt_method)
    seed_data <- Filter(function(x) (x$parameters$name_input %||% "") %in% want, seed_data)
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

  # "Load from Database" button — inline with Load Examples / Load from Disk. Offered for HSP only
  # (ML mode needs just a SMILES, entered via a saved calc).
  output$db_load_btn <- renderUI({
    # Available in BOTH modes — the DB modal filters solutes by method (Solubility (ML) needs a SMILES,
    # HSP needs measured HSP + Tc), so ML-suitable compounds are offered in Solubility mode too.
    actionButton(ns("open_db_modal"), i18n_r()$t("Load from Database"), icon = icon("database"),
      class = "btn btn-success btn-sm", style = "color:white; margin-left: 5px;")
  })
  outputOptions(output, "db_load_btn", suspendWhenHidden = FALSE)

  # Render the list of saved calculations
  # Is a saved calculation usable with the selected optimisation mode?
  #  - Solubility (ML): needs a SMILES (predict_solubility works from structure).
  #  - HSP (Miscibility): needs Hansen parameters AND a critical temperature — either a DB-experimental
  #    compound (measured Tc + HSP) or a GCM calc with both an HSP method and a critical-parameter method.
  calc_suitable_opt <- function(p, method) {
    if (method %in% c("csp", "ocrsphi")) {            # database-limited EoS: must resolve in that database
      nm <- p$name_input %||% ""
      return(nzchar(nm) && isTRUE(db_engine_eligible(nm, method, p$smiles_input %||% NULL)))
    }
    if (identical(method, "solubility")) return(nzchar(p$smiles_input %||% ""))
    isTRUE(p$db_experimental) ||
      (!identical(p$gcm_hsp %||% "none", "none") && !identical(p$gcm_crit %||% "none", "none"))
  }

  output$saved_calculations_list_ui <- renderUI({
    # Create reactive dependency on saved_calculations + the selected mode (+ solubility engine)
    calcs <- sfe_rv$saved_calculations
    method <- input$opt_method %||% "hsp"
    if (identical(method, "solubility") && (input$sol_engine %||% "ocrsphi") %in% c("csp", "ocrsphi")) method <- input$sol_engine

    if (length(calcs) == 0) {
      return(tagList(
        p(i18n$t("No saved GCM calculations found.")),
        p(style = "font-size: 0.9em; color: #888; margin-top: 10px;",
          sprintf(i18n$t("Calculations from %s will appear here."), i18n$t("Solute Characterization")))
      ))
    }

    # Suitability for the selected mode. Unsuitable calcs stay in the list but are greyed out and
    # disabled inline (not moved to a separate list). All examples now carry a SMILES + HSP methods,
    # so this only ever greys out user calcs that genuinely lack the data a mode needs.
    ok <- vapply(calcs, function(c) calc_suitable_opt(c$parameters, method), logical(1))
    suit_idx <- which(ok); unsuit_idx <- which(!ok)
    need <- switch(method,
      solubility = i18n$t("needs a SMILES"),
      csp        = i18n$t("not in the CSP-2021 database"),
      ocrsphi    = i18n$t("not in the openCOSMO-RS-Phi database"),
      i18n$t("needs Hansen parameters and a critical temperature"))

    has_selection <- !is.null(rv_selected_calc_id()) && rv_selected_calc_id() %in% suit_idx

    delete_btn <- actionButton(
      ns("delete_selected_calc"),
      label = i18n$t("Remove Selected from Session"),
      icon = icon("trash-alt"),
      class = "btn-xs btn-danger",
      style = "color: white;"
    )
    save_btn <- actionButton(ns("save_selected_calcs"), label = i18n$t("Save Selected to Disk"),
      icon = icon("save"), class = "btn-xs btn-primary", style = "color: white;")
    if (!has_selection) {
      delete_btn <- shinyjs::disabled(delete_btn)
      save_btn <- shinyjs::disabled(save_btn)
    }

    # One list; unsuitable rows are greyed via the label and disabled via a small script so they can't
    # be selected. choice VALUE = the real index into saved_calculations.
    choice_names <- lapply(seq_along(calcs), function(i) {
      nm <- htmltools::htmlEscape(calcs[[i]]$display_name)
      if (ok[i]) HTML(nm)
      else HTML(sprintf("<span style='color:#bbb; font-style:italic;'>%s <small>(%s)</small></span>",
                        nm, htmltools::htmlEscape(need)))
    })
    radio <- radioButtons(ns("saved_calculation_radio"), label = i18n$t("Select a saved calculation"),
      choiceNames = choice_names, choiceValues = as.character(seq_along(calcs)),
      selected = if (has_selection) as.character(rv_selected_calc_id()) else character(0))

    disable_js <- if (length(unsuit_idx))
      paste(sprintf("$(\"#%s input[value='%d']\").prop('disabled', true).closest('.radio').css('opacity','0.5');",
                    ns("saved_calculation_radio"), unsuit_idx), collapse = " ")
    else ""
    disable_script <- if (nzchar(disable_js))
      tags$script(HTML(sprintf("setTimeout(function(){%s}, 0);", disable_js))) else NULL

    tagList(
      div(style = if (length(calcs) > 6) "margin-bottom: 10px; max-height: 300px; overflow-y: auto;" else "margin-bottom: 10px;",
        radio, disable_script),
      div(style = "margin-top: -5px; display:flex; gap:6px; flex-wrap:wrap;", delete_btn, save_btn),
      p(style = "font-size: 0.9em; color: #888; margin-top: 10px;",
        sprintf(i18n$t("Showing %d of %d calculation(s) suitable for this mode."), length(suit_idx), length(calcs)))
    )
  })

  # Save the radio-selected saved calculation to disk (JSON; round-trips via Load from Disk).
  setup_save_selected_to_disk(input, session, i18n_r, list(
    trigger = "save_selected_calcs", confirm = "save_sel_confirm", dir_input = "save_sel_dir", browse = "save_sel_browse",
    default_dir = function() supercrit_user_dir(file.path("user-settings", "sfe_gcm_calculations")),
    collect = function() {
      sel <- input$saved_calculation_radio
      if (is.null(sel) || !length(sel) || !nzchar(sel)) return(NULL)
      cc <- sfe_rv$saved_calculations[[as.integer(sel)]]
      if (is.null(cc)) return(NULL)
      list(list(name = cc$parameters$name_input %||% cc$display_name %||% "calculation",
        data = list(display_name = cc$display_name, timestamp = cc$timestamp,
                    source_module = cc$source_module, parameters = cc$parameters,
                    metadata = list(created_date = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), app_version = "0.9.0"))))
    }))

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
    rv$loaded_tm <- calc$parameters$melting_point   # NULL for calcs saved before Tm was added
    rv$loaded_crit <- calc$parameters$gcm_crit
    rv$loaded_hsp <- calc$parameters$gcm_hsp
    rv$loaded_simplicity <- calc$parameters$gcm_simplicity
    rv$loaded_gorder <- calc$parameters$gcm_gorder
    # Experimental override carried by DB-loaded compounds: measured Tc + Hansen parameters used
    # directly in HSP mode (no GCM estimation). NULL/FALSE for ordinary Solute-Characterization calcs.
    rv$loaded_db_exp   <- isTRUE(calc$parameters$db_experimental)
    rv$loaded_exp_crit <- calc$parameters$exp_crit_value
    rv$loaded_exp_hsp  <- calc$parameters$exp_hsp_values

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

  # ---- Load a compound from the experimental database (method-filtered) -> synthetic saved-calc ----
  db_refresh_opt <- reactiveVal(0)
  # The active EoS engine (CSP-2021 / openCOSMO-RS-Phi) when Solubility mode is on — enables the modal's
  # "Engine compounds" source so you can pick ANY compound the engine predicts, not only the measured subset.
  eng_now <- reactive(if (identical(input$opt_method %||% "hsp", "solubility") &&
    (input$sol_engine %||% "ocrsphi") %in% c("csp", "ocrsphi")) input$sol_engine else NULL)
  observeEvent(input$open_db_modal, showModal(exp_db_modal(ns, i18n, engine = eng_now())))
  db_filtered_opt <- reactive({ db_refresh_opt()
    eng <- eng_now(); d <- exp_db_source(input, eng); if (is.null(d) || !nrow(d)) return(NULL)
    # "Engine compounds" source = the engine's FULL list; it's already the eligible set, so skip the
    # category/method/SFE filters (the engine rows aren't categorised).
    if (isTRUE(input$db_source == "engine") && !is.null(eng)) return(d)
    # Experimental DB: in Solubility mode the EoS engines (csp/ocrsphi) restrict Load-from-DB to compounds
    # in their own database; Makarov keeps the SMILES filter.
    meth <- input$opt_method %||% "hsp"
    if (identical(meth, "solubility") && (input$sol_engine %||% "ocrsphi") %in% c("csp", "ocrsphi")) meth <- input$sol_engine
    d <- d[exp_db_method_keep(d, meth), , drop = FALSE]
    exp_db_filtered(d, input$db_category, isTRUE(input$db_sfe_only)) })
  output$db_table <- DT::renderDataTable({ exp_db_dt(i18n_r, db_filtered_opt()) }, server = TRUE)  # server-side: only the visible page is sent (fast); full-DB export via exp_db_wire_download
  exp_db_wire_download(output, session, db_filtered_opt)
  exp_db_wire_category(input, session)
  observeEvent(input$db_add_save, {
    if (isTRUE(exp_db_add_save(input, session, i18n, close = FALSE))) db_refresh_opt(db_refresh_opt() + 1) })
  exp_db_wire_edit_delete(input, output, session, i18n,
    refresh = function() db_refresh_opt(db_refresh_opt() + 1), db_fn = db_filtered_opt)
  observeEvent(input$db_load_selected, {
    sel <- input$db_table_rows_selected
    if (is.null(sel) || !length(sel)) { showNotification(i18n$t("Select a compound first."), type = "warning"); return() }
    db <- db_filtered_opt(); req(db); row <- db[sel, ]
    meth <- input$opt_method %||% "hsp"
    exp_hsp <- suppressWarnings(as.numeric(c(row$dD, row$dP, row$dHB))); Tc <- suppressWarnings(as.numeric(row$Tc))
    has_exp_hsp <- all(is.finite(exp_hsp)) && is.finite(Tc)   # carry measured HSP regardless of load mode
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
    removeModal()
    updateRadioButtons(session, "saved_calculation_radio", selected = as.character(idx))
    showNotification(sprintf(i18n$t("Loaded '%s' from the database."), row$name), type = "message", duration = 3)
  })

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
          translate_modifier(solvent, co2_prefix = FALSE)
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
    unit_lab <- if (identical(input$opt_method %||% "hsp", "hsp")) i18n_r()$t("vol %") else i18n_r()$t("mol %")
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
      list(if (.step_is_eos()) input$pressure_step_eos else input$pressure_step, 1, 100, "Pressure Step (bar)"),
      list(if (.step_is_eos()) input$temperature_step_eos else input$temperature_step, 1, 50, "Temperature Step (\u00B0C)")
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

    # ---- Solubility (ML) mode: rank co-solvents by predicted scCO₂ solubility enhancement ----
    # Parallel path to the HSP miscibility optimisation below; uses cosolvent_solubility_scan
    # (predict_solubility) instead of sfe_mod. Needs only the loaded solute SMILES (+ its Tm, estimated
    # by gcm_mp when not supplied). Single co-solvents only — blends are an HSP-mode feature.
    if (identical(input$opt_method, "solubility")) {
      smi <- rv$loaded_smiles
      if (is.null(smi) || !nzchar(smi)) {
        showNotification(i18n$t("Solubility mode needs the solute SMILES. Load a calculation that has a SMILES string."), type = "error"); return(NULL)
      }
      req(input$pressure_min, input$pressure_max, input$temperature_min, input$temperature_max)
      if (input$pressure_min >= input$pressure_max) {
        showNotification(i18n$t("Minimum pressure must be less than maximum pressure."), type = "error"); return(NULL) }
      if (input$temperature_min >= input$temperature_max) {
        showNotification(i18n$t("Minimum temperature must be less than maximum temperature."), type = "error"); return(NULL) }
      # Automatic Solvent Finder screens every candidate co-solvent; otherwise use the manual selection.
      auto_finder <- isTRUE(input$auto_finder)
      sel <- if (auto_finder) unname(active_cosolv()$value) else input$modifier_selection
      eng <- input$sol_engine %||% "ocrsphi"; is_eos <- eng != "makarov"
      # The EoS engines are DATABASE-LIMITED: verify the loaded solute is actually in the selected engine's
      # database up front. Without this, a solute picked under one engine (e.g. Caffeine in CSP-2021) that
      # is not supported after switching engines would still "run" and silently produce an all-NA scan with
      # no results. Fail fast with a clear, specific message instead.
      if (is_eos) {
        nm <- rv$loaded_name %||% ""
        if (!nzchar(nm)) {
          showNotification(i18n$t("Solubility mode needs a loaded solute — load a calculation first."), type = "error"); return(NULL) }
        if (!isTRUE(db_engine_eligible(nm, eng, smi))) {
          eng_lab <- switch(eng, csp = "CSP-2021", ocrsphi = "openCOSMO-RS-Phi", eng)
          showNotification(sprintf(i18n$t("'%s' is not in the %s database, so this engine cannot predict its solubility. Select a solute the engine supports, or switch the solubility engine."), nm, eng_lab), type = "error", duration = 8); return(NULL) }
      }
      # Manual custom blends are evaluated by the n-component EoS engines (CSP / openCOSMO-RS-Phi). The
      # Makarov ML model has no blend-mixing rule, so any saved blends are ignored there.
      use_manual_blends <- is_eos && !auto_finder && length(rv$saved_blends) > 0
      if (!is_eos && length(rv$saved_blends) > 0)
        showNotification(i18n$t("The Makarov ML model evaluates single co-solvents only; saved blends are ignored. Use CSP-2021 or openCOSMO-RS-Phi to screen blends."), type = "warning")
      if (!auto_finder && length(sel) == 0 && !use_manual_blends) {
        showNotification(i18n$t("Please select at least one co-solvent (or add a blend)."), type = "error"); return(NULL) }

      # Mode-specific grid step: the EoS engines use their own coarser input (pressure_step_eos /
      # temperature_step_eos); HSP / Makarov use the fine pressure_step / temperature_step. Used as-is.
      step_p <- if (is_eos) input$pressure_step_eos else input$pressure_step
      step_t <- if (is_eos) input$temperature_step_eos else input$temperature_step
      pres_step  <- if (!is.null(step_p) && step_p > 0) step_p else (if (is_eos) 15 else 5)
      temps_step <- if (!is.null(step_t) && step_t > 0) step_t else (if (is_eos) 6 else 3)
      pres_seq  <- seq(input$pressure_min, input$pressure_max, by = pres_step)
      if (tail(pres_seq, 1) != input$pressure_max) pres_seq <- c(pres_seq, input$pressure_max)
      temps_seq <- seq(input$temperature_min, input$temperature_max, by = temps_step)
      if (tail(temps_seq, 1) != input$temperature_max) temps_seq <- c(temps_seq, input$temperature_max)
      xfrac <- (if (is.null(input$sol_molfrac)) 5 else input$sol_molfrac) / 100

      ac <- active_cosolv(); cosolv_smiles <- setNames(ac$smiles, ac$value)[sel]
      # Optional EXPERIMENTAL binary-blend screening (Auto Finder only): every two-solvent 50:50 blend,
      # scored by mole-fraction feature superposition in predict_solubility. Blends are an extrapolation of
      # a single-co-solvent-trained model — reliable for ranking, indicative in absolute value.
      # Prefer the solute's known melting point (manual / experimental / ML from Solute Characterization);
      # solub_optim estimates it with gcm_mp only when none is supplied.
      tm_use <- suppressWarnings(as.numeric(rv$loaded_tm))
      tm_use <- if (length(tm_use) == 1 && is.finite(tm_use)) tm_use else NULL
      # Auto blend screening: CSP / openCOSMO-RS-Phi are genuine n-component models. When "Also screen
      # enhancing blends" is ticked, solub_optim evaluates 50:50 blends of the top single co-solvents
      # directly via the EoS (capped at top-N to stay tractable). Makarov ML has no blend rule.
      blend_top_n <- if (is_eos && auto_finder && isTRUE(input$screen_blends)) 6L else 0L
      # Manual custom blends -> solub_optim `blends` (n-component EoS solve). Each entered percentage is a
      # co-solvent proportion, scaled by the overall co-solvent mole fraction xfrac (so CO2 is 1 - xfrac,
      # split among the blend components by proportion).
      blends_arg <- if (use_manual_blends)
        setNames(lapply(rv$saved_blends, function(b) list(names = b$solvents, x = xfrac * (b$fractions / sum(b$fractions)))),
                 vapply(rv$saved_blends, function(b) b$preview %||% paste(b$solvents, collapse = " + "), character(1)))
      else NULL
      # Warn (but still compute) when a manual blend's components are not fully miscible.
      if (use_manual_blends) for (b in rv$saved_blends) {
        mm <- tryCatch(blend_miscibility(b$solvents), error = function(e) NULL)
        if (!is.null(mm) && !isTRUE(mm$miscible)) {
          pr <- b$preview %||% paste(b$solvents, collapse = " + ")
          showNotification(
            if (length(mm$reaction_pairs)) sprintf(i18n$t("Blend '%s': its solvents react or interact on contact; the result is shown but treat it with caution."), pr)
            else if (length(mm$immiscible_pairs)) sprintf(i18n$t("Blend '%s': its solvents are immiscible (phase-separating); the predicted blend may not form a single phase."), pr)
            else sprintf(i18n$t("Blend '%s': its solvents are only partially miscible; the blend may not form a single phase at all ratios."), pr),
            type = "warning", duration = 8)
        }
      }
      # (No run-time coarsening: the EoS engines already default to a coarser grid via their own
      # pressure_step_eos / temperature_step_eos inputs, set above and used verbatim.)
      # The (slow) scan runs in a background process (launched below); this closure post-processes its
      # result once it finishes and is invoked by the poller. Captures is_eos / xfrac from this click.
      finalize <- function(scan) {
      if (is.null(scan) || !length(scan$enhancement) || all(vapply(scan$enhancement, function(m) all(!is.finite(m)), logical(1)))) {
        showNotification(if (is_eos) i18n$t("No results: the selected EoS engine needs the solute and co-solvent(s) in its database (see the model help).") else i18n$t("No solubility results were produced."), type = "warning"); return(NULL) }

      dom <- scan$domain
      bn_set <- scan$blend_names %||% character(0)
      mk_row <- function(nm) { enh <- scan$enhancement[[nm]]; sol <- scan$solubility[[nm]]; inm <- dom & is.finite(enh)
        data.frame(Modifier = nm,
          MeanEnh = if (any(inm)) mean(enh[inm]) else NA_real_,
          MaxEnh  = if (any(inm)) max(enh[inm])  else NA_real_,
          MeanSol = if (any(inm)) mean(sol[inm]) else NA_real_, stringsAsFactors = FALSE) }
      # Singles -> Summary ranking; blends -> dedicated blends tab.
      single_nms <- setdiff(names(scan$enhancement), bn_set)
      rank_df <- do.call(rbind, lapply(single_nms, mk_row))
      rank_df <- rank_df[order(rank_df$MeanEnh, decreasing = TRUE), , drop = FALSE]
      blend_df <- if (length(bn_set)) { bd <- do.call(rbind, lapply(bn_set, mk_row))
                    bd[order(bd$MeanEnh, decreasing = TRUE), , drop = FALSE] } else NULL

      rv$sfe_mod_output <- NULL                       # clear HSP results so HSP-only renders stay blank
      rv$sol_scan <- list(scan = scan, ranking = rank_df, blend_ranking = blend_df,
                          Tm = scan$Tm, x = xfrac, n_out = sum(!dom))
      # Populate the Enhancement-tab co-solvent dropdown for ML mode (singles only).
      rv$me_solvent_list <- single_nms
      updateSelectInput(session, "miscibility_enhancement_solvent_selection",
        choices = setNames(rv$me_solvent_list, sapply(rv$me_solvent_list, translate_modifier)),
        selected = rv$me_solvent_list[1])
      if (any(!dom))
        showNotification(sprintf(i18n$t("%d of %d pressure/temperature points are outside the solubility model's reliable range (308-373 K, 80-360 bar)."),
                                 sum(!dom), length(dom)), type = "warning", duration = 6)

      output$best_modifier_vb <- renderValueBox({
        req(sol_ranked()); best <- sol_ranked()$df$Modifier[1]; disp <- translate_modifier(best, co2_prefix = FALSE)
        fs <- if (nchar(disp) > 30) "14px" else if (nchar(disp) > 20) "18px" else if (nchar(disp) > 14) "22px" else "28px"
        valueBox(tags$span(disp, style = paste0("font-size:", fs)), i18n$t("Best Co-Solvent"), icon = icon("star"), color = "green") })
      output$max_me_vb <- renderValueBox({
        req(sol_ranked()); sr <- sol_ranked(); v <- sr$df[[sr$key]]; v <- v[is.finite(v)]
        rng <- if (!length(v)) "—" else if (sr$metric == "enh")
          paste0(round(min(v), 1), " – ", round(max(v), 1), "×")
        else paste0(formatC(min(v), format = if (sr$gkg) "fg" else "e", digits = if (sr$gkg) 3 else 2),
                    " – ", formatC(max(v), format = if (sr$gkg) "fg" else "e", digits = if (sr$gkg) 3 else 2))
        # Shrink the font for long range strings (e.g. "1.91e-02 – 1.26e-01") so they don't overflow the box.
        fs <- if (nchar(rng) > 18) "15px" else if (nchar(rng) > 13) "19px" else "24px"
        valueBox(tags$span(rng, style = paste0("font-size:", fs)), sol_metric_label(sr$metric, sr$unit, sr$mw),
                 icon = icon("up-right-and-down-left-from-center"), color = "blue") })
      output$num_modifiers_vb <- renderValueBox({
        req(rv$sol_scan); valueBox(nrow(rv$sol_scan$ranking), i18n$t("Solvents Tested"), icon = icon("flask"), color = "yellow") })

      # Best Modifier grid for Solubility (ML) mode: the highest-enhancement co-solvent at each
      # pressure/temperature point, colour-coded (mirrors the HSP grid). The toggle switches between
      # co-solvent names and the enhancement (×) values; "CO2" means no co-solvent beats pure CO2.
      output$best_modifier_table <- renderDT({
        req(rv$sol_scan)
        view_mode <- rv$best_table_view
        display_mode <- if (!is.null(input$table_display_mode)) input$table_display_mode else "fit"
        bar_label <- i18n_r()$t("bar")
        max_fit_rows <- 10; max_fit_cols <- 10

        best_mat <- rv$sol_scan$scan$best
        scan <- rv$sol_scan$scan; metric <- sol_metric()
        unit <- input$sol_unit %||% "molfrac"; mw <- scan$mw; gkg <- identical(unit, "gkg") && isTRUE(is.finite(mw))
        peak <- sol_ranked()$peak                          # cell with the globally highest achieved solubility
        req(!is.null(best_mat), length(best_mat) > 0)

        if (display_mode == "fit") {
          if (nrow(best_mat) > max_fit_rows)
            best_mat <- best_mat[round(seq(1, nrow(best_mat), length.out = max_fit_rows)), , drop = FALSE]
          if (ncol(best_mat) > max_fit_cols)
            best_mat <- best_mat[, round(seq(1, ncol(best_mat), length.out = max_fit_cols)), drop = FALSE]
        }

        cmap <- modifier_color_map()
        color_lookup <- if (!is.null(cmap)) setNames(cmap$bg_colors, cmap$methods) else character(0)
        pressures <- as.numeric(rownames(best_mat)); temperatures <- as.numeric(colnames(best_mat))

        cell_html <- function(text, bg, outline = FALSE) {
          ol <- if (isTRUE(outline)) "box-shadow:inset 0 0 0 3px #111;border-radius:2px;" else ""
          if (!is.null(bg) && !is.na(bg) && nchar(bg) > 0)
            paste0('<div style="background-color:', bg, ';', ol,
              'font-weight:bold;padding:4px;margin:-4px;text-align:center;">', text, '</div>')
          else if (isTRUE(outline))
            paste0('<div style="', ol, 'font-weight:bold;padding:4px;margin:-4px;text-align:center;">', text, '</div>')
          else as.character(text)
        }

        disp <- matrix("", nrow(best_mat), ncol(best_mat))
        for (ri in seq_len(nrow(best_mat))) for (ci in seq_len(ncol(best_mat))) {
          b <- best_mat[ri, ci]; rn <- rownames(best_mat)[ri]; cn <- colnames(best_mat)[ci]
          # b may be NA, "CO2" (no co-solvent wins — common for ocrsphi where few compounds are eligible),
          # or a co-solvent not in the colour map; [[ on a named vector errors ("subscript out of bounds")
          # for those, so look up by membership and fall back to no background.
          bg <- if (length(color_lookup) && !is.na(b) && b %in% names(color_lookup)) color_lookup[[b]] else NA_character_
          if (view_mode == "me") {
            v <- if (!is.na(b) && b != "CO2") sol_point_value(scan, b, rn, cn, metric, gkg, mw) else NA_character_
            text <- if (is.na(v)) "" else v
          } else {
            text <- if (is.na(b)) "" else if (b == "CO2") i18n_r()$t("None") else translate_modifier(b, co2_prefix = FALSE)
          }
          # Outline the single cell where the highest absolute solubility is achieved.
          is_peak <- !is.null(peak) && !is.na(b) && identical(as.character(b), peak$nm) &&
                     identical(rn, peak$r) && identical(cn, peak$c)
          disp[ri, ci] <- cell_html(text, bg, outline = is_peak)
        }

        df <- as.data.frame(disp, stringsAsFactors = FALSE)
        colnames(df) <- paste0(temperatures, " °C")
        rownames(df) <- paste0("<b>", pressures, " ", bar_label, "</b>")

        table_options <- if (display_mode == "fit")
          list(dom = "t", pageLength = nrow(df), paging = FALSE, searching = FALSE,
               scrollX = FALSE, scrollY = FALSE, autoWidth = TRUE)
        else
          list(dom = "t", pageLength = nrow(df), paging = FALSE, searching = FALSE,
               scrollX = TRUE, scrollY = "400px", autoWidth = TRUE,
               initComplete = htmlwidgets::JS(
                 "function(settings, json) {",
                 "  $(this.api().table().container()).find('td, th').css('white-space', 'nowrap');",
                 "}"))
        datatable(df, options = table_options, rownames = TRUE, escape = FALSE)
      })

      output$modifier_ranking_table <- DT::renderDataTable({
        req(sol_ranked()); sr <- sol_ranked(); df <- sr$df; gkg <- sr$gkg
        u <- if (gkg) i18n_r()$t("g/kg") else i18n_r()$t("mol/mol")
        gis <- lapply(df$Modifier, gsk_info)   # GSK sustainability for every co-solvent (all engines)
        # All three metrics shown (median over in-domain points, in the display unit), rows already sorted
        # by the selected metric; the active metric's column is emphasised.
        disp <- data.frame(Modifier = sapply(df$Modifier, translate_modifier),
          Gain = signif(df$Gain, 3), Enh = round(df$Enh, 2), Sol = signif(df$Sol, 3),
          Sustainability = vapply(gis, function(gi) gsk_band_html(if (is.null(gi)) NA_integer_ else as.integer(gi$gsk)), character(1)),
          Notes = vapply(gis, gsk_notes, character(1)),
          check.names = FALSE, stringsAsFactors = FALSE)
        active_col <- switch(sr$metric, gain = "Gain", enh = "Enh", abs = "Sol")
        DT::datatable(disp, escape = FALSE,
          colnames = c(i18n_r()$t("Co-Solvent"),
                       paste0(i18n_r()$t("Median gain"), " (Δ ", u, ")"),
                       i18n_r()$t("Median Enhancement (×)"),
                       paste0(i18n_r()$t("Median solubility"), " (", u, ")"),
                       i18n_r()$t("Sustainability"), i18n_r()$t("Notes")),
          caption = htmltools::tags$caption(style = "caption-side: bottom; font-size: 0.82em; color:#777;",
            i18n_r()$t("Absolute-solubility scale in pure CO2 — extremely insoluble ~1e-7 mol/mol (~1e-4 g/kg, e.g. disperse dyes); typical ~1e-4 (~0.7 g/kg); highly soluble ~0.1 (~300 g/kg, e.g. naphthalene).")),
          rownames = FALSE, extensions = "Buttons",
          # Align headers WITH their column content: numeric cols (Gain/Enh/Sol) right, Sustainability centred;
          # dt-right/-center classes apply to both <th> and <td>, so headers no longer sit left of the data.
          options = list(scrollX = TRUE, paging = FALSE, language = tablang(), dom = "Bfrtip",
                         columnDefs = list(list(className = "dt-right", targets = c(1, 2, 3)),
                                           list(className = "dt-center", targets = 4)),
                         buttons = create_dt_export_buttons(i18n_r, "supercrit_cosolvent_solubility"))) %>%
          # No / negligible solubility boost (median enhancement <= 1.05x) flagged red bg + white text.
          DT::formatStyle("Enh",
            backgroundColor = DT::styleInterval(1.05, c("#dc3545", "")),
            color = DT::styleInterval(1.05, c("white", ""))) %>%
          # Emphasise the column of the currently-selected ranking metric.
          DT::formatStyle(active_col, fontWeight = "bold", backgroundColor = "#eef5ff") })
      }  # ---- end finalize(scan) closure ----

      # Launch the (slow) EoS/COSMO scan in a BACKGROUND process (callr) so the UI stays responsive and the
      # run is cancellable. solub_optim writes stage/per-co-solvent progress markers into progdir, polled below.
      if (!is.null(sol_async$proc) && isTRUE(tryCatch(sol_async$proc$is_alive(), error = function(e) FALSE))) {
        showNotification(i18n$t("An evaluation is already running."), type = "warning"); return(NULL) }
      progdir <- file.path(tempdir(), paste0("solprog_", format(Sys.time(), "%Y%m%d_%H%M%S")))
      dir.create(progdir, recursive = TRUE, showWarnings = FALSE)
      # Parallel evaluation (checkbox, on by default): reserve ~25% of cores (at least one) so the app and
      # the user's other work keep responsive, rather than saturating every core. solub_optim parallelises
      # the per-co-solvent scans AND the P/T grid (grid-level), so all the allotted cores stay busy.
      n_workers <- 1L
      if (is_eos && isTRUE(input$sol_parallel %||% TRUE)) {
        nc <- suppressWarnings(parallel::detectCores()); if (is.na(nc) || nc < 1L) nc <- 1L
        n_workers <- max(1L, nc - max(1L, as.integer(ceiling(nc / 4))))   # keep ~1/4 of cores free (min 1)
      }
      sol_args <- list(solute = if (is_eos) rv$loaded_name %||% smi else smi,
        cosolvents = cosolv_smiles, blends = blends_arg, blend_top_n = blend_top_n,
        temp_C = temps_seq, pres_bar = pres_seq, x = xfrac, Tm = tm_use, silent = TRUE, engine = eng,
        workers = n_workers, prog_dir = progdir)
      # dev (load_all): make the worker load the source, not the stale install; prod: NULL -> installed namespace.
      dev_path <- tryCatch(if (requireNamespace("pkgload", quietly = TRUE) && isTRUE(pkgload::is_dev_package("supeRcrit")))
        getNamespaceInfo("supeRcrit", "path") else NULL, error = function(e) NULL)
      prog <- shiny::Progress$new(session, min = 0, max = 1)
      prog$set(message = i18n$t("Predicting co-solvent solubility..."), value = 0.02,
               detail = i18n$t("starting the background solver…"))
      proc <- tryCatch(r_bg_quiet(function(sol_args, dev_path) {
          if (!is.null(dev_path) && requireNamespace("pkgload", quietly = TRUE))
            pkgload::load_all(dev_path, quiet = TRUE, attach_testthat = FALSE, export_all = FALSE)
          do.call(supeRcrit::solub_optim, sol_args) },
        args = list(sol_args = sol_args, dev_path = dev_path), supervise = TRUE),
        error = function(e) { try(prog$close(), silent = TRUE)
          showNotification(paste(i18n$t("Error during solubility prediction:"), conditionMessage(e)), type = "error"); NULL })
      if (is.null(proc)) { unlink(progdir, recursive = TRUE); return(NULL) }
      sol_async$proc <- proc; sol_async$prog <- prog; sol_async$progdir <- progdir
      sol_async$ctx <- list(finalize = finalize, ncos = length(cosolv_smiles))
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

    # Input validation for GCM methods from loaded calculation (a DB-experimental compound supplies its
    # own measured Tc + HSP, so it is exempt from needing a GCM method).
    if (!isTRUE(rv$loaded_db_exp) && rv$loaded_tb == "none" && rv$loaded_crit == "none" && rv$loaded_hsp == "none") {
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

    # Check for modifier selection and Added Blends. Skipped when the Automatic Solvent Finder is on —
    # it screens ALL candidate solvents automatically, so no manual selection is required.
    auto_finder <- isTRUE(input$auto_finder)
    if (!auto_finder && length(input$modifier_selection) == 0 && length(rv$saved_blends) == 0) {
      showNotification(i18n$t("Please select at least one co-solvent or define a blend for miscibility optimization."), type = "error")
      return(NULL)
    }

    withProgress(message = i18n$t("Optimizing Miscibility..."), {
      # Build modif and modfracs parameters
      modif_list <- list()
      modfracs_list <- list()

      if (auto_finder) {
        # Automatic Solvent Finder: screen ALL candidate single solvents (no blends, no manual list).
        for (mod in modifier_values) modif_list[[length(modif_list) + 1]] <- mod
      } else {
        # 1. Add individual modifiers
        if (!is.null(input$modifier_selection) && length(input$modifier_selection) > 0) {
          for (mod in input$modifier_selection) {
            modif_list[[length(modif_list) + 1]] <- mod
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

      # Sanitize the loaded GCM method strings: a non-GCM value ("manual" / "none" / "experimental") must
      # never reach est_gcm, which fails with a cryptic "get1index". A manual/experimental crit or HSP is
      # applied through the override path below (crit_value / hsp_values); the method passed here is then
      # just a valid placeholder. tb: manual -> its numeric value; any other non-GCM value -> NA (excluded).
      .is_gcm <- function(m) !is.null(m) && length(m) == 1 && !is.na(m) && nzchar(m) && !(m %in% c("manual", "none"))
      tb_param <- if (identical(rv$loaded_tb, "manual")) { req(rv$loaded_tb_manual_input); as.numeric(rv$loaded_tb_manual_input) }
                  else if (.is_gcm(rv$loaded_tb)) rv$loaded_tb else NA
      crit_method <- if (.is_gcm(rv$loaded_crit)) rv$loaded_crit else "JR"
      hsp_method  <- if (.is_gcm(rv$loaded_hsp))  rv$loaded_hsp  else "SP12"
      # Warn if a manual/experimental crit or HSP could not be recovered (older calc saved before the values
      # were persisted): the run falls back to the group-contribution estimate for that property.
      .have_ov <- function(v, n) isTRUE(rv$loaded_db_exp) && length(suppressWarnings(as.numeric(v))) >= n &&
                                 all(is.finite(suppressWarnings(as.numeric(v))[seq_len(n)]))
      if ((!.is_gcm(rv$loaded_crit) && !.have_ov(rv$loaded_exp_crit, 1)) ||
          (!.is_gcm(rv$loaded_hsp)  && !.have_ov(rv$loaded_exp_hsp, 3)))
        showNotification(i18n$t("This calculation used manual/experimental critical-T or HSP values that were not saved with it; the optimization fell back to the group-contribution estimate. Re-characterize the solute and save it again to use the exact values."), type = "warning", duration = 8)

      # Then proceed with the tryCatch (unchanged)
      sfe_mod_output <- tryCatch(
        {
          sfe_mod(
            solute = solute_input_vector, # Use the directly provided input vector
            tb = tb_param,
            crit = crit_method,
            hsp = hsp_method,
            gorder = rv$loaded_gorder,
            hlight = TRUE,
            simplicity = rv$loaded_simplicity,
            modif = modif_list,
            modfracs = modfracs_param,
            pres = pres_seq, # Use the generated sequence
            temps = temps_seq, # Use the generated sequence
            vfrac = vfrac_val, # Use the converted fraction
            silent = TRUE,
            # DB-experimental compounds drive HSP mode from their measured Tc + HSP (skip GCM).
            crit_value = if (isTRUE(rv$loaded_db_exp)) suppressWarnings(as.numeric(rv$loaded_exp_crit)[1]) else NULL,
            hsp_values = if (isTRUE(rv$loaded_db_exp)) suppressWarnings(as.numeric(rv$loaded_exp_hsp)) else NULL
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
        rv$sol_scan <- NULL   # clear any Solubility-mode results when an HSP run succeeds

        # Automatic Solvent Finder: recommend enhancing blends (scCO2 primary) from the solute's HSP.
        # Populates the "Recommended Enhancing Blends" tab; NULL when the finder was not requested.
        if (auto_finder && isTRUE(input$screen_blends)) {
          .p <- sfe_mod_output$parameters
          .tc <- .crit_get(if (is.null(.p)) NULL else .p$Critical, "Tc")
          rv$finder_result <- if (!is.null(.p) && !is.null(.p$HSP) &&
                                  isTRUE(is.finite(suppressWarnings(as.numeric(.tc)[1]))))
            # Wider blend coverage (HSP blending is cheap Hansen-distance math): pairs from the top-8
            # singles (C(8,2)=28 pairs) over 5 volume ratios = up to 140 candidate blends, vs the default
            # top-5 x 3 ratios (~30). Still instant; captures far more of the binary-blend space.
            tryCatch(hsp_solvent_finder(hsp_vals = .p$HSP, primary = "CO2",
                       cp = as.numeric(.tc)[1], top_n = 8, ratios = c(20, 35, 50, 65, 80),
                       silent = TRUE), error = function(e) NULL)
          else NULL
        } else rv$finder_result <- NULL

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
          display_text <- translate_modifier(best_mod, co2_prefix = FALSE)
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
          rng <- paste0(round(min_me, 2), " – ", round(max_me, 2), " %")
          fs <- if (nchar(rng) > 18) "15px" else if (nchar(rng) > 13) "19px" else "24px"
          valueBox(
            tags$span(rng, style = paste0("font-size:", fs)),
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
                df[[col_i]] <- sapply(df[[col_i]], translate_modifier, co2_prefix = FALSE)
              }

              translated_methods <- sapply(methods, translate_modifier, co2_prefix = FALSE)
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

        # Combined Summary table (shown under the value boxes): per co-solvent Min / Max / Average
        # miscibility enhancement across the P/T grid, plus Occurrence = % of grid points where it was
        # the BEST co-solvent ("-" when it was never the best). Replaces the old separate Occurrence and
        # Co-Solvent Comparison tables.
        mr <- rv$miscibility_results
        solv_list <- unique(mr$Modifier)
        # Occurrence: % of (P,T) grid points where each co-solvent gives the highest enhancement (>0;
        # otherwise pure CO2 wins and no co-solvent is credited). Computed directly from the results to
        # avoid the abbreviation/full-name mismatch in sfe$Modifier_Ranking (which caused all "-").
        gk <- paste(mr$Pressure_bar, mr$Temperature_C, sep = "@")
        best_grid <- tapply(seq_len(nrow(mr)), gk, function(ix) {
          j <- ix[which.max(mr$ME_percent[ix])]
          if (isTRUE(mr$ME_percent[j] > 0)) as.character(mr$Modifier[j]) else "CO2" })
        ng <- length(best_grid); occ_cnt <- table(unlist(best_grid))
        occ_map <- setNames(rep(0, length(solv_list)), solv_list)
        for (nm in intersect(names(occ_cnt), solv_list)) occ_map[nm] <- 100 * occ_cnt[[nm]] / ng

        reslist <- lapply(solv_list, function(i) {
          v <- mr$ME_percent[mr$Modifier == i]; gi <- gsk_info(i)
          data.frame(Co_solvent = i,
            Min = round(min(v, na.rm = TRUE), 2), Max = round(max(v, na.rm = TRUE), 2),
            Average = round(mean(v, na.rm = TRUE), 2),
            Occurrence = if (occ_map[i] > 0) sprintf("%.2f", occ_map[i]) else "-",
            Sustainability = gsk_band_html(if (is.null(gi)) NA_integer_ else as.integer(gi$gsk)),
            Notes = gsk_notes(gi), stringsAsFactors = FALSE)
        })
        rv$miscibility_comparison_data <- do.call(rbind, reslist)
        # Best-first by average enhancement.
        rv$miscibility_comparison_data <- rv$miscibility_comparison_data[
          order(-rv$miscibility_comparison_data$Average), , drop = FALSE]

        output$modifier_ranking_table <- DT::renderDataTable({
          req(rv$miscibility_comparison_data)
          display_data <- rv$miscibility_comparison_data
          display_data$Co_solvent <- sapply(display_data$Co_solvent, translate_modifier)
          DT::datatable(
            display_data, escape = FALSE, rownames = FALSE, extensions = "Buttons",
            colnames = c(i18n_r()$t("Co-Solvent"), i18n_r()$t("Min"), i18n_r()$t("Max"),
                         i18n_r()$t("Average"), i18n_r()$t("Occurrence (%)"),
                         i18n_r()$t("Sustainability"), i18n_r()$t("Notes")),
            options = list(
              dom = "Bt", scrollX = TRUE, paging = FALSE, language = tablang(),
              columnDefs = list(list(className = "dt-center", targets = "_all")),
              buttons = create_dt_export_buttons(i18n_r, "supercrit_cosolvent_summary")
            )
          ) %>%
            # No / negligible miscibility boost (<= 1% enhancement) flagged red bg + white text.
            DT::formatStyle(c("Min", "Max", "Average"),
              backgroundColor = DT::styleInterval(1, c("#dc3545", "")),
              color = DT::styleInterval(1, c("white", "")))
        })

        # Update co-solvent selection for Miscibility Enhancement tab
        rv$me_solvent_list <- solv_list
        updateSelectInput(session, "miscibility_enhancement_solvent_selection",
          choices = setNames(solv_list, sapply(solv_list, translate_modifier)),
          selected = solv_list[1]
        )
      }
    })
  })

  # (Co-Solvent Comparison table removed — merged into the combined Summary table above, and its tab
  #  is now "Recommended Enhancing Blends".)

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

  # Render the per-condition enhancement grid for the selected co-solvent. In Solubility (ML) mode this
  # is the scCO2 solubility enhancement (x) grid; in HSP mode it is the miscibility enhancement (%) grid.
  output$miscibility_enhancement_table <- DT::renderDataTable({
    req(input$miscibility_enhancement_solvent_selection)
    bar_label <- i18n_r()$t("bar")
    sel <- input$miscibility_enhancement_solvent_selection

    if (!is.null(rv$sol_scan)) {
      m <- rv$sol_scan$scan$enhancement[[sel]]; req(!is.null(m))
      pressures <- as.numeric(rownames(m)); temperatures <- as.numeric(colnames(m))
      df <- as.data.frame(round(m, 2))
      colnames(df) <- paste0(temperatures, " °C")
      rownames(df) <- paste0("<b>", pressures, " ", bar_label, "</b>")
      return(DT::datatable(df, extensions = "Buttons", rownames = TRUE, escape = FALSE,
        options = list(scrollX = TRUE, paging = FALSE, language = tablang(), dom = "Bfrtip",
          buttons = create_dt_export_buttons(i18n_r, "supercrit_solubility_enhancement"))) %>%
        # No / negligible boost (enhancement <= 1.05x) flagged red bg + white text.
        DT::formatStyle(colnames(df), backgroundColor = DT::styleInterval(1.05, c("#dc3545", "")),
          color = DT::styleInterval(1.05, c("white", ""))))
    }

    req(rv$miscibility_results)
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
  # ---- Export the Co-Solvent Selection results as a branded PDF report (report_pdf.R / cosolv_export.R).
  # Assembles a plain, localised `export` list here (needs the module's GSK + name-translation helpers) and
  # hands it to the pure renderer. Covers Solubility (EoS/COSMO) mode fully + a compact HSP-miscibility PDF.
  output$export_pdf_report <- downloadHandler(
    filename = function() {
      nm <- rv$loaded_name %||% (if (!is.null(rv$sfe_mod_output)) unname(rv$sfe_mod_output$solute_ids["Name"]) else NULL) %||% "cosolvent"
      paste0(generate_filename_with_timestamp(paste0("CoSolvent_", gsub("[^A-Za-z0-9_.-]", "_", nm))), ".pdf")
    },
    content = function(file) {
      tr <- tryCatch(i18n_r(), error = function(e) i18n)
      tt <- function(x) tryCatch(tr$t(x), error = function(e) x)
      logo   <- if (file.exists("www/altraflora-15-min.png")) "www/altraflora-15-min.png" else NULL
      appver <- tryCatch(paste0("supeRcrit v", as.character(utils::packageVersion("supeRcrit"))), error = function(e) "supeRcrit")
      plain_band <- function(gi) if (is.null(gi) || is.na(gi$gsk)) tt("not rated")
                                 else tryCatch(supeRcrit::lle_solvent_band(as.integer(gi$gsk))$label, error = function(e) tt("not rated"))
      fg <- function(v, sig = 3) if (!isTRUE(is.finite(v))) "–" else formatC(signif(v, sig), format = "fg")
      tryCatch({
        if (!is.null(rv$sol_scan)) {
          # ---------- Solubility (EoS / COSMO) mode ----------
          sr <- sol_ranked(); df <- sr$df; scan <- rv$sol_scan$scan
          u  <- if (isTRUE(sr$gkg)) tt("g/kg") else tt("mol/mol")
          gis <- lapply(df$Modifier, gsk_info)
          ranking <- data.frame(
            CoSolvent = vapply(df$Modifier, function(m) translate_modifier(m, co2_prefix = FALSE), character(1)),
            Gain = vapply(df$Gain, fg, character(1)),
            Enh  = vapply(df$Enh, function(v) if (!isTRUE(is.finite(v))) "–" else formatC(round(v, 2), format = "f", digits = 2), character(1)),
            Sol  = vapply(df$Sol, fg, character(1)),
            Sustainability = vapply(gis, plain_band, character(1)),
            Notes = vapply(gis, gsk_notes, character(1)), stringsAsFactors = FALSE, check.names = FALSE)
          colnames(ranking) <- c(tt("Co-Solvent"), paste0(tt("Median gain"), " (Δ ", u, ")"),
            tt("Median Enhancement (×)"), paste0(tt("Median solubility"), " (", u, ")"), tt("Sustainability"), tt("Notes"))
          v <- df[[sr$key]]; v <- v[is.finite(v)]
          gain_range <- if (!length(v)) "—" else if (sr$metric == "enh")
            paste0(round(min(v), 1), " – ", round(max(v), 1), "×")
            else paste0(formatC(min(v), format = if (sr$gkg) "fg" else "e", digits = if (sr$gkg) 3 else 2),
                        " – ", formatC(max(v), format = if (sr$gkg) "fg" else "e", digits = if (sr$gkg) 3 else 2))
          blends <- NULL; br <- rv$sol_scan$blend_ranking
          if (!is.null(br) && nrow(br)) {
            sol_disp <- if (isTRUE(sr$gkg)) solub_gkg(br$MeanSol, sr$mw) else br$MeanSol
            blends <- data.frame(
              Blend = vapply(br$Modifier, function(m) translate_modifier(m, co2_prefix = FALSE), character(1)),
              MeanEnh = formatC(round(br$MeanEnh, 2), format = "f", digits = 2),
              MaxEnh  = formatC(round(br$MaxEnh, 2), format = "f", digits = 2),
              MeanSol = vapply(sol_disp, fg, character(1)), stringsAsFactors = FALSE, check.names = FALSE)
            colnames(blends) <- c(tt("Blend"), tt("Mean Enhancement (×)"), tt("Max Enhancement (×)"), paste0(tt("Mean Solubility"), " (", u, ")"))
          }
          eng <- input$sol_engine %||% "ocrsphi"
          eng_disp <- switch(eng, makarov = "Makarov ML (TIML)", csp = "CSP-2021 (COSMO-SAC-Phi)", ocrsphi = "openCOSMO-RS-Phi", eng)
          pv <- suppressWarnings(as.numeric(rownames(scan$pure))); tvv <- suppressWarnings(as.numeric(colnames(scan$pure)))
          rng2 <- function(x) { x <- x[is.finite(x)]; if (!length(x)) c(NA, NA) else range(x) }
          pr <- rng2(pv); tr2 <- rng2(tvv)
          # Sustainability text colours (mirror the app's lle_solvent_band palette); Sustainability is column 5.
          sust_col <- vapply(gis, function(gi) if (is.null(gi) || is.na(gi$gsk)) "#999999"
                             else tryCatch(supeRcrit::lle_solvent_band(as.integer(gi$gsk))$colour, error = function(e) "#999999"), character(1))
          rk_text_col <- matrix("#212529", nrow = nrow(ranking), ncol = ncol(ranking)); rk_text_col[, 5] <- sust_col
          rk_col_frac <- c(0.17, 0.12, 0.12, 0.13, 0.15, 0.31)   # widen Sustainability so "Recommended" fits
          # ---- Best-co-solvent-by-condition grid heatmap (colour = best co-solvent; cell text = enhancement ×) ----
          grid_plot <- tryCatch({
            bm <- scan$best
            if (nrow(bm) > 10) bm <- bm[round(seq(1, nrow(bm), length.out = 10)), , drop = FALSE]
            if (ncol(bm) > 10) bm <- bm[, round(seq(1, ncol(bm), length.out = 10)), drop = FALSE]
            # Colour the tiles by the RAW modifier name (so the fill matches the raw-keyed colour map), and
            # relabel the legend with the translated solvent names. (Keying fill by the translated name while
            # the map is keyed raw was the bug that made every tile fall to the grey na.value.)
            cmap <- modifier_color_map()
            clook <- if (!is.null(cmap)) stats::setNames(cmap$bg_colors, cmap$methods) else character(0)
            clab  <- if (!is.null(cmap)) stats::setNames(vapply(cmap$methods, function(.m) translate_modifier(.m, co2_prefix = FALSE), character(1)), cmap$methods) else character(0)
            prs <- as.numeric(rownames(bm)); tps <- as.numeric(colnames(bm)); pk <- sr$peak; gl <- list()
            for (ri in seq_len(nrow(bm))) for (ci in seq_len(ncol(bm))) {
              b <- bm[ri, ci]; rn <- rownames(bm)[ri]; cn <- colnames(bm)[ci]
              nmv <- if (is.null(b) || is.na(b) || b == "CO2") NA_character_ else as.character(b)   # raw name; CO2/NA -> grey
              val <- if (!is.na(b) && b != "CO2") { p2 <- sol_point_value(scan, b, rn, cn, sr$metric, sr$gkg, sr$mw); if (is.na(p2)) "" else p2 } else ""
              is_pk <- !is.null(pk) && !is.na(b) && identical(as.character(b), pk$nm) && identical(rn, pk$r) && identical(cn, pk$c)
              gl[[length(gl) + 1]] <- data.frame(P = prs[ri], Temp = tps[ci], best = nmv, val = val, peak = is_pk, stringsAsFactors = FALSE)
            }
            gdf <- do.call(rbind, gl)
            gdf$P <- factor(gdf$P, levels = sort(unique(gdf$P))); gdf$Temp <- factor(gdf$Temp, levels = sort(unique(gdf$Temp)))
            ggplot2::ggplot(gdf, ggplot2::aes(x = Temp, y = P)) +
              ggplot2::geom_tile(ggplot2::aes(fill = best), colour = "white", linewidth = 0.4) +
              ggplot2::geom_tile(data = gdf[gdf$peak, , drop = FALSE], fill = NA, colour = "#111", linewidth = 1.0) +
              ggplot2::geom_text(ggplot2::aes(label = val), size = 2.8, colour = "#222") +
              ggplot2::scale_fill_manual(values = clook, labels = clab, na.value = "#eeeeee", name = tt("Best co-solvent"), na.translate = FALSE) +
              ggplot2::labs(x = tt("Temperature (°C)"), y = tt("Pressure (bar)")) +
              ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, title.position = "left")) +
              ggplot2::theme_minimal(base_size = 9) +
              ggplot2::theme(panel.grid = ggplot2::element_blank(),
                legend.position = "top", legend.direction = "horizontal", legend.box.just = "left",
                legend.margin = ggplot2::margin(0, 0, 1, 0), legend.key.size = grid::unit(11, "pt"),
                legend.text = ggplot2::element_text(size = 8.5), legend.title = ggplot2::element_text(size = 9),
                axis.text = ggplot2::element_text(size = 8.5), axis.title = ggplot2::element_text(size = 9))
          }, error = function(e) NULL)
          export <- list(
            identity = list(name = rv$loaded_name %||% "solute", cas = rv$loaded_cas %||% NA, smiles = rv$loaded_smiles %||% NA,
                            mw = suppressWarnings(as.numeric(scan$mw)), Tm = suppressWarnings(as.numeric(scan$Tm))),
            conditions = list(engine = eng_disp, p_lo = pr[1], p_hi = pr[2], t_lo = tr2[1], t_hi = tr2[2],
                              x_pct = suppressWarnings(as.numeric(input$sol_molfrac))),
            summary = list(best = translate_modifier(df$Modifier[1], co2_prefix = FALSE),
                           gain_label = sol_metric_label(sr$metric, sr$unit, sr$mw), gain_range = gain_range,
                           n_tested = nrow(rv$sol_scan$ranking)),
            unit_label = u, ranking = ranking, ranking_col_frac = rk_col_frac, ranking_text_col = rk_text_col,
            grid_plot = grid_plot, blends = blends,
            scale_note = tt("Absolute-solubility scale in pure CO2 — extremely insoluble ~1e-7 mol/mol (~1e-4 g/kg, e.g. disperse dyes); typical ~1e-4 (~0.7 g/kg); highly soluble ~0.1 (~300 g/kg, e.g. naphthalene)."))
          build_cosolv_pdf_report(export, tr, file, company = appver, logo = logo)
        } else if (!is.null(rv$miscibility_comparison_data)) {
          # ---------- HSP miscibility mode ----------
          cd <- rv$miscibility_comparison_data; gis <- lapply(cd$Co_solvent, gsk_info)
          ranking <- data.frame(
            CoSolvent = vapply(cd$Co_solvent, function(m) translate_modifier(m, co2_prefix = FALSE), character(1)),
            Min = cd$Min, Max = cd$Max, Average = cd$Average, Occurrence = cd$Occurrence,
            Sustainability = vapply(gis, plain_band, character(1)), Notes = cd$Notes, stringsAsFactors = FALSE, check.names = FALSE)
          colnames(ranking) <- c(tt("Co-Solvent"), tt("Min"), tt("Max"), tt("Average"), tt("Occurrence (%)"), tt("Sustainability"), tt("Notes"))
          # Sustainability text colours (Sustainability is column 6 in HSP mode).
          sust_col <- vapply(gis, function(gi) if (is.null(gi) || is.na(gi$gsk)) "#999999"
                             else tryCatch(supeRcrit::lle_solvent_band(as.integer(gi$gsk))$colour, error = function(e) "#999999"), character(1))
          rk_text_col <- matrix("#212529", nrow = nrow(ranking), ncol = ncol(ranking)); rk_text_col[, 6] <- sust_col
          rk_col_frac <- c(0.17, 0.09, 0.09, 0.10, 0.11, 0.14, 0.30)   # widen Sustainability
          me <- rv$miscibility_results$ME_percent; ids <- if (!is.null(rv$sfe_mod_output)) rv$sfe_mod_output$solute_ids else NULL
          # ---- Best-modifier-by-condition grid heatmap (colour = best co-solvent; cell text = ME %) ----
          grid_plot <- tryCatch({
            bm <- rv$sfe_mod_output$sfe$Best_Modifier
            mel <- rv$sfe_mod_output$sfe$Miscib_Enhancement
            if (is.null(bm) || !length(bm)) stop("no grid")
            if (nrow(bm) > 10) bm <- bm[round(seq(1, nrow(bm), length.out = 10)), , drop = FALSE]
            if (ncol(bm) > 10) bm <- bm[, round(seq(1, ncol(bm), length.out = 10)), drop = FALSE]
            # Tiles coloured by the RAW modifier name (matches the raw-keyed colour map); legend relabelled
            # with the translated solvent names. (Keying fill by the translated name = all-grey bug.)
            cmap <- modifier_color_map()
            clook <- if (!is.null(cmap)) stats::setNames(cmap$bg_colors, cmap$methods) else character(0)
            clab  <- if (!is.null(cmap)) stats::setNames(vapply(cmap$methods, function(.m) translate_modifier(.m, co2_prefix = FALSE), character(1)), cmap$methods) else character(0)
            prs <- as.numeric(rownames(bm)); tps <- as.numeric(colnames(bm)); gl <- list()
            me_at <- function(b, rn, cn) {   # ME% for the winning modifier at this P/T cell
              key <- paste0("CO2 with ", b); v <- NA_real_
              md <- mel[[key]]
              if (!is.null(md) && rn %in% rownames(md) && cn %in% colnames(md)) v <- md[rn, cn]
              if (is.na(v)) for (nm in names(mel)) { md <- mel[[nm]]
                if (rn %in% rownames(md) && cn %in% colnames(md)) { x <- md[rn, cn]
                  if (!is.na(x) && (is.na(v) || x > v)) v <- x } }
              if (is.na(v)) "" else round(v, 1)
            }
            for (ri in seq_len(nrow(bm))) for (ci in seq_len(ncol(bm))) {
              b <- bm[ri, ci]; rn <- rownames(bm)[ri]; cn <- colnames(bm)[ci]
              nmv <- if (is.null(b) || is.na(b) || b == "CO2") NA_character_ else as.character(b)   # raw name; CO2/NA -> grey
              val <- if (!is.null(b) && !is.na(b) && b != "CO2") me_at(b, rn, cn) else ""
              gl[[length(gl) + 1]] <- data.frame(P = prs[ri], Temp = tps[ci], best = nmv, val = val, stringsAsFactors = FALSE)
            }
            gdf <- do.call(rbind, gl)
            gdf$P <- factor(gdf$P, levels = sort(unique(gdf$P))); gdf$Temp <- factor(gdf$Temp, levels = sort(unique(gdf$Temp)))
            ggplot2::ggplot(gdf, ggplot2::aes(x = Temp, y = P)) +
              ggplot2::geom_tile(ggplot2::aes(fill = best), colour = "white", linewidth = 0.4) +
              ggplot2::geom_text(ggplot2::aes(label = val), size = 2.8, colour = "#222") +
              ggplot2::scale_fill_manual(values = clook, labels = clab, na.value = "#eeeeee", name = tt("Best co-solvent"), na.translate = FALSE) +
              ggplot2::labs(x = tt("Temperature (°C)"), y = tt("Pressure (bar)")) +
              ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, title.position = "left")) +
              ggplot2::theme_minimal(base_size = 9) +
              ggplot2::theme(panel.grid = ggplot2::element_blank(),
                legend.position = "top", legend.direction = "horizontal", legend.box.just = "left",
                legend.margin = ggplot2::margin(0, 0, 1, 0), legend.key.size = grid::unit(11, "pt"),
                legend.text = ggplot2::element_text(size = 8.5), legend.title = ggplot2::element_text(size = 9),
                axis.text = ggplot2::element_text(size = 8.5), axis.title = ggplot2::element_text(size = 9))
          }, error = function(e) NULL)
          export <- list(
            identity = list(name = if (!is.null(ids)) unname(ids["Name"]) else (rv$loaded_name %||% "solute"),
                            cas = if (!is.null(ids)) unname(ids["CAS"]) else NA, smiles = rv$loaded_smiles %||% NA, mw = NA_real_, Tm = NA_real_),
            summary = list(best = ranking[[1]][1], gain_label = paste0(tt("ME Range"), " (%)"),
                           gain_range = paste0(round(min(me, na.rm = TRUE), 1), " – ", round(max(me, na.rm = TRUE), 1), " %"),
                           n_tested = length(unique(rv$miscibility_results$Modifier))),
            unit_label = "%", ranking = ranking, ranking_col_frac = rk_col_frac, ranking_text_col = rk_text_col,
            grid_plot = grid_plot, blends = NULL, scale_note = NULL)
          build_cosolv_pdf_report(export, tr, file, company = appver, logo = logo)
        } else stop(tt("Run a selection first."))
      }, error = function(e) {
        showNotification(sprintf(tt("PDF export failed: %s"), conditionMessage(e)), type = "error", duration = 8)
        grDevices::cairo_pdf(file); grid::grid.text(tt("Report generation failed.")); grDevices::dev.off()
      })
    }
  )

  output$export_results <- downloadHandler(
    filename = function() {
      # Solubility / COSMO mode: a SINGLE well-formatted file (via solub_export), like HSP's hsp_export.
      if (!is.null(rv$sol_scan)) {
        nm <- rv$loaded_name %||% "solute"
        return(paste0(generate_filename_with_timestamp(paste0("Solubility_", nm)), ".csv"))
      }
      req(rv$sfe_mod_output)
      solute_name <- rv$sfe_mod_output$solute_ids["Name"]
      solute_cas <- rv$sfe_mod_output$solute_ids["CAS"]
      base_name <- paste0("HSP_", solute_name, "_", solute_cas)
      paste0(generate_filename_with_timestamp(base_name), ".zip")
    },
    content = function(file) {
      # Solubility / COSMO mode export: ONE well-formatted file via the dedicated solub_export() engine
      # function (the solubility-mode counterpart of hsp_export). Restores the single-file behaviour and
      # covers every engine (Makarov ML, CSP-2021, openCOSMO-RS-Phi).
      if (!is.null(rv$sol_scan)) {
        sc <- rv$sol_scan$scan
        # Human-readable co-solvent labels for every identifier that appears in the grids/rankings.
        keys <- unique(c(names(sc$enhancement), names(sc$solubility), as.vector(sc$best),
                         rv$sol_scan$ranking$Modifier, rv$sol_scan$blend_ranking$Modifier))
        keys <- keys[!is.na(keys) & nzchar(keys) & keys != "CO2"]
        # co2_prefix = FALSE -> bare solvent names ("Ethanol", not "CO2 + Ethanol"), matching the Best Modifier table.
        labels <- if (length(keys)) stats::setNames(vapply(keys, function(k) translate_modifier(k, co2_prefix = FALSE), character(1)), keys) else NULL
        eng <- input$sol_engine %||% "ocrsphi"
        eng_disp <- switch(eng, makarov = "Makarov ML (TIML)", csp = "CSP-2021 (COSMO-SAC-Phi)",
                           ocrsphi = "openCOSMO-RS-Phi", eng)
        solute <- c(Name = rv$loaded_name %||% "solute", SMILES = rv$loaded_smiles %||% "",
                    CAS = rv$loaded_cas %||% "",
                    MW = if (!is.null(sc$mw) && isTRUE(is.finite(sc$mw))) as.character(round(sc$mw, 2)) else "",
                    Tm = if (!is.null(sc$Tm) && isTRUE(is.finite(sc$Tm))) as.character(round(sc$Tm, 2)) else "")
        tdir <- file.path(tempdir(), paste0("sol_export_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S")))
        dir.create(tdir, recursive = TRUE, showWarnings = FALSE)
        tabfile <- solub_export(rv$sol_scan, solute = solute, engine = eng_disp,
                                cosolvent_labels = labels, expath = tdir, silent = TRUE)
        # Convert the single tab-delimited file to a single CSV (UTF-8 BOM); quote fields containing commas.
        raw_bytes <- readBin(tabfile, "raw", file.info(tabfile)$size)
        content <- rawToChar(raw_bytes); Encoding(content) <- "UTF-8"

        # ---- Localise the (English) section headers, labels and cell values written by solub_export ----
        # Ordered longest/most-specific first so substrings (e.g. "Co-solvent") never clobber a longer phrase.
        sol_tr_keys <- c(
          "SOLUBILITY ENHANCEMENT WITH EACH CO-SOLVENT (× relative to pure CO2; pressure bar × temperature °C)",
          "SOLUBILITY WITH EACH CO-SOLVENT (mole fraction; pressure bar × temperature °C)",
          "BEST CO-SOLVENT AT EACH CONDITION (highest enhancement; pressure bar × temperature °C)",
          "PURE scCO2 SOLUBILITY BASELINE (mole fraction; pressure bar × temperature °C)",
          "APPLICABILITY DOMAIN (whether each condition is within the model's reliable range)",
          "SOLVENT BLEND RANKING (higher mean enhancement = better)",
          "CO-SOLVENT RANKING (higher mean enhancement = better)",
          "Grid points outside applicability domain",
          "Mean solubility (mole fraction)",
          "Co-solvent mole fraction",
          "Co-solvents screened",
          "Mean enhancement (x)",
          "Max enhancement (x)",
          "Temperature grid (°C)",
          "Pressure grid (bar)",
          "SCREENING CONDITIONS",
          "Prediction engine",
          "None (pure CO2)",
          "Pressure (bar)",
          "Solvent blend",
          "Co-solvent",
          "SOLUTE")
        for (s in sol_tr_keys) {
          tr <- i18n$t(s)
          if (!identical(tr, s)) content <- gsub(s, tr, content, fixed = TRUE)
        }
        # Applicability-domain cells: compact "In" / "Out" (kept short and language-neutral by request).
        content <- gsub("Out-of-domain", "Out", content, fixed = TRUE)
        content <- gsub("In-domain", "In", content, fixed = TRUE)

        lines <- strsplit(content, "\n", fixed = TRUE)[[1]]
        csv_lines <- vapply(lines, function(line) {
          if (!grepl("\t", line, fixed = TRUE)) return(line)
          fields <- strsplit(line, "\t", fixed = TRUE)[[1]]
          fields <- vapply(fields, function(f) if (grepl(",", f, fixed = TRUE)) paste0('"', f, '"') else f, character(1))
          paste(fields, collapse = ",")
        }, character(1), USE.NAMES = FALSE)
        content <- paste(csv_lines, collapse = "\n")
        con <- file(file, "wb")
        writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con)
        writeBin(charToRaw(enc2utf8(content)), con)
        close(con)
        unlink(tdir, recursive = TRUE)
        showNotification(i18n$t("Export completed successfully!"), type = "message")
        return(invisible())
      }
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
            "SP_ENK" = i18n$t("Enekvist 2021"),
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
            "SELECTION OF THE BEST scCO₂ SFE MODIFIER AMONGST: " = paste0(i18n$t("SELECTION OF THE BEST scCO₂ SFE MODIFIER AMONGST"), ": "),
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

        # Incorporate the Automatic Solvent Finder recommendations (enhancing blends) into the export,
        # when it was run. hsp_export() only writes the miscibility optimization; the finder is a
        # separate source, so its results are added here as an extra CSV in the same zip.
        if (!is.null(rv$finder_result)) {
          fr <- rv$finder_result
          bl <- fr$blends_ranked
          if (is.null(bl) || !nrow(bl)) bl <- data.frame(Note = "No miscible enhancing blends found.")
          fpath <- file.path(latest_folder, "recommended_enhancing_blends.csv")
          hdr <- c(paste0("Best single solvent,", fr$best_single %||% "-"),
                   paste0("Best enhancing blend,",
                          if (is.null(fr$best_blend)) "-"
                          else paste0(fr$best_blend$comp1, ":", fr$best_blend$comp2, " (", fr$best_blend$ratio, ")")),
                   "")
          writeLines(hdr, fpath, useBytes = TRUE)
          suppressWarnings(utils::write.table(bl, fpath, sep = ",", row.names = FALSE,
            col.names = TRUE, append = TRUE, fileEncoding = "UTF-8"))
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
    rv$finder_result <- NULL   # clear Automatic Solvent Finder recommendations

    # Clear loaded calculation parameters
    rv$loaded_calculation <- NULL
    rv$loaded_cas <- NULL
    rv$loaded_name <- NULL
    rv$loaded_smiles <- NULL
    rv$loaded_specify_smiles <- NULL
    rv$loaded_tb <- NULL
    rv$loaded_tb_manual_input <- NULL
    rv$loaded_tm <- NULL
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
    updateNumericInput(session, "vfrac_input", value = defaults$vfrac_input)
    updateNumericInput(session, "pressure_min", value = defaults$pressure_min)
    updateNumericInput(session, "pressure_max", value = defaults$pressure_max)
    updateNumericInput(session, "pressure_step", value = defaults$pressure_step)
    updateNumericInput(session, "pressure_step_eos", value = defaults$pressure_step_eos)
    updateNumericInput(session, "temperature_min", value = defaults$temperature_min)
    updateNumericInput(session, "temperature_max", value = defaults$temperature_max)
    updateNumericInput(session, "temperature_step", value = defaults$temperature_step)
    updateNumericInput(session, "temperature_step_eos", value = defaults$temperature_step_eos)
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

  # ---- Automatic Solvent Finder outputs -------------------------------------------------------------
  # hsp_solvent_finder is invoked in the Run handler when the "Automatic Solvent Finder" checkbox is on
  # (scCO2 is always the primary solvent — the old "any solvent"/ambient mode was removed). These outputs
  # drive the "Recommended Enhancing Blends" tab.
  # Robust element access for parameters$Critical (a NAMED ATOMIC VECTOR, e.g. c(Tc=...)): `$` throws on
  # atomic vectors, `[[` works for both a named vector and a list.
  .crit_get <- function(crit, key) {
    if (is.null(crit)) return(NULL)
    if (is.list(crit)) return(crit[[key]])
    if (key %in% names(crit)) return(crit[[key]])
    NULL
  }

  output$finder_summary_ui <- renderUI({
    req(rv$finder_result); r <- rv$finder_result
    bb <- if (is.null(r$best_blend)) i18n_r()$t("no blend beats the best single solvent")
          else paste0(translate_modifier(r$best_blend$comp1, co2_prefix = FALSE), " : ", translate_modifier(r$best_blend$comp2, co2_prefix = FALSE),
                      " (", r$best_blend$ratio, ")")
    tagList(
      tags$p(tags$b(i18n_r()$t("Best single solvent: ")), translate_solvent_token(r$best_single, i18n_r())),
      tags$p(tags$b(i18n_r()$t("Best enhancing blend: ")), bb)
    )
  })

  # EoS (CSP / openCOSMO-RS-Phi) enhancing-blend results — genuine n-component 50:50 blends of the top
  # single co-solvents, shown in the dedicated blends tab (Solubility/EoS mode).
  output$eos_blends_table <- DT::renderDataTable({
    req(rv$sol_scan$blend_ranking)
    df <- rv$sol_scan$blend_ranking
    unit <- if (is.null(input$sol_unit)) "molfrac" else input$sol_unit
    mw <- rv$sol_scan$scan$mw; gkg <- identical(unit, "gkg") && isTRUE(is.finite(mw))
    sol_disp <- if (gkg) solub_gkg(df$MeanSol, mw) else df$MeanSol
    sol_hdr  <- if (gkg) i18n_r()$t("Mean Solubility (g/kg)") else i18n_r()$t("Mean Solubility (mol/mol)")
    disp <- data.frame(Blend = vapply(df$Modifier, function(m) paste0(translate_modifier(m, co2_prefix = FALSE),
                                        misc_badge(strsplit(m, " + ", fixed = TRUE)[[1]])), character(1)),
      MeanEnh = round(df$MeanEnh, 2), MaxEnh = round(df$MaxEnh, 2), MeanSol = signif(sol_disp, 3),
      check.names = FALSE, stringsAsFactors = FALSE)
    DT::datatable(disp, rownames = FALSE, escape = FALSE, extensions = "Buttons",
      colnames = c(i18n_r()$t("Blend"), i18n_r()$t("Mean Enhancement (×)"), i18n_r()$t("Max Enhancement (×)"), sol_hdr),
      # Clarify the blend proportion (issue): EoS/COSMO screens 50:50 MOLAR blends at the same total
      # co-solvent mole fraction as the single-co-solvent screen (solub_optim splits the total 50:50).
      caption = htmltools::tags$caption(style = "caption-side: bottom; font-size: 0.82em; color:#777;",
        i18n_r()$t("Each blend is an equal-molar (50:50) mixture of its two co-solvents, evaluated at the same total co-solvent mole fraction as the single-co-solvent screen.")),
      options = list(scrollX = TRUE, paging = FALSE, language = tablang(), dom = "Bfrtip",
                     columnDefs = list(list(className = "dt-right", targets = c(1, 2, 3))),
                     buttons = create_dt_export_buttons(i18n_r, "supercrit_cosolvent_blends"))) %>%
      DT::formatStyle(c("MeanEnh", "MaxEnh"),
        backgroundColor = DT::styleInterval(1.05, c("#dc3545", "")),
        color = DT::styleInterval(1.05, c("white", "")))
  })

  output$finder_blends_table <- DT::renderDataTable({
    req(rv$finder_result)
    bl <- rv$finder_result$blends_ranked
    if (is.null(bl) || !nrow(bl))
      return(DT::datatable(data.frame(Note = i18n_r()$t("No miscible enhancing blends found.")),
                           rownames = FALSE, options = list(dom = "t")))
    # Yes/No, colour-coded (green Yes / red No). beats_best_single is logical.
    bb <- bl$beats_best_single %in% TRUE
    beats <- ifelse(bb, "<span style='color:#2e7d32;font-weight:700;'>Yes</span>",
                        "<span style='color:#b00020;font-weight:600;'>No</span>")
    disp <- data.frame(
      c1 = vapply(bl$comp1, function(x) translate_modifier(x, co2_prefix = FALSE), character(1)),
      c2 = vapply(seq_len(nrow(bl)), function(i) paste0(translate_modifier(bl$comp2[i], co2_prefix = FALSE),
                    misc_badge(c(bl$comp1[i], bl$comp2[i]))), character(1)),
      comp = bl$ratio,
      mn  = bl[["miscibility_enhancement_min"]] %||% NA_real_,
      mx  = bl[["miscibility_enhancement_max"]] %||% NA_real_,
      avg = bl[["miscibility_enhancement_pct"]],
      beats = beats, stringsAsFactors = FALSE)
    ehdr <- paste0(i18n_r()$t("Miscibility Enhancement"), " (%)")
    # Two-row header: Min / Max / Average grouped under "Miscibility Enhancement (%)".
    container <- htmltools::withTags(table(class = "display",
      thead(
        tr(
          th(rowspan = 2, i18n_r()$t("Component 1")), th(rowspan = 2, i18n_r()$t("Component 2")),
          th(rowspan = 2, i18n_r()$t("Composition")),
          th(colspan = 3, style = "text-align:center;", ehdr),
          th(rowspan = 2, i18n_r()$t("Beats Best Single Solvent?"))),
        tr(th(i18n_r()$t("Min")), th(i18n_r()$t("Max")), th(i18n_r()$t("Average"))))))
    DT::datatable(disp, container = container, escape = FALSE, rownames = FALSE, extensions = "Buttons",
      options = list(dom = "Bfrtip", scrollX = TRUE, pageLength = 10, language = tablang(),
        columnDefs = list(list(className = "dt-center", targets = "_all")),
        buttons = create_dt_export_buttons(i18n_r, "supercrit_enhancing_blends")))
  })

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
