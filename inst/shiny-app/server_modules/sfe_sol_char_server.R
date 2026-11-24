solute_characterization_server <- function(input, output, session, defaults, i18n, tablang, sfe_rv) {



  ns <- session$ns
  i18n_r <- reactive({
    i18n
  })

  # Value cell for the parameter cards: a voided (NA) estimate means the GCM produced a non-physical value
  # (out of applicability domain), so show a clear warning marker instead of a bare "NA"; otherwise the
  # rounded value + unit. (Called inside reactive renders, so i18n_r() is valid here.)
  ood_cell <- function(val, unit) {
    if (length(val) != 1 || is.na(suppressWarnings(as.numeric(val))))
      tags$span(style = "color:#d9534f;font-weight:bold;",
                title = i18n_r()$t("Out of applicability domain – the method produced a non-physical value, so no reliable estimate is available"),
                HTML("&#9888; "), i18n_r()$t("Out of domain"))
    else list(round(as.numeric(val), 2), " ", tags$em(class = "text-muted", unit))
  }

  # Render a number; very small/large magnitudes become "m × 10^e" with a real HTML superscript
  # (so values like 1.12e-04 read as 1.12 × 10⁻⁴ instead of raw "e-04"). Returns an HTML object.
  fmt_sci <- function(x, sig = 3) {
    x <- suppressWarnings(as.numeric(x)[1])
    if (!isTRUE(is.finite(x))) return(HTML("&ndash;"))
    if (x != 0 && (abs(x) < 1e-3 || abs(x) >= 1e5)) {
      e <- floor(log10(abs(x))); m <- x / 10^e
      HTML(sprintf("%s &times; 10<sup>%d</sup>", formatC(m, digits = sig - 1, format = "f"), e))
    } else HTML(as.character(signif(x, sig)))
  }

  # Reactive output to check if results are available
  output$has_results <- reactive({
    !is.null(sfe_rv$gcm_results)
  })
  outputOptions(output, "has_results", suspendWhenHidden = FALSE)

  # Reactive output to check if GCM comparison results are available
  output$has_gcm_comparison <- reactive({
    !is.null(sfe_rv$gcm_comparison)
  })
  outputOptions(output, "has_gcm_comparison", suspendWhenHidden = FALSE)

  # Observer to enable/disable result tabs based on results availability
  # Tab 0: Solute Characterization Results - enabled when gcm_results available
  # Tab 1: Predicted Parameters (HSP & Critical) - enabled when gcm_results available
  # Tab 2: Group Contributions - enabled when gcm_results available
  # Tab 3: GCM Comparison - enabled ONLY when gcm_comparison available
  observe({
    has_results <- !is.null(sfe_rv$gcm_results)
    has_gcm_comparison <- !is.null(sfe_rv$gcm_comparison)
    
    if (has_results) {
      # Enable all tabs first
      session$sendCustomMessage("enableTabs", list(tabsetId = ns("sol_char_tabs")))
      # But disable tab 3 (GCM Comparison) if no comparison results
      if (!has_gcm_comparison) {
        session$sendCustomMessage("disableTabsByIndex", list(tabsetId = ns("sol_char_tabs"), indices = c(3)))
      }
    } else {
      session$sendCustomMessage("disableTabs", list(tabsetId = ns("sol_char_tabs"), keepFirst = TRUE))
    }
  })

  # Render Solute Identification inputs
  output$cas_input_ui <- renderUI({
    textInput(ns("cas_input"),
      tags$span(i18n_r()$t("CAS Number"),
        input_help(i18n_r()$t("Chemical Abstracts Service registry number of the target solute (e.g. 7235-40-7 for beta-carotene). Used to retrieve molecular structure and SMILES automatically."),
                   title = i18n_r()$t("CAS Number"), buttonLabel = i18n_r()$t("OK"))),
      value = isolate(input$cas_input) %||% defaults$cas_input)
  })

  # --- Field-activation toggles for Display Name + SMILES (same pattern as the kinetic module's
  # Advanced Parameters): a toggle beside the label; OFF shows a greyed "Auto" field, ON enables entry. ---
  name_enabled   <- reactiveVal(FALSE)   # OFF -> auto display name (from CAS / database)
  smiles_enabled <- reactiveVal(FALSE)   # OFF -> SMILES auto-retrieved from CAS (was the "Specify SMILES" checkbox)
  observeEvent(input$toggle_name,   name_enabled(!name_enabled()))
  observeEvent(input$toggle_smiles, smiles_enabled(!smiles_enabled()))
  # Effective display name: the custom value when the toggle is on, else an auto label (CAS, else "Solute").
  eff_name <- function() {
    nm <- trimws(input$name_input %||% "")
    if (isTRUE(name_enabled()) && nzchar(nm)) return(nm)
    # Auto mode: prefer the real chemical name mol_find resolved for the CURRENT CAS
    # (this is how CAS 58-08-2 displays as "Caffeine"); fall back to the CAS, then a label.
    cas <- trimws(input$cas_input %||% "")
    rid <- suppressWarnings(sfe_rv$solute_data$IDs)
    if (!is.null(rid) && identical(unname(rid["CAS"]), cas)) {
      rn <- unname(rid["Name"])
      if (length(rn) == 1 && !is.na(rn) && nzchar(rn) && !grepl("not found|SMILES input", rn)) return(rn)
    }
    if (nzchar(cas)) cas else i18n_r()$t("Solute")
  }
  # A field with an on/off activation toggle (label + toggle button; ON -> input, OFF -> greyed "Auto").
  toggle_field <- function(label_ui, toggle_id, is_on, input_when_on, auto_text) {
    div(class = "form-group shiny-input-container", style = "width: 100%;",
      div(style = "display: flex; justify-content: space-between; align-items: center;",
        tags$label(class = "control-label", label_ui),
        actionButton(ns(toggle_id), label = NULL,
          icon = icon(if (is_on) "toggle-on" else "toggle-off"),
          class = paste("btn btn-xs", if (is_on) "btn-primary" else "btn-default"),
          style = "padding: 2px 6px; font-size: 14px;",
          title = i18n_r()$t("Enable custom value"))),
      if (is_on) input_when_on
      else div(style = "background-color:#f5f5f5; border:1px solid #ddd; border-radius:4px; padding:6px 12px; color:#888; height:34px; line-height:22px;", auto_text))
  }

  output$name_input_ui <- renderUI({
    toggle_field(
      tags$span(i18n_r()$t("Display Name"),
        input_help(i18n_r()$t("A descriptive name for the solute, shown in plots and results tables. Can be any text. Leave off (Auto) to name it from the CAS number automatically."),
                   title = i18n_r()$t("Display Name"), buttonLabel = i18n_r()$t("OK"))),
      "toggle_name", name_enabled(),
      textInput(ns("name_input"), label = NULL, value = isolate(input$name_input) %||% defaults$name_input),
      i18n_r()$t("Auto (from CAS / database)"))
  })

  output$smiles_input_ui <- renderUI({
    toggle_field(
      tags$span(i18n_r()$t("SMILES String"),
        input_help(i18n_r()$t("Simplified Molecular Input Line Entry System notation describing the molecular structure of the solute. Required for Group Contribution Method calculations. Leave off (Auto) to retrieve it from the CAS number."),
                   title = "SMILES", buttonLabel = i18n_r()$t("OK"))),
      "toggle_smiles", smiles_enabled(),
      textInput(ns("smiles_input"), label = NULL, value = isolate(input$smiles_input) %||% defaults$smiles_input),
      i18n_r()$t("Auto (retrieved from CAS)"))
  })

  output$tb_manual_input_ui <- renderUI({
    numericInput(ns("tb_manual_input"),
      tags$span(i18n_r()$t("Boiling Point (K):"),
        input_help(i18n_r()$t("Normal boiling point of the solute in Kelvin. Provide this when the Boiling Point Method is set to Manual Input instead of a GCM estimation."),
                   title = i18n_r()$t("Boiling Point"), buttonLabel = i18n_r()$t("OK"))),
      value = isolate(input$tb_manual_input) %||% defaults$tb_manual_input, min = 50, max = 1000, step = 0.1)
  })

  # Manual-input fields shown when the Critical / HSP method is "Manual Input" (3 fields each).
  output$crit_manual_input_ui <- renderUI({
    div(style = "display:flex; gap:8px;",
      div(style = "flex:1;", numericInput(ns("tc_manual_input"), HTML("<i>T</i><sub>c</sub> (K)"),
        value = isolate(input$tc_manual_input), min = 50, max = 2000, step = 1)),
      div(style = "flex:1;", numericInput(ns("pc_manual_input"), HTML("<i>P</i><sub>c</sub> (bar)"),
        value = isolate(input$pc_manual_input), min = 1, max = 2000, step = 0.1)),
      div(style = "flex:1;", numericInput(ns("vc_manual_input"), HTML("<i>V</i><sub>c</sub> (cm³/mol)"),
        value = isolate(input$vc_manual_input), min = 10, max = 5000, step = 1)))
  })
  outputOptions(output, "crit_manual_input_ui", suspendWhenHidden = FALSE)
  output$hsp_manual_input_ui <- renderUI({
    div(style = "display:flex; gap:8px;",
      div(style = "flex:1;", numericInput(ns("dd_manual_input"), HTML("<i>δ</i><sub>D</sub> (√MPa)"),
        value = isolate(input$dd_manual_input), min = 0, max = 60, step = 0.1)),
      div(style = "flex:1;", numericInput(ns("dp_manual_input"), HTML("<i>δ</i><sub>P</sub> (√MPa)"),
        value = isolate(input$dp_manual_input), min = 0, max = 60, step = 0.1)),
      div(style = "flex:1;", numericInput(ns("dhb_manual_input"), HTML("<i>δ</i><sub>HB</sub> (√MPa)"),
        value = isolate(input$dhb_manual_input), min = 0, max = 60, step = 0.1)))
  })
  outputOptions(output, "hsp_manual_input_ui", suspendWhenHidden = FALSE)

  # ---- Load from Database: searchable browser of the bundled experimental DB (shared helpers) ----
  db_refresh <- reactiveVal(0)   # bumped after an in-modal "add" so the table re-renders with the new row
  db_view <- reactive({ db_refresh(); exp_db_filtered(exp_db_all(), input$db_category, isTRUE(input$db_sfe_only)) })
  output$db_table <- DT::renderDataTable(exp_db_dt(i18n_r, db_view()), server = TRUE)  # server-side: only the visible page is sent (fast); full-DB export via exp_db_wire_download
  exp_db_wire_download(output, session, db_view)
  exp_db_wire_category(input, session)
  observeEvent(input$open_db_modal, showModal(exp_db_modal(ns, i18n)))
  observeEvent(input$db_load_selected, {
    sel <- input$db_table_rows_selected
    if (is.null(sel) || !length(sel)) { showNotification(i18n$t("Select a compound first."), type = "warning"); return() }
    row <- db_view()[sel, ]
    updateTextInput(session, "cas_input",  value = if (!is.na(row$cas))  sub("\\|.*$", "", row$cas) else "")
    if (!is.na(row$name)) { name_enabled(TRUE); updateTextInput(session, "name_input", value = row$name) }
    if (!is.na(row$smiles)) { smiles_enabled(TRUE); updateTextInput(session, "smiles_input", value = row$smiles) }
    if (is.finite(row$Tb)) { updateSelectInput(session, "gcm_tb", selected = "manual"); updateNumericInput(session, "tb_manual_input", value = round(row$Tb, 2)) }
    if (is.finite(row$Tc)) { updateSelectInput(session, "gcm_crit", selected = "manual")
      updateNumericInput(session, "tc_manual_input", value = round(row$Tc, 2))
      if (is.finite(row$Pc)) updateNumericInput(session, "pc_manual_input", value = round(row$Pc, 2))
      if (is.finite(row$Vc)) updateNumericInput(session, "vc_manual_input", value = round(row$Vc, 1)) }
    if (is.finite(row$dD)) { updateSelectInput(session, "gcm_hsp", selected = "manual")
      updateNumericInput(session, "dd_manual_input", value = round(row$dD, 2))
      updateNumericInput(session, "dp_manual_input", value = round(row$dP, 2))
      updateNumericInput(session, "dhb_manual_input", value = round(row$dHB, 2)) }
    if (!is.null(row$Tm) && is.finite(row$Tm)) { updateSelectInput(session, "gcm_tm", selected = "manual")
      updateNumericInput(session, "tm_manual_input", value = round(row$Tm, 2)) }
    if (!is.null(row$Hvap) && is.finite(row$Hvap)) { updateSelectInput(session, "gcm_hvap", selected = "manual")
      updateNumericInput(session, "hvap_manual_input", value = round(row$Hvap, 2)) }
    if (!is.null(row$Chr_k) && is.finite(row$Chr_k)) { updateSelectInput(session, "gcm_chrastil", selected = "manual")
      updateNumericInput(session, "chrastil_k_manual", value = round(row$Chr_k, 4))
      updateNumericInput(session, "chrastil_A_manual", value = round(row$Chr_A, 1))
      updateNumericInput(session, "chrastil_B_manual", value = round(row$Chr_B, 3)) }
    removeModal()
    showNotification(sprintf(i18n$t("Loaded '%s' from the database."), row$name), type = "message", duration = 3)
  })

  # ---- Add to Database: the form is embedded in the Load-from-Database modal; save without closing it
  # and refresh the table so the new row appears immediately. ----
  observeEvent(input$db_add_save, {
    if (isTRUE(exp_db_add_save(input, session, i18n, close = FALSE))) db_refresh(db_refresh() + 1)
  })
  # Edit / Delete of user-added DB rows (shared behaviour across the app).
  exp_db_wire_edit_delete(input, output, session, i18n, refresh = function() db_refresh(db_refresh() + 1), db_fn = db_view)

  # ---- auto-notify: experimental data for the entered compound (offline match; values used in results) ----
  db_match <- reactive(exp_db_match(exp_db_all(),
    smiles = if (smiles_enabled()) input$smiles_input else NULL,
    cas = input$cas_input, name = if (name_enabled()) input$name_input else NULL))
  output$db_notify_ui <- renderUI({
    r <- db_match(); if (is.null(r)) return(NULL)
    bits <- exp_db_bits(r, c("Tb", "Tc", "Pc", "Vc", "HSP", "Chr", "Tm", "Hvap", "dHfus")); if (!length(bits)) return(NULL)
    nbtn <- function(id, lbl) actionButton(ns(id), HTML(lbl), class = "btn-info btn-xs", style = "color:white;")
    has <- function(x) !is.null(x) && is.finite(x)
    div(class = "alert alert-info", style = "margin-top:8px; padding:8px 12px; font-size:13px;",
      span(icon("circle-info"), " ", HTML(sprintf(i18n_r()$t("Experimental data available: %s."), paste(bits, collapse = ", ")))),
      div(style = "margin-top:6px; display:flex; gap:8px; flex-wrap:wrap; align-items:center;",
        actionButton(ns("notify_load_all"), i18n_r()$t("Load All"), icon = icon("circle-down"),
          class = "btn-success btn-xs", style = "color:white; font-weight:700;"),
        if (is.finite(r$Tb)) nbtn("notify_load_tb",   paste0(i18n_r()$t("Load"), " <i>T</i><sub>b</sub>")),
        if (is.finite(r$Tc)) nbtn("notify_load_crit", paste0(i18n_r()$t("Load"), " <i>T</i><sub>c</sub>/<i>P</i><sub>c</sub>/<i>V</i><sub>c</sub>")),
        if (is.finite(r$dD)) nbtn("notify_load_hsp",  paste0(i18n_r()$t("Load"), " HSP")),
        if (has(r$Tm))   nbtn("notify_load_tm",   paste0(i18n_r()$t("Load"), " <i>T</i><sub>m</sub>")),
        if (has(r$Hvap)) nbtn("notify_load_hvap", paste0(i18n_r()$t("Load"), " Δ<i>H</i><sub>vap</sub>")),
        if (has(r$Chr_k)) nbtn("notify_load_chr", paste0(i18n_r()$t("Load"), " Chrastil <i>k/A/B</i>"))))
  })

  # ---- COSMO scCO₂ solubility engine coverage: is the loaded solute in CSP-2021 / openCOSMO-RS-Phi? ----
  # Live identity from the current inputs, enriched by the offline experimental-DB match and (for the same
  # CAS) the last Calculate snapshot, so the badge is informative even before Calculate is pressed.
  cosmo_identity <- reactive({
    cas  <- trimws(input$cas_input %||% "")
    name <- if (isTRUE(name_enabled())   && nzchar(input$name_input   %||% "")) trimws(input$name_input)   else NA_character_
    smi  <- if (isTRUE(smiles_enabled()) && nzchar(input$smiles_input %||% "")) trimws(input$smiles_input) else NA_character_
    dm <- db_match()
    if (!is.null(dm)) {
      if (is.na(name) && !is.null(dm$name)   && !is.na(dm$name)   && nzchar(dm$name))   name <- dm$name
      if (is.na(smi)  && !is.null(dm$smiles) && !is.na(dm$smiles) && nzchar(dm$smiles)) smi  <- dm$smiles
    }
    rid <- suppressWarnings(sfe_rv$solute_data$IDs)
    if (!is.null(rid) && nzchar(cas) && identical(unname(rid["CAS"]), cas)) {
      if (is.na(name)) { rn <- unname(rid["Name"]);   if (length(rn) == 1 && !is.na(rn) && nzchar(rn) && !grepl("not found|SMILES input", rn)) name <- rn }
      if (is.na(smi))  { rs <- unname(rid["SMILES"]); if (length(rs) == 1 && !is.na(rs) && nzchar(rs)) smi <- rs }
    }
    list(cas = cas, name = name, smiles = smi)
  })

  # On-demand structural-similarity list (heavy fingerprinting is deferred until requested). Cleared only
  # when the compound identity TRULY changes — keyed on the trimmed (cas,name,smiles) so cosmetic keystrokes,
  # db_match recomputes and the Calculate re-snapshot don't collapse an open list (observeEvent fires on
  # every invalidation, not just value changes, so a plain reset would wipe it constantly).
  cosmo_sim_rv <- reactiveVal(NULL)
  cosmo_last_key <- reactiveVal(NULL)
  observeEvent(cosmo_identity(), {
    i <- cosmo_identity()
    nz <- function(x) if (is.null(x) || is.na(x)) "" else as.character(x)
    k <- paste(nz(i$cas), nz(i$name), nz(i$smiles), sep = "\r")
    if (!identical(k, cosmo_last_key())) { cosmo_last_key(k); cosmo_sim_rv(NULL) }
  })
  observeEvent(input$cosmo_sim_csp,     cosmo_sim_rv(list(engine = "csp",     smiles = cosmo_identity()$smiles)))
  observeEvent(input$cosmo_sim_ocrsphi, cosmo_sim_rv(list(engine = "ocrsphi", smiles = cosmo_identity()$smiles)))

  # Warm the process-wide COSMO similarity indices once, deferred (low priority) so it runs after the initial
  # render and the first "show similar" click hits the cache instead of a ~1-2 s cold fingerprint build. The
  # cache is process-wide, so only the first session pays; later sessions/clicks are instant.
  observe({ try(cosmo_build_fingerprints("ocrsphi"), silent = TRUE)
            try(cosmo_build_fingerprints("csp"),     silent = TRUE) }, priority = -100)

  output$cosmo_avail_ui <- renderUI({
    id <- cosmo_identity()
    if (!nzchar(id$cas) && is.na(id$name) && is.na(id$smiles)) return(NULL)
    csp_ok <- cosmo_in_db(name = id$name, cas = id$cas, smiles = id$smiles, engine = "csp")
    ocr_ok <- cosmo_in_db(name = id$name, cas = id$cas, smiles = id$smiles, engine = "ocrsphi")
    have_smiles <- !is.na(id$smiles) && nzchar(id$smiles)
    pill <- function(ok, lab) tags$span(icon(if (ok) "check" else "xmark"), HTML(paste0("&nbsp;", lab)),
      style = paste0("display:inline-block;color:#fff;padding:1px 9px;border-radius:10px;font-size:11px;",
                     "margin-right:6px;white-space:nowrap;background:", if (ok) "#2e7d32" else "#c0392b", ";"))
    sim_line <- function(engine, lab) {
      if (!have_smiles) return(NULL)
      div(style = "margin-top:4px;font-size:11px;",
        actionLink(ns(paste0("cosmo_sim_", engine)),
          sprintf(i18n_r()$t("Show structurally similar compounds available in %s"), lab), icon = icon("magnifying-glass")))
    }
    div(class = "alert alert-info", style = "margin-top:8px;padding:8px 12px;font-size:13px;",
      div(style = "font-weight:600;margin-bottom:5px;",
        span(icon("database"), " ", i18n_r()$t("COSMO scCO₂ solubility engine coverage")),
        input_help(i18n_r()$t("The COSMO-based scCO₂ solubility engines (CSP-2021 and openCOSMO-RS-Phi) resolve the solute by name/SMILES/CAS; this shows whether the loaded solute is present in each engine's database. When it is not, you can list the nearest available compounds by structural (Tanimoto) similarity."),
                   title = i18n_r()$t("COSMO scCO₂ solubility engine coverage"), buttonLabel = i18n_r()$t("OK"))),
      div(style = "display:flex;gap:4px;flex-wrap:wrap;align-items:center;",
        pill(csp_ok, "CSP-2021"), pill(ocr_ok, "openCOSMO-RS-Phi")),
      if (!csp_ok) sim_line("csp", "CSP-2021"),
      if (!ocr_ok) sim_line("ocrsphi", "openCOSMO-RS-Phi"),
      uiOutput(ns("cosmo_sim_ui")))
  })
  outputOptions(output, "cosmo_avail_ui", suspendWhenHidden = FALSE)

  output$cosmo_sim_ui <- renderUI({
    s <- cosmo_sim_rv(); if (is.null(s) || is.null(s$smiles) || is.na(s$smiles) || !nzchar(s$smiles)) return(NULL)
    lab <- switch(s$engine, csp = "CSP-2021", ocrsphi = "openCOSMO-RS-Phi", s$engine)
    sim <- withProgress(message = i18n_r()$t("Searching for structurally similar compounds…"), value = NULL,
      cosmo_find_similar(s$smiles, engine = s$engine, n = 5, threshold = 0.5))
    if (!nrow(sim)) return(div(style = "margin-top:6px;font-size:12px;color:#777;",
      sprintf(i18n_r()$t("No sufficiently similar compounds found in %s."), lab)))
    chip <- function(nm, tan) tags$span(HTML(paste0(htmltools::htmlEscape(nm), " <b>", sprintf("%.2f", tan), "</b>")),
      style = "display:inline-block;background:#eef2f7;color:#333;padding:1px 8px;border-radius:10px;font-size:11px;margin:2px;")
    div(style = "margin-top:6px;",
      div(style = "font-size:12px;color:#555;margin-bottom:3px;",
        sprintf(i18n_r()$t("Nearest available compounds in %s by structural similarity (Tanimoto):"), lab)),
      div(style = "display:flex;flex-wrap:wrap;gap:2px;", lapply(seq_len(nrow(sim)), function(i) chip(sim$name[i], sim$tanimoto[i]))))
  })

  observeEvent(input$notify_load_tb, { r <- db_match(); req(r, is.finite(r$Tb))
    updateSelectInput(session, "gcm_tb", selected = "manual"); updateNumericInput(session, "tb_manual_input", value = round(r$Tb, 2))
    showNotification(i18n$t("Loaded experimental Tb."), type = "message", duration = 2) })
  observeEvent(input$notify_load_crit, { r <- db_match(); req(r, is.finite(r$Tc))
    updateSelectInput(session, "gcm_crit", selected = "manual")
    updateNumericInput(session, "tc_manual_input", value = round(r$Tc, 2))
    if (is.finite(r$Pc)) updateNumericInput(session, "pc_manual_input", value = round(r$Pc, 2))
    if (is.finite(r$Vc)) updateNumericInput(session, "vc_manual_input", value = round(r$Vc, 1))
    showNotification(i18n$t("Loaded experimental critical parameters."), type = "message", duration = 2) })
  observeEvent(input$notify_load_hsp, { r <- db_match(); req(r, is.finite(r$dD))
    updateSelectInput(session, "gcm_hsp", selected = "manual")
    updateNumericInput(session, "dd_manual_input", value = round(r$dD, 2))
    updateNumericInput(session, "dp_manual_input", value = round(r$dP, 2))
    updateNumericInput(session, "dhb_manual_input", value = round(r$dHB, 2))
    showNotification(i18n$t("Loaded experimental HSP."), type = "message", duration = 2) })
  observeEvent(input$notify_load_tm, { r <- db_match(); req(r, !is.null(r$Tm), is.finite(r$Tm))
    updateSelectInput(session, "gcm_tm", selected = "manual")
    updateNumericInput(session, "tm_manual_input", value = round(r$Tm, 2))
    showNotification(i18n$t("Loaded experimental melting point."), type = "message", duration = 2) })
  observeEvent(input$notify_load_hvap, { r <- db_match(); req(r, !is.null(r$Hvap), is.finite(r$Hvap))
    updateSelectInput(session, "gcm_hvap", selected = "manual")
    updateNumericInput(session, "hvap_manual_input", value = round(r$Hvap, 2))
    showNotification(i18n$t("Loaded experimental enthalpy of vaporization."), type = "message", duration = 2) })
  # Load every experimental property the matched compound has, in one click.
  observeEvent(input$notify_load_all, { r <- db_match(); req(r); fin <- function(x) !is.null(x) && is.finite(x)
    if (fin(r$Tb)) { updateSelectInput(session, "gcm_tb", selected = "manual"); updateNumericInput(session, "tb_manual_input", value = round(r$Tb, 2)) }
    if (fin(r$Tc)) { updateSelectInput(session, "gcm_crit", selected = "manual")
      updateNumericInput(session, "tc_manual_input", value = round(r$Tc, 2))
      if (fin(r$Pc)) updateNumericInput(session, "pc_manual_input", value = round(r$Pc, 2))
      if (fin(r$Vc)) updateNumericInput(session, "vc_manual_input", value = round(r$Vc, 1)) }
    if (fin(r$dD)) { updateSelectInput(session, "gcm_hsp", selected = "manual")
      updateNumericInput(session, "dd_manual_input", value = round(r$dD, 2))
      updateNumericInput(session, "dp_manual_input", value = round(r$dP, 2))
      updateNumericInput(session, "dhb_manual_input", value = round(r$dHB, 2)) }
    if (fin(r$Tm)) { updateSelectInput(session, "gcm_tm", selected = "manual"); updateNumericInput(session, "tm_manual_input", value = round(r$Tm, 2)) }
    if (fin(r$Hvap)) { updateSelectInput(session, "gcm_hvap", selected = "manual"); updateNumericInput(session, "hvap_manual_input", value = round(r$Hvap, 2)) }
    if (fin(r$Chr_k)) { updateSelectInput(session, "gcm_chrastil", selected = "manual")
      updateNumericInput(session, "chrastil_k_manual", value = round(r$Chr_k, 4))
      updateNumericInput(session, "chrastil_A_manual", value = round(r$Chr_A, 1))
      updateNumericInput(session, "chrastil_B_manual", value = round(r$Chr_B, 3)) }
    showNotification(i18n$t("Loaded all available experimental data."), type = "message", duration = 2) })
  observeEvent(input$notify_load_chr, { r <- db_match(); req(r, !is.null(r$Chr_k), is.finite(r$Chr_k))
    updateSelectInput(session, "gcm_chrastil", selected = "manual")
    updateNumericInput(session, "chrastil_k_manual", value = round(r$Chr_k, 4))
    updateNumericInput(session, "chrastil_A_manual", value = round(r$Chr_A, 1))
    updateNumericInput(session, "chrastil_B_manual", value = round(r$Chr_B, 3))
    showNotification(i18n$t("Loaded experimental Chrastil parameters."), type = "message", duration = 2) })

  # Render selectInput elements with i18n translations
  output$gcm_tb_ui <- renderUI({
    selectInput(ns("gcm_tb"),
      tags$span(i18n_r()$t("Boiling Point Method"),
        input_help(i18n_r()$t("Group Contribution Method for estimating the normal boiling point. Choose Manual Input to enter a known value directly. Each method uses different structural group databases and correlation equations."),
                   title = i18n_r()$t("Boiling Point Method"), buttonLabel = i18n_r()$t("OK"))),
      choices = setNames(
        c("manual", "JR", "JR_corr", "SB", "SB_corr", "NL04", "HKR_STW", "HKR_SIM"),
        i18n_r()$t(c("Manual Input", "Joback-Reid", "Joback-Reid corrected", "Stein-Brown", "Stein-Brown corrected", "Nannoolal 2004", "Hukkerikar-Stepwise", "Hukkerikar-Simultaneous"))
      ),
      selected = defaults$gcm_tb %||% "manual"
    )
  })
  outputOptions(output, "gcm_tb_ui", suspendWhenHidden = FALSE)

  output$gcm_crit_ui <- renderUI({
    selectInput(ns("gcm_crit"),
      tags$span(i18n_r()$t("Critical Parameters Method"),
        input_help(i18n_r()$t("Group Contribution Method for estimating critical temperature, pressure, and volume. These are needed for temperature-dependent HSP corrections. Set to None if not needed."),
                   title = i18n_r()$t("Critical Parameters Method"), buttonLabel = i18n_r()$t("OK"))),
      choices = setNames(
        c("none", "manual", "JR", "NL07", "NL07_robust", "HKR_STW", "HKR_SIM"),
        i18n_r()$t(c("None", "Manual Input", "Joback-Reid", "Nannoolal 2007", "Nannoolal 2007 robust", "Hukkerikar-Stepwise", "Hukkerikar-Simultaneous"))
      ),
      selected = defaults$gcm_crit %||% "none"
    )
  })
  outputOptions(output, "gcm_crit_ui", suspendWhenHidden = FALSE)


  output$gcm_hsp_ui <- renderUI({
    selectInput(ns("gcm_hsp"),
      tags$span(i18n_r()$t("Hansen Solubility (HSP) Method"),
        input_help(i18n_r()$t("Group Contribution Method for estimating Hansen Solubility Parameters (dispersion, polarity, and hydrogen bonding components). Required for co-solvent miscibility evaluation. Set to None if not needed."),
                   title = i18n_r()$t("HSP Method"), buttonLabel = i18n_r()$t("OK"))),
      choices = setNames(
        c("none", "manual", "SP08", "SP12", "SP_ENK", "HKR_STW", "HKR_SIM"),
        i18n_r()$t(c("None", "Manual Input", "Stefanis-Panayiotou 2008", "Stefanis-Panayiotou 2012", "Enekvist 2021", "Hukkerikar-Stepwise", "Hukkerikar-Simultaneous"))
      ),
      selected = defaults$gcm_hsp %||% "none"
    )
  })
  outputOptions(output, "gcm_hsp_ui", suspendWhenHidden = FALSE)

  output$gcm_vdw_ui <- renderUI({
    selectInput(ns("gcm_vdw"),
      tags$span(i18n_r()$t("Van der Waals Volume Method"),
        input_help(i18n_r()$t("Group Contribution Method for estimating the Van der Waals molecular volume. Used for molar volume calculations. Set to None if not needed."),
                   title = i18n_r()$t("VdW Volume Method"), buttonLabel = i18n_r()$t("OK"))),
      choices = setNames(
        c("none", "ZHAO", "BND", "SLON"),
        i18n_r()$t(c("None", "Zhao 2003", "Bondi 1964", "Slonimskii 1970"))
      ),
      selected = "none"
    )
  })
  outputOptions(output, "gcm_vdw_ui", suspendWhenHidden = FALSE)

  # NOTE: the "Fragmentation Simplicity" input was removed — the Müller engine ignores it (the disjoint
  # fragmentation is deterministic), so it had no effect. The run logic now passes a fixed "auto".

  # Chrastil k/A/B input: experimental coefficients entered manually or loaded from the database.
  # The predictive García-Sámano GCM estimate has been removed (it was unreliable).
  output$gcm_chrastil_ui <- renderUI({
    selectInput(ns("gcm_chrastil"),
      # Keep the k/A/B symbols (italic) OUT of the translation key so spacing + translations stay correct.
      tags$span(HTML(paste0(i18n_r()$t("Chrastil Solubility Method"), " (<i>k</i>, <i>A</i>, <i>B</i>)")),
        input_help(i18n_r()$t("The three Chrastil density-based solubility parameters (mol/mol basis; the g/L form is also reported). Choose Manual Input to enter measured coefficients, or load experimental values from the database. Lets the solute be imported into the Solubility & Fractionation > Solubility & Separator Cascade tool."),
                   title = i18n_r()$t("Chrastil Solubility Method"), buttonLabel = i18n_r()$t("OK"))),
      choices = setNames(c("none", "manual"),
        i18n_r()$t(c("None", "Manual Input"))),
      selected = "none")
  })
  outputOptions(output, "gcm_chrastil_ui", suspendWhenHidden = FALSE)

  # Manual-input fields shown when the Chrastil method is "Manual Input" (k, A, B on the mol/mol basis).
  output$chrastil_manual_input_ui <- renderUI({
    div(style = "display:flex; gap:8px;",
      div(style = "flex:1;", numericInput(ns("chrastil_k_manual"), HTML("<i>k</i>"),
        value = isolate(input$chrastil_k_manual), step = 0.01)),
      div(style = "flex:1;", numericInput(ns("chrastil_A_manual"), HTML("<i>A</i> (K)"),
        value = isolate(input$chrastil_A_manual), step = 1)),
      div(style = "flex:1;", numericInput(ns("chrastil_B_manual"), HTML("<i>B</i>"),
        value = isolate(input$chrastil_B_manual), step = 0.01)))
  })
  outputOptions(output, "chrastil_manual_input_ui", suspendWhenHidden = FALSE)

  # Melting point (Tm) and enthalpy of vaporization (Hvap): native-R ML predictors (gcm_mp / gcm_hvap,
  # from SMILES alone) or a manually entered value. Optional (default None). Feed the Solubility &
  # Separator Cascade and Co-Solvent tools.
  output$gcm_tm_ui <- renderUI({
    selectInput(ns("gcm_tm"),
      tags$span(i18n_r()$t("Melting Point Method"),
        input_help(i18n_r()$t("Source of the solute melting point. \"ML predictor\" uses a native-R gradient-boosted model trained on the open Bradley melting-point dataset (SMILES required). Choose Manual Input to enter a known value, or None to skip."),
                   title = i18n_r()$t("Melting Point Method"), buttonLabel = i18n_r()$t("OK"))),
      choices = setNames(c("none", "manual", "gcm_mp"),
        i18n_r()$t(c("None", "Manual Input", "Bradley dataset (ML)"))),
      selected = "none")
  })
  outputOptions(output, "gcm_tm_ui", suspendWhenHidden = FALSE)

  output$gcm_hvap_ui <- renderUI({
    selectInput(ns("gcm_hvap"),
      tags$span(i18n_r()$t("Enthalpy of Vaporization Method"),
        input_help(i18n_r()$t("Source of the solute enthalpy of vaporization. \"ML predictor\" uses a native-R gradient-boosted model (SMILES required); \"Chickos GCM\" estimates it from the Chen boiling-point correlation adjusted to 298 K with the Chickos-Acree heat-capacity groups (uses the group-contribution critical properties). Choose Manual Input to enter a known value, or None to skip."),
                   title = i18n_r()$t("Enthalpy of Vaporization Method"), buttonLabel = i18n_r()$t("OK"))),
      choices = setNames(c("none", "manual", "gcm_hvap", "gcm_hvap_chickos"),
        i18n_r()$t(c("None", "Manual Input", "ML predictor", "Chickos GCM (Chen + Cp)"))),
      selected = "none")
  })
  outputOptions(output, "gcm_hvap_ui", suspendWhenHidden = FALSE)

  output$tm_manual_input_ui <- renderUI({
    numericInput(ns("tm_manual_input"),
      tags$span(i18n_r()$t("Melting Point (K):"),
        input_help(i18n_r()$t("Melting point of the solute in Kelvin. Provide this when the Melting Point Method is set to Manual Input."),
                   title = i18n_r()$t("Melting Point"), buttonLabel = i18n_r()$t("OK"))),
      value = isolate(input$tm_manual_input), min = 50, max = 1000, step = 0.1)
  })
  outputOptions(output, "tm_manual_input_ui", suspendWhenHidden = FALSE)

  output$hvap_manual_input_ui <- renderUI({
    numericInput(ns("hvap_manual_input"),
      tags$span(i18n_r()$t("ΔHvap (kJ/mol):"),
        input_help(i18n_r()$t("Enthalpy of vaporization of the solute in kJ/mol. Provide this when the Enthalpy of Vaporization Method is set to Manual Input."),
                   title = i18n_r()$t("Enthalpy of Vaporization"), buttonLabel = i18n_r()$t("OK"))),
      value = isolate(input$hvap_manual_input), min = 0, max = 500, step = 0.1)
  })
  outputOptions(output, "hvap_manual_input_ui", suspendWhenHidden = FALSE)

  # Fusion enthalpy (ΔHfus): Chickos-Acree group-contribution estimate (needs a melting point) or manual.
  # The selector is DISABLED while the Melting Point Method is "None" — ΔHfus = Tfus·ΔS_tpce cannot be
  # estimated without Tfus (see the enable/disable observer keyed on input$gcm_tm below).
  output$gcm_hfus_ui <- renderUI({
    selectInput(ns("gcm_hfus"),
      tags$span(i18n_r()$t("Fusion Enthalpy Method"),
        input_help(i18n_r()$t("Source of the solute enthalpy of fusion. \"Chickos-Acree GCM\" estimates it from structure as ΔHfus = Tfus·ΔS_tpce (total-phase-change-entropy group contributions), and also returns ΔS_tpce and the liquid/solid heat capacities. It REQUIRES a melting point, so this field is disabled until a Melting Point Method other than None is chosen. Choose Manual Input to enter a known value."),
                   title = i18n_r()$t("Fusion Enthalpy Method"), buttonLabel = i18n_r()$t("OK"))),
      choices = setNames(c("none", "manual", "gcm_fusion"),
        i18n_r()$t(c("None", "Manual Input", "Chickos-Acree GCM"))),
      selected = "none")
  })
  outputOptions(output, "gcm_hfus_ui", suspendWhenHidden = FALSE)

  output$hfus_manual_input_ui <- renderUI({
    numericInput(ns("hfus_manual_input"),
      tags$span(i18n_r()$t("ΔHfus (kJ/mol):"),
        input_help(i18n_r()$t("Enthalpy of fusion of the solute in kJ/mol. Provide this when the Fusion Enthalpy Method is set to Manual Input."),
                   title = i18n_r()$t("Fusion Enthalpy"), buttonLabel = i18n_r()$t("OK"))),
      value = isolate(input$hfus_manual_input), min = 0, max = 200, step = 0.1)
  })
  outputOptions(output, "hfus_manual_input_ui", suspendWhenHidden = FALSE)

  # (The Makarov ML scCO₂-solubility method was removed — it gave unreliable estimates. scCO₂ solubility now
  # comes only from experimental Chrastil coefficients entered manually or loaded from the database.)

  # Reference T/P for the displayed scCO₂ (Chrastil) solubility. Default 40 °C / 200 bar. Chrastil solubility
  # appears only from manually-entered/DB coefficients (the GCM estimate was removed), so the help text does
  # not imply it is estimated.
  output$additional_params_ui <- renderUI({
    tagList(
      helpText(i18n_r()$t("Reference temperature and pressure at which the displayed scCO₂ (Chrastil) solubility is evaluated, when experimental Chrastil coefficients are provided.")),
      fluidRow(
        column(6, numericInput(ns("sol_ref_temp"), i18n_r()$t("Temperature (°C)"),
          value = isolate(input$sol_ref_temp) %||% 40, min = 31, max = 100, step = 1)),
        column(6, numericInput(ns("sol_ref_pres"), i18n_r()$t("Pressure (bar)"),
          value = isolate(input$sol_ref_pres) %||% 200, min = 80, max = 600, step = 5))))
  })
  outputOptions(output, "additional_params_ui", suspendWhenHidden = FALSE)

  # Reference T (°C) / P (bar) for solubility, read live with sensible defaults.
  ref_T <- function() { v <- suppressWarnings(as.numeric(input$sol_ref_temp)); if (length(v) == 1 && is.finite(v)) v else 40 }
  ref_P <- function() { v <- suppressWarnings(as.numeric(input$sol_ref_pres)); if (length(v) == 1 && is.finite(v)) v else 200 }


  # Maximum group order available per GCM method
  gcm_max_order <- c(
    "manual" = 0, "none" = 0,
    "JR" = 1, "JR_corr" = 1, "SB" = 1, "SB_corr" = 1,
    "NL04" = 2, "NL07" = 2, "NL07_robust" = 2,
    "SP08" = 2, "SP12" = 2, "SP_ENK" = 2,
    "HKR_STW" = 3, "HKR_SIM" = 3,
    "ZHAO" = 1, "BND" = 1, "SLON" = 1
  )

  # Helper to render a group order input with COM-style range badge
  render_gorder_input <- function(input_id, method_value, show_help = FALSE) {
    max_ord <- gcm_max_order[method_value]
    if (is.na(max_ord)) max_ord <- 0
    is_disabled <- max_ord <= 1
    # Labelled choices: "All available orders" (0 = use every order) + an explicit cap per available order.
    ord_labels <- c(i18n_r()$t("1st order only"), i18n_r()$t("Up to 2nd order"), i18n_r()$t("Up to 3rd order"))
    if (is_disabled) {
      choices <- setNames("1", i18n_r()$t("1st order only")); sel <- "1"
    } else {
      choices <- setNames(c("0", as.character(seq_len(max_ord))),
                          c(i18n_r()$t("All available orders"), ord_labels[seq_len(max_ord)]))
      sel <- "0"
    }
    sel_input <- selectInput(ns(input_id), NULL, choices = choices, selected = sel, width = "100%")
    if (is_disabled) sel_input <- shinyjs::disabled(sel_input)

    help_icon <- if (show_help) {
      input_help(i18n_r()$t("Maximum order of structural groups to include. \"All available orders\" uses every order the method defines; a lower setting falls back to a simpler first-/second-order model. First-order groups are always used."),
                 title = i18n_r()$t("Group Order"), buttonLabel = i18n_r()$t("OK"))
    } else NULL

    tags$div(
      tags$label(i18n_r()$t("Group Order:"), help_icon, class = "control-label",
        style = "display: flex; align-items: center; gap: 6px; width: 100%;"),
      sel_input
    )
  }

  # Boiling point group order
  output$gcm_tb_gorder_ui <- renderUI({
    method <- input$gcm_tb
    if (is.null(method) || method %in% c("manual", "none", "")) return(NULL)
    render_gorder_input("gcm_tb_gorder", method, show_help = TRUE)
  })
  outputOptions(output, "gcm_tb_gorder_ui", suspendWhenHidden = FALSE)

  # Critical parameters group order
  output$gcm_crit_gorder_ui <- renderUI({
    method <- input$gcm_crit
    if (is.null(method) || method == "none" || method == "") return(NULL)
    render_gorder_input("gcm_crit_gorder", method)
  })
  outputOptions(output, "gcm_crit_gorder_ui", suspendWhenHidden = FALSE)

  # HSP group order
  output$gcm_hsp_gorder_ui <- renderUI({
    method <- input$gcm_hsp
    if (is.null(method) || method == "none" || method == "") return(NULL)
    render_gorder_input("gcm_hsp_gorder", method)
  })
  outputOptions(output, "gcm_hsp_gorder_ui", suspendWhenHidden = FALSE)

  # Reactive value to store solute information and GCM results


  # Render molecular plot
  #   output$molecular_plot <- renderPlot({
  #   grid::grid.raster(
  #     sfe_rv$gcm_results[["visres"]][[1]],
  #     width = 1,
  #     height = 1,
  #     interpolate = TRUE
  #   )
  # }, res = 150)  #

  # ---- Group-ORDER switcher for the group-highlight visualizations (Tb / Critical / HSP) ----
  # visres[[param]] is now a named list "1"/"2"/"3" -> depiction (one image per available group order:
  # 1st-order partition + any 2nd/3rd-order correction groups). The modal shows buttons to switch.
  vis_order_label <- function(o) c("1" = i18n_r()$t("1st order"), "2" = i18n_r()$t("2nd order"),
                                   "3" = i18n_r()$t("3rd order"))[as.character(o)]
  vis_order_buttons <- function(param, order_iid) {
    v <- sfe_rv$gcm_results$visres[[param]]; ords <- if (is.list(v)) names(v) else "1"
    if (length(ords) <= 1) return(NULL)
    div(style = "text-align:center; margin-bottom:10px;",
      shinyWidgets::radioGroupButtons(ns(order_iid), label = NULL,
        choices = stats::setNames(ords, unname(vis_order_label(ords))), selected = ords[1],
        size = "sm", status = "primary"))
  }
  vis_render <- function(param, order_iid, alt) {
    v <- sfe_rv$gcm_results$visres[[param]]; req(v)
    ords <- if (is.list(v)) names(v) else "1"; o <- input[[order_iid]] %||% ords[1]; if (!o %in% ords) o <- ords[1]
    img <- if (is.list(v)) v[[o]] else v; req(img)
    outfile <- tempfile(fileext = ".png"); png(outfile, width = 1200, height = 1200, res = 300)
    grid::grid.raster(img, width = 1, height = 1, interpolate = TRUE); dev.off()
    list(src = outfile, contentType = "image/png", width = "100%", height = "auto", alt = alt)
  }

  # HSP Visualization
  observeEvent(input$show_hsp_vis, {
    req(sfe_rv$gcm_results$visres$HSP)
    showModal(modalDialog(
      title = i18n$t("HSP Visualization"),
      size = "xl",
      vis_order_buttons("HSP", "hsp_vis_order"),
      div(
        style = "max-height: 80vh; overflow: auto; text-align: center;",
        imageOutput(ns("hsp_fullsize_img"), width = "100%", height = "auto")
      ),
      footer = modalButton(i18n$t("Close")),
      easyClose = TRUE,
      fade = TRUE
    ))
  })

  output$hsp_fullsize_img <- renderImage(vis_render("HSP", "hsp_vis_order", "HSP Visualization"), deleteFile = TRUE)

  # Critical Parameters Visualization
  observeEvent(input$show_critical_vis, {
    req(sfe_rv$gcm_results$visres$Critical)
    showModal(modalDialog(
      title = i18n$t("Critical Parameters Visualization"),
      size = "xl",
      vis_order_buttons("Critical", "critical_vis_order"),
      div(
        style = "max-height: 80vh; overflow: auto; text-align: center;",
        imageOutput(ns("critical_fullsize_img"), width = "100%", height = "auto")
      ),
      footer = modalButton(i18n$t("Close")),
      easyClose = TRUE,
      fade = TRUE
    ))
  })

  output$critical_fullsize_img <- renderImage(vis_render("Critical", "critical_vis_order", "Critical Parameters Visualization"), deleteFile = TRUE)

  # Tb Visualization
  observeEvent(input$show_tb_vis, {
    req(sfe_rv$gcm_results$visres$Tb)
    showModal(modalDialog(
      title = HTML(paste0("<i>T</i><sub><i>b</i></sub> ", i18n$t("Visualization"))),
      size = "xl",
      vis_order_buttons("Tb", "tb_vis_order"),
      div(
        style = "max-height: 80vh; overflow: auto; text-align: center;",
        imageOutput(ns("tb_fullsize_img"), width = "100%", height = "auto")
      ),
      footer = modalButton(i18n$t("Close")),
      easyClose = TRUE,
      fade = TRUE
    ))
  })

  output$tb_fullsize_img <- renderImage(vis_render("Tb", "tb_vis_order", "Tb Visualization"), deleteFile = TRUE)

  # VDW Volume Visualization — the engine renders a depiction for the VDW decomposition too, so make it
  # reachable (its Group Contributions table is colour-coded against it, same as the other properties).
  observeEvent(input$show_vdw_vis, {
    req(sfe_rv$gcm_results$visres$VDW)
    showModal(modalDialog(
      title = i18n$t("VDW Volume Visualization"),
      size = "xl",
      vis_order_buttons("VDW", "vdw_vis_order"),
      div(style = "max-height: 80vh; overflow: auto; text-align: center;",
        imageOutput(ns("vdw_fullsize_img"), width = "100%", height = "auto")),
      footer = modalButton(i18n$t("Close")), easyClose = TRUE, fade = TRUE))
  })
  output$vdw_fullsize_img <- renderImage(vis_render("VDW", "vdw_vis_order", "VDW Visualization"), deleteFile = TRUE)

  # Chrastil Visualization (solubility curve from the estimated k/A/B). Drives the conditional
  # "Show Chrastil Visualization" button and the extra results column.
  # Gate the visualization button on the rendered molecule image (the params column shows
  # independently whenever the estimate exists).
  output$has_chrastil <- reactive({ !is.null(sfe_rv$gcm_results$chrastil$visual) })
  outputOptions(output, "has_chrastil", suspendWhenHidden = FALSE)

  # Reactive solubility value for the Chrastil card's single row. The UNIT is a clickable green badge
  # that toggles mol/mol <-> g/L in place (a plain span with onclick -> setInputValue, so re-rendering
  # the value never re-fires the toggle). State held in chr_sol_unit_rv.
  chr_sol_unit_rv <- reactiveVal("mol/mol")
  observeEvent(input$chr_sol_toggle, {
    chr_sol_unit_rv(if (identical(chr_sol_unit_rv(), "g/L")) "mol/mol" else "g/L")
  }, ignoreInit = TRUE)
  output$chr_sol_value <- renderUI({
    chr <- sfe_rv$gcm_results$chrastil; req(chr)
    mw  <- suppressWarnings(as.numeric(sfe_rv$solute_data$IDs["MW"])[1])
    rT  <- sfe_rv$gcm_results$sol_ref$T; rT <- if (length(rT) && isTRUE(is.finite(rT))) rT else 40
    rP  <- sfe_rv$gcm_results$sol_ref$P; rP <- if (length(rP) && isTRUE(is.finite(rP))) rP else 200
    sol <- tryCatch(chrastil_solubility(chr$k, chr$A, chr$B, rT, rP,
             mw = if (isTRUE(is.finite(mw))) mw else NULL), error = function(e) NULL)
    if (is.null(sol)) return(tags$span("–"))
    has_gL <- !is.null(sol$S_gL)
    unit <- if (has_gL) chr_sol_unit_rv() else "mol/mol"
    num  <- if (identical(unit, "g/L")) fmt_sci(sol$S_gL[1], 4) else fmt_sci(sol$y_molmol[1], 4)
    badge <- if (has_gL)
      tags$span(unit, title = i18n_r()$t("Click to switch units"),
        onclick = sprintf("Shiny.setInputValue('%s', Math.random())", ns("chr_sol_toggle")),
        style = "display:inline-block; background:#16a085; color:#fff; padding:1px 8px; border-radius:10px; font-size:11px; cursor:pointer; user-select:none; white-space:nowrap;")
    else tags$span(unit, class = "text-muted", style = "font-size:11px; white-space:nowrap;")
    # flex-wrap -> if the cell is ever too narrow the badge drops to a new line instead of off the card edge
    div(style = "display:flex; align-items:center; gap:6px; flex-wrap:wrap;", tags$span(num), badge)
  })

  # (Makarov ML scCO₂-solubility display removed along with the method.)

  observeEvent(input$show_chrastil_vis, {
    req(sfe_rv$gcm_results$chrastil$visual)
    showModal(modalDialog(
      title = i18n$t("Chrastil Visualization"),
      size = "xl",
      div(
        style = "max-height: 85vh; overflow: auto; text-align: center;",
        imageOutput(ns("chrastil_fullsize_img"), width = "100%", height = "auto")
      ),
      footer = modalButton(i18n$t("Close")),
      easyClose = TRUE,
      fade = TRUE
    ))
  })

  output$chrastil_fullsize_img <- renderImage(
    {
      req(sfe_rv$gcm_results$chrastil$visual)
      outfile <- tempfile(fileext = ".png")
      png(outfile, width = 1200, height = 1200, res = 300)
      grid::grid.raster(sfe_rv$gcm_results$chrastil$visual,
        width = 1, height = 1,
        interpolate = TRUE
      )
      dev.off()
      list(
        src = outfile,
        contentType = "image/png",
        width = "100%",
        height = "auto",
        alt = "Chrastil GCM Visualization"
      )
    },
    deleteFile = TRUE
  )

  # Per-(property, order) group-ID → highlight-colour maps, matching the decomposition images (visres). Cached
  # once per result set (parses SMILES + fragments via rJava, so compute lazily, not on every render). Used to
  # colour-code the Group Contributions tables, the legend overlay, and the PDF export.
  group_colmaps <- reactive({
    r <- sfe_rv$gcm_results; if (is.null(r)) return(list())
    meths <- r$pares$Methods; if (is.null(meths)) return(list())
    ob <- lapply(r$visres %||% list(), function(v) if (is.list(v)) names(v) else NULL)
    tryCatch(solchar_group_colmaps(r$solute_data, meths, ob), error = function(e) list())
  })
  # property -> the EXISTING per-property "Show … Visualization" modal trigger (their buttons were removed when
  # the context-vis dropdown replaced them; the observers still exist, so an icon with this id reopens them).
  .solchar_vis_trigger <- c(Tb = "show_tb_vis", Critical = "show_critical_vis", HSP = "show_hsp_vis", VDW = "show_vdw_vis")

  # ---- Inline contextual visualization (shown to the LEFT of the Molecular Descriptors table) ----
  # A single dropdown selects which group-highlight / solubility visualization to render live,
  # replacing the old row of "Show ... Visualization" modal buttons. Choices appear only for the
  # visualizations that were actually computed.
  vis_choices <- reactive({
    vr <- sfe_rv$gcm_results$visres
    meth <- sfe_rv$gcm_results$pares$Methods; md <- val_method_disp()
    # Append the employed estimator method in parentheses (item 9), e.g. "Boiling Point (Nannoolal 2007)".
    lbl <- function(base, key, fixed = NULL) {
      m <- if (!is.null(fixed)) fixed else meth[[key]]
      if (!is.null(m) && !is.na(m) && nzchar(m)) {
        fn <- if (!is.na(md[m])) unname(md[m]) else m
        paste0(base, " (", fn, ")")
      } else base
    }
    ch <- character(0)
    if (!is.null(vr$Tb))       ch <- c(ch, stats::setNames("Tb",       lbl(i18n_r()$t("Boiling Point"), "Tb")))
    if (!is.null(vr$Critical)) ch <- c(ch, stats::setNames("Critical", lbl(i18n_r()$t("Critical Parameters"), "Critical")))
    if (!is.null(vr$HSP))      ch <- c(ch, stats::setNames("HSP",      lbl(i18n_r()$t("Hansen Solubility Parameters"), "HSP")))
    if (!is.null(sfe_rv$gcm_results$chrastil$visual))
      ch <- c(ch, stats::setNames("Chrastil", lbl(i18n_r()$t("Chrastil Solubility"), NULL, fixed = "García-Sámano 2024")))
    ch
  })
  output$context_vis_ui <- renderUI({
    ch <- vis_choices(); if (!length(ch)) return(NULL)
    sel <- isolate(input$context_vis_sel); if (is.null(sel) || !sel %in% ch) sel <- unname(ch[1])
    tagList(
      # Dropdown + a small legend badge; the badge toggles a group-colour legend that overlays the Molecular
      # Descriptors table on the right. flex-end + the badge's 15px bottom margin align it with the input box
      # (not its label) — the input's .form-group carries the 15px bottom margin.
      div(style = "display:flex; align-items:flex-end; gap:6px;",
        div(style = "flex:1 1 auto;",
          selectInput(ns("context_vis_sel"), i18n_r()$t("Select Plot"), choices = ch, selected = sel, width = "100%")),
        div(style = "margin-bottom:15px;",
          # Match the Select-Plot input box height (34px) and bottom-align with its box (the 15px margin), so the
          # badge sits level with the dropdown, not below it.
          tags$button(type = "button", class = "btn",
            style = "background:#16a085; color:#fff; border:none; height:34px; padding:6px 10px; display:inline-flex; align-items:center;",
            title = i18n_r()$t("Show group legend"),
            onclick = sprintf("Shiny.setInputValue('%s', Math.random())", ns("legend_toggle")),
            icon("palette")))),
      uiOutput(ns("context_vis_order_ui")),
      # Image box flex-grows to fill the column so the visualization height tracks the Descriptor +
      # Other-Parameters tables on the right; the image scales to contain within the box (object-fit).
      div(style = "flex:1 1 auto; min-height:260px; border:1px solid #ddd; border-radius:4px; padding:6px; background:#fff; display:flex; align-items:center; justify-content:center; overflow:hidden;",
        imageOutput(ns("context_vis_img"), height = "100%", width = "100%"))
    )
  })
  output$context_vis_order_ui <- renderUI({
    sel <- input$context_vis_sel; req(sel)
    if (identical(sel, "Chrastil")) return(NULL)
    vis_order_buttons(sel, "context_vis_order")
  })
  output$context_vis_img <- renderImage({
    sel <- input$context_vis_sel %||% unname(vis_choices()[1]); req(sel)
    res <- if (identical(sel, "Chrastil")) {
      img <- sfe_rv$gcm_results$chrastil$visual; req(img)
      outfile <- tempfile(fileext = ".png"); png(outfile, width = 1200, height = 1000, res = 220)
      grid::grid.raster(img, width = 1, height = 1, interpolate = TRUE); dev.off()
      list(src = outfile, contentType = "image/png", alt = "Chrastil Visualization")
    } else {
      vis_render(sel, "context_vis_order", paste(sel, "Visualization"))
    }
    # Fill the flex box and scale to contain (no distortion) so the visualization height matches the
    # combined Descriptor + Other-Parameters tables on the right.
    res$width <- "100%"; res$height <- "100%"; res$style <- "object-fit:contain;"
    res
  }, deleteFile = TRUE)

  # ---- Group-colour legend overlay (toggled by the badge next to the visualization dropdown) ----
  # Shows, for the SELECTED property + group order, each highlighted group's colour swatch + name, overlaying
  # the Molecular Descriptors table. A transparent full-screen backdrop closes it on any outside click.
  legend_open <- reactiveVal(FALSE)
  observeEvent(input$legend_toggle, legend_open(!isTRUE(legend_open())), ignoreInit = TRUE)
  observeEvent(input$legend_close,  legend_open(FALSE), ignoreInit = TRUE)
  # close automatically if the user switches the visualization / order while it is open
  observeEvent(list(input$context_vis_sel, input$context_vis_order), { if (isTRUE(legend_open())) legend_open(FALSE) }, ignoreInit = TRUE)
  output$group_legend_overlay <- renderUI({
    if (!isTRUE(legend_open())) return(NULL)
    sel <- input$context_vis_sel %||% unname(vis_choices()[1]); req(sel)
    close_js <- sprintf("Shiny.setInputValue('%s', Math.random())", ns("legend_close"))
    panel <- function(body) tagList(
      div(style = "position:fixed; inset:0; z-index:1900; background:transparent;", onclick = close_js),
      div(onclick = "event.stopPropagation();",
        style = "position:absolute; top:0; left:0; width:320px; max-width:100%; max-height:100%; overflow:auto; z-index:2000; background:#fff; border:1px solid #16a085; border-radius:6px; box-shadow:0 6px 22px rgba(0,0,0,0.20); padding:10px 12px;",
        div(style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:8px; border-bottom:1px solid #eee; padding-bottom:6px;",
          tags$strong(i18n_r()$t("Group colour legend")),
          tags$span(HTML("&times;"), title = i18n_r()$t("Close"), onclick = close_js,
            style = "cursor:pointer; font-size:20px; line-height:1; color:#888; padding:0 4px;")),
        body))
    if (identical(sel, "Chrastil"))
      return(panel(div(class = "text-muted", i18n_r()$t("No group decomposition for this visualization."))))
    cm <- group_colmaps()[[sel]]
    ord <- input$context_vis_order %||% (if (!is.null(cm) && length(cm)) names(cm)[1] else "1")
    m <- if (!is.null(cm)) cm[[as.character(ord)]] else NULL
    if (is.null(m) || !length(m))
      return(panel(div(class = "text-muted", i18n_r()$t("No group decomposition for this visualization."))))
    contr <- sfe_rv$gcm_results$pares$Contribs[[sel]]
    gname <- function(id) { if (is.null(contr) || !"ID" %in% names(contr)) return(id)
      j <- match(as.character(id), as.character(contr$ID)); if (is.na(j)) id else as.character(contr$Group[j]) }
    rows <- lapply(seq_along(m), function(i) {
      div(style = "display:flex; align-items:center; gap:8px; padding:3px 0;",
        tags$span(style = sprintf("display:inline-block; width:15px; height:15px; border:1px solid #999; border-radius:3px; background:%s; flex:0 0 auto;", m[[i]])),
        tags$span(gname(names(m)[i])),
        tags$span(class = "text-muted", style = "margin-left:auto; font-size:11px;", paste0("ID ", names(m)[i])))
    })
    panel(tagList(rows))
  })

  # Render predicted parameters table
  output$predicted_params_table <- DT::renderDataTable({
    req(sfe_rv$gcm_results$pares)
    pares <- sfe_rv$gcm_results$pares
    ph    <- sfe_rv$gcm_results$phase
    mkdf  <- function(p, v, cat) data.frame(Parameter = p, Value = v, Category = cat, stringsAsFactors = FALSE)

    # === Phase-Transition Properties: boiling point (Tb), melting point (Tm), heat of vaporization ===
    # Tb is split out of the Critical block so all three phase-transition properties sit together
    # (consistent with the results cards). Tm/Hvap come from the ML/manual phase estimates.
    crit_all <- pares$Critical
    tb_val   <- if (!is.null(crit_all) && "Tb" %in% names(crit_all)) crit_all[["Tb"]] else NA_real_
    phase_p <- character(0); phase_v <- numeric(0)
    if (isTRUE(is.finite(tb_val)))            { phase_p <- c(phase_p, "Tb");   phase_v <- c(phase_v, tb_val) }
    if (!is.null(ph) && isTRUE(is.finite(ph$Tm)))   { phase_p <- c(phase_p, "Tm");   phase_v <- c(phase_v, ph$Tm) }
    if (!is.null(ph) && isTRUE(is.finite(ph$Hvap))) { phase_p <- c(phase_p, "Hvap"); phase_v <- c(phase_v, ph$Hvap) }
    if (!is.null(ph) && isTRUE(is.finite(ph$Hfus))) { phase_p <- c(phase_p, "Hfus"); phase_v <- c(phase_v, ph$Hfus) }
    if (!is.null(ph) && isTRUE(is.finite(ph$dS_tpce))) { phase_p <- c(phase_p, "dS_tpce"); phase_v <- c(phase_v, ph$dS_tpce) }
    if (!is.null(ph) && isTRUE(is.finite(ph$Cp_l)))  { phase_p <- c(phase_p, "Cp_l"); phase_v <- c(phase_v, ph$Cp_l) }
    if (!is.null(ph) && isTRUE(is.finite(ph$Cp_s)))  { phase_p <- c(phase_p, "Cp_s"); phase_v <- c(phase_v, ph$Cp_s) }
    phase_df <- if (length(phase_p)) mkdf(phase_p, phase_v, "Phase-Transition Properties") else NULL

    # === Critical Parameters (Tc, Pc, Vc) — Tb removed (now under Phase-Transition) ===
    crit_keep <- if (!is.null(crit_all)) crit_all[setdiff(names(crit_all), "Tb")] else NULL
    critical_df <- if (length(crit_keep)) mkdf(names(crit_keep), unname(crit_keep), "Critical Parameters") else NULL

    hsp_df <- if (!is.null(pares$HSP)) mkdf(names(pares$HSP), unname(unlist(pares$HSP)), "Hansen Solubility Parameters") else NULL

    # === Other Parameters: Van der Waals volume (Makarov ML scCO₂ solubility removed — unreliable) ===
    other_p <- character(0); other_v <- numeric(0)
    if (!is.null(pares$VDW)) { other_p <- c(other_p, "VDW"); other_v <- c(other_v, pares$VDW) }
    other_df <- if (length(other_p)) mkdf(other_p, other_v, "Other Parameters") else NULL

    # === Chrastil solubility parameters + derived solubility at the reference T/P (mol/mol & g/L) ===
    chrastil_df <- NULL
    if (!is.null(sfe_rv$gcm_results$chrastil)) {
      ch <- sfe_rv$gcm_results$chrastil
      rT <- sfe_rv$gcm_results$sol_ref$T; rT <- if (length(rT) && isTRUE(is.finite(rT))) rT else 40
      rP <- sfe_rv$gcm_results$sol_ref$P; rP <- if (length(rP) && isTRUE(is.finite(rP))) rP else 200
      chrastil_df <- mkdf(c("chrastil_k", "chrastil_A", "chrastil_B"), c(ch$k, ch$A, ch$B), "Chrastil Solubility Parameters")
      mw  <- suppressWarnings(as.numeric(sfe_rv$solute_data$IDs["MW"])[1])
      sol <- tryCatch(chrastil_solubility(ch$k, ch$A, ch$B, rT, rP,
               mw = if (isTRUE(is.finite(mw))) mw else NULL), error = function(e) NULL)
      if (!is.null(sol)) chrastil_df <- rbind(chrastil_df, mkdf(
        c("chrastil_sol_molmol", "chrastil_sol_gL"),
        c(sol$y_molmol[1], if (!is.null(sol$S_gL)) sol$S_gL[1] else NA_real_),
        "Chrastil Solubility Parameters"))
    }

    # Assemble in card order: Phase -> Critical -> HSP -> Other -> Chrastil.
    parameters_df <- do.call(rbind, Filter(Negate(is.null), list(phase_df, critical_df, hsp_df, other_df, chrastil_df)))
    req(!is.null(parameters_df) && nrow(parameters_df) > 0)

    # Reliability / domain flag: mark estimates that use a detected group with no calibrated
    # contribution for that parameter (best-effort value, treat as rough). Lookup BEFORE the
    # Parameter names are re-formatted to HTML below (uses the raw ids: Tb, Tc, Pc, Vc, dD, ...).
    domain <- sfe_rv$gcm_results$pares$Domain
    chr_dom <- sfe_rv$gcm_results$chrastil$domain   # Chrastil solute-level applicability-domain flag (in_domain + reasons)
    raw_params <- parameters_df$Parameter
    exp_src <- sfe_rv$gcm_results$exp_src %||% character(0)   # params replaced with an experimental DB value
    db_src  <- sfe_rv$gcm_results$db_src  %||% character(0)   # values loaded from the experimental database (item 5)
    parameters_df$Reliability <- vapply(seq_along(raw_params), function(ix) {
      pn <- raw_params[ix]; val <- suppressWarnings(as.numeric(parameters_df$Value[ix]))
      bad <- if (!is.null(domain) && !is.null(domain[[pn]])) domain[[pn]] else NULL
      reason <- paste(bad, collapse = "; ")
      if (pn %in% exp_src || pn %in% db_src || (grepl("^chrastil_", pn) && "Chr" %in% db_src))  # experimental DB value preferred over the estimate
        sprintf("<span style='color:#16a085;font-weight:bold;' title='%s'>&#9878; %s</span>",
          i18n_r()$t("Experimental value from the database (used instead of a GCM estimate)"), i18n_r()$t("experimental (DB)"))
      else if (pn %in% c("Tm", "Hvap")) {           # phase-transition: ML/GCM prediction or user/DB value
        src <- if (pn == "Tm") ph$Tm_src else ph$Hvap_src
        if (identical(src, "manual"))
          sprintf("<span style='color:#777;' title='%s'>%s</span>", i18n_r()$t("User-provided or database value"), i18n_r()$t("user value"))
        else if (identical(src, "gcm_hvap_chickos"))
          sprintf("<span style='color:#f0ad4e;font-weight:bold;' title='%s'>%s</span>", i18n_r()$t("Chen correlation + Chickos-Acree heat-capacity adjustment"), i18n_r()$t("GCM estimate"))
        else
          sprintf("<span style='color:#f0ad4e;font-weight:bold;' title='%s'>%s</span>", i18n_r()$t("Machine-learning prediction"), i18n_r()$t("ML estimate"))
      }
      else if (pn %in% c("Hfus", "dS_tpce", "Cp_l", "Cp_s")) {   # fusion GCM: user value or Chickos-Acree estimate
        if (identical(ph$Hfus_src, "manual"))
          sprintf("<span style='color:#777;' title='%s'>%s</span>", i18n_r()$t("User-provided or database value"), i18n_r()$t("user value"))
        else {
          # data-driven reliability tier: good (~15% typical error) / fair (~29%) / poor (~60%, flagged)
          tier <- ph$Hfus_tier %||% "fair"
          if (identical(tier, "poor"))
            sprintf("<span style='color:#d9534f;font-weight:bold;' title='%s'>&#9888; %s</span>",
              i18n_r()$t("High structural risk (implausibly low fusion entropy, ring carbonyls, or unusual atoms) – treat as a rough estimate (~60% typical error)"), i18n_r()$t("Rough"))
          else if (identical(tier, "fair"))
            sprintf("<span style='color:#f0ad4e;font-weight:bold;' title='%s'>%s</span>",
              i18n_r()$t("Group-contribution estimate (Chickos-Acree), moderate reliability (~29% typical error)"), i18n_r()$t("GCM (approx.)"))
          else
            sprintf("<span style='color:#5cb85c;font-weight:bold;' title='%s'>%s</span>",
              i18n_r()$t("Group-contribution estimate (Chickos-Acree), good reliability (~15% typical error)"), i18n_r()$t("GCM estimate"))
        }
      }
      else if (grepl("^chrastil_", pn) && is.list(chr_dom) && isFALSE(chr_dom$in_domain))   # Chrastil solute out of domain
        sprintf("<span style='color:#d9534f;font-weight:bold;' title='%s: %s'>&#9888; %s</span>",
          i18n_r()$t("Solute is outside the Chrastil GCM applicability domain – the k/A/B estimate (and any solubility derived from it) is unreliable"),
          paste(chr_dom$reasons, collapse = "; "), i18n_r()$t("Out of domain"))
      else if (length(bad) && is.na(val))        # voided to NA by the physical-plausibility guard -> not estimable
        sprintf("<span style='color:#d9534f;font-weight:bold;' title='%s: %s'>&#9888; %s</span>",
          i18n_r()$t("Out of applicability domain – the method produced a non-physical value, so no reliable estimate is available"),
          reason, i18n_r()$t("Out of domain"))
      else if (length(bad))                      # value present but a detected group is uncalibrated -> rough
        sprintf("<span style='color:#f0ad4e;font-weight:bold;' title='%s: %s'>&#9888; %s</span>",
          i18n_r()$t("Uses group(s) without calibrated parameters for this property – treat as a rough estimate"),
          reason, i18n_r()$t("Rough"))
      else if (is.na(val))                       # NA for another reason (e.g. parameter not computable)
        sprintf("<span style='color:#777;' title='%s'>&ndash;</span>", i18n_r()$t("Not estimated"))
      else sprintf("<span style='color:#5cb85c;' title='%s'>&#10003;</span>", i18n_r()$t("Calibrated"))
    }, character(1))

    # Format parameter names with proper scientific notation (HTML) and explanatory text
    param_formatting <- c(
      "Tb" = paste0("<i>T</i><sub><i>b</i></sub> (", i18n_r()$t("Boiling Point"), ", K)"),
      "Tm" = paste0("<i>T</i><sub><i>m</i></sub> (", i18n_r()$t("Melting Point"), ", K)"),
      "Hvap" = paste0("Δ<i>H</i><sub>vap</sub> (", i18n_r()$t("Heat of Vaporization"), ", kJ/mol)"),
      "Hfus" = paste0("Δ<i>H</i><sub>fus</sub> (", i18n_r()$t("Fusion Enthalpy"), ", kJ/mol)"),
      "dS_tpce" = paste0("Δ<i>S</i><sub>tpce</sub> (", i18n_r()$t("Total Phase Change Entropy"), ", J/mol·K)"),
      "Cp_l" = paste0("<i>C</i><sub>p,l</sub> (", i18n_r()$t("Liquid Heat Capacity"), ", J/mol·K)"),
      "Cp_s" = paste0("<i>C</i><sub>p,s</sub> (", i18n_r()$t("Solid Heat Capacity"), ", J/mol·K)"),
      "Tb_corr" = paste0("<i>T</i><sub><i>b,corr</i></sub> (", i18n_r()$t("Corrected Boiling Point"), ", K)"),
      "Tc" = paste0("<i>T</i><sub><i>c</i></sub> (", i18n_r()$t("Critical Temperature"), ", K)"),
      "Pc" = paste0("<i>P</i><sub><i>c</i></sub> (", i18n_r()$t("Critical Pressure"), ", bar)"),
      "Vc" = paste0("<i>V</i><sub><i>c</i></sub> (", i18n_r()$t("Critical Volume"), ", cm³/mol)"),
      "dD" = paste0("δ<sub>D</sub> (", i18n_r()$t("Dispersion"), ", √MPa)"),
      "dP" = paste0("δ<sub>P</sub> (", i18n_r()$t("Polarity"), ", √MPa)"),
      "dHB" = paste0("δ<sub>HB</sub> (", i18n_r()$t("Hydrogen Bonding"), ", √MPa)"),
      "VDW" = paste0(i18n_r()$t("VDW Volume"), " (L/mol)"),
      "chrastil_k" = paste0("<i>k</i> (", i18n_r()$t("Chrastil density exponent"), ")"),
      "chrastil_A" = paste0("<i>A</i> (", i18n_r()$t("Chrastil temperature coeff."), ", K)"),
      "chrastil_B" = paste0("<i>B</i> (", i18n_r()$t("Chrastil intercept"), ")"),
      "chrastil_sol_molmol" = paste0("<i>y</i> (", i18n_r()$t("scCO₂ solubility (Chrastil)"), ", mol/mol)"),
      "chrastil_sol_gL" = paste0("<i>S</i> (", i18n_r()$t("scCO₂ solubility (Chrastil)"), ", g/L)")
    )
    parameters_df$Parameter <- my_mapvalues(parameters_df$Parameter, names(param_formatting), param_formatting, warn_missing = FALSE)

    # Translate category values
    category_translations <- c(
      "Phase-Transition Properties" = i18n_r()$t("Phase-Transition Properties"),
      "Critical Parameters" = i18n_r()$t("Critical Parameters"),
      "Hansen Solubility Parameters" = i18n_r()$t("Hansen Solubility Parameters"),
      "Other Parameters" = i18n_r()$t("Other Parameters"),
      "Chrastil Solubility Parameters" = i18n_r()$t("Chrastil Solubility Parameters")
    )
    parameters_df$Category <- my_mapvalues(parameters_df$Category, names(category_translations), category_translations, warn_missing = FALSE)

    # Display formatting: scientific for very small/large magnitudes (e.g. mol/mol solubility ~1e-5),
    # fixed 2-dp otherwise. Done here (not via DT::formatRound) so tiny solubilities don't round to 0.00.
    parameters_df$Value <- vapply(suppressWarnings(as.numeric(parameters_df$Value)), function(x) {
      if (!is.finite(x)) return("–")
      ax <- abs(x)
      if (ax != 0 && (ax < 1e-2 || ax >= 1e6)) formatC(x, format = "e", digits = 3) else sprintf("%.2f", x)
    }, character(1))

    # Move Category to the FIRST column — it drives the row grouping below (item 10).
    parameters_df <- parameters_df[, c("Category", "Parameter", "Value", "Reliability")]

    # Per-category colours (match the result-card palette), keyed by the TRANSLATED label so the
    # JS group renderer can look them up by the value DataTables passes it.
    cat_colors <- c(
      "Phase-Transition Properties"    = "#d35400",
      "Critical Parameters"            = "#f39c12",
      "Hansen Solubility Parameters"   = "#605ca8",
      "Other Parameters"               = "#00a65a",
      "Chrastil Solubility Parameters" = "#16a085"
    )
    names(cat_colors) <- my_mapvalues(names(cat_colors), names(category_translations), category_translations, warn_missing = FALSE)
    color_map_json <- jsonlite::toJSON(as.list(cat_colors), auto_unbox = TRUE)

    # Create column headers with tooltips (Category first now)
    cols_short <- c(
      i18n_r()$t("Category"),
      i18n_r()$t("Parameter"),
      i18n_r()$t("Value"),
      i18n_r()$t("Reliability")
    )

    cols_full <- c(
      i18n_r()$t("Parameter Category"),
      i18n_r()$t("Parameter Type"),
      i18n_r()$t("Numeric Value"),
      i18n_r()$t("Whether all detected groups are calibrated for this parameter")
    )

    header_html <- sprintf(
      "<span title='%s'>%s</span>",
      cols_full, cols_short
    )

    DT::datatable(
      parameters_df,
      extensions = c("Buttons", "RowGroup"),
      colnames = header_html,
      escape = FALSE,
      rownames = FALSE,
      options = list(
        scrollX = TRUE,
        dom = "Bfrtip",
        paging = FALSE,     # small fixed table - show every row (so the appended solubility rows are visible)
        ordering = FALSE,   # keep the grouped order intact (sorting would fragment the categories)
        language = tablang(),
        buttons = create_dt_export_buttons(i18n_r, "supercrit_predicted_params"),
        # Hide the raw Category column; it is rendered as a coloured group-header row instead.
        columnDefs = list(list(visible = FALSE, targets = 0)),
        rowGroup = list(
          dataSrc = 0,
          startRender = DT::JS(sprintf(
            "function(rows, group) {
               var colors = %s;
               var c = colors[group] || '#777777';
               return $('<tr/>').append(
                 '<td colspan=\"3\" style=\"background-color:'+c+';color:#fff;font-weight:bold;padding:6px 10px;border:none;\">'+
                 group + ' <span style=\"opacity:0.85;font-weight:normal;\">(' + rows.count() + ')</span></td>'
               );
             }", color_map_json))
        ),
        # Tint each data row's left edge with its category colour for continuity with the header.
        rowCallback = DT::JS(sprintf(
          "function(row, data) {
             var colors = %s;
             var c = colors[data[0]] || '#777777';
             $(row).css('border-left', '4px solid ' + c);
           }", color_map_json))
      )
    )
  })

  # Render GCM comparison table
  output$gcm_comparison_table <- DT::renderDataTable({
    req(sfe_rv$gcm_comparison)

    # Prepare data for translation
    dt_data <- sfe_rv$gcm_comparison

    # Translate parameter values
    param_translations <- c(
      "tb" = i18n_r()$t("Boiling Point"),
      "crit" = i18n_r()$t("Critical Parameters"),
      "hsp" = i18n_r()$t("Hansen Solubility Parameters"),
      "vdw" = i18n_r()$t("Van der Waals Volume"),
      "chrastil" = i18n_r()$t("Chrastil Solubility Parameters")
    )
    dt_data$parameter <- my_mapvalues(dt_data$parameter, names(param_translations), param_translations, warn_missing = FALSE)

    # Translate method names
    method_translations <- c(
      "JR" = i18n_r()$t("Joback-Reid"),
      "JR_corr" = i18n_r()$t("Joback-Reid corrected"),
      "SB" = i18n_r()$t("Stein-Brown"),
      "SB_corr" = i18n_r()$t("Stein-Brown corrected"),
      "NL04" = i18n_r()$t("Nannoolal 2004"),
      "NL07" = i18n_r()$t("Nannoolal 2007"),
      "NL07_robust" = i18n_r()$t("Nannoolal 2007 robust"),
      "SP08" = i18n_r()$t("Stefanis-Panayiotou 2008"),
      "SP12" = i18n_r()$t("Stefanis-Panayiotou 2012"),
      "SP_ENK" = i18n_r()$t("Enekvist 2021"),
      "HKR_STW" = i18n_r()$t("Hukkerikar-Stepwise"),
      "HKR_SIM" = i18n_r()$t("Hukkerikar-Simultaneous"),
      "ZHAO" = i18n_r()$t("Zhao 2003"),
      "BND" = i18n_r()$t("Bondi 1964"),
      "SLON" = i18n_r()$t("Slonimskii 1970"),
      "CHRASTIL" = i18n_r()$t("Chrastil GCM")
    )
    dt_data$method <- my_mapvalues(dt_data$method, names(method_translations), method_translations, warn_missing = FALSE)
    
    # Remove method abbreviations in parentheses from display (e.g. "Joback-Reid" -> "Joback-Reid")
    dt_data$method <- trimws(gsub("\\s*\\([^)]*\\)\\s*$", "", dt_data$method))

    # Translate simplicity values
    simplicity_translations <- c(
      "auto" = i18n_r()$t("Auto"),
      "simple" = i18n_r()$t("Simple"),
      "normal" = i18n_r()$t("Normal"),
      "complex" = i18n_r()$t("Complex")
    )
    dt_data$simplicity <- my_mapvalues(dt_data$simplicity, names(simplicity_translations), simplicity_translations, warn_missing = FALSE)

    # Create column headers with tooltips - use HTML for proper scientific notation. Built from a
    # column-name -> (short, full) lookup so the header always matches whatever columns the (possibly
    # method-filtered) comparison produced. Tb_corr is gone: the corrected Tb is folded into Tb.
    col_map <- list(
      parameter  = list(s = i18n_r()$t("Parameter"),  f = i18n_r()$t("Parameter Type")),
      method     = list(s = i18n_r()$t("Method"),     f = i18n_r()$t("Estimation Method")),
      simplicity = list(s = i18n_r()$t("Simplicity"), f = i18n_r()$t("Fragmentation Simplicity Level")),
      order      = list(s = i18n_r()$t("Order"),      f = i18n_r()$t("Maximum Group Order")),
      Tb     = list(s = "<i>T</i><sub><i>b</i></sub>", f = i18n_r()$t("Boiling Point (K)")),
      Tc     = list(s = "<i>T</i><sub><i>c</i></sub>", f = i18n_r()$t("Critical Temperature (K)")),
      Pc     = list(s = "<i>P</i><sub><i>c</i></sub>", f = i18n_r()$t("Critical Pressure (bar)")),
      Vc     = list(s = "<i>V</i><sub><i>c</i></sub>", f = i18n_r()$t("Critical Volume (cm³/mol)")),
      dD     = list(s = "δ<sub>D</sub>",  f = i18n_r()$t("Dispersion Component (√MPa)")),
      dP     = list(s = "δ<sub>P</sub>",  f = i18n_r()$t("Polarity Component (√MPa)")),
      dHB    = list(s = "δ<sub>HB</sub>", f = i18n_r()$t("Hydrogen Bonding Component (√MPa)")),
      dP_low = list(s = "δ<sub>P,low</sub>",  f = i18n_r()$t("Polarity Component (√MPa)")),
      dHB_low= list(s = "δ<sub>HB,low</sub>", f = i18n_r()$t("Hydrogen Bonding Component (√MPa)")),
      VDW    = list(s = i18n_r()$t("VDW"), f = i18n_r()$t("Van der Waals Volume (cm³/mol)")),
      k      = list(s = "<i>k</i>", f = i18n_r()$t("Chrastil density exponent")),
      A      = list(s = "<i>A</i>", f = i18n_r()$t("Chrastil temperature coeff.")),
      B      = list(s = "<i>B</i>", f = i18n_r()$t("Chrastil intercept"))
    )
    actual_cols <- colnames(dt_data)
    add_prettynames_short <- vapply(actual_cols, function(cn) if (!is.null(col_map[[cn]])) col_map[[cn]]$s else cn, character(1))
    add_prettynames_full  <- vapply(actual_cols, function(cn) if (!is.null(col_map[[cn]])) col_map[[cn]]$f else cn, character(1))
    add_prettynames_html <- sprintf(
      "<span title='%s'>%s</span>",
      add_prettynames_full, add_prettynames_short
    )

    tbl <- DT::datatable(
      dt_data,
      extensions = "Buttons",
      colnames =add_prettynames_html,
      escape = FALSE,
      options = list(
        scrollX = TRUE,
        dom = "Bfrtip",
        pageLength = 20,
        language = tablang(),
        buttons = create_dt_export_buttons(i18n_r, "supercrit_gcm_comparison")
      ),
      rownames = FALSE
    )
    # Round only the numeric columns that are actually present (method filtering / Tb_corr removal
    # mean the column set varies); formatRound errors on a non-existent column otherwise.
    r2 <- intersect(c("Tb", "Tc", "Pc", "Vc", "dD", "dP", "dHB", "dP_low", "dHB_low", "k", "A", "B"), colnames(dt_data))
    if (length(r2)) tbl <- DT::formatRound(tbl, columns = r2, digits = 2)
    if ("VDW" %in% colnames(dt_data)) tbl <- DT::formatRound(tbl, columns = "VDW", digits = 8)
    tbl
  })

  # Render GCM comparison title with solute name
  output$gcm_comparison_title <- renderUI({
    req(sfe_rv$gcm_comparison)
    solute_name <- eff_name()
    if (is.null(solute_name) || solute_name == "") {
      solute_name <- i18n_r()$t("Unknown Solute")
    }
    div(
      style = "margin-bottom: 15px;",
      h4(sprintf("%s: %s", i18n_r()$t("Estimator Comparison for"), solute_name))
    )
  })

  # ---- GCM Validation (predicted-vs-experimental parity plots) ----------------
  # Reference data bundled in inst/extdata (generated by data-raw/gcm_validation).
  gcm_val <- reactive({ supeRcrit::gcm_validation() })
  output$has_validation <- reactive({
    v <- gcm_val(); !is.null(v) && !is.null(v$predictions) && nrow(v$predictions) > 0
  })
  outputOptions(output, "has_validation", suspendWhenHidden = FALSE)

  # Point categories (precomputed in the merged-by-property dataset). Only the four PLOTTABLE ones carry a
  # predicted value; "Voided"/"Failed" have none (logged below the plot). Best-fit uses ONLY "Valid".
  val_plot_cats <- c("Valid", "High error", "Missing atoms", "Out of domain")
  val_cat_style <- list(
    "Valid"         = list(color = "#3c8dbc", symbol = "circle",       size = 7),
    "High error"    = list(color = "#f39c12", symbol = "diamond",      size = 9),
    "Missing atoms" = list(color = "#8e44ad", symbol = "square",       size = 9),
    "Out of domain" = list(color = "#e41a1c", symbol = "x",            size = 10))

  # Display maps: method code -> full name (same as the input panel / GCM comparison); property -> rich
  # HTML label (full name + symbol with italics & subscripts + unit). plotly axis titles accept the same.
  val_method_disp <- function() c(
    JR = i18n_r()$t("Joback-Reid"), JR_corr = i18n_r()$t("Joback-Reid corrected"),
    SB = i18n_r()$t("Stein-Brown"), SB_corr = i18n_r()$t("Stein-Brown corrected"),
    NL04 = i18n_r()$t("Nannoolal 2004"), NL07 = i18n_r()$t("Nannoolal 2007"), NL07_robust = i18n_r()$t("Nannoolal 2007 robust"),
    SP08 = i18n_r()$t("Stefanis-Panayiotou 2008"), SP12 = i18n_r()$t("Stefanis-Panayiotou 2012"), SP_ENK = i18n_r()$t("Enekvist 2021"),
    HKR_STW = i18n_r()$t("Hukkerikar-Stepwise"), HKR_SIM = i18n_r()$t("Hukkerikar-Simultaneous"),
    ZHAO = i18n_r()$t("Zhao 2003"), BND = i18n_r()$t("Bondi 1964"), SLON = i18n_r()$t("Slonimskii 1970"), CHRASTIL = i18n_r()$t("Chrastil GCM"),
    CHICKOS_FUS = i18n_r()$t("Chickos-Acree GCM"))
  val_prop_html <- function() c(
    Tb  = paste0(i18n_r()$t("Boiling Point"), " <i>T</i><sub><i>b</i></sub> (K)"),
    Tc  = paste0(i18n_r()$t("Critical Temperature"), " <i>T</i><sub><i>c</i></sub> (K)"),
    Pc  = paste0(i18n_r()$t("Critical Pressure"), " <i>P</i><sub><i>c</i></sub> (bar)"),
    Vc  = paste0(i18n_r()$t("Critical Volume"), " <i>V</i><sub><i>c</i></sub> (cm³/mol)"),
    dD  = paste0(i18n_r()$t("Dispersion"), " <i>δ</i><sub>D</sub> (√MPa)"),
    dP  = paste0(i18n_r()$t("Polarity"), " <i>δ</i><sub>P</sub> (√MPa)"),
    dHB = paste0(i18n_r()$t("Hydrogen Bonding"), " <i>δ</i><sub>HB</sub> (√MPa)"),
    VDW = paste0(i18n_r()$t("Van der Waals Volume"), " <i>V</i><sub>vdw</sub> (cm³/mol)"),
    Hfus = paste0(i18n_r()$t("Fusion Enthalpy"), " Δ<i>H</i><sub>fus</sub> (kJ/mol)"))
    # NOTE: the Chrastil "Solubility" validation property is intentionally omitted here so it is
    # excluded from the Global Validation picker (Chrastil is hidden from Solute Characterization).

  # Experimental-data source label -> short citation (author + year) for the picker/hover, and the full
  # reference + DOI for the "References in view" panel. Citations are language-independent (left untranslated).
  val_source_cite <- list(
    "Joback" = list(short = "Joback & Reid (1987)",
      full = "Joback, K.G.; Reid, R.C. Estimation of pure-component properties from group-contributions. Chem. Eng. Commun. 1987, 57, 233–243.",
      doi = "10.1080/00986448708960487"),
    "Biswas" = list(short = "Biswas et al. (2023)",
      full = "Biswas, S.; Chung, Y.; Ramirez, J.; Wu, H.; Green, W.H. Predicting Critical Properties and Acentric Factors of Fluids Using Multitask Machine Learning. J. Chem. Inf. Model. 2023, 63, 4574–4588.",
      doi = "10.1021/acs.jcim.3c00546"),
    "Alshehri" = list(short = "Alshehri et al. (2022)",
      full = "Alshehri, A.S.; Tula, A.K.; Zhang, L.; Gani, R.; You, F. Next generation pure component property estimation models: With and without machine learning techniques. AIChE J. 2022, 68, e17469.",
      doi = "10.1002/aic.17469"),
    "Zhao" = list(short = "Zhao et al. (2003)",
      full = "Zhao, Y.H.; Abraham, M.H.; Zissimos, A.M. Fast Calculation of van der Waals Volume as a Sum of Atomic and Bond Contributions and Its Application to Drug Compounds. J. Org. Chem. 2003, 68, 7368–7373.",
      doi = "10.1021/jo034808o"),
    "Hansen_Handbook" = list(short = "Hansen (2007)",
      full = "Hansen, C.M. Hansen Solubility Parameters: A User's Handbook, 2nd ed.; CRC Press: Boca Raton, FL, 2007.",
      doi = "10.1201/9781420006834"),
    "Hansen_Fragrance" = list(short = "Fragrance HSP DB",
      full = "In-house fragrance/flavour Hansen Solubility Parameter compilation (curated from literature).",
      doi = NA_character_),
    "Chrastil Revisited (J. Supercrit. Fluids 2021; S0896844621001881)" = list(short = "Stiver (2021)",
      full = "Stiver, W. Chrastil revisited for supercritical carbon dioxide. J. Supercrit. Fluids 2021, 177, 105348.",
      doi = "10.1016/j.supflu.2021.105348"),
    "AcreeChickos" = list(short = "Acree & Chickos (2010–2022)",
      full = "Acree, W.; Chickos, J.S. Phase Transition Enthalpy Measurements of Organic and Organometallic Compounds and Ionic Liquids. Sublimation, Vaporization, and Fusion Enthalpies (compendia + total-phase-change-entropy group values). J. Phys. Chem. Ref. Data 2010–2022.",
      doi = "10.1063/1.3309507"))
  # Short citation for a vector of source labels (falls back to the raw label if unmapped).
  val_src_short <- function(s) vapply(s, function(x) {
    info <- val_source_cite[[x]]; if (!is.null(info)) info$short else x }, character(1), USE.NAMES = FALSE)

  # --- Dropdown plot selectors (property / method / references), left-aligned in the UI ---
  output$val_property_ui <- renderUI({
    v <- gcm_val(); req(v); props <- intersect(names(val_prop_html()), unique(v$summary$property))
    shinyWidgets::pickerInput(ns("val_property"), i18n_r()$t("Property"), choices = props,
      choicesOpt = list(content = unname(val_prop_html()[props])), selected = props[1], width = "100%")
  })
  output$val_method_ui <- renderUI({
    v <- gcm_val(); req(v, input$val_property)
    meths <- unique(v$summary$method[v$summary$property == input$val_property]); md <- val_method_disp()
    selectInput(ns("val_method"), i18n_r()$t("Estimation Method"),
      choices = setNames(meths, ifelse(is.na(md[meths]), meths, md[meths])), selected = meths[1], width = "100%")
  })
  # References (sources) multi-select - replaces the per-testset dropdown now that data is merged by
  # property. Every point keeps its experimental source; the filter subsets by reference (default = all).
  output$val_sources_ui <- renderUI({
    v <- gcm_val(); req(v, input$val_property, input$val_method)
    srcs <- sort(unique(v$predictions$source[v$predictions$property == input$val_property &
                                             v$predictions$method == input$val_method]))
    # Display short citations (author + year); the VALUE stays the raw source label so filtering works.
    shinyWidgets::pickerInput(ns("val_sources"), i18n_r()$t("References (sources)"),
      choices = setNames(srcs, val_src_short(srcs)), selected = srcs, multiple = TRUE, width = "100%",
      options = list(`actions-box` = TRUE, `selected-text-format` = "count > 2",
                     countSelectedText = i18n_r()$t("{0} references")))
  })

  # All predictions for the selected (property, method), filtered by chosen references. The per-point
  # category (Valid / High error / Missing atoms / Out of domain / Voided / Failed) is precomputed.
  val_filtered <- reactive({
    v <- gcm_val(); req(v, input$val_property, input$val_method)
    p <- v$predictions[v$predictions$property == input$val_property & v$predictions$method == input$val_method, ]
    # Apply the reference filter only when the selection overlaps THIS selection's sources; otherwise a
    # stale selection carried over from a previous method/property would filter every row out (item 7).
    sel <- input$val_sources
    if (!is.null(sel) && length(sel) && any(p$source %in% sel)) p <- p[p$source %in% sel, ]
    req(nrow(p) > 0)
    if (is.null(p$abs_pct_err)) p$abs_pct_err <- ifelse(is.finite(p$predicted) & is.finite(p$experimental) &
      p$experimental != 0, 100 * abs((p$predicted - p$experimental) / p$experimental), NA_real_)
    p
  })

  # Shared plot ingredients. Axes are scaled to the in-domain "Valid" cloud (with a small pad) so the
  # view stays sensible even when a few flagged points are extreme (item 3). The best-fit line + its
  # equation come from "Valid" ONLY, so toggling the flagged series never changes them.
  val_plot_parts <- reactive({
    d <- val_filtered()
    pm <- gcm_val()$meta$prop_meta[[input$val_property]]; unit <- pm$unit
    islog <- isTRUE(pm$log)   # stored values are log10(y); plotted on a log-scaled axis (ticks 10^-n)
    pl <- d[d$category %in% val_plot_cats & is.finite(d$predicted) & is.finite(d$experimental), ]
    req(nrow(pl) > 0)
    vd <- pl[pl$category == "Valid", ]
    base <- if (nrow(vd) >= 2) vd else pl                      # scale to Valid; fall back to all plottable
    lim <- range(c(base$experimental, base$predicted), na.rm = TRUE)
    pad <- 0.04 * diff(lim); if (!is.finite(pad) || pad == 0) pad <- max(abs(lim)) * 0.04 + 1e-9
    lim <- lim + c(-pad, pad)
    fit <- if (nrow(vd) >= 2 && length(unique(vd$experimental)) >= 2)
      stats::lm(predicted ~ experimental, data = vd) else NULL
    propname <- val_prop_html()[[input$val_property]]         # full name + symbol + unit (HTML; plotly-safe)
    methname <- { md <- val_method_disp(); m <- input$val_method; if (is.na(md[m])) m else md[[m]] }
    list(d = d, pl = pl, vd = vd, lim = lim, unit = unit, fit = fit, islog = islog, propname = propname, methname = methname,
         xlab = paste0(propname, " — ", i18n_r()$t("experimental")),
         ylab = paste0(propname, " — ", i18n_r()$t("predicted"), " (", methname, ")"))
  })

  # Best-fit equation + R² text (item 2: shown above the plot instead of accuracy badges).
  val_fit_text <- reactive({
    pp <- val_plot_parts(); if (is.null(pp$fit)) return(NULL)
    co <- stats::coef(pp$fit); r2 <- summary(pp$fit)$r.squared
    sprintf("<i>y</i> = %.3f<i>x</i> %s %.3f &nbsp;&nbsp; <i>R</i><sup>2</sup> = %.3f &nbsp;&nbsp; <i>n</i><sub>Valid</sub> = %d",
            co[[2]], if (co[[1]] >= 0) "+" else "−", abs(co[[1]]), r2, nrow(pp$vd))
  })

  # --- Interactive parity plot (plotly): one toggleable series per category. Only "Valid" feeds the
  #     best-fit line; the flagged series (High error / Missing atoms / Out of domain) are shown for
  #     context and can be hidden by clicking the legend, but NEVER enter the regression. ---
  output$val_parity_plot <- plotly::renderPlotly({
    pp <- val_plot_parts(); lim <- pp$lim
    vfmt <- if (pp$islog) function(z) signif(10^z, 3) else function(z) signif(z, 4)  # back to real y for hover
    htxt <- function(g) paste0(g$name, " [", val_src_short(g$source), "]<br>exp: ", vfmt(g$experimental),
      "<br>pred: ", vfmt(g$predicted),
      ifelse(is.finite(g$abs_pct_err), paste0("<br>|%err|: ", round(g$abs_pct_err, 1)), ""),
      ifelse(!is.na(g$coverage_gap) & g$coverage_gap > 0, paste0("<br>uncovered atoms: ", g$coverage_gap), ""))
    p <- plotly::plot_ly()
    # High error + Missing atoms start hidden (item 9); Valid + Out of domain visible. No y=x line (item 2).
    off_by_default <- c("High error", "Missing atoms")
    for (cat in val_plot_cats) {
      dd <- pp$pl[pp$pl$category == cat, ]; if (!nrow(dd)) next
      st <- val_cat_style[[cat]]
      p <- plotly::add_markers(p, data = dd, x = ~experimental, y = ~predicted,
        name = sprintf("%s (%d)", i18n_r()$t(cat), nrow(dd)),
        marker = list(color = st$color, size = st$size, symbol = st$symbol,
                      line = list(width = if (cat == "Valid") 0 else 1, color = "white")),
        text = htxt(dd), hoverinfo = "text",
        visible = if (cat %in% off_by_default) "legendonly" else TRUE)
    }
    if (!is.null(pp$fit)) {
      xs <- seq(lim[1], lim[2], length.out = 60); ys <- stats::predict(pp$fit, newdata = data.frame(experimental = xs))
      p <- plotly::add_lines(p, x = xs, y = ys, line = list(color = "#00a65a", width = 2),
        name = i18n_r()$t("Best fit (Valid)"))
    }
    ax_x <- list(title = pp$xlab, range = lim, zeroline = FALSE)
    ax_y <- list(title = pp$ylab, range = lim, zeroline = FALSE, scaleanchor = "x")
    if (isTRUE(pp$islog)) {   # relabel the (stored-as-log10) ticks as real mole fractions 10^-n
      bk <- seq(ceiling(lim[1]), floor(lim[2])); tt <- paste0("10<sup>", bk, "</sup>")
      ax_x$tickvals <- bk; ax_x$ticktext <- tt; ax_y$tickvals <- bk; ax_y$ticktext <- tt
    }
    plotly::layout(p, xaxis = ax_x, yaxis = ax_y,
                   # legend overlaps the plot, top-right (item 11)
                   legend = list(x = 0.99, y = 0.99, xanchor = "right", yanchor = "top",
                                 bgcolor = "rgba(255,255,255,0.65)", bordercolor = "#cccccc", borderwidth = 1),
                   margin = list(t = 20))
  })

  # Static ggplot mirror, used only for the PNG export.
  val_parity_ggplot <- reactive({
    pp <- val_plot_parts(); d <- pp$pl
    d$kind <- factor(vapply(d$category, function(x) i18n_r()$t(x), character(1)),
                     levels = vapply(val_plot_cats, function(x) i18n_r()$t(x), character(1)))
    cols  <- setNames(vapply(val_plot_cats, function(c) val_cat_style[[c]]$color, character(1)),
                      vapply(val_plot_cats, function(x) i18n_r()$t(x), character(1)))
    shp   <- setNames(c(16, 18, 15, 4), vapply(val_plot_cats, function(x) i18n_r()$t(x), character(1)))
    strip <- function(x) gsub("<[^>]+>", "", x)               # HTML tags don't render in a ggplot device
    sub <- if (!is.null(pp$fit)) { co <- stats::coef(pp$fit); r2 <- summary(pp$fit)$r.squared
      sprintf("y = %.3f x %s %.3f   (R² = %.3f, Valid only)", co[[2]], if (co[[1]] >= 0) "+" else "-", abs(co[[1]]), r2) } else NULL
    g <- ggplot2::ggplot(d, ggplot2::aes(x = experimental, y = predicted)) +
      ggplot2::geom_point(ggplot2::aes(colour = kind, shape = kind), alpha = 0.75, size = 2) +
      ggplot2::scale_colour_manual(values = cols, drop = FALSE) +
      ggplot2::scale_shape_manual(values = shp, drop = FALSE) +
      ggplot2::coord_equal(xlim = pp$lim, ylim = pp$lim) +
      ggplot2::labs(x = strip(pp$xlab), y = strip(pp$ylab), subtitle = sub, colour = NULL, shape = NULL) +
      ggplot2::theme_bw(base_size = 13) + ggplot2::theme(legend.position = "bottom")
    if (!is.null(pp$fit)) g <- g + ggplot2::geom_abline(slope = stats::coef(pp$fit)[2],
      intercept = stats::coef(pp$fit)[1], colour = "#00a65a", linewidth = 0.9)
    if (isTRUE(pp$islog)) {   # mirror the log-axis tick relabelling (10^-n) used in the interactive plot
      bk <- seq(ceiling(pp$lim[1]), floor(pp$lim[2]))
      g <- g + ggplot2::scale_x_continuous(breaks = bk, labels = parse(text = sprintf("10^%d", bk))) +
               ggplot2::scale_y_continuous(breaks = bk, labels = parse(text = sprintf("10^%d", bk)))
    }
    g
  })

  # Best-fit equation + R² shown above the plot (item 2; replaces the accuracy badges).
  output$val_fit_eqn <- renderUI({
    txt <- val_fit_text()
    if (is.null(txt)) return(div(style = "color:#888; font-size:13px; margin:4px 0;",
      i18n_r()$t("No in-domain (Valid) points to fit for this selection.")))
    div(style = "font-size:15px; margin:4px 0 8px 0; color:#2c3e50;",
        tags$b(i18n_r()$t("Best fit (Valid):")), " ", HTML(txt))
  })

  # All methods for this property: accuracy over Valid points + per-category counts (merged by property).
  # Method codes shown as full names; the 4 standard export buttons (Copy/Excel/CSV/PDF) like other tables.
  output$val_summary_table <- DT::renderDataTable({
    v <- gcm_val(); req(v, input$val_property)
    s <- v$summary[v$summary$property == input$val_property, ]
    md <- val_method_disp(); s$method <- ifelse(is.na(md[s$method]), s$method, md[s$method])
    cols <- c("method", "unit", "n_valid", "AARD_pct", "MedARD_pct", "RMSE", "bias", "R2",
              "n_high", "n_missing", "n_ood", "n_voided", "n_failed")
    cols <- intersect(cols, names(s)); s <- s[order(s$AARD_pct), cols]
    s$AARD_pct <- round(s$AARD_pct, 2); s$MedARD_pct <- round(s$MedARD_pct, 2)
    s$RMSE <- signif(s$RMSE, 4); s$bias <- signif(s$bias, 3); s$R2 <- round(s$R2, 3)
    DT::datatable(s, rownames = FALSE, extensions = "Buttons",
      options = list(scrollX = TRUE, dom = "Bfrtip", pageLength = 10, language = tablang(),
        buttons = create_dt_export_buttons(i18n_r, "supercrit_global_validation_summary")),
      colnames = c(i18n_r()$t("Method"), i18n_r()$t("Unit"), i18n_r()$t("n (Valid)"),
                   "Mean ARD (%)", "Median ARD (%)", "RMSE", i18n_r()$t("Bias"), "R²",
                   i18n_r()$t("High error"), i18n_r()$t("Missing atoms"), i18n_r()$t("Out of domain"),
                   i18n_r()$t("Voided"), i18n_r()$t("Failed")))
  })

  # References panel: the FULL reference (with clickable DOI) for every experimental source contributing
  # to the current (property, method) selection — one per line, instead of the old short chips.
  output$val_references_panel <- renderUI({
    d <- val_filtered(); req(nrow(d) > 0)
    tab <- sort(table(d$source), decreasing = TRUE)
    items <- lapply(names(tab), function(s) {
      info <- val_source_cite[[s]]
      full <- if (!is.null(info)) info$full else s
      doi  <- if (!is.null(info)) info$doi else NA_character_
      doi_part <- if (!is.na(doi) && nzchar(doi))
        tagList(" ", tags$a(href = paste0("https://doi.org/", doi), target = "_blank",
                            rel = "noopener", paste0("doi:", doi))) else NULL
      tags$li(style = "margin-bottom:5px;", full, doi_part,
              tags$span(class = "text-muted", sprintf(" (n = %d)", tab[[s]])))
    })
    div(tags$b(i18n_r()$t("References in view:")),
        tags$ul(style = "margin:4px 0 0 0; padding-left:18px; font-size:12px; line-height:1.45;", items))
  })

  # Per-method diagnostic notes (data-mined in data-raw/gcm_validation/method_diagnostic_notes.R): which
  # structural classes dominate the method's uncovered / high-error / out-of-domain compounds.
  output$val_method_notes <- renderUI({
    v <- gcm_val(); req(v, input$val_method)
    nts <- v$meta$method_notes; n <- if (!is.null(nts)) nts[[input$val_method]] else NULL
    req(n)
    md <- val_method_disp(); mname <- if (is.na(md[input$val_method])) input$val_method else md[[input$val_method]]
    div(style = "background:#fbfcfd; border:1px solid #e1e6eb; border-left:4px solid #3c8dbc; border-radius:4px; padding:8px 12px; font-size:12px;",
      tags$b(style = "color:#2c3e50;", sprintf("%s — %s", i18n_r()$t("Method diagnostics"), mname)),
      tags$ul(style = "margin:4px 0 0 0; padding-left:18px; line-height:1.5;",
        tags$li(tags$b(i18n_r()$t("Coverage:")), " ", n$coverage),
        tags$li(tags$b(i18n_r()$t("Errors:")), " ", n$errors)),
      tags$div(style = "color:#999; margin-top:3px;", i18n_r()$t("Auto-generated from the validation set; percentages are of the flagged compounds.")))
  })

  output$val_export_plot <- downloadHandler(
    filename = function() sprintf("global_validation_%s_%s.png", input$val_property %||% "prop", input$val_method %||% "method"),
    content = function(file) ggplot2::ggsave(file, plot = val_parity_ggplot(), width = 8, height = 7, dpi = 150)
  )

  # Export the data currently in view (all plottable + logged points for the selected property/method/refs).
  output$val_export_data <- downloadHandler(
    filename = function() sprintf("global_validation_%s_%s.csv", input$val_property %||% "prop", input$val_method %||% "method"),
    content = function(file) {
      d <- val_filtered()
      cols <- intersect(c("property","method","name","smiles","source","experimental","predicted",
                          "abs_pct_err","coverage_gap","category","status"), names(d))
      utils::write.csv(d[, cols, drop = FALSE], file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  # ---- Machine-Learning model validation (item 6): honest held-out predicted-vs-experimental ----
  # Per-point held-out predictions are precomputed (data-raw/build_ml_validation.R) and bundled as
  # inst/extdata/ml_validation.rds: solubility = leave-15%-solutes-out; MP/Hvap = 10% random hold-out.
  ml_val <- reactive({
    f <- system.file("extdata", "properties", "ml_validation.rds", package = "supeRcrit")
    if (!nzchar(f)) return(NULL)
    tryCatch(readRDS(f), error = function(e) NULL)
  })
  output$has_ml_val <- reactive({ v <- ml_val()
    !is.null(v) && any(vapply(c("solubility", "mp", "hvap"),
      function(k) !is.null(v[[k]]) && is.data.frame(v[[k]]) && nrow(v[[k]]) > 0, logical(1))) })
  outputOptions(output, "has_ml_val", suspendWhenHidden = FALSE)

  # Plain text from i18n: usei18n makes t() return a <span> tag, so for selectInput/table labels and
  # plotly axis titles (where a tag renders literally or breaks setNames) we pull the child text.
  t1 <- function(s) { v <- i18n_r()$t(s)
    if (inherits(v, "shiny.tag")) { ch <- v$children; if (length(ch)) as.character(ch[[1]]) else s }
    else if (length(v)) as.character(v[1]) else s }
  # axunit = the unit shown on the plot axis (solubility ticks are relabelled to real mole fractions).
  ml_val_info <- function() list(
    solubility = list(label = t1("scCO₂ Solubility (ML)"), unit = "log₁₀ y", axunit = "mol/mol", islog = TRUE,  scheme = t1("leave-solute-out")),
    mp         = list(label = t1("Melting Point"),         unit = "K",       axunit = "K",       islog = FALSE, scheme = t1("10% hold-out")),
    hvap       = list(label = t1("Heat of Vaporization"),  unit = "kJ/mol",  axunit = "kJ/mol",  islog = FALSE, scheme = t1("10% hold-out")))
  ml_val_op <- function(d, islog) if (islog && !is.null(d$log_obs)) list(obs = d$log_obs, pred = d$log_pred) else list(obs = d$obs, pred = d$pred)
  ml_val_metrics <- function(d, islog) {
    op <- ml_val_op(d, islog); keep <- is.finite(op$obs) & is.finite(op$pred)
    o <- op$obs[keep]; p <- op$pred[keep]
    c(RMSE = sqrt(mean((p - o)^2)), MAE = mean(abs(p - o)), R2 = 1 - sum((p - o)^2) / sum((o - mean(o))^2), n = length(o))
  }
  ml_val_keys <- function(v) { ks <- intersect(c("solubility", "mp", "hvap"), names(v))
    ks[vapply(ks, function(k) !is.null(v[[k]]) && is.data.frame(v[[k]]) && nrow(v[[k]]) > 0, logical(1))] }

  output$ml_val_prop_ui <- renderUI({
    v <- ml_val(); req(v); info <- ml_val_info(); keys <- ml_val_keys(v); req(length(keys) > 0)
    ch <- stats::setNames(keys, vapply(keys, function(k) info[[k]]$label, character(1)))
    selectInput(ns("ml_val_prop"), t1("Model"), choices = ch, selected = ch[1], width = "100%")
  })

  # Shared scatter ingredients — mirrors the GCM val_plot_parts: axis-equal limits + lm best-fit on
  # the held-out points (the ML analogue of GCM's "Valid" set, since every point here is unseen).
  ml_val_parts <- reactive({
    v <- ml_val(); req(v, input$ml_val_prop); d <- v[[input$ml_val_prop]]; req(d)
    info <- ml_val_info()[[input$ml_val_prop]]; op <- ml_val_op(d, info$islog)
    keep <- is.finite(op$obs) & is.finite(op$pred)
    o <- op$obs[keep]; p <- op$pred[keep]
    nm <- if (!is.null(d$name)) d$name[keep] else if (!is.null(d$smiles)) d$smiles[keep] else rep("", length(o))
    req(length(o) > 1)
    lim <- range(c(o, p), na.rm = TRUE); pad <- 0.04 * diff(lim); if (!is.finite(pad) || pad == 0) pad <- max(abs(lim)) * 0.04 + 1e-9
    lim <- lim + c(-pad, pad)
    fit <- if (length(unique(o)) >= 2) stats::lm(p ~ o) else NULL
    list(o = o, p = p, nm = nm, lim = lim, fit = fit, info = info,
         xlab = paste0(info$label, " — ", t1("experimental"), " (", info$axunit, ")"),
         ylab = paste0(info$label, " — ", t1("predicted"), " (", info$axunit, ")"))
  })

  # Best-fit equation + R², mirroring the GCM val_fit_eqn wording and styling exactly.
  output$ml_val_fit_eqn <- renderUI({
    pp <- ml_val_parts(); req(pp$fit)
    co <- stats::coef(pp$fit); r2 <- summary(pp$fit)$r.squared
    txt <- sprintf("<i>y</i> = %.3f<i>x</i> %s %.3f &nbsp;&nbsp; <i>R</i><sup>2</sup> = %.3f &nbsp;&nbsp; <i>n</i><sub>held-out</sub> = %d",
            co[[2]], if (co[[1]] >= 0) "+" else "−", abs(co[[1]]), r2, length(pp$o))
    div(style = "font-size:15px; margin:4px 0 8px 0; color:#2c3e50;",
        tags$b(i18n_r()$t("Best fit (held-out):")), " ", HTML(txt))
  })

  # Held-out predicted-vs-experimental scatter, styled like the GCM parity plot (same blue circle
  # markers, same green best-fit line, same log-axis tick relabelling for solubility).
  output$ml_val_scatter <- plotly::renderPlotly({
    pp <- ml_val_parts(); lim <- pp$lim; info <- pp$info
    vfmt <- if (info$islog) function(z) signif(10^z, 3) else function(z) signif(z, 4)   # back to real y for hover
    htxt <- paste0(pp$nm, "<br>exp: ", vfmt(pp$o), "<br>pred: ", vfmt(pp$p))
    p <- plotly::plot_ly()
    p <- plotly::add_markers(p, x = pp$o, y = pp$p, name = sprintf("%s (%d)", t1("held-out"), length(pp$o)),
      marker = list(color = "#3c8dbc", size = 7, symbol = "circle", line = list(width = 0)),
      text = htxt, hoverinfo = "text")
    if (!is.null(pp$fit)) {
      xs <- seq(lim[1], lim[2], length.out = 60); ys <- stats::predict(pp$fit, newdata = data.frame(o = xs))
      p <- plotly::add_lines(p, x = xs, y = ys, line = list(color = "#00a65a", width = 2), name = t1("Best fit"))
    }
    ax_x <- list(title = pp$xlab, range = lim, zeroline = FALSE)
    ax_y <- list(title = pp$ylab, range = lim, zeroline = FALSE, scaleanchor = "x")
    if (isTRUE(info$islog)) {   # relabel the (stored-as-log10) ticks as real mole fractions 10^-n
      bk <- seq(ceiling(lim[1]), floor(lim[2])); tt <- paste0("10<sup>", bk, "</sup>")
      ax_x$tickvals <- bk; ax_x$ticktext <- tt; ax_y$tickvals <- bk; ax_y$ticktext <- tt
    }
    plotly::layout(p, xaxis = ax_x, yaxis = ax_y,
      legend = list(x = 0.99, y = 0.99, xanchor = "right", yanchor = "top",
                    bgcolor = "rgba(255,255,255,0.65)", bordercolor = "#cccccc", borderwidth = 1),
      margin = list(t = 20))
  })

  output$ml_val_table <- DT::renderDataTable({
    v <- ml_val(); req(v); info <- ml_val_info(); keys <- ml_val_keys(v); req(length(keys) > 0)
    train_n <- v$meta$train_n   # total samples each model was TRAINED on (baked into ml_validation.rds)
    fmtn <- function(x) if (isTRUE(is.finite(suppressWarnings(as.numeric(x))))) format(as.integer(x), big.mark = ",") else "—"
    rows <- lapply(keys, function(k) { d <- v[[k]]; m <- ml_val_metrics(d, info[[k]]$islog)
      data.frame(Model = info[[k]]$label, Validation = info[[k]]$scheme,
        TrainN = fmtn(train_n[[k]]), ValN = fmtn(m[["n"]]),
        RMSE = sprintf("%.3g %s", m[["RMSE"]], info[[k]]$unit), MAE = sprintf("%.3g %s", m[["MAE"]], info[[k]]$unit),
        R2 = sprintf("%.3f", m[["R2"]]), stringsAsFactors = FALSE) })
    disp <- do.call(rbind, rows)
    colnames(disp) <- c(t1("Model"), t1("Validation"), t1("Training samples"), t1("Validation samples"), "RMSE", "MAE", "R²")
    DT::datatable(disp, extensions = "Buttons", rownames = FALSE, escape = FALSE,
      options = list(dom = "Bfrtip", paging = FALSE, ordering = FALSE, scrollX = TRUE,
        language = tablang(), buttons = create_dt_export_buttons(i18n_r, "supercrit_ml_validation")))
  })

  # Render group contributions table
  output$group_contributions_table <- renderUI({
    contribs_list <- sfe_rv$gcm_results$pares$Contribs
    if (!is.list(contribs_list)) contribs_list <- list()
    # Append the Chrastil per-group k/A/B contributions (computed on a separate path) so they appear here too.
    if (!is.null(sfe_rv$gcm_results$chrastil$Contribs))
      contribs_list$Chrastil <- sfe_rv$gcm_results$chrastil$Contribs
    req(length(contribs_list) > 0)

    # Define translation mapping for all possible column names
    col_name_mapping <- list(
      # Common columns
      "Group" = list(short = i18n_r()$t("Group"), full = i18n_r()$t("Functional Group Name")),
      "ID" = list(short = i18n_r()$t("ID"), full = i18n_r()$t("Group Identifier")),
      "Type" = list(short = i18n_r()$t("Type"), full = i18n_r()$t("Group Order")),
      "Occurrences" = list(short = i18n_r()$t("Occurrences"), full = i18n_r()$t("Number of Occurrences")),

      # Tb columns
      "Tb_cont" = list(short = "<i>T</i><sub><i>b</i></sub>", full = i18n_r()$t("Single group contribution to boiling point")),
      "Sum_Tb_cont" = list(short = paste0("Σ<i>T</i><sub><i>b</i></sub>"), full = i18n_r()$t("Cumulative contribution (group × occurrences)")),

      # Critical columns
      "Tc_cont" = list(short = "<i>T</i><sub><i>c</i></sub>", full = i18n_r()$t("Single group contribution to critical temperature")),
      "Sum_Tc_cont" = list(short = "Σ<i>T</i><sub><i>c</i></sub>", full = i18n_r()$t("Cumulative contribution (group × occurrences)")),
      "Pc_cont" = list(short = "<i>P</i><sub><i>c</i></sub>", full = i18n_r()$t("Single group contribution to critical pressure")),
      "Sum_Pc_cont" = list(short = "Σ<i>P</i><sub><i>c</i></sub>", full = i18n_r()$t("Cumulative contribution (group × occurrences)")),
      "Vc_cont" = list(short = "<i>V</i><sub><i>c</i></sub>", full = i18n_r()$t("Single group contribution to critical volume")),
      "Sum_Vc_cont" = list(short = "Σ<i>V</i><sub><i>c</i></sub>", full = i18n_r()$t("Cumulative contribution (group × occurrences)")),

      # HSP columns
      "dD_cont" = list(short = "δ<sub>D</sub>", full = i18n_r()$t("Single group contribution to dispersion parameter")),
      "Sum_dD_cont" = list(short = "Σδ<sub>D</sub>", full = i18n_r()$t("Cumulative contribution (group × occurrences)")),
      "dP_cont" = list(short = "δ<sub>P</sub>", full = i18n_r()$t("Single group contribution to polarity parameter")),
      "Sum_dP_cont" = list(short = "Σδ<sub>P</sub>", full = i18n_r()$t("Cumulative contribution (group × occurrences)")),
      "dHB_cont" = list(short = "δ<sub>HB</sub>", full = i18n_r()$t("Single group contribution to hydrogen bonding parameter")),
      "Sum_dHB_cont" = list(short = "Σδ<sub>HB</sub>", full = i18n_r()$t("Cumulative contribution (group × occurrences)")),
      "dP_low_cont" = list(short = "δ<sub>P,low</sub>", full = i18n_r()$t("Single group contribution to polarity parameter (low)")),
      "Sum_dP_low_cont" = list(short = "Σδ<sub>P,low</sub>", full = i18n_r()$t("Cumulative contribution (group × occurrences)")),
      "dHB_low_cont" = list(short = "δ<sub>HB,low</sub>", full = i18n_r()$t("Single group contribution to hydrogen bonding parameter (low)")),
      "Sum_dHB_low_cont" = list(short = "Σδ<sub>HB,low</sub>", full = i18n_r()$t("Cumulative contribution (group × occurrences)")),

      # VDW columns
      "VDW_cont" = list(short = i18n_r()$t("VDW"), full = i18n_r()$t("Single group contribution to Van der Waals volume")),
      "Sum_VDW_cont" = list(short = paste0("Σ", i18n_r()$t("VDW")), full = i18n_r()$t("Cumulative contribution (group × occurrences)")),

      # Chrastil columns (k = density exponent, A = temperature coeff., B = intercept)
      "k_cont" = list(short = "<i>k</i>", full = i18n_r()$t("Single group contribution to Chrastil k")),
      "Sum_k_cont" = list(short = "Σ<i>k</i>", full = i18n_r()$t("Cumulative contribution (group × occurrences)")),
      "A_cont" = list(short = "<i>A</i>", full = i18n_r()$t("Single group contribution to Chrastil A")),
      "Sum_A_cont" = list(short = "Σ<i>A</i>", full = i18n_r()$t("Cumulative contribution (group × occurrences)")),
      "B_cont" = list(short = "<i>B</i>", full = i18n_r()$t("Single group contribution to Chrastil B")),
      "Sum_B_cont" = list(short = "Σ<i>B</i>", full = i18n_r()$t("Cumulative contribution (group × occurrences)"))
    )

    # Display names for section headers (use plain strings, will wrap with HTML when rendering)
    display_names_translations <- c(
      "Boiling Point (Tb) Contributions" = paste0(i18n_r()$t("Boiling Point"), " (<i>T</i><sub><i>b</i></sub>) ", i18n_r()$t("Contributions")),
      "Critical Parameters Contributions" = i18n_r()$t("Critical Parameters Contributions"),
      "Hansen Solubility Parameters (HSP) Contributions" = i18n_r()$t("Hansen Solubility Parameters (HSP) Contributions"),
      "Van der Waals Volume Contributions" = i18n_r()$t("Van der Waals Volume Contributions"),
      "Chrastil Solubility Parameters Contributions" = i18n_r()$t("Chrastil Solubility Parameters Contributions")
    )

    display_names <- c(
      "Tb" = "Boiling Point (Tb) Contributions",
      "Critical" = "Critical Parameters Contributions",
      "HSP" = "Hansen Solubility Parameters (HSP) Contributions",
      "VDW" = "Van der Waals Volume Contributions",
      "Chrastil" = "Chrastil Solubility Parameters Contributions"
    )

    # Keep only the human-readable columns per parameter type. The Müller-engine results table carries
    # internal columns (SMARTS, smarts, pr, id, name, Tf_cont, the dP_low/dHB_low variants, ...) that
    # cluttered the display; show just Group/ID/Type/Occurrences + the contribution & Σ columns relevant
    # to THIS parameter — restoring the simpler table the module used to show.
    param_cont_keep <- list(
      Tb = "Tb", Critical = c("Tc", "Pc", "Vc"), HSP = c("dD", "dP", "dHB"), VDW = "VDW", Chrastil = c("k", "A", "B"))
    clean_cols <- function(df, param_name) {
      pref <- param_cont_keep[[param_name]]
      keep <- c("Group", "ID", "Type", "Occurrences")
      if (!is.null(pref)) keep <- c(keep, paste0(pref, "_cont"), paste0("Sum_", pref, "_cont"))
      df[, intersect(keep, names(df)), drop = FALSE]
    }

    cmaps <- tryCatch(group_colmaps(), error = function(e) list())

    # Generate a list of data tables, one for each contribution type
    table_outputs <- lapply(names(contribs_list), function(param_name) {
      df <- contribs_list[[param_name]]

      # Skip if NULL or empty (VDW may not exist for some molecules)
      if (is.null(df) || nrow(df) == 0) {
        return(NULL)
      }
      # Capture the raw per-row ID + Type (group ORDER) BEFORE clean_cols drops columns — needed to look up
      # each row's highlight colour, which is keyed by (order, ID) to match the decomposition image.
      raw_id   <- if ("ID" %in% names(df)) as.character(df$ID) else rep(NA_character_, nrow(df))
      raw_type <- if ("Type" %in% names(df)) df$Type else rep(1L, nrow(df))
      df <- clean_cols(df, param_name)
      # Colour-code the Group column with a swatch matching the group's highlight in the decomposition image
      # (same scheme as Process Simulation). Groups with no spatial highlight get a blank spacer for alignment.
      cmp <- cmaps[[param_name]]
      if ("Group" %in% names(df)) {
        sw <- vapply(seq_len(nrow(df)), function(i) {
          col <- if (!is.null(cmp)) solchar_row_color(cmp, raw_id[i], raw_type[i]) else ""
          if (nzchar(col)) sprintf("<span style='display:inline-block;width:12px;height:12px;border:1px solid #999;border-radius:3px;background:%s;margin-right:7px;vertical-align:middle;'></span>", col)
          else "<span style='display:inline-block;width:12px;height:12px;margin-right:7px;vertical-align:middle;'></span>"
        }, character(1))
        df$Group <- paste0(sw, htmltools::htmlEscape(as.character(df$Group)))
      }

      # Get actual column names from dataframe
      actual_cols <- colnames(df)

      # Map column names to short and full translations
      cols_short <- sapply(actual_cols, function(col) {
        if (!is.null(col_name_mapping[[col]])) {
          col_name_mapping[[col]]$short
        } else {
          col  # Fallback to original name if no mapping
        }
      })

      cols_full <- sapply(actual_cols, function(col) {
        if (!is.null(col_name_mapping[[col]])) {
          col_name_mapping[[col]]$full
        } else {
          col  # Fallback to original name if no mapping
        }
      })

      # Create tooltip headers
      header_html <- sprintf("<span title='%s'>%s</span>", cols_full, cols_short)

      output_id <- paste0("group_contrib_", param_name)
      output[[output_id]] <- DT::renderDataTable({
        # Trim trailing zeros on every numeric column (integers show as integers, 65.0000 -> 65,
        # 23.5800 -> 23.58) instead of the fixed 4-decimal formatRound, which looked cluttered.
        num_idx0 <- which(vapply(df, is.numeric, logical(1))) - 1L   # 0-based numeric column indices

        DT::datatable(
          df,
          options = list(
            scrollX = TRUE,
            pageLength = 10,
            dom = "Bfrtip",
            columnDefs = trim_zeros_columndefs(num_idx0, digits = 4),
            buttons = list(
              list(
                extend = "copy",
                text = i18n_r()$t("Copy"),
                titleAttr = i18n_r()$t("Copy"),
                action = copy_button_no_popup(
                  copy_label = i18n_r()$t("Copy"),
                  copied_label = i18n_r()$t("Copied!")
                )
              ),
              list(extend = "csv", filename = generate_filename_with_timestamp(paste0("supercrit_contrib_", param_name))),
              list(extend = "excel", filename = generate_filename_with_timestamp(paste0("supercrit_contrib_", param_name))),
              list(extend = "pdf", filename = generate_filename_with_timestamp(paste0("supercrit_contrib_", param_name)))
            ),
            language = tablang()
          ),
          extensions = "Buttons",
          colnames = header_html,
          escape = FALSE,
          rownames = FALSE
        )
      })

      # Get translated display name
      display_key <- display_names[param_name]
      translated_name <- display_names_translations[display_key]
      if (is.na(translated_name)) translated_name <- param_name

      # Small icon next to the title that opens the group-decomposition image (reuses the existing per-property
      # visualization modal). Shown only where a decomposition image exists (Tb / Critical / HSP) — mirrors the
      # "image icon beside the table title" pattern from Components & Parameters.
      vr <- sfe_rv$gcm_results$visres[[param_name]]
      # NB: .solchar_vis_trigger is a NAMED VECTOR — [[absent]] ERRORS; guard with %in% (VDW/Chrastil have no entry).
      trig <- if (param_name %in% names(.solchar_vis_trigger)) .solchar_vis_trigger[[param_name]] else NULL
      img_icon <- if (!is.null(trig) && is.list(vr) && length(vr))
        actionButton(ns(trig), label = NULL, icon = icon("magnifying-glass-plus"), class = "btn-info btn-xs",
          style = "color:white;", title = i18n_r()$t("Show group decomposition")) else NULL

      tagList(
        div(style = "display:flex; align-items:center; gap:8px; margin-top:12px;",
          h4(HTML(translated_name), style = "margin:0;"), img_icon),
        DT::dataTableOutput(ns(output_id))
      )
    })

    # Remove NULL entries (from skipped tables)
    table_outputs <- Filter(Negate(is.null), table_outputs)

    # Return all tables wrapped in a tagList
    do.call(tagList, table_outputs)
  })



  # Render solute details
  output$solute_details <- renderUI({
    req(sfe_rv$gcm_results)

    ids <- sfe_rv$gcm_results$solute_data$IDs
    synonyms <- sfe_rv$gcm_results$solute_data$Synonyms
    pares <- sfe_rv$gcm_results$pares
    chr <- sfe_rv$gcm_results$chrastil               # NULL unless the Chrastil estimate was requested
    ph  <- sfe_rv$gcm_results$phase                  # NULL unless Tm/Hvap were requested (manual or ML)
    
    # Build name display with synonyms
    display_name <- ids[["Name"]]
    if (!is.null(synonyms) && length(synonyms) > 0) {
      # Take first 3 synonyms for display
      syns_to_show <- head(synonyms, 3)
      synonyms_text <- paste(syns_to_show, collapse = ", ")
      name_display <- tagList(
        tags$span(display_name),
        tags$br(),
        tags$small(
          class = "text-muted",
          style = "font-style: italic;",
          paste0(i18n_r()$t("Names from CAS: "), synonyms_text)
        )
      )
    } else {
      name_display <- display_name
    }

    # ---- Property cards (Phase / Critical / Other / HSP), laid out two-per-row; a row's columns
    #      share the width (1->12, 2->6). Chrastil is hidden from Solute Characterization. ----
    finok <- function(x) isTRUE(is.finite(suppressWarnings(as.numeric(x)[1])))
    # Small "DB" badge shown to the LEFT of a property label when that value came from the
    # experimental database (item 5). db_src is captured at characterization time.
    db_keys <- sfe_rv$gcm_results$db_src %||% character(0)
    dbb <- function(key) if (key %in% db_keys)
      sprintf("<span title='%s' style='display:inline-block;background:#16a085;color:#fff;padding:0 5px;border-radius:8px;font-size:9px;font-weight:700;vertical-align:middle;margin-right:5px;'>DB</span>",
              i18n_r()$t("Value from the experimental database")) else ""
    prow  <- function(lab, val) tags$tr(tags$td(tags$strong(HTML(lab))), tags$td(val))
    prop_card <- function(color, ic, title, rows, head_bg, footer = NULL) tagList(
      h4(style = sprintf("color:%s; border-bottom:2px solid %s; padding-bottom:5px; margin-bottom:15px;", color, color),
         icon(ic), " ", title),
      tags$table(class = "table table-striped table-hover table-bordered table-sm",
        style = "margin-bottom:8px; table-layout:fixed; width:100%;",
        tags$thead(tags$tr(
          tags$th(style = sprintf("width:60%%; background-color:%s;", head_bg), i18n_r()$t("Parameter")),
          tags$th(style = sprintf("width:40%%; background-color:%s;", head_bg), i18n_r()$t("Value")))),
        tags$tbody(rows)),
      footer)

    tb_ok <- finok(pares$Critical["Tb"]); tm_ok <- !is.null(ph) && finok(ph$Tm); hv_ok <- !is.null(ph) && finok(ph$Hvap)
    crit_ok <- any(vapply(c("Tc", "Pc", "Vc"), function(p) finok(pares$Critical[p]), logical(1)))
    vdw_ok  <- !is.null(pares$VDW)
    hsp_ok  <- any(vapply(c("dD", "dP", "dHB"), function(p) finok(pares$HSP[p]), logical(1)))

    # Phase-Transition Properties: Tb (moved from Critical) + Tm + Hvap (no source disambiguation).
    # "Sublimes" flag: when a (real) boiling point is below the melting point, the compound sublimes
    # at atmospheric pressure and the tabulated Tb is really a sublimation temperature (e.g. caffeine).
    tb_val <- suppressWarnings(as.numeric(pares$Critical["Tb"])[1])
    tm_val <- if (!is.null(ph)) suppressWarnings(as.numeric(ph$Tm)[1]) else NA_real_
    sublimes <- tb_ok && tm_ok && isTRUE(is.finite(tb_val)) && isTRUE(is.finite(tm_val)) && tb_val < tm_val
    subl_badge <- if (sublimes)
      HTML(sprintf(" <span title='%s' style='display:inline-block;background:#d35400;color:#fff;padding:0 6px;border-radius:8px;font-size:9px;font-weight:700;vertical-align:middle;margin-left:6px;'>%s</span>",
                   i18n_r()$t("Boiling point is below the melting point, so this compound sublimes at atmospheric pressure — the tabulated boiling point is a sublimation temperature."),
                   i18n_r()$t("SUBLIMES"))) else NULL
    phase_card <- if (tb_ok || tm_ok || hv_ok) prop_card("#d35400", "icicles", i18n_r()$t("Phase-Transition Properties"), tagList(
      if (tb_ok) prow(paste0(dbb("Tb"), "<i>T</i><sub><i>b</i></sub> (", i18n_r()$t("Boiling Point"), "):"), list(ood_cell(pares$Critical["Tb"], i18n_r()$t("K")), subl_badge)),
      if (tm_ok) prow(paste0(dbb("Tm"), "<i>T</i><sub><i>m</i></sub> (", i18n_r()$t("Melting Point"), "):"), list(round(ph$Tm, 2), " ", tags$em(class = "text-muted", i18n_r()$t("K")))),
      if (hv_ok) prow(paste0(dbb("Hvap"), "Δ<i>H</i><sub>vap</sub>:"), list(round(ph$Hvap, 2), " ", tags$em(class = "text-muted", i18n_r()$t("kJ/mol"))))
    ), "#fdebd0",
    footer = if (sublimes) tags$small(class = "text-muted", style = "font-style:italic; display:block; margin-top:2px;",
      icon("snowflake"), " ", i18n_r()$t("Sublimes: the boiling point lies below the melting point (sublimation temperature at 1 atm).")) else NULL)

    crit_card <- if (crit_ok) prop_card("#f39c12", "temperature-high", i18n_r()$t("Critical Parameters"), tagList(
      prow(paste0(dbb("Tc"), "<i>T</i><sub><i>c</i></sub> (", i18n_r()$t("Critical Temperature"), "):"), ood_cell(pares$Critical["Tc"], i18n_r()$t("K"))),
      prow(paste0(dbb("Pc"), "<i>P</i><sub><i>c</i></sub> (", i18n_r()$t("Critical Pressure"), "):"), ood_cell(pares$Critical["Pc"], i18n_r()$t("bar"))),
      prow(paste0(dbb("Vc"), "<i>V</i><sub><i>c</i></sub> (", i18n_r()$t("Critical Volume"), "):"), ood_cell(pares$Critical["Vc"], i18n_r()$t("mL/mol")))
    ), "#fff3e0")

    # Other Parameters: Van der Waals volume (ML solubility row added in a later batch).
    # Other Parameters card: Van der Waals volume (the Makarov ML scCO₂-solubility row was removed).
    other_card <- if (vdw_ok) prop_card("#00a65a", "cube", i18n_r()$t("Other Parameters"), tagList(
      prow(paste0(i18n_r()$t("VDW Volume"), ":"), list(fmt_sci(pares$VDW), " ", tags$em(class = "text-muted", i18n_r()$t("L/mol"))))
    ), "#edfaf4")

    hsp_total <- sqrt(sum(suppressWarnings(as.numeric(pares$HSP[c("dD", "dP", "dHB")]))^2, na.rm = TRUE))
    hsp_card <- if (hsp_ok) prop_card("#605ca8", "water", i18n_r()$t("Hansen Solubility Parameters"), tagList(
      prow(paste0(dbb("dD"), "δ<sub>D</sub> (", i18n_r()$t("Dispersion"), "):"), ood_cell(pares$HSP["dD"], i18n_r()$t("√MPa"))),
      prow(paste0(dbb("dP"), "δ<sub>P</sub> (", i18n_r()$t("Polarity"), "):"), ood_cell(pares$HSP["dP"], i18n_r()$t("√MPa"))),
      prow(paste0(dbb("dHB"), "δ<sub>HB</sub> (", i18n_r()$t("Hydrogen Bonding"), "):"), ood_cell(pares$HSP["dHB"], i18n_r()$t("√MPa"))),
      tags$tr(style = "border-top:2px solid #605ca8;",
        tags$td(tags$strong(HTML(paste0("δ<sub>T</sub> (", i18n_r()$t("Total"), "):")))),
        tags$td(list(round(hsp_total, 2), " ", tags$em(class = "text-muted", i18n_r()$t("√MPa")))))
    ), "#f3f2f7")

    rT <- sfe_rv$gcm_results$sol_ref$T; rT <- if (length(rT) && isTRUE(is.finite(rT))) rT else 40
    rP <- sfe_rv$gcm_results$sol_ref$P; rP <- if (length(rP) && isTRUE(is.finite(rP))) rP else 200
    chr_card <- if (!is.null(chr)) {
      chr_mw  <- suppressWarnings(as.numeric(sfe_rv$solute_data$IDs["MW"])[1])
      chr_sol <- tryCatch(chrastil_solubility(chr$k, chr$A, chr$B, rT, rP,
                   mw = if (isTRUE(is.finite(chr_mw))) chr_mw else NULL), error = function(e) NULL)
      prop_card("#16a085", "tint", i18n_r()$t("Chrastil Solubility Parameters"), tagList(
        prow(paste0(dbb("Chr"), "<i>k</i> (", i18n_r()$t("Density exponent"), "):"), round(chr$k, 3)),
        prow(paste0(dbb("Chr"), "<i>A</i> (", i18n_r()$t("Temperature coeff."), "):"), list(round(chr$A, 2), " ", tags$em(class = "text-muted", i18n_r()$t("K")))),
        prow(paste0(dbb("Chr"), "<i>B</i> (", i18n_r()$t("Intercept"), "):"), round(chr$B, 3)),
        if (!is.null(chr_sol)) tags$tr(style = "border-top:2px solid #16a085;",
          tags$td(tags$strong(HTML(paste0(i18n_r()$t("Solubility in CO₂"), "<sup style='color:#d9534f;'>*</sup>:")))), tags$td(uiOutput(ns("chr_sol_value"))))
      ), "#e8f8f5",
      footer = tagList(
        tags$small(class = "text-muted", style = "font-style:italic;",
          HTML(paste0("<span style='color:#d9534f;'>*</span> ",
          if (isTRUE(is.finite(chr$aard_pct))) sprintf(i18n_r()$t("mol/mol coefficients; solubility at %g °C, %g bar; AARD ~%d%%"), rT, rP, round(chr$aard_pct))
          else sprintf(i18n_r()$t("mol/mol coefficients (user-provided); solubility shown at %g °C, %g bar."), rT, rP)))),
        if (is.list(chr$domain) && isFALSE(chr$domain$in_domain))
          div(class = "alert alert-warning", style = "margin-top:10px; padding:8px 12px; font-size:0.9em;",
            icon("triangle-exclamation"), tags$strong(" ", i18n_r()$t("Out of applicability domain")), tags$br(),
            i18n_r()$t("This solute is outside the Chrastil GCM's applicability domain, so the k/A/B estimate (and any solubility derived from it) is unreliable:"),
            tags$ul(style = "margin-bottom:0; margin-top:4px;", lapply(chr$domain$reasons, function(r) tags$li(r))))))
    } else NULL

    # Property cards flow two-per-row; chr_card appears only when Chrastil k/A/B were entered
    # manually or loaded from the database (no GCM estimate). other_card is NOT in this grid — it is
    # embedded directly below the Molecular Descriptors table (see desc_block) since VDW volume and
    # scCO₂ solubility are molecule-level descriptors.
    cards <- Filter(Negate(is.null), list(phase_card, crit_card, hsp_card, chr_card))
    lay  <- function(cards) { if (!length(cards)) return(list())
      rows <- split(cards, (seq_along(cards) - 1L) %/% 2L)
      lapply(unname(rows), function(rc) { w <- 12L %/% length(rc)
        do.call(fluidRow, lapply(rc, function(cc) column(width = w, cc))) }) }

    vr_av <- sfe_rv$gcm_results$visres
    has_vis <- (!is.null(vr_av$Tb) || !is.null(vr_av$Critical) || !is.null(vr_av$HSP) ||
                !is.null(sfe_rv$gcm_results$chrastil$visual))
    # table-layout:auto + a shrink-to-fit first column (white-space:nowrap; width:1%) so the Property
    # column takes the least width that avoids wrapping and the Value column absorbs the rest. Because
    # solute_details re-renders on language change (it reads i18n_r()), the browser recomputes the
    # width for the new labels automatically — no JS, no fixed widths. See .mol-desc-table CSS (UI).
    desc_table <-
          tags$table(
            class = "table table-striped table-hover table-bordered table-sm mol-desc-table",
            style = "margin-bottom: 0; width: 100%; height: 100%;",
            tags$thead(
              tags$tr(
                tags$th(style = "background-color: #f4f4f4; white-space: nowrap; width: 1%;", i18n_r()$t("Property")),
                tags$th(style = "background-color: #f4f4f4; width: 100%;", i18n_r()$t("Value"))
              )
            ),
            tags$tbody(
              tags$tr(
                tags$td(tags$strong(i18n_r()$t("Display Name:"))),
                tags$td(name_display)
              ),
              tags$tr(
                tags$td(tags$strong(i18n_r()$t("CAS:"))),
                tags$td(ids[["CAS"]])
              ),
              tags$tr(
                tags$td(tags$strong(i18n_r()$t("Molecular Formula:"))),
                tags$td(ids[["MF"]])
              ),
              tags$tr(
                tags$td(tags$strong(i18n_r()$t("Molecular Weight:"))),
                tags$td(round(as.numeric(ids[["MW"]]), 3), " ", tags$em(class = "text-muted", i18n_r()$t("g/mol")))
              ),
              tags$tr(
                tags$td(tags$strong(i18n_r()$t("Atom Count:"))),
                tags$td(ids[["Atom_Count"]])
              ),
              tags$tr(
                tags$td(tags$strong(i18n_r()$t("Hydrogen Count:"))),
                tags$td(ids[["H_Count"]])
              ),
              tags$tr(
                tags$td(tags$strong(i18n_r()$t("Aromaticity:"))),
                tags$td(ids[["Aromaticity"]])
              ),
              tags$tr(
                tags$td(tags$strong(i18n_r()$t("SMILES:"))),
                tags$td(style = "word-break: break-all; font-family: monospace; font-size: 0.9em;", ids[["SMILES"]])
              ),
              tags$tr(
                tags$td(tags$strong(i18n_r()$t("InChI:"))),
                tags$td(style = "word-break: break-all; font-family: monospace; font-size: 0.85em;", ids[["InChI"]])
              ),
              tags$tr(
                tags$td(tags$strong(i18n_r()$t("InChIKey:"))),
                tags$td(style = "font-family: monospace; font-size: 0.9em;", ids[["InChIKey"]])
              )
            )
          )

    desc_h4 <- function(txt) h4(
      style = "color: #3c8dbc; border-bottom: 2px solid #3c8dbc; padding-bottom: 5px; margin-bottom: 15px;",
      icon("flask"), " ", txt)
    # Live contextual visualization to the LEFT of the descriptor table (item 8). A flexbox with
    # align-items:stretch makes the two columns equal height; the table wrapper flex-grows and the
    # table is height:100%, so the descriptor table stretches to match the (taller) plot column
    # instead of ending short above it (item 4). When no visualization was computed, the table
    # spans full width.
    # "Other Parameters" (VDW volume + ML scCO₂ solubility) is embedded directly below the Molecular
    # Descriptors table when computed, rather than as a separate card in the property grid. The
    # Descriptors table flex-grows and Other Parameters is pinned at its natural height at the bottom,
    # so together the two tables span the full column height and align with the visualization.
    other_block <- if (!is.null(other_card)) div(style = "flex:0 0 auto; margin-top:15px;", other_card) else NULL
    desc_block <- if (has_vis)
      div(style = "display:flex; gap:15px; align-items:stretch; flex-wrap:wrap;",
        div(style = "flex:0 0 40%; max-width:40%; min-width:280px; display:flex; flex-direction:column;",
          desc_h4(i18n_r()$t("Group Contribution Visualization")),
          uiOutput(ns("context_vis_ui"), style = "flex:1 1 auto; min-height:0; display:flex; flex-direction:column;")),
        div(style = "flex:1 1 0; min-width:300px; display:flex; flex-direction:column;",
          desc_h4(i18n_r()$t("Molecular Descriptors")),
          # position:relative anchors the group-legend overlay directly over the descriptors table.
          div(style = "flex:1 1 auto; display:flex; flex-direction:column; position:relative;",
            uiOutput(ns("group_legend_overlay")), desc_table),
          other_block))
    else
      fluidRow(column(width = 12,
        desc_h4(i18n_r()$t("Molecular Descriptors")),
        desc_table,
        other_block))

    do.call(tagList, c(list(desc_block), lay(cards)))
  })




  # Observe "Characterize Solute (GCM)" button click
  observeEvent(input$characterize_solute, {
    req(input$cas_input) # Display Name + SMILES are optional (auto by default via their toggles)

    # Pass a Name only when the user explicitly entered one; in auto mode omit it so
    # mol_find resolves the real chemical name from the CAS (e.g. 58-08-2 -> "Caffeine").
    solute_input <- c(CAS = input$cas_input)
    if (smiles_enabled() && nzchar(input$smiles_input %||% ""))
      solute_input <- c(SMILES = input$smiles_input, solute_input)
    if (isTRUE(name_enabled()) && nzchar(trimws(input$name_input %||% "")))
      solute_input <- c(solute_input, Name = trimws(input$name_input))

    withProgress(message = i18n$t("Characterizing Solute (GCM)..."), {
      sfe_rv$gcm_results <- tryCatch(
        {
          # First get solute data
          sfe_rv$solute_data <- mol_find(solute_input)

          # Then characterize with GCM
          # Build named gorder vector from per-method inputs
          gorder_vec <- c()
          if (!is.null(input$gcm_tb) && !input$gcm_tb %in% c("manual", "none", "")) {
            val <- if (!is.null(input$gcm_tb_gorder)) input$gcm_tb_gorder else 0
            gorder_vec[input$gcm_tb] <- val
          }
          if (!is.null(input$gcm_crit) && !input$gcm_crit %in% c("manual", "none", "")) {
            val <- if (!is.null(input$gcm_crit_gorder)) input$gcm_crit_gorder else 0
            gorder_vec[input$gcm_crit] <- val
          }
          if (!is.null(input$gcm_hsp) && !input$gcm_hsp %in% c("manual", "none", "")) {
            val <- if (!is.null(input$gcm_hsp_gorder)) input$gcm_hsp_gorder else 0
            gorder_vec[input$gcm_hsp] <- val
          }
          # Use 0 (all orders) as fallback if no methods selected
          if (length(gorder_vec) == 0) gorder_vec <- 0

          params <- list(
            solute = solute_input,
            simplicity = "auto",   # engine ignores this; input removed (was redundant)
            gorder = gorder_vec,
            hlight = TRUE, # Always highlight for visualization
            silent = TRUE
          )

          # Only add parameters if they have valid values (not "none" or empty)
          if (!is.na(input$gcm_tb) && input$gcm_tb != "none" && input$gcm_tb != "") {
            if (input$gcm_tb == "manual") {
              # Use the manual input value (numeric)
              req(input$tb_manual_input)
              params$tb <- as.numeric(input$tb_manual_input)
            } else {
              params$tb <- input$gcm_tb
            }
          }
          crit_manual <- identical(input$gcm_crit, "manual")
          hsp_manual  <- identical(input$gcm_hsp, "manual")
          if (!is.na(input$gcm_crit) && input$gcm_crit != "none" && input$gcm_crit != "") {
            # manual -> pass the numeric Tc so est_gcm builds pares$Critical (Pc/Vc injected below).
            params$crit <- if (crit_manual) { req(input$tc_manual_input); as.numeric(input$tc_manual_input) } else input$gcm_crit
          }
          if (!is.na(input$gcm_hsp) && input$gcm_hsp != "none" && input$gcm_hsp != "" && !hsp_manual) {
            params$hsp <- input$gcm_hsp
          }
          if (!is.na(input$gcm_vdw) && input$gcm_vdw != "none" && input$gcm_vdw != "") {
            params$vdw <- input$gcm_vdw
          }

          gcm_output <- do.call(est_gcm, params)

          # Manual Critical / HSP: inject the user-entered values (the Load buttons fill these from the DB).
          # No silent DB override any more — the user explicitly loads values into these manual fields.
          if (crit_manual && !is.null(gcm_output$pares$Critical)) {
            tbv <- unname(gcm_output$pares$Critical[1])   # est_gcm puts Tb first (numeric crit -> c(Tb, Tc))
            gcm_output$pares$Critical <- c(Tb = tbv,
              Tc = suppressWarnings(as.numeric(input$tc_manual_input)),
              Pc = suppressWarnings(as.numeric(input$pc_manual_input)),
              Vc = suppressWarnings(as.numeric(input$vc_manual_input)))
          }
          if (hsp_manual) {
            gcm_output$pares$HSP <- c(dD = suppressWarnings(as.numeric(input$dd_manual_input)),
              dP = suppressWarnings(as.numeric(input$dp_manual_input)),
              dHB = suppressWarnings(as.numeric(input$dhb_manual_input)))
          }

          # Phase-transition properties: melting point (Tm) and enthalpy of vaporization (Hvap),
          # either entered manually or predicted from SMILES with the native-R ML models (gcm_mp /
          # gcm_hvap). Stored in gcm_output$phase and rendered in the results cards.
          # SMILES for the ML predictors (Tm / Hvap / solubility). Prefer a manually-entered SMILES,
          # but fall back to the one mol_find resolved from the CAS/name so DB-loaded compounds (where
          # "specify SMILES" is unchecked) still get ML estimates.
          smi_for_ml <- if (smiles_enabled() && nzchar(input$smiles_input %||% "")) input$smiles_input
                        else suppressWarnings(unname(sfe_rv$solute_data$IDs["SMILES"]))
          if (length(smi_for_ml) != 1 || is.na(smi_for_ml) || !nzchar(smi_for_ml)) smi_for_ml <- NA_character_
          ph <- list(Tm = NA_real_, Hvap = NA_real_, Tm_src = NA_character_, Hvap_src = NA_character_)
          if (identical(input$gcm_tm, "manual")) {
            ph$Tm <- suppressWarnings(as.numeric(input$tm_manual_input)); ph$Tm_src <- "manual"
          } else if (identical(input$gcm_tm, "gcm_mp") && !is.na(smi_for_ml) && nzchar(smi_for_ml)) {
            ph$Tm <- suppressWarnings(tryCatch(unname(gcm_mp(smi_for_ml)[1]), error = function(e) NA_real_))
            ph$Tm_src <- "gcm_mp"
          }
          if (identical(input$gcm_hvap, "manual")) {
            ph$Hvap <- suppressWarnings(as.numeric(input$hvap_manual_input)); ph$Hvap_src <- "manual"
          } else if (identical(input$gcm_hvap, "gcm_hvap") && !is.na(smi_for_ml) && nzchar(smi_for_ml)) {
            ph$Hvap <- suppressWarnings(tryCatch(unname(gcm_hvap(smi_for_ml)[1]), error = function(e) NA_real_))
            ph$Hvap_src <- "gcm_hvap"
          } else if (identical(input$gcm_hvap, "gcm_hvap_chickos") && !is.na(smi_for_ml) && nzchar(smi_for_ml)) {
            ph$Hvap <- suppressWarnings(tryCatch(gcm_hvap_chickos(smi_for_ml)[1], error = function(e) NA_real_))
            ph$Hvap_src <- "gcm_hvap_chickos"
          }
          # Enthalpy of fusion (ΔHfus) + the derived ΔS_tpce and liquid/solid Cp (Chickos-Acree GCM). The
          # GCM route needs the melting point resolved above; manual entry is independent of it.
          ph$Hfus <- NA_real_; ph$Hfus_src <- NA_character_; ph$Hfus_reliable <- NA
          ph$dS_tpce <- NA_real_; ph$Cp_l <- NA_real_; ph$Cp_s <- NA_real_
          if (identical(input$gcm_hfus, "manual")) {
            ph$Hfus <- suppressWarnings(as.numeric(input$hfus_manual_input)); ph$Hfus_src <- "manual"
          } else if (identical(input$gcm_hfus, "gcm_fusion") && isTRUE(is.finite(ph$Tm)) &&
                     !is.na(smi_for_ml) && nzchar(smi_for_ml)) {
            fus <- tryCatch(gcm_fusion(smi_for_ml, ph$Tm), error = function(e) NULL)
            if (!is.null(fus) && isTRUE(is.finite(fus$dHfus_Tm))) {
              ph$Hfus <- fus$dHfus_Tm; ph$Hfus_src <- "gcm_fusion"; ph$Hfus_reliable <- isTRUE(fus$reliable)
              ph$Hfus_tier <- fus$reliability %||% (if (isTRUE(fus$reliable)) "fair" else "poor")
              ph$dS_tpce <- fus$dS_tpce; ph$Cp_l <- fus$Cp_l; ph$Cp_s <- fus$Cp_s
            }
          }
          gcm_output$phase <- ph

          # Database provenance (item 5): a property is "from the database" when its method is set to
          # 'manual' AND the manual value equals the matched DB record (as loaded, i.e. rounded). This
          # detects Load-from-DB values without flagging user-typed numbers.
          gcm_output$db_src <- local({
            dbm <- tryCatch(db_match(), error = function(e) NULL)
            keys <- character(0)
            if (!is.null(dbm)) {
              eq <- function(inp, dbv, prec) { iv <- suppressWarnings(as.numeric(inp))
                !is.null(dbv) && isTRUE(is.finite(dbv)) && isTRUE(is.finite(iv)) && abs(iv - round(dbv, prec)) < 1e-6 }
              if (identical(input$gcm_tb, "manual")   && eq(input$tb_manual_input,  dbm$Tb,  2)) keys <- c(keys, "Tb")
              if (identical(input$gcm_crit, "manual")) {
                if (eq(input$tc_manual_input, dbm$Tc, 2)) keys <- c(keys, "Tc")
                if (eq(input$pc_manual_input, dbm$Pc, 2)) keys <- c(keys, "Pc")
                if (eq(input$vc_manual_input, dbm$Vc, 1)) keys <- c(keys, "Vc")
              }
              if (identical(input$gcm_hsp, "manual")) {
                if (eq(input$dd_manual_input,  dbm$dD,  2)) keys <- c(keys, "dD")
                if (eq(input$dp_manual_input,  dbm$dP,  2)) keys <- c(keys, "dP")
                if (eq(input$dhb_manual_input, dbm$dHB, 2)) keys <- c(keys, "dHB")
              }
              if (identical(input$gcm_tm, "manual")   && eq(input$tm_manual_input,   dbm$Tm,   2)) keys <- c(keys, "Tm")
              if (identical(input$gcm_hvap, "manual") && eq(input$hvap_manual_input, dbm$Hvap, 2)) keys <- c(keys, "Hvap")
              if (identical(input$gcm_chrastil, "manual") && eq(input$chrastil_k_manual, dbm$Chr_k, 4)) keys <- c(keys, "Chr")
            }
            keys
          })

          # Reference T/P (captured at characterization time) for the displayed scCO₂ solubility.
          refT <- ref_T(); refP <- ref_P()
          gcm_output$sol_ref <- list(T = refT, P = refP)
          # (Makarov ML scCO₂-solubility prediction removed — unreliable. scCO₂ solubility is now shown only
          # from experimental Chrastil coefficients below.)

          # Chrastil k/A/B: experimental coefficients entered manually or loaded from the database.
          # The predictive García-Sámano GCM estimate was removed (unreliable). Failure here must
          # not discard the Tb/Tc/HSP results, so the path is guarded.
          if (identical(input$gcm_chrastil, "manual")) {
            mk <- suppressWarnings(as.numeric(input$chrastil_k_manual))
            mA <- suppressWarnings(as.numeric(input$chrastil_A_manual))
            mB <- suppressWarnings(as.numeric(input$chrastil_B_manual))
            if (all(is.finite(c(mk, mA, mB)))) {
              # User-provided / DB-loaded coefficients: no AARD / applicability domain / group visual.
              gcm_output$chrastil <- list(k = mk, A = mA, B = mB, basis = "mol/mol",
                                          aard_pct = NA_real_, source = "manual")
            } else {
              showNotification(i18n$t("Enter all three Chrastil coefficients (k, A, B) for Manual Input."), type = "warning")
            }
          }

          gcm_output
        },
        error = function(e) {
          showNotification(paste(i18n$t("Error during GCM characterization:"), e$message), type = "error")
          return(NULL)
        }
      )

      # Save the characterization results after successful processing
      if (!is.null(sfe_rv$gcm_results)) {
        # Save the calculation to session storage for misc_comp and misc_opt modules
        # FIFO limit: Maximum 10 calculations (including seed data)
        if (length(sfe_rv$saved_calculations) >= 10) {
          # Find oldest calculation based on timestamp
          timestamps <- sapply(sfe_rv$saved_calculations, function(x) as.POSIXct(x$timestamp))
          oldest_idx <- which.min(timestamps)

          # Remove oldest and compact list
          sfe_rv$saved_calculations[[oldest_idx]] <- NULL
          sfe_rv$saved_calculations <- Filter(Negate(is.null), sfe_rv$saved_calculations)
        }

        # Display name = compound name + date-time only. Estimation methods are intentionally NOT
        # listed (they conflated "manual" across properties and lost which property it referred to).
        display_name <- sprintf("%s | %s", eff_name(), format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

        # Bundle the SMILES with the saved calc so downstream modules and the ML predictors (Tm/Hvap, and
        # the EoS solubility engines) can use it: prefer the user's manual SMILES, else the one mol_find
        # resolved from the CAS/name. This
        # is why the Solute Characterization output always carries a SMILES even when the toggle is "Auto".
        smi_manual <- if (smiles_enabled() && nzchar(trimws(input$smiles_input %||% ""))) trimws(input$smiles_input) else ""
        smi_resolved <- suppressWarnings(unname(sfe_rv$solute_data$IDs["SMILES"]))
        smi_save <- if (nzchar(smi_manual)) smi_manual
                    else if (length(smi_resolved) == 1 && !is.na(smi_resolved) && nzchar(smi_resolved)) smi_resolved
                    else ""

        # Save to session-based storage (only characterize parameters for misc_comp/misc_opt usage)
        new_id <- length(sfe_rv$saved_calculations) + 1
        sfe_rv$saved_calculations[[new_id]] <- list(
          id = new_id,
          display_name = display_name,
          timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          source_module = "Solute Characterization",
          parameters = list(
            cas_input = input$cas_input,
            name_input = eff_name(),
            smiles_input = smi_save,
            specify_smiles = nzchar(smi_save),   # saved-calc schema key kept for cross-module compatibility
            gcm_tb = if (!is.na(input$gcm_tb) && input$gcm_tb != "none") input$gcm_tb else "none",
            tb_manual_input = if (input$gcm_tb == "manual") input$tb_manual_input else NA,
            gcm_crit = if (!is.na(input$gcm_crit) && input$gcm_crit != "none") input$gcm_crit else "none",
            gcm_hsp = if (!is.na(input$gcm_hsp) && input$gcm_hsp != "none") input$gcm_hsp else "none",
            # Persist manual critical-T and HSP VALUES so downstream modules (esp. Co-Solvent Optimization
            # HSP mode) apply them directly via the experimental-override path, instead of passing a
            # "manual" method string to the GCM engine (which crashes with "get1index").
            db_experimental = identical(input$gcm_crit, "manual") || identical(input$gcm_hsp, "manual"),
            exp_crit_value = if (identical(input$gcm_crit, "manual")) suppressWarnings(as.numeric(input$tc_manual_input)) else NA_real_,
            exp_hsp_values = if (identical(input$gcm_hsp, "manual")) suppressWarnings(as.numeric(c(input$dd_manual_input, input$dp_manual_input, input$dhb_manual_input))) else NA_real_,
            gcm_tb_gorder = if (!is.null(input$gcm_tb_gorder)) input$gcm_tb_gorder else 0,
            gcm_crit_gorder = if (!is.null(input$gcm_crit_gorder)) input$gcm_crit_gorder else 0,
            gcm_hsp_gorder = if (!is.null(input$gcm_hsp_gorder)) input$gcm_hsp_gorder else 0,
            gcm_chrastil = !identical(input$gcm_chrastil, "none") && !is.null(sfe_rv$gcm_results$chrastil),
            # Resolved phase-transition properties (manual / experimental / ML), for the Co-Solvent
            # Solubility mode and the Separator-Cascade module. NA when not requested.
            melting_point = if (!is.null(sfe_rv$gcm_results$phase)) sfe_rv$gcm_results$phase$Tm else NA_real_,
            enthalpy_vap  = if (!is.null(sfe_rv$gcm_results$phase)) sfe_rv$gcm_results$phase$Hvap else NA_real_,
            # Chrastil coefficients (mol/mol + g/L) for import into the Separator-Cascade module
            chrastil = local({
              ch <- sfe_rv$gcm_results$chrastil
              if (is.null(ch)) NULL else list(
                k = ch$k, A = ch$A, B = ch$B, basis = "mol/mol",
                k_gL = ch$k_gL, A_gL = ch$A_gL, B_gL = ch$B_gL,
                mw = suppressWarnings(as.numeric(sfe_rv$solute_data$IDs["MW"])),
                smiles = unname(sfe_rv$solute_data$IDs["SMILES"]),
                aard_pct = ch$aard_pct
              )
            })
          )
        )

        # Sort calculations by timestamp (newest first) for dropdown display
        timestamps <- sapply(sfe_rv$saved_calculations, function(x) as.POSIXct(x$timestamp))
        sorted_indices <- order(timestamps, decreasing = TRUE)
        sfe_rv$saved_calculations <- sfe_rv$saved_calculations[sorted_indices]

        showNotification(i18n$t("GCM characterization completed and saved to session."), type = "message")
        updateTabsetPanel(session, "sol_char_tabs", selected = "solute_results")
      }
    })
  })

  # (The old "Specify SMILES" checkbox that show/hid smiles_div is replaced by the SMILES field's
  #  activation toggle; nothing to observe here anymore.)

  # Toggle manual Tb input visibility (replaces Group Order field)
  observeEvent(input$gcm_tb,
    {
      if (input$gcm_tb == "manual") {
        shinyjs::show("tb_manual_div")
        shinyjs::hide("gcm_tb_gorder_wrapper")
      } else {
        shinyjs::hide("tb_manual_div")
        shinyjs::show("gcm_tb_gorder_wrapper")
      }
    },
    ignoreInit = FALSE
  )

  # Toggle the manual Critical (Tc/Pc/Vc) and HSP (dD/dP/dHB) field rows when "Manual Input" is chosen.
  observeEvent(input$gcm_crit, {
    if (identical(input$gcm_crit, "manual")) { shinyjs::show("crit_manual_div"); shinyjs::hide("gcm_crit_gorder_wrapper") }
    else { shinyjs::hide("crit_manual_div"); shinyjs::show("gcm_crit_gorder_wrapper") }
  }, ignoreNULL = FALSE, ignoreInit = FALSE)
  observeEvent(input$gcm_hsp, {
    if (identical(input$gcm_hsp, "manual")) { shinyjs::show("hsp_manual_div"); shinyjs::hide("gcm_hsp_gorder_wrapper") }
    else { shinyjs::hide("hsp_manual_div"); shinyjs::show("gcm_hsp_gorder_wrapper") }
  }, ignoreNULL = FALSE, ignoreInit = FALSE)
  # Tm / Hvap have no group-order field; just reveal the manual numeric box when "Manual Input" is chosen.
  observeEvent(input$gcm_tm, {
    if (identical(input$gcm_tm, "manual")) shinyjs::show("tm_manual_div") else shinyjs::hide("tm_manual_div")
  }, ignoreNULL = FALSE, ignoreInit = FALSE)
  observeEvent(input$gcm_hvap, {
    if (identical(input$gcm_hvap, "manual")) shinyjs::show("hvap_manual_div") else shinyjs::hide("hvap_manual_div")
  }, ignoreNULL = FALSE, ignoreInit = FALSE)
  observeEvent(input$gcm_hfus, {
    if (identical(input$gcm_hfus, "manual")) shinyjs::show("hfus_manual_div") else shinyjs::hide("hfus_manual_div")
  }, ignoreNULL = FALSE, ignoreInit = FALSE)
  # GATING: the fusion-enthalpy field is disabled while the Melting Point Method is "None" (ΔHfus needs
  # Tfus). Selecting any other Tm source enables it; if it was on the GCM route with no Tm, reset to None.
  observeEvent(input$gcm_tm, {
    if (identical(input$gcm_tm %||% "none", "none")) {
      if (identical(input$gcm_hfus, "gcm_fusion")) updateSelectInput(session, "gcm_hfus", selected = "none")
      shinyjs::disable("gcm_hfus"); shinyjs::hide("hfus_manual_div")
    } else shinyjs::enable("gcm_hfus")
  }, ignoreNULL = FALSE, ignoreInit = FALSE)
  observeEvent(input$gcm_chrastil, {
    if (identical(input$gcm_chrastil, "manual")) shinyjs::show("chrastil_manual_div") else shinyjs::hide("chrastil_manual_div")
  }, ignoreNULL = FALSE, ignoreInit = FALSE)

  # GCM Comparison: Maximum Group Order with range badge
  output$gcm_comp_gorder_ui <- renderUI({
    # Labelled cap applied to every method in the comparison (each method uses min(cap, its own max order)).
    ord_labels <- c(i18n_r()$t("1st order only"), i18n_r()$t("Up to 2nd order"), i18n_r()$t("Up to 3rd order"))
    choices <- setNames(c("0", "1", "2", "3"), c(i18n_r()$t("All available orders"), ord_labels))
    # Method selection (item 13): pick which estimators enter the comparison table (it is otherwise
    # very crowded). Default = all. Labels use the same full method names as elsewhere.
    md <- val_method_disp()
    # CHRASTIL is intentionally excluded — Chrastil estimation is hidden from Solute Characterization,
    # so it is neither offered in the picker nor passed to compare_gcm() (which drops unlisted methods).
    all_methods <- c("JR", "JR_corr", "SB", "SB_corr", "NL04", "NL07", "NL07_robust",
                     "HKR_STW", "HKR_SIM", "SP08", "SP12", "BND", "SLON", "ZHAO")
    meth_choices <- stats::setNames(all_methods, ifelse(is.na(md[all_methods]), all_methods, unname(md[all_methods])))
    tags$div(
      tags$label(
        i18n_r()$t("Maximum Group Order"),
        input_help(i18n_r()$t("Maximum order of structural groups to include for every method in the comparison. \"All available orders\" uses each method's full set; lower settings fall back to a simpler model. First-order groups are always used."),
                   title = i18n_r()$t("Maximum Group Order"), buttonLabel = i18n_r()$t("OK")),
        class = "control-label",
        style = "display: flex; align-items: center; gap: 6px; width: 100%;"
      ),
      selectInput(ns("gcm_comp_gorder"), label = NULL, choices = choices, selected = "0", width = "100%"),
      tags$div(style = "margin-top: 10px;",
        tags$label(
          i18n_r()$t("Methods to Compare"),
          input_help(i18n_r()$t("Select which estimation methods to include in the comparison table. By default all methods are compared; deselecting some makes the table less crowded."),
                     title = i18n_r()$t("Methods to Compare"), buttonLabel = i18n_r()$t("OK")),
          class = "control-label",
          style = "display: flex; align-items: center; gap: 6px; width: 100%;"
        ),
        shinyWidgets::pickerInput(ns("gcm_comp_methods"), label = NULL,
          choices = meth_choices, selected = all_methods, multiple = TRUE, width = "100%",
          options = list(`actions-box` = TRUE, `selected-text-format` = "count > 3",
                         countSelectedText = i18n_r()$t("{0} methods")))
      )
    )
  })

  # Observe "Compare GCM" button click
  observeEvent(input$compare_gcm, {
    req(input$cas_input) # Display Name + SMILES are optional (auto by default via their toggles)

    # Pass a Name only when the user explicitly entered one; in auto mode omit it so
    # mol_find resolves the real chemical name from the CAS (e.g. 58-08-2 -> "Caffeine").
    solute_input <- c(CAS = input$cas_input)
    if (smiles_enabled() && nzchar(input$smiles_input %||% ""))
      solute_input <- c(SMILES = input$smiles_input, solute_input)
    if (isTRUE(name_enabled()) && nzchar(trimws(input$name_input %||% "")))
      solute_input <- c(solute_input, Name = trimws(input$name_input))

    withProgress(message = i18n$t("Comparing GCM..."), {
      sfe_rv$gcm_comparison <- tryCatch(
        {
          compare_gcm(
            solute = solute_input,
            gorder = input$gcm_comp_gorder,
            simplicity = "auto",   # engine ignores this; input removed (was redundant)
            methods = { ms <- input$gcm_comp_methods; if (length(ms)) ms else NULL }
          )
        },
        error = function(e) {
          showNotification(paste(i18n$t("Error during GCM comparison:"), e$message), type = "error")
          return(NULL)
        }
      )

      if (!is.null(sfe_rv$gcm_comparison)) {
        updateTabsetPanel(session, "sol_char_tabs", selected = "gcm_comparison")
        showNotification(i18n$t("GCM comparison completed"), type = "message")
      } else {
        showNotification(i18n$t("No comparison results"), type = "warning")
      }
    })
  })

  # ===== EXPORT RESULTS (PDF / CSV) — the caret dropdown beside "Save to Disk" =====
  # Reuse the shared sol_char_export.R builders so the PDF and CSV always agree; both localise via the
  # current-language translator (i18n_r()). Filenames carry the solute name + timestamp. Guarded: any error
  # (incl. no results yet) writes a minimal placeholder so the browser download never dead-ends.
  solchar_export_basename <- function() {
    nm <- tryCatch(unname(sfe_rv$gcm_results$solute_data$IDs["Name"]), error = function(e) NA)
    nm <- if (length(nm) == 1 && !is.na(nm) && nzchar(nm)) gsub("[^a-zA-Z0-9_.-]", "_", nm) else "solute"
    generate_filename_with_timestamp(paste0(nm, "_solute_char"))
  }
  output$export_pdf <- downloadHandler(
    filename = function() paste0(solchar_export_basename(), ".pdf"),
    content = function(file) {
      tr <- tryCatch(i18n_r(), error = function(e) i18n)
      tryCatch({
        export <- solchar_export_tables(sfe_rv$gcm_results, tr)
        if (is.null(export)) stop(tr$t("Run a characterization first."))
        logo   <- if (file.exists("www/altraflora-15-min.png")) "www/altraflora-15-min.png" else NULL
        appver <- tryCatch(paste0("supeRcrit v", as.character(utils::packageVersion("supeRcrit"))), error = function(e) "supeRcrit")
        build_solchar_pdf_report(export, tr, file, company = appver, logo = logo)
      }, error = function(e) {
        showNotification(sprintf(tr$t("PDF export failed: %s"), conditionMessage(e)), type = "error", duration = 8)
        grDevices::cairo_pdf(file); grid::grid.text(tr$t("Report generation failed.")); grDevices::dev.off()
      })
    }
  )
  output$export_csv <- downloadHandler(
    filename = function() paste0(solchar_export_basename(), ".csv"),
    content = function(file) {
      tr <- tryCatch(i18n_r(), error = function(e) i18n)
      tryCatch({
        export <- solchar_export_tables(sfe_rv$gcm_results, tr)
        if (is.null(export)) stop(tr$t("Run a characterization first."))
        solchar_write_csv(export, tr, file)
      }, error = function(e) {
        showNotification(sprintf(tr$t("CSV export failed: %s"), conditionMessage(e)), type = "error", duration = 8)
        writeLines(tr$t("Run a characterization first."), file)
      })
    }
  )

  # ===== SAVE TO DISK FUNCTIONALITY =====

  # Set up shinyFiles directory chooser with system volumes
  volumes <- c(
    Home = Sys.getenv("HOME"),
    supercrit_get_volumes()
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
  
  # Show the "Save Calculation to Disk" modal (the split button's main action; only shown once results
  # exist, since it persists a characterized solute — see the UI comment).
  open_save_modal <- function() {
    # Check if there's a recent calculation to save
    if (length(sfe_rv$saved_calculations) == 0) {
      showNotification(i18n$t("No calculations to save. Run a calculation first."), type = "warning")
      return()
    }
    
    # Get the most recent calculation
    recent_calc <- sfe_rv$saved_calculations[[1]]
    
    # Get default directory
    default_dir <- supercrit_user_dir(file.path("user-settings", "sfe_gcm_calculations"))

    # Show save modal
    showModal(modalDialog(
      title = i18n$t("Save Calculation to Disk"),
      size = "m",
      
      # Show which calculation will be saved
      div(
        class = "alert alert-info",
        icon("info-circle"),
        strong(i18n$t("Saving:")), " ", recent_calc$display_name
      ),
      
      # Filename input
      textInput(
        ns("save_modal-filename"),
        i18n$t("Filename:"),
        value = gsub("[^a-zA-Z0-9_.-]", "_", recent_calc$parameters$name_input),
        width = "100%"
      ),
      
      # Directory selection
      div(
        style = "display: flex; gap: 10px; align-items: flex-end;",
        div(
          style = "flex-grow: 1;",
          textInput(
            ns("save_modal-save_directory"),
            i18n$t("Save Directory"),
            value = default_dir,
            width = "100%"
          )
        ),
        div(
          style = "margin-bottom: 15px;",
          shinyFiles::shinyDirButton(
            ns("save_modal-browse_directory"),
            label = i18n$t("Browse..."),
            title = i18n$t("Select Save Directory"),
            class = "btn-default"
          )
        )
      ),
      tags$small(
        class = "text-muted",
        i18n$t("Leave empty to use the default package directory.")
      ),
      
      # Filename preview
      uiOutput(ns("save_modal-filename_preview")),
      
      footer = tagList(
        modalButton(i18n$t("Cancel")),
        actionButton(ns("save_modal-confirm_save"), i18n$t("Save"), class = "btn btn-success", style = "color: white;")
      )
    ))
  }
  observeEvent(input$save_to_disk, open_save_modal())

  # Render filename preview
  output[["save_modal-filename_preview"]] <- renderUI({
    req(input[["save_modal-filename"]])
    filename <- input[["save_modal-filename"]]
    
    # Sanitize filename
    sanitized <- gsub("[^a-zA-Z0-9_.-]", "_", filename)
    if (nchar(sanitized) == 0) sanitized <- "untitled"
    
    # Add timestamp
    timestamp_str <- format(Sys.time(), "%d-%m-%Y_%Hhrs-%Mmin-%Ssec")
    final_filename <- paste0(sanitized, "_", timestamp_str, ".json")
    
    div(
      style = "margin-top: 10px;",
      class = "alert alert-secondary",
      strong(i18n$t("Final filename:")), " ", final_filename
    )
  })
  
  # Confirm save to disk
  observeEvent(input[["save_modal-confirm_save"]], {
    req(input[["save_modal-filename"]])
    
    removeModal()
    
    tryCatch({
      # Get the most recent calculation
      recent_calc <- sfe_rv$saved_calculations[[1]]
      
      # Sanitize filename
      filename <- gsub("[^a-zA-Z0-9_.-]", "_", input[["save_modal-filename"]])
      if (nchar(filename) == 0) filename <- "untitled"
      
      # Add timestamp to filename
      timestamp_str <- format(Sys.time(), "%d-%m-%Y_%Hhrs-%Mmin-%Ssec")
      final_filename <- paste0(filename, "_", timestamp_str, ".json")
      
      # Determine save directory
      save_dir <- input[["save_modal-save_directory"]]
      if (is.null(save_dir) || nchar(trimws(save_dir)) == 0) {
        save_dir <- supercrit_user_dir(file.path("user-settings", "sfe_gcm_calculations"))
      }
      
      # Ensure directory exists
      if (!dir.exists(save_dir)) {
        dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
      }
      
      # Full file path
      filepath <- file.path(save_dir, final_filename)
      
      # Prepare data to save (same structure as session storage)
      save_data <- list(
        display_name = recent_calc$display_name,
        timestamp = recent_calc$timestamp,
        source_module = recent_calc$source_module,
        parameters = recent_calc$parameters,
        metadata = list(
          created_date = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          app_version = "0.9.0"
        )
      )
      
      # Save as JSON
      jsonlite::write_json(save_data, filepath, auto_unbox = TRUE, pretty = TRUE)
      
      showNotification(
        paste(i18n$t("Calculation saved to:"), basename(filepath)),
        type = "message",
        duration = 5
      )
      
    }, error = function(e) {
      showNotification(
        paste(i18n$t("Error saving file:"), e$message),
        type = "error"
      )
    })
  })

  # Observe "Reset" button click
  observeEvent(input$reset, {
    # Reset solute information inputs
    updateTextInput(session, "cas_input", value = defaults$cas_input)
    updateTextInput(session, "name_input", value = defaults$name_input); name_enabled(FALSE)
    updateTextInput(session, "smiles_input", value = defaults$smiles_input); smiles_enabled(FALSE)

    # Reset GCM method selection inputs
    updateSelectInput(session, "gcm_tb", selected = defaults$gcm_tb %||% "manual")
    updateNumericInput(session, "tb_manual_input", value = 298.15)
    shinyjs::hide("tb_manual_div")
    updateSelectInput(session, "gcm_crit", selected = defaults$gcm_crit %||% "none")
    updateSelectInput(session, "gcm_hsp", selected = defaults$gcm_hsp %||% "none")
    for (mid in c("tc_manual_input", "pc_manual_input", "vc_manual_input", "dd_manual_input", "dp_manual_input", "dhb_manual_input"))
      updateNumericInput(session, mid, value = NA)
    shinyjs::hide("crit_manual_div"); shinyjs::hide("hsp_manual_div")
    updateSelectInput(session, "gcm_vdw", selected = "none")
    updateSelectInput(session, "gcm_tb_gorder", selected = "0")
    updateSelectInput(session, "gcm_crit_gorder", selected = "0")
    updateSelectInput(session, "gcm_hsp_gorder", selected = "0")

    # Reset GCM comparison inputs
    updateSelectInput(session, "gcm_comp_gorder", selected = "0")

    # Clear reactive values
    sfe_rv$gcm_results <- NULL
    sfe_rv$gcm_comparison <- NULL
    sfe_rv$solute_data <- NULL

    # Show success notification
    showNotification(i18n$t("Parameters reset"), type = "message")
  })

  # Render HELP output
  output$sfe_sol_char_HELP <- renderUI({
    create_help_modal(i18n_r, "sfe_sol_char_help_en")
  })

  # Render accordion expand/collapse button with translated tooltips
  output$accordion_toggle_btn <- renderUI({
    create_accordion_toggle_btn(ns, i18n_r, "accordion", start_expanded = FALSE)
  })
  # Ensure UI outputs in collapsed accordions are rendered immediately
  outputOptions(output, "gcm_tb_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "gcm_crit_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "gcm_hsp_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "gcm_vdw_ui", suspendWhenHidden = FALSE)

  # suspendWhenHidden for renderUI outputs (auto-added)
  outputOptions(output, "accordion_toggle_btn", suspendWhenHidden = FALSE)
  outputOptions(output, "cas_input_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "gcm_comp_gorder_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "gcm_comparison_title", suspendWhenHidden = FALSE)
  outputOptions(output, "group_contributions_table", suspendWhenHidden = FALSE)
  outputOptions(output, "name_input_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "sfe_sol_char_HELP", suspendWhenHidden = FALSE)
  outputOptions(output, "smiles_input_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "solute_details", suspendWhenHidden = FALSE)
  outputOptions(output, "tb_manual_input_ui", suspendWhenHidden = FALSE)

}
