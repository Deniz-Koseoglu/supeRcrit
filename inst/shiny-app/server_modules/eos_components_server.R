# Process Simulation - Components & Parameters Server Module
# Builds a GC-EOS / GCA-EOS component from a CAS number (mol_find) or a SMILES
# string: SMARTS group decomposition (gceos_decompose), GCM-estimated or manual
# critical temperature / boiling point (est_gcm), critical-diameter regression
# (gceos_fit_dc) and pure vapour pressure at one or more temperatures
# (gceos_psat). On a successful analysis the component is added to the session
# (eos_rv$saved_components) automatically; "Save to Disk" persists it as JSON.
eos_components_server <- function(input, output, session, defaults, i18n, tablang, eos_rv) {

  ns <- session$ns
  i18n_r <- reactive({ i18n })

  # ---- constants --------------------------------------------------------
  TC_RANGE <- c(200, 1200); TB_RANGE <- c(200, 900); TEVAL_RANGE <- c(150, 1000)
  gcm_max_order <- c("manual" = 0, "JR" = 1, "JR_corr" = 1, "SB" = 1, "SB_corr" = 1,
                     "NL04" = 2, "NL07" = 2, "NL07_robust" = 2, "HKR_STW" = 3, "HKR_SIM" = 3)
  # Full GCM method names (shown in results), matching the method dropdowns.
  method_labels <- c(JR = "Joback-Reid", JR_corr = "Joback-Reid corrected",
                     SB = "Stein-Brown", SB_corr = "Stein-Brown corrected", NL04 = "Nannoolal 2004",
                     NL07 = "Nannoolal 2007", NL07_robust = "Nannoolal 2007 robust",
                     HKR_STW = "Hukkerikar-Stepwise", HKR_SIM = "Hukkerikar-Simultaneous")
  # The molecule-highlight palette, in the SAME order plot_gcm() assigns it (the
  # java.awt.Color order), so the i-th decomposed group's colour in the table
  # matches the i-th highlighted group in the fragmentation image.
  hl_colors <- c("#C0C0C0", "#FF0000", "#00FF00", "#00FFFF", "#FFAFAF", "#FFC800", "#FFFF00",
                 "#FF00FF", "#0000FF", "#990099", "#336600", "#660000", "#99FFCC", "#808080", "#404040")

  # Example presets only seed the input fields; dc is always re-regressed.
  presets <- list(
    `Limonene`          = list(smiles = "CC1=CCC(CC1)C(=C)C",  cas = "5989-27-5", Tc = 660.0, Tb = 449.65, model = "gceos"),
    `Squalene`          = list(smiles = "CC(=CCCC(=CCCC(=CCCC=C(C)CCC=C(C)CCC=C(C)CCC=C(C)C)C)C)C",
                               cas = "111-02-4", Tc = 795.0, Tb = 694.55, model = "gceos"),
    `Linalool`          = list(smiles = "CC(C)=CCCC(C)(O)C=C", cas = "78-70-6", Tc = 630.5, Tb = 471.15, model = "gceos"),
    `n-Decane`          = list(smiles = "CCCCCCCCCC",          cas = "124-18-5", Tc = 617.7, Tb = 447.30, model = "gceos"),
    `Lauric acid (FFA)` = list(smiles = "CCCCCCCCCCCC(=O)O",   cas = "143-07-7", Tc = 743.0, Tb = 571.85, model = "gcaeos")
  )

  # ---- reactive state ---------------------------------------------------
  eos_state <- reactiveValues(result = NULL)
  name_on   <- reactiveVal(FALSE)
  smiles_on <- reactiveVal(FALSE)

  output$has_results <- reactive({ !is.null(eos_state$result) })
  outputOptions(output, "has_results", suspendWhenHidden = FALSE)

  model_pretty <- function(m) switch(m %||% "gceos",
    gcaeos = "GCA-EOS", `gceos-fae` = "GC-EOS (esters)", `gceos-hmw` = "GC-EOS (high-MW)",
    diaz2005 = "GC-EOS (Diaz terpenes)", ganan2011 = "GC-EOS (Ganan terpenes)", "GC-EOS")
  picker_models <- c("diaz2005", "ganan2011")   # use a supported-molecule picker, not decomposition
  picker_pretty <- function(s) sub("^([a-z])", "\\U\\1", s, perl = TRUE)   # capitalise first letter for display
  fmt_num <- function(x, digits = 4) {
    if (is.null(x) || length(x) == 0 || is.na(x)) return("—")
    s <- formatC(round(x, digits), format = "f", digits = digits)
    if (grepl(".", s, fixed = TRUE)) s <- sub("\\.?0+$", "", s)   # strip trailing zeros only past the decimal (else "100" -> "1")
    s
  }
  html_table <- function(df) {
    tags$table(class = "table table-striped table-condensed", style = "width:100%; margin-bottom:8px;",
      tags$thead(tags$tr(lapply(names(df), function(h) tags$th(h)))),
      tags$tbody(lapply(seq_len(nrow(df)), function(i)
        tags$tr(lapply(df[i, ], function(v) tags$td(as.character(v)))))))
  }
  toggle_btn <- function(id, is_on, title) {
    actionButton(ns(id), label = NULL, icon = icon(if (is_on) "toggle-on" else "toggle-off"),
      class = paste("btn btn-xs", if (is_on) "btn-primary" else "btn-default"),
      style = "padding: 2px 6px; font-size: 14px;", title = title)
  }
  # Labelled input: a flex header with [bold label + help] left and an optional
  # toggle (a real button, OUTSIDE the <label>, so its click fires) right.
  field_block <- function(label_html, help_tag, input_tag, toggle_id = NULL, toggle_on = FALSE, toggle_title = "") {
    # Fixed header height so columns with a toggle button line up exactly with
    # columns without one (otherwise the taller button pushes the input down).
    hdr <- div(style = "display:flex; align-items:center; justify-content:space-between; height:28px; margin-bottom:3px;",
      tags$span(class = "control-label", style = "margin-bottom:0; font-weight:700;", HTML(label_html), help_tag),
      if (!is.null(toggle_id)) toggle_btn(toggle_id, toggle_on, toggle_title) else tags$span())
    div(class = "form-group", style = "margin-bottom:10px;", hdr, input_tag)
  }
  # numeric input with a range badge recoloured live (runjs) to red when out of
  # range. The HTML label text is wrapped in ONE span so its internal spaces
  # survive the flex layout (separate HTML nodes would each become flex items).
  badge_num <- function(id_in, badge_id, label_html, helptxt, value, lo, hi, step) {
    bad <- is.null(value) || is.na(value) || value < lo || value > hi
    tags$div(
      tags$label(class = "control-label", style = "display:flex; align-items:center; justify-content:space-between; width:100%;",
        tags$span(HTML(label_html), input_help(i18n_r()$t(helptxt), buttonLabel = i18n_r()$t("OK"))),
        tags$span(paste0(lo, "–", hi), id = ns(badge_id),
          style = sprintf("font-size:10px; padding:1px 5px; border-radius:3px; background-color:%s; color:white; font-weight:normal;",
                          if (bad) "#dc3545" else "#6c757d"))),
      numericInput(ns(id_in), label = NULL, value = value, min = lo, max = hi, step = step))
  }
  # Group-order selector: a labelled DROPDOWN ("All available orders" = 0, or a cap up to the method's max
  # order), matching the system used in Solute Characterization (the numeric+badge input was retired along
  # with the engine change). `badge_id` is unused now (kept for call-site compatibility).
  gorder_input <- function(id_in, badge_id, method) {
    max_ord <- gcm_max_order[method]; if (is.na(max_ord)) max_ord <- 0
    is_disabled <- max_ord <= 1
    ord_labels <- c(i18n_r()$t("1st order only"), i18n_r()$t("Up to 2nd order"), i18n_r()$t("Up to 3rd order"))
    if (is_disabled) { choices <- stats::setNames("1", i18n_r()$t("1st order only")); sel <- "1" }
    else { choices <- stats::setNames(c("0", as.character(seq_len(max_ord))),
                                      c(i18n_r()$t("All available orders"), ord_labels[seq_len(max_ord)])); sel <- "0" }
    sel_input <- selectInput(ns(id_in), NULL, choices = choices, selected = sel, width = "100%")
    if (is_disabled) sel_input <- shinyjs::disabled(sel_input)
    tags$div(
      tags$label(i18n_r()$t("Group Order:"),
        input_help(i18n_r()$t("Maximum order of structural groups to include. \"All available orders\" uses every order the method defines; a lower setting falls back to a simpler first-/second-order model. First-order groups are always used."),
                   title = i18n_r()$t("Group Order"), buttonLabel = i18n_r()$t("OK")),
        class = "control-label", style = "display:flex; align-items:center; gap:6px; width:100%;"),
      sel_input)
  }
  recolor <- function(badge_id, value, lo, hi) {
    bad <- is.null(value) || length(value) == 0 || any(is.na(value)) || any(value < lo) || any(value > hi)
    shinyjs::runjs(sprintf("$('#%s').css('background-color','%s');", ns(badge_id), if (bad) "#dc3545" else "#6c757d"))
  }

  # ---- model ------------------------------------------------------------
  output$model_ui <- renderUI({
    selectInput(ns("model"),
      tags$span(i18n_r()$t("EOS Model"),
        input_help(i18n_r()$t("GC-EOS for hydrocarbons / terpenes (deterpenation, squalene). GCA-EOS adds hydrogen-bonding association for fatty acids, water and alcohols. GC-EOS (esters) carries the fatty-acid-ester CO2 parameters and GC-EOS (high-MW) the Fornari high-molecular-weight set; both decompose like GC-EOS and matter only for downstream mixture (phase / column) work. Diaz 2005 and Ganan 2011 are molecule-specific terpene sets: pick a supported molecule (its published decomposition, Tc and dc load directly)."),
                   title = i18n_r()$t("EOS Model"), buttonLabel = i18n_r()$t("OK"))),
      choices = setNames(c("gceos", "gcaeos", "gceos-fae", "gceos-hmw", "diaz2005", "ganan2011"),
        c(i18n_r()$t("GC-EOS"), i18n_r()$t("GCA-EOS"), i18n_r()$t("GC-EOS (esters)"), i18n_r()$t("GC-EOS (high-MW)"),
          i18n_r()$t("Diaz 2005 (terpenes)"), i18n_r()$t("Ganan 2011 (terpenes)"))),
      selected = isolate(input$model) %||% (defaults$model %||% "gceos"))
  })
  outputOptions(output, "model_ui", suspendWhenHidden = FALSE)

  # ---- supported-molecule picker (diaz2005 / ganan2011 terpene sets) ---
  output$picker_ui <- renderUI({
    m <- input$model %||% "gceos"
    if (!m %in% picker_models) return(NULL)
    ps <- tryCatch(gceos_params(m), error = function(e) NULL)
    if (is.null(ps) || is.null(ps$decomp))
      return(div(class = "alert alert-warning", style = "padding:8px 10px; font-size:12px;",
        icon("triangle-exclamation"), " ",
        i18n_r()$t("This terpene set could not be loaded from the supeRcrit package - the loaded build is out of date. Reload it (pkgload::load_all) or reinstall, then restart the app.")))
    mols <- setdiff(rownames(ps$decomp), "CO2")
    tagList(
      selectInput(ns("picker_mol"),
        tags$span(i18n_r()$t("Molecule"),
          input_help(i18n_r()$t("Choose a molecule from this published terpene parameter set. Its group decomposition, critical temperature and critical diameter are loaded directly from the source paper - no structure decomposition is performed."),
                     title = i18n_r()$t("Supported molecule"), buttonLabel = i18n_r()$t("OK"))),
        choices = stats::setNames(mols, picker_pretty(mols)), selected = isolate(input$picker_mol) %||% mols[1]),
      p(style = "font-size:12px; color:#6c757d;",
        sprintf(i18n_r()$t("Groups, Tc and dc are loaded from the %s set."), model_pretty(m))))
  })
  outputOptions(output, "picker_ui", suspendWhenHidden = FALSE)

  # ---- identification: CAS | Display Name | SMILES ---------------------
  output$identification_ui <- renderUI({
    non <- name_on(); smon <- smiles_on()
    cur_cas    <- isolate(input$cas)    %||% (defaults$cas %||% "")
    cur_name   <- isolate(input$name)   %||% (defaults$name %||% "")
    cur_smiles <- isolate(input$smiles) %||% (defaults$smiles %||% "")
    cas_in <- textInput(ns("cas"), label = NULL, value = cur_cas, placeholder = "e.g. 5989-27-5")
    name_in <- textInput(ns("name"), label = NULL, value = cur_name, placeholder = i18n_r()$t("optional"))
    smiles_in <- textInput(ns("smiles"), label = NULL, value = cur_smiles, placeholder = "e.g. CC1=CCC(CC1)C(=C)C")
    if (!non)  name_in   <- shinyjs::disabled(name_in)
    if (!smon) smiles_in <- shinyjs::disabled(smiles_in)

    fluidRow(
      column(4, field_block(i18n_r()$t("CAS Number"),
        input_help(i18n_r()$t("CAS registry number. Used to retrieve the structure and SMILES, and required for GCM estimation of Tc / Tb."),
                   title = i18n_r()$t("CAS Number"), buttonLabel = i18n_r()$t("OK")), cas_in)),
      column(4, field_block(i18n_r()$t("Display Name"),
        input_help(i18n_r()$t("Optional custom display name for the component."),
                   title = i18n_r()$t("Display Name"), buttonLabel = i18n_r()$t("OK")), name_in,
        toggle_id = "name_toggle", toggle_on = non, toggle_title = i18n_r()$t("Add a custom name"))),
      column(4, field_block("SMILES",
        input_help(i18n_r()$t("Enter the SMILES string directly instead of looking it up by CAS. Needed only when both Tc and Tb are set manually."),
                   title = "SMILES", buttonLabel = i18n_r()$t("OK")), smiles_in,
        toggle_id = "smiles_toggle", toggle_on = smon, toggle_title = i18n_r()$t("Enter SMILES directly")))
    )
  })
  outputOptions(output, "identification_ui", suspendWhenHidden = FALSE)

  observeEvent(input$name_toggle,   { name_on(!name_on()) })
  observeEvent(input$smiles_toggle, { smiles_on(!smiles_on()) })

  # ---- Load Example modal ----------------------------------------------
  observeEvent(input$open_example_modal, {
    showModal(modalDialog(
      title = i18n$t("Load Example Component"), size = "s", easyClose = TRUE,
      selectInput(ns("example_pick"), i18n$t("Example molecule"),
        choices = names(presets), selected = isolate(input$example_pick) %||% names(presets)[1], width = "100%"),
      tags$p(style = "font-size: 12px; color: #6c757d;",
        i18n$t("The fields are filled with the example data and remain editable.")),
      footer = tagList(modalButton(i18n$t("Cancel")),
        actionButton(ns("confirm_example"), i18n$t("Load"), class = "btn-primary", style = "color: white;"))))
  })

  observeEvent(input$confirm_example, {
    p <- presets[[input$example_pick]]; if (is.null(p)) { removeModal(); return() }
    name_on(TRUE); smiles_on(TRUE)
    updateSelectInput(session, "model", selected = p$model)
    updateTextInput(session, "cas", value = p$cas)
    updateTextInput(session, "name", value = input$example_pick)
    updateTextInput(session, "smiles", value = p$smiles)
    updateSelectInput(session, "tc_method", selected = "manual")
    updateSelectInput(session, "tb_method", selected = "manual")
    updateNumericInput(session, "tc_manual", value = p$Tc)
    updateNumericInput(session, "tb_manual", value = p$Tb)
    removeModal()
    showNotification(sprintf(i18n$t("Loaded example: %s"), input$example_pick), type = "message", duration = 2)
  })

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
    name_on(TRUE); smiles_on(TRUE)
    updateTextInput(session, "cas",    value = if (!is.na(row$cas))    sub("\\|.*$", "", row$cas) else "")
    updateTextInput(session, "name",   value = if (!is.na(row$name))   row$name   else "")
    updateTextInput(session, "smiles", value = if (!is.na(row$smiles)) row$smiles else "")
    if (is.finite(row$Tb)) { updateSelectInput(session, "tb_method", selected = "manual"); updateNumericInput(session, "tb_manual", value = round(row$Tb, 2)) }
    if (is.finite(row$Tc)) { updateSelectInput(session, "tc_method", selected = "manual"); updateNumericInput(session, "tc_manual", value = round(row$Tc, 2)) }
    removeModal()
    showNotification(sprintf(i18n$t("Loaded '%s' from the database."), row$name), type = "message", duration = 3)
  })

  # ---- Add to Database: the form is embedded in the Load-from-Database modal; save in place + refresh. ----
  observeEvent(input$db_add_save, {
    if (isTRUE(exp_db_add_save(input, session, i18n, close = FALSE))) db_refresh(db_refresh() + 1)
  })
  # Edit / Delete of user-added DB rows (shared behaviour across the app; this table is unfiltered).
  exp_db_wire_edit_delete(input, output, session, i18n, refresh = function() db_refresh(db_refresh() + 1), db_fn = db_view)

  # ---- auto-notify: experimental data available for the manually-entered compound (Tb / Tc loadable) ----
  db_match <- reactive(exp_db_match(exp_db_all(),
    smiles = if (isTRUE(smiles_on())) input$smiles else NULL,
    cas = input$cas, name = if (isTRUE(name_on())) input$name else NULL))
  output$db_notify_ui <- renderUI({
    r <- db_match(); if (is.null(r)) return(NULL)
    bits <- exp_db_bits(r, c("Tb", "Tc")); if (!length(bits)) return(NULL)
    div(class = "alert alert-info", style = "margin-top:8px; padding:8px 12px; font-size:13px;",
      span(icon("circle-info"), " ",
        HTML(sprintf(i18n_r()$t("Experimental data available for this compound: %s."), paste(bits, collapse = ", ")))),
      div(style = "margin-top:6px; display:flex; gap:8px; flex-wrap:wrap;",
        if (is.finite(r$Tb)) actionButton(ns("notify_load_tb"), HTML(paste0(i18n_r()$t("Load"), " <i>T</i><sub>b</sub>")), class = "btn-info btn-xs", style = "color:white;"),
        if (is.finite(r$Tc)) actionButton(ns("notify_load_tc"), HTML(paste0(i18n_r()$t("Load"), " <i>T</i><sub>c</sub>")), class = "btn-info btn-xs", style = "color:white;")))
  })
  observeEvent(input$notify_load_tb, { r <- db_match(); req(r, is.finite(r$Tb))
    updateSelectInput(session, "tb_method", selected = "manual"); updateNumericInput(session, "tb_manual", value = round(r$Tb, 2))
    showNotification(i18n$t("Loaded experimental Tb."), type = "message", duration = 2) })
  observeEvent(input$notify_load_tc, { r <- db_match(); req(r, is.finite(r$Tc))
    updateSelectInput(session, "tc_method", selected = "manual"); updateNumericInput(session, "tc_manual", value = round(r$Tc, 2))
    showNotification(i18n$t("Loaded experimental Tc."), type = "message", duration = 2) })

  # ---- Tc / Tb method selectors + side input (gorder | manual) ---------
  output$tc_method_ui <- renderUI({
    selectInput(ns("tc_method"),
      tags$span(HTML(paste0(i18n_r()$t("Critical Temperature"), " <i>T</i><sub>c</sub>")),
        input_help(i18n_r()$t("Choose Manual Input to type a known Tc, or a Group Contribution Method to estimate it from the structure (requires a CAS number)."),
                   title = i18n_r()$t("Critical Temperature"), buttonLabel = i18n_r()$t("OK"))),
      choices = setNames(c("manual", "JR", "NL07", "NL07_robust", "HKR_STW", "HKR_SIM"),
        i18n_r()$t(c("Manual Input", "Joback-Reid", "Nannoolal 2007", "Nannoolal 2007 robust", "Hukkerikar-Stepwise", "Hukkerikar-Simultaneous"))),
      selected = isolate(input$tc_method) %||% "manual")
  })
  outputOptions(output, "tc_method_ui", suspendWhenHidden = FALSE)

  output$tb_method_ui <- renderUI({
    selectInput(ns("tb_method"),
      tags$span(HTML(paste0(i18n_r()$t("Boiling Point"), " <i>T</i><sub>b</sub>")),
        input_help(i18n_r()$t("Choose Manual Input to type a known normal boiling point, or a Group Contribution Method to estimate it (requires a CAS number). The critical diameter dc is regressed so the EOS reproduces Psat = 1 atm at Tb."),
                   title = i18n_r()$t("Boiling Point"), buttonLabel = i18n_r()$t("OK"))),
      choices = setNames(c("manual", "JR", "JR_corr", "SB", "SB_corr", "NL04", "HKR_STW", "HKR_SIM"),
        i18n_r()$t(c("Manual Input", "Joback-Reid", "Joback-Reid corrected", "Stein-Brown", "Stein-Brown corrected", "Nannoolal 2004", "Hukkerikar-Stepwise", "Hukkerikar-Simultaneous"))),
      selected = isolate(input$tb_method) %||% "manual")
  })
  outputOptions(output, "tb_method_ui", suspendWhenHidden = FALSE)

  output$tc_side_ui <- renderUI({
    m <- input$tc_method %||% "manual"
    if (m == "manual")
      badge_num("tc_manual", "tc_badge", "<i>T</i><sub>c</sub> (K)",
        "Critical temperature in Kelvin.", isolate(input$tc_manual) %||% NA_real_, TC_RANGE[1], TC_RANGE[2], 1)
    else gorder_input("tc_gorder", "tc_gorder_badge", m)
  })
  outputOptions(output, "tc_side_ui", suspendWhenHidden = FALSE)

  output$tb_side_ui <- renderUI({
    m <- input$tb_method %||% "manual"
    if (m == "manual")
      badge_num("tb_manual", "tb_badge", "<i>T</i><sub>b</sub> (K)",
        "Normal boiling point in Kelvin (Psat = 1 atm).", isolate(input$tb_manual) %||% NA_real_, TB_RANGE[1], TB_RANGE[2], 0.1)
    else gorder_input("tb_gorder", "tb_gorder_badge", m)
  })
  outputOptions(output, "tb_side_ui", suspendWhenHidden = FALSE)

  # ---- critical-diameter (dc) source -----------------------------------
  output$dc_method_ui <- renderUI({
    selectInput(ns("dc_method"),
      tags$span(HTML(paste0(i18n_r()$t("Critical Diameter"), " (<i>d</i><sub>c</sub>) ", i18n_r()$t("Calculation Method"))),
        input_help(i18n_r()$t("How the critical diameter dc is obtained. Boiling point: regress dc so the EOS gives Psat = 1 atm at Tb (needs Tb). Ester correlation: Espinosa Eq. 2-3 from the van der Waals volume (fatty-acid alkyl esters only). Binary VLE: fit dc to a single measured CO2 + solute bubble point."),
                   title = i18n_r()$t("Critical Diameter"), buttonLabel = i18n_r()$t("OK"))),
      # "ester" (Espinosa Eq. 2-3 ester correlation) is HIDDEN for now — too niche a use case. The handling
      # code (gceos_dc_ester, below) is kept intact; re-add "ester" / "Ester correlation (Eq. 2-3)" here to
      # restore the option.
      choices = setNames(c("boiling", "vle"),
        i18n_r()$t(c("Fit to boiling point", "Fit to a binary VLE point"))),
      selected = isolate(input$dc_method) %||% "boiling")
  })
  outputOptions(output, "dc_method_ui", suspendWhenHidden = FALSE)

  output$dc_vle_ui <- renderUI({
    if ((input$dc_method %||% "boiling") != "vle") return(NULL)
    fluidRow(
      column(4, numericInput(ns("dc_vle_T"),
        tags$span(HTML("<i>T</i> (K)"), input_help(i18n_r()$t("Temperature of the binary CO2 + solute VLE data point (K)."), buttonLabel = i18n_r()$t("OK"))),
        value = isolate(input$dc_vle_T) %||% 313.15, min = 250, max = 600, step = 1)),
      column(4, numericInput(ns("dc_vle_P"),
        tags$span(HTML("<i>P</i><sub>bub</sub> (bar)"), input_help(i18n_r()$t("Measured bubble pressure of the CO2 + solute mixture at that temperature and liquid composition (bar)."), buttonLabel = i18n_r()$t("OK"))),
        value = isolate(input$dc_vle_P) %||% 80, min = 1, max = 1000, step = 1)),
      column(4, numericInput(ns("dc_vle_x"),
        tags$span(HTML("<i>x</i><sub>solute</sub>"), input_help(i18n_r()$t("Liquid-phase mole fraction of the solute (the remainder is CO2), between 0 and 1."), buttonLabel = i18n_r()$t("OK"))),
        value = isolate(input$dc_vle_x) %||% 0.2, min = 0.001, max = 0.999, step = 0.01)))
  })
  outputOptions(output, "dc_vle_ui", suspendWhenHidden = FALSE)

  # ---- vapour-pressure evaluation temperatures (tag input) -------------
  output$teval_ui <- renderUI({
    cur <- isolate(input$teval) %||% (defaults$teval %||% c("350", "400", "450"))
    chip_render <- I(sprintf("{ item: function(item, escape) {
        var val = parseFloat(item.value);
        var oor = isNaN(val) || val < %d || val > %d;
        var style = oor ? 'background-color:#dc3545; border-color:#dc3545; color:white;' : '';
        return '<div class=\"item\" style=\"' + style + '\">' + escape(item.value) + '</div>';
      } }", TEVAL_RANGE[1], TEVAL_RANGE[2]))
    tags$div(
      tags$label(class = "control-label", style = "display:flex; align-items:center; justify-content:space-between; width:100%;",
        tags$span(HTML(paste0(i18n_r()$t("Evaluate"), " <i>P</i><sub>sat</sub> ", i18n_r()$t("at temperatures (K)"))),
          input_help(i18n_r()$t("One or more temperatures (K) at which to report the pure-component vapour pressure. Type a value and press Enter; up to 10 are allowed."),
                     title = i18n_r()$t("Evaluation Temperatures"), buttonLabel = i18n_r()$t("OK"))),
        tags$span(paste0(TEVAL_RANGE[1], "–", TEVAL_RANGE[2]), id = ns("teval_badge"),
          style = "font-size:10px; padding:1px 5px; border-radius:3px; background-color:#6c757d; color:white; font-weight:normal;")),
      selectizeInput(ns("teval"), label = NULL, choices = as.character(cur), selected = as.character(cur),
        multiple = TRUE, width = "100%",
        options = list(create = TRUE, persist = FALSE, maxItems = 10,
                       placeholder = i18n_r()$t("Type a temperature and press Enter"), render = chip_render)))
  })
  outputOptions(output, "teval_ui", suspendWhenHidden = FALSE)

  # ---- live range-badge recolouring ------------------------------------
  observeEvent(input$tc_manual, recolor("tc_badge", input$tc_manual, TC_RANGE[1], TC_RANGE[2]), ignoreInit = TRUE)
  observeEvent(input$tb_manual, recolor("tb_badge", input$tb_manual, TB_RANGE[1], TB_RANGE[2]), ignoreInit = TRUE)
  observeEvent(input$teval, {
    v <- suppressWarnings(as.numeric(input$teval))
    recolor("teval_badge", v, TEVAL_RANGE[1], TEVAL_RANGE[2])
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  # ---- range validation (blocks the run) -------------------------------
  validate_ranges <- function() {
    bad <- c()
    # Group order is now a bounded dropdown (always valid), so only the manual T inputs need range checks.
    if ((input$tc_method %||% "manual") == "manual") {
      v <- input$tc_manual
      if (is.null(v) || is.na(v) || v < TC_RANGE[1] || v > TC_RANGE[2]) bad <- c(bad, i18n$t("Critical Temperature"))
    }
    if ((input$tb_method %||% "manual") == "manual") {
      v <- input$tb_manual
      if (is.null(v) || is.na(v) || v < TB_RANGE[1] || v > TB_RANGE[2]) bad <- c(bad, i18n$t("Boiling Point"))
    }
    tev <- suppressWarnings(as.numeric(input$teval))
    if (length(tev) == 0 || any(is.na(tev)) || any(tev < TEVAL_RANGE[1] | tev > TEVAL_RANGE[2]))
      bad <- c(bad, i18n$t("Evaluation Temperatures"))
    if (length(bad) == 0) return(NULL)
    paste(i18n$t("Out of range:"), paste(bad, collapse = ", "))
  }

  # ---- analyze: picker mode (load a published terpene decomposition) ----
  analyze_picker <- function(model) {
    ps <- tryCatch(gceos_params(model), error = function(e) NULL)
    if (is.null(ps) || is.null(ps$decomp)) { showNotification(i18n$t("This terpene set could not be loaded from the supeRcrit package - the loaded build is out of date. Reload it (pkgload::load_all) or reinstall, then restart the app."), type = "error", duration = 8); return() }
    mols <- setdiff(rownames(ps$decomp), "CO2")
    mol <- input$picker_mol %||% mols[1]
    if (!mol %in% mols) { showNotification(i18n$t("Pick a supported molecule."), type = "error"); return() }
    tev <- suppressWarnings(as.numeric(input$teval))
    if (length(tev) == 0 || any(is.na(tev)) || any(tev < TEVAL_RANGE[1] | tev > TEVAL_RANGE[2])) {
      showNotification(paste(i18n$t("Out of range:"), i18n$t("Evaluation Temperatures")), type = "error"); return() }

    withProgress(message = i18n$t("Analyzing component..."), {
      drow <- ps$decomp[mol, ]; groups <- drow[drow > 0]
      groups <- stats::setNames(as.integer(groups), names(groups))
      Tc <- as.numeric(ps$components[mol, "Tc"]); dc <- as.numeric(ps$components[mol, "dc"])
      Tb <- if ("Tb" %in% colnames(ps$components)) as.numeric(ps$components[mol, "Tb"]) else NA_real_
      MWv <- if ("MW" %in% colnames(ps$components)) as.numeric(ps$components[mol, "MW"]) else NA_real_
      nm <- if (isTRUE(name_on()) && nzchar(input$name %||% "")) input$name else picker_pretty(mol)
      incProgress(0.4)
      tev <- sort(unique(tev))
      comp <- list(groups = groups, Tc = Tc, dc = dc, n = 1)
      psat_P <- if (!is.na(dc)) sapply(tev, function(tt)
        tryCatch(suppressWarnings(gceos_psat(tt, comp, model)), error = function(e) NA_real_)) else rep(NA_real_, length(tev))
      tc_eos <- if (!is.na(dc)) tryCatch(gceos_tcrit(comp, model), error = function(e) NA_real_) else NA_real_
      incProgress(0.4)
      eos_state$result <- list(
        name = nm, model = model, cas = "", smiles = NA_character_,
        groups = groups, Tc = Tc, Tb = Tb, dc = dc, tc_eos = tc_eos,
        psat_T = tev, psat_P = psat_P, assoc = character(0), grp_colors = NULL,
        tc_method = "literature", tb_method = "literature", frag = NULL, vis_tb = NULL, vis_tc = NULL)
      comp_obj <- list(
        display_name = sprintf("%s | %s | dc=%s", nm, model_pretty(model), fmt_num(dc, 3)),
        timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), source_module = "EOS Components",
        component = list(name = nm, model = model, smiles = "", cas = "",
          groups = as.list(groups), Tc = Tc, Tb = Tb, dc = dc, MW = MWv))
      keep <- !vapply(eos_rv$saved_components, function(x)
        identical(x$component$name, nm) && identical(x$component$model, model), logical(1))
      eos_rv$saved_components <- c(eos_rv$saved_components[keep], list(comp_obj))
      shinyjs::enable("save_to_disk")
      showNotification(sprintf(i18n$t("'%s' analyzed and added to the session."), nm), type = "message", duration = 3)
    })
  }

  # ---- analyze ----------------------------------------------------------
  observeEvent(input$analyze, {
    model <- input$model %||% "gceos"
    if (model %in% picker_models) { analyze_picker(model); return() }

    rng_err <- validate_ranges()
    if (!is.null(rng_err)) { showNotification(rng_err, type = "error"); return() }

    # SMARTS decomposition only ships a gceos and a gcaeos table; gceos-fae /
    # gceos-hmw share the gceos groups (they differ only in CO2 binaries, which
    # affect mixtures, not the pure-component dc / Psat computed here).
    decomp_model <- if (identical(model, "gcaeos")) "gcaeos" else "gceos"
    tc_method0 <- input$tc_method %||% "database"; tb_method0 <- input$tb_method %||% "database"
    smiles_flag <- isTRUE(smiles_on()) && nzchar(input$smiles %||% "")
    cas <- input$cas %||% ""; smiles_val <- input$smiles %||% ""
    nm <- if (isTRUE(name_on()) && nzchar(input$name %||% "")) input$name else if (nzchar(cas)) cas else i18n$t("Component")
    solute_input <- if (smiles_flag) c(SMILES = smiles_val, CAS = cas, Name = nm) else c(CAS = cas, Name = nm)
    if (!nzchar(cas) && !smiles_flag) {
      showNotification(i18n$t("Provide a CAS number, or specify a SMILES string."), type = "error"); return()
    }

    withProgress(message = i18n$t("Analyzing component..."), {
      # Experimental-DB preference: if Tb or Tc is "Database", resolve the compound once, look it up and
      # PREFER the measured value; if absent, fall back to a GCM estimate (Joback-Reid). Origin is tracked
      # for the result label.
      solute_data <- NULL; db_tb <- NA_real_; db_tc <- NA_real_
      tb_origin <- NA_character_; tc_origin <- NA_character_           # "db" | "gcm" (only set when method == database)
      if (tb_method0 == "database" || tc_method0 == "database") {
        if (smiles_flag) {
          mol <- tryCatch(rcdk::parse.smiles(smiles_val)[[1]], error = function(e) NULL)
          if (!is.null(mol)) solute_data <- list(IDs = c(SMILES = smiles_val, Name = nm, CAS = cas, Atom_Count = NA, H_Count = NA), Molfile = mol)
        } else {
          solute_data <- tryCatch(mol_find(solute_input), error = function(e) NULL)
        }
        if (is.null(solute_data)) { showNotification(i18n$t("Could not look up the molecule (check the CAS number / internet)."), type = "error"); return() }
        dbrow <- tryCatch(exp_prop_lookup(solute_data), error = function(e) NULL)
        if (!is.null(dbrow)) { db_tb <- suppressWarnings(as.numeric(dbrow$Tb)); db_tc <- suppressWarnings(as.numeric(dbrow$Tc)) }
        sd_smi <- unname(solute_data$IDs["SMILES"])                    # feed the resolved SMILES forward (offline GCM, no 2nd lookup)
        if (!is.na(sd_smi) && nzchar(sd_smi)) solute_input <- c(SMILES = sd_smi, CAS = cas, Name = nm)
      }
      # Effective method per property + the value/origin it should use.
      tb_method <- tb_method0; tc_method <- tc_method0
      tb_val <- suppressWarnings(as.numeric(input$tb_manual)); tc_val <- suppressWarnings(as.numeric(input$tc_manual))
      if (tb_method0 == "database") { if (is.finite(db_tb)) { tb_method <- "manual"; tb_val <- db_tb; tb_origin <- "db" } else { tb_method <- "JR"; tb_origin <- "gcm" } }
      if (tc_method0 == "database") { if (is.finite(db_tc)) { tc_method <- "manual"; tc_val <- db_tc; tc_origin <- "db" } else { tc_method <- "JR"; tc_origin <- "gcm" } }
      use_gcm <- tc_method != "manual" || tb_method != "manual"
      Tc <- if (tc_method == "manual") tc_val else NA_real_
      Tb <- if (tb_method == "manual") tb_val else NA_real_
      if (use_gcm && is.null(solute_data) && !nzchar(cas)) {
        showNotification(i18n$t("GCM estimation of Tc / Tb requires a CAS number."), type = "error"); return()
      }

      gres <- NULL
      if (use_gcm) {
        tb_arg <- if (tb_method == "manual") tb_val else tb_method
        tc_arg <- if (tc_method == "manual") tc_val else tc_method
        gorder_vec <- c()
        # Group-order dropdown values are characters ("0" = all orders, "1"/"2"/"3" = cap); coerce to int.
        if (tb_method != "manual") gorder_vec[tb_method] <- as.integer(input$tb_gorder %||% 0)
        if (tc_method != "manual") gorder_vec[tc_method] <- as.integer(input$tc_gorder %||% 0)
        if (length(gorder_vec) == 0) gorder_vec <- 0
        gres <- tryCatch(suppressWarnings(est_gcm(solute = solute_input, tb = tb_arg, crit = tc_arg,
          gorder = gorder_vec, simplicity = "auto", hlight = TRUE, silent = TRUE)),
          error = function(e) { showNotification(paste(i18n$t("GCM estimation failed:"), e$message), type = "error", duration = 6); NULL })
        if (is.null(gres)) return()
        solute_data <- gres$solute_data
        Tc <- unname(gres$pares$Critical["Tc"]); Tb <- unname(gres$pares$Critical["Tb"])
      } else if (is.null(solute_data)) {
        if (smiles_flag) {
          mol <- tryCatch(rcdk::parse.smiles(smiles_val)[[1]], error = function(e) NULL)
          if (is.null(mol)) { showNotification(i18n$t("Could not parse the molecule."), type = "error"); return() }
          solute_data <- list(IDs = c(SMILES = smiles_val, Name = nm, CAS = cas, Atom_Count = NA, H_Count = NA), Molfile = mol)
        } else {
          solute_data <- tryCatch(mol_find(solute_input), error = function(e) NULL)
          if (is.null(solute_data)) { showNotification(i18n$t("Could not look up the molecule (check the CAS number / internet)."), type = "error"); return() }
        }
      }
      incProgress(0.3)

      # Warning suppressed here because the unmatched-atom count is surfaced gracefully in the results table.
      groups <- tryCatch(suppressWarnings(gceos_decompose(solute_data, decomp_model)), error = function(e) {
        showNotification(paste(i18n$t("Decomposition failed:"), e$message), type = "error", duration = 6); NULL })
      if (is.null(groups) || length(groups) == 0) {
        showNotification(i18n$t("The molecule could not be decomposed into known groups for this model."), type = "error"); return()
      }
      n_unmatched <- as.integer(attr(groups, "unmatched_atoms") %||% 0L)

      # Obtain dc by the chosen route. Tight dc_range avoids the NaN-Z region that
      # breaks the default c(1,20) scan. Fast now that gceos_psat caches its EOS
      # inputs (~0.6 s).
      dc_method <- input$dc_method %||% "boiling"
      dc <- tryCatch(suppressWarnings({
        if (dc_method == "ester") {
          gceos_dc_ester(groups)
        } else if (dc_method == "vle") {
          co2 <- list(groups = c(CO2 = 1), dc = 3.1289, Tc = 304.2)
          gceos_fit_dc_vle(groups, Tc = Tc, T = as.numeric(input$dc_vle_T %||% 313.15),
            Pbub = as.numeric(input$dc_vle_P %||% 80) * 0.986923,
            x_solute = as.numeric(input$dc_vle_x %||% 0.2),
            solvent = co2, model = model, dc_range = c(2, 12))
        } else {
          gceos_fit_dc(groups, Tc = Tc, Tsat = Tb, Psat = 1, model = model, dc_range = c(2, 12))
        }
      }), error = function(e) NA_real_)
      if (length(dc) != 1 || !is.finite(dc)) {
        dc <- NA_real_
        showNotification(i18n$t("Could not determine dc by the selected method; check the inputs (or pick another dc source)."), type = "warning", duration = 5)
      }
      incProgress(0.3)

      tev <- sort(unique(suppressWarnings(as.numeric(input$teval))))
      comp <- list(groups = groups, Tc = Tc, dc = dc, n = 1)
      psat_P <- if (!is.na(dc)) sapply(tev, function(tt)
        tryCatch(suppressWarnings(gceos_psat(tt, comp, model)), error = function(e) NA_real_)) else rep(NA_real_, length(tev))
      # Model-predicted critical temperature: lets the results flag temperatures
      # above it as supercritical ("> Tc") rather than a blank.
      tc_eos <- if (!is.na(dc)) tryCatch(gceos_tcrit(comp, model), error = function(e) NA_real_) else NA_real_

      assoc_groups <- character(0)
      if (model == "gcaeos") {
        ps <- tryCatch(gceos_params("gcaeos"), error = function(e) NULL)
        if (!is.null(ps) && !is.null(ps$groups$assoc)) {
          amap <- stats::setNames(as.character(ps$groups$assoc), rownames(ps$groups))
          assoc_groups <- unique(stats::na.omit(amap[names(groups)]))
        }
      }

      # One representative SMARTS per present group, in names(groups) order, so
      # plot_gcm colours group i with hl_colors[i] (-> table legend lines up).
      frag <- NULL; grp_colors <- NULL
      sd <- tryCatch(gceos_params(decomp_model)$smarts, error = function(e) NULL)
      sm <- unname(solute_data$IDs["SMILES"])
      if (!is.null(sd) && !is.null(sm) && !is.na(sm) && nzchar(sm)) {
        msm <- vapply(names(groups), function(gn) { i <- match(gn, sd$group); if (is.na(i)) NA_character_ else sd$smarts[i] }, character(1))
        msm <- unname(msm[!is.na(msm)])
        frag <- tryCatch(suppressWarnings(plot_gcm(solute_data, msm, hlight = TRUE, draw_plot = FALSE, plot_title = TRUE)),
                         error = function(e) NULL)
        if (!is.null(frag)) grp_colors <- hl_colors[((seq_along(names(groups)) - 1) %% length(hl_colors)) + 1]
      }
      incProgress(0.3)

      eos_state$result <- list(
        name = nm, model = model, cas = cas, smiles = sm,
        groups = groups, unmatched = n_unmatched, Tc = Tc, Tb = Tb, dc = dc, tc_eos = tc_eos,
        psat_T = tev, psat_P = psat_P, assoc = assoc_groups, grp_colors = grp_colors,
        tc_method = tc_method0, tb_method = tb_method0,
        tc_origin = tc_origin, tb_origin = tb_origin, tc_eff = tc_method, tb_eff = tb_method, frag = frag,
        vis_tb = if (!is.null(gres) && is.list(gres$visres)) gres$visres$Tb else NULL,
        vis_tc = if (!is.null(gres) && is.list(gres$visres)) gres$visres$Critical else NULL
      )

      # Auto-export to the session (dedupe by name + model) for downstream tools.
      comp_obj <- list(
        display_name = sprintf("%s | %s | dc=%s", nm, model_pretty(model), fmt_num(dc, 3)),
        timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        source_module = "EOS Components",
        component = list(name = nm, model = model, smiles = sm, cas = cas,
          groups = as.list(groups), Tc = Tc, Tb = Tb, dc = dc))
      keep <- !vapply(eos_rv$saved_components, function(x)
        identical(x$component$name, nm) && identical(x$component$model, model), logical(1))
      eos_rv$saved_components <- c(eos_rv$saved_components[keep], list(comp_obj))

      shinyjs::enable("save_to_disk")
      showNotification(sprintf(i18n$t("'%s' analyzed and added to the session."), nm), type = "message", duration = 3)
    })
  })

  # ---- results (viz inline, left of the group table) -------------------
  output$results <- renderUI({
    r <- eos_state$result; req(r)
    meth_lbl <- function(m) {
      if (m == "manual") return(i18n_r()$t("manual"))
      lbl <- method_labels[m]; if (is.na(lbl)) lbl <- m
      i18n_r()$t(unname(lbl))
    }
    # method label for Tc/Tb honoring the "Database" choice: experimental hit, or GCM fallback on a miss.
    meth_lbl_prop <- function(which) {
      m0 <- r[[paste0(which, "_method")]]; origin <- r[[paste0(which, "_origin")]]; eff <- r[[paste0(which, "_eff")]]
      if (identical(m0, "database")) {
        if (identical(origin, "db")) return(i18n_r()$t("experimental (DB)"))
        return(paste0(meth_lbl(eff %||% "JR"), " — ", i18n_r()$t("database miss")))
      }
      meth_lbl(m0)
    }
    show_btn <- function(id) actionButton(ns(id), i18n_r()$t("Show Visualization"),
      class = "btn-primary btn-xs", style = "color:white; margin-left:8px;")
    tc_btn <- if (r$tc_method != "manual" && !is.null(r$vis_tc) && !all(is.na(r$vis_tc))) show_btn("show_tc_vis") else NULL
    tb_btn <- if (r$tb_method != "manual" && !is.null(r$vis_tb) && !all(is.na(r$vis_tb))) show_btn("show_tb_vis") else NULL

    # Tables styled like the Solute Characterization widgets: striped/bordered/hover, tinted header row,
    # an accent section title with an icon. Full-width with tuned column proportions (not shrink-to-fit).
    TBL <- "table table-striped table-hover table-bordered table-sm eos-ctr"   # eos-ctr -> centered cells
    # Section header with an optional right-aligned icon button (opens a visualisation modal).
    sec_h <- function(accent, ic, txt, btn = NULL) h4(
      style = sprintf("color:%s; border-bottom:2px solid %s; padding-bottom:5px; margin:0 0 10px 0; display:flex; align-items:center; justify-content:space-between; min-height:32px;", accent, accent),
      tags$span(icon(ic), " ", txt), btn)
    # height:100% + width:100% so each table fills its (equal-height) flex column and the rows scale to match.
    tbl_style <- "margin-bottom:0; height:100%; width:100%;"

    # Group decomposition (colour swatch + name | count). The colour footnote moves to a header tooltip.
    grp_note <- if (!is.null(r$grp_colors)) i18n_r()$t("Each colour marks a distinct EOS group matched on the molecule.") else NULL
    # Extra footer row: atoms NOT claimed by any EOS group (a GC(A)-EOS gap for the molecule), separated
    # from the group rows by a thicker rule. Amber + warning icon when > 0, muted green check when 0.
    n_unm <- as.integer(r$unmatched %||% 0L)
    unm_col <- if (n_unm > 0) "#e67e22" else "#27ae60"
    unm_row <- tags$tr(style = "border-top:3px solid #16a085;",
      tags$td(style = sprintf("color:%s;", unm_col),
        icon(if (n_unm > 0) "triangle-exclamation" else "circle-check"), " ",
        tags$strong(i18n_r()$t("Unmatched atoms")),
        tags$span(style = "cursor:help;",
          input_help(i18n_r()$t("Heavy atoms not claimed by any EOS group for this model. When present, the group decomposition is incomplete — supply a manual group vector (e.g. a published GC-EOS decomposition), and treat the estimate with caution. Hydrogens are never counted here."),
                     title = i18n_r()$t("Unmatched atoms"), buttonLabel = i18n_r()$t("OK")))),
      tags$td(style = sprintf("color:%s; font-weight:bold;", unm_col), n_unm))
    grp_tbl <- tags$table(class = TBL, style = tbl_style,
      tags$thead(tags$tr(
        tags$th(style = "width:72%; background-color:#e8f6f3;", title = grp_note,
          HTML(paste0(i18n_r()$t("Group"), if (!is.null(grp_note)) " *" else ""))),
        tags$th(style = "width:28%; background-color:#e8f6f3;", i18n_r()$t("Count")))),
      tags$tbody(c(lapply(seq_along(r$groups), function(i) {
        col <- if (!is.null(r$grp_colors)) r$grp_colors[i] else NULL
        name_cell <- if (!is.null(col)) tags$td(
          tags$span(style = sprintf("display:inline-block; width:14px; height:14px; border:1px solid #999; border-radius:3px; background:%s; margin-right:8px; vertical-align:middle;", col)),
          tags$strong(names(r$groups)[i]))
        else tags$td(tags$strong(names(r$groups)[i]))
        tags$tr(name_cell, tags$td(as.integer(r$groups)[i]))
      }), list(unm_row))))

    # Psat cells: value; "> Tc" if supercritical (model has no saturation loop);
    # "< range" if the saturation pressure is too low to resolve at that T (the
    # dilute-vapour molar volume exceeds the solver's search ceiling); else em-dash.
    has_supercrit <- FALSE; has_lowT <- FALSE
    psat_cell <- function(i) {
      p <- r$psat_P[i]
      if (!is.na(p)) return(tags$td(fmt_num(p)))
      if (!is.null(r$tc_eos) && !is.na(r$tc_eos)) {
        if (r$psat_T[i] >= r$tc_eos) { has_supercrit <<- TRUE
          return(tags$td(HTML("<span style='color:#6c757d;'>&gt; <i>T</i><sub>c</sub></span>"))) }
        has_lowT <<- TRUE
        return(tags$td(HTML(sprintf("<span style='color:#6c757d;' title='%s'>&lt; range</span>",
          i18n_r()$t("Saturation pressure too low to resolve at this temperature (below the solver range)."))))) }
      tags$td("—")
    }
    psat_rows <- lapply(seq_along(r$psat_T), function(i) tags$tr(tags$td(tags$strong(r$psat_T[i])), psat_cell(i)))
    psat_note <- if (has_supercrit || has_lowT) tags$p(style = "font-size:12px; color:#6c757d; font-style:italic; margin:4px 0 0 0;",
      HTML(paste0(
        if (has_supercrit) paste0("<b>&gt; <i>T</i><sub>c</sub></b>: ", i18n_r()$t("above the model critical temperature (supercritical).")) else "",
        if (has_supercrit && has_lowT) "<br>" else "",
        if (has_lowT) paste0("<b>&lt; range</b>: ", i18n_r()$t("saturation pressure too low to resolve at this temperature.")) else ""))) else NULL
    psat_table <- tags$table(class = TBL, style = tbl_style,
      tags$thead(tags$tr(
        tags$th(style = "width:50%; background-color:#eaf3f8;", HTML("<i>T</i> (K)")),
        tags$th(style = "width:50%; background-color:#eaf3f8;", HTML("<i>P</i><sub>sat</sub> (atm)")))),
      tags$tbody(psat_rows))

    # Parameters: label (italic symbol + full name) | value (+ muted method + optional viz button).
    param_row <- function(lab_html, val_html, meth = NULL, btn = NULL) tags$tr(
      tags$td(tags$strong(HTML(lab_html))),
      tags$td(HTML(val_html), if (!is.null(meth)) tags$small(class = "text-muted", paste0("  (", meth, ")")), btn))
    params_tbl <- tags$table(class = TBL, style = tbl_style,
      tags$thead(tags$tr(
        tags$th(style = "width:60%; background-color:#fff3e0;", i18n_r()$t("Parameter")),
        tags$th(style = "width:40%; background-color:#fff3e0;", i18n_r()$t("Value")))),
      tags$tbody(
        param_row(paste0("<i>T</i><sub>c</sub> (", i18n_r()$t("Critical Temperature"), ")"), paste0(fmt_num(r$Tc), " K"), meth_lbl_prop("tc"), tc_btn),
        param_row(paste0("<i>T</i><sub>b</sub> (", i18n_r()$t("Boiling Point"), ")"), paste0(fmt_num(r$Tb), " K"), meth_lbl_prop("tb"), tb_btn),
        param_row(paste0("<i>d</i><sub>c</sub> (", i18n_r()$t("Critical Diameter"), ")"), paste0(fmt_num(r$dc), " cm/mol"), i18n_r()$t("fitted")),
        if (length(r$assoc) > 0) param_row(i18n_r()$t("Associating groups"), paste(r$assoc, collapse = ", "))
      ))

    # When only ONE temperature resolves to a value, show it as a sentence rather than a 1-value table.
    n_fin <- sum(is.finite(r$psat_P))
    vapour_content <- if (n_fin == 1) {
      ip <- which(is.finite(r$psat_P))[1]
      div(tags$p(style = "margin:4px 0; font-size:14px;",
            HTML(sprintf(i18n_r()$t("Estimated <i>P</i><sub>sat</sub> &asymp; <b>%s atm</b> at %s K."), fmt_num(r$psat_P[ip]), r$psat_T[ip]))),
          if (length(r$psat_T) > 1) tags$small(class = "text-muted", i18n_r()$t("Other temperatures fall outside the resolvable range.")))
    } else div(style = "display:flex; flex-direction:column; height:100%;",
      div(style = "flex:1 1 auto; min-height:0;", psat_table),
      if (!is.null(psat_note)) div(style = "flex:0 0 auto; margin-top:2px;", psat_note))

    # Visualisation buttons (open zoomable modals), placed in the section headers. Plot needs >= 3 points.
    has_frag <- !is.null(r$frag)
    has_psat_plot <- n_fin >= 3
    frag_btn <- if (has_frag) actionButton(ns("show_frag_vis"), label = NULL, icon = icon("magnifying-glass-plus"),
      class = "btn-info btn-xs", style = "color:white;", title = i18n_r()$t("Show Visualization")) else NULL
    psat_btn <- if (has_psat_plot) actionButton(ns("show_psat_plot"), label = NULL, icon = icon("chart-line"),
      class = "btn-info btn-xs", style = "color:white;", title = i18n_r()$t("Show Plot")) else NULL
    id_sub <- tags$small(style = "color:#6c757d; font-weight:normal;",
      paste0(" (", model_pretty(r$model), if (!is.null(r$cas) && nzchar(r$cas)) paste0(", CAS ", r$cas) else "", ")"))

    # Equal-height flex columns: each is header + a growing wrapper holding the (height:100%) table.
    col_wrap <- function(grow, basis, ...) div(
      style = sprintf("flex:%s 1 %s; min-width:%s; display:flex; flex-direction:column;", grow, basis, basis), ...)
    fill <- function(x) div(style = "flex:1 1 auto;", x)
    tagList(
      # Centre all cell text (horizontally + vertically) in the three result tables.
      tags$style(HTML(".eos-ctr>thead>tr>th,.eos-ctr>tbody>tr>td{text-align:center !important;vertical-align:middle !important;}")),
      h4(style = "margin-top:0; color:#2c3e50;", tags$strong(r$name), id_sub),
      div(style = "display:flex; flex-wrap:wrap; gap:18px; align-items:stretch;",
        col_wrap("1", "190px",
          sec_h("#16a085", "cubes", i18n_r()$t("Group Decomposition"), frag_btn), fill(grp_tbl)),
        col_wrap("1.4", "270px",
          sec_h("#f39c12", "temperature-high", i18n_r()$t("Parameters")), fill(params_tbl)),
        col_wrap("1.1", "220px",
          sec_h("#3c8dbc", "chart-line", HTML(paste0(i18n_r()$t("Vapour pressure"), " <i>P</i><sub>sat</sub>")), psat_btn), fill(vapour_content))
      ),
      tags$hr(),
      tags$p(style = "font-size:13px; color:#555; margin-top:6px;",
        i18n_r()$t("This component (groups + Tc + dc) was added to the session for the flash, column and phase-diagram tools. Use 'Save to Disk' to keep it across sessions."))
    )
  })

  output$psat_plot <- plotly::renderPlotly({
    r <- eos_state$result; req(r, length(r$psat_T) > 2)
    df <- data.frame(T = r$psat_T, P = r$psat_P); df <- df[!is.na(df$P), ]
    req(nrow(df) >= 2)
    p <- plotly::plot_ly(df, x = ~T, y = ~P, type = "scatter", mode = "lines+markers",
      line = list(color = "#27ae60", width = 2), marker = list(color = "#27ae60", size = 8),
      hovertemplate = "T = %{x:.1f} K<br>Psat = %{y:.4g} atm<extra></extra>")
    plotly::layout(p,
      xaxis = list(title = "<i>T</i> (K)", zeroline = FALSE),
      yaxis = list(title = "<i>P</i><sub>sat</sub> (atm)", zeroline = FALSE, rangemode = "tozero"),
      margin = list(l = 55, r = 10, t = 10, b = 45))
  })

  # ---- Unified Export (CSV / PDF) — mirrors the on-screen identity + group decomposition + properties + Psat ----
  .comp_identity <- function() {
    r <- eos_state$result; if (is.null(r)) return(list())
    Filter(function(p) nzchar(p$value %||% ""), list(
      list(label = i18n_r()$t("Name"), value = r$name %||% ""),
      list(label = i18n_r()$t("CAS Number"), value = r$cas %||% ""),
      list(label = "SMILES", value = r$smiles %||% ""),
      list(label = i18n_r()$t("EOS Model"), value = model_pretty(r$model))))
  }
  .comp_groups_df <- function() {
    r <- eos_state$result; if (is.null(r) || is.null(r$groups) || !length(r$groups)) return(NULL)
    out <- data.frame(Group = names(r$groups), Count = as.integer(r$groups), check.names = FALSE, stringsAsFactors = FALSE)
    nu <- as.integer(r$unmatched %||% 0L)
    out <- rbind(out, data.frame(Group = i18n_r()$t("Unmatched atoms"), Count = nu, check.names = FALSE))
    names(out) <- c(i18n_r()$t("Group"), i18n_r()$t("Count")); out
  }
  # pm = TRUE -> Property column holds plotmath SOURCE (PDF math_col): the prose name stays upright (quoted
  # literal, translation-safe) while the symbol/units render as true math (italic T_c, d_c, cm/mol^(1/3)).
  # pm = FALSE -> plain readable text for CSV.
  .comp_props_df <- function(pm = FALSE) {
    r <- eos_state$result; if (is.null(r)) return(NULL)
    rows <- list()
    add <- function(lbl, v) if (!is.null(v) && length(v) == 1 && is.finite(suppressWarnings(as.numeric(v))))
      rows[[length(rows) + 1L]] <<- data.frame(Property = lbl, Value = signif(as.numeric(v), 5), check.names = FALSE)
    if (pm) {
      add(paste0(rp_pm_quote(i18n_r()$t("Critical temperature")), "~italic(T)[c]~", rp_pm_quote("(K)")), r$Tc)
      add(paste0(rp_pm_quote(i18n_r()$t("Boiling point")), "~italic(T)[b]~", rp_pm_quote("(K)")), r$Tb)
      add(paste0(rp_pm_quote(i18n_r()$t("Critical diameter")), "~italic(d)[c]~", '"(cm/mol"^{1/3}*")"'), r$dc)
    } else {
      add(paste0(i18n_r()$t("Critical temperature"), " Tc (K)"), r$Tc)
      add(paste0(i18n_r()$t("Boiling point"), " Tb (K)"), r$Tb)
      add(i18n_r()$t("Critical diameter dc (cm/mol^(1/3))"), r$dc)
    }
    if (!length(rows)) return(NULL)
    out <- do.call(rbind, rows); names(out) <- c(i18n_r()$t("Property"), i18n_r()$t("Value")); rownames(out) <- NULL; out
  }
  .comp_psat_gg <- function() {
    r <- eos_state$result; if (is.null(r) || length(r$psat_T %||% numeric(0)) < 2) return(NULL)
    df <- data.frame(T = r$psat_T, P = r$psat_P); df <- df[is.finite(df$P), , drop = FALSE]; if (nrow(df) < 2) return(NULL)
    ggplot2::ggplot(df, ggplot2::aes(x = T, y = P)) +
      ggplot2::geom_line(colour = "#27ae60", linewidth = 0.8) + ggplot2::geom_point(colour = "#27ae60", size = 1.6) +
      ggplot2::labs(x = "T (K)", y = expression(P[sat] ~ "(atm)")) + ggplot2::theme_minimal(base_size = 12)
  }
  output$export_pdf <- downloadHandler(
    filename = function() paste0(generate_filename_with_timestamp(paste0("supercrit_component_", gsub("[^A-Za-z0-9]", "_", eos_state$result$name %||% "report"))), ".pdf"),
    content = function(file) {
      req(eos_state$result)
      pid <- showNotification(i18n$t("Preparing export..."), type = "message", duration = NULL); on.exit(removeNotification(pid), add = TRUE)
      tryCatch({
        logo <- if (file.exists("www/altraflora-15-min.png")) "www/altraflora-15-min.png" else NULL
        ver  <- tryCatch(paste0("supeRcrit v", as.character(utils::packageVersion("supeRcrit"))), error = function(e) "supeRcrit")
        secs <- list(list(title = i18n_r()$t("Group decomposition"), table = .comp_groups_df(), digits = 0))
        pr <- .comp_props_df(pm = TRUE); if (!is.null(pr)) secs <- c(secs, list(list(title = i18n_r()$t("Estimated properties"), table = pr, digits = 5, math_col = 1L)))
        pg <- tryCatch(.comp_psat_gg(), error = function(e) NULL)
        if (!is.null(pg)) secs <- c(secs, list(list(title = i18n_r()$t("Saturation pressure"),
          plots = list(list(plot = pg, title = i18n_r()$t("Vapour pressure vs temperature"), height = 3.4)))))
        export <- list(title = i18n_r()$t("Component & Parameters — Report"),
          intro = i18n_r()$t("GC-EOS / GCA-EOS component group decomposition and estimated parameters."),
          setup = .comp_identity(), sections = secs)
        build_module_pdf_report(export, i18n_r(), file, company = ver, logo = logo)
      }, error = function(e) showNotification(paste(i18n$t("Error exporting plots:"), e$message), type = "error", duration = 8))
    })
  output$export_csv <- downloadHandler(
    filename = function() paste0(generate_filename_with_timestamp(paste0("supercrit_component_", gsub("[^A-Za-z0-9]", "_", eos_state$result$name %||% "data"))), ".csv"),
    content = function(file) {
      con <- file(file, "wb"); on.exit(close(con), add = TRUE); writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con)
      wblock <- function(title, df) { if (is.null(df) || !nrow(df)) return(invisible())
        writeBin(charToRaw(enc2utf8(paste0('"', gsub('"', '""', title), '"\r\n'))), con)
        tmp <- tempfile(fileext = ".csv"); on.exit(unlink(tmp), add = TRUE)
        utils::write.csv(df, tmp, row.names = FALSE, na = "", fileEncoding = "UTF-8")
        writeBin(readBin(tmp, "raw", n = file.info(tmp)$size), con); writeBin(charToRaw("\r\n"), con) }
      wblock(i18n_r()$t("Group decomposition"), .comp_groups_df())
      wblock(i18n_r()$t("Estimated properties"), .comp_props_df())
      r <- eos_state$result; if (!is.null(r) && length(r$psat_T %||% numeric(0)) >= 2)
        wblock(i18n_r()$t("Saturation pressure"), data.frame(T_K = r$psat_T, Psat_atm = r$psat_P, check.names = FALSE))
    }, contentType = "text/csv")

  # ---- visualisations ---------------------------------------------------
  raster_to_img <- function(raster) {
    outfile <- tempfile(fileext = ".png")
    grDevices::png(outfile, width = 1200, height = 1200, res = 300)
    grid::grid.raster(raster, width = 1, height = 1, interpolate = TRUE)
    grDevices::dev.off()
    list(src = outfile, contentType = "image/png", width = "100%", height = "auto", alt = "molecule")
  }
  output$frag_img <- renderImage({ req(eos_state$result$frag); raster_to_img(eos_state$result$frag) }, deleteFile = TRUE)

  show_vis_modal <- function(title_html) {
    showModal(modalDialog(title = HTML(title_html), size = "xl",
      uiOutput(ns("gcm_vis_order_ui")),
      div(style = "max-height: 85vh; overflow: auto; text-align: center;",
        imageOutput(ns("gcm_vis_img"), width = "100%", height = "auto")),
      footer = modalButton(i18n$t("Close")), easyClose = TRUE, fade = TRUE))
  }
  vis_which <- reactiveVal(NULL)
  observeEvent(input$show_tb_vis, { vis_which("tb"); show_vis_modal(paste0("<i>T</i><sub>b</sub> ", i18n$t("GCM Visualization"))) })
  observeEvent(input$show_tc_vis, { vis_which("tc"); show_vis_modal(paste0("<i>T</i><sub>c</sub> ", i18n$t("GCM Visualization"))) })
  # est_gcm's visres is now a per-order list ("1"/"2"/"3" -> depiction); show buttons to switch (matches
  # Solute Characterization) and render the selected order's single raster.
  vis_order_label <- function(o) c("1" = i18n_r()$t("1st order"), "2" = i18n_r()$t("2nd order"),
                                   "3" = i18n_r()$t("3rd order"))[as.character(o)]
  cur_vis_raster <- reactive({ r <- eos_state$result; if (is.null(r) || is.null(vis_which())) return(NULL)
    if (vis_which() == "tb") r$vis_tb else r$vis_tc })
  output$gcm_vis_order_ui <- renderUI({
    v <- cur_vis_raster(); ords <- if (is.list(v)) names(v) else "1"
    if (length(ords) <= 1) return(NULL)
    div(style = "text-align:center; margin-bottom:10px;",
      shinyWidgets::radioGroupButtons(ns("gcm_vis_order"), label = NULL,
        choices = stats::setNames(ords, unname(vis_order_label(ords))), selected = ords[1],
        size = "sm", status = "primary"))
  })
  output$gcm_vis_img <- renderImage({
    v <- cur_vis_raster(); req(!is.null(v) && !all(is.na(v)))
    ords <- if (is.list(v)) names(v) else "1"; o <- input$gcm_vis_order %||% ords[1]; if (!o %in% ords) o <- ords[1]
    img <- if (is.list(v)) v[[o]] else v; req(img)
    raster_to_img(img)
  }, deleteFile = TRUE)

  # Fragmentation image + Psat plot open in a zoomable modal from their table buttons.
  observeEvent(input$show_frag_vis, {
    req(eos_state$result$frag)
    showModal(modalDialog(title = i18n$t("EOS Group Fragmentation"), size = "xl",
      div(style = "max-height:85vh; overflow:auto; text-align:center;",
        imageOutput(ns("frag_img"), width = "100%", height = "auto")),
      footer = modalButton(i18n$t("Close")), easyClose = TRUE, fade = TRUE))
  })
  observeEvent(input$show_psat_plot, {
    showModal(modalDialog(title = HTML(paste0(i18n$t("Vapour pressure"), " <i>P</i><sub>sat</sub>")), size = "l",
      plotly::plotlyOutput(ns("psat_plot"), height = "460px"),
      footer = modalButton(i18n$t("Close")), easyClose = TRUE, fade = TRUE))
  })

  # ---- save to disk (modal: choose filename + directory) ---------------
  eos_volumes <- c(Home = Sys.getenv("HOME"), supercrit_get_volumes())
  shinyFiles::shinyDirChoose(input, "save_modal-browse_directory", roots = eos_volumes, session = session)

  observeEvent(input[["save_modal-browse_directory"]], {
    req(input[["save_modal-browse_directory"]])
    dir_path <- shinyFiles::parseDirPath(eos_volumes, input[["save_modal-browse_directory"]])
    if (length(dir_path) > 0 && nchar(dir_path) > 0)
      updateTextInput(session, "save_modal-save_directory", value = dir_path)
  })

  observeEvent(input$save_to_disk, {
    r <- eos_state$result
    if (is.null(r)) { showNotification(i18n$t("No component to save. Run an analysis first."), type = "warning"); return() }
    default_dir <- supercrit_user_dir(file.path("user-settings", "eos_components"))
    showModal(modalDialog(
      title = i18n$t("Save Component to Disk"), size = "m",
      div(class = "alert alert-info", icon("info-circle"),
        strong(i18n$t("Saving:")), " ", sprintf("%s (%s)", r$name, model_pretty(r$model))),
      textInput(ns("save_modal-filename"), i18n$t("Filename:"),
        value = gsub("[^a-zA-Z0-9_.-]", "_", r$name), width = "100%"),
      div(style = "display: flex; gap: 10px; align-items: flex-end;",
        div(style = "flex-grow: 1;",
          textInput(ns("save_modal-save_directory"), i18n$t("Save Directory"), value = default_dir, width = "100%")),
        div(style = "margin-bottom: 15px;",
          shinyFiles::shinyDirButton(ns("save_modal-browse_directory"), label = i18n$t("Browse..."),
            title = i18n$t("Select Save Directory"), class = "btn-default"))),
      tags$small(class = "text-muted", i18n$t("Leave empty to use the default package directory.")),
      uiOutput(ns("save_modal-filename_preview")),
      footer = tagList(modalButton(i18n$t("Cancel")),
        actionButton(ns("save_modal-confirm_save"), i18n$t("Save"), class = "btn btn-success", style = "color: white;"))))
  })

  output[["save_modal-filename_preview"]] <- renderUI({
    req(input[["save_modal-filename"]])
    sanitized <- gsub("[^a-zA-Z0-9_.-]", "_", input[["save_modal-filename"]])
    if (nchar(sanitized) == 0) sanitized <- "untitled"
    final <- paste0(sanitized, "_", format(Sys.time(), "%d-%m-%Y_%Hhrs-%Mmin-%Ssec"), ".json")
    div(style = "margin-top: 10px;", class = "alert alert-secondary",
      strong(i18n$t("Final filename:")), " ", final)
  })

  observeEvent(input[["save_modal-confirm_save"]], {
    req(input[["save_modal-filename"]])
    r <- eos_state$result; req(r)
    removeModal()
    tryCatch({
      filename <- gsub("[^a-zA-Z0-9_.-]", "_", input[["save_modal-filename"]])
      if (nchar(filename) == 0) filename <- "untitled"
      final_filename <- paste0(filename, "_", format(Sys.time(), "%d-%m-%Y_%Hhrs-%Mmin-%Ssec"), ".json")
      save_dir <- input[["save_modal-save_directory"]]
      if (is.null(save_dir) || nchar(trimws(save_dir)) == 0)
        save_dir <- supercrit_user_dir(file.path("user-settings", "eos_components"))
      if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
      filepath <- file.path(save_dir, final_filename)
      jsonlite::write_json(list(name = r$name, model = r$model, smiles = r$smiles, cas = r$cas,
        groups = as.list(r$groups), Tc = r$Tc, Tb = r$Tb, dc = r$dc,
        metadata = list(created_date = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), app_version = "0.9.0")),
        filepath, auto_unbox = TRUE, pretty = TRUE)
      showNotification(paste(i18n$t("Component saved to:"), basename(filepath)), type = "message", duration = 5)
    }, error = function(e) showNotification(paste(i18n$t("Error saving file:"), e$message), type = "error"))
  })

  # ---- help / reset -----------------------------------------------------
  output$eos_components_HELP <- renderUI({
    helper(shiny_tag = "", buttonLabel = "OK",
      content = i18n_r()$t("eos_components_help_en"), type = "markdown", size = "l",
      style = "color:white; font-size:20px; vertical-align:middle; margin-top:0; margin-right:3%;")
  })
  outputOptions(output, "eos_components_HELP", suspendWhenHidden = FALSE)

  output$accordion_toggle_btn <- renderUI({ create_accordion_toggle_btn(ns, i18n_r, "comp_accordion") })
  outputOptions(output, "accordion_toggle_btn", suspendWhenHidden = FALSE)

  observeEvent(input$reset, {
    name_on(FALSE); smiles_on(FALSE)
    updateSelectInput(session, "model", selected = "gceos")
    updateTextInput(session, "cas", value = "")
    updateTextInput(session, "name", value = "")
    updateTextInput(session, "smiles", value = "")
    updateSelectInput(session, "tc_method", selected = "manual")
    updateSelectInput(session, "tb_method", selected = "manual")
    updateSelectInput(session, "dc_method", selected = "boiling")
    updateNumericInput(session, "tc_manual", value = NA)
    updateNumericInput(session, "tb_manual", value = NA)
    updateSelectizeInput(session, "teval", choices = c("350", "400", "450"), selected = c("350", "400", "450"))
    eos_state$result <- NULL
    shinyjs::disable("save_to_disk")
  })
}
