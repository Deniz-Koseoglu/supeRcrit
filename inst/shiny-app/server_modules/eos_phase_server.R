# Process Simulation - Phase Behaviour Server Module
# CO2 + solute(s) phase behaviour with the GC(A)-EOS. Solutes come from the
# session store (eos_rv$saved_components, empty until Load Examples / Load from
# Disk); CO2 is the implicit light component. Three calculations are shown as
# tabs and computed lazily on first view (flash / P-x,y / critical locus).
eos_phase_server <- function(input, output, session, defaults, i18n, tablang, eos_rv) {

  ns <- session$ns
  i18n_r <- reactive({ i18n })

  bar2atm <- function(b) b * 0.986923
  atm2bar <- function(a) a * 1.01325
  model_pretty <- function(m) switch(m %||% "gceos",
    gcaeos = "GCA-EOS", `gceos-fae` = "GC-EOS (esters)", `gceos-hmw` = "GC-EOS (high-MW)",
    diaz2005 = "GC-EOS (Diaz terpenes)", ganan2011 = "GC-EOS (Ganan terpenes)", "GC-EOS")
  co2_html <- "CO<sub>2</sub>"
  disp_name <- function(nm) ifelse(nm == "CO2", co2_html, nm)   # subscript CO2 for display
  # checkbox label for a session component: name | model | acquired-time
  solute_label <- function(c) {
    ts <- c$timestamp %||% ""
    tdisp <- if (ts %in% c("seed", "")) i18n_r()$t("example") else ts
    sprintf("%s | %s | %s", c$name, model_pretty(c$model), tdisp)
  }
  fmt_num <- function(x, digits = 4) {
    if (is.null(x) || length(x) == 0 || is.na(x)) return("—")
    s <- formatC(round(x, digits), format = "f", digits = digits)
    if (grepl(".", s, fixed = TRUE)) s <- sub("\\.?0+$", "", s)   # strip trailing zeros only past the decimal (else "100" -> "1")
    s
  }
  CO2_DC <- 3.1289; CO2_TC <- 304.2; CO2_MW <- 44.01
  T_RANGE <- c(0, 300); P_RANGE <- c(1, 1000)
  PR_ATM  <- c(5, 400)   # pressure search bracket for the envelope / critical (atm)

  eos_state <- reactiveValues(ctx = NULL, flash = NULL, pxy = NULL, locus = NULL)

  # ---- solute registry (session components) ----------------------------
  solute_registry <- reactive({
    sc <- eos_rv$saved_components; reg <- list()
    for (i in seq_along(sc)) {
      cc <- sc[[i]]$component
      if (is.null(cc) || is.null(cc$groups)) next
      smi <- cc$smiles %||% ""
      mw <- cc$MW %||% (if (nzchar(smi)) tryCatch(rcdk::get.natural.mass(rcdk::parse.smiles(smi)[[1]]), error = function(e) NA_real_) else NA_real_)
      reg[[as.character(i)]] <- list(idx = i, name = cc$name %||% paste("Component", i),
        groups = unlist(cc$groups), dc = cc$dc, Tc = cc$Tc, model = cc$model %||% "gceos", smiles = smi, mw = mw,
        timestamp = sc[[i]]$timestamp %||% "")
    }
    reg
  })

  # ---- range-badge helper (label_node may be HTML / i18n tag) ----------
  badge_num <- function(id_in, badge_id, label_node, helptxt, value, lo, hi, step) {
    bad <- is.null(value) || is.na(value) || value < lo || value > hi
    tags$div(
      tags$label(class = "control-label", style = "display:flex; align-items:center; justify-content:space-between; width:100%;",
        tags$span(label_node, input_help(i18n_r()$t(helptxt), buttonLabel = i18n_r()$t("OK"))),
        tags$span(paste0(lo, "–", hi), id = ns(badge_id),
          style = sprintf("font-size:10px; padding:1px 5px; border-radius:3px; background-color:%s; color:white; font-weight:normal;",
                          if (bad) "#dc3545" else "#6c757d"))),
      numericInput(ns(id_in), label = NULL, value = value, min = lo, max = hi, step = step))
  }
  recolor <- function(badge_id, value, lo, hi) {
    bad <- is.null(value) || length(value) == 0 || any(is.na(value)) || any(value < lo) || any(value > hi)
    shinyjs::runjs(sprintf("$('#%s').css('background-color','%s');", ns(badge_id), if (bad) "#dc3545" else "#6c757d"))
  }
  observeEvent(input$temp_c, recolor("temp_badge", input$temp_c, T_RANGE[1], T_RANGE[2]), ignoreInit = TRUE)
  observeEvent(input$pres_bar, recolor("pres_badge", input$pres_bar, P_RANGE[1], P_RANGE[2]), ignoreInit = TRUE)
  observeEvent(input$tmin_c, recolor("tmin_badge", input$tmin_c, T_RANGE[1], T_RANGE[2]), ignoreInit = TRUE)
  observeEvent(input$tmax_c, recolor("tmax_badge", input$tmax_c, T_RANGE[1], T_RANGE[2]), ignoreInit = TRUE)
  observeEvent(input$locus_tvals, recolor("ltv_badge", suppressWarnings(as.numeric(input$locus_tvals)), T_RANGE[1], T_RANGE[2]),
               ignoreInit = TRUE, ignoreNULL = FALSE)

  validate_ranges <- function() {
    bad <- c()
    chk <- function(v, lo, hi, lbl) if (is.null(v) || is.na(v) || v < lo || v > hi) bad <<- c(bad, i18n$t(lbl))
    chk(input$temp_c, T_RANGE[1], T_RANGE[2], "Temperature (°C)")
    chk(input$pres_bar, P_RANGE[1], P_RANGE[2], "Pressure (bar)")
    if ((input$locus_mode %||% "range") == "tags") {
      v <- suppressWarnings(as.numeric(input$locus_tvals))
      if (length(v) > 0 && (any(is.na(v)) || any(v < T_RANGE[1]) || any(v > T_RANGE[2]))) bad <- c(bad, i18n$t("Temperatures"))
    } else {
      chk(input$tmin_c, T_RANGE[1], T_RANGE[2], "T min (°C)")
      chk(input$tmax_c, T_RANGE[1], T_RANGE[2], "T max (°C)")
    }
    if (length(bad) == 0) return(NULL)
    paste(i18n$t("Out of range:"), paste(unique(bad), collapse = ", "))
  }

  # ---- solute selection (checkbox, max 2) + load / remove --------------
  output$components_ui <- renderUI({
    reg <- solute_registry()
    if (length(reg) == 0)
      return(tagList(
        p(style = "margin-bottom: 2px;", i18n_r()$t("No components in the session.")),
        p(style = "font-size: 0.9em; color: #888; margin-top: 0;",
          i18n_r()$t("Load the examples, load from disk, or build one in Components & Parameters."))))
    choices <- stats::setNames(names(reg), vapply(reg, solute_label, character(1)))
    sel <- isolate(input$solutes); sel <- sel[sel %in% names(reg)]
    del <- actionButton(ns("remove_solutes"), i18n_r()$t("Remove Selected from Session"),
      icon = icon("trash-alt"), class = "btn-xs btn-danger", style = "color:white;")
    save_btn <- actionButton(ns("save_selected_solutes"), i18n_r()$t("Save Selected to Disk"),
      icon = icon("save"), class = "btn-xs btn-primary", style = "color:white;")
    if (length(sel) == 0) { del <- shinyjs::disabled(del); save_btn <- shinyjs::disabled(save_btn) }
    tagList(
      div(style = if (length(reg) > 6) "max-height:200px; overflow-y:auto; margin-bottom:6px;" else "margin-bottom:6px;",
        checkboxGroupInput(ns("solutes"), label = i18n_r()$t("Select 1-2 solutes"), choices = choices, selected = sel)),
      div(style = "margin-top:-4px; display:flex; gap:6px; flex-wrap:wrap;", del, save_btn)
    )
  })
  outputOptions(output, "components_ui", suspendWhenHidden = FALSE)

  # keep the Remove button enabled state in sync with the live checkbox selection
  # (components_ui only re-renders when the registry changes, not on each tick).
  observeEvent(input$solutes, {
    shinyjs::toggleState("remove_solutes", condition = length(input$solutes) > 0)
    shinyjs::toggleState("save_selected_solutes", condition = length(input$solutes) > 0)
  }, ignoreNULL = FALSE)

  # cap selection at two AND disable the rest once two are chosen
  observeEvent(input$solutes, {
    n <- length(input$solutes)
    if (n > 2) { updateCheckboxGroupInput(session, "solutes", selected = input$solutes[1:2])
      showNotification(i18n$t("At most two solutes can be selected."), type = "warning", duration = 2); return() }
    shinyjs::runjs(sprintf(
      "var g=$('#%s'); if(%d>=2){g.find('input:checkbox:not(:checked)').prop('disabled',true);} else {g.find('input:checkbox').prop('disabled',false);}",
      ns("solutes"), n))
  }, ignoreNULL = FALSE)

  observeEvent(input$load_examples, {
    seeds <- eos_rv$seed_components
    if (length(seeds) == 0) { showNotification(i18n$t("No example components available."), type = "warning"); return() }
    existing <- vapply(eos_rv$saved_components, function(x) x$display_name %||% "", character(1))
    added <- 0
    for (s in seeds) if (!(s$display_name %in% existing)) {
      eos_rv$saved_components[[length(eos_rv$saved_components) + 1]] <- s; added <- added + 1 }
    showNotification(if (added == 0) i18n$t("Example components already loaded.")
                     else sprintf(i18n$t("Loaded %d example component(s)."), added), type = "message")
  })

  # Load from Disk: read component JSON(s) saved by Components & Parameters
  observeEvent(input$load_from_disk, {
    showModal(modalDialog(title = i18n$t("Load Components from Disk"), size = "m",
      fileInput(ns("load_file_input"), i18n$t("Select JSON file(s)"), multiple = TRUE,
        accept = c(".json", "application/json"), buttonLabel = i18n$t("Browse..."), placeholder = i18n$t("No file selected")),
      tags$small(class = "text-muted", i18n$t("Select one or more .json files exported from Components & Parameters.")),
      footer = tagList(modalButton(i18n$t("Cancel")),
        actionButton(ns("confirm_load_disk"), i18n$t("Load"), class = "btn btn-primary", style = "color:white;"))))
  })
  observeEvent(input$confirm_load_disk, {
    req(input$load_file_input); removeModal()
    files <- input$load_file_input; loaded <- 0
    existing <- vapply(eos_rv$saved_components, function(x) x$component$name %||% "", character(1))
    for (i in seq_len(nrow(files))) {
      d <- tryCatch(jsonlite::fromJSON(files$datapath[i], simplifyVector = TRUE), error = function(e) NULL)
      if (is.null(d) || is.null(d$groups) || is.null(d$dc) || is.null(d$Tc)) {
        showNotification(sprintf(i18n$t("Invalid component file: %s"), files$name[i]), type = "warning"); next }
      nm <- d$name %||% tools::file_path_sans_ext(files$name[i])
      if (nm %in% existing) { showNotification(sprintf(i18n$t("Component '%s' already loaded."), nm), type = "warning"); next }
      comp <- list(name = nm, model = d$model %||% "gceos", smiles = d$smiles %||% "", cas = d$cas %||% "",
        groups = as.list(d$groups), Tc = d$Tc, Tb = d$Tb %||% NA, dc = d$dc)
      eos_rv$saved_components[[length(eos_rv$saved_components) + 1]] <- list(
        display_name = sprintf("%s | %s | dc=%s", nm, model_pretty(comp$model), fmt_num(d$dc, 3)),
        timestamp = d$timestamp %||% format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        source_module = "Loaded from Disk", component = comp)
      loaded <- loaded + 1
    }
    if (loaded > 0) showNotification(sprintf(i18n$t("Loaded %d component(s) from disk."), loaded), type = "message")
  })

  observeEvent(input$remove_solutes, {
    req(length(input$solutes) > 0)
    reg <- solute_registry(); names_sel <- vapply(input$solutes, function(k) reg[[k]]$name, character(1))
    showModal(modalDialog(title = i18n$t("Confirm Removal"),
      p(i18n$t("Remove the selected component(s) from the session?")),
      tags$ul(lapply(names_sel, function(n) tags$li(tags$strong(n)))),
      footer = tagList(modalButton(i18n$t("Cancel")),
        actionButton(ns("confirm_remove"), i18n$t("Remove"), class = "btn-danger", style = "color:white;"))))
  })
  observeEvent(input$confirm_remove, {
    removeModal(); req(length(input$solutes) > 0)
    idx <- sort(as.integer(input$solutes), decreasing = TRUE)
    for (i in idx) if (i <= length(eos_rv$saved_components)) eos_rv$saved_components[[i]] <- NULL
    eos_rv$saved_components <- Filter(Negate(is.null), eos_rv$saved_components)
    showNotification(i18n$t("Removed selected component(s)."), type = "warning", duration = 3)
  })

  # Save the selected components to disk (one JSON each; round-trips via "Load from Disk").
  setup_save_selected_to_disk(input, session, i18n_r, list(
    trigger = "save_selected_solutes", confirm = "save_sel_confirm", dir_input = "save_sel_dir", browse = "save_sel_browse",
    default_dir = function() supercrit_user_dir(file.path("user-settings", "eos_components")),
    collect = function() {
      reg <- solute_registry(); sel <- input$solutes %||% character(0); sel <- sel[sel %in% names(reg)]
      if (!length(sel)) return(NULL)
      lapply(sel, function(k) {
        cc <- eos_rv$saved_components[[reg[[k]]$idx]]$component
        list(name = cc$name %||% "component",
             data = list(name = cc$name, model = cc$model %||% "gceos", smiles = cc$smiles %||% "",
                         cas = cc$cas %||% "", groups = as.list(cc$groups), Tc = cc$Tc, Tb = cc$Tb %||% NA, dc = cc$dc,
                         metadata = list(created_date = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), app_version = "0.9.0")))
      })
    }))

  resolve_model <- reactive({
    reg <- solute_registry(); sel <- input$solutes %||% character(0); sel <- sel[sel %in% names(reg)]
    if (length(sel) == 0) return(list(model = NA, conflict = FALSE, n = 0))
    models <- unique(vapply(reg[sel], function(c) c$model, character(1)))
    list(model = if (length(models)) models[1] else "gceos", conflict = length(models) > 1, n = length(sel))
  })
  output$model_note_ui <- renderUI({
    rm <- resolve_model()
    if (rm$n > 1 && rm$conflict)
      div(class = "alert alert-danger", style = "padding:6px 10px; margin:6px 0; font-size:12px;",
        i18n_r()$t("The selected components were built with different EOS models. Pick components of the same model."))
    else NULL
  })
  outputOptions(output, "model_note_ui", suspendWhenHidden = FALSE)

  # ---- conditions / composition / locus range --------------------------
  output$conditions_ui <- renderUI({
    fluidRow(
      column(6, badge_num("temp_c", "temp_badge", i18n_r()$t("Temperature (°C)"), "Temperature for the flash and the P-x,y diagram.",
        isolate(input$temp_c) %||% (defaults$temp_c %||% 50), T_RANGE[1], T_RANGE[2], 1)),
      column(6, badge_num("pres_bar", "pres_badge", i18n_r()$t("Pressure (bar)"), "Pressure for the flash.",
        isolate(input$pres_bar) %||% (defaults$pres_bar %||% 100), P_RANGE[1], P_RANGE[2], 1))
    )
  })
  outputOptions(output, "conditions_ui", suspendWhenHidden = FALSE)

  output$composition_ui <- renderUI({
    reg <- solute_registry(); sel <- (input$solutes %||% character(0)); sel <- sel[sel %in% names(reg)]
    sol_nm <- vapply(reg[sel], function(c) c$name, character(1))
    n <- length(sel)
    # Unit suffix on each composition label, switching with the Mass fractions checkbox (reactive read, so a
    # toggle re-renders these labels; the z-values + checkbox state are preserved via isolate() below).
    unit <- if (isTRUE(input$mass_basis)) i18n_r()$t("g·g⁻¹") else i18n_r()$t("mol·mol⁻¹")
    with_unit <- function(base) tagList(base, tags$span(style = "color:#888; font-weight:normal;", sprintf(" (%s)", unit)))
    labs <- list(with_unit(HTML(co2_html)),
      with_unit(if (n >= 1) sol_nm[1] else i18n_r()$t("Component 2")),
      with_unit(if (n >= 2) sol_nm[2] else i18n_r()$t("Component 3")))
    zdef <- if (n >= 2) c(0.85, 0.1, 0.05) else c(0.9, 0.1, 0)
    enabled <- c(TRUE, n >= 1, n >= 2)
    fld <- function(i) {
      ni <- numericInput(ns(paste0("z", i)), labs[[i]], value = isolate(input[[paste0("z", i)]]) %||% zdef[i], min = 0, max = 1, step = 0.01)
      if (!enabled[i]) shinyjs::disabled(ni) else ni
    }
    tagList(
      tags$label(class = "control-label", style = "margin-top:6px;", i18n_r()$t("Feed composition (Flash)")),
      fluidRow(column(4, fld(1)), column(4, fld(2)), column(4, fld(3))),
      # badge (reactive to the fraction inputs) + mass toggle (stable per selection)
      div(style = "display:flex; align-items:center; gap:16px; margin-top:4px;",
        uiOutput(ns("total_badge"), inline = TRUE),
        div(class = "eos-mass-toggle",
          checkboxInput(ns("mass_basis"), i18n_r()$t("Mass fractions"), value = isolate(input$mass_basis) %||% FALSE)))
    )
  })
  outputOptions(output, "composition_ui", suspendWhenHidden = FALSE)

  comp_total <- reactive({
    reg <- solute_registry(); sel <- (input$solutes %||% character(0)); sel <- sel[sel %in% names(reg)]
    n <- length(sel); ncomp <- 1 + n
    sum(vapply(seq_len(ncomp), function(i) { v <- input[[paste0("z", i)]]; if (is.null(v) || is.na(v)) 0 else as.numeric(v) }, numeric(1)))
  })
  output$total_badge <- renderUI({
    tot <- comp_total(); ok <- abs(tot - 1) < 1e-6
    tags$span(strong(i18n_r()$t("Total fraction:")), " ",
      span(fmt_num(tot, 4), style = sprintf("background-color:%s; color:white; font-weight:bold; padding:2px 8px; border-radius:4px;",
        if (ok) "#28a745" else "#dc3545")))
  })

  output$locus_mode_ui <- renderUI({
    radioButtons(ns("locus_mode"), label = NULL, inline = TRUE,
      choices = stats::setNames(c("range", "tags"), c(i18n_r()$t("Temperature range"), i18n_r()$t("Specific values"))),
      selected = isolate(input$locus_mode) %||% "range")
  })
  outputOptions(output, "locus_mode_ui", suspendWhenHidden = FALSE)

  output$locus_range_ui <- renderUI({
    if ((input$locus_mode %||% "range") == "tags") {
      cur <- isolate(input$locus_tvals) %||% c("30", "50", "70")
      chip_render <- I(sprintf("{ item: function(item, escape) {
          var v = parseFloat(item.value); var oor = isNaN(v) || v < %d || v > %d;
          var s = oor ? 'background-color:#dc3545; border-color:#dc3545; color:white;' : '';
          return '<div class=\"item\" style=\"' + s + '\">' + escape(item.value) + '</div>'; } }", T_RANGE[1], T_RANGE[2]))
      bad0 <- { v <- suppressWarnings(as.numeric(cur)); length(v) == 0 || any(is.na(v)) || any(v < T_RANGE[1]) || any(v > T_RANGE[2]) }
      tags$div(
        tags$label(class = "control-label", style = "display:flex; align-items:center; justify-content:space-between; width:100%;",
          tags$span(HTML(paste0(i18n_r()$t("Temperatures"), " (°C)"))),
          tags$span(paste0(T_RANGE[1], "–", T_RANGE[2]), id = ns("ltv_badge"),
            style = sprintf("font-size:10px; padding:1px 5px; border-radius:3px; background-color:%s; color:white; font-weight:normal;",
                            if (bad0) "#dc3545" else "#6c757d"))),
        selectizeInput(ns("locus_tvals"), label = NULL, choices = as.character(cur), selected = as.character(cur),
          multiple = TRUE, width = "100%",
          options = list(create = TRUE, persist = FALSE, maxItems = 12, render = chip_render,
            placeholder = i18n_r()$t("Type a temperature and press Enter"))))
    } else {
      fluidRow(
        column(4, badge_num("tmin_c", "tmin_badge", HTML(paste0("<i>T</i><sub>min</sub> (°C)")), "Lowest temperature of the sweep.",
          isolate(input$tmin_c) %||% (defaults$tmin_c %||% 20), T_RANGE[1], T_RANGE[2], 1)),
        column(4, badge_num("tmax_c", "tmax_badge", HTML(paste0("<i>T</i><sub>max</sub> (°C)")), "Highest temperature of the sweep.",
          isolate(input$tmax_c) %||% (defaults$tmax_c %||% 90), T_RANGE[1], T_RANGE[2], 1)),
        column(4, numericInput(ns("npts"), i18n_r()$t("Points"), value = isolate(input$npts) %||% (defaults$npts %||% 6), min = 3, max = 16, step = 1))
      )
    }
  })
  outputOptions(output, "locus_range_ui", suspendWhenHidden = FALSE)

  # ---- run --------------------------------------------------------------
  observeEvent(input$run, {
    reg <- solute_registry(); sel <- (input$solutes %||% character(0)); sel <- sel[sel %in% names(reg)]
    if (length(sel) < 1) { showNotification(i18n$t("Select at least one solute."), type = "error"); return() }
    rng_err <- validate_ranges(); if (!is.null(rng_err)) { showNotification(rng_err, type = "error"); return() }
    if (abs(comp_total() - 1) >= 1e-6) { showNotification(i18n$t("Feed fractions must sum to exactly 1."), type = "error"); return() }
    rm <- resolve_model(); if (rm$conflict) {
      showNotification(i18n$t("The selected components were built with different EOS models. Pick components of the same model."), type = "error"); return() }
    model <- rm$model; selc <- reg[sel]
    nms <- make.unique(c("CO2", vapply(selc, function(c) c$name, character(1))))
    comps <- stats::setNames(c(list(list(groups = c(CO2 = 1), dc = CO2_DC, Tc = CO2_TC, n = 1)),
      lapply(selc, function(c) list(groups = c$groups, dc = c$dc, Tc = c$Tc, n = 1))), nms)
    ncomp <- length(nms)
    zin <- vapply(seq_len(ncomp), function(i) as.numeric(input[[paste0("z", i)]] %||% 0), numeric(1))
    if (isTRUE(input$mass_basis)) {
      mws <- c(CO2_MW, vapply(selc, function(c) c$mw, numeric(1)))
      if (!any(is.na(mws)) && all(mws > 0)) zin <- zin / mws
      else showNotification(i18n$t("Molecular weight unavailable; using the values as mole fractions."), type = "warning", duration = 4)
    }
    z <- zin / sum(zin)

    Tvec <- if ((input$locus_mode %||% "range") == "tags") {
      tv <- sort(unique(suppressWarnings(as.numeric(input$locus_tvals)))); tv[is.finite(tv)] + 273.15
    } else seq((input$tmin_c %||% 20) + 273.15, (input$tmax_c %||% 90) + 273.15, length.out = max(3, min(16, round(input$npts %||% 6))))

    eos_state$ctx <- list(comps = comps, model = model, nms = nms,
      T_K = (input$temp_c %||% 50) + 273.15, P_atm = bar2atm(input$pres_bar %||% 100), z = z,
      Tvec = Tvec, is_binary = (length(sel) == 1), temp_c = input$temp_c, pres_bar = input$pres_bar, mass = isTRUE(input$mass_basis))
    eos_state$flash <- NULL; eos_state$pxy <- NULL; eos_state$locus <- NULL
    updateTabsetPanel(session, "result_tabs", selected = "flash")
    compute_flash()
    # Generate the P-x,y envelope RIGHT AWAY (binary only) so it is ready to view and always exported — not
    # deferred to the first tab click. (The slower critical locus stays lazy for display, but is force-computed
    # at export time so it is never silently missing from the report.)
    compute_pxy()
  })

  compute_flash <- function() {
    ctx <- eos_state$ctx; if (is.null(ctx)) return()
    withProgress(message = i18n$t("Computing flash..."), {
      eos_state$flash <- list(fl = tryCatch(gceos_flash3_solve(ctx$T_K, ctx$P_atm, ctx$z, ctx$comps, ctx$model), error = function(e) NULL))
    })
  }
  compute_pxy <- function() {
    ctx <- eos_state$ctx; if (is.null(ctx) || !ctx$is_binary) return()
    withProgress(message = i18n$t("Computing P-x,y envelope..."), {
      px <- tryCatch(gceos_pxy(ctx$T_K, ctx$comps, ctx$model, x1 = seq(0.02, 0.98, by = 0.03), Prange = PR_ATM), error = function(e) NULL)
      crit <- NULL
      if (!is.null(px)) { d <- px[is.finite(px$P), , drop = FALSE]
        if (nrow(d) > 0) { i <- which.max(d$P); crit <- list(x_c = d$x1[i], P_c = d$P[i]) } }   # apex of the envelope
      eos_state$pxy <- list(px = px, crit = crit)
    })
  }
  compute_locus <- function() {
    ctx <- eos_state$ctx; if (is.null(ctx) || !ctx$is_binary) return()
    if (length(ctx$Tvec) < 1) { eos_state$locus <- list(loc = NULL); return() }
    withProgress(message = i18n$t("Computing critical locus..."), {
      eos_state$locus <- list(loc = tryCatch(gceos_critical_locus(ctx$comps, ctx$model, Tvec = ctx$Tvec, Prange = PR_ATM), error = function(e) NULL))
    })
  }
  observeEvent(input$result_tabs, {
    if (is.null(eos_state$ctx)) return()
    if (input$result_tabs == "pxy" && eos_state$ctx$is_binary && is.null(eos_state$pxy)) compute_pxy()
    if (input$result_tabs == "locus" && eos_state$ctx$is_binary && is.null(eos_state$locus)) compute_locus()
  }, ignoreInit = TRUE)

  # ---- result renderers -------------------------------------------------
  placeholder <- function(icn, msg) div(style = "text-align:center; padding:45px; color:#888;",
    icon(icn, style = "font-size:42px; margin-bottom:12px;"), h4(msg))
  binary_note <- function() div(class = "alert alert-info", style = "margin-top:10px;",
    icon("info-circle"), " ", i18n_r()$t("Select a single solute (binary CO2 + solute) for this diagram."))
  result_title <- function(ctx) h5(style = "margin:0;",
    tags$strong(HTML(paste(disp_name(ctx$nms), collapse = " + "))),
    tags$small(style = "color:#6c757d;", paste0("  (", model_pretty(ctx$model), ")")))
  result_header <- function(ctx) tagList(result_title(ctx), tags$hr())
  plot_toolbar <- function(plot_id, data_id) div(style = "display:flex; gap:8px; flex-wrap:wrap;",
    actionButton(ns(paste0(plot_id, "_img")), i18n_r()$t("Export Plot"), icon = icon("image"),
      class = "btn btn-success btn-sm", style = "color:white;"),
    downloadButton(ns(data_id), i18n_r()$t("Export Data"), class = "btn btn-success btn-sm", style = "color:white;"))
  # Plot title on the LEFT with the Export Plot / Export Data buttons inline on the RIGHT, then a rule — instead
  # of a separate button strip below the title (which wasted vertical space).
  plot_header_row <- function(title_tag, plot_id, data_id) tagList(
    div(style = "display:flex; justify-content:space-between; align-items:center; flex-wrap:wrap; gap:10px;",
      div(style = "flex:1 1 auto; min-width:0;", title_tag),
      div(style = "flex:0 0 auto;", plot_toolbar(plot_id, data_id))),
    tags$hr())

  output$flash_result <- renderUI({
    ctx <- eos_state$ctx
    if (is.null(ctx)) return(placeholder("chart-area", i18n_r()$t("Pick solutes and conditions, then Calculate.")))
    fr <- eos_state$flash; if (is.null(fr)) return(placeholder("spinner", i18n_r()$t("Computing...")))
    fl <- fr$fl
    cond <- tags$p(style = "font-size:14px; color:#555;",
      HTML(sprintf("<i>T</i> = %s °C &nbsp;·&nbsp; <i>P</i> = %s bar%s", fmt_num(ctx$temp_c, 2), fmt_num(ctx$pres_bar, 1),
        if (ctx$mass) paste0(" &nbsp;·&nbsp; ", i18n_r()$t("mass basis")) else "")))
    body <- if (is.null(fl) || is.na(fl$nphase) || fl$status < 1)
      div(class = "alert alert-warning", i18n_r()$t("The flash did not converge for these conditions."))
    else if (fl$nphase == 1)
      div(class = "alert alert-info", icon("info-circle"), " ", i18n_r()$t("Single homogeneous phase at these conditions (no split)."))
    else if (fl$nphase == 2) {
      av <- if (length(ctx$nms) >= 3) { K <- fl$y / fl$x; tags$p(HTML(sprintf("%s (%s / %s) = <strong>%s</strong>",
        i18n_r()$t("Relative volatility"), ctx$nms[2], ctx$nms[3], fmt_num(K[2] / K[3], 3)))) } else NULL
      tagList(tags$p(HTML(sprintf("<strong>%s</strong> &nbsp; (β<sub>V</sub> = %s)",
        i18n_r()$t("Two phases (vapour-liquid)"), fmt_num(fl$beta, 3)))),
        DT::dataTableOutput(ns("flash_table")), av)
    } else
      tagList(tags$p(HTML(sprintf("<strong>%s</strong> &nbsp; (β<sub>V</sub>=%s, β<sub>L1</sub>=%s, β<sub>L2</sub>=%s)",
        i18n_r()$t("Three phases (vapour-liquid-liquid)"), fmt_num(fl$beta_V, 3), fmt_num(fl$beta_L1, 3), fmt_num(fl$beta_L2, 3)))),
        DT::dataTableOutput(ns("flash_table")))
    tagList(result_header(ctx), cond, body)
  })

  output$flash_table <- DT::renderDataTable({
    ctx <- eos_state$ctx; fr <- eos_state$flash; req(ctx, fr, fr$fl, fr$fl$nphase %in% c(2, 3))
    fl <- fr$fl
    if (fl$nphase == 2) {
      K <- fl$y / fl$x
      df <- data.frame(Component = disp_name(ctx$nms), x = vapply(fl$x, fmt_num, character(1)),
        y = vapply(fl$y, fmt_num, character(1)), K = vapply(K, fmt_num, character(1)), check.names = FALSE)
      hdr <- list(c(i18n_r()$t("Component"), i18n_r()$t("Component")),
        c("<i>x</i>", i18n_r()$t("Liquid-phase mole fraction")),
        c("<i>y</i>", i18n_r()$t("Vapour-phase mole fraction")),
        c("<i>K</i> = <i>y</i>/<i>x</i>", i18n_r()$t("Distribution coefficient K = y/x")))
    } else {
      df <- data.frame(Component = disp_name(ctx$nms), y = vapply(fl$y, fmt_num, character(1)),
        x1 = vapply(fl$x1, fmt_num, character(1)), x2 = vapply(fl$x2, fmt_num, character(1)), check.names = FALSE)
      hdr <- list(c(i18n_r()$t("Component"), i18n_r()$t("Component")),
        c("<i>y</i>", i18n_r()$t("Vapour-phase mole fraction")),
        c("<i>x</i><sub>1</sub>", i18n_r()$t("Liquid-1 mole fraction")),
        c("<i>x</i><sub>2</sub>", i18n_r()$t("Liquid-2 mole fraction")))
    }
    sketch <- htmltools::withTags(table(class = "display",
      thead(tags$tr(lapply(hdr, function(h) tags$th(HTML(h[1]), title = h[2], style = "cursor:help;"))))))
    DT::datatable(df, container = sketch, rownames = FALSE, escape = FALSE, extensions = "Buttons",
      options = list(dom = "Bt", paging = FALSE, ordering = FALSE, language = tablang(),
        columnDefs = list(list(className = "dt-center", targets = "_all")),
        buttons = create_dt_export_buttons(i18n_r, "supercrit_phase_flash")))
  }, server = FALSE)

  output$pxy_result <- renderUI({
    ctx <- eos_state$ctx
    if (is.null(ctx)) return(placeholder("chart-area", i18n_r()$t("Pick solutes and conditions, then Calculate.")))
    if (!ctx$is_binary) return(tagList(result_header(ctx), binary_note()))
    pr <- eos_state$pxy; if (is.null(pr)) return(placeholder("spinner", i18n_r()$t("Computing...")))
    if (is.null(pr$px) || all(!is.finite(pr$px$P)))
      return(tagList(result_header(ctx), div(class = "alert alert-warning", i18n_r()$t("No two-phase envelope found in the pressure range."))))
    pxy_title <- h5(style = "margin:0;",
      tags$strong(HTML(paste(disp_name(ctx$nms), collapse = " + "))),
      HTML(sprintf(" %s %s °C", i18n_r()$t("at"), fmt_num(ctx$temp_c, 2))),
      tags$small(style = "color:#6c757d;", paste0("  (", model_pretty(ctx$model), ")")))
    tagList(plot_header_row(pxy_title, "pxy_plot", "pxy_data"),
      plotly::plotlyOutput(ns("pxy_plot"), height = "430px"),
      div(class = "alert alert-info", style = "margin-top:10px;", icon("info-circle"), " ",
        i18n_r()$t("Bubble curve: liquid x of CO2. Dew curve: vapour y of CO2.")))
  })

  output$locus_result <- renderUI({
    ctx <- eos_state$ctx
    if (is.null(ctx)) return(placeholder("chart-area", i18n_r()$t("Pick solutes and conditions, then Calculate.")))
    if (!ctx$is_binary) return(tagList(result_header(ctx), binary_note()))
    lr <- eos_state$locus; if (is.null(lr)) return(placeholder("spinner", i18n_r()$t("Computing...")))
    if (is.null(lr$loc) || all(!is.finite(lr$loc$P_atm)))
      return(tagList(result_header(ctx), div(class = "alert alert-warning", i18n_r()$t("No critical locus found in this temperature range."))))
    tagList(plot_header_row(result_title(ctx), "locus_plot", "locus_data"),
      plotly::plotlyOutput(ns("locus_plot"), height = "430px"),
      div(class = "alert alert-info", style = "margin-top:10px;", icon("info-circle"), " ",
        i18n_r()$t("Above the critical locus the mixture is a single supercritical fluid; below it a two-phase split (and separation) is possible.")))
  })

  output$pxy_plot <- plotly::renderPlotly({
    pr <- eos_state$pxy; req(pr, pr$px); d <- pr$px[is.finite(pr$px$P), , drop = FALSE]
    cx <- if (!is.null(pr$crit) && is.finite(pr$crit$x_c)) pr$crit$x_c else NA_real_
    cP <- if (!is.null(pr$crit) && is.finite(pr$crit$P_c)) atm2bar(pr$crit$P_c) else NA_real_
    # Keep only the SUB-critical branch (liquid x up to the critical composition) and draw in x-order.
    # Grid points past the apex (x1 > x_c) are beyond the critical point and do NOT lie on the two-phase
    # envelope; including them — especially after sorting by pressure — produced a fold/horizontal spur
    # near the top of the diagram. P is monotonic over the kept range, so x-order is the natural curve order.
    if (is.finite(cx)) d <- d[d$x1 <= cx + 1e-6, , drop = FALSE]
    d <- d[order(d$x1), , drop = FALSE]
    bx <- d$x1; dx <- d$y1; py <- atm2bar(d$P)
    # Close the envelope at the mixture critical point: bubble (liquid x) and dew (vapour y) meet at x = y = x_c.
    if (is.finite(cx) && is.finite(cP)) { bx <- c(bx, cx); dx <- c(dx, cx); py <- c(py, cP) }
    p <- plotly::plot_ly()
    p <- plotly::add_trace(p, x = bx, y = py, type = "scatter", mode = "lines+markers",
      name = i18n$t("bubble (liquid)"), line = list(color = "#2980b9"), marker = list(color = "#2980b9", size = 6))
    p <- plotly::add_trace(p, x = dx, y = py, type = "scatter", mode = "lines+markers",
      name = i18n$t("dew (vapour)"), line = list(color = "#e67e22"), marker = list(color = "#e67e22", size = 6))
    if (!is.null(pr$crit))
      p <- plotly::add_trace(p, x = pr$crit$x_c, y = atm2bar(pr$crit$P_c), type = "scatter", mode = "markers",
        name = sprintf("%s: %s bar @ x=%s", i18n$t("critical"), fmt_num(atm2bar(pr$crit$P_c), 1), fmt_num(pr$crit$x_c, 3)),
        marker = list(color = "#c0392b", size = 13, symbol = "x"))
    plotly::layout(p,
      xaxis = list(title = paste0("<i>x</i>, <i>y</i> &nbsp; ", co2_html, " (mol)"), range = c(0, 1), showline = TRUE, linecolor = "#bbb"),
      yaxis = list(title = "<i>P</i> (bar)", rangemode = "tozero", showline = TRUE, linecolor = "#bbb"),
      legend = list(x = 0.02, y = 0.98, xanchor = "left", yanchor = "top", bgcolor = "rgba(255,255,255,0.6)"),
      margin = list(l = 55, r = 10, t = 10, b = 45))
  })
  output$locus_plot <- plotly::renderPlotly({
    lr <- eos_state$locus; req(lr, lr$loc); d <- lr$loc[is.finite(lr$loc$P_atm), , drop = FALSE]
    p <- plotly::plot_ly(x = d$T - 273.15, y = atm2bar(d$P_atm), type = "scatter", mode = "lines+markers",
      line = list(color = "#c0392b", width = 2), marker = list(color = "#c0392b", size = 7),
      hovertemplate = "T = %{x:.1f} °C<br>Pc = %{y:.1f} bar<extra></extra>")
    plotly::layout(p, xaxis = list(title = "<i>T</i> (°C)", showline = TRUE, linecolor = "#bbb"),
      yaxis = list(title = paste0(i18n$t("critical"), " <i>P</i> (bar)"), rangemode = "tozero", showline = TRUE, linecolor = "#bbb"),
      margin = list(l = 55, r = 10, t = 10, b = 45))
  })

  # plot image export (client-side Plotly.downloadImage) + data CSV
  observeEvent(input$pxy_plot_img, shinyjs::runjs(sprintf(
    "var g=document.getElementById('%s'); var d=g&&(g.querySelector('.js-plotly-plot')||g); if(d&&window.Plotly)Plotly.downloadImage(d,{format:'png',filename:'supercrit_pxy',width:1000,height:700});", ns("pxy_plot"))))
  observeEvent(input$locus_plot_img, shinyjs::runjs(sprintf(
    "var g=document.getElementById('%s'); var d=g&&(g.querySelector('.js-plotly-plot')||g); if(d&&window.Plotly)Plotly.downloadImage(d,{format:'png',filename:'supercrit_critical_locus',width:1000,height:700});", ns("locus_plot"))))
  output$pxy_data <- downloadHandler(
    filename = function() paste0(generate_filename_with_timestamp("supercrit_pxy"), ".csv"),
    content = function(file) { pr <- eos_state$pxy; req(pr, pr$px)
      d <- pr$px; d$P_bar <- atm2bar(d$P); utils::write.csv(d[, c("x1", "y1", "P_bar")], file, row.names = FALSE) })
  output$locus_data <- downloadHandler(
    filename = function() paste0(generate_filename_with_timestamp("supercrit_critical_locus"), ".csv"),
    content = function(file) { lr <- eos_state$locus; req(lr, lr$loc)
      d <- lr$loc; d$T_C <- d$T - 273.15; d$Pc_bar <- atm2bar(d$P_atm)
      utils::write.csv(d[, c("T_C", "x_c", "Pc_bar")], file, row.names = FALSE) })

  # ---- Unified Export (CSV / PDF) — mirrors the on-screen flash table + P-x,y + critical-locus figures ----
  output$has_phase_results <- reactive({ !is.null(eos_state$flash) && !is.null(eos_state$flash$fl) })
  outputOptions(output, "has_phase_results", suspendWhenHidden = FALSE)

  # Flash phase-composition data.frame with PLAIN (non-HTML) localised headers, for PDF + CSV.
  .phase_flash_df <- function() {
    ctx <- eos_state$ctx; fr <- eos_state$flash
    if (is.null(ctx) || is.null(fr) || is.null(fr$fl) || !(fr$fl$nphase %in% c(2, 3))) return(NULL)
    fl <- fr$fl
    if (fl$nphase == 2) {
      out <- data.frame(disp_name(ctx$nms), signif(fl$x, 5), signif(fl$y, 5), signif(fl$y / fl$x, 5), check.names = FALSE, stringsAsFactors = FALSE)
      names(out) <- c(i18n_r()$t("Component"), "x", "y", "K = y/x")
    } else {
      out <- data.frame(disp_name(ctx$nms), signif(fl$y, 5), signif(fl$x1, 5), signif(fl$x2, 5), check.names = FALSE, stringsAsFactors = FALSE)
      names(out) <- c(i18n_r()$t("Component"), "y", "x₁", "x₂")
    }
    out
  }
  .phase_flash_title <- function() {
    fr <- eos_state$flash; if (is.null(fr) || is.null(fr$fl)) return(NULL)
    if (fr$fl$nphase == 2) i18n_r()$t("Two phases (vapour-liquid)")
    else if (fr$fl$nphase == 3) i18n_r()$t("Three phases (vapour-liquid-liquid)") else NULL
  }
  .phase_setup <- function() {
    ctx <- eos_state$ctx; if (is.null(ctx)) return(list())
    list(
      list(label = i18n_r()$t("Components"), value = paste(disp_name(ctx$nms), collapse = " + ")),
      list(label = i18n_r()$t("Temperature"), value = sprintf("%s °C", fmt_num(ctx$temp_c, 2))),
      list(label = i18n_r()$t("Pressure"), value = sprintf("%s bar", fmt_num(ctx$pres_bar, 1))),
      list(label = i18n_r()$t("EOS Model"), value = model_pretty(ctx$model)),
      list(label = i18n_r()$t("Basis"), value = if (isTRUE(ctx$mass)) i18n_r()$t("mass basis") else i18n_r()$t("mole basis")))
  }
  # ggplot twins of the plotly P-x,y and critical-locus figures (for the PDF; binary mixtures only).
  .phase_pxy_gg <- function() {
    pr <- eos_state$pxy; if (is.null(pr) || is.null(pr$px)) return(NULL)
    d <- pr$px[is.finite(pr$px$P), , drop = FALSE]; if (!nrow(d)) return(NULL)
    cx <- if (!is.null(pr$crit) && is.finite(pr$crit$x_c)) pr$crit$x_c else NA_real_
    cP <- if (!is.null(pr$crit) && is.finite(pr$crit$P_c)) atm2bar(pr$crit$P_c) else NA_real_
    if (is.finite(cx)) d <- d[d$x1 <= cx + 1e-6, , drop = FALSE]
    d <- d[order(d$x1), , drop = FALSE]
    bx <- d$x1; dx <- d$y1; py <- atm2bar(d$P)
    if (is.finite(cx) && is.finite(cP)) { bx <- c(bx, cx); dx <- c(dx, cx); py <- c(py, cP) }
    df <- rbind(data.frame(xy = bx, P = py, curve = i18n_r()$t("bubble (liquid)")),
                data.frame(xy = dx, P = py, curve = i18n_r()$t("dew (vapour)")))
    g <- ggplot2::ggplot(df, ggplot2::aes(x = xy, y = P, colour = curve)) +
      ggplot2::geom_line(linewidth = 0.7) + ggplot2::geom_point(size = 1.4) +
      ggplot2::scale_colour_manual(values = stats::setNames(c("#2980b9", "#e67e22"),
        c(i18n_r()$t("bubble (liquid)"), i18n_r()$t("dew (vapour)")))) +
      ggplot2::labs(x = expression(italic(x) * "," ~ italic(y) ~ "  CO"[2] ~ "(mol)"), y = "P (bar)", colour = NULL) +
      ggplot2::xlim(0, 1) + ggplot2::theme_minimal(base_size = 12) + ggplot2::theme(legend.position = "top")
    if (is.finite(cx) && is.finite(cP))
      g <- g + ggplot2::annotate("point", x = cx, y = cP, colour = "#c0392b", size = 3, shape = 4, stroke = 1.2)
    g
  }
  .phase_locus_gg <- function() {
    lr <- eos_state$locus; if (is.null(lr) || is.null(lr$loc)) return(NULL)
    d <- lr$loc[is.finite(lr$loc$P_atm), , drop = FALSE]; if (!nrow(d)) return(NULL)
    ggplot2::ggplot(data.frame(T = d$T - 273.15, P = atm2bar(d$P_atm)), ggplot2::aes(x = T, y = P)) +
      ggplot2::geom_line(colour = "#c0392b", linewidth = 0.8) + ggplot2::geom_point(colour = "#c0392b", size = 1.6) +
      ggplot2::labs(x = "T (°C)", y = paste0(i18n_r()$t("critical"), " P (bar)")) +
      ggplot2::theme_minimal(base_size = 12)
  }

  output$export_pdf <- downloadHandler(
    filename = function() paste0(generate_filename_with_timestamp("supercrit_phase_report"), ".pdf"),
    content = function(file) {
      req(eos_state$flash)
      pid <- showNotification(i18n$t("Preparing export..."), type = "message", duration = NULL); on.exit(removeNotification(pid), add = TRUE)
      tryCatch({
        logo <- if (file.exists("www/altraflora-15-min.png")) "www/altraflora-15-min.png" else NULL
        ver  <- tryCatch(paste0("supeRcrit v", as.character(utils::packageVersion("supeRcrit"))), error = function(e) "supeRcrit")
        # Make sure BOTH binary diagrams exist so neither is silently absent from the report (e.g. the critical
        # locus tab was never opened). compute_* are no-ops for non-binary mixtures / when already computed.
        if (!is.null(eos_state$ctx) && isTRUE(eos_state$ctx$is_binary)) {
          if (is.null(eos_state$pxy)) compute_pxy()
          if (is.null(eos_state$locus)) compute_locus()
        }
        plots <- list()
        pg <- tryCatch(.phase_pxy_gg(), error = function(e) NULL)
        if (!is.null(pg)) plots <- c(plots, list(list(plot = pg, title = i18n_r()$t("P-x,y phase envelope"), height = 3.6)))
        lg <- tryCatch(.phase_locus_gg(), error = function(e) NULL)
        if (!is.null(lg)) plots <- c(plots, list(list(plot = lg, title = i18n_r()$t("Critical locus"), height = 3.4)))
        export <- list(
          intro = i18n_r()$t("Phase-equilibrium results from the GC-EOS / GCA-EOS engine."),
          setup = .phase_setup(), flash_df = .phase_flash_df(), flash_title = .phase_flash_title(), plots = plots)
        build_eos_phase_pdf_report(export, i18n_r(), file, company = ver, logo = logo)
      }, error = function(e) showNotification(paste(i18n$t("Error exporting plots:"), e$message), type = "error", duration = 8))
    })

  output$export_csv <- downloadHandler(
    filename = function() paste0(generate_filename_with_timestamp("supercrit_phase_results"), ".csv"),
    content = function(file) {
      con <- file(file, "wb"); on.exit(close(con), add = TRUE)
      writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con)                                   # UTF-8 BOM
      wblock <- function(title, df) { if (is.null(df) || !nrow(df)) return(invisible())
        writeBin(charToRaw(enc2utf8(paste0('"', gsub('"', '""', title), '"\r\n'))), con)
        tmp <- tempfile(fileext = ".csv"); on.exit(unlink(tmp), add = TRUE)
        utils::write.csv(df, tmp, row.names = FALSE, na = "", fileEncoding = "UTF-8")
        writeBin(readBin(tmp, "raw", n = file.info(tmp)$size), con); writeBin(charToRaw("\r\n"), con) }
      # Force-compute both binary diagrams if a tab was never opened, so they are always in the CSV too.
      if (!is.null(eos_state$ctx) && isTRUE(eos_state$ctx$is_binary)) {
        if (is.null(eos_state$pxy)) compute_pxy()
        if (is.null(eos_state$locus)) compute_locus()
      }
      wblock(i18n_r()$t("Flash equilibrium"), .phase_flash_df())
      pr <- eos_state$pxy; if (!is.null(pr) && !is.null(pr$px)) { d <- pr$px; d$P_bar <- atm2bar(d$P)
        wblock(i18n_r()$t("P-x,y phase envelope"), d[, c("x1", "y1", "P_bar")]) }
      lr <- eos_state$locus; if (!is.null(lr) && !is.null(lr$loc)) { d <- lr$loc; d$T_C <- d$T - 273.15; d$Pc_bar <- atm2bar(d$P_atm)
        wblock(i18n_r()$t("Critical locus"), d[, c("T_C", "x_c", "Pc_bar")]) }
    },
    contentType = "text/csv")

  # ---- help / reset -----------------------------------------------------
  output$eos_phase_HELP <- renderUI({
    helper(shiny_tag = "", buttonLabel = "OK",
      content = i18n_r()$t("eos_phase_help_en"), type = "markdown", size = "l",
      style = "color:white; font-size:20px; vertical-align:middle; margin-top:0; margin-right:3%;")
  })
  outputOptions(output, "eos_phase_HELP", suspendWhenHidden = FALSE)

  output$accordion_toggle_btn <- renderUI({ create_accordion_toggle_btn(ns, i18n_r, "phase_accordion") })
  outputOptions(output, "accordion_toggle_btn", suspendWhenHidden = FALSE)

  observeEvent(input$reset, {
    updateNumericInput(session, "temp_c", value = 50); updateNumericInput(session, "pres_bar", value = 100)
    updateNumericInput(session, "tmin_c", value = 20); updateNumericInput(session, "tmax_c", value = 90)
    updateNumericInput(session, "npts", value = 6); updateCheckboxInput(session, "mass_basis", value = FALSE)
    updateRadioButtons(session, "locus_mode", selected = "range")
    eos_state$ctx <- NULL; eos_state$flash <- NULL; eos_state$pxy <- NULL; eos_state$locus <- NULL
  })
}
