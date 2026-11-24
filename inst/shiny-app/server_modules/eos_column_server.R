# Process Simulation - Fractionation Column Server Module
# Counter-current supercritical-CO2 fractionation column solved by the
# Naphtali-Sandholm simultaneous-correction method (gceos_column_ns_solve) over
# the GC(A)-EOS. Feed (oil) solutes come from the session store; CO2 is the
# implicit solvent. Hydraulic sizing via gceos_column_sizing. Conventions mirror
# the Phase Behaviour module.
eos_column_server <- function(input, output, session, defaults, i18n, tablang, eos_rv) {

  ns <- session$ns
  i18n_r <- reactive({ i18n })

  bar2atm <- function(b) b * 0.986923
  model_pretty <- function(m) switch(m %||% "gceos",
    gcaeos = "GCA-EOS", `gceos-fae` = "GC-EOS (esters)", `gceos-hmw` = "GC-EOS (high-MW)",
    diaz2005 = "GC-EOS (Diaz terpenes)", ganan2011 = "GC-EOS (Ganan terpenes)", "GC-EOS")
  co2_html <- "CO<sub>2</sub>"
  disp_name <- function(nm) ifelse(nm == "CO2", co2_html, nm)
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
  CO2_TC <- 304.2; CO2_DC <- 3.1289; CO2_MW <- 44.01
  T_RANGE <- c(0, 300); P_RANGE <- c(1, 1000); MAX_SOLUTES <- 8

  # HETP (m) is a user-supplied value (measured or otherwise calculated) that links
  # theoretical stages <-> packed height (height = stages x HETP). There is no reliable
  # first-principles HETP for SCF columns, so it is an input, not a prediction.
  hetp_val <- reactive({ v <- input$hetp; if (is.null(v) || is.na(v) || v <= 0) (defaults$hetp %||% 0.685) else as.numeric(v) })
  # N from the spec mode: theoretical stages directly, or packed height / HETP
  n_from_spec <- function() {
    if ((input$spec_mode %||% "stages") == "height") max(2L, as.integer(round((input$col_height %||% 19) / hetp_val())))
    else round(input$nstages %||% (defaults$nstages %||% 20))
  }
  # Stage numbers for the solver. In height mode the oil/solvent feeds are given as a
  # height (m) from the top and converted to the nearest stage via the HETP (stage = h/HETP).
  feed_stages <- function() {
    N <- n_from_spec(); hetp <- hetp_val()
    if ((input$spec_mode %||% "stages") == "height") {
      to_stage <- function(h) { if (is.null(h) || is.na(h)) h <- 0; max(1L, min(N, as.integer(round(h / hetp)))) }
      list(N = N, feedL = to_stage(input$feedl_h %||% 5.5), feedV = to_stage(input$feedv_h %||% (input$col_height %||% 19)))
    } else {
      list(N = N, feedL = round(input$feedl %||% 8), feedV = round(input$feedv %||% N))
    }
  }

  eos_state <- reactiveValues(result = NULL, optim = NULL)

  # ---- solute registry (session components, with MW) -------------------
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

  # ---- range badge + recolor (label_node may be HTML / i18n tag) -------
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
  observeEvent(input$sf, recolor("sf_badge", input$sf, 1, 3000), ignoreInit = TRUE)
  observeEvent(input$col_height, recolor("height_badge", input$col_height, 0.5, 50), ignoreInit = TRUE)
  observeEvent(input$feedl_h, recolor("feedl_h_badge", input$feedl_h, 0, 50), ignoreInit = TRUE)
  observeEvent(input$feedv_h, recolor("feedv_h_badge", input$feedv_h, 0, 50), ignoreInit = TRUE)
  # Keep the feed stages valid as the stage count changes (either spec mode):
  #  - the SOLVENT normally enters at the bottom (stage N); if it was at the old
  #    bottom, let it follow N so it doesn't get stranded mid-column (the stages
  #    below a solvent inlet are starved of vapour and the column won't converge);
  #  - otherwise just clamp anything that now exceeds N.
  last_feed_N <- reactiveVal(round(defaults$nstages %||% 20))
  last_height <- reactiveVal(defaults$col_height %||% 19)
  clamp_feed_stages <- function() {
    N <- n_from_spec(); prevN <- last_feed_N(); fv <- input$feedv
    if (!is.null(fv) && !is.na(fv)) {
      if (!is.null(prevN) && fv == prevN && fv != N) updateNumericInput(session, "feedv", value = N)  # was at bottom -> follow
      else if (fv > N) updateNumericInput(session, "feedv", value = N)                                 # clamp into range
    }
    if (!is.null(input$feedl) && !is.na(input$feedl) && input$feedl > N) updateNumericInput(session, "feedl", value = N)
    # height-mode feeds (metres): same logic against the packed height H
    H <- input$col_height; prevH <- last_height()
    if (!is.null(H) && !is.na(H)) {
      fvh <- input$feedv_h
      if (!is.null(fvh) && !is.na(fvh)) {
        if (!is.null(prevH) && abs(fvh - prevH) < 1e-9 && abs(fvh - H) > 1e-9) updateNumericInput(session, "feedv_h", value = H)  # at bottom -> follow
        else if (fvh > H) updateNumericInput(session, "feedv_h", value = H)
      }
      if (!is.null(input$feedl_h) && !is.na(input$feedl_h) && input$feedl_h > H) updateNumericInput(session, "feedl_h", value = H)
      last_height(H)
    }
    last_feed_N(N)
  }
  observeEvent(input$nstages, clamp_feed_stages(), ignoreInit = TRUE)
  observeEvent(list(input$col_height, input$hetp, input$spec_mode), clamp_feed_stages(), ignoreInit = TRUE)
  observeEvent(input$ttop_c, recolor("ttop_badge", input$ttop_c, T_RANGE[1], T_RANGE[2]), ignoreInit = TRUE)
  observeEvent(input$tbot_c, recolor("tbot_badge", input$tbot_c, T_RANGE[1], T_RANGE[2]), ignoreInit = TRUE)

  # ---- solute selection (checkbox, max 3) + load / remove --------------
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
        checkboxGroupInput(ns("solutes"), label = sprintf(i18n_r()$t("Select up to %d feed solutes"), MAX_SOLUTES), choices = choices, selected = sel)),
      div(style = "margin-top:-4px; display:flex; gap:6px; flex-wrap:wrap;", del, save_btn)
    )
  })
  outputOptions(output, "components_ui", suspendWhenHidden = FALSE)

  observeEvent(input$solutes, {
    shinyjs::toggleState("remove_solutes", condition = length(input$solutes) > 0)
    shinyjs::toggleState("save_selected_solutes", condition = length(input$solutes) > 0)
  }, ignoreNULL = FALSE)
  observeEvent(input$solutes, {
    n <- length(input$solutes)
    if (n > MAX_SOLUTES) { updateCheckboxGroupInput(session, "solutes", selected = input$solutes[1:MAX_SOLUTES])
      showNotification(sprintf(i18n$t("At most %d feed solutes can be selected."), MAX_SOLUTES), type = "warning", duration = 2); return() }
    shinyjs::runjs(sprintf(
      "var g=$('#%s'); if(%d>=%d){g.find('input:checkbox:not(:checked)').prop('disabled',true);} else {g.find('input:checkbox').prop('disabled',false);}",
      ns("solutes"), n, MAX_SOLUTES))
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

  # Save the selected feed components to disk (one JSON each; round-trips via "Load from Disk").
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

  # ---- feed composition (oil, solvent-free) ----------------------------
  output$composition_ui <- renderUI({
    reg <- solute_registry(); sel <- (input$solutes %||% character(0)); sel <- sel[sel %in% names(reg)]
    sol_nm <- vapply(reg[sel], function(c) c$name, character(1)); n <- length(sel)
    labs <- lapply(seq_len(MAX_SOLUTES), function(i) if (i <= n) sol_nm[i] else sprintf(i18n_r()$t("Solute %d"), i))
    # Component 1 is the balance and is ALWAYS disabled: its fraction = 1 - sum(others),
    # kept in sync live (see the observer below) so the feed always totals 1.
    # Components 2+ are editable and default to 0.
    others0 <- sum(vapply(setdiff(seq_len(MAX_SOLUTES), 1L), function(i)
      if (i <= n) { v <- isolate(input[[paste0("z", i)]]); if (is.null(v) || is.na(v)) 0 else as.numeric(v) } else 0, numeric(1)))
    zval <- function(i) if (i == 1) round(1 - others0, 6) else if (i <= n) isolate(input[[paste0("z", i)]]) %||% 0 else 0
    fld <- function(i) { ni <- numericInput(ns(paste0("z", i)), labs[[i]],
      value = zval(i), min = 0, max = 1, step = 0.01)
      if (i == 1 || i > n) shinyjs::disabled(ni) else ni }   # comp 1 always disabled; unselected slots disabled
    # Lay the composition fields out three-per-row (each row its own fluidRow) so that with more than
    # three solutes the fields wrap into clean rows — a single fluidRow can snag when a long solute name
    # wraps its label to two lines and makes columns unequal height.
    comp_rows <- lapply(split(seq_len(MAX_SOLUTES), (seq_len(MAX_SOLUTES) - 1L) %/% 3L),
      function(ridx) fluidRow(lapply(ridx, function(i) column(4, fld(i)))))
    tagList(
      comp_rows,
      div(style = "display:flex; align-items:center; gap:16px; margin-top:4px;",
        uiOutput(ns("total_badge"), inline = TRUE),
        div(class = "eos-mass-toggle",
          checkboxInput(ns("mass_basis"), i18n_r()$t("Mass fractions"), value = isolate(input$mass_basis) %||% FALSE)))
    )
  })
  outputOptions(output, "composition_ui", suspendWhenHidden = FALSE)

  # Component 1 absorbs the balance: whenever an editable fraction (2+) changes,
  # set z1 = 1 - sum(others) so the oil feed always totals 1 in real time.
  observeEvent(lapply(setdiff(seq_len(MAX_SOLUTES), 1L), function(i) input[[paste0("z", i)]]), {
    reg <- solute_registry(); sel <- (input$solutes %||% character(0)); sel <- sel[sel %in% names(reg)]; n <- length(sel)
    oth <- sum(vapply(setdiff(seq_len(MAX_SOLUTES), 1L), function(i)
      if (i <= n) { v <- input[[paste0("z", i)]]; if (is.null(v) || is.na(v)) 0 else as.numeric(v) } else 0, numeric(1)))
    z1 <- round(1 - oth, 6)
    if (is.null(input$z1) || is.na(input$z1) || abs(input$z1 - z1) > 1e-9) updateNumericInput(session, "z1", value = z1)
  }, ignoreInit = TRUE)

  comp_total <- reactive({
    reg <- solute_registry(); sel <- (input$solutes %||% character(0)); sel <- sel[sel %in% names(reg)]
    n <- length(sel)
    sum(vapply(seq_len(n), function(i) { v <- input[[paste0("z", i)]]; if (is.null(v) || is.na(v)) 0 else as.numeric(v) }, numeric(1)))
  })
  # comp 1 (the balance) is negative when components 2+ exceed 1 -> invalid feed
  comp_balance_ok <- reactive({ v <- input$z1; is.null(v) || is.na(v) || v >= -1e-9 })
  output$total_badge <- renderUI({
    tot <- comp_total(); ok <- abs(tot - 1) < 1e-6 && comp_balance_ok()
    tags$span(strong(i18n_r()$t("Total fraction:")), " ",
      span(fmt_num(tot, 4), style = sprintf("background-color:%s; color:white; font-weight:bold; padding:2px 8px; border-radius:4px;",
        if (ok) "#28a745" else "#dc3545")))
  })

  # ---- assemble the column problem (comps, feeds) from the inputs -------
  # Shared by Run and Optimize. Returns NULL if the selection is unusable
  # (no solutes or a model conflict); callers validate ranges/sums first.
  assemble_comps <- function() {
    reg <- solute_registry(); sel <- (input$solutes %||% character(0)); sel <- sel[sel %in% names(reg)]
    if (length(sel) < 1) return(NULL)
    rm <- resolve_model(); if (rm$conflict) return(NULL)
    selc <- reg[sel]; nsol <- length(sel)
    nms <- make.unique(c(vapply(selc, function(c) c$name, character(1)), "CO2"))
    comps <- stats::setNames(c(
      lapply(selc, function(c) list(groups = c$groups, dc = c$dc, Tc = c$Tc, n = 1, MW = c$mw)),
      list(list(groups = c(CO2 = 1), dc = CO2_DC, Tc = CO2_TC, n = 1, MW = CO2_MW))), nms)
    zin <- vapply(seq_len(nsol), function(i) as.numeric(input[[paste0("z", i)]] %||% 0), numeric(1))
    mass_ok <- TRUE
    if (isTRUE(input$mass_basis)) {
      mws <- vapply(selc, function(c) c$mw, numeric(1))
      if (!any(is.na(mws)) && all(mws > 0)) zin <- zin / mws else mass_ok <- FALSE
    }
    oil_z <- zin / sum(zin)
    list(comps = comps, nms = nms, sol_names = vapply(selc, function(c) c$name, character(1)),
         oil_z = oil_z, zL = c(oil_z, 0), zV = c(rep(0, nsol), 1), nsol = nsol, selc = selc,
         model = rm$model, MW = vapply(comps, function(c) c$MW, numeric(1)), mass_ok = mass_ok)
  }

  # ---- S/F unit conversion: S/F(mass kg/kg) = S/F(mol) * MW_CO2 / MW_oil ----
  mw_oil <- reactive({                          # avg molar mass of the oil feed (g/mol)
    prob <- assemble_comps(); if (is.null(prob)) return(NA_real_)
    mws <- prob$MW[seq_len(prob$nsol)]
    if (any(is.na(mws)) || sum(prob$oil_z) <= 0) return(NA_real_)
    sum(prob$oil_z * mws)
  })
  # S/F has its own mol/mass toggle in Column parameters (independent of the composition
  # basis). The Optimization unit display follows this same toggle (input$sf_unit).
  cur_sf_unit <- function() input$sf_unit %||% "mass"
  sf_to_molar <- function(val) {                # entered S/F -> the molar value the solver needs
    if (cur_sf_unit() != "mass") return(val)
    mo <- mw_oil(); if (is.na(mo) || mo <= 0) return(val)
    val * mo / CO2_MW
  }
  sf_unit_lbl <- function(u) if (identical(u %||% "mass", "mass")) "mass" else "mol"   # display unit
  # instantaneous conversion of the displayed value (and optimisation bounds) when toggled
  sf_unit_seen <- reactiveVal(FALSE)
  observeEvent(input$sf_unit, {
    # sf_unit is built via renderUI, so its initial appearance reaches this observer as a
    # "change" even with ignoreInit. Swallow that first firing so it doesn't raise a spurious
    # startup warning or override the user's default basis.
    if (!sf_unit_seen()) { sf_unit_seen(TRUE); return() }
    cur <- input$sf; if (is.null(cur) || length(cur) == 0 || is.na(cur)) return()
    mo <- mw_oil()
    if (is.na(mo) || mo <= 0) {
      if ((input$sf_unit %||% "mol") == "mass") {
        showNotification(i18n$t("Feed molecular weight unavailable; the S/F mass basis needs component MW."), type = "warning", duration = 4)
        updateRadioButtons(session, "sf_unit", selected = "mol")
      }
      return()
    }
    f <- CO2_MW / mo                            # S/F(mass) = S/F(mol) * f
    conv <- function(v) signif(if (input$sf_unit == "mass") v * f else v / f, 5)
    updateNumericInput(session, "sf", value = conv(cur))
    for (k in c("opt_sf_lo", "opt_sf_hi")) { v <- input[[k]]
      if (!is.null(v) && length(v) == 1 && !is.na(v)) updateNumericInput(session, k, value = conv(v)) }
  }, ignoreInit = TRUE)

  # ---- column parameters + thermal gradient ----------------------------
  output$column_params_ui <- renderUI({
    tagList(
      # --- HETP (always editable) + how the column is specified ---
      fluidRow(
        column(6, numericInput(ns("hetp"),
          tags$span("HETP (m)", input_help(i18n_r()$t("Height equivalent to a theoretical plate: enter a measured or otherwise calculated value for your packing and system. It links theoretical stages and packed height (height = stages x HETP). There is no reliable first-principles HETP for SCF columns."), buttonLabel = i18n_r()$t("OK"))),
          value = isolate(input$hetp) %||% (defaults$hetp %||% 0.685), min = 0.02, max = 5, step = 0.01)),
        column(6,
          tags$label(class = "control-label", style = "display:block;", i18n_r()$t("Specify column by")),
          # flex box the height of one form-control, contents vertically centred -> radios
          # line up with the HETP input regardless of font/zoom (no pixel offset)
          div(class = "eos-radio-center",
            radioButtons(ns("spec_mode"), label = NULL, inline = TRUE,
              choices = stats::setNames(c("stages", "height"), c(i18n_r()$t("Theoretical stages"), i18n_r()$t("Packed height"))),
              selected = isolate(input$spec_mode) %||% (defaults$spec_mode %||% "stages"))))
      ),
      fluidRow(
        column(6,
          conditionalPanel("input.spec_mode != 'height'", ns = ns,
            badge_num("nstages", "nstages_badge", i18n_r()$t("Stages"), "Number of equilibrium stages (stage 1 = top).",
              isolate(input$nstages) %||% (defaults$nstages %||% 20), 3, 80, 1)),
          conditionalPanel("input.spec_mode == 'height'", ns = ns,
            div(class = "eos-height-spec",                                 # tightens the gap to the derived-N note
              badge_num("col_height", "height_badge", HTML(paste0(i18n_r()$t("Packed height"), " (m)")),
                "Total packed height of the column; the theoretical stage count is the height divided by the packing HETP.",
                isolate(input$col_height) %||% (defaults$col_height %||% 19), 0.5, 50, 0.5),
              uiOutput(ns("derived_n_note"))))),
        column(6,
          conditionalPanel("input.spec_mode != 'height'", ns = ns,
            badge_num("feedl", "feedl_badge", i18n_r()$t("Oil feed stage"), "Stage where the liquid (oil) feed enters; above it is the rectifying section.",
              isolate(input$feedl) %||% (defaults$feedl %||% 8), 1, 80, 1)),
          conditionalPanel("input.spec_mode == 'height'", ns = ns,
            badge_num("feedl_h", "feedl_h_badge", HTML(paste0(i18n_r()$t("Oil feed height"), " (m)")),
              "Height from the top at which the liquid (oil) feed enters; converted to the nearest stage via the HETP.",
              isolate(input$feedl_h) %||% (defaults$feedl_h %||% 5.5), 0, 50, 0.5)))
      ),
      fluidRow(
        column(6,
          conditionalPanel("input.spec_mode != 'height'", ns = ns,
            badge_num("feedv", "feedv_badge", i18n_r()$t("Solvent feed stage"), "Stage where the CO2 solvent enters (default = bottom stage N).",
              isolate(input$feedv) %||% (defaults$feedv %||% (isolate(input$nstages) %||% (defaults$nstages %||% 20))), 1, 80, 1)),
          conditionalPanel("input.spec_mode == 'height'", ns = ns,
            badge_num("feedv_h", "feedv_h_badge", HTML(paste0(i18n_r()$t("Solvent feed height"), " (m)")),
              "Height from the top at which the CO2 solvent enters; the default is the bottom (= the packed height).",
              isolate(input$feedv_h) %||% (defaults$feedv_h %||% 19), 0, 50, 0.5))),
        column(6, badge_num("reflux", "reflux_badge", i18n_r()$t("Reflux ratio"), "External reflux ratio R/E (0 = no reflux). Higher reflux sharpens the separation. Reflux and product are the same solvent-free condensate, so this ratio is identical on a molar or mass basis (no unit toggle needed).",
          isolate(input$reflux) %||% (defaults$reflux %||% 2), 0, 50, 0.1))
      ),
      fluidRow(
        column(6, {
          sfval <- isolate(input$sf) %||% (defaults$sf %||% 100)
          sfbad <- is.null(sfval) || is.na(sfval) || sfval < 1 || sfval > 3000
          tags$div(
            # header row: label + help on the left, mol/mass toggle + range badge on the right
            tags$div(style = "display:flex; align-items:center; justify-content:space-between; width:100%; margin-bottom:5px;",
              tags$span(class = "control-label", style = "margin:0; font-weight:700;",
                HTML("S/F"), input_help(i18n_r()$t("Solvent-to-feed ratio (CO2 / oil feed). Higher S/F extracts more. Choose mol (mol/mol) or mass (kg/kg); toggling converts the value. The Optimization S/F unit follows this toggle."), buttonLabel = i18n_r()$t("OK"))),
              tags$div(style = "display:flex; align-items:center; gap:8px;",
                div(class = "eos-sf-unit",
                  radioButtons(ns("sf_unit"), label = NULL, inline = TRUE,
                    choices = stats::setNames(c("mol", "mass"), c(i18n_r()$t("mol"), i18n_r()$t("mass"))),
                    selected = isolate(input$sf_unit) %||% (defaults$sf_unit %||% "mass"))),
                tags$span("1–3000", id = ns("sf_badge"),
                  style = sprintf("font-size:10px; padding:1px 5px; border-radius:3px; background-color:%s; color:white; font-weight:normal;",
                                  if (sfbad) "#dc3545" else "#6c757d")))),
            numericInput(ns("sf"), label = NULL, value = sfval, min = 1, max = 3000, step = 5))
        }),
        column(6, badge_num("temp_c", "temp_badge", i18n_r()$t("Temperature (°C)"), "Column temperature (isothermal, or the reference for the gradient).",
          isolate(input$temp_c) %||% (defaults$temp_c %||% 61), T_RANGE[1], T_RANGE[2], 1))
      ),
      fluidRow(
        column(6, badge_num("pres_bar", "pres_badge", i18n_r()$t("Pressure (bar)"), "Column pressure.",
          isolate(input$pres_bar) %||% (defaults$pres_bar %||% 100), P_RANGE[1], P_RANGE[2], 1))
      )
    )
  })
  outputOptions(output, "column_params_ui", suspendWhenHidden = FALSE)

  output$derived_n_note <- renderUI({
    h <- input$col_height; hetp <- hetp_val()
    if (is.null(h) || is.na(h) || is.null(hetp) || hetp <= 0) return(NULL)
    tags$small(class = "eos-derived-n", style = "display:block; color:#6c757d;",
      HTML(sprintf("&asymp; <strong>%d</strong> %s (HETP %.3f m)",
        max(2L, as.integer(round(h / hetp))), i18n_r()$t("theoretical stages"), hetp)))
  })
  outputOptions(output, "derived_n_note", suspendWhenHidden = FALSE)

  output$gradient_ui <- renderUI({
    grad <- isolate(input$gradient) %||% FALSE
    tagList(
      div(class = "eos-mass-toggle", style = "margin-top:2px;",
        checkboxInput(ns("gradient"), i18n_r()$t("Impose a temperature gradient (top -> bottom)"), value = grad)),
      conditionalPanel("input.gradient == true", ns = ns,
        fluidRow(
          column(6, badge_num("ttop_c", "ttop_badge", HTML(paste0("<i>T</i><sub>top</sub> (°C)")), "Temperature at the top stage (stage 1).",
            isolate(input$ttop_c) %||% (defaults$ttop_c %||% 66), T_RANGE[1], T_RANGE[2], 1)),
          column(6, badge_num("tbot_c", "tbot_badge", HTML(paste0("<i>T</i><sub>bottom</sub> (°C)")), "Temperature at the bottom stage (stage N).",
            isolate(input$tbot_c) %||% (defaults$tbot_c %||% 52), T_RANGE[1], T_RANGE[2], 1))
        ))
    )
  })
  outputOptions(output, "gradient_ui", suspendWhenHidden = FALSE)

  # ---- advanced solver controls (collapsible) --------------------------
  output$advanced_ui <- renderUI({
    adv <- isolate(input$show_advanced) %||% FALSE
    tagList(
      div(class = "eos-mass-toggle", style = "margin-top:4px;",
        checkboxInput(ns("show_advanced"), i18n_r()$t("Show advanced solver options"), value = adv)),
      conditionalPanel("input.show_advanced == true", ns = ns,
        fluidRow(
          column(6, numericInput(ns("max_iter"),
            tags$span(i18n_r()$t("Max iterations"),
              input_help(i18n_r()$t("Maximum Naphtali-Sandholm iterations before giving up."), buttonLabel = i18n_r()$t("OK"))),
            value = isolate(input$max_iter) %||% (defaults$max_iter %||% 600), min = 50, max = 5000, step = 50)),
          column(6, selectInput(ns("tol"),
            tags$span(i18n_r()$t("Convergence tolerance"),
              input_help(i18n_r()$t("Residual tolerance for stage convergence (smaller = stricter)."), buttonLabel = i18n_r()$t("OK"))),
            choices = c("1e-6", "1e-7", "1e-8", "1e-9"), selected = isolate(input$tol) %||% (defaults$tol %||% "1e-8"))))
      ))
  })
  outputOptions(output, "advanced_ui", suspendWhenHidden = FALSE)

  validate_ranges <- function() {
    bad <- c()
    chk <- function(v, lo, hi, lbl) if (is.null(v) || is.na(v) || v < lo || v > hi) bad <<- c(bad, i18n$t(lbl))
    chk(input$temp_c, T_RANGE[1], T_RANGE[2], "Temperature (°C)")
    chk(input$pres_bar, P_RANGE[1], P_RANGE[2], "Pressure (bar)")
    if (isTRUE(input$gradient)) { chk(input$ttop_c, T_RANGE[1], T_RANGE[2], "T (°C)"); chk(input$tbot_c, T_RANGE[1], T_RANGE[2], "T (°C)") }
    if (length(bad) == 0) return(NULL)
    paste(i18n$t("Out of range:"), paste(unique(bad), collapse = ", "))
  }

  # ---- run the column --------------------------------------------------
  observeEvent(input$run, {
    reg <- solute_registry(); sel <- (input$solutes %||% character(0)); sel <- sel[sel %in% names(reg)]
    if (length(sel) < 1) { showNotification(i18n$t("Select at least one feed solute."), type = "error"); return() }
    rng_err <- validate_ranges(); if (!is.null(rng_err)) { showNotification(rng_err, type = "error"); return() }
    if (!comp_balance_ok()) { showNotification(i18n$t("Feed fractions of components 2+ exceed 1; component 1 (the balance) would be negative."), type = "error"); return() }
    if (abs(comp_total() - 1) >= 1e-6) { showNotification(i18n$t("Feed fractions must sum to exactly 1."), type = "error"); return() }
    rm <- resolve_model(); if (rm$conflict) {
      showNotification(i18n$t("The selected components were built with different EOS models. Pick components of the same model."), type = "error"); return() }
    fs <- feed_stages(); Nst <- fs$N; feedL <- fs$feedL; feedV <- fs$feedV
    if ((input$spec_mode %||% "stages") == "height" && (is.null(input$col_height) || is.na(input$col_height) || input$col_height < 0.5 || input$col_height > 50)) {
      showNotification(paste(i18n$t("Out of range:"), i18n$t("Packed height")), type = "error"); return() }
    if (Nst < 2) { showNotification(i18n$t("The packed height is too small for even two theoretical stages at this HETP."), type = "error"); return() }
    if (feedL > Nst) { showNotification(i18n$t("The feed stage cannot exceed the number of stages."), type = "error"); return() }
    if (feedV < 1 || feedV > Nst) { showNotification(i18n$t("The solvent feed stage must be between 1 and the number of stages."), type = "error"); return() }

    prob <- assemble_comps()
    if (is.null(prob)) { showNotification(i18n$t("Select at least one feed solute."), type = "error"); return() }
    if (!prob$mass_ok) showNotification(i18n$t("Molecular weight unavailable; using the values as mole fractions."), type = "warning", duration = 4)
    comps <- prob$comps; nms <- prob$nms; model <- prob$model; nsol <- prob$nsol
    oil_z <- prob$oil_z; zL <- prob$zL; zV <- prob$zV

    SF <- sf_to_molar(input$sf %||% 100); P_atm <- bar2atm(input$pres_bar %||% 100)
    T_K <- (input$temp_c %||% 61) + 273.15
    Tvec <- if (isTRUE(input$gradient))
      seq((input$ttop_c %||% 66) + 273.15, (input$tbot_c %||% 52) + 273.15, length.out = Nst) else rep(T_K, Nst)
    max_iter <- round(input$max_iter %||% 600); tol <- as.numeric(input$tol %||% "1e-8")
    refluxR <- input$reflux %||% 2

    withProgress(message = i18n$t("Solving the column..."), {
      r <- tryCatch(supeRcrit:::gceos_column_ns_solve(N = Nst, T = Tvec, P = P_atm, comps = comps, FL = 1, zL = zL,
        FV = SF, zV = zV, feedL = feedL, feedV = feedV, reflux = refluxR, solvent = "CO2",
        model = model, max_iter = max_iter, tol = tol), error = function(e) {
          showNotification(paste(i18n$t("Column solve failed:"), e$message), type = "error", duration = 6); NULL })

      # Robust fallback: a cold solve can stall at high S/F / reflux. Walk to the
      # target along an S/F homotopy, warm-starting each step (gceos_column_continuation).
      if (!is.null(r) && !isTRUE(r$converged)) {
        setProgress(message = i18n$t("Retrying via S/F continuation..."))
        rc <- tryCatch(supeRcrit:::gceos_column_continuation(N = Nst, T = Tvec, P = P_atm, comps = comps,
          FL = 1, zL = zL, zV = zV, target = SF, reflux = refluxR, var = "SF",
          feedL = feedL, feedV = feedV, solvent = "CO2", model = model,
          n_steps = 10L, max_iter = max_iter, tol = tol), error = function(e) NULL)
        if (!is.null(rc) && isTRUE(rc$converged)) r <- rc
      }
      if (is.null(r)) return()
      # Over-solvented column: at very high S/F the raffinate liquid vanishes and
      # the stage equations go singular (NaN solution). Report it cleanly rather
      # than rendering NaN compositions (which would crash the products table).
      finite_sol <- all(is.finite(as.numeric(r$raffinate$x))) && all(is.finite(as.numeric(r$recovery))) &&
        (is.null(r$extract$comp) || all(is.finite(as.numeric(r$extract$comp)))) &&
        (is.null(r$extract$y) || all(is.finite(as.numeric(r$extract$y))))
      if (!finite_sol) {
        showNotification(i18n$t("No physical two-phase solution at this S/F: the liquid product vanishes (column over-solvented). Reduce the S/F ratio."), type = "error", duration = 7)
        return()
      }
      eos_state$result <- list(col = r, comps = comps, nms = nms, sol_names = prob$sol_names,
        oil_z = oil_z, MW = prob$MW, zL = zL, nsol = nsol,
        N = Nst, feedL = feedL, feedV = feedV, reflux = refluxR, SF = SF,
        sf_disp = input$sf %||% 100, sf_unit = cur_sf_unit(),
        T_K = mean(Tvec), P_atm = P_atm,
        P_bar = input$pres_bar %||% 100, temp_c = input$temp_c, Tvec_C = Tvec - 273.15,
        model = model, gradient = isTRUE(input$gradient),
        HETP = hetp_val(), spec_mode = input$spec_mode %||% "stages", col_height = input$col_height)
      updateTabsetPanel(session, "result_tabs", selected = "products")
      if (r$status != "converged") {
        showNotification(sprintf(i18n$t("Column status: %s"), r$status), type = "warning", duration = 5)
        if (feedV < Nst)
          showNotification(i18n$t("Tip: the CO2 solvent normally enters at the bottom stage (= the number of stages). Stages below the solvent feed are starved of vapour and often prevent convergence."), type = "warning", duration = 9)
      }
    })
  })

  # ---- optimization ----------------------------------------------------
  OPT_DEFAULTS <- list(pres_bar = c(80, 300), temp = c(40, 80), sf = c(20, 1000), reflux = c(0, 10))
  OPT_LABELS   <- c(pres_bar = "Pressure (bar)", temp = "Temperature (°C)", sf = "S/F", reflux = "Reflux")
  OPT_OBJ_LBL  <- c(recovery = "Extract recovery", raff_recovery = "Raffinate recovery",
                    ext_purity = "Extract purity", raff_purity = "Raffinate purity")

  # build the scalar objective (to MAXIMISE) from a converged column result
  make_objective <- function(otype, solute_name, nms) {
    sidx <- length(nms); solnames <- nms[-sidx]
    sfree_vec <- function(vec) { v <- vec[-sidx]; s <- sum(v)
      if (!is.finite(s) || s <= 0) stats::setNames(rep(0, length(v)), solnames) else stats::setNames(v / s, solnames) }
    function(r) {
      val <- tryCatch({
        if (otype == "recovery") as.numeric(stats::setNames(r$recovery, nms)[solute_name])
        else if (otype == "raff_recovery") 1 - as.numeric(stats::setNames(r$recovery, nms)[solute_name])  # mass balance: 1 - extract
        else if (otype == "ext_purity") {
          ev <- if (!is.null(r$extract$comp)) as.numeric(r$extract$comp) else as.numeric(r$extract$y)
          as.numeric(sfree_vec(stats::setNames(ev, nms))[solute_name])
        } else as.numeric(sfree_vec(stats::setNames(as.numeric(r$raffinate$x), nms))[solute_name])
      }, error = function(e) NA_real_)
      if (!is.finite(val)) -1 else val
    }
  }

  # Combine one or more responses into a single score the optimiser MAXIMISES, via a
  # weighted-geometric-mean desirability (Derringer). Each response is in 0-1; its goal sets
  # the desirability: maximise -> d = y, minimise -> d = 1 - y. Then D = (prod d_i^w_i)^(1/sum w).
  # `dirs` is a character vector ("max"/"min") aligned with `otypes`. Any infeasible component
  # (-1 sentinel from make_objective) makes the whole point infeasible.
  make_desirability <- function(otypes, weights, dirs, solute_name, nms) {
    fns <- lapply(otypes, function(ot) make_objective(ot, solute_name, nms))
    w <- as.numeric(weights); w[!is.finite(w) | w <= 0] <- 1; sw <- sum(w)
    mx <- (dirs %||% rep("max", length(otypes))) != "min"     # TRUE = maximise
    function(r) {
      vals <- vapply(fns, function(f) f(r), numeric(1))
      if (any(!is.finite(vals)) || any(vals < 0)) return(-1)   # any component infeasible
      d <- ifelse(mx, vals, 1 - vals)                          # minimise -> smaller-is-better
      d <- pmax(pmin(d, 1), 1e-12)                             # geometric mean needs (0, 1]
      if (length(d) == 1) return(d[[1]])
      exp(sum(w * log(d)) / sw)
    }
  }

  # selection now comes from a per-row checkbox (opt_obj_<type> / opt_var_<name>) rather than
  # a checkbox-group, so the Goal/Weight and Min/Max fields can stay visible-but-disabled.
  OBJ_DEFAULT <- "recovery"; VAR_DEFAULT <- c("pres_bar", "sf")
  selected_objectives <- reactive(names(OPT_OBJ_LBL)[vapply(names(OPT_OBJ_LBL),
    function(t) isTRUE(input[[paste0("opt_obj_", t)]]), logical(1))])
  selected_vars <- reactive(names(OPT_DEFAULTS)[vapply(names(OPT_DEFAULTS),
    function(v) isTRUE(input[[paste0("opt_var_", v)]]), logical(1))])

  output$optimize_ui <- renderUI({
    reg <- solute_registry(); sel <- (input$solutes %||% character(0)); sel <- sel[sel %in% names(reg)]
    if (length(sel) < 1)
      return(p(style = "color:#888; margin:4px 0;",
        i18n_r()$t("Select feed solutes first, then choose an objective and the variables to optimise.")))
    sol_nm <- unname(vapply(reg[sel], function(c) c$name, character(1)))   # unname -> dropdown shows names, not indices
    tagList(
      p(style = "font-size:12px; color:#555; margin-bottom:8px;",
        i18n_r()$t("Tune operating variables to maximise a separation objective. Variables left unchecked are held at the Column-parameters values; optimisation is isothermal (uses the Temperature field).")),
      fluidRow(
        column(6, selectInput(ns("opt_solute"), i18n_r()$t("Target solute"),
          choices = sol_nm, selected = isolate(input$opt_solute) %||% sol_nm[1])),
        column(6, selectInput(ns("opt_method"),
          tags$span(i18n_r()$t("Search method"),
            input_help(i18n_r()$t("Local (gradient): fast - descends from your current operating point; best for fine-tuning. Global (thorough): scans the whole bound box first, then polishes the best point - slower but avoids local optima (use when the result looks trapped or you want the global best)."), buttonLabel = i18n_r()$t("OK"))),
          choices = stats::setNames(c("local", "global"),
            c(i18n_r()$t("Local (gradient, fast)"), i18n_r()$t("Global (thorough, slower)"))),
          selected = isolate(input$opt_method) %||% "local"))),
      tags$div(style = "font-weight:700; margin:2px 0 6px;",
        i18n_r()$t("Objectives (combined by desirability)"),
        input_help(i18n_r()$t("Tick one or more responses for the target solute and set each Goal (Maximise / Minimise). Each is put on a 0-1 desirability scale and combined into a weighted geometric mean the optimiser maximises. Recovery = fraction of the solute leaving in that product; purity = its solvent-free mole fraction in that product. Tip: to drive the solute INTO the extract, maximise extract recovery AND minimise raffinate purity."), buttonLabel = i18n_r()$t("OK"))),
      uiOutput(ns("opt_targets_ui")),
      tags$div(style = "font-weight:700; margin:18px 0 6px;", i18n_r()$t("Variables to optimise")),
      uiOutput(ns("opt_bounds_ui")),
      uiOutput(ns("opt_fixed_info")),
      actionButton(ns("optimize"), i18n_r()$t("Optimize"), icon = icon("bullseye"),
        class = "btn btn-primary btn-block", style = "color:white; margin-top:6px;"),
      uiOutput(ns("opt_run_status")))
  })
  outputOptions(output, "optimize_ui", suspendWhenHidden = FALSE)

  # Every response is shown as a row: [checkbox + name] [Goal] [Weight]. The Goal/Weight
  # fields stay visible but are disabled until the row is ticked (see the toggle observer).
  output$opt_targets_ui <- renderUI({
    hdr <- fluidRow(class = "eos-opt-hdr",
      column(5, NULL),
      column(4, tags$small(i18n_r()$t("Goal"), style = "color:#6c757d; font-weight:600;")),
      column(3, tags$small(i18n_r()$t("Weight"), style = "color:#6c757d; font-weight:600;")))
    rows <- lapply(names(OPT_OBJ_LBL), function(o) {
      sel <- isolate(input[[paste0("opt_obj_", o)]]); if (is.null(sel)) sel <- o %in% OBJ_DEFAULT
      goal_in <- selectInput(ns(paste0("opt_dir_", o)), NULL,
        choices = stats::setNames(c("max", "min"), c(i18n_r()$t("Maximise"), i18n_r()$t("Minimise"))),
        selected = isolate(input[[paste0("opt_dir_", o)]]) %||% "max")
      w_in <- numericInput(ns(paste0("opt_w_", o)), NULL,
        value = isolate(input[[paste0("opt_w_", o)]]) %||% 1, min = 0.1, max = 10, step = 0.5)
      if (!isTRUE(sel)) { goal_in <- shinyjs::disabled(goal_in); w_in <- shinyjs::disabled(w_in) }
      fluidRow(class = "eos-opt-row",
        column(5, div(class = "eos-opt-check",
          checkboxInput(ns(paste0("opt_obj_", o)), i18n_r()$t(unname(OPT_OBJ_LBL[o])), value = isTRUE(sel)))),
        column(4, goal_in), column(3, w_in))
    })
    tagList(hdr, rows)
  })
  outputOptions(output, "opt_targets_ui", suspendWhenHidden = FALSE)

  # Every variable is a row: [checkbox + name] [Min] [Max]. Min/Max stay visible but are
  # disabled until the row is ticked. No separate "Bounds" title (the section title is above).
  output$opt_bounds_ui <- renderUI({
    hdr <- fluidRow(class = "eos-opt-hdr",
      column(4, NULL),
      column(4, tags$small(i18n_r()$t("Min"), style = "color:#6c757d; font-weight:600; font-size:13px;")),
      column(4, tags$small(i18n_r()$t("Max"), style = "color:#6c757d; font-weight:600; font-size:13px;")))
    rows <- lapply(names(OPT_DEFAULTS), function(v) { d <- OPT_DEFAULTS[[v]]
      lbl <- if (v == "sf") sprintf("%s (%s)", i18n_r()$t("S/F"), sf_unit_lbl(cur_sf_unit()))
             else i18n_r()$t(unname(OPT_LABELS[v]))
      sel <- isolate(input[[paste0("opt_var_", v)]]); if (is.null(sel)) sel <- v %in% VAR_DEFAULT
      lo_in <- numericInput(ns(paste0("opt_", v, "_lo")), NULL, value = isolate(input[[paste0("opt_", v, "_lo")]]) %||% d[1])
      hi_in <- numericInput(ns(paste0("opt_", v, "_hi")), NULL, value = isolate(input[[paste0("opt_", v, "_hi")]]) %||% d[2])
      if (!isTRUE(sel)) { lo_in <- shinyjs::disabled(lo_in); hi_in <- shinyjs::disabled(hi_in) }
      fluidRow(class = "eos-opt-row",
        column(4, div(class = "eos-opt-check",
          checkboxInput(ns(paste0("opt_var_", v)), lbl, value = isTRUE(sel)))),
        column(4, lo_in), column(4, hi_in))
    })
    tagList(hdr, rows)
  })
  outputOptions(output, "opt_bounds_ui", suspendWhenHidden = FALSE)

  # keep the Goal/Weight and Min/Max fields enabled exactly when their row is ticked
  observe({
    for (t in names(OPT_OBJ_LBL)) { on <- isTRUE(input[[paste0("opt_obj_", t)]])
      shinyjs::toggleState(paste0("opt_dir_", t), condition = on)
      shinyjs::toggleState(paste0("opt_w_", t), condition = on) }
    for (v in names(OPT_DEFAULTS)) { on <- isTRUE(input[[paste0("opt_var_", v)]])
      shinyjs::toggleState(paste0("opt_", v, "_lo"), condition = on)
      shinyjs::toggleState(paste0("opt_", v, "_hi"), condition = on) }
  })

  # The structural variables (N, feed stages) and any unchecked operating variable are HELD
  # at the Column-parameters values during optimisation - show them so it is clear what is fixed.
  output$opt_fixed_info <- renderUI({
    fs <- feed_stages(); optv <- selected_vars()
    held <- character(0)
    if (!("pres_bar" %in% optv)) held <- c(held, sprintf("P = %s bar", fmt_num(input$pres_bar %||% 100, 0)))
    if (!("temp" %in% optv))     held <- c(held, sprintf("T = %s °C", fmt_num(input$temp_c %||% 61, 1)))
    if (!("sf" %in% optv))       held <- c(held, sprintf("S/F = %s %s", fmt_num(input$sf %||% 100, 1), sf_unit_lbl(cur_sf_unit())))
    if (!("reflux" %in% optv))   held <- c(held, sprintf("R = %s", fmt_num(input$reflux %||% 2, 2)))
    held_txt <- if (length(held)) paste(held, collapse = ",  ") else i18n_r()$t("none — all operating variables are being optimised")
    # the feed (oil) composition is also held fixed; show it AS ENTERED (in the current
    # mass/mole basis), normalised, so the run is fully specified
    prob <- assemble_comps()
    if (!is.null(prob)) {
      zraw <- vapply(seq_len(prob$nsol), function(i) as.numeric(input[[paste0("z", i)]] %||% 0), numeric(1))
      s <- sum(zraw); if (is.finite(s) && s > 0) zraw <- zraw / s
      feed_txt <- paste(sprintf("%s %s", prob$sol_names, vapply(zraw, function(z) fmt_num(z, 4), character(1))), collapse = ",  ")
    } else feed_txt <- "—"
    feed_lbl <- paste0(i18n_r()$t("Feed composition"), " (",
      if (isTRUE(input$mass_basis)) i18n_r()$t("mass") else i18n_r()$t("mole"), ")")
    tags$div(style = "font-size:12px; color:#555; margin-top:8px; padding:8px 10px; background:#f6f8fa; border:1px solid #e1e4e8; border-radius:4px;",
      tags$div(style = "margin-bottom:3px;", tags$strong(i18n_r()$t("Held constant during optimisation:"))),
      tags$div(style = "margin-bottom:2px;", tags$strong(paste0(feed_lbl, ": ")), feed_txt),
      tags$div(sprintf("%s: %d  ·  %s: %d  ·  %s: %d",
        i18n_r()$t("Stages"), fs$N, i18n_r()$t("Oil feed stage"), fs$feedL, i18n_r()$t("CO2 feed stage"), fs$feedV)),
      tags$div(style = "margin-top:2px;", paste0(i18n_r()$t("Fixed operating values"), ": ", held_txt)))
  })
  outputOptions(output, "opt_fixed_info", suspendWhenHidden = FALSE)

  # async optimisation state: a background worker process + a poller. See observeEvent below.
  opt_async <- reactiveValues(proc = NULL, prog = NULL, progfile = NULL, budget = NULL, fmt = NULL, ctx = NULL)

  observeEvent(input$optimize, {
    prob <- assemble_comps()
    if (is.null(prob)) { showNotification(i18n$t("Select at least one feed solute."), type = "error"); return() }
    rm <- resolve_model(); if (rm$conflict) {
      showNotification(i18n$t("The selected components were built with different EOS models. Pick components of the same model."), type = "error"); return() }
    if (!comp_balance_ok()) { showNotification(i18n$t("Feed fractions of components 2+ exceed 1; component 1 (the balance) would be negative."), type = "error"); return() }
    if (abs(comp_total() - 1) >= 1e-6) { showNotification(i18n$t("Feed fractions must sum to exactly 1."), type = "error"); return() }
    vars <- selected_vars()
    if (length(vars) == 0) { showNotification(i18n$t("Select at least one variable to optimise."), type = "error"); return() }
    fs <- feed_stages(); Nst <- fs$N; feedL <- fs$feedL; feedV <- fs$feedV
    if (Nst < 2) { showNotification(i18n$t("The packed height is too small for even two theoretical stages at this HETP."), type = "error"); return() }
    if (feedL > Nst) { showNotification(i18n$t("The feed stage cannot exceed the number of stages."), type = "error"); return() }
    if (feedV < 1 || feedV > Nst) { showNotification(i18n$t("The solvent feed stage must be between 1 and the number of stages."), type = "error"); return() }

    getb <- function(v, k, dflt) { x <- input[[paste0("opt_", v, "_", k)]]; if (is.null(x) || is.na(x)) dflt else as.numeric(x) }
    varlist <- list()
    for (v in vars) {
      lo <- getb(v, "lo", OPT_DEFAULTS[[v]][1]); hi <- getb(v, "hi", OPT_DEFAULTS[[v]][2])
      if (!(hi > lo)) { showNotification(sprintf(i18n$t("Invalid bounds for %s (max must exceed min)."), i18n$t(unname(OPT_LABELS[v]))), type = "error"); return() }
      varlist[[v]] <- if (v == "temp") c(lo + 273.15, hi + 273.15)
                      else if (v == "sf") c(sf_to_molar(lo), sf_to_molar(hi))   # bounds entered in the chosen S/F unit
                      else c(lo, hi)
    }
    fixed <- list(pres_bar = input$pres_bar %||% 100, temp = (input$temp_c %||% 61) + 273.15,
                  sf = sf_to_molar(input$sf %||% 100), reflux = input$reflux %||% 2)
    sf_unit <- cur_sf_unit(); moil <- mw_oil()
    solute <- input$opt_solute %||% prob$sol_names[1]
    otypes <- selected_objectives()
    if (length(otypes) == 0) { showNotification(i18n$t("Select at least one objective to optimise."), type = "error"); return() }
    weights <- vapply(otypes, function(o) { w <- input[[paste0("opt_w_", o)]]
      if (is.null(w) || is.na(w) || w <= 0) 1 else as.numeric(w) }, numeric(1))
    dirs <- vapply(otypes, function(o) if (identical(input[[paste0("opt_dir_", o)]], "min")) "min" else "max", character(1))
    # Extract recovery + raffinate recovery of the SAME solute always sum to 100%; maximising
    # (or minimising) both is contradictory - warn rather than silently returning a compromise.
    if (all(c("recovery", "raff_recovery") %in% otypes) && identical(dirs[["recovery"]], dirs[["raff_recovery"]]))
      showNotification(i18n$t("Extract and raffinate recovery of the same solute sum to 100%, so they cannot both be maximised. The optimiser returns the best weighted compromise (split ≈ the weight ratio). Use one recovery plus a purity instead."), type = "warning", duration = 9)
    # Recovery alone is degenerate: max extract recovery -> extract everything (S/F max), max
    # raffinate recovery -> extract nothing (S/F min); neither is a separation. Pair it with a purity.
    else if (any(c("recovery", "raff_recovery") %in% otypes) && !any(c("ext_purity", "raff_purity") %in% otypes))
      showNotification(i18n$t("A recovery objective on its own is maximised trivially at an S/F limit (extract everything, or nothing) and is not a real separation. Add Extract purity or Raffinate purity so the optimiser trades recovery against selectivity."), type = "warning", duration = 9)
    objective <- make_desirability(otypes, weights, dirs, solute, prob$nms)
    max_iter <- round(input$max_iter %||% 600); tol <- as.numeric(input$tol %||% "1e-8")

    # ---- live per-evaluation progress (Option A) ----
    maxit_opt <- 40L
    # L-BFGS-B's eval count isn't known ahead (and varies hugely with the continuation
    # fallback), so the bar is ASYMPTOTIC: frac = eval/(eval+budget) always inches up
    # and never falsely completes. The eval count + best-so-far are the honest signal.
    budget <- max(12L, as.integer(maxit_opt * (length(varlist) + 4L)))
    fmt_pt <- function(par) paste(vapply(names(par), function(nm) { v <- par[[nm]]
      switch(nm,
        pres_bar = sprintf("P=%s bar", fmt_num(v, 0)),
        temp     = sprintf("T=%s °C", fmt_num(v - 273.15, 1)),
        sf       = if (identical(sf_unit, "mass") && is.finite(moil %||% NA))
                     sprintf("S/F=%s mass", fmt_num(v * CO2_MW / moil, 1))
                   else sprintf("S/F=%s mol", fmt_num(v, 0)),
        reflux   = sprintf("R=%s", fmt_num(v, 2)), sprintf("%s=%s", nm, fmt_num(v, 2)))
    }, character(1)), collapse = ", ")
    # seed the optimiser from the CURRENT operating point (= the fixed values for each
    # optimised var) so it starts where the column already converges, not the bounds
    # midpoint (which can sit in an infeasible region and trap the gradient solver).
    start0 <- unlist(fixed[names(varlist)])

    # ---- run it in a background process (R is single-threaded; an inline run of tens of
    # expensive column solves would freeze the whole session). The worker streams its
    # per-evaluation progress to a temp file that the poller below reads; the run is
    # cancellable, and supervise=TRUE means it cannot outlive the app. ----
    if (!is.null(opt_async$proc) && isTRUE(tryCatch(opt_async$proc$is_alive(), error = function(e) FALSE))) {
      showNotification(i18n$t("An optimisation is already running."), type = "warning"); return()
    }
    # Show the bar IMMEDIATELY (before spawning the worker) so the click feels instant. The
    # eval counter then begins once the background R process has booted and loaded the package
    # namespace - a one-time few-second startup that is inherent to running off the main thread.
    prog <- shiny::Progress$new(session, min = 0, max = 1)
    prog$set(message = i18n$t("Optimising the column..."), value = 0, detail = i18n$t("starting the background solver…"))
    progfile <- tempfile("eos_opt_", fileext = ".tsv")
    opt_method <- if (identical(input$opt_method, "global")) "global" else "local"
    opt_args <- list(N = Nst, comps = prob$comps, FL = 1, zL = prob$zL, zV = prob$zV,
      objective = objective, vars = varlist, start = start0, fixed = fixed,
      feedL = feedL, feedV = feedV, solvent = "CO2", model = prob$model,
      max_iter = max_iter, tol = tol, control = list(maxit = maxit_opt),
      method = opt_method, n_global = 32L)
    # If we are running under devtools::load_all (dev mode), the background worker - a FRESH R
    # session - would otherwise load the stale INSTALLED build (and miss un-reinstalled changes
    # to the optimiser). Pass the dev source path so the worker load_all()s it instead; in
    # production this is NULL and the worker just uses the installed namespace.
    dev_path <- tryCatch(
      if (requireNamespace("pkgload", quietly = TRUE) && isTRUE(pkgload::is_dev_package("supeRcrit")))
        getNamespaceInfo("supeRcrit", "path") else NULL,
      error = function(e) NULL)
    proc <- tryCatch(
      r_bg_quiet(
        func = function(opt_args, progfile, dev_path) {
          opt_args$on_eval <- function(s) {
            bp <- if (!is.null(s$best_par))
              paste(names(s$best_par), as.numeric(s$best_par), sep = "=", collapse = ";") else ""
            line <- paste(s$eval, if (is.finite(s$best)) s$best else NA_real_, bp, sep = "\t")
            try(writeLines(line, progfile), silent = TRUE)   # a single short line; poller is tolerant
          }
          # dev: load the source so the worker tracks load_all without a reinstall; prod: installed namespace
          if (!is.null(dev_path) && requireNamespace("pkgload", quietly = TRUE))
            pkgload::load_all(dev_path, quiet = TRUE, attach_testthat = FALSE, export_all = FALSE)
          do.call(supeRcrit:::gceos_column_optimize, opt_args)
        },
        args = list(opt_args = opt_args, progfile = progfile, dev_path = dev_path),
        supervise = TRUE),
      error = function(e) { try(prog$close(), silent = TRUE)
        showNotification(paste(i18n$t("Optimization failed:"), conditionMessage(e)), type = "error", duration = 6); NULL })
    if (is.null(proc)) return()
    opt_async$proc <- proc; opt_async$progfile <- progfile; opt_async$prog <- prog
    opt_async$budget <- budget; opt_async$fmt <- fmt_pt
    opt_async$ctx <- list(prob = prob, otypes = otypes, weights = weights, dirs = dirs, solute = solute, vars = vars,
      fixed = fixed, N = Nst, feedL = feedL, feedV = feedV, sf_unit = sf_unit, mw_oil = moil)
  })

  # Poll the background optimisation: stream progress while alive, finalise once it exits.
  observe({
    proc <- opt_async$proc
    if (is.null(proc)) return()
    invalidateLater(400, session)
    isolate({
      pf <- opt_async$progfile
      if (!is.null(pf) && file.exists(pf) && !is.null(opt_async$prog)) {
        ln <- tryCatch(readLines(pf, warn = FALSE), error = function(e) character(0))
        if (length(ln) >= 1 && nzchar(ln[[1]])) {
          parts <- strsplit(ln[[1]], "\t", fixed = TRUE)[[1]]
          ev <- suppressWarnings(as.integer(parts[1]))
          best <- if (length(parts) >= 2) suppressWarnings(as.numeric(parts[2])) else NA_real_
          if (is.finite(ev)) {
            best_txt <- if (is.finite(best)) sprintf("%.1f%%", 100 * best) else "—"
            detail <- sprintf("%s %d · %s %s", i18n$t("Evaluation"), ev, i18n$t("best so far:"), best_txt)
            if (length(parts) >= 3 && nzchar(parts[3])) {
              kv <- strsplit(parts[3], ";", fixed = TRUE)[[1]]
              bp <- stats::setNames(suppressWarnings(as.numeric(sub("^.*=", "", kv))), sub("=.*$", "", kv))
              if (all(is.finite(bp))) detail <- paste0(detail, "  (", opt_async$fmt(bp), ")")
            }
            opt_async$prog$set(value = ev / (ev + opt_async$budget), detail = detail)
          }
        }
      }
      if (!proc$is_alive()) {
        op <- tryCatch(proc$get_result(), error = function(e) {
          showNotification(paste(i18n$t("Optimization failed:"), conditionMessage(e)), type = "error", duration = 6); NULL })
        if (!is.null(opt_async$prog)) { try(opt_async$prog$set(value = 1, detail = i18n$t("done")), silent = TRUE)
          try(opt_async$prog$close(), silent = TRUE) }
        if (!is.null(opt_async$progfile)) unlink(c(opt_async$progfile, paste0(opt_async$progfile, ".tmp")))
        ctx <- opt_async$ctx
        opt_async$proc <- NULL; opt_async$prog <- NULL; opt_async$progfile <- NULL; opt_async$ctx <- NULL
        if (!is.null(op) && !is.null(ctx)) {
          eos_state$optim <- list(op = op, prob = ctx$prob, otypes = ctx$otypes, weights = ctx$weights,
            dirs = ctx$dirs, solute = ctx$solute,
            vars = ctx$vars, fixed = ctx$fixed, N = ctx$N, feedL = ctx$feedL, feedV = ctx$feedV,
            sf_unit = ctx$sf_unit, mw_oil = ctx$mw_oil)
          updateTabsetPanel(session, "result_tabs", selected = "optimize")
          if (isFALSE(op$feasible))
            showNotification(i18n$t("No feasible column was found anywhere in the optimisation bounds — usually the pressure or S/F range spans a region with no two-phase solution. Narrow the bounds around conditions where the column converges (e.g. your current operating point)."), type = "error", duration = 10)
          else if (is.null(op$result) || !isTRUE(op$result$converged))
            showNotification(i18n$t("Optimization finished but the best point did not fully converge."), type = "warning", duration = 5)
        }
      }
    })
  })

  # Cancel button (only shown while a run is in flight) -> kill the worker and clean up.
  output$opt_run_status <- renderUI({
    if (is.null(opt_async$proc)) return(NULL)
    tags$div(style = "margin-top:6px;",
      actionButton(ns("opt_cancel"), i18n_r()$t("Cancel optimisation"), icon = icon("stop"),
        class = "btn btn-danger btn-block", style = "color:white; background-color:#dc3545; border-color:#c82333; font-weight:600;"))
  })
  observeEvent(input$opt_cancel, {
    proc <- opt_async$proc
    if (!is.null(proc)) try(proc$kill(), silent = TRUE)
    if (!is.null(opt_async$prog)) try(opt_async$prog$close(), silent = TRUE)
    if (!is.null(opt_async$progfile)) unlink(c(opt_async$progfile, paste0(opt_async$progfile, ".tmp")))
    opt_async$proc <- NULL; opt_async$prog <- NULL; opt_async$progfile <- NULL; opt_async$ctx <- NULL
    showNotification(i18n$t("Optimisation cancelled."), type = "message", duration = 3)
  })

  # ---- Export All Results (zip of products / profile / sizing / optimisation) ----
  # A single title bar above the result tabs holds the component title and the Export
  # button (one DOM instance -> no duplicate output IDs across tabs).
  output$results_titlebar <- renderUI({
    r <- eos_state$result; o <- eos_state$optim
    if (is.null(r) && is.null(o)) return(NULL)
    nms <- if (!is.null(r)) r$nms else o$prob$nms
    mdl <- if (!is.null(r)) r$model else o$prob$model
    div(style = "display:flex; align-items:center; justify-content:flex-start; gap:14px; margin-bottom:6px;",
      h4(style = "margin:0;", tags$strong(HTML(paste(disp_name(nms), collapse = " + "))),
        tags$small(style = "color:#6c757d;", paste0("  (", model_pretty(mdl), ")"))),
      if (!is.null(r))
        downloadButton(ns("export_all"), i18n_r()$t("Export All Results"),
          class = "btn btn-success", style = "color:white; white-space:nowrap;"))
  })
  outputOptions(output, "results_titlebar", suspendWhenHidden = FALSE)

  # build the optimisation export bundle (translated) from the current optim state, or NULL
  optim_export_bundle <- function() {
    o <- eos_state$optim; if (is.null(o)) return(NULL)
    op <- o$op; col <- op$result; if (is.null(col) || !isTRUE(col$converged)) return(NULL)
    par <- op$par
    getp <- function(v) if (v %in% names(par)) as.numeric(par[[v]]) else o$fixed[[v]]
    fmt_cond <- function(v) {
      val <- getp(v)
      if (v == "pres_bar") paste0(fmt_num(val, 1), " bar")
      else if (v == "temp") paste0(fmt_num(val - 273.15, 1), " °C")
      else if (v == "sf") { if (identical(o$sf_unit, "mass") && is.finite(o$mw_oil %||% NA))
          paste0(fmt_num(val * CO2_MW / o$mw_oil, 1), " mass") else paste0(fmt_num(val, 1), " mol") }
      else fmt_num(val, 2)
    }
    conditions <- data.frame(
      Variable = vapply(c("pres_bar", "temp", "sf", "reflux"),
        function(v) paste0(i18n_r()$t(unname(OPT_LABELS[v])), if (v %in% o$vars) " *" else ""), character(1)),
      Value = vapply(c("pres_bar", "temp", "sf", "reflux"), fmt_cond, character(1)),
      stringsAsFactors = FALSE)
    otypes <- o$otypes %||% "recovery"; dirs <- o$dirs %||% rep("max", length(otypes))
    responses <- data.frame(
      Response = vapply(otypes, function(t) i18n_r()$t(unname(OPT_OBJ_LBL[t])), character(1)),
      Goal = vapply(dirs, function(d) if (identical(d, "min")) i18n_r()$t("Minimise") else i18n_r()$t("Maximise"), character(1)),
      Value = vapply(otypes, function(t) { v <- make_objective(t, o$solute, o$prob$nms)(col)
        if (is.finite(v)) sprintf("%.2f%%", 100 * v) else "NA" }, character(1)),
      Weight = fmt_num_vec(o$weights %||% rep(1, length(otypes))),
      check.names = FALSE, stringsAsFactors = FALSE)
    list(overall_pct = 100 * op$value, conditions = conditions, responses = responses,
         solute = o$solute)
  }
  fmt_num_vec <- function(x) vapply(x, function(v) fmt_num(v, 2), character(1))

  output$export_all <- downloadHandler(
    filename = function() paste0(generate_filename_with_timestamp("supercrit_column_results"), ".csv"),
    content = function(file) {
      r <- eos_state$result; req(r)
      tryCatch(
        eos_column_export(result = r, sizing = sizing_calc(), optim = optim_export_bundle(),
          tr = i18n_r()$t, file = file, silent = TRUE),
        error = function(e) showNotification(paste(i18n$t("Error exporting results:"), conditionMessage(e)),
          type = "error", duration = 6))
    },
    contentType = "text/csv")

  # ---- helpers for results ---------------------------------------------
  sfree <- function(v, sidx) { v <- v[-sidx]; s <- sum(v); if (!is.finite(s) || s <= 0) v else v / s }   # solvent-free (NaN-safe)
  status_badge <- function(st) {
    col <- if (st == "converged") "#28a745" else if (st == "flooded") "#dc3545" else "#e0a800"
    tags$span(i18n_r()$t(st), style = sprintf("background-color:%s; color:white; padding:2px 10px; border-radius:4px; font-weight:bold;", col))
  }
  # headers/cells may be i18n tags, HTML() objects or plain strings -> as.character
  # first (c() would flatten shiny tags, so callers pass a list()).
  html_table <- function(headers, rows) tags$table(class = "table table-striped table-condensed",
    style = "width:100%; margin-bottom:8px;",
    tags$thead(tags$tr(lapply(headers, function(h) tags$th(HTML(as.character(h)))))),
    tags$tbody(lapply(rows, function(r) tags$tr(lapply(r, function(v) tags$td(HTML(as.character(v))))))))
  # the component title now lives in the shared results_titlebar (above the tabs); each
  # tab just opens with a divider to keep the visual separation.
  result_header <- function(r) tags$hr(style = "margin-top:2px;")

  # Feed / extract / raffinate / recovery table + the top-product summary line,
  # solvent-free. Shared by the Products and Optimization tabs.
  products_block <- function(col, sol_names, oil_z, nsol, reflux, nms) {
    sidx <- length(nms)
    ext_sf <- if (reflux > 0) as.numeric(col$extract$comp)[-sidx] else sfree(as.numeric(col$extract$y), sidx)
    ext_sf <- ext_sf / sum(ext_sf)
    raff_sf <- sfree(as.numeric(col$raffinate$x), sidx)
    Eflow <- if (reflux > 0) col$extract$E else col$extract$V * (1 - as.numeric(col$extract$y)[sidx])
    purity <- function(z) max(z) * 100; topnm <- function(z) sol_names[which.max(z)]
    rows <- lapply(seq_len(nsol), function(i) list(
      tags$strong(sol_names[i]), fmt_num(oil_z[i], 4), fmt_num(ext_sf[i], 4),
      fmt_num(raff_sf[i], 4), fmt_num(100 * col$recovery[i], 2)))
    tagList(
      html_table(list(i18n_r()$t("Solute"), i18n_r()$t("Feed"), i18n_r()$t("Extract"), i18n_r()$t("Raffinate"),
                   paste0(as.character(i18n_r()$t("Recovery")), " (%)")), rows),
      tags$p(style = "font-size:12px; color:#6c757d;",
        HTML(sprintf("%s: <strong>%s</strong> @ %s%% &nbsp; (E = %s) &nbsp;·&nbsp; %s: <strong>%s</strong> @ %s%% &nbsp; (L = %s)",
          i18n_r()$t("Extract"), topnm(ext_sf), fmt_num(purity(ext_sf), 1), fmt_num(Eflow, 4),
          i18n_r()$t("Raffinate"), topnm(raff_sf), fmt_num(purity(raff_sf), 1), fmt_num(col$raffinate$L, 4)))))
  }

  output$products_result <- renderUI({
    r <- eos_state$result
    if (is.null(r)) return(div(style = "text-align:center; padding:45px; color:#888;",
      icon("water", style = "font-size:42px; margin-bottom:12px;"), h4(i18n_r()$t("Set up the column and click Run."))))
    col <- r$col
    tagList(result_header(r),
      # ---- stage-composition profile (merged in from the former Stage Profiles tab) ----
      div(style = "display:flex; justify-content:flex-start; gap:8px; margin-bottom:10px;",
        actionButton(ns("prof_img"), i18n_r()$t("Export Plot"), icon = icon("image"), class = "btn btn-success btn-sm", style = "color:white;"),
        downloadButton(ns("prof_data"), i18n_r()$t("Export Data"), class = "btn btn-success btn-sm", style = "color:white;")),
      plotly::plotlyOutput(ns("profile_plot"), height = "360px"),
      tags$p(style = "font-size:11px; color:#6c757d; margin:4px 0 12px;",
        i18n_r()$t("Solvent-free liquid mole fraction of each solute along the column (stage 1 = top / extract end, stage N = bottom / raffinate end).")),
      tags$hr(style = "margin:8px 0;"),
      tags$p(strong(i18n_r()$t("Status:")), " ", status_badge(col$status), " ",
        tags$small(style = "color:#6c757d;", HTML(sprintf("(%s %s, resid %.1e)", col$iters, i18n_r()$t("iters"), col$resid)))),
      tags$p(style = "font-size:13px; color:#555;",
        HTML(sprintf("S/F = %s %s &nbsp;·&nbsp; %s = %s &nbsp;·&nbsp; R = %s &nbsp;·&nbsp; <i>T</i> = %s °C &nbsp;·&nbsp; <i>P</i> = %s bar",
          fmt_num(r$sf_disp %||% r$SF, 1), sf_unit_lbl(r$sf_unit), i18n_r()$t("stages"), r$N, fmt_num(r$reflux, 2), fmt_num(r$temp_c, 1), fmt_num(r$P_bar, 1)))),
      products_block(col, r$sol_names, r$oil_z, r$nsol, r$reflux, r$nms),
      div(class = "alert alert-info", style = "margin-top:8px;", icon("info-circle"), " ",
        i18n_r()$t("Compositions are on a solvent (CO2)-free basis. Recovery is the fraction of each solute leaving in the extract.")))
  })

  output$optimize_result <- renderUI({
    o <- eos_state$optim
    if (is.null(o)) return(div(style = "text-align:center; padding:45px; color:#888;",
      icon("bullseye", style = "font-size:42px; margin-bottom:12px;"),
      h4(i18n_r()$t("Set an objective in the Optimization box and click Optimize."))))
    op <- o$op; col <- op$result; par <- op$par; prob <- o$prob
    if (isFALSE(op$feasible)) return(tagList(
      tags$hr(style = "margin-top:2px;"),
      div(class = "alert alert-danger", style = "margin-top:8px;",
        icon("exclamation-triangle"), " ", tags$strong(i18n_r()$t("No feasible operating point found.")), tags$br(),
        i18n_r()$t("The column did not converge anywhere in the optimisation bounds (often a pressure or S/F range with no two-phase solution). Narrow the bounds around conditions where the column converges — e.g. start from your current operating point — then optimise again."))))
    fmt_var <- function(v) {
      val <- if (v %in% names(par)) par[[v]] else o$fixed[[v]]
      if (v == "pres_bar") paste0(fmt_num(val, 1), " bar")
      else if (v == "temp") paste0(fmt_num(val - 273.15, 1), " °C")
      else if (v == "sf") {                                   # solver value is molar; show in the chosen unit
        if (identical(o$sf_unit, "mass") && is.finite(o$mw_oil %||% NA)) paste0(fmt_num(val * CO2_MW / o$mw_oil, 1), " kg/kg")
        else paste0(fmt_num(val, 1), " mol/mol")
      }
      else fmt_num(val, 2)
    }
    sf_unit_disp <- if (identical(o$sf_unit, "mass") && is.finite(o$mw_oil %||% NA)) "mass" else "mol"
    prows <- lapply(c("pres_bar", "temp", "sf", "reflux"), function(v) {
      optd <- v %in% o$vars
      lbl <- if (v == "sf") sprintf("%s (%s)", i18n_r()$t("S/F"), i18n_r()$t(sf_unit_disp))
             else i18n_r()$t(unname(OPT_LABELS[v]))
      list(HTML(paste0(lbl, if (optd) " *" else "")), fmt_var(v))
    })
    reflux_opt <- if ("reflux" %in% names(par)) par[["reflux"]] else o$fixed$reflux
    otypes <- o$otypes %||% "recovery"; dirs <- o$dirs %||% rep("max", length(otypes))
    multi <- length(otypes) > 1; show_desir <- multi || any(dirs == "min")
    obj_lbls <- vapply(otypes, function(t) i18n_r()$t(unname(OPT_OBJ_LBL[t])), character(1))
    goal_lbl <- function(d) if (identical(d, "min")) i18n_r()$t("Minimise") else i18n_r()$t("Maximise")
    comp_vals <- if (!is.null(col) && isTRUE(col$converged))
      vapply(otypes, function(t) make_objective(t, o$solute, prob$nms)(col), numeric(1))
      else rep(NA_real_, length(otypes))
    tagList(
      tags$hr(style = "margin-top:2px;"),
      tags$p(strong(if (show_desir) i18n_r()$t("Desirability:") else i18n_r()$t("Objective:")), " ",
        if (show_desir) (if (multi) sprintf(i18n_r()$t("overall (%d responses)"), length(otypes))
                         else paste0(obj_lbls[[1]], " (", tolower(goal_lbl(dirs[[1]])), ")"))
        else obj_lbls[[1]],
        if (show_desir) input_help(i18n_r()$t("Overall desirability D = the weighted geometric mean of the responses below, each mapped to a 0-1 scale by its Goal (maximise -> the value itself; minimise -> 1 minus the value). It reaches 100% only when every response meets its goal; it is NOT a simple average."), buttonLabel = i18n_r()$t("OK")),
        " — ", tags$strong(o$solute),
        tags$span(style = "background-color:#28a745; color:white; padding:2px 8px; border-radius:4px; font-weight:bold; margin-left:8px;",
          paste0(fmt_num(100 * op$value, 2), "%"))),
      if (show_desir) {
        hdr <- list(i18n_r()$t("Response"), i18n_r()$t("Goal"), i18n_r()$t("Value"))
        if (multi) hdr <- c(hdr, list(i18n_r()$t("Weight")))
        html_table(hdr, lapply(seq_along(otypes), function(i) {
          row <- list(obj_lbls[[i]], goal_lbl(dirs[[i]]),
            if (is.finite(comp_vals[[i]])) paste0(fmt_num(100 * comp_vals[[i]], 2), "%") else "—")
          if (multi) row <- c(row, list(fmt_num((o$weights %||% rep(1, length(otypes)))[[i]], 2)))
          row
        }))
      },
      h5(tags$strong(i18n_r()$t("Optimal operating conditions"))),
      html_table(list(i18n_r()$t("Variable"), i18n_r()$t("Value")), prows),
      tags$p(style = "font-size:12px; color:#6c757d;", i18n_r()$t("* optimised; others held at the Column-parameters values.")),
      tags$hr(),
      h5(tags$strong(i18n_r()$t("Products at the optimum"))),
      if (!is.null(col) && isTRUE(col$converged)) products_block(col, prob$sol_names, prob$oil_z, prob$nsol, reflux_opt, prob$nms)
      else tags$p(style = "color:#b00;", i18n_r()$t("No fully-converged column at the optimum; tighten the bounds or adjust the fixed conditions.")),
      if (!is.null(col) && isTRUE(col$converged)) tagList(
        actionButton(ns("run_optimum"), i18n_r()$t("Run with optimum values"), icon = icon("play"),
          class = "btn btn-primary btn-block", style = "color:white; margin-top:8px;"),
        div(class = "alert alert-info", style = "margin-top:8px;", icon("info-circle"), " ",
          i18n_r()$t("Loads the optimum into the Products / Sizing tabs without changing your Column-parameters inputs."))))
  })

  # Promote the optimisation result to the main Products/Sizing tabs WITHOUT touching the
  # Column-parameters inputs. The optimiser already returns a fully-converged column at the
  # optimum (op$result), so we just wrap it in the result structure those tabs consume.
  observeEvent(input$run_optimum, {
    o <- eos_state$optim; if (is.null(o)) return()
    op <- o$op; col <- op$result; par <- op$par
    if (is.null(col) || !isTRUE(col$converged)) {
      showNotification(i18n$t("No fully-converged column at the optimum; tighten the bounds or adjust the fixed conditions."), type = "error", duration = 6); return() }
    getp  <- function(v) if (v %in% names(par)) as.numeric(par[[v]]) else o$fixed[[v]]
    P_bar <- getp("pres_bar"); T_K <- getp("temp"); SF <- getp("sf"); refl <- getp("reflux")
    temp_c <- T_K - 273.15
    sf_disp <- if (identical(o$sf_unit, "mass") && is.finite(o$mw_oil %||% NA)) SF * CO2_MW / o$mw_oil else SF
    prob <- o$prob
    eos_state$result <- list(col = col, comps = prob$comps, nms = prob$nms, sol_names = prob$sol_names,
      oil_z = prob$oil_z, MW = prob$MW, zL = prob$zL, nsol = prob$nsol,
      N = o$N, feedL = o$feedL, feedV = o$feedV, reflux = refl, SF = SF,
      sf_disp = sf_disp, sf_unit = o$sf_unit,
      T_K = T_K, P_atm = bar2atm(P_bar),
      P_bar = P_bar, temp_c = temp_c, Tvec_C = rep(temp_c, o$N),
      model = prob$model, gradient = FALSE,
      HETP = hetp_val(), spec_mode = input$spec_mode %||% "stages", col_height = input$col_height)
    updateTabsetPanel(session, "result_tabs", selected = "products")
    showNotification(i18n$t("Loaded the optimum operating point into Products / Sizing."), type = "message", duration = 3)
  })

  output$sizing_result <- renderUI({
    r <- eos_state$result
    if (is.null(r)) return(div(style = "text-align:center; padding:45px; color:#888;",
      icon("ruler-combined", style = "font-size:42px; margin-bottom:12px;"), h4(i18n_r()$t("Run the column first, then size it."))))
    tagList(result_header(r),
      div(style = "font-size:12px; color:#6c757d; margin-bottom:8px;",
        HTML(sprintf("HETP %.3f m &middot; <strong>%d</strong> %s",
          r$HETP %||% 0.685, r$N, i18n_r()$t("theoretical stages")))),
      div(style = "display:flex; gap:18px; align-items:flex-start; flex-wrap:wrap;",
        div(style = "flex:1 1 150px; min-width:140px;",
          numericInput(ns("feed_rate"), tags$span(i18n_r()$t("Oil feed rate (kg/h)"),
            input_help(i18n_r()$t("Mass feed rate of the oil; sets the absolute flow scale for sizing."), buttonLabel = i18n_r()$t("OK"))),
            value = isolate(input$feed_rate) %||% (defaults$feed_rate %||% 25), min = 0.1, step = 1)),
        div(style = "flex:1 1 170px; min-width:160px;",
          selectInput(ns("density"), i18n_r()$t("Phase densities"),
            choices = stats::setNames(c("eos", "budich"), c(i18n_r()$t("GC-EOS"), i18n_r()$t("Budich-Brunner (oil)"))),
            selected = isolate(input$density) %||% "eos")),
        div(style = "flex:1 1 150px; min-width:140px;",
          conditionalPanel("input.size_mode != 'rating'", ns = ns,
            numericInput(ns("frac_flood"), tags$span(i18n_r()$t("Flooding (%)"),
              input_help(i18n_r()$t("Design vapour velocity as a percentage of the flooding velocity (typically 70-85%)."), buttonLabel = i18n_r()$t("OK"))),
              value = isolate(input$frac_flood) %||% (defaults$frac_flood %||% 80), min = 10, max = 95, step = 5)),
          conditionalPanel("input.size_mode == 'rating'", ns = ns,
            numericInput(ns("size_diameter"), tags$span(i18n_r()$t("Column diameter (m)"),
              input_help(i18n_r()$t("Diameter of an existing column to rate; the tool reports the resulting fraction of flooding."), buttonLabel = i18n_r()$t("OK"))),
              value = isolate(input$size_diameter) %||% (defaults$size_diameter %||% 0.5), min = 0.02, max = 5, step = 0.01))),
        div(style = "flex:0 0 auto;", class = "eos-size-radio",
          radioButtons(ns("size_mode"), i18n_r()$t("Diameter"), inline = FALSE,
            choices = stats::setNames(c("throughput", "rating"),
              c(i18n_r()$t("Size for throughput"), i18n_r()$t("Rate a given diameter"))),
            selected = isolate(input$size_mode) %||% (defaults$size_mode %||% "throughput")))
      ),
      tags$hr(),
      uiOutput(ns("sizing_output")))
  })

  sizing_calc <- reactive({
    r <- eos_state$result; req(r, input$feed_rate)
    rating <- (input$size_mode %||% "throughput") == "rating"
    tryCatch(supeRcrit:::gceos_column_sizing(r$col, feed_rate = input$feed_rate, feed_z = r$zL, T = r$T_K, P = r$P_atm,
      comps = r$comps, model = r$model, solvent = "CO2", density = input$density %||% "eos",
      HETP = r$HETP %||% 0.685, frac_flood = (input$frac_flood %||% 80) / 100,   # field is a %
      diameter = if (rating) input$size_diameter else NULL), error = function(e) NULL)
  })
  output$sizing_output <- renderUI({
    sz <- sizing_calc()
    if (is.null(sz)) return(div(class = "alert alert-warning", i18n_r()$t("Sizing could not be computed for these inputs.")))
    flood_col <- if (sz$pct_flood > 100) "#dc3545" else if (sz$pct_flood > 90) "#e0a800" else "#28a745"
    flood_cell <- sprintf("<span style='background-color:%s; color:white; padding:1px 7px; border-radius:4px; font-weight:bold;'>%s%%</span>",
      flood_col, fmt_num(sz$pct_flood, 0))
    rows <- list(
      list(i18n_r()$t("Column diameter"), paste0("<strong>", fmt_num(sz$diameter, 3), "</strong> m")),
      list(i18n_r()$t("Packed height"), paste0("<strong>", fmt_num(sz$height, 2), "</strong> m")),
      list(i18n_r()$t("Packed volume"), paste0(fmt_num(sz$packed_volume, 3), " m³")),
      list(i18n_r()$t("Aspect ratio (H/D)"), fmt_num(sz$aspect_ratio, 1)),
      list(i18n_r()$t("Flooding (%)"), flood_cell),
      list(i18n_r()$t("Superficial vapour velocity"), paste0(fmt_num(sz$u_super, 4), " m/s")),
      list(i18n_r()$t("Superficial liquid velocity"), paste0(fmt_num(sz$u_liq, 5), " m/s")),
      list(i18n_r()$t("Flooding velocity"), paste0(fmt_num(sz$u_flood, 4), " m/s")),
      list(i18n_r()$t("Vapour residence time"), paste0(fmt_num(sz$residence_v, 1), " s")),
      list(i18n_r()$t("Feed rate at flooding"), paste0(fmt_num(sz$feed_at_flood, 1), " kg/h")),
      list(paste0(i18n_r()$t("Vapour density"), " &rho;<sub>V</sub>"), paste0(fmt_num(sz$rho_g, 1), " kg/m³")),
      list(paste0(i18n_r()$t("Liquid density"), " &rho;<sub>L</sub>"), paste0(fmt_num(sz$rho_l, 1), " kg/m³")),
      list(paste0(i18n_r()$t("CO2 density"), " &rho;<sub>CO2</sub>"), paste0(fmt_num(sz$rho_CO2, 1), " kg/m³")))
    tagList(
      html_table(list(i18n_r()$t("Quantity"), i18n_r()$t("Value")), rows),
      if (identical(sz$mode, "rating") && sz$pct_flood > 100)
        div(class = "alert alert-danger", style = "padding:6px 10px; font-size:12px;", icon("exclamation-triangle"), " ",
          i18n_r()$t("This diameter floods at the current feed rate and S/F. Increase the diameter or lower the feed rate / S/F."))
      else if (identical(sz$mode, "rating"))
        div(class = "alert alert-info", style = "padding:6px 10px; font-size:12px;", icon("info-circle"), " ",
          HTML(sprintf(i18n_r()$t("Operating at %s%% of flooding; up to %s kg/h could be fed before flooding."),
            fmt_num(sz$pct_flood, 0), fmt_num(sz$feed_at_flood, 0)))),
      div(style = "font-size:11px; color:#6c757d; margin-top:4px;",
        i18n_r()$t("HETP is the value set in Column parameters; sizing uses the worst-loaded stage.")))
  })

  # ---- plots ------------------------------------------------------------
  output$profile_plot <- plotly::renderPlotly({
    r <- eos_state$result; req(r); col <- r$col; sidx <- length(r$nms); N <- r$N
    xsf <- t(apply(col$x, 1, function(row) sfree(row, sidx)))   # N x nsol solvent-free liquid
    stg <- seq_len(N); pal <- c("#2980b9", "#e67e22", "#27ae60", "#8e44ad", "#c0392b")
    p <- plotly::plot_ly()
    for (i in seq_len(r$nsol))
      p <- plotly::add_trace(p, x = stg, y = xsf[, i], type = "scatter", mode = "lines+markers",
        name = r$sol_names[i], line = list(color = pal[(i - 1) %% length(pal) + 1]),
        marker = list(color = pal[(i - 1) %% length(pal) + 1], size = 5))
    plotly::layout(p,
      xaxis = list(title = i18n$t("Stage (1 = top)"), showline = TRUE, linecolor = "#bbb", dtick = max(1, floor(N / 10))),
      yaxis = list(title = i18n$t("solvent-free liquid mole fraction"), range = c(0, 1), showline = TRUE, linecolor = "#bbb"),
      legend = list(x = 0.02, y = 0.98, xanchor = "left", yanchor = "top", bgcolor = "rgba(255,255,255,0.6)"),
      margin = list(l = 55, r = 10, t = 10, b = 45))
  })
  observeEvent(input$prof_img, shinyjs::runjs(sprintf(
    "var g=document.getElementById('%s'); var d=g&&(g.querySelector('.js-plotly-plot')||g); if(d&&window.Plotly)Plotly.downloadImage(d,{format:'png',filename:'supercrit_column_profile',width:1000,height:700});", ns("profile_plot"))))
  output$prof_data <- downloadHandler(
    filename = function() paste0(generate_filename_with_timestamp("supercrit_column_profile"), ".csv"),
    content = function(file) { r <- eos_state$result; req(r); col <- r$col; sidx <- length(r$nms)
      xsf <- t(apply(col$x, 1, function(row) sfree(row, sidx)))
      df <- data.frame(stage = seq_len(r$N)); for (i in seq_len(r$nsol)) df[[r$sol_names[i]]] <- xsf[, i]
      df$T_C <- if (!is.null(r$Tvec_C)) r$Tvec_C else rep(r$temp_c, r$N)
      utils::write.csv(df, file, row.names = FALSE) })

  # ---- help / reset -----------------------------------------------------
  output$eos_column_HELP <- renderUI({
    helper(shiny_tag = "", buttonLabel = "OK",
      content = i18n_r()$t("eos_column_help_en"), type = "markdown", size = "l",
      style = "color:white; font-size:20px; vertical-align:middle; margin-top:0; margin-right:3%;")
  })
  outputOptions(output, "eos_column_HELP", suspendWhenHidden = FALSE)

  output$accordion_toggle_btn <- renderUI({ create_accordion_toggle_btn(ns, i18n_r, "col_accordion") })
  outputOptions(output, "accordion_toggle_btn", suspendWhenHidden = FALSE)

  observeEvent(input$reset, {
    updateNumericInput(session, "nstages", value = 20); updateNumericInput(session, "feedl", value = 8)
    updateNumericInput(session, "feedv", value = 20)
    updateNumericInput(session, "feedl_h", value = 5.5); updateNumericInput(session, "feedv_h", value = 19)
    updateNumericInput(session, "reflux", value = 2); updateNumericInput(session, "sf", value = 100)
    updateRadioButtons(session, "sf_unit", selected = "mass")
    updateNumericInput(session, "z1", value = 1); updateNumericInput(session, "z2", value = 0); updateNumericInput(session, "z3", value = 0)
    updateNumericInput(session, "hetp", value = 0.685); updateRadioButtons(session, "spec_mode", selected = "stages")
    updateNumericInput(session, "col_height", value = 19); updateRadioButtons(session, "size_mode", selected = "throughput")
    updateNumericInput(session, "temp_c", value = 61); updateNumericInput(session, "pres_bar", value = 100)
    updateCheckboxInput(session, "gradient", value = FALSE); updateCheckboxInput(session, "mass_basis", value = FALSE)
    updateCheckboxInput(session, "show_advanced", value = FALSE)
    updateNumericInput(session, "max_iter", value = 600); updateSelectInput(session, "tol", selected = "1e-8")
    eos_state$result <- NULL; eos_state$optim <- NULL
  })
}
