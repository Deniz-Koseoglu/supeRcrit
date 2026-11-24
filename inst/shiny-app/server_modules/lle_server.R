# Liquid-Liquid Extraction solvent-screening server module.
# Ranks / optimises candidate solvents for extracting target solute(s) from a feed phase via
# COSMO-SAC-dsp (csac_lle / csac_lle_extract / csac_lle_optimize_blend) or the openCOSMO-RS-Phi EoS
# (ocrsphi_lle_extract). Solute/feed are name/CAS-searchable; candidate solvents are an explicit,
# GSK-rated list. A config-driven run core lets the built-in industry examples run deterministically.
library(shinyjs)

lle_server <- function(input, output, session, defaults, i18n, tablang) {
  ns <- session$ns
  i18n_r <- reactive({ i18n })

  rv <- reactiveValues(res = NULL, meta = NULL, flash = NULL, flash_meta = NULL, flash_best = NULL,
                       opt = NULL, opt_meta = NULL)
  # Background-run state for the cancellable Two-phase flash / Optimize blend (callr worker + poll + kill).
  lle_async <- reactiveValues(proc = NULL, prog = NULL, ctx = NULL)

  # R >= 4.3 refuses to grow a 0-row data.frame via `out[[col]] <- vec` ("replacement has N rows, data
  # has 0"). Our table renderers build `out` column-by-column, so seed it with the right number of
  # (name-less) rows and 0 columns; the first [[<- then matches. n = 0 yields a valid 0-row frame.
  .tbl0 <- function(n) data.frame(row.names = seq_len(max(0L, as.integer(n))))

  # Corner toast notifications (replacing modal shinyalert pop-ups) to match the rest of the app. Keeps
  # the shinyalert-style (title, text, type) call shape: error/warning map to themselves, info -> message.
  notify <- function(title, text = NULL, type = "message", ...) {
    sev <- if (type %in% c("error", "warning")) type else "message"
    dur <- if (identical(type, "error")) 8 else if (identical(type, "warning")) 7 else 6
    msg <- if (is.null(text) || !nzchar(text)) title
           else if (nzchar(title %||% "") && !identical(title, text)) paste0(title, ": ", text) else text
    showNotification(msg, type = sev, duration = dur)
  }

  # Local range-badge helper (module ns + i18n). Renders a numeric input whose range badge turns red
  # when the value falls outside [min,max]; mirrors the rest of the app.
  local_range_badge_input <- function(input_id, label_text, value, min_val, max_val,
                                      step = NA, help_content = NULL, help_title = NULL) {
    range_badge_input(ns, i18n_r, input_id, label_text, value, min_val, max_val,
      step = step, help_content = help_content, help_title = help_title)
  }

  # ---------- curated GSK solvent library (the candidate list) ----------
  SOLV <- supeRcrit::lle_solvents()
  # (The GSK colour-dot render for the candidate-solvent selectize is set at UI creation — see lle_ui.R —
  # so it survives choice updates; here we only push choices/selection.)
  # Sensible default candidate set (spread across sustainability bands, all common LLE solvents).
  # Default candidate set: common lab/process extraction solvents, ordered non-polar -> polar.
  SOLV_DEFAULT <- intersect(
    c("n-Hexane", "n-Heptane", "Toluene", "Benzene", "MTBE", "Chloroform", "Dichloromethane",
      "Tetrahydrofuran", "Ethyl acetate", "Acetone", "Isopropanol", "Ethanol", "Methanol"),
    SOLV$name)

  # ---------- CHAOS name/CAS enrichment (applied in-app; engine also overlays it) ----------
  chaos_names_lut <- tryCatch(
    readRDS(system.file("extdata", "solubility", "csac_chaos_names.rds", package = "supeRcrit")),
    error = function(e) NULL)
  enrich_chaos <- function(cmp) {
    if (is.null(chaos_names_lut) || !nrow(chaos_names_lut)) return(cmp)
    mi <- match(as.character(cmp$key), as.character(chaos_names_lut$key)); hit <- !is.na(mi)
    cmp$name[hit] <- chaos_names_lut$name[mi[hit]]
    cmp$cas[hit]  <- chaos_names_lut$cas[mi[hit]]
    cmp
  }

  pretty <- function(x, db) if (identical(db, "chaos")) x else gsub("_", " ", x)

  # Name/CAS-searchable choices for a database. Value is what the engine resolves (UD: name; CHAOS:
  # SMILES). Label carries the name and, when known, the CAS — so typing either finds the compound.
  db_choices <- function(db) {
    cmp <- supeRcrit::csac_compounds(db)
    if (identical(db, "chaos")) {
      cmp <- enrich_chaos(cmp)
      nm <- cmp$name; cas <- cmp$cas; sm <- cmp$smiles
      # Only offer the compounds we have a real common name for (CHAOS ships no names; ~72% resolved
      # via curated assets + PubChem). The unnamed remainder (novel / isotope-labelled structures) are
      # hidden from the list — an off-list compound can still be entered directly as a SMILES (create=TRUE),
      # which FastSigma resolves. Value stays the SMILES so the engine resolves it either way.
      named <- !is.na(nm) & nzchar(nm) & nm != sm
      nm <- nm[named]; cas <- cas[named]; sm <- sm[named]
      dnm <- pretty_name(nm)                                   # Title-case the display name
      lab <- ifelse(!is.na(cas) & nzchar(cas), paste0(dnm, "  (", cas, ")"), dnm)
      ord <- order(tolower(dnm))
      stats::setNames(sm[ord], lab[ord])
    } else {
      nm <- cmp$name; cas <- cmp$cas
      dnm <- pretty_name(nm)                                   # CHOLESTEROL -> Cholesterol, etc.
      lab <- ifelse(!is.na(cas) & nzchar(cas), paste0(dnm, "  (", cas, ")"), dnm)
      ord <- order(tolower(dnm))
      stats::setNames(nm[ord], lab[ord])
    }
  }
  chaos_def <- list(solutes = "c1ccc2ccccc2c1", feed = "O")   # default solute Naphthalene (CHAOS SMILES), feed water

  # ---- unified compound labelling across engines: every picker shows "Name  (CAS)" where a CAS is known.
  # CAS normaliser: bare digits -> standard NNNN-NN-N (so ocrsphi matches the curated DB's dashed format).
  .cas_fmt <- function(x) {
    d <- gsub("[^0-9]", "", ifelse(is.na(x), "", x))
    ifelse(nchar(d) >= 5L, sub("^([0-9]+)([0-9]{2})([0-9])$", "\\1-\\2-\\3", d), NA_character_)
  }
  # name -> CAS lookup from the experimental DB (built once), to add CAS to engines whose own list lacks it.
  .name_cas <- local({ cache <- NULL; function() {
    if (is.null(cache)) {
      ep <- tryCatch(supeRcrit::exp_prop_db(), error = function(e) NULL)
      cache <<- if (is.null(ep) || !nrow(ep)) stats::setNames(character(0), character(0))
                else { v <- .cas_fmt(ep$cas); stats::setNames(v, toupper(trimws(ep$name))) }
    }
    cache
  } })
  .cas_for <- function(names) {
    m <- .name_cas(); out <- unname(m[toupper(trimws(names))]); out[is.na(out) | !nzchar(out)] <- NA_character_; out
  }

  # openCOSMO-RS-Phi compound list (individually profiled bundle; names) -> same "Name  (CAS)" labels as the
  # curated/CHAOS/CS25 pickers; value stays the engine-resolved name.
  ocrsphi_choices <- function() {
    ch <- tryCatch(supeRcrit::ocrsphi_lle_compounds(), error = function(e) character(0))
    ch <- ch[order(tolower(ch))]
    cas <- .cas_for(ch)
    lab <- ifelse(!is.na(cas), paste0(ch, "  (", cas, ")"), ch)
    stats::setNames(ch, lab)
  }

  # name -> chemical class (experimental DB category), for the Browse-database table's Class column.
  .name_cat <- local({ cache <- NULL; function() {
    if (is.null(cache)) {
      ep <- tryCatch(supeRcrit::exp_prop_db(), error = function(e) NULL)
      cache <<- if (is.null(ep) || !nrow(ep)) stats::setNames(character(0), character(0))
                else stats::setNames(as.character(ep$category), toupper(trimws(ep$name)))
    }
    cache
  } })
  .cat_for <- function(names) { m <- .name_cat(); out <- unname(m[toupper(trimws(names))]); out[is.na(out)] <- ""; out }

  # Common-solvent identifiers (the vetted extended solvent library), used to filter the feed picker down
  # to recognisable solvents (the full DB carries many obscure compounds). Matched per engine namespace.
  common_ids <- function(engine, db) {
    lib <- tryCatch(.lle_ext_lib(), error = function(e) NULL); if (is.null(lib) || !nrow(lib)) return(character(0))
    if (identical(engine, "ocrsphi")) toupper(trimws(lib$name))
    else if (identical(db, "chaos")) lib$smiles[nzchar(lib$smiles)]
    else lib$ud_name[nzchar(lib$ud_name)]
  }
  # Feed choices for an engine/db, optionally restricted to the common-solvent set.
  feed_choices <- function(engine, db, common) {
    ch <- if (identical(engine, "ocrsphi")) ocrsphi_choices() else db_choices(db)
    if (isTRUE(common)) {
      cids <- common_ids(engine, db); vals <- unname(ch)
      keep <- if (identical(engine, "ocrsphi")) toupper(trimws(vals)) %in% cids else vals %in% cids
      ch <- ch[keep]
    }
    ch
  }

  # Full compound table (Name, CAS, Class, value) for the current engine/db — drives the Browse modal.
  # `value` is the engine-resolved identifier (UD/ocrsphi: name; CHAOS: SMILES).
  compound_table <- function(engine, db) {
    if (identical(engine, "ocrsphi")) {
      ch <- tryCatch(supeRcrit::ocrsphi_lle_compounds(), error = function(e) character(0)); ch <- ch[order(tolower(ch))]
      data.frame(Name = ch, CAS = .cas_for(ch), Class = .cat_for(ch), value = ch, stringsAsFactors = FALSE)
    } else {
      cmp <- supeRcrit::csac_compounds(db)
      if (identical(db, "chaos")) {
        cmp <- enrich_chaos(cmp)
        keep <- !is.na(cmp$name) & nzchar(cmp$name) & cmp$name != cmp$smiles
        cmp <- cmp[keep, , drop = FALSE]; val <- cmp$smiles
      } else val <- cmp$name
      ord <- order(tolower(pretty_name(cmp$name)))
      data.frame(Name = pretty_name(cmp$name)[ord], CAS = .cas_fmt(cmp$cas[ord]),
                 Class = .cat_for(cmp$name[ord]), value = val[ord], stringsAsFactors = FALSE)
    }
  }

  populate_solutes_feed <- function(engine, db, solutes_sel = NULL, feed_sel = NULL) {
    if (identical(engine, "ocrsphi")) {
      ch <- ocrsphi_choices()
      fd <- feed_sel %||% (if ("Water" %in% ch) "Water" else if (length(ch)) ch[1] else "")
      sol <- solutes_sel %||% (if ("Naphthalene" %in% ch) "Naphthalene" else if (length(ch) > 1) ch[2] else fd)
      updateSelectizeInput(session, "solutes", choices = ch, selected = sol, server = TRUE,
        options = list(placeholder = i18n_r()$t("Search name or CAS — add one or more")))
      updateSelectizeInput(session, "feed", choices = feed_choices("ocrsphi", NULL, isTRUE(isolate(input$feed_common))), selected = fd, server = TRUE,
        options = list(placeholder = i18n_r()$t("Search name or CAS — type to filter")))
    } else {
      ch <- db_choices(db)
      opt <- if (identical(db, "chaos")) list(create = TRUE) else list()
      # In CHAOS, a pasted SMILES not on the list is accepted (create=TRUE) and resolved by FastSigma.
      sol_ph <- if (identical(db, "chaos")) i18n_r()$t("Search name/CAS, or paste a SMILES (FastSigma)")
                else i18n_r()$t("Search name or CAS — add one or more")
      if (is.null(solutes_sel)) solutes_sel <- if (identical(db, "chaos")) chaos_def$solutes else "NAPHTHALENE"   # default solute Naphthalene
      if (is.null(feed_sel))    feed_sel    <- if (identical(db, "chaos")) chaos_def$feed else (defaults$feed %||% "")
      updateSelectizeInput(session, "solutes", choices = ch, selected = solutes_sel, server = TRUE,
        options = c(opt, list(placeholder = sol_ph)))
      updateSelectizeInput(session, "feed", choices = feed_choices("csac", db, isTRUE(isolate(input$feed_common))), selected = feed_sel, server = TRUE,
        options = c(opt, list(placeholder = i18n_r()$t("Search name or CAS — type to filter"))))
    }
  }

  # Candidate-solvent selectize: push the curated GSK library as choices (render/plugins set in the UI).
  populate_solvents <- function(selected = SOLV_DEFAULT) {
    updateSelectizeInput(session, "candidate_solvents",
      choices = stats::setNames(SOLV$name, SOLV$name), selected = selected, server = FALSE)
  }

  # Initial population + repopulate on engine / database change.
  observeEvent(input$engine, {
    eng <- input$engine %||% "csac"
    # openCOSMO-RS-Phi and CS25 support the Two-phase flash ONLY — restrict the mode dropdown to it so the
    # other modes are not even selectable (COSMO-SAC-dsp keeps all three).
    mode_full <- stats::setNames(c("screen", "flash", "optimize"),
      c(i18n_r()$t("Quick screen (rank solvents)"),
        i18n_r()$t("Two-phase flash (rigorous split + T scan)"),
        i18n_r()$t("Optimize blend (best binary co-solvent)")))
    if (eng %in% c("ocrsphi", "cs25")) {
      updateSelectInput(session, "mode", choices = mode_full[mode_full == "flash"], selected = "flash")
    } else {
      sel_mode <- isolate(input$mode) %||% "screen"; if (!(sel_mode %in% mode_full)) sel_mode <- "screen"
      updateSelectInput(session, "mode", choices = mode_full, selected = sel_mode)
    }
    if (identical(eng, "ocrsphi")) populate_solutes_feed("ocrsphi", NULL)
    else populate_solutes_feed("csac", input$database %||% "chaos")
    populate_solvents(isolate(input$candidate_solvents) %||% SOLV_DEFAULT)
    # Engine-appropriate auto-search: COSMO-SAC-dsp gets the fast extended library; openCOSMO-RS-Phi
    # offers its whole profiled set; CS25 is local/serial so it is hand-pick only (use COSMO-SAC-dsp to
    # auto-search, then CS25 to verify specific solvents).
    ch <- switch(input$engine %||% "csac",
      ocrsphi = c("Choose solvents (curated list)" = "pick", "Auto: entire openCOSMO-RS-Phi set" = "all_ocrsphi"),
      cs25    = c("Choose solvents (curated list)" = "pick"),
                c("Choose solvents (curated list)" = "pick", "Auto: extended solvent library" = "extended"))
    sel <- isolate(input$cand_source) %||% "pick"; if (!(sel %in% ch)) sel <- "pick"
    updateSelectInput(session, "cand_source", choices = ch, selected = sel)
  }, ignoreInit = FALSE)
  observeEvent(input$database, {
    if (identical(input$engine %||% "csac", "csac")) populate_solutes_feed("csac", input$database)
  }, ignoreInit = TRUE)

  # Separation selectivity needs >=2 solutes; with fewer, offer only the distribution objective.
  observeEvent(input$solutes, {
    multi <- length(input$solutes[nzchar(input$solutes)]) >= 2L
    ch <- if (multi)
        stats::setNames(c("selectivity", "distribution"),
          c(i18n_r()$t("Separation selectivity"), i18n_r()$t("Target distribution coefficient")))
      else stats::setNames("distribution", i18n_r()$t("Target distribution coefficient"))
    sel <- isolate(input$objective) %||% "distribution"
    if (!(sel %in% ch)) sel <- if (multi) "selectivity" else "distribution"
    updateSelectInput(session, "objective", choices = ch, selected = sel)
  }, ignoreNULL = FALSE)

  # When the "common solvents only" toggle changes, re-filter the feed picker.
  observeEvent(input$feed_common, {
    eng <- input$engine %||% "csac"; db <- input$database %||% "chaos"
    updateSelectizeInput(session, "feed", choices = feed_choices(eng, db, isTRUE(input$feed_common)),
      selected = isolate(input$feed), server = TRUE,
      options = list(placeholder = i18n_r()$t("Search name or CAS — type to filter")))
  }, ignoreInit = TRUE)

  # Clear the feed selection.
  observeEvent(input$clear_feed, {
    updateSelectizeInput(session, "feed", selected = "", server = TRUE)
  }, ignoreInit = TRUE)

  # ---------- Browse-database modal (pick solute(s) OR the feed from a searchable table) ----------
  browse_mode <- reactiveVal("solutes")
  browse_tbl  <- reactive({
    eng <- input$engine %||% "csac"; db <- input$database %||% "chaos"
    tb <- compound_table(eng, db)
    # When browsing the FEED with "common solvents only" on, restrict the table to the common-solvent set.
    if (identical(browse_mode(), "feed") && isTRUE(input$feed_common)) {
      cids <- common_ids(eng, db)
      keep <- if (identical(eng, "ocrsphi")) toupper(trimws(tb$value)) %in% cids else tb$value %in% cids
      tb <- tb[keep, , drop = FALSE]
    }
    tb
  })
  .show_browse <- function(mode) {
    browse_mode(mode)
    showModal(modalDialog(
      title = if (identical(mode, "feed")) i18n_r()$t("Browse compounds — feed / diluent phase")
              else i18n_r()$t("Browse compounds — solutes"),
      size = "l", easyClose = TRUE,
      tags$p(style = "color:#6c757d;",
        if (identical(mode, "feed")) i18n_r()$t("Search, select ONE row for the feed / diluent phase, then Add selected.")
        else i18n_r()$t("Search, select one or more rows, then Add selected. The list reflects the current engine and database.")),
      DT::dataTableOutput(ns("browse_table")),
      footer = tagList(
        modalButton(i18n_r()$t("Close")),
        actionButton(ns("browse_add"), i18n_r()$t("Add selected"), class = "btn btn-primary", style = "color:white;"))))
  }
  observeEvent(input$browse_solutes, .show_browse("solutes"))
  observeEvent(input$browse_feed,    .show_browse("feed"))
  output$browse_table <- DT::renderDataTable({
    tb <- browse_tbl()
    DT::datatable(tb[, c("Name", "CAS", "Class")], rownames = FALSE, filter = "none",
      selection = if (identical(browse_mode(), "feed")) "single" else "multiple",
      colnames = c(i18n_r()$t("Name"), i18n_r()$t("CAS"), i18n_r()$t("Class")),
      options = list(pageLength = 12, lengthMenu = c(12, 25, 50), order = list()))
  })
  observeEvent(input$browse_add, {
    sel <- input$browse_table_rows_selected
    if (length(sel)) {
      tb <- browse_tbl(); eng <- input$engine %||% "csac"; db <- input$database %||% "chaos"
      ch <- if (identical(eng, "ocrsphi")) ocrsphi_choices() else db_choices(db)   # full list so new chips show names
      opt <- c(if (identical(eng, "csac") && identical(db, "chaos")) list(create = TRUE) else list(),
               list(placeholder = i18n_r()$t("Search name or CAS, or Browse the database")))
      if (identical(browse_mode(), "feed")) {
        updateSelectizeInput(session, "feed", choices = ch, selected = tb$value[sel[1]], server = TRUE, options = opt)
      } else {
        newsel <- unique(c(input$solutes %||% character(0), unique(tb$value[sel])))
        updateSelectizeInput(session, "solutes", choices = ch, selected = newsel, server = TRUE, options = opt)
      }
    }
    removeModal()
  })

  # ---------- display-name + unit-conversion helpers ----------
  # Sentence-case ALL-CAPS DB names for display (the VALUE passed to the engine stays the raw identifier).
  pretty_name <- function(x) {
    fixone <- function(s) {
      s <- gsub("_", " ", s)
      if (grepl("[a-z]", s)) return(s)                       # already mixed case -> leave
      s <- tolower(s); s <- sub("([a-z])", "\\U\\1", s, perl = TRUE)   # sentence case
      s <- sub("^(Cis|Trans|Sec|Tert|Ortho|Meta|Para|Alpha|Beta|Gamma|Delta|Endo|Exo|Syn|Anti|Rac|Dl)-",
               "\\L\\1-", s, perl = TRUE)                      # lowercase leading stereo/locant words
      s
    }
    vapply(as.character(x), fixone, character(1), USE.NAMES = FALSE)
  }
  # Molar volume (cm3/mol) estimated from the COSMO cavity volume (A^3/molecule); cached. For unit conversions.
  .mvc <- new.env(parent = emptyenv())
  mol_vol <- function(id, db) {
    if (is.null(id) || !nzchar(id)) return(NA_real_)
    k <- paste0(db, "|", id); if (!is.null(.mvc[[k]])) return(.mvc[[k]])
    cmp <- tryCatch(supeRcrit::csac_compounds(db), error = function(e) NULL); v <- NA_real_
    if (!is.null(cmp)) { i <- which(cmp$name == id | cmp$smiles == id | cmp$cas == id)[1]
      if (!is.na(i)) v <- as.numeric(cmp$vol[i]) * 0.602214 }
    .mvc[[k]] <- v; v
  }
  # Molecular weight (g/mol) from SMILES via rcdk; cached. NA when unavailable.
  .mwc <- new.env(parent = emptyenv())
  mw_of <- function(id, db) {
    if (is.null(id) || !nzchar(id)) return(NA_real_)
    k <- paste0(db, "|", id); if (!is.null(.mwc[[k]])) return(.mwc[[k]])
    cmp <- tryCatch(supeRcrit::csac_compounds(db), error = function(e) NULL); smi <- NA_character_
    if (!is.null(cmp)) { i <- which(cmp$name == id | cmp$smiles == id | cmp$cas == id)[1]
      smi <- if (!is.na(i)) cmp$smiles[i] else if (identical(db, "chaos")) id else NA_character_ }
    m <- if (!is.na(smi)) tryCatch({ mol <- rcdk::parse.smiles(smi)[[1]]
      if (!is.null(mol)) rcdk::get.mol2formula(mol)@mass else NA_real_ }, error = function(e) NA_real_) else NA_real_
    .mwc[[k]] <- m; m
  }
  # Display name for a selectize VALUE: UD/ocrsphi value IS the name (Title-cased); a CHAOS value is a
  # SMILES, so reverse-look up its common name from the enrichment (cached), else show the SMILES.
  .chaos_nm_map <- NULL
  disp_name <- function(value, db, engine = "csac") {
    if (is.null(value) || !nzchar(value)) return("")
    if (identical(engine, "ocrsphi")) return(value)
    if (identical(db, "chaos")) {
      if (is.null(.chaos_nm_map)) {
        cmp <- tryCatch(enrich_chaos(supeRcrit::csac_compounds("chaos")), error = function(e) NULL)
        .chaos_nm_map <<- if (is.null(cmp)) character(0) else stats::setNames(cmp$name, cmp$smiles)
      }
      nm <- .chaos_nm_map[value]
      return(if (!is.na(nm) && nm != value) pretty_name(nm) else value)
    }
    pretty_name(value)
  }

  # ---------- input renders ----------
  # Single extraction temperature (screen mode) — range badge, italic T symbol.
  output$temp_ui <- renderUI(local_range_badge_input("temp", i18n_r()$t("Temperature (°C)"),
    value = input$temp %||% defaults$temp, min_val = -20, max_val = 250, step = 1,
    help_content = i18n_r()$t("Extraction temperature. Distribution and mutual solubility both depend on it; the flash/optimize modes scan a range instead."),
    help_title = i18n_r()$t("Temperature")))
  output$pressure_ui <- renderUI(local_range_badge_input("pressure", i18n_r()$t("Pressure (bar)"),
    value = input$pressure %||% 1.01325, min_val = 0.1, max_val = 500, step = 0.5,
    help_content = i18n_r()$t("System pressure for the openCOSMO-RS-Phi equation-of-state flash (fugacities are equated at this pressure)."), help_title = i18n_r()$t("Pressure")))
  output$multistart_ui <- renderUI(local_range_badge_input("multistart", i18n_r()$t("Multistart seeds"),
    value = input$multistart %||% 4, min_val = 1, max_val = 12, step = 1,
    help_content = i18n_r()$t("Number of simplex-grid seeds refined by local search so the blend optimiser does not get stuck in a poor local optimum."), help_title = i18n_r()$t("Multistart seeds")))

  # Temperature-scan Min / Max / Step (flash + optimize). Each is a range-badge input that re-evaluates on
  # every edit and turns the badge red when its value leaves the valid window (−20–250 °C; step 1–100 °C).
  output$temp_lo_ui <- renderUI(local_range_badge_input("temp_lo", i18n_r()$t("Min"),
    value = input$temp_lo %||% defaults$temp, min_val = -20, max_val = 250, step = 1))   # group label carries the help
  outputOptions(output, "temp_lo_ui", suspendWhenHidden = FALSE)
  output$temp_hi_ui <- renderUI(local_range_badge_input("temp_hi", i18n_r()$t("Max"),
    value = input$temp_hi %||% 60, min_val = -20, max_val = 250, step = 1))
  outputOptions(output, "temp_hi_ui", suspendWhenHidden = FALSE)
  output$temp_step_ui <- renderUI(local_range_badge_input("temp_step", i18n_r()$t("Step"),
    value = input$temp_step %||% 10, min_val = 1, max_val = 100, step = 1,
    help_content = i18n_r()$t("Temperature increment across the scan. Smaller steps resolve the optimum more finely but take longer."),
    help_title = i18n_r()$t("Step")))
  outputOptions(output, "temp_step_ui", suspendWhenHidden = FALSE)

  # Per-solute concentration list: one row per selected solute (first = target, rest = co-extract), each
  # with its own input, plus a unit toggle (mole fraction / g·L⁻¹). Modelled on the DOE factor rows.
  output$solute_conc_ui <- renderUI({
    sols <- input$solutes; sols <- sols[nzchar(sols)]; if (!length(sols)) return(NULL)
    unit <- input$conc_unit %||% "molfrac"; gL <- identical(unit, "gL")
    unit_lbl <- if (gL) i18n_r()$t("g·L⁻¹") else i18n_r()$t("mol·mol⁻¹")
    css <- tags$style(HTML(paste(
      # align the mole-frac/g·L toggle on the title's baseline (kill the radio form-group's extra height)
      ".lle-unit-toggle .form-group{margin:0;} .lle-unit-toggle .control-label{display:none;}",
      ".lle-unit-toggle .shiny-options-group{margin-top:0;} .lle-unit-toggle .radio-inline{margin-top:0;}",
      # per-solute row: a tidy light card with the value + unit pinned to the right
      ".lle-conc-row{display:flex;align-items:center;gap:10px;padding:6px 10px;margin-bottom:6px;background:#f8f9fa;border:1px solid #e9ecef;border-radius:5px;}",
      ".lle-conc-row .nm{flex:1;min-width:0;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;}",
      ".lle-conc-row .val{flex:0 0 96px;} .lle-conc-row .val .form-group{margin:0;} .lle-conc-row .val .form-control{height:32px;padding:4px 8px;text-align:right;}",
      ".lle-conc-row .un{flex:0 0 auto;font-size:11px;color:#8a8f98;white-space:nowrap;min-width:52px;}")))
    hdr <- div(style = "display:flex; align-items:center; justify-content:space-between; gap:10px; margin:10px 0 8px;",
      tags$label(class = "control-label", style = "margin:0;",
        i18n_r()$t("Solute concentrations"),
        input_help(i18n_r()$t("Each solute's amount in the feed, set individually. The first solute is the extraction target; any others are co-extracted species used for the selectivity. Mole fraction is per solute (lower values approach infinite dilution); g·L⁻¹ is converted using the feed's molar volume and the solute's molecular weight."),
          title = i18n_r()$t("Solute concentrations"), buttonLabel = i18n_r()$t("OK"))),
      div(class = "lle-unit-toggle", style = "flex:0 0 auto;",
        radioButtons(ns("conc_unit"), NULL, inline = TRUE,
          choices = stats::setNames(c("molfrac", "gL"), c(i18n_r()$t("mol·mol⁻¹"), i18n_r()$t("g·L⁻¹"))),
          selected = unit)))
    db <- input$database %||% "ud"; engine <- input$engine %||% "csac"
    rows <- lapply(seq_along(sols), function(i) {
      nm <- sols[i]; key <- gsub("[^A-Za-z0-9]", "_", nm)
      dn <- disp_name(nm, db, engine)
      # A CHAOS value that did not resolve to a common name is a pasted, off-database SMILES → FastSigma.
      is_custom <- identical(db, "chaos") && identical(dn, nm)
      dn_show <- if (is_custom && nchar(dn) > 22) paste0(substr(dn, 1, 21), "…") else dn
      badge <- if (i == 1)
        tags$span(i18n_r()$t("target"), style = "font-size:9px;font-weight:700;color:#fff;background:#2e7d32;border-radius:3px;padding:1px 5px;margin-left:6px;")
      else tags$span(i18n_r()$t("co-extract"), style = "font-size:9px;font-weight:700;color:#fff;background:#6c757d;border-radius:3px;padding:1px 5px;margin-left:6px;")
      fsbadge <- if (is_custom) tags$span(title = nm, "FastSigma",
        style = "font-size:9px;font-weight:700;color:#fff;background:#3c8dbc;border-radius:3px;padding:1px 5px;margin-left:6px;") else NULL
      div(class = "lle-conc-row",
        div(class = "nm", tags$strong(title = if (is_custom) nm else NULL, dn_show), badge, fsbadge),
        div(class = "val", numericInput(ns(paste0("conc_", key)), label = NULL,
          value = isolate(input[[paste0("conc_", key)]]) %||% (if (gL) 5 else 0.01),
          min = 0, step = if (gL) 0.5 else 0.005)),
        div(class = "un", unit_lbl))
    })
    tagList(css, hdr, rows)
  })

  # FastSigma guidance for off-database solutes (COSMO-SAC only; FastSigma is trained on / compatible with
  # the CHAOS set). In CHAOS you can paste any SMILES and its σ-profile is estimated; in the curated set an
  # off-database solute isn't supported, so point the user to CHAOS.
  output$fastsigma_note <- renderUI({
    engine <- input$engine %||% "csac"; db <- input$database %||% "ud"
    if (identical(engine, "ocrsphi")) return(NULL)
    if (identical(db, "chaos"))
      div(style = "font-size:11px; color:#31708f; background:#eaf3f8; border:1px solid #bce8f1; border-radius:3px; padding:5px 8px; margin:-2px 0 8px;",
        icon("wand-magic-sparkles"), " ",
        i18n_r()$t("Not in the list? Paste a SMILES to add a solute that isn't in the database — FastSigma estimates its σ-profile (see the Solute σ-Profile tab)."))
    else
      div(style = "font-size:11px; color:#8a6d3b; background:#fcf8e3; border:1px solid #faebcc; border-radius:3px; padding:5px 8px; margin:-2px 0 8px;",
        icon("circle-info"), " ",
        i18n_r()$t("To model a solute that isn't in the curated set, switch the database to CHAOS — FastSigma can then estimate any SMILES you paste."))
  })

  # Solvent : feed ratio — value + unit toggle (mole ratio / volume %) + an "optimise" switch that scans
  # a range (like the temperature scan) to balance solvent use against separation performance.
  # Solvent:feed basis toggle — a single compact button that cycles mol ratio -> mass ratio -> volume %
  # (mass by default), with a translated tooltip. Lives inline on the "Solvent : feed" label row.
  sf_unit_rv <- reactiveVal("mass")
  observeEvent(input$sf_unit_btn, {
    sf_unit_rv(c(mol = "mass", mass = "vol", vol = "mol")[[sf_unit_rv()]])
  })
  output$sf_ui <- renderUI({
    unit <- sf_unit_rv(); vol <- identical(unit, "vol"); mass <- identical(unit, "mass")
    unit_lbl <- c(mol = i18n_r()$t("mol ratio"), mass = i18n_r()$t("mass ratio"), vol = i18n_r()$t("volume %"))[[unit]]
    lab <- if (vol) i18n_r()$t("Solvent volume %") else if (mass) i18n_r()$t("Solvent : feed (mass)") else i18n_r()$t("Solvent : feed (mol)")
    lo0 <- if (vol) 1 else 0.05; hi0 <- if (vol) 99 else 20
    sfv <- isolate(input$sf_range)
    if (is.null(sfv) || length(sfv) < 2) sfv <- if (vol) c(20, 80) else c(0.5, 10)
    # Clamp the carried-over value into the current unit's [min, max] so sliderInput never warns on switch.
    sfv <- c(max(lo0, min(hi0, sfv[1])), max(lo0, min(hi0, sfv[2])))
    if (sfv[1] >= sfv[2]) sfv <- if (vol) c(20, 80) else c(0.5, 10)
    # The solvent:feed ratio is ALWAYS optimised — scan a range and report the best point (no fixed value).
    tagList(
      tags$label(class = "control-label", style = "display:flex; justify-content:space-between; align-items:center; gap:8px;",
        tags$span(lab, input_help(i18n_r()$t("The solvent:feed ratio is always scanned over this range; the optimum (best balance of solvent used vs separation) is reported alongside the temperature optimum."),
          title = i18n_r()$t("Solvent : feed"), buttonLabel = i18n_r()$t("OK"))),
        tags$span(style = "display:flex; align-items:center; gap:6px;",
          tags$span(i18n_r()$t("Toggle units:"), style = "font-size:10px;color:#6c757d;white-space:nowrap;"),
          actionButton(ns("sf_unit_btn"), unit_lbl, class = "btn btn-default btn-xs", style = "padding:1px 8px; font-size:11px;",
            title = i18n_r()$t("Click to switch the solvent-to-feed basis: mol ratio → mass ratio → volume %.")))),
      sliderInput(ns("sf_range"), NULL, min = lo0, max = hi0,
        value = sfv, step = if (vol) 1 else 0.1, width = "100%"))
  })

  # Live feed↔solvent miscibility check: LLE needs the solvent to be IMMISCIBLE with the feed (two liquid
  # phases). γ∞ of the (dilute) solvent in the feed is large when immiscible, ~1 when miscible; flag the
  # miscible ones so the user knows they cannot form an extract phase.
  output$miscibility_ui <- renderUI({
    feed <- input$feed; sols <- input$candidate_solvents
    if (is.null(feed) || !nzchar(feed) || is.null(sols) || !length(sols)) return(NULL)
    if (identical(input$engine %||% "csac", "ocrsphi")) return(NULL)
    db <- input$database %||% "ud"; ids <- solvent_ids(sols, "csac", db)
    misc <- vapply(ids, function(s) isTRUE(tryCatch(
      as.numeric(csac_gamma(s, feed, T = 298.15, dsp = isTRUE(input$dsp), database = db))[1] < 10,
      error = function(e) FALSE)), logical(1))
    bad <- sols[misc]
    if (!length(bad)) return(div(class = "alert alert-success", style = "padding:6px 10px; font-size:12px; margin:6px 0;",
      icon("circle-check"), " ",
      sprintf(i18n_r()$t("All candidate solvents look immiscible with %s — a two-phase LLE is feasible."), pretty_name(feed))))
    div(class = "alert alert-warning", style = "padding:6px 10px; font-size:12px; margin:6px 0;",
      icon("triangle-exclamation"), " ",
      sprintf(i18n_r()$t("Miscible with the feed (%s) — these form a single phase, so no LLE: %s"),
        pretty_name(feed), paste(vapply(bad, pretty_name, character(1)), collapse = ", ")))
  })

  for (o in c("temp_ui","pressure_ui","multistart_ui","solute_conc_ui","fastsigma_note","sf_ui","miscibility_ui"))
    outputOptions(output, o, suspendWhenHidden = FALSE)

  # ---------- info badges (compound coverage) ----------
  output$engine_info <- renderUI({
    eng <- input$engine %||% "csac"
    if (identical(eng, "ocrsphi")) {
      n <- tryCatch(length(supeRcrit::ocrsphi_lle_compounds()), error = function(e) 0)
      txt <- sprintf(i18n_r()$t("%s compounds"), format(n, big.mark = ","))
    } else if (identical(eng, "cs25")) {
      txt <- i18n_r()$t("CS25 (2025 COSMO-SAC) — most accurate for water/organic systems; matches compounds against its built-in database")
    } else {
      txt <- i18n_r()$t("2,259 curated + 53,088 CHAOS compounds")
    }
    div(style = "margin:-6px 0 10px;", tags$span(txt,
      style = "font-size:11px; background:#eef2f7; color:#3c5a80; border-radius:3px; padding:2px 7px;"))
  })
  output$db_info <- renderUI({
    db <- input$database %||% "ud"
    txt <- sprintf(i18n_r()$t("%s compounds"), if (identical(db, "chaos")) "53,088" else "2,259")
    div(style = "margin:-6px 0 10px;", tags$span(txt,
      style = "font-size:11px; background:#eef2f7; color:#3c5a80; border-radius:3px; padding:2px 7px;"))
  })
  outputOptions(output, "engine_info", suspendWhenHidden = FALSE)
  outputOptions(output, "db_info", suspendWhenHidden = FALSE)

  # Auto-search candidate-source note (shown for the extended library / whole-EoS-set sources).
  output$cand_note <- renderUI({
    src <- input$cand_source %||% "pick"; if (identical(src, "pick")) return(NULL)
    eng <- input$engine %||% "csac"; db <- input$database %||% "ud"
    if (identical(src, "all_ocrsphi")) {
      n <- tryCatch(length(supeRcrit::ocrsphi_lle_compounds()), error = function(e) 0)
      msg <- sprintf(i18n_r()$t("Auto-searching the entire openCOSMO-RS-Phi set (%s compounds)."), format(n, big.mark = ","))
    } else {
      Tc <- suppressWarnings(as.numeric(input$temp %||% (input$temp_range %||% c(25))[1])); if (!isTRUE(is.finite(Tc))) Tc <- 25
      n <- tryCatch(lle_extended_count(Tc, eng, db), error = function(e) 0)
      extra <- if (identical(eng, "cs25")) i18n_r()$t(" CS25 runs locally and serially, so this can take a few minutes.") else ""
      msg <- paste0(sprintf(i18n_r()$t("Auto-searching %d solvents from the extended library (those liquid at %g °C)."), n, Tc), extra)
    }
    div(style = "margin:-4px 0 10px;", tags$span(msg,
      style = "font-size:11px; background:#eef2f7; color:#3c5a80; border-radius:3px; padding:2px 7px;"))
  })
  outputOptions(output, "cand_note", suspendWhenHidden = FALSE)

  # (The per-solvent GSK chip list was removed — the colour rating now lives inline in the candidate-
  # solvent dropdown, with a compact static legend beneath it in the UI.)

  # ---------- help modal + accordion toggle ----------
  output$lle_HELP <- renderUI({ create_help_modal(i18n_r, "lle_help_en") })
  outputOptions(output, "lle_HELP", suspendWhenHidden = FALSE)
  output$accordion_toggle_btn <- renderUI({ create_accordion_toggle_btn(ns, i18n_r, "lle_accordion") })
  outputOptions(output, "accordion_toggle_btn", suspendWhenHidden = FALSE)

  output$has_results <- reactive({ !is.null(rv$res) && nrow(rv$res) > 0 })
  outputOptions(output, "has_results", suspendWhenHidden = FALSE)
  output$has_flash <- reactive({ !is.null(rv$flash) && nrow(rv$flash) > 0 })
  outputOptions(output, "has_flash", suspendWhenHidden = FALSE)
  output$has_opt <- reactive({ !is.null(rv$opt) })
  outputOptions(output, "has_opt", suspendWhenHidden = FALSE)
  # Run <-> Kill button, rendered server-side so the swap always reaches the browser (a cancellable run sets
  # lle_async$proc, which re-renders this; the Kill button terminates the worker). Mirrors the Fractionation
  # Column module's server-rendered Cancel button rather than a client-side conditionalPanel.
  output$lle_run_button <- renderUI({
    if (is.null(lle_async$proc))
      actionButton(ns("run_screen"), i18n_r()$t("Run"), class = "btn btn-primary btn-block", style = "color:white;")
    else
      actionButton(ns("kill_run"), tagList(icon("stop"), " ", i18n_r()$t("Kill")),
        class = "btn btn-danger btn-block", style = "color:white; font-weight:600;")
  })
  outputOptions(output, "lle_run_button", suspendWhenHidden = FALSE)

  observe({
    ok <- (!is.null(rv$res) && nrow(rv$res) > 0) || (!is.null(rv$flash) && nrow(rv$flash) > 0) || !is.null(rv$opt)
    if (ok) session$sendCustomMessage("enableTabs", list(tabsetId = ns("results_tabs")))
    else session$sendCustomMessage("disableTabs", list(tabsetId = ns("results_tabs"), keepFirst = TRUE))
  })

  # Map candidate-solvent names to the identifiers the chosen engine/database resolves.
  solvent_ids <- function(names, engine, db) {
    names <- names[nzchar(names)]
    if (identical(engine, "ocrsphi")) {
      pool <- tryCatch(supeRcrit::ocrsphi_lle_compounds(), error = function(e) character(0))
      names[tolower(names) %in% tolower(pool)]
    } else if (identical(db, "chaos")) {
      mi <- match(names, SOLV$name)
      ifelse(is.na(mi), names, SOLV$smiles[mi])   # library -> SMILES; off-list -> as typed
    } else {
      # UD: a display name can differ from the UD-stored name (e.g. MTBE -> METHYL_TERT-BUTYL_ETHER) and
      # UD's SMILES may not string-match, so remap via the extended library's resolved ud_name (matched by
      # InChIKey at build time); fall back to the name as-is for anything not in the library.
      lib <- tryCatch(.lle_ext_lib(), error = function(e) NULL)
      if (is.null(lib) || !nrow(lib)) return(names)
      mi <- match(toupper(names), toupper(lib$name))
      ifelse(!is.na(mi) & nzchar(lib$ud_name[mi]), lib$ud_name[mi], names)
    }
  }

  # ---------- config-driven run core (used by both the Run button and the examples) ----------
  # Converts a per-solute mass concentration (g·L⁻¹) to a feed mole fraction using the feed's molar volume
  # and the solute's MW:  x ≈ c · V_m(feed) / (1000 · MW).  Returns NA when MW/V_m are unavailable.
  gL_to_x <- function(c, solute, feed, db) {
    mw <- mw_of(solute, db); vmf <- mol_vol(feed, db)
    if (isTRUE(is.finite(mw)) && isTRUE(is.finite(vmf)) && mw > 0) c * vmf / (1000 * mw) else NA_real_
  }
  # Converts a solvent VOLUME % (of solvent in solvent+feed) to a mole ratio using representative molar
  # volumes:  r = vf·V_m(feed) / ((1−vf)·V_m(solvent)).  vf in [0,1].
  vol_to_ratio <- function(volpct, solv_ids, feed, db) {
    vf <- volpct / 100; if (!isTRUE(is.finite(vf)) || vf <= 0 || vf >= 1) return(1)
    vms <- suppressWarnings(mean(vapply(solv_ids, function(s) mol_vol(s, db), numeric(1)), na.rm = TRUE))
    vmf <- mol_vol(feed, db)
    if (!isTRUE(is.finite(vms)) || !isTRUE(is.finite(vmf)) || vms <= 0) return(1)
    vf * vmf / ((1 - vf) * vms)
  }
  # Converts a solvent:feed MASS ratio to a mole ratio:  r = massratio · MW(feed) / mean(MW(solvent)).
  mass_to_ratio <- function(massr, solv_ids, feed, db) {
    if (!isTRUE(is.finite(massr)) || massr <= 0) return(1)
    mws <- suppressWarnings(mean(vapply(solv_ids, function(s) mw_of(s, db), numeric(1)), na.rm = TRUE))
    mwf <- mw_of(feed, db)
    if (!isTRUE(is.finite(mws)) || !isTRUE(is.finite(mwf)) || mws <= 0) return(massr)
    massr * mwf / mws
  }
  cfg_from_inputs <- function() {
    engine <- input$engine %||% "csac"; db <- input$database %||% "ud"; feed <- input$feed
    sols <- input$solutes; sols <- sols[nzchar(sols)]; cunit <- input$conc_unit %||% "molfrac"
    # per-solute mole fractions (converting g·L⁻¹ where selected)
    sf_vec <- vapply(sols, function(nm) {
      v <- suppressWarnings(as.numeric(input[[paste0("conc_", gsub("[^A-Za-z0-9]", "_", nm))]]))
      if (!isTRUE(is.finite(v)) || v <= 0) return(0.01)
      x <- if (identical(cunit, "gL")) gL_to_x(v, nm, feed, db) else v
      if (!isTRUE(is.finite(x))) x <- 0.01
      max(1e-6, min(0.9, x))
    }, numeric(1))
    names(sf_vec) <- sols
    sf_scalar <- if (length(sf_vec)) mean(sf_vec) else 0.01
    # temperature range -> min/max numeric inputs (existing run code reads temp_min/temp_max)
    tlo <- suppressWarnings(as.numeric(input$temp_lo)); thi <- suppressWarnings(as.numeric(input$temp_hi))
    if (!isTRUE(is.finite(tlo))) tlo <- defaults$temp
    if (!isTRUE(is.finite(thi))) thi <- 60
    tr <- c(tlo, thi)
    # Candidate source: hand-picked list, the extended auto-search library, or (ocrsphi) its whole set.
    cand_src <- input$cand_source %||% "pick"
    T_rep <- suppressWarnings(as.numeric(input$temp %||% tr[1])); if (!isTRUE(is.finite(T_rep))) T_rep <- 25
    solvents_vec <- if (identical(cand_src, "extended")) lle_extended_solvents(T_rep, engine, db)
                    else if (identical(cand_src, "all_ocrsphi")) character(0)
                    else input$candidate_solvents
    solvents_vec <- setdiff(solvents_vec[nzchar(solvents_vec)], c(feed, sols))
    # Solvent:feed is ALWAYS scanned/optimised. Convert the scan range to a mole ratio per the chosen unit
    # (volume % or mass ratio -> mole ratio; mol ratio as-is).
    sf_unit <- sf_unit_rv()
    solv_ids <- if (identical(engine, "csac")) solvent_ids(solvents_vec, "csac", db) else character(0)
    to_r <- function(v) if (identical(sf_unit, "vol")) vol_to_ratio(v, solv_ids, feed, db)
                        else if (identical(sf_unit, "mass")) mass_to_ratio(v, solv_ids, feed, db) else v
    sfo <- TRUE
    rng <- input$sf_range %||% (if (identical(sf_unit, "vol")) c(20, 80) else c(0.5, 10))
    lo <- to_r(rng[1]); hi <- to_r(rng[2]); if (hi < lo) { t <- lo; lo <- hi; hi <- t }
    sf_grid <- unique(round(seq(lo, hi, length.out = 6), 4)); solvent_ratio <- lo
    list(engine = engine, db = db, mode = input$mode %||% "screen", solutes = sols, feed = feed,
      cand_source = cand_src,
      solvents = solvents_vec, temp = input$temp, temp_min = tr[1], temp_max = tr[2],
      temp_step = input$temp_step, solute_frac = sf_scalar, sf_vec = sf_vec, solvent_ratio = solvent_ratio,
      sf_grid = sf_grid, sf_optimize = sfo, pressure = input$pressure, dsp = isTRUE(input$dsp),
      objective = input$objective %||% "selectivity", multistart = input$multistart,
      warm = isTRUE(input$warmstart), opt_method = input$opt_method %||% "gamma_inf")
  }

  observeEvent(input$run_screen, { do_run(cfg_from_inputs()) })

  # Per-field bounds check for the temperature scan (mirrors the Min/Max/Step range badges): Min/Max within
  # −20–250 °C, Step within 1–100 °C. Returns a localised "Out of range: …" message, or NULL when all valid.
  .temp_range_error <- function(cfg) {
    checks <- list(list(cfg$temp_min, -20, 250, i18n_r()$t("Min")),
                   list(cfg$temp_max, -20, 250, i18n_r()$t("Max")),
                   list(cfg$temp_step, 1, 100, i18n_r()$t("Step")))
    bad <- character(0)
    for (ch in checks) { v <- suppressWarnings(as.numeric(ch[[1]]))
      if (length(v) != 1 || is.na(v) || v < ch[[2]] || v > ch[[3]]) bad <- c(bad, ch[[4]]) }
    if (!length(bad)) return(NULL)
    paste(i18n_r()$t("Out of range:"), paste(bad, collapse = ", "))
  }

  do_run <- function(cfg) {
    engine <- cfg$engine %||% "csac"; mode <- cfg$mode %||% "screen"
    if (!is.null(lle_async$proc)) { notify(i18n_r()$t("Please wait"),
      i18n_r()$t("An evaluation is already running."), type = "warning"); return() }
    if (engine %in% c("ocrsphi", "cs25") && !identical(mode, "flash")) {
      nm <- if (identical(engine, "cs25")) "CS25" else "openCOSMO-RS-Phi"
      notify(i18n_r()$t("Engine"),
        sprintf(i18n_r()$t("%s supports the Two-phase flash mode only. Switch the analysis mode to Two-phase flash, or switch the engine to COSMO-SAC-dsp for screening / blend optimization."), nm),
        type = "info"); return() }
    if (identical(mode, "optimize") && !identical(cfg$cand_source %||% "pick", "pick")) {
      notify(i18n_r()$t("Candidate source"),
        i18n_r()$t("Optimize blend tests binary mixtures of a hand-picked set of solvents. Set Candidate source to 'Choose solvents' and pick the solvents."),
        type = "info"); return() }
    solutes <- unique(cfg$solutes[nzchar(cfg$solutes)]); feed <- cfg$feed
    if (!length(solutes)) { notify(i18n_r()$t("Input required"),
      i18n_r()$t("Please add at least one target solute."), type = "warning"); return() }
    if (is.null(feed) || !nzchar(feed)) { notify(i18n_r()$t("Input required"),
      i18n_r()$t("Please choose a feed / diluent phase."), type = "warning"); return() }
    if (mode %in% c("flash", "optimize")) {
      rng_err <- .temp_range_error(cfg)   # per-field bounds (mirrors the badges): Min/Max −20–250 °C, Step 1–100
      if (!is.null(rng_err)) { notify(i18n_r()$t("Temperature range"), rng_err, type = "warning"); return() }
      if (!isTRUE(is.finite(cfg$temp_min)) || !isTRUE(is.finite(cfg$temp_max)) || cfg$temp_min >= cfg$temp_max) {
        notify(i18n_r()$t("Temperature range"),
          i18n_r()$t("The minimum temperature must be below the maximum."), type = "warning"); return() }
    }
    # EVERY run (Quick screen, Two-phase flash, Blend optimize — all engines) goes through the cancellable
    # background worker, so the red Kill button appears for all of them. If the worker cannot start, each
    # branch falls back to running synchronously on the main process (see launch_async).
    launch_async(mode, cfg, solutes, feed)
  }

  # ---------- cancellable background run (COSMO-SAC-dsp / openCOSMO-RS-Phi flash + blend optimize) ----------
  launch_async <- function(mode, cfg, solutes, feed) {
    db <- cfg$db %||% "ud"; engine <- cfg$engine %||% "csac"
    tmin <- cfg$temp_min; tmax <- cfg$temp_max; tstep <- cfg$temp_step
    if (is.null(tmin) || is.na(tmin)) tmin <- 25
    if (is.null(tmax) || is.na(tmax)) tmax <- tmin
    if (is.null(tstep) || is.na(tstep) || tstep <= 0) tstep <- 10
    if (tmax < tmin) { tt <- tmin; tmin <- tmax; tmax <- tt }
    Tgrid <- seq(tmin, tmax, by = tstep); if (!length(Tgrid)) Tgrid <- tmin
    sr <- cfg$solvent_ratio; if (is.null(sr) || is.na(sr) || sr <= 0) sr <- 1
    sf <- cfg$solute_frac;   if (is.null(sf) || is.na(sf) || sf <= 0) sf <- 0.01

    if (identical(mode, "screen")) {
      solvent_names <- cfg$solvents[nzchar(cfg$solvents)]
      if (!length(solvent_names)) { notify(i18n_r()$t("No solvents"),
        i18n_r()$t("Add at least one candidate solvent."), type = "warning"); return() }
      solvents <- solvent_ids(solvent_names, "csac", db)
      Tk <- (if (is.null(cfg$temp) || is.na(cfg$temp)) 25 else cfg$temp) + 273.15
      args <- list(mode = "screen", engine = "csac", db = db, solute = solutes[1],
        coex = if (length(solutes) >= 2) solutes[2] else NULL, solvents = solvents, feed = feed,
        Tk = Tk, dsp = isTRUE(cfg$dsp))
      fin <- function(result) .finalize_screen(result, cfg, solutes, feed)
      msg <- i18n_r()$t("Screening solvents...")
    } else if (identical(mode, "optimize")) {
      pool_names <- cfg$solvents[nzchar(cfg$solvents)]
      if (length(pool_names) < 2) { notify(i18n_r()$t("Input required"),
        i18n_r()$t("Pick at least two candidate solvents to blend."), type = "warning"); return() }
      obj <- cfg$objective %||% "selectivity"
      if (identical(obj, "selectivity") && length(solutes) < 2) {
        notify(i18n_r()$t("Input required"),
          i18n_r()$t("Selectivity needs at least two solutes — add another target solute or switch the objective to distribution."), type = "warning"); return() }
      pool <- solvent_ids(pool_names, "csac", db)
      args <- list(mode = "optimize", engine = "csac", db = db, solutes = solutes, feed = feed,
        solvents = pool, objective = obj, Tgrid = Tgrid, solvent_ratio = sr, sf = sf,
        multistart = cfg$multistart, warm = isTRUE(cfg$warm), opt_method = cfg$opt_method %||% "gamma_inf", dsp = isTRUE(cfg$dsp))
      fin <- function(result) .finalize_optimize(result, cfg, solutes, feed, pool_names, pool)
      msg <- i18n_r()$t("Optimizing binary blends...")
    } else if (identical(engine, "cs25")) {
      # CS25: resolve names + alias + a fast gamma-inf prescreen on the MAIN side (bounded), then flash the
      # top-K in the worker (the slow, cancellable part — the worker opens its own jshell).
      if (!exists("cs25_available") || !isTRUE(cs25_available())) { notify(i18n_r()$t("CS25 unavailable"),
        i18n_r()$t("The CS25 engine needs a local Java runtime and its model files, neither of which was detected on this machine."), type = "error"); return() }
      solvent_names <- cfg$solvents[nzchar(cfg$solvents)]
      if (!length(solvent_names)) { notify(i18n_r()$t("No solvents"), i18n_r()$t("Add at least one candidate solvent."), type = "warning"); return() }
      solutes_cs <- vapply(solutes, function(s) disp_name(s, db, "csac"), character(1)); feed_cs <- disp_name(feed, db, "csac")
      if (identical(db, "chaos") && any(c(solutes_cs, feed_cs) == c(solutes, feed))) {
        notify(i18n_r()$t("CS25 compound names"),
          i18n_r()$t("CS25 matches compounds by name, not structure. One of the chosen compounds resolved only to a SMILES — switch the database to the curated set (UD) or pick a named compound, or use the COSMO-SAC-dsp engine."), type = "warning"); return() }
      cs25_alias <- c("MTBE" = "Methyl tert-butyl ether", "DMF" = "N,N-Dimethylformamide", "DMSO" = "Dimethyl sulfoxide", "NMP" = "N-Methylpyrrolidone")
      K_cs <- 6L; keep_names <- solvent_names
      if (length(solvent_names) > K_cs) {
        sids <- solvent_ids(solvent_names, "csac", db); id2nm <- stats::setNames(solvent_names, sids)
        pre <- tryCatch(withProgress(message = i18n_r()$t("Pre-ranking candidate solvents..."), value = 0.1,
          supeRcrit::csac_lle(solutes[1], solvents = sids, feed = feed, T = mean(Tgrid) + 273.15, dsp = isTRUE(cfg$dsp), database = db)), error = function(e) NULL)
        if (!is.null(pre) && nrow(pre)) { kn <- unname(id2nm[utils::head(pre$solvent[order(-pre$D)], K_cs)]); kn <- kn[!is.na(kn)]; if (length(kn)) keep_names <- kn }
      }
      ai <- match(keep_names, names(cs25_alias)); cs25_names <- unname(ifelse(is.na(ai), keep_names, cs25_alias[ai]))
      args <- list(mode = "flash", engine = "cs25", db = db, solutes_cs = solutes_cs, feed_cs = feed_cs, solvents = cs25_names,
        Tgrid = Tgrid, sf = sf, solvent_ratio = sr,
        cs25_bridge_path = system.file("shiny-app/utils/cs25_bridge.R", package = "supeRcrit"),
        jcosmo_dir = tryCatch(cs25_jcosmo_dir(), error = function(e) ""))
      fin <- function(result) .finalize_flash(result, cfg, solutes, feed)
      msg <- i18n_r()$t("Solving CS25 two-phase flash...")
    } else {
      solvent_names <- cfg$solvents[nzchar(cfg$solvents)]
      if (identical(engine, "ocrsphi")) {
        solv <- if (length(solvent_names)) solvent_ids(solvent_names, "ocrsphi", db) else character(0)
      } else {
        if (!length(solvent_names)) { notify(i18n_r()$t("No solvents"),
          i18n_r()$t("Add at least one candidate solvent."), type = "warning"); return() }
        solv <- solvent_ids(solvent_names, "csac", db)
      }
      sfv <- cfg$sf_vec; if (is.null(sfv)) sfv <- stats::setNames(rep(sf, length(solutes)), solutes)
      sfv <- unname(sfv[solutes]); sfv[!is.finite(sfv)] <- sf
      sfgrid <- cfg$sf_grid; if (is.null(sfgrid) || !length(sfgrid)) sfgrid <- sr
      args <- list(mode = "flash", engine = engine, db = db, solutes = solutes, feed = feed,
        solvents = solv, Tgrid = Tgrid, sf = sf, sf_grid = sfgrid, sf_vec = sfv,
        solvent_ratio = sr, pressure = cfg$pressure, dsp = isTRUE(cfg$dsp))
      fin <- function(result) .finalize_flash(result, cfg, solutes, feed)
      msg <- i18n_r()$t("Solving two-phase flash over solvents and temperature...")
    }
    worker_src <- system.file("shiny-app/utils/lle_worker.R", package = "supeRcrit")
    prog <- shiny::Progress$new(session, min = 0, max = 1)
    prog$set(message = msg, value = 0.6, detail = i18n_r()$t("running in the background — press Kill to cancel"))
    # Spawn the cancellable worker. The callr SUPERVISOR (supervise/cleanup_tree) can fail to start in some
    # packaged / hosted / RStudio-launched R contexts; if it does, retry WITHOUT it so the Kill button still
    # works (a plain r_bg almost always starts). Only if even that fails do we fall back to a blocking sync run.
    .spawn <- function(sup) r_bg_quiet(function(args, worker_src) { source(worker_src); lle_compute(args) },
      args = list(args = args, worker_src = worker_src), supervise = sup, cleanup_tree = sup)
    werr <- NULL
    proc <- tryCatch(.spawn(TRUE), error = function(e) {
      werr <<- conditionMessage(e)
      tryCatch(.spawn(FALSE), error = function(e2) { werr <<- paste0(werr, " | ", conditionMessage(e2)); NULL }) })
    if (is.null(proc)) {   # worker truly could not start -> blocking synchronous run (NO Kill button possible)
      try(prog$close(), silent = TRUE)
      message("[LLE] background worker failed to start: ", werr %||% "unknown", " | worker_src='", worker_src, "'")
      showNotification(paste0(i18n_r()$t("Could not start the background worker; this run is on the main thread and has no Kill button."),
        if (!is.null(werr) && nzchar(werr)) paste0(" (", werr, ")") else ""), type = "warning", duration = 15)
      if (identical(mode, "optimize")) run_optimize(cfg, solutes, feed)
      else if (identical(mode, "screen")) run_screen(cfg, solutes, feed)
      else run_flash(cfg, solutes, feed)
      return() }
    lle_async$proc <- proc; lle_async$prog <- prog; lle_async$ctx <- list(finalize = fin)
  }

  # Poll the background run; finalise once it exits (populate rv + UI on the main process).
  observe({
    proc <- lle_async$proc
    if (is.null(proc)) return()
    invalidateLater(400, session)
    isolate({
      if (!proc$is_alive()) {
        result <- tryCatch(proc$get_result(), error = function(e) list(status = "error", message = conditionMessage(e)))
        fin <- lle_async$ctx$finalize
        if (!is.null(lle_async$prog)) try(lle_async$prog$close(), silent = TRUE)
        lle_async$proc <- NULL; lle_async$prog <- NULL; lle_async$ctx <- NULL
        if (is.function(fin)) tryCatch(fin(result),
          error = function(e) notify(i18n_r()$t("Flash failed"), conditionMessage(e), type = "error"))
      }
    })
  })

  # Kill button -> terminate the background worker (and its children, e.g. parallel helpers) and clean up.
  observeEvent(input$kill_run, {
    proc <- lle_async$proc
    if (!is.null(proc)) try(proc$kill(), silent = TRUE)
    if (!is.null(lle_async$prog)) try(lle_async$prog$close(), silent = TRUE)
    lle_async$proc <- NULL; lle_async$prog <- NULL; lle_async$ctx <- NULL
    showNotification(i18n_r()$t("Evaluation cancelled."), type = "message", duration = 3)
  })

  .finalize_flash <- function(result, cfg, solutes, feed) {
    if (is.null(result) || identical(result$status, "error")) {
      notify(i18n_r()$t("Flash failed"), (result$message %||% i18n_r()$t("Unknown error.")), type = "error"); return() }
    if (identical(result$status, "cs25_unavailable")) {
      notify(i18n_r()$t("CS25 unavailable"), i18n_r()$t("The CS25 engine needs a local Java runtime and its model files, neither of which was detected on this machine."), type = "error"); return() }
    if (identical(result$status, "cs25_noresolve")) {
      notify(i18n_r()$t("No results"), i18n_r()$t("CS25 could not resolve these compounds in its built-in compound database. Use the COSMO-SAC-dsp engine, or pick compounds that CS25 recognises."), type = "warning"); rv$flash <- NULL; return() }
    if (identical(result$status, "noresult")) {
      notify(i18n_r()$t("No results"), i18n_r()$t("No candidate solvent could be evaluated (missing profiles)."), type = "warning"); rv$flash <- NULL; return() }
    db <- cfg$db %||% "ud"; engine <- cfg$engine %||% "csac"; res <- result$res
    if (engine %in% c("ocrsphi", "cs25")) {   # these engines already return display NAMES, not ids
      res$solvent_disp <- res$solvent
      cs25 <- identical(engine, "cs25")
      sdisp <- if (cs25) vapply(solutes, function(s) disp_name(s, db, "csac"), character(1)) else solutes   # CS25 solutes are ids (chaos SMILES) -> names
      fdisp <- if (cs25) disp_name(feed, db, "csac") else feed
      rv$flash <- res
      rv$flash_meta <- list(solutes = solutes, solutes_disp = sdisp, feed = fdisp, db = engine, ns_solute = length(solutes), sf_scan = FALSE)
      if (!is.null(result$best)) { ph <- result$best$phase; ph$disp <- ph$component
        rv$flash_best <- list(phase = ph, solvent = res$solvent_disp[result$best$bi], Tc = result$best$Tc, beta = result$best$beta,
          sf_ratio = if (cs25) result$best$sf_ratio %||% NULL else NULL) }
      else rv$flash_best <- NULL
    } else {
      key_col <- if (identical(db, "chaos")) SOLV$smiles else SOLV$name
      mi <- match(res$solvent, key_col)
      res$solvent_disp <- ifelse(is.na(mi), vapply(res$solvent, function(s) disp_name(s, db, "csac"), character(1)), SOLV$name[mi])
      rv$flash <- res
      rv$flash_meta <- list(solutes = solutes, solutes_disp = vapply(solutes, function(s) disp_name(s, db, "csac"), character(1)),
        feed = disp_name(feed, db, "csac"), db = db, ns_solute = length(solutes), sf_scan = FALSE)
      if (!is.null(result$best)) {
        ph <- result$best$phase; cix <- match(ph$component, key_col)
        ph$disp <- ifelse(is.na(cix), vapply(ph$component, function(cc) disp_name(cc, db, "csac"), character(1)), SOLV$name[cix])
        rv$flash_best <- list(phase = ph, solvent = res$solvent_disp[result$best$bi], Tc = result$best$Tc, beta = result$best$beta, sf_ratio = result$best$sf_ratio)
      } else rv$flash_best <- NULL
      if (isTRUE(result$pruned)) showNotification(sprintf(i18n_r()$t("Pre-ranked %d solvents; flashing the top %d."), result$n_in, result$n_used), type = "message", duration = 6)
    }
    if (!isTRUE(result$any_split))
      showNotification(i18n_r()$t("No candidate solvent forms two liquid phases with the feed under these conditions — all are miscible with the diluent."), type = "warning", duration = 9)
    updateTabsetPanel(session, "results_tabs", selected = i18n_r()$t("Two-Phase Optimize"))
  }

  .finalize_optimize <- function(result, cfg, solutes, feed, pool_names, pool) {
    if (is.null(result) || identical(result$status, "error")) {
      notify(i18n_r()$t("Optimization failed"), (result$message %||% i18n_r()$t("Unknown error.")), type = "error"); return() }
    if (identical(result$status, "toofew")) {
      notify(i18n_r()$t("Input required"), i18n_r()$t("Need at least two solvents that resolve in this database to form binary blends."), type = "warning"); return() }
    if (identical(result$status, "empty")) {
      notify(i18n_r()$t("No two-phase blend"), i18n_r()$t("No binary blend of these solvents formed two liquid phases under the given conditions. Try different solvents, a larger solvent:feed ratio, or another temperature."), type = "warning"); rv$opt <- NULL; return() }
    db <- cfg$db %||% "ud"; obj <- cfg$objective %||% "selectivity"
    id2disp <- stats::setNames(pool_names, pool); res_list <- result$res_list
    o <- res_list[[1]]; pool_best <- o$solvents
    disp1 <- function(s) { d <- unname(id2disp[s]); if (!is.na(d) && nzchar(d)) return(d)
      k <- match(s, if (identical(db, "chaos")) SOLV$smiles else SOLV$name)
      if (is.na(k)) pretty(s, db) else SOLV$name[k] }
    rv$opt <- o
    rv$opt_meta <- list(solvents = pool_best, solvents_disp = vapply(pool_best, disp1, character(1)),
      solutes = solutes, solutes_disp = vapply(solutes, function(s) disp_name(s, db, "csac"), character(1)),
      objective = obj, feed = disp_name(feed, db, "csac"), n_pairs = length(res_list))
    .blend_label <- function(solv_ids, comp) {
      sd <- vapply(solv_ids, disp1, character(1)); cp <- round(100 * comp); kk <- cp >= 1
      if (sum(kk) <= 1) { i <- if (any(kk)) which(kk)[1] else which.max(cp); sd[i] }
      else paste(sprintf("%s %d%%", sd[kk], cp[kk]), collapse = " · ") }
    rv$opt_pairs <- do.call(rbind, lapply(res_list, function(r) data.frame(
      Blend = .blend_label(r$solvents, r$composition),
      Value = signif(r$value %||% NA_real_, 4), Tc = round((r$T_K %||% NA_real_) - 273.15, 1), stringsAsFactors = FALSE)))
    rv$opt_pairs <- rv$opt_pairs[!duplicated(rv$opt_pairs$Blend), , drop = FALSE]
    rv$opt_best_label <- .blend_label(pool_best, o$composition)
    updateTabsetPanel(session, "results_tabs", selected = i18n_r()$t("Blend Optimizer"))
  }

  # ---------- quick screen ----------
  run_screen <- function(cfg, solutes, feed) {
    db <- cfg$db %||% "ud"
    solvent_names <- cfg$solvents[nzchar(cfg$solvents)]
    if (!length(solvent_names)) { notify(i18n_r()$t("No solvents"),
      i18n_r()$t("Add at least one candidate solvent."), type = "warning"); return() }
    solvents <- solvent_ids(solvent_names, "csac", db)
    Tk <- (if (is.null(cfg$temp) || is.na(cfg$temp)) 25 else cfg$temp) + 273.15
    solute <- solutes[1]; coex <- if (length(solutes) >= 2) solutes[2] else NULL
    res <- tryCatch(withProgress(message = i18n_r()$t("Screening solvents..."), value = 0.5, {
      supeRcrit::csac_lle(solute, solvents = solvents, feed = feed, T = Tk,
        coextract = coex, dsp = isTRUE(cfg$dsp), database = db) }), error = function(e) e)
    if (inherits(res, "error")) { notify(i18n_r()$t("Screening failed"), conditionMessage(res), type = "error"); return() }
    if (is.null(res) || !nrow(res)) { notify(i18n_r()$t("No results"),
      i18n_r()$t("No candidate solvent could be evaluated (missing profiles)."), type = "warning"); rv$res <- NULL; return() }
    res$feed_miscible <- vapply(res$solvent, function(s)
      isTRUE(tryCatch(as.numeric(csac_gamma(s, feed, T = Tk, dsp = isTRUE(cfg$dsp), database = db))[1] < 10,
        error = function(e) FALSE)), logical(1))
    .screen_set(res, cfg, solutes, feed)
  }

  # Finalise an async Quick-screen worker result (status + raw csac_lle data) onto the main process.
  .finalize_screen <- function(result, cfg, solutes, feed) {
    if (is.null(result) || identical(result$status, "error")) {
      notify(i18n_r()$t("Screening failed"), (result$message %||% i18n_r()$t("Unknown error.")), type = "error"); return() }
    if (identical(result$status, "noresult")) {
      notify(i18n_r()$t("No results"), i18n_r()$t("No candidate solvent could be evaluated (missing profiles)."), type = "warning"); rv$res <- NULL; return() }
    .screen_set(result$res, cfg, solutes, feed)
  }

  # Enrich the raw csac_lle ranking (sustainability / notes columns via the curated SOLV table) and publish
  # it to rv — shared by the synchronous run_screen fallback and the async .finalize_screen.
  .screen_set <- function(res, cfg, solutes, feed) {
    db <- cfg$db %||% "ud"; solute <- solutes[1]; coex <- if (length(solutes) >= 2) solutes[2] else NULL
    # Map the engine's solvent id back to the curated SOLV row. CHAOS ids are SMILES (case-sensitive);
    # UD ids are the resolved UD names (upper-case) — translate those back to the SOLV title-case name via
    # the extended library's ud_name↔name map, then match case-insensitively (the batch-4 MTBE remap made
    # solvent_ids emit UD names, which otherwise no longer match SOLV$name and dropped all the Notes).
    if (identical(db, "chaos")) {
      mi <- match(res$solvent, SOLV$smiles)
    } else {
      canon <- res$solvent
      lib <- tryCatch(.lle_ext_lib(), error = function(e) NULL)
      if (!is.null(lib) && nrow(lib)) {
        li <- match(toupper(res$solvent), toupper(lib$ud_name)); canon <- ifelse(!is.na(li), lib$name[li], res$solvent)
      }
      mi <- match(toupper(canon), toupper(SOLV$name))
    }
    res$solvent <- ifelse(is.na(mi), pretty(res$solvent, db), SOLV$name[mi])
    res$gsk <- SOLV$gsk[mi]; res$ich <- SOLV$ich[mi]; res$hazard <- SOLV$hazard[mi]
    res$food <- SOLV$food[mi]; res$ppm <- SOLV$ppm[mi]
    rv$res <- res
    rv$meta <- list(solute = disp_name(solute, db, "csac"), feed = disp_name(feed, db, "csac"),
      Tc = cfg$temp, has_sel = !is.null(coex))
    if (!is.na(res$gsk[1]) && res$gsk[1] >= 3L) {
      alt <- which(!is.na(res$gsk) & res$gsk <= 2L)
      altmsg <- if (length(alt)) sprintf(i18n_r()$t(" A greener high-ranking alternative is %s."), res$solvent[alt[1]]) else ""
      showNotification(paste0(
        sprintf(i18n_r()$t("Note: the top solvent (%s) carries a sustainability/hazard flag."), res$solvent[1]), altmsg),
        type = "warning", duration = 9)
    }
    updateTabsetPanel(session, "results_tabs", selected = i18n_r()$t("Ranked Solvents"))
  }

  # ---------- two-phase flash & temperature optimization ----------
  run_flash <- function(cfg, solutes, feed) {
    db <- cfg$db %||% "ud"
    tmin <- cfg$temp_min; tmax <- cfg$temp_max; tstep <- cfg$temp_step
    if (is.null(tmin) || is.na(tmin)) tmin <- 25
    if (is.null(tmax) || is.na(tmax)) tmax <- tmin
    if (is.null(tstep) || is.na(tstep) || tstep <= 0) tstep <- 10
    if (tmax < tmin) { tt <- tmin; tmin <- tmax; tmax <- tt }
    Tgrid <- seq(tmin, tmax, by = tstep); if (!length(Tgrid)) Tgrid <- tmin
    sr <- cfg$solvent_ratio; if (is.null(sr) || is.na(sr) || sr <= 0) sr <- 1
    sf <- cfg$solute_frac;   if (is.null(sf) || is.na(sf) || sf <= 0) sf <- 0.01

    if (identical(cfg$engine %||% "csac", "ocrsphi")) {
      picks <- solvent_ids(cfg$solvents[nzchar(cfg$solvents)], "ocrsphi", db)
      pool <- if (length(picks)) setdiff(picks, c(feed, solutes))
              else setdiff(supeRcrit::ocrsphi_lle_compounds(), c(feed, solutes))
      P_bar <- cfg$pressure; if (is.null(P_bar) || is.na(P_bar) || P_bar <= 0) P_bar <- 1.01325
      res <- tryCatch(withProgress(message = i18n_r()$t("Solving two-phase flash over solvents and temperature..."), value = 0.5, {
        supeRcrit::ocrsphi_lle_extract(solutes, solvents = pool, feed = feed, T = Tgrid + 273.15,
          P = P_bar * 1e5, solute_frac = sf, solvent_ratio = sr) }), error = function(e) e)
      if (inherits(res, "error")) { notify(i18n_r()$t("Flash failed"), conditionMessage(res), type = "error"); return() }
      if (is.null(res) || !nrow(res)) { notify(i18n_r()$t("No results"), i18n_r()$t("No candidate solvent could be evaluated (missing profiles)."), type = "warning"); rv$flash <- NULL; return() }
      dcol <- grep("^D_", names(res)); names(res)[dcol] <- paste0("D", seq_along(dcol))
      res$solvent_disp <- res$solvent
      res$solvent_in_raffinate <- NA_real_; res$feed_in_extract <- NA_real_
      rv$flash <- res
      rv$flash_meta <- list(solutes = solutes, solutes_disp = solutes, feed = feed, db = "ocrsphi", ns_solute = length(solutes))
      rv$flash_best <- NULL
      ti <- which(res$split %in% TRUE)[1]
      if (!is.na(ti)) {
        comps <- c(feed, res$solvent[ti], solutes); z <- c(1, sr, rep(sf, length(solutes)))
        fl <- tryCatch(supeRcrit::ocrsphi_lle_flash(comps, z = z, T = res$T_K[ti], P = P_bar * 1e5), error = function(e) NULL)
        if (!is.null(fl) && isTRUE(fl$split)) { ph <- fl$phase; ph$disp <- ph$component
          rv$flash_best <- list(phase = ph, solvent = res$solvent_disp[ti], Tc = res$T_K[ti] - 273.15, beta = fl$beta) }
      }
      if (!any(res$split %in% TRUE))
        showNotification(i18n_r()$t("No candidate solvent forms two liquid phases with the feed under these conditions — all are miscible with the diluent."), type = "warning", duration = 9)
      updateTabsetPanel(session, "results_tabs", selected = i18n_r()$t("Two-Phase Optimize"))
      return()
    }

    # ---- CS25 (JCOSMO local bridge) -------------------------------------------------------------------
    # Real CS25 numbers via the persistent jshell gateway. One JVM session serves the whole solvent x T
    # scan and the best-point detail flash. Names resolve against JCOSMO's own compound database.
    if (identical(cfg$engine %||% "csac", "cs25")) {
      if (!exists("cs25_available") || !isTRUE(cs25_available())) {
        notify(i18n_r()$t("CS25 unavailable"),
          i18n_r()$t("The CS25 engine needs a local Java runtime and its model files, neither of which was detected on this machine."), type = "error"); return() }
      solvent_names <- cfg$solvents[nzchar(cfg$solvents)]
      if (!length(solvent_names)) { notify(i18n_r()$t("No solvents"), i18n_r()$t("Add at least one candidate solvent."), type = "warning"); return() }
      # CS25 resolves compounds by NAME against JCOSMO's own database — a SMILES hangs the jshell (~90 s/flash
      # timeout). The CHAOS picker supplies SMILES, so map the solute(s)/feed to display names first (solvent
      # picks are already curated names). If a name is not in JCOSMO the flash just returns no-split (fast).
      solutes_cs <- vapply(solutes, function(s) disp_name(s, db, "csac"), character(1))
      feed_cs <- disp_name(feed, db, "csac")
      # In CHAOS, disp_name returns the raw SMILES when a compound has no resolved name — those would hang CS25.
      if (identical(db, "chaos") && any(c(solutes_cs, feed_cs) == c(solutes, feed))) {
        notify(i18n_r()$t("CS25 compound names"),
          i18n_r()$t("CS25 matches compounds by name, not structure. One of the chosen compounds resolved only to a SMILES — switch the database to the curated set (UD) or pick a named compound, or use the COSMO-SAC-dsp engine."),
          type = "warning"); return() }
      # CS25 is serial (~1-1.5 s/flash); a large pick × T grid is slow. Pre-rank with the fast COSMO-SAC-dsp
      # screen and CS25-flash only the top K (small curated picks of <= K are kept in full).
      K_cs <- 6L
      if (length(solvent_names) > K_cs) {
        sids <- solvent_ids(solvent_names, "csac", db); id2name <- stats::setNames(solvent_names, sids)
        pre <- tryCatch(withProgress(message = i18n_r()$t("Pre-ranking candidate solvents..."), value = 0.1,
          supeRcrit::csac_lle(solutes[1], solvents = sids, feed = feed, T = mean(Tgrid) + 273.15,
            dsp = isTRUE(cfg$dsp), database = db)), error = function(e) NULL)
        if (!is.null(pre) && nrow(pre)) {
          keep <- unname(id2name[utils::head(pre$solvent[order(-pre$D)], K_cs)]); keep <- keep[!is.na(keep)]
          if (length(keep)) { showNotification(sprintf(i18n_r()$t("Pre-ranked %d solvents; flashing the top %d."),
            length(solvent_names), length(keep)), type = "message", duration = 6); solvent_names <- keep }
        }
      }
      # JCOSMO does not know the curated library's abbreviations (MTBE/DMF/DMSO/NMP) — they hang the jshell.
      # Map them to the full names JCOSMO resolves. (Any other unknown name now fails fast via the 20 s wait.)
      cs25_alias <- c("MTBE" = "Methyl tert-butyl ether", "DMF" = "N,N-Dimethylformamide",
                      "DMSO" = "Dimethyl sulfoxide", "NMP" = "N-Methylpyrrolidone")
      ai <- match(solvent_names, names(cs25_alias)); solvent_names <- ifelse(is.na(ai), solvent_names, cs25_alias[ai])
      sess <- tryCatch(cs25_open(), error = function(e) e)
      if (inherits(sess, "error")) { notify(i18n_r()$t("CS25 unavailable"), conditionMessage(sess), type = "error"); return() }
      on.exit(cs25_close(sess), add = TRUE)
      res <- tryCatch(withProgress(message = i18n_r()$t("Solving CS25 two-phase flash..."), value = 0.05, {
        cs25_lle_extract(solutes_cs, solvents = solvent_names, feed = feed_cs, T = Tgrid + 273.15,
          solute_frac = sf, solvent_ratio = sr, sess = sess,
          progress = function(fr) setProgress(value = max(0.05, min(0.99, fr))))
      }), error = function(e) e)
      if (inherits(res, "error")) { notify(i18n_r()$t("Flash failed"), conditionMessage(res), type = "error"); return() }
      if (is.null(res) || !nrow(res)) { notify(i18n_r()$t("No results"), i18n_r()$t("No candidate solvent could be evaluated."), type = "warning"); rv$flash <- NULL; return() }
      if (!any(res$split %in% TRUE) && all(is.na(res$beta))) {
        notify(i18n_r()$t("No results"),
          i18n_r()$t("CS25 could not resolve these compounds in its built-in compound database. Use the COSMO-SAC-dsp engine, or pick compounds that CS25 recognises."), type = "warning"); rv$flash <- NULL; return() }
      dcol <- grep("^D_", names(res)); names(res)[dcol] <- paste0("D", seq_along(dcol))
      res$solvent_disp <- res$solvent
      rv$flash <- res
      rv$flash_meta <- list(solutes = solutes_cs, solutes_disp = solutes_cs, feed = feed_cs, db = "cs25", ns_solute = length(solutes_cs))
      rv$flash_best <- NULL
      ti <- which(res$split %in% TRUE)[1]
      if (!is.na(ti)) {
        comps <- c(feed_cs, res$solvent[ti], solutes_cs); z <- c(1, sr, rep(sf, length(solutes_cs)))
        fl <- tryCatch(cs25_lle_flash(comps, z = z, T = res$T_K[ti], sess = sess), error = function(e) NULL)
        if (!is.null(fl) && isTRUE(fl$split)) { ph <- fl$phase; ph$disp <- ph$component
          rv$flash_best <- list(phase = ph, solvent = res$solvent_disp[ti], Tc = res$T_K[ti] - 273.15, beta = fl$beta) }
      }
      if (!any(res$split %in% TRUE))
        showNotification(i18n_r()$t("No candidate solvent forms two liquid phases with the feed under these conditions — all are miscible with the diluent."), type = "warning", duration = 9)
      updateTabsetPanel(session, "results_tabs", selected = i18n_r()$t("Two-Phase Optimize"))
      return()
    }

    solvent_names <- cfg$solvents[nzchar(cfg$solvents)]
    if (!length(solvent_names)) { notify(i18n_r()$t("No solvents"), i18n_r()$t("Add at least one candidate solvent."), type = "warning"); return() }
    solvents <- solvent_ids(solvent_names, "csac", db)
    # A rigorous flash of every candidate × temperature × solvent:feed ratio is the slow part. Two speedups:
    # (1) pre-rank the candidates by the fast infinite-dilution screen and flash only the top K (the Quick
    # screen already ranks the whole set); (2) do NOT scan the solvent:feed ratio for every solvent — flash
    # the solvent × T grid at ONE representative ratio, then optimise the ratio only for the single best point.
    K_flash <- 8L
    if (length(solvents) > K_flash) {
      pre <- tryCatch(withProgress(message = i18n_r()$t("Pre-ranking candidate solvents..."), value = 0.25,
        supeRcrit::csac_lle(solutes[1], solvents = solvents, feed = feed, T = mean(Tgrid) + 273.15,
          dsp = isTRUE(cfg$dsp), database = db)), error = function(e) NULL)
      if (!is.null(pre) && nrow(pre)) {
        keep <- utils::head(pre$solvent[order(-pre$D)], K_flash)
        solvents <- solvents[solvents %in% keep]
        showNotification(sprintf(i18n_r()$t("Pre-ranked %d solvents; flashing the top %d."), nrow(pre), length(solvents)),
          type = "message", duration = 6)
      }
    }
    sfv <- cfg$sf_vec; if (is.null(sfv)) sfv <- stats::setNames(rep(sf, length(solutes)), solutes)
    sfv <- sfv[solutes]; sfv[!is.finite(sfv)] <- sf
    sfgrid <- cfg$sf_grid; if (is.null(sfgrid) || !length(sfgrid)) sfgrid <- sr
    sf_mid <- stats::median(sfgrid)                                    # one representative solvent:feed ratio for the table
    res <- tryCatch(withProgress(message = i18n_r()$t("Solving two-phase flash over solvents and temperature..."), value = 0.5,
      supeRcrit::csac_lle_extract(solutes, solvents = solvents, feed = feed, T = Tgrid + 273.15,
        solute_frac = sf, solvent_ratio = sf_mid, dsp = isTRUE(cfg$dsp), database = db)), error = function(e) e)
    if (inherits(res, "error")) { notify(i18n_r()$t("Flash failed"), conditionMessage(res), type = "error"); return() }
    if (is.null(res) || !nrow(res)) { notify(i18n_r()$t("No results"), i18n_r()$t("No candidate solvent could be evaluated (missing profiles)."), type = "warning"); rv$flash <- NULL; return() }
    res$sf_ratio <- sf_mid
    dcol <- grep("^D_", names(res)); names(res)[dcol] <- paste0("D", seq_along(dcol))
    key_col <- if (identical(db, "chaos")) SOLV$smiles else SOLV$name
    mi <- match(res$solvent, key_col)
    res$solvent_disp <- ifelse(is.na(mi), vapply(res$solvent, function(s) disp_name(s, db, "csac"), character(1)), SOLV$name[mi])
    rv$flash <- res
    rv$flash_meta <- list(solutes = solutes, solutes_disp = vapply(solutes, function(s) disp_name(s, db, "csac"), character(1)),
      feed = disp_name(feed, db, "csac"), db = db, ns_solute = length(solutes), sf_scan = FALSE)
    rv$flash_best <- NULL
    # Best (solvent, temperature) by the objective among the two-phase rows; then optimise the solvent:feed
    # ratio for just that point (cheap) and build the detailed phase split there.
    spl <- which(res$split %in% TRUE)
    if (length(spl)) {
      objv <- if (length(solutes) >= 2) res$selectivity else res$D1
      bi <- spl[which.max(ifelse(is.finite(objv[spl]), objv[spl], -Inf))]
      bsolv <- res$solvent[bi]; bT <- res$T_K[bi]; sfbest <- sf_mid; obest <- -Inf
      for (rr in sfgrid) {                                             # optimise solvent:feed at the best (solvent, T)
        rrr <- tryCatch(supeRcrit::csac_lle_extract(solutes, solvents = bsolv, feed = feed, T = bT,
          solute_frac = sf, solvent_ratio = rr, dsp = isTRUE(cfg$dsp), database = db), error = function(e) NULL)
        if (is.null(rrr) || !nrow(rrr) || !isTRUE(rrr$split[1])) next
        ov <- if (length(solutes) >= 2) rrr$selectivity[1] else rrr$D1[1]
        if (isTRUE(ov > obest)) { obest <- ov; sfbest <- rr }
      }
      comps <- c(feed, bsolv, solutes); z <- c(1, sfbest, sfv); z <- z / sum(z)
      fl <- tryCatch(supeRcrit::csac_lle_flash(comps, z = z, T = bT, dsp = isTRUE(cfg$dsp), database = db), error = function(e) NULL)
      if (!is.null(fl) && isTRUE(fl$split)) {
        ph <- fl$phase; cix <- match(ph$component, key_col)
        ph$disp <- ifelse(is.na(cix), vapply(ph$component, function(cc) disp_name(cc, db, "csac"), character(1)), SOLV$name[cix])
        rv$flash_best <- list(phase = ph, solvent = res$solvent_disp[bi], Tc = bT - 273.15, beta = fl$beta, sf_ratio = sfbest)
      }
    }
    if (!any(res$split %in% TRUE))
      showNotification(i18n_r()$t("No candidate solvent forms two liquid phases with the feed under these conditions — all are miscible with the diluent."), type = "warning", duration = 9)
    updateTabsetPanel(session, "results_tabs", selected = i18n_r()$t("Two-Phase Optimize"))
  }

  flash_top <- reactive({ req(rv$flash); ti <- which(rv$flash$split %in% TRUE)[1]
    if (is.na(ti)) NULL else rv$flash[ti, , drop = FALSE] })

  output$flash_best_vb <- renderValueBox({ req(rv$flash); top <- flash_top()
    valueBox(if (is.null(top)) i18n_r()$t("none (no split)") else top$solvent_disp[1],
      i18n_r()$t("Best solvent"), icon = icon("trophy"), color = "green") })
  output$flash_sel_vb <- renderValueBox({ req(rv$flash); top <- flash_top()
    if (rv$flash_meta$ns_solute >= 2) {
      valueBox(if (is.null(top) || is.na(top$selectivity[1])) "—" else format(signif(top$selectivity[1], 4)),
        i18n_r()$t("Best selectivity (S)"), icon = icon("filter"), color = "aqua")
    } else { d1 <- if (is.null(top)) NA_real_ else top$D1[1]
      valueBox(if (is.na(d1)) "—" else format(signif(d1, 4)),
        i18n_r()$t("Best distribution D"), icon = icon("arrow-up-right-dots"), color = "aqua") } })
  output$flash_n_vb <- renderValueBox({ req(rv$flash)
    valueBox(sum(rv$flash$split %in% TRUE), i18n_r()$t("Two-phase (solvent × T) points"),
      icon = icon("layer-group"), color = "blue") })

  output$flash_table <- DT::renderDataTable({
    req(rv$flash); df <- rv$flash; sdisp <- rv$flash_meta$solutes_disp; ns_sol <- rv$flash_meta$ns_solute
    out <- .tbl0(nrow(df))
    out[[i18n_r()$t("Solvent")]] <- df$solvent_disp
    out[[i18n_r()$t("T (°C)")]] <- round(df$T_K - 273.15, 1)
    if (isTRUE(rv$flash_meta$sf_scan) && !is.null(df$sf_ratio)) out[[i18n_r()$t("Solvent : feed")]] <- signif(df$sf_ratio, 3)
    out[[i18n_r()$t("Two phases?")]] <- ifelse(df$split %in% TRUE, i18n_r()$t("yes"),
      sprintf("<span style='color:#b71c1c;'>&#9888; %s</span>", i18n_r()$t("no (single phase)")))
    for (k in seq_len(ns_sol)) out[[sprintf("%s: %s", i18n_r()$t("D"), sdisp[k])]] <- signif(df[[paste0("D", k)]], 4)
    if (ns_sol >= 2) out[[i18n_r()$t("Selectivity S")]] <- signif(df$selectivity, 4)
    out[[i18n_r()$t("Solvent in raffinate (mol·mol⁻¹)")]] <- signif(df$solvent_in_raffinate, 3)
    out[[i18n_r()$t("Feed in extract (mol·mol⁻¹)")]] <- signif(df$feed_in_extract, 3)
    DT::datatable(out, rownames = FALSE, selection = "none", escape = FALSE, extensions = "Buttons",
      options = list(pageLength = 15, order = list(), dom = "Bfrtip", language = tablang(),
        buttons = create_dt_export_buttons(i18n_r, "supercrit_lle_flash"))) })

  output$flash_phase_table <- DT::renderDataTable({
    req(rv$flash_best); ph <- rv$flash_best$phase
    out <- .tbl0(nrow(ph))
    out[[i18n_r()$t("Component")]] <- ph$disp
    out[[i18n_r()$t("Raffinate (feed-rich) x")]] <- signif(ph$x1, 4)
    out[[i18n_r()$t("Extract (solvent-rich) x")]] <- signif(ph$x2, 4)
    out[[i18n_r()$t("D = extract / raffinate")]] <- signif(ph$x2 / ph$x1, 4)
    cap <- if (is.null(rv$flash_best$sf_ratio))
      sprintf(i18n_r()$t("Best: %s at %s °C — extract-phase fraction β = %s"),
        rv$flash_best$solvent, round(rv$flash_best$Tc, 1), signif(rv$flash_best$beta, 3))
    else sprintf(i18n_r()$t("Best: %s at %s °C, solvent:feed %s — extract-phase fraction β = %s"),
      rv$flash_best$solvent, round(rv$flash_best$Tc, 1), signif(rv$flash_best$sf_ratio, 3), signif(rv$flash_best$beta, 3))
    DT::datatable(out, rownames = FALSE, selection = "none", escape = FALSE, extensions = "Buttons",
      caption = cap, options = list(pageLength = 12, order = list(), dom = "Bfrtip", language = tablang(),
        buttons = create_dt_export_buttons(i18n_r, "supercrit_lle_phase_compositions"))) })

  output$flash_plot <- plotly::renderPlotly({
    req(rv$flash); df <- rv$flash[rv$flash$split %in% TRUE, , drop = FALSE]; req(nrow(df) > 0)
    multi <- rv$flash_meta$ns_solute >= 2
    df$yval <- if (multi) df$selectivity else df$D1
    df$Tc <- df$T_K - 273.15
    best_by <- tapply(df$yval, df$solvent_disp, function(v) suppressWarnings(max(v, na.rm = TRUE)))
    topn <- names(sort(best_by, decreasing = TRUE))[seq_len(min(6, length(best_by)))]
    df <- df[df$solvent_disp %in% topn, , drop = FALSE]
    df$solvent_disp <- factor(df$solvent_disp, levels = topn)
    plotly::plot_ly(df, x = ~Tc, y = ~yval, color = ~solvent_disp, type = "scatter", mode = "lines+markers",
      hovertemplate = "%{fullData.name}<br>T = %{x:.1f} °C<br>%{y:.3g}<extra></extra>") |>
      plotly::layout(
        title = if (multi) i18n_r()$t("Separation selectivity vs temperature") else i18n_r()$t("Distribution coefficient vs temperature"),
        xaxis = list(title = i18n_r()$t("Temperature (°C)")),
        yaxis = list(title = if (multi) i18n_r()$t("Selectivity S") else i18n_r()$t("Distribution D")),
        legend = list(title = list(text = i18n_r()$t("Solvent")))) })

  # ---------- blend-composition optimizer ----------
  run_optimize <- function(cfg, solutes, feed) {
    pool_names <- cfg$solvents[nzchar(cfg$solvents)]
    if (length(pool_names) < 2) { notify(i18n_r()$t("Input required"),
      i18n_r()$t("Pick at least two candidate solvents to blend."), type = "warning"); return() }
    db <- cfg$db %||% "ud"
    pool <- solvent_ids(pool_names, "csac", db)
    id2disp <- stats::setNames(pool_names, pool)   # map resolved engine id -> original display name (e.g. MTBE)
    obj <- cfg$objective %||% "selectivity"
    if (identical(obj, "selectivity") && length(solutes) < 2) {
      notify(i18n_r()$t("Input required"),
        i18n_r()$t("Selectivity needs at least two solutes — add another target solute or switch the objective to distribution."), type = "warning"); return() }
    tmin <- cfg$temp_min; tmax <- cfg$temp_max; tstep <- cfg$temp_step
    if (is.null(tmin) || is.na(tmin)) tmin <- 25
    if (is.null(tmax) || is.na(tmax)) tmax <- tmin
    if (is.null(tstep) || is.na(tstep) || tstep <= 0) tstep <- 10
    if (tmax < tmin) { tt <- tmin; tmin <- tmax; tmax <- tt }
    Tgrid <- seq(tmin, tmax, by = tstep); if (!length(Tgrid)) Tgrid <- tmin
    sr <- cfg$solvent_ratio; if (is.null(sr) || is.na(sr) || sr <= 0) sr <- 1
    sf <- cfg$solute_frac;   if (is.null(sf) || is.na(sf) || sf <= 0) sf <- 0.01
    ms <- cfg$multistart;    if (is.null(ms) || is.na(ms) || ms < 1) ms <- 4

    # Only BINARY mixtures are physically sensible (a pure solvent or one co-solvent) — an N-way blend is
    # impractical and explodes the simplex grid. Pre-rank the pool, then optimise the composition of EACH
    # candidate PAIR (a binary blend; the pure endpoints are included) and rank the pairs by objective.
    K <- 5L
    if (length(pool) > K) {
      pre <- tryCatch(withProgress(message = i18n_r()$t("Pre-ranking candidate solvents..."), value = 0.1,
        supeRcrit::csac_lle(solutes[1], solvents = pool, feed = feed, T = mean(Tgrid) + 273.15,
          dsp = isTRUE(cfg$dsp), database = db)), error = function(e) NULL)
      if (!is.null(pre) && nrow(pre)) pool <- pool[pool %in% utils::head(pre$solvent[order(-pre$D)], K)]
    }
    if (length(pool) < 2) { notify(i18n_r()$t("Input required"),
      i18n_r()$t("Need at least two solvents that resolve in this database to form binary blends."), type = "warning"); return() }
    pairs <- utils::combn(pool, 2, simplify = FALSE)
    res_list <- tryCatch(withProgress(message = i18n_r()$t("Optimizing binary blends..."), value = 0.2, {
      lapply(seq_along(pairs), function(ii) {
        setProgress(value = 0.2 + 0.75 * ii / length(pairs))
        r <- tryCatch(supeRcrit::csac_lle_optimize_blend(solutes, solvents = pairs[[ii]], feed = feed, T = Tgrid + 273.15,
               objective = obj, solute_frac = sf, solvent_ratio = sr, dsp = isTRUE(cfg$dsp),
               database = db, multistart = as.integer(min(ms, 2L)), warm = isTRUE(cfg$warm),   # binary is 1-D
               method = cfg$opt_method %||% "gamma_inf"), error = function(e) NULL)
        if (is.null(r) || is.null(r$composition)) return(NULL)
        r$solvents <- pairs[[ii]]; r
      }) }), error = function(e) e)
    if (inherits(res_list, "error")) { notify(i18n_r()$t("Optimization failed"), conditionMessage(res_list), type = "error"); return() }
    res_list <- Filter(Negate(is.null), res_list)
    if (!length(res_list)) { notify(i18n_r()$t("No two-phase blend"),
      i18n_r()$t("No binary blend of these solvents formed two liquid phases under the given conditions. Try different solvents, a larger solvent:feed ratio, or another temperature."), type = "warning"); rv$opt <- NULL; return() }
    res_list <- res_list[order(-vapply(res_list, function(r) r$value %||% -Inf, numeric(1)))]
    o <- res_list[[1]]; pool_best <- o$solvents
    disp1 <- function(s) { d <- unname(id2disp[s]); if (!is.na(d) && nzchar(d)) return(d)   # resolved id -> display name
      k <- match(s, if (identical(db,"chaos")) SOLV$smiles else SOLV$name)
      if (is.na(k)) pretty(s, db) else SOLV$name[k] }
    rv$opt <- o
    rv$opt_meta <- list(solvents = pool_best, solvents_disp = vapply(pool_best, disp1, character(1)),
      solutes = solutes, solutes_disp = vapply(solutes, function(s) disp_name(s, db, "csac"), character(1)),
      objective = obj, feed = disp_name(feed, db, "csac"), n_pairs = length(res_list))
    # Ranked results: pure solvents labelled by name, genuine binaries as "A x% · B y%". Many pairs collapse
    # to the same pure solvent (co-solvent optimised to 0%), so DEDUPLICATE by the blend identity.
    .blend_label <- function(solv_ids, comp) {
      sd <- vapply(solv_ids, disp1, character(1)); cp <- round(100 * comp); kk <- cp >= 1
      if (sum(kk) <= 1) { i <- if (any(kk)) which(kk)[1] else which.max(cp); sd[i] }   # pure solvent -> just the name
      else paste(sprintf("%s %d%%", sd[kk], cp[kk]), collapse = " · ")
    }
    rv$opt_pairs <- do.call(rbind, lapply(res_list, function(r) data.frame(
      Blend = .blend_label(r$solvents, r$composition),
      Value = signif(r$value %||% NA_real_, 4), Tc = round((r$T_K %||% NA_real_) - 273.15, 1),
      stringsAsFactors = FALSE)))
    rv$opt_pairs <- rv$opt_pairs[!duplicated(rv$opt_pairs$Blend), , drop = FALSE]   # collapse pure-solvent dups
    rv$opt_best_label <- .blend_label(pool_best, o$composition)
    updateTabsetPanel(session, "results_tabs", selected = i18n_r()$t("Blend Optimizer"))
  }

  output$opt_recipe_vb <- renderValueBox({ req(rv$opt)
    valueBox(rv$opt_best_label %||% "—", i18n_r()$t("Best solvent / blend"), icon = icon("flask-vial"), color = "green") })
  output$opt_value_vb <- renderValueBox({ req(rv$opt)
    lbl <- if (identical(rv$opt_meta$objective, "selectivity")) i18n_r()$t("Best selectivity (S)") else i18n_r()$t("Best distribution D")
    valueBox(format(signif(rv$opt$value, 4)), lbl, icon = icon("bullseye"), color = "aqua") })
  output$opt_temp_vb <- renderValueBox({ req(rv$opt)
    valueBox(sprintf("%.0f °C", rv$opt$T_K - 273.15), i18n_r()$t("Best temperature"), icon = icon("temperature-half"), color = "blue") })

  output$opt_recipe_table <- DT::renderDataTable({ req(rv$opt); m <- rv$opt_meta
    pct <- round(100 * rv$opt$composition, 1); keep <- pct >= 1; if (!any(keep)) keep <- rep(TRUE, length(pct))
    out <- .tbl0(sum(keep))
    out[[i18n_r()$t("Solvent")]] <- m$solvents_disp[keep]
    out[[i18n_r()$t("Blend fraction (mol %)")]] <- pct[keep]
    DT::datatable(out, rownames = FALSE, selection = "none", extensions = "Buttons",
      caption = sprintf(i18n_r()$t("At %.0f °C — per-solute D: %s"), rv$opt$T_K - 273.15,
        paste(sprintf("%s = %.3g", m$solutes_disp, rv$opt$D), collapse = ", ")),
      options = list(dom = "Bfrtip", order = list(), language = tablang(),
        buttons = create_dt_export_buttons(i18n_r, "supercrit_lle_blend_recipe"))) })
  # Ranked candidate binary blends (pure solvents + co-solvent pairs), best first.
  output$opt_trace_table <- DT::renderDataTable({ req(rv$opt_pairs); pr <- rv$opt_pairs
    out <- .tbl0(nrow(pr))
    out[[i18n_r()$t("Solvent / blend")]] <- pr$Blend
    lbl <- if (identical(rv$opt_meta$objective, "selectivity")) i18n_r()$t("Selectivity S") else i18n_r()$t("Distribution D")
    out[[lbl]] <- pr$Value
    out[[i18n_r()$t("T (°C)")]] <- pr$Tc
    DT::datatable(out, rownames = FALSE, selection = "none", extensions = "Buttons",
      caption = i18n_r()$t("Candidate solvents & blends, ranked best first."),
      options = list(pageLength = 10, order = list(), dom = "Bfrtip", language = tablang(),
        buttons = create_dt_export_buttons(i18n_r, "supercrit_lle_candidate_blends"))) })

  # ---------- value boxes (screen) ----------
  output$best_solvent_vb <- renderValueBox({ req(rv$res)
    valueBox(rv$res$solvent[1], i18n_r()$t("Top solvent (highest D)"), icon = icon("trophy"), color = "green") })
  output$best_d_vb <- renderValueBox({ req(rv$res)
    valueBox(format(signif(rv$res$D[1], 4), big.mark = ","),
      i18n_r()$t("Distribution coefficient D"), icon = icon("arrow-up-right-dots"), color = "aqua") })
  output$n_candidates_vb <- renderValueBox({ req(rv$res)
    valueBox(nrow(rv$res), i18n_r()$t("Solvents evaluated"), icon = icon("flask"), color = "blue") })

  # ---------- ranking table ----------
  output$ranking_table <- DT::renderDataTable({
    req(rv$res); df <- rv$res
    out <- .tbl0(nrow(df))
    out[[i18n_r()$t("Solvent")]] <- df$solvent
    out[[i18n_r()$t("γ∞ (solvent)")]] <- signif(df$gamma_solvent, 4)
    out[[i18n_r()$t("D = γ∞feed / γ∞solvent")]] <- signif(df$D, 4)
    out[[i18n_r()$t("Capacity = 1/γ∞")]] <- signif(df$capacity, 3)
    if (!is.null(df$selectivity)) out[[i18n_r()$t("Selectivity")]] <- signif(df$selectivity, 4)
    band <- supeRcrit::lle_solvent_band(ifelse(is.na(df$gsk), 2L, df$gsk))
    out[[i18n_r()$t("Sustainability")]] <- ifelse(is.na(df$gsk),
      "<span style='color:#888;'>&mdash;</span>",
      sprintf("<span style='color:%s;font-weight:600;'>&#9679; %s</span>", band$colour, band$label))
    ich_lbl <- ifelse(is.na(df$ich), "",
      ifelse(is.na(df$ppm), sprintf("ICH %d. ", df$ich),
             sprintf("ICH %d (≤%s ppm). ", df$ich, format(df$ppm, big.mark = ",", trim = TRUE))))
    food_lbl <- ifelse(!is.na(df$food) & df$food, sprintf("<span style='color:#2e7d32;'>%s</span>. ", i18n_r()$t("Food-grade")), "")
    psep <- if (!is.null(df$feed_miscible)) ifelse(df$feed_miscible %in% TRUE,
      sprintf("<span style='color:#b71c1c;font-weight:600;'>&#9888; %s</span>. ",
        i18n_r()$t("Miscible with feed – single phase")), "") else ""
    # Hazard flags are stored lower-case without terminal punctuation — capitalise the first letter and add
    # a full stop so the Notes read as proper sentences (matches the Co-Solvent Selection module's gsk_notes).
    haz <- ifelse(is.na(df$hazard) | !nzchar(df$hazard), "",
      paste0(toupper(substr(df$hazard, 1, 1)), substr(df$hazard, 2, nchar(df$hazard)), "."))
    notes <- trimws(paste0(psep, food_lbl, ich_lbl, haz))
    out[[i18n_r()$t("Notes")]] <- notes
    out[["_miscible"]] <- as.integer(if (!is.null(df$feed_miscible)) df$feed_miscible %in% TRUE else rep(FALSE, nrow(df)))
    DT::datatable(out, rownames = FALSE, selection = "none", escape = FALSE, extensions = "Buttons",
      options = list(pageLength = 15, order = list(), dom = "Bfrtip", language = tablang(),
        buttons = create_dt_export_buttons(i18n_r, "supercrit_lle_ranked_solvents",
          export_options = list(columns = ":visible")),   # drop the hidden _miscible flag from exports
        columnDefs = list(list(visible = FALSE, targets = ncol(out) - 1L)))) |>   # hide the flag column
      DT::formatStyle("_miscible", target = "row",                                # miscible-with-feed rows -> red
        backgroundColor = DT::styleEqual(1, "#f8d7da"),
        color = DT::styleEqual(1, "#7a1620")) })

  # (On-screen distribution-coefficient bar removed per owner request — the ranked table conveys the D values;
  #  the ggplot `ranking_gg` above is retained for the PDF export only.)

  # Solute sigma-profile ggplot (also reused by the PDF export). Always overlays the SMILES-based FastSigma
  # estimate when a structure is available — but the engine uses the Experimental (database) profile unless
  # an off-database SMILES was entered (then FastSigma IS the profile used, labelled "estimated").
  sigma_gg <- reactive({
    sol <- input$solutes[1]; db <- input$database %||% "ud"
    if (is.null(sol) || !nzchar(sol)) return(NULL)
    pr <- tryCatch(supeRcrit::csac_profile(sol, db, predict = TRUE), error = function(e) NULL)
    if (is.null(pr) || !length(pr)) return(NULL)
    labs <- list()
    if (!is.null(pr$database))  labs[[i18n_r()$t("Experimental")]] <- pr$database
    if (!is.null(pr$predicted)) labs[[i18n_r()$t(if (is.null(pr$database)) "FastSigma (estimated)" else "FastSigma")]] <- pr$predicted
    if (!length(labs)) return(NULL)
    ttl <- disp_name(sol, db, input$engine %||% "csac")                 # common name, never the raw SMILES
    if (identical(db, "chaos") && (!nzchar(ttl) || identical(ttl, sol))) ttl <- i18n_r()$t("Custom solute")
    supeRcrit::plot_sigma_profile(labs, title = ttl)
  })
  output$sigma_plot <- renderPlot({ g <- sigma_gg(); if (is.null(g)) return(NULL); g })

  # ---------- ggplot versions of the plots (reused by the PDF export) ----------
  ranking_gg <- reactive({
    req(rv$res); df <- utils::head(rv$res, 15); df <- df[order(df$D), ]
    df$solvent <- factor(df$solvent, levels = df$solvent)
    ggplot2::ggplot(df, ggplot2::aes(x = D, y = solvent)) +
      ggplot2::geom_col(width = 0.55, fill = "#2c7fb8") +
      ggplot2::scale_x_log10() +
      ggplot2::labs(x = i18n_r()$t("Distribution coefficient D"), y = NULL,
        title = sprintf("%s: %s ← %s", i18n_r()$t("Top extraction solvents"), rv$meta$solute %||% "", rv$meta$feed %||% "")) +
      ggplot2::theme_minimal(base_size = 11)
  })
  flash_gg <- reactive({
    req(rv$flash); df <- rv$flash[rv$flash$split %in% TRUE, , drop = FALSE]; req(nrow(df) > 0)
    multi <- rv$flash_meta$ns_solute >= 2
    df$yval <- if (multi) df$selectivity else df$D1; df$Tc <- df$T_K - 273.15
    best_by <- tapply(df$yval, df$solvent_disp, function(v) suppressWarnings(max(v, na.rm = TRUE)))
    topn <- names(sort(best_by, decreasing = TRUE))[seq_len(min(6, length(best_by)))]
    df <- df[df$solvent_disp %in% topn, , drop = FALSE]; df$solvent_disp <- factor(df$solvent_disp, levels = topn)
    ggplot2::ggplot(df, ggplot2::aes(x = Tc, y = yval, colour = solvent_disp)) +
      ggplot2::geom_line(linewidth = 0.9) + ggplot2::geom_point(size = 1.6) +
      ggplot2::labs(x = i18n_r()$t("Temperature (°C)"),
        y = if (multi) i18n_r()$t("Selectivity S") else i18n_r()$t("Distribution D"), colour = i18n_r()$t("Solvent"),
        title = if (multi) i18n_r()$t("Separation selectivity vs temperature") else i18n_r()$t("Distribution coefficient vs temperature")) +
      ggplot2::theme_minimal(base_size = 11)
  })

  # ---------- Export (ONE contextual dropdown in the action row): PDF report + CSV of the LATEST results ----------
  # Enabled whenever any run has produced results; the export emits a titled section for EVERY populated
  # result (screen / flash / optimize), not just the last run.
  output$has_any_results <- reactive({ !is.null(rv$res) || !is.null(rv$flash) || !is.null(rv$opt) })
  outputOptions(output, "has_any_results", suspendWhenHidden = FALSE)

  .fname   <- function(stub) if (exists("generate_filename_with_timestamp")) generate_filename_with_timestamp(stub) else paste0(stub, "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  .applogo <- function() if (file.exists("www/altraflora-15-min.png")) "www/altraflora-15-min.png" else NULL
  .appver  <- function() tryCatch(paste0("supeRcrit v", as.character(utils::packageVersion("supeRcrit"))), error = function(e) "supeRcrit")

  # ----- Shared formatted tables, mirroring the on-screen tables exactly (used by BOTH PDF and CSV export) -----
  # Ranked Solvents table: the full on-screen column set incl. Sustainability band label + the plain-text Notes
  # (ICH / Food-grade / hazard / miscible-with-feed) — same construction as output$ranking_table, minus HTML.
  .tbl_screen <- function() {
    if (is.null(rv$res)) return(NULL); r <- rv$res
    out <- data.frame(r$solvent, signif(r$gamma_solvent, 4), signif(r$D, 4), signif(r$capacity, 3),
                      check.names = FALSE, stringsAsFactors = FALSE)
    names(out) <- c(i18n_r()$t("Solvent"), i18n_r()$t("γ∞ (solvent)"), i18n_r()$t("D = γ∞feed / γ∞solvent"), i18n_r()$t("Capacity = 1/γ∞"))
    if (!is.null(r$selectivity)) out[[i18n_r()$t("Selectivity")]] <- signif(r$selectivity, 4)
    band <- supeRcrit::lle_solvent_band(ifelse(is.na(r$gsk), 2L, r$gsk))
    out[[i18n_r()$t("Sustainability")]] <- ifelse(is.na(r$gsk), "—", band$label)
    ich_lbl <- ifelse(is.na(r$ich), "", ifelse(is.na(r$ppm), sprintf("ICH %d. ", r$ich),
                      sprintf("ICH %d (≤%s ppm). ", r$ich, format(r$ppm, big.mark = ",", trim = TRUE))))
    food_lbl <- ifelse(!is.na(r$food) & r$food, sprintf("%s. ", i18n_r()$t("Food-grade")), "")
    psep <- if (!is.null(r$feed_miscible)) ifelse(r$feed_miscible %in% TRUE, sprintf("%s. ", i18n_r()$t("Miscible with feed – single phase")), "") else ""
    haz <- ifelse(is.na(r$hazard) | !nzchar(r$hazard), "", paste0(toupper(substr(r$hazard, 1, 1)), substr(r$hazard, 2, nchar(r$hazard)), "."))
    out[[i18n_r()$t("Notes")]] <- trimws(paste0(psep, food_lbl, ich_lbl, haz))
    out
  }
  .tbl_flash <- function() {
    if (is.null(rv$flash)) return(NULL)
    fl <- rv$flash[rv$flash$split %in% TRUE, , drop = FALSE]; if (!nrow(fl)) return(NULL)
    ns <- rv$flash_meta$ns_solute %||% 1L; sdisp <- rv$flash_meta$solutes_disp
    out <- data.frame(fl$solvent_disp, round(fl$T_K - 273.15, 1), check.names = FALSE, stringsAsFactors = FALSE)
    names(out) <- c(i18n_r()$t("Solvent"), i18n_r()$t("T (°C)"))
    for (k in seq_len(ns)) out[[sprintf("%s: %s", i18n_r()$t("D"), sdisp[k])]] <- signif(fl[[paste0("D", k)]], 4)   # per-solute D (matches the on-screen table)
    if (ns >= 2 && !is.null(fl$selectivity)) out[[i18n_r()$t("Selectivity S")]] <- signif(fl$selectivity, 4)
    out[[i18n_r()$t("Solvent in raffinate (mol·mol⁻¹)")]] <- signif(fl$solvent_in_raffinate, 3)
    out[[i18n_r()$t("Feed in extract (mol·mol⁻¹)")]] <- signif(fl$feed_in_extract, 3)
    out
  }
  # Phase compositions of the best (solvent, temperature) — mirrors output$flash_phase_table.
  .tbl_phase <- function() {
    if (is.null(rv$flash_best) || is.null(rv$flash_best$phase)) return(NULL); ph <- rv$flash_best$phase
    out <- data.frame(ph$disp, signif(ph$x1, 4), signif(ph$x2, 4), signif(ph$x2 / ph$x1, 4), check.names = FALSE, stringsAsFactors = FALSE)
    names(out) <- c(i18n_r()$t("Component"), i18n_r()$t("Raffinate (feed-rich) x"),
                    i18n_r()$t("Extract (solvent-rich) x"), i18n_r()$t("D = extract / raffinate"))
    out
  }
  .phase_caption <- function() {
    if (is.null(rv$flash_best)) return(NULL)
    if (is.null(rv$flash_best$sf_ratio))
      sprintf(i18n_r()$t("Best: %s at %s °C — extract-phase fraction β = %s"), rv$flash_best$solvent, round(rv$flash_best$Tc, 1), signif(rv$flash_best$beta, 3))
    else sprintf(i18n_r()$t("Best: %s at %s °C, solvent:feed %s — extract-phase fraction β = %s"), rv$flash_best$solvent, round(rv$flash_best$Tc, 1), signif(rv$flash_best$sf_ratio, 3), signif(rv$flash_best$beta, 3))
  }
  # Best blend composition (per-component mol %) — mirrors output$opt_recipe_table.
  .tbl_recipe <- function() {
    if (is.null(rv$opt) || is.null(rv$opt_meta)) return(NULL)
    pct <- round(100 * rv$opt$composition, 1); keep <- pct >= 1; if (!any(keep)) keep <- rep(TRUE, length(pct))
    out <- data.frame(rv$opt_meta$solvents_disp[keep], pct[keep], check.names = FALSE, stringsAsFactors = FALSE)
    names(out) <- c(i18n_r()$t("Solvent"), i18n_r()$t("Blend fraction (mol %)")); out
  }
  # Ranked candidate solvents & blends — mirrors output$opt_trace_table.
  .tbl_trace <- function() {
    if (is.null(rv$opt_pairs)) return(NULL); sel <- identical(rv$opt_meta$objective, "selectivity")
    out <- data.frame(rv$opt_pairs$Blend, rv$opt_pairs$Value, rv$opt_pairs$Tc, check.names = FALSE, stringsAsFactors = FALSE)
    names(out) <- c(i18n_r()$t("Solvent / blend"), if (sel) i18n_r()$t("Selectivity S") else i18n_r()$t("Distribution D"), i18n_r()$t("T (°C)")); out
  }
  # ----- PDF-specific formatting (the on-screen DT tables colour-code + badge; the PDF mirrors that) -----
  # Number formatters (elementwise → no cross-row decimal alignment): D / selectivity as thousands-separated
  # integers; mole fractions at 4 dp; γ∞ / capacity at sig-figs; temperature as a clean integer / half-degree.
  .pf_D   <- function(x) vapply(x, function(v) if (!is.finite(v)) "—" else      # 4 sig figs, matching the on-screen
                 format(signif(v, 4), big.mark = ",", trim = TRUE, scientific = FALSE), character(1))  # value boxes/DT (17,030 not 17,030.00; keeps O(1) D distinguishable)
  .pf_mf  <- function(x) ifelse(is.finite(x), formatC(x, format = "f", digits = 4), "—")
  .pf_num <- function(x, d = 4) vapply(x, function(v) if (!is.finite(v)) "—" else
                 format(signif(v, d), big.mark = ",", trim = TRUE, scientific = FALSE), character(1))
  .pf_T   <- function(x) vapply(x, function(v) if (!is.finite(v)) "—" else
                 format(round(v, 1), trim = TRUE, nsmall = 0, scientific = FALSE), character(1))
  .KPI <- list(green = "#2e7d32", aqua = "#0097a7", blue = "#1565c0")               # best / metric / count badges
  .ctx <- function(sol, fd) list(list(label = i18n_r()$t("Solute(s)"), value = sol %||% ""),
                                  list(label = i18n_r()$t("Feed / diluent"), value = fd %||% ""))

  # Coloured KPI "value boxes" echoing each tab's badges (rp_kpi_block = a row of filled boxes).
  .kpis_screen <- function() list(
    list(label = i18n_r()$t("Best solvent"), value = rv$res$solvent[1], color = .KPI$green),
    list(label = i18n_r()$t("Distribution coefficient D"), value = .pf_D(rv$res$D[1]), color = .KPI$aqua),
    list(label = i18n_r()$t("Solvents evaluated"), value = as.character(nrow(rv$res)), color = .KPI$blue))
  .kpis_flash <- function() { fl <- rv$flash[rv$flash$split %in% TRUE, , drop = FALSE]
    multi <- (rv$flash_meta$ns_solute %||% 1L) >= 2; bv <- if (multi) fl$selectivity else fl$D1
    bi <- which.max(ifelse(is.finite(bv), bv, -Inf))
    list(list(label = i18n_r()$t("Best solvent"), value = fl$solvent_disp[bi], color = .KPI$green),
      list(label = if (multi) i18n_r()$t("Best selectivity (S)") else i18n_r()$t("Best distribution D"),
        value = .pf_D(bv[bi]), color = .KPI$aqua),
      list(label = i18n_r()$t("Two-phase (solvent × T) points"), value = as.character(nrow(fl)), color = .KPI$blue)) }
  .kpis_optimize <- function() list(
    list(label = i18n_r()$t("Best solvent / blend"), value = rv$opt_best_label %||% "", color = .KPI$green),
    list(label = if (identical(rv$opt_meta$objective, "selectivity")) i18n_r()$t("Best selectivity (S)") else i18n_r()$t("Best distribution D"),
         value = .pf_D(rv$opt$value), color = .KPI$aqua),
    list(label = i18n_r()$t("Best temperature"), value = paste0(.pf_T(rv$opt$T_K - 273.15), " °C"), color = .KPI$blue))

  # Ranked-solvents PDF table (top 20) with a per-cell colour matrix: Sustainability cells take the GSK band
  # colour, Notes cells go red (miscible-with-feed warning) or green (food-grade) like the on-screen spans.
  .pdf_screen_tbl <- function() {
    if (is.null(rv$res)) return(NULL); r <- utils::head(rv$res, 20)
    band <- supeRcrit::lle_solvent_band(ifelse(is.na(r$gsk), 2L, r$gsk))
    cols <- list(r$solvent, .pf_num(r$gamma_solvent, 4), .pf_D(r$D), .pf_num(r$capacity, 3))
    nm   <- c(i18n_r()$t("Solvent"), i18n_r()$t("γ∞ (solvent)"), i18n_r()$t("D = γ∞feed / γ∞solvent"), i18n_r()$t("Capacity = 1/γ∞"))
    if (!is.null(r$selectivity)) { cols <- c(cols, list(.pf_D(r$selectivity))); nm <- c(nm, i18n_r()$t("Selectivity")) }
    cols <- c(cols, list(ifelse(is.na(r$gsk), "—", band$label))); nm <- c(nm, i18n_r()$t("Sustainability"))
    ich_lbl <- ifelse(is.na(r$ich), "", ifelse(is.na(r$ppm), sprintf("ICH %d. ", r$ich),
                      sprintf("ICH %d (≤%s ppm). ", r$ich, format(r$ppm, big.mark = ",", trim = TRUE))))
    food_lbl <- ifelse(!is.na(r$food) & r$food, sprintf("%s. ", i18n_r()$t("Food-grade")), "")
    fm   <- if (!is.null(r$feed_miscible)) r$feed_miscible %in% TRUE else rep(FALSE, nrow(r))
    psep <- ifelse(fm, sprintf("%s. ", i18n_r()$t("Miscible with feed – single phase")), "")
    haz  <- ifelse(is.na(r$hazard) | !nzchar(r$hazard), "", paste0(toupper(substr(r$hazard, 1, 1)), substr(r$hazard, 2, nchar(r$hazard)), "."))
    cols <- c(cols, list(trimws(paste0(psep, food_lbl, ich_lbl, haz)))); nm <- c(nm, i18n_r()$t("Notes"))
    names(cols) <- nm; out <- as.data.frame(cols, check.names = FALSE, stringsAsFactors = FALSE)
    tc <- matrix(NA_character_, nrow = nrow(out), ncol = ncol(out))
    tc[, which(nm == i18n_r()$t("Sustainability"))] <- ifelse(is.na(r$gsk), NA_character_, band$colour)
    tc[, which(nm == i18n_r()$t("Notes"))] <- ifelse(fm, "#b71c1c", ifelse(!is.na(r$food) & r$food, "#2e7d32", NA_character_))
    list(df = out, text_col = tc)
  }
  # Two-phase flash PDF table: D (integer), optional selectivity (integer), raffinate/extract leakage at 4 dp.
  .pdf_flash_tbl <- function() {
    if (is.null(rv$flash)) return(NULL)
    fl <- rv$flash[rv$flash$split %in% TRUE, , drop = FALSE]; if (!nrow(fl)) return(NULL)
    ns <- rv$flash_meta$ns_solute %||% 1L; sdisp <- rv$flash_meta$solutes_disp
    cols <- list(fl$solvent_disp, .pf_T(fl$T_K - 273.15)); nm <- c(i18n_r()$t("Solvent"), i18n_r()$t("T (°C)"))
    for (k in seq_len(ns)) { cols <- c(cols, list(.pf_D(fl[[paste0("D", k)]]))); nm <- c(nm, sprintf("%s: %s", i18n_r()$t("D"), sdisp[k])) }
    if (ns >= 2 && !is.null(fl$selectivity)) { cols <- c(cols, list(.pf_D(fl$selectivity))); nm <- c(nm, i18n_r()$t("Selectivity S")) }
    cols <- c(cols, list(.pf_mf(fl$solvent_in_raffinate), .pf_mf(fl$feed_in_extract)))
    nm   <- c(nm, i18n_r()$t("Solvent in raffinate (mol·mol⁻¹)"), i18n_r()$t("Feed in extract (mol·mol⁻¹)"))
    names(cols) <- nm; utils::head(as.data.frame(cols, check.names = FALSE, stringsAsFactors = FALSE), 25)
  }
  # Phase compositions of the best (solvent, T): mole fractions at 4 dp, D as integer.
  .pdf_phase_tbl <- function() {
    if (is.null(rv$flash_best) || is.null(rv$flash_best$phase)) return(NULL); ph <- rv$flash_best$phase
    cols <- list(ph$disp, .pf_mf(ph$x1), .pf_mf(ph$x2), .pf_D(ph$x2 / ph$x1))
    names(cols) <- c(i18n_r()$t("Component"), i18n_r()$t("Raffinate (feed-rich) x"),
                     i18n_r()$t("Extract (solvent-rich) x"), i18n_r()$t("D = extract / raffinate"))
    as.data.frame(cols, check.names = FALSE, stringsAsFactors = FALSE)
  }
  # Candidate solvents & blends (ranked best-first): D/S as integer, T clean; the best row is highlighted bold
  # (so the separate "optimal composition" table is no longer needed).
  .pdf_trace_tbl <- function() {
    if (is.null(rv$opt_pairs)) return(NULL); sel <- identical(rv$opt_meta$objective, "selectivity")
    cols <- list(rv$opt_pairs$Blend, .pf_D(rv$opt_pairs$Value), .pf_T(rv$opt_pairs$Tc))
    names(cols) <- c(i18n_r()$t("Solvent / blend"), if (sel) i18n_r()$t("Selectivity S") else i18n_r()$t("Distribution D"), i18n_r()$t("T (°C)"))
    as.data.frame(cols, check.names = FALSE, stringsAsFactors = FALSE)
  }

  # The PDF `export`: one section per on-screen tab (mirroring layout) for EVERY populated result. Each carries
  # its OWN Solute(s)/Feed context (tabs may differ), coloured KPI badges, a colour-coded table, and the plot
  # underneath (no duplicate plot-title: the section header is the single label; redundant ggplot titles stripped).
  .lle_pdf_export <- function() {
    secs <- list()
    if (!is.null(rv$res)) {
      st <- .pdf_screen_tbl()
      secs <- c(secs, list(list(
        title = i18n_r()$t("Quick screen — ranked solvents"),
        context = .ctx(rv$meta$solute, rv$meta$feed), kpis = .kpis_screen(),
        table = st$df, text_col = st$text_col,
        plots = list(list(plot = tryCatch(sigma_gg(), error = function(e) NULL),
                          title = i18n_r()$t("Solute σ-profile"), height = 3.0)))))
    }
    ft <- .pdf_flash_tbl()
    if (!is.null(ft)) {
      fctx <- .ctx(paste(rv$flash_meta$solutes_disp, collapse = ", "), rv$flash_meta$feed)
      secs <- c(secs, list(list(
        title = i18n_r()$t("Two-phase flash — results"), context = fctx, kpis = .kpis_flash(), table = ft,
        plots = list(list(plot = tryCatch(flash_gg() + ggplot2::labs(title = NULL), error = function(e) NULL),
                          title = i18n_r()$t("Distribution vs temperature"), height = 3.6)))))
      pt <- .pdf_phase_tbl()
      if (!is.null(pt)) secs <- c(secs, list(list(
        title = i18n_r()$t("Phase compositions (best solvent / temperature)"),
        paragraph = .phase_caption(), table = pt)))
    }
    tt <- .pdf_trace_tbl()
    if (!is.null(tt)) {
      bold <- tt[[1]] == (rv$opt_best_label %||% ""); if (!any(bold)) bold <- seq_len(nrow(tt)) == 1L
      secs <- c(secs, list(list(
        title = i18n_r()$t("Blend optimizer — candidate solvents & blends"),
        context = .ctx(paste(rv$opt_meta$solutes_disp, collapse = ", "), rv$opt_meta$feed),
        kpis = .kpis_optimize(), table = tt, bold = bold)))
    }
    if (!length(secs)) return(NULL)
    list(intro = i18n_r()$t("Liquid-liquid extraction results (COSMO-SAC family) — all available analyses."),
      sections = secs)
  }
  # CSV: one titled block per on-screen table, stacked, with a UTF-8 BOM.
  .lle_csv_blocks <- function() {
    bl <- list(); add <- function(title, df) if (!is.null(df)) bl[[length(bl) + 1L]] <<- list(title = title, df = df)
    add(i18n_r()$t("Quick screen — ranked solvents"), .tbl_screen())
    add(i18n_r()$t("Two-phase flash — points (solvent × T)"), .tbl_flash())
    add(i18n_r()$t("Phase compositions (best solvent / temperature)"), .tbl_phase())
    add(i18n_r()$t("Blend optimizer — optimal composition"), .tbl_recipe())
    add(i18n_r()$t("Blend optimizer — candidate solvents & blends"), .tbl_trace())
    bl
  }
  output$export_csv <- downloadHandler(
    filename = function() paste0(.fname("supercrit_lle_results"), ".csv"),
    content = function(file) { blocks <- .lle_csv_blocks(); req(length(blocks) > 0)
      con <- file(file, "wb"); on.exit(close(con), add = TRUE)
      writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con)   # UTF-8 BOM so Excel renders °C / symbols correctly
      for (i in seq_along(blocks)) { b <- blocks[[i]]
        if (i > 1) writeBin(charToRaw("\r\n"), con)                                   # blank line between sections
        writeBin(charToRaw(enc2utf8(paste0('"', gsub('"', '""', b$title), '"\r\n'))), con)   # section title row
        tmp <- tempfile(fileext = ".csv"); on.exit(unlink(tmp), add = TRUE)
        utils::write.csv(b$df, tmp, row.names = FALSE, na = "", fileEncoding = "UTF-8")
        writeBin(readBin(tmp, "raw", n = file.info(tmp)$size), con) } },
    contentType = "text/csv")
  output$export_pdf <- downloadHandler(
    filename = function() paste0(.fname("supercrit_lle_report"), ".pdf"),
    content = function(file) {
      pid <- showNotification(i18n_r()$t("Preparing export..."), type = "message", duration = NULL); on.exit(removeNotification(pid), add = TRUE)
      tryCatch({
        export <- .lle_pdf_export(); req(!is.null(export))
        build_lle_pdf_report(export, i18n_r(), file, company = .appver(), logo = .applogo())
      }, error = function(e) showNotification(paste(i18n_r()$t("Error exporting plots:"), e$message), type = "error", duration = 8))
    })

  # ---------- Per-plot download (single PDF via ggsave on the plot's ggplot twin), matching Co-Solvent Comparison ----------
  .plot_dl <- function(gg_fun, stub) downloadHandler(
    filename = function() paste0(.fname(stub), ".pdf"),
    content = function(file) { g <- tryCatch(gg_fun(), error = function(e) NULL); req(!is.null(g))
      ggplot2::ggsave(file, plot = g, device = cairo_pdf, width = 10, height = 7, units = "in")
      showNotification(i18n_r()$t("Done!"), type = "message", duration = 3) },
    contentType = "application/pdf")
  output$dl_sigma_plot <- .plot_dl(sigma_gg, "supercrit_lle_sigma_profile")
  output$dl_flash_plot <- .plot_dl(flash_gg, "supercrit_lle_distribution_vs_T")

  # ---------- built-in industry examples ----------
  # Real, well-known liquid-liquid extractions (all compounds present in the curated UD database).
  lle_examples <- list(
    list(label = "Phenol recovery from water (screen)",
         engine = "csac", db = "ud", mode = "screen",
         solutes = "PHENOL", feed = "WATER", temp = 25,
         solvents = c("Methyl isobutyl ketone", "n-Butyl acetate", "Diisopropyl ether", "Anisole", "Cyclohexane")),
    list(label = "Caffeine decaffeination (screen)",
         engine = "csac", db = "ud", mode = "screen",
         solutes = "CAFFEINE", feed = "WATER", temp = 25,
         solvents = c("Ethyl acetate", "Dichloromethane", "Methyl isobutyl ketone")),
    list(label = "Biobutanol recovery, ABE (screen)",
         engine = "csac", db = "ud", mode = "screen",
         solutes = "1-BUTANOL", feed = "WATER", temp = 37,
         solvents = c("1-Octanol", "n-Butyl acetate", "Methyl isobutyl ketone", "Isooctane")),
    list(label = "Aromatic/aliphatic split, sulfolane (flash)",
         engine = "csac", db = "ud", mode = "flash",
         solutes = c("TOLUENE", "N-HEXANE"), feed = "N-HEPTANE",
         temp_min = 25, temp_max = 75, temp_step = 10,
         solvents = c("Sulfolane", "DMSO", "NMP", "Propylene carbonate")),
    list(label = "Green blend for phenol (optimize)",
         engine = "csac", db = "ud", mode = "optimize", objective = "distribution",
         solutes = "PHENOL", feed = "WATER",
         temp_min = 25, temp_max = 45, temp_step = 10,
         solvents = c("Ethyl acetate", "2-Methyltetrahydrofuran", "Anisole"))
  )
  observeEvent(input$load_example, {
    showModal(modalDialog(
      title = i18n_r()$t("Load an example"), size = "m", easyClose = TRUE,
      tags$p(style = "color:#6c757d;", i18n_r()$t("Pick a well-known industrial liquid-liquid extraction. It sets every field and runs immediately.")),
      radioButtons(ns("example_pick"), NULL,
        choiceNames = lapply(lle_examples, function(e) i18n_r()$t(e$label)),
        choiceValues = seq_along(lle_examples), selected = 1),
      footer = tagList(modalButton(i18n_r()$t("Cancel")),
        actionButton(ns("example_go"), i18n_r()$t("Load & Run"), class = "btn btn-primary", style = "color:white;"))))
  })
  observeEvent(input$example_go, {
    removeModal()
    ex <- lle_examples[[as.integer(input$example_pick %||% 1)]]
    # Reflect the example in every control (so the UI shows what ran).
    updateSelectInput(session, "engine", selected = ex$engine)
    updateSelectInput(session, "mode", selected = ex$mode)
    updateSelectInput(session, "database", selected = ex$db)
    if (!is.null(ex$objective)) updateSelectInput(session, "objective", selected = ex$objective)
    populate_solutes_feed(ex$engine, ex$db, solutes_sel = ex$solutes, feed_sel = ex$feed)
    populate_solvents(ex$solvents)
    if (!is.null(ex$temp)) updateNumericInput(session, "temp", value = ex$temp)
    if (!is.null(ex$temp_min)) updateNumericInput(session, "temp_lo", value = ex$temp_min)
    if (!is.null(ex$temp_max)) updateNumericInput(session, "temp_hi", value = ex$temp_max)
    if (!is.null(ex$temp_step)) updateNumericInput(session, "temp_step", value = ex$temp_step)
    # Run directly from the example config (independent of the async selectize updates above).
    cfg <- list(engine = ex$engine, db = ex$db, mode = ex$mode, solutes = ex$solutes, feed = ex$feed,
      solvents = ex$solvents, temp = ex$temp, temp_min = ex$temp_min, temp_max = ex$temp_max,
      temp_step = ex$temp_step, solvent_ratio = 1, solute_frac = 0.01, pressure = 1.01325,
      dsp = TRUE, objective = ex$objective %||% "selectivity", multistart = 4, warm = TRUE,
      opt_method = "gamma_inf")
    do_run(cfg)
  })

  # ---------- reset ----------
  observeEvent(input$reset, {
    rv$res <- NULL; rv$meta <- NULL; rv$flash <- NULL; rv$flash_meta <- NULL; rv$flash_best <- NULL
    rv$opt <- NULL; rv$opt_meta <- NULL; rv$opt_pairs <- NULL; rv$opt_best_label <- NULL
    updateSelectInput(session, "engine", selected = "csac")
    updateSelectInput(session, "mode", selected = "screen")
    updateSelectInput(session, "database", selected = "ud")
    populate_solutes_feed("csac", "ud")
    populate_solvents(SOLV_DEFAULT)
    updateNumericInput(session, "temp", value = defaults$temp)
    updateNumericInput(session, "temp_lo", value = defaults$temp)
    updateNumericInput(session, "temp_hi", value = 60)
    sf_unit_rv("mass")
    updateCheckboxInput(session, "sf_optimize", value = FALSE)
    updateCheckboxInput(session, "dsp", value = TRUE)
  })
}
