library(shinyjs)

# Co-Solvent Selection > Solubility & Separator Cascade module.
# scCO₂ solubility curves + an ideal depressurisation-cascade fractionation simulator. Solubility models:
# Makarov (ML, from SMILES), Experimental (measured density-based k/A/B), and CSP-2021 (predictive EoS).
# The Chrastil GCM (structure-predicted k/A/B) is deliberately NOT offered here — it is a weak predictor
# superseded by the ML model and is kept only in Solute Characterization. Solute sources: session import
# from Solute Characterization, the experimental DB, manual entry, plus CSV import/export.

sep_cascade_server <- function(input, output, session, defaults, i18n, tablang, sfe_rv) {
  ns <- session$ns
  i18n_r <- reactive({ i18n })

  rv <- reactiveValues(
    selected_saved_calc_ids = character(0),  # indices into sfe_rv$saved_calculations
    extra_solutes = NULL,                    # data.frame: name,k,A,B,mw,basis,source
    sol_curves = NULL,                       # tidy solubility data.frame
    cascade = NULL,                          # chrastil_cascade() output
    optim = NULL,                            # list of per-solute optimized (name,T,P,rec) when optimizing
    current_blend = list()                   # co-solvent Custom Blend: solvent name -> fraction (%)
  )

  # ---- Helpers --------------------------------------------------------------
  # Pull Chrastil coeffs out of a saved calculation; NULL when the calc has none.
  calc_chrastil <- function(calc) {
    ch <- calc$parameters$chrastil
    if (is.null(ch)) return(NULL)
    k <- suppressWarnings(as.numeric(ch$k)); A <- suppressWarnings(as.numeric(ch$A)); B <- suppressWarnings(as.numeric(ch$B))
    if (!all(is.finite(c(k, A, B)))) return(NULL)
    list(
      name  = calc$parameters$name_input %||% "solute",
      k = k, A = A, B = B,
      mw    = suppressWarnings(as.numeric(ch$mw %||% NA)),
      basis = ch$basis %||% "mol/mol",
      smiles = ch$smiles %||% (calc$parameters$smiles_input %||% "")
    )
  }

  # Indices (into sfe_rv$saved_calculations) of calcs that carry Chrastil coeffs.
  chrastil_calc_indices <- reactive({
    calcs <- sfe_rv$saved_calculations
    if (length(calcs) == 0) return(integer(0))
    keep <- vapply(calcs, function(c) !is.null(calc_chrastil(c)), logical(1))
    which(keep)
  })

  # The bundled Chrastil database (loaded once).
  chr_db <- tryCatch(chrastil_db(), error = function(e) NULL)

  # ---- Solubility model + per-model solute eligibility ----------------------
  # Models: chrastil/experimental use density-based k/A/B (GCM-estimated vs measured); csp is the
  # predictive CSP-2021 EoS (in-DB compounds only); makarov is the ML model (needs SMILES). Each solute
  # is only selectable for a model whose required inputs it carries (live checkers).
  output$sol_model_ui <- renderUI({
    selectInput(ns("sol_model"),
      label = tags$span(i18n_r()$t("Solubility Model"),
        input_help(i18n_r()$t("Model used to compute each solute's scCO₂ solubility at the curve and separator conditions. Experimental uses density-based k/A/B coefficients measured for the compound; CSP-2021 and openCOSMO-RS-Phi are predictive COSMO-SAC-Phi equations of state for compounds in their databases (from quantum σ-profiles + fitted parameters); Makarov (ML) predicts from SMILES. Only solutes that carry the data a model needs are selectable for it."),
                   title = i18n_r()$t("Solubility Model"), buttonLabel = i18n_r()$t("OK"))),
      # The Chrastil GCM (structure-predicted k/A/B) is intentionally not offered here — it is a weak
      # predictor superseded by the ML model; it remains available in Solute Characterization only.
      choices = setNames(c("makarov", "experimental", "csp", "ocrsphi"),
        c(i18n_r()$t("Makarov (ML, from SMILES)"), i18n_r()$t("Experimental (measured k/A/B)"),
          i18n_r()$t("CSP-2021 (in database)"), i18n_r()$t("openCOSMO-RS-Phi (in database)"))),
      selected = if (!is.null(input$sol_model)) input$sol_model else "makarov", width = "100%")
  })
  outputOptions(output, "sol_model_ui", suspendWhenHidden = FALSE)

  # Model-routing / applicability-domain advisory for the Makarov (ML) model. Its training set is
  # drug-like / natural solutes; outside this domain it over-predicts (the triolein-at-98-g/kg case).
  # The domain below reflects that training coverage (MW g/mol, T degC, P bar).
  MAKAROV_MW <- c(100, 500); MAKAROV_T_C <- c(35, 75); MAKAROV_P_BAR <- c(80, 360)
  chrastil_names <- reactive(tryCatch(toupper(chrastil_db()$name), error = function(e) character(0)))
  output$sol_model_advice_ui <- renderUI({
    if (!identical(cur_model(), "makarov")) return(NULL)
    df <- combined_solutes(); if (is.null(df) || !nrow(df)) return(NULL)
    dom <- sprintf(i18n_r()$t("Makarov (ML) applicability domain: MW %d–%d g/mol, T %d–%d °C, P %d–%d bar. Outside it the model may over-predict."),
                   MAKAROV_MW[1], MAKAROV_MW[2], MAKAROV_T_C[1], MAKAROV_T_C[2], MAKAROV_P_BAR[1], MAKAROV_P_BAR[2])
    warn <- character(0)
    # Makarov is pure-CO2 only: flag that any selected co-solvent is ignored (use an EoS engine for it).
    if (!is.null(cosolv_sel()))
      warn <- c(warn, i18n_r()$t("Makarov (ML) is a pure-CO₂ predictor and IGNORES the selected co-solvent — its co-solvent sensitivity is too weak to be reliable. Switch to CSP-2021 or openCOSMO-RS-Phi to model co-solvent enhancement."))
    # per-solute MW out of domain (name + its MW)
    oob <- df[is.finite(df$mw) & (df$mw < MAKAROV_MW[1] | df$mw > MAKAROV_MW[2]), , drop = FALSE]
    if (nrow(oob)) warn <- c(warn, sprintf(i18n_r()$t("Molar mass outside the domain (expect over-prediction): %s."),
      paste(sprintf("%s (%.0f g/mol)", oob$name, oob$mw), collapse = ", ")))
    # condition ranges: extraction P/T and the lowest separator (recovery) pressure
    exT <- suppressWarnings(as.numeric(input$extract_temp %||% 40))[1]
    exP <- suppressWarnings(as.numeric(input$extract_pres %||% 350))[1]
    loP <- suppressWarnings(recovery_pressure_r())
    if ((isTRUE(is.finite(exP)) && exP > MAKAROV_P_BAR[2]) || (isTRUE(is.finite(loP)) && loP < MAKAROV_P_BAR[1]))
      warn <- c(warn, sprintf(i18n_r()$t("Pressures reach outside the calibration range (%d–%d bar) — separator/recovery below %d bar is extrapolation."),
        MAKAROV_P_BAR[1], MAKAROV_P_BAR[2], MAKAROV_P_BAR[1]))
    if (isTRUE(is.finite(exT)) && (exT < MAKAROV_T_C[1] || exT > MAKAROV_T_C[2]))
      warn <- c(warn, sprintf(i18n_r()$t("Temperature %g °C is outside the calibration range (%d–%d °C)."), exT, MAKAROV_T_C[1], MAKAROV_T_C[2]))
    # solutes that have measured Chrastil coefficients -> prefer Experimental
    indb <- df$name[toupper(df$name) %in% chrastil_names()]
    info <- if (length(indb)) sprintf(i18n_r()$t("These solutes have measured Chrastil coefficients — the Experimental model is likely more reliable: %s."), paste(indb, collapse = ", ")) else NULL
    if (!length(warn) && is.null(info))
      return(div(class = "alert alert-info", style = "padding:6px 12px; font-size:0.85em; margin:6px 15px 0 15px;",
        icon("circle-info"), " ", dom))
    div(class = "alert alert-warning", style = "padding:8px 12px; font-size:0.88em; margin:6px 15px 0 15px;",
      icon("triangle-exclamation"), " ",
      HTML(paste(c(warn, info, dom), collapse = "<br>")))
  })
  outputOptions(output, "sol_model_advice_ui", suspendWhenHidden = FALSE)
  cur_model <- reactive(input$sol_model %||% "makarov")

  # CSP compound name set (uppercase) and experimental Chrastil-coefficient lookup (compounds with
  # measured k/A/B). csp_names is reactive so user compounds added via csp_add_compound() (the
  # "Add to CSP" modal below) immediately make matching solutes eligible for the CSP model. Restrict to
  # compounds that actually carry CSP-2021 parameters (has_csp21): csp_solubility(model="2021") stop()s for
  # the rest, so counting/admitting all jcosmo names would overstate the usable set (matches db_engine_eligible).
  .csp_usable_names <- function() { cc <- csp_compounds(); h <- !is.na(cc$has_csp21) & cc$has_csp21
    toupper(unique(cc$jcosmo[h])) }
  csp_names_rv <- reactiveVal(tryCatch(.csp_usable_names(), error = function(e) character(0)))
  refresh_csp_names <- function()
    csp_names_rv(tryCatch(.csp_usable_names(), error = function(e) character(0)))
  # openCOSMO-RS-Phi FITTED compound set (uppercase) -> which solutes are eligible for the ocrsphi model.
  # Excludes "pending-fit" entries (ingested profile, placeholder params) via ocrsphi_fitted_meta().
  ocrsphi_names_rv <- reactiveVal(tryCatch(toupper(ocrsphi_fitted_meta()$compound), error = function(e) character(0)))
  exp_kab <- tryCatch({ ed <- exp_prop_db(); ed[is.finite(ed$Chr_k), c("name", "smiles", "key", "Chr_k", "Chr_A", "Chr_B")] },
                      error = function(e) NULL)
  exp_name_set <- if (!is.null(exp_kab)) toupper(exp_kab$name) else character(0)

  # ---- CSP model options (fusion-data fallback) --------------------------------------------------
  # Adding user compounds to CSP from the UI has been removed (the QM/COSMO sigma-profile workflow is
  # too involved for end users). The CSP database is curated via periodic package updates;
  # csp_add_compound() remains available programmatically for that.
  # Shown under the model selector when CSP is chosen: DB size + the fusion-data fallback option.
  output$csp_manage_ui <- renderUI({
    if (!identical(cur_model(), "csp")) return(NULL)
    n <- length(csp_names_rv())
    div(style = "margin:6px 15px 0 15px;",
      div(class = "alert alert-info", style = "padding:6px 12px; font-size:0.85em; margin-bottom:6px;",
        icon("circle-info"), " ",
        sprintf(i18n_r()$t("CSP-2021 predicts solubility for %d compounds in its quantum-chemical database."), n)),
      # Opt-in: estimate missing fusion data (Tm/ΔHfus/vs) from structure so the CSP solid-solubility
      # route can run for compounds without experimental fusion data. Off by default.
      div(style = "margin-top:8px;",
        checkboxInput(ns("csp_fusion_fallback"),
          tagList(i18n_r()$t("Estimate missing fusion data from structure"),
            input_help(i18n_r()$t("When a solute has no experimental fusion data (melting point, enthalpy of fusion, solid molar volume), estimate it from the SMILES — melting point via the ML predictor, ΔHfus via the Chickos-Acree group-contribution method, and a rough solid molar volume — so the CSP solid-solubility route can still run. These are estimates (~20% typical error on ΔHfus); leave off to require experimental fusion data."),
              title = i18n_r()$t("Fusion-data fallback"))),
          value = FALSE, width = "100%")))
  })
  outputOptions(output, "csp_manage_ui", suspendWhenHidden = FALSE)

  # Shown under the model selector when openCOSMO-RS-Phi is chosen: its bundled database size (parallel to
  # the CSP-2021 note above). Both EoS engines resolve a solute only if it is present in their own database.
  output$ocrsphi_manage_ui <- renderUI({
    if (!identical(cur_model(), "ocrsphi")) return(NULL)
    n <- length(ocrsphi_names_rv())
    div(style = "margin:6px 15px 0 15px;",
      div(class = "alert alert-info", style = "padding:6px 12px; font-size:0.85em; margin-bottom:6px;",
        icon("circle-info"), " ",
        sprintf(i18n_r()$t("openCOSMO-RS-Phi predicts solubility for %d compounds in its bundled database."), n)))
  })
  outputOptions(output, "ocrsphi_manage_ui", suspendWhenHidden = FALSE)

  # Inform the user, when they enable the fusion-data fallback, that it mainly changes results for
  # high-melting solids (the fusion term scales with 1/T - 1/Tm, so it is negligible near the melting point).
  observeEvent(input$csp_fusion_fallback, {
    if (isTRUE(input$csp_fusion_fallback))
      showNotification(i18n$t("Fusion-data fallback on: ΔHfus is estimated from structure for solutes without experimental data. Its effect on solubility scales with 1/T − 1/Tm, so it mainly matters for HIGH-melting solids separated well below their melting point — a solute melting near the separator temperature is barely affected."),
        type = "message", duration = 12)
  }, ignoreInit = TRUE)

  # Generalized solute info from a saved calc (works whether or not it carries Chrastil coeffs).
  calc_info <- function(calc) {
    p <- calc$parameters; ch <- p$chrastil
    k <- A <- B <- NA_real_; mw <- NA_real_; basis <- "mol/mol"
    if (!is.null(ch)) {
      k <- suppressWarnings(as.numeric(ch$k)); A <- suppressWarnings(as.numeric(ch$A)); B <- suppressWarnings(as.numeric(ch$B))
      mw <- suppressWarnings(as.numeric(ch$mw %||% NA)); basis <- ch$basis %||% "mol/mol"
    }
    list(name = p$name_input %||% "solute", cas = p$cas_input %||% "",
         smiles = (if (!is.null(ch)) ch$smiles else NULL) %||% (p$smiles_input %||% ""),
         k = k, A = A, B = B, mw = mw, basis = basis)
  }

  # Is a solute (by its carried data) usable with the given model? For openCOSMO-RS-Phi, being in the fitted
  # bundle is necessary but not sufficient — a solid with no fusion data can't actually be computed at the
  # operating temperature (returns NA), so when `T_C` is known we additionally require computability
  # (ocrsphi_compute_status). `near_melting` still computes (just flagged later), so it stays eligible.
  solute_eligible <- function(name, smiles, k, A, B, model, T_C = NA_real_) {
    switch(model,
      chrastil     = all(is.finite(c(k, A, B))),
      experimental = isTRUE(toupper(name) %in% exp_name_set),
      csp          = isTRUE(toupper(name) %in% csp_names_rv()),
      ocrsphi      = if (!isTRUE(toupper(name) %in% ocrsphi_names_rv())) FALSE
                     else if (!isTRUE(is.finite(T_C))) TRUE
                     else ocrsphi_compute_status(name, smiles, T_C)$code != "solid_no_fusion",
      makarov      = is.character(smiles) && length(smiles) == 1 && nzchar(smiles),
      FALSE)
  }

  # Indices of saved calcs eligible for the current model (replaces the Chrastil-only filter).
  eligible_calc_indices <- reactive({
    calcs <- sfe_rv$saved_calculations; if (length(calcs) == 0) return(integer(0))
    m <- cur_model()
    # ocrsphi computability is temperature-dependent (a solid with no fusion data can't be computed below its
    # melting point). Judge against the HOTTEST configured temperature (curve temps + extraction T) so a
    # solute computable at ANY requested temperature stays offered; the offered list re-filters when either
    # changes. (Per-condition NA is still handled where it occurs, with a specific reason.)
    Ts <- c(parse_num(input$curve_temps), suppressWarnings(as.numeric(input$extract_temp %||% 40))[1])
    T_C <- suppressWarnings(max(Ts[is.finite(Ts)])); if (!is.finite(T_C)) T_C <- 40
    keep <- vapply(calcs, function(c) { inf <- calc_info(c); solute_eligible(inf$name, inf$smiles, inf$k, inf$A, inf$B, m, T_C) }, logical(1))
    which(keep)
  })

  # MW from SMILES (for solutes whose mw is missing but that the model can still place).
  mw_of_smiles <- function(smi) {
    if (!is.character(smi) || !nzchar(smi)) return(NA_real_)
    m <- tryCatch(suppressWarnings(rcdk::parse.smiles(smi)[[1]]), error = function(e) NULL)
    if (is.null(m)) return(NA_real_)
    tryCatch(rcdk::get.natural.mass(m), error = function(e) NA_real_)
  }

  # ---- Co-solvent (entrainer) --------------------------------------------------
  # Restricted to solvents we hold COMPREHENSIVE data for — the HSP set (solv_db: molar volume + Hansen
  # parameters + SMILES) INTERSECTED with the CSP set. Non-co-solvents (CO2, hydrogen, water) and the
  # HSP solvents CSP lacks (heptane, hexane, methyl oleate) are dropped. CSP takes the solvent NAME
  # (resolved by .csp_match); Makarov takes its SMILES. Only these two models support a co-solvent.
  COSOLV_DB <- local({
    s <- tryCatch(show_solv(), error = function(e) NULL)
    if (is.null(s)) data.frame(Solvent = character(0), SMILES = character(0), MV = numeric(0))
    else s[!s$Solvent %in% c("CarbonDioxide", "Hydrogen", "Water", "Heptane", "Hexane", "MethylOleate"),
           c("Solvent", "SMILES", "MV"), drop = FALSE]
  })
  COSOLV_LABEL <- c(DiethylEther = "Diethyl ether", PXylene = "p-Xylene", OXylene = "o-Xylene")
  cosolv_pretty <- function(nm) ifelse(nm %in% names(COSOLV_LABEL), COSOLV_LABEL[nm], nm)
  # Current co-solvent selection -> list(name, smiles, x) or NULL when none/zero. name/smiles/x are VECTORS
  # for a blend: 2+ co-solvents split WITHIN the fixed total mole fraction by their relative parts (default
  # equal). The COSMO-SAC-Phi EoS engines (CSP / openCOSMO-RS-Phi) evaluate the blend n-component; Makarov
  # (ML) has no blend rule and approximates it by the first (primary) component (see model_solub).
  # Co-solvent composition-unit toggle (mol % <-> mass %) and models that CANNOT represent a co-solvent.
  MW_CO2 <- 44.01
  cosolvent_unit <- reactiveVal("mol")
  cosolvent_blocked <- function() (cur_model() %in% c("makarov", "experimental"))
  .cosolv_mw <- function(sel) vapply(sel, function(s) {
    smi <- COSOLV_DB$SMILES[match(s, COSOLV_DB$Solvent)]
    m <- if (length(smi) && !is.na(smi) && nzchar(smi)) suppressWarnings(mw_of_smiles(smi)) else NA_real_
    if (isTRUE(is.finite(m))) m else NA_real_ }, numeric(1))
  cosolv_sel <- function() {
    if (!isTRUE(input$use_cosolvent) || cosolvent_blocked()) return(NULL)   # gated by the checkbox / model
    raw <- suppressWarnings(as.numeric(input$cosolvent_frac %||% 0))[1]
    if (!isTRUE(is.finite(raw)) || raw <= 0) return(NULL)
    # Pure Modifier vs Custom Blend, from the ported builder.
    if (identical(input$modifier_type %||% "pure", "blend")) {
      sel <- names(rv$current_blend); if (!length(sel)) return(NULL)
      props <- vapply(sel, function(s) { v <- input[[paste0("frac_", s)]] %||% rv$current_blend[[s]]
        if (is.null(v) || is.na(v) || v < 0) 0 else as.numeric(v) }, numeric(1))
      if (sum(props) <= 0) props <- rep(1, length(sel))
    } else {
      sel <- input$modifier_selection; sel <- sel[!is.na(sel) & nzchar(sel)]
      if (!length(sel)) return(NULL); props <- 1
    }
    smi <- COSOLV_DB$SMILES[match(sel, COSOLV_DB$Solvent)]
    # Total co-solvent MOLE fraction cx. When the toggle is mass %, convert with the (mole-weighted)
    # effective co-solvent molar mass: cx = (w/Mw)/((w/Mw)+(1-w)/Mw_CO2). mol % is used verbatim.
    cx <- if (identical(cosolvent_unit(), "mass")) {
      w <- raw / 100; mws <- .cosolv_mw(sel); pv <- props / sum(props); ok <- is.finite(mws)
      mw_eff <- if (any(ok)) sum(pv[ok] * mws[ok]) / sum(pv[ok]) else NA_real_
      if (!isTRUE(is.finite(mw_eff)) || w <= 0 || w >= 1) NA_real_
      else (w / mw_eff) / ((w / mw_eff) + (1 - w) / MW_CO2)
    } else raw / 100
    if (!isTRUE(is.finite(cx)) || cx <= 0) return(NULL)
    if (length(sel) == 1) return(list(name = sel, smiles = if (length(smi)) smi else NA_character_, x = cx))
    list(name = sel, smiles = smi, x = cx * props / sum(props))     # each component's mole fraction; sum = cx
  }

  # Per-solute mol/mol solubility over a (temp_C, pres_bar) grid via the chosen model. Returns a numeric
  # vector aligned to the recycled (temp, pres) pairs, NA where the model cannot place the solute. A
  # selected co-solvent modifies the Makarov (ML) and CSP models; Chrastil/experimental ignore it.
  model_solub <- function(row, temp_C, pres_bar, model, eos) {
    n <- max(length(temp_C), length(pres_bar)); temp_C <- rep_len(temp_C, n); pres_bar <- rep_len(pres_bar, n)
    cs <- cosolv_sel()
    if (model == "chrastil" || model == "experimental") {
      kab <- c(row$k, row$A, row$B)
      if (model == "experimental") {
        i <- match(toupper(row$name), exp_name_set)
        if (!is.na(i)) kab <- c(exp_kab$Chr_k[i], exp_kab$Chr_A[i], exp_kab$Chr_B[i])
      }
      if (!all(is.finite(kab))) return(rep(NA_real_, n))
      cv <- tryCatch(chrastil_solubility(kab[1], kab[2], kab[3], temp_C = temp_C, pres_bar = pres_bar,
              eos = eos, basis = "mol/mol", mw = row$mw), error = function(e) NULL)
      if (is.null(cv)) return(rep(NA_real_, n))
      # chrastil_solubility expands to the full temp x pres grid; map back to our recycled pairs
      vapply(seq_len(n), function(j) { hit <- cv$y_molmol[cv$temp_C == temp_C[j] & cv$pres_bar == pres_bar[j]]; if (length(hit)) hit[1] else NA_real_ }, numeric(1))
    } else if (model == "csp") {
      tryCatch(as.numeric(csp_solubility(row$name, T = temp_C + 273.15, P = pres_bar, model = "2021",
        cosolvent = if (!is.null(cs)) cs$name else NULL, x = if (!is.null(cs)) cs$x else 0,
        fusion_fallback = isTRUE(input$csp_fusion_fallback), smiles = row$smiles %||% NULL)), error = function(e) rep(NA_real_, n))
    } else if (model == "makarov") {
      if (!nzchar(row$smiles %||% "")) return(rep(NA_real_, n))
      # Makarov (ML) is a PURE-CO2 solubility predictor here: its co-solvent sensitivity is too weak to be
      # reliable (co-solvent features carry <2% of model gain, and some solvents silently zero out), so a
      # selected co-solvent is intentionally IGNORED. Use CSP-2021 or openCOSMO-RS-Phi for co-solvent effects.
      tryCatch(predict_solubility(row$smiles, temp = temp_C + 273.15, pres = pres_bar,
        cosolvent = NULL, x = 0)$mole_frac, error = function(e) rep(NA_real_, n))
    } else if (model == "ocrsphi") {
      # openCOSMO-RS-Phi: predictive EoS solubility for compounds in its bundle (by name). Supports a
      # co-solvent via the n-component route (solute + CO2 + co-solvent(s)), when the co-solvent is
      # also in the bundle; otherwise the tryCatch falls back to NA for that solute.
      tryCatch(as.numeric(ocrsphi_solubility(row$name, T = temp_C + 273.15, P = pres_bar,
        cosolvent = if (!is.null(cs)) cs$name else NULL, x = if (!is.null(cs)) cs$x else 0)),
        error = function(e) rep(NA_real_, n))
    } else rep(NA_real_, n)
  }

  # ---- Solute Selection (session import, mirrors Miscibility Comparison) -----
  observeEvent(input$load_example_data, {
    seed_data <- sfe_rv$seed_calculations
    if (length(seed_data) == 0) { showNotification(i18n$t("No example data available."), type = "warning"); return() }
    existing_names <- sapply(sfe_rv$saved_calculations, function(x) x$display_name)
    seed_names <- sapply(seed_data, function(x) x$display_name)
    new_seeds <- seed_data[!seed_names %in% existing_names]
    if (length(new_seeds) == 0) { showNotification(i18n$t("Example data already loaded."), type = "message"); return() }
    current_len <- length(sfe_rv$saved_calculations)
    for (i in seq_along(new_seeds)) { new_seeds[[i]]$id <- current_len + i; sfe_rv$saved_calculations[[current_len + i]] <- new_seeds[[i]] }
    showNotification(sprintf(i18n$t("Loaded %d example calculation(s)."), length(new_seeds)), type = "message")
  })

  observeEvent(input$load_from_disk, {
    showModal(modalDialog(
      title = i18n$t("Load Calculation from Disk"), size = "m",
      fileInput(ns("load_file_input"), i18n$t("Select JSON file(s)"), multiple = TRUE,
        accept = c(".json", "application/json"), buttonLabel = i18n$t("Browse..."),
        placeholder = i18n$t("No file selected")),
      tags$small(class = "text-muted", i18n$t("Select one or more .json files exported from Solute Characterization.")),
      footer = tagList(modalButton(i18n$t("Cancel")),
        actionButton(ns("confirm_load_from_disk"), i18n$t("Load"), class = "btn btn-primary", style = "color: white;"))
    ))
  })

  observeEvent(input$confirm_load_from_disk, {
    req(input$load_file_input); removeModal()
    files <- input$load_file_input; loaded_count <- 0
    for (i in seq_len(nrow(files))) {
      tryCatch({
        json_data <- jsonlite::fromJSON(files$datapath[i], simplifyVector = FALSE)
        if (is.null(json_data$parameters) || is.null(json_data$display_name)) {
          showNotification(sprintf(i18n$t("Invalid file format: %s"), files$name[i]), type = "warning"); return()
        }
        existing_names <- sapply(sfe_rv$saved_calculations, function(x) x$display_name)
        if (json_data$display_name %in% existing_names) {
          showNotification(sprintf(i18n$t("Calculation '%s' already loaded."), json_data$display_name), type = "warning"); return()
        }
        new_id <- length(sfe_rv$saved_calculations) + 1
        sfe_rv$saved_calculations[[new_id]] <- list(
          id = new_id, display_name = json_data$display_name,
          timestamp = json_data$timestamp %||% format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          source_module = json_data$source_module %||% "Loaded from Disk", parameters = json_data$parameters)
        loaded_count <- loaded_count + 1
      }, error = function(e) showNotification(sprintf(i18n$t("Error loading %s: %s"), files$name[i], e$message), type = "error"))
    }
    if (loaded_count > 0) showNotification(sprintf(i18n$t("Loaded %d calculation(s) from disk."), loaded_count), type = "message")
  })

  output$saved_calculations_list_ui <- renderUI({
    idx <- eligible_calc_indices()
    calcs <- sfe_rv$saved_calculations
    if (length(idx) == 0) {
      need <- switch(cur_model(),
        chrastil = i18n$t("Chrastil coefficients"), experimental = i18n$t("a name with measured k/A/B in the database"),
        csp = i18n$t("a name in the CSP-2021 database"), makarov = i18n$t("a SMILES string"),
        ocrsphi = i18n$t("a name in the openCOSMO-RS-Phi database"), "")
      return(tagList(
        p(sprintf(i18n$t("No saved calculations eligible for the selected model (need %s)."), need)),
        p(style = "font-size: 0.9em; color: #888; margin-top: 10px;",
          i18n$t("Characterize a solute in Solute Characterization (with the data the chosen model needs), then it will appear here."))
      ))
    }
    choices <- setNames(idx, sapply(idx, function(i) calcs[[i]]$display_name))
    has_selection <- length(rv$selected_saved_calc_ids) > 0
    delete_btn <- actionButton(ns("delete_selected_calcs"), label = i18n$t("Remove Selected from Session"),
      icon = icon("trash-alt"), class = "btn-xs btn-danger", style = "color: white;")
    save_btn <- actionButton(ns("save_selected_calcs"), label = i18n$t("Save Selected to Disk"),
      icon = icon("save"), class = "btn-xs btn-primary", style = "color: white;")
    if (!has_selection) { delete_btn <- shinyjs::disabled(delete_btn); save_btn <- shinyjs::disabled(save_btn) }
    tagList(
      div(style = if (length(idx) > 6) "margin-bottom: 10px; max-height: 300px; overflow-y: auto;" else "margin-bottom: 10px;",
        checkboxGroupInput(ns("saved_calculation_checkboxes"), label = i18n$t("Select saved calculations"),
          choices = choices, selected = rv$selected_saved_calc_ids)),
      div(style = "margin-top: -5px; display:flex; gap:6px; flex-wrap:wrap;", delete_btn, save_btn),
      p(style = "font-size: 0.9em; color: #888; margin-top: 10px;",
        sprintf(i18n$t("Showing %d calculation(s) eligible for the selected solubility model."), length(idx)))
    )
  })

  # Compound name of a saved-calculation id (for the no-duplicate-name rule).
  calc_name_of <- function(id) { i <- suppressWarnings(as.numeric(id))
    if (length(i) == 1 && is.finite(i) && i <= length(sfe_rv$saved_calculations))
      calc_info(sfe_rv$saved_calculations[[i]])$name else NA_character_ }

  # A solute name may appear only once in the whole module: dedupe the session selection by name, and drop
  # any that clash with a database-added solute (notifying which were skipped).
  observeEvent(input$saved_calculation_checkboxes, {
    ids <- input$saved_calculation_checkboxes
    if (length(ids) > 1) {
      nms <- vapply(ids, calc_name_of, character(1)); key <- tolower(trimws(nms))
      dup <- duplicated(key) & !is.na(nms)
      if (any(dup)) {
        showNotification(sprintf(i18n$t("Only one solute per name may be selected; skipped: %s"),
          paste(unique(nms[dup]), collapse = ", ")), type = "warning")
        ids <- ids[!dup]
      }
    }
    if (length(ids) && !is.null(rv$extra_solutes) && nrow(rv$extra_solutes)) {
      nms <- vapply(ids, calc_name_of, character(1))
      clash <- tolower(trimws(nms)) %in% tolower(trimws(rv$extra_solutes$name))
      if (any(clash)) {
        showNotification(sprintf(i18n$t("Already added from the database; removed from session selection: %s"),
          paste(unique(nms[clash]), collapse = ", ")), type = "warning")
        ids <- ids[!clash]
      }
    }
    if (!identical(ids, input$saved_calculation_checkboxes))
      updateCheckboxGroupInput(session, "saved_calculation_checkboxes", selected = ids)
    rv$selected_saved_calc_ids <- ids
  }, ignoreNULL = FALSE)

  observeEvent(input$delete_selected_calcs, {
    req(length(input$saved_calculation_checkboxes) > 0)
    sel <- as.numeric(input$saved_calculation_checkboxes)
    nm <- na.omit(sapply(sel, function(i) if (i <= length(sfe_rv$saved_calculations)) sfe_rv$saved_calculations[[i]]$display_name else NA))
    showModal(modalDialog(title = i18n$t("Confirm Deletion"),
      p(i18n$t("Are you sure you want to delete the selected calculation(s)?")),
      tags$ul(lapply(nm, function(n) tags$li(tags$strong(n)))),
      footer = tagList(modalButton(i18n$t("Cancel")),
        actionButton(ns("confirm_delete_calcs"), i18n$t("Delete"), class = "btn-danger", style = "color: white;"))))
  })

  observeEvent(input$confirm_delete_calcs, {
    removeModal(); req(input$saved_calculation_checkboxes)
    sel <- sort(as.numeric(input$saved_calculation_checkboxes), decreasing = TRUE)
    for (i in sel) if (i <= length(sfe_rv$saved_calculations)) sfe_rv$saved_calculations[[i]] <- NULL
    sfe_rv$saved_calculations <- Filter(Negate(is.null), sfe_rv$saved_calculations)
    rv$selected_saved_calc_ids <- character(0)
    showNotification(i18n$t("Selected calculation(s) removed."), type = "warning", duration = 3)
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

  # Unified "solutes to process" list: solutes selected from saved calculations PLUS solutes added
  # from the database. Database-added solutes carry an inline remove button.
  output$selected_solutes_ui <- renderUI({
    m <- cur_model(); ex <- rv$extra_solutes
    n_sess <- length(rv$selected_saved_calc_ids)
    n_ex <- if (is.null(ex)) 0L else nrow(ex)
    if (n_sess == 0 && n_ex == 0) return(p(i18n$t("No solutes selected.")))
    sess_items <- lapply(rv$selected_saved_calc_ids, function(id) {
      i <- as.numeric(id); if (i > length(sfe_rv$saved_calculations)) return(NULL)
      inf <- calc_info(sfe_rv$saved_calculations[[i]])
      # SMILES is never shown to the user; Makarov solutes therefore show no extra detail here.
      detail <- switch(m,
        chrastil = if (all(is.finite(c(inf$k, inf$A, inf$B)))) sprintf(" — k=%.2f, A=%.0f, B=%.2f", inf$k, inf$A, inf$B) else "",
        "")
      # Uniform format across the app: bold name + CAS in parentheses (then any model detail).
      fluidRow(column(12, p(style = "margin-bottom:2px;", tagList(strong(inf$name), solute_cas_suffix(inf$cas), detail,
        if (is.finite(inf$mw)) sprintf(" — MW %.1f", inf$mw) else ""))))
    })
    db_items <- if (n_ex == 0) NULL else lapply(seq_len(n_ex), function(i) {
      fluidRow(style = "margin-bottom:1px;",
        column(9, span(strong(ex$name[i]), solute_cas_suffix(ex$cas[i]),
          tags$span(style = "color:#888;", sprintf(" [%s]", ex$source[i])))),
        column(3, actionButton(ns(paste0("rm_", i)), NULL, icon = icon("times"),
          class = "btn-xs btn-danger", style = "color:white;padding:0 6px;",
          onclick = sprintf("Shiny.setInputValue('%s', %d, {priority:'event'})", ns("remove_extra"), i))))
    })
    tagList(sess_items, db_items)
  })

  # ---- Database source: the SHARED experimental-DB browser (exp_db_modal), identical to Solute
  #      Characterization and the Co-Solvent modules. The table is filtered to compounds usable by the
  #      SELECTED solubility model; "Load Selected" adds them to the solute list via add_rows() below.
  db_refresh_casc <- reactiveVal(0)
  db_filtered_casc <- reactive({ db_refresh_casc()
    d <- exp_db_all(); if (is.null(d) || !nrow(d)) return(NULL)
    d <- d[exp_db_method_keep(d, cur_model()), , drop = FALSE]
    exp_db_filtered(d, input$db_category, isTRUE(input$db_sfe_only)) })
  observeEvent(input$open_db_search, showModal(exp_db_modal(ns, i18n)))
  output$db_table <- DT::renderDataTable({ exp_db_dt(i18n_r, db_filtered_casc()) }, server = TRUE)
  exp_db_wire_download(output, session, db_filtered_casc)
  exp_db_wire_category(input, session)
  observeEvent(input$db_add_save, {
    if (isTRUE(exp_db_add_save(input, session, i18n, close = FALSE))) db_refresh_casc(db_refresh_casc() + 1) })
  exp_db_wire_edit_delete(input, output, session, i18n,
    refresh = function() db_refresh_casc(db_refresh_casc() + 1), db_fn = db_filtered_casc)

  # Normalise to the working-set schema (name,k,A,B,mw,basis,smiles,source) so all sources rbind cleanly.
  # A name already present anywhere (session selection or previously added) is skipped (with a notice) so
  # no solute name can appear twice in the module.
  add_rows <- function(df) {
    if (is.null(df$smiles)) df$smiles <- ""
    if (is.null(df$cas)) df$cas <- ""
    df <- df[, c("name", "k", "A", "B", "mw", "basis", "smiles", "cas", "source"), drop = FALSE]
    df <- df[!is.na(df$name) & nzchar(df$name), , drop = FALSE]
    if (!nrow(df)) return(invisible())
    cur <- rv$extra_solutes
    sess_names <- vapply(rv$selected_saved_calc_ids, calc_name_of, character(1))
    taken <- tolower(trimws(c(sess_names, cur$name)))
    keep <- !tolower(trimws(df$name)) %in% taken
    if (any(!keep))
      showNotification(sprintf(i18n$t("Already selected; skipped: %s"),
        paste(unique(df$name[!keep]), collapse = ", ")), type = "warning")
    df <- df[keep, , drop = FALSE]
    if (!nrow(df)) return(invisible())
    rv$extra_solutes <- if (is.null(cur)) df else rbind(cur, df)
  }

  # "Load Selected" (shared modal footer) -> add the picked compound(s) to the solute list. Chrastil
  # coefficients are carried when present (used by the Chrastil/Experimental models) and the SMILES is
  # carried for Makarov / CSP / openCOSMO-RS-Phi. db_filtered_casc has already restricted the table to
  # compounds usable by the selected model, so whatever is selected is loadable.
  observeEvent(input$db_load_selected, {
    sel <- input$db_table_rows_selected
    if (is.null(sel) || !length(sel)) { showNotification(i18n$t("Select a compound first."), type = "warning"); return() }
    db <- db_filtered_casc(); req(db); rows <- db[sel, , drop = FALSE]
    add_rows(data.frame(name = rows$name, k = rows$Chr_k, A = rows$Chr_A, B = rows$Chr_B,
      mw = NA_real_, basis = "mol/mol", smiles = rows$smiles %||% "",
      cas = if (is.null(rows$cas)) "" else sub("\\|.*$", "", rows$cas),
      source = "database", stringsAsFactors = FALSE))
    removeModal()
    showNotification(sprintf(i18n$t("Added %d solute(s) from the database."), length(sel)), type = "message")
  })

  # Remove a single (database-added) solute from the list (dynamic buttons)
  observeEvent(input$remove_extra, {
    i <- as.integer(input$remove_extra)
    if (!is.null(rv$extra_solutes) && i >= 1 && i <= nrow(rv$extra_solutes))
      rv$extra_solutes <- rv$extra_solutes[-i, , drop = FALSE]
  })

  # ---- Combined working set -------------------------------------------------
  combined_solutes <- reactive({
    rows <- list()
    for (id in rv$selected_saved_calc_ids) {
      i <- as.numeric(id); if (i > length(sfe_rv$saved_calculations)) next
      inf <- calc_info(sfe_rv$saved_calculations[[i]])
      rows[[length(rows) + 1]] <- data.frame(name = inf$name, k = inf$k, A = inf$A, B = inf$B,
        mw = inf$mw, basis = inf$basis, smiles = inf$smiles, cas = inf$cas, source = "session", stringsAsFactors = FALSE)
    }
    sess <- if (length(rows)) do.call(rbind, rows) else NULL
    df <- rbind(sess, rv$extra_solutes)
    if (is.null(df) || !nrow(df)) return(NULL)
    df <- df[!duplicated(df$name), , drop = FALSE]
    # Fill missing molar mass from SMILES where possible (so ML/experimental solutes can enter the cascade).
    miss <- !is.finite(df$mw) & nzchar(df$smiles %||% "")
    if (any(miss)) df$mw[miss] <- vapply(df$smiles[miss], mw_of_smiles, numeric(1))
    df
  })

  # ---- Conditions renderUI (multi-value selectize, like misc_comp) ----------
  cur_vals <- function(input_id, default_csv) {
    if (!is.null(input[[input_id]])) input[[input_id]] else {
      v <- as.numeric(strsplit(default_csv %||% "", ",")[[1]]); as.character(v[!is.na(v)]) }
  }
  # Solubility-curve temperatures / pressures — multi-value inputs with a units-bearing range badge (badge
  # and any out-of-range tag turn red), plus a help icon, matching the other tag inputs in the app.
  output$curve_temps_ui <- renderUI(range_badge_multi_input(ns, i18n_r, "curve_temps",
    i18n_r()$t("Temperatures (°C)"), cur_vals("curve_temps", defaults$curve_temps %||% "40,60"),
    min_val = 31, max_val = 200, range_text = paste0("31–200 ", "°C"),
    placeholder = i18n_r()$t("Type temperatures..."),
    help_content = i18n_r()$t("Temperatures at which the solubility curves are computed — one curve per value. Supercritical CO₂ requires temperatures above its critical temperature (~31 °C)."),
    help_title = i18n_r()$t("Temperatures (°C)")))
  outputOptions(output, "curve_temps_ui", suspendWhenHidden = FALSE)
  output$curve_pres_ui <- renderUI(range_badge_multi_input(ns, i18n_r, "curve_pres",
    i18n_r()$t("Pressures (bar)"), cur_vals("curve_pres", defaults$curve_pres %||% "100,150,200,250,300,350,400"),
    min_val = 40, max_val = 1000, range_text = paste0("40–1000 ", i18n_r()$t("bar")),
    placeholder = i18n_r()$t("Type pressures..."),
    help_content = i18n_r()$t("Pressures at which the solubility curves are computed — one point per value."),
    help_title = i18n_r()$t("Pressures (bar)")))
  outputOptions(output, "curve_pres_ui", suspendWhenHidden = FALSE)

  # Co-solvent selection: Modifier Type (Pure / Custom Blend) + builder, ported verbatim from the Co-Solvent
  # Comparison module so both modules share the exact same UI/behaviour. rv$current_blend = {solvent -> %}.
  cosolv_choices <- reactive(stats::setNames(COSOLV_DB$Solvent, cosolv_pretty(COSOLV_DB$Solvent)))
  output$modifier_type_ui <- renderUI(radioButtons(ns("modifier_type"),
    label = tags$span(i18n_r()$t("Modifier Type"), " ",
      input_help(i18n_r()$t("Choose a single pure co-solvent or a custom blend of multiple solvents at specified proportions. Blends are evaluated n-component by CSP-2021 / openCOSMO-RS-Phi."),
        title = i18n_r()$t("Modifier Type"), buttonLabel = i18n_r()$t("OK"))),
    choices = setNames(c("pure", "blend"), c(i18n_r()$t("Pure Modifier"), i18n_r()$t("Custom Blend"))),
    selected = input$modifier_type %||% "pure", inline = TRUE))
  outputOptions(output, "modifier_type_ui", suspendWhenHidden = FALSE)
  output$modifier_selection_ui <- renderUI({ ch <- cosolv_choices()
    selectInput(ns("modifier_selection"), i18n_r()$t("Select Modifier"), choices = ch,
      selected = input$modifier_selection %||% unname(ch)[1], width = "100%") })
  outputOptions(output, "modifier_selection_ui", suspendWhenHidden = FALSE)
  output$blend_solvent_selector_ui <- renderUI(selectInput(ns("blend_solvent_selector"),
    i18n_r()$t("Solvent"), choices = cosolv_choices(), width = "100%"))
  outputOptions(output, "blend_solvent_selector_ui", suspendWhenHidden = FALSE)
  output$custom_blend_box_ui <- renderUI(
    div(style = "border:1px solid #ddd; border-radius:6px; padding:10px; margin-top:8px;",
      h5(i18n_r()$t("Build a Blend"), style = "margin-top:0;"),
      uiOutput(ns("blend_solvent_selector_ui")),
      div(style = "display:flex; gap:6px; margin-top:4px;",
        div(style = "flex:1;", actionButton(ns("add_to_blend"), i18n_r()$t("Add to Blend"), class = "btn btn-primary btn-block", style = "color:white;")),
        div(style = "flex:1;", shinyjs::disabled(actionButton(ns("equalize_blend"), i18n_r()$t("Equalize"), class = "btn btn-success btn-block", style = "color:white;")))),
      hr(),
      h5(i18n_r()$t("Current Blend Composition:")),
      uiOutput(ns("current_blend_ui")),
      div(style = "margin-top:16px;", actionButton(ns("clear_blend"), i18n_r()$t("Clear Blend"), class = "btn btn-primary btn-block", style = "color:white;"))))
  outputOptions(output, "custom_blend_box_ui", suspendWhenHidden = FALSE)
  # --- Blend management (ported) ---
  .blend_frac_obs <- list(); .blend_rm_obs <- list()
  observeEvent(input$add_to_blend, { req(input$blend_solvent_selector); s <- input$blend_solvent_selector
    if (s %in% names(rv$current_blend)) { showNotification(i18n$t("Solvent already in blend!"), type = "warning"); return() }
    rv$current_blend[[s]] <- 0 })
  observe(shinyjs::toggleState("equalize_blend", condition = length(rv$current_blend) >= 2))
  observeEvent(input$equalize_blend, { ss <- names(rv$current_blend); req(length(ss) >= 2); eq <- round(100 / length(ss), 1)
    for (s in ss) { rv$current_blend[[s]] <- eq; updateNumericInput(session, paste0("frac_", s), value = eq) } })
  output$current_blend_ui <- renderUI({
    if (!length(rv$current_blend)) return(p(i18n_r()$t("No solvents added yet.")))
    inputs <- lapply(names(rv$current_blend), function(s)
      div(style = "display:flex; align-items:center; gap:8px; margin-bottom:5px;",
        tags$span(style = "font-weight:bold; min-width:100px;", cosolv_pretty(s)),
        div(style = "flex:1;", numericInput(ns(paste0("frac_", s)), NULL, value = rv$current_blend[[s]],
          min = 1, max = 100, step = 0.1, width = "100%") |> tagAppendAttributes(style = "margin-bottom:0;")),
        actionButton(ns(paste0("remove_", s)), NULL, icon = icon("times"), class = "btn btn-danger btn-sm", style = "flex-shrink:0; color:white;")))
    tagList(inputs, hr(), uiOutput(ns("blend_total_display")))
  })
  outputOptions(output, "current_blend_ui", suspendWhenHidden = FALSE)
  # Total badge + a "Units:" mol%/mass% toggle (separated by a vertical divider), synced with the total field.
  output$blend_total_display <- renderUI({ ss <- names(rv$current_blend); if (!length(ss)) return(NULL)
    total <- sum(vapply(ss, function(s) { v <- input[[paste0("frac_", s)]]; if (is.null(v) || is.na(v)) 0 else as.numeric(v) }, numeric(1)), na.rm = TRUE)
    bg <- if (isTRUE(all.equal(total, 100))) "#28a745" else "#dc3545"
    ulab <- if (identical(cosolvent_unit(), "mass")) i18n_r()$t("mass %") else i18n_r()$t("mol %")
    div(style = "display:flex; align-items:center; gap:10px; flex-wrap:wrap;",
      tags$span(strong(i18n_r()$t("Total:")), " ", span(paste0(round(total, 1), "%"),
        style = paste0("background-color:", bg, "; color:white; font-weight:bold; padding:2px 8px; border-radius:4px;"))),
      tags$span(style = "border-left:1px solid #ccc; align-self:stretch;"),
      tags$span(style = "display:flex; align-items:center; gap:6px;", strong(i18n_r()$t("Units:")),
        actionButton(ns("cosolvent_unit_toggle2"), ulab, title = i18n_r()$t("Toggle mol % / mass %"),
          class = "btn btn-xs", style = "background:#3c8dbc;color:#fff;border:0;border-radius:11px;padding:2px 10px;font-size:11px;font-weight:700;"))) })
  outputOptions(output, "blend_total_display", suspendWhenHidden = FALSE)
  observeEvent(input$cosolvent_unit_toggle2, cosolvent_unit(if (identical(cosolvent_unit(), "mass")) "mol" else "mass"))
  observe({ cs <- names(rv$current_blend)
    for (old in setdiff(names(.blend_frac_obs), cs)) if (!is.null(.blend_frac_obs[[old]])) { .blend_frac_obs[[old]]$destroy(); .blend_frac_obs[[old]] <- NULL }
    for (s in cs) if (is.null(.blend_frac_obs[[s]])) local({ ss <- s
      .blend_frac_obs[[ss]] <<- observeEvent(input[[paste0("frac_", ss)]],
        { v <- input[[paste0("frac_", ss)]]; if (!is.null(v) && !is.na(v)) rv$current_blend[[ss]] <- as.numeric(v) },
        ignoreInit = TRUE, ignoreNULL = TRUE) }) })
  observe({ cs <- names(rv$current_blend)
    for (s in cs) if (is.null(.blend_rm_obs[[s]])) local({ ss <- s
      .blend_rm_obs[[ss]] <<- observeEvent(input[[paste0("remove_", ss)]],
        { updateNumericInput(session, paste0("frac_", ss), value = NA); rv$current_blend[[ss]] <- NULL },
        ignoreInit = TRUE, ignoreNULL = TRUE) }) })
  observeEvent(input$clear_blend, { old <- names(rv$current_blend); rv$current_blend <- list()
    for (s in old) updateNumericInput(session, paste0("frac_", s), value = NA) })
  # Current blend names (for the miscibility warning + cosolv_sel).
  .cosolv_selected <- function() if (identical(input$modifier_type %||% "pure", "blend")) names(rv$current_blend) else {
    s <- input$modifier_selection; s[!is.na(s) & nzchar(s)] }
  # "Total co-solvent" field: a numeric input when the checkbox is on, a disabled "No co-solvent"
  # placeholder when off. The small badge to its right toggles the unit (mol % <-> mass %). The label uses
  # white-space:nowrap so its title never wraps to a second line.
  output$cosolvent_field_ui <- renderUI({
    on <- isTRUE(input$use_cosolvent) && !cosolvent_blocked()
    unit <- cosolvent_unit(); ulab <- if (identical(unit, "mass")) i18n_r()$t("mass %") else i18n_r()$t("mol %")
    # Interactive unit toggle: a distinct blue pill (not the grey of the static range badges) so it reads as
    # clickable, placed inline at the RIGHT of the title. margin-left:auto pushes it to the label's far edge.
    badge <- actionButton(ns("cosolvent_unit_toggle"), ulab, title = i18n_r()$t("Toggle mol % / mass %"),
      class = "btn btn-xs", style = "background:#3c8dbc;color:#fff;border:0;border-radius:11px;padding:1px 10px;font-size:11px;font-weight:700;margin-left:auto;")
    help_icon <- input_help(i18n_r()$t("Total amount of co-solvent (modifier) added to the CO₂, expressed as a fraction of the whole fluid. Toggle the badge to switch between mole % and mass %."),
      title = i18n_r()$t("Total co-solvent"), buttonLabel = i18n_r()$t("OK"))
    field <- if (on)
      numericInput(ns("cosolvent_frac"), NULL, value = input$cosolvent_frac %||% (defaults$cosolvent_frac %||% 5),
                   min = 0.1, max = if (identical(unit, "mass")) 60 else 30, step = 0.5, width = "100%")
    else shinyjs::disabled(textInput(ns("cosolvent_frac_off"), NULL, value = i18n_r()$t("No co-solvent"), width = "100%"))
    tags$div(
      tags$label(class = "control-label",
        style = "display:flex; align-items:center; gap:6px; width:100%; margin-bottom:4px;",
        tags$span(paste0(i18n_r()$t("Total co-solvent"), " (", ulab, ")"), style = "font-weight:600;"),
        help_icon, badge),
      field)
  })
  outputOptions(output, "cosolvent_field_ui", suspendWhenHidden = FALSE)
  observeEvent(input$cosolvent_unit_toggle,
    cosolvent_unit(if (identical(cosolvent_unit(), "mass")) "mol" else "mass"))
  # Gate the "Use co-solvent" checkbox for models that cannot represent a co-solvent (disable + force off).
  observe({
    if (cosolvent_blocked()) {
      shinyjs::disable("use_cosolvent")
      if (isTRUE(input$use_cosolvent)) updateCheckboxInput(session, "use_cosolvent", value = FALSE)
    } else shinyjs::enable("use_cosolvent")
  })
  output$cosolvent_block_info_ui <- renderUI({
    if (!cosolvent_blocked()) return(NULL)
    lab <- switch(cur_model(), makarov = "Makarov (ML)", experimental = i18n_r()$t("Experimental"), cur_model())
    div(class = "alert alert-info", style = "padding:6px 10px; margin-top:8px; font-size:0.86em;",
      icon("circle-info"), " ",
      sprintf(i18n_r()$t("The %s model predicts pure-CO₂ solubility and does not model a co-solvent. Switch to CSP-2021 or openCOSMO-RS-Phi to add one."), lab))
  })
  outputOptions(output, "cosolvent_block_info_ui", suspendWhenHidden = FALSE)
  # Contextual blend miscibility warning (below): verifies — at the entered proportions and the extraction
  # temperature — whether the blend forms one homogeneous entrainer phase (COSMO-SAC-dsp flash, composition-
  # and temperature-aware). Proportions come from the Custom Blend builder above (input$frac_<solvent>).
  .cosolv_props <- function(sel) {
    props <- vapply(sel, function(s) { v <- input[[paste0("frac_", s)]] %||% rv$current_blend[[s]]
      if (is.null(v) || is.na(v) || v < 0) NA_real_ else as.numeric(v) }, numeric(1))
    if (all(is.na(props)) || sum(props, na.rm = TRUE) <= 0) props <- rep(1, length(sel))
    props[is.na(props)] <- 0; props
  }
  output$cosolvent_misc_ui <- renderUI({
    sel <- .cosolv_selected(); if (length(sel) < 2) return(NULL)
    props <- .cosolv_props(sel)
    Tc <- suppressWarnings(as.numeric(input$extract_temp)); if (!isTRUE(is.finite(Tc))) Tc <- 40
    chk <- tryCatch(blend_phase_check(sel, x = props, T = Tc + 273.15), error = function(e) NULL)
    if (is.null(chk)) return(NULL)
    ratio_txt <- paste(vapply(seq_along(sel), function(i)
      sprintf("%s %d%%", cosolv_pretty(sel[i]), round(100 * props[i] / sum(props))), character(1)),
      collapse = " : ")
    danger <- function(msg) div(class = "alert alert-danger",
      style = "padding:6px 10px;margin:6px 0;font-size:0.86em;", icon("triangle-exclamation"), " ", msg)
    # Reaction (thermodynamics can't judge) is always surfaced first.
    if (isTRUE(chk$reaction))
      return(danger(sprintf(i18n_r()$t("These co-solvents may react or interact on contact (%s). They will not form a stable entrainer — treat any blend result with caution."), ratio_txt)))
    if (isTRUE(chk$single_phase)) return(NULL)                 # verified homogeneous at these conditions → no warning
    if (identical(chk$method, "flash"))
      danger(sprintf(i18n_r()$t("At the selected ratio (%s) and %s °C these co-solvents split into two liquid phases — they will not form a single homogeneous entrainer. Adjust the ratio, raise the temperature, or choose different co-solvents."), ratio_txt, round(Tc)))
    else
      div(class = "alert alert-warning", style = "padding:6px 10px;margin:6px 0;font-size:0.86em;",
        icon("circle-exclamation"), " ",
        sprintf(i18n_r()$t("These co-solvents may not be fully miscible (%s); their miscibility at the exact selected ratio could not be verified from structure."), ratio_txt))
  })
  outputOptions(output, "cosolvent_misc_ui", suspendWhenHidden = FALSE)
  # Separator pressures / temperatures — multi-value inputs with a units-bearing range badge (turns red
  # when a value is out of range). The "Optimize" checkbox rides inline to the right of each input box
  # (vertically centred by the helper); ticking it greys/disables the corresponding input. Optimize is ON
  # by default. isolate() preserves the checkbox state across the badge's re-renders.
  # Number of separator STAGES (1–6) — caps how many separator pressures/temperatures can be entered, and
  # how many the optimiser produces.
  n_stages_r <- reactive({
    v <- suppressWarnings(as.integer(input$n_stages %||% (defaults$n_stages %||% 3)))[1]
    if (isTRUE(is.finite(v)) && v >= 1) min(v, 6L) else 3L
  })
  # Recovery pressure — the lowest separator pressure allowed (a hard floor for BOTH manual and optimised
  # separator pressures; the stream cannot be dropped below it).
  recovery_pressure_r <- reactive({
    v <- suppressWarnings(as.numeric(input$recovery_pressure %||% (defaults$recovery_pressure %||% 60)))[1]
    if (isTRUE(is.finite(v)) && v >= 1) v else 60
  })
  # Short italic "P" / "T" labels (units kept) so the range badge fits on one line without wrapping. The
  # separator-pressure badge's lower bound is the recovery pressure (entries below it flag red).
  output$sep_pres_ui <- renderUI(range_badge_multi_input(ns, i18n_r, "sep_pres",
    HTML(paste0(i18n_r()$t("Separator"), "&nbsp;<i>P</i>&nbsp;(bar)")), cur_vals("sep_pres", defaults$sep_pres %||% "150,90,60"),
    min_val = recovery_pressure_r(), max_val = 200,
    range_text = paste0(round(recovery_pressure_r()), "–200 ", i18n_r()$t("bar")),
    placeholder = i18n_r()$t("Type separator pressures..."), max_items = n_stages_r(),
    disabled = isTRUE(input$optimize_pres),
    help_content = i18n_r()$t("Separator stage pressures, listed highest first (the stream is depressurised stage by stage). Ignored when Optimize is ticked; none may be below the recovery pressure."),
    help_title = i18n_r()$t("Separator pressures (bar)"),
    trailing = div(class = "ck-flush", checkboxInput(ns("optimize_pres"), i18n_r()$t("Optimize"),
      value = isolate(input$optimize_pres) %||% TRUE))))
  outputOptions(output, "sep_pres_ui", suspendWhenHidden = FALSE)
  output$sep_temp_ui <- renderUI(range_badge_multi_input(ns, i18n_r, "sep_temp",
    HTML(paste0(i18n_r()$t("Separator"), "&nbsp;<i>T</i>&nbsp;(°C)")), cur_vals("sep_temp", defaults$sep_temp %||% "40"),
    min_val = -60, max_val = 200, range_text = paste0("−60–200 ", "°C"),
    placeholder = i18n_r()$t("Type separator temperatures..."), max_items = n_stages_r(),
    disabled = isTRUE(input$optimize_temp),
    help_content = i18n_r()$t("Separator stage temperatures. Enter one value to apply the same temperature to every stage, or one per stage. Ignored when Optimize is ticked."),
    help_title = i18n_r()$t("Separator temperatures (°C)"),
    trailing = div(class = "ck-flush", checkboxInput(ns("optimize_temp"), i18n_r()$t("Optimize"),
      value = isolate(input$optimize_temp) %||% FALSE))))
  outputOptions(output, "sep_temp_ui", suspendWhenHidden = FALSE)

  # Extraction conditions — single numeric inputs with a units-bearing range badge and short italic P / T
  # labels. Spaces preserved via &nbsp; around the italic symbol.
  output$extract_temp_ui <- renderUI(range_badge_input(ns, i18n_r, "extract_temp",
    HTML(paste0(i18n_r()$t("Extraction"), "&nbsp;<i>T</i>&nbsp;(°C)")),
    value = input$extract_temp %||% defaults$extract_temp %||% 40, min_val = 32, max_val = 200, step = 1,
    range_text = paste0("32–200 ", "°C"),
    help_content = i18n_r()$t("Temperature of the extraction vessel. When a solute's feed loading is left blank, its saturation at these extraction conditions is used as the feed."),
    help_title = i18n_r()$t("Extraction temperature")))
  outputOptions(output, "extract_temp_ui", suspendWhenHidden = FALSE)
  output$extract_pres_ui <- renderUI(range_badge_input(ns, i18n_r, "extract_pres",
    HTML(paste0(i18n_r()$t("Extraction"), "&nbsp;<i>P</i>&nbsp;(bar)")),
    value = input$extract_pres %||% defaults$extract_pres %||% 350, min_val = 60, max_val = 1000, step = 5,
    range_text = paste0("60–1000 ", i18n_r()$t("bar")),
    help_content = i18n_r()$t("Pressure of the extraction vessel. Must be at or above the highest separator pressure (the stream is depressurised into the separators)."),
    help_title = i18n_r()$t("Extraction pressure")))
  outputOptions(output, "extract_pres_ui", suspendWhenHidden = FALSE)

  # CO2 flow rate with a kg/h <-> g/min unit toggle (converted to kg/h for the cascade balance).
  # CO2-flow unit is a clickable green badge (kg/h <-> g/min), matching the unit-badge pattern used
  # elsewhere in the app. The value is kept canonically in kg/h so toggling the unit AUTO-CONVERTS the
  # displayed number (kg/h <-> g/min) rather than reinterpreting it.
  co2_unit_rv <- reactiveVal("kgh")
  co2_flow_kgh_rv <- reactiveVal(NA_real_)                    # canonical kg/h
  observeEvent(input$co2_flow, {                              # user edited the (unit-dependent) field
    v <- suppressWarnings(as.numeric(input$co2_flow))[1]; if (!is.finite(v)) return()
    co2_flow_kgh_rv(if (identical(isolate(co2_unit_rv()), "gmin")) v * 0.06 else v)
  })
  observeEvent(input$toggle_co2_unit, { co2_unit_rv(if (identical(co2_unit_rv(), "kgh")) "gmin" else "kgh") })
  output$co2_flow_ui <- renderUI({
    u <- co2_unit_rv(); ulab <- if (identical(u, "kgh")) "kg/h" else "g/min"
    base <- isolate(co2_flow_kgh_rv()); if (!isTRUE(is.finite(base))) base <- defaults$co2_flow %||% 3
    disp <- if (identical(u, "gmin")) base * 1000 / 60 else base
    div(
      tags$label(class = "control-label", style = "display:flex; align-items:center; justify-content:space-between; width:100%;",
        tags$span(HTML(paste0("CO<sub>2</sub>&nbsp;", i18n_r()$t("flow"))), " ",
          input_help(i18n_r()$t("Mass flow rate of CO₂ through the cascade. It sets the absolute deposited mass per stage (kg/h); click the unit badge to toggle kg/h and g/min."),
            title = i18n_r()$t("CO₂ flow rate"), buttonLabel = i18n_r()$t("OK"))),
        actionLink(ns("toggle_co2_unit"), ulab,
          style = "font-size:10px; padding:1px 6px; border-radius:3px; text-decoration:none; background-color:#17a2b8; color:white; margin-left:auto; font-weight:normal; cursor:pointer;",
          title = i18n_r()$t("Click to toggle: kg/h / g/min"))),
      numericInput(ns("co2_flow"), label = NULL, value = round(disp, 4), min = 0)
    )
  })
  outputOptions(output, "co2_flow_ui", suspendWhenHidden = FALSE)

  # Target recovery (%) and number of separators — single numeric inputs with range badges, inline with
  # the CO2 flow rate. Rendered server-side so the badge turns red when out of range.
  output$recovery_target_ui <- renderUI(range_badge_input(ns, i18n_r, "recovery_target",
    i18n_r()$t("Recovery (%)"), value = input$recovery_target %||% 90, min_val = 0.1, max_val = 100,
    step = 1, range_text = "0.1–100%",
    help_content = i18n_r()$t("Fraction of each solute you aim to recover. The optimizer reports the highest separator pressure that still reaches it (or flags it as not reached)."),
    help_title = i18n_r()$t("Target recovery (%)")))
  outputOptions(output, "recovery_target_ui", suspendWhenHidden = FALSE)
  output$n_stages_ui <- renderUI(range_badge_input(ns, i18n_r, "n_stages",
    i18n_r()$t("Separators"), value = input$n_stages %||% (defaults$n_stages %||% 3), min_val = 1,
    max_val = 6, step = 1, range_text = "1–6",
    help_content = i18n_r()$t("Number of separator stages (1–6). Caps how many separator pressures/temperatures can be entered and how many the optimizer produces."),
    help_title = i18n_r()$t("Number of separators")))
  outputOptions(output, "n_stages_ui", suspendWhenHidden = FALSE)
  output$recovery_pressure_ui <- renderUI(range_badge_input(ns, i18n_r, "recovery_pressure",
    HTML(paste0(i18n_r()$t("Recovery"), "&nbsp;<i>P</i>&nbsp;(bar)")),
    value = input$recovery_pressure %||% (defaults$recovery_pressure %||% 60), min_val = 1, max_val = 200,
    step = 5, range_text = paste0("1–200 ", i18n_r()$t("bar")),
    help_content = i18n_r()$t("The lowest separator pressure allowed. Neither manual entry nor optimization can drop a separator below this pressure — if the target recovery needs a lower pressure, it is reported as not reached."),
    help_title = i18n_r()$t("Recovery pressure")))
  outputOptions(output, "recovery_pressure_ui", suspendWhenHidden = FALSE)

  parse_num <- function(x) { v <- suppressWarnings(as.numeric(x)); v[!is.na(v)] }

  # Per-solute feed loading (g solute / kg CO2) from its input; NA when blank/invalid (caller falls back
  # to the solute's saturation at the extraction conditions).
  loading_of <- function(nm) {
    v <- suppressWarnings(as.numeric(input[[paste0("load_", gsub("[^A-Za-z0-9]", "_", nm))]]))
    if (length(v) == 1 && is.finite(v) && v > 0) v else NA_real_
  }

  # Per-solute co-deposition reference partition D0 (= gamma_CO2/gamma_oil at the extraction condition);
  # NA when blank/invalid (that solute is not co-deposited). Only used when co-deposition is enabled.
  codep_of <- function(nm) {
    v <- suppressWarnings(as.numeric(input[[paste0("codep_", gsub("[^A-Za-z0-9]", "_", nm))]]))
    if (length(v) == 1 && is.finite(v) && v > 0) v else NA_real_
  }

  # Per-solute FEED LOADING list (in the Separator Cascade section): each selected solute inline — bold
  # name on the left (no CAS here), a green saturation badge, its loading input, and a red delete button.
  # A CSS grid with a `max-content` first column sizes the name column to EXACTLY the longest name (so the
  # loading inputs line up with no estimation gap), and the small column gap keeps everything tight. The
  # Per-solute loadings grid: [name (+ green saturation badge)] [g/kg CO₂ input] [D₀ input?] [delete].
  # The loading input DEFAULTS to each solute's saturation (shown as the badge); edit for a known value.
  # Column titles are shortened with dynamic hover tooltips. State preserved across re-renders via isolate().
  output$feed_loadings_ui <- renderUI({
    df <- combined_solutes()
    if (is.null(df) || !nrow(df))
      return(tags$p(style = "color:#888; font-size:0.9em;", i18n$t("Select solutes in the Solute Selection section first.")))
    m <- cur_model(); eos <- input$eos %||% "spanwagner"
    ext_T <- suppressWarnings(as.numeric(input$extract_temp %||% 40))[1]
    ext_P <- suppressWarnings(as.numeric(input$extract_pres %||% 350))[1]
    codep_on <- isTRUE(input$codep_enable)
    # g/kg CO₂ header: title + a static range badge (0–1000) + the loading-estimation magic wand inline on
    # the right (margin-left:auto in create_estimate_link pushes it to the far edge of the flex label).
    load_hdr <- tags$label(class = "control-label", style = "margin:0; display:flex; align-items:center; gap:6px;",
      tags$span(i18n_r()$t("g/kg CO₂"),
        title = i18n_r()$t("Feed loading in g solute per kg CO₂. Defaults to the saturation solubility at the extraction condition; edit to enter a known loading. Range 0–1000.")),
      tags$span("0–1000", title = i18n_r()$t("Allowed range 0–1000 g/kg CO₂"),
        style = "font-size:10px; padding:1px 5px; border-radius:3px; background:#6c757d; color:white; font-weight:normal;"),
      create_estimate_link(ns, i18n_r, "open_cer_modal", i18n_r()$t("Estimate loading from an extraction curve")))
    # D₀ column: no range badge (any positive value is valid); dynamic hover tooltip; tight D-subscript-0.
    # The D₀-estimation magic wand sits inline with this title (pushed right by create_estimate_link).
    codep_hdr <- if (codep_on) tags$label(class = "control-label", style = "margin:0; display:flex; align-items:center; gap:6px;",
      tags$span(HTML("<i>D</i><sub>0</sub>"),
        title = i18n_r()$t("Reference partition D₀ = γ(CO₂)/γ(oil) at the extraction condition. D₀ > 1 means the solute prefers the oil (bulk deposit) and is co-precipitated. Any positive value.")),
      create_estimate_link(ns, i18n_r, "open_codep_modal", i18n_r()$t("Auto-estimate D₀ (FastSigma)"))) else NULL
    cells <- c(list(div(), load_hdr),
               if (codep_on) list(codep_hdr),
               list(div()))                                    # (name + delete columns have no header)
    for (i in seq_len(nrow(df))) {
      nm <- df$name[i]; key <- gsub("[^A-Za-z0-9]", "_", nm)
      sat <- suppressWarnings(solub_gkg(model_solub(df[i, , drop = FALSE], ext_T, ext_P, m, eos)[1], df$mw[i]))
      sat_badge <- if (isTRUE(is.finite(sat)))
        tags$span(sprintf("%.3g g/kg", sat),
          title = i18n_r()$t("Saturation solubility at the extraction condition (the default loading)."),
          style = "font-size:10px; font-weight:700; padding:1px 6px; margin-left:6px; border-radius:3px; background:#28a745; color:white; vertical-align:middle; white-space:nowrap;")
      else NULL
      name_cell <- tags$span(style = "white-space:nowrap; font-size:14px;", strong(nm), sat_badge)
      load_input <- numericInput(ns(paste0("load_", key)), label = NULL,
        value = isolate(input[[paste0("load_", key)]]) %||% (if (isTRUE(is.finite(sat))) round(sat, 3) else NA),
        min = 0, max = 1000, step = 0.1) |> tagAppendAttributes(style = "margin-bottom:0; width:100%;")
      cells <- c(cells, list(name_cell, load_input),
        if (codep_on) list(numericInput(ns(paste0("codep_", key)), label = NULL,
          value = isolate(input[[paste0("codep_", key)]]), min = 0, step = 0.5) |>
          tagAppendAttributes(style = "margin-bottom:0; width:100%;")),
        list(actionButton(ns("del_solute_btn"), NULL, icon = icon("times"), class = "btn-danger",
          style = "color:white; width:34px; height:34px; padding:0;",
          onclick = sprintf("Shiny.setInputValue('%s', %s, {priority:'event'})",
            ns("del_solute_name"), jsonlite::toJSON(nm, auto_unbox = TRUE)))))
    }
    tmpl <- if (codep_on)
      "display:grid; grid-template-columns:max-content minmax(150px,210px) minmax(90px,130px) 34px; column-gap:10px; row-gap:6px; align-items:center;"
    else
      "display:grid; grid-template-columns:max-content minmax(170px,230px) 34px; column-gap:10px; row-gap:6px; align-items:center;"
    do.call(tags$div, c(list(style = tmpl), cells))
  })
  outputOptions(output, "feed_loadings_ui", suspendWhenHidden = FALSE)

  # (The loading- and D₀-estimation magic wands are rendered inline inside feed_loadings_ui's column
  # headers — see load_hdr / codep_hdr above — so they sit next to the "g/kg CO₂" and "D₀" titles.)

  # Clicking a saturation badge fills that solute's loading input with the saturation value.
  observeEvent(input$fill_load, {
    p <- input$fill_load; if (is.null(p$id) || is.null(p$val)) return()
    updateNumericInput(session, p$id, value = suppressWarnings(as.numeric(p$val)))
  })

  # ---- Feed loading from an extraction curve (CER slope) --------------------
  # A modal that turns an overall extraction curve into the near-saturation (peak) loading the separator
  # sees — the CER-period slope via sfe_cer_loading() — and applies it to a solute's feed field. Users can
  # paste/upload their own curve or pick a bundled REFERENCE oil (inst/extdata/extraction_curves.json).
  cer_refs <- reactive({
    f <- system.file("extdata", "examples", "extraction_curves.json", package = "supeRcrit")
    if (!nzchar(f)) return(list())
    tryCatch(jsonlite::fromJSON(f, simplifyVector = FALSE)$curves %||% list(), error = function(e) list())
  })
  parse_curve <- function(txt) {
    if (is.null(txt) || !nzchar(trimws(txt))) return(NULL)
    rows <- lapply(strsplit(txt, "\n")[[1]], function(l) {
      v <- suppressWarnings(as.numeric(strsplit(trimws(l), "[ ,\t;]+")[[1]])); v <- v[is.finite(v)]
      if (length(v) >= 2) v[1:2] else NULL
    })
    rows <- Filter(Negate(is.null), rows)
    if (length(rows) < 3) return(NULL)
    m <- do.call(rbind, rows); data.frame(x = m[, 1], y = m[, 2])
  }
  # ---- Reference OEC database. Bundled reference oils (extraction_curves.json) are "core"; users can add
  #      their own by uploading a 2-column OEC (CSV). Each entry carries its full process parameters + axes.
  #      Selecting a row + "Load Selected" computes its CER loading and shows the curve + apply options.
  #      User OECs persist to the user-settings folder.
  # NB: supercrit_user_dir() dir.create()s its whole argument, so pass the DIRECTORY and append the file.
  .oec_path <- tryCatch(file.path(supercrit_user_dir("user-settings"), "reference_oecs.json"), error = function(e) NULL)
  if (!is.null(.oec_path) && dir.exists(.oec_path)) try(unlink(.oec_path, recursive = TRUE), silent = TRUE)  # heal a path wrongly made a dir
  user_oecs <- reactiveVal(local({
    if (!is.null(.oec_path) && file.exists(.oec_path) && !dir.exists(.oec_path))
      tryCatch(jsonlite::fromJSON(.oec_path, simplifyVector = FALSE), error = function(e) list()) else list() }))
  save_user_oecs <- function() if (!is.null(.oec_path)) tryCatch({
    if (dir.exists(.oec_path)) unlink(.oec_path, recursive = TRUE)
    dir.create(dirname(.oec_path), recursive = TRUE, showWarnings = FALSE)
    jsonlite::write_json(user_oecs(), .oec_path, auto_unbox = TRUE, pretty = TRUE) }, error = function(e) NULL)
  all_oecs <- reactive(c(lapply(cer_refs(), function(o) { o$core <- TRUE; o }),
                         lapply(user_oecs(), function(o) { o$core <- FALSE; o })))
  .xtype_lab <- function(t) switch(t %||% "co2_mass", co2_mass = "CO₂ (kg)", sf = "S/F", time = "Time (min)", t)
  .ytype_lab <- function(t) switch(t %||% "mass_g", mass_g = "Extract (g)", yield_frac = "Yield (g/g)", yield_pct = "Yield (%)", t)
  cer_selected <- reactiveVal(NULL)                       # currently-loaded OEC entry
  oec_csv <- reactiveVal(NULL)                            # parsed CSV pending a column pick
  # CER loading for one OEC entry (params + curve), with an optional target-solute wt%.
  oec_cer <- function(o, tq_pct = NULL) {
    if (is.null(o$x) || is.null(o$y)) return(NULL)
    x <- suppressWarnings(as.numeric(unlist(o$x))); y <- suppressWarnings(as.numeric(unlist(o$y)))
    ok <- is.finite(x) & is.finite(y); x <- x[ok]; y <- y[ok]; if (length(x) < 3) return(NULL)
    tqf <- { v <- suppressWarnings(as.numeric(tq_pct %||% o$tq_pct)); if (isTRUE(is.finite(v))) v / 100 else NULL }
    tryCatch(supeRcrit:::sfe_cer_loading(x, y, x_type = o$x_type %||% "co2_mass", y_type = o$y_type %||% "mass_g",
      co2_flow = suppressWarnings(as.numeric(o$co2_flow)), feed_mass = suppressWarnings(as.numeric(o$feed_mass)),
      tq_frac = tqf), error = function(e) NULL) }

  output$oec_table <- DT::renderDataTable({
    oecs <- all_oecs()
    badge <- function(txt, bg) sprintf("<span style='background:%s;color:#fff;font-size:9px;font-weight:700;border-radius:3px;padding:1px 5px;'>%s</span>", bg, txt)
    df <- data.frame(
      Name = vapply(oecs, function(o) o$name %||% "oil", character(1)),
      Source = vapply(oecs, function(o) if (isTRUE(o$core)) badge("CORE", "#2e7d32") else badge(i18n_r()$t("MINE"), "#1565c0"), character(1)),
      `T (°C)` = vapply(oecs, function(o) if (is.null(o$T)) "" else as.character(o$T), character(1)),
      `P (bar)` = vapply(oecs, function(o) if (is.null(o$P)) "" else as.character(o$P), character(1)),
      X = vapply(oecs, function(o) .xtype_lab(o$x_type), character(1)),
      Y = vapply(oecs, function(o) .ytype_lab(o$y_type), character(1)),
      OEC = vapply(seq_along(oecs), function(i) { o <- oecs[[i]]
        if (!is.null(o$x) && length(unlist(o$x)) >= 3) "<span style='color:#2e7d32; font-weight:700;'>&#10003;</span>"
        else sprintf("<button class='btn btn-primary btn-xs' style='color:#fff;' onclick=\"Shiny.setInputValue('%s', %d, {priority:'event'})\">%s</button>",
          ns("oec_upload_row"), i, i18n_r()$t("Upload OEC (CSV)")) }, character(1)),
      check.names = FALSE, stringsAsFactors = FALSE)
    DT::datatable(df, escape = FALSE, rownames = FALSE, selection = "single",
      options = list(pageLength = 8, dom = "ftp", language = tablang(), scrollX = TRUE))
  }, server = FALSE)
  outputOptions(output, "oec_table", suspendWhenHidden = FALSE)

  cer_modal <- function() modalDialog(
    title = i18n_r()$t("Reference OEC database — estimate feed loading"), size = "xl", easyClose = TRUE,
    tags$style(HTML("@media (min-width:768px){.modal-dialog.modal-xl{width:95vw!important;max-width:1500px!important;}}")),
    div(style = "width:100%; overflow-x:hidden;",
      tags$p(style = "color:#6c757d; font-size:13px; margin-bottom:6px;",
        i18n_r()$t("Reference overall extraction curves (OECs). Pick a row and press Load Selected to compute its CER-slope near-saturation loading; then apply it to a solute. Add your own by uploading a 2-column OEC (CSV).")),
      div(style = "display:flex; gap:10px; margin-bottom:8px;",
        actionButton(ns("oec_add_open"), i18n_r()$t("Add reference OEC (upload CSV)"), icon = icon("plus"),
          class = "btn btn-success btn-sm", style = "color:white;"),
        actionButton(ns("oec_load_selected"), i18n_r()$t("Load Selected"), icon = icon("download"),
          class = "btn btn-primary btn-sm", style = "color:white;")),
      DT::dataTableOutput(ns("oec_table")),
      tags$hr(),
      fluidRow(column(6, uiOutput(ns("cer_result_ui"))),
               column(6, plotly::plotlyOutput(ns("cer_plot"), height = "300px")))),
    footer = modalButton(i18n_r()$t("Close")))
  observeEvent(input$open_cer_modal, { rv$cer_result <- NULL; cer_selected(NULL); showModal(cer_modal()) })

  # Load Selected -> compute the CER loading for the picked row.
  observeEvent(input$oec_load_selected, {
    sel <- input$oec_table_rows_selected
    if (is.null(sel) || !length(sel)) { showNotification(i18n$t("Select a reference OEC first."), type = "warning"); return() }
    o <- all_oecs()[[sel]]; cer_selected(o)
    r <- oec_cer(o, input$cer_tqpct)
    if (is.null(r)) { showNotification(i18n$t("This entry has no OEC curve yet — upload one first."), type = "warning"); rv$cer_result <- NULL; return() }
    rv$cer_result <- r
  })
  # Recompute when the target wt% changes (keeps the target-solute loading live).
  observeEvent(input$cer_tqpct, { o <- cer_selected(); if (is.null(o)) return()
    r <- oec_cer(o, input$cer_tqpct); if (!is.null(r)) rv$cer_result <- r }, ignoreInit = TRUE)
  output$cer_result_ui <- renderUI({
    r <- rv$cer_result
    if (is.null(r)) return(tags$p(style = "color:#888;", i18n_r()$t("Select a reference OEC above and press Load Selected.")))
    tqL <- r$tq_loading
    tagList(
      div(class = "alert alert-success", style = "margin-bottom:10px;",
        tags$h4(style = "margin-top:0;", icon("droplet"), " ",
          sprintf(i18n_r()$t("CER loading: %.3g g/kg CO₂"), r$Y_cer)),
        tags$p(style = "margin-bottom:4px;", sprintf(i18n_r()$t("Whole-run average: %.3g g/kg (the separator sees the CER peak, not the average). Fit R² = %.4f over %d points."),
          r$Y_avg, r$fit$r2, r$cer_n)),
        if (isTRUE(is.finite(tqL))) tags$p(style = "margin-bottom:0;", tags$strong(sprintf(i18n_r()$t("Target-solute loading: %.3g g/kg CO₂"), tqL))) else
          tags$p(style = "margin-bottom:0; color:#888;", i18n_r()$t("Set a target wt% to get the solute loading."))),
      # Apply row: Apply-to-solute + Target-in-extract (wt %) + Use-as-feed-loading.
      div(style = "display:flex; gap:8px; align-items:flex-end; flex-wrap:wrap;",
        div(style = "flex:1; min-width:140px;", selectInput(ns("cer_apply_solute"), i18n_r()$t("Apply to solute"),
          choices = { df <- combined_solutes(); if (is.null(df) || !nrow(df)) character(0) else df$name }, width = "100%")),
        div(style = "flex:0 0 130px;", numericInput(ns("cer_tqpct"), i18n_r()$t("Target in extract (wt %)"),
          value = isolate(input$cer_tqpct) %||% (cer_selected()$tq_pct %||% 2), min = 0, max = 100, step = 0.5, width = "100%")),
        # margin-bottom:15px matches the inputs' Bootstrap .form-group bottom margin so that, under the
        # row's align-items:flex-end, the button's VISIBLE bottom lines up with the input boxes (not 15px
        # below them, where a margin-bottom:0 button lands against their invisible margin edge).
        actionButton(ns("cer_apply"), i18n_r()$t("Use as feed loading"), icon = icon("check"),
          class = "btn btn-success", style = "color:white; margin-bottom:15px;",
          disabled = if (isTRUE(is.finite(tqL))) NULL else "disabled")))
  })
  outputOptions(output, "cer_result_ui", suspendWhenHidden = FALSE)
  output$cer_plot <- plotly::renderPlotly({
    r <- rv$cer_result; req(r); cc <- r$curve
    cerpts <- cc[cc$is_cer, , drop = FALSE]; rest <- cc[!cc$is_cer, , drop = FALSE]
    xr <- c(0, max(cerpts$m_co2_kg))
    p <- plotly::plot_ly()
    p <- plotly::add_markers(p, data = rest, x = ~m_co2_kg, y = ~extract_g, name = i18n_r()$t("Falling / diffusion"),
      marker = list(color = "#bbb", size = 7))
    p <- plotly::add_markers(p, data = cerpts, x = ~m_co2_kg, y = ~extract_g, name = i18n_r()$t("CER (linear)"),
      marker = list(color = "#e8720c", size = 8))
    p <- plotly::add_lines(p, x = xr, y = r$fit$intercept + r$fit$slope * xr, name = i18n_r()$t("CER fit"),
      line = list(color = "#e8720c", dash = "dash", width = 2))
    plotly::layout(p, xaxis = list(title = i18n_r()$t("Cumulative CO₂ (kg)")),
      yaxis = list(title = i18n_r()$t("Cumulative extract (g)")), legend = list(orientation = "h", y = 1.1))
  })
  outputOptions(output, "cer_plot", suspendWhenHidden = FALSE)
  observeEvent(input$cer_apply, {
    r <- rv$cer_result; req(r, isTRUE(is.finite(r$tq_loading)))
    nm <- input$cer_apply_solute; req(nm)
    updateNumericInput(session, paste0("load_", gsub("[^A-Za-z0-9]", "_", nm)), value = round(r$tq_loading, 4))
    showNotification(sprintf(i18n$t("Applied %.3g g/kg CO₂ to %s."), r$tq_loading, nm), type = "message", duration = 4)
    removeModal()
  })

  # ---- Add / upload a reference OEC (name + process params + a 2-column CSV with a column picker) ----
  rv_oec_edit <- reactiveVal(NULL)     # index into user_oecs() being edited (NULL = new entry)
  oec_form_modal <- function(entry = NULL, edit_idx = NULL) {
    oec_csv(NULL); rv_oec_edit(edit_idx)
    xt <- stats::setNames(c("co2_mass", "sf", "time"), c(i18n_r()$t("Cumulative CO₂ (kg)"), i18n_r()$t("Solvent-to-feed ratio"), i18n_r()$t("Time (min)")))
    yt <- stats::setNames(c("mass_g", "yield_frac", "yield_pct"), c(i18n_r()$t("Extract mass (g)"), i18n_r()$t("Yield (g/g feed)"), i18n_r()$t("Yield (%)")))
    num <- function(id, lab, val, ...) numericInput(ns(id), lab, value = val, width = "100%", ...)
    modalDialog(title = i18n_r()$t("Add reference OEC"), size = "l", easyClose = FALSE,
      textInput(ns("oec_name"), i18n_r()$t("Name"), value = entry$name %||% "", width = "100%"),
      fluidRow(column(6, num("oec_T", HTML(paste0(i18n_r()$t("Extraction"), " <i>T</i> (°C)")), entry$T %||% suppressWarnings(as.numeric(input$extract_temp %||% 40)))),
               column(6, num("oec_P", HTML(paste0(i18n_r()$t("Extraction"), " <i>P</i> (bar)")), entry$P %||% suppressWarnings(as.numeric(input$extract_pres %||% 350))))),
      fluidRow(column(6, selectInput(ns("oec_xtype"), i18n_r()$t("X axis"), choices = xt, selected = entry$x_type %||% "co2_mass", width = "100%")),
               column(6, selectInput(ns("oec_ytype"), i18n_r()$t("Y axis"), choices = yt, selected = entry$y_type %||% "mass_g", width = "100%"))),
      fluidRow(column(4, num("oec_flow", i18n_r()$t("CO₂ flow (kg/min)"), entry$co2_flow %||% NA, min = 0)),
               column(4, num("oec_feed", i18n_r()$t("Feed mass (kg)"), entry$feed_mass %||% NA, min = 0)),
               column(4, num("oec_tqpct2", i18n_r()$t("Target in extract (wt %)"), entry$tq_pct %||% 2, min = 0, max = 100))),
      tags$small(class = "text-muted", i18n_r()$t("CO₂ flow is needed only for a time axis; feed mass only for a yield axis that is not solvent-to-feed.")),
      tags$hr(),
      fileInput(ns("oec_file"), i18n_r()$t("Upload OEC (2-column CSV)"), accept = ".csv", width = "100%"),
      uiOutput(ns("oec_colpick_ui")),
      footer = tagList(modalButton(i18n_r()$t("Cancel")),
        shinyjs::disabled(actionButton(ns("oec_save"), i18n_r()$t("Save"), icon = icon("save"), class = "btn-success", style = "color:white;"))))
  }
  observeEvent(input$oec_add_open, showModal(oec_form_modal()))
  observeEvent(input$oec_upload_row, { i <- input$oec_upload_row - length(cer_refs())   # -> user-list index
    ent <- if (i >= 1 && i <= length(user_oecs())) user_oecs()[[i]] else NULL
    showModal(oec_form_modal(ent, if (!is.null(ent)) i else NULL)) })
  observeEvent(input$oec_file, {
    f <- input$oec_file; req(f)
    first <- tryCatch(readLines(f$datapath, n = 1), error = function(e) "")
    toks <- strsplit(trimws(first), "[,;\t]")[[1]]
    has_header <- length(toks) >= 2 && any(is.na(suppressWarnings(as.numeric(toks))))
    d <- tryCatch(utils::read.csv(f$datapath, header = has_header, stringsAsFactors = FALSE, check.names = TRUE), error = function(e) NULL)
    if (is.null(d) || ncol(d) < 2) { showNotification(i18n$t("Could not read a 2-column CSV."), type = "error"); oec_csv(NULL); return() }
    oec_csv(d)
  })
  # Parse the picked columns into a validated OEC curve (no negatives; the origin is prepended if missing).
  .oec_parse <- function() {
    d <- oec_csv(); if (is.null(d)) return(list(ok = FALSE, msg = ""))
    xc <- input$oec_xcol; yc <- input$oec_ycol
    if (is.null(xc) || is.null(yc)) return(list(ok = FALSE, msg = i18n_r()$t("Choose the temporal (X) and signal (Y) columns.")))
    if (identical(xc, yc)) return(list(ok = FALSE, msg = i18n_r()$t("X and Y must be different columns.")))
    x <- suppressWarnings(as.numeric(d[[xc]])); y <- suppressWarnings(as.numeric(d[[yc]]))
    ok <- is.finite(x) & is.finite(y); x <- x[ok]; y <- y[ok]
    if (length(x) < 3) return(list(ok = FALSE, msg = i18n_r()$t("Need at least 3 numeric rows.")))
    if (any(x < 0) || any(y < 0)) return(list(ok = FALSE, msg = i18n_r()$t("No values below 0 are allowed in either column.")))
    o <- order(x); x <- x[o]; y <- y[o]
    if (!(isTRUE(x[1] == 0) && isTRUE(y[1] == 0))) { x <- c(0, x); y <- c(0, y) }   # silently prepend the origin
    list(ok = TRUE, x = x, y = y, msg = sprintf(i18n_r()$t("Looks good — %d points (origin included)."), length(x)))
  }
  output$oec_colpick_ui <- renderUI({
    d <- oec_csv(); if (is.null(d)) return(NULL)
    cols <- names(d); pr <- .oec_parse()
    tagList(
      fluidRow(
        column(6, selectInput(ns("oec_xcol"), i18n_r()$t("Temporal column (X)"), choices = cols, selected = input$oec_xcol %||% cols[1], width = "100%")),
        column(6, selectInput(ns("oec_ycol"), i18n_r()$t("Signal column (Y)"), choices = cols, selected = input$oec_ycol %||% cols[min(2, length(cols))], width = "100%"))),
      div(style = paste0("font-size:0.85em; font-weight:600; color:", if (isTRUE(pr$ok)) "#2e7d32" else "#c0392b", ";"), pr$msg))
  })
  outputOptions(output, "oec_colpick_ui", suspendWhenHidden = FALSE)
  # Save is enabled only once the columns are chosen + valid AND a name is given.
  observe(shinyjs::toggleState("oec_save", condition = isTRUE(.oec_parse()$ok) && nzchar(input$oec_name %||% "")))
  observeEvent(input$oec_save, {
    pr <- .oec_parse(); if (!isTRUE(pr$ok)) return()
    entry <- list(name = input$oec_name, T = suppressWarnings(as.numeric(input$oec_T)), P = suppressWarnings(as.numeric(input$oec_P)),
      x_type = input$oec_xtype, y_type = input$oec_ytype,
      co2_flow = suppressWarnings(as.numeric(input$oec_flow)), feed_mass = suppressWarnings(as.numeric(input$oec_feed)),
      tq_pct = suppressWarnings(as.numeric(input$oec_tqpct2)), x = as.list(pr$x), y = as.list(pr$y))
    us <- user_oecs(); ei <- rv_oec_edit()
    if (!is.null(ei) && ei >= 1 && ei <= length(us)) us[[ei]] <- entry else us[[length(us) + 1]] <- entry
    user_oecs(us); save_user_oecs()
    removeModal(); showNotification(sprintf(i18n$t("Saved reference OEC '%s'."), entry$name), type = "message")
  })

  # ---- Co-Deposition Matrix database + builder. A matrix is a named mixture of components (each resolved
  #      against the compound database by name/CAS/SMILES, or by structure via mol_find), with weight %.
  #      Saved matrices persist and become selectable bulk-deposit surrogates in the D₀ estimator below.
  .cdm_path <- tryCatch(file.path(supercrit_user_dir("user-settings"), "codep_matrices.json"), error = function(e) NULL)
  if (!is.null(.cdm_path) && dir.exists(.cdm_path)) try(unlink(.cdm_path, recursive = TRUE), silent = TRUE)  # heal a path wrongly made a dir
  codep_matrices <- reactiveVal(local({
    if (!is.null(.cdm_path) && file.exists(.cdm_path) && !dir.exists(.cdm_path)) tryCatch(jsonlite::fromJSON(.cdm_path, simplifyVector = FALSE), error = function(e) list()) else list() }))
  save_codep_matrices <- function() if (!is.null(.cdm_path)) tryCatch({
    if (dir.exists(.cdm_path)) unlink(.cdm_path, recursive = TRUE)
    dir.create(dirname(.cdm_path), recursive = TRUE, showWarnings = FALSE)
    jsonlite::write_json(codep_matrices(), .cdm_path, auto_unbox = TRUE, pretty = TRUE) }, error = function(e) NULL)
  # A matrix is built as a list of component ROWS keyed by a stable auto-incrementing id. Each row is a
  # small state machine: unresolved (separate Name / CAS / SMILES fields + a "Resolve & add" button, enabled
  # once at least one field is filled) -> resolved (fields auto-filled & locked except the Name, a wt% input
  # with a 0.1–100 range badge, and a red delete button).
  matrix_comps <- reactiveVal(list())          # id -> list(name, cas, smiles, wt, resolved)
  .mrow_seq <- reactiveVal(0L)                 # monotonic row-id source (Date.now/Math.random unavailable)
  .cdm_obs <- list()
  .m_addrow <- function() {
    n <- .mrow_seq() + 1L; .mrow_seq(n); k <- paste0("r", n)
    cur <- matrix_comps(); cur[[k]] <- list(name = "", cas = "", smiles = "", wt = NA, resolved = FALSE)
    matrix_comps(cur); invisible(k)
  }
  matrix_modal <- function() { matrix_comps(list()); .m_addrow()   # start with one empty component row
    modalDialog(title = i18n_r()$t("Add co-deposition matrix"), size = "l", easyClose = FALSE,
      tags$p(style = "color:#6c757d; font-size:13px;", i18n_r()$t("Define the bulk deposit (oil/matrix) as a mixture of components — the same way you build a co-solvent blend. For each component type a name, CAS or SMILES and resolve it against the compound database (or by its structure), then set its weight %.")),
      # Matrix name + an "Add compound" button (adds a blank component row), separated by a vertical rule. A
      # spacer label above the button aligns it with the Name INPUT box (which carries its own label above).
      div(style = "display:flex; gap:10px; align-items:flex-end;",
        div(style = "flex:1;", textInput(ns("matrix_name"), i18n_r()$t("Matrix name"), width = "100%") |> tagAppendAttributes(style = "margin-bottom:0;")),
        tags$span(style = "border-left:1px solid #ccc; align-self:stretch;"),
        div(style = "flex:0 0 auto;",
          tags$label(HTML("&nbsp;"), class = "control-label", style = "display:block; margin-bottom:5px;"),
          actionButton(ns("matrix_add_row"), i18n_r()$t("Add compound"), icon = icon("plus"), class = "btn btn-success", style = "color:white;"))),
      tags$hr(), h5(i18n_r()$t("Matrix composition (wt %):")),
      uiOutput(ns("matrix_comp_ui")),
      footer = tagList(actionButton(ns("matrix_cancel"), i18n_r()$t("Cancel")),
        shinyjs::disabled(actionButton(ns("matrix_save"), i18n_r()$t("Save matrix"), icon = icon("save"), class = "btn-success", style = "color:white;"))))
  }
  observeEvent(input$codep_add_matrix, showModal(matrix_modal()))
  observeEvent(input$matrix_cancel, showModal(codep_modal()))   # return to the D₀ estimator
  observeEvent(input$matrix_add_row, .m_addrow())
  # Resolve ONE row's Name/CAS/SMILES fields -> {name, cas, smiles}: compound database first, then mol_find.
  .resolve_matrix_row <- function(k) {
    cur <- matrix_comps(); cc <- cur[[k]]; if (is.null(cc)) return()
    nm <- trimws(input[[paste0("mnm_", k)]] %||% ""); cs <- trimws(input[[paste0("mcas_", k)]] %||% "")
    sm <- trimws(input[[paste0("msmi_", k)]] %||% "")
    if (!nzchar(nm) && !nzchar(cs) && !nzchar(sm)) return()
    hit <- tryCatch(exp_db_match(exp_db_all(), smiles = if (nzchar(sm)) sm else NULL,
      cas = if (nzchar(cs)) cs else NULL, name = if (nzchar(nm)) nm else NULL), error = function(e) NULL)
    res <- if (!is.null(hit)) list(name = hit$name %||% nm, cas = hit$cas %||% cs, smiles = hit$smiles %||% sm)
      else { q <- if (nzchar(sm)) sm else if (nzchar(cs)) cs else nm
        md <- tryCatch(mol_find(q), error = function(e) NULL)
        if (is.null(md) || is.null(md$IDs)) { showNotification(sprintf(i18n$t("Could not resolve '%s' from the database or its structure."), q), type = "error"); return() }
        ids <- md$IDs; list(name = unname(ids["Name"]) %||% nm, cas = unname(ids["CAS"]) %||% cs, smiles = unname(ids["SMILES"]) %||% sm) }
    dup <- vapply(setdiff(names(cur), k), function(j) isTRUE(cur[[j]]$resolved) && identical(toupper(cur[[j]]$name %||% ""), toupper(res$name %||% "")), logical(1))
    if (length(dup) && any(dup)) { showNotification(i18n$t("Component already in the matrix."), type = "warning"); return() }
    cur[[k]] <- list(name = res$name %||% nm, cas = res$cas %||% "", smiles = res$smiles %||% "", wt = NA, resolved = TRUE)
    matrix_comps(cur)
    # Sync the (now editable) Name input to the resolved name — the re-render reads the live input via
    # isolate(), which still holds the pre-resolve query text until this update lands.
    updateTextInput(session, paste0("mnm_", k), value = res$name %||% nm)
  }
  output$matrix_comp_ui <- renderUI({
    cs <- matrix_comps(); if (!length(cs)) return(p(i18n_r()$t("No components yet — click \"Add compound\".")))
    rows <- lapply(names(cs), function(k) { cc <- cs[[k]]
      rmbtn <- actionButton(ns(paste0("mrm_", k)), NULL, icon = icon("times"), class = "btn btn-danger btn-sm", style = "flex-shrink:0; color:white;")
      if (!isTRUE(cc$resolved)) {
        # Unresolved: three separate query fields + a "Resolve & add" button (toggled on once one is filled).
        div(style = "display:flex; align-items:center; gap:6px; margin-bottom:6px;",
          div(style = "flex:1;", textInput(ns(paste0("mnm_", k)), NULL, value = cc$name, placeholder = i18n_r()$t("Name"), width = "100%") |> tagAppendAttributes(style = "margin-bottom:0;")),
          div(style = "flex:1;", textInput(ns(paste0("mcas_", k)), NULL, value = cc$cas, placeholder = "CAS", width = "100%") |> tagAppendAttributes(style = "margin-bottom:0;")),
          div(style = "flex:1.4;", textInput(ns(paste0("msmi_", k)), NULL, value = cc$smiles, placeholder = "SMILES", width = "100%") |> tagAppendAttributes(style = "margin-bottom:0;")),
          actionButton(ns(paste0("mres_", k)), i18n_r()$t("Resolve & add"), icon = icon("plus"), class = "btn btn-primary btn-sm", style = "flex-shrink:0; color:white;"),
          rmbtn)
      } else {
        # Resolved: Name editable; CAS/SMILES locked; wt% with a 0.1–100 range badge (red when out of range).
        wt <- suppressWarnings(as.numeric(cc$wt)); oor <- !isTRUE(is.finite(wt)) || wt < 0.1 || wt > 100
        badge <- tags$span("0.1–100", title = i18n_r()$t("Allowed range 0.1–100 wt %"),
          style = paste0("font-size:10px; padding:1px 5px; border-radius:3px; color:white; font-weight:normal; background:", if (oor) "#dc3545" else "#6c757d", ";"))
        div(style = "display:flex; align-items:flex-end; gap:6px; margin-bottom:6px;",
          # Name stays editable; read the LIVE value via isolate() (no re-render dependency) so a wt-triggered
          # re-render of another row does not wipe an in-progress name edit. Synced on resolve below.
          div(style = "flex:1;", textInput(ns(paste0("mnm_", k)), NULL, value = isolate(input[[paste0("mnm_", k)]]) %||% cc$name, width = "100%") |> tagAppendAttributes(style = "margin-bottom:0;")),
          div(style = "flex:1;", shinyjs::disabled(textInput(ns(paste0("mcas_", k)), NULL, value = cc$cas, width = "100%")) |> tagAppendAttributes(style = "margin-bottom:0;")),
          div(style = "flex:1.4;", shinyjs::disabled(textInput(ns(paste0("msmi_", k)), NULL, value = cc$smiles, width = "100%")) |> tagAppendAttributes(style = "margin-bottom:0;")),
          div(style = "flex:0 0 120px; display:flex; flex-direction:column;",
            div(style = "display:flex; justify-content:flex-end; margin-bottom:2px;", badge),
            numericInput(ns(paste0("mwt_", k)), NULL, value = if (isTRUE(is.finite(wt))) wt else NA, min = 0.1, max = 100, step = 0.1, width = "100%") |> tagAppendAttributes(style = "margin-bottom:0;")),
          rmbtn)
      } })
    # Live Total over RESOLVED rows (from stored wt) + an Equalize button, separated by a vertical rule.
    rk <- names(cs)[vapply(cs, function(c) isTRUE(c$resolved), logical(1))]
    total <- sum(vapply(rk, function(k) { v <- suppressWarnings(as.numeric(cs[[k]]$wt)); if (isTRUE(is.finite(v))) v else 0 }, numeric(1)), na.rm = TRUE)
    bg <- if (isTRUE(all.equal(total, 100))) "#28a745" else "#dc3545"
    tagList(rows, hr(),
      div(style = "display:flex; align-items:center; gap:10px; flex-wrap:wrap;",
        tags$span(strong(i18n_r()$t("Total:")), " ",
          span(paste0(round(total, 1), "%"), style = paste0("background:", bg, "; color:white; font-weight:bold; padding:2px 8px; border-radius:4px;"))),
        tags$span(style = "border-left:1px solid #ccc; align-self:stretch;"),
        shinyjs::disabled(actionButton(ns("matrix_equalize"), i18n_r()$t("Equalize"), icon = icon("equals"), class = "btn btn-success btn-xs", style = "color:white;"))))
  })
  outputOptions(output, "matrix_comp_ui", suspendWhenHidden = FALSE)
  # Per-row observers (resolve / remove / wt), created once per row id and destroyed when the row is gone.
  observe({ ks <- names(matrix_comps())
    for (old in setdiff(names(.cdm_obs), ks)) if (!is.null(.cdm_obs[[old]])) {
      lapply(.cdm_obs[[old]], function(o) try(o$destroy(), silent = TRUE)); .cdm_obs[[old]] <- NULL }
    for (k in ks) if (is.null(.cdm_obs[[k]])) local({ kk <- k
      .cdm_obs[[kk]] <<- list(
        res = observeEvent(input[[paste0("mres_", kk)]], .resolve_matrix_row(kk), ignoreInit = TRUE, ignoreNULL = TRUE),
        rm  = observeEvent(input[[paste0("mrm_", kk)]],
          { cur <- matrix_comps(); cur[[kk]] <- NULL; matrix_comps(cur) }, ignoreInit = TRUE, ignoreNULL = TRUE),
        wt  = observeEvent(input[[paste0("mwt_", kk)]],
          { cur <- matrix_comps(); if (!is.null(cur[[kk]]) && isTRUE(cur[[kk]]$resolved)) {
              v <- input[[paste0("mwt_", kk)]]; cur[[kk]]$wt <- if (is.null(v) || is.na(v)) NA else as.numeric(v); matrix_comps(cur) } },
          ignoreInit = TRUE, ignoreNULL = FALSE)) }) })
  # Enable each unresolved row's "Resolve & add" once at least one of its Name/CAS/SMILES fields is filled.
  observe({ cs <- matrix_comps()
    for (k in names(cs)) if (!isTRUE(cs[[k]]$resolved)) {
      filled <- any(nzchar(trimws(c(input[[paste0("mnm_", k)]] %||% "", input[[paste0("mcas_", k)]] %||% "", input[[paste0("msmi_", k)]] %||% ""))))
      shinyjs::toggleState(paste0("mres_", k), condition = isTRUE(filled)) } })
  # Equalize wt across resolved components; enabled once there are >= 2 of them.
  observe(shinyjs::toggleState("matrix_equalize",
    condition = sum(vapply(matrix_comps(), function(c) isTRUE(c$resolved), logical(1))) >= 2))
  observeEvent(input$matrix_equalize, {
    cs <- matrix_comps(); rk <- names(cs)[vapply(cs, function(c) isTRUE(c$resolved), logical(1))]
    if (length(rk) < 1) return(); eq <- round(100 / length(rk), 1)
    for (k in rk) { cs[[k]]$wt <- eq; updateNumericInput(session, paste0("mwt_", k), value = eq) }
    matrix_comps(cs) })
  # Save is enabled once a name is given AND at least one component is resolved.
  observe(shinyjs::toggleState("matrix_save", condition = nzchar(input$matrix_name %||% "") &&
    any(vapply(matrix_comps(), function(c) isTRUE(c$resolved), logical(1)))))
  observeEvent(input$matrix_save, {
    cs <- matrix_comps(); rk <- names(cs)[vapply(cs, function(c) isTRUE(c$resolved), logical(1))]
    comps <- lapply(rk, function(k) { cc <- cs[[k]]
      nmv <- trimws(input[[paste0("mnm_", k)]] %||% cc$name); v <- input[[paste0("mwt_", k)]]
      list(name = if (nzchar(nmv)) nmv else cc$name, cas = cc$cas %||% "", smiles = cc$smiles %||% "",
        wt = if (is.null(v) || is.na(v)) { sv <- suppressWarnings(as.numeric(cc$wt)); if (isTRUE(is.finite(sv))) sv else 0 } else as.numeric(v)) })
    ms <- codep_matrices(); ms[[length(ms) + 1]] <- list(name = trimws(input$matrix_name), components = comps)
    codep_matrices(ms); save_codep_matrices()
    showNotification(sprintf(i18n$t("Saved co-deposition matrix '%s'."), trimws(input$matrix_name)), type = "message")
    showModal(codep_modal())                                   # return to the D₀ estimator (matrix now selectable)
  })

  # ---- Co-deposition: auto-estimate the reference partition D0 (FastSigma) --------------------------
  # Modal: pick an oil/matrix surrogate (a preset, a saved matrix, or a custom SMILES) for the bulk deposit
  # phase, then estimate each solute's D0 = gamma(CO2)/gamma(oil) at the extraction temperature via
  # COSMO-SAC-dsp on FastSigma sigma profiles. Fills the per-solute D0 inputs in the loadings grid.
  codep_modal <- function() {
    presets <- c(
      stats::setNames("CCCCCCCCC=CCCCCCCCC(=O)O",  i18n_r()$t("Oleic acid (liquid-oil surrogate)")),
      stats::setNames("CCCCCC=CCC=CCCCCCCCC(=O)O", i18n_r()$t("Linoleic acid")),
      stats::setNames("CCCCCCCCCCCCCCCC(=O)O",     i18n_r()$t("Palmitic acid")))
    mats <- codep_matrices()
    # Saved matrices are represented by their database NAME only (their composition is shown as a footnote
    # below, reactive to the selection). Kept under a "Saved matrices" optgroup to separate them from the
    # presets; the presets themselves are listed directly (no "Preset oils" group header).
    mat_choices <- if (length(mats)) stats::setNames(paste0("matrix:", seq_along(mats)),
      vapply(mats, function(m) m$name %||% i18n_r()$t("(unnamed matrix)"), character(1))) else NULL
    choices <- as.list(presets)
    if (!is.null(mat_choices)) choices[[i18n_r()$t("Saved matrices")]] <- mat_choices
    modalDialog(
      title = tagList(icon("flask"), " ", i18n_r()$t("Auto-estimate co-deposition partition D₀")),
      size = "m", easyClose = TRUE,
      div(class = "alert alert-info", style = "font-size:0.9em;",
        i18n_r()$t("Estimates each solute's reference partition D₀ = γ(CO₂)/γ(oil) between the CO₂ phase and the bulk (oil) deposit from FastSigma σ-profiles — structure only, no calibration. D₀ > 1 means the solute is pulled into the deposit. The pressure/density dependence is added afterwards from each solute's own solubility curve.")),
      # Surrogate selector + an "Add new matrix" button, separated by a vertical rule. A spacer label above
      # the button aligns it with the select INPUT box (which carries its own label above).
      div(style = "display:flex; gap:8px; align-items:flex-end;",
        div(style = "flex:1;", selectInput(ns("codep_matrix"), i18n_r()$t("Bulk deposit (oil/matrix) surrogate"),
          choices = choices, selected = unname(presets[1]), width = "100%") |> tagAppendAttributes(style = "margin-bottom:0;")),
        tags$span(style = "border-left:1px solid #ccc; align-self:stretch;"),
        div(style = "flex:0 0 auto;",
          tags$label(HTML("&nbsp;"), class = "control-label", style = "display:block; margin-bottom:5px;"),
          actionButton(ns("codep_add_matrix"), i18n_r()$t("Add new matrix"), icon = icon("plus"),
            class = "btn btn-success", style = "color:white;"))),
      # Composition footnote (where the "dominant component" note used to be) — reactive to the selection.
      div(style = "margin-top:6px;", uiOutput(ns("codep_matrix_footnote"))),
      div(style = "font-size:0.85em; color:#666; margin-top:6px;",
        i18n_r()$t("Only solutes with a SMILES can be estimated; others keep whatever value you typed.")),
      footer = tagList(modalButton(i18n_r()$t("Cancel")),
        actionButton(ns("codep_estimate_go"), i18n_r()$t("Estimate & apply"),
          icon = icon("bolt"), class = "btn-primary")))
  }
  observeEvent(input$open_codep_modal, showModal(codep_modal()))
  # Footnote under the surrogate selector: a saved matrix -> its composition; a preset -> a one-liner.
  output$codep_matrix_footnote <- renderUI({
    sel <- input$codep_matrix %||% ""
    if (grepl("^matrix:", sel)) {
      i <- suppressWarnings(as.integer(sub("^matrix:", "", sel))); m <- tryCatch(codep_matrices()[[i]], error = function(e) NULL)
      if (is.null(m) || !length(m$components))
        return(tags$small(class = "text-muted", i18n_r()$t("This matrix has no components.")))
      txt <- paste(vapply(m$components, function(c) sprintf("%s %.3g%%", c$name %||% "?", suppressWarnings(as.numeric(c$wt %||% 0))), character(1)), collapse = ", ")
      tags$small(class = "text-muted", HTML(paste0("<strong>", i18n_r()$t("Composition:"), "</strong> ", txt,
        "<br>", i18n_r()$t("Represented by its dominant (highest-wt%) component for this structure-only estimate."))))
    } else tags$small(class = "text-muted", i18n_r()$t("A preset single-component oil surrogate."))
  })
  outputOptions(output, "codep_matrix_footnote", suspendWhenHidden = FALSE)
  # Resolve the selected surrogate to a SMILES (preset SMILES verbatim; a saved matrix -> its dominant comp).
  codep_surrogate_smiles <- function() {
    sel <- input$codep_matrix %||% "CCCCCCCCC=CCCCCCCCC(=O)O"
    if (grepl("^matrix:", sel)) { i <- suppressWarnings(as.integer(sub("^matrix:", "", sel)))
      m <- codep_matrices()[[i]]; if (is.null(m) || !length(m$components)) return("CCCCCCCCC=CCCCCCCCC(=O)O")
      wts <- vapply(m$components, function(c) { v <- suppressWarnings(as.numeric(c$wt %||% 0)); if (is.finite(v)) v else 0 }, numeric(1))
      sm <- m$components[[which.max(wts)]]$smiles %||% ""; if (nzchar(sm)) sm else "CCCCCCCCC=CCCCCCCCC(=O)O"
    } else sel
  }

  observeEvent(input$codep_estimate_go, {
    df <- combined_solutes()
    if (is.null(df) || !nrow(df)) { showNotification(i18n$t("Add at least one solute first."), type = "warning"); return() }
    oil <- codep_surrogate_smiles()
    co2 <- "O=C=O"                                                   # the named CHAOS CO2 entry (NOT the
    # equivalent SMILES "C(=O)=O", which mis-resolves to a degenerate FastSigma-predicted profile)
    Tc <- suppressWarnings(as.numeric(input$extract_temp %||% 40))[1]
    Tk <- (if (isTRUE(is.finite(Tc))) Tc else 40) + 273.15
    has_smi <- "smiles" %in% names(df)
    n_ok <- 0L; n_try <- 0L
    withProgress(message = i18n$t("Estimating partitions (FastSigma)…"), value = 0, {
      for (i in seq_len(nrow(df))) {
        incProgress(1 / nrow(df))
        smi <- if (has_smi) df$smiles[i] else NA
        if (is.null(smi) || is.na(smi) || !nzchar(smi)) next
        n_try <- n_try + 1L
        gc <- suppressWarnings(tryCatch(csac_gamma(smi, co2, T = Tk, database = "chaos", predict = TRUE)[1], error = function(e) NA_real_))
        go <- suppressWarnings(tryCatch(csac_gamma(smi, oil, T = Tk, database = "chaos", predict = TRUE)[1], error = function(e) NA_real_))
        if (!isTRUE(is.finite(gc)) || !isTRUE(is.finite(go)) || go <= 0) next
        updateNumericInput(session, paste0("codep_", gsub("[^A-Za-z0-9]", "_", df$name[i])), value = signif(gc / go, 4))
        n_ok <- n_ok + 1L
      }
    })
    updateCheckboxInput(session, "codep_enable", value = TRUE)
    removeModal()
    if (n_ok > 0) showNotification(sprintf(i18n$t("Estimated D₀ for %d of %d solute(s) with a SMILES."), n_ok, n_try), type = "message", duration = 5)
    else showNotification(i18n$t("Could not estimate D₀ for any solute (no parseable SMILES, or FastSigma failed)."), type = "warning")
  })

  # Delete a solute (by name) from the working set — removes it from the session selection and/or the
  # database-added list, wherever it lives.
  observeEvent(input$del_solute_name, {
    nm <- input$del_solute_name; if (is.null(nm) || !nzchar(nm)) return()
    key <- tolower(trimws(nm))
    ids <- rv$selected_saved_calc_ids
    keep <- ids[vapply(ids, function(id) !identical(tolower(trimws(calc_name_of(id))), key), logical(1))]
    if (length(keep) != length(ids)) {
      rv$selected_saved_calc_ids <- keep
      updateCheckboxGroupInput(session, "saved_calculation_checkboxes", selected = keep)
    }
    if (!is.null(rv$extra_solutes)) {
      ex2 <- rv$extra_solutes[tolower(trimws(rv$extra_solutes$name)) != key, , drop = FALSE]
      if (nrow(ex2) != nrow(rv$extra_solutes)) rv$extra_solutes <- if (nrow(ex2)) ex2 else NULL
    }
  })

  # ---- Automatic separator optimization -------------------------------------
  # For each solute, the HIGHEST separator pressure (searched at each candidate temperature) that still
  # recovers >= `target` of the solute from the CO2 stream. Single-stage mass-balance recovery:
  #   recovery(P,T) = 1 - loading(P,T)/loading(extract),   loading = y/(1-y)  (density cancels).
  # Highest pressure is preferred (least depressurization). Reproduces the target-only analysis
  # (e.g. thymoquinone -> ~90 bar). Returns list(name,T,P,rec) per solute (NULL entries dropped).
  optimize_separator <- function(sols, mdl, eos, ext_T, ext_P, T_grid, target = 0.90) {
    if (is.null(sols) || !nrow(sols)) return(list())
    P_hi <- max(70, min(ext_P - 5, 400)); P_grid <- seq(60, P_hi, by = 2)
    loading <- function(y) { y <- suppressWarnings(as.numeric(y)); ifelse(is.finite(y) & y > 0 & y < 1, y / (1 - y), NA_real_) }
    out <- lapply(seq_len(nrow(sols)), function(j) {
      s <- sols[j, , drop = FALSE]
      L_ext <- loading(model_solub(s, ext_T, ext_P, mdl, eos)[1])
      if (!isTRUE(is.finite(L_ext)) || L_ext <= 0) return(NULL)
      best <- NULL
      for (Tc in T_grid) {
        y <- model_solub(s, rep(Tc, length(P_grid)), P_grid, mdl, eos)
        rec <- 1 - loading(y) / L_ext
        ok <- which(is.finite(rec) & rec >= target)
        if (length(ok)) {
          i <- ok[which.max(P_grid[ok])]
          cand <- list(name = s$name, T = Tc, P = P_grid[i], rec = rec[i])
          if (is.null(best) || cand$P > best$P) best <- cand
        }
      }
      best
    })
    Filter(Negate(is.null), out)
  }

  # ---- Run ------------------------------------------------------------------
  observeEvent(input$run_cascade, {
    sols <- combined_solutes()
    if (is.null(sols) || !nrow(sols)) { showNotification(i18n$t("Add at least one solute first."), type = "warning"); return() }
    temps <- parse_num(input$curve_temps); pres <- parse_num(input$curve_pres)
    if (!length(temps) || !length(pres)) { showNotification(i18n$t("Enter at least one temperature and one pressure."), type = "warning"); return() }
    eos <- input$eos %||% "spanwagner"

    # ---- Range validation (mirrors the badge limits; blocks the run when any value is out of range,
    #      as in the other modules). Separator fields are skipped when their value is auto-optimized. ----
    val_errs <- character(0)
    chk1 <- function(v, lo, hi, lab) { x <- suppressWarnings(as.numeric(v))[1]
      if (!isTRUE(is.finite(x)) || x < lo || x > hi) val_errs <<- c(val_errs, i18n$t(lab)) }
    chkN <- function(v, lo, hi, lab) { x <- parse_num(v)
      if (length(x) && any(x < lo | x > hi)) val_errs <<- c(val_errs, i18n$t(lab)) }
    chk1(input$extract_temp, 32, 200, "Extraction T (°C)")
    chk1(input$extract_pres, 60, 1000, "Extraction P (bar)")
    if (!isTRUE(input$optimize_pres)) chkN(input$sep_pres, 1, 200, "Separator pressures (bar)")
    if (!isTRUE(input$optimize_temp)) chkN(input$sep_temp, -60, 200, "Separator temperatures (°C)")
    if (length(val_errs)) {
      showNotification(paste(i18n$t("Out of range:"), paste(val_errs, collapse = ", ")), type = "error"); return()
    }

    # Separator pressures must not INCREASE down the cascade: the CO₂ stream is depressurised stage by
    # stage, so each separator must sit at or below the previous one. Skipped when auto-optimizing pressure.
    if (!isTRUE(input$optimize_pres)) {
      .sp <- parse_num(input$sep_pres)
      if (length(.sp) >= 2 && any(diff(.sp) > 0)) {
        showNotification(i18n$t("Separator pressures must be entered in non-increasing order — each stage at or below the previous stage's pressure."), type = "error"); return()
      }
    }

    # Extraction pressure must be >= the highest separator pressure (the stream is depressurised through
    # the separators). Skipped when the separator pressure is auto-optimized.
    .sep_p_chk <- parse_num(input$sep_pres); .ext_P_chk <- suppressWarnings(as.numeric(input$extract_pres %||% 350))[1]
    if (!isTRUE(input$optimize_pres) && length(.sep_p_chk) && isTRUE(is.finite(.ext_P_chk)) && .ext_P_chk < max(.sep_p_chk)) {
      showNotification(sprintf(i18n$t("Extraction pressure (%g bar) cannot be below the highest separator pressure (%g bar)."),
                               .ext_P_chk, max(.sep_p_chk)), type = "error"); return()
    }
    # CO2 flow for the mass balance — the canonical kg/h value (kept in sync with the unit toggle).
    co2_flow_kgh <- { v <- co2_flow_kgh_rv(); if (isTRUE(is.finite(v)) && v > 0) v else (defaults$co2_flow %||% 3) }

    mdl <- cur_model()
    # A co-solvent only modifies the Makarov (ML) and CSP models; warn if one is set under Chrastil/experimental.
    if (!is.null(cosolv_sel()) && mdl %in% c("chrastil", "experimental"))
      showNotification(i18n$t("The selected co-solvent is ignored by the Chrastil/experimental model (only Makarov and CSP-2021 account for it)."), type = "warning", duration = 5)
    # Restrict to solutes the chosen model can place (belt-and-suspenders to the live checkers). For ocrsphi,
    # "placeable" is temperature-dependent — a solid with no fusion data can't be computed below its melting
    # point — so judge against the HOTTEST temperature this run will evaluate (curve temps, extraction T, and
    # the separator/optimiser T). This mirrors the engine keeping a solute whenever ANY (T,P) grid point is
    # finite, so we never drop one that still yields a usable curve at some requested temperature; a solute
    # that can't be placed at the cascade conditions is reported separately at the cascade step below.
    elig_sepT <- if (isTRUE(input$optimize_temp)) 70 else parse_num(input$sep_temp)
    elig_T <- suppressWarnings(max(c(temps, suppressWarnings(as.numeric(input$extract_temp %||% 40))[1], elig_sepT), na.rm = TRUE))
    if (!is.finite(elig_T)) elig_T <- 40
    elig <- vapply(seq_len(nrow(sols)), function(j)
      solute_eligible(sols$name[j], sols$smiles[j], sols$k[j], sols$A[j], sols$B[j], mdl, elig_T), logical(1))
    if (!any(elig)) {
      # Give the specific openCOSMO-RS-Phi reason (not in bundle vs high-melting solid without fusion data)
      # when that is why nothing is eligible — otherwise the generic message.
      msg <- i18n$t("None of the selected solutes are eligible for the chosen solubility model.")
      if (identical(mdl, "ocrsphi")) {
        om <- ocrsphi_fitted_meta()
        nf <- vapply(seq_len(nrow(sols)), function(j) identical(ocrsphi_compute_status(sols$name[j], sols$smiles[j], elig_T, om)$code, "solid_no_fusion"), logical(1))
        if (all(nf)) msg <- i18n$t("The selected solute(s) are high-melting solids with no fusion data in openCOSMO-RS-Phi, so their solubility cannot be computed — switch to the Chrastil or CSP-2021 model.")
      }
      showNotification(msg, type = "warning"); return()
    }
    if (any(!elig)) {
      skipped <- sols$name[!elig]
      if (identical(mdl, "ocrsphi")) {
        # Split the skipped ocrsphi solutes into "high-melting solid, no fusion data" (with melting point) vs
        # "not in the database", and recommend a model that can handle them.
        om <- ocrsphi_fitted_meta()
        st <- lapply(which(!elig), function(j) ocrsphi_compute_status(sols$name[j], sols$smiles[j], elig_T, om))
        is_nf <- vapply(st, function(s) identical(s$code, "solid_no_fusion"), logical(1))
        nf_lab <- vapply(which(is_nf), function(ii) { s <- st[[ii]]
          sprintf("%s (%s %g °C)", skipped[ii], i18n$t("melts"), round(s$Tm_C)) }, character(1))
        if (length(nf_lab)) showNotification(sprintf(i18n$t("Solid with no fusion data in openCOSMO-RS-Phi — solubility cannot be computed (switch to Chrastil or CSP-2021): %s"), paste(nf_lab, collapse = ", ")), type = "warning", duration = 10)
        if (any(!is_nf)) showNotification(sprintf(i18n$t("Not in the openCOSMO-RS-Phi database (switch to Chrastil or CSP-2021): %s"), paste(skipped[!is_nf], collapse = ", ")), type = "warning", duration = 10)
      } else showNotification(sprintf(i18n$t("Skipped (not eligible for this model): %s"), paste(skipped, collapse = ", ")), type = "warning")
    }
    sols <- sols[elig, , drop = FALSE]

    # Warn (but still compute) when a co-solvent BLEND does not form ONE homogeneous entrainer phase at the
    # selected proportions and extraction temperature. Composition-/T-aware via the COSMO-SAC-dsp flash
    # (blend_phase_check); reactions come from the curated table.
    cs_run <- cosolv_sel()
    if (!is.null(cs_run) && length(cs_run$name) > 1) {
      Tc0 <- suppressWarnings(as.numeric(input$extract_temp)); if (!isTRUE(is.finite(Tc0))) Tc0 <- 40
      chk <- tryCatch(blend_phase_check(cs_run$name, x = cs_run$x, T = Tc0 + 273.15), error = function(e) NULL)
      if (!is.null(chk) && (isTRUE(chk$reaction) || isFALSE(chk$single_phase))) {
        nm <- paste(cs_run$name, collapse = " + ")
        showNotification(
          if (isTRUE(chk$reaction)) sprintf(i18n$t("Co-solvent blend '%s': its solvents may react or interact on contact; results are shown but treat them with caution."), nm)
          else sprintf(i18n$t("Co-solvent blend '%s': at the selected ratio and %s °C its solvents split into two liquid phases (not a single entrainer); results are shown but the blend may not be physical."), nm, round(Tc0)),
          type = "warning", duration = 9) }
    }

    withProgress(message = i18n$t("Computing solubility..."), {
      # Solubility curves per solute (model-routed); S_gL/g-kg derived from y + CO2 density.
      grid <- expand.grid(pres_bar = pres, temp_C = temps)
      curves <- list()
      for (j in seq_len(nrow(sols))) {
        s <- sols[j, ]
        y <- model_solub(s, grid$temp_C, grid$pres_bar, mdl, eos)
        if (all(!is.finite(y))) next
        rho <- mapply(function(P, Tc) unname(co2dens(P, Tc, "g/L", eos)[["rho"]]), grid$pres_bar, grid$temp_C)
        S_gL <- if (isTRUE(is.finite(s$mw))) (y / (1 - y)) * s$mw * rho / 44.01 else rep(NA_real_, length(y))
        curves[[length(curves) + 1]] <- data.frame(solute = s$name, temp_C = grid$temp_C, pres_bar = grid$pres_bar,
          rho = rho, y_molmol = y, S_gL = S_gL, mw = s$mw, stringsAsFactors = FALSE)
      }
      rv$sol_curves <- if (length(curves)) do.call(rbind, curves) else NULL

      opt_P <- isTRUE(input$optimize_pres); opt_T <- isTRUE(input$optimize_temp)
      ext_T <- suppressWarnings(as.numeric(input$extract_temp %||% 40))[1]
      ext_P <- suppressWarnings(as.numeric(input$extract_pres %||% 350))[1]
      rec_target <- { v <- suppressWarnings(as.numeric(input$recovery_target %||% 90))[1]
        if (isTRUE(is.finite(v)) && v > 0 && v <= 100) v / 100 else 0.90 }
      user_T <- parse_num(input$sep_temp); user_T <- if (length(user_T)) user_T[1] else ext_T
      # Single-temperature (user's separator T) or optimise over a temperature grid.
      T_search <- if (opt_T) seq(30, 70, by = 5) else user_T
      # Recovery pressure: the lowest separator pressure allowed. The recovery search only goes down to it,
      # so a target needing a lower pressure is (correctly) reported as not reached.
      rec_P <- recovery_pressure_r()
      usable0 <- sols[is.finite(sols$mw), , drop = FALSE]

      # Per-solute feed loading in g/kg CO2 (user input; blank -> saturation at the extraction conditions).
      feed_of <- function(s) {
        v <- loading_of(s$name); if (isTRUE(is.finite(v))) return(v)
        solub_gkg(model_solub(s, ext_T, ext_P, mdl, eos)[1], s$mw)
      }

      # ---- Target-fractionation optimisation: per solute, the precipitation ONSET + the recovery-target
      #      pressure at its feed loading (mass basis), optimised over temperature. ----
      rv$optim <- NULL
      if (nrow(usable0)) {
        opt <- lapply(seq_len(nrow(usable0)), function(j) {
          s <- usable0[j, , drop = FALSE]; feed <- feed_of(s)
          if (!isTRUE(is.finite(feed)) || feed <= 0) return(NULL)
          sfun <- function(T, P) solub_gkg(model_solub(s, rep(T, length(P)), P, mdl, eos), s$mw)
          d <- tryCatch(supeRcrit:::sfe_frac(sfun, feed = feed, sep_T = T_search, P_lo = rec_P,
                    P_hi = max(rec_P + 5, min(ext_P - 5, 200)), recovery_target = rec_target),
                    error = function(e) NULL)
          if (is.null(d)) return(NULL)
          list(name = s$name, feed = feed, onset_P = d$onset_P, onset_T = d$onset_T,
               target_P = d$target_P, target_T = d$target_T, target_recovery = d$target_recovery,
               onset_at_target_T = d$onset_at_target_T, curve = d$curve)
        })
        opt <- Filter(Negate(is.null), opt)
        if (length(opt)) rv$optim <- list(results = opt, recovery_target = rec_target, opt_T = opt_T)
        # Warn about physically implausible feed loadings (> 1000 g/kg CO2), and about solutes whose target
        # recovery cannot be reached in range — but still DISPLAY the results (best achievable) rather than
        # dropping them.
        over <- vapply(usable0$name, function(nm) { v <- loading_of(nm); isTRUE(is.finite(v)) && v > 1000 }, logical(1))
        if (any(over)) showNotification(sprintf(i18n$t("Feed loading exceeds 1000 g/kg CO₂ for: %s (physically implausible — please check)."),
          paste(usable0$name[over], collapse = ", ")), type = "warning")
        if (length(opt)) {
          unmet <- vapply(opt, function(o) !isTRUE(is.finite(o$target_P)), logical(1))
          if (any(unmet)) showNotification(sprintf(i18n$t("Target recovery %d%% not reached in range for: %s. Showing the best achievable."),
            round(100 * rec_target), paste(vapply(opt[unmet], function(o) o$name, character(1)), collapse = ", ")), type = "warning")
        }
      }

      # Cascade stages: user-specified, or (when optimising pressure) the per-solute recovery-target
      # pressures. When optimising, produce EXACTLY n_stages separators as a decreasing pressure sequence
      # from the highest precipitation onset down to the deepest recovery-target pressure (a stepped
      # depressurisation cascade); manual mode keeps at most n_stages entered pressures. Temperature:
      # single (user's) or the optimiser's mean target temperature.
      n_stages <- n_stages_r()
      if (opt_P && !is.null(rv$optim)) {
        onsets <- vapply(rv$optim$results, function(o) o$onset_P, numeric(1))
        targs  <- vapply(rv$optim$results, function(o) o$target_P, numeric(1))
        p_hi <- suppressWarnings(max(onsets[is.finite(onsets)]))
        p_lo <- suppressWarnings(min(targs[is.finite(targs)]))
        if (!is.finite(p_hi)) p_hi <- min(ext_P - 5, 200)
        # Deepest stage: the target pressure when reachable, else the recovery-pressure floor (go as low as
        # allowed to capture the most). Never below the floor.
        if (!is.finite(p_lo)) p_lo <- rec_P
        p_lo <- max(p_lo, rec_P); p_hi <- max(p_hi, p_lo)
        # >=2 stages: step from the onset down to the deepest allowed pressure. A single stage sits at that
        # deep pressure so it actually collects the solute (the onset alone recovers ~0%).
        sep_p <- if (n_stages <= 1) round(p_lo) else unique(round(seq(p_hi, p_lo, length.out = n_stages)))
        st_T <- if (opt_T) { tt <- vapply(rv$optim$results, function(o) o$target_T, numeric(1))
          tt <- tt[is.finite(tt)]; if (length(tt)) round(mean(tt)) else user_T } else user_T
        sep_t <- rep(st_T, length(sep_p))
      } else {
        # Manual: at most n_stages pressures, none below the recovery-pressure floor (clamped up, notify).
        sep_p <- utils::head(parse_num(input$sep_pres), n_stages)
        if (length(sep_p) && any(sep_p < rec_P)) {
          showNotification(sprintf(i18n$t("Separator pressures below the recovery pressure (%g bar) were raised to it."), rec_P), type = "warning")
          sep_p <- pmax(sep_p, rec_P)
        }
        sep_t <- parse_num(input$sep_temp)
      }
      if (length(sep_p) > 0 && length(sep_t) > 0 && any(is.finite(sols$mw))) {
        if (length(sep_t) == 1) sep_t <- rep(sep_t, length(sep_p))
        if (length(sep_t) != length(sep_p)) sep_t <- rep(sep_t[1], length(sep_p))
        stages <- data.frame(temp_C = sep_t, pres_bar = sep_p)
        usable <- sols[is.finite(sols$mw), , drop = FALSE]; dropped <- sols$name[!is.finite(sols$mw)]
        nS <- nrow(stages); ymat <- matrix(NA_real_, nrow(usable), nS)
        # Per-solute feed MASS (kg/h) from the loading (g/kg CO2) x CO2 flow — the dilute, direct-loading feed.
        feed_kgh <- vapply(seq_len(nrow(usable)), function(j)
          feed_of(usable[j, , drop = FALSE]) * co2_flow_kgh / 1000, numeric(1))
        for (j in seq_len(nrow(usable))) ymat[j, ] <- model_solub(usable[j, ], stages$temp_C, stages$pres_bar, mdl, eos)
        okrow <- apply(ymat, 1, function(r) all(is.finite(r))) & is.finite(feed_kgh) & feed_kgh > 0
        # Report solutes dropped from the cascade. For openCOSMO-RS-Phi, a solid with no fusion data below its
        # melting point can't be placed (feed/stage solubility is NA) — diagnose at the COLDEST cascade
        # temperature (where NA arises) and recommend Chrastil/CSP-2021; everything else keeps the generic
        # wording. Shared by the normal and the all-NA branches so the specific reason shows even when NO
        # solute is placeable (e.g. a single solid solute below its melting point).
        report_dropped <- function(na_names, na_smiles, no_mw_names) {
          ocr_nf <- character(0); rest <- na_names
          if (identical(mdl, "ocrsphi") && length(na_names)) {
            om <- ocrsphi_fitted_meta()
            diag_T <- suppressWarnings(min(c(ext_T, stages$temp_C), na.rm = TRUE)); if (!is.finite(diag_T)) diag_T <- ext_T
            is_nf <- vapply(seq_along(na_names), function(j) identical(ocrsphi_compute_status(na_names[j], na_smiles[j], diag_T, om)$code, "solid_no_fusion"), logical(1))
            ocr_nf <- na_names[is_nf]; rest <- na_names[!is_nf]
          }
          if (length(ocr_nf)) showNotification(sprintf(i18n$t("Not placed in the cascade — solid below its melting point with no fusion data in openCOSMO-RS-Phi, so its solubility cannot be computed (use Chrastil or CSP-2021): %s"), paste(ocr_nf, collapse = ", ")), type = "warning", duration = 10)
          other <- c(no_mw_names, rest)
          if (length(other)) showNotification(sprintf(i18n$t("Excluded from cascade (no molar mass or solubility): %s"), paste(other, collapse = ", ")), type = "warning")
        }
        if (any(okrow)) {
          sub <- usable[okrow, , drop = FALSE]
          # ---- Optional co-deposition coupling: per-solute D0 (input) + reference solubility (at the
          #      extraction condition, where D0 is defined). The engine adds the density trend from ysolub. ----
          codeposit <- NULL
          if (isTRUE(input$codep_enable)) {
            Dref_vec <- vapply(sub$name, function(nm) { v <- codep_of(nm); if (isTRUE(is.finite(v)) && v > 0) v else NA_real_ }, numeric(1))
            if (any(is.finite(Dref_vec))) {
              yref_vec <- vapply(seq_len(nrow(sub)), function(j)
                suppressWarnings(model_solub(sub[j, , drop = FALSE], ext_T, ext_P, mdl, eos)[1]), numeric(1))
              codeposit <- list(Dref = unname(Dref_vec), yref = unname(yref_vec))
            } else {
              showNotification(i18n$t("Co-deposition is on but no solute has a valid D₀ — enter or auto-estimate D₀ first."), type = "warning", duration = 5)
            }
          }
          casc <- tryCatch(supeRcrit:::solub_cascade(
            solutes = data.frame(name = sub$name, mw = sub$mw, feed = feed_kgh[okrow], stringsAsFactors = FALSE),
            stages = stages, ysolub = ymat[okrow, , drop = FALSE], co2_flow = co2_flow_kgh, codeposit = codeposit),
            error = function(e) { showNotification(paste(i18n$t("Cascade failed:"), e$message), type = "error"); NULL })
          rv$cascade <- casc
          if (!is.null(casc)) {
            report_dropped(usable$name[!okrow], usable$smiles[!okrow], dropped)
            # Reliability note: solutes computed within ~10 °C below their melting point are unreliable (the
            # ocrsphi solid fixed point degenerates near Tm). They still compute (finite), so just flag them —
            # checked across ALL cascade temperatures, since a separator stage can run hotter than extraction
            # (and thus closer to the melting point) than the feed condition.
            if (identical(mdl, "ocrsphi") && nrow(sub)) {
              om <- ocrsphi_fitted_meta(); casc_Ts <- c(ext_T, stages$temp_C)
              nearm <- vapply(seq_len(nrow(sub)), function(j)
                any(vapply(casc_Ts, function(tt) identical(ocrsphi_compute_status(sub$name[j], sub$smiles[j], tt, om)$code, "near_melting"), logical(1))), logical(1))
              if (any(nearm)) showNotification(sprintf(i18n$t("Near its melting point, the openCOSMO-RS-Phi solubility can be unreliable for: %s. Consider the Chrastil model."), paste(sub$name[nearm], collapse = ", ")), type = "warning", duration = 10)
            }
          }
        } else {
          rv$cascade <- NULL
          # No solute could be placed at the cascade conditions — give the specific per-solute reason (e.g. a
          # single solid solute below its melting point on ocrsphi) instead of only a generic note.
          report_dropped(usable$name, usable$smiles, dropped)
        }
      } else {
        rv$cascade <- NULL
        if (!any(is.finite(sols$mw))) showNotification(i18n$t("Cascade needs molar masses; none of the selected solutes have one."), type = "warning")
      }
    })
  })

  output$has_results <- reactive({ !is.null(rv$sol_curves) }); outputOptions(output, "has_results", suspendWhenHidden = FALSE)
  output$has_cascade <- reactive({ !is.null(rv$cascade) }); outputOptions(output, "has_cascade", suspendWhenHidden = FALSE)

  # Result tabs stay disabled (all but the first) until results exist — same pattern as other modules.
  observe({
    if (!is.null(rv$sol_curves)) session$sendCustomMessage("enableTabs", list(tabsetId = ns("results_tabs")))
    else session$sendCustomMessage("disableTabs", list(tabsetId = ns("results_tabs"), keepFirst = TRUE))
  })

  # ---- Solubility outputs ---------------------------------------------------
  solubility_ggplot <- reactive({
    req(rv$sol_curves)
    d <- rv$sol_curves; basis <- input$curve_basis %||% "g/L"
    d$value <- switch(basis, "g/L" = d$S_gL, "g/kg" = solub_gkg(d$y_molmol, d$mw), d$y_molmol)
    d <- d[is.finite(d$value), , drop = FALSE]; req(nrow(d) > 0)
    d$series <- if (length(unique(d$temp_C)) > 1) paste0(d$solute, " @ ", d$temp_C, "°C") else d$solute
    ylab <- switch(basis, "g/L" = i18n$t("Solubility (g/L)"), "g/kg" = i18n$t("Solubility (g/kg)"), i18n$t("Solubility (mol/mol)"))
    ggplot2::ggplot(d, ggplot2::aes(x = pres_bar, y = value, colour = series)) +
      ggplot2::geom_line(linewidth = 0.9) + ggplot2::geom_point(size = 1.6) +
      ggplot2::labs(x = i18n$t("Pressure (bar)"), y = ylab, colour = NULL) +
      ggplot2::theme_bw(base_size = 15) + ggplot2::theme(legend.position = "bottom")
  })
  # Interactive (plotly) solubility plot — hover values + zoom. The static ggplot is kept for export.
  output$solubility_plot <- plotly::renderPlotly({
    plotly::layout(quiet_ggplotly(solubility_ggplot(), tooltip = c("x", "y", "colour")),
      legend = list(orientation = "h", y = -0.2))
  })

  # Units selector — server-rendered so i18n returns plain strings (the old UI-side version showed
  # "span" because usei18n makes i18n$t() return a tag). Inline "Units:" title, dropdown.
  output$curve_basis_ui <- renderUI({
    # The label=NULL selectInput still ships a .form-group bottom margin; zero it (via .cascade-units-row)
    # so vertical centring lines the "Units:" label up with the dropdown box, not its margin box.
    div(class = "cascade-units-row", style = "display:flex; align-items:center; gap:10px; flex-wrap:wrap;",
      tags$strong(paste0(i18n_r()$t("Units"), ":"), style = "font-size:14px; line-height:34px;"),
      div(style = "min-width:240px;",
        selectInput(ns("curve_basis"), label = NULL,
          choices = setNames(c("mol/mol", "g/L", "g/kg"),
            c(i18n_r()$t("Mole fraction (mol/mol)"), i18n_r()$t("Concentration (g/L)"), i18n_r()$t("Loading (g/kg)"))),
          selected = input$curve_basis %||% "g/L", width = "100%")))
  })
  outputOptions(output, "curve_basis_ui", suspendWhenHidden = FALSE)

  # Solute filter for the data table shown beneath the plot (Solubility Data merged into this tab).
  output$curve_solute_filter_ui <- renderUI({
    req(rv$sol_curves); sols <- sort(unique(rv$sol_curves$solute))
    selectInput(ns("curve_solute_filter"), tags$span(i18n_r()$t("Filter by solute")),
      choices = c(setNames("__all__", i18n_r()$t("All solutes")), setNames(sols, sols)),
      selected = "__all__", width = "260px")
  })
  outputOptions(output, "curve_solute_filter_ui", suspendWhenHidden = FALSE)

  output$solubility_table <- DT::renderDataTable({
    req(rv$sol_curves)
    d <- rv$sol_curves
    f <- input$curve_solute_filter %||% "__all__"
    if (!identical(f, "__all__")) d <- d[d$solute == f, , drop = FALSE]
    req(nrow(d) > 0)
    out <- data.frame(Solute = d$solute, `T (°C)` = d$temp_C, `P (bar)` = d$pres_bar,
      `rho (g/L)` = round(d$rho, 1), `y (mol/mol)` = signif(d$y_molmol, 4), `S (g/L)` = signif(d$S_gL, 4),
      `S (g/kg)` = signif(solub_gkg(d$y_molmol, d$mw), 4),
      check.names = FALSE)
    DT::datatable(out, rownames = FALSE, extensions = "Buttons",
      options = list(scrollX = TRUE, dom = "Bfrtip", language = tablang(),
        buttons = create_dt_export_buttons(i18n_r, "supercrit_chrastil_solubility")))
  })

  # ---- Cascade outputs ------------------------------------------------------
  # Names of the solutes with an optimization result (used by the selector + exports).
  cascade_solutes <- reactive({ op <- rv$optim; req(op, op$results); vapply(op$results, function(o) o$name, character(1)) })
  # Flag: more than one solute analysed (gates the "Export All Plots" button).
  output$has_multiple_solutes <- reactive({ !is.null(rv$optim) && length(rv$optim$results) > 1 })
  outputOptions(output, "has_multiple_solutes", suspendWhenHidden = FALSE)

  # Dynamically-translated column-header tooltip: a <span title='...'> wrapping the header (rendered with
  # escape=FALSE), the same idiom used by the Group Contributions tables in Solute Characterization.
  tip_col <- function(head, tt) sprintf("<span title='%s'>%s</span>", tt, head)

  # Solute selector for the combined per-solute figure.
  output$cascade_solute_sel_ui <- renderUI({
    nms <- cascade_solutes()
    selectInput(ns("cascade_solute_sel"), i18n_r()$t("Select solute"),
      choices = setNames(nms, nms), selected = input$cascade_solute_sel %||% nms[1], width = "100%")
  })
  outputOptions(output, "cascade_solute_sel_ui", suspendWhenHidden = FALSE)

  # Per-solute figure DATA: the cumulative-recovery design curve + per-separator capture bars (tops meet the
  # curve) on one "% of feed" axis. Shared by the on-screen plotly and the ggplot used for PDF export.
  cascade_fig_data <- function(solute_name) {
    op <- rv$optim; casc <- rv$cascade; if (is.null(op)) return(NULL)
    o <- NULL; for (r in op$results) if (identical(r$name, solute_name)) o <- r
    if (is.null(o)) return(NULL)
    Tsel <- if (isTRUE(is.finite(o$target_T))) o$target_T else o$onset_T
    cc <- o$curve[o$curve$temp_C == Tsel & is.finite(o$curve$recovery), , drop = FALSE]
    if (!nrow(cc)) return(NULL)
    cc$rec_pct <- 100 * cc$recovery
    sd <- NULL
    if (!is.null(casc) && !is.null(casc$tidy)) {
      sd <- casc$tidy[casc$tidy$solute == solute_name, , drop = FALSE]
      if (nrow(sd)) { sd <- sd[order(sd$stage), , drop = FALSE]
        sd$cum_pct <- 100 * cumsum(sd$recovery)
        # deposited mass in g/h (short label; kg/h values are tiny, e.g. 0.000559 -> 0.559 g/h)
        sd$lab <- sprintf("S%d: %.3g g/h", sd$stage, 1000 * sd$deposit_kg_h)
      } else sd <- NULL
    }
    list(o = o, Tsel = Tsel, cc = cc, sd = sd, target_pct = 100 * op$recovery_target,
         bw = max(2, diff(range(cc$pres_bar)) / 45), ymax = max(105, 100 * op$recovery_target + 6))
  }

  # On-screen figure — native plotly for full control of the vertical bar labels, the target-line label, and
  # the (understated) axis-title fonts. No title on screen (the solute selector already names it).
  output$combined_cascade_plot <- plotly::renderPlotly({
    req(rv$optim, rv$cascade)
    fd <- cascade_fig_data(input$cascade_solute_sel %||% cascade_solutes()[1]); req(!is.null(fd))
    axfont <- list(size = 13, color = "#333")
    p <- plotly::plot_ly()
    if (!is.null(fd$sd))
      p <- plotly::add_bars(p, data = fd$sd, x = ~pres_bar, y = ~cum_pct, width = fd$bw,
        marker = list(color = "rgba(158,202,225,0.9)", line = list(color = "#3182bd", width = 1)),
        name = i18n_r()$t("Per-separator capture"),
        hovertemplate = paste0("%{x:.0f} bar<br>", i18n_r()$t("Cumulative recovery"), ": %{y:.1f}%<extra></extra>"))
    p <- plotly::add_lines(p, data = fd$cc, x = ~pres_bar, y = ~rec_pct,
        line = list(color = "#e8720c", width = 2.4), name = i18n_r()$t("Recovery curve"),
        hovertemplate = paste0("%{x:.0f} bar<br>%{y:.1f}%<extra></extra>"))
    # Vertical (90°) per-stage deposit labels, sitting above each bar in a bold high-contrast navy.
    bar_annos <- if (!is.null(fd$sd)) lapply(seq_len(nrow(fd$sd)), function(i) list(
      x = fd$sd$pres_bar[i], y = fd$sd$cum_pct[i], text = fd$sd$lab[i], textangle = -90,
      showarrow = FALSE, xanchor = "center", yanchor = "bottom", yshift = 5,
      font = list(size = 11.5, color = "#08306b"))) else list()
    tgt_anno <- list(x = 0.015, xref = "paper", xanchor = "left", y = fd$target_pct, yref = "y",
      yanchor = "bottom", yshift = 2, showarrow = FALSE, font = list(size = 12.5, color = "#c0392b"),
      text = sprintf(i18n_r()$t("Target recovery (%d%%)"), round(fd$target_pct)))
    plotly::layout(p,
      xaxis = list(title = list(text = i18n_r()$t("Separator pressure (bar)"), font = axfont),
        autorange = "reversed", tickfont = list(size = 11)),
      yaxis = list(title = list(text = i18n_r()$t("Recovery (% of feed)"), font = axfont),
        range = c(0, fd$ymax), tickfont = list(size = 11)),
      shapes = list(list(type = "line", xref = "paper", x0 = 0, x1 = 1, yref = "y",
        y0 = fd$target_pct, y1 = fd$target_pct, line = list(dash = "dash", color = "#c0392b", width = 1.4))),
      annotations = c(bar_annos, list(tgt_anno)),
      legend = list(orientation = "h", x = 0, y = 1.08), margin = list(t = 30))
  })
  outputOptions(output, "combined_cascade_plot", suspendWhenHidden = FALSE)

  # ggplot version (PDF export only) — mirrors the plotly figure; here geom_text angle DOES render, and a
  # title is added (it is not shown on screen). Axis titles kept understated (size 13, not bold).
  cascade_plot_for <- function(solute_name, for_export = FALSE) {
    fd <- cascade_fig_data(solute_name); if (is.null(fd)) return(NULL)
    gg <- ggplot2::ggplot()
    if (!is.null(fd$sd))
      gg <- gg +
        ggplot2::geom_col(data = fd$sd, ggplot2::aes(x = pres_bar, y = cum_pct),
          width = fd$bw, fill = "#9ecae1", colour = "#3182bd", alpha = 0.9) +
        ggplot2::geom_text(data = fd$sd, ggplot2::aes(x = pres_bar, y = cum_pct, label = lab),
          angle = 90, hjust = 0, vjust = 0.4, nudge_y = 1, size = 3.3, fontface = "bold", colour = "#08306b")
    gg <- gg +
      ggplot2::geom_line(data = fd$cc, ggplot2::aes(x = pres_bar, y = rec_pct),
        colour = "#e8720c", linewidth = 0.9) +
      ggplot2::geom_hline(yintercept = fd$target_pct, linetype = "dashed", colour = "#c0392b") +
      ggplot2::annotate("text", x = max(fd$cc$pres_bar) - 0.03 * diff(range(fd$cc$pres_bar)),
        y = fd$target_pct + 3, hjust = 0, vjust = 0,
        label = sprintf(i18n$t("Target recovery (%d%%)"), round(fd$target_pct)), size = 3.6, colour = "#c0392b") +
      ggplot2::scale_x_reverse() +
      ggplot2::coord_cartesian(ylim = c(0, fd$ymax)) +
      ggplot2::labs(x = i18n$t("Separator pressure (bar)"), y = i18n$t("Recovery (% of feed)")) +
      ggplot2::theme_bw(base_size = 13) +
      ggplot2::theme(axis.title = ggplot2::element_text(size = 13),
        plot.title = ggplot2::element_text(size = 14, hjust = 0.5))
    if (isTRUE(for_export)) gg <- gg + ggplot2::labs(title = sprintf("%s  (%.0f °C)", solute_name, fd$Tsel))
    gg
  }

  # Precipitation onset & recovery — one row per target solute, with dynamically-translated column tooltips
  # and Copy/CSV/Excel/PDF export. The recovery-target cell shows the pressure, or a red "Not reached (…)".
  output$onset_table <- DT::renderDataTable({
    op <- rv$optim; req(op, op$results)
    pct <- round(100 * op$recovery_target)
    fmtP <- function(p) if (isTRUE(is.finite(p))) sprintf("%.0f bar", p) else "—"
    df <- do.call(rbind, lapply(op$results, function(o) {
      onsetP <- o$onset_at_target_T; if (!isTRUE(is.finite(onsetP))) onsetP <- o$onset_P
      Tuse <- if (isTRUE(is.finite(o$target_T))) o$target_T else o$onset_T
      rec_cell <- if (isTRUE(is.finite(o$target_P))) fmtP(o$target_P) else {
        # When neither target nor onset is reachable, Tuse is NA — and `curve$temp_C == NA` returns NA, which
        # would NA-index the curve and make which.max() return integer(0), yielding a zero-length cell and a
        # data.frame() "differing number of rows" error. So only subset at a FINITE Tuse; a solute that never
        # drops below its feed across the separator range (onset NA => stays dissolved) is reported plainly.
        cc <- if (isTRUE(is.finite(Tuse)))
          o$curve[o$curve$temp_C == Tuse & is.finite(o$curve$recovery), , drop = FALSE] else o$curve[0, , drop = FALSE]
        if (nrow(cc) && any(cc$recovery > 0)) { i <- which.max(cc$recovery)
          sprintf("<span style='color:#c0392b;'>%s</span>",
            sprintf(i18n_r()$t("Not reached (max %.0f%% at %.0f bar)"), 100 * cc$recovery[i], cc$pres_bar[i]))
        } else sprintf("<span style='color:#c0392b;'>%s</span>", i18n_r()$t("Does not precipitate in range")) }
      data.frame(
        Solute = sprintf("<strong>%s</strong>", o$name),
        Feed = sprintf("%.2f", o$feed),
        Onset = sprintf("<strong>%s</strong>", fmtP(onsetP)),
        Temperature = if (isTRUE(is.finite(Tuse))) sprintf("%.0f °C", Tuse) else "—",
        Recovery = rec_cell, stringsAsFactors = FALSE, check.names = FALSE)
    }))
    cn <- c(
      tip_col(i18n_r()$t("Solute"), i18n_r()$t("The target solute")),
      tip_col(i18n_r()$t("Feed (g/kg CO₂)"), i18n_r()$t("Feed loading of the solute in the CO₂ stream")),
      tip_col(i18n_r()$t("Onset"), i18n_r()$t("Highest separator pressure at which the solute begins to precipitate")),
      tip_col(i18n_r()$t("Temperature"), i18n_r()$t("Separator temperature used for this solute")),
      tip_col(sprintf(i18n_r()$t("≥%d%% recovery"), pct),
        i18n_r()$t("Highest separator pressure that still recovers the target fraction (or the best achievable if the target is not reached)")))
    DT::datatable(df, rownames = FALSE, escape = FALSE, colnames = cn, extensions = "Buttons",
      options = list(scrollX = TRUE, dom = "Bt", ordering = FALSE, language = tablang(),
        buttons = create_dt_export_buttons(i18n_r, "supercrit_onset_recovery")))
  })

  # Recovery per separator stage (%), with column tooltips and Copy/CSV/Excel/PDF export. Columns are the
  # separators (labelled with their pressures); each cell is the fraction of that solute's feed captured.
  output$recovery_table <- DT::renderDataTable({
    req(rv$cascade)
    rec <- rv$cascade$recovery
    stg <- unique(rv$cascade$tidy[, c("stage", "pres_bar")]); stg <- stg[order(stg$stage), , drop = FALSE]
    df <- as.data.frame(round(100 * rec, 1)); names(df) <- sprintf("S%d", stg$stage)
    df <- cbind(Solute = rownames(rec), df)
    cn <- c(tip_col(i18n_r()$t("Solute"), i18n_r()$t("The target solute")),
      vapply(seq_len(nrow(stg)), function(i)
        tip_col(sprintf("S%d (%.0f bar)", stg$stage[i], stg$pres_bar[i]),
          sprintf(i18n_r()$t("Percent of feed captured at separator %d (%.0f bar)"), stg$stage[i], stg$pres_bar[i])),
        character(1)))
    DT::datatable(df, rownames = FALSE, escape = FALSE, colnames = cn, extensions = "Buttons",
      options = list(scrollX = TRUE, dom = "Bt", ordering = FALSE, language = tablang(),
        buttons = create_dt_export_buttons(i18n_r, "supercrit_cascade_recovery")))
  })

  # ---- Exports --------------------------------------------------------------
  output$export_curve_plot <- downloadHandler(
    filename = function() "chrastil_solubility_curves.png",
    content = function(file) ggplot2::ggsave(file, plot = solubility_ggplot(), width = 9, height = 6, dpi = 150)
  )
  # Combined-figure exports — current solute (PDF) and all solutes (ZIP), mirroring Solubility Comparison.
  output$export_current_plot <- downloadHandler(
    filename = function() paste0("supercrit_cascade_",
      gsub("[^a-zA-Z0-9]", "_", input$cascade_solute_sel %||% "plot"), "_",
      format(Sys.time(), "%d-%m-%Y_%Hhrs-%Mmin-%Ssec"), ".pdf"),
    content = function(file) {
      sel <- input$cascade_solute_sel %||% cascade_solutes()[1]
      gg <- cascade_plot_for(sel, for_export = TRUE); req(!is.null(gg))
      ggplot2::ggsave(file, plot = gg, device = "pdf", width = 10, height = 7, units = "in", dpi = 300)
      showNotification(i18n$t("Done!"), type = "message", duration = 3)
    }, contentType = "application/pdf")
  output$export_all_plots <- downloadHandler(
    filename = function() paste0("supercrit_cascade_all_plots_",
      format(Sys.time(), "%d-%m-%Y_%Hhrs-%Mmin-%Ssec"), ".zip"),
    content = function(file) {
      req(rv$optim); nms <- cascade_solutes()
      prep <- showNotification(i18n$t("Preparing export..."), type = "message", duration = NULL)
      on.exit(removeNotification(prep), add = TRUE)
      td <- file.path(tempdir(), paste0("cascade_plots_", format(Sys.time(), "%d-%m-%Y_%Hhrs-%Mmin-%Ssec")))
      dir.create(td, showWarnings = FALSE)
      tryCatch({
        for (nm in nms) { gg <- cascade_plot_for(nm, for_export = TRUE); if (is.null(gg)) next
          ggplot2::ggsave(file.path(td, paste0(gsub("[^a-zA-Z0-9]", "_", nm), ".pdf")),
            plot = gg, device = "pdf", width = 10, height = 7, units = "in", dpi = 300) }
        zip::zip(zipfile = file, files = list.files(td, full.names = TRUE), mode = "cherry-pick")
        showNotification(i18n$t("Done!"), type = "message", duration = 3)
      }, error = function(e) showNotification(paste(i18n$t("Export failed:"), e$message), type = "error"))
    }, contentType = "application/zip")

  # ---- Reset, help, accordion ----------------------------------------------
  observeEvent(input$reset, {
    rv$selected_saved_calc_ids <- character(0); rv$extra_solutes <- NULL
    rv$sol_curves <- NULL; rv$cascade <- NULL; rv$optim <- NULL
    updateCheckboxGroupInput(session, "saved_calculation_checkboxes", selected = character(0))
    showNotification(i18n$t("Inputs reset."), type = "message", duration = 2)
  })

  output$sfe_sep_cascade_HELP <- renderUI({ create_help_modal(i18n_r, "sep_cascade_help_en") })
  output$accordion_toggle_btn <- renderUI({ create_accordion_toggle_btn(ns, i18n_r, "cascade_accordion") })
}
