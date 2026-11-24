# Shared helpers for the "Load from Database" experimental-property browser, used by both the
# EOS Components and Solute Characterization modules. The module-specific parts (which input fields
# a selection fills, which Load buttons the notify shows) stay in each module; everything generic
# (the searchable table, the modal, the offline matcher, the available-data summary) lives here.

# ---- user-added compounds (persisted to the writable user-data dir; works on hosted deployments) ----
# Compounds the user adds via "Add to Database" live in one RDS under supercrit_user_dir, merged with the
# bundled DB by exp_db_all(). This is the SAME persistence mechanism as Save-to-Disk / saved components,
# so it survives across sessions wherever those do (on ephemeral free hosts it resets on restart).
exp_db_user_file <- function() file.path(supercrit_user_dir("user-settings"), "exp_db_user.rds")

exp_db_user <- function() {
  f <- exp_db_user_file()
  if (file.exists(f)) tryCatch(readRDS(f), error = function(e) NULL) else NULL
}

# Bundled DB + user additions (user row wins on a key collision, so users can correct a bundled value).
exp_db_all <- function() {
  base <- exp_prop_db(); u <- exp_db_user()
  if (is.null(base)) return(u)
  if (is.null(u) || !nrow(u)) return(base)
  for (cc in setdiff(names(base), names(u))) u[[cc]] <- NA      # align to the bundled schema
  u <- u[, names(base), drop = FALSE]
  out <- rbind(u, base)                                          # user first ...
  out[!duplicated(out$key), , drop = FALSE]                      # ... so it wins the de-dup
}

# --- Category / SFE-relevance filtering (the `category` + `sfe_relevant` columns are added to the
# bundled DB by data-raw/exp_property_db/categorize.R). SFE-relevant = natural products / actives
# extractable or measured (Chrastil) in scCO2 -> exactly the openCOSMO-RS-Phi fit candidates. ---

# Sorted category choices for the dropdown ("All" first); if sfe_only, only categories present in the
# SFE-relevant subset. Tolerates an un-enriched DB (returns just "All").
exp_db_categories <- function(sfe_only = FALSE, db = NULL) {
  if (is.null(db)) db <- exp_db_all()
  if (is.null(db) || !"category" %in% names(db)) return(c("All"))
  d <- if (isTRUE(sfe_only) && "sfe_relevant" %in% names(db)) db[isTRUE(db$sfe_relevant) | db$sfe_relevant %in% TRUE, , drop = FALSE] else db
  cats <- sort(unique(d$category[!is.na(d$category)]))
  c("All", cats)
}

# Filter the DB by category (or "All") and optionally to SFE-relevant compounds only.
exp_db_filtered <- function(db, category = "All", sfe_only = FALSE) {
  if (is.null(db) || !nrow(db)) return(db)
  if (isTRUE(sfe_only) && "sfe_relevant" %in% names(db))
    db <- db[db$sfe_relevant %in% TRUE, , drop = FALSE]
  if (!is.null(category) && nzchar(category) && category != "All" && "category" %in% names(db))
    db <- db[db$category %in% category, , drop = FALSE]
  db
}

# Wire the category dropdown to the SFE-only checkbox: toggling SFE-only restricts the dropdown to the
# categories present in the SFE subset (and keeps the current pick if still valid). Call once per module.
exp_db_wire_category <- function(input, session) {
  observeEvent(input$db_sfe_only, {
    ch <- exp_db_categories(isTRUE(input$db_sfe_only))
    sel <- if (!is.null(input$db_category) && input$db_category %in% ch) input$db_category else "All"
    updateSelectInput(session, "db_category", choices = ch, selected = sel)
  }, ignoreInit = TRUE)
}

# Append/replace one compound in the user file. Builds a row in the bundled schema (so rbind/merge are
# type-clean) and tags every provided property's provenance as "user-added".
exp_db_add <- function(name, cas, smiles, key, Tb = NA, Tc = NA, Pc = NA, Vc = NA,
                       dD = NA, dP = NA, dHB = NA, Chr_k = NA, Chr_A = NA, Chr_B = NA,
                       Tm = NA, Hvap = NA, dHfus = NA, category = NA) {
  base <- exp_prop_db(); if (is.null(base)) stop("bundled DB unavailable")
  for (cc in c("Tm", "Hvap", "Tm_src", "hvap_src", "dHfus", "dHfus_src"))
    if (!cc %in% names(base)) base[[cc]] <- NA   # tolerate old schema
  row <- base[1, , drop = FALSE]; for (cc in names(row)) row[[cc]][1] <- NA   # blank row, keep col types
  fin <- function(x) { x <- suppressWarnings(as.numeric(x)); if (length(x) && is.finite(x)) x else NA_real_ }
  row$key <- key; row$smiles <- smiles; row$inchikey <- NA_character_
  row$name <- if (nzchar(trimws(name))) trimws(name) else smiles
  row$cas <- trimws(cas)
  if ("category" %in% names(row)) row$category <- { cc <- trimws(category %||% ""); if (nzchar(cc)) cc else NA_character_ }
  row$Tb <- fin(Tb); row$Tc <- fin(Tc); row$Pc <- fin(Pc); row$Vc <- fin(Vc)
  row$dD <- fin(dD); row$dP <- fin(dP); row$dHB <- fin(dHB)
  row$Chr_k <- fin(Chr_k); row$Chr_A <- fin(Chr_A); row$Chr_B <- fin(Chr_B)
  row$Tm <- fin(Tm); row$Hvap <- fin(Hvap); row$dHfus <- fin(dHfus)
  row$Tb_src   <- if (is.finite(row$Tb)) "user-added" else NA_character_
  row$crit_src <- if (any(is.finite(c(row$Tc, row$Pc, row$Vc)))) "user-added" else NA_character_
  row$hsp_src  <- if (is.finite(row$dD)) "user-added" else NA_character_
  row$chr_src  <- if (is.finite(row$Chr_k)) "user-added" else NA_character_
  row$Tm_src   <- if (is.finite(row$Tm)) "user-added" else NA_character_
  row$hvap_src <- if (is.finite(row$Hvap)) "user-added" else NA_character_
  row$dHfus_src <- if (is.finite(row$dHfus)) "user-added" else NA_character_
  cur <- exp_db_user()
  new <- if (!is.null(cur) && nrow(cur)) rbind(cur[cur$key != key, , drop = FALSE], row) else row
  f <- exp_db_user_file(); dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  saveRDS(new, f); invisible(new)
}

# Keys of user-added compounds (rows that live in the user file). Only these can be edited/deleted —
# the bundled core DB is read-only. A user row that shares a bundled key overrides that bundled row
# (see exp_db_all), so deleting it reverts the compound to its bundled values rather than removing it.
exp_db_user_keys <- function() { u <- exp_db_user(); if (is.null(u) || !nrow(u)) character(0) else u$key }

# Remove one user-added compound (by canonical key) from the user file. Returns TRUE if a row was
# removed. If no user rows remain the file is deleted. The bundled DB is never touched.
exp_db_delete <- function(key) {
  cur <- exp_db_user(); if (is.null(cur) || !nrow(cur)) return(invisible(FALSE))
  keep <- cur$key != key
  if (all(keep)) return(invisible(FALSE))
  new <- cur[keep, , drop = FALSE]
  f <- exp_db_user_file()
  if (nrow(new)) saveRDS(new, f) else if (file.exists(f)) file.remove(f)
  invisible(TRUE)
}

# Which of `names` are usable with a database-limited EoS engine, using the SAME resolution the engine's
# solubility function uses (NOT exact string equality): CSP-2021 via .csp_match = normalised (strip
# non-alphanumerics, lowercase) name matched against jcosmo AND the s2 alternate-name field; openCOSMO-RS-Phi
# by normalised bundle name or (optionally) SMILES. So "Aspirin", "1,2-Benzenediol" etc. resolve correctly.
# The openCOSMO-RS-Phi bundle meta filtered to compounds that carry a REAL fit. "pending-fit" entries have an
# ingested sigma-profile but placeholder default dispersion params (never fitted to Psat or scCO2 solubility),
# so their EoS predictions are unvalidated/misleading. Everything else (CHEMCAD Psat, raw-solubility Zini,
# Table-A1 published) is a genuine parameterisation. Single source of truth for ocrsphi usability across the app.
ocrsphi_fitted_meta <- function() {
  m <- tryCatch(ocrsphi_data()$meta, error = function(e) NULL)
  if (is.null(m)) return(NULL)
  m[!is.na(m$calibration) & !grepl("pending", m$calibration, ignore.case = TRUE), , drop = FALSE]
}

# Can openCOSMO-RS-Phi actually COMPUTE a solubility for a fitted compound at operating temperature `T_C`?
# Being fitted (real EoS dispersion params) is necessary but NOT sufficient: below the melting point the
# SOLID-solubility route needs an enthalpy of fusion, and some bundle solids have none (dHfus = NA) -> the
# solver returns NA at every pressure. Right at the melting point the solid fixed point also degenerates
# (returns the near-ideal fusion activity, physically far too high). Mirrors the engine's real behaviour:
# a LIQUID at T (T >= Tm, or Tm unknown) always computes; a solid computes only with a dHfus, and is flagged
# unreliable within `near_K` of its melting point. Returns list(code, Tm_C). Pass `meta` to avoid refetching.
#   code: "ok" (computable) | "near_melting" (computes but unreliable) | "solid_no_fusion" (NA) | "not_fitted".
ocrsphi_compute_status <- function(name, smiles = NULL, T_C = 40, meta = ocrsphi_fitted_meta(), near_K = 10) {
  norm <- function(x) gsub("[^a-z0-9]", "", tolower(x))
  if (is.null(meta) || !nrow(meta)) return(list(code = "not_fitted", Tm_C = NA_real_))
  i <- match(norm(name), norm(meta$compound))
  if (is.na(i) && !is.null(smiles) && length(smiles) == 1 && nzchar(smiles))
    i <- match(tolower(smiles), tolower(meta$smiles))
  if (is.na(i)) return(list(code = "not_fitted", Tm_C = NA_real_))
  Tm <- suppressWarnings(as.numeric(meta$Tm[i])); dH <- suppressWarnings(as.numeric(meta$dHfus[i]))
  Tk <- suppressWarnings(as.numeric(T_C)[1]) + 273.15; Tm_C <- if (is.finite(Tm)) Tm - 273.15 else NA_real_
  if (is.finite(Tm) && is.finite(Tk) && Tk < Tm) {              # solid at the operating temperature
    if (!is.finite(dH) || dH <= 0) return(list(code = "solid_no_fusion", Tm_C = Tm_C))
    if ((Tm - Tk) < near_K)        return(list(code = "near_melting",    Tm_C = Tm_C))
  }
  list(code = "ok", Tm_C = Tm_C)
}

db_engine_eligible <- function(names, engine, smiles = NULL) {
  norm <- function(x) gsub("[^a-z0-9]", "", tolower(x))
  q <- norm(names)
  if (identical(engine, "csp")) {
    cc <- tryCatch(csp_compounds(), error = function(e) NULL)
    if (is.null(cc)) return(rep(FALSE, length(names)))
    # CSP-2021 membership requires the compound to actually carry CSP-2021 parameters (has_csp21), not just
    # a name match: csp_solubility(model="2021") stops() otherwise (affects e.g. N-DOCOSANE, the ions H+1/OH-1).
    has21 <- rep(!is.na(cc$has_csp21) & cc$has_csp21, 2L)
    q %in% unique(norm(c(cc$jcosmo, cc$s2))[has21])
  } else if (identical(engine, "ocrsphi")) {
    # Only compounds carrying a REAL fit are ocrsphi-usable (pending-fit entries have placeholder params).
    m <- ocrsphi_fitted_meta()
    if (is.null(m)) return(rep(FALSE, length(names)))
    hit <- q %in% norm(m$compound)
    if (!is.null(smiles)) hit <- hit | (!is.na(smiles) & nzchar(smiles) & tolower(smiles) %in% tolower(m$smiles))
    hit
  } else rep(TRUE, length(names))
}

# The ENGINE's OWN compound list (CSP-2021 / openCOSMO-RS-Phi) as an exp_prop_db()-schema data.frame, so the
# Load-from-DB browser can offer EVERY compound the engine can predict — not just the subset that also carries
# measured properties in the experimental DB. Both engines RESOLVE BY NAME (ocrsphi also by SMILES), exactly
# as csp_solubility()/ocrsphi_solubility() do, so a name is enough to run them. Unknown experimental fields
# (Tb/Tc/HSP/Chrastil k-A-B) are NA; Tm/dHfus are carried where the engine has them. CSP's database has no
# SMILES/CAS (name-keyed), ocrsphi has both. Returns NULL for non-EoS models.
engine_compound_db <- function(model) {
  cols <- tryCatch(names(exp_prop_db()), error = function(e) NULL); if (is.null(cols)) return(NULL)
  blank <- function(n) { d <- as.data.frame(matrix(NA, n, length(cols))); names(d) <- cols
    for (cc in cols) d[[cc]] <- NA; d }
  if (identical(model, "csp")) {
    cc <- tryCatch(csp_compounds(), error = function(e) NULL); if (is.null(cc) || !nrow(cc)) return(NULL)
    cc <- cc[!is.na(cc$has_csp21) & cc$has_csp21, , drop = FALSE]
    nm <- cc$jcosmo; d <- blank(length(nm))
    d$name <- nm; d$key <- paste0("csp:", toupper(nm))
    if ("Tm" %in% cols) d$Tm <- suppressWarnings(as.numeric(cc$Tm))
    if ("dHfus" %in% cols) d$dHfus <- suppressWarnings(as.numeric(cc$dHfus))
    if ("category" %in% cols) d$category <- "CSP-2021 database"
  } else if (identical(model, "ocrsphi")) {
    m <- tryCatch(ocrsphi_fitted_meta(), error = function(e) NULL); if (is.null(m) || !nrow(m)) return(NULL)
    d <- blank(nrow(m))
    d$name <- m$compound; d$key <- paste0("ocrsphi:", toupper(m$compound))
    if ("cas" %in% cols) d$cas <- m$cas
    if ("smiles" %in% cols) d$smiles <- m$smiles
    if ("Tm" %in% cols) d$Tm <- suppressWarnings(as.numeric(m$Tm))
    if ("dHfus" %in% cols) d$dHfus <- suppressWarnings(as.numeric(m$dHfus))
    if ("category" %in% cols) d$category <- "openCOSMO-RS-Phi database"
  } else return(NULL)
  if ("sfe_relevant" %in% cols) d$sfe_relevant <- NA
  d[!is.na(d$name) & nzchar(d$name), , drop = FALSE]
}

# Resolve the Load-from-DB source for a module: the engine's full compound list when the "Engine compounds"
# toggle is on AND an EoS engine is active, else the experimental DB. `engine` is the active EoS model
# ("csp"/"ocrsphi") or NULL. Falls back to the experimental DB if the engine list is unavailable.
exp_db_source <- function(input, engine = NULL) {
  if (!is.null(engine) && isTRUE(input$db_source == "engine") && engine %in% c("csp", "ocrsphi")) {
    ed <- engine_compound_db(engine); if (!is.null(ed) && nrow(ed)) return(ed)
  }
  exp_db_all()
}

# Logical mask of which experimental-DB rows are usable with a given co-solvent method/engine, so the
# Load-from-DB modal can show only eligible compounds: HSP needs measured δD/δP/δHB; Solubility (ML/Makarov)
# needs a SMILES; the database-limited EoS engines (csp, ocrsphi) need the compound to resolve in that
# engine's database (via db_engine_eligible). Pass "csp"/"ocrsphi" as `method` for the respective EoS.
exp_db_method_keep <- function(db, method) {
  if (is.null(db) || !nrow(db)) return(logical(0))
  fin <- function(x) is.finite(suppressWarnings(as.numeric(x)))
  switch(method,
    hsp        = fin(db$dD) & fin(db$dP) & fin(db$dHB) & fin(db$Tc),  # Tc needed for the T-correction
    solubility = !is.na(db$smiles) & nzchar(db$smiles),
    makarov    = !is.na(db$smiles) & nzchar(db$smiles),              # Makarov predicts from SMILES
    experimental = fin(db$Chr_k) & fin(db$Chr_A) & fin(db$Chr_B),    # needs measured Chrastil k/A/B
    chrastil   = fin(db$Chr_k) & fin(db$Chr_A) & fin(db$Chr_B),
    csp        = db_engine_eligible(db$name, "csp"),
    ocrsphi    = db_engine_eligible(db$name, "ocrsphi", db$smiles),
    rep(TRUE, nrow(db)))
}

# Offline match of a compound to the experimental DB: SMILES (canonical) > CAS (exact, pipe-aware) > Name.
# Matching is against the passed `db` (use exp_db_all() so user additions are matched too). NULL/"" skipped.
exp_db_match <- function(db, smiles = NULL, cas = NULL, name = NULL) {
  if (is.null(db)) return(NULL)
  if (!is.null(smiles) && nzchar(smiles)) {
    k <- tryCatch(exp_canon_key(smiles), error = function(e) NA_character_)
    if (!is.na(k)) { i <- which(db$key == k); if (length(i)) return(db[i[1], ]) } }
  if (!is.null(cas)) { cc <- trimws(cas)
    if (nzchar(cc)) { i <- which(!is.na(db$cas) & grepl(paste0("(^|\\|)", cc, "(\\||$)"), db$cas)); if (length(i)) return(db[i[1], ]) } }
  if (!is.null(name)) { nq <- tolower(trimws(name))
    if (nzchar(nq)) { i <- which(!is.na(db$name) & tolower(db$name) == nq); if (length(i)) return(db[i[1], ]) } }
  NULL
}

# Shared column widths (percent, in table column order) applied via a <colgroup> to BOTH the table and
# the add/edit row, so the inputs line up directly under their table columns. Order MUST match exp_db_dt's
# `d`. A colgroup is the only reliable way to size columns under the grouped (colspan) HSP/Chrastil header.
exp_db_colwidths <- function() c(
  Name = 13, CAS = 7.5, Category = 9, Tb = 5.4, Tc = 5.4, Pc = 5.4, Vc = 5,
  Tm = 5.4, Hvap = 6, dHfus = 6, dD = 5, dP = 5, dHB = 5, k = 4.7, A = 4.7, B = 4.7)

# The searchable DT for the modal: every parameter shown, with the three HSP components grouped under an
# "HSP" header and the three Chrastil parameters under a "Chrastil" header (two-row header container).
exp_db_dt <- function(i18n_r, db = NULL) {
  if (is.null(db)) db <- exp_db_all()
  if (is.null(db) || !nrow(db)) return(NULL)
  rnd <- function(x, d = 1) ifelse(is.finite(x), round(x, d), NA_real_)
  col <- function(nm) if (nm %in% names(db)) db[[nm]] else rep(NA_real_, nrow(db))   # tolerate old schema
  # SFE (solubility-fit) + Psat (vapour-pressure-fit) badges, each with a dynamically-translated,
  # HTML-attribute-safe tooltip (htmlEscape(attribute=TRUE) so a translated string with quotes/<> can't
  # break the title=""; the whole table re-renders via i18n_r() on a language switch).
  esc <- function(s) htmltools::htmlEscape(s, attribute = TRUE)
  sfe  <- if ("sfe_relevant"  %in% names(db)) db$sfe_relevant  %in% TRUE else rep(FALSE, nrow(db))
  psat <- if ("psat_fittable" %in% names(db)) db$psat_fittable %in% TRUE else rep(FALSE, nrow(db))
  sfe_badge  <- sprintf(' <span class="sfe-badge" title="%s">SFE</span>',
                        esc(i18n_r()$t("SFE / extraction-relevant compound (natural product or scCO2-measured solute; solubility-fit route)")))
  psat_badge <- sprintf(' <span class="psat-badge" title="%s">Psat</span>',
                        esc(i18n_r()$t("Vapour-pressure fittable (experimental DIPPR Psat + liquid volume; CO2-style Psat-fit route)")))
  nm <- paste0(db$name, ifelse(sfe, sfe_badge, ""), ifelse(psat, psat_badge, ""))
  ctg <- if ("category" %in% names(db)) factor(ifelse(is.na(db$category), "", db$category)) else factor(rep("", nrow(db)))
  d <- data.frame(
    Name = nm, CAS = ifelse(is.na(db$cas), "", sub("\\|.*$", "", db$cas)),
    Category = ctg,
    Tb = rnd(db$Tb), Tc = rnd(db$Tc), Pc = rnd(db$Pc), Vc = rnd(db$Vc, 0),
    Tm = rnd(col("Tm")), Hvap = rnd(col("Hvap")), dHfus = rnd(col("dHfus")),
    dD = rnd(db$dD, 2), dP = rnd(db$dP, 2), dHB = rnd(db$dHB, 2),
    k = rnd(db$Chr_k, 3), A = rnd(db$Chr_A, 0), B = rnd(db$Chr_B, 2),
    check.names = FALSE, stringsAsFactors = FALSE)
  th <- function(..., tip = NULL) if (is.null(tip)) tags$th(...) else tags$th(title = tip, ...)
  ctr <- function(...) tags$th(style = "text-align:center;border-bottom:1px solid #ddd;", ...)
  container <- tags$table(class = "display", tags$thead(
    tags$tr(
      th(rowspan = 2, i18n_r()$t("Name"), tip = i18n_r()$t("Compound name")),
      th(rowspan = 2, i18n_r()$t("CAS"),  tip = i18n_r()$t("CAS registry number")),
      th(rowspan = 2, i18n_r()$t("Category"), tip = i18n_r()$t("Compound category (SFE badge marks natural / extractable compounds)")),
      th(rowspan = 2, HTML("<i>T</i><sub>b</sub> (K)"),   tip = i18n_r()$t("Normal boiling point (K)")),
      th(rowspan = 2, HTML("<i>T</i><sub>c</sub> (K)"),   tip = i18n_r()$t("Critical temperature (K)")),
      th(rowspan = 2, HTML("<i>P</i><sub>c</sub> (bar)"), tip = i18n_r()$t("Critical pressure (bar)")),
      th(rowspan = 2, HTML("<i>V</i><sub>c</sub>"),       tip = i18n_r()$t("Critical volume (cm³/mol)")),
      th(rowspan = 2, HTML("<i>T</i><sub>m</sub> (K)"),       tip = i18n_r()$t("Melting point (K)")),
      th(rowspan = 2, HTML("Δ<i>H</i><sub>vap</sub>"),        tip = i18n_r()$t("Enthalpy of vaporization (kJ/mol)")),
      th(rowspan = 2, HTML("Δ<i>H</i><sub>fus</sub>"),        tip = i18n_r()$t("Enthalpy of fusion (kJ/mol)")),
      ctr(colspan = 3, HTML(paste0(i18n_r()$t("HSP"), " (√MPa)"))),
      ctr(colspan = 3, i18n_r()$t("Chrastil (mol/mol)"))
    ),
    tags$tr(
      th(HTML("<i>δ</i><sub>D</sub>")), th(HTML("<i>δ</i><sub>P</sub>")), th(HTML("<i>δ</i><sub>HB</sub>")),
      th(HTML("<i>k</i>")), th(HTML("<i>A</i>")), th(HTML("<i>B</i>"))
    )
  ))
  w <- exp_db_colwidths()
  jsw <- paste0("[", paste(unname(w), collapse = ","), "]")
  # Rendered server-side (server = TRUE at the render call) so only the visible page is sent to the
  # browser instead of all ~8k rows - the modal opens and filters fast. Full-database export is a Shiny
  # downloadHandler (exp_db_wire_download) writing to the modal's .db-dl-slot; the client-side DataTables
  # Buttons could only ever export the current page under server-side processing, so they are removed.
  DT::datatable(d, container = container, selection = "single", rownames = FALSE, filter = "top",
    escape = FALSE, class = "display expdb-dt",
    options = list(pageLength = 10, scrollX = FALSE, autoWidth = FALSE, dom = "lfrtip",
      # Prepend a <colgroup> so column widths are deterministic despite the grouped (colspan) header
      # (the add/edit row uses the SAME widths -> they align).
      initComplete = DT::JS(sprintf("function(settings){var t=$(this.api().table().node());if(!t.children('colgroup').length){var w=%s,c='<colgroup>';for(var i=0;i<w.length;i++)c+='<col style=\"width:'+w[i]+'%%\">';c+='</colgroup>';t.prepend(c);}}", jsw)),
      columnDefs = list(
        list(className = "dt-center", targets = 3:15),
        list(className = "dt-nowrap", targets = 0:2),   # Name / CAS / Category: no wrapping
        list(targets = 3:15, render = DT::JS("function(d){return (d===null||d==='')?'\\u2013':d;}")))))  # '-' for missing
}

# The modal dialog. `ns` is the module namespace fn; the output `ns("db_table")` and button
# `ns("db_load_selected")` are wired in each module server. `i18n` is the static i18n object.
# An "Add a new compound" form is embedded as a collapsible section under the table, so adding and
# browsing live in one place (no separate Add modal). SMILES is required (it becomes the match key).
exp_db_modal <- function(ns, i18n, engine = NULL) {
  w <- exp_db_colwidths()
  ni <- function(id) numericInput(ns(id), label = NULL, value = NA, width = "100%")
  ti <- function(id) textInput(ns(id), label = NULL, value = "", width = "100%")
  # A <colgroup> with the shared widths: the add/edit row is its own fixed-layout table using the SAME
  # colgroup as the data table below (which gets an identical colgroup injected), so columns line up.
  colgroup <- tags$colgroup(lapply(unname(w), function(x) tags$col(style = paste0("width:", x, "%"))))
  addlab <- list(i18n$t("Name"), i18n$t("CAS"), i18n$t("Category"),
    HTML("<i>T</i><sub>b</sub> (K)"), HTML("<i>T</i><sub>c</sub> (K)"), HTML("<i>P</i><sub>c</sub> (bar)"),
    HTML("<i>V</i><sub>c</sub>"), HTML("<i>T</i><sub>m</sub> (K)"),
    HTML("Δ<i>H</i><sub>vap</sub>"), HTML("Δ<i>H</i><sub>fus</sub>"),
    HTML("<i>δ</i><sub>D</sub>"), HTML("<i>δ</i><sub>P</sub>"), HTML("<i>δ</i><sub>HB</sub>"),
    HTML("<i>k</i>"), HTML("<i>A</i>"), HTML("<i>B</i>"))
  addin <- list(ti("add_name"), ti("add_cas"), ti("add_category"),
    ni("add_Tb"), ni("add_Tc"), ni("add_Pc"), ni("add_Vc"), ni("add_Tm"),
    ni("add_Hvap"), ni("add_dHfus"), ni("add_dD"), ni("add_dP"), ni("add_dHB"),
    ni("add_k"), ni("add_A"), ni("add_B"))
  add_table <- tags$table(class = "db-add-table", colgroup,
    tags$thead(tags$tr(lapply(addlab, function(l) tags$th(l)))),
    tags$tbody(tags$tr(lapply(addin, function(ctrl) tags$td(ctrl)))))
  # Tb / Tc with proper italic + subscript in the intro line (translated first; safe because "Tb"/"Tc"
  # occur only as those symbols in this sentence).
  intro <- i18n$t("Search by name or CAS, pick a compound, and load its identifiers plus any experimental Tb / Tc. Tb sources: Joback/Poling + OPERA/PHYSPROP; Tc: Joback/Poling.")
  intro <- gsub("Tb", "<i>T</i><sub>b</sub>", intro, fixed = TRUE)
  intro <- gsub("Tc", "<i>T</i><sub>c</sub>", intro, fixed = TRUE)

  modalDialog(title = i18n$t("Load from Experimental Database"), size = "l", easyClose = TRUE,
    tags$style(HTML(paste0(
      "@media (min-width:768px){.modal-lg{width:96vw!important;max-width:1700px!important;}}",
      ".sfe-badge,.psat-badge{display:inline-block;font-size:9px;font-weight:700;color:#fff;border-radius:3px;",
      "padding:1px 4px;margin-left:5px;vertical-align:middle;letter-spacing:.3px;}",
      ".sfe-badge{background:#2e7d32;}.psat-badge{background:#1565c0;}",
      # deterministic table columns (a matching colgroup is injected into the DT via initComplete)
      ".expdb table.dataTable{table-layout:fixed;width:100%!important;}",
      ".expdb table.dataTable th,.expdb table.dataTable td{overflow:hidden;text-overflow:ellipsis;padding:6px 6px;}",
      ".expdb .dt-nowrap{white-space:nowrap;}",
      ".expdb .db-sfe-only{white-space:nowrap;}",
      ".expdb .db-sfe-only .checkbox{margin-top:0;margin-bottom:0;}",
      # add/edit row: its own fixed-layout table sharing the SAME colgroup widths as the data table
      ".expdb .db-add-table{table-layout:fixed;width:100%;margin:0;}",
      # column labels above the inputs: green + same size as the table column headers
      ".expdb .db-add-table th{font-size:14px;font-weight:700;color:#218838;padding:1px 6px 2px;text-align:center;",
      "white-space:nowrap;overflow:hidden;text-overflow:ellipsis;border:0;}",
      ".expdb .db-add-table th:nth-child(-n+3){text-align:left;}",
      ".expdb .db-add-table td{padding:0 6px;border:0;}",
      ".expdb .db-add-table .form-group{margin-bottom:0;}",
      ".expdb .db-add-table .form-control{padding:3px 5px;height:32px;}",
      # kill the default Bootstrap form-group bottom margin in the add section so flex-end button
      # alignment lands on the SMILES input's real bottom edge (not 15px below it)
      ".expdb .db-add-section .form-group{margin-bottom:0;}",
      # green-themed editable fields (border + fill) with GREEN input text at the column-name size
      ".expdb .db-add-section .form-control{border:1px solid #7cbf7c;background:#f7fcf7;color:#218838;font-size:14px;}",
      ".expdb .db-add-section .form-control:focus{border-color:#28a745;background:#fff;box-shadow:0 0 0 .12rem rgba(40,167,69,.25);}",
      # Download-Database button (a relocated DT collection button) styled to MATCH the Save button:
      # same font size, padding and height, sitting right beside it.
      ".expdb .db-dl-slot{display:flex;align-items:flex-end;}",
      ".expdb .db-dl-slot .dt-buttons{margin:0;float:none;}",
      # the MAIN 'Download Database' button only (class buttons-collection) matches the Save button
      ".expdb .db-dl-slot .dt-button.buttons-collection{margin:0;font-size:14px;line-height:1.42857;padding:6px 12px;height:34px;",
      "background:#337ab7;color:#fff;border:1px solid #2e6da4;border-radius:4px;font-weight:400;}",
      ".expdb .db-dl-slot .dt-button.buttons-collection:hover{background:#286090;border-color:#204d74;color:#fff;}",
      ".expdb .db-dl-slot .dt-button.buttons-collection span{font-size:14px;}",
      # dropdown items (CSV / Excel / PDF): plain, no blue background (menu may be appended to <body>)
      ".dt-button-collection .dt-button{background:#fff;color:#333;border:0;box-shadow:none;font-size:14px;font-weight:400;text-align:left;}",
      ".dt-button-collection .dt-button:hover,.dt-button-collection .dt-button:focus{background:#f2f2f2;color:#333;box-shadow:none;}",
      ".expdb .db-add-table input[type=number]{-moz-appearance:textfield;}",
      ".expdb .db-add-table input[type=number]::-webkit-outer-spin-button,",
      ".expdb .db-add-table input[type=number]::-webkit-inner-spin-button{-webkit-appearance:none;margin:0;}"
    ))),
    div(class = "expdb",
      tags$p(style = "color:#6c757d; font-size:13px; margin-bottom:6px;", HTML(intro)),
      tags$p(style = "color:#6c757d; font-size:12px; margin:2px 0 6px 0;",
        i18n$t("Select a row to Load, Edit, or Delete it. Editing loads the compound into the row below the headers (change values, then Save to update). Only compounds you added can be edited or deleted — the core database is read-only.")),
      div(style = "display:flex; align-items:flex-end; gap:24px; margin-bottom:8px;",
        # Source toggle: experimental DB (default) vs the active EoS engine's FULL compound list. Only shown
        # when a database-limited engine (CSP-2021 / openCOSMO-RS-Phi) is active, so you can select ANY
        # compound the engine can predict — not just the subset that also has measured properties.
        if (!is.null(engine)) div(style = "padding-bottom:4px;",
          radioButtons(ns("db_source"), i18n$t("Source"), inline = TRUE, selected = "exp",
            choiceNames = list(i18n$t("Experimental database"),
              sprintf(i18n$t("%s compounds"),
                if (identical(engine, "csp")) "CSP-2021" else if (identical(engine, "ocrsphi")) "openCOSMO-RS-Phi" else engine)),
            choiceValues = c("exp", "engine"))),
        div(style = "width:300px;",
          selectInput(ns("db_category"), i18n$t("Category"),
                      choices = exp_db_categories(FALSE), selected = "All", width = "100%")),
        div(class = "db-sfe-only", style = "padding-bottom:8px;",
          checkboxInput(ns("db_sfe_only"),
                        HTML(paste0(i18n$t("SFE / extraction-relevant compounds only"),
                                    " <span class='sfe-badge'>SFE</span>")), value = FALSE))
      ),
      # ADD / EDIT strip — ABOVE the table, as a column-aligned "new row" (SMILES has no table column,
      # so it + the Save button sit on their own line just above the aligned input row). Kept a direct
      # child of .expdb (no padded wrapper) so its columns align exactly with the data table below.
      div(id = ns("db_add_details"), class = "db-add-section",
        div(style = "font-weight:700; color:#218838; font-size:16px; margin:4px 0 4px;",
          HTML(paste0("&#43;&nbsp;", i18n$t("Add or edit a compound in the database")))),
        tags$p(style = "color:#6c757d; font-size:12px; margin:0 0 6px;",
          i18n$t("SMILES is required (it is used to match the compound later). Leave unknown values blank; the entry is saved to your personal data and persists across sessions. Saving a compound whose SMILES already exists overwrites that entry. HSP (δD/δP/δHB) and Chrastil (k/A/B) are each all-or-none.")),
        # SMILES on the left, then Save + Download DIRECTLY to its right (both same size, bottom-aligned).
        div(style = "display:flex; align-items:flex-end; gap:12px; margin-bottom:8px;",
          div(style = "flex:0 0 340px;",
            tags$label(HTML("SMILES <span style='color:#c0392b;'>*</span>"),
              style = "font-size:11px;color:#555;margin-bottom:2px;display:block;"),
            textInput(ns("add_smiles"), label = NULL, width = "100%")),
          actionButton(ns("db_add_save"), i18n$t("Save to Database"), icon = icon("plus"),
            class = "btn-success", style = "color:white;"),
          div(class = "db-dl-slot",     # full-database CSV export (Shiny downloadHandler; server-side table)
            downloadButton(ns("db_download"), i18n$t("Download Database"), icon = icon("download"),
              class = "btn-primary", style = "color:white; height:34px;"))),
        add_table,
        tags$hr(style = "margin:8px 0 6px;")),
      DT::dataTableOutput(ns("db_table"))),
    footer = tagList(modalButton(i18n$t("Cancel")),
      actionButton(ns("db_delete_selected"), i18n$t("Delete Selected"), icon = icon("trash"),
        class = "btn-danger", style = "color:white;"),
      actionButton(ns("db_edit_selected"), i18n$t("Edit Selected"), icon = icon("pen"),
        class = "btn-warning", style = "color:white;"),
      actionButton(ns("db_load_selected"), i18n$t("Load Selected"), icon = icon("download"),
        class = "btn-success", style = "color:white;")))
}

# Wire the modal's "Edit Selected" and "Delete Selected" buttons for a module. Edit pre-fills the
# in-modal add/edit form from the selected table row and opens it (Save then overwrites by SMILES key);
# Delete removes a user-added compound (guarded — the core DB is read-only). `refresh` is the module's
# table-refresh callback (typically bumps its db_refresh reactiveVal). Shared by every module that
# embeds exp_db_modal(), so the behaviour is identical everywhere.
exp_db_wire_edit_delete <- function(input, output, session, i18n, refresh = function() {}, db_fn = exp_db_all) {
  ns <- session$ns
  # Row index maps to whatever db the module's table renders (some modules show a method-filtered
  # subset), so read from db_fn() — NOT necessarily exp_db_all().
  sel_row <- function() {
    sel <- input$db_table_rows_selected
    if (is.null(sel) || !length(sel)) return(NULL)
    db <- db_fn(); if (is.null(db) || !nrow(db) || sel[1] > nrow(db)) return(NULL)
    db[sel[1], , drop = FALSE]
  }
  observeEvent(input$db_edit_selected, {
    r <- sel_row()
    if (is.null(r)) { showNotification(i18n$t("Select a compound in the table first."), type = "warning", session = session); return() }
    updateTextInput(session, "add_name",   value = if (is.na(r$name)) "" else r$name)
    updateTextInput(session, "add_cas",    value = if (is.na(r$cas)) "" else sub("\\|.*$", "", r$cas))
    updateTextInput(session, "add_smiles", value = if (is.na(r$smiles)) "" else r$smiles)
    updateTextInput(session, "add_category", value = if (!"category" %in% names(r) || is.na(r$category)) "" else r$category)
    setn <- function(id, v) { v <- suppressWarnings(as.numeric(v)[1]); updateNumericInput(session, id, value = if (isTRUE(is.finite(v))) v else NA) }
    setn("add_Tb", r$Tb); setn("add_Tc", r$Tc); setn("add_Pc", r$Pc); setn("add_Vc", r$Vc)
    setn("add_Tm", r$Tm); setn("add_Hvap", r$Hvap); setn("add_dHfus", r$dHfus)
    setn("add_dD", r$dD); setn("add_dP", r$dP); setn("add_dHB", r$dHB)
    setn("add_k", r$Chr_k); setn("add_A", r$Chr_A); setn("add_B", r$Chr_B)
    # Remember which compound is being edited, so the duplicate check treats re-saving it as an update
    # (not a clashing new entry). Keyed by SMILES (the canonical match key).
    session$userData[[session$ns("expdb_edit_key")]] <- r$key
    # Scroll the (always-visible) add/edit strip into view.
    try(shinyjs::runjs(sprintf("var d=document.getElementById('%s'); if(d){d.scrollIntoView({behavior:'smooth',block:'center'});}", ns("db_add_details"))), silent = TRUE)
    showNotification(i18n$t("Editing this compound — change the values in the row above the table and click Save to Database to update it."), type = "message", duration = 4, session = session)
  })
  observeEvent(input$db_delete_selected, {
    r <- sel_row()
    if (is.null(r)) { showNotification(i18n$t("Select a compound in the table first."), type = "warning", session = session); return() }
    if (!r$key %in% exp_db_user_keys()) {
      showNotification(i18n$t("Only compounds you added can be deleted — the core database is read-only."), type = "warning", session = session); return() }
    if (isTRUE(exp_db_delete(r$key))) {
      refresh()
      showNotification(sprintf(i18n$t("Removed '%s' from your database."), r$name), type = "message", duration = 3, session = session)
    }
  })
}

# Register the modal's "Download Database" button as a full-database CSV export. With the table rendered
# server-side (only the visible page reaches the browser), the old client-side DataTables export would only
# dump the current page - so the full, currently-filtered database is written here instead. `db_fn` is the
# module's table-data reactive (the SAME one passed to exp_db_dt), so the export matches exactly what the
# table shows (category / SFE / method filters included). Call once per module.
exp_db_wire_download <- function(output, session, db_fn = exp_db_all, i18n = NULL) {
  output$db_download <- downloadHandler(
    filename = function() paste0(generate_filename_with_timestamp("experimental_database"), ".csv"),
    content = function(file) {
      db <- tryCatch(db_fn(), error = function(e) NULL)
      if (is.null(db) || !NROW(db)) db <- data.frame()
      utils::write.csv(db, file, row.names = FALSE, na = "")
    })
}

# Validate + persist the Add-to-Database form. Shared by both modules (input ids are identical via ns).
# Returns TRUE on a successful save (so the caller can refresh the table), FALSE/invisible otherwise.
# `close = TRUE` closes the modal after saving; the integrated in-modal form passes FALSE so the user
# stays on the table (the form is reset and the new row appears after a refresh).
exp_db_add_save <- function(input, session, i18n, close = TRUE) {
  smi <- trimws(input$add_smiles %||% "")
  if (!nzchar(smi)) { showNotification(i18n$t("A SMILES string is required to add a compound."), type = "warning", session = session); return(invisible(FALSE)) }
  key <- tryCatch(exp_canon_key(smi), error = function(e) NA_character_)
  if (is.na(key)) { showNotification(i18n$t("That SMILES could not be parsed. Please check it and try again."), type = "error", session = session); return(invisible(FALSE)) }
  if (!any(is.finite(suppressWarnings(as.numeric(c(input$add_Tb, input$add_Tc, input$add_Pc, input$add_Vc,
        input$add_dD, input$add_dP, input$add_dHB, input$add_k, input$add_A, input$add_B, input$add_Tm, input$add_Hvap, input$add_dHfus)))))) {
    showNotification(i18n$t("Enter at least one experimental property value before saving."), type = "warning", session = session); return(invisible(FALSE)) }
  # Composite parameters are all-or-none: HSP needs all of δD/δP/δHB; Chrastil needs all of k/A/B.
  nfin <- function(...) sum(is.finite(suppressWarnings(as.numeric(c(...)))))
  if (nfin(input$add_dD, input$add_dP, input$add_dHB) %in% c(1L, 2L)) {
    showNotification(i18n$t("Hansen parameters must be entered all together (δD, δP, δHB) or left all blank."), type = "warning", session = session); return(invisible(FALSE)) }
  if (nfin(input$add_k, input$add_A, input$add_B) %in% c(1L, 2L)) {
    showNotification(i18n$t("Chrastil coefficients must be entered all together (k, A, B) or left all blank."), type = "warning", session = session); return(invisible(FALSE)) }
  # --- duplicate / overwrite awareness. SMILES is the canonical match key, so an existing SMILES always
  #     overwrites (that is the intended "personal override" behaviour); we flag WHAT is being overwritten
  #     and warn if the name/CAS collides with a DIFFERENT compound. Editing a row (via Edit Selected) is a
  #     deliberate re-save of the same key, so it is not flagged as a clash. ---
  edit_key <- session$userData[[session$ns("expdb_edit_key")]]
  is_edit  <- !is.null(edit_key) && identical(edit_key, key)
  dball <- tryCatch(exp_db_all(), error = function(e) NULL)
  if (!is.null(dball) && nrow(dball)) {
    if (!is_edit && key %in% dball$key) {
      nm <- dball$name[match(key, dball$key)]
      if (key %in% exp_db_user_keys())
        showNotification(sprintf(i18n$t("A compound with this SMILES already exists in your data ('%s'); saving overwrites it."), nm), type = "warning", duration = 7, session = session)
      else
        showNotification(sprintf(i18n$t("This SMILES matches core-database compound '%s'; saving stores a personal override of it."), nm), type = "warning", duration = 7, session = session)
    }
    nmq <- tolower(trimws(input$add_name %||% ""))
    if (nzchar(nmq)) {
      j <- which(!is.na(dball$name) & tolower(dball$name) == nmq & dball$key != key)
      if (length(j)) showNotification(sprintf(i18n$t("Note: the name '%s' is also used by a different compound."), dball$name[j[1]]), type = "warning", duration = 7, session = session)
    }
    casq <- trimws(input$add_cas %||% "")
    if (nzchar(casq)) {
      jc <- which(!is.na(dball$cas) & grepl(paste0("(^|\\|)", casq, "(\\||$)"), dball$cas) & dball$key != key)
      if (length(jc)) showNotification(sprintf(i18n$t("Note: CAS %s is also used by a different compound ('%s')."), casq, dball$name[jc[1]]), type = "warning", duration = 7, session = session)
    }
  }
  exp_db_add(name = input$add_name %||% "", cas = input$add_cas %||% "", smiles = smi, key = key,
    Tb = input$add_Tb, Tc = input$add_Tc, Pc = input$add_Pc, Vc = input$add_Vc,
    dD = input$add_dD, dP = input$add_dP, dHB = input$add_dHB,
    Chr_k = input$add_k, Chr_A = input$add_A, Chr_B = input$add_B,
    Tm = input$add_Tm, Hvap = input$add_Hvap, dHfus = input$add_dHfus,
    category = input$add_category %||% "")
  session$userData[[session$ns("expdb_edit_key")]] <- NULL   # edit (if any) consumed
  if (close) removeModal(session)
  else {  # stay in the modal: clear the form so another compound can be added
    for (tid in c("add_name", "add_cas", "add_smiles", "add_category")) updateTextInput(session, tid, value = "")
    for (nid in c("add_Tb", "add_Tc", "add_Pc", "add_Vc", "add_Tm", "add_Hvap", "add_dHfus", "add_dD", "add_dP", "add_dHB", "add_k", "add_A", "add_B"))
      updateNumericInput(session, nid, value = NA)
  }
  showNotification(i18n$t("Compound added to your database."), type = "message", duration = 3, session = session)
  invisible(TRUE)
}

# HTML "bits" summarising the experimental data available in row `r`, for the auto-notify message.
# `props` selects which to include (e.g. EOS shows Tb/Tc; Solute Char also Pc/Vc/HSP).
exp_db_bits <- function(r, props = c("Tb", "Tc", "Pc", "Vc", "Tm", "Hvap", "dHfus", "HSP", "Chr")) {
  fmt <- list(
    Tb = function(v) sprintf("<i>T</i><sub>b</sub> %s K", round(v, 1)),
    Tc = function(v) sprintf("<i>T</i><sub>c</sub> %s K", round(v, 1)),
    Pc = function(v) sprintf("<i>P</i><sub>c</sub> %s bar", round(v, 1)),
    Vc = function(v) sprintf("<i>V</i><sub>c</sub> %s cm³/mol", round(v, 0)),
    Tm = function(v) sprintf("<i>T</i><sub>m</sub> %s K", round(v, 1)),
    Hvap = function(v) sprintf("Δ<i>H</i><sub>vap</sub> %s kJ/mol", round(v, 1)),
    dHfus = function(v) sprintf("Δ<i>H</i><sub>fus</sub> %s kJ/mol", round(v, 1)))
  out <- character(0)
  for (p in props) {
    if (identical(p, "HSP")) { if (is.finite(r$dD)) out <- c(out, "HSP") }
    else if (identical(p, "Chr")) { if (is.finite(r$Chr_k)) out <- c(out, "Chrastil <i>k/A/B</i>") }
    else { v <- suppressWarnings(as.numeric(r[[p]])); if (is.finite(v)) out <- c(out, fmt[[p]](v)) }
  }
  out
}
