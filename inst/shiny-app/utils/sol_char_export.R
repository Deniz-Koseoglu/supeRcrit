# sol_char_export.R
# ======================================================================================================
# "Export Results" (PDF / CSV) for the Solute Characterization module. Mirrors the Cost Analysis approach:
# a single builder turns the module's result object (sfe_rv$gcm_results) into localised, plain-text tables
# that feed BOTH the PDF (via the reusable report_pdf.R toolkit) and the CSV, so the two can never diverge.
# Pure R (cairo_pdf/grid) — no LaTeX/pandoc — so it deploys everywhere the app does.
# ======================================================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

# ---- Group-highlight colour palette + per-(property, order) group-ID → colour map ---------------------
# The 15-colour highlight palette, in the EXACT order plot_gcm()/jcol_vals assigns it, so a table swatch for
# group ID g matches that group's highlight colour in the decomposition image (visres[[property]][[order]]).
solchar_jhex <- c("#C0C0C0", "#FF0000", "#00FF00", "#00FFFF", "#FFAFAF", "#FFC800", "#FFFF00",
                  "#FF00FF", "#0000FF", "#990099", "#336600", "#660000", "#99FFCC", "#808080", "#404040")

# For each property/method, compute the per-group-ORDER colour map — a named character vector keyed by group
# ID (as character), value = hex — assigned in the SAME order the image used, i.e. the position of the group
# ID within names(.frag_group_atoms(iface, method, order)). The decomposition image colours its k-th group
# jcol_vals[k], so joining a Contribs row to its colour BY ID (not row position) reproduces the image colour.
# Returns list[[property]][[order_str]] = c(ID → hex). `methods` = named property → GCM method string.
# `orders_by_prop` (optional) = named property → character orders present (from names(visres[[prop]])).
solchar_group_colmaps <- function(solute_data, methods, orders_by_prop = NULL) {
  if (is.null(solute_data) || is.null(methods) || !length(methods)) return(list())
  ids <- tryCatch(solute_data$IDs, error = function(e) NULL); if (is.null(ids)) return(list())
  sm <- suppressWarnings(unname(ids["SMILES"])); if (length(sm) != 1 || is.na(sm) || !nzchar(sm)) return(list())
  draw_mol <- tryCatch(depict::parse_smiles(paste0(ids["SMILES"], " ", ids["Name"], " (", ids["CAS"], ")")),
                       error = function(e) NULL); if (is.null(draw_mol)) return(list())
  iface <- tryCatch(rJava::.jcast(draw_mol, "org/openscience/cdk/interfaces/IAtomContainer"),
                    error = function(e) NULL); if (is.null(iface)) return(list())
  out <- list()
  for (P in names(methods)) {
    m <- methods[[P]]; m <- if (length(m)) as.character(m)[1] else NA_character_
    if (is.na(m) || !nzchar(m) || m %in% c("none", "manual")) next
    ords <- orders_by_prop[[P]]; if (is.null(ords) || !length(ords)) ords <- as.character(1:3)
    pm <- list()
    for (o in ords) {
      ga <- tryCatch(supeRcrit:::.frag_group_atoms(iface, m, as.integer(o)), error = function(e) list())
      if (length(ga) && !is.null(names(ga)))
        pm[[as.character(o)]] <- stats::setNames(solchar_jhex[((seq_along(ga) - 1L) %% length(solchar_jhex)) + 1L], names(ga))
    }
    if (length(pm)) out[[P]] <- pm
  }
  out
}
# Look up the highlight colour for one Contribs row (by its ID + Type/order) in a property's colour map.
# cm_prop = colmaps[[property]] (list order_str → c(ID→hex)). Returns "" when the group has no highlight
# (e.g. interaction/whole-molecule rows whose ID is absent from the fragmentation partition).
solchar_row_color <- function(cm_prop, id, type) {
  if (is.null(cm_prop) || !length(cm_prop)) return("")
  o <- as.character(suppressWarnings(as.integer(type))); if (is.na(o) || !nzchar(o)) o <- names(cm_prop)[1]
  m <- cm_prop[[o]]; if (is.null(m)) return("")
  key <- as.character(id)
  # m is a NAMED CHARACTER VECTOR: `m[[key]]` ERRORS if key is absent (unlike a list) — guard with %in%.
  if (length(key) != 1 || is.na(key) || !key %in% names(m)) return("")
  v <- unname(m[[key]]); if (is.null(v) || is.na(v)) "" else v
}

# ---- Assemble the export tables from a gcm_results object ---------------------------------------------
# Returns list(identity=list(name,cas,smiles,mw), sol_ref=list(T,P), params=<data.frame Category|Parameter|
# Value|Unit|Source>, groups=<named list of per-method contribution data.frames>). All text localised via
# `i18n`. Value cells are display strings (scientific for very small/large magnitudes). Mirrors the
# on-screen "Predicted Parameters" table assembly (phase -> critical -> HSP -> other -> Chrastil) but plain.
solchar_export_tables <- function(gcm_results, i18n) {
  t <- function(x) tryCatch(i18n$t(x), error = function(e) x)
  if (is.null(gcm_results)) return(NULL)
  pares <- gcm_results$pares
  ph    <- gcm_results$phase
  ids   <- gcm_results$solute_data$IDs
  id_get <- function(k) { v <- suppressWarnings(unname(ids[k])); if (length(v) && !is.na(v) && nzchar(as.character(v))) as.character(v) else NA_character_ }
  mw     <- suppressWarnings(as.numeric(ids["MW"])[1])

  exp_src <- gcm_results$exp_src %||% character(0)
  db_src  <- gcm_results$db_src  %||% character(0)
  domain  <- pares$Domain

  # plain label + unit per raw parameter id (matches the on-screen param_formatting, HTML stripped)
  LAB <- list(
    Tb = t("Boiling Point"), Tm = t("Melting Point"), Hvap = t("Heat of Vaporization"),
    Hfus = t("Fusion Enthalpy"), dS_tpce = t("Total Phase Change Entropy"),
    Cp_l = t("Liquid Heat Capacity"), Cp_s = t("Solid Heat Capacity"),
    Tc = t("Critical Temperature"), Pc = t("Critical Pressure"), Vc = t("Critical Volume"),
    dD = paste0(t("Dispersion"), " (δD)"), dP = paste0(t("Polarity"), " (δP)"),
    dHB = paste0(t("Hydrogen Bonding"), " (δHB)"), VDW = t("VDW Volume"),
    chrastil_k = paste0(t("Chrastil density exponent"), " (k)"),
    chrastil_A = paste0(t("Chrastil temperature coeff."), " (A)"),
    chrastil_B = paste0(t("Chrastil intercept"), " (B)"),
    chrastil_sol_molmol = t("scCO₂ solubility (Chrastil)"),
    chrastil_sol_gL     = t("scCO₂ solubility (Chrastil)"))
  UNIT <- list(Tb = "K", Tm = "K", Hvap = "kJ/mol", Hfus = "kJ/mol", dS_tpce = "J/mol·K",
    Cp_l = "J/mol·K", Cp_s = "J/mol·K", Tc = "K", Pc = "bar", Vc = "cm³/mol",
    dD = "√MPa", dP = "√MPa", dHB = "√MPa", VDW = "L/mol",
    chrastil_k = "", chrastil_A = "K", chrastil_B = "", chrastil_sol_molmol = "mol/mol", chrastil_sol_gL = "g/L")

  # plain-text provenance of a value, mirroring the on-screen Reliability column
  source_of <- function(pn, val) {
    if (pn %in% exp_src || pn %in% db_src || (grepl("^chrastil_", pn) && "Chr" %in% db_src)) return(t("experimental (DB)"))
    if (pn %in% c("Tm", "Hvap")) {
      src <- if (pn == "Tm") ph$Tm_src else ph$Hvap_src
      if (identical(src, "manual")) return(t("user value"))
      if (identical(src, "gcm_hvap_chickos")) return(t("GCM estimate"))
      return(t("ML estimate"))
    }
    if (pn %in% c("Hfus", "dS_tpce", "Cp_l", "Cp_s")) {
      if (identical(ph$Hfus_src, "manual")) return(t("user value"))
      if (identical(ph$Hfus_tier %||% "fair", "poor")) return(t("Rough"))
      return(t("GCM estimate"))
    }
    if (grepl("^chrastil_", pn)) return(t("user value"))
    bad <- if (!is.null(domain) && !is.null(domain[[pn]])) domain[[pn]] else NULL
    if (length(bad)) return(t("Rough"))
    if (!isTRUE(is.finite(val))) return(t("Not estimated"))
    t("GCM estimate")
  }
  fmt_val <- function(x) { x <- suppressWarnings(as.numeric(x)[1])
    if (!isTRUE(is.finite(x))) return("–")
    ax <- abs(x); if (ax != 0 && (ax < 1e-2 || ax >= 1e6)) formatC(x, format = "e", digits = 3) else sprintf("%.2f", x) }

  rows <- list()
  add <- function(cat, pn, val) rows[[length(rows) + 1]] <<- data.frame(
    Category = cat, Parameter = LAB[[pn]] %||% pn, Value = fmt_val(val),
    Unit = UNIT[[pn]] %||% "", Source = source_of(pn, suppressWarnings(as.numeric(val)[1])),
    stringsAsFactors = FALSE, check.names = FALSE)

  crit_all <- pares$Critical
  # Phase-Transition
  cP <- t("Phase-Transition Properties")
  tb_val <- if (!is.null(crit_all) && "Tb" %in% names(crit_all)) crit_all[["Tb"]] else NA_real_
  if (isTRUE(is.finite(tb_val)))                              add(cP, "Tb", tb_val)
  if (!is.null(ph) && isTRUE(is.finite(ph$Tm)))              add(cP, "Tm", ph$Tm)
  if (!is.null(ph) && isTRUE(is.finite(ph$Hvap)))            add(cP, "Hvap", ph$Hvap)
  if (!is.null(ph) && isTRUE(is.finite(ph$Hfus)))            add(cP, "Hfus", ph$Hfus)
  if (!is.null(ph) && isTRUE(is.finite(ph$dS_tpce)))         add(cP, "dS_tpce", ph$dS_tpce)
  if (!is.null(ph) && isTRUE(is.finite(ph$Cp_l)))            add(cP, "Cp_l", ph$Cp_l)
  if (!is.null(ph) && isTRUE(is.finite(ph$Cp_s)))            add(cP, "Cp_s", ph$Cp_s)
  # Critical (minus Tb)
  cC <- t("Critical Parameters")
  crit_keep <- if (!is.null(crit_all)) crit_all[setdiff(names(crit_all), "Tb")] else NULL
  for (nm in names(crit_keep)) if (isTRUE(is.finite(crit_keep[[nm]]))) add(cC, nm, crit_keep[[nm]])
  # HSP
  cH <- t("Hansen Solubility Parameters")
  if (!is.null(pares$HSP)) for (nm in names(pares$HSP)) if (isTRUE(is.finite(suppressWarnings(as.numeric(pares$HSP[[nm]]))))) add(cH, nm, pares$HSP[[nm]])
  # Other (VDW only — Makarov ML solubility removed)
  cO <- t("Other Parameters")
  if (!is.null(pares$VDW)) add(cO, "VDW", pares$VDW)
  # Chrastil params + derived solubility at the reference T/P
  rT <- gcm_results$sol_ref$T; rT <- if (length(rT) && isTRUE(is.finite(rT))) rT else 40
  rP <- gcm_results$sol_ref$P; rP <- if (length(rP) && isTRUE(is.finite(rP))) rP else 200
  if (!is.null(gcm_results$chrastil)) {
    cK <- t("Chrastil Solubility Parameters"); ch <- gcm_results$chrastil
    add(cK, "chrastil_k", ch$k); add(cK, "chrastil_A", ch$A); add(cK, "chrastil_B", ch$B)
    sol <- tryCatch(chrastil_solubility(ch$k, ch$A, ch$B, rT, rP, mw = if (isTRUE(is.finite(mw))) mw else NULL),
                    error = function(e) NULL)
    if (!is.null(sol)) {
      add(cK, "chrastil_sol_molmol", sol$y_molmol[1])
      add(cK, "chrastil_sol_gL", if (!is.null(sol$S_gL)) sol$S_gL[1] else NA_real_)
    }
  }
  params <- if (length(rows)) do.call(rbind, rows) else NULL

  # Group-contribution tables per GCM method (+ the Chrastil per-group k/A/B, appended on a separate path,
  # exactly like the on-screen Group Contributions tab).
  groups <- pares$Contribs
  if (!is.list(groups)) groups <- list()
  if (!is.null(gcm_results$chrastil$Contribs)) groups$Chrastil <- gcm_results$chrastil$Contribs

  # Per-(property, order) group → highlight-colour maps (match the decomposition images) + the images
  # themselves (visres), so the PDF can place each decomposition image above its table and colour-code the
  # Group column exactly like the on-screen Group Contributions tab.
  ob      <- lapply(gcm_results$visres %||% list(), function(v) if (is.list(v)) names(v) else NULL)
  colmaps <- tryCatch(solchar_group_colmaps(gcm_results$solute_data, pares$Methods, ob), error = function(e) list())
  images  <- gcm_results$visres %||% list()

  list(
    identity = list(name = id_get("Name"), cas = id_get("CAS"), smiles = id_get("SMILES"),
                    mw = if (isTRUE(is.finite(mw))) mw else NA_real_),
    sol_ref  = list(T = rT, P = rP),
    params   = params,
    groups   = groups,
    colmaps  = colmaps,
    images   = images)
}

# ---- Group-contribution tables (plain, localised) for the PDF/CSV export ------------------------------
# Turns export$groups (named list of per-method contribution data.frames from the GCM engine) into a list
# of list(title=<translated section name>, df=<data.frame with translated, plain headers>). Column ids like
# "Tb_cont"/"Sum_Tb_cont" become "Tb"/"ΣTb"; Group/ID/Type/Occurrences are translated. Empty -> list().
solchar_group_tables <- function(groups, i18n, colmaps = NULL) {
  t <- function(x) tryCatch(i18n$t(x), error = function(e) x)
  if (is.null(groups) || !length(groups)) return(list())
  sect   <- list(Tb = t("Boiling Point"), Critical = t("Critical Parameters"),
                 HSP = t("Hansen Solubility Parameters"), VDW = t("VDW Volume"),
                 Chrastil = t("Chrastil Solubility Parameters"))
  common <- list(Group = t("Functional Group Name"), ID = t("Group Identifier"),
                 Type = t("Group Order"), Occurrences = t("Number of Occurrences"))
  collab <- function(col) {
    if (!is.null(common[[col]])) return(common[[col]])
    if (grepl("^Sum_.+_cont$", col)) return(paste0("Σ", sub("^Sum_(.+)_cont$", "\\1", col)))
    if (grepl("^.+_cont$", col))     return(sub("^(.+)_cont$", "\\1", col))
    col
  }
  # Keep only the columns relevant to THIS property (mirrors clean_cols() on the Group Contributions tab):
  # Group/ID/Type/Occurrences + that property's single-group and Σ contribution columns. The Müller-engine
  # table also carries SMARTS/smarts/pr/id/name and every OTHER property's contributions; including all of
  # them produced a far-too-wide table whose cells overlapped/scrambled in the PDF.
  keep_by <- list(Tb = "Tb", Critical = c("Tc", "Pc", "Vc"), HSP = c("dD", "dP", "dHB"),
                  VDW = "VDW", Chrastil = c("k", "A", "B"))
  # plotmath symbol per contribution key (language-independent) — gives the subscripts/Greek shown in the
  # app (T_b, P_c, δ_D, italic k/A/B). Expressions are BOLD (plotmath ignores the theme's fontface="bold",
  # so bold() / bolditalic() must be baked in to match the other, bold, table headers). The Σ (cumulative)
  # variant prefixes bold Sigma.
  cmath <- c(Tb = "bolditalic(T)[b]", Tc = "bolditalic(T)[c]", Pc = "bolditalic(P)[c]", Vc = "bolditalic(V)[c]",
             dD = "bold(delta[D])", dP = "bold(delta[P])", dHB = "bold(delta[HB])", VDW = 'bold("VDW")',
             k = "bolditalic(k)", A = "bolditalic(A)", B = "bolditalic(B)")
  # Turn a (translated) plain header into a BOLD plotmath label, wrapping to ~2 stacked lines via atop() so
  # long multi-word titles don't run off the column (plotmath can't word-wrap on its own).
  plain_math <- function(txt) {
    txt <- gsub('"', "", as.character(txt))
    lines <- strwrap(txt, width = 13); if (!length(lines)) lines <- txt
    blin <- function(s) paste0('bold("', s, '")')
    if (length(lines) == 1) return(blin(lines))
    expr <- blin(lines[length(lines)])
    for (i in rev(seq_len(length(lines) - 1))) expr <- paste0("atop(", blin(lines[i]), ",", expr, ")")
    expr
  }
  col_math <- function(col) {
    if (grepl("^Sum_(.+)_cont$", col)) { p <- sub("^Sum_(.+)_cont$", "\\1", col); return(paste0("bold(Sigma)*", cmath[[p]] %||% paste0('bold("', p, '")'))) }
    if (grepl("^(.+)_cont$", col))     { p <- sub("^(.+)_cont$", "\\1", col);     return(cmath[[p]] %||% paste0('bold("', p, '")')) }
    plain_math(collab(col))
  }
  out <- list()
  for (k in names(groups)) {
    df <- groups[[k]]
    if (is.null(df) || !is.data.frame(df) || !nrow(df)) next
    # Per-row Group highlight colour (by order + ID), matching the decomposition image — captured from the FULL
    # table before columns are dropped. "" where a row has no spatial highlight (interaction/whole-molecule).
    cmp <- if (!is.null(colmaps)) colmaps[[k]] else NULL
    gfill <- if (!is.null(cmp) && "ID" %in% names(df))
      vapply(seq_len(nrow(df)), function(i) solchar_row_color(cmp, df$ID[i], if ("Type" %in% names(df)) df$Type[i] else 1L), character(1))
    else rep("", nrow(df))
    pref <- keep_by[[k]]
    keep <- c("Group", "ID", "Type", "Occurrences")
    if (!is.null(pref)) keep <- c(keep, paste0(pref, "_cont"), paste0("Sum_", pref, "_cont"))
    sel <- intersect(keep, names(df))
    if (length(sel)) df <- df[, sel, drop = FALSE]     # unknown methods: fall back to the full table
    sel <- names(df)
    # Group Identifier / Group Order / Number of Occurrences are counts — show as integers, not x.00.
    for (ic in c("ID", "Type", "Occurrences"))
      if (ic %in% sel && is.numeric(df[[ic]])) df[[ic]] <- as.character(round(df[[ic]]))
    hmath <- vapply(sel, col_math, character(1))       # plotmath header labels (PDF); plain labels drive the CSV
    colnames(df) <- vapply(sel, collab, character(1))
    out[[length(out) + 1]] <- list(title = sect[[k]] %||% k, df = df, header_math = hmath,
                                   key = k, group_fill = gfill)
  }
  out
}

# ---- PDF report --------------------------------------------------------------------------------------
# Branded PDF (report_pdf.R): solute identity + one table per property category (Parameter|Value|Unit|
# Source), a reference-state note for the Chrastil solubility, and a provenance-legend footnote.
build_solchar_pdf_report <- function(export, i18n, file, company = "supeRcrit", logo = NULL) {
  t <- function(x) tryCatch(i18n$t(x), error = function(e) x)
  pal <- rp_palette()
  blocks <- list(rp_paragraph_block(
    t("Group-contribution and machine-learning property estimates for the selected solute, used to parameterise the supercritical-extraction models. Each value is tagged with its provenance (experimental database, GCM, ML, or user input).")))

  # Solute identity — one bold-labelled field per line (SMILES wraps if long).
  id <- export$identity
  idpairs <- Filter(Negate(is.null), list(
    if (!is.na(id$name %||% NA))   list(label = t("Name"), value = id$name) else NULL,
    if (!is.na(id$cas %||% NA))    list(label = t("CAS"), value = id$cas) else NULL,
    if (isTRUE(is.finite(id$mw)))  list(label = t("Molecular weight"), value = sprintf("%s g/mol", formatC(id$mw, format = "f", digits = 2))) else NULL,
    if (!is.na(id$smiles %||% NA)) list(label = "SMILES", value = id$smiles) else NULL))
  if (length(idpairs))
    blocks <- c(blocks, list(rp_section_block(t("Solute identity")),
                             rp_keyvals_block(idpairs, size = 10)))

  # One table per property category, in card order
  if (!is.null(export$params) && nrow(export$params)) {
    cont <- t("continued")
    hdr  <- c(t("Parameter"), t("Value"), t("Unit"), t("Source"))
    for (cat in unique(export$params$Category)) {
      sub <- export$params[export$params$Category == cat, c("Parameter", "Value", "Unit", "Source"), drop = FALSE]
      colnames(sub) <- hdr
      # bold rows whose value is an experimental DB measurement (the most trustworthy)
      bold <- export$params$Source[export$params$Category == cat] == t("experimental (DB)")
      blocks <- c(blocks, list(rp_section_block(cat), rp_table_block(sub, pal = pal, bold = bold, continued = cont)))
    }
    # reference-state note when a Chrastil solubility is present
    if (any(grepl("Chrastil", export$params$Category)))
      blocks <- c(blocks, list(rp_paragraph_block(
        sprintf(t("Chrastil scCO₂ solubility is evaluated at %g °C and %g bar."), export$sol_ref$T, export$sol_ref$P),
        size = 9, col = pal$muted, keep_prev = TRUE)))
    blocks <- c(blocks, list(rp_paragraph_block(
      t("Source legend: experimental (DB) = measured value from the database; GCM estimate = group-contribution prediction; ML estimate = machine-learning prediction; user value = entered manually; Rough = uses uncalibrated groups, treat with caution."),
      size = 8, col = pal$muted, keep_prev = TRUE)))
  }

  # Group-contribution breakdown (one titled table per GCM method), when present — each preceded by its
  # group-decomposition image(s) and with the Group column colour-coded to match the image highlights.
  gt <- solchar_group_tables(export$groups, i18n, export$colmaps)
  if (length(gt)) {
    cont <- t("continued")
    # Pale tint of a highlight colour (a saturated full-cell fill would be unreadable; the tint keeps the hue).
    lighten <- function(hex, f = 0.72) { if (is.null(hex) || is.na(hex) || !nzchar(hex)) return("")
      rc <- tryCatch(grDevices::col2rgb(hex), error = function(e) NULL); if (is.null(rc)) return("")
      v <- round(rc[, 1] * (1 - f) + 255 * f); grDevices::rgb(v[1], v[2], v[3], maxColorValue = 255) }
    blocks <- c(blocks, list(rp_section_block(t("Group Contributions"))))
    for (g in gt) {
      # One decomposition image per table — the primary (lowest-order) partition — kept with its table. The
      # section title (e.g. "Boiling Point") is placed ON the image block so it renders ABOVE the structure
      # image (not between the image and the table); the table then carries no title.
      imgs <- export$images[[g$key]]
      im <- if (is.list(imgs) && length(imgs)) imgs[[names(imgs)[1]]] else if (!is.null(imgs) && !is.list(imgs)) imgs else NULL
      has_img <- FALSE
      if (!is.null(im)) {
        rg <- tryCatch(grid::rasterGrob(im, width = grid::unit(2.7, "in"), height = grid::unit(2.7, "in"), interpolate = TRUE), error = function(e) NULL)
        if (!is.null(rg)) { blocks <- c(blocks, list(rp_plot_block(rg, height = 2.8, title = g$title, pal = pal, shrink = TRUE, min_height = 2.2, keep_next = TRUE))); has_img <- TRUE }
      }
      cf <- NULL; gf <- g$group_fill
      if (!is.null(gf) && length(gf) == nrow(g$df) && any(nzchar(gf))) {
        cf <- matrix("", nrow(g$df), ncol(g$df)); cf[, 1] <- vapply(gf, lighten, character(1))
      }
      blocks <- c(blocks, list(rp_table_block(g$df, title = if (has_img) NULL else g$title, pal = pal, continued = cont, header_math = g$header_math, cell_fill = cf)))
    }
  }

  rp_render_report(file, blocks,
    header = list(company = company, title = t("Solute Characterization — Report"),
                  subtitle = format(Sys.time(), "%d-%m-%Y"), logo = logo),
    footer = list(left = format(Sys.time(), "%d-%m-%Y %H:%M"), center = "supeRcrit",
                  page_fmt = t("Page %d of %d")), palette = pal, continued = t("continued"))
}

# ---- CSV export --------------------------------------------------------------------------------------
# UTF-8 (BOM) CSV: a small identity header block, the full parameter table (Category|Parameter|Value|Unit|
# Source), then the per-method group-contribution tables. All headers and cell text are localised. Uses the
# SAME assembled tables as the PDF so the two exports can never diverge.
solchar_write_csv <- function(export, i18n, file) {
  t <- function(x) tryCatch(i18n$t(x), error = function(e) x)
  id <- export$identity
  con <- file(file, "wb")
  on.exit(close(con), add = TRUE)
  writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con)                      # UTF-8 BOM (Excel-friendly, like the COM export)
  wln <- function(txt) writeBin(charToRaw(paste0(txt, "\r\n")), con)
  # append a data.frame as UTF-8 CSV rows (without re-adding a BOM)
  wdf <- function(df) {
    tmp <- tempfile(); on.exit(unlink(tmp), add = TRUE)
    utils::write.csv(df, tmp, row.names = FALSE, fileEncoding = "UTF-8")
    body <- readBin(tmp, "raw", file.info(tmp)$size)
    if (length(body) >= 3 && all(body[1:3] == as.raw(c(0xEF, 0xBB, 0xBF)))) body <- body[-(1:3)]
    writeBin(body, con)
  }
  # identity block
  wln(t("Solute Characterization Results"))
  wln(paste0(t("Generated"), ",", format(Sys.time(), "%d-%m-%Y %H:%M")))
  wln(paste0(t("Name"), ",", id$name %||% ""))
  wln(paste0(t("CAS"), ",", id$cas %||% ""))
  wln(paste0("SMILES,", id$smiles %||% ""))
  wln(paste0(t("Molecular weight"), ",", if (isTRUE(is.finite(id$mw))) formatC(id$mw, format = "f", digits = 2) else ""))
  wln(paste0(t("Chrastil reference state"), ",", sprintf("%g °C / %g bar", export$sol_ref$T, export$sol_ref$P)))
  wln("")
  # predicted parameters
  if (!is.null(export$params) && nrow(export$params)) {
    df <- export$params
    colnames(df) <- c(t("Category"), t("Parameter"), t("Value"), t("Unit"), t("Source"))
    wdf(df)
  }
  # group contributions (one section per method)
  gt <- solchar_group_tables(export$groups, i18n)
  if (length(gt)) {
    wln(""); wln(t("Group Contributions"))
    for (g in gt) { wln(""); wln(g$title); wdf(g$df) }
  }
  invisible(file)
}
