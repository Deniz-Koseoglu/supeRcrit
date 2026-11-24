# cosolv_export.R
# ======================================================================================================
# "Export as PDF" for the Co-Solvent Selection module. A branded report built on the reusable report_pdf.R
# toolkit (pure R; cairo_pdf/grid) — identity + screening conditions + summary KPIs + the co-solvent
# ranking table (+ enhancing-blends table). The CSV export stays the module's existing solub_export()/
# hsp_export() download. The caller (server) assembles the plain `export` list because the sustainability
# lookup and modifier-name translation live in the module's server scope; this builder only renders.
#
# export = list(
#   identity   = list(name, cas, smiles, mw, Tm),
#   conditions = list(engine, p_lo, p_hi, t_lo, t_hi, x_pct, unit_label),   # any field may be NA/NULL
#   summary    = list(best, gain_label, gain_range, n_tested),
#   ranking    = data.frame(CoSolvent, Gain, Enh, Sol, Sustainability, Notes),  # cells pre-formatted strings
#   unit_label = "mol/mol" | "g/kg",
#   blends     = data.frame(Blend, MeanEnh, MaxEnh, MeanSol) | NULL,            # cells pre-formatted strings
#   scale_note = <string> | NULL)                                              # the absolute-solubility caption
# ======================================================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

build_cosolv_pdf_report <- function(export, i18n, file, company = "supeRcrit", logo = NULL) {
  t <- function(x) tryCatch(i18n$t(x), error = function(e) x)
  pal <- rp_palette()
  fmtv <- function(v) { v <- suppressWarnings(as.numeric(v)); if (!isTRUE(is.finite(v))) NA else formatC(v, format = "fg", digits = 4, big.mark = ",") }

  blocks <- list(rp_paragraph_block(
    t("Co-solvent selection for supercritical-CO2 extraction of the solute: each candidate co-solvent is ranked by the solubility enhancement it provides, with green-chemistry (GSK/ICH) sustainability guidance.")))

  # ---- Solute identity ----
  id <- export$identity %||% list()
  idparts <- Filter(nzchar, c(
    if (!is.null(id$name) && !is.na(id$name))   sprintf("%s: %s", t("Name"), id$name) else "",
    if (!is.null(id$cas) && !is.na(id$cas))     sprintf("%s: %s", t("CAS"), id$cas) else "",
    if (!is.null(id$mw) && isTRUE(is.finite(id$mw)))  sprintf("%s: %s g/mol", t("Molecular weight"), formatC(id$mw, format = "f", digits = 2)) else "",
    if (!is.null(id$Tm) && isTRUE(is.finite(id$Tm)))  sprintf("%s: %s K", t("Melting Point"), formatC(id$Tm, format = "f", digits = 1)) else "",
    if (!is.null(id$smiles) && !is.na(id$smiles) && nzchar(id$smiles)) sprintf("SMILES: %s", id$smiles) else ""))
  if (length(idparts))
    blocks <- c(blocks, list(rp_section_block(t("Solute")),
                             rp_paragraph_block(paste(idparts, collapse = "    •    "), size = 10, col = pal$ink)))

  # ---- Screening conditions (BOLD sub-titles, one per line) ----
  cd <- export$conditions %||% list()
  cdpairs <- Filter(Negate(is.null), list(
    if (!is.null(cd$engine) && nzchar(cd$engine)) list(label = t("Prediction engine"), value = cd$engine) else NULL,
    if (isTRUE(is.finite(cd$p_lo)) && isTRUE(is.finite(cd$p_hi))) list(label = t("Pressure grid"), value = sprintf("%g – %g bar", cd$p_lo, cd$p_hi)) else NULL,
    if (isTRUE(is.finite(cd$t_lo)) && isTRUE(is.finite(cd$t_hi))) list(label = t("Temperature grid"), value = sprintf("%g – %g °C", cd$t_lo, cd$t_hi)) else NULL,
    if (isTRUE(is.finite(cd$x_pct))) list(label = t("Co-Solvent Mole Fraction (%)"), value = sprintf("%g%%", cd$x_pct)) else NULL))
  if (length(cdpairs))
    blocks <- c(blocks, list(rp_section_block(t("Screening conditions")), rp_keyvals_block(cdpairs, size = 10)))

  # ---- Summary KPIs (mirror the on-screen badges) ----
  sm <- export$summary %||% list()
  if (length(sm)) {
    kpis <- Filter(Negate(is.null), list(
      if (!is.null(sm$best))     list(label = t("Best Co-Solvent"), value = sm$best, sub = "", color = "#2ecc71") else NULL,
      if (!is.null(sm$gain_range)) list(label = sm$gain_label %||% t("Solubility gain"), value = sm$gain_range, sub = "", color = "#3498db") else NULL,
      if (!is.null(sm$n_tested))  list(label = t("Solvents Tested"), value = as.character(sm$n_tested), sub = "", color = "#f39c12") else NULL))
    if (length(kpis)) blocks <- c(blocks, list(rp_section_block(t("Summary")), rp_kpi_block(kpis)))
  }

  cont <- t("continued")
  # ---- Co-solvent ranking table (colnames localised by caller; bold top row; widen & colour Sustainability) ----
  rk <- export$ranking
  if (!is.null(rk) && nrow(rk)) {
    bold <- c(TRUE, rep(FALSE, nrow(rk) - 1))          # highlight the best-ranked co-solvent
    blocks <- c(blocks, list(rp_section_block(t("Co-solvent ranking")),
                             rp_table_block(rk, pal = pal, bold = bold, continued = cont,
                                            col_frac = export$ranking_col_frac, text_col = export$ranking_text_col)))
    if (!is.null(export$scale_note))
      blocks <- c(blocks, list(rp_paragraph_block(export$scale_note, size = 8, col = pal$muted, keep_prev = TRUE)))
  }

  # ---- Best-co-solvent-by-condition grid (heatmap image) ----
  if (inherits(export$grid_plot, "ggplot") || inherits(export$grid_plot, "grob") || inherits(export$grid_plot, "gtable"))
    blocks <- c(blocks, list(rp_section_block(t("Best co-solvent at each condition")),
                             rp_plot_block(export$grid_plot, height = 3.6)))

  # ---- Recommended enhancing blends (colnames already localised by the caller) ----
  bl <- export$blends
  if (!is.null(bl) && nrow(bl)) {
    blocks <- c(blocks, list(rp_section_block(t("Recommended enhancing blends")),
                             rp_table_block(bl, pal = pal, continued = cont),
                             rp_paragraph_block(
                               t("Each blend is an equal-molar (50:50) mixture of its two co-solvents, evaluated at the same total co-solvent mole fraction as the single-co-solvent screen."),
                               size = 8, col = pal$muted, keep_prev = TRUE)))
  }

  rp_render_report(file, blocks,
    header = list(company = company, title = t("Co-Solvent Selection — Report"),
                  subtitle = format(Sys.time(), "%d-%m-%Y"), logo = logo),
    footer = list(left = format(Sys.time(), "%d-%m-%Y %H:%M"), center = "supeRcrit",
                  page_fmt = t("Page %d of %d")), palette = pal, continued = t("continued"))
}
