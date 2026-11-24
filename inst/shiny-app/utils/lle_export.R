# lle_export.R
# ======================================================================================================
# "Export PDF" for the Liquid-Liquid Extraction module — a branded report built on the reusable
# report_pdf.R toolkit (pure R; cairo_pdf/grid), matching the Co-Solvent Comparison export. The caller
# (server) passes an already-localised `export` list of sections; each section may carry a paragraph,
# pre-built ggplots (reusing the module's own plot reactives so the report reproduces the on-screen
# plots) and a data table. The CSV export (raw data) stays a separate server downloadHandler.
#
# export = list(
#   intro = "<localised intro sentence>",
#   setup = list(list(label=, value=), ...),               # localised key-value pairs (or empty)
#   sections = list(
#     list(title=, paragraph=, plots=list(list(plot=<ggplot>, title=, height=)), table=<df>, table_title=)
#   ))
# ======================================================================================================

build_lle_pdf_report <- function(export, i18n, file, company = "supeRcrit", logo = NULL) {
  t <- function(x) tryCatch(i18n$t(x), error = function(e) x)
  pal <- rp_palette()

  blocks <- list(rp_paragraph_block(export$intro %||%
    t("Liquid-liquid extraction solvent screening / flash results from the COSMO-SAC family of models.")))

  # Each section mirrors one on-screen tab: title, a per-section Solute/Feed context (tabs may differ),
  # a row of coloured KPI badges (the value boxes), the plot(s), and a table (with optional per-cell colour
  # coding `text_col`, highlighted rows `bold`, and pre-formatted character columns so digits are respected).
  n_content <- 0L
  for (sec in (export$sections %||% list())) {
    blocks <- c(blocks, list(rp_section_block(sec$title %||% "")))
    if (length(sec$context %||% list()))
      blocks <- c(blocks, list(rp_keyvals_block(sec$context, size = 9)))
    if (length(sec$kpis %||% list()))
      blocks <- c(blocks, list(rp_kpi_block(sec$kpis)))
    if (!is.null(sec$paragraph) && nzchar(sec$paragraph))
      blocks <- c(blocks, list(rp_paragraph_block(sec$paragraph, col = pal$muted)))
    if (!is.null(sec$table) && is.data.frame(sec$table) && nrow(sec$table)) {   # table first (mirrors the tabs), plot(s) below it
      blocks <- c(blocks, list(rp_table_block(sec$table, title = sec$table_title %||% NULL,
        bold = sec$bold %||% NULL, text_col = sec$text_col %||% NULL, digits = sec$digits %||% 2)))
      n_content <- n_content + 1L
    }
    for (pl in (sec$plots %||% list())) {
      if (is.null(pl) || is.null(pl$plot)) next
      blocks <- c(blocks, list(rp_plot_block(pl$plot, height = pl$height %||% 3.4, title = pl$title,
                                             shrink = TRUE, min_height = 2.6)))
      n_content <- n_content + 1L
    }
  }
  if (!n_content)
    blocks <- c(blocks, list(rp_paragraph_block(t("No results available — run the analysis first."), col = pal$muted)))

  rp_render_report(file, blocks,
    header = list(company = company, title = t("Liquid-Liquid Extraction — Report"),
                  subtitle = format(Sys.time(), "%d-%m-%Y"), logo = logo),
    footer = list(left = format(Sys.time(), "%d-%m-%Y %H:%M"), center = "supeRcrit",
                  page_fmt = t("Page %d of %d")), palette = pal, continued = t("continued"))
}
