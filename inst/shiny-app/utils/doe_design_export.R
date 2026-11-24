# doe_design_export.R
# ======================================================================================================
# "Export PDF" for the DOE Design module — a branded report built on the reusable report_pdf.R toolkit
# (pure R; cairo_pdf/grid), mirroring the on-screen output: the design description, a key-value summary of
# the design (type / runs / factors / centre points / randomised), the full design matrix table, and — for
# fractional-factorial designs — the confounding (alias) pattern. The CSV export stays a separate handler.
#
# export = list(
#   intro        = "<localised design description>",
#   matrix_name  = "<e.g. BBD 3^3>",
#   setup        = list(list(label=, value=), ...),   # localised key-value pairs
#   design_df    = <data.frame, localised column names>,
#   confounding  = <data.frame or NULL>
# )
# ======================================================================================================

build_doe_design_pdf_report <- function(export, i18n, file, company = "supeRcrit", logo = NULL) {
  t   <- function(x) tryCatch(i18n$t(x), error = function(e) x)
  pal <- rp_palette()

  blocks <- list(rp_paragraph_block(export$intro %||%
    t("Design of experiments — generated design matrix."), size = 15, col = pal$ink, face = "bold"),
    rp_spacer_block(0.08))

  if (length(export$setup %||% list()))
    blocks <- c(blocks, list(rp_section_block(t("Design summary")), rp_keyvals_block(export$setup, size = 10)))

  if (is.data.frame(export$design_df) && nrow(export$design_df)) {
    # Use the full design DESCRIPTION as the matrix title (e.g. "Optimization Box-Behnken design 3^3 (BBD)
    # with 21 runs"); shrink the title font when long so it stays on one line.
    ttl <- rp_unhtml(export$intro %||% export$matrix_name %||% t("Design matrix"))
    tsize <- if (nchar(ttl) > 60) 9 else if (nchar(ttl) > 46) 10 else 11
    blocks <- c(blocks, list(rp_section_block(t("Design matrix")),
      rp_table_block(rp_fmt_df(export$design_df, 4), title = ttl, title_size = tsize, digits = 4)))
  }

  if (is.data.frame(export$confounding) && nrow(export$confounding))
    blocks <- c(blocks, list(rp_section_block(t("Confounding pattern")),
      rp_table_block(rp_fmt_df(export$confounding, 4), title = t("Effect / alias structure"))))

  if (length(blocks) <= 1)
    blocks <- c(blocks, list(rp_paragraph_block(t("No design available — generate a design first."), col = pal$muted)))

  rp_render_report(file, blocks,
    header = list(company = company, title = t("Design of Experiments — Design Report"),
                  subtitle = format(Sys.time(), "%d-%m-%Y"), logo = logo),
    footer = list(left = format(Sys.time(), "%d-%m-%Y %H:%M"), center = "supeRcrit",
                  page_fmt = t("Page %d of %d")), palette = pal, continued = t("continued"))
}
