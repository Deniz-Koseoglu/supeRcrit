# eos_phase_export.R
# ======================================================================================================
# "Export PDF" for the Phase Behaviour (GC-EOS flash / P-x,y / critical-locus) module — a branded report
# on the reusable report_pdf.R toolkit, mirroring the on-screen output: the calculation conditions, the
# flash-equilibrium phase table, and (for binary mixtures) the P-x,y envelope and critical-locus plots.
#
# export = list(
#   intro        = "<localised one-line summary>",
#   setup        = list(list(label=, value=), ...),   # components / T / P / model / basis
#   flash_df     = <data.frame or NULL>,              # phase compositions (localised headers)
#   flash_title  = "<e.g. Two phases (vapour-liquid)>",
#   plots        = list(list(plot=<ggplot>, title=, height=), ...)   # pxy + locus ggplot twins (binary only)
# )
# ======================================================================================================

build_eos_phase_pdf_report <- function(export, i18n, file, company = "supeRcrit", logo = NULL) {
  t   <- function(x) tryCatch(i18n$t(x), error = function(e) x)
  pal <- rp_palette()

  blocks <- list(rp_paragraph_block(export$intro %||%
    t("Phase-equilibrium results from the GC-EOS / GCA-EOS engine."), size = 15, col = pal$ink, face = "bold"),
    rp_spacer_block(0.08))

  prep_kv <- function(kvs) lapply(kvs, function(k) { k$label <- rp_unhtml(k$label %||% ""); k$value <- rp_unhtml(k$value %||% ""); k })
  if (length(export$setup %||% list()))
    blocks <- c(blocks, list(rp_section_block(t("Conditions")), rp_keyvals_block(prep_kv(export$setup), size = 10)))

  if (is.data.frame(export$flash_df) && nrow(export$flash_df)) {
    fd <- rp_fmt_df(export$flash_df, 5); fd[] <- lapply(fd, function(c) if (is.character(c)) rp_unhtml(c) else c); names(fd) <- rp_unhtml(names(fd))
    blocks <- c(blocks, list(rp_section_block(t("Flash equilibrium")),
      rp_table_block(fd, title = export$flash_title %||% t("Phase compositions"), digits = 5)))
  }

  for (pl in (export$plots %||% list())) {
    if (is.null(pl) || is.null(pl$plot)) next
    blocks <- c(blocks, list(rp_section_block(pl$title %||% ""),
      rp_plot_block(pl$plot, height = pl$height %||% 3.6, shrink = TRUE, min_height = 2.6)))
  }

  if (length(blocks) <= 1)
    blocks <- c(blocks, list(rp_paragraph_block(t("No results — run a calculation first."), col = pal$muted)))

  rp_render_report(file, blocks,
    header = list(company = company, title = t("Phase Behaviour — Report"),
                  subtitle = format(Sys.time(), "%d-%m-%Y"), logo = logo),
    footer = list(left = format(Sys.time(), "%d-%m-%Y %H:%M"), center = "supeRcrit",
                  page_fmt = t("Page %d of %d")), palette = pal, continued = t("continued"))
}
