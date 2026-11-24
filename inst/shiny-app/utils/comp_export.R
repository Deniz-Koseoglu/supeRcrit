# comp_export.R
# ======================================================================================================
# "Export PDF" for the Co-Solvent Comparison module. A branded report built on the reusable report_pdf.R
# toolkit (pure R; cairo_pdf/grid): intro + comparison setup (bold key-value lines) + EVERY comparison
# graph, grouped by family (analyte / pressure / temperature) exactly as shown on screen. The graphs are
# pre-built ggplots supplied by the server (reusing the module's own comp_plot_for(), so the report
# reproduces the on-screen plots), with a per-graph title. The CSV export (the raw data) stays a separate
# server downloadHandler. The caller (server) assembles the plain, already-localised `export` list.
#
# export = list(
#   setup  = list(list(label=, value=), ...),        # localised key-value pairs for the setup summary (or empty)
#   groups = list(                                    # one entry per graph family that has graphs
#     list(title = <localised group header>,
#          plots = list(list(title = <localised graph title>, plot = <ggplot/grob>), ...)),
#     ...))
# ======================================================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

build_comp_pdf_report <- function(export, i18n, file, company = "supeRcrit", logo = NULL) {
  t <- function(x) tryCatch(i18n$t(x), error = function(e) x)
  pal <- rp_palette()

  blocks <- list(rp_paragraph_block(
    t("Multi-solute co-solvent comparison for supercritical-CO2 extraction: each graph compares the selected solutes (or pressures/temperatures) against the co-solvent fraction, under the screening conditions below.")))

  # ---- Comparison setup (bold-labelled key-value lines) ----
  if (length(export$setup %||% list()))
    blocks <- c(blocks, list(rp_section_block(t("Comparison setup")), rp_keyvals_block(export$setup, size = 10)))

  # Extract the (bottom) legend grob from a ggplot — pure ggplot2/grid, no cowplot/ggpubr dependency that
  # might be absent on the deploy host. Returns a nullGrob if none found.
  ggleg <- function(p) tryCatch({
    g <- ggplot2::ggplotGrob(p)
    nm <- vapply(g$grobs, function(x) if (is.null(x$name)) "" else x$name, character(1))
    i <- grep("guide-box", nm); if (length(i)) g$grobs[[i[1]]] else grid::nullGrob()
  }, error = function(e) grid::nullGrob())

  # ---- Every comparison graph, grouped by family. Two graphs per row (2-up grid) so the square panels
  #      fill the page width; a SINGLE common legend (all plots in a family share the same colour/shape
  #      mapping) is drawn once under each row — so it's visible on every page and the per-plot bottom
  #      legends no longer collide between neighbouring panels. A trailing odd graph pairs with a blank. ----
  n_plots <- 0L
  for (g in (export$groups %||% list())) {
    pls <- Filter(function(p) !is.null(p) && !is.null(p$plot), g$plots %||% list())
    if (!length(pls)) next
    blocks <- c(blocks, list(rp_section_block(g$title %||% t("Comparison"))))
    leg <- ggleg(pls[[1]]$plot)                                   # one legend for the whole family
    panels <- lapply(pls, function(pl) tryCatch(
      ggplot2::ggplotGrob(pl$plot + ggplot2::theme(legend.position = "none")),
      error = function(e) grid::nullGrob()))
    i <- 1L
    while (i <= length(panels)) {
      pair <- gridExtra::arrangeGrob(grobs = list(panels[[i]],
                if (i + 1L <= length(panels)) panels[[i + 1L]] else grid::nullGrob()), ncol = 2)
      row <- gridExtra::arrangeGrob(pair, leg, ncol = 1,
               heights = grid::unit.c(grid::unit(1, "null"), grid::unit(0.45, "in")))
      blocks <- c(blocks, list(rp_plot_block(row, height = 3.9, shrink = TRUE, min_height = 3.0)))
      n_plots <- n_plots + 1L
      i <- i + 2L
    }
  }
  if (!n_plots)
    blocks <- c(blocks, list(rp_paragraph_block(t("No graphs available — run a comparison first."), col = pal$muted)))

  rp_render_report(file, blocks,
    header = list(company = company, title = t("Co-Solvent Comparison — Report"),
                  subtitle = format(Sys.time(), "%d-%m-%Y"), logo = logo),
    footer = list(left = format(Sys.time(), "%d-%m-%Y %H:%M"), center = "supeRcrit",
                  page_fmt = t("Page %d of %d")), palette = pal, continued = t("continued"))
}
