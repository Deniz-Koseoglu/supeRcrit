# report_module.R
# ======================================================================================================
# Generic branded "Export PDF" builder on top of the report_pdf.R toolkit — a single section-based engine
# reused by every module's export (DOE Analysis, Two-Site Desorption, Broken-and-Intact Cells, Desirability,
# Components & Parameters, ...). Each module just assembles a localised `export` list; this renders it.
# (DOE Design / Phase Behaviour / LLE have their own thin wrappers for historical reasons but follow the
#  same section model.)
#
# export = list(
#   title    = "<report title shown in the green header>",
#   intro    = "<one-line summary paragraph>",
#   setup    = list(list(label=, value=), ...),        # optional top-level key-value "Setup" block
#   sections = list(
#     list(title       = "<section heading>",          # omitted/"" -> no heading rule
#          context     = list(list(label=, value=)),   # optional per-section key-value block
#          kpis        = list(list(label=, value=, color=)),   # optional coloured value-box row (rp_kpi_block)
#          paragraph   = "<grey note>",                # optional
#          table       = <data.frame>, table_title = , # optional table (+ bold/text_col/digits/header_math)
#          bold = , text_col = , digits = , header_math = ,
#          plots       = list(list(plot=<ggplot>, title=, height=))   # optional plot(s), rendered after the table
#   )))
# ======================================================================================================

build_module_pdf_report <- function(export, i18n, file, company = "supeRcrit", logo = NULL) {
  t   <- function(x) tryCatch(i18n$t(x), error = function(e) x)
  pal <- rp_palette()

  # Smart numbers (integers get no ".00") + HTML-subset → Unicode (CO₂, MPa²) on every table / key-value / badge.
  prep_tbl <- function(df, dp) { df <- rp_fmt_df(df, dp)
    df[] <- lapply(df, function(c) if (is.character(c)) rp_unhtml(c) else c); names(df) <- rp_unhtml(names(df)); df }
  prep_kv  <- function(kvs) lapply(kvs, function(k) { k$label <- rp_unhtml(k$label %||% ""); k$value <- rp_unhtml(k$value %||% ""); k })
  prep_kpi <- function(ks)  lapply(ks,  function(k) { k$label <- rp_unhtml(k$label %||% ""); k$value <- rp_unhtml(k$value %||% ""); k })

  # The intro line IS the document title — render it prominently (large, dark, bold), not muted grey.
  blocks <- list(rp_paragraph_block(export$intro %||% t("Results report."), size = 15, col = pal$ink, face = "bold"),
                 rp_spacer_block(0.08))

  if (length(export$setup %||% list()))
    blocks <- c(blocks, list(rp_section_block(t("Setup")), rp_keyvals_block(prep_kv(export$setup), size = 10)))

  n_content <- 0L
  for (sec in (export$sections %||% list())) {
    if (!is.null(sec$title) && nzchar(sec$title)) blocks <- c(blocks, list(rp_section_block(rp_unhtml(sec$title))))
    if (length(sec$context %||% list())) blocks <- c(blocks, list(rp_keyvals_block(prep_kv(sec$context), size = 9)))
    if (length(sec$kpis %||% list()))    blocks <- c(blocks, list(rp_kpi_block(prep_kpi(sec$kpis))))
    if (!is.null(sec$paragraph) && nzchar(sec$paragraph))
      blocks <- c(blocks, list(rp_paragraph_block(sec$paragraph, col = sec$paragraph_col %||% pal$muted, size = sec$paragraph_size %||% 9.5)))
    # `math` = a vector of plotmath LINE strings (e.g. a wrapped model equation) rendered in true math notation.
    # NB: use [[ ]] (exact) — `sec$math` would PARTIAL-MATCH `sec$math_col` and render the column index as text.
    if (length(sec[["math"]] %||% character(0))) {
      blocks <- c(blocks, list(rp_math_block(sec[["math"]], size = sec[["math_size"]] %||% 11, col = pal$ink)))
      n_content <- n_content + 1L
    }
    if (!is.null(sec$table) && is.data.frame(sec$table) && nrow(sec$table)) {
      blocks <- c(blocks, list(rp_table_block(prep_tbl(sec$table, sec$digits %||% 4), title = rp_unhtml(sec$table_title %||% ""),
        bold = sec$bold %||% NULL, text_col = sec$text_col %||% NULL, digits = sec$digits %||% 4,
        header_math = sec$header_math %||% NULL, body_size = sec$body_size %||% NULL, math_col = sec$math_col %||% NULL,
        cell_fill = sec$cell_fill %||% NULL, col_frac = sec$col_frac %||% NULL)))
      n_content <- n_content + 1L
    }
    # 2-column plot grid (e.g. DOE Analysis diagnostics): a list of bare ggplots, paired per row, no headings.
    grid_plots <- Filter(Negate(is.null), sec$plots_grid %||% list())
    if (length(grid_plots)) {
      for (i in seq(1, length(grid_plots), by = 2)) {
        pair <- grid_plots[i:min(i + 1L, length(grid_plots))]
        blocks <- c(blocks, list(rp_plot_grid_block(pair, height = sec$grid_height %||% 2.5, ncol = 2L)))
        n_content <- n_content + 1L
      }
    }
    for (pl in (sec$plots %||% list())) {
      if (is.null(pl) || is.null(pl$plot)) next
      blocks <- c(blocks, list(rp_plot_block(pl$plot, height = pl$height %||% 3.4, title = pl$title,
                                             shrink = TRUE, min_height = 2.4)))
      n_content <- n_content + 1L
    }
  }
  if (!n_content && !length(export$setup %||% list()))
    blocks <- c(blocks, list(rp_paragraph_block(t("No results available — run the analysis first."), col = pal$muted)))

  rp_render_report(file, blocks,
    header = list(company = company, title = export$title %||% t("Report"),
                  subtitle = format(Sys.time(), "%d-%m-%Y"), logo = logo),
    footer = list(left = format(Sys.time(), "%d-%m-%Y %H:%M"), center = "supeRcrit",
                  page_fmt = t("Page %d of %d")), palette = pal, continued = t("continued"))
}

# Shared action-row Export dropdown (CSV + PDF), gated by a `has_output` reactive output name. Returns a
# `column(width, ...)` with the disabled placeholder + live dropdown pair used across the finished modules.
# Call inside a fluidRow in the module UI: rp_export_dropdown_col(ns, i18n, "has_results", width = 4).
rp_export_dropdown_col <- function(ns, i18n, has_output, width = 4, extra_style = "") {
  cond_off <- paste0("!output.", has_output); cond_on <- paste0("output.", has_output)
  # Wrap in a width:100% div so the dropdown fills its column even when the column is a flexbox (align-items:
  # center) — otherwise the btn-group shrinks to its content width and the Export button looks tiny vs the others.
  column(width, style = extra_style,
    div(style = "width:100%;",
      conditionalPanel(condition = cond_off, ns = ns,
        tags$button(class = "btn btn-success btn-block dropdown-toggle", disabled = NA,
          style = "color:white; opacity:0.65; width:100%; margin:0;",
          icon("download"), " ", i18n$t("Export"), " ", tags$span(class = "caret"))),
      conditionalPanel(condition = cond_on, ns = ns,
        div(class = "btn-group", style = "width:100%; display:block;",
          tags$button(type = "button", class = "btn btn-success btn-block dropdown-toggle",
            `data-toggle` = "dropdown", `aria-haspopup` = "true", `aria-expanded` = "false",
            style = "color:white; width:100%; margin:0;",
            icon("download"), " ", i18n$t("Export"), " ", tags$span(class = "caret")),
          tags$ul(class = "dropdown-menu dropdown-menu-right", style = "min-width:100%;",
            tags$li(downloadLink(ns("export_pdf"), tagList(icon("file-pdf"), " ", i18n$t("Export PDF")),
              style = "padding:5px 15px; display:block;")),
            tags$li(downloadLink(ns("export_csv"), tagList(icon("file-csv"), " ", i18n$t("Export CSV")),
              style = "padding:5px 15px; display:block;")))))))
}
