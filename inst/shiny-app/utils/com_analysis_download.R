# com_analysis_download.R

# ---- Auxiliary-material description localisation (shared by every COM translation path) --------------
# Dynamic auxiliary-material rows carry descriptions of the form "<prefix>: <material name>", where the
# material name is arbitrary user text (any language) and must NEVER be used as a translation key: doing
# so leaves it unlocalised AND emits shiny.i18n "translation does not exist" warnings. `com_translate_desc`
# localises any description string safely:
#   1. prefer a full bundled translation when one exists (covers the default "...: oil" -> "...: yağ");
#   2. otherwise, for an auxiliary row, translate only the recognised prefix and keep the user name verbatim;
#   3. otherwise translate the string normally.
# `t` is a function taking one English string and returning its localised form (unchanged if none). The
# full-string probe is wrapped so the missing-translation warning never fires for user-named materials.
com_aux_desc_prefixes <- c("Price of auxiliary material", "Required fraction of auxiliary material")
com_translate_desc <- function(d, t) {
  if (length(d) != 1 || is.na(d)) return(d)
  full <- tryCatch(suppressWarnings(t(d)), error = function(e) d)
  if (!identical(full, d)) return(full)
  for (pfx in com_aux_desc_prefixes) {
    tag <- paste0(pfx, ": ")
    if (startsWith(d, tag))
      return(paste0(tryCatch(suppressWarnings(t(pfx)), error = function(e) pfx), ": ",
                    substring(d, nchar(tag) + 1L)))
  }
  d
}

# ---- Per-product financial COMPARISON matrix (shared by the on-screen UI, the PDF export and the CSV) --
# One scannable matrix comparing every product's economics, plus — for the PRIMARY product only — a
# hypothetical "if produced alone" row that loads the ENTIRE cost of manufacturing (COM) onto it (so the
# reader sees how co-producing the by-products improves the primary's specific cost / margin).
# Manufacturing cost is apportioned across products by REVENUE share (not extract mass). Rows:
#   1. primary product — real co-production (alongside the others)
#   2. primary product — "if produced alone" (full COM on the primary, only its revenue)
#   3..  each other product — co-production
#   last. Total (all products)
# `com_total` is the total COM (USD/month); when NULL it is derived from the shared specific cost. Returns
# list(df = <matrix with translated headers>, bold = <logical per row>, note = <units/allocation caption>)
# or NULL when there are fewer than two products (nothing to compare). All numeric so callers format/round.
com_response_comparison <- function(response_details, simple, row_idx, i18n, com_total = NULL) {
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a
  t <- function(x) tryCatch(i18n$t(x), error = function(e) x)
  n <- length(response_details %||% list())
  if (n < 2) return(NULL)
  pick    <- function(v) if (length(v) > 1) v[row_idx] else v
  extract <- vapply(response_details, function(r) as.numeric(pick(r$extract)), numeric(1))
  svol    <- vapply(response_details, function(r) as.numeric(pick(r$svol)),    numeric(1))
  price   <- vapply(response_details, function(r) as.numeric(r$pr_sale %||% NA_real_), numeric(1))
  nm      <- vapply(response_details, function(r) as.character(r$name %||% ""), character(1))
  total_extract <- sum(extract); total_rev <- sum(svol)
  com <- if (!is.null(com_total) && is.finite(as.numeric(com_total))) as.numeric(com_total)
         else as.numeric(simple$SC_per_kg %||% 0) * total_extract
  primary <- which(vapply(response_details, function(r) isTRUE(r$primary), logical(1)))[1]
  if (is.na(primary)) primary <- 1L
  rev_frac     <- if (total_rev > 0) svol / total_rev else rep(1 / n, n)
  along_share  <- com * rev_frac
  along_sc     <- ifelse(extract > 0, along_share / extract, NA_real_)
  along_profit <- svol - along_share
  along_margin <- ifelse(svol != 0, along_profit / svol * 100, NA_real_)
  rows <- list(); bold <- logical()
  addrow <- function(lbl, ext, pr, rev, sc, prof, mar, b = FALSE) {
    rows[[length(rows) + 1]] <<- data.frame(a = lbl, b = round(ext, 2), c = round(pr, 2), d = round(rev, 2),
      e = round(sc, 2), f = round(prof, 2), g = round(mar, 2), stringsAsFactors = FALSE)
    bold[length(bold) + 1] <<- b
  }
  # primary — real co-production
  addrow(nm[primary], extract[primary], price[primary], svol[primary],
         along_sc[primary], along_profit[primary], along_margin[primary], b = TRUE)
  # primary — hypothetical sole production (full COM, only its own revenue)
  sole_sc     <- if (extract[primary] > 0) com / extract[primary] else NA_real_
  sole_profit <- svol[primary] - com
  sole_margin <- if (svol[primary] != 0) sole_profit / svol[primary] * 100 else NA_real_
  addrow(paste0("   ", t("(if produced alone)")), extract[primary], price[primary], svol[primary],
         sole_sc, sole_profit, sole_margin, b = TRUE)
  # every other product — co-production. Margin is left blank (NA -> "—") for the non-primary products
  # because revenue-based COM allocation forces every alongside margin to equal the blended margin, so a
  # repeated column adds no information; margin is meaningful only for the primary's sole-vs-alongside
  # contrast and the overall Total.
  for (i in setdiff(seq_len(n), primary))
    addrow(nm[i], extract[i], price[i], svol[i], along_sc[i], along_profit[i], NA_real_)
  # total
  tot_sc     <- if (total_extract > 0) com / total_extract else NA_real_
  tot_profit <- total_rev - com
  tot_margin <- if (total_rev != 0) tot_profit / total_rev * 100 else NA_real_
  addrow(t("Total (all products)"), total_extract, NA_real_, total_rev, tot_sc, tot_profit, tot_margin, b = TRUE)
  df <- do.call(rbind, rows)
  colnames(df) <- c(t("Product"), t("Extract"), t("Sale price"), t("Revenue"),
                    t("Specific cost"), t("Gross profit"), t("Margin"))
  note <- t("Manufacturing cost (COM) is allocated across products by revenue share; all figures are monthly. The 'if produced alone' row loads the entire COM onto the primary product. Units — Extract: kg/month; Sale price & Specific cost: USD/kg; Revenue & Gross profit: USD/month; Margin: %.")
  list(df = df, bold = bold, note = note)
}

# ---- Input-parameter sections for the PDF -----------------------------------------------------------
# Groups the flat input table (`input_params_data()`: columns Description, Value, Unit, Type) into the
# same CRM/CUT/COL/FMC categories shown on screen, translating description and unit cells generically
# through i18n$t (their English strings are the translation keys). Returns an ordered list of
# list(title=, df=data.frame(Parameter, Value, Unit)); Value cells are already display-formatted strings.
com_input_param_sections <- function(input_df, i18n) {
  t <- function(x) tryCatch(i18n$t(x), error = function(e) x)
  # Auxiliary-material rows ("<prefix>: <material name>") are localised via the shared helper: prefix
  # translated, user-supplied name kept verbatim, full "...: oil" default still honoured, no i18n warnings.
  t_desc <- function(d) com_translate_desc(d, t)
  if (is.null(input_df) || !nrow(input_df) || !all(c("Description", "Value", "Unit", "Type") %in% colnames(input_df)))
    return(list())
  # The product name(s) already head the report (Process summary), so drop the redundant input-table row.
  input_df <- input_df[as.character(input_df$Description) != "Product name(s)", , drop = FALSE]
  if (!nrow(input_df)) return(list())
  groups <- list(
    list(types = c("gen", "general"), title = t("General & process parameters")),
    list(types = c("crm", "crm_aux"), title = t("Raw-material inputs (CRM)")),
    list(types = c("cut"),            title = t("Utility inputs (CUT)")),
    list(types = c("col"),            title = t("Labour inputs (COL)")),
    list(types = c("fci"),            title = t("Fixed-cost & tax inputs (FMC)")))
  out <- list()
  for (g in groups) {
    sub <- input_df[tolower(as.character(input_df$Type)) %in% g$types, , drop = FALSE]
    if (!nrow(sub)) next
    # Temperature unit: the parameter metadata (desc_pars in sysdata) now stores "°C" directly, so this is
    # normally a no-op. Kept as a defensive guard in case an older cached result or a reverted data file
    # still carries the legacy "degC" string, so the report always shows the proper degree symbol.
    u <- as.character(sub$Unit); u[u %in% c("degC", "deg C")] <- "°C"
    df <- data.frame(
      Parameter = vapply(as.character(sub$Description), t_desc, character(1)),
      Value     = as.character(sub$Value),
      Unit      = vapply(u, t, character(1)),
      stringsAsFactors = FALSE, check.names = FALSE)
    out[[length(out) + 1]] <- list(title = g$title, df = df)
  }
  out
}

# ---- Extraction-curve graphs for the PDF ------------------------------------------------------------
# Builds the specific-cost and payback curves vs extraction time for the report. The specific-cost curve
# reuses the engine ggplot (com_result$plots$sc); the payback curve is rebuilt from simple_data with the
# same outlier filtering the on-screen plot applies (raw payback_yr can be negative/huge near break-even
# and would wreck the axis). Both get a dotted marker at the slider-selected time so the reader can see
# which point the single-point figures correspond to. Returns list(sc=<ggplot|NULL>, payback=<ggplot|NULL>).
com_build_curve_plots <- function(simple_data, com_result, i18n, sel_time = NULL) {
  t <- function(x) tryCatch(i18n$t(x), error = function(e) x)
  marker <- function(p) {
    if (inherits(p, "ggplot") && !is.null(sel_time) && isTRUE(is.finite(suppressWarnings(as.numeric(sel_time)))))
      p + ggplot2::geom_vline(xintercept = as.numeric(sel_time), linetype = "dotted",
                              colour = "#3c8dbc", linewidth = 0.7)
    else p
  }
  base_theme <- ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(face = "bold", size = 11))

  # Specific-cost curve (engine plot) — restyle the axis labels/title through i18n.
  sc <- com_result$plots$sc
  if (inherits(sc, "ggplot")) {
    sc <- sc + ggplot2::labs(title = t("Specific cost vs extraction time"),
                             x = t("Time (min)"), y = t("Specific cost (USD/kg)")) + base_theme
    sc <- marker(sc)
  } else sc <- NULL

  # Payback curve — rebuilt with outlier filtering (mirrors output$payback_plot).
  payback <- NULL
  pv <- suppressWarnings(as.numeric(simple_data$payback_yr))
  keep <- is.finite(pv) & pv > 0
  vp <- pv[keep]
  if (length(vp) >= 4) {
    q1 <- stats::quantile(vp, 0.25, na.rm = TRUE); q3 <- stats::quantile(vp, 0.75, na.rm = TRUE)
    keep <- keep & pv <= (q3 + 3 * (q3 - q1))
  }
  fd <- simple_data[keep, , drop = FALSE]
  if (nrow(fd) > 0) {
    fd$payback_yr <- as.numeric(fd$payback_yr)
    mn <- fd[which.min(fd$payback_yr), , drop = FALSE]
    min_pb <- min(fd$payback_yr, na.rm = TRUE)
    payback <- ggplot2::ggplot(fd, ggplot2::aes(x = .data[["time"]], y = .data[["payback_yr"]])) +
      ggplot2::geom_hline(yintercept = min_pb, linetype = 2, colour = "darkgreen", linewidth = 0.7) +
      ggplot2::geom_line(colour = "darkred") + ggplot2::geom_point(colour = "darkred") +
      ggplot2::geom_point(data = mn, colour = "darkgreen", size = 2.5) +
      # Label the minimum-payback line (mirrors the "43.85" min-cost label on the specific-cost curve).
      ggplot2::geom_label(data = data.frame(x = max(fd$time, na.rm = TRUE), y = min_pb),
        ggplot2::aes(x = x, y = y), label = formatC(min_pb, format = "f", digits = 2),
        inherit.aes = FALSE, fill = "darkgreen", colour = "white", fontface = "bold",
        size = 3, hjust = 1, vjust = 0.5, label.size = 0) +
      ggplot2::labs(title = t("Payback time vs extraction time"), x = t("Time (min)"), y = t("Payback time (yr)")) +
      base_theme
    payback <- marker(payback)
  }
  list(sc = sc, payback = payback)
}

# ---- Financial-bar (waterfall) data + plot, multi-response aware ------------------------------------
# Mirrors the Shiny cost_waterfall_chart: cost bars (CRM/CUT/COL/FMC/COM, negative) plus Revenue, Gross
# Profit and (when a tax rate is set) Net Profit — the latter three STACKED per product/response. Profit
# is apportioned to each response in proportion to its revenue share, exactly like the on-screen chart.
com_waterfall_data <- function(costs, simple, response_details, row_idx, taxrate, i18n) {
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
  cats        <- c("CRM", "CUT", "COL", "FMC", "COM")
  cost_values <- c(-as.numeric(costs$CRM), -as.numeric(costs$CUT), -as.numeric(costs$COL),
                   -as.numeric(costs$FCI), -as.numeric(costs$COM))
  cost_colors <- c("#3498db", "#e74c3c", "#2ecc71", "#9b59b6", "#34495e")
  tr      <- suppressWarnings(as.numeric(taxrate)); has_tax <- isTRUE(is.finite(tr) && tr > 0)
  pick    <- function(v) if (length(v) > 1) v[row_idx] else v
  products <- vapply(response_details, function(r) as.character(r$name %||% ""), character(1))
  revenue  <- vapply(response_details, function(r) as.numeric(pick(r$svol)), numeric(1))
  total_rev   <- sum(revenue, na.rm = TRUE)
  gross_total <- total_rev - as.numeric(costs$COM)
  frac  <- if (total_rev > 0) revenue / total_rev else rep(1 / length(revenue), length(revenue))
  gross <- gross_total * frac
  net   <- if (has_tax) gross * (1 - tr / 100) else NULL
  list(cats = cats, cost_values = cost_values, cost_colors = cost_colors, products = products,
       revenue = revenue, gross = gross, net = net, has_tax = has_tax)
}

com_stacked_waterfall_bar <- function(wf, i18n) {
  t <- function(x) tryCatch(i18n$t(x), error = function(e) x)
  sdollar <- function(x) { x <- as.numeric(x); if (!is.finite(x)) return(""); ax <- abs(x)
    s <- if (ax >= 1e6) sprintf("$%.1fM", ax / 1e6) else if (ax >= 1e3) sprintf("$%.0fK", ax / 1e3) else sprintf("$%.0f", ax)
    if (x < 0) paste0("-", s) else s }
  rev_lab <- t("Revenue"); gp_lab <- t("Gross Profit"); np_lab <- t("Net Profit")
  cat_levels <- c(wf$cats, rev_lab, gp_lab, if (isTRUE(wf$has_tax)) np_lab)
  rows <- list()
  for (i in seq_along(wf$cats))
    rows[[length(rows) + 1]] <- data.frame(cat = wf$cats[i], key = wf$cats[i], value = wf$cost_values[i], stringsAsFactors = FALSE)
  addst <- function(lab, vals) for (i in seq_along(wf$products))
    rows[[length(rows) + 1]] <<- data.frame(cat = lab, key = wf$products[i], value = as.numeric(vals[i]), stringsAsFactors = FALSE)
  addst(rev_lab, wf$revenue); addst(gp_lab, wf$gross); if (isTRUE(wf$has_tax)) addst(np_lab, wf$net)
  df <- do.call(rbind, rows)
  df$cat <- factor(df$cat, levels = cat_levels)
  df$key <- factor(df$key, levels = c(wf$cats, wf$products))
  prod_pal  <- c("#27ae60", "#2980b9", "#8e44ad", "#d35400", "#16a085", "#c0392b", "#f39c12", "#2c3e50")
  prod_cols <- stats::setNames(prod_pal[((seq_along(wf$products) - 1) %% length(prod_pal)) + 1], wf$products)
  fill_vals <- c(stats::setNames(wf$cost_colors, wf$cats), prod_cols)
  tot <- stats::aggregate(value ~ cat, df, sum)
  tot$lab   <- vapply(tot$value, sdollar, character(1))
  tot$vjust <- ifelse(tot$value >= 0, -0.4, 1.3)
  ggplot2::ggplot(df, ggplot2::aes(x = cat, y = value, fill = key)) +
    ggplot2::geom_col(width = 0.72, colour = "white", linewidth = 0.3) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey55", linewidth = 0.3) +
    ggplot2::geom_text(data = tot, ggplot2::aes(x = cat, y = value, label = lab, vjust = vjust),
      inherit.aes = FALSE, size = 2.6, colour = "#333333") +
    # Wrap long product names so the legend never runs off the page (issue 4); stack them one-per-line.
    ggplot2::scale_fill_manual(values = fill_vals, breaks = wf$products, name = t("Product"),
      labels = function(x) vapply(x, function(s) paste(strwrap(as.character(s), width = 26), collapse = "\n"), character(1))) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.14, 0.16)),
      labels = function(v) vapply(v, sdollar, character(1))) +
    ggplot2::labs(x = NULL, y = NULL) + ggplot2::theme_minimal(base_size = 9.5) +
    ggplot2::theme(legend.position = "bottom", legend.title = ggplot2::element_text(size = 7.5),
      legend.text = ggplot2::element_text(size = 6.8), legend.key.size = grid::unit(8, "pt"),
      panel.grid.major.x = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = -40, hjust = 0, size = 7.3),
      plot.margin = grid::unit(c(2, 2, 2, 2), "pt")) +
    ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1, byrow = TRUE))
}

# ---- Cost-of-Manufacturing PDF report ---------------------------------------------------------------
# Assembles the module's results (KPIs, cost-breakdown chart, structured tables, per-product financials,
# input parameters and the specific-cost / payback extraction curves) into a branded PDF via the
# reusable report_pdf.R toolkit (pure R; no LaTeX/pandoc). All single-point figures (KPIs, per-product
# financials) pertain to the extraction time selected on the slider — supplied via `sel_time`.
#   `costs`         list(CRM, CUT, COL, FCI, COM)
#   `headline`      optional list(profit, margin, payback, sc, revenue)
#   `input_sections`list of list(title, df) from com_input_param_sections()
#   `per_response`  list(df, bold, note) from com_response_comparison() — the sole-vs-alongside matrix
#   `curves`        optional list(sc=<ggplot>, payback=<ggplot>) — the extraction-curve graphs
#   `sel_time`      the selected extraction time (min) the single-point results correspond to
#   `process_summary` optional list(product, volume, temp, pressure, time) for the top-of-report summary
# Robust: skips any missing table/plot/section.
build_com_pdf_report <- function(comres, costs, i18n, file,
                                 company = "supeRcrit", logo = NULL, headline = NULL,
                                 input_sections = NULL, per_response = NULL, curves = NULL,
                                 sel_time = NULL, waterfall = NULL, process_summary = NULL) {
  t <- function(x) tryCatch(i18n$t(x), error = function(e) x)
  pal <- rp_palette()
  usd <- function(v) if (isTRUE(is.finite(v))) paste0("$", format(round(as.numeric(v)), big.mark = ",")) else "—"
  fmtv <- function(v) { v <- suppressWarnings(as.numeric(v))
    if (!isTRUE(is.finite(v))) "—" else formatC(v, format = "fg", digits = 4, big.mark = ",") }

  blocks <- list(rp_paragraph_block(
    t("Cost-of-manufacturing analysis for the selected supercritical-extraction scenario. Values are computed from the process, raw-material, utility, labour and fixed-cost inputs entered in the module.")))

  # ---- Process summary: critical scenario facts (product, vessel volume, T/P, extraction time) ----
  if (!is.null(process_summary)) {
    ps <- process_summary
    parts <- Filter(nzchar, c(
      if (!is.null(ps$product) && nzchar(ps$product))
        sprintf("%s: %s", t("Product(s)"), ps$product) else "",
      if (!is.null(ps$volume))
        sprintf("%s: %s L", t("Extraction system volume"), fmtv(ps$volume)) else "",
      if (!is.null(ps$temp) || !is.null(ps$pressure))
        sprintf("%s: %s °C / %s bar", t("Process conditions"), fmtv(ps$temp), fmtv(ps$pressure)) else "",
      if (!is.null(ps$time))
        sprintf("%s: %s %s", t("Extraction time (selected)"), fmtv(ps$time), t("min")) else ""))
    if (length(parts)) blocks <- c(blocks, list(rp_section_block(t("Process summary")),
      rp_paragraph_block(paste(parts, collapse = "    •    "), size = 10, col = pal$ink),
      rp_paragraph_block(t("All single-point figures below pertain to the selected extraction time."),
                         size = 9, col = pal$muted)))
  } else if (!is.null(sel_time) && isTRUE(is.finite(suppressWarnings(as.numeric(sel_time))))) {
    blocks <- c(blocks, list(rp_paragraph_block(
      sprintf("%s: %s %s", t("Single-point results shown for the selected extraction time"),
              formatC(as.numeric(sel_time), format = "fg", digits = 4), t("min")), size = 9.5, col = pal$ink)))
  }

  kpis <- list(
    list(label = t("COM (total)"), value = usd(costs$COM), sub = t("cost of manufacturing"), color = pal$primary2),
    list(label = "CRM", value = usd(costs$CRM), sub = t("raw materials"), color = pal$kpi[1]),
    list(label = "CUT", value = usd(costs$CUT), sub = t("utilities"),     color = pal$kpi[2]),
    list(label = "COL", value = usd(costs$COL), sub = t("labour"),        color = pal$kpi[3]),
    list(label = "FMC", value = usd(costs$FCI), sub = t("fixed costs"), color = pal$kpi[4]))  # display label FMC (matches the rest of the app); costs$FCI is the internal field name
  blocks <- c(blocks, list(rp_section_block(t("Key cost indicators")), rp_kpi_block(kpis)),
    # Period clarification: ALL FIVE indicators are monthly costs. FCI is the monthly fixed manufacturing
    # cost (maintenance/overheads + monthly depreciation of the capital), NOT the one-time capital outlay.
    list(rp_paragraph_block(
      t("All five indicators above are monthly costs. FMC is the fixed manufacturing cost — maintenance, overheads and the monthly depreciation of the invested capital — not the one-time capital outlay."),
      size = 8.5, col = pal$muted)))

  # ---- Key performance indicators — the four headline results from the General Results tab
  # (extract produced, GROSS pre-tax profit, gross margin, payback period), as coloured boxes. Profit and
  # margin are shown GROSS (pre-tax) so they are not affected by the tax rate (issue 2). ----
  if (!is.null(headline)) {
    num     <- function(v) suppressWarnings(as.numeric(v))
    fmt_kg  <- function(v) if (isTRUE(is.finite(num(v)))) paste0(format(round(num(v)), big.mark = ","), " kg") else "—"
    fmt_pct <- function(v) if (isTRUE(is.finite(num(v)))) sprintf("%.1f%%", num(v)) else "—"
    pv <- num(headline$payback); pay_ok <- isTRUE(is.finite(pv) && pv >= 0)
    # Sub-labels state the period explicitly: the first three are monthly figures; payback is in years.
    perf <- list(
      list(label = t("Extract produced"), value = fmt_kg(headline$extract), sub = t("monthly"), color = "#3498db"),
      list(label = t("Gross profit"),     value = usd(headline$profit),     sub = t("monthly"), color = "#2ecc71"),
      list(label = t("Gross margin"),     value = fmt_pct(headline$margin), sub = t("monthly"), color = "#9b59b6"),
      list(label = t("Payback period"),
           value = if (pay_ok) sprintf("%.2f", pv) else t("Not Viable"),
           sub   = if (pay_ok) t("years") else t("negative margin"), color = "#f39c12"))
    blocks <- c(blocks, list(rp_section_block(t("Key performance indicators")), rp_kpi_block(perf)))
    # Footnote: the payback period is measured against the project's one-time capital investment (CAPEX).
    if (isTRUE(is.finite(suppressWarnings(as.numeric(headline$capex)))))
      blocks <- c(blocks, list(rp_paragraph_block(
        sprintf(t("Payback period is measured against the total capital investment of %s."), usd(headline$capex)),
        size = 8.5, col = pal$muted, keep_prev = TRUE)))
  }

  # Specific cost and revenue are not among the KPI boxes; surface them as a short note.
  if (!is.null(headline)) {
    hp <- paste(Filter(nzchar, c(
      if (!is.null(headline$sc))      sprintf("%s: $%.2f/kg", t("Specific cost"), headline$sc) else "",
      if (!is.null(headline$revenue)) sprintf("%s: %s %s", t("Revenue"), usd(headline$revenue), t("per month")) else "")),
      collapse = "    •    ")
    if (nzchar(hp)) blocks <- c(blocks, list(rp_paragraph_block(hp, size = 10, col = pal$ink)))
  }

  # ---- Cost breakdown: a DOUGHNUT (left) + a financial BAR chart (right, wider) side by side ----
  dabbr <- function(x) { x <- abs(as.numeric(x))
    if (!isTRUE(is.finite(x))) "" else if (x >= 1e6) sprintf("$%.1fM", x/1e6) else if (x >= 1e3) sprintf("$%.0fK", x/1e3) else sprintf("$%.0f", x) }
  vals <- c(costs$CRM, costs$CUT, costs$COL, costs$FCI)
  labs <- c("CRM", "CUT", "COL", "FMC"); cols4 <- pal$kpi[1:4]; ok <- is.finite(vals) & vals > 0
  if (any(ok)) {
    # Doughnut. Each slice with enough arc to hold the text is labelled inside; smaller slices carry no
    # on-ring label and are read from the legend, which is the single source of truth (category + % + $ for
    # EVERY slice). Labels are centred with position_stack(vjust = 0.5) so each sits on its OWN slice: a
    # manual cumsum midpoint does NOT line up, because ggplot stacks factor levels top-down (first level on
    # top), so a hand-computed angle lands the label on the wrong slice (e.g. a 12% label on the 68% slice).
    dd <- data.frame(cat = factor(labs[ok], levels = labs[ok]), val = vals[ok], col = cols4[ok], stringsAsFactors = FALSE)
    tot <- sum(dd$val)
    dd$pctnum <- 100 * dd$val / tot
    dd$pct    <- paste0(round(dd$pctnum), "%")
    dd$lab    <- ifelse(dd$pctnum >= 3, dd$pct, "")   # label all but hair-thin slices (issue 5); legend covers the rest
    leg <- setNames(sprintf("%s  %s (%s)", dd$cat, dd$pct, vapply(dd$val, dabbr, character(1))), as.character(dd$cat))
    doughnut <- ggplot2::ggplot(dd, ggplot2::aes(x = 2, y = val, fill = cat)) +
      ggplot2::geom_col(width = 1, colour = "white", linewidth = 0.4) +
      ggplot2::coord_polar(theta = "y") + ggplot2::xlim(0.5, 2.5) +
      ggplot2::geom_text(ggplot2::aes(label = lab), position = ggplot2::position_stack(vjust = 0.5),
        colour = "white", fontface = "bold", size = 3.1, show.legend = FALSE) +
      ggplot2::scale_fill_manual(values = stats::setNames(dd$col, as.character(dd$cat)), labels = leg, name = NULL) +
      ggplot2::theme_void(base_size = 10) +
      ggplot2::theme(legend.position = "bottom", legend.text = ggplot2::element_text(size = 8),
        legend.key.size = grid::unit(9, "pt"), plot.margin = grid::unit(c(2,2,2,2), "pt")) +
      ggplot2::guides(fill = ggplot2::guide_legend(ncol = 2))
    # Financial bar chart. With multiple responses, Revenue/Gross/Net Profit are STACKED per product
    # (mirrors the Shiny cost_waterfall_chart); otherwise a single Revenue + Profit pair from `headline`.
    multi <- !is.null(waterfall) && length(waterfall$products) > 1
    if (multi) {
      bar <- com_stacked_waterfall_bar(waterfall, i18n)
    } else {
      bcat <- c("CRM","CUT","COL","FMC","COM"); bval <- c(-costs$CRM,-costs$CUT,-costs$COL,-costs$FCI,-costs$COM)
      bcol <- c(cols4, "#34495e")
      rev_v <- suppressWarnings(as.numeric(headline$revenue)); prof_v <- suppressWarnings(as.numeric(headline$profit))
      if (isTRUE(is.finite(rev_v)) && rev_v > 0) { bcat <- c(bcat, t("Revenue")); bval <- c(bval, rev_v); bcol <- c(bcol, "#27ae60") }
      if (isTRUE(is.finite(prof_v))) { bcat <- c(bcat, t("Profit")); bval <- c(bval, prof_v)
        bcol <- c(bcol, if (prof_v >= 0) "#16a085" else "#c0392b") }
      bdf <- data.frame(cat = factor(bcat, levels = bcat), val = bval, stringsAsFactors = FALSE)
      bar <- ggplot2::ggplot(bdf, ggplot2::aes(x = cat, y = val, fill = cat)) +
        ggplot2::geom_col(width = 0.72) +
        ggplot2::geom_hline(yintercept = 0, colour = "grey55", linewidth = 0.3) +
        ggplot2::geom_text(ggplot2::aes(label = vapply(val, dabbr, character(1)),
          vjust = ifelse(val >= 0, -0.4, 1.3)), size = 2.9, colour = "#333333") +
        ggplot2::scale_fill_manual(values = stats::setNames(bcol, bcat)) +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.12, 0.12)),
          labels = function(v) vapply(v, function(z) if (isTRUE(z < 0)) paste0("-", dabbr(z)) else dabbr(z), character(1))) +
        ggplot2::labs(x = NULL, y = NULL) +
        ggplot2::theme_minimal(base_size = 9.5) +
        ggplot2::theme(legend.position = "none", panel.grid.major.x = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_text(angle = 0, size = 8))
    }
    # Side by side: doughnut : bar = 1 : 1.7 (bar gets the most space).
    combo <- gridExtra::arrangeGrob(ggplot2::ggplotGrob(doughnut), ggplot2::ggplotGrob(bar),
      ncol = 2, widths = grid::unit(c(1, 1.7), "null"))
    # Height kept modest so the section heading + chart fit below the KPIs on page 1 rather than spilling
    # over and leaving page 1 half-empty (issue 3). shrink = TRUE lets the chart scale down to fill whatever
    # space is left under the KPIs (which varies by language — longer RU/TR notes push it lower) instead of
    # migrating to page 2; keep-with-next keeps heading + chart together.
    blocks <- c(blocks, list(rp_section_block(t("Cost breakdown")),
                             rp_plot_block(combo, height = if (multi) 2.9 else 2.7,
                                           shrink = TRUE, min_height = 2.0)))
  }

  tb <- if (!is.null(comres$tables)) comres$tables else list()
  cont <- t("continued")
  tspec <- list(production = t("Production overview"), crm = t("Cost of raw materials (CRM)"),
                cut = t("Cost of utilities (CUT)"), financial = t("Financial summary"),
                performance = t("Performance across the extraction curve"))
  # Rows to highlight (bold) in each table — matched on the translated description text.
  fin_bold_keys <- vapply(c("Cost of Manufacturing (COM)", "Specific extract cost",
                            "Profit margin", "Payback period"), t, character(1))
  crm_bold_keys <- vapply(c("Cost of Raw Materials (CRM)"), t, character(1))
  cut_bold_keys <- vapply(c("Total electricity consumption (monthly)", "Total electricity cost (monthly)",
                            "Cost of Utilities (CUT)"), t, character(1))
  for (key in names(tspec)) if (!is.null(tb[[key]]) && nrow(tb[[key]])) {
    bkeys <- switch(key, financial = fin_bold_keys, crm = crm_bold_keys, cut = cut_bold_keys, NULL)
    bold <- if (!is.null(bkeys)) as.character(tb[[key]][[1]]) %in% bkeys else NULL
    blocks <- c(blocks, list(rp_section_block(tspec[[key]]),
                             rp_table_block(tb[[key]], pal = pal, bold = bold, continued = cont)))
  }

  # ---- Per-product financial comparison matrix: sole vs alongside-others (issue 12/14) ----
  if (!is.null(per_response) && !is.null(per_response$df) && nrow(per_response$df)) {
    blocks <- c(blocks, list(rp_section_block(t("Per-product financial comparison")),
      rp_table_block(per_response$df, pal = pal, bold = per_response$bold, continued = cont)))
    if (!is.null(per_response$note))
      blocks <- c(blocks, list(rp_paragraph_block(per_response$note, size = 8, col = pal$muted, keep_prev = TRUE)))
  }

  # ---- Extraction-curve graphs (specific cost & payback vs extraction time) ----
  # Prefer the caller-supplied `curves` (filtered/marked to match the app); fall back to comres$plots.
  cv <- if (!is.null(curves)) curves else (if (!is.null(comres$plots)) comres$plots else list())
  if (inherits(cv$sc, "ggplot") || inherits(cv$payback, "ggplot")) {
    blocks <- c(blocks, list(rp_section_block(t("Extraction-curve graphs"))))
    if (inherits(cv$sc, "ggplot"))      blocks <- c(blocks, list(rp_plot_block(cv$sc, height = 2.7)))
    if (inherits(cv$payback, "ggplot")) blocks <- c(blocks, list(rp_plot_block(cv$payback, height = 2.7)))
  }

  # ---- Input parameters (appendix: the exact inputs the scenario was computed from) ----
  # Distinct slate-blue scheme so the reference inputs read as a separate section from the green results.
  if (length(input_sections)) {
    input_pal <- pal
    input_pal$primary  <- "#2c5468"   # section heading + table titles (slate)
    input_pal$primary2 <- "#35708f"   # table header fill (steel blue)
    input_pal$zebra    <- "#eef3f7"   # alternating row tint (cool grey-blue)
    blocks <- c(blocks, list(rp_section_block(t("Input Parameters"), pal = input_pal)))
    for (sec in input_sections) {
      df <- sec$df
      colnames(df) <- c(t("Parameter"), t("Value"), t("Unit"))
      blocks <- c(blocks, list(rp_table_block(df, title = sec$title, pal = input_pal)))
    }
  }

  rp_render_report(file, blocks,
    header = list(company = company, title = t("Cost of Manufacturing — Report"),
                  subtitle = format(Sys.time(), "%d-%m-%Y"), logo = logo),
    footer = list(left = format(Sys.time(), "%d-%m-%Y %H:%M"), center = "supeRcrit",
                  page_fmt = t("Page %d of %d")), palette = pal, continued = t("continued"))
}


download_roi_monthly_processed_data <- function(data) {
  current_time <- Sys.time()

  formatted_time <- format(current_time, "%d-%m-%Y_%Hhrs-%Mmin-%Ssec")
  downloadHandler(
    filename = function() {
      paste("com-analysis-monthly-", formatted_time, ".csv", sep = "")
    },
    content = function(file) {
      if (!is.null(data)) {
        write.csv(data, file, row.names = FALSE)
      }
    }
  )
}


download_detailed_data <- function(detailed_data) {
  current_time <- Sys.time()

  formatted_time <- format(current_time, "%d-%m-%Y_%Hhrs-%Mmin-%Ssec")

  downloadHandler(
    filename = function() {
      paste("com-analysis-detailed-", formatted_time, ".csv", sep = "")
    },
    content = function(file) {
      if (!is.null(detailed_data)) {
        write.csv(detailed_data, file, row.names = FALSE)
      }
    }
  )
}
