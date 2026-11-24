# =====================================================================================================
# report_pdf.R — Self-contained PDF report toolkit for the Shiny app's "Export Results as PDF" feature.
#
# Pure R: uses only the built-in cairo_pdf() device + grid + gridExtra + ggplot2 (all package deps, no
# LaTeX / pandoc / Quarto / Chrome). Portable to shinyapps.io, Posit Connect, Docker and local alike.
# cairo_pdf gives full Unicode (°C, µ, ₂, currency symbols) and antialiasing.
#
# Design: a small "flow layout" engine. The caller builds a list of BLOCKS (KPI row, section heading,
# table, chart, spacer, paragraph). Each block is a grid grob plus a measured height in inches. The
# renderer stacks them top-to-bottom on A4 pages, starting a new page when a block does not fit, and
# splitting long tables across pages. A branded header and a footer (page N of M, timestamp) are drawn
# on every page. A dry measuring pass computes the total page count so the footer can show "N of M".
#
# Public API:
#   rp_palette()                              -> named colour list (override per report if desired)
#   rp_kpi_block(kpis, ...)                   -> a row of coloured KPI/value boxes
#   rp_section_block(title, ...)              -> a section heading
#   rp_paragraph_block(text, ...)             -> a wrapped text paragraph
#   rp_table_block(df, title, ...)            -> a themed, auto-splitting table
#   rp_plot_block(ggplot_obj, height, ...)    -> a chart (any grob/ggplot)
#   rp_spacer_block(height)                   -> vertical space
#   rp_render_report(file, blocks, header=list(...), footer=list(...), palette=...)  -> writes the PDF
# =====================================================================================================

suppressMessages({
  library(grid); library(gridExtra); library(ggplot2)
})

# ---- palette ----------------------------------------------------------------------------------------
rp_palette <- function() list(
  primary   = "#1f5c3f",   # brand green (header band)
  primary2  = "#2e7d32",
  accent    = "#3c8dbc",
  ink       = "#212529",
  muted     = "#6c757d",
  rule      = "#d9dee3",
  headfill  = "#eef2f7",   # table header fill
  zebra     = "#f7f9fb",   # alternate row fill
  kpi       = c("#3498db", "#e74c3c", "#2ecc71", "#9b59b6", "#f39c12", "#16a085")
)

# ---- report font: a Unicode-complete family so subscripts (CO₂, y₂), ⁻¹ etc. render on the cairo device
# (the default "sans" lacks U+2082 subscript digits → shows as □). Windows: Segoe UI (always present);
# Linux/shinyapps.io: DejaVu Sans (standard, has subscripts); macOS: Helvetica/Arial fallback. Overridable
# via option(supercrit.report.font=...). Falls back gracefully if the chosen family is absent. ------------
.rp_font <- function() {
  getOption("supercrit.report.font", default = {
    sn <- tolower(Sys.info()[["sysname"]] %||% "")
    if (grepl("windows", sn)) "Segoe UI" else if (grepl("darwin", sn)) "Arial" else "DejaVu Sans"
  })
}

# ---- font-warning muffler ---------------------------------------------------------------------------
# Text measurement (tableGrob / ggplotGrob) happens on whatever device is active while the report grobs
# are built — which, before cairo_pdf() opens, is the null/Windows device that does not know the report
# family ("Segoe UI" etc.). That floods the console with harmless "font family not found in Windows font
# database" / "font width unknown" warnings, one per text element. The final cairo device DOES carry the
# font, so these are purely cosmetic; muffle just this warning class around grob building and rendering.
.rp_quiet_fonts <- function(expr) withCallingHandlers(expr, warning = function(w) {
  # message varies by device: "font family 'Segoe UI' not found in PostScript font database" (pdf/postscript),
  # "... not found in Windows font database" (windows/RStudio), plus width/metric variants.
  if (grepl("font family.*not found|not found in .*font database|font width unknown|no font could be found|font metrics unknown",
            conditionMessage(w), ignore.case = TRUE)) invokeRestart("muffleWarning")
})

# Measuring PLOTMATH text (parsed headers) needs a device that can render math glyphs. During grob building
# and pagination the active device is usually the default pdf()/null device, which throws "failed to find or
# load PDF CID font" on plotmath. Run such measurement on a throwaway cairo_pdf (same family as the final
# output) so widths/heights come out right on every platform. Used only for tables with plotmath headers.
.rp_on_cairo <- function(expr) {
  f <- tempfile(fileext = ".pdf")
  ok <- tryCatch({ grDevices::cairo_pdf(f, width = .RP$pw, height = .RP$ph, family = .rp_font()); TRUE },
                 error = function(e) FALSE)
  if (ok) on.exit({ try(grDevices::dev.off(), silent = TRUE); unlink(f) }, add = TRUE)
  force(expr)
}

# ---- A4 portrait geometry (inches) ------------------------------------------------------------------
.RP <- list(pw = 8.27, ph = 11.69, mx = 0.6, mtop = 1.25, mbot = 0.7, gap = 0.14)
.rp_content <- function() list(
  x = .RP$mx, w = .RP$pw - 2 * .RP$mx,
  top = .RP$ph - .RP$mtop,                 # inches from bottom of page to top of content
  bot = .RP$mbot,                          # inches from bottom of page to bottom of content
  h = (.RP$ph - .RP$mtop) - .RP$mbot
)

# in-from-bottom helper -> npc y for a band of height h whose TOP sits at `top_in`
.rp_vp <- function(x_in, top_in, w_in, h_in) viewport(
  x = unit(x_in, "in"), y = unit(top_in - h_in, "in"), width = unit(w_in, "in"), height = unit(h_in, "in"),
  just = c("left", "bottom"))

# ---- measuring a grob's height in inches ------------------------------------------------------------
.rp_grob_h_in <- function(g) {
  h <- tryCatch(grid::convertHeight(grid::grobHeight(g), "in", valueOnly = TRUE), error = function(e) NA_real_)
  if (!isTRUE(is.finite(h)) || h <= 0) 0.3 else h
}

# ---- number/format helpers --------------------------------------------------------------------------
.rp_dash <- "—"   # em-dash placeholder for non-available (NaN/NA/Inf) numeric cells
rp_fmt_num <- function(x, digits = 2) {
  if (is.numeric(x)) formatC(x, format = "f", digits = digits, big.mark = ",")
  else as.character(x)
}
# Format a data.frame's cells to display strings. Non-finite numerics (NaN/NA/Inf — e.g. a utility whose
# capacity is 0, so its power draw is undefined) and stray "NaN"/"NA"/"Inf" text render as an em-dash rather
# than an alarming "NaN" in the report.
.rp_df_fmt <- function(df, digits = 2) {
  as.data.frame(lapply(df, function(col) {
    if (is.numeric(col)) ifelse(is.finite(col), formatC(col, format = "f", digits = digits, big.mark = ","), .rp_dash)
    else { s <- as.character(col); s[is.na(s) | grepl("^\\s*(NaN|NA|[+-]?Inf)\\s*$", s)] <- .rp_dash; s }
  }), stringsAsFactors = FALSE, check.names = FALSE)
}

# SMART number formatting: integers print with NO decimal places; fractional values print up to `max_dp`
# decimals with trailing zeros trimmed (so 300 -> "300", 15.00 -> "15", 449.650 -> "449.65", 0.4596 -> "0.4596").
# Non-finite -> NA (rendered as an em-dash downstream). Use rp_fmt_df() on a data.frame before rp_table_block
# so Standard-Order / coded-factor / integer columns never show a spurious ".00".
rp_num <- function(x, max_dp = 4) {
  if (!is.numeric(x)) return(as.character(x))
  vapply(x, function(v) {
    if (is.na(v) || !is.finite(v)) return(NA_character_)
    # format="f", digits=0 (NOT "d", which coerces to 32-bit int and returns NA for integer-valued doubles > 2.1e9)
    if (isTRUE(v == round(v)) && abs(v) < 1e15) return(formatC(v, format = "f", digits = 0, big.mark = ","))
    s <- formatC(v, format = "f", digits = max_dp, big.mark = ",")
    if (grepl("\\.", s)) { s <- sub("0+$", "", s); s <- sub("\\.$", "", s) }
    s
  }, character(1))
}
rp_fmt_df <- function(df, max_dp = 4) {
  if (is.null(df) || !is.data.frame(df)) return(df)
  as.data.frame(lapply(df, function(c) if (is.numeric(c)) rp_num(c, max_dp) else c),
                stringsAsFactors = FALSE, check.names = FALSE)
}
# Convert the small HTML subset used in on-screen labels (sub/sup tags, &amp; etc.) to Unicode so it renders
# correctly in the pure-grid PDF (e.g. "CO<sub>2</sub>" -> "CO₂", "MPa<sup>½</sup>" -> "MPa½"). Unknown tags
# are stripped. Also maps the stray ASCII-mangled fraction-slash used for units so no .notdef boxes appear.
rp_unhtml <- function(x) {
  if (is.null(x)) return(x)
  sub_map <- c("0"="₀","1"="₁","2"="₂","3"="₃","4"="₄","5"="₅",
               "6"="₆","7"="₇","8"="₈","9"="₉","+"="₊","-"="₋","n"="ₙ")
  sup_map <- c("0"="⁰","1"="¹","2"="²","3"="³","4"="⁴","5"="⁵",
               "6"="⁶","7"="⁷","8"="⁸","9"="⁹","+"="⁺","-"="⁻",
               "½"="½","n"="ⁿ")
  conv <- function(s, tag, map) { repeat { m <- regmatches(s, regexec(paste0("<", tag, ">(.*?)</", tag, ">"), s, ignore.case = TRUE))[[1]]
      if (length(m) < 2) break
      inner <- m[2]; out <- paste(vapply(strsplit(inner, "")[[1]], function(ch) if (ch %in% names(map)) map[[ch]] else ch, character(1)), collapse = "")
      s <- sub(paste0("<", tag, ">.*?</", tag, ">"), out, s, ignore.case = TRUE) }; s }
  vapply(as.character(x), function(s) {
    if (is.na(s) || !nzchar(s)) return(s)
    s <- conv(s, "sub", sub_map); s <- conv(s, "sup", sup_map)
    s <- gsub("<[^>]+>", "", s)                      # strip any remaining tags
    s <- gsub("&amp;", "&", s, fixed = TRUE); s <- gsub("&lt;", "<", s, fixed = TRUE); s <- gsub("&gt;", ">", s, fixed = TRUE)
    s <- gsub("ᐧ", "ᐟ", s, fixed = TRUE)   # bad canadian-syllabics slash -> none; use real fraction slash below
    s
  }, character(1), USE.NAMES = FALSE)
}
# Render a model-equation string in readable notation: "*" -> "·", "A^2" -> "A²", "x_1" -> "x₁".
rp_fmt_equation <- function(eq) {
  if (is.null(eq) || !nzchar(eq)) return(eq)
  sup <- c("0"="⁰","1"="¹","2"="²","3"="³","4"="⁴","5"="⁵","6"="⁶","7"="⁷","8"="⁸","9"="⁹")
  eq <- gsub("\\^([0-9])", "§\\1§", eq)    # mark powers
  for (d in names(sup)) eq <- gsub(paste0("§", d, "§"), sup[[d]], eq, fixed = TRUE)
  eq <- gsub("\\*", " · ", eq)                 # multiplication dot
  eq <- gsub("\\s*·\\s*", "·", eq)         # tidy spacing around the dot
  eq <- gsub("\\s+", " ", eq); trimws(eq)
}

# =====================================================================================================
# PLOTMATH (proper math notation) — grid can render parsed R "plotmath" expressions with true italic
# variables, real super/subscripts and Greek letters (exactly like the app's KaTeX, no extra deps). Used
# for model equations and for the "Parameter"/"Property" column of parameter tables. Prose that must stay
# upright (and may be non-ASCII / translated) is wrapped as a quoted-string literal; only symbols/units are
# left as math. See rp_math_block(), rp_eq_to_plotmath(), and rp_table_block(..., math_col=).
# =====================================================================================================
# Wrap arbitrary (possibly translated / unicode) prose as a plotmath string literal so it renders upright.
rp_pm_quote <- function(s) {
  s <- as.character(s %||% ""); s <- gsub("\\\\", "\\\\\\\\", s); s <- gsub('"', '\\\\"', s)
  paste0('"', s, '"')
}
# Approximate the VISIBLE text of a plotmath source string (strip italic()/phantom()/sub-/superscript markup
# and spacing operators) — used only to budget column widths for a math column, never for display.
.rp_pm_visible <- function(x) vapply(as.character(x), function(s) {
  if (is.na(s)) return("")
  s <- gsub("phantom\\([^()]*\\)", "", s)
  s <- gsub("italic\\(([^()]*)\\)", "\\1", s)
  s <- gsub('"', "", s, fixed = TRUE)
  s <- gsub("\\[([^][]*)\\]", "\\1", s); s <- gsub("\\^\\{?([^}~* ]*)\\}?", "\\1", s)
  s <- gsub("%\\.%", " ", s); s <- gsub("[~*]", " ", s); s <- gsub("minute", "'", s, fixed = TRUE)
  trimws(gsub("\\s+", " ", s))
}, character(1), USE.NAMES = FALSE)
# Convert ONE equation term body (e.g. "0.363*A", "1.364*A^2", "0.5*A*B", "6.671") to a plotmath fragment:
# numeric coefficients stay upright, variables become italic with sub/superscripts, factors juxtaposed.
.rp_term_to_pm <- function(body) {
  body <- gsub("I\\(([^()]*)\\)", "\\1", body)                 # drop lm's I(...) wrapper
  factors <- strsplit(body, "[*:]")[[1]]
  parts <- vapply(factors, function(f) {
    f <- trimws(f); if (!nzchar(f)) return("")
    if (grepl("^[0-9.]+([eE][+-]?[0-9]+)?$", f)) return(f)     # numeric coefficient (upright)
    m <- regmatches(f, regexec("^([A-Za-z]+)([0-9]+)?(?:_([0-9A-Za-z]+))?(?:\\^([0-9]+))?$", f, perl = TRUE))[[1]]
    if (length(m) == 5 && nzchar(m[2])) {
      sub <- if (nzchar(m[4])) m[4] else m[3]                  # x_1 or x1 -> subscript 1
      out <- paste0("italic(", m[2], ")")
      if (nzchar(sub)) out <- paste0(out, "[", sub, "]")
      if (nzchar(m[5])) out <- paste0(out, "^", m[5])
      out
    } else rp_pm_quote(f)                                      # unexpected token -> upright literal
  }, character(1), USE.NAMES = FALSE)
  paste(parts[nzchar(parts)], collapse = "*")
}
# Split an equation right-hand side into signed terms ("6.671", "+0.363*A", "-1.364*A^2", ...), protecting
# scientific-notation exponent signs so "1.2e-05" is not split.
.rp_eq_terms <- function(rhs) {
  s <- gsub("\\s+", "", rhs)
  # A term = optional sign then a run that is NOT a +/- boundary, but DOES absorb a sci-notation "e+"/"e-"
  # exponent sign so "1.2e-05" is one token, not split. The e/E only counts as an exponent when preceded by a
  # digit or dot ((?<=[0-9.])) — otherwise a DOE factor literally named "E" before a +/- (e.g. "...+1.2*E+3.3*D")
  # would be mistaken for an exponent and merge two terms into one garbled one. Greedy per term.
  m <- regmatches(s, gregexpr("[+-]?(?:(?<=[0-9.])[eE][+-]|[^+-])+", s, perl = TRUE))[[1]]
  m[nzchar(m)]
}
# Build a model equation as a vector of plotmath LINE strings (wrapped). First line "<lhs> = <terms>";
# continuation lines use phantom()== alignment so the "=" and terms line up under the first row.
rp_eq_to_plotmath <- function(lhs, rhs, max_chars = 54) {
  if (is.null(rhs) || !nzchar(trimws(rhs))) return(character(0))
  terms <- .rp_eq_terms(rhs); if (!length(terms)) return(character(0))
  segs <- lapply(terms, function(tm) list(sign = if (startsWith(tm, "-")) "-" else "+",
                                          pm = .rp_term_to_pm(sub("^[+-]", "", tm))))
  lines_idx <- list(); cur <- integer(0); clen <- 0L
  for (i in seq_along(segs)) {
    w <- nchar(.rp_pm_visible(segs[[i]]$pm)) + 4L
    if (length(cur) && clen + w > max_chars) { lines_idx[[length(lines_idx) + 1L]] <- cur; cur <- integer(0); clen <- 0L }
    cur <- c(cur, i); clen <- clen + w
  }
  if (length(cur)) lines_idx[[length(lines_idx) + 1L]] <- cur
  lhs_pm <- rp_pm_quote(lhs); out <- character(length(lines_idx))
  for (li in seq_along(lines_idx)) {
    idx <- lines_idx[[li]]; body <- ""
    for (k in seq_along(idx)) { s <- segs[[idx[k]]]
      if (li == 1 && k == 1) body <- paste0(" ", if (s$sign == "-") "-" else "", s$pm)
      else body <- paste0(body, " ", s$sign, " ", s$pm) }
    prefix <- if (li == 1) paste0(lhs_pm, " ==") else paste0("phantom(", lhs_pm, ") == phantom(0)")
    out[li] <- paste0(prefix, body)
  }
  out
}

# approx average glyph advance (inches) for the report font at a point size — budgets how many characters
# fit in a column/paragraph. Proportional fonts vary; ~0.48em is a reasonable mean for mixed-case Latin text.
.rp_char_w   <- function(size) size / 72 * 0.48
.rp_base_size <- 8.4                                # table body point size (keep in sync with .rp_table_theme)

# Wrap a character vector so each element fits `maxchars` per line (word-wrap; long unbreakable tokens are
# left intact rather than hard-split). Empty/blank cells pass through untouched.
.rp_wrap_cell <- function(x, maxchars) vapply(as.character(x), function(s) {
  if (is.na(s) || !nzchar(trimws(s))) return(s)
  paste(strwrap(s, width = max(4, maxchars)), collapse = "\n")
}, character(1), USE.NAMES = FALSE)

# =====================================================================================================
# BLOCK CONSTRUCTORS — each returns list(type, grob, h, splittable, df?, title?, opts?)
# =====================================================================================================

rp_spacer_block <- function(height = 0.15) list(type = "spacer", grob = grid::nullGrob(), h = height, split = FALSE)

# A section heading. keep_next = TRUE so the paginator never strands the heading at the foot of a page while
# its table/plot flows to the next (see .rp_paginate).
rp_section_block <- function(title, pal = rp_palette()) {
  g <- grid::gTree(children = grid::gList(
    grid::textGrob(title, x = unit(0, "npc"), just = "left",
      gp = grid::gpar(fontsize = 12.5, fontface = "bold", col = pal$primary)),
    grid::linesGrob(x = unit(c(0, 1), "npc"), y = unit(c(0.02, 0.02), "npc"),
      gp = grid::gpar(col = pal$rule, lwd = 1.2))))
  list(type = "section", grob = g, h = 0.34, split = FALSE, keep_next = TRUE)
}

# keep_prev = TRUE marks a trailing caption/note that annotates the block just before it, so the paginator
# never strands it alone on a fresh page away from that block (see .rp_paginate).
rp_paragraph_block <- function(text, size = 9.5, col = NULL, pal = rp_palette(), keep_prev = FALSE, face = "plain") {
  col <- col %||% pal$muted
  # wrap to the actual content width for this point size (fixed char counts wrapped too early — issue 13).
  # Explicit newlines in `text` are PRESERVED (each line wrapped independently) so equation/conclusion lists
  # keep one item per line instead of being re-flowed into a single blob.
  maxchars <- max(20, floor((.RP$pw - 2 * .RP$mx) / .rp_char_w(size)))
  wrapped <- paste(vapply(strsplit(text, "\n", fixed = TRUE)[[1]], function(ln)
    if (nzchar(trimws(ln))) paste(strwrap(ln, width = maxchars), collapse = "\n") else " ",
    character(1), USE.NAMES = FALSE), collapse = "\n")
  nlines <- length(gregexpr("\n", wrapped)[[1]]) + 1
  g <- grid::textGrob(wrapped, x = unit(0, "npc"), y = unit(1, "npc"), just = c("left", "top"),
    gp = grid::gpar(fontsize = size, col = col, lineheight = 1.15, fontface = face))
  list(type = "para", grob = g, h = 0.02 + nlines * (size / 72) * 1.3, split = FALSE, keep_prev = keep_prev)
}

# A block of PLOTMATH lines — each element of `lines` is a plotmath-expression STRING (see rp_eq_to_plotmath,
# or a quoted "label" line). Rendered left-aligned, stacked, in proper math notation (italic variables, real
# super/subscripts, Greek). Non-parseable lines fall back to literal text so a bad token never aborts the PDF.
rp_math_block <- function(lines, size = 11, col = NULL, pal = rp_palette(), keep_prev = FALSE) {
  col <- col %||% pal$ink
  lines <- as.character(lines); lines <- lines[!is.na(lines) & nzchar(lines)]
  n <- length(lines); if (!n) return(rp_spacer_block(0))
  lh <- (size / 72) * 1.75
  gl <- grid::gList()
  for (i in seq_len(n)) {
    ex <- tryCatch(parse(text = lines[i]), error = function(e) NULL)
    lab <- if (is.null(ex) || !length(ex)) lines[i] else ex
    gl <- grid::gList(gl, grid::textGrob(lab, x = unit(0, "npc"), y = unit(1, "npc") - unit((i - 1) * lh, "in"),
      just = c("left", "top"), gp = grid::gpar(fontsize = size, col = col)))
  }
  list(type = "math", grob = grid::gTree(children = gl), h = 0.05 + n * lh, split = FALSE, keep_prev = keep_prev)
}

# KPI boxes: kpis = list(list(label=, value=, sub=, color=), ...). Drawn as a single row.
rp_kpi_block <- function(kpis, height = 0.95, pal = rp_palette()) {
  n <- length(kpis); if (!n) return(rp_spacer_block(0))
  gap <- 0.12
  gl <- grid::gList()
  for (i in seq_len(n)) {
    k <- kpis[[i]]; col <- k$color %||% pal$kpi[((i - 1) %% length(pal$kpi)) + 1]
    x0 <- (i - 1) / n; x1 <- i / n
    cx <- (x0 + x1) / 2; bw <- (x1 - x0)
    padx <- (gap / 2) / (n)                        # gap in npc terms
    gl <- grid::gList(gl,
      grid::roundrectGrob(x = unit(cx, "npc"), y = unit(0.5, "npc"),
        width = unit(bw, "npc") - unit(gap, "in"), height = unit(0.85, "npc"), r = unit(3, "pt"),
        gp = grid::gpar(fill = col, col = NA)),
      grid::textGrob(k$label %||% "", x = unit(cx, "npc"), y = unit(0.72, "npc"),
        gp = grid::gpar(col = "white", fontsize = 9, fontface = "bold")),
      grid::textGrob(k$value %||% "", x = unit(cx, "npc"), y = unit(0.44, "npc"),
        gp = grid::gpar(col = "white", fontsize = 14, fontface = "bold")),
      grid::textGrob(k$sub %||% "", x = unit(cx, "npc"), y = unit(0.22, "npc"),
        gp = grid::gpar(col = "white", fontsize = 7.5)))
  }
  list(type = "kpi", grob = grid::gTree(children = gl), h = height, split = FALSE)
}

# Themed table grob for a data.frame (already string-formatted or auto-formatted). `bold` is an optional
# logical vector (length = nrow) that renders those rows bold across every column. `text_col` is an optional
# per-cell text-colour vector in column-major order (length nrow*ncol) — used to colour-code specific cells
# (e.g. a Sustainability column) while the rest stay ink.
.rp_table_theme <- function(pal, n, bold = NULL, text_col = NULL, head_parse = FALSE, base_size = .rp_base_size, fill_vec = NULL) {
  ff  <- if (is.null(bold)) "plain" else ifelse(as.logical(rep_len(bold, n)), "bold", "plain")
  col <- if (is.null(text_col)) pal$ink else ifelse(is.na(text_col) | !nzchar(text_col), pal$ink, text_col)
  # head_parse = TRUE renders the column headers as plotmath expressions (for subscripts/Greek, e.g. T[b]).
  # RULE: every report table centres its content and headers both horizontally and vertically (hjust/vjust
  # = 0.5, x/y = 0.5). Applies to all modules via this shared theme. `base_size` lets specific tables (kinetic
  # parameters, model comparison) use a larger, more readable body font than the default .rp_base_size.
  # `fill_vec` (full nrow*ncol COLUMN-MAJOR colour vector) overrides the default zebra fill — used to highlight
  # best-in-column cells (e.g. the model-comparison table) exactly like the on-screen conditional formatting.
  gridExtra::ttheme_minimal(
    base_size = base_size, base_family = .rp_font(), padding = unit(c(4, 3.4), "mm"),
    core = list(fg_params = list(hjust = 0.5, x = 0.5, vjust = 0.5, y = 0.5, col = col, fontface = ff),
                bg_params = list(fill = if (is.null(fill_vec)) c(rep(c("white", pal$zebra), length.out = n)) else fill_vec, col = NA)),
    colhead = list(fg_params = list(col = "white", fontface = "bold", hjust = 0.5, x = 0.5, vjust = 0.5, y = 0.5,
                                    parse = isTRUE(head_parse)),
                   bg_params = list(fill = pal$primary2, col = NA)))
}
# Relabel the core text grobs of ONE column with parsed plotmath expressions (math_col). gtable core cells are
# all named "core-fg" (unindexed) so they are located by layout position: data row r -> gtable row r+1 (header
# is row 1), data column j -> gtable column j. A non-parseable cell is left as-is (literal text).
.rp_relabel_math <- function(tg, src, col_idx) {
  lay <- tg$layout
  for (r in seq_along(src)) {
    gi <- which(lay$name == "core-fg" & lay$t == (r + 1L) & lay$l == col_idx)
    if (length(gi) == 1L && !is.na(src[r]) && nzchar(src[r])) {
      ex <- tryCatch(parse(text = src[r]), error = function(e) NULL)
      if (!is.null(ex) && length(ex)) tg$grobs[[gi]]$label <- ex
    }
  }
  tg
}
# Stable fractional column widths for a (formatted) data.frame: proportional to natural content, but no single
# column may exceed `cap` of the content width — the overflow is redistributed to the others so a long cell
# (e.g. a product-name list) wraps within its column instead of stretching the table and clipping. Computed
# once per table so split chunks across pages stay aligned.
.rp_col_frac <- function(df, pal, header_math = NULL, base_size = .rp_base_size) {
  nc <- ncol(df)
  if (!is.null(header_math)) names(df) <- header_math   # measure header widths from the plotmath labels
  tg0 <- gridExtra::tableGrob(df, rows = NULL, theme = .rp_table_theme(pal, nrow(df), head_parse = !is.null(header_math), base_size = base_size))
  nat <- grid::convertWidth(tg0$widths, "in", valueOnly = TRUE)
  fr  <- if (isTRUE(sum(nat) > 0)) nat / sum(nat) else rep(1 / nc, nc)
  cap <- if (nc <= 3) 0.60 else 0.34
  if (any(fr > cap)) {
    ex <- pmax(fr - cap, 0); fr <- pmin(fr, cap); donors <- fr < cap
    if (any(donors) && sum(fr[donors]) > 0) fr[donors] <- fr[donors] + sum(ex) * fr[donors] / sum(fr[donors])
    fr <- fr / sum(fr)
  }
  fr
}
# tableGrob stretched to fill the content width, with per-column text WRAPPING so long cells never overflow.
.rp_make_tablegrob <- function(df, pal, bold = NULL, col_frac = NULL, text_col = NULL, header_math = NULL,
                               base_size = .rp_base_size, math_col_idx = NULL, cell_fill = NULL) {
  df <- .rp_df_fmt(df)
  math_src <- if (!is.null(math_col_idx)) as.character(df[[math_col_idx]]) else NULL   # plotmath source for relabel
  # per-cell background highlight: start from the zebra-by-row pattern, overlay the requested cell colours.
  fill_vec <- NULL
  if (!is.null(cell_fill)) {
    nr <- nrow(df); nc <- ncol(df)
    base <- matrix(rep(c("white", pal$zebra), length.out = nr), nrow = nr, ncol = nc)
    cf0 <- as.matrix(cell_fill); cf <- matrix("", nr, nc)                      # align cell_fill to the table dims
    rr <- seq_len(min(nr, nrow(cf0))); cc <- seq_len(min(nc, ncol(cf0))); cf[rr, cc] <- cf0[rr, cc]
    ov <- !is.na(cf) & nzchar(cf); base[ov] <- cf[ov]
    fill_vec <- as.vector(base)
  }
  w_target <- .RP$pw - 2 * .RP$mx
  if (is.null(col_frac)) col_frac <- .rp_col_frac(df, pal, header_math = header_math, base_size = base_size)
  col_frac  <- col_frac / sum(col_frac)
  widths_in <- col_frac * w_target
  padx <- 4 / 25.4                                   # horizontal cell padding (theme padding[1] = 4 mm)
  nm <- names(df)
  for (j in seq_len(ncol(df))) {
    if (!is.null(math_col_idx) && j == math_col_idx) next     # math column stays intact (plotmath source, relabeled below)
    budget <- max(4, floor((widths_in[j] - padx) / .rp_char_w(base_size)))
    df[[j]] <- .rp_wrap_cell(df[[j]], budget)
    if (is.null(header_math)) nm[j] <- .rp_wrap_cell(nm[j], budget)   # wrap plain headers; plotmath headers are left intact
  }
  names(df) <- if (!is.null(header_math)) header_math else nm   # plotmath labels (parsed) or the wrapped plain names
  tg <- gridExtra::tableGrob(df, rows = NULL, theme = .rp_table_theme(pal, nrow(df), bold, text_col, head_parse = !is.null(header_math), base_size = base_size, fill_vec = fill_vec))
  tg$widths <- grid::unit(widths_in, "in")
  if (!is.null(math_col_idx) && !is.null(math_src)) tg <- .rp_relabel_math(tg, math_src, math_col_idx)
  tg
}
# reliable tableGrob height (sum of row heights) in inches
.rp_tg_h_in <- function(tg) grid::convertHeight(sum(tg$heights), "in", valueOnly = TRUE)
# A table BLOCK. Splittable across pages (row-wise), header repeated on each chunk. `bold` (logical, length
# nrow) highlights key rows; `continued` is the localised word appended to the title on continuation chunks.
# `col_frac` overrides the auto column-width fractions (e.g. to widen a Sustainability column so a word like
# "Recommended" is not clipped). `text_col` is a matrix (nrow x ncol) of per-cell text colours (NA/"" -> ink)
# to colour-code cells; it is sliced with the table when it splits across pages.
rp_table_block <- function(df, title = NULL, pal = rp_palette(), digits = 2, bold = NULL, continued = NULL,
                           col_frac = NULL, text_col = NULL, header_math = NULL, title_size = NULL,
                           body_size = NULL, math_col = NULL, cell_fill = NULL) {
  if (is.null(df) || !nrow(df)) return(rp_spacer_block(0))
  if (!is.null(title) && !nzchar(title)) title <- NULL
  bs <- body_size %||% .rp_base_size
  build <- function() .rp_quiet_fonts({
    df <- .rp_df_fmt(df, digits)
    # math_col: the index (or name) of a column whose cells are plotmath SOURCE strings (rendered as true math).
    mci <- if (is.null(math_col)) NULL else if (is.character(math_col)) match(math_col, names(df)) else as.integer(math_col)
    if (!is.null(mci) && (is.na(mci) || mci < 1 || mci > ncol(df))) mci <- NULL
    if (is.null(col_frac)) {                           # measure widths with the math column's VISIBLE text, not its source
      dfv <- df; if (!is.null(mci)) dfv[[mci]] <- .rp_pm_visible(dfv[[mci]])
      col_frac <- .rp_col_frac(dfv, pal, header_math = header_math, base_size = bs)
    }
    tc_vec <- if (!is.null(text_col)) as.vector(as.matrix(text_col)) else NULL
    tg <- .rp_make_tablegrob(df, pal, bold = bold, col_frac = col_frac, text_col = tc_vec, header_math = header_math,
                             base_size = bs, math_col_idx = mci, cell_fill = cell_fill)
    h_tbl <- .rp_tg_h_in(tg); row_h <- h_tbl / (nrow(df) + 1)
    th <- if (!is.null(title)) 0.36 else 0
    list(type = "table", df = df, title = title, pal = pal, grob = tg,
         h = h_tbl + th, split = TRUE, header_h = row_h, title_h = th, title_size = title_size,
         bold = bold, col_frac = col_frac, continued = continued, text_col = text_col, header_math = header_math,
         base_size = bs, math_col = mci, cell_fill = cell_fill)
  })
  # plotmath headers / math cells must be measured + rendered on a cairo device (the default pdf device errors on plotmath)
  if (!is.null(header_math) || !is.null(math_col)) .rp_on_cairo(build()) else build()
}

# A block of "label: value" lines with BOLD labels — compact key-value summaries (e.g. screening conditions,
# solute identity), so each field stands out on its own line. `pairs` = list(list(label=, value=), ...).
# Long values (e.g. a SMILES string) WRAP onto continuation lines aligned under the value, so they never run
# off the page.
rp_keyvals_block <- function(pairs, size = 10, pal = rp_palette()) {
  pairs <- Filter(function(p) !is.null(p$value) && nzchar(trimws(as.character(p$value))), pairs)
  n <- length(pairs); if (!n) return(rp_spacer_block(0))
  lh <- (size / 72) * 1.5                                   # line height (inches)
  # Conservative glyph width: SMILES-type values are mostly wide glyphs (C O ( ) =), so the 0.48em mean
  # under-budgets and the line overruns; 0.62em keeps wrapped lines inside the content width.
  cw <- (size / 72) * 0.62; content_w <- .RP$pw - 2 * .RP$mx
  # word-wrap, then hard-split any remaining over-long token (e.g. a SMILES string has no spaces to break on)
  wrap_hard <- function(s, budget) {
    lines <- strwrap(s, width = budget); if (!length(lines)) lines <- s
    unlist(lapply(lines, function(ln) if (nchar(ln) <= budget) ln else {
      idx <- seq(1L, nchar(ln), by = budget); substring(ln, idx, pmin(idx + budget - 1L, nchar(ln)))
    }), use.names = FALSE)
  }
  gl <- grid::gList(); line <- 0L                           # running line index from the top
  for (i in seq_len(n)) {
    p <- pairs[[i]]
    lab <- paste0(p$label, ":  ")
    budget <- max(8L, floor(content_w / cw) - nchar(lab))   # chars for the value, after the bold label
    wrapped <- wrap_hard(as.character(p$value), budget); if (!length(wrapped)) wrapped <- ""
    labgrob <- grid::textGrob(lab, gp = grid::gpar(fontsize = size, fontface = "bold"))
    yy <- unit(1, "npc") - unit(line * lh, "in")
    gl <- grid::gList(gl,
      grid::textGrob(lab, x = unit(0, "npc"), y = yy, just = c("left", "top"),
                     gp = grid::gpar(fontsize = size, fontface = "bold", col = pal$ink)),
      grid::textGrob(wrapped[1], x = unit(0, "npc") + grid::grobWidth(labgrob), y = yy,
                     just = c("left", "top"), gp = grid::gpar(fontsize = size, col = pal$ink)))
    line <- line + 1L
    for (k in seq_len(length(wrapped) - 1L) + 1L) {         # continuation lines, indented under the value
      yy2 <- unit(1, "npc") - unit(line * lh, "in")
      gl <- grid::gList(gl, grid::textGrob(wrapped[k], x = unit(0, "npc") + grid::grobWidth(labgrob), y = yy2,
                        just = c("left", "top"), gp = grid::gpar(fontsize = size, col = pal$ink)))
      line <- line + 1L
    }
  }
  list(type = "keyvals", grob = grid::gTree(children = gl), h = 0.05 + line * lh, split = FALSE)
}

# A plot block. `shrink = TRUE` lets the paginator scale the plot DOWN (never up) to fill the remaining
# space on the current page when it would otherwise overflow to a fresh page and leave this one half-empty
# — but never below `min_height` (keeps the chart legible; default = min(height, 2.0)). The grob fills its
# body viewport, so reducing the stored height simply draws it shorter.
# A ggplot that carries theme(aspect.ratio=...) draws a fixed-shape panel centred in the (wide) body viewport
# — wasting half the page width in a PDF. Strip it so the panel fills the allotted width.
.rp_unfix_ar <- function(plot) { if (inherits(plot, "ggplot")) plot$theme$aspect.ratio <- NULL; plot }
rp_plot_block <- function(plot, height = 3.0, title = NULL, pal = rp_palette(), shrink = FALSE, min_height = NULL,
                          keep_next = FALSE) {
  g <- .rp_quiet_fonts(if (inherits(plot, "ggplot")) ggplotGrob(.rp_unfix_ar(plot)) else plot)
  th <- if (!is.null(title)) 0.36 else 0
  mh <- if (isTRUE(shrink)) (min(if (is.null(min_height)) 2.0 else min_height, height) + th) else NULL
  # keep_next = TRUE: the paginator keeps this plot with the block that follows (e.g. a figure above its
  # table) rather than stranding it at the foot of a page away from it.
  list(type = "plot", grob = g, title = title, pal = pal, h = height + th, split = FALSE,
       title_h = th, plot_h = height, shrink = isTRUE(shrink), min_h = mh, keep_next = isTRUE(keep_next))
}

# A ROW of up to `ncol` plots side-by-side (2-column grid). `plots` is a list of ggplots/grobs. Used for
# diagnostic-plot galleries (e.g. DOE Analysis) so pairs of charts sit together instead of one-per-row.
rp_plot_grid_block <- function(plots, height = 2.4, ncol = 2L, pal = rp_palette()) {
  plots <- Filter(Negate(is.null), plots); n <- length(plots); if (!n) return(rp_spacer_block(0))
  grobs <- lapply(plots, function(p) .rp_quiet_fonts(if (inherits(p, "ggplot")) ggplotGrob(.rp_unfix_ar(p)) else p))
  gap <- 0.12
  gl <- grid::gList()
  for (i in seq_len(n)) {
    col <- (i - 1L) %% ncol; cw <- 1 / ncol
    x0 <- col * cw
    gl <- grid::gList(gl, grid::gTree(children = grid::gList(grobs[[i]]),
      vp = grid::viewport(x = unit(x0 + cw / 2, "npc"), y = unit(0.5, "npc"),
        width = unit(cw, "npc") - unit(gap, "in"), height = unit(0.94, "npc"))))
  }
  list(type = "plot", grob = grid::gTree(children = gl), title = NULL, pal = pal,
       h = height + 0.08, split = FALSE, title_h = 0, plot_h = height, shrink = FALSE, min_h = NULL)
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

# =====================================================================================================
# PAGE FRAME (header + footer) ------------------------------------------------------------------------
# =====================================================================================================
.rp_draw_header <- function(header, pal) {
  band_h <- 0.92
  top <- .RP$ph                                                        # page top (in from bottom)
  # coloured band across the top
  grid::pushViewport(.rp_vp(0, top, .RP$pw, band_h))
  grid::grid.rect(gp = grid::gpar(fill = pal$primary, col = NA))
  # logo (optional) — drawn at its natural aspect ratio on a white rounded backing box so a dark or
  # coloured wordmark stays legible against the coloured header band.
  lx <- .RP$mx
  if (!is.null(header$logo) && file.exists(header$logo) && requireNamespace("png", quietly = TRUE)) {
    img <- tryCatch(png::readPNG(header$logo), error = function(e) NULL)
    if (!is.null(img)) {
      dims  <- dim(img); ar <- if (length(dims) >= 2 && dims[1] > 0) dims[2] / dims[1] else 1  # width/height
      logo_h <- 0.42; logo_w <- min(logo_h * ar, 2.6)                                          # cap very wide marks
      if (logo_w / logo_h < ar) logo_h <- logo_w / ar                                          # keep ratio when capped
      padx <- 0.11; pady <- 0.09
      box_w <- logo_w + 2 * padx; box_h <- logo_h + 2 * pady
      grid::grid.roundrect(x = unit(.RP$mx + box_w / 2, "in"), y = unit(0.5, "npc"),
        width = unit(box_w, "in"), height = unit(box_h, "in"), r = unit(6, "pt"),
        gp = grid::gpar(fill = "white", col = NA))
      grid::pushViewport(viewport(x = unit(.RP$mx + box_w / 2, "in"), y = unit(0.5, "npc"),
        width = unit(logo_w, "in"), height = unit(logo_h, "in"), just = "centre"))
      grid::grid.raster(img); grid::popViewport()
      lx <- .RP$mx + box_w + 0.18
    }
  }
  grid::grid.text(header$company %||% "supeRcrit", x = unit(lx, "in"), y = unit(0.62, "npc"),
    just = "left", gp = grid::gpar(col = "white", fontsize = 13, fontface = "bold"))
  grid::grid.text(header$title %||% "Analysis Report", x = unit(lx, "in"), y = unit(0.34, "npc"),
    just = "left", gp = grid::gpar(col = "white", fontsize = 10))
  if (!is.null(header$subtitle))
    grid::grid.text(header$subtitle, x = unit(.RP$pw - .RP$mx, "in"), y = unit(0.5, "npc"),
      just = "right", gp = grid::gpar(col = "white", fontsize = 8.5))
  grid::popViewport()
}
.rp_draw_footer <- function(page, total, footer, pal) {
  grid::pushViewport(.rp_vp(.RP$mx, .RP$mbot, .RP$pw - 2 * .RP$mx, 0.3))
  grid::grid.lines(x = unit(c(0, 1), "npc"), y = unit(0.95, "npc"), gp = grid::gpar(col = pal$rule, lwd = 0.8))
  grid::grid.text(footer$left %||% format(Sys.time(), "%d-%m-%Y %H:%M"),
    x = unit(0, "npc"), y = unit(0.55, "npc"), just = "left", gp = grid::gpar(col = pal$muted, fontsize = 8))
  grid::grid.text(sprintf(footer$page_fmt %||% "Page %d of %d", page, total),
    x = unit(1, "npc"), y = unit(0.55, "npc"), just = "right", gp = grid::gpar(col = pal$muted, fontsize = 8))
  if (!is.null(footer$center))
    grid::grid.text(footer$center, x = unit(0.5, "npc"), y = unit(0.55, "npc"), gp = grid::gpar(col = pal$muted, fontsize = 8))
  grid::popViewport()
}

# =====================================================================================================
# RENDERER --------------------------------------------------------------------------------------------
# =====================================================================================================
# Expand blocks into "draw items" that each fit on a page, and assign page numbers (measuring pass).
.rp_paginate <- function(blocks, continued = "continued") {
  ct <- .rp_content(); avail0 <- ct$h
  pages <- list(); cur <- list(); cur_used <- 0
  push_new_page <- function() { pages[[length(pages) + 1]] <<- cur; cur <<- list(); cur_used <<- 0 }
  add_item <- function(it, h) {
    if (cur_used + h > avail0 + 1e-6 && length(cur) > 0) push_new_page()
    cur[[length(cur) + 1]] <<- it; cur_used <<- cur_used + h + .RP$gap
  }
  # Height the START of a block needs on the current page — for keep-with-next look-ahead. A splittable
  # table only needs room for its title + header + ~1.6 rows to legitimately begin here; anything else
  # needs its whole height (capped at a page so an over-tall block still gets to start fresh).
  min_start_h <- function(b) {
    if (identical(b$type, "table") && isTRUE(b$split))
      (b$title_h %||% 0) + (b$header_h %||% 0.2) * 2.6
    else if (identical(b$type, "plot") && isTRUE(b$shrink))
      min(b$min_h %||% b$h, avail0)        # a shrinkable plot may begin wherever its minimum height fits
    else min(b$h, avail0)
  }
  n <- length(blocks)
  for (bi in seq_len(n)) {
    b <- blocks[[bi]]
    # keep-with-next: a heading must not be stranded at the page foot while its content flows overleaf.
    if (isTRUE(b$keep_next) && bi < n && length(cur) > 0 &&
        cur_used + b$h + .RP$gap + min_start_h(blocks[[bi + 1]]) > avail0 + 1e-6)
      push_new_page()
    if (identical(b$type, "table") && isTRUE(b$split)) {
      # split rows to fit remaining/whole pages
      df <- b$df; hh <- b$header_h; th <- b$title_h; pal <- b$pal
      cont <- b$continued %||% continued
      i <- 1; first <- TRUE
      while (i <= nrow(df)) {
        remain <- avail0 - (cur_used) - (if (first) th else 0) - hh - .RP$gap
        if (remain < hh * 1.5 && length(cur) > 0) { push_new_page(); remain <- avail0 - th - hh - .RP$gap }
        nfit <- max(1, floor(remain / hh))
        j <- min(nrow(df), i + nfit - 1)
        chunk <- df[i:j, , drop = FALSE]
        tg <- .rp_make_tablegrob(chunk, pal, bold = if (!is.null(b$bold)) b$bold[i:j] else NULL, col_frac = b$col_frac,
          text_col = if (!is.null(b$text_col)) as.vector(as.matrix(b$text_col[i:j, , drop = FALSE])) else NULL,
          header_math = b$header_math, base_size = b$base_size %||% .rp_base_size, math_col_idx = b$math_col,
          cell_fill = if (!is.null(b$cell_fill)) b$cell_fill[i:j, , drop = FALSE] else NULL)
        ttl <- if (first) b$title else (if (!is.null(b$title)) paste0(b$title, " (", cont, ")") else NULL)
        tth <- if (!is.null(ttl)) th else 0
        h <- .rp_tg_h_in(tg) + tth
        add_item(list(type = "table", grob = tg, title = ttl, pal = pal, h = h, title_h = tth, title_size = b$title_size), h)
        i <- j + 1; first <- FALSE
      }
    } else {
      # shrink-to-fit: a shrinkable plot that would overflow the space left on a non-empty page is scaled
      # DOWN to fill it (rather than migrating to a fresh page and leaving this one half-empty), as long as
      # the remaining space still clears its minimum legible height. The grob fills its viewport, so we only
      # reduce the stored height. keep-with-next above already used the plot's floor, so if we reach here on
      # a non-empty page the floor is guaranteed to fit.
      if (identical(b$type, "plot") && isTRUE(b$shrink) && length(cur) > 0) {
        remain <- avail0 - cur_used
        if (b$h > remain + 1e-6 && (remain - .RP$gap) >= (b$min_h %||% b$h) - 1e-6) {
          b$h <- remain - .RP$gap; b$plot_h <- b$h - (b$title_h %||% 0)
        }
      }
      # keep-with-previous: a small trailing caption/note must not be orphaned onto a fresh page away from
      # the block it annotates. If it won't fit on the current page, move the annotated block onto the next
      # page WITH it — plus a section heading sitting directly above that block, so a title is not left
      # behind either. Only when earlier content remains on the page (never empties it) and the whole group
      # fits together on a fresh page; otherwise leave things as-is.
      if (isTRUE(b$keep_prev) && length(cur) >= 2 && cur_used + b$h + .RP$gap > avail0 + 1e-6) {
        take_one <- function() { it <- cur[[length(cur)]]; cur[[length(cur)]] <<- NULL
          cur_used <<- cur_used - ((it$h %||% 0) + .RP$gap); it }
        pulled <- list(take_one())                                   # the annotated block/last table chunk
        if (length(cur) >= 2 && isTRUE(cur[[length(cur)]]$keep_next)) pulled[[2]] <- take_one()  # its heading
        pulled <- rev(pulled)
        gh <- sum(vapply(pulled, function(x) (x$h %||% 0) + .RP$gap, numeric(1))) + b$h + .RP$gap
        if (length(cur) >= 1 && gh <= avail0 + 1e-6) {
          push_new_page(); for (g in pulled) add_item(g, g$h %||% 0)
        } else for (g in pulled) { cur[[length(cur) + 1]] <- g; cur_used <- cur_used + (g$h %||% 0) + .RP$gap }
      }
      add_item(b, b$h)
    }
  }
  if (length(cur)) push_new_page()
  pages
}

.rp_draw_item <- function(it, x_in, top_in, w_in) {
  th <- it$title_h %||% 0
  if (!is.null(it$title) && th > 0) {
    grid::pushViewport(.rp_vp(x_in, top_in, w_in, th))
    # Bottom-justify with an absolute clearance so descenders (ğ, p, y) never dip into the table header
    # or plot that sits immediately below (previously vjust-centred, which let the text overlap).
    grid::grid.text(it$title, x = unit(0, "npc"), y = unit(0.13, "in"), just = c("left", "bottom"),
      gp = grid::gpar(fontsize = it$title_size %||% 11, fontface = "bold", col = (it$pal %||% rp_palette())$primary))
    grid::popViewport(); top_in <- top_in - th
  }
  body_h <- it$h - th
  # Tables are anchored TOP-left and left-justified (their widths already fill the content area) so the
  # title sits directly above them; other grobs fill the body viewport.
  if (identical(it$type, "table")) {
    grid::pushViewport(viewport(x = unit(x_in, "in"), y = unit(top_in, "in"),
      width = unit(w_in, "in"), height = unit(body_h, "in"), just = c("left", "top")))
  } else {
    grid::pushViewport(.rp_vp(x_in, top_in, w_in, body_h))
  }
  grid::grid.draw(it$grob)
  grid::popViewport()
}

rp_render_report <- function(file, blocks, header = list(), footer = list(), palette = rp_palette(),
                             continued = "continued") .rp_quiet_fonts({
  # pagination re-measures any table that splits across pages; if any table has plotmath headers, do that on
  # a cairo device (the default pdf device errors on plotmath — see .rp_on_cairo).
  has_math <- any(vapply(blocks, function(b) !is.null(b$header_math) || !is.null(b$math_col), logical(1)))
  pages <- if (has_math) .rp_on_cairo(.rp_paginate(blocks, continued = continued)) else .rp_paginate(blocks, continued = continued)
  total <- max(1, length(pages))
  ct <- .rp_content()
  grDevices::cairo_pdf(filename = file, width = .RP$pw, height = .RP$ph, onefile = TRUE, family = .rp_font())
  on.exit(grDevices::dev.off(), add = TRUE)
  for (p in seq_len(total)) {
    if (p > 1) grid::grid.newpage()
    .rp_draw_header(header, palette)
    .rp_draw_footer(p, total, footer, palette)
    cur_top <- ct$top
    for (it in pages[[p]]) {
      .rp_draw_item(it, ct$x, cur_top, ct$w)
      cur_top <- cur_top - it$h - .RP$gap
    }
  }
  invisible(file)
})
