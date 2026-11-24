# eos_column_export.R
# Source export function for the Process Simulation > Fractionation Column module.
# Mirrors the app's other dedicated exports (e.g. desir_export): writes ALL results to a
# SINGLE .csv file, one ALL-CAPS section title per block, two blank lines between sections.
# (The stage-profile plot PNG is exported separately via the tab's "Export Plot" button.)
#
# Sections: run conditions; products (solvent-free) + summary; stage profile; hydraulic
# sizing (if supplied); optimisation responses + optimal conditions (if supplied).
#
# Dynamic translations: pass `tr` (a translator function, e.g. i18n_r()$t); all
# user-facing titles/headers are routed through it, falling back to English.
#
# Args:
#   result : the eos_state$result list (col, nms, sol_names, oil_z, nsol, reflux, N, ...)
#   sizing : the gceos_column_sizing() list, or NULL
#   optim  : list(overall_pct, conditions = data.frame(Variable, Value),
#                 responses = data.frame(Response, Value, Weight)) or NULL
#   tr     : translator function (default identity)
#   file   : full path of the .csv file to write
eos_column_export <- function(result, sizing = NULL, optim = NULL, tr = function(x) x,
                              file = "column_results.csv", silent = FALSE) {
  if (is.null(result) || is.null(result$col))
    stop("eos_column_export: 'result' must be a converged column result.", call. = FALSE)
  L <- function(x) { y <- tryCatch(tr(x), error = function(e) x); if (is.null(y) || !nzchar(y)) x else y }

  sfree <- function(v, sidx) { v <- v[-sidx]; s <- sum(v)
    if (!is.finite(s) || s <= 0) v else v / s }
  num <- function(x) format(round(as.numeric(x), 6), trim = TRUE, scientific = FALSE)
  has_dt <- requireNamespace("data.table", quietly = TRUE)
  put_tbl <- function(df) {
    if (has_dt) data.table::fwrite(df, file = file, append = TRUE, col.names = TRUE, bom = FALSE)
    else suppressWarnings(utils::write.table(df, file = file, sep = ",", row.names = FALSE,
            col.names = TRUE, append = TRUE, qmethod = "double"))
  }
  # each section: two blank lines, then the ALL-CAPS (translated) title, optional note lines,
  # then the table. The very first cat() truncates the file; all others append.
  started <- FALSE
  sec <- function(title, df = NULL, lines = NULL) {
    cat(if (started) "\n\n\n" else "", toupper(L(title)), "\n", file = file, sep = "", append = started)
    started <<- TRUE
    if (length(lines)) for (ln in lines) cat(ln, "\n", file = file, sep = "", append = TRUE)
    if (!is.null(df)) put_tbl(df)
  }

  col <- result$col; nms <- result$nms; sidx <- length(nms)
  nsol <- result$nsol; reflux <- result$reflux %||% 0; sol <- result$sol_names

  # ---- run conditions ----------------------------------------------------
  sec(L("Run conditions"), data.frame(
    Quantity = c(L("Components"), L("Stages"), L("Oil feed stage"), L("CO2 feed stage"),
                 L("Reflux ratio"), paste0("S/F (", result$sf_unit %||% "mol", ")"),
                 L("Temperature (°C)"), L("Pressure (bar)"), L("HETP (m)"), L("Status")),
    Value = c(paste(sol, collapse = " + "), result$N, result$feedL, result$feedV,
              num(reflux), num(result$sf_disp %||% result$SF),
              num(result$temp_c), num(result$P_bar), num(result$HETP %||% 0.685),
              as.character(col$status)),
    check.names = FALSE, stringsAsFactors = FALSE))

  # ---- products (solvent-free) + summary lines ---------------------------
  ext_sf <- if (reflux > 0) as.numeric(col$extract$comp)[-sidx] else sfree(as.numeric(col$extract$y), sidx)
  ext_sf <- ext_sf / sum(ext_sf)
  raff_sf <- sfree(as.numeric(col$raffinate$x), sidx)
  Eflow <- if (reflux > 0) col$extract$E else col$extract$V * (1 - as.numeric(col$extract$y)[sidx])
  topnm <- function(z) sol[which.max(z)]; purity <- function(z) max(z) * 100
  prod <- data.frame(
    Solute = sol, Feed = as.numeric(result$oil_z), Extract = as.numeric(ext_sf),
    Raffinate = as.numeric(raff_sf), Recovery_pct = 100 * as.numeric(col$recovery),
    check.names = FALSE, stringsAsFactors = FALSE)
  names(prod) <- c(L("Solute"), L("Feed"), L("Extract"), L("Raffinate"), paste0(L("Recovery"), " (%)"))
  sec(L("Products (solvent-free basis)"), prod, lines = c(
    sprintf("%s: %s @ %.1f%% (E = %s)", L("Extract"), topnm(ext_sf), purity(ext_sf), num(Eflow)),
    sprintf("%s: %s @ %.1f%% (L = %s)", L("Raffinate"), topnm(raff_sf), purity(raff_sf), num(col$raffinate$L))))

  # ---- stage composition profile -----------------------------------------
  xsf <- t(apply(col$x, 1, function(row) sfree(row, sidx)))   # N x nsol, solvent-free
  if (is.null(dim(xsf))) xsf <- matrix(xsf, ncol = nsol)
  prof <- data.frame(Stage = seq_len(result$N), check.names = FALSE)
  for (i in seq_len(nsol)) prof[[sol[i]]] <- xsf[, i]
  prof[[L("Temperature (°C)")]] <- if (!is.null(result$Tvec_C)) result$Tvec_C else rep(result$temp_c, result$N)
  sec(L("Stage composition profile (solvent-free liquid mole fraction)"), prof)

  # ---- hydraulic sizing --------------------------------------------------
  if (!is.null(sizing)) {
    sz <- sizing
    sec(L("Hydraulic sizing"), data.frame(
      Quantity = c(L("Column diameter (m)"), L("Packed height (m)"), L("Packed volume (m³)"),
                   L("Aspect ratio (H/D)"), L("Flooding (%)"), L("Superficial vapour velocity (m/s)"),
                   L("Superficial liquid velocity (m/s)"), L("Flooding velocity (m/s)"),
                   L("Vapour residence time (s)"), L("Feed rate at flooding (kg/h)"),
                   L("Vapour density (kg/m³)"), L("Liquid density (kg/m³)"), L("CO2 density (kg/m³)")),
      Value = c(num(sz$diameter), num(sz$height), num(sz$packed_volume), num(sz$aspect_ratio),
                num(sz$pct_flood), num(sz$u_super), num(sz$u_liq), num(sz$u_flood),
                num(sz$residence_v), num(sz$feed_at_flood), num(sz$rho_g), num(sz$rho_l), num(sz$rho_CO2)),
      check.names = FALSE, stringsAsFactors = FALSE))
  }

  # ---- optimisation results ----------------------------------------------
  if (!is.null(optim)) {
    if (!is.null(optim$responses) && nrow(optim$responses) > 0)
      sec(L("Optimisation - responses"), optim$responses,
          lines = if (!is.null(optim$overall_pct)) sprintf("%s: %.2f%%", L("Overall desirability"), optim$overall_pct))
    if (!is.null(optim$conditions))
      sec(L("Optimisation - optimal conditions"), optim$conditions)
  }

  if (!silent) message("eos_column_export: wrote ", file)
  invisible(file)
}
