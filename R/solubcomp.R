#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FUNCTION: Compare scCO2 solubility enhancement for two or more solutes (ML counterpart to miscomp)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Compare scCO2 solubility of two or more solutes with a co-solvent (ML counterpart to miscomp)
#'
#' @description The machine-learning solubility counterpart to \code{\link{miscomp}}: rather than comparing
#' Hansen \emph{miscibility} enhancement, it compares the predicted supercritical-CO2 \strong{solubility}
#' (and the enhancement produced by a single co-solvent) for several solutes across a pressure-temperature
#' grid and a set of co-solvent mole fractions, using the machine-learned model through
#' \code{\link{solub_optim}}. It is the data-generating core behind the Co-Solvent Comparison Shiny module's
#' Solubility (ML) mode, and mirrors \code{\link{miscomp}}'s \code{list(data, plots)} return contract.
#'
#' Unlike \code{\link{miscomp}}, the temperature argument accepts \strong{several} temperatures (the ML model
#' takes temperature as a direct feature), so the returned \code{data} carries a \code{Temperature} column.
#' A single co-solvent, or a co-solvent \strong{blend}, may be compared: a blend is evaluated as a genuine
#' \strong{n-component} solve by the COSMO-SAC-Phi EoS engines (\code{ocrsphi}, \code{csp}); the Makarov ML
#' engine has no blend-mixing rule and compares a single co-solvent only.
#'
#' @param solutes A \code{data.frame} (or object coercible to one) with at least \code{name} and
#' \code{smiles} columns; optional \code{Tm} (melting point, K) and \code{mw} (g/mol) per solute are used
#' when present and estimated otherwise.
#' @param cosolvent A single co-solvent as a \strong{named} character (\code{c(Ethanol = "CCO")}) or a plain
#' SMILES string; or a \strong{vector} of SMILES (optionally named) for a co-solvent \strong{blend}. SMILES
#' for the bundled Hansen solvents are in the \code{SMILES} column of \code{\link{show_solv}}.
#' @param temp_C \code{numeric} vector of temperatures (deg C), up to the caller's discretion.
#' @param pres_bar \code{numeric} vector of pressures (bar).
#' @param x \code{numeric} vector of co-solvent mole fractions (0-1). Defaults to \code{seq(0.05, 0.40, 0.05)}.
#'   For a blend these are the \strong{total} co-solvent mole fractions swept along the x-axis.
#' @param blend_ratio for a blend \code{cosolvent}, the composition (relative parts, normalised to sum 1;
#'   defaults to equal parts). Each component's mole fraction at total \code{xf} is \code{xf * ratio_k}.
#'   Blends are evaluated n-component by the EoS engines (\code{ocrsphi}, \code{csp}); the Makarov ML engine
#'   does not support blends.
#' @param cols Either \code{"default"} or a named vector of up to six plot colours (names \code{"one"} ...
#' \code{"six"}), as in \code{\link{miscomp}}.
#' @param plt_title A \code{logical}: add titles to the static plots.
#' @param draw A \code{logical}: auto-print the static plots (\code{TRUE} by default).
#' @param silent A \code{logical}: suppress console progress when \code{TRUE}.
#' @param workers number of parallel worker processes for the (independent) per-solute x per-fraction
#'   \code{\link{solub_optim}} evaluations. \code{1} (default) runs sequentially; \code{>1} spreads the
#'   solute x fraction jobs across a PSOCK cluster (each worker its own process, so the COSMO-SAC-Phi
#'   engines' C++ state stays isolated). When there is only a SINGLE job, the workers are instead handed to
#'   \code{solub_optim} for grid-level parallelism, so cores stay busy either way. Results are numerically
#'   identical to the sequential path; only the wall-clock changes. The EoS engines (\code{csp}/
#'   \code{ocrsphi}) benefit; the vectorised \code{makarov} model does not.
#'
#' @return A named \code{list} with:
#' \describe{
#' \item{data}{A long \code{data.frame}: \code{Solute}, \code{Fraction}, \code{Temperature}, \code{Pressure},
#'  \code{Enhancement} (y2 ratio vs pure CO2), \code{Solubility_molfrac} (absolute y2, mol/mol) and \code{mw}.}
#' \item{plots}{Static \code{ggplot} objects mirroring \code{\link{miscomp}} (per solute -> lines per pressure;
#'  \code{"pres_<P>"} -> lines per solute), evaluated at the first temperature. The Shiny module builds
#'  richer, temperature-aware plots reactively.}
#' \item{cosolvent, fractions, temp_C, pres_bar, n_out}{The co-solvent name, the fractions/grid used, and the
#'  number of grid points that fell outside the model's reliable domain.}
#' }
#' @seealso \code{\link{miscomp}}, \code{\link{solub_optim}}, \code{\link{predict_solubility}}, \code{\link{show_solv}}.
#' @export
#'
#' @examples
#' \dontrun{
#' cs <- stats::setNames(show_solv()$SMILES, show_solv()$Solvent)   # solvent name -> SMILES
#' sols <- data.frame(name = c("Naphthalene", "Vanillin"),
#'                    smiles = c("c1ccc2ccccc2c1", "O=Cc1ccc(O)c(OC)c1"))
#' cmp <- solubcomp(sols, cosolvent = cs["Ethanol"],
#'                  temp_C = c(40, 50, 60), pres_bar = c(150, 250), x = c(0.05, 0.10))
#' head(cmp$data)
#' }
#'
#' @import ggplot2
solubcomp <- function(solutes, cosolvent, temp_C, pres_bar, x = seq(0.05, 0.40, 0.05),
                      blend_ratio = NULL, cols = "default", plt_title = TRUE, draw = TRUE, silent = FALSE,
                      engine = c("makarov", "ocrsphi", "csp"), workers = 1L) {
  engine <- match.arg(engine)
  solutes <- as.data.frame(solutes, stringsAsFactors = FALSE)
  if (!all(c("name", "smiles") %in% names(solutes)))
    stop("`solutes` must have at least 'name' and 'smiles' columns.", call. = FALSE)
  if (length(cosolvent) < 1 || !nzchar(as.character(cosolvent)[1]))
    stop("`cosolvent` must be a (optionally named) SMILES string, or a vector of SMILES for a blend.", call. = FALSE)

  # A single co-solvent (length 1) or a BLEND (length > 1). For a blend, `x` sweeps the TOTAL co-solvent
  # mole fraction and `blend_ratio` fixes the composition (defaults to equal parts); each component's
  # mole fraction at total xf is xf * ratio_k.
  cos_smi <- unname(as.character(cosolvent))
  cn <- names(cosolvent); cos_nms <- if (!is.null(cn) && all(nzchar(cn))) cn else cos_smi
  is_blend <- length(cos_smi) > 1
  if (is_blend) {
    ratio <- if (is.null(blend_ratio)) rep(1 / length(cos_smi), length(cos_smi)) else as.numeric(blend_ratio)
    if (length(ratio) != length(cos_smi)) stop("`blend_ratio` must have one value per co-solvent.", call. = FALSE)
    ratio <- ratio / sum(ratio)
    cos_name <- paste(cos_nms, collapse = " + ")
  } else {
    cos_name <- cos_nms[1]; cos_smiles <- stats::setNames(cos_smi[1], cos_name)
  }

  temp_C   <- sort(unique(as.numeric(temp_C)))
  pres_bar <- sort(unique(as.numeric(pres_bar)))
  if (!length(temp_C) || !length(pres_bar))
    stop("`temp_C` and `pres_bar` must each have at least one value.", call. = FALSE)
  fr <- sort(unique(as.numeric(x))); fr <- fr[is.finite(fr) & fr > 0 & fr < 1]
  if (!length(fr)) fr <- 0.05

  # Flatten the (valid solute x fraction) grid into independent jobs. Each job is one solub_optim() call, so
  # they can be spread across a PSOCK cluster (workers > 1). Nested parallelism is avoided: the outer loop is
  # parallelised when there are >= 2 jobs; only when there is a SINGLE job are the workers handed to
  # solub_optim for grid-level parallelism instead. Results are identical to the sequential path.
  valids <- list()
  for (i in seq_len(nrow(solutes))) {
    smi <- solutes$smiles[i]
    if (is.null(smi) || is.na(smi) || !nzchar(smi)) next
    nm <- solutes$name[i]; if (is.null(nm) || is.na(nm) || !nzchar(nm)) nm <- smi
    tmv <- if ("Tm" %in% names(solutes)) suppressWarnings(as.numeric(solutes$Tm[i])) else NA_real_
    tmv <- if (length(tmv) == 1 && is.finite(tmv)) tmv else NULL
    valids[[length(valids) + 1]] <- list(nm = nm, smi = smi, tmv = tmv)
  }
  jobs <- list()
  for (v in valids) for (xf in fr) jobs[[length(jobs) + 1]] <- c(v, list(xf = xf))
  if (!length(jobs))
    stop("No solubility results were produced (check that the solutes carry valid SMILES).", call. = FALSE)

  nw <- suppressWarnings(as.integer(workers)[1]); if (is.na(nw) || nw < 1L) nw <- 1L
  outer_workers <- if (nw > 1L && length(jobs) > 1L) nw else 1L
  inner_workers <- if (nw > 1L && length(jobs) == 1L) nw else 1L

  # One job -> a solub_optim call keyed by engine (EoS by NAME, makarov by SMILES) -> per-temperature row blocks.
  run_job <- function(job) {
    nm <- job$nm; smi <- job$smi; tmv <- job$tmv; xf <- job$xf
    sc <- tryCatch(
      if (is_blend) {
        # n-component blend: the COSMO-SAC-Phi EoS engines (csp / ocrsphi) evaluate a blend directly, keyed
        # by database NAME. The Makarov ML model has no blend-mixing rule, so it does not blend.
        if (engine == "makarov") NULL
        else solub_optim(nm, temp_C = temp_C, pres_bar = pres_bar, Tm = tmv, silent = TRUE, engine = engine,
                         workers = inner_workers,
                         blends = stats::setNames(list(list(names = cos_nms, x = xf * ratio)), cos_name))
      } else
        solub_optim(if (engine %in% c("ocrsphi", "csp")) nm else smi, cosolvents = cos_smiles,
                    temp_C = temp_C, pres_bar = pres_bar, x = xf, Tm = tmv, silent = TRUE, engine = engine,
                    workers = inner_workers),
      error = function(e) NULL)
    if (is.null(sc) || is.null(sc$enhancement[[cos_name]])) return(NULL)
    enh <- sc$enhancement[[cos_name]]; sol <- sc$solubility[[cos_name]]
    Pr <- as.numeric(rownames(enh)); Tc <- as.numeric(colnames(enh))
    rws <- lapply(seq_along(Tc), function(ti) data.frame(Solute = nm, Fraction = xf, Temperature = Tc[ti],
      Pressure = Pr, Enhancement = round(as.numeric(enh[, ti]), 3),
      Solubility_molfrac = as.numeric(sol[, ti]), mw = sc$mw, stringsAsFactors = FALSE))
    list(rows = rws, n_out = sum(!sc$domain))
  }
  if (!silent) cat("\nComparing", length(valids), "solute(s) x", length(fr), "fraction(s)",
                   if (outer_workers > 1L) paste0(" across ", outer_workers, " cores") else "", "...")
  res <- .solub_lapply(jobs, run_job, workers = outer_workers)

  rows <- list(); n_out <- 0L
  for (r in res) { if (is.null(r)) next; rows <- c(rows, r$rows); n_out <- n_out + r$n_out }
  if (!length(rows))
    stop("No solubility results were produced (check that the solutes carry valid SMILES).", call. = FALSE)
  dat <- do.call(rbind, rows)

  plots <- .solubcomp_plots(dat, cos_name, temp_C[1], cols, isTRUE(plt_title), isTRUE(draw))

  list(data = dat, plots = plots, cosolvent = cos_name, fractions = fr,
       temp_C = temp_C, pres_bar = pres_bar, n_out = n_out)
}

# Static plots mirroring miscomp (per-solute lines-per-pressure + per-pressure lines-per-solute), evaluated
# at a single temperature. Kept internal; the Shiny app builds richer temperature-aware plots reactively.
.solubcomp_plots <- function(dat, cosolvent, at_temp, cols, plt_title, draw) {
  defcols <- c(one = "black", two = "blue", three = "darkgreen", four = "darkorange", five = "darkred", six = "purple")
  if (identical(cols, "default") || is.null(cols)) cols <- defcols
  d <- dat[dat$Temperature == at_temp, , drop = FALSE]
  base_theme <- ggplot2::theme(aspect.ratio = 1, panel.background = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(colour = "black", size = 12),
    axis.title = ggplot2::element_text(colour = "black", size = 13),
    axis.line = ggplot2::element_line(colour = "black"), legend.title = ggplot2::element_blank(),
    legend.position = "right", legend.text = ggplot2::element_text(size = 11))
  wants_log <- function(y) { yy <- y[is.finite(y) & y > 0]; length(yy) > 1 && (max(yy) / min(yy) > 100) }
  one_plot <- function(dd, grp_levels, title) {
    dd$grp <- factor(dd$grp, levels = grp_levels)
    p <- ggplot2::ggplot(dd, ggplot2::aes(x = .data[["Fraction"]], y = .data[["Y"]],
                                          colour = .data[["grp"]], shape = .data[["grp"]])) +
      ggplot2::geom_line() + ggplot2::geom_point(size = 2) +
      ggplot2::labs(title = if (plt_title) title else NULL,
                    x = paste0(cosolvent, " mole fraction"), y = "Solubility (mol/mol)") +
      ggplot2::scale_x_continuous(breaks = scales::breaks_pretty(n = 6)) +
      ggplot2::scale_colour_manual(values = unname(cols[seq_along(grp_levels)])) +
      ggplot2::scale_shape_manual(values = c(0:2, 5:7)[seq_along(grp_levels)]) + base_theme
    if (wants_log(dd$Y)) p <- p + ggplot2::scale_y_log10()
    p
  }
  plots <- list()
  press <- sort(unique(d$Pressure)); sols <- sort(unique(d$Solute))
  if (length(press) > 1) for (s in sols) {
    dd <- d[d$Solute == s, , drop = FALSE]
    dd <- data.frame(Fraction = dd$Fraction, Y = dd$Solubility_molfrac, grp = dd$Pressure)
    plots[[s]] <- one_plot(dd, press, paste0(s, " in ", cosolvent, " at ", at_temp, " C"))
  }
  if (length(sols) > 1) for (P in press) {
    dd <- d[d$Pressure == P, , drop = FALSE]
    dd <- data.frame(Fraction = dd$Fraction, Y = dd$Solubility_molfrac, grp = dd$Solute)
    plots[[paste0("pres_", P)]] <- one_plot(dd, sols, paste0("Solubility at ", P, " bar, ", at_temp, " C in ", cosolvent))
  }
  if (draw) for (p in plots) print(p)
  plots
}
