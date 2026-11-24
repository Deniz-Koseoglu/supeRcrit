#' Solubility-based co-solvent optimisation for supercritical-CO2 fractionation
#'
#' The solubility-based counterpart to \code{\link{hsp_optim}}: instead of ranking co-solvents by Hansen
#' miscibility (a relative \eqn{R_a} distance that depends on temperature/pressure only through solvent
#' density grids), it evaluates the machine-learned scCO2 solubility model (\code{\link{predict_solubility}})
#' on a pressure-temperature grid for the pure solute and for each candidate co-solvent at a fixed
#' co-solvent mole fraction. Temperature and pressure enter the solubility model directly as features, so
#' no Hansen parameters, critical constants or molar-volume grids are needed — only the solute SMILES (and
#' its melting point, supplied or estimated with \code{\link{gcm_mp}}). For each co-solvent it returns the
#' absolute solubility \eqn{y_2} (solute mole fraction, mol/mol) and the \strong{enhancement ratio}
#' \eqn{y_2(\text{co-solvent})/y_2(\text{pure CO}_2)} over the grid.
#'
#' The model is reliable only within its training domain (roughly 308-373 K and 80-360 bar; co-solvent
#' mole fraction up to ~0.1). Grid points outside this window are flagged in the returned \code{domain}
#' matrices and a warning is issued.
#'
#' @param solute SMILES string of the solute (single compound).
#' @param cosolvents named character vector mapping a display name to a co-solvent SMILES, e.g.
#'   \code{c(Ethanol = "CCO", Acetone = "CC(C)=O")}. The SMILES for the bundled Hansen solvent set are in
#'   the \code{SMILES} column of \code{\link{show_solv}}. \code{NULL} computes the pure-CO2 baseline only.
#' @param temp_C numeric vector of temperatures in degrees Celsius (grid columns).
#' @param pres_bar numeric vector of pressures in bar (grid rows).
#' @param x co-solvent mole fraction (0-1; default 0.05).
#' @param Tm optional solute melting point (K); estimated with \code{\link{gcm_mp}} when omitted.
#' @param silent suppress console messages when \code{TRUE}.
#' @param blends optional named list of co-solvent \strong{blends} to evaluate alongside the single
#'   \code{cosolvents}. Each element is a list \code{list(smiles = <character vector>, x = <numeric
#'   per-component mole fractions>)}; it is scored by mole-fraction feature superposition in
#'   \code{\link{predict_solubility}} and appears in \code{solubility}/\code{enhancement}/\code{best}
#'   under its list name. Blends extrapolate a single-co-solvent-trained model — reliable for ranking,
#'   approximate in absolute value.
#' @param workers number of parallel worker processes for the solubility evaluation. \code{1} (default)
#'   runs sequentially; \code{>1} spreads the work across a PSOCK cluster. The baseline and each
#'   (independent) co-solvent are one job each; additionally, when FEWER co-solvents are screened than
#'   there are workers, each job's pressure/temperature grid is split into chunks so every core stays busy
#'   (grid-level parallelism). Each worker is its own R process, so the COSMO-SAC-Phi engines' C++ global
#'   state stays isolated. Because every (T,P) point is evaluated independently, results are numerically
#'   identical to the sequential path; only the wall-clock changes. The EoS engines (\code{csp}/
#'   \code{ocrsphi}) benefit most; the vectorised \code{makarov} model does not.
#' @param prog_dir optional path to an existing directory used to publish coarse progress markers for a
#'   UI (e.g. a Shiny progress bar polling a background run). When supplied, the function drops empty
#'   marker files as it advances: \code{ncos_<N>} (number of co-solvents), then either \code{cs_<k>} after
#'   each co-solvent finishes (co-solvent-level run) or, when the grid is chunked, \code{njobs_<M>} plus a
#'   \code{job_<i>_<c>} per finished chunk; followed by \code{stage_baseline}, \code{stage_blend},
#'   \code{stage_rank} and \code{stage_done}. Markers work with parallel \code{workers} (each writes its
#'   own file). Defaults to \code{NULL} (no progress reporting).
#' @return a list with: \code{pure} (a pressure x temperature matrix of pure-CO2 \eqn{y_2}); \code{solubility}
#'   and \code{enhancement} (named lists of pressure x temperature matrices, one per co-solvent);
#'   \code{best} (a pressure x temperature character matrix naming the highest-enhancement co-solvent at each
#'   point); \code{domain} (a logical pressure x temperature matrix, \code{TRUE} where in-domain); and
#'   \code{Tm} (the melting point used).
#' @seealso \code{\link{predict_solubility}}, \code{\link{hsp_optim}}, \code{\link{sfe_mod}}, \code{\link{show_solv}}.
#' @examples
#' \dontrun{
#' cs <- stats::setNames(show_solv()$SMILES, show_solv()$Solvent)   # solvent name -> SMILES
#' sc <- solub_optim("CC(=O)Nc1ccc(O)cc1",
#'         cosolvents = cs[c("Ethanol", "Acetone", "Methanol")],
#'         temp_C = c(40, 50, 60), pres_bar = c(100, 150, 200, 250), x = 0.05)
#' sc$enhancement$Ethanol
#' }
#' @export
solub_optim <- function(solute, cosolvents = NULL, temp_C, pres_bar,
                        x = 0.05, Tm = NULL, silent = FALSE, blends = NULL, blend_top_n = 0,
                        engine = c("makarov", "ocrsphi", "csp"), workers = 1L, prog_dir = NULL) {
  engine <- match.arg(engine)
  # Progress marker helper: writes an empty file `<name>` into prog_dir (a no-op when prog_dir is NULL).
  .mark <- function(name) if (!is.null(prog_dir))
    try(file.create(file.path(prog_dir, name)), silent = TRUE)
  if (length(solute) != 1L || is.na(solute) || !nzchar(solute))
    stop("`solute` must be a single non-empty identifier (SMILES for makarov; database name for ocrsphi/csp).", call. = FALSE)
  temp_C   <- sort(unique(as.numeric(temp_C)))
  pres_bar <- sort(unique(as.numeric(pres_bar)))
  nt <- length(temp_C); np <- length(pres_bar)
  if (!nt || !np) stop("`temp_C` and `pres_bar` must each have at least one value.", call. = FALSE)
  tK <- temp_C + 273.15
  grid <- expand.grid(P = pres_bar, T = tK)               # pressure-major; per-point fns are vectorised over it
  to_mat <- function(v) matrix(v, nrow = np, ncol = nt,
                               dimnames = list(as.character(pres_bar), as.character(temp_C)))

  # Per-point solubility, dispatched by engine (same "solute, T, P, cosolvent, x -> y2" contract). The
  # co-solvent is passed as (name, value): makarov/ocrsphi use the value (SMILES), csp uses the name.
  #   makarov -> predict_solubility (ML, from SMILES; has a training domain)
  #   ocrsphi -> ocrsphi_solubility (COSMO-SAC-Phi EoS, database name; no hard domain, no blend rule)
  #   csp     -> csp_solubility     (CSP-2021 COSMO-SAC-Phi, database name; no hard domain, no blend rule)
  if (engine == "makarov") {
    if (is.null(Tm) || !is.finite(Tm)) Tm <- suppressWarnings(gcm_mp(solute)[1])
    sol_fn <- function(cn, cv, xf, rows = seq_len(nrow(grid))) predict_solubility(solute, temp = grid$T[rows], pres = grid$P[rows],
                                                     cosolvent = cv, x = xf, Tm = Tm)$mole_frac
    T_LO <- 308; T_HI <- 373; P_LO <- 80; P_HI <- 360     # Makarov scCO2 training range
    dom <- outer(pres_bar >= P_LO & pres_bar <= P_HI, tK >= T_LO & tK <= T_HI, `&`)
    dimnames(dom) <- list(as.character(pres_bar), as.character(temp_C))
    if (!silent && any(!dom))
      warning(sprintf("%d of %d grid points are outside the solubility model's reliable domain (%g-%g K, %g-%g bar); treat those as extrapolation.",
                      sum(!dom), length(dom), T_LO, T_HI, P_LO, P_HI), call. = FALSE)
  } else if (engine == "ocrsphi") {
    sol_fn <- function(cn, cv, xf, rows = seq_len(nrow(grid))) ocrsphi_solubility(solute, T = grid$T[rows], P = grid$P[rows],
                                                     cosolvent = if (!is.null(cv)) cv else cn, x = xf, Tm = Tm)
    dom <- NULL                                           # filled from the pure baseline below (no hard domain)
  } else {                                                # csp (CSP-2021); resolves solute + co-solvent by NAME
    sol_fn <- function(cn, cv, xf, rows = seq_len(nrow(grid))) as.numeric(csp_solubility(solute, T = grid$T[rows], P = grid$P[rows], model = "2021",
                                                cosolvent = cn, x = xf, fusion_fallback = TRUE))
    dom <- NULL
  }
  mw <- .mw_from_smiles(solute)                          # NA for a database name (ocrsphi/csp); g/kg only
  ng <- nrow(grid)

  # Evaluation jobs = the pure-CO2 baseline + each co-solvent, each optionally split into pressure/temperature
  # GRID CHUNKS. Flattening (item x chunk) into a single worker pool keeps every core busy even when only a
  # few co-solvents are screened (grid-level parallelism); with many co-solvents each is one chunk (co-solvent-
  # level parallelism, as before). Chunks are only created when there are fewer items than workers. Because the
  # engines evaluate each (T,P) point independently, chunked results are numerically identical to a single call.
  nms <- if (!is.null(cosolvents) && length(cosolvents)) { z <- names(cosolvents); if (is.null(z)) unname(cosolvents) else z } else character(0)
  items <- c(list(list(cn = NULL, cv = NULL, xf = 0, k = NULL)),                      # [[1]] = pure baseline
             lapply(seq_along(nms), function(k) list(cn = nms[k], cv = cosolvents[[k]], xf = x, k = k)))
  n_items <- length(items)
  nw <- suppressWarnings(as.integer(workers)[1]); if (is.na(nw) || nw < 1L) nw <- 1L
  nchunks <- if (nw > 1L && n_items < nw && ng >= 2L) max(1L, min(nw %/% n_items, ng)) else 1L
  chunks  <- if (nchunks > 1L) unname(split(seq_len(ng), cut(seq_len(ng), nchunks, labels = FALSE))) else list(seq_len(ng))
  jobs <- list(); for (ii in seq_along(items)) for (ci in seq_along(chunks))
    jobs[[length(jobs) + 1L]] <- list(ii = ii, ci = ci, rows = chunks[[ci]])
  if (length(nms)) .mark(paste0("ncos_", length(nms)))
  if (nchunks > 1L) .mark(paste0("njobs_", length(jobs)))
  # csp_solubility()/others may stop() on an unknown compound; keep the whole scan graceful (-> NA).
  eval_job <- function(j) { it <- items[[j$ii]]
    y <- tryCatch(sol_fn(it$cn, it$cv, it$xf, j$rows), error = function(e) rep(NA_real_, length(j$rows)))
    if (nchunks > 1L) .mark(paste0("job_", j$ii, "_", j$ci)) else if (!is.null(it$k)) .mark(paste0("cs_", it$k))
    list(ii = j$ii, rows = j$rows, y = y) }
  res <- .solub_lapply(jobs, eval_job, workers = nw)
  yfull <- vector("list", n_items)
  for (r in res) { if (is.null(r)) next; if (is.null(yfull[[r$ii]])) yfull[[r$ii]] <- rep(NA_real_, ng); yfull[[r$ii]][r$rows] <- r$y }

  pure_y <- to_mat(if (is.null(yfull[[1L]])) rep(NA_real_, ng) else yfull[[1L]])
  if (is.null(dom)) { dom <- to_mat(is.finite(as.numeric(pure_y))); storage.mode(dom) <- "logical" }
  .mark("stage_baseline")

  sol_list <- enh_list <- list()
  for (k in seq_along(nms)) {
    yv <- yfull[[k + 1L]]; if (is.null(yv) || all(!is.finite(yv))) next
    ym <- to_mat(yv); sol_list[[nms[k]]] <- ym; enh_list[[nms[k]]] <- ym / pure_y
  }

  # Co-solvent BLENDS. Makarov: mole-fraction feature superposition (approximate). The COSMO-SAC-Phi EoS
  # engines (csp / ocrsphi) are GENUINE n-component models, so a blend is evaluated directly as the solute
  # in CO2 + the two co-solvents (bl$names, bl$x) — physically grounded, not an extrapolation. Blend names
  # are returned in `blend_names` so the caller can list them in a dedicated tab (they're kept out of the
  # single-co-solvent grid/ranking).
  blend_names <- character(0)
  if ((!is.null(blends) && length(blends)) || (blend_top_n >= 2 && engine != "makarov")) .mark("stage_blend")
  if (!is.null(blends) && length(blends)) {
    bnms <- names(blends); if (is.null(bnms)) bnms <- paste0("Blend ", seq_along(blends))
    for (k in seq_along(blends)) {
      bl <- blends[[k]]
      ym <- if (engine == "makarov") {
        r <- tryCatch(predict_solubility(solute, temp = grid$T, pres = grid$P, cosolvent = bl$smiles,
                                         x = bl$x, Tm = Tm), error = function(e) NULL)
        if (is.null(r)) NULL else to_mat(r$mole_frac)
      } else {
        tryCatch(to_mat(sol_fn(bl$names, NULL, bl$x)), error = function(e) NULL)   # EoS: true n-component
      }
      if (is.null(ym)) next
      sol_list[[bnms[k]]] <- ym; enh_list[[bnms[k]]] <- ym / pure_y
      blend_names <- c(blend_names, bnms[k])
    }
  }

  # Best (highest-enhancement) SINGLE co-solvent at each grid point. Blends are reported separately in the
  # Recommended-Enhancing-Blends tab and are EXCLUDED here, so they don't crowd every grid cell (a 2-solvent
  # blend at the same total mole fraction almost always out-scores a single in the ML superposition).
  # Robust to all-NA points (e.g. openCOSMO-RS-Phi solids without fusion data).
  .mark("stage_rank")
  single_names <- if (!is.null(cosolvents) && length(cosolvents)) {
    nn <- names(cosolvents); if (is.null(nn)) unname(cosolvents) else nn } else character(0)
  enh_singles <- enh_list[intersect(names(enh_list), single_names)]
  best <- matrix(NA_character_, np, nt, dimnames = list(as.character(pres_bar), as.character(temp_C)))
  if (length(enh_singles)) {
    arr <- array(unlist(enh_singles), c(np, nt, length(enh_singles)))
    for (i in seq_len(np)) for (j in seq_len(nt)) {
      v <- arr[i, j, ]
      if (any(is.finite(v))) { wi <- which.max(v); best[i, j] <- if (v[wi] > 1) names(enh_singles)[wi] else "CO2" }
    }
  }

  # Auto blend screening: rank the single co-solvents, then evaluate 50:50 binary blends of the TOP-N with
  # the (genuine n-component) EoS. Capping at the top-N keeps the pair count tractable (~C(N,2)) rather than
  # C(all,2) — the same "best components make the best blend" heuristic HSP uses. Only for the EoS engines.
  if (blend_top_n >= 2 && engine != "makarov" && length(enh_singles) >= 2) {
    dm <- if (is.logical(dom)) dom else matrix(TRUE, np, nt)
    smean <- vapply(names(enh_singles), function(nm) { v <- enh_singles[[nm]][dm]; v <- v[is.finite(v)]
      if (length(v)) mean(v) else -Inf }, numeric(1))
    topn <- names(sort(smean, decreasing = TRUE))[seq_len(min(blend_top_n, sum(is.finite(smean))))]
    xh <- as.numeric(x)[1] / 2                              # split total co-solvent fraction 50:50
    for (p in utils::combn(topn, 2, simplify = FALSE)) {
      # Skip physically impossible pairs: immiscible (phase-separating) or reacting auto-blends would return
      # a misleading number. Partially-miscible pairs are kept (flagged in the UI). Unknown/uncurated pairs
      # are treated as miscible, so nothing valid is dropped.
      if (!blend_miscibility(p)$usable) next
      ym <- tryCatch(to_mat(sol_fn(p, NULL, c(xh, xh))), error = function(e) NULL)
      if (is.null(ym)) next
      bn <- paste(p, collapse = " + ")
      sol_list[[bn]] <- ym; enh_list[[bn]] <- ym / pure_y; blend_names <- c(blend_names, bn)
    }
  }

  .mark("stage_done")
  list(pure = pure_y, solubility = sol_list, enhancement = enh_list,
       best = best, domain = dom, Tm = as.numeric(Tm), mw = mw, blend_names = blend_names,
       cosolvent_x = x, temp_C = temp_C, pres_bar = pres_bar)
}

# lapply over an index set, optionally spread across a short-lived PSOCK cluster. Each worker loads the
# package (own process => independent engine C++ state) and runs FUN. Any cluster failure, a single
# worker, or <2 items falls back to a plain sequential lapply, so results are always produced.
.solub_lapply <- function(X, FUN, workers = 1L) {
  workers <- suppressWarnings(as.integer(workers)[1])
  if (is.na(workers) || workers <= 1L || length(X) < 2L) return(lapply(X, FUN))
  nw <- min(workers, length(X))
  cl <- tryCatch(parallel::makeCluster(nw), error = function(e) NULL)
  if (is.null(cl)) return(lapply(X, FUN))
  on.exit(tryCatch(parallel::stopCluster(cl), error = function(e) NULL), add = TRUE)
  ok <- tryCatch({ parallel::clusterEvalQ(cl, suppressMessages(requireNamespace("supeRcrit", quietly = TRUE))); TRUE },
                 error = function(e) FALSE)
  if (!ok) return(lapply(X, FUN))
  tryCatch(parallel::parLapplyLB(cl, X, FUN), error = function(e) lapply(X, FUN))
}

# Average molecular weight (g/mol) of a SMILES, for unit conversions. NA when the SMILES does not parse.
.mw_from_smiles <- function(smi) {
  m <- tryCatch(suppressWarnings(rcdk::parse.smiles(smi)[[1]]), error = function(e) NULL)
  if (is.null(m)) return(NA_real_)
  tryCatch(rcdk::get.natural.mass(m),
           error = function(e) tryCatch(rcdk::get.exact.mass(m), error = function(e2) NA_real_))
}

#' Equilibrium-stage separator cascade from a precomputed solubility matrix
#'
#' The solubility-model-agnostic core of the separator cascade: given each solute's solubility (mol/mol)
#' at every separator stage, it runs the same ideal equilibrium-stage mass balance as
#' \code{\link{chrastil_cascade}} (at each stage whatever exceeds the new saturation limit precipitates),
#' but the solubility can come from \emph{any} model — Chrastil, COSMO-SAC-Phi, the machine-learned model
#' (\code{\link{predict_solubility}}), or experimental data. \code{chrastil_cascade} is the Chrastil-specific
#' convenience wrapper around the same balance.
#'
#' @param solutes data.frame with at least \code{name} and \code{mw} (g/mol); optional \code{feed} (kg/h).
#' @param stages data.frame with \code{temp_C} and \code{pres_bar}, ordered as the CO2 stream passes through.
#' @param ysolub solute x stage numeric matrix of solubilities in mole fraction (rows = solutes in the order
#'   of \code{solutes}, columns = stages).
#' @param y_extract optional per-solute mole-fraction solubility at the extraction conditions, used to set
#'   the feed for any solute whose \code{feed} is missing.
#' @param co2_flow CO2 mass flow in kg/h.
#' @param codeposit optional co-deposition (entrainment) coupling: a list with \code{Dref} (per-solute
#'   reference partition \eqn{D_0=\gamma_{CO_2}/\gamma_{oil}} between the CO2 phase and the bulk deposit,
#'   \code{NA} to disable a solute) and \code{yref} (per-solute mole-fraction solubility at the reference
#'   condition where \eqn{D_0} was defined, i.e. the extraction condition). When supplied, a dilute solute
#'   that is nowhere super-saturated is still partly trapped in the matrix (oil) deposit that forms at each
#'   stage: the density-scaled partition is \eqn{D=D_0\,y_{ref}/y_{stage}} (any solubility model supplies
#'   the density trend through \code{ysolub}), and the co-deposited fraction of the still-dissolved solute
#'   is \eqn{f=Dr/(1+Dr)} with \eqn{r} the bulk-deposit-to-CO2 mass ratio at that stage. See the
#'   co-deposition note in the Separator Cascade module.
#' @return the same list structure as \code{\link{chrastil_cascade}} (\code{deposit}, \code{recovery},
#'   \code{feed}, \code{residual}, \code{stage_purity}, \code{tidy}); \code{tidy} additionally carries a
#'   \code{codeposit_kg_h} column (the co-deposited part of each stage's deposit, zero without
#'   \code{codeposit}).
#' @seealso \code{\link{chrastil_cascade}}, \code{\link{solub_optim}}, \code{\link{predict_solubility}}.
#' @noRd
solub_cascade <- function(solutes, stages, ysolub, y_extract = NULL, co2_flow = 1, codeposit = NULL, cosolv_x = 0) {
  solutes <- as.data.frame(solutes, stringsAsFactors = FALSE)
  stages  <- as.data.frame(stages,  stringsAsFactors = FALSE)
  if (!all(c("name", "mw") %in% names(solutes))) stop("`solutes` needs 'name' and 'mw'.", call. = FALSE)
  if (!all(c("temp_C", "pres_bar") %in% names(stages))) stop("`stages` needs 'temp_C' and 'pres_bar'.", call. = FALSE)
  nC <- nrow(solutes); nS <- nrow(stages)
  ysolub <- matrix(as.numeric(ysolub), nC, nS)
  if (is.null(solutes$feed)) solutes$feed <- NA_real_
  n_co2 <- co2_flow * 1000 / 44.01                                   # mol/h CO2
  # Total carrier-fluid moles. With a co-solvent at total mole fraction `cosolv_x`, the solubility mole
  # fractions (ysolub / y_extract) are per mole of the WHOLE CO2 + co-solvent fluid, so the carrying capacity
  # and the saturation feed must scale with the total fluid moles, not CO2 alone (cosolv_x = 0 -> n_fluid = n_co2).
  cosolv_x <- suppressWarnings(as.numeric(cosolv_x)[1]); if (!isTRUE(is.finite(cosolv_x))) cosolv_x <- 0
  cosolv_x <- max(0, min(0.95, cosolv_x))
  n_fluid <- n_co2 / (1 - cosolv_x)                                  # mol/h total carrier fluid

  # Per-solute feed mass (kg/h): explicit, or from the extract-condition solubility.
  feed <- numeric(nC)
  for (j in seq_len(nC)) {
    m0 <- solutes$feed[j]
    if (!is.finite(m0)) {
      ye <- if (!is.null(y_extract)) y_extract[j] else NA_real_
      if (!isTRUE(is.finite(ye))) stop("Solute '", solutes$name[j], "' has no feed and no extract solubility.", call. = FALSE)
      m0 <- unname(ye * n_fluid * solutes$mw[j] / 1000)
    }
    feed[j] <- m0
  }
  mmax <- ysolub * n_fluid * matrix(solutes$mw, nC, nS) / 1000       # kg/h carryable per solute per stage

  # ---- Optional co-deposition (entrainment) coupling ------------------------------------------------
  co_on <- !is.null(codeposit) && !is.null(codeposit$Dref) && any(is.finite(as.numeric(codeposit$Dref)))
  if (co_on) {
    Dref <- rep_len(as.numeric(codeposit$Dref), nC)
    yref <- rep_len(as.numeric(if (is.null(codeposit$yref)) NA_real_ else codeposit$yref), nC)
  }

  deposit   <- matrix(0, nC, nS, dimnames = list(solutes$name, NULL))
  codep_dep <- matrix(0, nC, nS)
  carried <- feed
  # Per-stage balance (solutes precipitate independently by saturation; then dilute solutes co-deposit
  # into the bulk matrix deposit formed at that stage). Reduces exactly to the old per-solute saturation
  # cascade when co-deposition is off.
  for (i in seq_len(nS)) {
    sat <- pmax(0, carried - mmax[, i])                             # saturation precipitation
    deposit[, i] <- sat
    carried <- carried - sat
    if (co_on) {
      m_bulk <- sum(sat, na.rm = TRUE)                              # kg/h matrix deposit formed at this stage
      if (isTRUE(is.finite(m_bulk)) && m_bulk > 0 && co2_flow > 0) {
        r <- m_bulk / co2_flow                                      # deposit-to-CO2 mass ratio
        D <- Dref * yref / ysolub[, i]                              # density-scaled partition D0 * yref/y(rho)
        f <- D * r / (1 + D * r)                                    # co-deposited fraction of still-dissolved solute
        f[!is.finite(f) | f < 0] <- 0
        cod <- f * carried
        codep_dep[, i] <- cod
        deposit[, i]   <- deposit[, i] + cod
        carried        <- carried - cod
      }
    }
  }
  residual <- carried

  recovery <- deposit / ifelse(feed > 0, feed, NA_real_)
  stage_tot <- colSums(deposit)
  stage_purity <- t(deposit) / ifelse(stage_tot > 0, stage_tot, NA_real_)
  dimnames(stage_purity) <- list(paste0("S", seq_len(nS)), solutes$name)
  tidy <- data.frame(solute = rep(solutes$name, times = nS), stage = rep(seq_len(nS), each = nC),
    temp_C = rep(stages$temp_C, each = nC), pres_bar = rep(stages$pres_bar, each = nC),
    deposit_kg_h = as.vector(deposit), codeposit_kg_h = as.vector(codep_dep),
    recovery = as.vector(recovery), stringsAsFactors = FALSE)
  list(deposit = deposit, recovery = recovery, feed = setNames(feed, solutes$name),
       residual = setNames(residual, solutes$name), stage_purity = stage_purity, tidy = tidy)
}

#' Convert scCO2 solubility from mole fraction to grams of solute per kilogram of CO2
#'
#' @param y solute mole fraction in the CO2-rich phase (numeric vector).
#' @param mw solute molecular weight (g/mol).
#' @return solubility in g solute per kg CO2: \eqn{y/(1-y) \cdot M_w/M_{CO_2} \cdot 1000}
#'   (\eqn{M_{CO_2} = 44.01}).
#' @examples
#' solub_gkg(1.7e-4, 151.2)   # paracetamol ~ 0.58 g/kg
#' @seealso \code{\link{solub_optim}}, \code{\link{predict_solubility}}.
#' @export
solub_gkg <- function(y, mw) {
  y <- as.numeric(y)
  (y / (1 - y)) * (as.numeric(mw) / 44.01) * 1000
}
