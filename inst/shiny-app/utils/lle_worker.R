# lle_worker.R
# ======================================================================================================
# Self-contained flash / blend-optimize computation for the BACKGROUND (callr) worker, so Two-Phase Flash
# (COSMO-SAC-dsp / openCOSMO-RS-Phi) and Optimize Blend become cancellable (kill button). Runs in a FRESH
# R process with ONLY the installed supeRcrit package — no shiny/session/module scope. It takes already-
# resolved engine ids + parameters and returns RAW results keyed by those ids; the server's finalize maps
# display names (disp_name/SOLV) and builds rv + the UI. (CS25 stays synchronous in the module — its jshell
# gateway does not move cleanly into a worker — so it is not routed here.)
#
#   args(a): list(mode, engine, db, solutes, feed, solvents, Tgrid(°C), sf, sf_grid, sf_vec, pressure,
#                 objective, multistart, warm, opt_method, dsp)
#   returns: list(status = "ok"|"noresult"|"empty"|"error", ...)
# ======================================================================================================

lle_compute <- function(a) {
  tryCatch({
    if (identical(a$mode, "screen")) return(.lle_worker_screen(a))
    if (identical(a$mode, "optimize")) return(.lle_worker_optimize(a))
    if (identical(a$engine, "ocrsphi")) return(.lle_worker_flash_ocrsphi(a))
    if (identical(a$engine, "cs25")) return(.lle_worker_flash_cs25(a))
    .lle_worker_flash_csac(a)
  }, error = function(e) list(status = "error", message = conditionMessage(e)))
}

# ---- Quick-screen (COSMO-SAC-dsp gamma-infinity ranking + per-solvent feed-miscibility check). Returns the
# raw csac_lle data.frame keyed by engine ids; the server's .finalize_screen maps display names + Notes.
.lle_worker_screen <- function(a) {
  res <- supeRcrit::csac_lle(a$solute, solvents = a$solvents, feed = a$feed, T = a$Tk,
    coextract = a$coex, dsp = isTRUE(a$dsp), database = a$db)
  if (is.null(res) || !nrow(res)) return(list(status = "noresult"))
  res$feed_miscible <- vapply(res$solvent, function(s)
    isTRUE(tryCatch(as.numeric(supeRcrit::csac_gamma(s, a$feed, T = a$Tk, dsp = isTRUE(a$dsp), database = a$db))[1] < 10,
      error = function(e) FALSE)), logical(1))
  list(status = "ok", res = res)
}

# ---- CS25 two-phase flash via the JCOSMO jshell gateway (opened INSIDE this worker so Kill can cancel it;
# cleanup_tree on the worker kills the jshell child). Names/aliases/prescreen are resolved on the main side.
.lle_worker_flash_cs25 <- function(a) {
  source(a$cs25_bridge_path, local = FALSE)
  if (nzchar(a$jcosmo_dir %||% "")) options(supercrit.jcosmo_dir = a$jcosmo_dir)
  if (!isTRUE(cs25_available())) return(list(status = "cs25_unavailable"))
  Tgrid <- a$Tgrid; sf <- a$sf; sr <- a$solvent_ratio
  sess <- cs25_open(); on.exit(try(cs25_close(sess), silent = TRUE), add = TRUE)
  res <- cs25_lle_extract(a$solutes_cs, solvents = a$solvents, feed = a$feed_cs, T = Tgrid + 273.15,
    solute_frac = sf, solvent_ratio = sr, sess = sess)
  if (is.null(res) || !nrow(res)) return(list(status = "noresult"))
  if (!any(res$split %in% TRUE) && all(is.na(res$beta))) return(list(status = "cs25_noresolve"))
  dcol <- grep("^D_", names(res)); names(res)[dcol] <- paste0("D", seq_along(dcol))
  res$solvent_disp <- res$solvent; res$sf_ratio <- sr
  best <- NULL; ti <- which(res$split %in% TRUE)[1]
  if (!is.na(ti)) {
    comps <- c(a$feed_cs, res$solvent[ti], a$solutes_cs); z <- c(1, sr, rep(sf, length(a$solutes_cs)))
    fl <- tryCatch(cs25_lle_flash(comps, z = z, T = res$T_K[ti], sess = sess), error = function(e) NULL)
    if (!is.null(fl) && isTRUE(fl$split)) { ph <- fl$phase; ph$disp <- ph$component
      best <- list(phase = ph, solvent_id = res$solvent[ti], bi = ti, Tc = res$T_K[ti] - 273.15, beta = fl$beta) }
  }
  list(status = "ok", engine = "cs25", res = res, best = best, any_split = any(res$split %in% TRUE))
}

# ---- COSMO-SAC-dsp two-phase flash: prescreen -> top-K, flash solvent×T at one ratio, optimise ratio at best.
.lle_worker_flash_csac <- function(a) {
  db <- a$db; dsp <- isTRUE(a$dsp); Tgrid <- a$Tgrid; sf <- a$sf
  solutes <- a$solutes; feed <- a$feed; solvents <- a$solvents
  n_in <- length(solvents); pruned <- FALSE
  K_flash <- 8L
  if (length(solvents) > K_flash) {
    pre <- tryCatch(supeRcrit::csac_lle(solutes[1], solvents = solvents, feed = feed,
      T = mean(Tgrid) + 273.15, dsp = dsp, database = db), error = function(e) NULL)
    if (!is.null(pre) && nrow(pre)) { solvents <- solvents[solvents %in% utils::head(pre$solvent[order(-pre$D)], K_flash)]; pruned <- TRUE }
  }
  sfgrid <- a$sf_grid; sf_mid <- stats::median(sfgrid)
  res <- supeRcrit::csac_lle_extract(solutes, solvents = solvents, feed = feed, T = Tgrid + 273.15,
    solute_frac = sf, solvent_ratio = sf_mid, dsp = dsp, database = db)
  if (is.null(res) || !nrow(res)) return(list(status = "noresult"))
  res$sf_ratio <- sf_mid
  dcol <- grep("^D_", names(res)); names(res)[dcol] <- paste0("D", seq_along(dcol))
  best <- NULL; spl <- which(res$split %in% TRUE)
  if (length(spl)) {
    objv <- if (length(solutes) >= 2) res$selectivity else res$D1
    bi <- spl[which.max(ifelse(is.finite(objv[spl]), objv[spl], -Inf))]
    bsolv <- res$solvent[bi]; bT <- res$T_K[bi]; sfbest <- sf_mid; obest <- -Inf
    for (rr in sfgrid) {
      rrr <- tryCatch(supeRcrit::csac_lle_extract(solutes, solvents = bsolv, feed = feed, T = bT,
        solute_frac = sf, solvent_ratio = rr, dsp = dsp, database = db), error = function(e) NULL)
      if (is.null(rrr) || !nrow(rrr) || !isTRUE(rrr$split[1])) next
      ov <- if (length(solutes) >= 2) rrr$selectivity[1] else rrr$D1[1]
      if (isTRUE(ov > obest)) { obest <- ov; sfbest <- rr }
    }
    comps <- c(feed, bsolv, solutes); z <- c(1, sfbest, a$sf_vec); z <- z / sum(z)
    fl <- tryCatch(supeRcrit::csac_lle_flash(comps, z = z, T = bT, dsp = dsp, database = db), error = function(e) NULL)
    if (!is.null(fl) && isTRUE(fl$split))
      best <- list(phase = fl$phase, solvent_id = bsolv, bi = bi, Tc = bT - 273.15, beta = fl$beta, sf_ratio = sfbest)
  }
  list(status = "ok", engine = "csac", res = res, best = best,
       n_in = n_in, n_used = length(solvents), pruned = pruned, any_split = any(res$split %in% TRUE))
}

# ---- openCOSMO-RS-Phi two-phase flash (EoS; whole-set fallback when no picks).
.lle_worker_flash_ocrsphi <- function(a) {
  Tgrid <- a$Tgrid; sf <- a$sf; sr <- a$solvent_ratio; feed <- a$feed; solutes <- a$solutes
  P_bar <- a$pressure; if (is.null(P_bar) || is.na(P_bar) || P_bar <= 0) P_bar <- 1.01325
  pool <- a$solvents   # resolved ocrsphi ids, or character(0) for the whole set
  if (!length(pool)) pool <- setdiff(supeRcrit::ocrsphi_lle_compounds(), c(feed, solutes))
  else pool <- setdiff(pool, c(feed, solutes))
  res <- supeRcrit::ocrsphi_lle_extract(solutes, solvents = pool, feed = feed, T = Tgrid + 273.15,
    P = P_bar * 1e5, solute_frac = sf, solvent_ratio = sr)
  if (is.null(res) || !nrow(res)) return(list(status = "noresult"))
  dcol <- grep("^D_", names(res)); names(res)[dcol] <- paste0("D", seq_along(dcol))
  res$solvent_in_raffinate <- NA_real_; res$feed_in_extract <- NA_real_; res$sf_ratio <- sr
  best <- NULL; ti <- which(res$split %in% TRUE)[1]
  if (!is.na(ti)) {
    comps <- c(feed, res$solvent[ti], solutes); z <- c(1, sr, rep(sf, length(solutes)))
    fl <- tryCatch(supeRcrit::ocrsphi_lle_flash(comps, z = z, T = res$T_K[ti], P = P_bar * 1e5), error = function(e) NULL)
    if (!is.null(fl) && isTRUE(fl$split))
      best <- list(phase = fl$phase, solvent_id = res$solvent[ti], bi = ti, Tc = res$T_K[ti] - 273.15, beta = fl$beta)
  }
  list(status = "ok", engine = "ocrsphi", res = res, best = best, any_split = any(res$split %in% TRUE))
}

# ---- binary-blend optimizer: prescreen -> top-K, optimise each PAIR, rank. Returns raw per-pair results.
.lle_worker_optimize <- function(a) {
  db <- a$db; dsp <- isTRUE(a$dsp); Tgrid <- a$Tgrid; sf <- a$sf; sr <- a$solvent_ratio
  solutes <- a$solutes; feed <- a$feed; pool <- a$solvents; obj <- a$objective %||% "selectivity"
  ms <- a$multistart; if (is.null(ms) || is.na(ms) || ms < 1) ms <- 4
  K <- 5L
  if (length(pool) > K) {
    pre <- tryCatch(supeRcrit::csac_lle(solutes[1], solvents = pool, feed = feed, T = mean(Tgrid) + 273.15,
      dsp = dsp, database = db), error = function(e) NULL)
    if (!is.null(pre) && nrow(pre)) pool <- pool[pool %in% utils::head(pre$solvent[order(-pre$D)], K)]
  }
  if (length(pool) < 2) return(list(status = "toofew"))
  pairs <- utils::combn(pool, 2, simplify = FALSE)
  res_list <- lapply(pairs, function(pr) {
    r <- tryCatch(supeRcrit::csac_lle_optimize_blend(solutes, solvents = pr, feed = feed, T = Tgrid + 273.15,
      objective = obj, solute_frac = sf, solvent_ratio = sr, dsp = dsp, database = db,
      multistart = as.integer(min(ms, 2L)), warm = isTRUE(a$warm), method = a$opt_method %||% "gamma_inf"),
      error = function(e) NULL)
    if (is.null(r) || is.null(r$composition)) return(NULL)
    list(composition = r$composition, value = r$value, T_K = r$T_K, D = r$D, solvents = pr)
  })
  res_list <- Filter(Negate(is.null), res_list)
  if (!length(res_list)) return(list(status = "empty"))
  res_list <- res_list[order(-vapply(res_list, function(r) r$value %||% -Inf, numeric(1)))]
  list(status = "ok", res_list = res_list)
}

if (!exists("%||%")) `%||%` <- function(x, y) if (is.null(x)) y else x
