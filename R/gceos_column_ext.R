#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# COUNTER-CURRENT COLUMN: ADVANCED SOLVERS AND PROCESS TOOLS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Extensions over the base equilibrium-stage solver (gceos_column / _ns_solve):
#   * gceos_column_continuation : homotopy in S/F (or reflux) for stiff targets
#   * gceos_column_vlle         : three-phase (V-L-L) aware stage cascade
#   * gceos_column_energy       : non-isothermal column (energy-balance T profile)
#   * gceos_column_optimize     : tune operating variables to a purity/recovery goal
# All are R-level wrappers around the compiled NS core and gceos_flash3_solve;
# the core is unchanged.

#' @title Reach a hard column target by S/F (or reflux) continuation
#'
#' @description
#' The simultaneous-correction column solver (\code{\link{gceos_column_ns_solve}})
#' can fail to converge when started cold at a demanding operating point - very high
#' solvent-to-feed ratio, high reflux, or near the mixture critical. This wrapper
#' walks to the target along a homotopy path: it solves an easy point first, then
#' steps the continuation variable toward its target, \strong{warm-starting} each
#' step from the previous converged profile and \strong{bisecting} the step if a
#' point fails. This reaches operating points a cold solve cannot.
#'
#' @param N,T,P,comps,FL,zL,zV,feedL,feedV,solvent,model Column definition, as in
#'   \code{\link{gceos_column_ns_solve}}.
#' @param target Target value of the continuation variable.
#' @param reflux Reflux ratio (the target reflux if \code{var = "reflux"}, otherwise
#'   the fixed reflux).
#' @param var Continuation variable: \code{"SF"} (solvent-to-feed, i.e. \code{FV},
#'   default) or \code{"reflux"}.
#' @param from Starting value of the continuation variable (defaults: \code{SF} ->
#'   \code{20}, \code{reflux} -> \code{0}).
#' @param n_steps Number of ramp steps (geometric for \code{SF}, linear for
#'   \code{reflux}).
#' @param min_step Smallest fractional step before giving up the bisection.
#' @param max_iter,tol Passed to the inner solver.
#' @param verbose If \code{TRUE}, report each step.
#' @return The converged \code{\link{gceos_column_ns_solve}} result at \code{target}
#'   (with an added \code{path} data.frame of the steps taken), or the last
#'   converged result with a warning if the target was not reached.
#' @noRd
#' @seealso \code{\link{gceos_column_ns_solve}}
gceos_column_continuation <- function(N, T, P, comps, FL, zL, zV,
                                      target, reflux = 0, var = c("SF", "reflux"),
                                      from = NULL, feedL = 1, feedV = N,
                                      solvent = "CO2", model = "gceos",
                                      n_steps = 12L, min_step = 1e-3,
                                      max_iter = 600L, tol = 1e-8, verbose = FALSE,
                                      init = NULL) {
  var <- match.arg(var)
  if (is.null(from)) from <- if (var == "SF") min(20, target) else 0
  solve_at <- function(val, init) {
    FV <- if (var == "SF") val else target            # when ramping reflux, FV is fixed at target? no:
    sf <- if (var == "SF") val else target            # SF held at 'target' while reflux ramps
    rf <- if (var == "reflux") val else reflux
    tryCatch(gceos_column_ns_solve(N, T, P, comps, FL = FL, zL = zL,
               FV = if (var == "SF") val else target, zV = zV,
               feedL = feedL, feedV = feedV, reflux = rf, solvent = solvent,
               model = model, max_iter = max_iter, tol = tol, init = init),
             error = function(e) NULL)
  }
  # build the ramp schedule
  grid <- if (var == "SF") exp(seq(log(from), log(target), length.out = n_steps + 1L))
          else seq(from, target, length.out = n_steps + 1L)
  prev <- NULL; last_ok <- NULL; path <- list()
  i <- 1L; cur <- grid[1]
  # first point (optionally warm-started from a supplied profile)
  r0 <- solve_at(cur, init)
  if (is.null(r0) || !r0$converged) {
    # retry from an even easier sub-start by prepending finer steps
    sub <- if (var == "SF") exp(seq(log(max(from/8, 1)), log(from), length.out = 5L))
           else seq(0, from, length.out = 5L)
    for (s in sub) { rr <- solve_at(s, if (exists("r0") && !is.null(r0) && isTRUE(r0$converged)) r0 else init)
      if (!is.null(rr) && isTRUE(rr$converged)) { r0 <- rr; cur <- s } }
    if (is.null(r0) || !r0$converged)
      stop("Continuation could not converge the starting point (", var, " = ", round(cur, 3), ").", call. = FALSE)
  }
  prev <- last_ok <- r0; path[[1]] <- c(step = cur, iters = r0$iters, resid = r0$resid)
  if (verbose) message(sprintf("  %s=%.4g  ok (%d it)", var, cur, r0$iters))

  for (i in 2:(n_steps + 1L)) {
    tgt <- grid[i]; lo <- cur; hi <- tgt
    # try the full step; on failure, bisect toward the last converged value
    repeat {
      r <- solve_at(hi, prev)
      if (!is.null(r) && r$converged) {
        prev <- last_ok <- r; cur <- hi
        path[[length(path) + 1L]] <- c(step = hi, iters = r$iters, resid = r$resid)
        if (verbose) message(sprintf("  %s=%.4g  ok (%d it)", var, hi, r$iters))
        break
      }
      # failed: halve the step from the last good point
      frac <- abs(hi - lo) / max(abs(tgt), 1e-12)
      if (frac < min_step) {
        warning("Continuation stalled before the target: reached ", var, " = ",
                round(cur, 4), " of ", round(target, 4), ".", call. = FALSE)
        last_ok$path <- do.call(rbind, path); return(last_ok)
      }
      hi <- 0.5 * (lo + hi)
      if (verbose) message(sprintf("  %s=%.4g  fail -> bisect", var, tgt))
    }
    if (isTRUE(all.equal(cur, target))) break
  }
  last_ok$path <- do.call(rbind, path)
  last_ok
}

# Approximate ideal-gas heat capacities (J/mol/K, ~350 K) by GC-EOS group, used by
# the energy-balance column when the caller supplies no Cp. Crude group-additive
# estimates - good enough to predict the (usually small) SFE-column gradient, but
# pass 'cp_ig' for quantitative duty work.
.gceos_cpig_groups <- c(
  CH3 = 35, `CH3(B)` = 35, CH2 = 25, `CH2(B)` = 25, CH = 20, C = 15,
  cyCH2 = 25, cyCH = 20, `CH=CH` = 45, `CH=C` = 40, `C=C` = 35,
  `CH2=CH` = 50, `CH2=C` = 45, `C=CH` = 40, `C=CH2` = 45,
  ACH = 18, AC = 12, ACCH3 = 40, ACCH2 = 35,
  COH = 40, OH = 25, CH3OH = 50, C2H5OH = 75, CH2OH = 40, H2O = 34,
  COOH = 55, CH2COO = 70, CH3COO = 80, CH2COOH = 75, CO2 = 38)

.gceos_cpig_comp <- function(comp) {
  g <- comp$groups; r <- .gceos_cpig_groups[names(g)]
  if (anyNA(r)) 4 * 8.314 + 25 * sum(g) else sum(as.numeric(g) * r)   # fallback ~ size-scaled
}

# Molar enthalpy (J/mol) of a phase of composition x at (T, P_atm): EOS residual
# (numerical d(A^R/RT)/dT at fixed V) + ideal-gas sensible heat above Tref.
.gceos_Hmolar <- function(T, P_atm, x, comps, model, want, cp_ig, Tref) {
  Rj <- 8.314; ntot <- sum(x); xn <- x / ntot
  cc <- comps; for (i in seq_along(cc)) cc[[i]]$n <- x[i]
  xin <- .gceos_inputs(T, cc, model)
  V <- gceos_vroot(T, P_atm, xin$n, xin$d, xin$q, xin$nu, xin$g, xin$dg, xin$alpha,
                   GCEOS_R, as.integer(want), xin$numu, xin$Delta0)
  if (!is.finite(V) || V <= 0) return(NA_real_)
  dT <- 1e-3 * T
  ap <- .gceos_inputs(T + dT, cc, model); am <- .gceos_inputs(T - dT, cc, model)
  a1 <- gceos_ar_rt(T+dT, V, ap$n, ap$d, ap$q, ap$nu, ap$g, ap$dg, ap$alpha, GCEOS_R, ap$numu, ap$Delta0) / ntot
  a0 <- gceos_ar_rt(T-dT, V, am$n, am$d, am$q, am$nu, am$g, am$dg, am$alpha, GCEOS_R, am$numu, am$Delta0) / ntot
  dart <- (a1 - a0) / (2 * dT)                        # d(a^R/RT)/dT at fixed V (molar)
  Z <- P_atm * V / (ntot * GCEOS_R * T)
  hR <- (-T * dart + (Z - 1)) * Rj * T                # residual molar enthalpy, J/mol
  hig <- sum(xn * cp_ig) * (T - Tref)                 # ideal-gas sensible, J/mol
  hR + hig
}

#' @title Non-isothermal (energy-balance) counter-current column
#'
#' @description
#' Solves the column \emph{temperature profile} from adiabatic stage energy balances
#' instead of imposing it. An outer loop alternates between the isothermal
#' simultaneous-correction solve (\code{\link{gceos_column_ns_solve}}) at the current
#' profile and an enthalpy-balance temperature update, until both converge. Stage
#' enthalpies use the EOS \strong{residual} enthalpy (computed analytically-by-finite-
#' difference from \eqn{\partial(A^R/RT)/\partial T}, association included) plus an
#' ideal-gas sensible term. This captures the cooling/heating along an SFE column
#' (CO2 solution and expansion effects) that the isothermal model misses.
#'
#' \strong{Accuracy:} the residual (EOS) part is rigorous; the ideal-gas
#' \eqn{C_p} defaults to crude group estimates (\code{\link{gceos_column_ns_solve}}
#' is exact for the equilibrium, this only sets the \emph{sensible-heat} scale of the
#' gradient). Pass \code{cp_ig} (J/mol/K per component) for quantitative work.
#'
#' @param N,P,comps,FL,zL,FV,zV,feedL,feedV,reflux,solvent,model Column definition,
#'   as in \code{\link{gceos_column_ns_solve}}; \code{P} in atm.
#' @param T_init Initial temperature profile (scalar or length-N).
#' @param feed_TL,feed_TV Temperatures (K) of the liquid and vapour feeds (default
#'   \code{T_init} at their feed stages).
#' @param cp_ig Optional ideal-gas heat capacities (J/mol/K), one per component;
#'   if \code{NULL}, estimated from groups (approximate).
#' @param Q Optional per-stage external heat duty (J per mole of feed basis; default 0
#'   = adiabatic).
#' @param max_outer,T_tol,T_damp Outer-loop cap, temperature convergence tolerance
#'   (K) and damping.
#' @return The converged \code{\link{gceos_column_ns_solve}} result, with the solved
#'   \code{T} profile added and \code{T_converged}.
#' @noRd
#' @seealso \code{\link{gceos_column_ns_solve}}
gceos_column_energy <- function(N, P, comps, FL, zL, FV, zV, T_init,
                                feedL = 1, feedV = N, reflux = 0, solvent = "CO2",
                                model = "gceos", feed_TL = NULL, feed_TV = NULL,
                                cp_ig = NULL, Q = rep(0, N), max_outer = 30L,
                                T_tol = 0.1, T_damp = 0.5, max_iter = 400L, tol = 1e-8) {
  nc <- length(comps); nms <- names(comps); zL <- zL/sum(zL); zV <- zV/sum(zV)
  Tprof <- if (length(T_init) == 1L) rep(T_init, N) else T_init
  Tref <- mean(Tprof)
  if (is.null(cp_ig)) cp_ig <- vapply(comps, .gceos_cpig_comp, numeric(1))
  cp_ig <- stats::setNames(as.numeric(cp_ig), nms)
  if (is.null(feed_TL)) feed_TL <- Tprof[feedL]
  if (is.null(feed_TV)) feed_TV <- Tprof[feedV]
  hL <- function(T, x) .gceos_Hmolar(T, P, x, comps, model, 0L, cp_ig, Tref)
  hV <- function(T, y) .gceos_Hmolar(T, P, y, comps, model, 1L, cp_ig, Tref)

  col <- NULL; tconv <- FALSE
  for (outer in seq_len(max_outer)) {
    col <- if (is.null(col))
      tryCatch(gceos_column_continuation(N, Tprof, P, comps, FL = FL, zL = zL, zV = zV,
               target = FV, reflux = reflux, var = "SF", feedL = feedL, feedV = feedV,
               solvent = solvent, model = model, n_steps = 8L, max_iter = max_iter, tol = tol),
             error = function(e) NULL)
      else gceos_column_ns_solve(N, Tprof, P, comps, FL = FL, zL = zL, FV = FV, zV = zV,
             feedL = feedL, feedV = feedV, reflux = reflux, solvent = solvent,
             model = model, max_iter = max_iter, tol = tol, init = col)
    if (is.null(col) || !isTRUE(col$converged)) break
    x <- col$x; y <- col$y; L <- col$L; V <- col$V
    # molar enthalpies of the leaving streams at current T
    HLj <- vapply(seq_len(N), function(j) hL(Tprof[j], x[j, ]), numeric(1))
    HVj <- vapply(seq_len(N), function(j) hV(Tprof[j], y[j, ]), numeric(1))
    CpLj <- vapply(seq_len(N), function(j) sum((x[j,]/sum(x[j,])) * cp_ig), numeric(1))
    CpVj <- vapply(seq_len(N), function(j) sum((y[j,]/sum(y[j,])) * cp_ig), numeric(1))
    hFL <- hL(feed_TL, zL); hFV <- hV(feed_TV, zV)
    refl_frac <- reflux/(1 + reflux); Lrefl <- refl_frac * V[1]   # condensed top-vapour reflux
    dTmax <- 0
    for (j in seq_len(N)) {
      # liquid entering from above (reflux at the top), vapour entering from below
      # (nothing below stage N; the solvent enters there as an external feed)
      Lin  <- if (j == 1) Lrefl else L[j-1]
      HLin <- if (j == 1) HLj[1] else HLj[j-1]                    # reflux ~ top-stage liquid enthalpy
      Vin  <- if (j == N) 0 else V[j+1]
      HVin <- if (j == N) 0 else HVj[j+1]
      ein  <- Lin*HLin + Vin*HVin +
              (if (j == feedL) FL*hFL else 0) + (if (j == feedV) FV*hFV else 0)
      eout <- L[j]*HLj[j] + V[j]*HVj[j]
      imbal <- ein - eout + Q[j]
      Ccap <- L[j]*CpLj[j] + V[j]*CpVj[j]; if (Ccap < 1e-9) next
      dTj <- imbal / Ccap
      Tprof[j] <- Tprof[j] + T_damp * max(-15, min(15, dTj))      # clamp per-step move
      dTmax <- max(dTmax, abs(T_damp * dTj))
    }
    if (is.finite(dTmax) && dTmax < T_tol) { tconv <- TRUE; break }
  }
  if (!is.null(col)) { col$T <- Tprof; col$T_converged <- tconv; col$cp_ig <- cp_ig }
  col
}

# Deterministic space-filling fallback seeds for the feasible-seed search: the box
# midpoint, the two extreme corners, and each axis swept low/high with the others at
# the midpoint. Used to escape an infeasible starting region before the gradient solve.
.opt_seed_candidates <- function(lower, upper) {
  mid <- 0.5 * (lower + upper); n <- length(lower)
  cands <- list(mid, lower, upper)
  # a star pattern through the midpoint: each axis swept to its extremes (incl. the corner
  # points lower/upper above). Probing ALL of these and multi-starting from the best gives
  # the search enough coverage to escape a poor basin (e.g. an optimum at high S/F rather
  # than near the current operating point) without an expensive dense grid.
  for (i in seq_len(n)) {
    lo <- mid; lo[i] <- lower[i]; cands[[length(cands) + 1L]] <- lo
    hi <- mid; hi[i] <- upper[i]; cands[[length(cands) + 1L]] <- hi
  }
  cands
}

# i-th element of the van der Corput / Halton sequence in the given base (deterministic,
# low-discrepancy - no RNG, reproducible).
.halton1 <- function(i, base) { f <- 1; r <- 0
  while (i > 0) { f <- f / base; r <- r + f * (i %% base); i <- i %/% base }
  r }

# A space-filling sample of N points across the [lower, upper] box, as a list of named
# vectors, using a Halton sequence (one prime base per axis). Used by the GLOBAL optimiser
# to scan the whole box - including high-S/F regions a local search from the operating point
# can miss - before polishing the best feasible point with the local gradient method.
.opt_global_sample <- function(lower, upper, vnames, N = 32L) {
  bases <- c(2, 3, 5, 7, 11, 13, 17)[seq_along(lower)]
  lapply(seq_len(N), function(i)
    stats::setNames(lower + vapply(seq_along(lower),
      function(j) .halton1(i, bases[j]), numeric(1)) * (upper - lower), vnames))
}

#' @title Optimize column operating conditions for a purity/recovery objective
#'
#' @description
#' Tunes continuous operating variables - extraction pressure, temperature,
#' solvent-to-feed ratio, reflux - to maximize a user objective computed from the
#' converged column (e.g. a key-component recovery, a product purity, or a weighted
#' trade-off). The column is re-solved with warm-starting at each trial point; trial
#' points that fail to converge are penalised so the optimiser stays in the feasible
#' region. This mirrors the nonlinear-programming approach of Espinosa et al. (2002)
#' / Diaz et al. for supercritical-fractionation design.
#'
#' @param N,comps,FL,zL,zV,feedL,feedV,solvent,model Column definition (the fixed
#'   structural part), as in \code{\link{gceos_column_ns_solve}}.
#' @param objective A function of the column result returning a scalar to
#'   \strong{maximize}. Helpers: the result has \code{$recovery} (per-component
#'   fraction to extract), \code{$extract$comp}, \code{$raffinate$x}.
#' @param vars Named list of \code{c(lower, upper)} bounds for the variables to
#'   optimise. Recognised names: \code{"pres_bar"}, \code{"temp"} (K), \code{"sf"}
#'   (solvent-to-feed = FV), \code{"reflux"}.
#' @param start Named numeric vector of starting values (defaults to the midpoint of
#'   each \code{vars} range).
#' @param fixed Named list of fixed operating values for any of
#'   \code{pres_bar/temp/sf/reflux} not being optimised (required for those not in
#'   \code{vars}).
#' @param max_iter,tol Inner-solver controls (used for the feasible-seed search
#'   and the final solve at the optimum).
#' @param opt_max_iter Inner-solver iteration cap applied ONLY to the cheap
#'   objective probes during the gradient search. Keeping it well below
#'   \code{max_iter} stops L-BFGS-B grinding hundreds of Newton iterations (plus
#'   the S/F-continuation fallback) on infeasible trial points - the dominant
#'   cost. The seed search and the reported optimum still use the full
#'   \code{max_iter}, so accuracy at the answer is unchanged.
#' @param control \code{optim} control list (e.g. \code{list(maxit = 60)}). A
#'   \code{parscale} equal to each variable's range is supplied automatically (so
#'   pressure, S/F and reflux are equally weighted in the finite-difference
#'   gradient) unless the caller overrides it.
#' @param on_eval Optional progress callback invoked once per objective
#'   evaluation with a single \code{list(eval, par, value, feasible, best,
#'   best_par)} argument (eval count, the point, its objective value or
#'   \code{NA}, whether it was feasible, and the best value/point so far). Errors
#'   in the callback are swallowed so they cannot abort the optimisation.
#' @param method Search strategy. \code{"local"} (default) descends from the first
#'   feasible seed (the operating point, then a small fallback set) - fast, warm-started.
#'   \code{"global"} first scans a Halton space-filling sample of the whole box and
#'   polishes the best feasible point - slower but not trapped in a local basin.
#' @param n_global Number of space-filling sample points for \code{method = "global"}
#'   (default 32). Larger = more thorough and slower.
#' @return A \code{list} with \code{par} (optimal variable values), \code{value}
#'   (objective at the optimum), \code{result} (the column at the optimum), and
#'   \code{feasible} (\code{FALSE} if no converged column was found anywhere in the
#'   search box, in which case \code{par}/\code{result} are the least-infeasible probe).
#' @noRd
#' @seealso \code{\link{gceos_column_ns_solve}}, \code{\link{gceos_column_continuation}}
gceos_column_optimize <- function(N, comps, FL, zL, zV, objective, vars,
                                  start = NULL, fixed = list(), feedL = 1, feedV = N,
                                  solvent = "CO2", model = "gceos",
                                  max_iter = 400L, tol = 1e-8, control = list(maxit = 60),
                                  opt_max_iter = 120L, on_eval = NULL,
                                  method = c("local", "global"), n_global = 32L) {
  method <- match.arg(method)
  atm <- 101325
  vnames <- names(vars)
  getv <- function(par, nm) if (nm %in% vnames) par[[nm]] else if (!is.null(fixed[[nm]])) fixed[[nm]]
    else stop("Operating variable '", nm, "' is neither optimised nor fixed.", call. = FALSE)
  lower <- vapply(vars, `[`, numeric(1), 1); upper <- vapply(vars, `[`, numeric(1), 2)
  if (is.null(start)) start <- 0.5 * (lower + upper)
  start <- pmin(pmax(start[vnames], lower), upper)        # keep the seed inside the box
  warm <- NULL
  fast <- FALSE   # inner-loop speed mode (set TRUE during the gradient search): capped Newton
                  # iterations and NO continuation fallback, so infeasible probes bail cheaply.
  run_at <- function(par) {
    par <- stats::setNames(as.numeric(par), vnames)
    Pbar <- getv(par, "pres_bar"); T <- getv(par, "temp")
    SF   <- getv(par, "sf");       rf <- getv(par, "reflux")
    mi <- if (fast) min(opt_max_iter, max_iter) else max_iter
    r <- tryCatch(gceos_column_ns_solve(N, T, Pbar*1e5/atm, comps, FL = FL, zL = zL,
            FV = SF, zV = zV, feedL = feedL, feedV = feedV, reflux = rf, solvent = solvent,
            model = model, max_iter = mi, tol = tol, init = warm),
          error = function(e) NULL)
    # The 8-step S/F continuation is robust but ~8x the cost of a single solve. Run it only
    # OFF the hot path (seed search + final solve); during the gradient loop a non-converged
    # probe is simply treated as infeasible, which is enough to steer L-BFGS-B.
    if ((is.null(r) || !isTRUE(r$converged)) && !fast) {
      r <- tryCatch(gceos_column_continuation(N, T, Pbar*1e5/atm, comps, FL = FL, zL = zL,
              zV = zV, target = SF, reflux = rf, var = "SF", feedL = feedL, feedV = feedV,
              solvent = solvent, model = model, n_steps = 8L, max_iter = max_iter, tol = tol),
            error = function(e) NULL)
    }
    if (!is.null(r) && isTRUE(r$converged)) warm <<- r
    r
  }
  neval <- 0L; best_val <- -Inf; best_par <- NULL
  negobj <- function(par) {
    neval <<- neval + 1L
    feasible <- TRUE; val <- NA_real_
    # box penalty (L-BFGS-B respects bounds, but keep robust to Nelder-Mead too)
    if (any(par < lower - 1e-9) || any(par > upper + 1e-9)) {
      feasible <- FALSE
    } else {
      r <- run_at(par)
      if (is.null(r) || !isTRUE(r$converged)) feasible <- FALSE                 # infeasible -> penalty
      else { val <- tryCatch(objective(r), error = function(e) NA_real_)
             if (!is.finite(val)) { feasible <- FALSE; val <- NA_real_ } }
    }
    if (feasible && is.finite(val) && val > best_val) {
      best_val <<- val; best_par <<- stats::setNames(as.numeric(par), vnames)
    }
    if (is.function(on_eval)) tryCatch(on_eval(list(
      eval = neval, par = stats::setNames(as.numeric(par), vnames), value = val,
      feasible = feasible, best = if (is.finite(best_val)) best_val else NA_real_,
      best_par = best_par)), error = function(e) NULL)
    if (!feasible) 1e6 else -val
  }
  # A penalty-based objective + a gradient optimiser (L-BFGS-B) gets stuck if the start
  # sits in the infeasible plateau (flat 1e6 penalty -> zero gradient -> no descent). So pick
  # a FEASIBLE seed to descend from, then polish (shared L-BFGS-B below). If nothing in the box
  # converges, return cleanly with feasible = FALSE so the caller can report it.
  seed <- start; seed_val <- Inf
  if (method == "global") {
    # GLOBAL: scan the WHOLE box (operating point + box corners + a Halton space-filling
    # sample), evaluating EVERY point so the seed is the best feasible found anywhere - not
    # whichever the operating point happens to sit near (e.g. it finds high-S/F optima a local
    # search from the start would miss). Two things keep it fast: (1) the points are SORTED
    # along the hardest axis (S/F, the over-solvent direction) so each solve WARM-STARTS from
    # the previous one - a gradual sweep instead of a cold continuation per point; (2) the scan
    # uses cheap FAST solves - it only has to RANK basins, and the winner is re-solved to full
    # accuracy at the end. The expensive continuation is reserved for the final resolve.
    cands <- c(list(start), .opt_seed_candidates(lower, upper),
               .opt_global_sample(lower, upper, vnames, n_global))
    ax <- if ("sf" %in% vnames) "sf" else vnames[which.max(upper - lower)]
    cands <- cands[order(vapply(cands, function(p) p[[ax]], numeric(1)))]
    fast <- TRUE
    for (cand in cands) {
      v <- negobj(stats::setNames(as.numeric(cand), vnames))
      if (v < seed_val) { seed_val <- v; seed <- stats::setNames(as.numeric(cand), vnames) }
    }
    fast <- FALSE
    # the scan left `warm` at the last (highest-S/F) point; re-warm AT the seed so the polish's
    # first solves start from the right place.
    if (is.finite(seed_val) && seed_val < 1e6 - 1) run_at(seed)
  } else {
    # LOCAL: the caller's operating point first, then a small space-filling fallback set;
    # stop at the FIRST feasible point and descend from there (fast warm start).
    for (cand in c(list(start), .opt_seed_candidates(lower, upper))) {
      v <- negobj(stats::setNames(as.numeric(cand), vnames))
      if (v < seed_val) { seed_val <- v; seed <- stats::setNames(as.numeric(cand), vnames) }
      if (v < 1e6 - 1) break                                # feasible seed found -> stop
    }
  }
  if (!is.finite(seed_val) || seed_val >= 1e6 - 1) {        # no feasible point anywhere probed
    return(list(par = stats::setNames(seed, vnames), value = -seed_val,
                result = run_at(seed), convergence = 99L, feasible = FALSE))
  }
  # Scale variables by their range so the finite-difference gradient is not dominated by the
  # wide axes (pressure, S/F) over the narrow ones (reflux) - a major source of wasted iterations.
  sc <- upper - lower; sc[!is.finite(sc) | sc <= 0] <- 1
  control <- utils::modifyList(list(maxit = 60, parscale = sc), control)
  fast <- TRUE                      # cheap, continuation-free probes during the gradient search
  op <- stats::optim(seed, negobj, method = "L-BFGS-B",
                     lower = lower, upper = upper, control = control)
  # Derivative-free refinement: a short Nelder-Mead from the best point so far. A simplex method
  # is immune to the penalty-cliff noise and the small-gradient early stop that can leave L-BFGS-B
  # short of the optimum, so it nudges the result closer to the true optimum. Run in FAST mode so
  # infeasible probes near the bounds bail cheaply (no failed continuation per probe); reaching
  # high-S/F basins is the global scan's job, not the refinement's. We run it only for its side
  # effect: negobj keeps best_par/best_val at the GLOBAL best over BOTH passes, so it can only
  # improve the result, never worsen it. The box penalty keeps the (unconstrained) simplex in-box.
  nm_start <- if (!is.null(best_par)) best_par else stats::setNames(op$par, vnames)
  nm_start <- pmin(pmax(nm_start, lower), upper)
  fast <- TRUE
  invisible(tryCatch(stats::optim(nm_start, negobj, method = "Nelder-Mead",
              control = list(maxit = 60L, parscale = sc)), error = function(e) NULL))
  fast <- FALSE                     # robust, fully-converged solve at the reported optimum
  # L-BFGS-B can abnormally terminate in its line search (convergence 51/52) when a trial step
  # falls off the infeasibility-penalty cliff: the gradient explodes and optim returns its
  # STARTING iterate, throwing away the better feasible points it actually visited. So report
  # the best FEASIBLE point observed across the whole search (seed probes + gradient + refinement)
  # rather than op$par. best_par/best_val are maintained for every feasible evaluation above.
  final_par <- if (!is.null(best_par)) best_par else stats::setNames(op$par, vnames)
  final_par <- pmin(pmax(final_par, lower), upper) # guarantee the reported point is inside the box
  best <- run_at(final_par)                       # full-accuracy resolve at the chosen point
  final_val <- tryCatch(objective(best), error = function(e) NA_real_)
  if (!is.finite(final_val)) final_val <- if (is.finite(best_val)) best_val else -op$value
  list(par = stats::setNames(final_par, vnames), value = final_val, result = best,
       convergence = op$convergence, feasible = isTRUE(best$converged))
}

#' @title Flag three-phase (VLLE) stages in a converged column
#' @description Runs an isothermal three-phase flash (\code{\link{gceos_flash3_solve}})
#'   on each stage's bulk (liquid+vapour) composition of an already-solved column,
#'   reporting how many fluid phases are stable on each stage. Where a second liquid
#'   appears (\code{nphase == 3}) the two-phase stage result is only approximate -
#'   use \code{\link{gceos_column_vlle}} there.
#' @param col A result from \code{\link{gceos_column_ns_solve}} /
#'   \code{\link{gceos_column}}.
#' @param T,P,comps,model The column's temperature (scalar or length-N), pressure
#'   (atm), components and model.
#' @return A \code{data.frame} with one row per stage: \code{stage}, \code{nphase}
#'   (1/2/3), and \code{vlle} (logical).
#' @noRd
gceos_column_vlle_check <- function(col, T, P, comps, model = "gceos") {
  N <- nrow(col$x); nc <- ncol(col$x)
  Tv <- if (length(T) == 1L) rep(T, N) else T
  np <- integer(N)
  for (j in seq_len(N)) {
    Lj <- col$L[j]; Vj <- col$V[j]
    z <- (Lj * col$x[j, ] + Vj * col$y[j, ]); z <- z / sum(z)
    r <- tryCatch(gceos_flash3_solve(Tv[j], P, z, comps, model = model), error = function(e) NULL)
    np[j] <- if (is.null(r) || is.na(r$nphase)) NA_integer_ else r$nphase
  }
  data.frame(stage = seq_len(N), nphase = np, vlle = !is.na(np) & np >= 3L)
}

# One relaxation (Gauss-Seidel stage sweep) to convergence at a fixed solvent flow
# FV, given initial profiles. Each stage is a phase-stability-tested flash
# (gceos_flash3_solve -> 1/2/3 phases). A SINGLE-phase stage is handled
# continuously as a pass-through (both leaving streams take the bulk composition,
# the total split in the stage's current flow ratio) so the cascade does not jump
# when a stage crosses a phase boundary. NaN-safe throughout. Internal helper.
.gceos_vlle_relax <- function(N, Tv, P, comps, model, FV, zV, f, reflux, sidx,
                              X, Y, L, V, max_iter, tol, damp, flash3_maxit) {
  nc <- ncol(X); npv <- rep(2L, N); X2 <- matrix(NA_real_, N, nc); bL2 <- rep(0, N)
  status <- "max_iter"; dlt <- NA_real_
  fix <- function(m, fb) { m[!is.finite(m)] <- fb; m }
  nm <- names(comps)
  # Cache the (T-dependent) EOS inputs per UNIQUE stage temperature. Without this,
  # gceos_flash3_solve rebuilds the d-vector + group matrices + association inputs
  # on every stage of every sweep (N x max_iter identical rebuilds for an
  # isothermal column) -- the dominant cost. Call the C++ flash3 directly with the
  # cached inputs. (Same optimisation as gceos_psat/gceos_eval.)
  Tkey  <- as.character(Tv)
  xchin <- stats::setNames(lapply(unique(Tv), function(Tj) .gceos_inputs(Tj, comps, model)),
                           as.character(unique(Tv)))
  flash3 <- function(Tj, z, key) {
    xin <- xchin[[key]]
    rr <- gceos_flash3(Tj, P, z, xin$d, xin$q, xin$nu, xin$g, xin$dg, xin$alpha,
                       GCEOS_R, as.integer(flash3_maxit), xin$numu, xin$Delta0)
    .gceos_flash3_post(rr, nm)
  }
  # Cheap, warm-started TWO-phase flash. Most stages of a near-miscible column are
  # 1- or 2-phase; running the full 3-phase stability cascade on them every sweep
  # is the dominant cost. This fast path skips the liquid-split (tpd) test; the
  # caller confirms any phase-count CHANGE with the full flash3 and re-checks all
  # stages with flash3 every `recheck` sweeps, so phase transitions are not missed.
  Kc <- matrix(NA_real_, N, nc)                 # per-stage warm-start K
  recheck <- 5L
  fast2 <- function(Tj, z, key, Kprev) {
    xin <- xchin[[key]]
    Kg <- if (any(is.finite(Kprev)) && all(Kprev[is.finite(Kprev)] > 0)) Kprev else NULL
    fr <- tryCatch(gceos_flash_cpp(Tj, P, z, xin$d, xin$q, xin$nu, xin$g, xin$dg, xin$alpha,
                   GCEOS_R, Kg, 40L, 1e-6, xin$numu, xin$Delta0), error = function(e) NULL)
    if (is.null(fr) || is.null(fr$status) || fr$status < 0) return(NULL)
    if (fr$status == 0) return(list(nphase = 1L))
    list(nphase = 2L, beta = fr$beta, x = stats::setNames(as.numeric(fr$x), nm),
         y = stats::setNames(as.numeric(fr$y), nm), K = as.numeric(fr$K))
  }
  for (it in seq_len(max_iter)) {
    Xo <- X; Yo <- Y; Lo <- L; Vo <- V
    for (j in seq_len(N)) {
      Lin <- if (j == 1) reflux * V[1] else L[j-1]
      xin <- if (j == 1) Y[1, ] else X[j-1, ]
      Vin <- if (j == N) 0 else V[j+1]              # solvent enters at feedV as external feed f[]
      yin <- if (j == N) rep(0, nc) else Y[j+1, ]
      fin <- Lin*xin + Vin*yin + f[j, ]
      Ft  <- sum(fin); if (!is.finite(Ft) || Ft <= 1e-30) next
      z <- fin / Ft; if (anyNA(z) || any(!is.finite(z))) next
      # phase-aware dispatch: full flash3 for 3-phase stages, transitions and a
      # periodic recheck; otherwise the cheap warm-started 2-phase flash.
      do_full <- (npv[j] == 3L) || ((it %% recheck) == 1L)
      r <- NULL
      if (!do_full) {
        r <- fast2(Tv[j], z, Tkey[j], Kc[j, ])
        if (is.null(r) || r$nphase != npv[j]) r <- NULL   # phase changed -> confirm with full
      }
      if (is.null(r)) r <- tryCatch(flash3(Tv[j], z, Tkey[j]), error = function(e) NULL)
      if (is.null(r) || is.na(r$nphase)) next
      Kc[j, ] <- if (!is.null(r$K)) r$K else NA_real_     # cache K for the next sweep's warm start
      if (r$nphase == 1) {                          # single phase -> continuous pass-through
        npv[j] <- 1L; bL2[j] <- 0; X2[j, ] <- NA
        tot <- V[j] + L[j]
        vf <- if (is.finite(tot) && tot > 1e-30) V[j]/tot
              else if (!is.na(sidx) && z[sidx] > 0.85) 0.9 else 0.1
        Vn <- Ft*vf; Ln <- Ft*(1-vf); yn <- z; xn <- z
      } else if (r$nphase == 2) {
        npv[j] <- 2L; bL2[j] <- 0; X2[j, ] <- NA
        Vn <- Ft*r$beta; Ln <- Ft*(1-r$beta); yn <- r$y; xn <- r$x
      } else {                                      # 3 phases -> two liquids descend together
        npv[j] <- 3L; bl1 <- r$beta_L1; bl2 <- r$beta_L2
        Vn <- Ft*r$beta_V; Ln <- Ft*(bl1+bl2); yn <- r$y
        xn <- if ((bl1+bl2) > 1e-30) (bl1*r$x1 + bl2*r$x2)/(bl1+bl2) else r$x1
        X2[j, ] <- r$x2; bL2[j] <- bl2
      }
      if (!all(is.finite(c(Vn, Ln, yn, xn)))) next
      V[j] <- damp*Vn + (1-damp)*V[j]; L[j] <- damp*Ln + (1-damp)*L[j]
      Y[j, ] <- damp*yn + (1-damp)*Y[j, ]; X[j, ] <- damp*xn + (1-damp)*X[j, ]
      s <- sum(Y[j,]); if (is.finite(s) && s > 0) Y[j,] <- Y[j,]/s
      s <- sum(X[j,]); if (is.finite(s) && s > 0) X[j,] <- X[j,]/s
    }
    mL <- mean(L[is.finite(L)]); mV <- mean(V[is.finite(V)])
    X <- fix(X, 0); Y <- fix(Y, 0); L <- fix(L, if (is.finite(mL)) mL else FV); V <- fix(V, if (is.finite(mV)) mV else FV)
    dlt <- max(abs(c(L-Lo, V-Vo)))/max(c(abs(L), abs(V), 1e-9)) + max(abs(c(X-Xo, Y-Yo)))
    if (is.finite(dlt) && dlt < tol) { status <- "converged"; break }
    # early exit: a single-phase-dominated (miscible) column never reaches a real
    # two-phase solution -- detect it and stop instead of grinding all max_iter.
    if (mean(npv == 1L) > 0.6) { sp <- (if (exists("sp")) sp else 0L) + 1L
      if (sp >= 3L) { status <- "single_phase"; break } } else sp <- 0L
  }
  list(X = X, Y = Y, L = L, V = V, npv = npv, X2 = X2, bL2 = bL2,
       status = status, iters = it, resid = dlt)
}

#' @title Phase-stability-aware counter-current column
#'
#' @description
#' Equilibrium-stage column in which \strong{each stage is a phase-stability-tested
#' flash} (\code{\link{gceos_flash3_solve}}, returning one, two or three fluid
#' phases) rather than an assumed vapour-liquid split. This lets the column handle
#' regimes the two-phase simultaneous-correction solver
#' (\code{\link{gceos_column_ns_solve}}) cannot: a second liquid near the mixture
#' critical (V-L-L), and single-phase (miscible) stages. The vapour ascends and the
#' two liquids descend together; a single-phase stage is a continuous pass-through.
#' Solved by a damped Gauss-Seidel relaxation with an S/F homotopy
#' (\code{homotopy}) for robust start-up.
#'
#' \strong{Behaviour by regime.}
#' \itemize{
#'   \item \emph{Two-phase} (the common case): a fast path solves it with
#'     \code{\link{gceos_column_continuation}}; if no stage is three-phase the exact
#'     V-L answer is returned (identical to \code{\link{gceos_column_ns_solve}}).
#'   \item \emph{Single-phase dominated} (miscible operating point, e.g. very high
#'     S/F near-critical): detected early and reported with \code{status =
#'     "single_phase"} - no two-phase separation exists there; lower the solvent
#'     ratio or pressure.
#'   \item \emph{Three-phase} (genuine VLLE, e.g. water + oil + CO2): solved by the
#'     relaxation. Non-three-phase stages take a fast warm-started two-phase flash;
#'     the full three-phase flash is reserved for three-phase stages, phase-count
#'     changes and a periodic recheck. Use \code{force = TRUE} to skip the
#'     two-phase fast path.
#' }
#'
#' @param N,T,P,comps,FL,zL,FV,zV,feedL,feedV,reflux,solvent,model Column definition,
#'   as in \code{\link{gceos_column_ns_solve}} (T scalar or length-N).
#' @param max_iter,tol,damp Relaxation iteration cap, flow-convergence tolerance and
#'   damping factor (0-1; lower = more stable, slower).
#' @param flash3_maxit Inner three-phase-flash iteration cap.
#' @param force Skip the two-phase fast path and go straight to the stability-aware
#'   relaxation (use for known three-phase systems).
#' @param homotopy,n_homotopy Ramp the solvent flow from low to \code{FV} in
#'   \code{n_homotopy} warm-started steps for robust start-up (default \code{TRUE}).
#' @return A \code{list} like \code{\link{gceos_column_ns_solve}}, plus
#'   \code{nphase} (per-stage phase count), \code{x2}/\code{beta_L2} where a second
#'   liquid is present, and \code{single_phase} (logical). \code{recovery} is
#'   \code{NA} when the column is single-phase dominated.
#' @noRd
#' @seealso \code{\link{gceos_column_vlle_check}}, \code{\link{gceos_column_ns_solve}}
gceos_column_vlle <- function(N, T, P, comps, FL, zL, FV, zV,
                              feedL = 1, feedV = N, reflux = 0, solvent = "CO2",
                              model = "gceos", max_iter = 200L, tol = 1e-6, damp = 0.4,
                              flash3_maxit = 80L, force = FALSE,
                              homotopy = TRUE, n_homotopy = 6L) {
  nc <- length(comps); nms <- names(comps); zL <- zL/sum(zL); zV <- zV/sum(zV)
  Tv <- if (length(T) == 1L) rep(T, N) else T
  sidx <- match(solvent, names(comps))

  # FAST PATH: try the two-phase column (robustly, via S/F continuation) and check
  # for a second liquid. If it converges and no stage is three-phase, the two-phase
  # answer is exact - return it. Otherwise fall through to the stability-aware
  # relaxation (which also rescues cases the NS solver cannot converge at all).
  ns <- if (force) NULL else                        # force -> go straight to the relaxation
    tryCatch(gceos_column_continuation(N, T, P, comps, FL = FL, zL = zL, zV = zV,
             target = FV, reflux = reflux, var = "SF", feedL = feedL, feedV = feedV,
             solvent = solvent, model = model, n_steps = 8L),
           error = function(e) NULL)
  if (!is.null(ns) && isTRUE(ns$converged)) {
    chk <- gceos_column_vlle_check(ns, T, P, comps, model)
    ns$nphase <- chk$nphase
    if (!force && !any(chk$vlle, na.rm = TRUE)) {
      ns$x2 <- matrix(NA_real_, N, nc); ns$beta_L2 <- rep(0, N); return(ns)
    }
    X <- ns$x; Y <- ns$y; L <- ns$L; V <- ns$V
  } else {                                          # cold init: feed-like liquid, solvent vapour
    X <- matrix(zL, N, nc, byrow = TRUE); Y <- matrix(zV, N, nc, byrow = TRUE)
    L <- rep(FL, N); V <- rep(FV, N)
  }

  # S/F homotopy: solve at increasing solvent flow, warm-starting each step. The
  # low-S/F column is clearly two-phase and easy; stepping up walks into the
  # near-miscible / three-phase regime that breaks a cold solve.
  FVs <- if (homotopy && FV > 12) unique(c(exp(seq(log(max(FV/8, 5)),
            log(FV), length.out = n_homotopy)), FV)) else FV
  res <- NULL
  for (fv in FVs) {
    f <- matrix(0, N, nc); f[feedL, ] <- f[feedL, ] + FL*zL; f[feedV, ] <- f[feedV, ] + fv*zV
    Vs <- pmax(V * fv / max(FV, 1e-9), fv * 1e-3)   # rescale vapour init to this S/F
    res <- .gceos_vlle_relax(N, Tv, P, comps, model, fv, zV, f, reflux, sidx,
             X, Y, L, Vs, max_iter, tol, damp, flash3_maxit)
    X <- res$X; Y <- res$Y; L <- res$L; V <- res$V
  }

  f <- matrix(0, N, nc); f[feedL, ] <- f[feedL, ] + FL*zL; f[feedV, ] <- f[feedV, ] + FV*zV
  Ftot <- colSums(f)
  colnames(X) <- nms; colnames(Y) <- nms; X2 <- res$X2; colnames(X2) <- nms
  # if most stages are a single fluid phase, the operating point is miscible (no
  # two-phase separation possible) - report it plainly rather than a bogus profile.
  single_dom <- mean(res$npv == 1L, na.rm = TRUE) > 0.6
  status <- if (single_dom) "single_phase" else res$status
  if (single_dom)
    warning("Column is predominantly single-phase (miscible) at these conditions: ",
            "no two-phase separation. Lower the solvent ratio or pressure (or revise d_c).",
            call. = FALSE)
  refl_frac <- reflux/(1 + reflux); Eprod <- V[1]*(1 - refl_frac)
  yx <- Y[1, ]; yx[sidx] <- 0; ye <- if (is.finite(sum(yx)) && sum(yx) > 0) yx/sum(yx) else yx*NA
  recovery <- if (is.finite(Eprod) && all(is.finite(Y[1, ])))
                pmin((Eprod*Y[1, ]) / (Ftot + 1e-30), 1) else rep(NA_real_, nc)
  list(x = X, y = Y, L = L, V = V, nphase = res$npv, x2 = X2, beta_L2 = res$bL2,
       extract = list(comp = stats::setNames(ye, nms), E = Eprod, reflux = reflux),
       raffinate = list(x = stats::setNames(X[N, ], nms), L = L[N]),
       recovery = stats::setNames(recovery, nms),
       converged = (res$status == "converged" && !single_dom), status = status,
       single_phase = single_dom, iters = res$iters, resid = res$resid)
}

