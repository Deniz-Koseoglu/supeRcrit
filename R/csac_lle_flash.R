# Full n-component two-liquid-phase flash on the COSMO-SAC-dsp engine (src/csac_core.cpp).
# Unlike csac_lle() (which ranks solvents by infinite-dilution gamma ratios), this solves the actual
# equilibrium: given a feed of solvents + solutes at a temperature, it decides whether the mixture
# splits into two liquid phases and, if so, returns the composition of each phase and the distribution
# of every component between them. This is the engine behind the AMS-style "Optimize LLE" tool.
#
# Method (Michelsen): a tangent-plane stability test decides single- vs two-phase; when unstable, an
# isoactivity flash (Rachford-Rice + successive substitution, K_i = gamma_i^I / gamma_i^II) converges
# the two-phase split. All activity coefficients come from csac_lngamma_cpp at the true composition.

# Resolve a set of component names into the matrices csac_lngamma_cpp needs. Errors list every
# component that could not be resolved so the caller can report them all at once.
.csac_build <- function(components, database = c("ud", "chaos"), predict = NULL) {
  database <- match.arg(database)
  d <- csac_dataset(database)
  if (is.null(predict)) predict <- identical(database, "chaos")
  res <- lapply(components, function(cmp) .csac_resolve(cmp, d, predict))
  bad <- components[vapply(res, is.null, logical(1))]
  if (length(bad))
    stop(sprintf("Could not resolve %d component(s) in the %s database%s: %s",
                 length(bad), database, if (predict) " (FastSigma also failed)" else "",
                 paste(bad, collapse = ", ")), call. = FALSE)
  list(
    names = components,
    psA   = do.call(rbind, lapply(res, function(r) r$psA)),
    area  = vapply(res, function(r) r$area, numeric(1)),
    vol   = vapply(res, function(r) r$vol,  numeric(1)),
    ekB   = vapply(res, function(r) r$ekB,  numeric(1)),
    dcl   = as.integer(vapply(res, function(r) r$dclass, numeric(1))),
    predicted = vapply(res, function(r) isTRUE(r$predicted), logical(1)))
}

# ln(gamma) for one composition (normalized internally so callers may pass unnormalized mole numbers).
.csac_lng <- function(b, x, Tk, dsp) {
  x <- pmax(as.numeric(x), 0); s <- sum(x)
  if (!isTRUE(s > 0)) return(rep(NA_real_, length(x)))
  csac_lngamma_cpp(b$psA, b$area, b$vol, x / s, Tk, b$ekB, b$dcl, isTRUE(dsp))
}

# Rachford-Rice: fraction beta of phase II given K_i = x_i^II / x_i^I. Returns NA if no interior root
# (all K on one side of 1 -> single phase). Solved by safeguarded Newton within the physical window.
.csac_rr <- function(z, K, tol = 1e-12, max_iter = 100) {
  Km1 <- K - 1
  if (all(Km1 >= 0) || all(Km1 <= 0)) return(NA_real_)          # no sign change -> no interior root
  lo <- 1 / (1 - max(K)) + 1e-10                                # window that keeps all x_i > 0
  hi <- 1 / (1 - min(K)) - 1e-10
  g <- function(b) sum(z * Km1 / (1 + b * Km1))
  glo <- g(lo); ghi <- g(hi)
  if (glo * ghi > 0) return(NA_real_)
  b <- 0.5 * (lo + hi)
  for (i in seq_len(max_iter)) {
    gb <- g(b)
    dg <- -sum(z * Km1^2 / (1 + b * Km1)^2)
    if (gb > 0) lo <- b else hi <- b                            # maintain bracket
    step <- if (is.finite(dg) && dg != 0) gb / dg else Inf
    bn <- b - step
    if (!is.finite(bn) || bn <= lo || bn >= hi) bn <- 0.5 * (lo + hi)   # fall back to bisection
    if (abs(bn - b) < tol) return(bn)
    b <- bn
  }
  b
}

# Michelsen tangent-plane stability of feed z at T. Returns whether a second phase can form and, if
# so, the incipient-phase composition to seed the flash. Seeds each near-pure component (LLE splits
# are driven to near-pure corners) plus the feed itself.
.csac_stability <- function(b, z, Tk, dsp, tol = 1e-9, max_iter = 150) {
  n <- length(z)
  di <- log(z) + .csac_lng(b, z, Tk, dsp)                       # tangent-plane reference
  seeds <- lapply(seq_len(n), function(k) { w <- rep(1e-3, n); w[k] <- 1; w / sum(w) })
  best <- list(unstable = FALSE, tm = 0, w = z)
  for (w0 in seeds) {
    lnW <- log(w0); W <- exp(lnW)
    for (it in seq_len(max_iter)) {
      w <- W / sum(W)
      lg <- .csac_lng(b, w, Tk, dsp)
      lnW_new <- di - lg
      if (max(abs(lnW_new - lnW)) < tol) { lnW <- lnW_new; W <- exp(lnW); break }
      lnW <- lnW_new; W <- exp(lnW)
    }
    tm <- 1 - sum(W)
    w <- W / sum(W)
    if (tm < best$tm && max(abs(w - z)) > 1e-4) best <- list(unstable = TRUE, tm = tm, w = w)
  }
  if (best$tm < -1e-7) best$unstable <- TRUE
  best
}

# Molar Gibbs energy of mixing / RT for composition x (used to confirm a split lowers G).
.csac_gmix <- function(b, x, Tk, dsp) {
  x <- x / sum(x); pos <- x > 1e-14
  sum(x[pos] * (log(x[pos]) + .csac_lng(b, x, Tk, dsp)[pos]))
}

# Isoactivity two-phase flash on a PRE-BUILT profile set `b` (from .csac_build). This is the hot inner
# routine shared by csac_lle_flash and the blend optimizer, so it avoids re-resolving compounds on every
# call. `K_init` (a warm-start K = x2/x1) skips the Michelsen stability test entirely - the caller
# guarantees the split structure - which is what makes the optimizer fast (the stability test dominates
# cost). With `K_init = NULL` the full stability test runs (robust cold start). Returns a compact list;
# collapse to a single phase yields `split = FALSE`.
.csac_flash_core <- function(b, z, Tk, dsp, K_init = NULL, max_iter = 300, tol = 1e-10, stab_maxit = 150) {
  z <- pmax(as.numeric(z), 1e-12); z <- z / sum(z)
  r <- csac_lle_flash_cpp(b$psA, b$area, b$vol, z, Tk, b$ekB, b$dcl, isTRUE(dsp),
                          if (is.null(K_init)) NULL else as.numeric(K_init),
                          as.integer(max_iter), tol, as.integer(stab_maxit))
  if (!isTRUE(r$split)) return(list(split = FALSE, z = z))
  list(split = TRUE, beta = r$beta, dG_mix = r$dG_mix, x1 = r$x1, x2 = r$x2,
       lg1 = log(r$gamma1), lg2 = log(r$gamma2), K = r$K, z = r$z)
}

# Engine-AGNOSTIC two-liquid-phase flash. `logcoef(x)` returns the log of the per-component coefficient
# whose product with mole fraction gives the phase activity/fugacity: ln(gamma) for an activity model,
# ln(phi) for a fixed-(T,P) fugacity EoS (e.g. openCOSMO-RS-Phi). Equilibrium is x_i^I coef_i^I =
# x_i^II coef_i^II, so K_i = coef_i^I / coef_i^II and the Michelsen stability + Rachford-Rice + successive
# substitution structure is identical to the isoactivity flash - only the coefficient function differs.
# Returns NA-guarded (a logcoef returning NA aborts to single-phase). This is the ocrsphi LLE path; the
# csac path uses the faster C++ core above.
.lle_flash_generic <- function(logcoef, z, tol = 1e-9, max_iter = 200, stab_maxit = 100) {
  z <- pmax(as.numeric(z), 1e-12); z <- z / sum(z); n <- length(z)
  lc_z <- logcoef(z); if (anyNA(lc_z)) return(list(split = FALSE, z = z))
  di <- log(z) + lc_z
  best <- list(tm = 0, w = z, unstable = FALSE)
  for (k in seq_len(n)) {                                        # Michelsen: seed each near-pure component
    w <- rep(1e-3, n); w[k] <- 1; w <- w / sum(w); lnW <- log(w); W <- exp(lnW)
    for (it in seq_len(stab_maxit)) {
      w <- W / sum(W); lc <- logcoef(w); if (anyNA(lc)) break
      lnW_new <- di - lc
      if (max(abs(lnW_new - lnW)) < 1e-9) { lnW <- lnW_new; W <- exp(lnW); break }
      lnW <- lnW_new; W <- exp(lnW)
    }
    tm <- 1 - sum(W); w <- W / sum(W)
    if (tm < best$tm && max(abs(w - z)) > 1e-4) best <- list(tm = tm, w = w, unstable = TRUE)
  }
  if (!(best$tm < -1e-7)) return(list(split = FALSE, z = z))
  K <- pmax(best$w, 1e-12) / z
  x1 <- z; x2 <- z; beta <- NA_real_
  for (it in seq_len(max_iter)) {
    beta <- .csac_rr(z, K); if (is.na(beta)) break
    x1 <- z / (1 + beta * (K - 1)); x2 <- K * x1; x1 <- x1 / sum(x1); x2 <- x2 / sum(x2)
    lc1 <- logcoef(x1); lc2 <- logcoef(x2); if (anyNA(lc1) || anyNA(lc2)) break
    Knew <- exp(lc1 - lc2)
    if (max(abs(log(Knew) - log(K))) < tol) { K <- Knew; break }
    K <- Knew
  }
  if (is.na(beta) || max(abs(x1 - x2)) < 1e-4) return(list(split = FALSE, z = z))
  lc1 <- logcoef(x1); lc2 <- logcoef(x2)
  if (x2[1] > x1[1]) { tmp <- x1; x1 <- x2; x2 <- tmp; tmp <- lc1; lc1 <- lc2; lc2 <- tmp; beta <- 1 - beta; K <- 1 / K }
  list(split = TRUE, beta = beta, x1 = x1, x2 = x2, lc1 = lc1, lc2 = lc2, K = K, z = z)
}

#' Two-liquid-phase equilibrium flash (COSMO-SAC-dsp)
#'
#' Solves the actual liquid-liquid equilibrium for a multicomponent feed of solvents and solutes at a
#' given temperature: a tangent-plane stability test decides whether the mixture is single-phase or
#' splits, and when it splits an isoactivity flash returns the composition of both liquid phases and
#' the distribution of every component between them. Activity coefficients come from the native
#' COSMO-SAC-dsp engine, so no binary interaction parameters are required.
#'
#' @param components character vector of compound names (InChIKey, common name, CAS or SMILES; see
#'   \code{\link{csac_compounds}}). Include every solvent and solute.
#' @param z feed mole fractions, same length/order as \code{components} (normalized internally). If
#'   \code{NULL}, an equimolar feed is used.
#' @param T temperature (K; default 298.15).
#' @param dsp include the dispersion contribution (default \code{TRUE}).
#' @param database \code{"ud"} (default, 2259 named compounds) or \code{"chaos"} (~53k, SMILES-keyed).
#' @param predict estimate missing profiles from SMILES with FastSigma (defaults \code{TRUE} for
#'   \code{"chaos"}, \code{FALSE} for \code{"ud"}).
#' @param max_iter,tol successive-substitution controls.
#' @return list with \code{split} (logical; \code{TRUE} if two phases), \code{beta} (mole fraction of
#'   phase II), \code{phase} (data frame: component, z, x1, x2, gamma1, gamma2, \code{D} = x1/x2), and
#'   \code{dG_mix} (Gibbs energy of mixing change on splitting, /RT; negative confirms the split). When
#'   \code{split} is \code{FALSE} the feed is a single stable liquid phase.
#' @examples
#' \dontrun{
#' # Water / ethanol / benzene ternary at 25 C
#' csac_lle_flash(c("WATER","ETHANOL","BENZENE"), z = c(0.4,0.2,0.4))
#' }
#' @seealso \code{\link{csac_lle}}, \code{\link{csac_lle_extract}}, \code{\link{csac_gamma}}.
#' @export
csac_lle_flash <- function(components, z = NULL, T = 298.15, dsp = TRUE,
                           database = c("ud", "chaos"), predict = NULL,
                           max_iter = 500, tol = 1e-10, K_init = NULL) {
  database <- match.arg(database)
  b <- .csac_build(components, database, predict)
  n <- length(components)
  if (is.null(z)) z <- rep(1 / n, n)
  Tk <- T[1]
  r <- .csac_flash_core(b, z, Tk, dsp, K_init = K_init, max_iter = max_iter, tol = tol)
  if (!isTRUE(r$split)) {
    g1 <- .csac_lng(b, r$z, Tk, dsp)
    return(list(split = FALSE, beta = NA_real_, dG_mix = 0,
                phase = data.frame(component = components, z = r$z, x1 = r$z, x2 = NA_real_,
                                   gamma1 = exp(g1), gamma2 = NA_real_, D = NA_real_,
                                   stringsAsFactors = FALSE)))
  }
  list(split = TRUE, beta = r$beta, dG_mix = r$dG_mix, K = r$K,
       phase = data.frame(component = components, z = r$z, x1 = r$x1, x2 = r$x2,
                          gamma1 = exp(r$lg1), gamma2 = exp(r$lg2), D = r$x1 / r$x2,
                          stringsAsFactors = FALSE))
}

#' Screen and optimize liquid-liquid extraction solvents (COSMO-SAC-dsp)
#'
#' AMS-style "Optimize LLE": for each candidate extraction solvent (and each temperature) the routine
#' builds a feed of the diluent solvent + the candidate + the dilute solutes, runs the two-phase flash
#' (\code{\link{csac_lle_flash}}), identifies the solvent-rich (extract) and diluent-rich (raffinate)
#' phases, and reports each solute's distribution coefficient \eqn{D = x^{extract}/x^{raffinate}}. With
#' two or more solutes it also reports the pairwise separation selectivity of the first solute over the
#' second, \eqn{S = D_1/D_2}. Rows where the mixture stays single-phase (solvent fully miscible with
#' the diluent, no extraction) are flagged. Results are ranked by selectivity (or \eqn{D_1} for a
#' single solute), so the best solvent/temperature choice sits at the top.
#'
#' @param solutes character vector of solute names to be extracted/separated (see
#'   \code{\link{csac_compounds}}). The first two define the selectivity ratio.
#' @param solvents character vector of candidate extraction solvent names.
#' @param feed diluent solvent the solutes start dissolved in (default \code{"WATER"}).
#' @param T temperature(s) in K (a vector scans temperature; default 298.15).
#' @param solute_frac feed mole fraction of EACH solute (kept dilute; default 0.01).
#' @param solvent_ratio moles of extraction solvent per mole of diluent in the feed (default 1).
#' @param dsp include the dispersion contribution (default \code{TRUE}).
#' @param database,predict see \code{\link{csac_lle_flash}}.
#' @return data frame, one row per (solvent, T): \code{solvent}, \code{T_K}, \code{split}, a
#'   \code{D_<solute>} column per solute, \code{selectivity} (first over second solute),
#'   \code{solvent_in_raffinate} and \code{feed_in_extract} (mutual solubilities, mole fraction), and
#'   \code{beta} (extract-phase fraction). Ordered best-first.
#' @examples
#' \dontrun{
#' csac_lle_extract(c("PHENOL","ANILINE"), solvents = c("N-HEXANE","TOLUENE","ETHYL_ACETATE"),
#'                  feed = "WATER", T = c(298.15, 313.15))
#' }
#' @seealso \code{\link{csac_lle_flash}}, \code{\link{csac_lle}}.
#' @export
csac_lle_extract <- function(solutes, solvents, feed = "WATER", T = 298.15,
                             solute_frac = 0.01, solvent_ratio = 1, dsp = TRUE,
                             database = c("ud", "chaos"), predict = NULL) {
  database <- match.arg(database)
  solutes <- solutes[nzchar(solutes)]
  if (!length(solutes)) stop("Provide at least one solute.", call. = FALSE)
  if (!length(solvents)) stop("Provide at least one candidate solvent.", call. = FALSE)
  ns <- length(solutes)
  grid <- expand.grid(solvent = solvents, T_K = as.numeric(T), stringsAsFactors = FALSE)
  rows <- lapply(seq_len(nrow(grid)), function(g) {
    sv <- grid$solvent[g]; Tk <- grid$T_K[g]
    comps <- c(feed, sv, solutes)
    z <- c(1, solvent_ratio, rep(solute_frac, ns)); z <- z / sum(z)
    fl <- tryCatch(csac_lle_flash(comps, z = z, T = Tk, dsp = dsp, database = database, predict = predict),
                   error = function(e) NULL)
    base <- data.frame(solvent = sv, T_K = Tk, stringsAsFactors = FALSE)
    Dcols <- stats::setNames(as.list(rep(NA_real_, ns)), paste0("D_", solutes))
    if (is.null(fl) || !isTRUE(fl$split)) {
      base$split <- FALSE
      out <- cbind(base, as.data.frame(Dcols), selectivity = NA_real_,
                   solvent_in_raffinate = NA_real_, feed_in_extract = NA_real_, beta = NA_real_)
      return(out)
    }
    ph <- fl$phase
    svi <- 2L                                                   # solvent index in comps
    # Extract phase = the one richer in the candidate solvent; map x_extract / x_raffinate.
    if (ph$x1[svi] >= ph$x2[svi]) { xe <- ph$x1; xr <- ph$x2 } else { xe <- ph$x2; xr <- ph$x1 }
    D <- xe[2 + seq_len(ns)] / xr[2 + seq_len(ns)]
    for (k in seq_len(ns)) Dcols[[k]] <- D[k]
    base$split <- TRUE
    cbind(base, as.data.frame(Dcols),
          selectivity = if (ns >= 2 && isTRUE(is.finite(D[2])) && D[2] > 0) D[1] / D[2] else NA_real_,
          solvent_in_raffinate = xr[svi], feed_in_extract = xe[1], beta = fl$beta)
  })
  res <- do.call(rbind, rows)
  dcn <- grep("^D_", names(res), value = TRUE)                    # robust to name-mangling of spaced solutes
  key <- if (ns >= 2) res$selectivity else res[[dcn[1]]]
  res[order(-ifelse(is.na(key), -Inf, key)), , drop = FALSE]
}

# Compositions of `n` parts on a simplex grid of resolution `k` (each row sums to 1). Used to seed the
# blend optimizer so its local search starts from every region of composition space (multistart).
.simplex_grid <- function(n, k) {
  if (n <= 1) return(matrix(1, 1, 1))
  out <- list()
  rec <- function(prefix, remaining, parts) {
    if (parts == 1L) { out[[length(out) + 1L]] <<- c(prefix, remaining); return(invisible()) }
    for (i in 0:remaining) rec(c(prefix, i), remaining - i, parts - 1L)
  }
  rec(numeric(0), k, n)
  do.call(rbind, out) / k
}

#' Optimize a solvent-blend composition for liquid-liquid extraction (COSMO-SAC-dsp)
#'
#' Finds the \strong{mixed-solvent composition} that best extracts/separates the solutes - the
#' continuous analog of \code{\link{csac_lle_extract}} (which screens pure solvents). Given a small pool
#' of candidate solvents, it searches over their blend fractions (and optionally temperature) to
#' maximize either the separation selectivity (target over the first additional solute) or the target's
#' distribution coefficient, evaluating every trial blend with the full two-phase flash
#' (\code{\link{csac_lle_flash}}). This reproduces the AMS-style "Optimize LLE" mixture search.
#'
#' The objective from a phase-equilibrium flash is non-smooth (blends that do not split score
#' \eqn{-\infty}), so the search combines \strong{multistart} - a simplex grid pre-scan plus local
#' Nelder-Mead refinement from the best few seeds - with optional \strong{warm-start} across a
#' temperature scan (each temperature is seeded from the previous temperature's optimum), exactly the
#' robustness devices a mixture-design MINLP needs.
#'
#' @param solutes character vector of solutes; the first two define the selectivity ratio.
#' @param solvents pool of candidate solvents to blend (2-4 is typical).
#' @param feed diluent phase (default \code{"WATER"}).
#' @param T temperature(s) in K; a vector optimizes over temperature too (default 298.15).
#' @param objective \code{"selectivity"} (target over first extra solute) or \code{"distribution"}
#'   (target's D). Falls back to distribution with a single solute.
#' @param solute_frac,solvent_ratio feed make-up (see \code{\link{csac_lle_extract}}).
#' @param dsp,database,predict passed to \code{\link{csac_lle_flash}}.
#' @param n_grid simplex pre-scan resolution (parts per solvent; default 5 -> 0.2 steps).
#' @param multistart number of best grid seeds to locally refine (default 4).
#' @param warm warm-start across the temperature scan (default \code{TRUE}).
#' @param method \code{"gamma_inf"} (default, AMS-style) scores each trial blend from infinite-dilution
#'   activity-coefficient ratios (\eqn{D = \gamma^\infty_{feed}/\gamma^\infty_{blend}}) - a few gamma
#'   calls per evaluation, and blends miscible with the feed are rejected; the winner is then verified
#'   with one full flash. \code{"flash"} scores every trial blend with the rigorous two-phase flash
#'   (finite-concentration, far slower but self-consistent).
#' @return list: \code{composition} (named optimal blend fractions over \code{solvents}), \code{T_K},
#'   \code{objective}, \code{value}, \code{D} (per solute), \code{selectivity}, \code{flash} (the winning
#'   \code{\link{csac_lle_flash}} result), and \code{trace} (per-temperature best rows).
#' @examples
#' \dontrun{
#' csac_lle_optimize_blend(c("PHENOL","ANILINE"), c("N-HEXANE","ETHYL_ACETATE","TOLUENE"),
#'                         feed = "WATER", T = c(298.15, 313.15))
#' }
#' @seealso \code{\link{csac_lle_extract}}, \code{\link{csac_lle_flash}}.
#' @export
csac_lle_optimize_blend <- function(solutes, solvents, feed = "WATER", T = 298.15,
                                    objective = c("selectivity", "distribution"),
                                    solute_frac = 0.01, solvent_ratio = 1, dsp = TRUE,
                                    database = c("ud", "chaos"), predict = NULL,
                                    n_grid = 5, multistart = 4, warm = TRUE,
                                    method = c("gamma_inf", "flash")) {
  objective <- match.arg(objective); database <- match.arg(database); method <- match.arg(method)
  solutes <- solutes[nzchar(solutes)]; solvents <- unique(solvents[nzchar(solvents)])
  ns <- length(solutes); nsv <- length(solvents)
  if (ns < 1) stop("Provide at least one solute.", call. = FALSE)
  if (nsv < 2) stop("Provide at least two candidate solvents to blend.", call. = FALSE)
  use_sel <- objective == "selectivity" && ns >= 2

  # Resolve the (fixed) component set ONCE - every trial blend reuses it instead of re-resolving.
  comps <- c(feed, solvents, solutes)
  b <- .csac_build(comps, database, predict)
  # Structural warm-start K (= x_organic / x_aqueous): the diluent stays aqueous, the solvents go to the
  # organic phase, the solutes are neutral. Seeding every flash from this (and chaining the last good K)
  # skips the Michelsen stability test - the dominant cost - which is what AMS's "warm-start heuristic"
  # does. Robustness is retained: a warm start that collapses triggers one structural retry.
  Kstruct <- c(1e-2, rep(1e2, nsv), rep(1, ns))
  last_K <- NULL

  # ---- FAST gamma-infinity objective (AMS-style; the DEFAULT) -----------------------------------------
  # Each trial blend costs a handful of activity-coefficient calls, NOT a full flash: the solute's
  # distribution D = gamma_inf(solute in feed) / gamma_inf(solute in blend) is what ranks blends, exactly
  # as AMS's "Optimize LLE" does. A blend that is miscible with the feed (no two phases) is rejected via
  # gamma_inf(feed in blend). This is ~100-1000x fewer gamma evaluations per objective than the flash.
  eps <- 1e-8
  Bfeed  <- .csac_build(c(feed, solutes), database, predict)      # solute(s) dilute in the feed
  Bblend <- .csac_build(c(solvents, solutes), database, predict)  # solute(s) dilute in the solvent blend
  Bfw    <- .csac_build(c(solvents, feed), database, predict)     # feed dilute in the solvent blend
  ginf_feed <- function(Tk) {
    x <- c(1 - ns * eps, rep(eps, ns))
    exp(csac_lngamma_cpp(Bfeed$psA, Bfeed$area, Bfeed$vol, x, Tk, Bfeed$ekB, Bfeed$dcl, isTRUE(dsp))[1 + seq_len(ns)])
  }
  score_ginf <- function(w, Tk) {
    w <- pmax(as.numeric(w), 0); s <- sum(w); if (!isTRUE(s > 0)) return(list(val = -Inf)); w <- w / s
    ws <- w * (1 - ns * eps)
    gb <- exp(csac_lngamma_cpp(Bblend$psA, Bblend$area, Bblend$vol, c(ws, rep(eps, ns)), Tk,
                               Bblend$ekB, Bblend$dcl, isTRUE(dsp))[nsv + seq_len(ns)])
    g_fw <- exp(csac_lngamma_cpp(Bfw$psA, Bfw$area, Bfw$vol, c(w * (1 - eps), eps), Tk,
                                 Bfw$ekB, Bfw$dcl, isTRUE(dsp))[nsv + 1L])
    if (!isTRUE(is.finite(g_fw)) || g_fw < 7) return(list(val = -Inf))   # miscible with feed -> no 2 phases
    D <- ginf_feed(Tk) / gb
    if (any(!is.finite(D)) || any(D <= 0)) return(list(val = -Inf))
    val <- if (use_sel) { if (isTRUE(D[2] > 0)) D[1] / D[2] else -Inf } else D[1]
    list(val = val, D = D, w = w)
  }

  score_flash <- function(w, Tk) {
    w <- pmax(as.numeric(w), 0); s <- sum(w); if (!isTRUE(s > 0)) return(list(val = -Inf))
    w <- w / s
    z <- c(1, solvent_ratio * w, rep(solute_frac, ns)); z <- z / sum(z)
    kin <- if (!is.null(last_K)) last_K else Kstruct
    r <- tryCatch(.csac_flash_core(b, z, Tk, dsp, K_init = kin), error = function(e) NULL)
    if (is.null(r) || !isTRUE(r$split))                        # warm start collapsed -> structural retry
      r <- tryCatch(.csac_flash_core(b, z, Tk, dsp, K_init = Kstruct), error = function(e) NULL)
    if (is.null(r) || !isTRUE(r$split)) return(list(val = -Inf))
    last_K <<- r$K
    sidx <- 1L + seq_len(nsv)
    if (sum(r$x1[sidx]) >= sum(r$x2[sidx])) { xe <- r$x1; xr <- r$x2 } else { xe <- r$x2; xr <- r$x1 }
    didx <- 1L + nsv + seq_len(ns); D <- xe[didx] / xr[didx]
    val <- if (use_sel) { if (isTRUE(D[2] > 0)) D[1] / D[2] else -Inf } else D[1]
    list(val = val, D = D, w = w)
  }
  score <- if (method == "flash") score_flash else score_ginf   # gamma_inf (fast, AMS-style) is default

  # softmax parametrization keeps Nelder-Mead on the interior of the simplex (w >= 0, sum w = 1).
  to_w    <- function(theta) { e <- exp(c(0, theta) - max(c(0, theta))); e / sum(e) }
  to_theta <- function(w) { w <- pmax(w, 1e-6); log(w[-1] / w[1]) }

  seeds <- .simplex_grid(nsv, n_grid)                          # multistart seed pool
  seeds <- rbind(seeds, diag(nsv), rep(1 / nsv, nsv))          # + pure corners + equal blend

  opt_at_T <- function(Tk, warm_w = NULL) {
    pool <- seeds; if (!is.null(warm_w)) pool <- rbind(warm_w, pool)
    gv <- vapply(seq_len(nrow(pool)), function(i) score(pool[i, ], Tk)$val, numeric(1))
    ord <- order(-ifelse(is.finite(gv), gv, -Inf))
    best <- list(val = -Inf)
    nref <- max(1L, min(multistart, sum(is.finite(gv))))
    starts <- if (nref >= 1 && is.finite(gv[ord[1]])) ord[seq_len(nref)] else integer(0)
    for (si in starts) {                                       # local refine from each good seed
      w0 <- pool[si, ]; th0 <- to_theta(w0)
      obj1 <- function(th) -score(to_w(th), Tk)$val
      # A binary blend (2 solvents) is 1-D: Nelder-Mead warns it is unreliable there, so use Brent (bounded
      # 1-D). theta = log(w2/w1) with w clamped to >=1e-6, so |theta| < 14; [-15,15] covers the simplex.
      o <- tryCatch(
        if (length(th0) == 1L)
          stats::optim(th0, obj1, method = "Brent", lower = -15, upper = 15)
        else
          stats::optim(th0, obj1, method = "Nelder-Mead", control = list(maxit = 120, reltol = 1e-6)),
        error = function(e) NULL)
      cand <- if (!is.null(o)) score(to_w(o$par), Tk) else score(w0, Tk)
      seed_res <- score(w0, Tk)                                # keep the better of seed vs refined
      if (isTRUE(seed_res$val > cand$val)) cand <- seed_res
      if (isTRUE(cand$val > best$val)) best <- cand
    }
    if (!is.finite(best$val)) return(NULL)
    best$T_K <- Tk; best
  }

  Ts <- as.numeric(T); trace <- list(); overall <- list(val = -Inf); warm_w <- NULL
  for (Tk in Ts) {
    r <- opt_at_T(Tk, if (isTRUE(warm)) warm_w else NULL)
    if (is.null(r)) next
    warm_w <- r$w
    trace[[length(trace) + 1L]] <- data.frame(T_K = Tk, value = r$val,
      composition = paste(sprintf("%s %d%%", solvents, round(100 * r$w)), collapse = " : "),
      stringsAsFactors = FALSE)
    if (isTRUE(r$val > overall$val)) overall <- r
  }
  if (!is.finite(overall$val))
    return(list(composition = NULL, value = NA_real_, objective = objective,
                trace = if (length(trace)) do.call(rbind, trace) else NULL,
                message = "No blend of these solvents formed two phases under the given conditions."))
  # Rebuild the full flash of the winning blend once, for the returned phase table.
  zbest <- c(1, solvent_ratio * overall$w, rep(solute_frac, ns))
  fl <- tryCatch(csac_lle_flash(comps, z = zbest, T = overall$T_K, dsp = dsp,
                                database = database, predict = predict), error = function(e) NULL)
  list(composition = stats::setNames(overall$w, solvents), T_K = overall$T_K,
       objective = objective, value = overall$val,
       D = stats::setNames(overall$D, solutes),
       selectivity = if (use_sel) overall$val else NA_real_,
       flash = fl,
       trace = do.call(rbind, trace))
}
