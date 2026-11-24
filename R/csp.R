#' COSMO-SAC-Phi (CSP) solid-solubility engine -- Rcpp port
#'
#' Lattice-fluid equation of state (Soares et al. 2019, FPE 488:13; de Souza Jr. 2022,
#' JSCF 191:105765) for predicting solid solubility in supercritical CO2. This file is the
#' R interface to the C++ core (\code{src/csp_core.cpp}); it loads the type-aware sigma
#' profiles and CSP parameters and exposes the pressure / fugacity-coefficient engine.
#'
#' PORT STATUS: complete. The EoS core (pressure + ln-fugacity-coefficient), the volume-root
#' solver, and the fusion/sublimation-route solubility (de Souza Eq. 16) are ported and validated
#' against the Python prototype to <= 1.4e-9 rel. across all 99 de Souza compounds (\code{csp_disp_of},
#' \code{csp_mix_cpp}, \code{csp_solubility_cpp} in src/csp_core.cpp). Built with -O3 (src/Makevars*),
#' the C++ solubility solver is ~1.6x faster than the numpy prototype.
#' @keywords internal
#' @name supeRcrit-csp
#' @useDynLib supeRcrit, .registration = TRUE
#' @importFrom Rcpp evalCpp
NULL

#' Load type-aware sigma profiles from a JCOSMO ExportProf TSV
#'
#' @param path File with repeating NAME/SIGMA/AREA/TYPE tab-separated blocks.
#' @return Named list of profiles, each \code{list(sig, area, hbt, A)}.
#' @keywords internal
csp_load_typed <- function(path) {
  lines <- readLines(path, warn = FALSE)
  out <- list(); i <- 1L; n <- length(lines)
  while (i <= n) {
    if (startsWith(lines[i], "NAME")) {
      f   <- strsplit(lines[i],   "\t", fixed = TRUE)[[1]]
      sig <- as.numeric(strsplit(lines[i + 1L], "\t", fixed = TRUE)[[1]][-1])
      ar  <- as.numeric(strsplit(lines[i + 2L], "\t", fixed = TRUE)[[1]][-1])
      ty  <- bitwAnd(as.integer(strsplit(lines[i + 3L], "\t", fixed = TRUE)[[1]][-1]), 0xFFL)
      # NAME line: NAME <tab> name <tab> volume <tab> area. Capture the cavity volume when present
      # (a positive finite number) so csp_add_compound() can set the geometric parameters from it.
      Vv <- suppressWarnings(as.numeric(f[3]))
      out[[f[2]]] <- list(sig = sig, area = ar, hbt = ty, A = as.numeric(f[4]),
                          V = if (isTRUE(is.finite(Vv) && Vv > 0)) Vv else NA_real_)
      i <- i + 4L
    } else i <- i + 1L
  }
  out
}

#' CSP pressure and ln-fugacity coefficients of a mixture at (T, v)
#'
#' @param profs list of profiles (each \code{list(sig, area, hbt)}).
#' @param x numeric mole fractions (sum 1).
#' @param T temperature (K).
#' @param v molar volume (A^3 per molecule).
#' @param pars list of CSP parameter vectors per compound: c(Rk, bh, Disp, DispT) for
#'   CSP(2019) or c(Rk, bh, Disp, DispT, DispT2) for CSP(2021).
#' @return list(P = pressure (bar), lnphi = numeric vector of ln-fugacity coefficients).
#' @keywords internal
csp_mix <- function(profs, x, T, v, pars) {
  nc    <- length(profs)
  sig   <- unlist(lapply(profs, `[[`, "sig"), use.names = FALSE)
  area0 <- unlist(lapply(profs, `[[`, "area"), use.names = FALSE)
  hbt   <- as.integer(unlist(lapply(profs, `[[`, "hbt"), use.names = FALSE))
  own   <- as.integer(unlist(lapply(seq_len(nc),
              function(i) rep.int(i - 1L, length(profs[[i]]$sig))), use.names = FALSE))
  bi    <- vapply(pars, `[`, numeric(1), 1L)
  bhi   <- vapply(pars, `[`, numeric(1), 2L)
  dseg  <- vapply(pars, function(p) csp_disp_of(p, T), numeric(1))
  csp_mix_cpp(sig, area0, hbt, own, x, bi, bhi, dseg, T, v)
}

#' CSP pressure and ln-fugacity coefficient of a pure compound at (T, v)
#' @inheritParams csp_mix
#' @param prof single profile; \code{par} single parameter vector.
#' @keywords internal
csp_point <- function(prof, T, v, par) csp_mix(list(prof), 1.0, T, v, list(par))

#' CSP solid solubility from explicit profiles + parameters (low-level)
#'
#' Low-level core: callers supply the typed profiles, CSP parameters and fusion thermodynamics
#' directly. Most users want the name-based \code{\link{csp_solubility}} wrapper instead, which
#' looks these up from the bundled \code{csp_data} dataset.
#'
#' @param co2,sol CO2 and solute profiles (\code{list(sig, area, hbt)}).
#' @param co2_par,sol_par CSP parameter vectors c(Rk, bh, Disp, DispT) or, for CSP(2021),
#'   c(Rk, bh, Disp, DispT, DispT2).
#' @param T,P temperature (K) and pressure (bar).
#' @param Tm,dHfus melting temperature (K) and enthalpy of fusion (J/mol).
#' @param vs_solid solid molar volume (cm^3/mol).
#' @param dCp heat capacity of fusion (J/mol/K); \code{NA} -> Poynting fusion route.
#' @param sublim optional c(A,B,C) Antoine sublimation constants (ln(Psub(Pa))=A-B/(T+C));
#'   if supplied, the sublimation route is used instead of fusion.
#' @param cosolv optional list of co-solvent profiles (each \code{list(sig, area, hbt)}) forming, with
#'   CO2, the CO2-rich solvent phase. \code{NULL} (default) is the pure-CO2 binary.
#' @param cosolv_par list of CSP parameter vectors for the co-solvents (same length/order as \code{cosolv}).
#' @param x_cosolv numeric co-solvent mole fractions within the solvent phase (same length as \code{cosolv});
#'   the CO2 fraction is \code{1 - sum(x_cosolv)}. Physically grounded blend mixing: the solute's fugacity
#'   coefficient is evaluated in the multi-component CO2 + co-solvent(s) fluid (sigma-profile mixing).
#' @return Solute mole fraction y2 in the CO2-rich fluid.
#' @seealso \code{\link{csp_solubility}} for the name-based wrapper; \code{\link{csp_dataset}}
#'   for the bundled profiles/parameters this expects.
#' @keywords internal
csp_solubility_raw <- function(co2, sol, co2_par, sol_par, T, P, Tm, dHfus, vs_solid,
                               dCp = NA_real_, sublim = NULL,
                               cosolv = NULL, cosolv_par = NULL, x_cosolv = numeric(0),
                               vF_guess = -1, vL_guess = -1, return_roots = FALSE) {
  x_cosolv <- as.numeric(x_cosolv)
  if (length(x_cosolv) != length(cosolv))
    stop("`x_cosolv` must have one value per co-solvent in `cosolv`.", call. = FALSE)
  if (length(x_cosolv) && (any(x_cosolv < 0) || sum(x_cosolv) >= 1))
    stop("Co-solvent mole fractions must be >= 0 and sum to < 1.", call. = FALSE)
  # Solvent = CO2 (component 0) + co-solvent(s); the solute is appended last (infinite dilution).
  all_profs <- c(list(co2), if (length(cosolv)) cosolv else NULL, list(sol))
  all_pars  <- c(list(co2_par), if (length(cosolv_par)) cosolv_par else NULL, list(sol_par))
  nc <- length(all_profs)
  sig_m  <- unlist(lapply(all_profs, `[[`, "sig"),  use.names = FALSE)
  area_m <- unlist(lapply(all_profs, `[[`, "area"), use.names = FALSE)
  hbt_m  <- as.integer(unlist(lapply(all_profs, `[[`, "hbt"), use.names = FALSE))
  own_m  <- as.integer(unlist(lapply(seq_len(nc),
              function(i) rep.int(i - 1L, length(all_profs[[i]]$sig))), use.names = FALSE))
  bi_m   <- vapply(all_pars, `[`, numeric(1), 1L)
  bhi_m  <- vapply(all_pars, `[`, numeric(1), 2L)
  dseg_m <- vapply(all_pars, function(p) csp_disp_of(p, T), numeric(1))
  xsolv  <- c(1 - sum(x_cosolv), x_cosolv)            # CO2 + co-solvent(s), sums to 1
  # csp_solubility_cpp now returns c(y2, vF, vL) so the caller can thread the volume roots as warm-start
  # seeds across a grid. Default return is the scalar y2 (byte-identical to the old behaviour).
  res <- csp_solubility_cpp(sig_m, area_m, hbt_m, own_m, bi_m, bhi_m, dseg_m,
                     sol$sig, sol$area, as.integer(sol$hbt),
                     sol_par[1], sol_par[2], csp_disp_of(sol_par, T),
                     xsolv, T, P, Tm, dHfus, vs_solid, dCp,
                     if (is.null(sublim)) NULL else as.numeric(sublim),
                     vF_guess, vL_guess)
  if (isTRUE(return_roots)) res else res[1]
}

# --- bundled data accessor (cached) ---------------------------------------------------------
.csp_cache <- new.env(parent = emptyenv())

#' Load the bundled CSP profile/parameter/fusion dataset (cached)
#'
#' @return list with \code{co2}, \code{co2_par19/21}, \code{profiles}, \code{par19/21}, \code{fusion}.
#' @seealso \code{\link{csp_solubility}} and \code{\link{csp_compounds}}, which read this dataset.
#' @keywords internal
csp_dataset <- function() {
  if (is.null(.csp_cache$data)) {
    f <- system.file("extdata", "solubility", "csp_data.rds", package = "supeRcrit")
    if (!nzchar(f)) stop("csp_data.rds not found; reinstall supeRcrit.", call. = FALSE)
    d <- readRDS(f)
    # Merge any user-added compounds (csp_add_compound) over the bundled set.
    .csp_cache$data <- .csp_merge_user(d, .csp_user_load())
  }
  .csp_cache$data
}

.csp_norm <- function(x) gsub("[^a-z0-9]", "", tolower(x))

# Resolve a user compound string to a bundled key (exact jcosmo, then s2 common name, then norm).
.csp_match <- function(compound, d) {
  nm <- names(d$profiles)
  if (compound %in% nm) return(compound)
  q <- .csp_norm(compound)
  hit <- nm[.csp_norm(nm) == q]
  if (length(hit)) return(hit[1])
  hit <- d$fusion$jcosmo[.csp_norm(d$fusion$s2) == q]
  if (length(hit)) return(hit[1])
  NULL
}

#' Compounds available for \code{csp_solubility}
#'
#' @return data frame of bundled compounds (JCOSMO key, common name, fusion data, de Souza ALDs).
#' @seealso \code{\link{csp_solubility}} to predict solubility for any of these compounds.
#' @export
csp_compounds <- function() {
  d <- csp_dataset()
  f <- d$fusion
  f$has_csp19 <- vapply(d$par19[f$jcosmo], Negate(is.null), logical(1))
  f$has_csp21 <- vapply(d$par21[f$jcosmo], Negate(is.null), logical(1))
  f[order(f$jcosmo), c("jcosmo", "s2", "Tm", "dHfus", "fusCp", "vs",
                       "has_csp19", "has_csp21", "ald_paper", "desouza_csp19", "desouza_csp21")]
}

# Look up a compound's SMILES from the CSP dataset (user-added compounds carry one; the bundled set may not).
.csp_smiles_of <- function(key, d) {
  s <- NULL
  if (!is.null(d$fusion$smiles)) s <- d$fusion$smiles[d$fusion$jcosmo == key][1]
  if (is.null(s) || is.na(s) || !nzchar(s)) return(NULL)
  s
}

# Estimate missing fusion data (Tm, dHfus [J/mol], vs [cm3/mol]) from structure. Tm via the melting-point
# ML predictor, dHfus via the Chickos-Acree GCM (needs Tm), vs from molar mass at a typical organic-solid
# density (~1.2 g/cm3) when no crystal value is available. Keeps any experimental value already present.
.csp_fusion_estimate <- function(fr, smiles) {
  Tm <- fr$Tm
  if (!isTRUE(is.finite(Tm))) Tm <- tryCatch(unname(gcm_mp(smiles)[1]), error = function(e) NA_real_)
  dHfus <- fr$dHfus
  if (!isTRUE(is.finite(dHfus)) && isTRUE(is.finite(Tm))) {
    f <- tryCatch(gcm_fusion(smiles, Tm), error = function(e) NULL)
    if (!is.null(f) && isTRUE(is.finite(f$dHfus_Tm))) dHfus <- f$dHfus_Tm * 1000   # kJ/mol -> J/mol
  }
  vs <- fr$vs
  if (!isTRUE(is.finite(vs))) {
    mw <- tryCatch(.mw_from_smiles(smiles), error = function(e) NA_real_)
    if (isTRUE(is.finite(mw))) vs <- mw / 1.2
  }
  list(Tm = Tm, dHfus = dHfus, vs = vs)
}

#' Solid solubility in supercritical CO2 by compound name (COSMO-SAC-Phi)
#'
#' High-level predictor: looks up the compound's typed sigma profile, CSP parameters and fusion
#' thermodynamics from the bundled \code{csp_data} dataset and solves de Souza (2022) Eq. 16 for
#' the solute mole fraction in the CO2-rich fluid. \code{T} and \code{P} are recycled to common
#' length, so a (T, P) grid can be evaluated in one call.
#'
#' @param compound Compound name (JCOSMO key or common name; matched case-insensitively).
#'   \code{\link{csp_compounds}()} lists the available set.
#' @param T Temperature (K), scalar or vector.
#' @param P Pressure (bar), scalar or vector (recycled with \code{T}).
#' @param model \code{"2021"} (CSP-alpha, default) or \code{"2019"}.
#' @param route \code{"fusion"} (default) or \code{"sublimation"}. Sublimation requires
#'   \code{sublim}.
#' @param sublim Antoine constants c(A, B, C) for the sublimation route, ln(Psub(Pa)) = A - B/(T+C).
#' @param fusCp Override the fusion heat capacity (J/mol/K). By default the bundled value is used;
#'   \code{NA} forces the Poynting fusion route.
#' @param cosolvent optional co-solvent(s) added to the CO2 phase: a character vector of one or more
#'   compound names (JCOSMO key or common name; each must be in \code{csp_compounds()}). A blend is
#'   simply a vector of length > 1. \code{NULL} (default) is pure CO2.
#' @param x co-solvent mole fraction(s) within the solvent phase, same length as \code{cosolvent} (a
#'   single value is recycled). The CO2 fraction is \code{1 - sum(x)}. Because COSMO-SAC-Phi is a genuine
#'   multi-component model, the co-solvent (or blend) effect is computed from the mole-fraction-mixed
#'   sigma profiles of the CO2 + co-solvent(s) fluid -- a physically grounded mixing rule (unlike an
#'   empirical or black-box one).
#' @return Numeric vector of solute mole fractions y2 (length = recycled length of T, P).
#' @seealso \code{\link{csp_compounds}} for the available compound set;
#'   \code{\link{csp_solubility_raw}} for the low-level engine taking explicit profiles/parameters.
#' @examples
#' \dontrun{
#' csp_solubility("naphthalene", T = 308.15, P = c(100, 150, 200))
#' csp_solubility("naphthalene", T = 313.15, P = 200, cosolvent = "ETHANOL", x = 0.05)
#' csp_solubility("naphthalene", T = 313.15, P = 200,
#'                cosolvent = c("ETHANOL", "ACETONE"), x = c(0.03, 0.02))   # a blend
#' }
#' @export
csp_solubility <- function(compound, T, P, model = c("2021", "2019"),
                           route = c("fusion", "sublimation"), sublim = NULL, fusCp = NULL,
                           cosolvent = NULL, x = 0, fusion_fallback = FALSE, smiles = NULL) {
  model <- match.arg(model); route <- match.arg(route)
  d <- csp_dataset()
  key <- .csp_match(compound, d)
  if (is.null(key))
    stop(sprintf("Unknown compound '%s'. See csp_compounds() for the available set.", compound),
         call. = FALSE)
  sol <- d$profiles[[key]]
  par <- if (model == "2021") d$par21[[key]] else d$par19[[key]]
  cpar <- if (model == "2021") d$co2_par21 else d$co2_par19
  pick <- if (model == "2021") d$par21 else d$par19
  if (is.null(par))
    stop(sprintf("Compound '%s' has no CSP(%s) parameters.", key, model), call. = FALSE)
  fr <- d$fusion[d$fusion$jcosmo == key, ]
  if (!nrow(fr)) fr <- data.frame(Tm = NA_real_, dHfus = NA_real_, vs = NA_real_, fusCp = NA_real_)
  # Opt-in fusion fallback: estimate any missing Tm/dHfus/vs from structure (gcm_mp + Chickos-Acree
  # gcm_fusion + a rough solid molar volume) so the fusion route can run for compounds without
  # experimental fusion data. Uses the compound's own bundled SMILES when `smiles` is not supplied.
  if (route == "fusion" && isTRUE(fusion_fallback) && (is.na(fr$Tm) || is.na(fr$dHfus) || is.na(fr$vs))) {
    smi <- if (!is.null(smiles) && nzchar(smiles)) smiles else .csp_smiles_of(key, d)
    if (!is.null(smi) && nzchar(smi)) {
      est <- .csp_fusion_estimate(fr, smi); fr$Tm <- est$Tm; fr$dHfus <- est$dHfus; fr$vs <- est$vs
    }
  }
  if (route == "sublimation" && is.null(sublim))
    stop("route = 'sublimation' requires Antoine 'sublim' = c(A, B, C).", call. = FALSE)
  if (route == "fusion" && (is.na(fr$Tm) || is.na(fr$dHfus) || is.na(fr$vs)))
    stop(sprintf("Compound '%s' lacks fusion data (Tm/dHfus/vs) for the fusion route%s.", key,
         if (isTRUE(fusion_fallback)) " (fallback could not estimate it \u2014 no usable SMILES)" else
           " \u2014 enable the fusion fallback to estimate it from structure"),
         call. = FALSE)
  dCp <- if (!is.null(fusCp)) fusCp else fr$fusCp

  # Resolve co-solvent(s): name -> (profile, CSP params); a blend is just >1 name.
  cosolv_prof <- NULL; cosolv_par <- NULL; xco <- numeric(0)
  if (!is.null(cosolvent) && length(cosolvent)) {
    cosolvent <- as.character(cosolvent)
    x <- rep_len(as.numeric(x), length(cosolvent))
    keep <- nzchar(cosolvent) & is.finite(x) & x > 0
    cosolvent <- cosolvent[keep]; x <- x[keep]
    if (length(cosolvent)) {
      cosolv_prof <- vector("list", length(cosolvent)); cosolv_par <- vector("list", length(cosolvent))
      for (j in seq_along(cosolvent)) {
        ck <- .csp_match(cosolvent[j], d)
        if (is.null(ck)) stop(sprintf("Unknown co-solvent '%s'. See csp_compounds().", cosolvent[j]), call. = FALSE)
        cp <- pick[[ck]]
        if (is.null(cp)) stop(sprintf("Co-solvent '%s' has no CSP(%s) parameters.", ck, model), call. = FALSE)
        cosolv_prof[[j]] <- d$profiles[[ck]]; cosolv_par[[j]] <- cp
      }
      xco <- x
    }
  }

  n <- max(length(T), length(P)); T <- rep_len(T, n); P <- rep_len(P, n)
  # Warm-started sweep: thread each point's fluid + liquid volume roots into the next as scan seeds. The
  # roots move smoothly across a (T,P) grid, so the wide volume-root scans collapse to a tight bracket
  # (with a full-scan fallback on a miss). Numerically identical to the old per-point vapply.
  out <- numeric(n); vF <- -1; vL <- -1
  for (i in seq_len(n)) {
    r <- csp_solubility_raw(d$co2, sol, cpar, par, T[i], P[i], fr$Tm, fr$dHfus, fr$vs,
                            dCp = dCp, sublim = if (route == "sublimation") sublim else NULL,
                            cosolv = cosolv_prof, cosolv_par = cosolv_par, x_cosolv = xco,
                            vF_guess = vF, vL_guess = vL, return_roots = TRUE)
    out[i] <- r[1]
    if (is.finite(r[2])) vF <- r[2]
    if (is.finite(r[3])) vL <- r[3]
  }
  out
}

#' Pure-compound saturation (vapor) pressure from the CSP EoS
#'
#' Solves the pure-component vapor-liquid equilibrium of the CSP lattice-fluid EoS: the pressure
#' at which the liquid and vapor volume roots have equal fugacity (de Souza/Soares Eq. for Psat).
#'
#' @param profile typed sigma profile (\code{list(sig, area, hbt)}).
#' @param par CSP parameter vector c(Rk, bh, Disp, DispT) or, for CSP(2021), c(Rk, bh, Disp, DispT, DispT2).
#' @param T temperature(s) (K), scalar or vector.
#' @return data frame with columns T, Psat (bar), vL (A^3/molecule), vl (cm^3/mol); NA rows where
#'   no two-phase root exists at T (supercritical, or the vdW loop has collapsed at low reduced T).
#' @seealso \code{\link{csp_fit_params}}, which fits CSP parameters to Psat/v_liq data via this.
#' @export
csp_psat <- function(profile, par, T) {
  sig <- profile$sig; area <- profile$area; hbt <- as.integer(profile$hbt)
  bi <- par[1]; bh <- par[2]
  out <- vapply(T, function(t)
    csp_psat_cpp(sig, area, hbt, bi, bh, csp_disp_of(par, t), t), numeric(3))
  data.frame(T = T, Psat = out[1, ], vL = out[2, ], vl = out[2, ] * 0.6022)
}

#' Fit CSP pure-compound parameters to saturation-pressure / liquid-volume data
#'
#' Reproduces the Soares (2019) / de Souza (2022) calibration: CSP parameters are not predictive --
#' they are fitted per compound to pure-component saturation pressure (and, optionally, saturated
#' liquid molar volume). This optimizes the Soares Eq. 24 objective
#' \deqn{F = mean_k [ w ((Psat_k - Psat_k^{calc})/Psat_k)^2 + ((vl_k - vl_k^{calc})/vl_k)^2 ]}
#' by Nelder-Mead, calling \code{\link{csp_psat}} for the model values. The reference data may be
#' experimental or estimated (e.g. a group-contribution Psat); the paper samples T in (0.5*Tc, 0.95*Tc).
#'
#' @param profile typed sigma profile (\code{list(sig, area, hbt)}).
#' @param T temperatures (K) of the reference data.
#' @param Psat reference saturation pressures (bar), same length as \code{T}.
#' @param vl optional reference saturated liquid molar volumes (cm^3/mol); if \code{NULL}, a
#'   Psat-only fit is done.
#' @param model \code{"2021"} (5-parameter, default) or \code{"2019"} (4-parameter).
#' @param start full starting parameter vector c(Rk, bh, Disp, DispT) or, for CSP(2021), c(Rk, bh, Disp, DispT, DispT2); required (it sets
#'   the fixed components and the geometry guess for Rk/bh).
#' @param fit character vector naming which components to optimize: any of
#'   \code{"bi","bh","d0","dT","p"} (\code{"p"} only for model 2021). Default \code{c("d0","dT")}.
#' @param weight Psat weighting w in the objective (default 10, as in the paper).
#' @param miss per-point penalty for a temperature where the model has no saturation root
#'   (default 5; moderate so feasibility guides but does not dominate the fit).
#' @param seed if \code{TRUE} (default) a coarse model-aware grid pre-search over the fitted
#'   dispersion components seeds the Nelder-Mead start (robust to poor \code{start}).
#' @param control passed to \code{\link[stats]{optim}}.
#' @return list with \code{par} (fitted full parameter vector), \code{objective}, \code{convergence},
#'   \code{fit} (optimized component names) and \code{n_feasible} (data points with a saturation
#'   root at the solution). Use \code{par} directly in \code{\link{csp_solubility_raw}}.
#' @seealso \code{\link{csp_psat}}, \code{\link{csp_solubility}}.
#' @export
csp_fit_params <- function(profile, T, Psat, vl = NULL, model = c("2021", "2019"),
                           start, fit = c("d0", "dT"), weight = 10, miss = 5,
                           seed = TRUE, control = list()) {
  model <- match.arg(model)
  np <- if (model == "2021") 5L else 4L
  if (missing(start) || length(start) < np)
    stop(sprintf("'start' must be a length-%d CSP(%s) vector c(Rk, bh, Disp, DispT%s).",
                 np, model, if (model == "2021") ", DispT2" else ""), call. = FALSE)
  idx <- c(bi = 1L, bh = 2L, d0 = 3L, dT = 4L, p = 5L)[fit]
  if (any(is.na(idx)) || (model == "2019" && 5L %in% idx))
    stop("'fit' names must be among bi, bh, d0, dT (and p for model 2021).", call. = FALSE)
  par0 <- as.numeric(start[seq_len(np)])

  obj <- function(theta) {
    par <- par0; par[idx] <- theta
    if (par[1] <= 0 || par[2] <= 0 || par[3] <= 0 || par[4] <= 0) return(1e6)
    if (model == "2021" && (par[5] <= 0 || par[5] > 1)) return(1e6)
    pr <- csp_psat(profile, par, T)
    e <- weight * ((Psat - pr$Psat) / Psat)^2
    if (!is.null(vl)) e <- e + ((vl - pr$vl) / vl)^2
    bad <- is.na(e)
    if (all(bad)) return(1e6)
    (sum(e[!bad]) + miss * sum(bad)) / length(e)   # moderate, scale-matched feasibility penalty
  }

  # coarse grid pre-seed over the fitted dispersion components (model-aware ranges)
  theta0 <- par0[idx]
  if (seed) {
    grid <- list(bi = par0[1] * c(0.9, 1, 1.1), bh = c(10, 14, 18),
                 d0 = c(0.3, 0.5, 0.8, 1.2),
                 dT = if (model == "2021") c(300, 600, 1000, 1500) else c(150, 300, 500),
                 p  = c(0.4, 0.7, 0.95))
    cand <- expand.grid(grid[names(idx)], KEEP.OUT.ATTRS = FALSE)
    best <- Inf
    for (r in seq_len(nrow(cand))) {
      v <- as.numeric(cand[r, , drop = TRUE]); o <- obj(v)
      if (o < best) { best <- o; theta0 <- v }
    }
  }

  op <- stats::optim(theta0, obj, method = "Nelder-Mead", control = control)
  par <- par0; par[idx] <- op$par
  nf <- sum(!is.na(csp_psat(profile, par, T)$Psat))
  list(par = par, objective = op$value, convergence = op$convergence,
       fit = names(idx), n_feasible = nf)
}

# Mixture ln-fugacity coefficients at the liquid (or fluid) volume root for (T, P, x).
.csp_lnphi <- function(profs, x, pars, T, P, liquid = TRUE) {
  nc    <- length(profs)
  sig   <- unlist(lapply(profs, `[[`, "sig"),  use.names = FALSE)
  area0 <- unlist(lapply(profs, `[[`, "area"), use.names = FALSE)
  hbt   <- as.integer(unlist(lapply(profs, `[[`, "hbt"), use.names = FALSE))
  own   <- as.integer(unlist(lapply(seq_len(nc),
              function(i) rep.int(i - 1L, length(profs[[i]]$sig))), use.names = FALSE))
  bi    <- vapply(pars, `[`, numeric(1), 1L)
  bhi   <- vapply(pars, `[`, numeric(1), 2L)
  dseg  <- vapply(pars, function(p) csp_disp_of(p, T), numeric(1))
  csp_lnphi_cpp(sig, area0, hbt, own, as.numeric(x), bi, bhi, dseg, T, P, liquid)
}

#' Solid solubility in a liquid solvent (solid-liquid equilibrium) from explicit profiles (low-level)
#'
#' Solves x2 from the SLE condition x2*gamma2 = exp(fusion term), with gamma2 = phi2(mixture liquid)
#' / phi2(pure subcooled liquid) taken from the CSP EoS at the dense-liquid root (Paiva et al. 2026).
#' Most users want the name-based \code{\link{csp_sle_solvent}}.
#'
#' @param solute,solvent typed sigma profiles (\code{list(sig, area, hbt)}).
#' @param sol_par,solv_par CSP parameter vectors for solute and solvent.
#' @param T temperature(s) (K).
#' @param Tm,dHfus solute melting temperature (K) and enthalpy of fusion (J/mol).
#' @param dCp solute heat capacity of fusion (J/mol/K); \code{NA} -> simple fusion term.
#' @param P system pressure (bar; default 1.01325, the SLE working pressure).
#' @return numeric vector of solute mole fractions x2 in the saturated liquid (NA where no root).
#' @seealso \code{\link{csp_sle_solvent}}, \code{\link{csp_solubility_raw}} (SC-CO2 analogue).
#' @keywords internal
csp_sle_solvent_raw <- function(solute, solvent, sol_par, solv_par, T, Tm, dHfus,
                                dCp = NA_real_, P = 1.01325) {
  R_GAS <- 8.314462
  vapply(T, function(Tk) {
    fus <- (dHfus / (R_GAS * Tm)) * (1 - Tm / Tk)
    lnK <- if (is.na(dCp)) fus else fus - (dCp / R_GAS) * (log(Tm / Tk) - Tm / Tk + 1)
    K <- exp(lnK)
    lnphi_pure <- .csp_lnphi(list(solute), 1.0, list(sol_par), Tk, P, liquid = TRUE)[1]
    if (is.na(lnphi_pure)) return(NA_real_)
    f <- function(x2) {
      lp <- .csp_lnphi(list(solvent, solute), c(1 - x2, x2), list(solv_par, sol_par), Tk, P, TRUE)
      if (anyNA(lp)) return(NA_real_)
      x2 * exp(lp[2] - lnphi_pure) - K          # x2*gamma2 - K
    }
    # f(0+) = -K < 0; f(1-) = 1 - K > 0 (pure solute: gamma2 -> 1). Root in (0,1).
    if (is.na(f(1 - 1e-9))) return(NA_real_)
    tryCatch(stats::uniroot(f, c(1e-10, 1 - 1e-9), tol = 1e-9)$root, error = function(e) NA_real_)
  }, numeric(1))
}

#' Solid solubility in a liquid organic solvent by compound name (COSMO-SAC-Phi SLE)
#'
#' Predicts the saturated mole fraction of a solid solute dissolved in a liquid solvent via the
#' CSP equation of state (Paiva et al. 2026): solves x2*gamma2 = exp(fusion term) self-consistently,
#' with the activity coefficient gamma2 from the EoS. Solute and solvent profiles + CSP parameters
#' are taken from the bundled \code{csp_data} dataset; solute fusion data defaults to the bundle and
#' can be overridden. This complements \code{\link{csp_solubility}} (the SC-CO2 analogue) and enables
#' solvent screening / crystallization-solvent selection.
#'
#' @param solute solute name (JCOSMO key or common name; see \code{\link{csp_compounds}}).
#' @param solvent solvent name (must also be in \code{csp_compounds()}, e.g. ETHANOL, BENZENE).
#' @param T temperature(s) (K).
#' @param model \code{"2021"} (default) or \code{"2019"}.
#' @param dHfus,Tm,fusCp solute enthalpy of fusion (J/mol), melting temperature (K) and fusion heat
#'   capacity (J/mol/K); each defaults to the bundled value for \code{solute} when available.
#' @param P system pressure (bar; default 1.01325).
#' @return data frame with columns T and x2 (solute mole fraction in the saturated liquid).
#' @examples
#' \dontrun{
#' csp_sle_solvent("NAPHTHALENE", "BENZENE", T = 298.15)   # ~ ideal, x2 ~ 0.30
#' csp_sle_solvent("NAPHTHALENE", "METHANOL", T = 298.15)  # positive deviation, lower x2
#' }
#' @seealso \code{\link{csp_solubility}}, \code{\link{csp_compounds}}.
#' @export
csp_sle_solvent <- function(solute, solvent, T, model = c("2021", "2019"),
                            dHfus = NULL, Tm = NULL, fusCp = NULL, P = 1.01325) {
  model <- match.arg(model)
  d <- csp_dataset()
  ks <- .csp_match(solute, d); kv <- .csp_match(solvent, d)
  if (is.null(ks)) stop(sprintf("Unknown solute '%s'. See csp_compounds().", solute), call. = FALSE)
  if (is.null(kv)) stop(sprintf("Unknown solvent '%s'. See csp_compounds().", solvent), call. = FALSE)
  pick <- if (model == "2021") d$par21 else d$par19
  sol_par <- pick[[ks]]; solv_par <- pick[[kv]]
  if (is.null(sol_par))  stop(sprintf("Solute '%s' has no CSP(%s) parameters.", ks, model), call. = FALSE)
  if (is.null(solv_par)) stop(sprintf("Solvent '%s' has no CSP(%s) parameters.", kv, model), call. = FALSE)
  fr <- d$fusion[d$fusion$jcosmo == ks, ]
  Tm    <- if (!is.null(Tm))    Tm    else fr$Tm
  dHfus <- if (!is.null(dHfus)) dHfus else fr$dHfus
  dCp   <- if (!is.null(fusCp)) fusCp else fr$fusCp
  if (length(Tm) != 1 || is.na(Tm) || is.na(dHfus))
    stop(sprintf("Solute '%s' lacks fusion data (Tm/dHfus); supply Tm= and dHfus=.", ks), call. = FALSE)
  data.frame(T = T,
             x2 = csp_sle_solvent_raw(d$profiles[[ks]], d$profiles[[kv]], sol_par, solv_par,
                                      T, Tm, dHfus, dCp = dCp, P = P))
}

#' Infinite-dilution activity coefficient of a solute in a liquid solvent (COSMO-SAC-Phi)
#'
#' Computes \eqn{\gamma^\infty} of a dilute solute in a liquid solvent from the CSP (COSMO-SAC-Phi)
#' engine using the bundled typed sigma profiles and CSP parameters. This is the key quantity for
#' liquid-liquid extraction solvent selection: a solute partitions toward the phase where its activity
#' coefficient is lower, so the distribution coefficient between two solvents is the ratio of the two
#' \eqn{\gamma^\infty} values (see \code{\link{csp_lle}}).
#'
#' @param solute,solvent compound names (JCOSMO key or common name; see \code{\link{csp_compounds}}).
#' @param T temperature(s) (K; default 298.15).
#' @param model \code{"2021"} (default) or \code{"2019"}.
#' @param P system pressure (bar; default 1.01325).
#' @return numeric vector of \eqn{\gamma^\infty} (one per \code{T}); \code{NA} when a compound lacks a
#'   bundled profile or CSP parameters.
#' @examples
#' \dontrun{
#' csp_gamma("NAPHTHALENE", "BENZENE")   # ~1 (near-ideal)
#' csp_gamma("NAPHTHALENE", "WATER")     # large (strong positive deviation)
#' }
#' @seealso \code{\link{csp_lle}}, \code{\link{csp_sle_solvent}}, \code{\link{csp_compounds}}.
#' @export
csp_gamma <- function(solute, solvent, T = 298.15, model = c("2021", "2019"), P = 1.01325) {
  model <- match.arg(model); d <- csp_dataset()
  ks <- .csp_match(solute, d); kv <- .csp_match(solvent, d)
  if (is.null(ks)) stop(sprintf("Unknown solute '%s'. See csp_compounds().", solute), call. = FALSE)
  if (is.null(kv)) stop(sprintf("Unknown solvent '%s'. See csp_compounds().", solvent), call. = FALSE)
  pick <- if (model == "2021") d$par21 else d$par19
  sp <- pick[[ks]]; vp <- pick[[kv]]; sprof <- d$profiles[[ks]]; vprof <- d$profiles[[kv]]
  if (is.null(sp) || is.null(vp) || is.null(sprof) || is.null(vprof)) return(rep(NA_real_, length(T)))
  vapply(T, function(Tk) {
    lnphi_pure <- .csp_lnphi(list(sprof), 1.0, list(sp), Tk, P, TRUE)[1]
    lp <- .csp_lnphi(list(vprof, sprof), c(1 - 1e-7, 1e-7), list(vp, sp), Tk, P, TRUE)
    if (is.na(lnphi_pure) || anyNA(lp)) NA_real_ else exp(lp[2] - lnphi_pure)
  }, numeric(1))
}

#' Rank candidate solvents for liquid-liquid extraction (COSMO-SAC-Phi)
#'
#' Screens candidate extraction solvents for pulling a target solute out of a feed/diluent phase. For
#' each candidate the distribution coefficient is \eqn{D = \gamma^\infty_{feed} / \gamma^\infty_{solvent}}
#' (the solute prefers the phase where its activity coefficient is lower), computed from the CSP
#' (COSMO-SAC-Phi) engine. A second solute (\code{coextract}) optionally yields the separation
#' selectivity \eqn{S = D_{solute}/D_{coextract}}; the solvent capacity is taken as
#' \eqn{1/\gamma^\infty_{solvent}}.
#'
#' @param solute target solute name (JCOSMO key or common name; see \code{\link{csp_compounds}}).
#' @param solvents character vector of candidate solvent names (default: all bundled compounds).
#' @param feed feed/diluent solvent the solute starts in (default \code{"WATER"}).
#' @param T temperature (K; default 298.15).
#' @param coextract optional second solute to compute extraction selectivity against.
#' @param model \code{"2021"} (default) or \code{"2019"}.
#' @return data frame ordered by descending \code{D}, with columns \code{solvent}, \code{gamma_feed},
#'   \code{gamma_solvent}, \code{D}, \code{capacity}, and (when \code{coextract} is given) \code{selectivity}.
#' @examples
#' \dontrun{
#' csp_lle("NAPHTHALENE", feed = "WATER")               # rank solvents to extract from water
#' csp_lle("NAPHTHALENE", coextract = "ANTHRACENE")     # + separation selectivity
#' }
#' @seealso \code{\link{csp_gamma}}, \code{\link{csp_compounds}}.
#' @export
csp_lle <- function(solute, solvents = NULL, feed = "WATER", T = 298.15,
                    coextract = NULL, model = c("2021", "2019")) {
  model <- match.arg(model)
  if (is.null(solvents)) solvents <- csp_compounds()$jcosmo
  g_feed <- csp_gamma(solute, feed, T, model)[1]
  if (!isTRUE(is.finite(g_feed))) stop(sprintf("Could not compute gamma for '%s' in feed '%s'.", solute, feed), call. = FALSE)
  g_co_feed <- if (!is.null(coextract)) csp_gamma(coextract, feed, T, model)[1] else NA_real_
  rows <- lapply(setdiff(solvents, feed), function(sv) {
    gs <- suppressWarnings(tryCatch(csp_gamma(solute, sv, T, model)[1], error = function(e) NA_real_))
    if (!isTRUE(is.finite(gs)) || gs <= 0) return(NULL)
    out <- data.frame(solvent = sv, gamma_feed = g_feed, gamma_solvent = gs,
                      D = g_feed / gs, capacity = 1 / gs, stringsAsFactors = FALSE)
    if (!is.null(coextract) && isTRUE(is.finite(g_co_feed))) {
      gco <- suppressWarnings(tryCatch(csp_gamma(coextract, sv, T, model)[1], error = function(e) NA_real_))
      out$selectivity <- if (isTRUE(is.finite(gco)) && gco > 0) (g_feed / gs) / (g_co_feed / gco) else NA_real_
    }
    out
  })
  res <- do.call(rbind, Filter(Negate(is.null), rows))
  if (is.null(res) || !nrow(res)) return(res)
  res[order(-res$D), , drop = FALSE]
}

# Acentric factor (Edmister 1958) from Tb, Tc (K), Pc (bar).
.csp_omega_edmister <- function(Tb, Tc, Pc) {
  th <- Tb / Tc
  (3 / 7) * (th / (1 - th)) * log10(Pc / 1.01325) - 1
}

#' Corresponding-states saturation pressure (Ambrose-Walton)
#'
#' Generalized Riedel/Ambrose-Walton vapor-pressure correlation from critical properties and the
#' acentric factor: ln(Pr) = f0 + omega*f1 + omega^2*f2. Used to supply Psat(T) to
#' \code{\link{csp_params_from_gcm}} when no experimental vapor pressure is available.
#'
#' @param T temperature(s) (K).
#' @param Tc,Pc critical temperature (K) and pressure (bar).
#' @param omega acentric factor.
#' @return saturation pressure(s) (bar).
#' @export
csp_psat_corrstates <- function(T, Tc, Pc, omega) {
  Tr <- T / Tc; ta <- 1 - Tr
  f0 <- (-5.97616 * ta + 1.29874 * ta^1.5 - 0.60394 * ta^2.5 - 1.06841 * ta^5) / Tr
  f1 <- (-5.03365 * ta + 1.11505 * ta^1.5 - 5.41217 * ta^2.5 - 7.46628 * ta^5) / Tr
  f2 <- (-0.64771 * ta + 2.41539 * ta^1.5 - 4.26979 * ta^2.5 + 3.25259 * ta^5) / Tr
  Pc * exp(f0 + omega * f1 + omega^2 * f2)
}

# Rackett saturated-liquid molar volume (cm^3/mol) from Tc (K), Pc (bar), Vc (cm^3/mol).
.csp_vliq_rackett <- function(T, Tc, Pc, Vc) {
  R <- 83.14; Zc <- Pc * Vc / (R * Tc)
  (R * Tc / Pc) * Zc^(1 + (1 - T / Tc)^(2 / 7))
}

#' Estimate CSP parameters from group-contribution critical properties (no experimental data)
#'
#' Fully predictive parametrization for a compound lacking fitted CSP parameters: builds a
#' pure-component Psat(T) (Ambrose-Walton, \code{\link{csp_psat_corrstates}}) and saturated liquid
#' volume v_liq(T) (Rackett) from group-contribution critical properties (Tc, Pc, Vc, Tb -> omega by
#' Edmister), then fits the CSP parameters to them via \code{\link{csp_fit_params}} over Tr in
#' \code{Tr_range}. This is the no-experimental-data pathway implied by de Souza (2022) (who note
#' critical properties are routinely group-contribution-estimated). Accuracy is bounded by the GCM
#' property accuracy; see package tests for the 99-compound benchmark.
#'
#' @param profile typed sigma profile (\code{list(sig, area, hbt, V)}) for the solute (QM/JCOSMO).
#'   The COSMO cavity volume \code{V} (A^3), when present, sets the geometric parameters bi, bh.
#' @param Tc,Pc,Vc,Tb critical temperature (K), pressure (bar), volume (cm^3/mol), boiling point (K),
#'   e.g. from \code{est_gcm()} or an experimental database.
#' @param model \code{"2019"} (4-parameter, default; best determined by estimated Psat) or \code{"2021"}.
#' @param fit components to optimize. Default \code{c("d0","dT")}: the geometric parameters bi, bh are
#'   set from the COSMO cavity volume (bi = 1.175 V - 4.35, R^2 0.98; bh = 0.0363 V + 12.2, over the 256
#'   LVPP compounds) and only the dispersion is fit to Psat. Add "bi"/"bh" to fit them instead (the
#'   fallback when the profile carries no \code{V}).
#' @param n number of Psat/v_liq points (default 9).
#' @param Tr_range reduced-temperature window for the fit data (default c(0.55, 0.88)).
#' @param ... passed to \code{\link{csp_fit_params}}.
#' @return the \code{\link{csp_fit_params}} result list, with added \code{omega} and the input
#'   critical properties; \code{$par} is the fitted CSP parameter vector.
#' @seealso \code{\link{csp_fit_params}}, \code{\link{csp_psat_corrstates}}, \code{\link{csp_solubility}}.
#' @export
csp_params_from_gcm <- function(profile, Tc, Pc, Vc, Tb, model = c("2019", "2021"),
                                fit = c("d0", "dT"), n = 6, Tr_range = c(0.55, 0.88), ...) {
  model <- match.arg(model)
  omega <- .csp_omega_edmister(Tb, Tc, Pc)
  Tg <- seq(Tr_range[1], Tr_range[2], length.out = n) * Tc
  Ps <- csp_psat_corrstates(Tg, Tc, Pc, omega)
  vl <- .csp_vliq_rackett(Tg, Tc, Pc, Vc)
  # geometric bi, bh from the COSMO cavity volume (population fit over the 256 LVPP compounds);
  # fall back to seeds + fitting them when the profile carries no V.
  V <- profile$V
  if (!is.null(V) && length(V) == 1 && is.finite(V)) {
    bi0 <- 1.1752 * V - 4.35; bh0 <- 0.03630 * V + 12.22
  } else {
    bi0 <- stats::median(vl) / 0.6022 * 0.85; bh0 <- 16
    fit <- union(fit, c("bi", "bh"))
  }
  start <- if (model == "2021") c(bi0, bh0, 0.6, 600, 0.7) else c(bi0, bh0, 0.6, 300)
  res <- csp_fit_params(profile, as.numeric(Tg), as.numeric(Ps), vl = as.numeric(vl),
                        model = model, start = start, fit = fit,
                        control = list(maxit = 120, reltol = 1e-4), ...)
  c(res, list(omega = as.numeric(omega), Tc = Tc, Pc = Pc, Vc = Vc, Tb = Tb))
}
