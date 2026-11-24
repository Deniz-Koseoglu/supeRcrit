#' Bundled openCOSMO-RS-Phi dataset (clustered profiles + fitted params)
#'
#' Returns the packaged dataset that stores openCOSMO-RS-Phi sigma profiles the same way CSP-2021
#' does (`csp_data.rds`): clustered profiles + fitted `c(b_i, delta_m0, delta_mT, b_h)` parameters +
#' metadata, keyed by compound. Storing the *clustered* profile (parsed once) is what lets the
#' `*_prof` compute path skip the per-call `.orcacosmo` re-parse (~30x faster fitting).
#' @param refresh If TRUE, ignore the in-memory cache and re-read the bundle from disk (use after an
#'   `assemble_bundle.R` rebuild within a live session).
#' @return A list: `co2`/`co2_par`, `profiles` (named clustered profiles), `params`, `meta`, `level`.
#' @export
ocrsphi_data <- function(refresh = FALSE, variant = c("4par", "zini")) {
  # Cache the parsed bundle per variant: ocrsphi_solubility() / the co-solvent scan call this once per
  # co-solvent, and a cold readRDS per call is pure waste (mirrors CSP's .csp_cache). Two parallel bundles:
  #   "4par" = ocrsphi_data.rds       (Markgraf old-form 4-param; the default/shipped model, 831 compounds)
  #   "zini" = ocrsphi_data_zini.rds  (consistent 5-param ZINI dispersion; 857 compounds)
  # Both hold the SAME sigma profiles; only the pure-component EoS params + CO2 reference differ.
  variant <- match.arg(variant)
  if (!refresh && !is.null(.ocrsphi_cache[[variant]])) return(.ocrsphi_cache[[variant]])
  fn <- if (variant == "zini") "ocrsphi_data_zini.rds" else "ocrsphi_data.rds"
  f <- system.file("extdata", "solubility", fn, package = "supeRcrit")
  if (!nzchar(f)) stop(fn, " not found; reinstall supeRcrit.", call. = FALSE)
  d <- readRDS(f); .ocrsphi_cache[[variant]] <- d; d
}
.ocrsphi_cache <- new.env(parent = emptyenv())

#' Get a bundled openCOSMO-RS-Phi clustered sigma profile by compound name
#' @param compound Compound name (e.g. "thymoquinone") or "co2".
#' @return A clustered-profile list usable by [ocrsphi_psat()] / [ocrsphi_lnphi()].
#' @export
ocrsphi_profile <- function(compound) {
  d <- ocrsphi_data()
  if (tolower(compound) %in% c("co2", "carbon_dioxide")) return(d$co2)
  if (!compound %in% names(d$profiles))
    stop("no bundled openCOSMO-RS-Phi profile for: ", compound, call. = FALSE)
  d$profiles[[compound]]
}

# Resolve a `profile` argument to a clustered-profile list: pass through a clustered list, load a
# bundled compound by name, or parse+cluster an .orcacosmo path once. Everything downstream then
# uses the fast *_prof_cpp compute path (no per-call re-parse).
.ocrsphi_prof <- function(x) {
  if (is.list(x) && !is.null(x[["group"]]) && !is.null(x[["sigma"]])) return(x)
  if (is.character(x) && length(x) == 1L) {
    if (file.exists(x)) return(ocrsphi_export_profile_cpp(normalizePath(x, mustWork = TRUE), FALSE))
    d <- ocrsphi_data()
    if (tolower(x) %in% c("co2", "carbon_dioxide")) return(d$co2)
    if (x %in% names(d$profiles)) return(d$profiles[[x]])
    stop("profile not found (not a file, not a bundled compound): ", x, call. = FALSE)
  }
  stop("profile must be a clustered-profile list, a bundled compound name, or an .orcacosmo path",
       call. = FALSE)
}

#' openCOSMO-RS-Phi vapor pressure / saturated densities (pure component)
#'
#' Native (no-Python) interface to the openCOSMO-RS-Phi equation of state
#' (Markgraf et al., 2026), a fully open-source COSMO-SAC-Phi implementation.
#' Given a pure-component ORCA sigma profile (`.orcacosmo`) and its four
#' EoS parameters, returns the saturation pressure and saturated molar
#' volumes/densities at each requested temperature.
#'
#' The C++ engine is vendored under `src/ocrsphi_core/` and bound via Rcpp; the
#' pure-component path is validated bit-identical to the upstream pybind11 build
#' (methanol Psat at 280/298.15/320 K = 6051.9 / 16774.5 / 48376.1 Pa).
#'
#' @param profile_path Path to a pure-component `.orcacosmo` sigma profile
#'   (level: CPCM BP86 def2-TZVP + def2-TZVPD single point).
#' @param pc_params Length-4 numeric vector `c(b_i, delta_m0, delta_mT, b_h)`.
#' @param temperatures Numeric vector of temperatures (K).
#' @param pressure_guess Initial pressure guess (Pa). Default 101325.
#' @param molar_mass Optional molar mass (kg/mol) to also return mass densities.
#' @param verbose Print engine binary specs and convergence messages.
#'
#' @return A `data.frame` with columns `T_K`, `Psat_Pa`, `Psat_bar`,
#'   `v_vapor_m3mol`, `v_liquid_m3mol`, and (if `molar_mass` given)
#'   `rho_vapor_kgm3`, `rho_liquid_kgm3`.
#' @export
ocrsphi_psat <- function(profile, pc_params, temperatures,
                         pressure_guess = 101325, molar_mass = NULL,
                         verbose = FALSE) {
  if (!(length(pc_params) %in% c(4L, 5L)))
    stop("pc_params must be length 4 (old form) or 5 (Zini): c(b_i, delta_m0, delta_mT, b_h[, k])")

  res <- ocrsphi_psat_prof_cpp(.ocrsphi_prof(profile),
                               as.numeric(pc_params), as.numeric(temperatures),
                               as.numeric(pressure_guess)[1], isTRUE(verbose))

  out <- data.frame(
    T_K           = res$T,
    Psat_Pa       = res$Psat_Pa,
    Psat_bar      = res$Psat_Pa / 1e5,
    v_vapor_m3mol = res$v_vapor_m3mol,
    v_liquid_m3mol = res$v_liquid_m3mol
  )
  if (!is.null(molar_mass)) {
    out$rho_vapor_kgm3  <- molar_mass / res$v_vapor_m3mol
    out$rho_liquid_kgm3 <- molar_mass / res$v_liquid_m3mol
  }
  out
}

#' openCOSMO-RS-Phi binary pT-flash
#'
#' Native (no-Python) binary pT-flash on the openCOSMO-RS-Phi equation of state.
#' Given two pure-component ORCA sigma profiles, their four EoS parameters each, a
#' temperature, pressure and overall composition, returns the equilibrium phase
#' compositions. Validated behaviourally identical to the upstream pybind11 build.
#'
#' @param profile1,profile2 Paths to the two `.orcacosmo` profiles.
#' @param pc_params1,pc_params2 Length-4 numeric vectors `c(b_i, delta_m0, delta_mT, b_h)`.
#' @param temperature Temperature (K).
#' @param pressure Total pressure (Pa).
#' @param z1 Overall mole fraction of component 1. Default 0.5.
#' @param type `"VL"` (vapor-liquid) or `"LL"` (liquid-liquid).
#' @param verbose Print engine/convergence messages.
#'
#' @return A list with `phase1` and `phase2` (each a length-2 mole-fraction vector;
#'   an all-zero `phase2` means a single stable phase), plus `T`, `P`, `z1`.
#'   For hard/near-critical inputs the underlying solver may signal a condition
#'   (e.g. supercritical / no bracketing root) via an error.
#' @export
ocrsphi_flash <- function(profile1, profile2, pc_params1, pc_params2,
                          temperature, pressure, z1 = 0.5,
                          type = c("VL", "LL"), verbose = FALSE) {
  type <- match.arg(type)
  if (!file.exists(profile1)) stop("profile1 not found: ", profile1)
  if (!file.exists(profile2)) stop("profile2 not found: ", profile2)
  if (!(length(pc_params1) %in% c(4L, 5L)) || length(pc_params1) != length(pc_params2))
    stop("pc_params1/pc_params2 must be equal length, 4 (old form) or 5 (Zini): c(b_i, delta_m0, delta_mT, b_h[, k])")

  par <- rbind(as.numeric(pc_params1), as.numeric(pc_params2))
  res <- ocrsphi_flash_binary_cpp(normalizePath(profile1, mustWork = TRUE),
                                  normalizePath(profile2, mustWork = TRUE),
                                  par, as.numeric(temperature)[1],
                                  as.numeric(pressure)[1], as.numeric(z1)[1],
                                  type, isTRUE(verbose))
  list(phase1 = res$phase_comps[1, ], phase2 = res$phase_comps[2, ],
       T = res$T, P = res$P, z1 = res$z1)
}

#' openCOSMO-RS-Phi single-phase fugacity coefficients at fixed (T, P, x)
#'
#' Evaluates the equation of state for a single fluid phase of a binary mixture
#' at a specified temperature, pressure and composition, returning the component
#' fugacity coefficients (and molar volume / density). This is the building block
#' for supercritical-CO2 **solid** solubility: at equilibrium the solute's fluid
#' fugacity `y2 * phi2 * P` equals its solid fugacity (from fusion data), and
#' solubility is found by root-finding `y2`. Fast (~0.1 s) — no phase-split solve.
#'
#' @param profile1,profile2 Paths to the two `.orcacosmo` profiles.
#' @param pc_params1,pc_params2 Length-4 numeric vectors `c(b_i, delta_m0, delta_mT, b_h)`.
#' @param temperature Temperature (K).
#' @param pressure Target pressure (Pa) at which phi is evaluated.
#' @param x1 Mole fraction of component 1 in the fluid phase.
#' @param root `"liquid"` (dense root, default, for scCO2) or `"vapor"`.
#' @param verbose Print diagnostics.
#'
#' @return A list: `lnphi` / `phi` (length-2, per component), `molar_volume`
#'   (m^3/mol), `density` (mol/m^3), `pressure_solved` (Pa, should match
#'   `pressure`), `status` (1 = root found, -1 = no root of requested type),
#'   and `T`, `P`, `x1`. On `status = -1` the phi/volume fields are `NA`.
#' @export
ocrsphi_lnphi <- function(profile1, profile2, pc_params1, pc_params2,
                          temperature, pressure, x1,
                          root = c("liquid", "vapor"), verbose = FALSE) {
  root <- match.arg(root)
  if (!(length(pc_params1) %in% c(4L, 5L)) || length(pc_params1) != length(pc_params2))
    stop("pc_params1/pc_params2 must be equal length, 4 (old form) or 5 (Zini): c(b_i, delta_m0, delta_mT, b_h[, k])")

  par <- rbind(as.numeric(pc_params1), as.numeric(pc_params2))
  res <- ocrsphi_lnphi_prof_cpp(.ocrsphi_prof(profile1), .ocrsphi_prof(profile2),
                                par, as.numeric(temperature)[1], as.numeric(pressure)[1],
                                as.numeric(x1)[1], if (root == "liquid") 0L else 1L,
                                isTRUE(verbose))
  list(lnphi = res$lnphi, phi = res$phi,
       molar_volume = res$molar_volume_m3mol, density = res$density_mol_m3,
       pressure_solved = res$pressure_solved_Pa, status = res$status,
       T = res$T, P = res$P, x1 = res$x1)
}

#' Supercritical-CO2 solubility of a solid solute (openCOSMO-RS-Phi)
#'
#' Predicts the equilibrium mole-fraction solubility \eqn{y_2} of a solid solute
#' in supercritical CO2 from the openCOSMO-RS-Phi equation of state plus the
#' solute's fusion properties. At solid-fluid equilibrium
#' \deqn{y_2\,\varphi_2^{mix}(T,P,y_2)\,P = \varphi_2^{pureL}(T,P)\,P\,\Gamma_{fus},}
#' where \eqn{\Gamma_{fus}} is the solid / sub-cooled-liquid fugacity ratio from
#' fusion, giving \eqn{y_2 = (\varphi_2^{pureL}/\varphi_2^{mix})\,\Gamma_{fus}},
#' solved by fixed-point iteration. The two fugacity coefficients come from
#' [ocrsphi_lnphi()] (dense/liquid root); fusion uses \eqn{\Delta H_{fus}},
#' \eqn{T_m} (from `gcm_fusion()` or the experimental DB) with an optional
#' heat-capacity-of-fusion term (Prausnitz form).
#'
#' @param solute_profile,co2_profile Paths to the solute and CO2 `.orcacosmo` profiles.
#' @param solute_params,co2_params Length-4 EoS parameters `c(b_i, delta_m0, delta_mT, b_h)` for each.
#' @param temperature Temperature (K).
#' @param pressure Pressure (Pa).
#' @param Tm Solute melting temperature (K).
#' @param dHfus Solute enthalpy of fusion (J/mol).
#' @param dCp Heat-capacity change on fusion (J/mol/K); default 0 (Prausnitz-simplified).
#' @param y2_start Initial guess for the solubility. Default 1e-4.
#' @param tol,maxit Fixed-point convergence tolerance and iteration cap.
#' @param verbose Print iteration trace.
#'
#' @return A list with `y2` (mole-fraction solubility), `gamma_fus`,
#'   `phi_pureL`, `phi2_mix`, `enhancement` (= phi_pureL/phi2_mix, the solvent
#'   effect), `iterations`, `T`, `P`.
#' @seealso [ocrsphi_lnphi()], [gcm_fusion()]
#' @export
ocrsphi_solid_solubility <- function(solute_profile, co2_profile,
                                     solute_params, co2_params,
                                     temperature, pressure, Tm, dHfus,
                                     dCp = 0, y2_start = 1e-4,
                                     tol = 1e-8, maxit = 100L, verbose = FALSE) {
  Rg <- 8.314462618
  if (temperature >= Tm)
    warning("temperature >= Tm: solute is not solid at these conditions")

  # solid / sub-cooled-liquid fugacity ratio (Prausnitz; dCp term optional)
  ln_gamma_fus <- -(dHfus / (Rg * Tm)) * (Tm / temperature - 1)
  if (dCp != 0)
    ln_gamma_fus <- ln_gamma_fus + (dCp / Rg) * (Tm / temperature - 1) -
                    (dCp / Rg) * log(Tm / temperature)
  gamma_fus <- exp(ln_gamma_fus)

  # pure sub-cooled-liquid solute fugacity coefficient at (T, P): solute as component 1, x1 = 1
  pureL <- ocrsphi_lnphi(solute_profile, co2_profile, solute_params, co2_params,
                         temperature, pressure, x1 = 1.0, root = "liquid",
                         verbose = verbose)
  if (isTRUE(pureL$status != 1))
    stop("No dense (liquid-like) root for the pure sub-cooled-liquid solute at (T, P); ",
         "solute reference fugacity is undefined here.")
  phi_pureL <- pureL$phi[1]

  # fixed-point on y2 (solute = component 2 in the CO2(1)-rich fluid)
  y2 <- y2_start; phi2_mix <- NA_real_; it <- 0L
  for (it in seq_len(maxit)) {
    mix <- ocrsphi_lnphi(co2_profile, solute_profile, co2_params, solute_params,
                         temperature, pressure, x1 = 1 - y2, root = "liquid",
                         verbose = FALSE)
    if (isTRUE(mix$status != 1))
      stop("No dense root for the CO2-rich mixture at (T, P, y2 = ", signif(y2, 3), ").")
    phi2_mix <- mix$phi[2]
    y2_new <- (phi_pureL / phi2_mix) * gamma_fus
    if (verbose)
      cat(sprintf("  it %2d: y2 = %.6e  phi2_mix = %.4e\n", it, y2_new, phi2_mix))
    if (!is.finite(y2_new) || y2_new <= 0 || y2_new >= 1)
      stop("y2 left the physical range during iteration: ", y2_new)
    conv <- abs(y2_new - y2) < tol * max(y2_new, 1e-14)
    y2 <- y2_new
    if (conv) break
  }

  list(y2 = y2, gamma_fus = gamma_fus, phi_pureL = phi_pureL, phi2_mix = phi2_mix,
       enhancement = phi_pureL / phi2_mix, iterations = it,
       T = temperature, P = pressure)
}

#' Fit openCOSMO-RS-Phi pure-component EoS parameters
#'
#' Fits the four per-compound parameters `c(b_i, delta_m0, delta_mT, b_h)` for a
#' molecule (given its `.orcacosmo` profile) to experimental saturation pressure
#' and, optionally, liquid density. The fit decouples cleanly: the dispersion
#' parameters `delta_m0`/`delta_mT` govern Psat (and are fit to it), while `b_i`
#' only sets the liquid molar volume (and is fit to density); `b_h` is a weak
#' secondary parameter held fixed by default. Validated on CO2 (Psat reproduced
#' to ~0.05% over 225-290 K).
#'
#' @param profile Path to the compound's `.orcacosmo` profile.
#' @param temperatures Numeric vector of temperatures (K) for the Psat fit.
#' @param Psat_exp Experimental saturation pressures (Pa) at `temperatures`.
#' @param rho_exp Optional experimental liquid density (kg/m^3) to fit `b_i`.
#' @param rho_T Temperature (K) at which `rho_exp` applies. Default: median of `temperatures`.
#' @param molar_mass Molar mass (kg/mol); required if `rho_exp` is given.
#' @param b_h Fixed hole-volume parameter. Default 8.0.
#' @param delta_start,b_i_start Initial guesses.
#'
#' @return A list with `params` = `c(b_i, delta_m0, delta_mT, b_h)`,
#'   `psat_mape` (mean abs. rel. Psat error), and `rho_fit` (if density fit).
#' @seealso [ocrsphi_psat()]
#' @export
ocrsphi_fit_params <- function(profile, temperatures, Psat_exp,
                               rho_exp = NULL, rho_T = NULL, molar_mass = NULL,
                               b_h = 8.0, delta_start = c(0.9, 3.2),
                               b_i_start = 25) {
  prof <- .ocrsphi_prof(profile)          # parse+cluster ONCE -> fit via the fast stored-profile path
  Tg <- as.numeric(temperatures); Pe <- as.numeric(Psat_exp)
  ps <- function(par, T)
    tryCatch(ocrsphi_psat_prof_cpp(prof, par, T, 101325.0, FALSE), error = function(e) NULL)

  # --- delta_m0, delta_mT -> Psat (b_i irrelevant to Psat; penalise supercritical Inf) ---
  obj <- function(x) {
    r <- ps(c(b_i_start, x[1], x[2], b_h), Tg)
    if (is.null(r)) return(1e6)
    p <- r$Psat_Pa
    ninf <- sum(!is.finite(p) | p <= 0)
    if (ninf > 0) return(100 * ninf + 10)
    mean(abs(p / Pe - 1))
  }
  # 2-parameter smooth objective: a lean Nelder-Mead converges quickly (each eval is ~0.2 s x n_T,
  # so keep maxit/tol modest -- see data-raw/ocrsphi timing notes). Callers should pass ~8 T points.
  o <- stats::optim(delta_start, obj, method = "Nelder-Mead",
                    control = list(maxit = 150, reltol = 1e-6))
  d0 <- o$par[1]; dT <- o$par[2]

  # --- b_i -> liquid density (optional) ---
  b_i <- b_i_start; rho_fit <- NA_real_
  if (!is.null(rho_exp)) {
    if (is.null(molar_mass)) stop("molar_mass is required when rho_exp is given")
    if (is.null(rho_T)) rho_T <- stats::median(Tg)
    objb <- function(bi) {
      r <- ps(c(bi, d0, dT, b_h), rho_T)
      if (is.null(r) || !is.finite(r$v_liquid_m3mol[1])) return(1e6)
      abs(molar_mass / r$v_liquid_m3mol[1] - rho_exp)
    }
    ob <- stats::optimize(objb, c(8, 120))
    b_i <- ob$minimum
    rr <- ps(c(b_i, d0, dT, b_h), rho_T)
    rho_fit <- molar_mass / rr$v_liquid_m3mol[1]
  }

  params <- c(b_i = b_i, delta_m0 = d0, delta_mT = dT, b_h = b_h)
  list(params = params, psat_mape = o$value, rho_fit = rho_fit)
}

#' openCOSMO-RS-Phi N-component single-phase fugacity coefficients
#'
#' Generalises [ocrsphi_lnphi()] to an arbitrary number of components (e.g. solute +
#' CO2 + one or more co-solvents). Evaluates one fluid phase at fixed `(T, P, x)`.
#' @param profiles List of N profiles (clustered lists, bundled names, or `.orcacosmo` paths).
#' @param pc_params N x 4 matrix (or list of length-4 vectors), rows `c(b_i, delta_m0, delta_mT, b_h)`.
#' @param temperature K. @param pressure Pa. @param x Length-N mole-fraction vector.
#' @param root `"liquid"` (dense, default) or `"vapor"`. @param verbose Print diagnostics.
#' @return List with `lnphi`, `phi` (length N), `molar_volume`, `density`, `status`, `T`, `P`.
#' @seealso [ocrsphi_lnphi()], [ocrsphi_solubility()]
#' @export
ocrsphi_lnphi_n <- function(profiles, pc_params, temperature, pressure, x,
                            root = c("liquid", "vapor"), verbose = FALSE) {
  root <- match.arg(root)
  profs <- lapply(profiles, .ocrsphi_prof)
  par <- if (is.matrix(pc_params)) pc_params else do.call(rbind, lapply(pc_params, as.numeric))
  if (nrow(par) != length(profs) || length(x) != length(profs))
    stop("profiles, pc_params rows and x must all have the same length N")
  res <- ocrsphi_lnphi_n_prof_cpp(profs, par, as.numeric(temperature)[1], as.numeric(pressure)[1],
                                  as.numeric(x), if (root == "liquid") 0L else 1L, isTRUE(verbose))
  list(lnphi = res$lnphi, phi = res$phi, molar_volume = res$molar_volume_m3mol,
       density = res$density_mol_m3, pressure_solved = res$pressure_solved_Pa,
       status = res$status, T = res$T, P = res$P)
}

#' Supercritical-CO2 solubility of a bundled compound (openCOSMO-RS-Phi)
#'
#' App-facing lookup + predict: resolves `compound` (and any co-solvents) to their bundled
#' openCOSMO-RS-Phi entries (clustered profile + params + fusion `Tm`/`dHfus`) and returns the
#' equilibrium mole-fraction solubility \eqn{y_2} in supercritical CO2 at each `(T, P)`. Mirrors
#' [csp_solubility()]: ONE unified route handles pure CO2 and any number of co-solvents (binary is just
#' the empty-co-solvent case). The solute fugacity coefficient in the `[CO2, co-solvent(s), solute]`
#' fluid comes from [ocrsphi_lnphi_n()]; solids (`T < Tm`) use a composition-accurate fixed point,
#' liquids (`T >= Tm`) the infinite-dilution (Henry) limit.
#'
#' @param compound Solute name present in the bundle (see [ocrsphi_data()] `meta`).
#' @param T Temperature(s) in K. @param P Pressure(s) in bar (recycled with `T`).
#' @param cosolvent Optional character vector of co-solvent names (must be in the bundle).
#' @param x Co-solvent mole fraction(s) on a solute-free basis (recycled to `cosolvent`).
#' @param dHfus,Tm Optional fusion overrides (J/mol, K); default from the bundle.
#' @param bias_correct Apply the bundle's global log-bias correction if present. No-op until fitted.
#' @return Numeric vector of mole-fraction solubilities `y2` (NA where unavailable).
#' @seealso [ocrsphi_solid_solubility()], [ocrsphi_lnphi_n()], [csp_solubility()]
#' @export
ocrsphi_solubility <- function(compound, T, P, cosolvent = NULL, x = 0,
                               dHfus = NULL, Tm = NULL, bias_correct = TRUE,
                               variant = c("4par", "zini")) {
  variant <- match.arg(variant)
  d <- ocrsphi_data(variant = variant); Rg <- 8.314462618
  n <- max(length(T), length(P)); T <- rep_len(as.numeric(T), n); P <- rep_len(as.numeric(P), n)
  resolve <- function(cmp) {
    keys <- names(d$profiles); k <- keys[match(tolower(cmp), tolower(keys))]
    if (is.na(k)) { hit <- which(tolower(d$meta$compound) == tolower(cmp))
      if (!length(hit) && !is.null(d$meta$smiles)) hit <- which(tolower(d$meta$smiles) == tolower(cmp))
      if (length(hit)) k <- d$meta$compound[hit[1]] }
    if (is.na(k) || is.null(d$profiles[[k]])) NA_character_ else k
  }
  nm <- resolve(compound); if (is.na(nm)) return(rep(NA_real_, n))
  prof <- d$profiles[[nm]]; par <- d$params[[nm]]; mi <- match(nm, d$meta$compound)
  Tmv <- if (!is.null(Tm)) Tm else if (!is.na(mi)) d$meta$Tm[mi] else NA_real_
  dH  <- if (!is.null(dHfus)) dHfus else if (!is.na(mi)) d$meta$dHfus[mi] else NA_real_

  # resolve co-solvents -> n-component fluid [CO2, co-solvent(s), solute]
  cs_prof <- list(); cs_par <- list(); xc <- numeric(0)
  if (!is.null(cosolvent) && length(cosolvent)) {
    cosolvent <- as.character(cosolvent); x <- rep_len(as.numeric(x), length(cosolvent))
    keep <- nzchar(cosolvent) & is.finite(x) & x > 0; cosolvent <- cosolvent[keep]; x <- x[keep]
    for (cc in cosolvent) {
      ck <- resolve(cc)
      if (is.na(ck)) stop(sprintf("Co-solvent '%s' is not in the openCOSMO-RS-Phi bundle.", cc), call. = FALSE)
      cs_prof[[length(cs_prof) + 1]] <- d$profiles[[ck]]; cs_par[[length(cs_par) + 1]] <- d$params[[ck]]
    }
    xc <- x
  }
  # ONE unified fluid + solver (binary = empty co-solvent). Fluid ordering [CO2, co-solvent(s), solute];
  # the solute is the last component. The whole (T,P) grid + solid fixed point (or the infinite-dilution
  # liquid limit) is HOISTED into a single C++ call: the molecule set is built ONCE (vs the old R loop
  # that re-marshalled the profiles and rebuilt the engine on every phi evaluation), and both the
  # composition fixed point and the molar-volume root are warm-started across grid points. Same numbers,
  # much faster. `profs` are clustered profiles the C++ side turns straight into molecules.
  profs <- c(list(d$co2), cs_prof, list(prof))
  pars  <- do.call(rbind, c(list(d$co2_par), cs_par, list(par)))
  y2 <- tryCatch(ocrsphi_solubility_cpp(profs, pars, as.numeric(T), as.numeric(P), as.numeric(xc),
                   if (is.finite(Tmv)) as.numeric(Tmv) else NA_real_,
                   if (is.finite(dH))  as.numeric(dH)  else NA_real_,
                   Rg, 100L, 1e-7, FALSE),
                 error = function(e) rep(NA_real_, n))
  if (isTRUE(bias_correct) && !is.null(d$bias) && all(c("a", "b") %in% names(d$bias)))
    y2 <- 10^(d$bias$a + d$bias$b * log10(y2))
  y2
}

# Resolve a compound name/CAS/SMILES to a bundled openCOSMO-RS-Phi key, or NA.
.ocrsphi_resolve <- function(cmp, d = ocrsphi_data()) {
  keys <- names(d$profiles); k <- keys[match(tolower(cmp), tolower(keys))]
  if (is.na(k)) { hit <- which(tolower(d$meta$compound) == tolower(cmp))
    if (!length(hit) && !is.null(d$meta$smiles)) hit <- which(tolower(d$meta$smiles) == tolower(cmp))
    if (length(hit)) k <- d$meta$compound[hit[1]] }
  if (is.na(k) || is.null(d$profiles[[k]])) NA_character_ else k
}

#' Compounds available for openCOSMO-RS-Phi liquid-liquid equilibrium
#'
#' The bundled openCOSMO-RS-Phi compounds (those with an ORCA sigma profile + fitted EoS parameters)
#' that can be used as solvents/solutes in \code{\link{ocrsphi_lle_flash}} / \code{\link{ocrsphi_lle_extract}}.
#' Unlike the COSMO-SAC-dsp LLE engine (2259 UD + 53k CHAOS), openCOSMO-RS-Phi is limited to this small,
#' individually-profiled set - see \code{\link{ocrsphi_data}}.
#' @return character vector of compound names.
#' @seealso \code{\link{ocrsphi_lle_flash}}, \code{\link{csac_lle_flash}}
#' @export
ocrsphi_lle_compounds <- function() {
  d <- ocrsphi_data()
  nm <- names(d$profiles)
  co2 <- vapply(nm, function(k) isTRUE(grepl("carbon dioxide|co2", tolower(k))), logical(1))
  sort(nm[!co2])                                                 # CO2 is the solute-solvent, not an LLE phase
}

#' Two-liquid-phase equilibrium flash (openCOSMO-RS-Phi EoS)
#'
#' The openCOSMO-RS-Phi analog of \code{\link{csac_lle_flash}}: it decides whether a liquid mixture of
#' bundled compounds splits into two phases at a given temperature and pressure, and if so returns both
#' phase compositions. Where the COSMO-SAC-dsp flash equates activity coefficients, this equates
#' \strong{fugacities} from the openCOSMO-RS-Phi equation of state (\code{\link{ocrsphi_lnphi_n}}):
#' \eqn{x_i^{I}\varphi_i^{I} = x_i^{II}\varphi_i^{II}} at fixed \eqn{(T,P)}, solved by the same Michelsen
#' stability + Rachford-Rice + successive-substitution scheme. Because it is a true EoS it needs a
#' pressure. Restricted to the small bundled compound set (\code{\link{ocrsphi_lle_compounds}}).
#'
#' @param components character vector of compound names (see \code{\link{ocrsphi_lle_compounds}}).
#' @param z feed mole fractions (normalized internally; equimolar if \code{NULL}).
#' @param T temperature (K; default 298.15).
#' @param P pressure (Pa; default 101325 = 1 atm).
#' @param tol,max_iter successive-substitution controls.
#' @note Rigorous but slow (a full EoS density-root solve per composition; ~seconds per flash) and
#'   restricted to the small bundled compound set. Validated: water/dichloromethane and water/chloroform
#'   split, water/methanol is single-phase, water/1-propanol partially miscible. (A latent AVX2 SIMD
#'   out-of-bounds read in the upstream segment-activity solver used to segfault here for segment counts
#'   not aligned to the SIMD width; the package now builds the openCOSMO-RS-Phi core scalar, which is
#'   bounds-safe and numerically identical.)
#' @return list with \code{split}, \code{beta}, \code{T}, \code{P} and \code{phase} (data frame:
#'   component, z, x1, x2, D = x1/x2). \code{split = FALSE} means one stable liquid phase.
#' @examples
#' \dontrun{
#' ocrsphi_lle_flash(c("Water", "Dichloromethane"), z = c(0.5, 0.5))   # splits
#' ocrsphi_lle_flash(c("Water", "Methanol"), z = c(0.5, 0.5))          # single phase
#' }
#' @seealso \code{\link{csac_lle_flash}}, \code{\link{ocrsphi_lnphi_n}}, \code{\link{ocrsphi_lle_compounds}}
#' @export
ocrsphi_lle_flash <- function(components, z = NULL, T = 298.15, P = 101325,
                              tol = 1e-9, max_iter = 200) {
  d <- ocrsphi_data()
  keys <- vapply(components, .ocrsphi_resolve, character(1), d = d)
  bad <- components[is.na(keys)]
  if (length(bad)) stop(sprintf("Not in the openCOSMO-RS-Phi bundle: %s. See ocrsphi_lle_compounds().",
                                paste(bad, collapse = ", ")), call. = FALSE)
  profs <- lapply(keys, function(k) d$profiles[[k]])
  pars  <- do.call(rbind, lapply(keys, function(k) as.numeric(d$params[[k]])))
  n <- length(components)
  if (is.null(z)) z <- rep(1 / n, n)
  Tk <- as.numeric(T)[1]; Pa <- as.numeric(P)[1]
  logcoef <- function(x) {
    r <- tryCatch(ocrsphi_lnphi_n(profs, pars, Tk, Pa, x, root = "liquid"), error = function(e) NULL)
    if (is.null(r) || !isTRUE(r$status == 1)) rep(NA_real_, n) else r$lnphi
  }
  r <- .lle_flash_generic(logcoef, z, tol = tol, max_iter = max_iter)
  if (!isTRUE(r$split))
    return(list(split = FALSE, beta = NA_real_, T = Tk, P = Pa,
                phase = data.frame(component = components, z = r$z, x1 = r$z, x2 = NA_real_,
                                   D = NA_real_, stringsAsFactors = FALSE)))
  list(split = TRUE, beta = r$beta, T = Tk, P = Pa,
       phase = data.frame(component = components, z = r$z, x1 = r$x1, x2 = r$x2,
                          D = r$x1 / r$x2, stringsAsFactors = FALSE))
}

#' Screen extraction solvents by openCOSMO-RS-Phi two-phase flash
#'
#' openCOSMO-RS-Phi analog of \code{\link{csac_lle_extract}}: for each candidate solvent (and each
#' temperature) it flashes feed + solvent + dilute solute(s) and reports the distribution
#' \eqn{D = x^{extract}/x^{raffinate}} per solute plus the pairwise selectivity. Restricted to the
#' bundled compounds; screening is slower than COSMO-SAC-dsp (a full EoS flash per candidate).
#'
#' @param solutes solute name(s) (first two define selectivity).
#' @param solvents candidate extraction-solvent names (bundled).
#' @param feed diluent (default \code{"Water"}).
#' @param T temperature(s) in K. @param P pressure (Pa; default 101325).
#' @param solute_frac,solvent_ratio feed make-up (see \code{\link{csac_lle_extract}}).
#' @return data frame ordered best-first: solvent, T_K, split, \code{D_<solute>}, selectivity, beta.
#' @seealso \code{\link{ocrsphi_lle_flash}}, \code{\link{csac_lle_extract}}
#' @export
ocrsphi_lle_extract <- function(solutes, solvents, feed = "Water", T = 298.15, P = 101325,
                                solute_frac = 0.01, solvent_ratio = 1) {
  solutes <- solutes[nzchar(solutes)]; solvents <- unique(solvents[nzchar(solvents)])
  if (!length(solutes) || !length(solvents)) stop("Provide solute(s) and candidate solvent(s).", call. = FALSE)
  ns <- length(solutes)
  grid <- expand.grid(solvent = solvents, T_K = as.numeric(T), stringsAsFactors = FALSE)
  rows <- lapply(seq_len(nrow(grid)), function(g) {
    sv <- grid$solvent[g]; Tk <- grid$T_K[g]
    comps <- c(feed, sv, solutes)
    z <- c(1, solvent_ratio, rep(solute_frac, ns)); z <- z / sum(z)
    fl <- tryCatch(ocrsphi_lle_flash(comps, z = z, T = Tk, P = P), error = function(e) NULL)
    Dcols <- stats::setNames(as.list(rep(NA_real_, ns)), paste0("D_", solutes))
    base <- data.frame(solvent = sv, T_K = Tk, stringsAsFactors = FALSE)
    if (is.null(fl) || !isTRUE(fl$split))
      return(cbind(base, split = FALSE, as.data.frame(Dcols), selectivity = NA_real_, beta = NA_real_))
    ph <- fl$phase
    if (ph$x1[2] >= ph$x2[2]) { xe <- ph$x1; xr <- ph$x2 } else { xe <- ph$x2; xr <- ph$x1 }
    D <- xe[2 + seq_len(ns)] / xr[2 + seq_len(ns)]
    for (k in seq_len(ns)) Dcols[[k]] <- D[k]
    cbind(base, split = TRUE, as.data.frame(Dcols),
          selectivity = if (ns >= 2 && isTRUE(is.finite(D[2])) && D[2] > 0) D[1] / D[2] else NA_real_,
          beta = fl$beta)
  })
  res <- do.call(rbind, rows)
  dcn <- grep("^D_", names(res), value = TRUE)                    # robust to name-mangling of spaced solutes
  key <- if (ns >= 2) res$selectivity else res[[dcn[1]]]
  res[order(-ifelse(is.na(key), -Inf, key)), , drop = FALSE]
}
