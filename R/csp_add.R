#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# User-extensible COSMO-SAC-Phi (CSP) compound layer
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Adds a compound to the CSP solid-solubility model (csp_solubility()) from a PROPER QM/COSMO sigma
# profile, fitting the CSP EoS parameters from group-contribution critical properties. New compounds
# persist to a per-user store (tools::R_user_dir) that csp_dataset() merges OVER the bundled 256-set.
#
# SCALE CONSISTENCY (critical): the sigma profile MUST be computed on the SAME QM/COSMO level as the
# bundled CO2 profile and the 256 LVPP compounds -- i.e. the JCOSMO "CSP(2019)" set. Generate it with
# JCOSMO / the open LVPP database (github.com/lvpp/sigma, NWChem-COSMO) and export via JCOSMO's
# ExportProf, which csp_load_typed() parses. A profile from a different QM level (e.g. an ORCA
# def2-TZVPD openCOSMO-RS profile) is on a different scale and mixing it with the bundled CO2 profile
# is NOT physically consistent -- it would also need the CO2 profile and every reference param re-done.

.csp_or <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

# Location of the persistent user CSP store (one rds holding a named list of compound entries).
.csp_user_file <- function()
  file.path(tools::R_user_dir("supeRcrit", "data"), "csp_user.rds")

.csp_user_load <- function() {
  f <- .csp_user_file()
  if (file.exists(f)) tryCatch(readRDS(f), error = function(e) NULL) else NULL
}

.csp_user_save <- function(store) {
  f <- .csp_user_file()
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  saveRDS(store, f)
  invisible(f)
}

# Merge a user store (named list keyed by CSP key) over a bundled dataset `d` (in-memory).
.csp_merge_user <- function(d, store) {
  if (is.null(store) || !length(store)) return(d)
  fcols <- names(d$fusion)
  for (key in names(store)) {
    e <- store[[key]]
    d$profiles[[key]] <- e$profile
    if (!is.null(e$par19)) d$par19[[key]] <- e$par19
    if (!is.null(e$par21)) d$par21[[key]] <- e$par21
    row <- stats::setNames(
      as.list(rep(NA, length(fcols))), fcols)          # NA-fill any columns we do not set
    row$jcosmo <- key; row$s2 <- .csp_or(e$s2, key)
    row$Tm <- e$Tm; row$dHfus <- e$dHfus; row$fusCp <- e$fusCp; row$vs <- e$vs
    row <- as.data.frame(row, stringsAsFactors = FALSE)[, fcols, drop = FALSE]
    d$fusion <- rbind(d$fusion[d$fusion$jcosmo != key, , drop = FALSE], row)
  }
  d
}

# Resolve `profile` (a typed list, or a JCOSMO ExportProf TSV path) to a typed profile list.
.csp_resolve_profile <- function(profile, pick = NULL) {
  if (is.list(profile) && all(c("sig", "area", "hbt") %in% names(profile))) {
    p <- profile
    p$A <- .csp_or(p$A, sum(p$area))
    return(p)
  }
  if (is.character(profile) && length(profile) == 1L && file.exists(profile)) {
    profs <- csp_load_typed(profile)
    if (!length(profs)) stop("No profiles parsed from '", profile, "'.", call. = FALSE)
    # Prefer a name match, but fall back to the sole profile when the file has just one (the
    # display name a user chooses need not equal the profile's internal name).
    if (!is.null(pick)) {
      i <- which(.csp_norm(names(profs)) == .csp_norm(pick))
      if (length(i)) return(profs[[i[1L]]])
    }
    if (length(profs) == 1L) return(profs[[1L]])
    stop(sprintf("TSV holds %d profiles; set jcosmo_name to one of: %s",
                 length(profs), paste(names(profs), collapse = ", ")), call. = FALSE)
  }
  stop("'profile' must be a typed list(sig, area, hbt[, A, V]) or a path to a JCOSMO ExportProf TSV.",
       call. = FALSE)
}

#' Add a compound to the CSP solid-solubility model from a QM sigma profile
#'
#' Extends \code{\link{csp_solubility}} / \code{\link{csp_compounds}} with a user compound, given a
#' \emph{proper} quantum-chemical COSMO sigma profile. The CSP equation-of-state parameters are fitted
#' from group-contribution critical properties via \code{\link{csp_params_from_gcm}} (no experimental
#' vapour-pressure data needed); the fusion thermodynamics (\code{Tm}, \code{dHfus}, \code{vs}) are
#' supplied for the solid-solubility route. The compound is written to a persistent per-user store
#' (\code{tools::R_user_dir("supeRcrit", "data")}) that \code{\link{csp_dataset}} merges over the
#' bundled set, so it is available to \code{csp_solubility()} by name in this and future sessions.
#'
#' @section Scale consistency: the profile MUST be on the same QM/COSMO level as the bundled CO2
#'   profile and the 256 LVPP compounds (JCOSMO "CSP(2019)"). Generate it with JCOSMO / the open LVPP
#'   database (\url{https://github.com/lvpp/sigma}, NWChem-COSMO) and export with JCOSMO's ExportProf.
#'   A profile from a different level of theory is on a different scale and is not consistent with the
#'   bundled CO2 profile.
#'
#' @param name Display / lookup name (used as the key, upper-cased, unless \code{key} is given).
#' @param profile A typed sigma profile \code{list(sig, area, hbt[, A, V])}, or a path to a JCOSMO
#'   \code{ExportProf} TSV (parsed by \code{\link{csp_load_typed}}; if it holds several profiles, pick
#'   one with \code{jcosmo_name}).
#' @param Tc,Pc,Vc,Tb Critical temperature (K), pressure (bar), volume (cm^3/mol) and normal boiling
#'   point (K). From \code{\link{est_gcm}} (Joback) or an experimental source. Required.
#' @param Tm,dHfus,vs Fusion data for the solid route: melting temperature (K), enthalpy of fusion
#'   (J/mol) and solid molar volume (cm^3/mol). Optional, but the fusion \code{csp_solubility()} route
#'   errors without them (they can be added later by re-calling with \code{overwrite = TRUE}).
#' @param fusCp Fusion heat-capacity difference (J/mol/K); \code{NA} uses the Poynting fusion route.
#' @param model \code{"2021"} (5-parameter CSP-alpha, default) or \code{"2019"} (4-parameter).
#' @param V COSMO cavity volume (A^3) if not already carried on \code{profile$V}; sets the geometric
#'   parameters bi, bh (else they are fitted). JCOSMO profiles usually carry it.
#' @param smiles Optional SMILES (stored for reference).
#' @param key Optional explicit storage key (upper-cased); defaults to \code{toupper(name)}.
#' @param s2 Optional common name shown by \code{\link{csp_compounds}} (defaults to \code{name}).
#' @param jcosmo_name Which profile to take when \code{profile} is a multi-entry TSV.
#' @param overwrite Overwrite an existing user compound with the same key (default \code{FALSE}).
#' @param quiet Suppress the fit-quality message (default \code{FALSE}).
#' @param ... Passed to \code{\link{csp_params_from_gcm}} (e.g. \code{fit}, \code{n}, \code{Tr_range}).
#' @return (Invisibly) the stored compound entry: the fitted \code{par}, \code{omega}, the input
#'   critical/fusion properties and the resolved \code{profile}.
#' @seealso \code{\link{csp_solubility}}, \code{\link{csp_user_compounds}},
#'   \code{\link{csp_remove_compound}}, \code{\link{csp_params_from_gcm}}, \code{\link{csp_load_typed}}.
#' @examples
#' \dontrun{
#' # From a JCOSMO ExportProf TSV, crit props from est_gcm(), fusion from the literature:
#' csp_add_compound("thymoquinone", profile = "thymoquinone_csp.tsv",
#'                  Tc = 562.6, Pc = 39.3, Vc = 470, Tb = 505,
#'                  Tm = 318.6, dHfus = 20000, vs = 150, smiles = "CC1=CC(=O)C(=CC1=O)C(C)C")
#' csp_solubility("thymoquinone", T = 323.15, P = c(80, 90, 100))
#' }
#' @export
csp_add_compound <- function(name, profile, Tc, Pc, Vc, Tb,
                             Tm = NA_real_, dHfus = NA_real_, vs = NA_real_, fusCp = NA_real_,
                             model = c("2021", "2019"), V = NULL, smiles = NULL,
                             key = NULL, s2 = NULL, jcosmo_name = NULL,
                             overwrite = FALSE, quiet = FALSE, ...) {
  model <- match.arg(model)
  if (missing(name) || !nzchar(name)) stop("'name' is required.", call. = FALSE)
  for (nm in c("Tc", "Pc", "Vc", "Tb")) {
    v <- get(nm)
    if (length(v) != 1L || !is.finite(v) || v <= 0)
      stop(sprintf("'%s' must be a positive finite scalar (from est_gcm() or experiment).", nm),
           call. = FALSE)
  }
  prof <- .csp_resolve_profile(profile, pick = .csp_or(jcosmo_name, name))
  if (!is.null(V) && length(V) == 1L && is.finite(V)) prof$V <- as.numeric(V)

  key <- toupper(.csp_or(key, name))
  bundled_keys <- names(csp_dataset()$profiles)
  store <- .csp_or(.csp_user_load(), list())
  if (key %in% names(store) && !isTRUE(overwrite))
    stop(sprintf("CSP user compound '%s' already exists; use overwrite = TRUE.", key), call. = FALSE)
  if (key %in% setdiff(bundled_keys, names(store)) && !isTRUE(overwrite))
    stop(sprintf("'%s' is a bundled CSP compound; use a different name or overwrite = TRUE.", key),
         call. = FALSE)

  fitres <- csp_params_from_gcm(prof, Tc, Pc, Vc, Tb, model = model, ...)
  par <- fitres$par

  entry <- list(key = key, s2 = .csp_or(s2, name), profile = prof,
                par19 = if (model == "2019") par else NULL,
                par21 = if (model == "2021") par else NULL,
                Tm = as.numeric(Tm), dHfus = as.numeric(dHfus), vs = as.numeric(vs),
                fusCp = as.numeric(fusCp), smiles = smiles,
                Tc = Tc, Pc = Pc, Vc = Vc, Tb = Tb, omega = fitres$omega,
                model = model, objective = fitres$objective, n_feasible = fitres$n_feasible)
  store[[key]] <- entry
  .csp_user_save(store)
  .csp_cache$data <- NULL                                # force csp_dataset() to re-merge

  if (!quiet) {
    have_fus <- all(is.finite(c(entry$Tm, entry$dHfus, entry$vs)))
    message(sprintf(
      "Added CSP compound '%s' (CSP %s): fit objective %.3g, %d/%d feasible Psat points%s.\n%s",
      key, model, fitres$objective, fitres$n_feasible, 6L,
      if (have_fus) "" else "; NO fusion data yet (set Tm/dHfus/vs before the fusion route)",
      "  Scale note: valid only if the profile matches the bundled JCOSMO CSP(2019) QM level."))
  }
  invisible(entry)
}

#' List user-added CSP compounds
#'
#' @return A data frame of the compounds added with \code{\link{csp_add_compound}} (key, name, model,
#'   fusion completeness, critical properties, fit objective), or \code{NULL} if none.
#' @seealso \code{\link{csp_add_compound}}, \code{\link{csp_remove_compound}}.
#' @export
csp_user_compounds <- function() {
  store <- .csp_user_load()
  if (is.null(store) || !length(store)) return(NULL)
  do.call(rbind, lapply(store, function(e) data.frame(
    key = e$key, name = .csp_or(e$s2, e$key), model = .csp_or(e$model, NA_character_),
    has_fusion = all(is.finite(c(e$Tm, e$dHfus, e$vs))),
    Tm = .csp_or(e$Tm, NA_real_), Tc = .csp_or(e$Tc, NA_real_), Pc = .csp_or(e$Pc, NA_real_),
    objective = .csp_or(e$objective, NA_real_),
    smiles = .csp_or(e$smiles, NA_character_), stringsAsFactors = FALSE)))
}

#' Remove a user-added CSP compound
#'
#' @param name Key or name of a compound previously added with \code{\link{csp_add_compound}}.
#' @return (Invisibly) \code{TRUE} if a compound was removed, \code{FALSE} if not found.
#' @seealso \code{\link{csp_add_compound}}, \code{\link{csp_user_compounds}}.
#' @export
csp_remove_compound <- function(name) {
  store <- .csp_user_load()
  if (is.null(store) || !length(store)) return(invisible(FALSE))
  key <- toupper(name)
  hit <- key %in% names(store)
  if (!hit) { hit2 <- which(vapply(store, function(e) identical(.csp_norm(.csp_or(e$s2, "")),
                                                                .csp_norm(name)), logical(1)))
    if (length(hit2)) { key <- names(store)[hit2[1]]; hit <- TRUE } }
  if (!hit) return(invisible(FALSE))
  store[[key]] <- NULL
  .csp_user_save(store)
  .csp_cache$data <- NULL
  invisible(TRUE)
}
