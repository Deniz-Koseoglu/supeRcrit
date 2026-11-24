# COSMO-SAC-dsp (Hsieh-Lin-Vrabec 2014) wrappers: activity coefficients + LLE solvent ranking.
# Engine: src/csac_core.cpp (csac_lngamma_cpp), validated bit-exact vs NIST cCOSMO.
# Profiles: two self-consistent databases (never mix in one ranking; different QM):
#   "ud"    bundled UD sigma3 DMol3 database (2259 compounds; csac_profiles.rds), named/CAS solvents.
#   "chaos" Gond et al. 2026 omegaB97X-D/def2-TZVP database (~53k; csac_chaos_profiles.rds), SMILES-keyed,
#           σ-profiles per the COSMO-SAC-dsp protocol (Bell 2020). Broad coverage incl. designer/heavy solutes.

.csac_cache <- new.env(parent = emptyenv())

#' Load a bundled COSMO-SAC-dsp profile database (cached)
#'
#' @param database \code{"ud"} (2259 DMol3 compounds, default) or \code{"chaos"} (~53k omegaB97X-D
#'   compounds, Gond et al. 2026). The two use different quantum-chemical levels and must not be mixed
#'   within a single ranking.
#' @return list with \code{psA} (n x 153 area-per-bin matrix, NHB|OH|OT) and \code{meta}
#'   (data frame: key, name, cas, smiles, area, vol, ekB, dclass).
#' @seealso \code{\link{csac_compounds}}, \code{\link{csac_gamma}}, \code{\link{csac_lle}}.
#' @export
csac_dataset <- function(database = c("ud", "chaos")) {
  database <- match.arg(database)
  if (is.null(.csac_cache[[database]])) {
    fn <- if (database == "chaos") "csac_chaos_profiles.rds" else "csac_profiles.rds"
    f <- system.file("extdata", "solubility", fn, package = "supeRcrit")
    if (!nzchar(f)) stop(sprintf("%s not found; reinstall supeRcrit.", fn), call. = FALSE)
    dat <- readRDS(f)
    # CHAOS meta is SMILES-keyed without common names/CAS; fill so the resolver/UI work uniformly.
    if (is.null(dat$meta$name)) dat$meta$name <- dat$meta$smiles
    if (is.null(dat$meta$cas))  dat$meta$cas  <- NA_character_
    # CHAOS enrichment: overlay real common name + CAS for the ~6.8k compounds resolvable by InChIKey14
    # against our named assets (UD + master registry). CHAOS itself ships no names/CAS (Gond et al. 2026),
    # so the unresolved majority keep their SMILES as the display name. Enables name/CAS lookup + selection.
    if (identical(database, "chaos")) {
      nf <- system.file("extdata", "solubility", "csac_chaos_names.rds", package = "supeRcrit")
      if (nzchar(nf)) {
        nm <- tryCatch(readRDS(nf), error = function(e) NULL)
        if (!is.null(nm) && nrow(nm)) {
          mi <- match(as.character(dat$meta$key), as.character(nm$key)); hit <- !is.na(mi)
          dat$meta$name[hit] <- nm$name[mi[hit]]
          dat$meta$cas[hit]  <- nm$cas[mi[hit]]
        }
      }
    }
    .csac_cache[[database]] <- dat
  }
  .csac_cache[[database]]
}

.csac_norm <- function(x) gsub("[^a-z0-9]", "", tolower(x))

# Resolve a compound to its COSMO-SAC-dsp inputs: from the database if present, else (when
# predict = TRUE, i.e. the CHAOS-scale database) a FastSigma structure->profile prediction.
.csac_resolve <- function(compound, d, predict = FALSE) {
  i <- .csac_match(compound, d)
  if (!is.null(i)) return(list(psA = d$psA[i, ], area = d$meta$area[i], vol = d$meta$vol[i],
                               ekB = d$meta$ekB[i], dclass = d$meta$dclass[i], predicted = FALSE))
  if (isTRUE(predict)) {
    p <- tryCatch(fastsigma_profile(compound), error = function(e) NULL)
    if (!is.null(p)) { p$predicted <- TRUE; return(p) }
  }
  NULL
}

# Resolve a compound string to a row index of d$meta (InChIKey, name, CAS, then exact SMILES).
.csac_match <- function(compound, d) {
  hit <- which(d$meta$key == compound); if (length(hit)) return(hit[1])
  q <- .csac_norm(compound)
  hit <- which(.csac_norm(d$meta$name) == q); if (length(hit)) return(hit[1])
  hit <- which(d$meta$cas == compound); if (length(hit)) return(hit[1])
  hit <- which(d$meta$smiles == compound); if (length(hit)) return(hit[1])
  NULL
}

#' Compounds available for the COSMO-SAC-dsp tools
#'
#' @return data frame of bundled compounds (InChIKey, name, CAS, SMILES, cavity area/volume,
#'   dispersion e/kB and class). These are the candidate solvents/solutes for \code{\link{csac_lle}}.
#' @param database \code{"ud"} (default) or \code{"chaos"}; see \code{\link{csac_dataset}}.
#' @seealso \code{\link{csac_gamma}}, \code{\link{csac_lle}}.
#' @export
csac_compounds <- function(database = c("ud", "chaos")) {
  d <- csac_dataset(match.arg(database))
  out <- d$meta[order(d$meta$name), c("key", "name", "cas", "smiles", "area", "vol", "ekB", "dclass")]
  rownames(out) <- NULL
  out
}

#' Infinite-dilution activity coefficient by COSMO-SAC-dsp
#'
#' Computes \eqn{\gamma^\infty} of a dilute solute in a liquid solvent from the native COSMO-SAC-dsp
#' engine (2010 NHB/OH/OT hydrogen-bond split plus the 2014 dispersion term), using the bundled UD
#' DMol3 sigma profiles. This is the key quantity for liquid-liquid extraction solvent selection: a
#' solute partitions toward the phase where its activity coefficient is lower, so the distribution
#' coefficient between two solvents is the ratio of the two \eqn{\gamma^\infty} (see \code{\link{csac_lle}}).
#'
#' @param solute,solvent compound names (InChIKey, common name, CAS or SMILES; see
#'   \code{\link{csac_compounds}}).
#' @param T temperature(s) (K; default 298.15).
#' @param dsp include the dispersion contribution (default \code{TRUE}); automatically skipped for a
#'   pair where either compound lacks a dispersion \code{e/kB}.
#' @param database \code{"ud"} (default) or \code{"chaos"}; see \code{\link{csac_dataset}}. Both
#'   compounds are resolved within the same database.
#' @param predict if a compound is absent from the database, estimate its profile from SMILES with
#'   \code{\link{fastsigma_profile}} (FastSigma). FastSigma is trained on CHAOS, so this defaults to
#'   \code{TRUE} for \code{database = "chaos"} and \code{FALSE} for \code{"ud"} (scale-mismatched).
#' @return numeric vector of \eqn{\gamma^\infty} (one per \code{T}); \code{NA} for an unknown compound.
#' @examples
#' \dontrun{
#' csac_gamma("ETHANOL", "WATER")        # moderate positive deviation
#' csac_gamma("N-HEXANE", "WATER")       # very large (near-immiscible)
#' }
#' @seealso \code{\link{csac_lle}}, \code{\link{csac_compounds}}, \code{\link{csp_gamma}}.
#' @export
csac_gamma <- function(solute, solvent, T = 298.15, dsp = TRUE, database = c("ud", "chaos"),
                       predict = NULL) {
  database <- match.arg(database); d <- csac_dataset(database)
  if (is.null(predict)) predict <- identical(database, "chaos")
  S <- .csac_resolve(solute, d, predict); V <- .csac_resolve(solvent, d, predict)
  if (is.null(S)) stop(sprintf("Unknown solute '%s' (not in the %s database%s).", solute, database,
                               if (predict) "; FastSigma could not parse it" else ""), call. = FALSE)
  if (is.null(V)) stop(sprintf("Unknown solvent '%s' (not in the %s database%s).", solvent, database,
                               if (predict) "; FastSigma could not parse it" else ""), call. = FALSE)
  psA  <- rbind(S$psA, V$psA)
  area <- c(S$area, V$area)
  vol  <- c(S$vol,  V$vol)
  ekB  <- c(S$ekB,  V$ekB)
  dcl  <- as.integer(c(S$dclass, V$dclass))
  vapply(T, function(Tk) {
    lng <- tryCatch(csac_lngamma_cpp(psA, area, vol, c(0, 1), Tk, ekB, dcl, isTRUE(dsp)),
                    error = function(e) NA_real_)
    if (length(lng) < 1 || anyNA(lng[1])) NA_real_ else exp(lng[1])
  }, numeric(1))
}

#' Rank candidate solvents for liquid-liquid extraction (COSMO-SAC-dsp)
#'
#' Screens candidate extraction solvents for pulling a target solute out of a feed/diluent phase. For
#' each candidate the distribution coefficient is
#' \eqn{D = \gamma^\infty_{feed} / \gamma^\infty_{solvent}} (the solute prefers the phase where its
#' activity coefficient is lower), from the native COSMO-SAC-dsp engine. A second solute
#' (\code{coextract}) optionally yields the separation selectivity \eqn{S = D_{solute}/D_{coextract}};
#' the solvent capacity is \eqn{1/\gamma^\infty_{solvent}}. This is the broad-coverage analog of
#' \code{\link{csp_lle}} (2259 bundled compounds vs 256 for CSP).
#'
#' @param solute target solute name (see \code{\link{csac_compounds}}).
#' @param solvents character vector of candidate solvent names (default: all bundled compounds; a full
#'   screen of ~2259 takes tens of seconds).
#' @param feed feed/diluent solvent the solute starts in (default \code{"WATER"}).
#' @param T temperature (K; default 298.15).
#' @param coextract optional second solute to compute extraction selectivity against.
#' @param dsp include the dispersion contribution (default \code{TRUE}).
#' @param database \code{"ud"} (default, 2259 named compounds) or \code{"chaos"} (~53k, SMILES-keyed;
#'   see \code{\link{csac_dataset}}). For \code{"chaos"} always pass a candidate \code{solvents} list:
#'   a full 53k screen is impractical.
#' @return data frame ordered by descending \code{D}, with columns \code{solvent}, \code{gamma_feed},
#'   \code{gamma_solvent}, \code{D}, \code{capacity}, and (with \code{coextract}) \code{selectivity}.
#' @examples
#' \dontrun{
#' csac_lle("NAPHTHALENE", feed = "WATER")
#' csac_lle("ETHANOL", solvents = c("N-HEXANE","TOLUENE","ETHYL_ACETATE"), feed = "WATER")
#' }
#' @seealso \code{\link{csac_gamma}}, \code{\link{csac_compounds}}, \code{\link{csp_lle}}.
#' @export
csac_lle <- function(solute, solvents = NULL, feed = "WATER", T = 298.15,
                     coextract = NULL, dsp = TRUE, database = c("ud", "chaos"), predict = NULL) {
  database <- match.arg(database)
  d <- csac_dataset(database)
  if (is.null(predict)) predict <- identical(database, "chaos")
  if (is.null(solvents)) solvents <- d$meta$name
  g_feed <- csac_gamma(solute, feed, T, dsp, database, predict)[1]
  if (!isTRUE(is.finite(g_feed)))
    stop(sprintf("Could not compute gamma for '%s' in feed '%s'.", solute, feed), call. = FALSE)
  g_co_feed <- if (!is.null(coextract)) csac_gamma(coextract, feed, T, dsp, database, predict)[1] else NA_real_
  fnorm <- .csac_norm(feed)
  rows <- lapply(solvents, function(sv) {
    if (.csac_norm(sv) == fnorm) return(NULL)
    gs <- suppressWarnings(tryCatch(csac_gamma(solute, sv, T, dsp, database, predict)[1], error = function(e) NA_real_))
    if (!isTRUE(is.finite(gs)) || gs <= 0) return(NULL)
    out <- data.frame(solvent = sv, gamma_feed = g_feed, gamma_solvent = gs,
                      D = g_feed / gs, capacity = 1 / gs, stringsAsFactors = FALSE)
    if (!is.null(coextract) && isTRUE(is.finite(g_co_feed))) {
      gco <- suppressWarnings(tryCatch(csac_gamma(coextract, sv, T, dsp, database, predict)[1], error = function(e) NA_real_))
      out$selectivity <- if (isTRUE(is.finite(gco)) && gco > 0) (g_feed / gs) / (g_co_feed / gco) else NA_real_
    }
    out
  })
  res <- do.call(rbind, Filter(Negate(is.null), rows))
  if (is.null(res) || !nrow(res)) return(res)
  res[order(-res$D), , drop = FALSE]
}
