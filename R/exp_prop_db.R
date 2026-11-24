#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EXPERIMENTAL pure-component property lookup
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Looks a compound up in the bundled experimental database (inst/extdata/exp_property_db.rds,
# built by data-raw/exp_property_db/build_exp_property_db.R from Joback/Poling + OPERA/PHYSPROP +
# Hansen Handbook). Used by the app to PREFER a measured value over a GCM estimate when available.
# Matching is by a stereo-flattened CANONICAL SMILES, computed the SAME way the DB was keyed.

# cached DB loader (read once per session)
.exp_db <- local({
  cache <- NULL; loaded <- FALSE
  function() {
    if (!loaded) {
      f <- system.file("extdata", "properties", "exp_property_db.rds", package = "supeRcrit")
      cache <<- if (nzchar(f) && file.exists(f)) tryCatch(readRDS(f), error = function(e) NULL) else NULL
      loaded <<- TRUE
    }
    cache
  }
})

# stereo-flattened canonical SMILES key (must match build_exp_property_db.R::canon_key)
#' @noRd
.exp_canon_key <- function(smi) {
  if (length(smi) != 1 || is.na(smi) || !nzchar(smi)) return(NA_character_)
  tryCatch({
    m <- rcdk::parse.smiles(as.character(smi))[[1]]
    if (is.null(m)) return(NA_character_)
    rcdk::get.smiles(m, rcdk::smiles.flavors(c("Canonical")))
  }, error = function(e) NA_character_)
}

#' @title Look up experimental pure-component properties for a compound
#' @description Matches a compound (by stereo-flattened canonical SMILES) against the bundled
#'   experimental property database and returns the measured values with their data source. Lets
#'   callers prefer a real experimental value over a group-contribution estimate. Properties:
#'   normal boiling point \code{Tb} (K), critical \code{Tc}/\code{Pc}/\code{Vc}, and Hansen
#'   \code{dD}/\code{dP}/\code{dHB}. Any property absent for the compound is \code{NA}.
#' @param solute A SMILES string, or a \code{\link{mol_find}} solute object (its \code{IDs["SMILES"]}
#'   is used).
#' @return A one-row \code{data.frame} (columns \code{name, cas, smiles, inchikey, Tb, Tc, Pc, Vc,
#'   dD, dP, dHB} plus per-property provenance \code{Tb_src}/\code{crit_src}/\code{hsp_src}), or
#'   \code{NULL} when the compound is not in the database.
#' @seealso \code{\link{mol_find}}
#' @export
exp_prop_lookup <- function(solute) {
  smi <- NULL
  if (is.character(solute) && length(solute) == 1) {
    smi <- solute
  } else if (is.list(solute)) {
    ids <- solute[["IDs"]]
    smi <- if (!is.null(ids) && "SMILES" %in% names(ids)) unname(ids[["SMILES"]]) else solute[["SMILES"]]
  }
  if (is.null(smi) || length(smi) != 1 || is.na(smi) || !nzchar(smi)) return(NULL)
  db <- .exp_db(); if (is.null(db)) return(NULL)
  key <- .exp_canon_key(smi); if (is.na(key)) return(NULL)
  i <- match(key, db$key); if (is.na(i)) return(NULL)
  db[i, , drop = FALSE]
}

#' @title The bundled experimental pure-component property database
#' @description Returns the whole experimental property table (one row per compound) backing
#'   \code{\link{exp_prop_lookup}} and the app's "Load from Database" browser. Columns: \code{name},
#'   \code{cas}, \code{smiles}, \code{inchikey}, \code{Tb}/\code{Tc}/\code{Pc}/\code{Vc},
#'   \code{dD}/\code{dP}/\code{dHB}, and per-property provenance \code{*_src}.
#' @return A \code{data.frame}, or \code{NULL} if the bundled database is unavailable.
#' @export
exp_prop_db <- function() .exp_db()

#' @title Canonical match key for the experimental property database
#' @description The stereo-flattened canonical SMILES used to key the experimental property database.
#'   Exposed so the app can match user-entered SMILES against DB records (including user-added ones).
#' @param smiles A SMILES string.
#' @return The canonical key (character), or \code{NA} when it cannot be parsed.
#' @export
exp_canon_key <- function(smiles) .exp_canon_key(smiles)
