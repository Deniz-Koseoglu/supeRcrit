# =====================================================================================================
# LLE "extended solvent library" — the AMS-style auto-search pool (Tier 2).
#
# A vetted ~200-solvent set (authored by chemical class + unioned with the curated GSK lle_solvents(),
# deduped by InChIKey14, resolved to the csac UD database and enriched with Tm/Tb from exp_prop_db) that
# lets the LLE module auto-search a broad, sensible solvent space instead of only the hand-picked 66.
# Built by data-raw/build_lle_extended.py -> inst/shiny-app/config/lle_extended_solvents.rds.
# Columns: name, smiles, cls, note, gsk, source(curated|extended), inchikey, ud_name, Tm, Tb (K).
# =====================================================================================================

.LLE_EXT_CACHE <- new.env(parent = emptyenv())

.lle_ext_lib <- function() {
  if (is.null(.LLE_EXT_CACHE$lib)) {
    p <- "config/lle_extended_solvents.rds"
    if (!file.exists(p)) p <- system.file("shiny-app", "config", "lle_extended_solvents.rds", package = "supeRcrit")
    lib <- tryCatch(readRDS(p), error = function(e) NULL)
    if (!is.null(lib)) { lib$Tm <- suppressWarnings(as.numeric(lib$Tm)); lib$Tb <- suppressWarnings(as.numeric(lib$Tb)) }
    .LLE_EXT_CACHE$lib <- lib %||% data.frame()
  }
  .LLE_EXT_CACHE$lib
}

# Plausible extraction solvents that are LIQUID at T_C, returned as the identifiers the chosen engine/
# database actually resolves: csac+ud -> UD names; csac+chaos -> SMILES; ocrsphi/cs25 -> common names
# (the module's solvent_ids() then filters these against each engine's compound pool).
lle_extended_solvents <- function(T_C = 25, engine = "csac", db = "ud") {
  lib <- .lle_ext_lib(); if (is.null(lib) || !nrow(lib)) return(character(0))
  T_K <- T_C + 273.15
  liq <- (is.na(lib$Tm) | lib$Tm < T_K) & (is.na(lib$Tb) | lib$Tb > T_K)   # NA-permissive liquid gate
  lib <- lib[liq, , drop = FALSE]
  ids <- if (identical(engine, "ocrsphi") || identical(engine, "cs25")) lib$name
         else if (identical(db, "chaos")) lib$smiles
         else lib$ud_name                                                    # csac + ud (default)
  unique(ids[!is.na(ids) & nzchar(ids)])
}

# Candidate count for a given engine/db/T (used for the "Auto-searching N solvents" UI note).
lle_extended_count <- function(T_C = 25, engine = "csac", db = "ud")
  length(lle_extended_solvents(T_C, engine, db))
