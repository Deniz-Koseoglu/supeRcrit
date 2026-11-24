#' GCM-style melting-point predictor from SMILES (machine-learned)
#'
#' A self-contained melting-point estimator: from a SMILES alone it computes lightweight
#' structural features (element counts, bond-type counts, ring/double/triple-bond counts,
#' molecular weight, XLogP, TPSA, hydrogen count) plus, when the compound is in the bundled CHAOS
#' database, its COSMO sigma-profile (51-bin) and QM descriptors (dipole, polarizability, HOMO-LUMO
#' gap, dispersion energy, ...), and applies a gradient-boosted-tree model (xgboost) trained on the
#' open Jean-Claude Bradley melting-point dataset (~24k compounds). Adding the CHAOS sigma-profile
#' features improved held-out accuracy from RMSE 42.2 -> 39.6 K (MAE 30.5 -> 27.2 K), most strongly
#' for polar / hydrogen-bonding compounds. Still works on SMILES alone: the sigma-profile is looked
#' up by canonical SMILES and left missing (xgboost handles it) for compounds outside CHAOS.
#' Complements \code{gcm_bp} / \code{gcm_crit} in the Solute Characterization workflow;
#' reimplements the Guillaume2126 predictor approach in native R (no Python dependency).
#'
#' @name gcm_mp
#' @importFrom xgboost xgb.load.raw
#' @keywords internal
NULL

.mp_elems <- c("C", "N", "O", "S", "P", "F", "Cl", "Br", "I", "B", "Si")

.mp_bond_order <- function(b)
  rJava::.jcall(rJava::.jcall(b, "Lorg/openscience/cdk/interfaces/IBond$Order;", "getOrder"),
                "S", "toString")
.mp_bond_atoms <- function(b)
  c(rcdk::get.symbol(rJava::.jcall(b, "Lorg/openscience/cdk/interfaces/IAtom;", "getBegin")),
    rcdk::get.symbol(rJava::.jcall(b, "Lorg/openscience/cdk/interfaces/IAtom;", "getEnd")))

# Compute the structural feature set for one SMILES; returns list(base = named numeric, pairs =
# table of bond-type counts) or NULL if the SMILES does not parse. Must stay in lock-step with
# scratch_tq/mp_feat.R (the trainer) so model columns line up.
.mp_feat <- function(smi) {
  m <- tryCatch(suppressWarnings(rcdk::parse.smiles(smi)[[1]]), error = function(e) NULL)
  if (is.null(m)) return(NULL)
  tryCatch({
    atoms <- rcdk::get.atoms(m); syms <- vapply(atoms, rcdk::get.symbol, character(1))
    bonds <- rcdk::get.bonds(m)
    ords  <- if (length(bonds)) vapply(bonds, .mp_bond_order, character(1)) else character(0)
    pairs <- if (length(bonds))
      vapply(bonds, function(b) { a <- .mp_bond_atoms(b); o <- .mp_bond_order(b)
        paste0(paste(sort(a), collapse = ""), substr(o, 1, 1)) }, character(1)) else character(0)
    el <- table(factor(syms, levels = .mp_elems))
    base <- c(setNames(as.integer(el), paste0("n", .mp_elems)),
              nHeavy = length(atoms), nElem = length(unique(syms)), nBond = length(bonds),
              nDouble = sum(ords == "DOUBLE"), nTriple = sum(ords == "TRIPLE"),
              nRing = length(bonds) - length(atoms) + 1L,
              nH = tryCatch(rcdk::get.total.hydrogen.count(m), error = function(e) NA_real_),
              MW = rcdk::get.exact.mass(m),
              XLogP = suppressWarnings(tryCatch(rcdk::get.xlogp(m), error = function(e) NA_real_)),
              TPSA = tryCatch(rcdk::get.tpsa(m), error = function(e) NA_real_))
    list(base = base, pairs = table(pairs))
  }, error = function(e) NULL)
}

.mp_cache <- new.env(parent = emptyenv())
.mp_model <- function() {
  if (is.null(.mp_cache$m)) {
    f <- system.file("extdata", "models", "mp_model.rds", package = "supeRcrit")
    if (!nzchar(f)) stop("mp_model.rds not found; reinstall supeRcrit.", call. = FALSE)
    m <- readRDS(f)
    # xgboost boosters serialize as raw bytes (external pointers don't survive saveRDS) -> rebuild
    if (m$type == "xgboost" && is.null(m$fit)) m$fit <- xgboost::xgb.load.raw(m$raw)
    .mp_cache$m <- m
  }
  .mp_cache$m
}

# CHAOS sigma-profile + QM lookup table (canonical-SMILES -> features), cached. Returns NULL if
# the model does not use CHAOS features.
.mp_chaos <- function() {
  if (is.null(.mp_cache$ch)) {
    f <- system.file("extdata", "solubility", "chaos_lookup.rds", package = "supeRcrit")
    .mp_cache$ch <- if (nzchar(f)) readRDS(f) else NA
  }
  if (identical(.mp_cache$ch, NA)) NULL else .mp_cache$ch
}

.mp_canon <- function(smi) {
  m <- tryCatch(suppressWarnings(rcdk::parse.smiles(smi)[[1]]), error = function(e) NULL)
  if (is.null(m)) return(NA_character_)
  tryCatch(rcdk::get.smiles(m, rcdk::smiles.flavors(c("Canonical"))), error = function(e) NA_character_)
}

# Assemble a 1-row feature matrix (cols = model$cols) from a .mp_feat() result. Base + bond-pair
# columns are 0-filled; CHAOS sigma-profile columns are filled from `chaosvec` (a named vector from
# the lookup) and left as NA when the compound is not in CHAOS (xgboost handles missing natively).
.mp_row <- function(f, chaosvec, model) {
  row <- setNames(rep(NA_real_, length(model$cols)), model$cols)
  bn <- intersect(names(f$base), model$cols); row[bn] <- as.numeric(f$base[bn])
  if (length(f$pairs)) {
    pk <- paste0("p_", names(f$pairs)); keep <- pk %in% model$cols
    if (any(keep)) row[pk[keep]] <- as.numeric(f$pairs[keep])
  }
  bp <- setdiff(model$cols, model$chaos_cols)            # base + pairs -> default 0
  row[bp][!is.finite(row[bp])] <- 0
  if (isTRUE(model$uses_chaos) && !is.null(chaosvec)) row[model$chaos_cols] <- chaosvec
  matrix(row, nrow = 1, dimnames = list(NULL, model$cols))
}

# Dispatch prediction to the chosen learner (random forest / ranger / xgboost / lightgbm).
.mp_predict <- function(model, Xrow) {
  switch(model$type,
    randomForest = as.numeric(stats::predict(model$fit, Xrow)),
    ranger       = as.numeric(stats::predict(model$fit, data = Xrow)$predictions),
    xgboost      = as.numeric(stats::predict(model$fit, Xrow)),
    lightgbm     = as.numeric(stats::predict(model$fit, Xrow)),
    stop("unknown model type: ", model$type))
}

# Shared SMILES -> value pipeline used by both gcm_mp and gcm_hvap: structural features (rcdk) +
# CHAOS sigma-profile lookup (NA when absent) -> the model's predict.
.gcm_ml_predict <- function(model, smiles) {
  CHL <- if (isTRUE(model$uses_chaos)) .mp_chaos() else NULL
  vapply(smiles, function(s) {
    f <- .mp_feat(s); if (is.null(f)) return(NA_real_)
    cv <- NULL
    if (!is.null(CHL)) {
      i <- match(.mp_canon(s), rownames(CHL$mat))
      cv <- if (is.na(i)) rep(NA_real_, length(CHL$cols)) else as.numeric(CHL$mat[i, ])
    }
    .mp_predict(model, .mp_row(f, cv, model))
  }, numeric(1), USE.NAMES = FALSE)
}

.hvap_model <- function() {
  if (is.null(.mp_cache$hv)) {
    f <- system.file("extdata", "models", "hvap_model.rds", package = "supeRcrit")
    if (!nzchar(f)) stop("hvap_model.rds not found; reinstall supeRcrit.", call. = FALSE)
    m <- readRDS(f)
    if (m$type == "xgboost" && is.null(m$fit)) m$fit <- xgboost::xgb.load.raw(m$raw)
    .mp_cache$hv <- m
  }
  .mp_cache$hv
}

#' Estimate melting point from a SMILES string
#'
#' @param smiles character vector of SMILES.
#' @param units \code{"K"} (default) or \code{"C"}.
#' @return numeric vector of predicted melting points (NA where the SMILES does not parse). The
#'   underlying model's held-out accuracy is reported in \code{attr(., "model")}.
#' @examples
#' \dontrun{
#' gcm_mp("c1ccccc1C(=O)O")            # benzoic acid ~ 395 K
#' gcm_mp("CC(=O)Nc1ccc(O)cc1", "C")   # paracetamol ~ 170 C
#' }
#' @seealso \code{gcm_bp}, \code{gcm_crit}, \code{\link{est_gcm}}.
#' @export
gcm_mp <- function(smiles, units = c("K", "C")) {
  units <- match.arg(units)
  model <- .mp_model()
  out <- .gcm_ml_predict(model, smiles)
  if (units == "C") out <- out - 273.15
  attr(out, "model") <- sprintf("%s | %s (n=%d, test RMSE %.0f K, MAE %.0f K, R2 %.2f)",
                                model$type, model$source, model$n_train, model$rmse_K, model$mae_K, model$r2)
  out
}

#' Estimate enthalpy of vaporization from a SMILES string
#'
#' Machine-learned ML estimator of the standard enthalpy of vaporization, built with the same native-R
#' pipeline as \code{\link{gcm_mp}}: structural features (rcdk) plus, when the compound is in the bundled
#' CHAOS database, its COSMO sigma-profile + QM descriptors (which relate directly to cohesive energy),
#' applied through a gradient-boosted-tree model (xgboost; held-out R^2 ~ 0.95, RMSE ~ 6 kJ/mol). Part of
#' the Solute Characterization workflow alongside \code{gcm_bp}, \code{gcm_crit} and
#' \code{\link{gcm_mp}}. Works on SMILES alone.
#'
#' @param smiles character vector of SMILES.
#' @return numeric vector of predicted enthalpies of vaporization (kJ/mol; NA where the SMILES does not
#'   parse). The underlying model's held-out accuracy is in \code{attr(., "model")}.
#' @examples
#' \dontrun{
#' gcm_hvap("c1ccccc1")        # benzene ~ 33 kJ/mol
#' gcm_hvap("CCO")             # ethanol ~ 42 kJ/mol
#' }
#' @seealso \code{\link{gcm_mp}}, \code{gcm_bp}, \code{gcm_crit}.
#' @export
gcm_hvap <- function(smiles) {
  model <- .hvap_model()
  out <- .gcm_ml_predict(model, smiles)
  attr(out, "model") <- sprintf("%s | %s (n=%d, test RMSE %.1f kJ/mol, R2 %.2f)",
                                model$type, model$source, model$n_train, model$rmse, model$r2)
  out
}
