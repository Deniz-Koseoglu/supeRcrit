#' Native-R machine-learned scCO2 (+cosolvent) solubility predictor
#'
#' Predicts the equilibrium solubility (solute mole fraction \eqn{y_2}, and its \eqn{\log_{10}}) of a
#' solid solute in supercritical CO2, optionally with a liquid co-solvent, from the solute SMILES and
#' operating temperature/pressure alone. It reimplements Makarov's "thermodynamics-informed ML" approach
#' with a fully native-R feature pipeline (no Python): the full rcdk/CDK 2D molecular-descriptor set for
#' the solute (\code{\link[rcdk]{eval.desc}}), native Abraham LSER solute descriptors (E, S, A, B, V; see
#' \code{\link{abraham_params}}), and the solute melting point (supplied, or estimated with
#' \code{\link{gcm_mp}}). For a co-solvent, its structural features and Abraham solvent coefficients are
#' mole-fraction-weighted and pooled with the pure-CO2 training data (the pure case is the co-solvent-zero
#' limit). The model is trained on the Makarov scCO2 dataset augmented with the García-Sámano Chrastil
#' database (experimental coefficients + experimental melting points). Held-out leave-solute-out accuracy
#' is RMSE ~0.40-0.56 in \eqn{\log_{10} y} (R^2 ~0.78-0.91, split-dependent), matching or exceeding the
#' published model. The enthalpy of vaporization is deliberately omitted (a predicted value injects net noise).
#'
#' @name predict_solubility
#' @importFrom xgboost xgb.load.raw
NULL

.sol_cache <- new.env(parent = emptyenv())

.abraham_model <- function() {
  if (is.null(.sol_cache$abra)) {
    f <- system.file("extdata", "models", "abraham_model.rds", package = "supeRcrit")
    if (!nzchar(f)) stop("abraham_model.rds not found; reinstall supeRcrit.", call. = FALSE)
    m <- readRDS(f)
    for (tg in m$targets) if (is.null(m$models[[tg]]$fit)) m$models[[tg]]$fit <- xgboost::xgb.load.raw(m$models[[tg]]$raw)
    .sol_cache$abra <- m
  }
  .sol_cache$abra
}

.sol_model <- function() {
  if (is.null(.sol_cache$sol)) {
    f <- system.file("extdata", "models", "sol_model.rds", package = "supeRcrit")
    if (!nzchar(f)) stop("sol_model.rds not found; reinstall supeRcrit.", call. = FALSE)
    m <- readRDS(f)
    if (is.null(m$fit)) m$fit <- xgboost::xgb.load.raw(m$raw)
    .sol_cache$sol <- m
  }
  .sol_cache$sol
}

# 1-row feature matrix (base + p_<pair> filled, 0 elsewhere; no CHAOS) from a .mp_feat() result.
.feat_row0 <- function(f, cols) {
  row <- stats::setNames(rep(0, length(cols)), cols)
  bn <- intersect(names(f$base), cols); row[bn] <- as.numeric(f$base[bn])
  if (length(f$pairs)) {
    pk <- paste0("p_", names(f$pairs)); keep <- pk %in% cols
    if (any(keep)) row[pk[keep]] <- as.numeric(f$pairs[keep])
  }
  row[!is.finite(row)] <- 0
  matrix(row, nrow = 1, dimnames = list(NULL, cols))
}

# Full rcdk/CDK 2D descriptor row for one SMILES, aligned to the model's rcdk_cols. Columns the molecule
# cannot produce (or non-finite/huge values) are imputed with the stored training medians, so prediction
# matches how the model was trained. Used by the descriptor-based solubility model.
.rcdk_dcls <- function() unlist(lapply(c("constitutional", "topological", "electronic", "hybrid"),
                                       rcdk::get.desc.names))
.rcdk_solute <- function(smi, cols, medians) {
  row <- as.numeric(medians[cols]); names(row) <- cols          # default every column to its training median
  mol <- tryCatch(suppressWarnings(rcdk::parse.smiles(smi)[[1]]), error = function(e) NULL)
  if (!is.null(mol)) {
    d <- tryCatch(suppressWarnings(rcdk::eval.desc(mol, .rcdk_dcls(), verbose = FALSE)), error = function(e) NULL)
    if (!is.null(d) && nrow(d) >= 1) {
      dv <- as.numeric(d[1, ]); names(dv) <- colnames(d)
      common <- intersect(cols, names(dv)); val <- dv[common]
      val[!is.finite(val) | abs(val) > 1e12] <- NA
      ok <- !is.na(val); if (any(ok)) row[common[ok]] <- val[ok]  # overwrite only computable columns
    }
  }
  matrix(row, nrow = 1, dimnames = list(NULL, cols))
}

# Native Abraham E,S,A,B,V for one SMILES (NA vector if the SMILES does not parse).
.abraham_one <- function(smi) {
  m <- .abraham_model(); f <- .mp_feat(smi)
  if (is.null(f)) return(stats::setNames(rep(NA_real_, length(m$targets)), m$targets))
  X <- .feat_row0(f, m$cols)
  stats::setNames(vapply(m$targets, function(tg) as.numeric(stats::predict(m$models[[tg]]$fit, X)), numeric(1)),
                  m$targets)
}

#' Estimate Abraham LSER solute descriptors from SMILES
#'
#' Native-R predictor of the five Abraham solute descriptors — excess molar refraction \eqn{E},
#' dipolarity/polarizability \eqn{S}, hydrogen-bond acidity \eqn{A} and basicity \eqn{B}, and McGowan
#' characteristic volume \eqn{V} — from a SMILES string. Gradient-boosted-tree models (xgboost) trained
#' on the open AbraLLaMA dataset using rcdk structural features; held-out R^2 are 0.98 (E), 0.94 (S),
#' 0.88 (A), 0.96 (B), 1.00 (V). Used internally by \code{\link{predict_solubility}}.
#'
#' @param smiles character vector of SMILES.
#' @return a numeric matrix with one row per input and columns \code{E,S,A,B,V} (NA where the SMILES
#'   does not parse).
#' @examples
#' \dontrun{
#' abraham_params(c("c1ccccc1", "CCO"))
#' }
#' @seealso \code{\link{predict_solubility}}.
#' @export
abraham_params <- function(smiles) {
  m <- t(vapply(smiles, .abraham_one, numeric(5)))
  colnames(m) <- .abraham_model()$targets
  rownames(m) <- NULL
  m
}

#' Predict scCO2 (+cosolvent) solubility from SMILES
#'
#' @param solute SMILES string of the solute (single compound).
#' @param temp temperature in Kelvin (scalar or vector; recycled with \code{pres}).
#' @param pres pressure in bar (scalar or vector).
#' @param cosolvent optional SMILES of a liquid co-solvent; \code{NULL} for pure CO2. May be a
#'   \strong{vector} of SMILES to model a co-solvent \strong{blend} (mole-fraction feature superposition;
#'   the model is trained on single co-solvents, so treat blend results as indicative — reliable for
#'   ranking, approximate in absolute value).
#' @param x co-solvent mole fraction(s) (0–1), matched to \code{cosolvent} (a scalar is recycled). For a
#'   blend these are the per-component mole fractions; the total co-solvent fraction is their sum and
#'   should stay within the model domain (~0.1). Ignored when \code{cosolvent} is \code{NULL}.
#' @param Tm optional solute melting point in Kelvin. When omitted it is estimated with
#'   \code{\link{gcm_mp}}; supplying an experimental value improves accuracy.
#' @return a \code{data.frame} with columns \code{temp_K}, \code{pres_bar}, \code{log10_y} (predicted
#'   \eqn{\log_{10}} of the solute mole fraction) and \code{mole_frac} (the solubility \eqn{y_2}).
#'   \code{attr(., "Tm")} records the melting point used and \code{attr(., "abraham")} the solute
#'   Abraham descriptors.
#' @examples
#' \dontrun{
#' predict_solubility("O=C1C=CC(=O)C(=C1)C(C)C", temp = 313, pres = c(150, 250))  # thymoquinone-like
#' predict_solubility("CC(=O)Nc1ccc(O)cc1", 318, 200, cosolvent = "CO", x = 0.05) # +5% methanol
#' }
#' @seealso \code{\link{abraham_params}}, \code{\link{gcm_mp}}, \code{\link{chrastil_solubility}}.
#' @export
predict_solubility <- function(solute, temp, pres, cosolvent = NULL, x = 0, Tm = NULL) {
  if (length(solute) != 1L || is.na(solute) || !nzchar(solute))
    stop("`solute` must be a single non-empty SMILES string.", call. = FALSE)
  m <- .sol_model()
  n <- max(length(temp), length(pres))
  temp <- rep_len(as.numeric(temp), n); pres <- rep_len(as.numeric(pres), n)

  f_sol <- .mp_feat(solute)
  if (is.null(f_sol)) stop("Could not parse solute SMILES: ", solute, call. = FALSE)
  # Solute structural block: full rcdk/CDK descriptors (current model) or legacy lean bond-pairs.
  solblk <- if (!is.null(m$rcdk_cols)) .rcdk_solute(solute, m$rcdk_cols, m$rcdk_medians)[1, ]
            else .feat_row0(f_sol, m$sol_cols)[1, ]
  abra <- .abraham_one(solute)
  if (is.null(Tm) || !is.finite(Tm)) Tm <- suppressWarnings(gcm_mp(solute)[1])

  # Co-solvent block: structural base features + Abraham e/a coefficients, mole-fraction-weighted. A
  # BLEND is handled by superposition — each co-solvent's feature block is scaled by its own mole
  # fraction and the blocks are summed (cs_vec = sum_k features_k * x_k, x_total = sum_k x_k). For a
  # single co-solvent this reduces exactly to the validated case. The Abraham a/e superposition is the
  # standard mole-fraction mixing rule for LSER solvent parameters; the structural part is a
  # composition-weighted effective co-solvent. NOTE: the model is trained on single co-solvents only, so
  # blends are an interpolation/extrapolation (reliable for ranking; treat absolute blend values as
  # indicative). Keep the total co-solvent fraction within the model domain (x up to ~0.1).
  cs_extra <- c(m$cs_base_cols, "cs_a", "cs_e")
  cs_vec <- rep(0, length(cs_extra)); xval <- 0
  if (!is.null(cosolvent)) {
    cosolvent <- as.character(cosolvent)
    xw <- rep_len(as.numeric(x), length(cosolvent))
    keep <- !is.na(cosolvent) & nzchar(cosolvent) & is.finite(xw) & xw > 0
    cosolvent <- cosolvent[keep]; xw <- xw[keep]
    for (k in seq_along(cosolvent)) {
      f_cos <- .mp_feat(cosolvent[k])
      cb <- if (is.null(f_cos)) rep(0, length(m$base_names)) else as.numeric(f_cos$base[m$base_names])
      cb[!is.finite(cb)] <- 0
      ci <- match(.mp_canon(cosolvent[k]), m$solvent_ae$ck)
      cs_a <- if (is.na(ci)) 0 else m$solvent_ae$a[ci]
      cs_e <- if (is.na(ci)) 0 else m$solvent_ae$e[ci]
      cs_vec <- cs_vec + c(cb, cs_a, cs_e) * xw[k]
    }
    xval <- sum(xw)
  }
  names(cs_vec) <- cs_extra

  # Assemble the full feature matrix in the model's column order, one row per (T, P).
  fixed <- c(solblk, E = abra[["E"]], S = abra[["S"]], A = abra[["A"]], B = abra[["B"]], V = abra[["V"]],
             Tm = as.numeric(Tm), cs_vec)
  X <- matrix(rep(fixed, each = n), nrow = n, dimnames = list(NULL, names(fixed)))
  X <- cbind(X, T = temp, P = pres, x = xval)
  X <- X[, m$allcols, drop = FALSE]

  log10_y <- as.numeric(stats::predict(m$fit, xgboost::xgb.DMatrix(X)))
  out <- data.frame(temp_K = temp, pres_bar = pres, log10_y = log10_y, mole_frac = 10^log10_y)
  attr(out, "Tm") <- as.numeric(Tm)
  attr(out, "abraham") <- abra
  out
}
