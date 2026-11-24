# Native FastSigma: estimate COSMO-SAC-dsp inputs (sigma profile, cavity area/volume, dispersion
# e/kB and class) from molecular structure, for compounds outside the bundled UD profile database.
#
# This file (Phase 2c-1) implements the DETERMINISTIC dispersion pieces -- the dispersion energy
# parameter e/kB and the dispersion class -- which the COSMO-SAC-dsp authors compute directly from
# the atomic connectivity (Hsieh, Lin & Vrabec 2014; NIST cCOSMO to_sigma.py). No QM or ML needed.
# The sigma profile + area + volume (genuinely QM-derived) are handled by the group-contribution
# model added in a later phase.

# Atomic dispersion energy parameters e/kB [K] (NIST to_sigma.py dispersive_parameter_lib).
.FS_DISP <- c(
  "C(sp3)" = 115.7023, "C(sp2)" = 117.4650, "C(sp)" = 66.0691,
  "-O-"    = 95.6184,  "=O"     = -11.0549,
  "N(sp3)" = 15.4901,  "N(sp2)" = 84.6268,  "N(sp)" = 109.6621,
  "F"      = 52.9318,  "Cl"     = 104.2534,
  "H(OH)"  = 19.3477,  "H(NH)"  = 141.1709, "H(water)" = 58.3301, "H(other)" = 0
)

# Dispersion-class integer codes used by the engine (csac_lngamma_cpp):
# 0 = WATER, 1 = COOH, 2 = NHB, 3 = HB-ACCEPTOR (only acceptor), 4 = HB-DONOR-ACCEPTOR
.FS_DCLASS <- c("H2O" = 0L, "COOH" = 1L, "NHB" = 2L, "HB-ACCEPTOR" = 3L, "HB-DONOR-ACCEPTOR" = 4L)

# Build a light molecular graph from SMILES: element symbol + neighbour indices per atom,
# with explicit hydrogens (bond count -> hybridisation, exactly as in to_sigma.py).
.fs_graph <- function(smi) {
  m <- tryCatch(rcdk::parse.smiles(smi)[[1]], error = function(e) NULL)
  if (is.null(m)) return(NULL)
  tryCatch(rcdk::convert.implicit.to.explicit(m), error = function(e) NULL)
  atoms <- rcdk::get.atoms(m)
  n <- length(atoms)
  if (!n) return(NULL)
  sym <- vapply(atoms, rcdk::get.symbol, character(1))
  hsh <- vapply(atoms, function(a) rJava::.jcall(a, "I", "hashCode"), integer(1))
  idx <- stats::setNames(seq_len(n), as.character(hsh))
  nb <- vector("list", n)
  for (b in rcdk::get.bonds(m)) {
    ba <- rcdk::get.atoms(b)
    i <- idx[[as.character(rJava::.jcall(ba[[1]], "I", "hashCode"))]]
    j <- idx[[as.character(rJava::.jcall(ba[[2]], "I", "hashCode"))]]
    if (is.null(i) || is.null(j)) next
    nb[[i]] <- c(nb[[i]], j); nb[[j]] <- c(nb[[j]], i)
  }
  list(sym = sym, nb = nb, n = n)
}

# Per-atom dispersion parameter following to_sigma.py get_dispersive_values().
# Returns NA for an atom outside {C,H,N,O,F,Cl} (renders the whole molecule's e/kB undefined).
.fs_atom_disp <- function(el, deg, nbsym, is_water) {
  switch(el,
    "C" = if (deg == 4) .FS_DISP[["C(sp3)"]] else if (deg == 3) .FS_DISP[["C(sp2)"]]
          else if (deg == 2) .FS_DISP[["C(sp)"]] else NA_real_,
    "N" = if (deg == 3) .FS_DISP[["N(sp3)"]] else if (deg == 2) .FS_DISP[["N(sp2)"]]
          else if (deg == 1) .FS_DISP[["N(sp)"]] else NA_real_,
    "O" = if (deg == 2) .FS_DISP[["-O-"]] else if (deg == 1) .FS_DISP[["=O"]] else NA_real_,
    "F" = .FS_DISP[["F"]],
    "Cl" = .FS_DISP[["Cl"]],
    # H bonded to C ("other") contributes no dispersion term (to_sigma.py returns None, not 0)
    "H" = if (is_water) .FS_DISP[["H(water)"]]
          else if ("O" %in% nbsym) .FS_DISP[["H(OH)"]]
          else if ("N" %in% nbsym) .FS_DISP[["H(NH)"]]
          else NA_real_,
    NA_real_  # any other element -> invalid (S, Br, P, Si, ...)
  )
}

# Estimate dispersion e/kB [K] and dispersion class for a SMILES.
# Deterministic; matches the COSMO-SAC-dsp parameterisation used to build the bundled profiles.
.fs_dispersion <- function(smi) {
  g <- .fs_graph(smi)
  if (is.null(g)) return(list(ekB = NA_real_, dclass = NA_integer_, flag = NA_character_))
  sym <- g$sym; nb <- g$nb; n <- g$n
  deg <- vapply(nb, length, integer(1))
  heavy <- which(sym != "H")
  is_water <- (length(heavy) == 1L && sym[heavy] == "O")

  # --- e/kB: mean of per-atom parameters over atoms with a defined parameter ---
  vals <- vapply(seq_len(n), function(i)
    .fs_atom_disp(sym[i], deg[i], sym[nb[[i]]], is_water), numeric(1))
  invalid <- any(sym %in% setdiff(unique(sym), c("C", "H", "N", "O", "F", "Cl")))
  ekB <- if (invalid) NA_real_ else {
    keep <- !is.na(vals)
    if (any(keep)) mean(vals[keep]) else NA_real_
  }

  # --- COOH detection: C(deg 3) bonded to two O, one of which is O bonded to exactly {C,H} ---
  has_COOH <- FALSE
  for (i in which(sym == "C" & deg == 3)) {
    onb <- nb[[i]][sym[nb[[i]]] == "O"]
    if (length(onb) == 2) {
      for (o in onb) {
        if (identical(sort(sym[nb[[o]]]), c("C", "H"))) { has_COOH <- TRUE; break }
      }
    }
    if (has_COOH) break
  }

  # --- dispersion class ---
  flag <- "NHB"
  hb_atoms <- which(sym %in% c("O", "N", "F"))
  if (length(hb_atoms)) {
    donor <- any(vapply(hb_atoms, function(i) "H" %in% sym[nb[[i]]], logical(1)))
    flag <- if (donor) "HB-DONOR-ACCEPTOR" else "HB-ACCEPTOR"
  }
  if (has_COOH) flag <- "COOH"
  if (is_water) flag <- "H2O"

  list(ekB = ekB, dclass = unname(.FS_DCLASS[[flag]]), flag = flag)
}

# --- FastSigma: predict a COSMO-SAC-dsp sigma profile from structure (trained on CHAOS 53k) ---

.fs_model_cache <- new.env(parent = emptyenv())
.fs_model <- function() {
  if (is.null(.fs_model_cache$m)) {
    f <- system.file("extdata", "models", "fastsigma_model.rds", package = "supeRcrit")
    if (!nzchar(f)) stop("fastsigma_model.rds not found; reinstall supeRcrit.", call. = FALSE)
    m <- readRDS(f)
    m$pc_boost <- lapply(m$pc_models, xgboost::xgb.load.raw)
    m$area_boost <- xgboost::xgb.load.raw(m$area_model)
    .fs_model_cache$m <- m
  }
  .fs_model_cache$m
}

# Which hydrogen-bond categories a molecule can populate (to mask reconstruction leakage):
# OH = any O bonded to H; OT = any N/F, or O not bonded to H, or H bonded to N/F.
.fs_hb_presence <- function(g) {
  sym <- g$sym; nb <- g$nb
  o <- which(sym == "O"); h <- which(sym == "H")
  hasOH <- any(vapply(o, function(i) "H" %in% sym[nb[[i]]], logical(1)))
  hasOT <- any(sym %in% c("N", "F")) ||
    any(vapply(o, function(i) !("H" %in% sym[nb[[i]]]), logical(1))) ||
    any(vapply(h, function(i) any(c("N", "F") %in% sym[nb[[i]]]), logical(1)))
  list(OH = isTRUE(hasOH), OT = isTRUE(hasOT))
}

.fs_featurize_one <- function(smi, dnames) {
  m <- tryCatch(rcdk::parse.smiles(smi)[[1]], error = function(e) NULL)
  if (is.null(m)) return(NULL)
  # CDK emits a benign "The AtomType null could not be found" warning for some atoms during descriptor
  # typing; muffle just that noise (any real failure is still caught below and returns NULL).
  mute_cdk <- function(w) if (grepl("AtomType", conditionMessage(w), fixed = TRUE)) invokeRestart("muffleWarning")
  tryCatch(withCallingHandlers({
    rcdk::convert.implicit.to.explicit(m)
    as.numeric(rcdk::eval.desc(m, dnames, verbose = FALSE)[1, ])
  }, warning = mute_cdk), error = function(e) NULL)
}

#' Predict a COSMO-SAC-dsp sigma profile from a SMILES string (FastSigma)
#'
#' Machine-learned estimate of the COSMO-SAC-dsp inputs for a molecule outside the bundled databases,
#' trained on the 53k CHAOS (omegaB97X-D/def2-TZVP) profiles. The profile shape is regressed in a PCA
#' basis (xgboost on rcdk constitutional + topological descriptors), reconstructed, masked to the
#' hydrogen-bond categories the structure actually supports, and rescaled to the predicted cavity area;
#' the dispersion e/kB and class are computed deterministically (\code{.fs_dispersion}). The
#' result is on the CHAOS scale, so it is consistent with \code{csac_gamma(..., database = "chaos")}.
#'
#' @param smiles a SMILES string.
#' @return list \code{psA} (153 area-per-bin, NHB|OH|OT), \code{area}, \code{vol}, \code{ekB},
#'   \code{dclass}; or \code{NULL} if the SMILES cannot be parsed/featurized.
#' @seealso \code{\link{csac_gamma}}, \code{\link{csac_lle}}.
#' @export
fastsigma_profile <- function(smiles) {
  mo <- .fs_model()
  f <- .fs_featurize_one(smiles, mo$dnames)
  if (is.null(f) || length(f) < max(mo$feat_cols)) return(NULL)
  x <- f[mo$feat_cols]; bad <- !is.finite(x); x[bad] <- mo$feat_med[bad]
  x <- matrix(as.numeric(x), nrow = 1)
  pcs <- vapply(mo$pc_boost, function(b) stats::predict(b, x), numeric(1))
  shape <- as.numeric(mo$center + mo$rotation %*% pcs)            # 153-bin shape
  area <- exp(stats::predict(mo$area_boost, x))
  psA <- pmax(shape * area, 0)
  g <- .fs_graph(smiles)
  if (!is.null(g)) { pres <- .fs_hb_presence(g)
    if (!pres$OH) psA[52:102] <- 0
    if (!pres$OT) psA[103:153] <- 0 }
  s <- sum(psA); if (s > 0) psA <- psA * (area / s)              # rescale to predicted area
  d <- .fs_dispersion(smiles)
  vol <- unname(mo$vol_coef[1] + mo$vol_coef[2] * area)
  list(psA = psA, area = sum(psA), vol = vol, ekB = d$ekB, dclass = d$dclass)
}
