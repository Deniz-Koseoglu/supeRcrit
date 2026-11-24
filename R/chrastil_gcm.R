#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# GROUP-CONTRIBUTION ESTIMATION of Chrastil solubility parameters (k, A, B)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Garcia-Samino et al., J. Mol. Liq. 403 (2024) 124890. Predicts the three Chrastil
# parameters of the Stiver (mol/mol) form  y = rho^k * exp(A/T + B)  from a molecule's
# Joback first-order groups, with NO intercept (k = Sum n_i k_i, etc.). Reported AARD ~51 %.
# The decomposition reuses the package's existing GCM pipeline (sub_smarts -> define_grps ->
# gcm_contribs); the "CHRASTIL" method's SMARTS / contributions live in sysdata (added
# alongside the JR/NL/SB/... methods - those are untouched).

# Sum the group contributions into the three Chrastil parameters (mirrors gcm_bp/gcm_hsp).
# No intercept term (per the source paper).
#' @noRd
gcm_chrastil <- function(contribs, method = NA) {
  if (length(contribs) < 4 || !all(c("results", "solute", "smarts", "method") %in% names(contribs)))
    stop("Argument 'contribs' must be output from the function 'gcm_contribs'!")
  cdata <- contribs[["results"]]
  c(k = sum(cdata[, "Sum_k_cont"], na.rm = TRUE),
    A = sum(cdata[, "Sum_A_cont"], na.rm = TRUE),
    B = sum(cdata[, "Sum_B_cont"], na.rm = TRUE))
}

#' @title Estimate Chrastil solubility parameters from molecular structure
#'
#' @description Group-contribution estimate of the three Chrastil parameters (\code{k}, \code{A},
#'   \code{B}) of the Stiver mol/mol form \eqn{y = \rho^{k}\,e^{A/T + B}} (\eqn{\rho} = scCO2
#'   density in g/L, \eqn{T} in K), for use when no fitted coefficients are available. Uses the
#'   Joback first-order group scheme of Garcia-Samino et al. (2024) via the package's standard
#'   SMARTS decomposition. The method is approximate: the reported AARD on solubility is
#'   \strong{~51 %} (10.8 % on log-values), so results are for screening, not exact prediction.
#'
#' @param solute A SMILES string, or a solute object from \code{\link{mol_find}}. A bare SMILES
#'   is resolved offline via \code{\link{mol_find}} (no web lookup); a CAS/Name goes through the
#'   usual online resolution.
#' @param mw Optional solute molar mass (g/mol); taken from the structure automatically when not
#'   supplied. Used to also return the g/L-basis parameters (\eqn{k_{g/L} = k+1}, \eqn{A}
#'   unchanged, \eqn{B_{g/L} = B + \ln(MW/44.01)}), matching the bundled coefficient database.
#' @param silent Suppress decomposition progress messages (default \code{TRUE}).
#'
#' @return A \code{list} with the mol/mol parameters \code{k}, \code{A}, \code{B}; the matched
#'   \code{groups}; \code{unsupported_elements} (elements outside the scheme, if any); the
#'   \code{aard_pct} (51.22); \code{domain} (a list \code{in_domain} = logical + \code{reasons} = character,
#'   the applicability-domain check); and, when \code{mw} is given, the g/L triplet \code{k_gL}/\code{A_gL}/\code{B_gL}.
#' @seealso \code{\link{mol_find}}
#' @export
chrastil_gcm_estimate <- function(solute, mw = NA, silent = TRUE) {
  if (is.character(solute) && length(solute) == 1) solute <- mol_find(solute)   # offline for a bare SMILES
  if (!is.list(solute) || !all(c("IDs", "Molfile") %in% names(solute)))
    stop("'solute' must be a SMILES string or a mol_find() output.", call. = FALSE)
  if (!(is.numeric(mw) && length(mw) == 1 && is.finite(mw))) {                  # use the structure's MW if not given
    m <- suppressWarnings(as.numeric(solute[["IDs"]]["MW"])); if (is.finite(m)) mw <- m
  }

  contribs <- gcm_contribs_route(solute, "CHRASTIL", gorder = 0, silent = silent)
  par      <- gcm_chrastil(contribs)

  res_df <- contribs[["results"]]
  grps   <- if (nrow(res_df)) res_df[, c("Group", "ID", "Occurrences")] else res_df
  syms   <- unique(vapply(rcdk::get.atoms(solute[["Molfile"]]), rcdk::get.symbol, character(1)))
  unsupported <- setdiff(syms, c("C", "H", "N", "O", "S", "F", "Cl", "Br", "I", "P"))

  out <- list(k = unname(par["k"]), A = unname(par["A"]), B = unname(par["B"]),
              basis = "mol/mol", groups = grps, Contribs = res_df,   # full per-group k/A/B table (for the UI)
              n_groups = if (nrow(res_df)) sum(res_df[, "Occurrences"]) else 0,
              unsupported_elements = unsupported, aard_pct = 51.22,
              smarts = contribs[["smarts"]])   # matched SMARTS, for plot_gcm() group highlighting
  if (is.numeric(mw) && length(mw) == 1 && is.finite(mw)) {
    out$k_gL <- out$k + 1; out$A_gL <- out$A; out$B_gL <- out$B + log(mw / 44.01)
  }

  # Applicability-domain check. The Chrastil model applies to BOTH solid and liquid solutes in scCO2, so
  # there is deliberately NO solid/liquid (melting-point) gate. The k/A/B coefficients have no physical
  # bounds (they cross zero), so the guard is on STRUCTURAL similarity to the GC training set, not on the
  # coefficient values. Robust runtime checks (catch the gross out-of-domain: organometallics, exotic,
  # oversized):
  #   (1) any element outside the C/H/N/O/S/halogen/P scheme        -> out of domain;
  #   (2) heavy atom(s) not matched to any calibrated Chrastil group -> structural extrapolation;
  #   (3) molar mass > 1000 g/mol                                    -> above the fitted range (macromolecule/polymer).
  # LIMITATION: these miss PARAMETRIC extrapolation - a fully-covered organic whose group COMPOSITION is
  # atypical of the training set (e.g. a long n-alkane: covered, organic, small MW, but the additive k/A/B
  # extrapolate so the reconstructed solubility explodes). Catching that needs a training-group-count
  # range/leverage AD, or a solubility-sanity check at the cascade stage where T/P are known.
  reasons <- character(0)
  if (length(unsupported))
    reasons <- c(reasons, sprintf("non-organic element(s) outside the scheme: %s", paste(unsupported, collapse = ", ")))
  cov <- contribs[["coverage"]]
  if (is.list(cov) && length(cov$covered) && length(cov$n_heavy) && is.finite(cov$covered) && is.finite(cov$n_heavy) && cov$covered < cov$n_heavy)
    reasons <- c(reasons, sprintf("%d of %d heavy atoms not matched to a calibrated group", cov$n_heavy - cov$covered, cov$n_heavy))
  if (is.numeric(mw) && length(mw) == 1 && is.finite(mw) && mw > 1000)
    reasons <- c(reasons, sprintf("molar mass %.0f g/mol above the fitted range (likely macromolecule/polymer)", mw))
  out$domain <- list(in_domain = length(reasons) == 0, reasons = reasons)
  out
}
