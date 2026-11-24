#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FUNCTION: Target-focused separator fractionation design (dilute-feed recovery)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Optimal separator pressure/temperature for recovering a target solute (dilute-feed design)
#'
#' @description Designs the depressurisation-separator step of a supercritical-CO2 fractionation around a
#' single \strong{target} solute at a known \strong{feed loading} in the CO2 stream. As the stream is
#' depressurised through a separator at \eqn{(T, P)} the carrying capacity falls to \eqn{S(T, P)} and the
#' target precipitates once its solubility drops below the feed; the single-stage recovery is
#' \deqn{\mathrm{recovery}(T, P) = \max\!\left(0,\; 1 - S(T, P)/\mathrm{feed}\right).}
#' It reports the \strong{precipitation onset} (the highest separator pressure at which the target begins to
#' drop out) and the \strong{recovery-target pressure} (the highest pressure that still recovers a chosen
#' fraction, default 90%), each optimised over a separator-temperature grid, plus the full recovery-vs-pressure
#' curve. Use the solubility and feed in a mass-conserving unit (g solute per kg CO2, or mole fraction) so the
#' balance is consistent with the separator (CO2 mass, not volume, is conserved).
#'
#' @param sfun A solubility function \code{sfun(temp_C, pres_bar)} returning the target's solubility in the
#'   \strong{same unit as \code{feed}} (use a mass-conserving unit, e.g. g/kg CO2 via \code{\link{solub_gkg}},
#'   or mole fraction). It is called with a \strong{scalar} temperature and the \strong{pressure vector}, and
#'   must return one value per pressure.
#' @param feed The target's feed loading in the CO2 stream (same unit as \code{sfun} returns; e.g. g solute
#'   per kg CO2). This is the master input — the onset is where \eqn{S(T, P) = \mathrm{feed}}.
#' @param sep_T Numeric vector of candidate separator temperatures (deg C). Pass one value for a single-
#'   temperature design, or several to optimise over temperature.
#' @param P_lo,P_hi Separator pressure search range (bar; defaults 40-400).
#' @param P_step Pressure grid step (bar; default 1).
#' @param recovery_target Target single-stage recovery fraction (0-1; default 0.90).
#' @param onset_thresh Recovery fraction defining the precipitation onset (default 0.01, i.e. 1%).
#'
#' @return A named \code{list}:
#' \describe{
#'   \item{C_feed}{The feed loading used (same unit as \code{sfun}).}
#'   \item{onset_P, onset_T}{Highest separator pressure (and its temperature) at which recovery exceeds
#'     \code{onset_thresh} — where the target starts precipitating.}
#'   \item{target_P, target_T, target_recovery}{Highest separator pressure (and its temperature) that
#'     achieves at least \code{recovery_target}, with the recovery actually attained.}
#'   \item{onset_at_target_T}{The onset pressure at the recovery-target's optimal temperature (for reporting
#'     both criteria at a single coherent temperature).}
#'   \item{curve}{A tidy \code{data.frame} (\code{temp_C}, \code{pres_bar}, \code{solubility},
#'     \code{recovery}) of the full grid, for plotting.}
#'   \item{by_temp}{Per-temperature \code{data.frame} of \code{onset_P} and \code{target_P}.}
#' }
#' @seealso \code{\link{chrastil_solubility}}, \code{\link{solub_cascade}}, \code{\link{chrastil_cascade}}.
#' @examples
#' \dontrun{
#' r <- chrastil_db(); tq <- r[r$name == "thymoquinone", ][1, ]
#' # solubility in g solute per kg CO2 (mass basis)
#' sfun <- function(T, P) solub_gkg(chrastil_solubility(tq$k, tq$a, tq$b, temp_C = T, pres_bar = P,
#'                                    basis = "mol/mol", mw = tq$mw)$y_molmol, tq$mw)
#' des <- sfe_frac(sfun, feed = 2.0, sep_T = 50)   # feed = 2 g TQ / kg CO2
#' des$onset_P; des$target_P
#' }
#' @noRd
sfe_frac <- function(sfun, feed, sep_T = seq(35, 70, by = 5), P_lo = 40, P_hi = 400,
                     P_step = 1, recovery_target = 0.90, onset_thresh = 0.01) {
  C_feed <- as.numeric(feed)[1]
  if (!isTRUE(is.finite(C_feed)) || C_feed <= 0)
    stop("`feed` must be a positive loading in the same unit as `sfun` returns.", call. = FALSE)
  sep_T <- sort(unique(as.numeric(sep_T))); sep_T <- sep_T[is.finite(sep_T)]
  if (!length(sep_T)) stop("`sep_T` must have at least one finite temperature.", call. = FALSE)
  if (P_hi <= P_lo) stop("`P_hi` must exceed `P_lo`.", call. = FALSE)
  P_grid <- seq(P_lo, P_hi, by = P_step)

  # recovery(P, T) over the grid.
  rows <- list()
  by_temp <- data.frame(temp_C = sep_T, onset_P = NA_real_, target_P = NA_real_)
  for (ti in seq_along(sep_T)) {
    # sfun is called with a SCALAR temperature and the pressure vector -> one solubility per pressure.
    S_P <- as.numeric(sfun(sep_T[ti], P_grid))
    if (length(S_P) != length(P_grid))
      stop("`sfun(temp_C, pres_bar)` must return one solubility per pressure (scalar T, vector P).", call. = FALSE)
    rec <- pmax(0, 1 - S_P / C_feed); rec[!is.finite(rec)] <- NA_real_
    rows[[ti]] <- data.frame(temp_C = sep_T[ti], pres_bar = P_grid, solubility = S_P,
                             recovery = rec, stringsAsFactors = FALSE)
    on_ok  <- P_grid[is.finite(rec) & rec > onset_thresh]
    tgt_ok <- P_grid[is.finite(rec) & rec >= recovery_target]
    by_temp$onset_P[ti]  <- if (length(on_ok))  max(on_ok)  else NA_real_
    by_temp$target_P[ti] <- if (length(tgt_ok)) max(tgt_ok) else NA_real_
  }
  curve <- do.call(rbind, rows)

  # Optimal separator condition = the temperature giving the HIGHEST pressure for each criterion (least
  # depressurisation). Ties broken by the lower temperature.
  pick_hi <- function(col) {
    v <- by_temp[[col]]; if (all(is.na(v))) return(c(P = NA_real_, T = NA_real_))
    i <- which(v == max(v, na.rm = TRUE)); i <- i[which.min(sep_T[i])]
    c(P = v[i], T = sep_T[i])
  }
  on <- pick_hi("onset_P"); tg <- pick_hi("target_P")
  tgt_rec <- if (is.finite(tg[["P"]])) {
    r <- curve$recovery[curve$temp_C == tg[["T"]] & curve$pres_bar == tg[["P"]]]; if (length(r)) r[1] else NA_real_
  } else NA_real_
  # Onset AT the recovery-target's optimal temperature — for coherent single-condition reporting (report
  # both criteria at one temperature). NA when the target is unreachable.
  onset_at_tgt_T <- if (is.finite(tg[["T"]])) by_temp$onset_P[by_temp$temp_C == tg[["T"]]][1] else NA_real_

  list(C_feed = C_feed,
       onset_P = unname(on[["P"]]), onset_T = unname(on[["T"]]),
       target_P = unname(tg[["P"]]), target_T = unname(tg[["T"]]), target_recovery = tgt_rec,
       onset_at_target_T = onset_at_tgt_T,
       recovery_target = recovery_target, curve = curve, by_temp = by_temp)
}
