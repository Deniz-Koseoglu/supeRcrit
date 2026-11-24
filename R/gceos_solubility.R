#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# GC-EOS SOLID-/LIQUID-FLUID SOLUBILITY  (TRIAL / EXPERIMENTAL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Equilibrium solubility of a heavy, low-volatility solute in a (near-pure) CO2
# fluid phase, from the GC-EOS. The classic solid-gas / liquid-gas SFE relation:
#
#     y2 = f2^cond(T,P) / ( phi2^fluid(T,P,y) * P )
#
# where f2^cond is the fugacity of the pure condensed solute and phi2^fluid its
# fugacity coefficient at infinite dilution in CO2 (from the EoS). The condensed
# reference is built from the subcooled-LIQUID vapour pressure, optionally turned
# into a SOLID sublimation fugacity with a fusion correction, and Poynting-
# corrected to the system pressure:
#
#   solid:  f2 = Pvap_liq(T) * exp[-dHfus/R (1/T - 1/Tm)] * exp[v_s (P - Pvap)/RT]
#   liquid: f2 = Pvap_liq(T)                              * exp[v_L (P - Pvap)/RT]
#
# This is a TRIAL extension of the GC-EOS module (no solid phase in the core EoS);
# it reuses the compiled fugacity-coefficient kernel for the fluid phase only.

#' @title Solid- or liquid-fluid solubility of a solute in supercritical CO2 (GC-EOS, trial)
#'
#' @description
#' Estimates the equilibrium mole fraction of a heavy solute dissolved in a
#' near-pure supercritical-CO2 phase, \eqn{y_2}, by equating the pure condensed-phase
#' fugacity of the solute to its fugacity in the CO2-rich fluid (the latter from the
#' GC-EOS at infinite dilution). Handles both a \strong{solid} reference (sublimation
#' pressure via a fusion correction) and a subcooled-\strong{liquid} reference.
#'
#' \strong{Experimental / trial feature.} The GC-EOS core has no solid phase; this
#' wraps the compiled fluid-phase fugacity kernel with a standard solid-gas (or
#' liquid-gas) equilibrium expression. Accuracy depends on the group decomposition,
#' the fitted critical diameter \code{dc}, and the supplied vapour-pressure / fusion
#' data.
#'
#' @param T Temperature (K).
#' @param P_bar Pressure (bar).
#' @param solute A component \code{list(groups, dc, Tc)} (as for
#'   \code{\link{gceos_flash}}); the solute dissolved in CO2.
#' @param co2 The CO2 component \code{list}; default is the standard GC-EOS CO2
#'   (\code{groups = c(CO2 = 1)}, \code{dc = 3.1289}, \code{Tc = 304.2}).
#' @param model \code{"gceos"} (default), \code{"gceos-hmw"}, \code{"gcaeos"}, ... .
#' @param state \code{"solid"} (default) or \code{"liquid"} reference condensed phase.
#' @param Pvap Subcooled-liquid vapour pressure of the solute at \code{T} (atm). If
#'   \code{NULL}, it is predicted from the EoS via \code{\link{gceos_psat}}.
#' @param Tm,dHfus Melting temperature (K) and enthalpy of fusion (J/mol), used for
#'   the liquid-to-solid fusion correction when \code{state = "solid"} and
#'   \code{T < Tm}. If omitted, no fusion correction is applied (solute treated as a
#'   subcooled liquid even in solid mode).
#' @param vmol Condensed-phase molar volume (cm3/mol) for the Poynting term. If
#'   \code{NULL} the Poynting correction is skipped (usually a few % effect).
#' @param ytrace Trace solute mole fraction at which the infinite-dilution fugacity
#'   coefficient is evaluated.
#'
#' @return A named numeric vector \code{c(y, phi, fref_atm)}: the solute mole
#'   fraction \code{y} in the CO2 phase, its fluid-phase fugacity coefficient
#'   \code{phi}, and the condensed reference fugacity \code{fref_atm} (atm). \code{y}
#'   is \code{NA} when the fluid volume root cannot be found.
#' @export
#' @seealso \code{\link{gceos_flash}}, \code{\link{gceos_psat}}, \code{\link{gceos_fit_dc}}
gceos_solubility <- function(T, P_bar, solute, co2 = list(groups = c(CO2 = 1), dc = 3.1289, Tc = 304.2),
                             model = "gceos", state = c("solid", "liquid"),
                             Pvap = NULL, Tm = NULL, dHfus = NULL, vmol = NULL,
                             ytrace = 1e-6) {
  state <- match.arg(state)
  Rj <- 8.314462; Rcc <- 82.0573                          # J/mol/K ; cm3 atm/mol/K
  Patm <- P_bar / 1.01325
  comps <- list(S = solute, CO2 = co2)
  x <- .gceos_inputs(T, comps, model)
  frac <- c(ytrace, 1 - ytrace)
  lnphi <- .gceos_lnphi_phase(T, Patm, frac, x, "vap")    # CO2-rich fluid phase
  if (anyNA(lnphi)) return(c(y = NA_real_, phi = NA_real_, fref_atm = NA_real_))
  phi2 <- exp(lnphi[[1]])
  if (is.null(Pvap)) Pvap <- gceos_psat(T, solute, model) # subcooled-liquid Pvap (atm)
  if (is.null(Pvap) || is.na(Pvap)) return(c(y = NA_real_, phi = phi2, fref_atm = NA_real_))
  fref <- Pvap
  if (state == "solid" && !is.null(Tm) && !is.null(dHfus) && T < Tm)
    fref <- fref * exp(-(dHfus / Rj) * (1 / T - 1 / Tm))  # subcooled-liquid -> solid
  if (!is.null(vmol)) fref <- fref * exp(vmol * (Patm - Pvap) / (Rcc * T))  # Poynting
  y2 <- fref / (phi2 * Patm)
  c(y = unname(y2), phi = unname(phi2), fref_atm = unname(fref))
}
