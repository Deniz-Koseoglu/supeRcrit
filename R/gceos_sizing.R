#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# HYDRAULIC SIZING of a counter-current supercritical-fluid extraction column
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Turns a converged equilibrium-stage solution (gceos_column_ns_solve /
# gceos_column) into a physical column: flooding velocity, diameter (sized at a
# fraction of flooding) and packed height. Flooding follows the capacity-factor
# (Souders-Brown) model used for packed SCF columns; the densities can come from
# the GC-EOS itself (general) or from the Budich & Brunner CO2 + vegetable-oil
# correlations (to reproduce Diaz et al. 2005, J. Supercrit. Fluids 35, 49).

# Pure-CO2 (or any single component) mass density (kg/m3) from the GC-EOS at T,P.
.gceos_pure_density <- function(T, P_atm, comp, model, phase = c("vapor","liquid")) {
  phase <- match.arg(phase)
  x <- .gceos_inputs(T, list(comp), model)
  MW <- if (!is.null(comp$MW)) comp$MW else NA_real_
  want <- if (phase == "vapor") 1L else 0L
  Vm <- .gceos_vroot(T, P_atm, c(1), x, want)        # cm3/mol
  if (is.na(Vm) || is.na(MW)) return(NA_real_)
  MW / Vm * 1000                                     # g/mol / (cm3/mol) -> kg/m3
}

# Budich & Brunner (2010) phase densities as a function of CO2 density (kg/m3).
.budich_rho_g <- function(rho_co2) 1.122*rho_co2 - 0.001606*rho_co2^2 + 5.317e-6*rho_co2^3
.budich_rho_l <- function(rho_co2, T)
  ((1859.45 - 3*T) - (8.2176 - 0.01851*T)*rho_co2) / (1 - (0.00597 - 1.1e-5*T)*rho_co2)

#' @title Hydraulic sizing of a supercritical-fluid extraction column
#' @description Sizes the diameter and packed height of a counter-current column
#'   from a converged equilibrium-stage solution, using the capacity-factor
#'   flooding model of packed SCF columns (Diaz et al. 2005, Appendix B).
#' @param col A converged column result from \code{\link{gceos_column_ns_solve}}
#'   or \code{\link{gceos_column}} (provides the per-feed \code{L},\code{V},
#'   \code{x},\code{y} profiles).
#' @param feed_rate Oil (raffinate-phase) feed mass rate, kg/h.
#' @param feed_z Oil feed mole-fraction vector (the \code{zL} used for
#'   \code{col}); sets the feed molar mass for the absolute flow scale.
#' @param T Column temperature (K). @param P Column pressure (atm).
#' @param comps,model The component list and GC-EOS model used for \code{col}
#'   (each component should carry an \code{MW} field, g/mol, for the mass flows).
#' @param solvent Name of the solvent component (default \code{"CO2"}).
#' @param density Either \code{"eos"} (phase densities from the GC-EOS volume
#'   roots, general) or \code{"budich"} (the Budich-Brunner CO2 + vegetable-oil
#'   correlations, to reproduce Diaz et al. 2005). Default \code{"eos"}.
#' @param rho_CO2 CO2 density (kg/m3) at \code{T},\code{P}; required for
#'   \code{density="budich"} unless it can be obtained from the EOS. If
#'   \code{NULL} it is computed from the GC-EOS.
#' @param HETP Height equivalent to a theoretical plate (m). Default 0.685 m
#'   (the value implied by Diaz's 28 stages over 19.14 m). Packing-specific.
#' @param frac_flood Fraction of flooding velocity for the diameter (default
#'   0.80, as in Diaz et al. 2005). Used only in design mode.
#' @param diameter Optional column diameter (m). If \code{NULL} (default) the
#'   column is sized for \code{frac_flood} of flooding (design mode). If given,
#'   that diameter is taken and the actual fraction of flooding is reported
#'   (rating mode).
#' @param flood_factor Packing-specific multiplier on the flooding velocity
#'   (1.0 = the baseline capacity-factor correlation; structured packings >1,
#'   random/beads <1). Default 1.0.
#' @return A \code{list} with \code{diameter} (m), \code{height} (m),
#'   \code{u_flood}, \code{u_super}, \code{u_liq} (m/s), \code{pct_flood} (%),
#'   \code{packed_volume} (m3), \code{aspect_ratio}, \code{residence_v} (s),
#'   \code{feed_at_flood} (kg/h), \code{Fv}, \code{psi}, \code{rho_g},
#'   \code{rho_l} (kg/m3), \code{Vmax} (mol/s), \code{n_stages}, and \code{mode}.
#' @references Diaz, Brignole, J. Supercrit. Fluids 35 (2005) 49, Appendix B.
#'   Budich, Brunner, J. Supercrit. Fluids (2010).
#' @noRd
gceos_column_sizing <- function(col, feed_rate, feed_z, T, P, comps, model = "gceos",
                                 solvent = "CO2", density = c("eos","budich"),
                                 rho_CO2 = NULL, HETP = 0.685, frac_flood = 0.80,
                                 diameter = NULL, flood_factor = 1.0) {
  density <- match.arg(density)
  nms <- names(comps); sidx <- match(solvent, nms)
  MW <- vapply(comps, function(c) if (!is.null(c$MW)) c$MW else NA_real_, numeric(1))
  if (anyNA(MW)) stop("Each component in 'comps' must carry an 'MW' (g/mol) field for sizing.", call. = FALSE)

  N <- nrow(col$x)
  # absolute molar flows: col is per unit (FL=1) feed; scale by the real feed rate
  feed_z   <- feed_z / sum(feed_z)
  MW_oil   <- sum(feed_z * MW)                                         # g/mol of the oil feed
  feed_mol_s <- (feed_rate * 1000 / MW_oil) / 3600                     # mol/s of feed (kg/h -> mol/s)
  V_abs <- col$V * feed_mol_s                                          # vapor molar flow per stage, mol/s
  L_abs <- col$L * feed_mol_s
  jmax  <- which.max(V_abs)                                            # worst (highest-load) stage
  Vmax  <- V_abs[jmax]; Lhere <- L_abs[jmax]
  M_g   <- sum(col$y[jmax, ] * MW) / 1000                              # vapor MW, kg/mol
  M_L   <- sum(col$x[jmax, ] * MW) / 1000                              # liquid MW, kg/mol

  # phase densities
  if (is.null(rho_CO2)) {
    co2 <- comps[[sidx]]; if (is.null(co2$MW)) co2$MW <- MW[sidx]
    rho_CO2 <- .gceos_pure_density(T, P, co2, model, "vapor")
  }
  if (density == "budich") {
    rho_g <- .budich_rho_g(rho_CO2); rho_l <- .budich_rho_l(rho_CO2, T)
  } else {
    # EOS densities of the actual vapor / liquid phases at the worst stage
    vco <- comps; for (i in seq_along(vco)) if (is.null(vco[[i]]$MW)) vco[[i]]$MW <- MW[i]
    rho_g <- .gceos_phase_density(T, P, col$y[jmax, ], vco, model, "vapor")
    rho_l <- .gceos_phase_density(T, P, col$x[jmax, ], vco, model, "liquid")
  }

  psi   <- (Lhere * M_L) / (Vmax * M_g) * sqrt(rho_g / rho_l)
  Fv    <- 1 / (8 + 60 * sqrt(psi))
  uflood <- flood_factor * Fv * sqrt((rho_l - rho_g) / rho_g)          # m/s (packing-scaled)
  vol_g <- Vmax * M_g / rho_g                                          # m3/s vapour (mol/s * kg/mol / kg/m3)
  vol_l <- Lhere * M_L / rho_l                                         # m3/s liquid at the worst stage
  H     <- N * HETP                                                    # m

  # Two sizing directions:
  #  - design  (diameter = NULL): pick the diameter that runs at frac_flood of flooding;
  #  - rating  (diameter given) : take the diameter, report the actual % of flooding.
  if (is.null(diameter)) {
    u_super <- frac_flood * uflood                                     # design superficial vapour velocity
    area    <- vol_g / u_super
    D       <- sqrt(4 * area / pi)
    pct_flood <- 100 * frac_flood
  } else {
    D       <- diameter
    area    <- pi * D^2 / 4
    u_super <- vol_g / area                                            # actual superficial vapour velocity
    pct_flood <- 100 * u_super / uflood
  }
  u_liq         <- vol_l / area                                        # superficial liquid velocity, m/s
  packed_volume <- area * H                                            # m3 (= pi/4 D^2 H)
  aspect_ratio  <- H / D                                               # slenderness
  residence_v   <- if (u_super > 0) H / u_super else NA_real_          # vapour superficial residence time, s
  feed_at_flood <- if (u_super > 0) feed_rate * (uflood / u_super) else NA_real_  # kg/h that reaches flooding

  list(diameter = D, height = H, u_flood = uflood, u_super = u_super, u_liq = u_liq,
       pct_flood = pct_flood, packed_volume = packed_volume, aspect_ratio = aspect_ratio,
       residence_v = residence_v, feed_at_flood = feed_at_flood, area = area,
       Fv = Fv, psi = psi, rho_g = rho_g, rho_l = rho_l, rho_CO2 = rho_CO2, Vmax = Vmax,
       n_stages = N, HETP = HETP, frac_flood = frac_flood, flood_factor = flood_factor,
       mode = if (is.null(diameter)) "design" else "rating", density = density)
}

# Mixture mass density (kg/m3) of a phase of composition z at T,P from the GC-EOS.
.gceos_phase_density <- function(T, P_atm, z, comps, model, phase = c("vapor","liquid")) {
  phase <- match.arg(phase); z <- z / sum(z)
  x <- .gceos_inputs(T, comps, model)
  MWmix <- sum(z * vapply(comps, function(c) c$MW, numeric(1)))
  want <- if (phase == "vapor") 1L else 0L
  Vm <- .gceos_vroot(T, P_atm, z, x, want)           # cm3/mol
  if (is.na(Vm)) return(NA_real_)
  MWmix / Vm * 1000
}
