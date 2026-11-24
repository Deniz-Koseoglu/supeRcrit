#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CRITICAL-DIAMETER ESTIMATION AND PSEUDO-COMPONENT LUMPING
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# For components that have no experimental vapour-pressure or VLE data, the
# GC-EOS hard-sphere diameter d_c can be estimated from molecular structure.
# Espinosa et al. (2002), Ind. Eng. Chem. Res. 41:1516, correlated d_c of fatty
# acid alkyl esters against the UNIFAC van der Waals volume r_vdW (their Eq. 2/3).
# Natural mixtures (fish oil, PFAD) are also commonly reduced to a handful of
# pseudo-components by carbon number; gceos_lump() does that reduction.

# UNIFAC group volume parameters R_k (Fredenslund/Hansen tables). Only the groups
# that appear in the GC-EOS decomposition dictionary are listed; verified against
# Espinosa (2002) Table 3 (e.g. ethyl stearate C18:0 -> 2*0.9011 + 16*0.6744 +
# 1.6764 = 14.269, matching the paper to 3 d.p.).
.gceos_unifac_Rk <- c(
  CH3 = 0.9011, `CH3(B)` = 0.9011, CH2 = 0.6744, `CH2(B)` = 0.6744,
  CH = 0.4469,  C = 0.2195,  cyCH2 = 0.6744, cyCH = 0.4469,
  `CH2=CH` = 1.3454, `CH=CH` = 1.1167, `CH=C` = 0.8886, `CH2=C` = 1.1173,
  `C=C` = 0.6605, `C=CH` = 0.8886, `C=CH2` = 1.1173,
  ACH = 0.5313, AC = 0.3652, ACCH3 = 1.2663, ACCH2 = 1.0396,
  OH = 1.0000, COH = 0.6744 + 1.0000, CH3OH = 1.4311, C2H5OH = 2.1055,
  CH2OH = 1.2044, H2O = 0.9200,
  COOH = 1.3013, CH2COO = 1.6764, CH3COO = 1.9031, CH2COOH = 1.6744 + 1.3013,
  CO2 = 1.3000)

#' @title UNIFAC van der Waals volume of a group-decomposed molecule
#' @description Sums the UNIFAC group volume parameters \eqn{R_k} over a molecule's
#'   group counts, \eqn{r_{vdW} = \sum_k \nu_k R_k}. Used by the ester critical-
#'   diameter correlation (\code{\link{gceos_dc_ester}}).
#' @param groups Named integer vector of group counts (e.g. from
#'   \code{\link{gceos_decompose}}).
#' @return The van der Waals volume (UNIFAC \eqn{r}, dimensionless), or \code{NA}
#'   with a warning if any group lacks an \eqn{R_k} value.
#' @export
#' @examples
#' g <- gceos_decompose("CCCCCCCCCCCCCCCCCC(=O)OCC", "gceos")  # ethyl stearate
#' gceos_vdw_volume(g)                                          # ~14.27
gceos_vdw_volume <- function(groups) {
  nm <- names(groups)
  miss <- setdiff(nm, names(.gceos_unifac_Rk))
  if (length(miss)) {
    warning("No UNIFAC R_k for group(s): ", paste(miss, collapse = ", "), call. = FALSE)
    return(NA_real_)
  }
  sum(as.numeric(groups) * .gceos_unifac_Rk[nm])
}

#' @title Estimate a fatty-acid alkyl ester's GC-EOS critical diameter from structure
#'
#' @description
#' Estimates the hard-sphere critical diameter \eqn{d_c} for a fatty-acid methyl or
#' ethyl ester from its UNIFAC van der Waals volume, using the correlation of
#' Espinosa et al. (2002), Ind. Eng. Chem. Res. \strong{41}:1516 (their Eq. 2-3):
#' \deqn{d_c = \exp(1.075124 + 0.386989\,\ln r_{vdW}) - 0.07\,w,}
#' where \eqn{r_{vdW}} is the van der Waals volume of the \emph{saturated} ester and
#' \eqn{w} the number of C=C double bonds (the unsaturation correction). This is the
#' route for fish-oil esters whose vapour-pressure / VLE data are unavailable.
#'
#' \strong{Scope:} fitted to saturated and unsaturated fatty-acid alkyl esters
#' (C14-C22). It is \emph{not} a general-purpose \eqn{d_c} predictor - for other
#' families fit \eqn{d_c} to data with \code{\link{gceos_fit_dc}} (boiling point) or
#' \code{\link{gceos_fit_dc_vle}} (binary VLE).
#'
#' @param groups Named group-count vector of the ester (\code{\link{gceos_decompose}}),
#'   OR a numeric scalar giving \eqn{r_{vdW}} of the saturated ester directly.
#' @param w Number of C=C double bonds. If \code{groups} is a group vector and
#'   \code{w} is \code{NULL}, it is counted from the alkene groups and \eqn{r_{vdW}}
#'   is taken for the corresponding saturated ester (each C=C contributes as the
#'   equivalent \code{CH2-CH2}).
#' @return The estimated critical diameter \eqn{d_c} (cm/mol).
#' @export
#' @seealso \code{\link{gceos_vdw_volume}}, \code{\link{gceos_fit_dc}},
#'   \code{\link{gceos_fit_dc_vle}}
#' @examples
#' # EPA ethyl ester (C20:5): from its decomposition, w counted automatically
#' g <- gceos_decompose("CCCCCC=CCC=CCC=CCC=CCC=CCC(=O)OCC", "gceos")
#' gceos_dc_ester(g)
gceos_dc_ester <- function(groups, w = NULL) {
  ene <- c("CH2=CH", "CH=CH", "CH=C", "CH2=C", "C=C", "C=CH", "C=CH2")
  if (is.numeric(groups) && is.null(names(groups))) {
    rv <- groups[1]; if (is.null(w)) stop("Provide 'w' when passing r_vdW directly.", call. = FALSE)
  } else {
    nene <- sum(groups[intersect(names(groups), ene)])
    if (is.null(w)) w <- nene
    # r_vdW of the SATURATED ester: convert each C=C back to a -CH2-CH2- fragment
    gsat <- groups[setdiff(names(groups), ene)]
    gsat["CH2"] <- (if ("CH2" %in% names(gsat)) gsat["CH2"] else 0) + 2 * nene
    rv <- gceos_vdw_volume(gsat)
    if (is.na(rv)) return(NA_real_)
  }
  unname(exp(1.075124 + 0.386989 * log(rv)) - 0.07 * w)
}

#' @title Lump components into pseudo-components for GC-EOS
#'
#' @description
#' Reduces a detailed multicomponent mixture (e.g. a 13-component fish-oil ester or
#' palm fatty-acid distillate) to a few \emph{pseudo-components} by a grouping key
#' such as carbon number. Each pseudo-component's group vector and critical diameter
#' are the mole-fraction-weighted averages of its members, and the lumped mole
#' fractions are summed - the standard reduction used in supercritical-fractionation
#' modelling (Riha & Brunner; Espinosa et al. 2002).
#'
#' @param comps Named \code{list} of components, each \code{list(groups, dc, Tc)}.
#' @param z Mole fractions of \code{comps} (same length/order). Renormalised.
#' @param key Vector (same length as \code{comps}) giving each component's lump
#'   label (e.g. carbon number \code{c(14,16,18,...)}). Components with the same
#'   key are merged.
#' @return A \code{list} with \code{comps} (the pseudo-components, named by key) and
#'   \code{z} (their mole fractions).
#' @export
#' @examples
#' \dontrun{
#' lumped <- gceos_lump(comps, z, key = carbon_numbers)
#' gceos_column(N = 30, ..., comps = c(lumped$comps, list(CO2 = co2)), zL = ...)
#' }
gceos_lump <- function(comps, z, key) {
  stopifnot(length(comps) == length(z), length(key) == length(comps))
  z <- z / sum(z)
  out_comps <- list(); out_z <- numeric(0)
  for (k in unique(key)) {
    idx <- which(key == k); w <- z[idx]; ws <- sum(w); wn <- w / ws
    allg <- unique(unlist(lapply(comps[idx], function(c) names(c$groups))))
    gv <- stats::setNames(numeric(length(allg)), allg)
    for (j in seq_along(idx)) { g <- comps[[idx[j]]]$groups; gv[names(g)] <- gv[names(g)] + wn[j] * g }
    dc <- sum(wn * vapply(comps[idx], function(c) c$dc, numeric(1)))
    Tc <- sum(wn * vapply(comps[idx], function(c) c$Tc, numeric(1)))
    nm <- as.character(k)
    out_comps[[nm]] <- list(groups = gv, dc = dc, Tc = Tc)
    out_z <- c(out_z, ws)
  }
  list(comps = out_comps, z = stats::setNames(out_z, names(out_comps)))
}
