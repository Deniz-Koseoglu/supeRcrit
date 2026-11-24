#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CHRASTIL density-based solubility model for solids/liquids in supercritical CO2
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Chrastil:  S = rho^k * exp(A/T + B)   <=>   ln S = k ln rho + A/T + B
# rho = CO2 density (g/L) from co2dens(); T in K. Used by the Co-Solvent Selection >
# solubility / separator-cascade tools.
#
# BASIS (important): both the bundled coefficient database (inst/extdata/chrastil_db.csv,
# from "Chrastil revisited", J. Supercrit. Fluids 168 (2021) 105065) and the GCM estimator
# report the *mol/mol* (mole-fraction) coefficients of that paper's Eq.(2) - NOT the original
# g/L form. So chrastil_solubility() returns y in mol/mol by default. The g/L form is recovered
# with Eq.(3): k_gL = k + 1, A_gL = A, B_gL = B + ln(MW/44.01); equivalently, pointwise,
# S(g/L) = y * (MW/44.01) * rho. Both use the Span-Wagner density basis (the paper's choice),
# hence the default EoS here.

#' @title Chrastil solubility over a temperature/pressure grid
#' @description Evaluates the Chrastil correlation \eqn{S = \rho^{k} e^{A/T + B}} over all
#'   combinations of the supplied temperatures and pressures, taking the CO2 density from
#'   \code{\link{co2dens}}. By default the parameters are the \strong{mol/mol} (mole-fraction)
#'   coefficients used by both the bundled database and the GCM estimator, so the native result
#'   is a mole fraction; supply \code{mw} to also get the g/L concentration.
#' @param k,A,B Chrastil parameters (in the basis given by \code{basis}).
#' @param temp_C Temperature(s) in degrees Celsius.
#' @param pres_bar Pressure(s) in bar.
#' @param eos Density EoS for \code{\link{co2dens}}: \code{"spanwagner"} (default) or \code{"bender"}.
#' @param basis Basis of \code{k}/\code{A}/\code{B}: \code{"mol/mol"} (default, database & GCM) or \code{"g/L"}.
#' @param mw Optional solute molar mass (g/mol) to convert between mol/mol and g/L.
#' @return A \code{data.frame} with \code{temp_C}, \code{pres_bar}, \code{rho} (g/L) and the
#'   solubility as \code{y_molmol} and/or \code{S_gL} (whichever the basis/\code{mw} allow).
#' @seealso \code{\link{chrastil_fit}}, \code{\link{co2dens}}
#' @export
chrastil_solubility <- function(k, A, B, temp_C, pres_bar, eos = c("spanwagner", "bender"),
                                basis = c("mol/mol", "g/L"), mw = NA) {
  eos <- match.arg(eos); basis <- match.arg(basis)
  grid <- expand.grid(pres_bar = pres_bar, temp_C = temp_C)
  grid$rho <- mapply(function(P, Tc) unname(co2dens(P, Tc, "g/L", eos)[["rho"]]), grid$pres_bar, grid$temp_C)
  base <- grid$rho^k * exp(A / (grid$temp_C + 273.15) + B)
  has_mw <- is.numeric(mw) && length(mw) == 1 && is.finite(mw) && mw > 0
  if (basis == "mol/mol") {
    grid$y_molmol <- base
    if (has_mw) grid$S_gL <- base * (mw / 44.01) * grid$rho
  } else {
    grid$S_gL <- base
    if (has_mw) grid$y_molmol <- base / ((mw / 44.01) * grid$rho)
  }
  grid
}

#' @title Fit Chrastil parameters to solubility data
#' @description Linear least-squares fit of \eqn{\ln S = k\,\ln\rho + A/T + B}. The CO2 density
#'   is taken from \code{\link{co2dens}} (so the fitted parameters are on that EoS's basis); pass
#'   a \code{rho} column to override. Returns the parameters and the average absolute relative
#'   deviation (AARD, %).
#' @param data A \code{data.frame} with the solubility \code{S} and either (\code{temp_C},
#'   \code{pres_bar}) or (\code{T_K}/\code{temp_C}, \code{rho}).
#' @param eos Density EoS used when \code{rho} is not supplied (default \code{"spanwagner"}).
#' @param robust If \code{TRUE}, down-weight outliers with an IRLS (Huber) refit (needs the data).
#' @return A \code{list}: \code{k}, \code{A}, \code{B}, \code{aard} (%), \code{r2}, \code{n}, and the
#'   per-point \code{fitted}/\code{rho}.
#' @export
chrastil_fit <- function(data, eos = c("spanwagner", "bender"), robust = FALSE) {
  eos <- match.arg(eos)
  d <- as.data.frame(data)
  if (!"S" %in% names(d) || any(!is.finite(d$S)) || any(d$S <= 0))
    stop("`data` needs a positive solubility column 'S'.", call. = FALSE)
  Tk <- if (!is.null(d$T_K)) d$T_K else if (!is.null(d$temp_C)) d$temp_C + 273.15 else
    stop("`data` needs 'temp_C' or 'T_K'.", call. = FALSE)
  rho <- if (!is.null(d$rho)) d$rho else {
    if (is.null(d$pres_bar) || is.null(d$temp_C)) stop("Need 'rho', or 'pres_bar' + 'temp_C' to compute it.", call. = FALSE)
    mapply(function(P, Tc) unname(co2dens(P, Tc, "g/L", eos)[["rho"]]), d$pres_bar, d$temp_C)
  }
  df <- data.frame(lnS = log(d$S), lnrho = log(rho), invT = 1 / Tk)
  fit <- if (robust && requireNamespace("robustbase", quietly = TRUE))
    robustbase::lmrob(lnS ~ lnrho + invT, data = df) else stats::lm(lnS ~ lnrho + invT, data = df)
  cf <- stats::coef(fit)
  k <- unname(cf["lnrho"]); A <- unname(cf["invT"]); B <- unname(cf["(Intercept)"])
  Spred <- rho^k * exp(A / Tk + B)
  list(k = k, A = A, B = B,
       aard = 100 * mean(abs(Spred - d$S) / d$S),
       r2 = suppressWarnings(stats::cor(log(d$S), log(Spred))^2),
       n = nrow(d), fitted = Spred, rho = rho)
}

#' @title Load the bundled Chrastil coefficient database
#' @description Reads the curated database (613 compounds, consolidated from "Chrastil revisited",
#'   J. Supercrit. Fluids 168 (2021) 105065). The \code{k}/\code{a}/\code{b} columns are the
#'   paper's \strong{mol/mol} (mole-fraction) coefficients on the Span-Wagner density basis -
#'   the same basis as \code{\link{chrastil_gcm_estimate}} - so they can be used together and
#'   converted to g/L with \eqn{k_{g/L}=k+1,\,A_{g/L}=A,\,B_{g/L}=B+\ln(MW/44.01)}.
#' @return A \code{data.frame} with columns including \code{name, cas, smiles, mw, k, a, b},
#'   the validity ranges, \code{aard}, \code{n_pts}, \code{density_eos} and \code{source}.
#' @export
chrastil_db <- function() {
  f <- system.file("extdata", "solubility", "chrastil_db.csv", package = "supeRcrit")
  if (!nzchar(f)) f <- file.path("inst", "extdata", "solubility", "chrastil_db.csv")   # dev fallback (load_all)
  utils::read.csv(f, stringsAsFactors = FALSE, check.names = FALSE, encoding = "UTF-8")
}

# Normalise a solute's Chrastil parameters to the mol/mol basis (so y = rho^k exp(A/T+B)).
.chrastil_to_molmol <- function(k, A, B, basis, mw) {
  if (basis == "g/L") {
    if (!(is.finite(mw) && mw > 0)) stop("A g/L-basis solute needs a molar mass to convert to mol/mol.", call. = FALSE)
    c(k = k - 1, A = A, B = B - log(mw / 44.01))
  } else c(k = k, A = A, B = B)
}

#' @title Simulate a supercritical-CO2 separator cascade (Chrastil fractionation)
#'
#' @description Models the depressurisation cascade used to fractionate an SFE extract. A CO2
#'   stream leaves the extractor carrying each solute, then passes through an ordered series of
#'   separator stages at progressively different temperature/pressure. At each stage the CO2
#'   density - and hence each solute's Chrastil solubility - changes; whatever exceeds the new
#'   saturation limit precipitates in that stage. Because solutes have different solubility
#'   curves, they deposit preferentially in different stages, which is the basis for fractionation.
#'   The model is an ideal equilibrium-stage cascade in the dilute limit (no solute-solute
#'   interaction, full saturation assumed leaving each stage).
#'
#' @param solutes A \code{data.frame} with one row per solute and columns \code{name}, \code{k},
#'   \code{A}, \code{B}, \code{mw} (g/mol) and optionally \code{basis} (\code{"mol/mol"} default or
#'   \code{"g/L"}) and \code{feed} (solute mass flow into the cascade, kg/h). When \code{feed} is
#'   absent it is set by saturation at \code{extract}.
#' @param stages A \code{data.frame} with columns \code{temp_C} and \code{pres_bar}, ordered as the
#'   CO2 flows through the separators (first row = first separator).
#' @param co2_flow CO2 mass flow rate (kg/h). Scales all absolute mass flows; recoveries are
#'   independent of it.
#' @param extract Optional \code{list(temp_C=, pres_bar=)} extraction conditions used to compute the
#'   saturated feed loading for any solute lacking an explicit \code{feed}.
#' @param eos Density EoS for \code{\link{co2dens}} (default \code{"spanwagner"}).
#'
#' @return A \code{list}: \code{deposit} (solute x stage matrix of precipitated kg/h), \code{recovery}
#'   (same, as a fraction of each solute's feed), \code{feed} (kg/h per solute), \code{residual}
#'   (kg/h leaving with the CO2 after the last stage), \code{stage_purity} (stage x solute mass
#'   fractions of each stage's deposit), and \code{tidy} (a long \code{data.frame} for plotting).
#' @seealso \code{\link{chrastil_solubility}}, \code{\link{chrastil_db}}
#' @noRd
chrastil_cascade <- function(solutes, stages, co2_flow = 1, extract = NULL,
                             eos = c("spanwagner", "bender")) {
  eos <- match.arg(eos)
  solutes <- as.data.frame(solutes, stringsAsFactors = FALSE)
  stages  <- as.data.frame(stages,  stringsAsFactors = FALSE)
  req_s <- c("name", "k", "A", "B", "mw"); miss <- setdiff(req_s, names(solutes))
  if (length(miss)) stop("`solutes` is missing column(s): ", paste(miss, collapse = ", "), call. = FALSE)
  if (!all(c("temp_C", "pres_bar") %in% names(stages))) stop("`stages` needs 'temp_C' and 'pres_bar'.", call. = FALSE)
  if (is.null(solutes$basis)) solutes$basis <- "mol/mol"
  if (is.null(solutes$feed))  solutes$feed  <- NA_real_

  nS <- nrow(stages); nC <- nrow(solutes)
  n_co2 <- co2_flow * 1000 / 44.01                                   # mol/h CO2
  rho_stage <- mapply(function(P, Tc) unname(co2dens(P, Tc, "g/L", eos)[["rho"]]),
                      stages$pres_bar, stages$temp_C)
  Tk_stage <- stages$temp_C + 273.15

  deposit <- matrix(0, nC, nS, dimnames = list(solutes$name, NULL))
  feed <- numeric(nC); residual <- numeric(nC)

  for (j in seq_len(nC)) {
    p <- .chrastil_to_molmol(solutes$k[j], solutes$A[j], solutes$B[j], solutes$basis[j], solutes$mw[j])
    ystage <- rho_stage^p["k"] * exp(p["A"] / Tk_stage + p["B"])     # mol/mol at each stage
    mmax <- ystage * n_co2 * solutes$mw[j] / 1000                    # kg/h carryable at each stage

    m0 <- solutes$feed[j]
    if (!is.finite(m0)) {
      if (is.null(extract)) stop("Solute '", solutes$name[j], "' has no feed and no `extract` conditions were given.", call. = FALSE)
      rho_e <- unname(co2dens(extract$pres_bar, extract$temp_C, "g/L", eos)[["rho"]])
      y_e <- rho_e^p["k"] * exp(p["A"] / (extract$temp_C + 273.15) + p["B"])
      m0 <- unname(y_e * n_co2 * solutes$mw[j] / 1000)
    }
    feed[j] <- m0
    carried <- m0
    for (i in seq_len(nS)) {
      precip <- max(0, carried - mmax[i])
      deposit[j, i] <- precip
      carried <- carried - precip
    }
    residual[j] <- carried
  }

  recovery <- deposit / ifelse(feed > 0, feed, NA_real_)
  stage_tot <- colSums(deposit)
  stage_purity <- t(deposit) / ifelse(stage_tot > 0, stage_tot, NA_real_)   # stage x solute
  dimnames(stage_purity) <- list(paste0("S", seq_len(nS)), solutes$name)

  tidy <- data.frame(
    solute = rep(solutes$name, times = nS),
    stage  = rep(seq_len(nS), each = nC),
    temp_C = rep(stages$temp_C, each = nC),
    pres_bar = rep(stages$pres_bar, each = nC),
    deposit_kg_h = as.vector(deposit),
    recovery = as.vector(recovery),
    stringsAsFactors = FALSE
  )

  list(deposit = deposit, recovery = recovery, feed = setNames(feed, solutes$name),
       residual = setNames(residual, solutes$name), stage_purity = stage_purity, tidy = tidy)
}
