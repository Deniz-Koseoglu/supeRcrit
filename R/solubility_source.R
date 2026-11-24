#' @title Bundled supercritical-CO2 solubility source data
#'
#' @description Raw experimental supercritical-CO2 solubility measurements compiled from 69 primary
#'   experimental papers — the source data underlying the Chrastil-Revisited correlations
#'   (\code{\link{chrastil_db}}) — with one row per measured point. Intended both as a reference
#'   dataset (browsable via the app's "Solubility source data" viewer) and as a target for fitting
#'   predictive models (openCOSMO-RS-Phi / CSP) directly to measurements rather than to a correlation.
#'
#' @details Columns:
#'   \describe{
#'     \item{source}{Full citation of the primary paper.}
#'     \item{solute, solute_cas}{Solute name as printed and its CAS (where reported).}
#'     \item{cosolvent, cosolvent_fraction}{Co-solvent/entrainer and its concentration (\code{NA} for a
#'       pure-CO2 binary).}
#'     \item{T_K, P_bar}{Temperature (K) and pressure (bar), normalised from the paper's units.}
#'     \item{y}{Solute mole fraction, or \code{NA} when the paper reported a non-mole-fraction basis
#'       (g/L, mass fraction, ...) that needs a molar mass / density to convert.}
#'     \item{y_raw, solubility_basis}{The value exactly as printed and its reported basis.}
#'     \item{phase}{\code{"SFV"} (normal solid/liquid-fluid solubility), \code{"near_UCEP_liquid"}
#'       (anomalous near-critical liquid-phase composition — exclude when fitting solid solubility),
#'       or \code{NA}.}
#'     \item{matched_name, in_chrastil}{This package's experimental-DB compound the solute was matched
#'       to (or \code{NA}), and whether that compound carries Chrastil coefficients.}
#'     \item{flag}{Soft sanity notes (e.g. unusually high T, non-standard pressure unit).}
#'   }
#'   Non-CO2 supercritical-solvent systems (e.g. the ethane / fluoroform data in Schmitt & Reid) and a
#'   duplicated source file were excluded during consolidation. Built by
#'   \code{data-raw/solubility_src/consolidate.R} from the per-paper extraction JSONs.
#'
#' @return A \code{data.frame} of roughly 5000 measured scCO2 solubility points.
#' @seealso \code{\link{chrastil_db}}, \code{\link{chrastil_solubility}}, \code{\link{ocrsphi_solubility}}
#' @export
solubility_source_data <- function() {
  f <- system.file("extdata", "solubility", "solubility_source_data.rds", package = "supeRcrit")
  if (!nzchar(f)) f <- file.path("inst", "extdata", "solubility", "solubility_source_data.rds")   # dev fallback (load_all)
  readRDS(f)
}
