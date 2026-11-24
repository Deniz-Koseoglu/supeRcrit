#' Load the bundled openCOSMO-RS-Phi global validation dataset
#'
#' Returns point-level solubility predictions, route-specific candidate fits,
#' summary statistics, references, coverage, and metadata.  The
#' `evaluation_role` field distinguishes independent Psat-route validation,
#' raw-solubility holdout points, and raw-solubility training points.
#'
#' @return A list with `predictions`, `summary`, `references`, `candidates`,
#'   `coverage`, and `meta`.
#' @export
ocrsphi_validation <- function() {
  f <- system.file("extdata", "solubility", "ocrsphi_global_validation.rds", package = "supeRcrit")
  if (!nzchar(f)) f <- file.path("inst", "extdata", "solubility", "ocrsphi_global_validation.rds")
  if (!file.exists(f)) {
    warning("openCOSMO-RS-Phi validation dataset not found. Run data-raw/ocrsphi/build_global_validation_bundle.R.",
            call. = FALSE)
    return(NULL)
  }
  readRDS(f)
}
