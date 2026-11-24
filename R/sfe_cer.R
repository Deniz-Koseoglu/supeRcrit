#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FUNCTION: Feed loading from a supercritical-extraction (overall) curve
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Estimate the scCO2 feed loading from an overall extraction curve
#'
#' @description
#' The slope of an overall extraction curve (cumulative extract vs. cumulative CO2) is the instantaneous
#' \strong{loading} (g extract per kg CO2). Its initial, roughly-linear \strong{constant-extraction-rate
#' (CER)} portion is the period during which the CO2 leaves the vessel essentially \emph{saturated}, so the
#' CER slope is the near-saturation loading a downstream separator sees during peak extraction — exactly the
#' feed loading required by \code{\link{sfe_frac}}. This function converts a curve (in several common axis
#' conventions) to a canonical \eqn{(\text{kg CO}_2,\ \text{g extract})} basis, auto-detects the CER window,
#' and returns the CER loading (and, given a target-solute weight fraction, the target-solute loading).
#'
#' @param x,y Numeric vectors of equal length: the extraction curve, sorted or unsorted by \code{x}.
#' @param x_type Axis of \code{x}: \code{"co2_mass"} (cumulative CO2, kg), \code{"sf"} (solvent-to-feed
#'   mass ratio, kg CO2 / kg feed), or \code{"time"} (min).
#' @param y_type Axis of \code{y}: \code{"mass_g"} (cumulative extract, g), \code{"yield_frac"}
#'   (g extract / g feed) or \code{"yield_pct"} (% of feed).
#' @param co2_flow CO2 mass flow (kg/min). \strong{Required} when \code{x_type = "time"}.
#' @param feed_mass Charge (feed) mass (kg). \strong{Required} when \code{x_type = "co2_mass"} or
#'   \code{"time"} \emph{and} \code{y_type} is a yield. Not needed for the \code{"sf"} axis (it cancels).
#' @param tq_frac Optional target-solute weight fraction in the extract (0-1). If supplied, the target-solute
#'   loading \code{tq_loading = Y_cer * tq_frac} is returned.
#' @param cer_end Optional manual end of the CER (linear) window: an integer point index (into the
#'   x-sorted curve) or, if \code{cer_end_is_x = TRUE}, an \code{x} value. \code{NULL} auto-detects.
#' @param cer_end_is_x Treat \code{cer_end} as an \code{x} value rather than a point index.
#' @param r2_min Retained for backward compatibility; unused by the current max-slope-band detector.
#' @param cer_band Fraction (0-1, default 0.8) of the maximum per-segment slope defining the
#'   constant-rate band: the CER window is the contiguous run of segments whose slope is at least
#'   \code{cer_band} times the steepest segment's slope, taken around that steepest segment.
#' @param include_origin Anchor the fit at the origin \eqn{(0, 0)} (default \code{TRUE}) when the first
#'   sampled point is downstream of it, since the curve physically starts at zero. This recovers the true
#'   near-origin CER slope for fast, coarsely-sampled curves whose constant-rate period is nearly over by
#'   the first sample.
#'
#' @return A \code{list}:
#' \describe{
#'   \item{Y_cer}{CER (near-saturation) loading, g extract / kg CO2.}
#'   \item{tq_loading}{Target-solute loading (g/kg CO2), or \code{NA} when \code{tq_frac} is \code{NULL}.}
#'   \item{Y_avg}{Whole-run average loading (total extract / total CO2), for comparison.}
#'   \item{cer_n}{Number of leading points that make up the CER window.}
#'   \item{fit}{\code{list(slope, intercept, r2)} of the CER-window linear fit.}
#'   \item{curve}{A tidy \code{data.frame} (\code{m_co2_kg}, \code{extract_g}, \code{loading}, \code{is_cer}).}
#' }
#' @seealso \code{\link{sfe_frac}}
#' @examples
#' \dontrun{
#' # S/F vs yield(%) curve; feed mass not needed
#' sf  <- c(0, 5, 10, 20, 35, 55, 80, 120)
#' yld <- c(0, 1.2, 2.4, 4.8, 7.1, 8.4, 9.0, 9.3)      # % of feed
#' r <- sfe_cer_loading(sf, yld, x_type = "sf", y_type = "yield_pct", tq_frac = 0.02)
#' r$Y_cer; r$tq_loading
#' }
#' @noRd
sfe_cer_loading <- function(x, y, x_type = c("co2_mass", "sf", "time"),
                            y_type = c("mass_g", "yield_frac", "yield_pct"),
                            co2_flow = NULL, feed_mass = NULL, tq_frac = NULL,
                            cer_end = NULL, cer_end_is_x = FALSE, r2_min = 0.998,
                            include_origin = TRUE, cer_band = 0.8) {
  x_type <- match.arg(x_type); y_type <- match.arg(y_type)
  x <- suppressWarnings(as.numeric(x)); y <- suppressWarnings(as.numeric(y))
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]; y <- y[ok]
  if (length(x) < 3) stop("Need at least 3 finite (x, y) points.", call. = FALSE)
  o <- order(x); x <- x[o]; y <- y[o]
  if (any(diff(x) <= 0)) {                                   # collapse/deduplicate non-increasing x
    keep <- c(TRUE, diff(x) > 0); x <- x[keep]; y <- y[keep]
    if (length(x) < 3) stop("Need at least 3 points with strictly increasing x.", call. = FALSE)
  }

  # --- canonicalise to (m_co2 in kg, extract in g) ------------------------------
  # 'sf' cancels the feed mass, so a dummy 1 kg feed is exact.
  feed_kg <- feed_mass
  m_co2 <- switch(x_type,
    co2_mass = x,
    sf       = { feed_kg <- 1; x * feed_kg },
    time     = { if (is.null(co2_flow) || !isTRUE(is.finite(co2_flow)) || co2_flow <= 0)
                   stop("x_type = 'time' requires a positive co2_flow (kg/min).", call. = FALSE)
                 co2_flow * x })
  yfrac <- switch(y_type, mass_g = NULL, yield_frac = y, yield_pct = y / 100)
  extract_g <- if (y_type == "mass_g") y else {
    if (is.null(feed_kg) || !isTRUE(is.finite(feed_kg)) || feed_kg <= 0)
      stop("A yield y-axis with this x-axis requires a positive feed_mass (kg).", call. = FALSE)
    yfrac * feed_kg * 1000
  }
  # Physically the extraction curve passes through the origin (no CO2 -> no extract); include it so a fast,
  # coarsely-sampled curve (whose constant-rate period is over by the first sample) still exposes a
  # near-origin segment.
  if (isTRUE(include_origin) && m_co2[1] > 0) { m_co2 <- c(0, m_co2); extract_g <- c(0, extract_g) }
  n <- length(m_co2)

  # --- CER window: the CONSTANT-EXTRACTION-RATE segment = the steepest sustained (near-maximal) slope band.
  # Using the max-slope band (rather than the first linear run) handles all shapes: fast (CER at the origin),
  # normal (CER at the start), and lagged/S-shaped curves (CER after an induction period). ------------------
  sl <- diff(extract_g) / diff(m_co2)                          # per-segment slopes (length n-1)
  if (!is.null(cer_end)) {
    cer_n <- if (isTRUE(cer_end_is_x)) sum(m_co2 <= cer_end) else as.integer(cer_end)
    cer_n <- max(2L, min(cer_n, n)); cer_idx <- seq_len(cer_n)
  } else {
    smax <- max(sl, na.rm = TRUE)
    band <- is.finite(sl) & sl >= cer_band * smax             # segments within the constant-rate band
    imax <- which.max(sl)                                     # the single steepest segment
    rl <- rle(band); ends <- cumsum(rl$lengths); starts <- ends - rl$lengths + 1L
    ri <- which(starts <= imax & ends >= imax)[1]             # the contiguous near-max run around it
    cer_idx <- seq.int(starts[ri], ends[ri] + 1L)             # segment indices -> point indices
    cer_n <- length(cer_idx)
  }
  fit <- stats::lm(extract_g[cer_idx] ~ m_co2[cer_idx])
  slope <- unname(stats::coef(fit)[2]); intercept <- unname(stats::coef(fit)[1])
  r2 <- suppressWarnings(summary(fit)$r.squared)

  Y_cer <- slope                                             # g / kg (extract_g per m_co2_kg)
  Y_avg <- extract_g[n] / m_co2[n]
  tqL <- if (!is.null(tq_frac) && isTRUE(is.finite(tq_frac)) && tq_frac > 0) Y_cer * tq_frac else NA_real_

  curve <- data.frame(m_co2_kg = m_co2, extract_g = extract_g,
                      loading = c(NA_real_, sl),
                      is_cer = seq_len(n) %in% cer_idx)
  list(Y_cer = Y_cer, tq_loading = tqL, Y_avg = Y_avg, cer_n = cer_n,
       fit = list(slope = slope, intercept = intercept, r2 = r2), curve = curve)
}
