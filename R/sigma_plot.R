# Sigma-profile visualisation (JCOSMO/COSMO-SAC style): total p(sigma)*A over the screening-charge
# grid, one curve per compound, with the hydrogen-bond donor/acceptor cutoff lines. Used to show
# FastSigma predictions and, for database compounds, predicted-vs-actual to gauge model performance.

# total (NHB+OH+OT) profile from a 153-bin psA vector, on the 51-bin sigma grid.
.sig_total <- function(psA) {
  if (length(psA) == 153L) psA[1:51] + psA[52:102] + psA[103:153]
  else if (length(psA) == 51L) psA
  else stop("profile must be length 51 or 153")
}

#' Plot one or more COSMO-SAC sigma profiles
#'
#' Draws the total sigma profile p(sigma)*A versus screening-charge density (sigma, e/A^2) in the
#' standard COSMO-SAC style, with the hydrogen-bond donor/acceptor cutoff lines. Pass several profiles
#' to overlay them (e.g. a FastSigma prediction against the database profile of the same compound).
#'
#' @param profiles a single 153- (or 51-) length area-per-bin vector, or a named list of them. List
#'   names become the legend labels.
#' @param labels optional character vector of legend labels (overrides list names).
#' @param sigma_hb hydrogen-bond cutoff (e/A^2) for the donor/acceptor guide lines (default 0.0084,
#'   the COSMO-SAC-dsp value).
#' @param smooth spline-interpolate each curve for a smooth line (default \code{TRUE}).
#' @param title optional plot title.
#' @return a \code{ggplot} object.
#' @seealso \code{\link{fastsigma_profile}}, \code{\link{csac_dataset}}.
#' @export
plot_sigma_profile <- function(profiles, labels = NULL, sigma_hb = 0.0084, smooth = TRUE, title = NULL) {
  if (!is.list(profiles)) profiles <- list(profiles)
  if (is.null(labels)) labels <- names(profiles)
  if (is.null(labels)) labels <- paste("profile", seq_along(profiles))
  grid <- seq(-0.025, 0.025, by = 0.001)
  df <- do.call(rbind, lapply(seq_along(profiles), function(i) {
    tot <- .sig_total(profiles[[i]])
    if (smooth) {
      sp <- stats::spline(grid, tot, n = 400)
      d <- data.frame(sigma = sp$x, pA = pmax(sp$y, 0))
    } else d <- data.frame(sigma = grid, pA = tot)
    d$compound <- labels[i]; d
  }))
  df$compound <- factor(df$compound, levels = labels)
  ann_y <- max(df$pA) * 0.62
  ggplot2::ggplot(df, ggplot2::aes(x = sigma, y = pA, colour = compound)) +
    ggplot2::geom_vline(xintercept = c(-sigma_hb, sigma_hb), colour = "grey55", linewidth = 0.4) +
    ggplot2::annotate("text", x = -sigma_hb, y = ann_y, label = "HB donor", colour = "grey45",
                      angle = 90, vjust = -0.4, size = 3.2) +
    ggplot2::annotate("text", x = sigma_hb, y = ann_y, label = "HB accep.", colour = "grey45",
                      angle = 90, vjust = -0.4, size = 3.2) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::scale_x_continuous(limits = c(-0.025, 0.025)) +
    ggplot2::labs(x = expression(sigma ~ "[e/\u00c5\u00b2]"), y = expression(p(sigma) %.% A ~ "[\u00c5\u00b2]"),
                  colour = NULL, title = title) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(legend.position = c(0.82, 0.9), legend.background = ggplot2::element_rect(fill = "white", colour = NA),
                   panel.grid.minor = ggplot2::element_blank())
}

#' Sigma profile for a compound, from a database and/or FastSigma
#'
#' Convenience accessor for plotting: returns the bundled (true) profile and/or the FastSigma
#' prediction for a compound, ready to pass to \code{\link{plot_sigma_profile}}.
#'
#' @param compound name/key/CAS/SMILES (database) or a SMILES (for prediction).
#' @param database \code{"ud"} or \code{"chaos"} for the bundled profile lookup.
#' @param predict also compute the FastSigma prediction (needs a resolvable SMILES).
#' @return named list of 153-bin profiles (\code{database} and/or \code{predicted}); empty if neither.
#' @seealso \code{\link{plot_sigma_profile}}.
#' @export
csac_profile <- function(compound, database = c("ud", "chaos"), predict = TRUE) {
  database <- match.arg(database); d <- csac_dataset(database)
  out <- list(); smi <- compound
  i <- .csac_match(compound, d)
  if (!is.null(i)) {
    out[["database"]] <- d$psA[i, ]
    smv <- d$meta$smiles[i]
    if (!is.null(smv) && !is.na(smv) && nzchar(smv)) smi <- smv
  }
  if (isTRUE(predict)) {
    p <- tryCatch(fastsigma_profile(smi), error = function(e) NULL)
    if (!is.null(p)) out[["predicted"]] <- p$psA
  }
  out
}
