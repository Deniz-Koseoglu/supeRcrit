#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PHASE BOUNDARIES: bubble / dew points and isothermal P-x diagrams
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Maps the edges of the two-phase region from the GC(A)-EOS. A bubble point is
# the pressure at which the first vapor appears from a liquid of composition x
# (sum K_i x_i = 1); a dew point the pressure at which the first liquid appears
# from a vapor y (sum y_i / K_i = 1). The P-x sweep traces the whole envelope.

# physical K spread for the inner SS init (lightest species -> high K),
# matching the C++ kinit_phys so the inner loop starts in the right basin.
.kphys <- function(d, lowK, highK) {
  rg <- max(d) - min(d); if (rg < 1e-12) rg <- 1
  frac <- (d - min(d))/rg
  exp(log(highK)*(1 - frac) + log(lowK)*frac)
}

# converged sum_i K_i*spec_i at (T,P): mode "bubble" -> spec=x liquid, returns
# sum K x ; mode "dew" -> spec=y vapor, returns sum y/K. Returns NULL on root
# failure; sets triv=TRUE when the K collapse to the trivial x=y solution (i.e.
# the state is single-phase / past the critical point at this P), which would
# otherwise spuriously satisfy the saturation condition with K=1.
.satS <- function(T, P, spec, xin, mode, Kinit = NULL, maxit = 40L, tol = 1e-9) {
  R <- GCEOS_R; K <- if (!is.null(Kinit)) Kinit else .kphys(xin$d, 0.01, 4)
  for (it in seq_len(maxit)) {
    if (mode == "bubble") { other <- K*spec; other <- other/sum(other); xl <- spec; yv <- other }
    else                  { other <- spec/K; other <- other/sum(other); xl <- other; yv <- spec }
    Kn <- gceos_stage_K(T, P, xl, yv, xin$d, xin$q, xin$nu, xin$g, xin$dg, xin$alpha, R, xin$numu, xin$Delta0)
    if (anyNA(Kn) || any(!is.finite(Kn)) || any(Kn <= 0)) return(NULL)
    if (max(abs(log(Kn/K))) < tol) { K <- Kn; break }
    K <- Kn
  }
  S <- if (mode == "bubble") sum(K*spec) else sum(spec/K)
  list(K = K, S = S, triv = (max(abs(log(K))) < 0.02))
}

#' @title Bubble or dew pressure from the GC(A)-EOS
#' @description At fixed temperature, the pressure bounding the two-phase region:
#'   the bubble point for a liquid of composition \code{comp}, or the dew point
#'   for a vapor of composition \code{comp}.
#' @param T Temperature (K).
#' @param comp Phase composition (mole fractions): the liquid for \code{"bubble"},
#'   the vapor for \code{"dew"}.
#' @param comps,model Component list and GC-EOS model (as in \code{gceos_flash}).
#' @param type \code{"bubble"} or \code{"dew"}.
#' @param Prange Pressure search bracket (atm).
#' @return A \code{list} with \code{P} (atm), the incipient phase composition and
#'   \code{K}, or \code{NULL} if no boundary is found in \code{Prange}.
#' @export
gceos_saturation <- function(T, comp, comps, model = "gceos",
                             type = c("bubble", "dew"), Prange = c(1, 800),
                             Kinit = NULL, xin = NULL) {
  type <- match.arg(type); comp <- comp/sum(comp)
  if (is.null(xin)) xin <- .gceos_inputs(T, comps, model)
  nms <- names(comps)
  # FAST PATH: the whole bubble/dew solve in C++ (scan + bisection + inner SS),
  # ~20x faster than the R loop below. Falls through to the R version on failure.
  Pc <- tryCatch(gceos_satP(T, comp, if (type == "bubble") 0L else 1L,
           xin$d, xin$q, xin$nu, xin$g, xin$dg, xin$alpha, GCEOS_R,
           Prange[1], Prange[2], numu = xin$numu, Delta0 = xin$Delta0),
         error = function(e) NA_real_)
  if (is.finite(Pc)) {
    rr <- .satS(T, Pc, comp, xin, type, if (!is.null(Kinit)) Kinit else .kphys(xin$d, 0.01, 4))
    if (!is.null(rr) && !rr$triv) {
      inc <- if (type == "bubble") { y <- rr$K*comp; y/sum(y) } else { x <- comp/rr$K; x/sum(x) }
      return(list(P = Pc, comp = stats::setNames(comp, nms),
                  incipient = stats::setNames(inc, nms),
                  K = stats::setNames(as.numeric(rr$K), nms), type = type))
    }
  }
  # f(P): below the boundary a real (non-trivial) two-phase split gives S>1 ->
  # f>0; above it the split is single-phase/trivial -> f<0. The boundary is where
  # the non-trivial S = 1. A trivial collapse is treated as f<0 (we are past it).
  # lastK warm-starts the inner SS from the previous pressure (K varies slowly).
  lastK <- if (!is.null(Kinit)) Kinit else .kphys(xin$d, 0.01, 4)
  f <- function(P) { r <- .satS(T, P, comp, xin, type, lastK)
    if (is.null(r)) return(NA_real_)
    if (!r$triv) lastK <<- r$K
    if (r$triv) -0.5 else r$S - 1 }
  Ps <- exp(seq(log(Prange[1]), log(Prange[2]), length.out = 16))
  fs <- vapply(Ps, f, numeric(1))
  ok <- which(is.finite(fs)); if (length(ok) < 2) return(NULL)
  br <- NULL                                              # first +/- sign change = the boundary
  for (k in seq_along(ok)[-1]) { a <- ok[k-1]; b <- ok[k]
    if (fs[a] > 0 && fs[b] < 0) { br <- c(Ps[a], Ps[b]); break } }
  if (is.null(br)) return(NULL)
  P <- tryCatch(stats::uniroot(f, br, tol = 1e-6)$root, error = function(e) NA_real_)
  if (is.na(P)) return(NULL)
  rr <- .satS(T, P, comp, xin, type, lastK); if (is.null(rr) || rr$triv) return(NULL); K <- rr$K
  inc <- if (type == "bubble") { y <- K*comp; y/sum(y) } else { x <- comp/K; x/sum(x) }
  nms <- names(comps)
  list(P = P, comp = stats::setNames(comp, nms), incipient = stats::setNames(inc, nms),
       K = stats::setNames(as.numeric(K), nms), type = type)
}

#' @title Isothermal P-x phase envelope of a binary
#' @description Sweeps the liquid mole fraction of component 1 and returns the
#'   bubble pressure and the coexisting vapor composition at each point, tracing
#'   the two-phase envelope (bubble curve in \code{x1}, dew curve in \code{y1}).
#' @param T Temperature (K).
#' @param comps Two-component list; \code{model} as in \code{gceos_flash}.
#' @param model GC-EOS model.
#' @param x1 Liquid mole fractions of component 1 to sweep (defaults to a grid).
#' @param Prange Pressure search bracket (atm).
#' @return A \code{data.frame} with columns \code{x1}, \code{y1}, \code{P} (atm).
#' @export
gceos_pxy <- function(T, comps, model = "gceos",
                      x1 = seq(0.02, 0.98, by = 0.04), Prange = c(1, 800)) {
  stopifnot(length(comps) == 2)
  xin <- .gceos_inputs(T, comps, model); lastK <- NULL
  out <- lapply(x1, function(xc) {
    s <- gceos_saturation(T, c(xc, 1 - xc), comps, model, "bubble", Prange, Kinit = lastK, xin = xin)
    if (is.null(s)) c(x1 = xc, y1 = NA, P = NA)
    else { lastK <<- as.numeric(s$K); c(x1 = xc, y1 = s$incipient[1], P = s$P) }
  })
  d <- as.data.frame(do.call(rbind, out)); names(d) <- c("x1", "y1", "P"); d
}

#' @title Mixture critical point of a binary at fixed temperature
#' @description Locates the critical point as the apex of the isothermal P-x
#'   envelope -- the highest bubble pressure, where the coexisting vapor and
#'   liquid compositions merge (\eqn{y \to x}). This is the pressure above which
#'   the two phases become identical and selectivity vanishes.
#' @param T Temperature (K).
#' @param comps Two-component list; component 1 is the volatile (e.g. CO2).
#' @param model GC-EOS model.
#' @param xrange Liquid mole-fraction range of component 1 to search.
#' @param Prange Pressure search bracket (atm).
#' @return A \code{list} with \code{T}, \code{x_c}, \code{y_c}, \code{P_c} (atm),
#'   or \code{NULL} if no apex is found.
#' @export
gceos_critical <- function(T, comps, model = "gceos",
                           xrange = c(0.70, 0.998), Prange = c(1, 1000), Kinit = NULL,
                           nrefine = c(12L, 8L)) {
  stopifnot(length(comps) == 2)
  xin <- .gceos_inputs(T, comps, model); lastK <- Kinit
  Pof <- function(xc) { s <- gceos_saturation(T, c(xc, 1 - xc), comps, model, "bubble", Prange, Kinit = lastK, xin = xin)
                        if (is.null(s)) c(NA, NA, NA) else { lastK <<- as.numeric(s$K); c(s$P, s$incipient[1], s$K[1]) } }
  refine <- function(lo, hi, n) {
    xs <- seq(lo, hi, length.out = n); m <- t(vapply(xs, Pof, numeric(3)))
    ok <- is.finite(m[, 1]); if (sum(ok) < 1) return(NULL)
    xs <- xs[ok]; m <- m[ok, , drop = FALSE]; i <- which.max(m[, 1])
    list(x = xs[i], P = m[i, 1], y = m[i, 2], K1 = m[i, 3], lo = xs[max(i-1,1)], hi = xs[min(i+1,length(xs))])
  }
  c1 <- refine(xrange[1], xrange[2], nrefine[1]); if (is.null(c1)) return(NULL)   # coarse
  c2 <- refine(c1$lo, c1$hi, nrefine[2])                                          # fine around the apex
  if (is.null(c2)) c2 <- c1
  list(T = T, x_c = c2$x, y_c = c2$y, P_c = c2$P, K = lastK)
}

#' @title Critical locus (P-T) of a binary
#' @description The mixture critical point as a function of temperature -- the
#'   curve bounding the region where a two-phase split (and hence separation) is
#'   possible. Above the critical locus the mixture is one fluid.
#' @inheritParams gceos_critical
#' @param Tvec Temperatures (K) to sweep.
#' @return A \code{data.frame} with \code{T}, \code{x_c}, \code{P_c} (atm, MPa).
#' @export
gceos_critical_locus <- function(comps, model = "gceos", Tvec,
                                 xrange = c(0.70, 0.998), Prange = c(1, 1000)) {
  lastK <- NULL; lastx <- NULL
  rows <- lapply(Tvec, function(T) {
    # continuation: the critical x moves slowly with T, so search a narrow window
    # around the previous point after the first (faster and more robust).
    xr <- if (is.null(lastx)) xrange else c(max(xrange[1], lastx - 0.06), min(xrange[2], lastx + 0.06))
    nr <- if (is.null(lastx)) c(12L, 8L) else c(8L, 5L)   # narrow window -> fewer points
    cc <- gceos_critical(T, comps, model, xr, Prange, Kinit = lastK, nrefine = nr)
    if (is.null(cc)) c(T = T, x_c = NA, P_atm = NA)
    else { lastK <<- cc$K; lastx <<- cc$x_c; c(T = T, x_c = cc$x_c, P_atm = cc$P_c) } })
  d <- as.data.frame(do.call(rbind, rows)); names(d) <- c("T", "x_c", "P_atm")
  d$P_MPa <- d$P_atm * 101325 / 1e6; d
}
