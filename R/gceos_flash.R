#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# GC-EOS TWO-PHASE FLASH
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Isothermal-isobaric (T, P) vapor-liquid flash over the compiled GC-EOS core.
# Successive substitution on the K-values with trivial-solution rejection and
# multiple physical initial guesses. The building block for distribution
# coefficients, relative volatilities and (later) the counter-current column.

# Volume root of P(T,V,n) = Ptarget for one phase: liquid = smallest real root,
# vapor = largest. Thin wrapper over the compiled C++ root finder (the hot path).
.gceos_vroot <- function(T, Ptarget, n, x, want, R = GCEOS_R) {
  gceos_vroot(T, Ptarget, n, x$d, x$q, x$nu, x$g, x$dg, x$alpha, R,
              if (want == "liq") 0L else 1L, x$numu, x$Delta0)
}

.gceos_lnphi_phase <- function(T, Ptarget, frac, x, want) {
  V <- .gceos_vroot(T, Ptarget, frac, x, want)
  if (is.na(V)) return(rep(NA_real_, length(frac)))
  gceos_lnphi(T, V, frac, x$d, x$q, x$nu, x$g, x$dg, x$alpha, GCEOS_R, x$numu, x$Delta0)
}

# one successive-substitution run from a given initial K (returns NULL on failure
# or the trivial x==y solution)
.gceos_flash_run <- function(T, Patm, z, x, K, maxit) {
  xl <- yv <- beta <- NULL
  for (it in seq_len(maxit)) {
    rr <- function(bb) sum(z * (K - 1) / (1 + bb * (K - 1)))
    if (rr(0) * rr(1) > 0) return(NULL)                 # single phase for this K
    beta <- stats::uniroot(rr, c(1e-8, 1 - 1e-8))$root
    xl <- z / (1 + beta * (K - 1)); xl <- xl / sum(xl)
    yv <- K * z / (1 + beta * (K - 1)); yv <- yv / sum(yv)
    lpL <- .gceos_lnphi_phase(T, Patm, xl, x, "liq")
    lpV <- .gceos_lnphi_phase(T, Patm, yv, x, "vap")
    if (anyNA(c(lpL, lpV))) return(NULL)
    Knew <- exp(lpL - lpV)
    if (max(abs(log(Knew / K))) < 1e-9) { K <- Knew; break }
    K <- Knew
  }
  if (max(abs(log(K))) < 0.05) return(NULL)             # trivial (x == y) solution
  list(K = K, x = xl, y = yv, beta = beta)
}

#' @title Isothermal-isobaric two-phase (VLE) flash with the GC-EOS
#'
#' @description
#' Flashes a mixture of overall composition \code{z} at temperature \code{T} and
#' pressure \code{P} into equilibrium liquid and vapor phases using the GC-EOS,
#' returning the distribution coefficients \eqn{K_i = y_i/x_i}, the phase
#' compositions and the vapor fraction. Uses successive substitution from several
#' physical initial \eqn{K} guesses and rejects the trivial (\eqn{x=y}) solution.
#'
#' @param T Temperature (K).
#' @param P Pressure (atm).
#' @param z Overall (feed) mole fractions, in the same order as \code{comps}.
#' @param comps List of components; each a \code{list} with \code{groups} (named
#'   integer group counts), \code{dc}, \code{Tc} (and \code{n}, ignored here).
#' @param model \code{"gceos"} (default), \code{"gcaeos"}, or a parameter-set list.
#' @param Kinit Optional vector of initial \eqn{K} values; otherwise a set of
#'   physical defaults is tried.
#'
#' @return A \code{list} with \code{K}, \code{x} (liquid), \code{y} (vapor) and
#'   \code{beta} (vapor mole fraction), or \code{NULL} if no two-phase split is
#'   found. Names follow \code{comps}.
#' @export
#'
#' @examples
#' \dontrun{
#' diaz <- gceos_params("diaz2005")
#' mk <- function(nm) { g <- diaz$decomp[nm, ]; g <- g[g > 0]
#'   list(groups = g, dc = diaz$components[nm, "dc"], Tc = diaz$components[nm, "Tc"]) }
#' cps <- list(limonene = mk("limonene"), linalool = mk("linalool"), CO2 = mk("CO2"))
#' fl <- gceos_flash(333, 8.8e6/101325, c(0.123, 0.027, 0.85), cps, diaz)
#' fl$K[1] / fl$K[2]          # limonene/linalool relative volatility
#' }
#' @seealso \code{\link{gceos_eval}}, \code{\link{gceos_psat}}
gceos_flash <- function(T, P, z, comps, model = "gceos", Kinit = NULL) {
  z <- z / sum(z)
  x <- .gceos_inputs(T, comps, model)
  inits <- if (!is.null(Kinit)) list(Kinit) else
    list(c(0.03, 0.015, 4), c(0.01, 0.004, 6), c(0.1, 0.05, 3), c(0.005, 0.002, 10))
  # pad/truncate default guesses to the number of components
  inits <- lapply(inits, function(k) { if (length(k) == length(z)) k else
    stats::setNames(rep(k, length.out = length(z)), NULL) })
  for (K0 in inits) {
    r <- .gceos_flash_run(T, P, z, x, K0, maxit = 300)
    if (!is.null(r)) {
      nm <- names(comps)
      r$K <- stats::setNames(r$K, nm); r$x <- stats::setNames(r$x, nm); r$y <- stats::setNames(r$y, nm)
      return(r)
    }
  }
  NULL
}

#' @title Two- or three-phase (VLLE) flash with the GC(A)-EOS
#'
#' @description
#' Flashes a mixture of overall composition \code{z} at \code{(T, P)} and returns
#' the number of equilibrium phases together with their compositions. A robust
#' two-phase VLE flash runs first; a Michelsen tangent-plane stability test then
#' escalates to a three-phase vapor-liquid-liquid split where one exists (e.g.
#' water + organic + CO2). A thin R wrapper over the compiled \code{gceos_flash3}.
#'
#' @param T Temperature (K).
#' @param P Pressure (atm).
#' @param z Overall (feed) mole fractions, in the same order as \code{comps}.
#' @param comps List of components (each \code{list(groups, dc, Tc)}); see
#'   \code{\link{gceos_flash}}.
#' @param model \code{"gceos"} (default), \code{"gcaeos"}, or a parameter-set list.
#' @param maxit Maximum successive-substitution iterations.
#'
#' @return A \code{list} with \code{nphase} (1, 2 or 3) and \code{status}. For two
#'   phases: \code{beta} (vapor/second-phase fraction), \code{x}, \code{y}. For
#'   three phases: \code{beta_V}, \code{beta_L1}, \code{beta_L2}, \code{y},
#'   \code{x1}, \code{x2}. Compositions are named after \code{comps}.
#' @export
#' @seealso \code{\link{gceos_flash}}
# Convert the raw C++ gceos_flash3 result to the named nphase list. Internal,
# shared by gceos_flash3_solve and the (input-cached) VLLE column relaxation.
.gceos_flash3_post <- function(r, nm) {
  setn <- function(v) stats::setNames(as.numeric(v), nm)
  st <- r$status
  if (is.null(st) || st < 1) return(list(nphase = NA_integer_, status = -1L))
  np <- if (!is.null(r$nphase)) r$nphase else st
  if (np == 1) return(list(nphase = 1L, status = 1L))
  if (np == 2) return(list(nphase = 2L, status = 2L, beta = r$beta, x = setn(r$x), y = setn(r$y)))
  list(nphase = 3L, status = 3L, beta_V = r$beta_V, beta_L1 = r$beta_L1, beta_L2 = r$beta_L2,
       y = setn(r$y), x1 = setn(r$x1), x2 = setn(r$x2))
}

gceos_flash3_solve <- function(T, P, z, comps, model = "gceos", maxit = 300L) {
  z <- z / sum(z); nm <- names(comps)
  x <- .gceos_inputs(T, comps, model)
  r <- gceos_flash3(T, P, z, x$d, x$q, x$nu, x$g, x$dg, x$alpha, GCEOS_R,
                    as.integer(maxit), x$numu, x$Delta0)
  .gceos_flash3_post(r, nm)
}
