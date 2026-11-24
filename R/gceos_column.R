#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# GC-EOS COUNTER-CURRENT MULTISTAGE EXTRACTION COLUMN
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Isothermal-isobaric equilibrium-stage (MESH) model of a high-pressure
# counter-current SFE column, solved by the sum-rates method with a per-component
# tridiagonal (Thomas) solve. Stage K-values come from the GC-EOS core
# (phi^L/phi^V at each stage's liquid/vapor composition). This is "Layer 2": the
# process-design layer for deterpenation, deacidification, squalene recovery, etc.
#
# Convention: stage 1 = top, stage N = bottom. Liquid flows down, supercritical
# vapor (solvent) flows up. The liquid (oil) feed normally enters at/near the top
# and the solvent (CO2) feed at the bottom; the vapor leaving stage 1 is the
# EXTRACT and the liquid leaving stage N is the RAFFINATE.


# Thomas algorithm for a tridiagonal system (sub a, diag b, super c, rhs d).
.thomas <- function(a, b, c, d) {
  n <- length(b); cp <- numeric(n); dp <- numeric(n)
  cp[1] <- c[1]/b[1]; dp[1] <- d[1]/b[1]
  for (j in 2:n) {
    m <- b[j] - a[j]*cp[j-1]
    cp[j] <- c[j]/m
    dp[j] <- (d[j] - a[j]*dp[j-1])/m
  }
  x <- numeric(n); x[n] <- dp[n]
  for (j in (n-1):1) x[j] <- dp[j] - cp[j]*x[j+1]
  x
}

#' @title Counter-current multistage SFE column (GC-EOS)
#'
#' @description
#' Simulates an isothermal, isobaric counter-current supercritical-fluid
#' extraction column with \code{N} equilibrium stages using the GC-EOS. A liquid
#' (oil) feed and a vapor (solvent) feed enter at chosen stages; the model returns
#' the converged stage composition/flow profiles, the extract and raffinate
#' products, and component recoveries. The building block for deterpenation,
#' deacidification and similar high-pressure fractionations.
#'
#' @param N Number of equilibrium stages (stage 1 = top).
#' @param T Temperature (K). @param P Pressure (atm).
#' @param comps List of components (each \code{list(groups, dc, Tc)}), in the
#'   order used by \code{zL}, \code{zV}.
#' @param FL,zL Liquid (oil) feed: molar flow and mole-fraction composition.
#' @param FV,zV Vapor (solvent) feed: molar flow and composition (usually pure CO2).
#' @param feedL,feedV Feed stages for the liquid and vapor feeds (default top / bottom).
#'   With reflux, set \code{feedL} to an intermediate stage to create a rectifying
#'   section above the feed.
#' @param reflux External reflux ratio \eqn{R/E} (default 0 = simple counter-current,
#'   no reflux). The top-stage vapor is sent to a separator that recovers the
#'   solvent and condenses the organics; a fraction \eqn{R/(R+E)} of the
#'   solvent-free organic condensate is returned to stage 1 as liquid reflux and
#'   the remainder leaves as the extract product \eqn{E}. The separator is modelled
#'   as ideal solvent recovery (reflux/product are solvent-free).
#' @param solvent Name of the solvent component in \code{comps} (default
#'   \code{"CO2"}); required when \code{reflux > 0}.
#' @param model \code{"gceos"} (default), \code{"gcaeos"}, or a parameter-set list.
#' @param max_iter,tol Iteration cap and convergence tolerance (on stage liquid flows).
#' @param damp Damping factor (0-1) on the flow/composition update.
#'
#' @return A \code{list}: \code{x},\code{y} (N-by-component stage compositions),
#'   \code{L},\code{V} (stage flows), \code{extract},\code{raffinate} (product
#'   compositions and flows), \code{recovery} (per-component fraction of feed
#'   leaving in the extract), \code{K} (stage K-values), and \code{converged}.
#' @noRd
#' @seealso \code{\link{gceos_flash}}
gceos_column <- function(N, T, P, comps, FL, zL, FV, zV,
                         feedL = 1, feedV = N, reflux = 0, solvent = "CO2",
                         model = "gceos", max_iter = 120, tol = 1e-6, damp = 0.5,
                         refl_damp = 0.15, init = NULL) {
  nc <- length(comps); zL <- zL/sum(zL); zV <- zV/sum(zV)
  xin <- .gceos_inputs(T, comps, model)
  R <- GCEOS_R
  sidx <- match(solvent, names(comps))
  if (reflux > 0 && is.na(sidx))
    stop("'solvent' must name a component in 'comps' when reflux > 0.", call. = FALSE)

  # external component feed flows f[stage, comp]
  f <- matrix(0, N, nc)
  f[feedL, ] <- f[feedL, ] + FL*zL
  f[feedV, ] <- f[feedV, ] + FV*zV
  Ftot <- colSums(f)                       # total feed per component
  refl <- rep(0, nc); xR <- NULL           # reflux liquid (enters top stage as l_0)

  # Per-stage K from a phase-stability-tested flash of the stage bulk (GCEXTRAC-
  # style), warm-started from the previous stage K. Because x and y are a
  # Rachford-Rice split of the bulk, this cannot return the trivial K=1 of a
  # collapsed x=y stage; an NA return means the stage is genuinely single-phase
  # (e.g. ~pure solvent above the pinch) and the caller holds the previous K.
  # explicit physical K spread (solvent volatile, solutes prefer the liquid) --
  # the column knows the solvent, so this seeds the flash in the right direction
  # without relying on a general auto-init at high solvent dilution.
  Kspread <- { k <- rep(0.02, nc); if (!is.na(sidx)) k[sidx] <- 5; k }
  # maxss small: one Rachford-Rice split of the stage bulk (which re-separates a
  # drifted x~y back into distinct phases -> non-trivial K) plus a refinement.
  # The outer cascade provides the rest of the iteration (inside-out principle),
  # so a full inner convergence every call is wasteful.
  stageK <- function(zj, Kprev) {
    K <- gceos_stage_flashK(T, P, zj, Kprev, xin$d, xin$q, xin$nu, xin$g, xin$dg, xin$alpha, R, 2L, 1e-4)
    if (anyNA(K) || !all(is.finite(K)) || any(K <= 0)) return(NULL)
    K
  }
  # standalone flash (cold-start seeding) with the physical spread; NULL on fail.
  seedflash <- function(zb) {
    s <- sum(zb); if (!is.finite(s) || s <= 0) return(NULL)
    fl <- gceos_flash_cpp(T, P, zb/s, xin$d, xin$q, xin$nu, xin$g, xin$dg, xin$alpha, R, Kspread, 300L, 1e-4)
    if (fl$status != 1 || anyNA(fl$K) || !all(is.finite(fl$K))) return(NULL)
    fl
  }

  # ---- initial profiles ----
  # representative two-phase seed (CO2-saturated liquid / CO2-rich vapor) from a
  # flash of an oil+solvent bulk; also the fallback K when no warm start exists.
  eS <- replace(numeric(nc), if (is.na(sidx)) which.max(zV) else sidx, 1)
  seed <- NULL
  for (zc in c(0.5, 0.65, 0.35, 0.8, 0.2)) {
    fl <- seedflash((1 - zc) * zL + zc * eS)
    if (!is.null(fl) && fl$beta > 1e-3 && fl$beta < 1 - 1e-3 && max(abs(fl$K - 1)) > 1e-2) {
      seed <- fl; break
    }
  }
  Kseed <- if (!is.null(seed)) as.numeric(seed$K) else { k <- rep(0.05, nc); if (!is.na(sidx)) k[sidx] <- 4; k }

  Kmat <- matrix(Kseed, N, nc, byrow = TRUE)
  if (!is.null(init) && all(dim(init$x) == c(N, nc))) {
    # warm start from a previous solution (continuation): reuse stage profiles + K
    X <- init$x; Y <- init$y
    L <- if (!is.null(init$L)) init$L else rep(FL, N)
    V <- if (!is.null(init$V)) init$V else rep(FV, N)
    if (!is.null(init$K) && all(dim(init$K) == c(N, nc))) Kmat <- init$K
    for (j in seq_len(N)) {
      Kj <- stageK(X[j, ] * L[j] + Y[j, ] * V[j], Kmat[j, ])
      if (!is.null(Kj)) Kmat[j, ] <- Kj
    }
  } else {
    # cold start: every stage seeded with the genuine two-phase split, so the
    # cascade begins inside the real (non-trivial) basin.
    if (!is.null(seed)) {
      X <- matrix(seed$x, N, nc, byrow = TRUE)
      Y <- matrix(seed$y, N, nc, byrow = TRUE)
    } else {
      X <- matrix(zL, N, nc, byrow = TRUE)
      Y <- matrix(zV, N, nc, byrow = TRUE)
    }
    L <- rep(FL, N); V <- rep(FV, N)
  }

  Smax   <- 1e8                                   # cap stripping factor (avoid Inf, keeps physics)
  Lfloor <- 1e-10 * (FL + FV)                     # liquid-collapse threshold (flooding)
  lcur   <- X * L                                 # component liquid flows (primary state)
  status <- "max_iter"; refl_in <- refl

  for (iter in seq_len(max_iter)) {
    # 1) stage K-values from a warm-started, stability-tested flash of each
    #    stage's bulk material. The flash builds x,y as a Rachford-Rice split of
    #    the bulk, so it cannot return the trivial K=1 of a collapsed x=y stage;
    #    a NULL (single-phase stage, e.g. ~pure solvent above the pinch) holds
    #    the previous K. This is the GCEXTRAC-style robustness that prevents the
    #    sum-rates cascade collapsing to the trivial branch near the critical pt.
    for (j in seq_len(N)) {
      Kj <- stageK(X[j, ] * L[j] + Y[j, ] * V[j], Kmat[j, ])
      if (!is.null(Kj)) Kmat[j, ] <- Kj
    }

    # 2) per-component tridiagonal solve (reflux enters as liquid l_0 at the top)
    Snew <- pmin(Kmat * (V / L), Smax)
    l <- matrix(0, N, nc)
    for (i in seq_len(nc)) {
      a <- rep(1, N); a[1] <- 0
      b <- -(1 + Snew[, i]); cc <- c(Snew[-1, i], 0)
      d <- -f[, i]; d[1] <- d[1] - refl_in[i]
      l[, i] <- .thomas(a, b, cc, d)
    }
    l[!is.finite(l) | l < 0] <- 1e-30

    # 3) numerical-failure guard (genuine collapse only; transients are allowed)
    if (any(!is.finite(l))) { status <- "flooded"; break }

    # 4) fixed-point residual + damped update of the primary state
    res <- max(abs(l - lcur) / (abs(lcur) + 1e-12))
    lcur <- lcur + damp * (l - lcur)

    # 5) derive flows/compositions (sum-rates), guarded against L collapse
    v <- Snew * lcur
    L <- pmax(rowSums(lcur), Lfloor); V <- rowSums(v)
    if (any(!is.finite(c(L, V)))) { status <- "flooded"; break }
    X <- lcur / L; X <- X / rowSums(X)
    Y <- v / V;    Y <- Y / rowSums(Y)

    # 6) external reflux recycle (gentle relaxation of the tear stream)
    if (reflux > 0) {
      Oflow <- V[1] * (1 - Y[1, sidx])
      xR <- Y[1, ]; xR[sidx] <- 0; xR <- xR / sum(xR)
      refl_g  <- (Oflow * reflux / (1 + reflux)) * xR
      refl_in <- refl_damp * refl_g + (1 - refl_damp) * refl_in
    }

    if (iter > 3 && res < tol) { status <- "converged"; break }
  }
  # flooding diagnosis: ran out of iterations with a (near-)collapsed liquid phase
  if (status == "max_iter" && min(L) < 1e-6 * (FL + FV)) status <- "flooded"

  raff_x <- X[N, ]; raff_L <- L[N]; nms <- names(comps)
  if (reflux > 0) {
    Oflow <- V[1] * (1 - Y[1, sidx]); Eprod <- Oflow / (1 + reflux)
    ext <- list(comp = stats::setNames(if (is.null(xR)) Y[1,] else xR, nms), E = Eprod, reflux = reflux)
    recovery <- (Eprod * (if (is.null(xR)) Y[1,] else xR)) / (Ftot + 1e-30)
  } else {
    ext <- list(y = stats::setNames(Y[1, ], nms), V = V[1])
    recovery <- (V[1] * Y[1, ]) / (Ftot + 1e-30)
  }
  list(x = `colnames<-`(X, nms), y = `colnames<-`(Y, nms), L = L, V = V,
       K = `colnames<-`(Kmat, nms), extract = ext,
       raffinate = list(x = stats::setNames(raff_x, nms), L = raff_L),
       recovery = stats::setNames(pmin(recovery, 1), nms),
       converged = (status == "converged"), status = status, iters = iter)
}

#' @title Counter-current GC-EOS column (Naphtali-Sandholm simultaneous correction)
#' @description Robust isothermal/isobaric equilibrium-stage solver. Unlike the
#'   sequential sum-rates cascade of \code{\link{gceos_column}} (which can diverge
#'   at the very large stripping factors of supercritical fractionation), this
#'   solves the component material balances and stage equilibria SIMULTANEOUSLY
#'   by a block-tridiagonal Newton (the method used by rigorous extraction
#'   simulators such as PLAPIQUI's GCEXTRAC), so the flows cannot blow up.
#' @inheritParams gceos_column
#' @param T Column temperature (K). A single value for an isothermal column, or a
#'   length-\code{N} vector to impose a stage-by-stage temperature profile
#'   (a thermal gradient, which creates internal reflux). Stage 1 is the top.
#' @return Same structure as \code{\link{gceos_column}} plus \code{resid}.
#' @noRd
gceos_column_ns_solve <- function(N, T, P, comps, FL, zL, FV, zV,
                                  feedL = 1, feedV = N, reflux = 0, solvent = "CO2",
                                  model = "gceos", max_iter = 200, tol = 1e-8,
                                  refl_damp = 0.1, init = NULL) {
  nc <- length(comps); zL <- zL/sum(zL); zV <- zV/sum(zV); R <- GCEOS_R
  sidx <- match(solvent, names(comps))
  sidx0 <- if (is.na(sidx)) -1L else as.integer(sidx - 1L)
  nms <- names(comps)

  # temperature profile: scalar -> isothermal; length-N vector -> thermal gradient
  if (length(T) == 1L) Tvec <- rep(T, N)
  else { if (length(T) != N) stop("'T' must be a single value or a length-N vector.", call. = FALSE); Tvec <- T }
  # per-stage T-dependent EOS inputs (d, g, dg); q, nu, alpha are T-independent.
  cache <- list(); dmat <- matrix(0, N, nc); glist <- vector("list", N); dglist <- vector("list", N)
  for (j in seq_len(N)) {
    key <- format(Tvec[j], digits = 12)
    xj <- cache[[key]]; if (is.null(xj)) { xj <- .gceos_inputs(Tvec[j], comps, model); cache[[key]] <- xj }
    dmat[j, ] <- xj$d; glist[[j]] <- xj$g; dglist[[j]] <- xj$dg
  }
  xin <- cache[[format(Tvec[1], digits = 12)]]   # stage-1 inputs (seed + shared q/nu/alpha)

  f <- matrix(0, N, nc)
  f[feedL, ] <- f[feedL, ] + FL*zL
  f[feedV, ] <- f[feedV, ] + FV*zV
  Ftot <- colSums(f)

  if (!is.null(init) && !is.null(init$l) && all(dim(init$l) == c(N, nc))) {
    lmat <- init$l; vmat <- init$v
  } else {
    # two-phase seed (CO2-saturated liquid / CO2-rich vapor) for the flow profile
    eS <- replace(numeric(nc), if (is.na(sidx)) which.max(zV) else sidx, 1)
    Kspread <- { k <- rep(0.02, nc); if (!is.na(sidx)) k[sidx] <- 5; k }
    seed <- NULL
    for (zc in c(0.5, 0.65, 0.35, 0.8, 0.2)) {
      fl <- gceos_flash_cpp(Tvec[1], P, ((1-zc)*zL + zc*eS), xin$d, xin$q, xin$nu, xin$g, xin$dg, xin$alpha, R, Kspread, 300L, 1e-4)
      if (fl$status == 1 && fl$beta > 1e-3 && fl$beta < 1 - 1e-3) { seed <- fl; break }
    }
    xsd <- if (!is.null(seed)) as.numeric(seed$x) else zL
    ysd <- if (!is.null(seed)) as.numeric(seed$y) else zV
    lmat <- matrix(FL*xsd, N, nc, byrow = TRUE)
    vmat <- matrix(FV*ysd, N, nc, byrow = TRUE)
  }

  res <- gceos_column_ns(N, Tvec, P, f, lmat, vmat, dmat, xin$q, xin$nu, glist, dglist, xin$alpha,
                         R, sidx0, reflux, as.integer(max_iter), tol, refl_damp)
  l <- res$l; v <- res$v; K <- res$K
  L <- rowSums(l); V <- rowSums(v)
  X <- l/L; Y <- v/V
  raff_x <- X[N, ]; raff_L <- L[N]
  if (reflux > 0) {
    refl_frac <- reflux/(1 + reflux)
    Oflow <- sum(v[1, -sidx]); Eprod <- Oflow*(1 - refl_frac)
    xR <- v[1, ]; xR[sidx] <- 0; xR <- xR/sum(xR)
    ext <- list(comp = stats::setNames(xR, nms), E = Eprod, reflux = reflux)
    recovery <- (Eprod*xR)/(Ftot + 1e-30)
  } else {
    ext <- list(y = stats::setNames(Y[1, ], nms), V = V[1])
    recovery <- (V[1]*Y[1, ])/(Ftot + 1e-30)
  }
  # A vanishing liquid (or vapour) phase at extreme S/F drives the stage
  # equations singular, so the C kernel can report "converged" on an all-NaN
  # solution. Treat any non-finite result as a failed (infeasible) solve so
  # callers never see NaN compositions reported as a valid column.
  finite_ok <- all(is.finite(l)) && all(is.finite(v)) &&
    all(is.finite(raff_x)) && all(is.finite(recovery))
  status <- if (!finite_ok) "no_solution" else res$status
  list(x = `colnames<-`(X, nms), y = `colnames<-`(Y, nms), l = l, v = v, L = L, V = V,
       K = `colnames<-`(K, nms), extract = ext,
       raffinate = list(x = stats::setNames(raff_x, nms), L = raff_L),
       recovery = stats::setNames(pmin(recovery, 1), nms),
       converged = isTRUE(finite_ok && res$status == "converged"), status = status,
       iters = res$iters, resid = res$resid)
}
