#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# GC-EOS THERMODYNAMIC ENGINE (R glue over the C++ core)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Connects the parameter tables (gceos_params) and group decomposition to the
# compiled core (gceos_ar_rt / gceos_pressure / gceos_lnphi). Temperature-
# dependent group energies and component hard-sphere diameters are assembled
# here; the C++ side handles the per-(V,n) hot path.

GCEOS_R <- 82.06   # cm^3 atm / mol / K  (matches the g* parameter units)

# Resolve a model argument to a parameter-set list. Accepts a registered name
# ("gceos"/"gcaeos"/"gceos-hmw"/"diaz2005"/"ganan2011") or a parameter-set list
# directly (e.g. a custom set).
.gceos_resolve <- function(model) {
  if (is.list(model)) model else gceos_params(model)
}

# Build the (T-dependent) C++ inputs for a set of components.
# comps: list of components, each a list(groups = named integer vector of group
#   counts, dc = critical diameter, Tc = critical temperature, n = moles).
.gceos_inputs <- function(T, comps, model = "gceos") {
  ps <- .gceos_resolve(model)
  Tstar <- stats::setNames(ps$groups$Tstar, rownames(ps$groups))
  gs    <- stats::setNames(ps$groups$gstar, rownames(ps$groups))
  gp    <- stats::setNames(ps$groups$gprime, rownames(ps$groups))
  gpp   <- stats::setNames(ps$groups$gpprime, rownames(ps$groups))

  grp <- sort(unique(unlist(lapply(comps, function(c) names(c$groups)))))
  if (!all(grp %in% rownames(ps$groups)))
    stop("Unknown group(s): ", paste(setdiff(grp, rownames(ps$groups)), collapse = ", "), call. = FALSE)
  NG <- length(grp); NC <- length(comps)

  # pure-group energies g_jj(T)
  gjj <- gs[grp] * (1 + gp[grp]*(T/Tstar[grp] - 1) + gpp[grp]*log(T/Tstar[grp]))

  # binary-interaction lookups: matrix form (gceos / Diaz inline set) or the
  # sparse asymmetric $binary table (gcaeos). kstar/kprime are symmetric; alpha
  # is asymmetric (alpha_ij from the (i,j) row). Unlisted pairs -> k*=1,k'=0,a=0.
  if (!is.null(ps$kstar)) {
    kstarM <- ps$kstar; kprimeM <- ps$kprime; alphaM <- ps$alpha
  } else if (!is.null(ps$binary)) {
    kstarM <- matrix(1, NG, NG, dimnames = list(grp, grp))
    kprimeM <- alphaM <- matrix(0, NG, NG, dimnames = list(grp, grp))
    bin <- ps$binary
    for (r in seq_len(nrow(bin))) {
      a <- bin$i[r]; b <- bin$j[r]
      if (a %in% grp && b %in% grp) {
        kstarM[a, b] <- kstarM[b, a] <- bin$kstar[r]
        kprimeM[a, b] <- kprimeM[b, a] <- bin$kprime[r]
        alphaM[a, b] <- bin$alpha_ij[r]; alphaM[b, a] <- bin$alpha_ji[r]
      }
    }
  } else stop("model has neither $kstar nor $binary interaction parameters.", call. = FALSE)

  # group-pair energy, Delta g, and nonrandomness matrices
  g  <- dg <- al <- matrix(0, NG, NG, dimnames = list(grp, grp))
  for (a in seq_len(NG)) for (b in seq_len(NG)) {
    ka <- grp[a]; kb <- grp[b]
    if (a == b) { g[a,b] <- gjj[a]; dg[a,b] <- 0; al[a,b] <- 0; next }
    kst <- kstarM[ka, kb]; kpr <- kprimeM[ka, kb]
    if (is.na(kst)) kst <- 1; if (is.na(kpr)) kpr <- 0          # n.a. -> default (no correction)
    Tij <- 0.5*(Tstar[ka] + Tstar[kb])
    kij <- kst * (1 + kpr*log(T/Tij))
    # geometric-mean combining rule; the pure-group energy g_jj(T) can turn
    # negative at high T (outside the model's range), making the product < 0.
    # Yield NaN silently there (P becomes NaN -> treated as no VL loop) instead
    # of warning on sqrt() of a negative.
    prodg <- gjj[a]*gjj[b]
    g[a,b]  <- kij * (if (!is.na(prodg) && prodg >= 0) sqrt(prodg) else NaN)
    dg[a,b] <- g[a,b] - gjj[b]                                  # Delta g_kj = g_kj - g_jj
    av <- alphaM[ka, kb]; al[a,b] <- if (is.na(av)) 0 else av
  }

  q  <- ps$groups[grp, "q"]
  nu <- matrix(0, NC, NG, dimnames = list(NULL, grp))
  for (i in seq_len(NC)) nu[i, names(comps[[i]]$groups)] <- comps[[i]]$groups
  d  <- vapply(comps, function(c) 1.065655*c$dc*(1 - 0.12*exp(-2*c$Tc/(3*T))), numeric(1))
  n  <- vapply(comps, function(c) if (is.null(c$n)) 1 else c$n, numeric(1))

  asc <- .gceos_assoc_inputs(T, comps, ps)
  list(q = q, nu = nu, g = g, dg = dg, alpha = al, d = d, n = n,
       numu = asc$numu, Delta0 = asc$Delta0)
}

# Build the GCA-EOS association inputs (numu, Delta0) from the model's
# $groups$assoc mapping (dispersive group -> associating functionality),
# $schemes (sites/scheme per associating group) and $assoc (bonding site-pairs
# with eps_R [K] and kappa [cm3/mol]). The site multiplicity is folded into numu
# so the C++ A_assoc needs no change. Returns list(numu=NULL,Delta0=NULL) when
# the model/components carry no association.
.gceos_assoc_inputs <- function(T, comps, ps) {
  none <- list(numu = NULL, Delta0 = NULL)
  if (is.null(ps$assoc) || is.null(ps$schemes) || is.null(ps$groups$assoc)) return(none)
  amap <- stats::setNames(as.character(ps$groups$assoc), rownames(ps$groups))
  NC <- length(comps)
  agcnt <- lapply(comps, function(c) {
    g <- c$groups; fn <- amap[names(g)]; keep <- !is.na(fn)
    if (!any(keep)) return(numeric(0))
    tapply(as.numeric(g)[keep], fn[keep], sum)
  })
  present <- unique(unlist(lapply(agcnt, names)))
  if (length(present) == 0) return(none)
  # site classes per scheme: list(type, multiplicity)
  sdef <- list("1A"  = list(list("+-",1)),
               "2B"  = list(list("+",1), list("-",1)),
               "3B"  = list(list("+",1), list("-",2)),
               "4C"  = list(list("+",2), list("-",2)),
               "ea2" = list(list("+",2)))
  cls <- list()
  for (ag in present) {
    sc <- ps$schemes$scheme[match(ag, ps$schemes$group)]
    if (is.na(sc) || is.null(sdef[[sc]])) next
    for (st in sdef[[sc]]) cls[[length(cls)+1]] <- list(group = ag, type = st[[1]], mult = st[[2]])
  }
  na <- length(cls); if (na == 0) return(none)
  numu <- matrix(0, na, NC)
  for (a in seq_len(na)) for (m in seq_len(NC)) {
    cc <- agcnt[[m]][cls[[a]]$group]
    if (length(cc) && !is.na(cc)) numu[a, m] <- cls[[a]]$mult * cc
  }
  # sites bond if their types are complementary (+/-, and +- bonds anything)
  comp_ok <- function(ta, tb) (ta == "+-" || tb == "+-") || (ta == "+" && tb == "-") || (ta == "-" && tb == "+")
  asc <- ps$assoc
  lookup <- function(gi, gj) {
    r <- which((asc$group_i == gi & asc$group_j == gj) | (asc$group_i == gj & asc$group_j == gi))
    if (!length(r)) return(NULL)
    list(eps = asc$eps_R[r[1]], kap = asc$kappa[r[1]])
  }
  Delta0 <- matrix(0, na, na)
  for (a in seq_len(na)) for (b in seq_len(na)) {
    if (!comp_ok(cls[[a]]$type, cls[[b]]$type)) next
    lk <- lookup(cls[[a]]$group, cls[[b]]$group); if (is.null(lk)) next
    Delta0[a, b] <- lk$kap * (exp(lk$eps / T) - 1)
  }
  list(numu = numu, Delta0 = Delta0,
       classes = vapply(cls, function(c) paste0(c$group, c$type), character(1)))
}

#' @title Evaluate the GC-EOS at a given temperature, volume and composition
#' @description Returns the residual Helmholtz energy, pressure, compressibility
#'   factor and log-fugacity coefficients of a mixture from the GC-EOS.
#' @param T Temperature (K). @param V Total volume (cm3).
#' @param comps List of components; each a \code{list} with \code{groups} (named
#'   integer vector of GC-EOS group counts), \code{dc} (critical diameter,
#'   cm/mol), \code{Tc} (critical temperature, K) and \code{n} (moles).
#' @param model \code{"gceos"} (default) or \code{"gcaeos"}.
#' @return A \code{list}: \code{ar_rt} (A^R/RT), \code{P} (atm), \code{Z}, and
#'   \code{lnphi} (named by component).
#' @export
gceos_eval <- function(T, V, comps, model = "gceos") {
  x <- .gceos_inputs(T, comps, model)
  ar <- gceos_ar_rt(T, V, x$n, x$d, x$q, x$nu, x$g, x$dg, x$alpha, GCEOS_R, x$numu, x$Delta0)
  P  <- gceos_pressure(T, V, x$n, x$d, x$q, x$nu, x$g, x$dg, x$alpha, GCEOS_R, x$numu, x$Delta0)
  lp <- gceos_lnphi(T, V, x$n, x$d, x$q, x$nu, x$g, x$dg, x$alpha, GCEOS_R, x$numu, x$Delta0)
  list(ar_rt = ar, P = P, Z = P*V/(sum(x$n)*GCEOS_R*T),
       lnphi = stats::setNames(lp, names(comps)))
}

#' @title Pure-component vapor pressure from the GC-EOS
#' @description Solves the GC-EOS pure-component saturation pressure at
#'   temperature \code{T} (equal-fugacity construction across the liquid and
#'   vapor volume roots). Returns \code{NA} above the critical temperature or if
#'   the van der Waals loop cannot be bracketed.
#' @param T Temperature (K).
#' @param comp A single component \code{list} (\code{groups}, \code{dc},
#'   \code{Tc}; \code{n} is ignored / set to 1).
#' @param model \code{"gceos"} (default) or \code{"gcaeos"}.
#' @param Vmin Lower volume bound (cm3/mol). By default it is set automatically
#'   just above the hard-sphere covolume \eqn{(\pi/6)\sum n_i d_i^3} (below which
#'   the free-volume term is undefined); pass a value to override.
#' @param Vmax Upper volume bound (cm3/mol) for the loop search.
#' @return Saturation pressure (atm), or \code{NA}.
#' @export
gceos_psat <- function(T, comp, model = "gceos", Vmin = NULL, Vmax = 1e6) {
  comp$n <- 1
  # close-packed covolume: V must exceed (pi/6) sum n_i d_i^3 for a valid Y
  x <- .gceos_inputs(T, list(comp), model)
  bcov <- (pi/6) * sum(x$n * x$d^3)
  Vlo <- if (is.null(Vmin)) 1.02 * bcov else max(Vmin, 1.02 * bcov)
  # Inputs (d, group matrices, association params) depend only on T and comp, not
  # V, so build them ONCE (above) and call the C++ kernels directly per volume -
  # gceos_eval() would rebuild .gceos_inputs() on every one of the ~1000 calls.
  ntot <- sum(x$n)
  Pf <- function(V) gceos_pressure(T, V, x$n, x$d, x$q, x$nu, x$g, x$dg, x$alpha, GCEOS_R, x$numu, x$Delta0)
  lnphi1 <- function(V) {
    P  <- Pf(V)
    ar <- gceos_ar_rt(T, V, x$n, x$d, x$q, x$nu, x$g, x$dg, x$alpha, GCEOS_R, x$numu, x$Delta0)
    Z  <- P * V / (ntot * GCEOS_R * T)
    ar + (Z - 1) - log(Z)
  }
  rootV <- function(Pt, lo, hi) tryCatch(stats::uniroot(function(V) Pf(V)-Pt, c(lo,hi))$root, error=function(e) NA)
  # Locate the van der Waals loop by scanning P(V). The loop sits in a narrow
  # low-V band (a few multiples of the covolume), while the high-V vapour branch
  # is monotone, so we concentrate ~80% of the scan points in [Vlo, 200*Vlo].
  # This finds the loop robustly at a fraction of the cost of a uniform 8000-pt
  # scan (which made gceos_fit_dc, calling this ~20x, take ~40 s); a lighter
  # fine scan is used only as a fallback.
  sat_at <- function(npts) {
    Vmid <- min(Vmax, 200 * Vlo)
    if (Vmid <= Vlo * 1.001) {
      Vs <- exp(seq(log(Vlo), log(Vmax), length.out = npts))
    } else {
      nlo <- max(2L, round(npts * 0.8)); nhi <- max(2L, npts - nlo)
      Vs <- c(exp(seq(log(Vlo), log(Vmid), length.out = nlo)),
              exp(seq(log(Vmid), log(Vmax), length.out = nhi + 1L))[-1])
    }
    Pv <- vapply(Vs, Pf, numeric(1))
    dP <- diff(Pv); turn <- which(dP[-1]*dP[-length(dP)] < 0)
    if (length(turn) < 2) return(NA_real_)
    V1 <- Vs[turn[1]+1]; V2 <- Vs[turn[2]+1]
    Pe1 <- Pv[turn[1]+1]; Pe2 <- Pv[turn[2]+1]          # loop extrema (min at V1, max at V2)
    Phi <- max(Pe1, Pe2)                                # loop maximum: above it no vapor root
    # Psat must allow BOTH a liquid root (Pt >= loop minimum) and a vapor root
    # within [V2, Vmax] (Pt >= vapor-branch pressure at Vmax).
    Plo <- max(min(Pe1, Pe2), Pv[length(Pv)] * 1.02, 1e-8)
    if (Plo >= Phi) return(NA_real_)
    obj <- function(Pt) {
      Vl <- rootV(Pt, Vlo, V1); Vv <- rootV(Pt, V2, Vmax)
      if (any(is.na(c(Vl,Vv)))) return(NA_real_)
      lnphi1(Vl) - lnphi1(Vv)
    }
    tryCatch(stats::uniroot(obj, c(Plo*1.001, Phi*0.999), tol = 1e-7)$root, error = function(e) NA_real_)
  }
  res <- sat_at(800)
  if (is.na(res)) res <- sat_at(3000)
  res
}

#' @title Fit a component's GC-EOS critical diameter to a vapor-pressure point
#'
#' @description
#' Regresses the pure-component hard-sphere critical diameter \eqn{d_c} - the only
#' molecule-specific parameter of the GC-EOS - so that the model reproduces a
#' single experimental vapor-pressure datum \eqn{(T_{sat}, P_{sat})}. With the
#' normal boiling point (\code{Tsat = Tb}, \code{Psat = 1} atm) this is the
#' standard way to parameterize a new molecule whose group parameters are already
#' in the table. Solves an outer root-find on \eqn{d_c} around the saturation
#' calculation (\code{\link{gceos_psat}}); \eqn{P_{sat}} decreases monotonically
#' with \eqn{d_c}, so the solution is unique.
#'
#' @param groups Named integer vector of GC-EOS group counts (e.g. from
#'   \code{\link{gceos_decompose}}).
#' @param Tc Critical temperature (K) of the component.
#' @param Tsat,Psat The reference vapor-pressure point: temperature (K) and
#'   pressure (atm). Defaults \code{Psat = 1} (the normal boiling point).
#' @param model \code{"gceos"} (default), \code{"gcaeos"}, or a parameter-set list.
#' @param dc_range Search interval for \eqn{d_c} (cm/mol).
#'
#' @return The fitted critical diameter \eqn{d_c} (cm/mol).
#' @export
#'
#' @examples
#' \dontrun{
#' # recover limonene's d_c from its boiling point using the Diaz 2005 groups
#' diaz <- gceos_params("diaz2005")
#' g <- diaz$decomp["limonene", ]; g <- g[g > 0]
#' gceos_fit_dc(g, Tc = 660, Tsat = 449.65, model = diaz)   # ~5.60
#' }
#' @seealso \code{\link{gceos_psat}}, \code{\link{gceos_decompose}}
gceos_fit_dc <- function(groups, Tc, Tsat, Psat = 1, model = "gceos", dc_range = c(1, 20)) {
  comp <- function(dc) list(groups = groups, Tc = Tc, dc = dc, n = 1)
  obj  <- function(dc) { p <- gceos_psat(Tsat, comp(dc), model); if (is.na(p)) NA_real_ else p - Psat }
  # bracket the (monotone) root: Psat falls as dc grows
  dcs <- seq(dc_range[1], dc_range[2], length.out = 16)
  ov  <- vapply(dcs, obj, numeric(1))
  ok  <- !is.na(ov)
  if (sum(ok) < 2) stop("Could not evaluate the saturation pressure over 'dc_range'.", call. = FALSE)
  dcs <- dcs[ok]; ov <- ov[ok]
  sc  <- which(ov[-1] * ov[-length(ov)] < 0)
  if (!length(sc))
    stop("No d_c in [", dc_range[1], ", ", dc_range[2], "] reproduces Psat = ", Psat, " atm.", call. = FALSE)
  stats::uniroot(obj, c(dcs[sc[1]], dcs[sc[1]+1]), tol = 1e-6)$root
}

#' @title Fit a component's GC-EOS critical diameter to a binary VLE (bubble) point
#'
#' @description
#' Regresses the pure-component hard-sphere critical diameter \eqn{d_c} so the model
#' reproduces an experimental \strong{mixture} bubble pressure for a binary of the
#' component with a solvent (typically CO2), at a fixed temperature and liquid
#' composition. This is the correct calibration route for \strong{associating}
#' components (\code{"gcaeos"}). Their pure-component vapour
#' pressure reflects strong self-association (e.g. carboxylic-acid dimerisation), so a
#' \eqn{d_c} fit to the boiling point with \code{\link{gceos_fit_dc}} does \emph{not}
#' transfer to the mixture, where the association state is different - fit to VLE
#' instead. (For non-associating \code{"gceos"} the two routes largely agree.)
#'
#' @param groups Named integer vector of group counts for the solute (e.g. from
#'   \code{\link{gceos_decompose}}).
#' @param Tc Critical temperature (K) of the solute.
#' @param T Temperature (K) of the experimental point.
#' @param Pbub Experimental bubble pressure (\strong{atm}) at \code{T} and
#'   \code{x_solute}. Convert from MPa via \code{P_MPa * 1e6 / 101325}.
#' @param x_solute Liquid mole fraction of the solute at the bubble point
#'   (\code{1 - x_solvent}).
#' @param solvent A single component \code{list} (\code{groups}, \code{dc},
#'   \code{Tc}) for the solvent, e.g. CO2.
#' @param model \code{"gceos"} (default), \code{"gcaeos"}, \code{"gceos-hmw"}, or a
#'   parameter-set list.
#' @param dc_range Search interval for \eqn{d_c} (cm/mol).
#'
#' @return The fitted critical diameter \eqn{d_c} (cm/mol).
#' @export
#' @seealso \code{\link{gceos_fit_dc}} (pure-component / boiling-point route),
#'   \code{\link{gceos_saturation}}.
#'
#' @examples
#' \dontrun{
#' # fatty-acid d_c from a CO2+acid bubble point under the associating GCA-EOS
#' # (the pure-component boiling point reflects acid dimerisation, so fit to VLE)
#' acid <- gceos_decompose("CCCCCCCCCCCC(=O)O", "gcaeos")     # lauric acid
#' co2  <- list(groups = c(CO2 = 1), dc = 3.1289, Tc = 304.2)
#' gceos_fit_dc_vle(acid, Tc = 743, T = 333.15, Pbub = 5e6/101325,
#'                  x_solute = 0.1, solvent = co2, model = "gcaeos")
#' }
gceos_fit_dc_vle <- function(groups, Tc, T, Pbub, x_solute, solvent,
                             model = "gceos", dc_range = c(1, 20)) {
  stopifnot(x_solute > 0, x_solute < 1, Pbub > 0)
  comps_at <- function(dc) list(solute  = list(groups = groups, dc = dc, Tc = Tc),
                                solvent = solvent)
  xliq   <- c(x_solute, 1 - x_solute)              # order matches comps: solute, solvent
  Prange <- c(1, max(2 * Pbub, 800))
  bub <- function(dc) {
    s <- tryCatch(gceos_saturation(T, xliq, comps_at(dc), model = model,
                                   type = "bubble", Prange = Prange),
                  error = function(e) NULL)
    if (is.null(s)) NA_real_ else s$P
  }
  obj <- function(dc) { p <- bub(dc); if (is.na(p)) NA_real_ else p - Pbub }
  dcs <- seq(dc_range[1], dc_range[2], length.out = 16)
  ov  <- vapply(dcs, obj, numeric(1))
  ok  <- !is.na(ov)
  if (sum(ok) < 2) stop("Could not evaluate the bubble pressure over 'dc_range'.", call. = FALSE)
  dcs <- dcs[ok]; ov <- ov[ok]
  sc  <- which(ov[-1] * ov[-length(ov)] < 0)
  if (!length(sc))
    stop("No d_c in [", dc_range[1], ", ", dc_range[2], "] reproduces Pbub = ",
         round(Pbub, 3), " atm at x_solute = ", x_solute, ".", call. = FALSE)
  stats::uniroot(obj, c(dcs[sc[1]], dcs[sc[1]+1]), tol = 1e-6)$root
}

#' @title Pure-component critical temperature predicted by the GC-EOS
#'
#' @description
#' Returns the temperature above which the model has no vapour-liquid loop - i.e.
#' the pure-component critical temperature \emph{predicted by the equation of
#' state}. This generally differs from the experimental \eqn{T_c} used in the
#' hard-sphere diameter's temperature dependence (GC-EOS typically under-predicts
#' it). Useful for flagging supercritical states, where \code{\link{gceos_psat}}
#' returns \code{NA} because no saturation pressure exists.
#'
#' @param comp A single component \code{list} (\code{groups}, \code{dc},
#'   \code{Tc}; \code{n} is ignored / set to 1).
#' @param model \code{"gceos"} (default), \code{"gcaeos"}, or a parameter-set list.
#' @param Trange Search interval (K) for the bisection.
#'
#' @return The predicted critical temperature (K), or \code{NA} if a loop exists
#'   even at the low end of \code{Trange}, or persists to its high end.
#' @export
#' @seealso \code{\link{gceos_psat}}
gceos_tcrit <- function(comp, model = "gceos", Trange = c(250, 1500)) {
  comp$n <- 1
  has_loop <- function(T) {
    x <- .gceos_inputs(T, list(comp), model)
    bcov <- (pi/6) * sum(x$n * x$d^3); Vlo <- 1.02 * bcov; Vmax <- 1e6
    Vmid <- min(Vmax, 200 * Vlo)
    Vs <- if (Vmid <= Vlo * 1.001) exp(seq(log(Vlo), log(Vmax), length.out = 600))
          else exp(seq(log(Vlo), log(Vmid), length.out = 600))
    Pv <- vapply(Vs, function(V) gceos_pressure(T, V, x$n, x$d, x$q, x$nu, x$g, x$dg, x$alpha, GCEOS_R, x$numu, x$Delta0), numeric(1))
    dP <- diff(Pv); sum(dP[-1] * dP[-length(dP)] < 0) >= 2
  }
  lo <- Trange[1]; hi <- Trange[2]
  if (!has_loop(lo)) return(NA_real_)   # already supercritical at the low end
  if (has_loop(hi))  return(NA_real_)   # loop never vanishes in range
  while (hi - lo > 0.5) { mid <- (lo + hi) / 2; if (has_loop(mid)) lo <- mid else hi <- mid }
  (lo + hi) / 2
}
