# =====================================================================================================
# Chickos & Acree group-contribution estimation of fusion enthalpy (and heat capacity) from structure.
#
#   dHfus(Tfus) = Tfus * dS_tpce                              (total phase change entropy route)
#   dS_tpce     = SUM_k  N_k * C_k * G_k   [J/mol/K]          (Method 2; A&C JPCRD 46(1) 013104 (2017))
#   dHfus(298)  = dHfus(Tfus) + (0.15*Cp_s - 0.26*Cp_l - 9.83)*(Tfus-298.15)/1000  [kJ/mol]  (Eq 4, 2010)
#   Cp_l, Cp_s  = SUM_k N_k * cp_k        [J/mol/K]           (Method 1; 2021 rev Table 1 + 2010 FGs)
#
# Group ASSIGNMENT is disjoint (each heavy atom -> exactly one group, most-specific first) via the
# package's gcm_fragment() engine. The group SMARTS below are ANCHORED (recursive [$(...)] on the core
# atoms) so a functional group does not swallow neighbouring skeletal carbons -- the same convention the
# bundled Joback/Nannoolal schemes use. Ring/aromatic and the context-dependent group coefficients that
# SMARTS cannot encode (the 1.31 methylene rule; the FG-carbon 0.60/0.66/0.75 rules; the Table-3 #FG
# scaling) are applied separately below.
# =====================================================================================================

# ---- Method 2: total-phase-change-entropy groups (Gk, J/mol/K), anchored SMARTS + coefficient rule tag.
# rule: ""=plain (Ck=1); "ch2"=methylene 1.31 excess rule; "fgc60/66/75"=coeff only if a FG is attached at
# this carbon; "t3:a/b/c"=Table-3 coefficient by total #functional-groups (k<=1 / k=2..3 / k>=4).
.fus_dS_scheme <- function() {
  # Chickos & Acree dS_tpce group table, read from sysdata (migrated from inline; ALL GCM SMARTS live
  # in gcm_smarts, contributions in gcm_cnt). Reconstructs the code/Gk/smarts/rule/fg/ring/generic
  # scheme the disjoint most-specific-first matcher below consumes; row order = sysdata order.
  s <- gcm_smarts[["CHICKOS_2017_dS_SMARTS"]]; k <- gcm_cnt[["CHICKOS_2017_dS_cnt"]]
  data.frame(code = s$ID, Gk = k$Gk[match(s$ID, k$ID)], smarts = s$SMARTS,
             rule = s$rule, fg = s$fg, ring = s$ring, generic = s$generic,
             stringsAsFactors = FALSE)
}

# ---- Method 1: liquid/solid Cp groups (J/mol/K @298K), anchored SMARTS. NA = value not reported. --------
.fus_cp_scheme <- function() {
  # Chickos & Acree liquid/solid Cp group table, read from sysdata (gcm_smarts$CHICKOS_2010_cp_SMARTS
  # + gcm_cnt$CHICKOS_2010_cp_cnt). NA cpl/cps = value not reported.
  s <- gcm_smarts[["CHICKOS_2010_cp_SMARTS"]]; k <- gcm_cnt[["CHICKOS_2010_cp_cnt"]]
  data.frame(code = s$ID, cpl = k$cpl[match(s$ID, k$ID)], cps = k$cps[match(s$ID, k$ID)],
             smarts = s$SMARTS, stringsAsFactors = FALSE)
}

# Count groups for a scheme via the disjoint fragmentation engine. Returns named counts + coverage.
.fus_count <- function(mol, scheme) {
  sc <- data.frame(id = scheme$code, name = scheme$code, smarts = scheme$smarts,
                   pr = NA_integer_, stringsAsFactors = FALSE)
  fr <- gcm_fragment(mol, sc)
  list(counts = fr$counts, covered = fr$covered, n_heavy = fr$n_heavy)
}

#' Estimate fusion enthalpy and heat capacity from structure (Chickos & Acree GCM)
#'
#' Group-contribution estimate of the enthalpy of fusion \eqn{\Delta_{fus}H} (and the liquid/solid heat
#' capacities) of an organic solid from its SMILES and melting temperature, using the Chickos & Acree
#' total-phase-change-entropy method: \eqn{\Delta_{fus}H(T_{fus}) = T_{fus}\,\Delta S_{tpce}} with
#' \eqn{\Delta S_{tpce}=\sum_k N_k C_k G_k}, and the 2010 heat-capacity route to adjust to 298.15 K.
#' Intended as a fallback when no experimental fusion data is available.
#'
#' @param smiles solute SMILES (or anything \code{\link{mol_find}} accepts).
#' @param Tm melting temperature in K.
#' @param ring_eq include the Chickos non-aromatic-ring entropy term \eqn{33.4R + 3.7(N-3R)} in place of
#'   summing saturated-ring carbon values (default \code{TRUE}).
#' @return list: \code{dS_tpce} (J/mol/K), \code{dHfus_Tm} and \code{dHfus_298} (kJ/mol), \code{Cp_l},
#'   \code{Cp_s} (J/mol/K), \code{coverage} (fraction of heavy atoms assigned), \code{n_fg} (functional
#'   groups counted), and a data-driven \code{reliability} tier (\code{"good"}/\code{"fair"}/\code{"poor"};
#'   validated median errors ~15/29/60%) with \code{reliable = reliability != "poor"}. \code{NA} values
#'   where the structure could not be parsed.
#' @seealso \code{\link{est_gcm}}, \code{\link{csp_solubility}}.
#' @export
gcm_fusion <- function(smiles, Tm, ring_eq = TRUE) {
  out <- list(dS_tpce = NA_real_, dHfus_Tm = NA_real_, dHfus_298 = NA_real_,
              Cp_l = NA_real_, Cp_s = NA_real_, coverage = NA_real_, n_fg = NA_integer_)
  mol <- tryCatch(mol_find(smiles), error = function(e) NULL)
  if (is.null(mol)) return(out)
  Tm <- suppressWarnings(as.numeric(Tm)[1])
  ds_s <- .fus_dS_scheme()
  ct <- .fus_count(mol, ds_s)
  if (!length(ct$counts)) return(out)
  cnt <- setNames(as.integer(ct$counts), names(ct$counts))
  gk  <- setNames(ds_s$Gk,   ds_s$code)
  rule<- setNames(ds_s$rule, ds_s$code)
  isfg<- setNames(ds_s$fg,   ds_s$code)
  isring <- setNames(ds_s$ring, ds_s$code)

  # total number of functional groups (for the methylene rule threshold and Table-3 #FG coefficient)
  n_fg <- sum(cnt[names(cnt) %in% ds_s$code[ds_s$fg]], na.rm = TRUE)
  # sum of the other CARBON-type groups (Table-1/2, i.e. non-FG non-generic, excluding CH2) — the exact
  # Chickos threshold for the methylene rule.
  carbon_codes <- setdiff(ds_s$code[!ds_s$fg & !ds_s$generic], "CH2")
  n_carbon_other <- sum(cnt[names(cnt) %in% carbon_codes], na.rm = TRUE)
  # atoms assigned only via a generic per-element catch-all (rough placeholder value) -> flag unreliable
  n_generic <- sum(cnt[names(cnt) %in% ds_s$code[ds_s$generic]], na.rm = TRUE)

  dS <- 0
  for (c in names(cnt)) {
    n <- cnt[[c]]; G <- gk[[c]]; rl <- rule[[c]]
    Ck <- 1
    if (identical(rl, "ch2")) {
      # Chickos 1.31 methylene rule (exact): applied to the EXCESS consecutive methylenes over the sum of
      # the other carbon groups, and ONLY when the molecule bears a functional group.
      if (n_fg > 0 && n >= n_carbon_other) {
        excess <- n - n_carbon_other
        dS <- dS + (n - excess) * G + excess * 1.31 * G
      } else dS <- dS + n * G
      next
    } else if (startsWith(rl, "t3:")) {
      Ck <- .fus_t3_coeff(sub("t3:", "", rl), n_fg)
    }
    dS <- dS + n * Ck * G
  }

  # Non-aromatic ring entropy term (Eq 3): replaces the summed saturated-ring-carbon Gk with 33.4R+3.7(N-3R).
  if (isTRUE(ring_eq)) {
    rr <- .fus_ring_term(mol)
    if (is.finite(rr$dS_ring)) {
      # subtract the saturated (non-aromatic) ring-carbon contributions already added, add the ring term
      sat_ring_codes <- c("CcH", "CcQ", "=CcH", "=CcQ")
      subtract <- sum(vapply(sat_ring_codes, function(c)
        if (c %in% names(cnt)) cnt[[c]] * gk[[c]] else 0, numeric(1)))
      dS <- dS - subtract + rr$dS_ring
    }
  }

  # ---- Heat capacities (Method 1) ----
  cp_s <- .fus_cp_scheme(); cc <- .fus_count(mol, cp_s)
  Cp_l <- Cp_s <- NA_real_
  if (length(cc$counts)) {
    ccnt <- setNames(as.integer(cc$counts), names(cc$counts))
    cpl <- setNames(cp_s$cpl, cp_s$code); cps <- setNames(cp_s$cps, cp_s$code)
    Cp_l <- sum(vapply(names(ccnt), function(k) ccnt[[k]] * cpl[[k]], numeric(1)), na.rm = TRUE)
    Cp_s <- sum(vapply(names(ccnt), function(k) ccnt[[k]] * cps[[k]], numeric(1)), na.rm = TRUE)
  }

  dHfus_Tm <- Tm * dS / 1000                          # kJ/mol
  dHfus_298 <- if (all(is.finite(c(dHfus_Tm, Cp_l, Cp_s))))
    dHfus_Tm + (0.15 * Cp_s - 0.26 * Cp_l - 9.83) * (Tm - 298.15) / 1000 else NA_real_

  cov <- ct$covered / ct$n_heavy
  rd <- .fus_ring_descriptors(mol[["Molfile"]])
  tier <- .fus_risk_tier(dS, ct$n_heavy, rd$narom, rd$ring_CO, n_generic)
  list(dS_tpce = dS, dHfus_Tm = dHfus_Tm, dHfus_298 = dHfus_298,
       Cp_l = Cp_l, Cp_s = Cp_s, coverage = cov, n_fg = as.integer(n_fg),
       n_heavy = as.integer(ct$n_heavy), n_covered = as.integer(ct$covered),
       n_groups = as.integer(sum(cnt)), n_generic = as.integer(n_generic),
       narom_ring = as.integer(rd$narom), ring_CO = as.integer(rd$ring_CO),
       # data-driven reliability tier ("good"/"fair"/"poor"); reliable = not "poor" (backward compat)
       reliability = tier, reliable = !identical(tier, "poor"))
}

# Structural reliability descriptors: number of AROMATIC rings and number of ring carbonyls (quinone-like).
.fus_ring_descriptors <- function(molf) {
  narom <- 0L
  rs <- tryCatch(rJava::J("org.openscience.cdk.graph.Cycles")$sssr(molf)$toRingSet(), error = function(e) NULL)
  if (!is.null(rs)) {
    n <- rs$getAtomContainerCount()
    for (i in seq_len(n) - 1L) {
      arom <- tryCatch(isTRUE(rs$getAtomContainer(as.integer(i))$getAtom(0L)$getFlag(
        rJava::J("org.openscience.cdk.CDKConstants")$ISAROMATIC)), error = function(e) FALSE)
      if (isTRUE(arom)) narom <- narom + 1L
    }
  }
  ring_CO <- length(tryCatch(.group_matches("[CX3;R]=[OX1]", molf), error = function(e) list()))
  list(narom = narom, ring_CO = ring_CO)
}

# Reliability tier for a fusion estimate. Data-driven risk score (validated on 441 Acree compounds: the
# tiers give median |APE| of ~15% (good) / ~29% (fair) / ~60% (poor)). The dominant risk is an implausibly
# low total-phase-change entropy (dS_tpce < 30 J/mol/K) — the rigid-polycyclic under-count failure mode
# (caffeine, quinones) — followed by ring carbonyls, generic-atom fallbacks, and small fully-aliphatic size.
.fus_risk_tier <- function(dS, nheavy, narom_ring, ring_CO, n_generic) {
  if (!isTRUE(is.finite(dS))) return("poor")
  score <- 3 * (dS < 30) + 2 * (dS > 160) +
           2 * (ring_CO >= 2) + 1 * (ring_CO == 1) +
           min(as.integer(n_generic), 2L) +
           1 * (narom_ring == 0 && nheavy < 10)
  if (score >= 3) "poor" else if (score >= 1) "fair" else "good"
}

#' Estimate the enthalpy of vaporization at 298.15 K (Chen correlation + Chickos Cp adjustment)
#'
#' A structure-based \eqn{\Delta_{vap}H(298.15\,K)} for the Solute-Characterization module, as a
#' group-contribution alternative to the machine-learned \code{gcm_hvap}. It combines the Chen (1965)
#' boiling-point correlation for \eqn{\Delta_{vap}H(T_b)} (from the group-contribution critical
#' properties \eqn{T_b, T_c, P_c}) with the Chickos & Acree 2010 heat-capacity adjustment (Eq. 2) that
#' brings it from \eqn{T_b} to 298.15 K, using the Method-1 liquid heat capacity. The papers do not
#' contain a from-structure vaporization GCM — this is the faithful route their Cp method supports.
#'
#' @param smiles solute SMILES (or anything \code{\link{mol_find}} accepts).
#' @param Tb,Tc,Pc boiling point (K), critical temperature (K), critical pressure (bar); estimated with
#'   \code{\link{est_gcm}} when \code{NULL}.
#' @return \eqn{\Delta_{vap}H(298.15\,K)} in kJ/mol (\code{NA} if the structure/criticals are unavailable
#'   or the compound is supercritical at \eqn{T_b}).
#' @seealso \code{\link{gcm_fusion}}, \code{\link{est_gcm}}.
#' @export
gcm_hvap_chickos <- function(smiles, Tb = NULL, Tc = NULL, Pc = NULL) {
  if (is.null(Tb) || is.null(Tc) || is.null(Pc)) {
    g <- tryCatch(suppressWarnings(est_gcm(solute = smiles, tb = "JR_corr", crit = "JR", gorder = 0,
                     simplicity = "auto", silent = TRUE))$pares$Critical, error = function(e) NULL)
    if (is.null(g) || !all(c("Tb", "Tc", "Pc") %in% names(g))) return(NA_real_)
    if (is.null(Tb)) Tb <- unname(g["Tb"]); if (is.null(Tc)) Tc <- unname(g["Tc"]); if (is.null(Pc)) Pc <- unname(g["Pc"])
  }
  Tb <- as.numeric(Tb)[1]; Tc <- as.numeric(Tc)[1]; Pc <- as.numeric(Pc)[1]
  if (!all(is.finite(c(Tb, Tc, Pc))) || Pc <= 0) return(NA_real_)
  Tbr <- Tb / Tc
  if (!is.finite(Tbr) || Tbr >= 1.05) return(NA_real_)
  # Chen (1965): dHvap(Tb) [J/mol], Pc in atm.
  Pc_atm <- Pc / 1.01325
  dHvap_Tb <- 8.314 * Tb * (3.978 * Tbr - 3.958 + 1.555 * log(Pc_atm)) / (1.07 - Tbr)  # J/mol
  if (!is.finite(dHvap_Tb) || dHvap_Tb <= 0) return(NA_real_)
  # Method-1 liquid heat capacity for the Chickos Eq-2 temperature adjustment to 298.15 K.
  mol <- tryCatch(mol_find(smiles), error = function(e) NULL)
  Cp_l <- NA_real_
  if (!is.null(mol)) { cp <- .fus_count(mol, .fus_cp_scheme())
    if (length(cp$counts)) { cps <- .fus_cp_scheme(); cpl <- setNames(cps$cpl, cps$code)
      Cp_l <- sum(vapply(names(cp$counts), function(k) cp$counts[[k]] * cpl[[k]], numeric(1)), na.rm = TRUE) } }
  dHvap_298 <- if (isTRUE(is.finite(Cp_l)))
    dHvap_Tb / 1000 + (10.58 + 0.26 * Cp_l) * (Tb - 298.15) / 1000 else dHvap_Tb / 1000
  dHvap_298   # kJ/mol
}

# Table-3 group coefficient by total number of functional groups k (Chickos Part 2, Table 3).
.fus_t3_coeff <- function(which, k) {
  tab <- list(
    alc  = c(1, 10.4, 9.7, 13.1),   # alcohol: k<=1, k=2, k=3, k>=4
    acid = c(1, 1.21, 1.21, 2.25),  # carboxylic acid
    cl   = c(1, 1.5,  1.5,  1.5)    # chlorine
  )
  v <- tab[[which]]; if (is.null(v)) return(1)
  if (k <= 1) v[1] else if (k == 2) v[2] else if (k == 3) v[3] else v[4]
}

# Non-aromatic ring entropy term: 33.4*R + 3.7*(N - 3R), R = # non-aromatic rings, N = # their ring atoms.
.fus_ring_term <- function(mol) {
  out <- list(dS_ring = NA_real_, R = 0L, N = 0L)
  rings <- tryCatch({
    sssr <- rJava::J("org.openscience.cdk.graph.Cycles")$sssr(mol)$toRingSet()
    n <- sssr$getAtomContainerCount()
    R <- 0L; atoms <- integer(0)
    for (i in seq_len(n) - 1L) {
      rg <- sssr$getAtomContainer(as.integer(i))
      arom <- tryCatch(rg$getAtom(0L)$getFlag(rJava::J("org.openscience.cdk.CDKConstants")$ISAROMATIC),
                       error = function(e) FALSE)
      if (isTRUE(arom)) next                          # aromatic rings handled by benzenoid Gk values
      R <- R + 1L
    }
    list(R = R)
  }, error = function(e) NULL)
  if (is.null(rings) || rings$R == 0L) { out$dS_ring <- 0; return(out) }
  # N = number of atoms in non-aromatic rings (ring membership, non-aromatic)
  syms <- vapply(rcdk::get.atoms(mol), function(a)
    tryCatch(a$getFlag(rJava::J("org.openscience.cdk.CDKConstants")$ISINRING) &&
             !a$getFlag(rJava::J("org.openscience.cdk.CDKConstants")$ISAROMATIC), logical(1)),
    logical(1))
  N <- sum(syms, na.rm = TRUE)
  out$R <- rings$R; out$N <- N
  out$dS_ring <- 33.4 * rings$R + 3.7 * (N - 3 * rings$R)
  out
}
