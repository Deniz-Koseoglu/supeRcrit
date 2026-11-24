#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Automatic solvent / solvent-blend optimiser based on Hansen solubility parameters
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ---- internal: coarse-but-curated pairwise miscibility screen --------------------------------------
# Two solvents are treated as miscible unless a curated rule says otherwise; for solvents not covered by
# the rules a Hansen-distance fallback (misc_Ra) is used. This is a screen, NOT a rigorous LLE calculation.
.hsp_miscible <- function(s1, s2, sdb, misc_Ra) {
  if (s1 == s2) return(TRUE)
  nonpolar <- c("Benzene","Cyclohexane","DiethylEther","Heptane","Hexane","Toluene","PXylene","OXylene","MethylOleate")
  pair <- c(s1, s2)
  if ("Water" %in% pair) return(!(setdiff(pair, "Water")[1] %in% nonpolar))       # water only with polar organics
  if ("MethylOleate" %in% pair) { o <- setdiff(pair, "MethylOleate")[1]; if (o == "Methanol") return(FALSE) }
  i1 <- which(sdb$Solvent == s1); i2 <- which(sdb$Solvent == s2)                  # HSP-distance fallback
  if (length(i1) && length(i2)) {
    raij <- sqrt(4*(sdb$dD[i1]-sdb$dD[i2])^2 + (sdb$dP[i1]-sdb$dP[i2])^2 + (sdb$dHB[i1]-sdb$dHB[i2])^2)
    return(raij <= misc_Ra)
  }
  TRUE
}
.hsp_Ra <- function(s, b) sqrt(4*(s[1]-b[1])^2 + (s[2]-b[2])^2 + (s[3]-b[3])^2)    # Hansen distance solute<->blend

# Clavilier et al. (2025) Green Chem. "updated solvent miscibility table" (visual room-temperature tests),
# restricted to the app's co-solvents. Only the NON-fully-miscible pairs are listed - every unlisted pair of
# these solvents is fully miscible. `state`: "immiscible" (forms two phases), "partial" (miscible only up to
# a limited fraction - the paper's A-J range cells), "reaction" (interact/react exothermically on contact).
# Names use the app's solvent keys (modifier_values / eos_extra_solvents). Source: O. Clavilier, D. Foy,
# F. Byrne, Green Chem. 2025, 27, 12151-12159, Table 2 (doi:10.1039/d5gc02901e).
.solvent_miscibility_table <- function() data.frame(
  s1 = c("Water","Water","Water","Water","Water","Water","Water",
         "Hexane","Hexane","Hexane","Hexane","DiethylEther","Chloroform"),
  s2 = c("Hexane","Toluene","Dichloromethane","Chloroform","DiethylEther","Ethyl acetate","Methyl ethyl ketone",
         "Dimethyl sulfoxide","Acetonitrile","Methanol","Ethanol","Dimethyl sulfoxide","Acetone"),
  state = c("immiscible","immiscible","immiscible","immiscible","partial","partial","partial",
            "immiscible","immiscible","partial","partial","partial","reaction"),
  stringsAsFactors = FALSE)

#' @title Classify whether the components of a solvent blend are mutually miscible
#' @description Pairwise miscibility classification used app-wide to keep blend predictions physically
#'   meaningful. Each component pair is classified \strong{miscible}, \strong{partial} (miscible only over a
#'   limited composition range), \strong{immiscible} (phase-separating), or \strong{reaction} (the solvents
#'   react / interact exothermically on contact). Pairs are looked up FIRST in an experimental miscibility
#'   table (Clavilier et al. 2025, restricted to the app's co-solvents); pairs not in the table fall back to
#'   a curated rule + Hansen-distance screen (immiscible above \code{misc_Ra}). Solvents that cannot be placed
#'   at all are conservatively treated as miscible (never excluded).
#' @param components Character vector of solvent names/keys (as used by the co-solvent modules / \code{show_solv()}).
#' @param misc_Ra Hansen-distance threshold for the fallback test. Defaults to 18 (see \code{\link{hsp_solvent_finder}}).
#' @param solv_db Solvent HSP table for the fallback; defaults to \code{\link{show_solv}()}.
#' @return A \code{list}: \code{usable} (\code{TRUE} unless any pair is immiscible or reactive - i.e. the blend
#'   is a physically sensible single phase to predict), \code{miscible} (\code{TRUE} only when every pair is
#'   fully miscible), and \code{immiscible_pairs} / \code{partial_pairs} / \code{reaction_pairs} (lists of the
#'   offending two-element name vectors).
#' @seealso \code{\link{hsp_solvent_finder}}
#' @export
blend_miscibility <- function(components, misc_Ra = 18, solv_db = NULL) {
  empty <- list(usable = TRUE, miscible = TRUE, immiscible_pairs = list(),
                partial_pairs = list(), reaction_pairs = list())
  comp <- unique(as.character(components)); comp <- comp[!is.na(comp) & nzchar(comp)]
  if (length(comp) < 2) return(empty)
  tbl <- .solvent_miscibility_table()
  if (is.null(solv_db)) solv_db <- tryCatch(show_solv(), error = function(e) NULL)
  if (!is.null(solv_db) && !"dHB" %in% names(solv_db) && "dH" %in% names(solv_db)) solv_db$dHB <- solv_db$dH
  can_hsp <- !is.null(solv_db) && all(c("Solvent", "dD", "dP", "dHB") %in% names(solv_db))
  pair_state <- function(a, b) {
    hit <- which((tbl$s1 == a & tbl$s2 == b) | (tbl$s1 == b & tbl$s2 == a))
    if (length(hit)) return(tbl$state[hit[1]])                          # experimental table wins
    if (can_hsp && all(c(a, b) %in% solv_db$Solvent))                   # fallback: curated rule + Ra screen
      return(if (.hsp_miscible(a, b, solv_db, misc_Ra)) "miscible" else "immiscible")
    "miscible"                                                          # cannot judge -> do not exclude
  }
  imm <- part <- rxn <- list()
  for (p in utils::combn(comp, 2, simplify = FALSE)) {
    st <- pair_state(p[1], p[2])
    if (identical(st, "immiscible")) imm <- c(imm, list(p))
    else if (identical(st, "partial")) part <- c(part, list(p))
    else if (identical(st, "reaction")) rxn <- c(rxn, list(p))
  }
  list(usable = length(imm) == 0 && length(rxn) == 0,
       miscible = length(imm) == 0 && length(part) == 0 && length(rxn) == 0,
       immiscible_pairs = imm, partial_pairs = part, reaction_pairs = rxn)
}

#' Composition- and temperature-aware phase check for a solvent blend
#'
#' Determines whether a set of solvents actually forms a \strong{single liquid phase at the given
#' composition and temperature}, by running the COSMO-SAC-dsp two-phase flash
#' (\code{\link{csac_lle_flash}}) on the neat blend. This is the rigorous complement to
#' \code{\link{blend_miscibility}}, whose curated table + Hansen-distance cutoff is
#' composition- and temperature-independent and can therefore over-flag pairs that are in fact
#' miscible at the chosen ratio and temperature (e.g. methanol + toluene at 40 C, whose upper critical
#' solution temperature lies well below room temperature). When every component resolves in the
#' COSMO-SAC database the flash verdict is used; otherwise it falls back to the table verdict. Chemical
#' reactions are not a phase-equilibrium phenomenon, so the \code{reaction} flag from
#' \code{\link{blend_miscibility}} is always carried through.
#'
#' @param components Character vector of solvent names/keys (as used by \code{show_solv()}).
#' @param x Relative composition, same length/order as \code{components} (normalized internally). Equal
#'   parts when \code{NULL}.
#' @param T Temperature (K; default 298.15).
#' @param database,predict Passed to \code{\link{csac_lle_flash}} (default \code{"ud"}, no FastSigma).
#' @param misc_Ra,solv_db Fallback controls for \code{\link{blend_miscibility}} when a solvent cannot be
#'   resolved by the flash engine.
#' @return A \code{list}: \code{single_phase} (\code{TRUE}/\code{FALSE}/\code{NA} - one homogeneous phase
#'   at these conditions), \code{method} (\code{"flash"} or \code{"table"} - which decided it),
#'   \code{reaction} (\code{TRUE} if the components react), \code{split} (the \code{\link{csac_lle_flash}}
#'   result, or \code{NULL}), and \code{blend_miscibility} (the underlying table verdict).
#' @seealso \code{\link{blend_miscibility}}, \code{\link{csac_lle_flash}}
#' @export
blend_phase_check <- function(components, x = NULL, T = 298.15,
                              database = c("ud", "chaos"), predict = FALSE,
                              misc_Ra = 18, solv_db = NULL) {
  database <- match.arg(database)
  comp <- as.character(components); keep <- !is.na(comp) & nzchar(comp)
  comp <- comp[keep]; if (!is.null(x)) x <- as.numeric(x)[keep]
  bm <- blend_miscibility(comp, misc_Ra = misc_Ra, solv_db = solv_db)
  out <- list(single_phase = TRUE, method = "table", reaction = length(bm$reaction_pairs) > 0,
              split = NULL, blend_miscibility = bm)
  if (length(comp) < 2) return(out)
  # Preferred: COSMO-SAC-dsp flash at the ACTUAL composition and temperature.
  fl <- tryCatch(csac_lle_flash(comp, z = x, T = T[1], database = database, predict = predict),
                 error = function(e) NULL)
  if (!is.null(fl)) {
    out$method <- "flash"; out$split <- fl; out$single_phase <- !isTRUE(fl$split)
    return(out)
  }
  # Fallback: table/HSP verdict (composition-independent).
  out$single_phase <- isTRUE(bm$miscible)
  out
}

#' @title Automatically find the best solvent and enhancing solvent blends for a solute (HSP-based)
#'
#' @description Given a solute's Hansen solubility parameters, rank candidate solvents and propose
#' \strong{miscible binary blends} that improve solubility further. Two modes:
#' \itemize{
#'   \item \strong{SFE mode} (\code{primary = "CO2"}, the default): wraps \code{\link{hsp_optim}} to evaluate
#'         supercritical CO\eqn{_2} + co-solvent systems over a temperature/pressure window, ranking by
#'         % miscibility enhancement of CO\eqn{_2} towards the solute. Requires \code{cp}.
#'   \item \strong{General / ambient mode} (\code{primary} is any other solvent name, or \code{NULL}/"any"):
#'         the primary solvent need NOT be CO\eqn{_2}. Solvents (and blends) are ranked by the Hansen distance
#'         \eqn{R_a=\sqrt{4\Delta\delta_D^2+\Delta\delta_P^2+\Delta\delta_H^2}} between the (volume-fraction
#'         mixed) blend and the solute - lower \eqn{R_a} is better. With \code{primary = "any"} all solvents
#'         and all miscible pairs are searched; with a named \code{primary} the blends are anchored on it.
#' }
#' Blend components are gated by a curated pairwise miscibility screen (see \strong{Details}); this is an HSP
#' screen, not a rigorous liquid-liquid equilibrium calculation.
#'
#' @param hsp_vals A named \code{numeric} vector (length 3-5) of solute HSPs (\code{dD}, \code{dP}, \code{dHB}).
#' @param primary \code{"CO2"} for SFE mode (default); a solvent name in \code{solv_db} to anchor ambient
#'   blends on it; or \code{"any"}/\code{NULL} for a global ambient search of all solvents and pairs.
#' @param cp Single \code{numeric} solute critical point - \strong{required for SFE mode only}.
#' @param candidates \code{character} vector of solvents to consider (must exist in \code{solv_db}). Defaults
#'   to all solvents in \code{solv_db} with valid HSPs (excluding H\eqn{_2}; CO\eqn{_2} excluded in ambient mode).
#' @param pres,temps Numeric SFE pressure (bar) / temperature (deg C) grids (SFE mode only).
#' @param vfrac Co-solvent volume fraction for SFE mode. Defaults to 0.10.
#' @param do_blends \code{logical}; also evaluate enhancing binary blends. Defaults to \code{TRUE}.
#' @param top_n Number of top single solvents to draw blend components from. Defaults to 5.
#' @param ratios Volume-percent splits to sweep for the first blend component. Defaults to \code{c(25,50,75)}.
#' @param misc_Ra Hansen-distance fallback threshold for the miscibility screen (solvents not covered by the
#'   curated rules). Defaults to 18.
#' @param silent \code{logical}; suppress progress messages. Defaults to \code{TRUE}.
#'
#' @details The curated miscibility screen marks water as immiscible with non-polar/aromatic/oil/ether solvents
#' (miscible only with polar organics such as methanol/ethanol/acetone) and methyl oleate as immiscible with
#' methanol/water; all other pairs default to miscible, falling back to the \code{misc_Ra} HSP-distance test for
#' solvents outside the curated set.
#'
#' @return A \code{list}: \code{mode}, \code{metric} ("miscibility_enhancement_pct" or "Ra"),
#'   \code{best_single}, \code{singles_ranked}, and (when \code{do_blends}) \code{blends_ranked} +
#'   \code{best_blend} (top blend that beats the best single, else \code{NULL}).
#' @seealso \code{\link{hsp_optim}}, \code{\link{est_gcm}}
#' @export
hsp_solvent_finder <- function(hsp_vals, primary = "CO2", cp = NULL, candidates = NULL,
                               pres = c(100, 200, 300), temps = c(40, 50, 60), vfrac = 0.10,
                               do_blends = TRUE, top_n = 5, ratios = c(25, 50, 75), misc_Ra = 18,
                               silent = TRUE) {
  # solute HSP -> dD/dP/dHB
  dD_i <- grep("dD", names(hsp_vals)); dP_i <- grep("^dP$", names(hsp_vals)); dH_i <- grep("dHB|^dH$", names(hsp_vals))
  sol <- as.numeric(hsp_vals[c(dD_i[1], dP_i[1], dH_i[1])])
  if (any(is.na(sol))) stop("hsp_vals must contain numeric dD, dP and dHB (dHB) components!")
  sfe <- !is.null(primary) && identical(primary, "CO2")
  sdb <- solv_db[!is.na(solv_db$dD) & solv_db$Solvent != "Hydrogen", , drop = FALSE]
  if (is.null(candidates)) candidates <- sdb$Solvent
  candidates <- setdiff(candidates, "CarbonDioxide")          # CO2 = SFE base / not an ambient liquid, never a candidate
  candidates <- intersect(candidates, sdb$Solvent)
  if (length(candidates) < 2) stop("Need >=2 valid candidate solvents present in solv_db with HSPs!")
  gm <- function(m) mean(as.matrix(m), na.rm = TRUE)

  if (sfe) {                                                                       # ===== SFE / CO2 mode =====
    if (is.null(cp) || !is.numeric(cp)) stop("SFE mode (primary='CO2') requires a numeric solute critical point 'cp'.")
    metric <- "miscibility_enhancement_pct"
    s_res <- hsp_optim(cp, hsp_vals, modif = candidates, pres = pres, temps = temps, vfrac = vfrac, silent = TRUE)
    val <- vapply(s_res[["Miscib_Enhancement"]], gm, numeric(1))
    singles <- data.frame(solvent = candidates, miscibility_enhancement_pct = round(unname(val), 2), stringsAsFactors = FALSE)
    singles <- singles[order(-singles$miscibility_enhancement_pct), , drop = FALSE]
    better <- function(blend, base) blend > base                                  # higher % = better
  } else {                                                                         # ===== general / ambient mode =====
    metric <- "Ra"
    rav <- vapply(candidates, function(s){ i<-which(sdb$Solvent==s); .hsp_Ra(sol, as.numeric(sdb[i, c("dD","dP","dHB")])) }, numeric(1))
    singles <- data.frame(solvent = candidates, Ra = round(unname(rav), 2), stringsAsFactors = FALSE)
    singles <- singles[order(singles$Ra), , drop = FALSE]                         # lower Ra = better
    better <- function(blend, base) blend < base
  }
  rownames(singles) <- NULL
  best_single <- singles$solvent[1]; base_val <- singles[1, metric]
  out <- list(mode = if (sfe) "SFE_CO2" else "ambient", metric = metric,
              best_single = best_single, singles_ranked = singles)

  if (do_blends) {
    # candidate pairs: in anchored ambient mode the primary is fixed; otherwise pairs from the top_n singles
    if (!sfe && !is.null(primary) && !identical(primary, "any") && primary %in% candidates) {
      prs <- lapply(setdiff(candidates, primary), function(s) c(primary, s))
    } else prs <- utils::combn(utils::head(singles$solvent, min(top_n, nrow(singles))), 2, simplify = FALSE)
    prs <- Filter(function(p) .hsp_miscible(p[1], p[2], sdb, misc_Ra), prs)        # miscibility gate
    meta <- data.frame()
    if (length(prs)) {
      if (sfe) {                                                                   # SFE: batch through hsp_optim
        bm <- list(); bf <- list()
        for (p in prs) for (r in ratios) { bm[[length(bm)+1]] <- p; bf[[length(bf)+1]] <- c(r, 100-r)
          meta <- rbind(meta, data.frame(comp1=p[1], comp2=p[2], ratio=paste0(r,":",100-r), stringsAsFactors=FALSE)) }
        b_res <- hsp_optim(cp, hsp_vals, modif = bm, modfracs = bf, pres = pres, temps = temps, vfrac = vfrac, silent = TRUE)
        gmin <- function(m) min(as.matrix(m), na.rm = TRUE); gmax <- function(m) max(as.matrix(m), na.rm = TRUE)
        # min / max / mean of the % miscibility enhancement across the pressure x temperature window.
        meta[["miscibility_enhancement_min"]] <- round(unname(vapply(b_res[["Miscib_Enhancement"]], gmin, numeric(1))), 2)
        meta[["miscibility_enhancement_max"]] <- round(unname(vapply(b_res[["Miscib_Enhancement"]], gmax, numeric(1))), 2)
        meta[[metric]] <- round(unname(vapply(b_res[["Miscib_Enhancement"]], gm, numeric(1))), 2)
      } else {                                                                     # ambient: direct Ra over ratio sweep
        for (p in prs) {
          h1 <- as.numeric(sdb[sdb$Solvent==p[1], c("dD","dP","dHB")]); h2 <- as.numeric(sdb[sdb$Solvent==p[2], c("dD","dP","dHB")])
          for (r in ratios) { b <- (r/100)*h1 + (1-r/100)*h2
            meta <- rbind(meta, data.frame(comp1=p[1], comp2=p[2], ratio=paste0(r,":",100-r), Ra=round(.hsp_Ra(sol, b),2), stringsAsFactors=FALSE)) }
        }
      }
      meta$beats_best_single <- better(meta[[metric]], base_val)
      meta <- meta[order(if (sfe) -meta[[metric]] else meta[[metric]]), , drop = FALSE]; rownames(meta) <- NULL
    }
    out$blends_ranked <- meta
    out$best_blend <- if (nrow(meta) && meta$beats_best_single[1]) meta[1, , drop = FALSE] else NULL
  }
  out
}
