#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MUELLER (2019) automatic molecule fragmentation engine
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Reimplementation (in R + rcdk) of S. Mueller, "Flexible heuristic algorithm for
# automatic molecule fragmentation" (J. Cheminform. 2019, 11:57). Replaces the ad-hoc
# sub_smarts `simplicity` resolver AND the define_grps magic-ID amendment with a
# principled prioritised greedy fragmentation:
#   (1) heuristic group prioritisation via 8 descriptors,
#   (2) parent-child prioritisation (containing pattern searched before contained),
#   (3) adjacent-group search + iterative clear-and-retry to guarantee coverage.
# Each group is defined by one or more SIMPLE SMARTS; no per-method correction code.

# ---- SMARTS descriptor computation (Mueller Table 1) ------------------------
# Tokenise a single SMARTS pattern into atom tokens + bond/structure tokens. Handles bracket
# atoms [..], organic-subset bare atoms, bonds (- = # : ~), ring-closure digits, branches and
# recursive SMARTS $(...). Returns a list with atom descriptors used by .smarts_descriptors().
.smarts_atoms <- function(sm) {
  # strip recursive environments $(...) but remember they contribute "vicinity" atoms (desc 3)
  vic <- 0L
  while (grepl("\\$\\(", sm)) {
    m <- regmatches(sm, regexpr("\\$\\([^()]*\\)", sm))
    if (!length(m)) break
    inner <- sub("^\\$\\(", "", sub("\\)$", "", m))
    vic <- vic + .count_atoms_raw(inner)
    sm <- sub("\\$\\([^()]*\\)", "", sm, fixed = FALSE)
  }
  i <- 1L; n <- nchar(sm); atoms <- list()
  dbl <- 0L; trp <- 0L; ringclose <- c()
  chars <- strsplit(sm, "")[[1]]
  while (i <= n) {
    ch <- chars[i]
    if (ch == "[") {                                   # bracket atom
      j <- i; depth <- 1L
      repeat { j <- j + 1L; if (j > n) break
               if (chars[j] == "[") depth <- depth + 1L
               else if (chars[j] == "]") { depth <- depth - 1L; if (depth == 0L) break } }
      atoms[[length(atoms)+1]] <- paste(chars[i:min(j, n)], collapse = "")
      i <- j + 1L
    } else if (grepl("[A-Za-z]", ch)) {                # organic-subset bare atom (Cl/Br multi-char)
      el <- ch
      if (ch %in% c("C","B") && i < n && chars[i+1] %in% c("l","r")) { el <- paste0(ch, chars[i+1]); i <- i + 1L }
      atoms[[length(atoms)+1]] <- el
      i <- i + 1L
    } else if (ch %in% c("-","=","#",":","~")) {        # explicit bond order
      if (ch == "=") dbl <- dbl + 1L; if (ch == "#") trp <- trp + 1L
      i <- i + 1L
    } else if (grepl("[0-9]", ch)) {                   # ring-closure digit
      ringclose <- c(ringclose, ch); i <- i + 1L
    } else i <- i + 1L                                  # ( ) and other
  }
  # connected single-pattern => bonds = (atoms-1) + ring closures
  ring_bonds <- length(ringclose) / 2
  bonds <- max(0, length(atoms) - 1L) + ring_bonds
  list(atoms = atoms, n_atoms = length(atoms) + vic, bonds = bonds,
       dbl = dbl, trp = trp, ringclose = ringclose)
}

.count_atoms_raw <- function(sm) {
  # crude atom count inside a recursive environment (for descriptor 3 vicinity)
  length(gregexpr("\\[|(?<![A-Za-z])(Cl|Br|[BCNOPSFIcnops])", sm, perl = TRUE)[[1]])
}

# element + explicit-H of a bracket/bare atom token, for valence / heteroatom analysis
.atom_element <- function(tok) {
  t <- gsub("[][]", "", tok)
  el <- regmatches(t, regexpr("Cl|Br|[A-Za-z]", t))
  if (!length(el)) el <- "*"
  toupper(substr(el, 1, 1)) -> first
  list(el = el, arom = grepl("^[a-z]", el))
}

# Normal valence for free-bond (descriptor 1) analysis.
.valence <- c(C=4, c=4, N=3, n=3, O=2, o=2, S=2, s=2, P=3, F=1, Cl=1, Br=1, I=1, H=1, B=3, Si=4)

#' @title Mueller (2019) fragmentation descriptors for a SMARTS group pattern
#' @description Computes the 8 heuristic descriptors used by Mueller (2019) to prioritise group
#'   patterns during automatic fragmentation. For a group defined by several SMARTS (comma list),
#'   pass each; the engine uses the representative (first) pattern's descriptors.
#' @param sm A single SMARTS string.
#' @return Named integer vector \code{d1..d8}: 1 zero free bonds (complete molecule); 2 simple
#'   (single valence-1 atom, optionally on a carbon); 3 number of atoms (incl. recursive vicinity);
#'   4 exactly one available bond; 5 number of non-H non-C atoms; 6 includes ring atoms; 7 triple
#'   bonds; 8 double bonds.
#' @keywords internal
gcm_descriptors <- function(sm) {
  a <- .smarts_atoms(sm)
  if (!length(a$atoms)) return(c(d1=0L,d2=0L,d3=0L,d4=0L,d5=0L,d6=0L,d7=a$trp,d8=a$dbl))
  els  <- lapply(a$atoms, .atom_element)
  elem <- vapply(els, function(x) x$el, character(1))
  arom <- vapply(els, function(x) x$arom, logical(1))
  val1 <- function(e) { v <- unname(.valence[e]); if (is.na(v)) 4 else v }   # valence with default (safe lookup)
  n_hetero <- sum(!toupper(elem) %in% c("C","H") & elem != "*")
  # free valences: sum(valence - explicit H) - 2*(single bonds) - 2*double - 4*triple. <=0 => complete.
  hcount <- function(tok) { hm <- regmatches(tok, regexpr("H[0-9]?", tok))
    if (length(hm)) { d <- gsub("H","",hm); if (d=="") 1L else as.integer(d) } else 0L }
  totval <- sum(vapply(elem, val1, numeric(1)))
  totH   <- sum(vapply(a$atoms, hcount, integer(1)))
  freeval <- totval - totH - 2*a$bonds - 2*a$dbl - 4*a$trp
  open_bonds <- max(0, freeval)
  # ring: explicit ring-closure, aromatic atom, or a POSITIVE ring primitive R/r in a bracket. Must
  # NOT count NEGATED / not-in-ring primitives (!R, !r, R0, r0) - else a generic non-ring group like
  # [CH0;!R;...] gets a false has_ring=1 and wrongly outranks real multi-atom groups (e.g. the
  # [CH0]#[CH0] alkyne), stealing their atoms (was: 2-butyne -> 2x"C" -> Tb = -63 K).
  ringprim <- any(vapply(a$atoms, function(t)
    grepl("[Rr]", gsub("!R[0-9]*|!r[0-9]*|R0|r0", "", gsub("Cl|Br", "", t))), logical(1)))
  has_ring <- length(a$ringclose) > 0 || any(arom) || ringprim
  d1 <- as.integer(freeval <= 0)
  d2 <- as.integer((length(a$atoms) == 1 && val1(elem[1]) == 1) ||
                   (length(a$atoms) == 2 && any(vapply(elem, val1, numeric(1)) == 1) && any(toupper(elem) == "C")))
  c(d1=d1, d2=d2, d3=as.integer(a$n_atoms), d4=as.integer(open_bonds == 1),
    d5=as.integer(n_hetero), d6=as.integer(has_ring), d7=as.integer(a$trp), d8=as.integer(a$dbl))
}

# Split a group's SMARTS field into individual patterns on TOP-LEVEL commas only (commas inside
# [...] are part of a single pattern, e.g. [F,Cl,Br,I] or [CX4;H1,H2]).
.split_smarts <- function(smarts) {
  ch <- strsplit(smarts, "")[[1]]; depth <- 0L; parts <- character(0); cur <- ""
  for (c in ch) {
    if (c == "[") depth <- depth + 1L else if (c == "]") depth <- depth - 1L
    if (c == "," && depth == 0L) { parts <- c(parts, cur); cur <- "" } else cur <- paste0(cur, c)
  }
  trimws(c(parts, cur))
}

# ---- molecule preparation for SMARTS matching (run once per molecule) --------
# Keep hydrogens EXPLICIT (as mol_find produces them — several SMARTS match an explicit-H atom, e.g.
# the SP cyclic-N group [#7;R1]([#1,C])([!#1])([!#1]); suppressing H would break them). Perceive atom
# types + ring membership, then apply the cdkLegacy aromaticity model (identical to the legacy
# SMARTSQueryTool the SMARTS were authored against). Finally DE-AROMATIZE any carbon bearing an
# exocyclic C=O double bond: a carbonyl carbon is never aromatic for GCM purposes, but CDK's model
# over-aromatizes them in large fused systems (e.g. indanthrone), mis-routing the carbonyl to the
# wrong/unparametrised group. Idempotent via a molecule-property flag.
.prep_mol_match <- function(mol) {
  flag <- tryCatch(rcdk::get.property(mol, "supeRcrit.prepped"), error = function(e) NA)
  if (!is.null(flag) && !is.na(flag)) return(invisible(mol))
  ACM <- "org/openscience/cdk/tools/manipulator/AtomContainerManipulator"
  rJava::.jcall(ACM, "V", "percieveAtomTypesAndConfigureAtoms", mol)
  rJava::J("org.openscience.cdk.graph.Cycles")$markRingAtomsAndBonds(mol)
  rJava::J("org.openscience.cdk.aromaticity.Aromaticity")$cdkLegacy()$apply(mol)
  for (b in rcdk::get.bonds(mol)) {
    ord <- tryCatch(rJava::.jcall(rJava::.jcall(b, "Lorg/openscience/cdk/interfaces/IBond$Order;",
              "getOrder"), "S", "toString"), error = function(e) "")
    if (ord != "DOUBLE") next
    a0 <- rJava::.jcall(b, "Lorg/openscience/cdk/interfaces/IAtom;", "getAtom", 0L)
    a1 <- rJava::.jcall(b, "Lorg/openscience/cdk/interfaces/IAtom;", "getAtom", 1L)
    s0 <- rcdk::get.symbol(a0); s1 <- rcdk::get.symbol(a1)
    if (s0 == "C" && s1 == "O") rJava::.jcall(a0, "V", "setFlag", 32L, FALSE)
    if (s1 == "C" && s0 == "O") rJava::.jcall(a1, "V", "setFlag", 32L, FALSE)
  }
  tryCatch(rcdk::set.property(mol, "supeRcrit.prepped", "1"), error = function(e) NULL)
  invisible(mol)
}

# Compiled-pattern cache. CDK SmartsPattern is immutable + designed for create-once-reuse (matchAll
# stores no per-molecule state, and setPrepare(FALSE) means no molecule-specific work), so caching one
# compiled pattern per distinct SMARTS string is identical to recreating it but avoids re-parsing on
# every call. Patterns are static (from sysdata) so no invalidation is needed; the env holds <=~1182
# Java refs, valid for the session/JVM lifetime. Parse failures are cached as a NULL sentinel.
.smarts_cache <- new.env(parent = emptyenv())
.get_pattern <- function(p) {
  if (exists(p, envir = .smarts_cache, inherits = FALSE)) return(get(p, envir = .smarts_cache, inherits = FALSE))
  ptn <- tryCatch({
    q <- rJava::J("org.openscience.cdk.smarts.SmartsPattern")$create(p)
    q$setPrepare(FALSE); q
  }, error = function(e) NULL)
  assign(p, ptn, envir = .smarts_cache)
  ptn
}

# ---- matches: unique heavy-atom-index sets for one SMARTS on a molecule ------
# Uses CDK's modern SmartsPattern with setPrepare(FALSE) so matching RESPECTS the aromaticity flags
# set by .prep_mol_match. (The legacy rcdk::matches / SMARTSQueryTool always self-re-perceives with
# cdkLegacy and would undo the carbonyl de-aromatization, re-introducing the over-aromatisation bug.)
.group_matches <- function(smarts, mol) {
  .prep_mol_match(mol)
  sets <- list()
  for (p in .split_smarts(smarts)) {
    if (!nzchar(p)) next
    ptn <- .get_pattern(p)
    if (is.null(ptn)) next
    mm <- tryCatch(ptn$matchAll(mol)$uniqueAtoms(), error = function(e) NULL)
    if (is.null(mm)) next
    rows <- tryCatch(rJava::.jevalArray(mm$toArray()), error = function(e) list())
    for (r in rows) sets[[length(sets)+1]] <- sort(unique(as.integer(rJava::.jevalArray(r))))
  }
  unique(sets)
}

#' @title Müller (2019) automatic fragmentation of a molecule into GCM groups
#' @description Greedy prioritised fragmentation: every group's SMARTS matches are found, groups are
#'   ranked by the 8 heuristic descriptors (complete-molecule and larger/more-specific groups first),
#'   and atom sets are assigned greedily without overlap. This replaces the ad-hoc \code{sub_smarts}
#'   resolver + \code{define_grps} amendment.
#' @param solute Output of \code{\link{mol_find}}.
#' @param scheme A \code{data.frame} with columns \code{id}, \code{name}, \code{smarts} (one or more
#'   comma-separated SMARTS per group).
#' @return A \code{list}: \code{counts} (named group-id -> occurrence), \code{covered}/\code{n_heavy}
#'   atom coverage, and \code{assignment} (atom -> group id).
#' @keywords internal
gcm_fragment <- function(solute, scheme, mode = "simple") {
  mol <- solute[["Molfile"]]
  syms <- vapply(rcdk::get.atoms(mol), rcdk::get.symbol, character(1))
  heavy <- which(syms != "H")
  # descriptor matrix per group (use representative = first SMARTS). Split on TOP-LEVEL commas only
  # (via .split_smarts) so a single SMARTS carrying an internal atom-list like [#6,#7] or [F,Cl,Br,I]
  # is NOT truncated to a malformed fragment -- a naive strsplit(s, ",")[[1]][1] would mangle the
  # descriptor and mis-order the group (e.g. a 2-atom carbonyl losing its O to the 1-atom O catch-all).
  desc <- t(vapply(scheme$smarts, function(s) gcm_descriptors(.split_smarts(s)[1]), integer(8)))
  # Matching order: when the scheme carries Nannoolal's explicit PRIORITY (pr; lower = claimed first), use it as
  # the PRIMARY key with the descriptor heuristic as tiebreaker; otherwise fall back to descriptor only. This
  # makes NL fragmentation follow the paper's authoritative order; other methods are unaffected (pr all NA).
  pr <- scheme$pr
  if (!is.null(pr) && any(!is.na(pr))) {
    prk <- ifelse(is.na(pr), max(pr, na.rm = TRUE) + 1L, pr)
    ord <- order(prk, -desc[,1], -desc[,2], -desc[,3], -desc[,4], -desc[,5], -desc[,6], -desc[,7], -desc[,8])
  } else {
    ord <- order(-desc[,1], -desc[,2], -desc[,3], -desc[,4], -desc[,5], -desc[,6], -desc[,7], -desc[,8])
  }
  assigned <- integer(0)                                   # heavy atoms already taken
  counts <- setNames(integer(nrow(scheme)), scheme$id)
  assignment <- setNames(rep(NA_character_, length(heavy)), as.character(heavy))
  group_atoms <- list()                                    # group id -> 0-based atom indices it actually claimed
  for (gi in ord) {
    sets <- .group_matches(scheme$smarts[gi], mol)
    if (!length(sets)) next
    # within a group, take largest atom-sets first, then any not overlapping assigned atoms
    sets <- sets[order(-lengths(sets))]
    for (s in sets) {
      # group SMARTS map only heavy atoms; use the match atom-set directly (no heavy re-index)
      if (length(s) && !any(s %in% assigned)) {
        assigned <- c(assigned, s)
        gid <- as.character(scheme$id[gi])
        counts[gid] <- counts[gid] + 1L
        assignment[as.character(s)] <- gid
        group_atoms[[gid]] <- c(group_atoms[[gid]], s)     # exact disjoint assignment (for visualisation)
      }
    }
  }
  list(counts = counts[counts > 0], covered = length(assigned), n_heavy = length(heavy),
       assignment = assignment, group_atoms = group_atoms)
}

#' @title Disjoint first-order group -> atom-index assignment for a molecule under a method
#' @description Runs the SAME first-order disjoint fragmentation the prediction uses, and returns the
#'   exact \code{group id -> 0-based atom indices} assignment. Used by \code{\link{plot_gcm}} to highlight
#'   each group on the depiction by the atoms it actually claimed (rather than re-matching SMARTS, whose
#'   aromaticity model can disagree), so the picture is guaranteed to match the fragmentation.
#' @param mol A CDK \code{IAtomContainer} (jobjRef) — the molecule whose atoms will be highlighted.
#' @param method GCM method id.
#' @param order Group order to return: 1 = the disjoint first-order assignment; 2/3 = the (overlapping)
#'   second/third-order CORRECTION groups' raw atom matches. Higher orders rarely have spatial groups
#'   (e.g. Nannoolal interactions are non-local), so they may return an empty list. @keywords internal
.frag_group_atoms <- function(mol, method, order = 1L) {
  sc <- .method_scheme(method); sc$id <- as.character(sc$ID); sc$name <- sc$Group
  order <- as.integer(order)[1]
  if (is.na(order) || order <= 1L) {
    is_nl <- any(c("NL04", "NL07", "NL07_robust") %in% method)
    t1 <- sc$Type == 1 & !(is_nl & sc$id == "35")
    if (!any(t1)) return(list())
    return(gcm_fragment(list(Molfile = mol), sc[t1, c("id", "name", "smarts", "pr")])[["group_atoms"]])
  }
  gi <- which(sc$Type == order); if (!length(gi)) return(list())
  ga <- list()
  for (k in gi) { ms <- .group_matches(sc$smarts[k], mol); if (length(ms)) ga[[sc$id[k]]] <- unique(unlist(ms)) }
  ga
}

# ---- method scheme from sysdata (Available groups only) ---------------------
.method_scheme <- function(method) {
  cn <- switch(method, HKR_STW = "HKR_2012_STW_cnt", HKR_SIM = "HKR_2012_SIM_cnt",
               JR = "JR_1987_cnt", JR_corr = "JR_1987_cnt", SB = "SB_1994_cnt", SB_corr = "SB_1994_cnt",
               SP08 = "SP_2008_cnt", SP12 = "SP_2012_cnt", SP_ENK = "SP_ENK_cnt", ZHAO = "ZHAO_2003_cnt", BND = "BND_1964_cnt",
               SLON = "SLON_1970_cnt", CHRASTIL = "CHRASTIL_2024_cnt",
               NL04 = "NL_2004_2007_cnt", NL07 = "NL_2004_2007_cnt", NL07_robust = "NL_2004_2007_cnt")
  sn <- switch(method, HKR_STW = , HKR_SIM = "HKR_2012_SMARTS", JR = , JR_corr = "JR_1987_SMARTS",
               SB = , SB_corr = "SB_1994_SMARTS", SP08 = "SP_2008_SMARTS", SP12 = "SP_2012_SMARTS",
               SP_ENK = "SP_ENK_SMARTS",
               ZHAO = "ZHAO_2003_SMARTS", BND = "BND_1964_SMARTS", SLON = "SLON_1970_SMARTS",
               CHRASTIL = "CHRASTIL_2024_SMARTS", NL04 = , NL07 = , NL07_robust = "NL_2004_2007_SMARTS")
  cnt <- gcm_cnt[[cn]]; sm <- gcm_smarts[[sn]]
  smstr <- setNames(sm$SMARTS, as.character(sm$ID))
  # SP/NL/etc. use an "Available"/"Unavailable" FLAG in cnt$SMARTS; the VDW tables (BND/ZHAO/SLON) store the
  # actual SMARTS pattern there instead. Only treat the column as a flag when it actually uses those tokens.
  avail <- if (!is.null(cnt$SMARTS) && any(cnt$SMARTS %in% c("Available", "Unavailable"), na.rm = TRUE))
             cnt$SMARTS == "Available" else rep(TRUE, nrow(cnt))
  cnt <- cnt[avail & !is.na(cnt$ID), , drop = FALSE]
  cnt$smarts <- smstr[as.character(cnt$ID)]
  cnt <- cnt[!is.na(cnt$smarts) & nzchar(cnt$smarts), , drop = FALSE]
  cnt$Type <- if (is.null(cnt$Type)) 1L else cnt$Type
  # Nannoolal explicit fragmentation PRIORITY (lower PR = claimed first). Only NL_2004_2007_cnt carries a PR
  # column; other methods fall back to the descriptor heuristic (cnt$pr all NA -> ignored in gcm_fragment).
  cnt$pr <- if (!is.null(cnt$PR)) as.integer(cnt$PR) else NA_integer_
  cnt
}

# ---- Nannoolal group-interaction contributions (ported from gcm_contribs, for the engine) ---------
# Map detected group occurrences -> interaction-group occurrences (define_grps lines 594-613).
.nl_inters_from_occ <- function(occ) {
  g <- function(ids) sum(suppressWarnings(as.numeric(occ[ids])), na.rm = TRUE)
  c(OH = g(c("33","34","35","36","301")), OH_a = g("37"), COOH = g(c("44","302")),
    EtherO = g("38"), Epox = g("39"), Ester = g(c("45","46","47","303")),
    Ketone = g(c("51","92")), Alde = g(c("52","90")), AO = g("65"), Teth = g("54"),
    Ats = g("56"), SH = g("53"), NH2 = g(c("40","41")), NH = g(c("42","97")),
    OCN = g("80"), CN = g("57"), Nitro = g("69"), AN5 = g("66"), AN6 = g("67"))
}

# Build the Nannoolal pairwise interaction rows (Type 3) for the result table. cont_cols = the param
# *_cont column names in the order they appear in the result data.frame (so rows align for rbind).
.nl_interactions <- function(inters, cont_cols, method, atoms_sans_H) {
  if (!any(inters > 0) || !is.finite(atoms_sans_H) || atoms_sans_H <= 0) return(data.frame())
  ID_vec <- setNames(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S")[seq_along(inters)], names(inters))
  suffix <- if (any(method %in% "NL07_robust")) "_robust" else ""
  ni <- gcm_int[grep(paste0("\\Cint", suffix), names(gcm_int))]
  ni <- ni[c(grep("PCint", names(ni)), grep("TCint", names(ni)), grep("VCint", names(ni)))]
  names(ni) <- c("Pc_cont", "Tc_cont", "Vc_cont")
  ni[["Tb_cont"]] <- gcm_int[["NL_2004_BPint"]]
  ni <- ni[cont_cols[cont_cols %in% names(ni)]]                 # order to match the result columns
  ex <- ni[[1]]
  df <- expand.grid(Group1 = rownames(ex), Group2 = colnames(ex), stringsAsFactors = FALSE)
  df <- df[!duplicated(apply(df, 1, function(x) paste(sort(x), collapse = "_"))), , drop = FALSE]
  for (i in names(inters)) {
    df[df$Group1 == i, "Group1_Occ"] <- inters[i]; df[df$Group2 == i, "Group2_Occ"] <- inters[i]
    df[df$Group1 == i, "Group1_ID"] <- ID_vec[i];  df[df$Group2 == i, "Group2_ID"] <- ID_vec[i]
  }
  df <- df[!is.na(df$Group1_Occ) & !is.na(df$Group2_Occ) & df$Group1_Occ != 0 & df$Group2_Occ != 0, , drop = FALSE]
  if (!nrow(df)) return(data.frame())
  df$Occurrences <- vapply(seq_len(nrow(df)), function(x) {
    r <- if (df$Group1[x] == df$Group2[x]) { if (df$Group1_Occ[x] > 1) df$Group1_Occ[x] / 2 else 0
    } else df$Group1_Occ[x] * df$Group2_Occ[x]
    r / atoms_sans_H
  }, numeric(1))
  df <- df[df$Occurrences != 0, , drop = FALSE]
  if (!nrow(df)) return(data.frame())
  df$Group <- paste0(df$Group1, " to ", df$Group2); df$Type <- 3L
  df$ID <- paste0(df$Group1_ID, "-", df$Group2_ID)
  for (i in names(ni)) {
    df[[i]] <- vapply(seq_len(nrow(df)), function(j) ni[[i]][rownames(ni[[i]]) == df$Group1[j], colnames(ni[[i]]) == df$Group2[j]], numeric(1))
    df[[paste0("Sum_", i)]] <- df[[i]] * df$Occurrences
  }
  df
}

#' @title Müller-engine group contributions (drop-in for sub_smarts -> define_grps -> gcm_contribs)
#' @description Fragments a molecule with the Müller engine and returns the same \code{contribs}
#'   structure consumed by \code{gcm_bp}/\code{gcm_crit}/\code{gcm_hsp}/\code{gcm_vdw}. First-order
#'   groups (Type 1) are assigned by disjoint greedy fragmentation; higher-order CORRECTION groups
#'   (Type 2/3) are counted by all unique atom-set matches (overlap allowed). No \code{define_grps}.
#' @param solute Output of \code{\link{mol_find}}. @param method GCM method id.
#' @return \code{list(results, solute, smarts, method)} as from \code{\link{gcm_contribs_muller}}.
#' @keywords internal
gcm_contribs_muller <- function(solute, method, gorder = 0) {
  sc <- .method_scheme(method); mol <- solute[["Molfile"]]
  sc$id <- as.character(sc$ID); sc$name <- sc$Group
  occ <- setNames(integer(nrow(sc)), sc$id)
  # gorder = MAXIMUM group order to include (0 / NA = all available orders, the default & back-compatible
  # behaviour). Higher-order corrections beyond `cap` are skipped: cap = 1 -> first-order groups ONLY;
  # cap = 2 -> + 2nd-order corrections (incl. the Nannoolal group interactions, which are NL's order-2
  # effect); cap >= 3 -> + 3rd-order (Hukkerikar/Marrero-Gani). Lets the user fall back to a simpler model.
  cap <- suppressWarnings(as.integer(gorder)[1])
  if (length(cap) == 0L || is.na(cap) || cap <= 0L) cap <- Inf
  # Nannoolal group 35 (-OH >=C4) is a define_grps-only placeholder with a catch-all [**] SMARTS; it is
  # set from group 36 by the size rule below, so keep it OUT of the disjoint first-order matching. This
  # exclusion is NL-SPECIFIC: id 35 is a real first-order group in other methods (JR "-N::: (ring)",
  # SP "CH2N", SB "-C(O)N<", CHRASTIL "-CN"), so excluding it for them left atoms uncovered (e.g. the
  # caffeine ring N -> false "Rough"/missing-atom flag and a dropped contribution).
  is_nl <- any(c("NL04", "NL07", "NL07_robust") %in% method)
  t1 <- sc$Type == 1 & !(is_nl & sc$id == "35")
  covered <- n_heavy <- NA_integer_
  if (any(t1)) { fr <- gcm_fragment(solute, sc[t1, c("id","name","smarts","pr")]); occ[names(fr$counts)] <- fr$counts
    covered <- fr$covered; n_heavy <- fr$n_heavy }
  # Higher-order (Marrero-Gani / Hukkerikar Type 2 & 3) CORRECTIONS: overlapping, counted on top
  # of first-order. Collect every matched atom-set, then apply the Marrero-Gani (2001) complete-
  # overlap rule: "if one group embodies completely another, the former must be chosen" -- i.e.
  # drop any occurrence whose atom-set is a STRICT SUBSET of another occurrence of the SAME order
  # (l-alanine: CHm(NHn)COOH is kept, the contained CHm-COOH is dropped). Applied within each
  # order (2nd vs 2nd, 3rd vs 3rd), matching Eq.(1)'s separate w- and z-weighted sums.
  ho <- list()
  for (k in which(sc$Type %in% c(2, 3) & sc$Type <= cap))     # gorder cap: skip orders above the requested max
    for (s in .group_matches(sc$smarts[k], mol))
      ho[[length(ho) + 1L]] <- list(id = sc$id[k], type = sc$Type[k], set = s)
  if (length(ho)) {
    # Complete-overlap rule applies to multi-atom FRAGMENT groups that are nested alternatives
    # (AROMRINGs1s3 in s1s3s5; CHm-COOH in CHm(NHn)COOH; NL 119 in 120). A SINGLE-ATOM higher-order
    # group encodes an intrinsic per-atom property correction (e.g. NL 121 "carbon with three halogens",
    # 122 "secondary C with two halogens") and legitimately coexists with any larger fragment that spans
    # its atom (paper ground truth: perfluoro-2-propanone has BOTH 120 and 2x121), so it is never pruned.
    # The SP (Stefanis-Panayiotou) "ring of N carbons" groups (830 C5, 831 C6, 856 C3) are whole-ring
    # SIZE descriptors, not fragment groups that "embody" their contents. The paper co-counts them with
    # the conjugation operators they enclose (e.g. methylcyclopentadiene = ring-of-5 + -C=C-C=C- on the
    # SAME atoms; worked example, Stefanis 2012). So a ring-size group must NEVER prune another group.
    # The Enekvist (2021) NEW second-order groups (ids 9001-9014) are CONJUGATION corrections, not
    # Marrero-Gani fragments: Table 3 co-counts e.g. PB60 AC-N<(4) WITH AC-N-AC(2) on overlapping atoms,
    # and AC-N-AC itself overlaps the existing N{H/C}cyclic group. So they neither prune nor are pruned
    # (counted by raw match like the ring-size and single-atom higher-order groups).
    ring_size_ids <- c("830", "831", "856")
    conj_ids <- as.character(9001:9014)
    no_prune <- c(ring_size_ids, conj_ids)
    keep <- rep(TRUE, length(ho))
    for (a in seq_along(ho)) for (b in seq_along(ho))
      if (a != b && keep[a] && ho[[a]]$type == ho[[b]]$type && length(ho[[a]]$set) > 1L &&
          length(ho[[a]]$set) < length(ho[[b]]$set) && all(ho[[a]]$set %in% ho[[b]]$set) &&
          !(ho[[a]]$id %in% conj_ids) && !(ho[[b]]$id %in% no_prune)) {
        keep[a] <- FALSE; break
      }
    for (i in which(keep)) occ[ho[[i]]$id] <- occ[ho[[i]]$id] + 1L
  }
  # Nannoolal whole-molecule correction flags (counted once per molecule, not per atom):
  # 123 "component has no hydrogen", 124 "component has one hydrogen". The generic Type-2 matcher
  # mis-handles these (their placeholder SMARTS is [H]); set them from the actual H count instead.
  if (any(c("NL04","NL07","NL07_robust") %in% method)) {
    # OH size split (define_grps): a primary OH (36, -OH<C5) in a molecule with >4 carbons is the larger
    # "-OH >=C4" group (35) instead. This is a FIRST-ORDER group reassignment, so it applies at every
    # gorder. 35 carries a different contribution, so reassign here.
    if (all(c("35","36") %in% sc$id)) {
      occ["35"] <- 0L
      if (occ["36"] > 0 && length(.group_matches("[#6]", mol)) > 4) { occ["35"] <- occ["36"]; occ["36"] <- 0L }
    }
    if (cap >= 2L) {   # second-order Nannoolal structural corrections (skipped when gorder caps at 1)
      # Whole-molecule correction flags (counted once per molecule): 123 "no hydrogen", 124 "one hydrogen".
      # The generic Type-2 matcher mis-handles these (placeholder SMARTS [H]); set from the actual H count.
      nH <- suppressWarnings(as.numeric(solute[["IDs"]]["H_Count"]))
      if (!is.na(nH)) {
        if ("123" %in% sc$id) occ["123"] <- as.integer(nH == 0)
        if ("124" %in% sc$id) occ["124"] <- as.integer(nH == 1)
      }
      # Aromatic ortho/meta/para corrections (127/128/129): Nannoolal counts ONE per molecule with a
      # priority ortho > meta > para (a ring's correction fires only if no higher-priority pair exists).
      # Apply on RAW match counts (bypassing the per-atom pruning above), mirroring define_grps.
      rawn <- function(id) if (id %in% sc$id) length(.group_matches(sc$smarts[match(id, sc$id)], mol)) else 0L
      o <- rawn("127"); m <- rawn("128"); p <- rawn("129")
      n127 <- as.integer(o > 0 & m == 0 & p == 0)
      n128 <- as.integer(n127 == 0 & m %in% c(1, 3) & p == 0)
      n129 <- as.integer(n127 == 0 & n128 == 0 & p == 2)
      if ("127" %in% sc$id) occ["127"] <- n127
      if ("128" %in% sc$id) occ["128"] <- n128
      if ("129" %in% sc$id) occ["129"] <- n129
      # group 125 (3/4-membered ring): fold the 4-ring helper (1251, no own contribution) into 125, as
      # define_grps does. 125's own SMARTS is the 3-ring (kept comma-free so the legacy path stays valid).
      if ("125" %in% sc$id) {
        fr <- gcm_smarts[["NL_2004_2007_SMARTS"]]$SMARTS[as.character(gcm_smarts[["NL_2004_2007_SMARTS"]]$ID) == "1251"]
        if (length(fr) && nzchar(fr)) occ["125"] <- occ["125"] + length(.group_matches(fr, mol))
      }
    }
  }
  res <- sc[occ[sc$id] > 0, , drop = FALSE]; res$Occurrences <- occ[res$id]
  ccols <- grep("_cont$", names(res), value = TRUE)
  for (cc in ccols) res[[paste0("Sum_", cc)]] <- suppressWarnings(as.numeric(res[[cc]])) * res$Occurrences
  gsmarts <- setNames(res$smarts, res$Type)
  # Nannoolal group interactions (Type-3 rows folded into the results, as gcm_contribs does) — needed
  # for NL07/NL07_robust Tc/Pc/Vc and the NL04 Tb interaction term. These are NL's order-2 effect, so
  # they are gated at cap >= 2 (skipped when the user requests first-order only).
  if (any(c("NL04","NL07","NL07_robust") %in% method) && cap >= 2L) {
    ash <- as.numeric(solute[["IDs"]]["Atom_Count"]) - as.numeric(solute[["IDs"]]["H_Count"])
    cont_cols <- setdiff(ccols, grep("^Sum_", ccols, value = TRUE))
    ir <- .nl_interactions(.nl_inters_from_occ(occ), cont_cols, method, ash)
    if (nrow(ir)) {
      for (cc in setdiff(names(res), names(ir))) ir[[cc]] <- NA
      res <- rbind(res[, names(res), drop = FALSE], ir[, names(res), drop = FALSE])
    }
  }
  # name smarts by group Type (as the gcm_contribs path does) so plot_gcm can filter to 1st-order.
  # coverage: heavy atoms assigned to first-order groups vs total; <full means the molecule has atoms
  # outside the method's group set (out-of-domain) -> est_gcm flags it so the estimate is not trusted silently.
  list(results = res, solute = solute, smarts = gsmarts, method = method,
       coverage = list(covered = covered, n_heavy = n_heavy))
}

#' @title Route group-contribution calculation through the Müller engine
#' @description Single entry point for GCM group contributions. ALL methods now use the disjoint Müller
#'   fragmentation engine (\code{\link{gcm_contribs_muller}}); the legacy \code{sub_smarts -> define_grps ->
#'   gcm_contribs} chain has been fully retired. ZHAO/SLON are geometric VDW methods whose value is computed
#'   directly in \code{gcm_vdw} from molecular geometry (their engine decomposition is built for the
#'   group display but ignored by the VDW calculation).
#' @param solute Output of \code{\link{mol_find}}. @param method GCM method id.
#' @param gorder Maximum group order to include (0 = all available orders, the default). 1 = first-order
#'   groups only; 2 = + second-order corrections; 3 = + third-order. Lets the user request a simpler model.
#' @param simplicity,silent Accepted for back-compatibility; ignored by the engine.
#' @keywords internal
gcm_contribs_route <- function(solute, method, simplicity = "auto", gorder = 0, silent = TRUE) {
  gcm_contribs_muller(solute, method, gorder = gorder)
}
