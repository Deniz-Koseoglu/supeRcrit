#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# GC-EOS / GCA-EOS MOLECULAR GROUP DECOMPOSITION
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Decomposes a molecule (SMILES) into the functional groups of the chosen
# equation-of-state model, using the SMARTS patterns shipped in gceos$smarts /
# gcaeos$smarts and the same rcdk substructure-matching machinery the package
# uses for its group-contribution property methods (gcm_smarts).

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FUNCTION: Decompose a molecule into GC-EOS / GCA-EOS groups
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Decompose a molecule into GC-EOS / GCA-EOS functional groups
#'
#' @description
#' Fragments a molecule, given as a SMILES string, into the functional-group
#' counts (\eqn{\nu_j}, the number of each group) used by the chosen
#' group-contribution equation of state. The groups and their parameters are
#' those returned by \code{\link{gceos_params}}; the matching uses the SMARTS
#' patterns in the model's \code{$smarts} table.
#'
#' @details
#' Decomposition is a precedence-ordered, atom-claiming fragmentation. Each group
#' carries an \code{order} (lower = higher precedence); groups are matched from
#' most specific to most generic, and every match claims its atoms so a generic
#' group (e.g. \code{CH2}) never double-counts atoms already taken by a specific
#' one (e.g. \code{COOH}). Patterns use \pkg{rcdk}'s \code{\link[rcdk]{matches}}.
#'
#' The two models fragment some functionalities differently, matching their
#' published group definitions: GC-EOS splits a carboxylic acid into
#' \code{CH3 + CH2 + COOH} and an alcohol into \code{... + CH2OH}, whereas
#' GCA-EOS uses compound acid groups (\code{CH3COOH}, \code{CH2COOH}, the general
#' acid group, \code{HCOOH}) and whole-molecule groups for methanol/ethanol
#' (\code{CH3OH}, \code{C2H5OH}).
#'
#' \strong{Scope.} The shipped SMARTS cover the organic groups relevant to the
#' module's target processes (paraffinic, cyclic, olefinic, aromatic, alcohol,
#' carbonyl, ester, carboxylic-acid groups, plus CO2 and water). Context-specific
#' GC-EOS variants that depend on the wider molecular environment - the
#' water-soluble \code{WS} groups and the triacylglyceride \code{TG} group - and
#' the inorganic/gas groups are not auto-detected and must be assigned manually
#' where needed. The bulky \code{(B)} groups (attached to a quaternary carbon)
#' \emph{are} detected. For \code{"gcaeos"}, branched aliphatic alkanes/alcohols
#' are additionally re-expressed in the Soria (2011) combined-group convention
#' (a methine + one CH3 \eqn{\to} the combined \code{CHCH3} group; a quaternary
#' carbon contributes no group, tagging its neighbours \code{(B)CH3}/\code{(B)CH2}).
#' This conversion is gated to branched, fully-aliphatic molecules, so aromatic,
#' acid, ester, cyclic and linear species are returned unchanged. When a methine
#' has no adjacent CH3 (so \code{CHCH3} is formed non-adjacently, as in
#' Gonzalez Prieto's practice), an advisory warning is emitted.
#'
#' @param solute The molecule to decompose. Either a SMILES \code{character}
#'   string, or a solute \code{list} as returned by \code{\link{mol_find}} (its
#'   \code{$Molfile} element is used). The latter form lets the decomposition
#'   chain directly with the package's molecule-handling workflow
#'   (\code{\link{mol_find}} / \code{\link{est_gcm}}), reusing the same molecule
#'   object - and hence the same aromaticity perception - as the
#'   group-contribution property methods.
#' @param model Character scalar: \code{"gceos"} (default) or \code{"gcaeos"}.
#'
#' @return A named integer vector of group counts (only groups present are
#'   returned), in decomposition-precedence order. Atoms not claimed by any
#'   shipped pattern (e.g. unsupported heteroatoms) are reported via a
#'   \code{"unmatched_atoms"} attribute.
#' @export
#'
#' @examples
#' \dontrun{
#' gceos_decompose("CCCCCCCCCCCCCCCC(=O)O")              # palmitic acid, GC-EOS
#' gceos_decompose("CCCCCCCCCCCCCCCC(=O)O", "gcaeos")    # ... GCA-EOS acid group
#' gceos_decompose("Cc1ccccc1")                          # toluene
#'
#' # chaining with the package's molecule workflow:
#' sol <- mol_find(c("CC1=CCC(CC1)C(=C)C", "5989-54-8", "Limonene"))
#' gceos_decompose(sol)
#' }
#'
#' @seealso \code{\link{gceos_params}}, \code{\link{mol_find}}
#'
#' @importFrom stats setNames
gceos_decompose <- function(solute, model = c("gceos", "gcaeos")) {
  model <- match.arg(model)
  if (!requireNamespace("rcdk", quietly = TRUE))
    stop("Package 'rcdk' is required for molecular decomposition.", call. = FALSE)

  sd <- gceos_params(model)$smarts
  if (is.null(sd))
    stop("No SMARTS table available for model '", model, "'.", call. = FALSE)

  # Accept either a mol_find() solute object (reuse its Molfile, so the same
  # molecule representation flows through the EOS and property workflows) or a
  # raw SMILES string (lightweight standalone use).
  if (is.list(solute) && "Molfile" %in% names(solute)) {
    mol <- solute[["Molfile"]]
  } else if (is.character(solute) && length(solute) == 1L && !is.na(solute)) {
    mol <- rcdk::parse.smiles(solute)[[1]]
  } else {
    stop("'solute' must be a single SMILES string or a mol_find() output list.", call. = FALSE)
  }
  if (is.null(mol))
    stop("Could not obtain a molecule from 'solute'.", call. = FALSE)

  atoms   <- rcdk::get.atoms(mol)
  natoms  <- length(atoms)
  # Hydrogens are never claimed by the heavy-atom GC-EOS SMARTS. They are IMPLICIT for a
  # SMILES-parsed mol, but EXPLICIT when the molecule arrives via a mol_find() Molfile or a
  # depiction-prepared mol - in which case counting them would wrongly report every H as an
  # "unmatched atom" (a GC-EOS group carries its own hydrogens). Track heavy atoms only.
  is_heavy <- vapply(atoms, function(a) !identical(rcdk::get.symbol(a), "H"), logical(1))
  claimed <- logical(natoms)
  counts  <- stats::setNames(integer(nrow(sd)), sd$group)

  for (k in order(sd$order)) {
    res <- tryCatch(rcdk::matches(sd$smarts[k], mol, return.matches = TRUE)[[1]],
                    error = function(e) list(match = FALSE, mapping = list()))
    if (!isTRUE(res$match)) next
    for (mp in res$mapping) {
      idx <- mp + 1L                       # rcdk atom mappings are 0-based
      if (any(claimed[idx])) next          # respect atoms already claimed
      claimed[idx] <- TRUE
      counts[sd$group[k]] <- counts[sd$group[k]] + 1L
    }
  }

  out <- counts[counts > 0L]
  attr(out, "unmatched_atoms") <- sum(is_heavy & !claimed)

  # GCA-EOS only: a BRANCHED ALIPHATIC alkane/alcohol must follow the Soria (2011)
  # combined-group convention (methine + one CH3 -> CHCH3; quaternary C dropped, its
  # neighbours -> (B)CH3/(B)CH2). The gcaeos SMARTS cannot express this, so rebuild
  # from the (branch-aware) gceos decomposition. Gated so it touches NOTHING else:
  # fires only when the molecule is branched AND fully aliphatic-convertible.
  if (model == "gcaeos") {
    conv <- .gcaeos_soria_convert(solute)
    if (!is.null(conv)) return(conv)
  }

  # A decomposition is directly usable only if every emitted group has EOS
  # parameters AND no atoms are left unmatched. Complex fused-ring molecules
  # (e.g. tocopherol's chromanol: a quaternary C and an aromatic ether the GC-EOS
  # table has no exact group for) trip one of these. Warn clearly and point to a
  # manual group vector, instead of failing cryptically downstream.
  partbl <- tryCatch(rownames(gceos_params(model)$groups), error = function(e) names(out))
  orphan <- setdiff(names(out), partbl)
  if (length(orphan) || attr(out, "unmatched_atoms") > 0L)
    warning("gceos_decompose(): incomplete decomposition",
            if (length(orphan)) paste0(" - matched group(s) lack EOS parameters: ",
                                       paste(orphan, collapse = ", ")) else "",
            if (attr(out, "unmatched_atoms") > 0L) paste0(" - ", attr(out, "unmatched_atoms"),
                                                          " unmatched atom(s)") else "",
            ". For fused-ring / complex molecules supply a manual group vector ",
            "(e.g. a published GC-EOS decomposition such as Fornari 2007).",
            call. = FALSE)
  out
}

# --- GCA-EOS branched-aliphatic (Soria 2011) group conversion ---------------
# Rebuild a branched aliphatic alkane/alcohol into the GCA-EOS branched-group
# convention of Soria et al. (Fluid Phase Equilib. 302 (2011) 1): each tertiary
# methine merges with one CH3 into a combined CHCH3 group (q = q_CH + q_CH3); a
# quaternary C contributes no group, only tagging its CH3/CH2 neighbours as
# (B)CH3/(B)CH2. The branch-aware GC-EOS decomposition supplies the markers
# (CH, CH3(B), CH2(B), C) that the gcaeos SMARTS cannot. Returns NULL (no
# conversion) unless the molecule is branched AND fully aliphatic-convertible,
# leaving aromatic / acid / ester / cyclic / linear species untouched.
.gcaeos_soria_convert <- function(solute) {
  dG <- tryCatch(suppressWarnings(gceos_decompose(solute, "gceos")), error = function(e) NULL)
  if (is.null(dG) || isTRUE(attr(dG, "unmatched_atoms") > 0L)) return(NULL)
  safe <- c("CH3","CH2","CH","CH3(B)","CH2(B)","C","CH2OH","CHOH","CH=CH")
  if (!any(c("CH","CH3(B)","CH2(B)","C") %in% names(dG))) return(NULL)  # not branched -> leave normal gcaeos
  if (!all(names(dG) %in% safe)) return(NULL)                          # aromatic/acid/cyclic -> out of scope
  v <- stats::setNames(as.integer(dG), names(dG)); g <- function(n) if (n %in% names(v)) v[[n]] else 0L
  nCH <- g("CH")
  if (g("CH3") < nCH) return(NULL)                                     # cannot pair every methine with a CH3
  o <- integer(0); ad <- function(n, k) if (k > 0L) o[n] <<- (if (n %in% names(o)) o[n] else 0L) + k
  ad("CH3", g("CH3") - nCH); ad("CH2", g("CH2")); ad("CHCH3", nCH)
  ad("(B)CH3", g("CH3(B)")); ad("(B)CH2", g("CH2(B)"))
  ad("CH2OH", g("CH2OH")); ad("CHOH", g("CHOH")); ad("CH=CH", g("CH=CH"))
  attr(o, "unmatched_atoms") <- 0L
  attr(o, "convention") <- "Soria2011"
  if (nCH > 0L) {                                                      # advisory: non-adjacent CHCH3
    nbad <- tryCatch(.gcaeos_methine_no_methyl(solute), error = function(e) 0L)
    if (nbad > 0L) {
      attr(o, "approx") <- TRUE
      warning("gceos_decompose(): branched aliphatic taken with the Soria (2011) convention; ",
              nbad, " methine(s) have no adjacent CH3, so CHCH3 was formed non-adjacently ",
              "(approximate, consistent with Gonzalez Prieto's practice).", call. = FALSE)
    }
  }
  o
}

# count branch methines (sp3 CH, 3 carbon neighbours) that have NO adjacent methyl
.gcaeos_methine_no_methyl <- function(solute) {
  if (!requireNamespace("rcdk", quietly = TRUE)) return(0L)
  mol <- if (is.list(solute) && "Molfile" %in% names(solute)) solute[["Molfile"]]
         else rcdk::parse.smiles(solute)[[1]]
  if (is.null(mol)) return(0L)
  m <- tryCatch(rcdk::matches("[$([CX4H1]([#6])([#6])[#6]);!$([CX4H1][CX4H3])]",
                              mol, return.matches = TRUE)[[1]], error = function(e) NULL)
  if (is.null(m) || length(m$mapping) == 0L) 0L
  else length(unique(vapply(m$mapping, function(x) x[1], integer(1))))
}
