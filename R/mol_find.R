#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Convert molecule from SMILES to MOL and include basic descriptors
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Get MOL from SMILES and derive molecular descriptors
#'
#' @description Converts a SMILES string and (optionally) other information from SMILES to MOL.
#' Part of the \code{\link{sfe_mod}} workflow.
#'
#' @param mol A \code{character} vector of \code{length} 1-3 containing the \strong{mandatory CAS number and name}
#' as well as \strong{optional SMILES string} of a given molecule.
#' The vector may \strong{optionally} be named with \code{"Name"}, \code{"SMILES"},
#' and/or \code{"CAS"}. Otherwise the function will attempt automatic assignment based on values.
#'
#' @return A \code{list} of length 3 containing the following elements:
#' \enumerate{
#' \item \strong{IDs}: A named vector of descriptors, including SMILES, CAS number, Name, InChI, InChIKey,
#' the number of atoms (\code{"Atom_Count"}), number of hydrogens (\code{"H_Count"}),
#' molecular weight (\code{"MW"}), molecular formula (\code{"MF"}), and a \code{logical}
#' specifying whether the molecule is aromatic (\code{"Aromaticity"}).
#' \item \strong{Molfile}: A representation of the molecule in MOL format.
#' \item \strong{Synonyms}: A character vector of alternative names found in PubChem (up to 5).
#' }
#' @export
#'
#' @examples
#' #Limonene input vector
#' mol <- c("CC1=CCC(CC1)C(=C)C", "5989-54-8", "Limonene")
#' #Get descriptors
#' res <- mol_find(mol)
#'
#' @importFrom webchem get_cid get_chebiid pc_prop pc_synonyms
#' @import rcdk
#'
mol_find <- function(mol) {

  #Preliminary checks
  if(!all(sapply(mol, is.character))) stop("Provided solute identifiers have to all be character strings!")

  #Vector to store solute identifiers
  solute_ids <- c()

  #Checking/generating solute identifiers.
  #Honour explicit names on the input vector ("CAS"/"SMILES"/"Name") when supplied:
  #the app always tags the roles, and re-deriving them from string patterns
  #misclassifies e.g. a CAS-valued display name (from the "auto name" toggle) as a
  #second CAS, which raised "repeated information". Unnamed elements still auto-detect.
  in_names <- names(mol)
  for(idx in seq_along(mol)) {
    i <- as.character(mol[[idx]])
    tag <- if(!is.null(in_names) && !is.na(in_names[idx]) && in_names[idx] %in% c("CAS", "SMILES", "Name")) {
      in_names[idx]
    } else if(grepl("^[0-9-]+$", i)) {
      "CAS"
    } else if(!suppressWarnings(is.null(rcdk::parse.smiles(i)[[1]]))) {
      "SMILES"
    } else if(grepl("[A-Z]+|[a-z]+", i)) {
      "Name"
    } else {
      NA_character_
    }
    if(is.na(tag)) next
    if(!tag %in% names(solute_ids)) solute_ids[tag] <- i else stop("Check provided solute identifiers for repeated information!")
  }
  # If ONLY a SMILES was provided, build the molecule entirely OFFLINE - the structure is
  # already known, so no PubChem/ChEBI resolution is needed (CAS/Name/InChI are left blank).
  smiles_only <- "SMILES" %in% names(solute_ids) && !any(c("CAS", "Name") %in% names(solute_ids))

  if(smiles_only) {
    solute_ids["CAS"]  <- "-"
    solute_ids["Name"] <- "- (SMILES input)"
    synonyms <- character(0)
  } else {

  #CAS Check - only the CAS is mandatory for the online lookup; the Name is optional and is
  #auto-resolved from ChEBI further below (see "Checking Name...") when the caller omits it.
  if(!"CAS" %in% names(solute_ids)) stop("A CAS number is required and was not provided!")

  #Obtaining identifying information from CI (Chemical Identifier DB)
  qu_name <- "CAS"
  qu_resolver <- "xref/rn" #"cas_number"
  qu_props <- c("CanonicalSMILES", "InChI", "InChIKey") #c("smiles", "stdinchi", "stdinchikey")

  #Get PubChem CID
  cid <- as.character(webchem::get_cid(solute_ids[qu_name], from = qu_resolver, match = "first")[,2])

  #Query for SMILES, InChI, InChiKey
  solute_ids[c("SMILES", "InChI", "InChIKey")] <- unlist(webchem::pc_prop(cid, properties = qu_props))[2:4]
  
  #Query for synonyms (alternative names) from PubChem - get first 5
  synonyms <- tryCatch({
    syn_result <- webchem::pc_synonyms(cid, from = "cid")
    if (!is.null(syn_result) && length(syn_result) > 0 && !is.null(syn_result[[1]])) {
      # Get up to 5 synonyms, excluding CAS numbers (which match pattern like "123-45-6")
      all_syns <- syn_result[[1]]
      # Filter out CAS-like patterns and empty strings
      filtered_syns <- all_syns[!grepl("^[0-9]+-[0-9]+-[0-9]+$", all_syns) & nchar(all_syns) > 0]
      utils::head(filtered_syns, 5)
    } else {
      character(0)
    }
  }, error = function(e) {
    character(0)
  })

  #Checking Name, resolving one when the caller did not supply it.
  if(!"Name" %in% names(solute_ids)) {
    #Prefer the PubChem synonyms already fetched above - the first non-CAS synonym is the common
    #name (e.g. "caffeine"). Only fall back to ChEBI if PubChem gave nothing, and wrap it so a name
    #lookup NEVER aborts the whole characterization: the ChEBI service is frequently unavailable and
    #signals an error with an empty message (which surfaced as a blank "Error during GCM..." toast).
    resolved_name <- if(length(synonyms) > 0 && !is.na(synonyms[1]) && nzchar(synonyms[1])) synonyms[1] else NA_character_
    if(is.na(resolved_name)) {
      csname <- tryCatch(webchem::get_chebiid(solute_ids[qu_name]), error = function(e) NULL, warning = function(w) NULL)
      if(!is.null(csname) && any(colnames(csname) %in% "chebiasciiname") && !is.na(csname[1, "chebiasciiname"]))
        resolved_name <- gsub("[[:punct:]]", "", csname[1, "chebiasciiname"])
    }
    solute_ids["Name"] <- if(!is.na(resolved_name) && nzchar(resolved_name)) .capitalize_auto_name(resolved_name) else "- (not found)"
  }
  }   # end of offline (SMILES-only) / online (CAS/Name) branch

  #Parse SMILES and get molecular structure (package rcdk)
  solute_mol <- rcdk::parse.smiles(solute_ids["SMILES"])[[1]]
  .normalize_nitro(solute_mol)   # canonicalise non-standard pentavalent nitro N(=O)=O -> [N+](=O)[O-]
  solute_mol <- rcdk::generate.2d.coordinates(solute_mol) #Generates reasonable 2D coordinates for solute

  #Clean up the generated IAtomContainer and get basic descriptors
  rcdk::convert.implicit.to.explicit(solute_mol) #Generates implicit hydrogens and converts them to explicit
  solute_ids["Atom_Count"] <- as.numeric(rcdk::get.atom.count(solute_mol)) #Used later to calculate Pc via JR method
  solute_ids["H_Count"] <- as.numeric(rcdk::get.total.hydrogen.count(solute_mol))
  solute_ids["MW"] <- as.numeric(rcdk::get.mol2formula(solute_mol)@mass)
  solute_ids["MF"] <- rcdk::get.mol2formula(solute_mol)@string
  solute_ids["Aromaticity"] <- rcdk::do.aromaticity(solute_mol)

  return(list("IDs"=solute_ids, "Molfile"=solute_mol, "Synonyms"=synonyms))
}

# Capitalise the first letter of an AUTO-RESOLVED compound name (e.g. PubChem's "caffeine" -> "Caffeine"),
# leaving chemical prefixes untouched: a name that starts with a number/locant, a parenthetical or Greek-
# symbol stereodescriptor ("(R)-...", "beta-..."), or a leading single-letter / spelled-out locant-stereo-
# Greek token before a hyphen (o-, m-, p-, n-, d-, l-, cis-, trans-, sec-, tert-, alpha-, beta-, gamma-, ...)
# is returned verbatim, as is a name already starting upper-case. Applied ONLY to auto-resolved names; a
# user-typed name is used exactly as entered.
.capitalize_auto_name <- function(nm) {
  if (length(nm) != 1 || is.na(nm) || !nzchar(nm)) return(nm)
  if (!grepl("^[a-z]", nm)) return(nm)                              # starts with number / symbol / already upper
  pref <- regmatches(nm, regexpr("^[a-z]+(?=-)", nm, perl = TRUE))  # leading lowercase token before a '-'
  keep <- c("cis", "trans", "sec", "tert", "syn", "anti", "exo", "endo", "ortho", "meta", "para",
            "alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta", "theta", "iota", "kappa",
            "lambda", "mu", "nu", "xi", "omicron", "pi", "rho", "sigma", "tau", "upsilon", "phi",
            "chi", "psi", "omega")
  if (length(pref) == 1L && (nchar(pref) == 1L || pref %in% keep)) return(nm)   # locant / stereo / Greek prefix
  paste0(toupper(substr(nm, 1, 1)), substring(nm, 2))
}

# Canonicalise the non-standard pentavalent-nitrogen nitro/nitrate representation N(=O)=O to the
# charge-separated [N+](=O)[O-] form. Many databases write nitro/nitrate/nitramine groups the first
# way, which leaves the group unmatched by GCM SMARTS (calibrated on the charge-separated form), e.g.
# JR drops nitrobenzene to 6/9 coverage. Triggers only on an N with >=2 double-bonded O (no-op on the
# already-charge-separated form, on nitrites O-N=O, or on N-oxides). Modifies the molecule in place.
.normalize_nitro <- function(mol) {
  SINGLE <- rJava::J("org.openscience.cdk.interfaces.IBond$Order")$SINGLE
  for (a in rcdk::get.atoms(mol)) {
    if (rcdk::get.symbol(a) != "N") next
    dO <- Filter(function(o) {
      if (rcdk::get.symbol(o) != "O") return(FALSE)
      b <- rJava::.jcall(mol, "Lorg/openscience/cdk/interfaces/IBond;", "getBond", a, o)
      !is.null(b) && grepl("DOUBLE", rJava::.jcall(rJava::.jcall(b,
        "Lorg/openscience/cdk/interfaces/IBond$Order;", "getOrder"), "S", "toString"))
    }, rcdk::get.connected.atoms(a, mol))
    if (length(dO) >= 2) {
      o <- dO[[1]]
      b <- rJava::.jcall(mol, "Lorg/openscience/cdk/interfaces/IBond;", "getBond", a, o)
      rJava::.jcall(b, "V", "setOrder", SINGLE)
      rJava::.jcall(a, "V", "setFormalCharge", rJava::.jnew("java/lang/Integer", 1L))
      rJava::.jcall(o, "V", "setFormalCharge", rJava::.jnew("java/lang/Integer", -1L))
    }
  }
  invisible(mol)
}
