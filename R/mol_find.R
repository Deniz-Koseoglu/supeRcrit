#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Convert molecule from SMILES to MOL and include basic descriptors
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Get MOL from SMILES and derive molecular descriptors
#'
#' @description Converts a SMILES string and (optionally) other information from SMILES to MOL.
#' Part of the \code{\link{sfe_mod}} workflow.
#'
#' @param mol A \code{character} vector of \code{length} 1-3 containing the \strong{mandatory CAS number}
#' as well as \strong{optional SMILES string} and \strong{name} of a given molecule.
#' The vector may \strong{optionally} be named with \code{"Name"}, \code{"SMILES"},
#' and/or \code{"CAS"}. Otherwise the function will attempt automatic assignment based on values.
#'
#' @return A \code{list} of length 2 containing the following elements:
#' \enumerate{
#' \item \strong{IDs}: A named vector of descriptors, including SMILES, CAS number, Name, InChI, InChIKey,
#' the number of atoms (\code{"Atom_Count"}), number of hydrogens (\code{"H_Count"}),
#' molecular weight (\code{"MW"}), molecular formula (\code{"MF"}), and a \code{logical}
#' specifying whether the molecule is aromatic (\code{"Aromaticity"}).
#' \item \strong{Molfile}: A representation of the molecule in MOL format.
#' }
#' @export
#'
#' @examples
#' #Limonene input vector
#' mol <- c("CC1=CCC(CC1)C(=C)C", "5989-54-8", "Limonene")
#' #Get descriptors
#' res <- mol_find(mol)
#'
#' @importFrom webchem get_cid get_chebiid pc_prop
#' @import rcdk
#'
mol_find <- function(mol) {

  #Preliminary checks
  if(!all(sapply(mol, is.character))) stop("Provided solute identifiers have to all be character strings!")

  #Vector to store solute identifiers
  solute_ids <- c()

  #Checking/generating solute identifiers
  for(i in mol) {
    if(grepl("^[0-9-]+$", i)) {
      if(!"CAS" %in% names(solute_ids)) solute_ids["CAS"] <- i else stop("Check provided solute identifiers for repeated information!")
    } else if(!suppressWarnings(is.null(rcdk::parse.smiles(i)[[1]]))) {
      if(!"SMILES" %in% names(solute_ids)) solute_ids["SMILES"] <- i else stop("Check provided solute identifiers for repeated information!")
    } else if(grepl("[A-Z]+|[a-z]+", i)) {
      if(!"Name" %in% names(solute_ids)) solute_ids["Name"] <- i else stop("Check provided solute identifiers for repeated information!")
    }
  }
  #CAS Check
  if(!any(names(solute_ids) %in% "CAS")) stop("CAS number is required and was not provided!")

  #Obtaining identifying information from CI (Chemical Identifier DB)
  qu_name <- "CAS"
  qu_resolver <- "xref/rn" #"cas_number"
  qu_props <- c("CanonicalSMILES", "InChI", "InChIKey") #c("smiles", "stdinchi", "stdinchikey")

  #Get PubChem CID
  cid <- as.character(webchem::get_cid(solute_ids[qu_name], from = qu_resolver, match = "first")[,2])

  #Query for SMILES, InChI, InChiKey
  solute_ids[c("SMILES", "InChI", "InChIKey")] <- unlist(webchem::pc_prop(cid, properties = qu_props))[2:4]

  #Checking Name, attempting to find one through ChEBI if missing...
  if(!"Name" %in% names(solute_ids)) {
    cat("\nSolute name not provided, attempting to retrieve name from ChEBI using ChEBI ID...")

    csname <- webchem::get_chebiid(solute_ids[qu_name])
    namecond <- any(colnames(csname) %in% "chebiasciiname")
    namecond <- if(namecond) !is.na(csname[1,"chebiasciiname"]) else FALSE
    if(namecond) {
      solute_ids["Name"] <- gsub("[[:punct:]]", "", csname[1,"chebiasciiname"]) #gsub() to remove illegal characters
      cat("\nSolute name successfully retrieved!")
    } else {
      solute_ids["Name"] <- "- (not found)"
      cat("\nNo name for solute could be found through ChEBI...")
    }
  }

  #Parse SMILES and get molecular structure (package rcdk)
  solute_mol <- rcdk::parse.smiles(solute_ids["SMILES"])[[1]]
  solute_mol <- rcdk::generate.2d.coordinates(solute_mol) #Generates reasonable 2D coordinates for solute

  #Clean up the generated IAtomContainer and get basic descriptors
  rcdk::convert.implicit.to.explicit(solute_mol) #Generates implicit hydrogens and converts them to explicit
  solute_ids["Atom_Count"] <- as.numeric(rcdk::get.atom.count(solute_mol)) #Used later to calculate Pc via JR method
  solute_ids["H_Count"] <- as.numeric(rcdk::get.total.hydrogen.count(solute_mol))
  solute_ids["MW"] <- as.numeric(rcdk::get.mol2formula(solute_mol)@mass)
  solute_ids["MF"] <- rcdk::get.mol2formula(solute_mol)@string
  solute_ids["Aromaticity"] <- rcdk::do.aromaticity(solute_mol)

  return(list("IDs"=solute_ids, "Molfile"=solute_mol))
}
