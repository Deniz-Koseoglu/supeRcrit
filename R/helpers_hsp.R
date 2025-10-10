#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Show HSP data for solvents
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title HSP data for various solvents
#'
#' @description Displays the Hansen Solubility Parameters (HSP), molecular weight, molar volume, boiling point, critical temperature and pressure,
#' as well as the molar density and triple point temperature of various solvents used in the \code{\link{sfe_mod}} workflow.
#'
#' @return A \code{data.frame} of solvent data to be used for HSP-based calculations of relative solubility improvement. The columns include
#' solvent name (\code{"Solvent"}), abbreviation for function arguments (\code{"Abbreviation"}), the \code{"CAS"} number, molecular weight (\code{"MW"}),
#' molar volume in mL/mol (\code{"MV"}), density in g/mL (\code{"RHO"}), Hansen Solubility Parameters (dispersion \code{"dD"}, dipole moment \code{"dP"}, and hydrogen bonding \code{"dH"}),
#' boiling point temperature (\code{"Tb"}), triple point temperature (\code{"Ttp"}), as well as the critical temperature (\code{"Tc"}), pressure (\code{"Pc"}, in MPa),
#' volume (\code{"Vc"}, in L/mol), density (\code{"RHOc"}, in mol/L), and acentric factor (\code{"Omega"}). All temperatures are in Kelvin.
#'
#' @export
#'
show_solv <- function() {
  solv_db
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Show GCM method selection chart
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title GCM method selection chart
#'
#' @return A \code{data.frame} showing various Group Contribution Methods (GCMs) usable with \code{\link{est_gcm}} and parameters
#' which they are able to estimate (marked with \code{"Y"}).
#' @export
#'
#' @seealso \code{\link{est_gcm}}
show_gcm <- function() {
  gcm_opts
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Match SMARTS substrings
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Identify GCM groups in a molecule
#'
#' @description Part of the \code{\link{sfe_mod}} workflow. Identifies GCM groups in a solute provided SMARTS substrings.
#'
#' @param solute Molecular descriptors and MOL file output from function \code{\link{mol_find}}.
#' @param method The GCM method to use as a character string. One of:
#' Joback & Reid (\code{"JR"}), Stein & Brown (\code{"SB"}), their corrected counterparts (\code{"JR_corr"} and \code{"SB_corr"}),
#' Nannoolal (2004; \code{"NL04"}), Nannoolal (2007; \code{"NL07"}) and its robust counterpart (\code{"NL07_robust"},
#' Stefanis & Panayiotou (2008 and 2012; \code{"SP08"} and \code{"SP12"}, respectively),
#' Hukkerikar (2012; \code{"HKR_STW"} and \code{"HKR_SIM"}),
#' Zhao (2003; \code{"ZHAO"}), Bondi (1964; \code{"BND"}), and Slonimskii (1970; \code{"SLON"}).
#' See \code{\link{est_gcm}} for details.
#' @param gorder A \code{numeric} value denoting the maximum order of groups to be considered. Defaults to \code{0}, which
#' considers all available group orders.
#' @param simplicity A \code{character} string indicating the level of fragmentation simplicity. One of:
#' \code{"simple"} (overlapping patterns are allowed), \code{"normal"} (overlaps not allowed with the first fragmentation accepted),
#' or \code{"complex"} (overlaps not allowed with every possible fragmentation pattern computed).
#' When set to \code{"auto"} (default), selects the most appropriate simplicity level based on \code{method}.
#' @param tlim A single \code{numeric} value setting the time limit (in seconds) of fragmentation iterations
#' when \code{simplicity} is set to \code{"complex"}. Exceeding the time limit without completing fragmentation
#' results in an error. Computing all possible fragmentation patterns for some molecules may take an impractically
#' long time.
#' @param silent Should console output be silenced? Defaults to \code{FALSE}).
#'
#' @return A \code{data.frame} of all GCM group contributions for the chosen \code{method} (\code{$contrib_data}),
#' corresponding SMARTS strings (\code{$smarts_data}), group occurrences in the input molecule (\code{$occurrences}),
#' and additional settings (\code{$method} and \code{$simplicity}). Also included is a \code{data.frame} of atom indices for
#' each group match (\code{$atoms}), and the function \code{call}.
#' @export
#'
#' @examples
#' mol <- mol_find(c("CC1=CCC(CC1)C(=C)C", "5989-54-8", "Limonene"))
#' res <- sub_smarts(mol, "JR")
#'
#' @seealso \code{\link{mol_find}}, \code{\link{sfe_mod}}, \code{\link{est_gcm}}
#'
#' @importFrom rcdk get.fingerprint matches
#' @importFrom dplyr bind_rows group_split group_by_all mutate_all mutate
#' @importFrom data.table rbindlist
#' @importFrom tidyr unnest
#' @importFrom tibble as_tibble
#' @importFrom purrr map pluck
#' @importFrom utils combn
#' @importFrom magrittr "%>%"
sub_smarts <- function(solute, method, gorder = 0, simplicity = "auto", tlim = 20, silent = FALSE) {

  #Preliminary checks
  if(length(solute)!=2 | !is.list(solute) | !all(names(solute) %in% c("IDs","Molfile"))) stop("The solute argument must be a list of length 2 output from function 'mol_find'!")
  defmethods <- c("JR", "JR_corr", "SB", "SB_corr", "NL04", "NL07", "NL07_robust", "SP08", "SP12",
                  "HKR_STW", "HKR_SIM", "ZHAO", "BND", "SLON")
  defsim <- c("auto", "simple", "normal", "complex")
  if(!any(defsim %in% simplicity)) stop(paste0("Fragmentation 'simplicity' must be one of: ", paste0("'", defsim, "'", collapse = ", "), "!"))
  if(!any(method %in% defmethods)) stop(paste0("Method ('method') not recognized. Possible values are:" , paste0("'", defmethods, "'", collapse = ", "), "!"))
  if(!is.numeric(gorder)) gorder <- 0 else if(gorder > 3 | gorder < 0) stop("The maximum group order `gorder` must be less than or equal to 3!")

  #Determine 'simplicity' automatically
  if(any(simplicity %in% "auto")) simplicity <- if(any(grepl("^HKR_", method))) "normal" else "simple"

  #Unpacking solute identifiers and getting basic descriptors
  solute_desc <- solute[["IDs"]]
  solute_mol <- solute[["Molfile"]]
  atom_count <- as.numeric(solute_desc["Atom_Count"])
  is_arom <- as.logical(solute_desc[["Aromaticity"]])
  atoms <- get.atoms(solute_mol)
  atom_inds <- sapply(atoms, get.atom.index, mol = solute_mol)
  atom_masses <- setNames(sapply(atoms, function(x) {res <- get.atomic.number(x); ifelse(res==1, res, res*2)}),
                          atom_inds)
  arom_sans <- which(sapply(atoms, is.aromatic))
  arom_H <- which(eval.atomic.desc(solute_mol, which.desc = c("org.openscience.cdk.qsar.descriptors.atomic.IsProtonInAromaticSystemDescriptor",
                                                              "org.openscience.cdk.qsar.descriptors.atomic.VdWRadiusDescriptor"))[,1]==1)-1
  arom_atoms <- c(arom_sans, arom_H)

  #Determine the filenames to be loaded
  loadtext <- c(rep("JR_1987",2), rep("SB_1994",2), rep("NL_2004_2007",3), "SP_2008", "SP_2012",
                "HKR_2012_STW", "HKR_2012_SIM", "ZHAO_2003", "BND_1964", "SLON_1970")
  loadtext <- loadtext[which(defmethods %in% method)]

  #Load data
  gcm_df <- gcm_cnt[[paste0(loadtext, "_cnt")]]
  gcm_df <- gcm_df[!gcm_df[,"SMARTS"]=="Excluded",]
  gcm_smartstr <- gcm_smarts[[paste0(gsub("_STW|_SIM", "", loadtext), "_SMARTS")]][1:2]
  all_grps <- gcm_smartstr[,grepl("^ID$", colnames(gcm_smartstr))]

  #Format data to include group order and remove unevaluated group order(s)
  multiorder_chk <- "Type" %in% colnames(gcm_df)
  if(gorder==0 & multiorder_chk) gorder <- max(gcm_df[,"Type"], na.rm =TRUE)
  if(!multiorder_chk) gcm_df[,"Type"] <- 1 else {
    gcm_smartstr <- gcm_smartstr[which(gcm_smartstr[,"ID"] %in% gcm_df[gcm_df[,"Type"] %in% 1:gorder,"ID"] |
                                         !gcm_smartstr[,"ID"] %in% gcm_df[,"ID"]),]
    gcm_df <- gcm_df[gcm_df[,"Type"] %in% 1:gorder,]
  }

  #Substructure matching from the SMARTS file
  atomlst <- subst <- list()
  occr <- setNames(rep(0, length(all_grps)), all_grps) #Vector of occurrences

  #Get all matches
  for(i in seq(nrow(gcm_smartstr))) {
    subname <- as.character(gcm_smartstr[i,"ID"])
    subst[[subname]] <- this_sub <- rcdk::matches(gcm_smartstr[i,"SMARTS"], solute_mol, return.matches = TRUE)[[1]]
    occr[subname] <- length(this_sub[["mapping"]])

    #subst[[subname]] <- rcdk::get.fingerprint(solute_mol, type='substructure',
    #                                          fp.mode='count', substructure.pattern = gcm_smartstr[i,"SMARTS"])
    #occr[subname] <- as.numeric(gsub("^[^:]*:", "", subst[[subname]]@features[[1]]))

  }

    #Evaluate each group order separately. INCOMPLETE overlaps are only allowed for higher-order groups!
    for(j in sort(unique(gcm_df[,"Type"]))) {
      if(!silent) cat("\nWorking on groups of order: ", j, "...")
      atomdf <- setNames(as.data.frame(matrix(ncol = 3, nrow = 0)), c("Order", "ID", "Atom_1"))    #List of matched atoms
      order_inds <- which(gcm_smartstr[,"ID"] %in% gcm_df[gcm_df[,"Type"]==j,"ID"])

      for(i in gcm_smartstr[order_inds,"ID"]) {
        #subst <- rcdk::get.fingerprint(solute_mol, type='substructure', fp.mode='count', substructure.pattern = smartpatt)
        #occr[as.character(i)] <- as.numeric(gsub("^[^:]*:", "", subst@features[[1]]))

        #Get ALL matching atom vectors and their occurrences
        this_sub <- subst[[as.character(i)]]
        if(this_sub[["match"]]) {
          submap <- this_sub[["mapping"]]
          cursub <- sapply(submap, "length<-", max(lengths(submap)))
          if(!all(lengths(submap)==1)) cursub <- t(cursub)
          newmatch <- cbind.data.frame(rep(j, length(submap)), rep(i, length(submap)), cursub)
          #names(newmatch) <- names(atomdf)
          atomdf <- rbindlist(list(atomdf, newmatch), use.names = FALSE, fill = TRUE)
        } else occr[as.character(i)] <- 0
      }
      atomdf <- setNames(as.data.frame(atomdf), c("Order", "ID", paste0("Atom_", 1:(ncol(atomdf)-2))))
      unique_groups <- sort(unique(atomdf[,"ID"]))

      if(j==1 & simplicity != "simple") {

        #Check if there is any duplication in atom assignments (there may not be)
        dupl_atoms <- unlist(atomdf[,!colnames(atomdf) %in% "ID"], use.names = FALSE)
        any_dupl <- any(duplicated(dupl_atoms, incomparables = NA))
        which_best <- table(atomdf[,"ID"]) #The DEFAULT which_best object

        if(any_dupl) {
          if(!silent) cat("\nDuplications found in atom assignments. Attempting to select the best fragmentation pattern...")
        if(simplicity == "complex") {
          #Checks for first-order groups (NO OVERLAP ALLOWED)
          unique_atoms <- as.vector(na.omit(unique(dupl_atoms)))
          unique_atom_count <- length(unique_atoms)

            #Compute combined matrix of all possible match combinations
            #Shameless hack to retain group ID's
            k_rng <- length(unique(atomdf[,"ID"])):min(c(nrow(atomdf), atom_count)) #max(apply(atomdf, 1, function(x) length(which(!is.na(x[-1])))))
            atom_copy <- atomdf
            atom_copy[is.na(atom_copy)] <- -1
            atom_copy[,-1] <- t(sapply(1:nrow(atom_copy), function(x) paste0(atom_copy[x,"ID"], "_", x, ";", atom_copy[x,-1])))

            good_match <- FALSE
            i <- 1
            prev_match <- prev_diff <- c()
            rows <- atom_copy[-c(1:2)] %>% group_by_all %>% group_split()

            while(!good_match) {

              cat(if(i>1) "\r" else "\n", "Working on match combination ", i, " out of ", length(k_rng), " (", k_rng[i], " atoms at a time)...", sep = "")
              active_k <- k_rng[i]

              #Set for loop time limit for complex fragmentation (in seconds)
              time_limit <- tlim

              setTimeLimit(cpu = time_limit, elapsed = time_limit, transient = TRUE)
              on.exit({
                setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
              })

              tryCatch({

                atom_combos <- t(combn(x = 1:nrow(atom_copy), m = active_k))
                colnames(atom_combos) <- paste0("Atom_", seq(1,active_k,1))
                atom_combos <- atom_combos %>%
                  as_tibble(.name_repair = "minimal") %>%
                  mutate_all(~ map(., ~ pluck(rows, .x))) %>%
                  unnest(cols = paste0("Atom_",seq(1,active_k,1)), names_sep = "_") %>%
                  mutate_all(list(ID = ~ gsub(";.*", "", .))) %>%
                  mutate_all(list(~ gsub(".*;","",.)))

              }, error = function(cond) {
                cat("\n")
                stop(simpleError("Time limit reached for complex fragmentation... Try setting argument 'simplicity' to 'normal' or 'simple'!"))
              })

              id_patt <- grep(".*_ID$", colnames(atom_combos))

              #Get atom number sums
              matched_atoms <- atom_combos[,-id_patt]
              matched_atoms <- apply(matched_atoms, 1:2, as.numeric)
              matched_atoms[matched_atoms==-1] <- NA

              #Correct matched group IDs to remove NAs
              matched_groups <- atom_combos[,id_patt]
              matched_groups[is.na(matched_atoms)] <- NA

              matched_atoms <- apply(matched_atoms, 1, function(x) if(!any(duplicated(x, incomparables = NA))) length(unique(na.omit(x))) else 0)

              #Calculate the difference between unique matched atoms between this and previous iteration
              max_match <- prev_match[i] <- max(matched_atoms)
              if(i>1) prev_diff <- c(prev_diff, max_match-prev_match[i-1])

              #Check if all atoms of the molecule were completely matched
              if(max_match<unique_atom_count & i<length(k_rng) & if(i<3) TRUE else !all(prev_diff[c(i-1,i-2)]==0)) { #If not, move on to the next iteration
                i <- i+1
                next
              } else {
                if(!silent) cat("\nCorrect match found. Proceeding...")

                #Retrieve sequence of groups from each row
                which_complete <- which(matched_atoms==max_match)
                group_rle <- apply(matched_groups[which_complete,], 1, function(x) rle(x[!is.na(x)]))
                group_rle <- lapply(group_rle, function(x) sort(unname(x[["values"]])))
                group_rle <- group_rle[!duplicated(group_rle)]
                group_counts <- lapply(group_rle, function(x) table(gsub("_.*", "", x)))

                mt_atoms <- lapply(group_rle, function(x) {
                  res <- atomdf[as.numeric(gsub(".*_","",x)),]
                  res <- lapply(split(res[,!colnames(res) %in% "ID"], x), function(y) res <- unlist(y, use.names = FALSE))
                  res <- setNames(unlist(res, use.names = FALSE), rep(names(res), lengths(res)))
                  res <- res[!is.na(res)]
                })

                #If more than one complete match is present, ascertain the best one
                if(length(group_counts)>1) {

                  grading <- setNames(as.data.frame(matrix(0, ncol = 4, nrow = length(group_counts))), c("match", "grp", "numgrp", "arom_override"))

                  #Check mass condition (heavier groups = better)
                  if(!silent) cat("\nApplying mass condition...")
                    mass_matches <- lapply(mt_atoms, function(x) {
                      atoms_in_x <- atom_masses[match(x, names(atom_masses))]
                      matchres <- tapply(atoms_in_x, names(x), sum, na.rm = TRUE)
                      maxmatch <- sum(sort(matchres, decreasing = TRUE)[1:2])
                      names(x) <- gsub("_.*", "", names(x))
                      grpres <- tapply(atoms_in_x, names(x), sum, na.rm = TRUE)
                      maxgrp <- max(grpres)
                      numgrp <- length(unique(names(x)))
                      return(c(maxmatch, maxgrp, numgrp))
                    })

                    #Global definitions to prevent devtools::check() from complaining
                    utils::globalVariables(c("grp", "numgrp"))

                    mass_matches <- setNames(Reduce(rbind.data.frame, mass_matches), c("match", "grp", "numgrp"))
                    grading[,1:3] <- apply(mass_matches %>% mutate(match = match==max(match, na.rm = TRUE), #Greatest mass matched by the top 2 matches
                                                                   grp = grp==max(grp, na.rm = TRUE), #Greatest mass matched by top 2 GROUPS
                                                                   numgrp = numgrp==min(numgrp,na.rm = TRUE)), 1:2, as.numeric) #Fewest number of total groups

                  #Check for aromaticity condition (aromatic substituents = aromatic groups) - TAKES PRECEDENCE OVER MASS CONDITION
                  if(grepl("^HKR.*", method) & is_arom) {
                    if(!silent) cat("\nApplying aromaticity condition (currently only for Hukkerikar methods...")

                    #Count the percentage of aromatic atoms matched by aromatic groups
                    arom_matches <- sapply(mt_atoms, function(x) {
                      res <- x[which(x %in% arom_atoms)]
                      grpnums <- as.numeric(gsub("_.*", "", names(res)))
                      mean(gcm_df[gcm_df[,"ID"] %in% unique(grpnums),"Aromatic"]=="Y", na.rm = TRUE)
                    })
                    grading[,4] <- as.numeric(arom_matches==max(arom_matches))

                  } else if(!is_arom & !silent) cat("\nAromaticity condition was not applied since there are no aromatic atoms in the molecule.")

                  #Finally, select groups based on total number of points
                  if(any(grading[,4]!=0)) {
                    which_arom <- which(grading[,4]==1)
                    rowsums_arom <- rowSums(grading[which_arom,])
                    best_ind <- as.numeric(rownames(grading[which_arom,])[which.max(rowsums_arom)])
                    which_best <- group_counts[[best_ind]]
                  } else which_best <- group_counts[[which.max(rowSums(grading))]]


                } else if(length(group_counts)==1) {
                  if(!silent) cat("\nFragmentation pattern checks were skipped since only one possible pattern was identified.")
                  which_best <- group_counts[[1]]
                }

                }
                good_match <- TRUE
              }

          } else if(simplicity == "normal") {

          #For standard simplicity of fragmentation
          #List of exceptions
          ovex <- c(830,831,856)

          #Retrieve mapping values and group numbers (as element names) from 'subst'
          subst_whole <- lapply(subst[sapply(subst, function(x) x[["match"]])], function(y) y[["mapping"]])
          subst_nms <- unlist(sapply(seq_along(subst_whole), function(x) rep(names(subst_whole)[x], length(subst_whole[[x]]))))
          subst_fl <- setNames(unlist(subst_whole, recursive = FALSE, use.names = FALSE), subst_nms)
          subst_fl <- subst_fl[names(subst_fl) %in% gcm_smartstr[order_inds,"ID"] & !names(subst_fl) %in% ovex]
          keep_inds <- c()

          for(i in seq_along(subst_fl)) {
            curgrp <- subst_fl[[i]]
            duplmatches <- sapply(seq_along(subst_fl), function(x) any(curgrp %in% subst_fl[[x]]))

            if(!any(duplmatches)) {
              keep_inds <- c(keep_inds, i)
              next
            } else {
              duplinds <- which(duplmatches)
              dupls <- subst_fl[duplmatches]
              aromvec <- massvec <- rep(FALSE, length(duplinds))
              initvec <- !aromvec

              #Aromaticity check
              if(grepl("^HKR.*", method) & is_arom) {
                arom_matches <- sapply(dupls, function(x) any(x %in% arom_atoms))
                arom_grps <- names(dupls) %in% gcm_df[gcm_df[,"Aromatic"] == "Y","ID"]
                arom_scores <- sapply(dupls, function(x) {
                  res <- length(which(x %in% arom_atoms))/length(x)
                  if(length(res)==0) res <- 0
                  return(res)
                })
                arom_keep <- arom_grps & arom_matches & (arom_scores == max(arom_scores, na.rm = TRUE))
                if(length(arom_keep)==1) aromvec <- !arom_keep else if(any(arom_keep)) aromvec <- arom_matches & !arom_grps
              }

              #Number of atoms and mass checks
              if(length(which(aromvec))!=1) {
                match_nums <- lengths(dupls)
                match_masses <- sapply(dupls, function(x) sum(atom_masses[names(atom_masses) %in% x], na.rm = TRUE))

                massvec <- match_nums != max(match_nums)
                if(length(which(massvec))!=1) massvec <- massvec & match_masses!=max(match_masses)
              }

              keepvec <- !aromvec & !massvec
              if(length(which(keepvec))==0) keepvec <- initvec
              keep_inds <- unique(c(keep_inds, duplinds[keepvec]))
            }
          }
          atomdf <- atomdf[keep_inds,]
          which_best <- table(atomdf[,"ID"])
          }
        }

      } else if(j>1 & simplicity != "simple") {

        #For higher-order groups (only COMPLETE OVERLAP is not allowed)
        #The below also removes NAs
        atomsplit <- lapply(split(atomdf[,grep("Atom_", colnames(atomdf))],
                                  sort(as.numeric(rownames(atomdf)))), function(x) sort(unlist(unname(x)))) #[,grep("Atom_", colnames(atomdf))]

        keep_inds <- c()

        for(i in seq_along(atomsplit)) {
          if(i %in% keep_inds) next
          curgrp <- atomsplit[[i]]
          duplmatches <- sapply(seq_along(atomsplit), function(x) any(curgrp %in% atomsplit[[x]]))

          if(!any(duplmatches)) {
            keep_inds <- c(keep_inds, i)
            next
          } else {
            duplinds <- which(duplmatches)
            dupls <- atomsplit[duplmatches]
            initvec <- rep(TRUE, length(duplinds))

            #Number of atoms and mass checks
            match_nums <- lengths(dupls)
            match_masses <- sapply(dupls, function(x) sum(atom_masses[names(atom_masses) %in% x], na.rm = TRUE))

            massvec <- match_nums != max(match_nums)
            if(length(which(massvec))!=1) massvec <- massvec & match_masses!=max(match_masses)

            keepvec <- !massvec
            if(length(which(keepvec))==0) keepvec <- initvec
            keep_inds <- unique(c(keep_inds, duplinds[keepvec]))
          }
        }
        atomdf <- atomdf[keep_inds,]
        which_best <- table(atomdf[,"ID"])
    }

      #Correct matched atom data.frame and occurrences to account for removed groups
      if(simplicity != "simple") {
        missing_groups <- unique_groups[which(!unique_groups %in% names(which_best))]
        occr[names(which_best)] <- which_best
        occr[names(occr) %in% missing_groups] <- 0
        atomlst[[j]] <- atomdf <- atomdf[!atomdf[,"ID"] %in% missing_groups,]
      }
    }
  #Row-bind the list of matched atoms
  atomres <- if(simplicity == "simple") atomdf else rbindlist(atomlst, use.names = TRUE, fill = TRUE)

  #Final check for missing values in 'occr' to replace these with zeroes
  occr[is.na(occr) | is.nan(occr)] <- 0

  #Compile output data
  output <- list(contrib_data = gcm_df, smarts_data = gcm_smartstr, atoms = atomres, occurrences = occr,
                 method = method, simplicity = simplicity, call = match.call())
  return(output)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Modify/Process SMARTS substrings matches to remove conflicts/double ids
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Modify/Process SMARTS substrings matches to remove conflicts
#'
#' @description Part of the \code{\link{sfe_mod}} workflow.
#' \strong{This function manual is hidden from the Index}.
#'
#' @param occr The output of function \code{\link{sub_smarts}}.
#'
#' @return A \code{list} of all GCM groups and their contributions (\code{$contrib_data}),
#' corresponding SMARTS strings (\code{$smarts_data}), a \strong{corrected} vector of \code{$occurrences},
#' group \code{$interactions} (only relevant to certain GCMs), and the chosen \code{$method}.
#'
#' @export
#' @keywords internal
#'
#' @seealso \code{\link{sub_smarts}}
define_grps <- function(occr) {

  #Preliminary checks
  if(length(occr)!=7 | !all(c("contrib_data", "smarts_data", "atoms", "occurrences", "method", "simplicity", "call") %in% names(occr))) stop("Argument 'occr' must be output from the function 'sub_smarts'!")

  #Unpack input data 'occr'
  gcm_df <- occr[["contrib_data"]]
  gcm_smartstr <- occr[["smarts_data"]]
  method <- occr[["method"]]
  simplicity <- occr[["simplicity"]]
  inters <- paste0("Group interactions are not a part of the chosen method ('", method,"') and were not calculated.")
  occ <- occr[["occurrences"]]

  #Derive corrected occurrences by testing for alternatives, subtracting or adding other SMARTS matches
  if(any(c("JR", "JR_corr") %in% method)) {

    #ALTERNATIVES
    occ["26"] <- max(occ[c("26", "261")], na.rm = TRUE)

  } else if(any(c("SB", "SB_corr") %in% method)) {

    #ALTERNATIVES
    occ["54"] <- max(as.numeric(occ[c("54", "107")]), na.rm=TRUE)
    occ["101"] <- max(as.numeric(occ[c("101", "108", "700")]), na.rm=TRUE)
    occ["106"] <- max(as.numeric(occ[c("106", "109")]), na.rm=TRUE)
    occ["700"] <- max(as.numeric(occ[c("700","7001")]), na.rm=TRUE)

    #ADDITIONS
    occ["3"] <- pmax(0, sum(as.numeric(occ[c("3", "704")]), na.rm = TRUE))
    occ["23"] <- pmax(0, sum(as.numeric(occ[c("23", "700")]), na.rm = TRUE))
    occ["24"] <- pmax(0, sum(as.numeric(occ[c("24", "305", "306")]), na.rm = TRUE))
    occ["26"] <- pmax(0, sum(as.numeric(occ[c("26","310", "702", "705", "706")]), na.rm = TRUE))
    occ["27"] <- pmax(0, sum(as.numeric(occ[c("27", "304", "308", "309", "703")]), na.rm = TRUE))
    occ["28"] <- pmax(0, sum(as.numeric(occ[c("28", "311")]), na.rm = TRUE))
    occ["29"] <- pmax(0, sum(as.numeric(occ[c("29", "303", "305", "308", "309", "310")], 2*occ["307"], 2*occ["705"]), na.rm = TRUE))
    occ["30"] <- pmax(0, sum(as.numeric(occ[c("30", "306", "311")]), na.rm = TRUE))
    occ["31"] <- pmax(0, sum(as.numeric(occ[c("31", "701")]), na.rm = TRUE))
    occ["103"] <- pmax(0, sum(as.numeric(occ[c("103", "706")], 2*occ["703"]), na.rm = TRUE))

    #SUBTRACTIONS
    #None for this method (SB)

  } else if(any(c("NL04", "NL07", "NL07_robust") %in% method)) {

    #ALTERNATIVES
    occ["4"] <- max(as.numeric(occ[c("4","400")]), na.rm=TRUE)
    occ["5"] <- max(as.numeric(occ[c("5","500")]), na.rm=TRUE)
    occ["6"] <- max(as.numeric(occ[c("6","600")]), na.rm=TRUE)
    occ["8"] <- max(as.numeric(occ[c("8","800")]), na.rm=TRUE)
    occ["9"] <- max(as.numeric(occ[c("9","900")]), na.rm=TRUE)
    occ["10"] <- max(as.numeric(occ[c("10","1000")]), na.rm=TRUE)
    occ["11"] <- max(as.numeric(occ[c("11","1100")]), na.rm=TRUE)
    occ["58"] <- max(as.numeric(occ[c("58","580")]), na.rm=TRUE)
    occ["59"] <- max(as.numeric(occ[c("59","590")]), na.rm=TRUE)
    occ["60"] <- max(as.numeric(occ[c("60","601")]), na.rm=TRUE)
    occ["62"] <- max(as.numeric(occ[c("62","620", "630")]), na.rm=TRUE)
    occ["68"] <- max(as.numeric(occ[c("68","681")]), na.rm=TRUE)
    occ["69"] <- max(as.numeric(occ[c("69", "691")]), na.rm=TRUE)
    occ["72"] <- max(as.numeric(occ[c("72", "721", "70021")]), na.rm=TRUE)
    occ["89"] <- max(as.numeric(occ[c("89", "8900")]), na.rm=TRUE)
    occ["801"] <- max(as.numeric(occ[c("801","803")]), na.rm=TRUE)
    occ["802"] <- max(as.numeric(occ[c("802","804")]), na.rm=TRUE)

    #ADDITIONS
    occ["1"] <- pmax(0, sum(as.numeric(c(occ[c("1", "7000")], 2*occ["7001"])), na.rm = TRUE))
    occ["2"] <- pmax(0, sum(as.numeric(occ[c("2", "7002")]), na.rm = TRUE))
    occ["7"] <- pmax(0, sum(as.numeric(occ[c("7", "703", "706","708","7007")]), na.rm = TRUE)) #"701", "702", "704", "705", and "707" excluded
    occ["12"] <- pmax(0, sum(as.numeric(occ[c("12", "1201", "1202")]), na.rm = TRUE))
    occ["13"] <- pmax(0, sum(as.numeric(occ[c("13", "1301", "1302", "1303",
                                              "1304", "1305", "1306", "1307")]), na.rm = TRUE))
    occ["14"] <- pmax(0, sum(as.numeric(occ[c("14", "1401", "1402")]), na.rm = TRUE))
    occ["25"] <- pmax(0, sum(as.numeric(occ[c("25", "2501", "2502","2503")]), na.rm = TRUE))
    occ["26"] <- pmax(0, 2*(sum(as.numeric(occ[c("26", "2601", "2602")]), na.rm = TRUE)))
    occ["27"] <- pmax(0, 3*(sum(as.numeric(occ[c("27", "2701", "2702")]), na.rm = TRUE)))
    occ["29"] <- pmax(0, sum(as.numeric(c(occ[c("29", "2902", "7005")], 2*occ["2901"])), na.rm = TRUE))
    occ["34"] <- pmax(0, sum(as.numeric(c(occ[c("34", "3401")], 2*occ["7007"])), na.rm = TRUE))
    occ["38"] <- pmax(0, sum(as.numeric(occ[c("38", "7002", "7009", "7010")]), na.rm = TRUE))
    occ["39"] <- pmax(0, sum(as.numeric(occ[c("39", "3901", "3902", "3903",
                                              "3904", "3905")]), na.rm = TRUE))
    occ["44"] <- pmax(0, sum(as.numeric(occ[c("44", "7003", "7009")]), na.rm = TRUE))
    occ["52"] <- pmax(0, sum(as.numeric(c(occ[c("52", "304", "7004")], 2*occ["7008"])), na.rm = TRUE))
    occ["58"] <- pmax(0, sum(as.numeric(occ[c("58", "5801", "5802")]), na.rm = TRUE))
    occ["59"] <- pmax(0, sum(as.numeric(occ[c("59", "5901", "5902", "5903")]), na.rm = TRUE))
    occ["60"] <- pmax(0, sum(as.numeric(occ[c("60", "6001", "6002", "6003",
                                              "6004", "6005", "6006", "6007",
                                              "6008", "6009")]), na.rm = TRUE))
    occ["61"] <- pmax(0, sum(as.numeric(occ[c("61", "6101", "6102")]), na.rm = TRUE))
    occ["62"] <- pmax(0, sum(as.numeric(occ[c("62", "6201", "6202", "6203",
                                              "6204", "6205", "7006")]), na.rm = TRUE))
    occ["64"] <- pmax(0, sum(as.numeric(occ[c("64", "6401")]), na.rm = TRUE))
    occ["72"] <- pmax(0, sum(as.numeric(occ[c("72", "7002")]), na.rm = TRUE))
    occ["76"] <- pmax(0, sum(as.numeric(occ[c("76", "7601", "7602")]), na.rm = TRUE))
    occ["77"] <- pmax(0, sum(as.numeric(occ[c("77", "7005", "7701")]), na.rm = TRUE))
    occ["88"] <- pmax(0, sum(as.numeric(occ[c("88", "7006", "8801", "8802",
                                              "8803", "8804", "8805", "8806",
                                              "8807", "8808", "8809")]), na.rm = TRUE))
    occ["89"] <- pmax(0, sum(as.numeric(occ[c("89", "8901", "8902", "8903",
                                              "8904", "8905", "8906", "8907",
                                              "8908", "8909", "8910", "8911",
                                              "8912", "8913", "8914", "8915",
                                              "8916", "8917", "8918", "8919")]), na.rm = TRUE))
    occ["103"] <- pmax(0, sum(as.numeric(occ[c("103", "304", "7008")]), na.rm = TRUE))
    occ["125"] <- pmax(0, sum(as.numeric(c(occ["125"], occ["1251"])), na.rm = TRUE))
    occ["131"] <- pmax(0, sum(as.numeric(occ[c("131", "1311", "1312", "1313")]), na.rm = TRUE))
    occ["132"] <- pmax(0, sum(as.numeric(occ[c("132", "1321")]), na.rm = TRUE))

    #SUBTRACTIONS
    occ["38"] <- pmax(0, occ["38"] - occ["65"])
    occ["62"] <- pmax(0, occ["62"] - occ["96"])
    occ["76"] <- pmax(0, occ["76"] - occ["96"])
    occ["118"] <- pmax(0, occ["118"] - occ["1181"])

    #SPECIAL CASES
    occ["35"] <- if(occ["36"]==occ["3601"] & occ["36"]==occ["3602"] &
                    occ["36"]==occ["3603"] & occ["36"]==occ["3604"] &
                    occ["10000"]<=4) 0 else occ["36"]
    occ["36"] <- if(occ["36"]==occ["3601"] & occ["36"]==occ["3602"] &
                    occ["36"]==occ["3603"] & occ["36"]==occ["3604"] &
                    occ["10000"]<=4) occ["36"] else 0
    occ["123"] <- if(occ["123"]==1) 1 else 0
    occ["124"] <- if(occ["124"]==1) 1 else 0
    #occ["126"] <- occ["126"]
    occ["127"] <- if(occ["127"]>0 & occ["128"]==0 & occ["129"]==0) 1 else 0
    occ["128"] <- if(occ["127"]==0 & any(c(1,3) %in% occ["128"]) & occ["129"]==0) 1 else 0
    occ["129"] <- if(occ["127"]==0 & occ["128"]==0 & occ["129"]==2) 1 else 0


    #Calculate group interaction contributions
    #Interacting groups
    inters <- c()
    inters["OH"] <- sum(as.numeric(occ[c("33","34","35","35","36","301")]), na.rm = TRUE) #Includes hydroperoxides
    inters["OH_a"] <- occ["37"]
    inters["COOH"] <- sum(as.numeric(occ[c("44","302")]), na.rm = TRUE)
    inters["EtherO"] <- occ["38"]
    inters["Epox"] <- occ["39"]
    inters["Ester"] <- sum(as.numeric(occ[c("45","46","47","303")]), na.rm = TRUE)
    inters["Ketone"] <- sum(as.numeric(occ[c("51","92")]), na.rm = TRUE)
    inters["Alde"] <- sum(as.numeric(occ[c("52","90")]), na.rm = TRUE)
    inters["AO"] <- occ["65"]
    inters["Teth"] <- occ["54"]
    inters["Ats"] <- occ["56"]
    inters["SH"] <- occ["53"]
    inters["NH2"] <- sum(as.numeric(occ[c("40","41")]), na.rm = TRUE)
    inters["NH"] <- sum(as.numeric(occ[c("42","97")]), na.rm = TRUE)
    inters["OCN"] <- occ["80"]
    inters["CN"] <- occ["57"]
    inters["Nitro"] <- occ["69"]
    inters["AN5"] <- occ["66"]
    inters["AN6"] <- occ["67"]

  } else if(any(c("SP08", "SP12") %in% method)) {

    sp12chk <- any(grepl("SP12", method))

    #ALTERNATIVES
    #None for these methods (SP08 and SP12)

    #SPECIAL CASES
    occ["31"] <- min(occ[c("31", "3001")], na.rm = TRUE)
    occ["32"] <- min(c(occ["32"], occ["3001"]-occ["31"]), na.rm = TRUE)
    if(sp12chk) occ["33"] <- min(c(occ["33"], occ["3001"]-occ["31"]-occ["32"]), na.rm = TRUE)
    occ["34"] <- min(occ[c("34", "3002")], na.rm = TRUE)
    occ["35"] <- min(c(occ["35"], occ["3002"]-occ["34"]), na.rm = TRUE)
    #occ["830"] <- occ["830"]/5
    #occ["831"] <- occ["831"]/6
    #occ["856"] <- occ["856"]/3

    #ADDITIONS
    if(sp12chk) occ["13"] <- pmax(0, sum(occ[c("13", "1301")], na.rm = TRUE))
    occ["14"] <- pmax(0, sum(occ[c("14", "301")], na.rm = TRUE))
    occ["19"] <- pmax(0, sum(occ[c("19", "5301","308","703")]
                             -sum(occ[c("1901", "1902", "1903")], na.rm=TRUE), na.rm=TRUE))
    occ["20"] <- pmax(0, sum(occ[c("20","309", "310", "702", "705")], na.rm = TRUE))
    occ["23"] <- pmax(0, sum(occ[c("23", "307", "310", "705")], na.rm = TRUE))
    occ["42"] <- pmax(0, sum(occ[c("42", "302", "43")], na.rm = TRUE))

    #SUBTRACTIONS
    if(simplicity=="simple") {
      occ["22"] <- pmax(0, sum(occ[c("22", "303", "306", "307", "308", "309", "705")]
                               -sum(occ[c("2201","2202")], na.rm = TRUE), na.rm = TRUE))
      occ["802"] <- pmax(0, sum(c(occ[c("802", "301", "302", "303", "304", "306")],
                                  2*occ[c("305", "700")])
                                -sum(occ[c("2501", "2502")], na.rm = TRUE), na.rm = TRUE))
      occ["814"] <- pmax(0, occ["814"]-sum(occ[c("71", "813")], na.rm = TRUE)-occ["810"]*3-occ["812"]*2)
      occ["816"] <- pmax(0, occ["816"]-sum(occ[c("42", "55", "56", "57", "2201", "2202", "23", "22", "803")], na.rm = TRUE)*2-
                           sum(occ[c("97","822","825", "19", "10", "1901", "1902","20", "14", "17", "2501", "2502","802", "804", "94")], na.rm = TRUE))
      occ["817"] <- pmax(0, occ["817"]-sum(occ[c("44", "45", "46", "53", "69")], na.rm = TRUE)-sum(occ[c("48", "49", "51")], na.rm = TRUE)*2)
      occ["820"] <- pmax(0, occ["820"]-sum(occ[c("31", "32", if(sp12chk) "33" else NULL)], na.rm = TRUE))
      occ["821"] <- pmax(0, occ["821"]-occ["811"])
      occ["823"] <- pmax(0, occ["823"]-occ["805"])
      occ["824"] <- pmax(0, occ["824"]-sum(occ[c("805","806", "807", "823", "826")], na.rm = TRUE))
      occ["827"] <- pmax(0, occ["827"]-sum(occ[c("55", "56", "57", "811", "818",
                                                 "819", "822", "29", "30", "31",
                                                 "32", if(sp12chk) "33" else NULL, "34", "35", "36",
                                                 "94", "97", "820", "821")], na.rm = TRUE))
      occ["1"] <- pmax(0, occ["1"]-sum(occ[c("11", "28", "31", "34", "40", "54",
                                             "1901", "2201", "2501", "806")], na.rm = TRUE)-occ["97"]*2)
      occ["2"] <- pmax(0, occ["2"]-sum(occ[c("12", "19", "22", "25", "29",
                                             "32", "35", "41", "44", "47",
                                             "55", "1902", "2202", "2502",
                                             "805", "807")], na.rm = TRUE))
      occ["3"] <- pmax(0, sum(occ[c("3", "704")], na.rm = TRUE)-sum(occ[c(if(sp12chk)"13" else NULL, "30", if(sp12chk) "33" else NULL,
                                                                          "45", "48", "50",
                                                                          "56", "801")], na.rm = TRUE))
      occ["4"] <- pmax(0, occ["4"]-sum(occ[c("1301", "46", "49", "51", "52")], na.rm = TRUE))
    }
  } else if("SLON" %in% method) {

    #ADDITIONS
    occ["18"] <- pmax(0, sum(occ[c("18", "1801", "1802", "1803", "1804", "1805", "1806", "1807", "1808")], na.rm = TRUE))
    occ["19"] <- pmax(0, sum(occ[c("19", "1901", "1902", "1903", "1904", "1905", "1906", "1907", "1908")], na.rm = TRUE))

  } else if("ZHAO" %in% method) {

    #ADDITIONS
    occ["16"] <- pmax(0, sum(occ[c("16", "1601", "1602", "1603", "1604", "1605", "1606", "1607", "1608")], na.rm = TRUE))
    occ["17"] <- pmax(0, sum(occ[c("17", "1701", "1702", "1703", "1704", "1705", "1706", "1707", "1708")], na.rm = TRUE))

  } else if("BND" %in% method) {

    #ADDITIONS
    occ["38"] <- pmax(0, sum(occ[c("38","3801")], na.rm=TRUE))
    occ["42"] <- pmax(0, sum(occ[c("42","4201")], na.rm=TRUE))
    occ["45"] <- pmax(0, sum(occ[c("45","4501")], na.rm=TRUE))
    occ["47"] <- pmax(0, sum(occ[c("47","4701")], na.rm=TRUE))
    occ["50"] <- pmax(0, sum(occ[c("50","5001", "5002")], na.rm=TRUE))

  }

  #Filter non-existent group occurrences
  occ <- occ[!is.na(occ) & !is.null(occ) & !is.nan(occ)]

  #Other methods like HKR_STW and HKR_SIM do not require any group definitions
  #Compile output data
  output <- list(contrib_data = gcm_df, smarts_data = gcm_smartstr, occurrences = occ, interactions = inters, method = method)
  return(output)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Compile group contributions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Compile GCM group contributions
#'
#' @description Part of the \code{\link{sfe_mod}} workflow.
#' \strong{This function manual is hidden from the Index}.
#'
#' @param solute Output from function \code{\link{mol_find}}.
#' @param occr Output from function \code{\link{define_grps}}.
#'
#' @return A \code{list} of the summarized group contributions for the input molecule (\code{$results}),
#' solute descriptors and MOL representation (\code{$solute}), SMARTS strings of all \strong{detected} groups
#' (\code{$smarts}), and the applied \code{$method}.
#'
#' @export
#' @keywords internal
#'
#' @seealso \code{\link{mol_find}}, \code{\link{sfe_mod}}
gcm_contribs <- function(solute, occr) {

  #Preliminary checks
  if(length(occr)!=5 | !all(c("contrib_data", "smarts_data", "occurrences","interactions","method") %in% names(occr))) stop("Argument 'occr' must be output from the function 'define_grps'!")
  if(length(solute)!=2 | !is.list(solute) | !all(names(solute) %in% c("IDs","Molfile"))) stop("The solute argument must be a list of length 2 output from function 'mol_find'!")
  method <- occr[["method"]]

  #Unpack solute atoms count information
  solute_ids <- solute[["IDs"]]
  atom_count <- as.numeric(solute_ids["Atom_Count"])
  h_count <- as.numeric(solute_ids["H_Count"])
  atoms_sans_H <- atom_count - h_count

  #Unpack the SMARTS sub-structure search results
  gcm_data <- occr[["contrib_data"]]
  gcm_smartstr <- occr[["smarts_data"]]
  inters <- occr[["interactions"]]
  occr <- occr[["occurrences"]]

  #Parsing SMILES and SMARTS substructure searching
  #Add occurrences to SMARTS DF
  gcm_smartstr[, "Occurrences"] <- as.numeric(occr)[match(gcm_smartstr[,"ID"], names(occr), nomatch = 0)]

  #Merge SMARTS occurrences with contribution data
  mergecols <- c("Group", "ID", "Type", grep("*_cont$", colnames(gcm_data), value = TRUE))

  gcm_data <- merge(gcm_data[,mergecols], gcm_smartstr[,c("ID", "SMARTS","Occurrences")], by = "ID", all.x = FALSE)
  gcm_data <- gcm_data[as.numeric(gcm_data[,"Occurrences"])>0,]

  #Get abbreviated names of parameters
  parnames <- gsub("_cont$", "", colnames(gcm_data)[grep("_cont$", colnames(gcm_data))])

  #Compile summed contributions
  for(i in parnames) gcm_data[,paste0("Sum_", i, "_cont")] <- gcm_data[,paste0(i, "_cont")]*gcm_data[,"Occurrences"]

  #Reordering columns in specific cases
  if(!any(c("JR","JR_corr") %in% method)) gcm_data <- gcm_data[,c(2,1,3:ncol(gcm_data))]

  #Calculate group interaction contributions if method is in the 'NL' group
  if(any(c("NL04","NL07","NL07_robust") %in% method)) {
    #Calculate group interaction contributions
    ID_vec <- c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S")
    names(ID_vec) <- names(inters)

    #Load Group Interaction Matrix and filter non-occurring groups
    suffix <- if(any(method %in% "NL07_robust")) "_robust" else ""
    nl_interact <- gcm_int[grep(paste0("\\Cint", suffix), names(gcm_int))]
    nl_interact <- nl_interact[c(grep("PCint", names(nl_interact)), grep("TCint", names(nl_interact)), grep("VCint", names(nl_interact)))]
    names(nl_interact) <- c("Pc_cont", "Tc_cont", "Vc_cont")

    nl_interact[[length(nl_interact)+1]] <- gcm_int[["NL_2004_BPint"]]
    names(nl_interact)[length(nl_interact)] <- "Tb_cont"

    #Change order of list elements
    nl_interact <- nl_interact[c(which(names(nl_interact)==colnames(gcm_data)[4]),
                                 which(names(nl_interact)==colnames(gcm_data)[5]),
                                 which(names(nl_interact)==colnames(gcm_data)[6]),
                                 which(names(nl_interact)==colnames(gcm_data)[7]))]

    example_interact <- nl_interact[[1]]
    nl_intres <- expand.grid(rownames(example_interact), colnames(example_interact))
    nl_intres <- nl_intres[!duplicated(apply(nl_intres,1,function(x) paste(sort(x),collapse="_"))),]
    colnames(nl_intres) <- c("Group1", "Group2")

    for(i in names(inters)) {
      nl_intres[nl_intres[,"Group1"]==i,"Group1_Occ"] <- inters[i]
      nl_intres[nl_intres[,"Group2"]==i,"Group2_Occ"] <- inters[i]
      nl_intres[nl_intres[,"Group1"]==i,"Group1_ID"] <- ID_vec[i]
      nl_intres[nl_intres[,"Group2"]==i,"Group2_ID"] <- ID_vec[i]
    }

    nl_intres <- nl_intres[!nl_intres[,"Group1_Occ"]==0 & !nl_intres[,"Group2_Occ"]==0 & !any(is.na(nl_intres[,c("Group1_Occ", "Group2_Occ")])),]

    if(nrow(nl_intres)!=0) {

      nl_intres[,"Occurrences"] <- sapply(1:nrow(nl_intres), function(x){ #Meaning occurrences of interactions
        if(nl_intres[x,"Group1"]==nl_intres[x,"Group2"] & nl_intres[x,"Group1_Occ"]>1) {
          res <- as.numeric(nl_intres[x,"Group1_Occ"])/2
        } else if(nl_intres[x,"Group1"]==nl_intres[x,"Group2"] & nl_intres[x,"Group1_Occ"]==1) {
          res <- 0
        } else if(nl_intres[x,"Group1"]!=nl_intres[x,"Group2"]) {
          res <- nrow(expand.grid(rep(nl_intres[x,"Group1"], as.numeric(nl_intres[x,"Group1_Occ"])),
                                  rep(nl_intres[x,"Group2"], as.numeric(nl_intres[x,"Group2_Occ"]))))
        }
        res <- res/atoms_sans_H
        return(res)
      })

      nl_intres <- nl_intres[!nl_intres[,"Occurrences"]==0,]

      if(nrow(nl_intres)>0) {
        nl_intres[,"Group"] <- paste0(nl_intres[,"Group1"], " to ", nl_intres[,"Group2"])
        nl_intres[,"Type"] <- 3
        nl_intres[,"ID"] <- paste0(nl_intres[,"Group1_ID"], "-", nl_intres[,"Group2_ID"])
        nl_intres[,"Plot_Colour"] <- "None"

        for(i in names(nl_interact)) {
          for(j in 1:nrow(nl_intres)) {
            nl_intres[j,i] <- nl_interact[[i]][rownames(nl_interact[[i]])==nl_intres[j,"Group1"],colnames(nl_interact[[i]])==nl_intres[j,"Group2"]]
          }
          nl_intres[,paste0("Sum_",i)] <- nl_intres[,i]*nl_intres[,"Occurrences"]
        }
      }
    }
  } else nl_intres <- as.data.frame(matrix(nrow = 0, ncol = 0))

  #Isolate SMARTS data
  group_smarts <- gcm_data[,"SMARTS"]
  names(group_smarts) <- gcm_data[,"Type"] #if(method %in% c("NL04","NL07","NL07_robust")) gcm_data[,"Type"] else rep(1, length(group_smarts))

  #Finally, compile all results together
  if(method %in% c("NL04","NL07", "NL07_robust") & nrow(nl_intres)>0) {
    res <- rbind.data.frame(gcm_data[,!colnames(gcm_data) %in% "SMARTS"],
                            nl_intres[,c("Group", "ID", "Type", names(nl_interact), "Occurrences", paste0("Sum_", names(nl_interact)))])
  } else res <- gcm_data[,!colnames(gcm_data) %in% c("SMARTS", "Tf_cont")]
  output <- list(results = res, solute = solute, smarts = group_smarts, method = method)
  return(output)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Visualize SMARTS substring matching on top of molecular structure
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Visualize molecules and sub-structures
#'
#' @description Parts of the \code{\link{sfe_mod}} workflow.
#' Visualizes a molecule from a MOL file and optionally highlights SMARTS substructures.
#'
#' @param solute MOL file and molecule descriptors output from function \code{\link{mol_find}}.
#' @param smarts A \code{character} vector of SMARTS strings specifying substructures to be highlighted.
#' Only relevant if \code{hlight} is \code{TRUE}.
#' @param hlight A \code{logical} indicating whether substructures are highlighted.
#' @param draw_plot A \code{logical} determining whether the plot is drawn.
#' @param plot_title A \code{logical} determining whether the plot should be annotated.
#'
#' @return The molecule visualization as an \code{array}.
#' @export
#'
#' @examples
#' smarts <- c("[CX4H3]", "[CX3H2]", "[$([!R;#6X3H0]);!$([!R;#6X3H0]=[#8])]", "[R;CX4H2]", "[R;CX4H]",
#' "[R;CX3H1,cX3H1]", "[$([R;#6X3H0]);!$([R;#6X3H0]=[#8])]")
#' mol <- mol_find(c("CC1=CCC(CC1)C(=C)C", "5989-54-8", "Limonene"))
#' res <- plot_gcm(mol, smarts, draw_plot = TRUE)
#'
#' @seealso \code{\link{mol_find}}, \code{\link{sfe_mod}}
#'
#' @importFrom rJava J .jnew
#' @import depict
#' @importFrom grid grid.raster
#'
plot_gcm <- function(solute, smarts, hlight = TRUE, draw_plot = TRUE, plot_title = TRUE) {

  #ALTERNATE PLOTTING OPTION
  #grid::grid.raster(rcdk::view.image.2d(solute$Molfile, depictor = rcdk::get.depictor(sma = gcm_smartstr[1,"SMARTS"]))) #or graphics::rasterImage()

  #Preliminary checks
  if(!is.vector(smarts) | !is.character(smarts)) stop("The 'smarts' argument must be a character vector of SMARTS sub-structures!")
  if(length(solute)!=2 | !is.list(solute) | !all(names(solute) %in% c("IDs","Molfile"))) stop("The solute argument must be a list of length 2 output from function 'mol_find'!")
  if(!is.logical(hlight)) stop("Argument 'hlight' must be logical!")

  #Unpack solute atoms count information
  solute_ids <- solute[["IDs"]]
  solute_mol <- solute[["Molfile"]]
  atom_count <- as.numeric(solute_ids["Atom_Count"])
  h_count <- as.numeric(solute_ids["H_Count"])
  atoms_sans_H <- atom_count - h_count

  #Filter out all but primary groups from 'smarts'
  if(is.character(names(smarts))) {
    if(any(names(smarts) %in% "1")) smarts <- smarts[names(smarts)=="1"]
  }

  #Preparing for visualisation
  #Java colours
  jcols <- rJava::J("java.awt.Color")
  jcol_vals <- list(jcols$LIGHT_GRAY, jcols$RED, jcols$GREEN,jcols$CYAN,
                    jcols$PINK, jcols$ORANGE, jcols$YELLOW, jcols$MAGENTA,
                    jcols$BLUE,
                    rJava::.jnew("java.awt.Color", 153L, 0L, 153L),
                    rJava::.jnew("java.awt.Color", 51L, 102L, 0L),
                    rJava::.jnew("java.awt.Color", 102L, 0L, 0L),
                    rJava::.jnew("java.awt.Color", 153L, 255L, 204L),
                    jcols$GRAY, jcols$DARK_GRAY)
  names(jcol_vals) <- c("LIGHT_GRAY", "RED", "GREEN", "CYAN", "PINK", "ORANGE", "YELLOW", "MAGENTA", "BLUE",
                        "PURPLE", "DARK_GREEN", "DARK_RED", "LIGHT_GREEN", "GRAY", "DARK_GRAY")

  #Depictor parsed SMILES
  draw_mol <- depict::parse_smiles(paste0(solute_ids["SMILES"], " ", solute_ids["Name"], "_", solute_ids["CAS"]))

  #JR
  #If an occurrence is detected, save SMARTS for visualisation later
  vis_vec <- c()
  if(hlight) {
    for(i in seq_along(smarts)) {
      vis_vec[smarts[i]] <- smarts[i]
    }
  }

  #Create and store visualisation in list
  dep_obj <- depict::depiction()
  plotcols <- c()

  if(hlight) {
    vis_match <- list()
    #Add colour labels for each matched group into the results table
    plotcols <- names(jcol_vals)[seq(length(vis_vec))]
    vis_match <- lapply(vis_vec, function(x) depict::match_smarts(x, draw_mol, limit = 100))
    for(i in seq_along(vis_match)) dep_obj <- depict::highlight_atoms(dep_obj, vis_match[[i]], jcol_vals[[i]])
  } else if(!hlight) plotcols <- rep("Not shown",length(smarts))

  dep_obj <- depict::set_size(dep_obj, 1200, 1200)
  dep_obj <- depict::set_zoom(dep_obj, 18)
  dep_obj <- depict::outerglow(dep_obj)
  if(plot_title) dep_obj <- depict::add_title(dep_obj)
  dep_obj <- depict::depict(dep_obj, draw_mol)

  output <- depict::get_image(dep_obj)
  if(draw_plot) grid::grid.raster(output)
  return(output)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Calculate GCM-based boiling point
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @noRd
gcm_bp <- function(contribs, method = NA) {

  #Preliminary checks
  if(length(contribs)!=4 | !all(c("results", "solute", "smarts", "method") %in% names(contribs))) stop("Argument 'contribs' must be output from the function 'gcm_contribs'!")
  #Retrieve solute info
  solute <- contribs[["solute"]]
  if(length(solute)!=2 | !is.list(solute) | !all(names(solute) %in% c("IDs","Molfile"))) stop("The solute argument must be a list of length 2 output from function 'mol_find'!")
  #Retrieve method
  if(any(is.na(method))) method <- contribs[["method"]]
  posmets <- c("JR", "JR_corr", "SB", "SB_corr", "NL04", "HKR_STW", "HKR_SIM")
  if(!any(method %in% posmets)) stop(paste0("Incorrect boiling point GCM method ('method') chosen! Possible values: ", paste0("'", posmets, "'", collapse = ", "), "!"))

  #Retrieve data frame with group contributions
  cdata <- contribs[["results"]]

  #Unpack solute atoms count information
  solute_ids <- solute[["IDs"]]
  atom_count <- as.numeric(solute_ids["Atom_Count"])
  h_count <- as.numeric(solute_ids["H_Count"])
  atoms_sans_H <- atom_count - h_count

  #Calculating Normal Boiling Point (Tb) via Group Contribution Methods (GCM)
  bpoint <- c()
  total_cont <- sum(cdata[,"Sum_Tb_cont"], na.rm = TRUE)

  if(method %in% c("JR", "JR_corr", "SB", "SB_corr")) {

    #Calculate boiling point and append to results list
    bpoint["Tb"] <- total_cont+198.2

    #Carry out JR/SB correction optionally
    if(any(method %in% c("JR_corr", "SB_corr"))) {
      bpoint["Tb_corr"] <- if(bpoint["Tb"]<=700) bpoint["Tb"]-94.84+0.5577*bpoint["Tb"]-0.0007705*bpoint["Tb"]^2 else bpoint["Tb_corr"] <- bpoint["Tb"]+282.7-0.5209*bpoint["Tb"]
    }
  } else if(method=="NL04") {

    #Calculate Normal Boiling Point
    bpoint["Tb"] <- total_cont/(atoms_sans_H^0.6583+1.6868)+84.3395

  } else if(any(method %in% c("HKR_STW", "HKR_SIM"))) {

    bpoint["Tb"] <- ifelse(any(grepl("_STW", method)), 244.5165, 244.7889)*log(total_cont)
  }
  return(bpoint)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Calculate GCM-based critical parameters
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @noRd
gcm_crit <- function(bp = NA, contribs, method = NA) {

  #Preliminary checks
  if(length(contribs)!=4 | !all(c("results", "solute", "smarts", "method") %in% names(contribs))) stop("Argument 'contribs' must be output from the function 'gcm_contribs'!")
  #Retrieve solute info
  solute <- contribs[["solute"]]
  if(length(solute)!=2 | !is.list(solute) | !all(names(solute) %in% c("IDs","Molfile"))) stop("The solute argument must be a list of length 2 output from function 'mol_find'!")
  #Retrieve method
  if(any(is.na(method))) method <- contribs[["method"]]
  posmets <- c("JR", "JR_corr", "NL07", "NL07_robust", "HKR_STW", "HKR_SIM")
  if(!any(method %in% posmets)) stop(paste0("Incorrect critical temperature GCM method ('method') chosen! Possible values: ", paste0("'", posmets, "'", collapse = ", "), "!"))
  if((!is.numeric(bp) & !any(grepl("^HKR_", method)))| length(bp)!=1) stop("Boiling point 'bp' must be a single numeric value or NA if 'method' is 'HKR_STW' or 'HKR_SIM' (Hukkerikar-type)!")

  #Retrieve data frame with group contributions
  cdata <- contribs[["results"]]

  #Unpack solute atoms count information
  solute_ids <- solute[["IDs"]]
  atom_count <- as.numeric(solute_ids["Atom_Count"])
  h_count <- as.numeric(solute_ids["H_Count"])
  atoms_sans_H <- atom_count - h_count
  mol_mass <- as.numeric(solute_ids["MW"])

  #Calculating critical parameters (Tc, Pc, Vc) via Group Contribution Methods (GCM)
  critres <- c()
  critres["Tb"] <- bp

  #Calculate summed contribution for first-order, second-order, and interacting groups
  total_cont <- c()
  contvec <- if(is.na(bp) & grepl("^HKR_",method)) c("Tb", "Tc", "Pc", "Vc") else c("Tc", "Pc", "Vc")
  for(i in contvec) total_cont[i] <- sum(cdata[,paste0("Sum_",i,"_cont")], na.rm = TRUE)

  if(any(method %in% c("JR", "JR_corr"))) {

    critres["Tc"] <- bp*(0.584+0.965*total_cont["Tc"]-(total_cont["Tc"])^2)^-1 #Units of K
    critres["Pc"] <- (0.113 + 0.0032*atom_count-total_cont["Pc"])^-2 #Units of bar
    critres["Vc"] <- 17.5+total_cont["Vc"] #Units of cm^3 mol^-1

  } else if(any(method %in% c("NL07", "NL07_robust"))) {

    #Calculate Critical Parameters
    critres["Tc"] <- bp*(0.6990+1/(0.9889+total_cont["Tc"]^0.8607))
    critres["Pc"] <- (mol_mass^-0.14041/(0.00939+total_cont["Pc"])^2)*0.01
    critres["Vc"] <- total_cont["Vc"]/(atoms_sans_H)^-0.2266+86.1539

  } else if(any(method %in% c("HKR_STW", "HKR_SIM"))) {
    univcon <- if(any(grepl("_STW", method))) c(244.5165, 181.6716, 0.0519, 0.1347, 28.0018) else c(244.7889, 181.6738, 0.0519, 0.1155, 14.6182)
    if(is.na(bp)) critres["Tb"] <- univcon[1]*log(total_cont["Tb"])
    critres["Tc"] <- univcon[2]*log(total_cont["Tc"])
    critres["Pc"] <- (univcon[4] + total_cont["Pc"])^-2 + univcon[3]
    critres["Vc"] <- univcon[5] + total_cont["Vc"]
  }
  return(critres)
}

#~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Calculate HSP
#~~~~~~~~~~~~~~~~~~~~~~~
#' @noRd
gcm_hsp <- function(contribs, method = NA) {

  #Preliminary checks
  if(length(contribs)!=4 | !all(c("results", "solute", "smarts", "method") %in% names(contribs))) stop("Argument 'contribs' must be output from the function 'gcm_contribs'!")
  #Retrieve method
  if(any(is.na(method))) method <- contribs[["method"]]
  posmets <- c("SP08", "SP12", "HKR_STW", "HKR_SIM")
  if(!any(method %in% posmets)) stop(paste0("Incorrect HSP GCM method ('method') chosen! Possible values: ", paste0("'", posmets, "'", collapse = ", "), "!"))

  #Retrieve data frame with group contributions
  cdata <- contribs[["results"]]

  #Set coefficients based on which SP method (2008 or 2012) is used
  cf <- if(any(grepl("SP08", method))) c(17.3231, 1, 7.3548, 7.9793, 2.7467, 1.3720)
  else if(any(grepl("SP12", method))) c(959.11, 0.4126, 7.6134, 7.7003, 2.6560, 1.3720)
  else if(any(grepl("HKR_", method))) c(0,1,0,0) #Effectively no coefficients

  #Calculate HSPs (NOTE: group contribution sums take into account both first- and second-order groups!)
  hspres <- c()
  hspres["dD"] <- (sum(cdata[,"Sum_dD_cont"], na.rm = TRUE)+cf[1])^cf[2] #Units of MPa^0.5
  hspres["dP"] <- sum(cdata[,"Sum_dP_cont"], na.rm = TRUE)+cf[3]
  hspres["dHB"] <- sum(cdata[,"Sum_dHB_cont"], na.rm = TRUE)+cf[4]

  #Perform checks for dP and dHB values <3 MPa^0.5
  if(any(grepl("^SP", method))) {
    for(i in c("dP","dHB")) {
      addval <- if(i=="dP") cf[5] else if(i=="dHB") cf[6]
      if(hspres[i]<3 & !any(is.na(cdata[,paste0(paste0(i,"_low_cont"))]))) {
        cdata[,paste0("Sum_",i,"_low_cont")] <- cdata[,paste0(i,"_low_cont")]*cdata[,"Occurrences"]
        hspres[paste0(i,"_low")] <- sum(cdata[,paste0("Sum_",i,"_low_cont")], na.rm = TRUE)+addval
      } else if(hspres[i]<3) {
        lownas <- which(is.na(cdata[,paste0(i,"_low_cont")]))
        lowsum <- vector("numeric",length = nrow(cdata))
        lowsum[-lownas] <- cdata[-lownas,paste0(i,"_low_cont")]*cdata[-lownas,"Occurrences"]
        lowsum[lownas] <- cdata[lownas,paste0(i,"_cont")]*cdata[lownas,"Occurrences"]
        cdata[,paste0("Sum_",i,"_low_cont")] <- lowsum
        hspres[paste0(i,"_low")] <- sum(cdata[,paste0("Sum_",i,"_low_cont")], na.rm = TRUE)+addval
      }
    }
  }

  #Making negative values 0
  hspres[which(hspres<0)] <- 0
  return(hspres)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Calculate van der Waals Volume
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @noRd
gcm_vdw <- function(contribs, method = NA) {

  #Preliminary checks
  if(length(contribs)!=4 | !all(c("results", "solute", "smarts", "method") %in% names(contribs))) stop("Argument 'contribs' must be output from the function 'gcm_contribs'!")
  #Retrieve method
  if(any(is.na(method))) method <- contribs[["method"]]
  posmets <- c("ZHAO", "SLON", "BND")
  if(!any(method %in% posmets)) stop(paste0("Incorrect HSP GCM method ('method') chosen! Possible values: ", paste0("'", posmets, "'", collapse = ", "), "!"))

  #Retrieve data frame with group contributions
  cdata <- contribs[["results"]]
  res <- c()

  #Retrieve auxiliary data
  mol <- contribs[["solute"]][[2]]
  numbonds <- length(get.bonds(mol)) #Number of bonds

  #Calculate summed group contributions
  VDW_res <- sum(cdata[,"Sum_VDW_cont"], na.rm = TRUE)
  if(!any(method %in% "BND")) VDW_res - 5.92*numbonds

  #Convert to SI units of m3/mole (Bondi gives results in cm3/mole, others in Angstroms/MOLECULE)
  res["VDW"] <- if(any(method %in% "BND")) VDW_res * 1e-6 else VDW_res * 0.602 * 1e-6
  return(res)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Estimate boiling points, critical parameters, and/or HSP
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Estimate various parameters via GCMs
#'
#' @description Part of the \code{\link{sfe_mod}} workflow. Estimates boiling point,
#' critical parameters (temperature, volume, pressure), van der Waals volume, and/or HSPs via Group Contribution Methods (GCM).
#' See \strong{Details} and \strong{References}.
#'
#' @param solute Input solute data as provided for \code{\link{mol_find}}.
#' @param tb,crit,hsp,vdw The chosen GCM(s) for parameter estimation (see \strong{Details}). Any of:
#' \enumerate{
#' \item \strong{For boiling point} (\code{tb}): \code{"JR"} (Joback-Reid 1987), \code{"JR_corr"} (corrected Joback-Reid),
#' \code{"SB"} (Stein-Brown), \code{"SB_corr"} (corrected Stein-Brown 1994), or \code{"NL04"} (Nannoolal 2004). Units are K.
#' \item \strong{For critical parameters} (\code{crit}): \code{"JR"} (Joback-Reid 1987), \code{"NL07"}, or \code{"NL07_robust"} (Nannoolal 2007).
#' The units are K, MPa, and mL/mol for critical temperature, critical pressure, and critical volume, respectively.
#' \item \strong{For HSP} (\code{hsp}): \code{"SP08"}, \code{"SP12"} (Stefanis & Panayiotou 2008-2012). The units for these
#' parameters are MPa^(1/2).
#' \item \strong{For all of the above}: \code{"HKR_STW"}, \code{"HKR_SIM"} (Hukkerikar 2012).
#' \item \strong{For van der Waals volume} (\code{vdw}): \code{"ZHAO"} (Zhao 2003), \code{"BND"} (Bondi 1964), or \code{"SLON"} (Slonimskii 1970).
#' The units are L/mole (i.e. SI units).
#' }
#' @param hlight A \code{logical} indicating whether substructures are highlighted (see \code{\link{plot_gcm}}).
#' @param simplicity A \code{character} string indicating whether overlapping sub-structures should be allowed (\code{"auto"} by default).
#' See \code{\link{sub_smarts}} for details on available degrees of simplicity.
#' @param gorder A \code{numeric} value or \strong{named} vector denoting the maximum order of groups to be considered. Defaults to \code{0}, which
#' considers all group orders. When length > 1, names corresponding to those provided in \code{tb}, \code{crit},
#' \code{hsp}, and/or \code{vdw} must be provided.
#' @param silent A \code{logical}. When \code{FALSE} (default), additional information is printed in the console.
#' See \code{\link{sfe_mod}} for more details on the available methods.
#'
#' @return A \code{list} of estimated property/parameter(s) as named \code{numeric} vectors:
#' \describe{
#' \item{\code{solute_data}}{Solute data as output from function \code{\link{mol_find}}.}
#' \item{\code{pares}}{A \code{list} containing the boiling point and critical parameters (element \code{$Critical}),
#' HSPs (element \code{$HSP}), GCMs used for their estimation (element \code{$Methods}), as well as a \code{list} of
#' \code{data.frame} objects containing group contributions for each calculated value (element \code{$Contribs}).}
#' \item{\code{visres}}{Raster visualisations of the solute molecule with GCM-identified substructures highlighted (as output from function \code{\link{plot_gcm}}).}
#' }
#'
#' @details
#' Group Contribution Methods (GCMs) predict thermodynamic and other properties from molecular structure using known
#' properties of functional groups. In addition to first-order groups (i.e. primary groups), second-order groups
#' (a.k.a. super-groups) may also be used and often encompass several first-order groups. Some newer methods utilize
#' group interactions to further enhance predictions.
#' Herein, several group contribution methods are utilized for prediction of boiling point, critical parameters
#' (temperature, pressure, and volume), and Hansen Solubility Parameters. The approach of Joback & Reid (1987)
#' remains the most popular method to estimate both boiling point \eqn{T_b} (K), critical temperature \eqn{T_c} (K),
#' critical pressure \eqn{P_c} (bar), and critical molar volume \eqn{V_c} (ml/mole) using the equations below,
#' where \eqn{\Sigma} is the sum-product of group contributions and the number of their occurrences in a given molecule:
#' \deqn{T_b = 198.2 + \Sigma}
#' \deqn{T_c = T_b\times(0.584 + 0.965\times\Sigma - \Sigma^2)^{-1}}
#' \deqn{P_c = (0.113 + 0.0032n_A - \Sigma)^{-2}}
#' \deqn{V_c = 17.5 + \Sigma}
#' The standard Joback-Reid approach tends to increasingly over-predict boiling points above 500 K. For this reason,
#' Stein and Brown (1994) introduced a correction either side of 700 K by fitting a polynomial to a larger set of
#' experimental data and minimizing the residuals:
#' \deqn{T_b\text{ }(corr.) = T_b - 94.84 + 0.5577\times T_b - 0.0007705\times T_b^2, \text{ where } \text{ } T_b \le 700 \text{ K}}
#' \deqn{T_b\text{ }(corr.) = T_b + 282.7 - 0.5209\times T_b, \text{ where } \text{ } T_b > 700 \text{ K}}
#' Both the Joback-Reid and Stein-Brown methods only utilize first-order groups, while second-order groups and possible
#' group interactions are not considered. The newer approach by Nannoolal et al. (2004, 2007) incorporates both of these aspects.
#' Same as above, \eqn{\Sigma} is the sum-product of group contributions, which \strong{include first-order and second-order}
#' groups as well as group interaction contributions. The \code{"NL07_robust"} variant of this method removes some group
#' \strong{interaction} contributions that were based on questionable or incomplete data according the authors. The letter
#' \eqn{n} denotes the number of atoms in the molecule \strong{\emph{sans}-hydrogen}, while \eqn{M} is the molar mass (g/mol).
#' \deqn{T_b = \Sigma/(n^{0.6583} + 1.6583) + 84.3395}
#' \deqn{T_c = T_b\times [0.6990 + 1/(0.9889 + \Sigma^{0.8607})]}
#' \deqn{P_c \text{ (bar)} = M^{-0.14041}/(0.00939 + \Sigma)^2\times 0.01}
#' \deqn{V_c \text{ (mL/mol)} = \Sigma/n^{-0.2266} + 86.1539}
#' In addition to boiling point and critical parameter estimation, HSP GCMs are also included as developed by
#' Stefanis & Panayiotou (2008, 2012). These methods incorporate both first-order and (\strong{optionally}) second-order
#' groups to estimate the dispersion (\eqn{\delta_d}), polarity (\eqn{\delta_p}), and hydrogen bonding (\eqn{\delta_{HB}}) HSPs.
#' The earlier version of the model developed in 2008 uses these equations:
#' \deqn{\delta_d\text{ }(MPa^{0.5}) = (\sum_i N_iC_i + W\sum_j M_jD_j + 17.3231)\text{ } MPa^{(1/2)}}
#' \deqn{\delta_p \text{ } (MPa^{0.5}) = (\sum_i N_iC_i + W\sum_j M_jD_j + 7.3548)\text{ } MPa^{(1/2)}, \text{ where } \delta_p > 3}
#' \deqn{\delta_p \text{ }(MPa^{0.5}) = (\sum_i N_iC_i + W\sum_j M_jD_j + 2.7467)\text{ } MPa^{(1/2)}, \text{ where } \delta_p < 3}
#' \deqn{\delta_{HB} \text{ }(MPa^{0.5}) = (\sum_i N_iC_i + W\sum_j M_jD_j + 7.9793)\text{ } MPa^{(1/2)}, \text{ where } \delta_{HB} > 3}
#' \deqn{\delta_{HB} \text{ }(MPa^{0.5}) = (\sum_i N_iC_i + W\sum_j M_jD_j + 1.3720)\text{ } MPa^{(1/2)}, \text{ where } \delta_{HB} < 3}
#' An update provided in 2012 updates the equations to the following:
#' \deqn{\delta_d \text{ }(MPa^{0.5}) = (\sum_i N_iC_i + W\sum_j M_jD_j + 959.11)^{0.4126} \text{ } MPa^{(1/2)}}
#' \deqn{\delta_p \text{ }(MPa^{0.5}) = (\sum_i N_iC_i + W\sum_j M_jD_j + 7.6134)\text{ } MPa^{(1/2)}, \text{ where } \delta_p > 3}
#' \deqn{\delta_p \text{ }(MPa^{0.5}) = (\sum_i N_iC_i + W\sum_j M_jD_j + 2.6560)\text{ } MPa^{(1/2)}, \text{ where } \delta_p < 3}
#' \deqn{\delta_{HB} \text{ }(MPa^{0.5}) = (\sum_i N_iC_i + W\sum_j M_jD_j + 7.7003)\text{ } MPa^{(1/2)}, \text{ where } \delta_{HB} > 3}
#' Another most versatile method for estimating boiling point, critical parameters, Hansen Solubility Parameters,
#' and many other values is the one developed by Hukkerikar et al. (2012) using the Marrero & Gani (2001)
#' set of first-, second-, and third-order groups. The associated equations are also simple sums of contributions
#' from first- and higher-order groups as listed below, but also incorporate a \strong{universal constant}
#' \eqn{UNIV} unique to each parameter. This is either added, multiplied, or otherwise incorporated into the group
#' contribution sum. Since this operation varies based on parameter, the universal constant was not shown in the equation
#' below and the reader is instead referred to the Hukkerikar et al. (2012) publication for more information:
#' \deqn{Parameter = \sum_{i} N_{i}C_{i} + \sum_{j} M_{j}D_{j} + \sum_{k} E_{k}O_{k}}
#' Finally, van der Waals volume \eqn{V_{vdW}\text{ }(\r{A}^3/molecule)} may be estimated using various methods by Zhao et al. (2003),
#' based on a simplified version of group contributions by Slonimskii et al. (1970) and Bondi (1964). While estimates using the
#' latter are simply calculated by summing group contributions and converting to \eqn{m^3/mol},
#' Zhao et al. (2003) is based on atom contributions and those form the number of bonds \eqn{N_B}, as well as
#' aromatic and non-aromatic rings (\eqn{R_A} and \eqn{R_{NA}}, respectively):
#' \deqn{V_{vdW} = \sum atom\text{ }contributions - 5.92N_B - 14.7R_A - 3.8R_{NA}}
#' Slonimskii et al. (1970) adapts a similar but slightly more complex method incorporating fused structures
#' of aromatic and non-aromatic rings (e.g. \eqn{R_{NA+A}}), and fused sulphur (\eqn{R_{FS}}) ring structures:
#' \deqn{V_{vdW} = \sum atom\text{ }contributions - 5.92N_B - 14.7R_A - 3.8R_{NA} + 5R_{A+A} + 3R_{NA+A} + 1R_{NA+NA} - 5R_{FS}}
#'
#' Note that in the above equations atomic symbols represent the number of their respective occurrences in a given molecule.
#'
#' @references
#' Bondi, A. (1964), 'Van der Walls Volumes and Radii', \emph{The Journal of Physical Chemistry} \strong{68} (3), pp. 441-451, DOI: \url{https://doi.org/10.1021/j100785a001}.
#'
#' Hukkerikar, A.S., Sarup, B., Kate, A.T., Abildskov, J., Sin, G., Gani, R. (2012), 'Group-contribution+ (GC+) based estimation of properties of pure components: Improved property estimation and uncertainty analysis', \emph{Fluid Phase Equilibria} \strong{321}, pp. 25-43, DOI: \url{https://dx.doi.org/10.1016/j.fluid.2012.02.010}.
#'
#' Joback, K.G., Reid, R.C. (1987), 'Estimation of Pure Components Properties from Group-contributions', \emph{Chemical Engineering Communications} \strong{57} (1-6), pp. 233-243, DOI: \url{https://www.doi.org/10.1080/00986448708960487}.
#'
#' Marrero, J., Gani, R. (2001), 'Group-contribution based estimation of pure component properties', \emph{Fluid Phase Equilibria} \strong{183-184}, pp. 183-208, DOI: \url{https://doi.org/10.1016/S0378-3812(01)00431-9}.
#'
#' Nannoolal, Y., Rarey, J., Ramjugernath, D., Cordes, W. (2004), 'Estimation of pure component properties: Part 1. Estimation of the normal boiling point of non-electrolyte organic compounds via group contributions and group interactions', \emph{Fluid Phase Equilibria} \strong{226}, pp. 45-63, DOI: \url{https://doi.org/10.1016/j.fluid.2004.09.001}.
#'
#' Nannoolal, Y., Rarey, J., Ramjugernath, D. (2007), 'Estimation of pure component properties: Part 2. Estimation of critical property data by group contribution', \emph{Fluid Phase Equilibria} \strong{252} (1), pp. 1-27, DOI: \url{https://doi.org/10.1016/j.fluid.2006.11.014}.
#'
#' Slonimskii, G.E., Askadskii, A.A., Kitaigorodskii, A.I. (1970), 'The Packing of Polymer Molecules', \emph{Polymer Science U.S.S.R.} \strong{12} (3), p. 556-577, DOI: \url{https://doi.org/10.1016/0032-3950(70)90345-X}.
#'
#' Stefanis, E., Panayiotou, C. (2008), 'Prediction of Hansen Solubility Parameters with a New Group-Contribution Method', \emph{International Journal of Thermophysics} \strong{29} (2), pp. 568-585, DOI: \url{https://www.doi.org/10.1007/s10765-008-0415-z}.
#'
#' Stefanis, E., Panayiotou, C. (2012), 'A new expanded solubility parameter approach', \emph{International Journal of Pharmaceutics} \strong{426} (1), pp. 29-43, DOI: \url{https://doi.org/10.1016/j.ijpharm.2012.01.001}.
#'
#' Stein, S.E., Brown, R.L. (1994), 'Estimation of normal boiling points from group contributions', \emph{Journal of Chemical Information and Computer Sciences} \strong{34} (3), DOI: \url{https://www.doi.org/10.1021/ci00019a016}.
#'
#' Zhao, Y.H., Abraham, M.H., Zissimos, A.M. (2003), 'Fast Calculation of van der Waals Volume as a Sum of Atomic and Bond Contributions and Its Application to Drug Compounds', \emph{The Journal of Organic Chemistry} \strong{68} (19), DOI: \url{https://doi.org/10.1021/jo034808o}.
#'
#' @export
#'
#' @examples
#' #Define solute data
#' mol <- c("CC1=C(C(CCC1)(C)C)C=CC(=CC=CC(=CC=CC=C(C)C=CC=C(C)C=CC2=C(CCCC2(C)C)C)C)C",
#' "7235-40-7", "Beta-carotene")
#'
#' #Estimate parameters by GCMs
#' estres <- est_gcm(mol, "JR", "JR", "SP08")
#'
#' @seealso \code{\link{mol_find}}, \code{\link{sfe_mod}}, \code{\link{plot_gcm}}, \code{\link{sub_smarts}}
#'
est_gcm <- function(solute, tb = NA, crit = NA, hsp = NA, vdw = NA, hlight = TRUE, simplicity = "auto", gorder = 0, silent = FALSE) {

  #Preliminary checks
  gcm_conds <- list(tb = tb, crit = crit, hsp = hsp, vdw = vdw)
  cond_nms <- unlist(gcm_conds[sapply(gcm_conds, function(x) !is.numeric(x) & !is.na(x))])

  if(all(is.na(gcm_conds))) stop("No GCMs were specified! At least one of 'tb', 'crit', 'hsp', and/or 'vdw' must be provided.") else metlst <- gcm_conds[!is.na(gcm_conds)]
  if(is.numeric(hsp) & length(hsp)!=3) stop("When numeric, 'hsp' must be of length 3!")

  if(length(gorder)>1 & is.null(names(gorder))) {
    stop("When group order 'gorder' is of length > 1, names corresponding to available GCM methods must be provided!")
  } else if(length(gorder)>1) {
    order_nameinds <- names(gorder) %in% cond_nms
    if(!all(order_nameinds)) stop(paste0("The following method names in 'gorder' were not recognized: ",
                                  paste0("'", names(gorder)[!order_nameinds], "'", collapse = ", "), "."))

  } else if(length(gorder)==1) gorder <- setNames(rep(gorder, length(cond_nms)), cond_nms)

  #Set default value for boiling point
  tb_forcrit <- NA

  #Create output lists
  visres <- pares <- conts <- list()
  estmets <- c()

  #Get solute data from CAS or SMILES
  if(!all(sapply(list(tb,crit,hsp,vdw),is.numeric))) {
    if(!silent) cat("\nRetrieving compound data...")
    solute_data <- mol_find(solute)
  }

  #Calculate parameters by GCM
  par_suffix <- c(tb = "boiling point", crit = "critical temperature and other parameters",
                  hsp = "Hansen Solubility Parameters (HSP)", vdw = "Van der Waals Volume")[!all(is.na(gcm_conds))]
  par_nms <- c(tb = "Tb", crit = "Critical", hsp = "HSP", vdw = "VDW")[!all(is.na(gcm_conds))]

  for(i in names(metlst)) {
    curnm <- par_nms[[i]]
    if(!silent) cat("\nCalculating/retrieving", par_suffix[i], "...")

    if(!is.numeric(metlst[[i]]) & !is.na(metlst[[i]])) {
      #Parse SMILES to get molecular descriptors and MOL file
      occr <- sub_smarts(solute_data, metlst[[i]], simplicity = simplicity, gorder = unique(gorder[names(gorder)==metlst[[i]]]), silent = TRUE)
      #Define SMARTS sub-groups
      occr <- define_grps(occr)
      #Get group contributions for GCM methods
      contribs <- gcm_contribs(solute_data, occr)
      conts[[curnm]] <- contribs[["results"]]
      #Create plot of identified groups
      visres[[curnm]] <- plot_gcm(solute_data, contribs[["smarts"]], hlight, draw_plot = FALSE, plot_title = TRUE)
      #Calculate parameter(s)
      res <- if(i=="tb") gcm_bp(contribs)
      else if(i=="crit") gcm_crit(tb_forcrit, contribs)
      else if(i=="hsp") gcm_hsp(contribs)
      else if(i=="vdw") gcm_vdw(contribs)

      if(i!="tb") pares[[curnm]] <- res
      #Subset tb for critical parameter calculation
      if(i=="tb") tb_forcrit <- res[length(res)] # & !is.numeric(crit)
      estmets[curnm] <- metlst[[i]]
    } else if(!is.na(metlst[[i]])) {
      if(i=="tb" & is.numeric(crit)) tb_forcrit <- tb
      if(i=="crit") pares[[curnm]] <- c("Tb" = tb_forcrit, "Tc" = crit)
      if(i!="crit") estmets[curnm] <- "experimental" else estmets["Tc"] <- "experimental"
      visres[[curnm]] <- conts[[curnm]] <- NA
    } else next
  }

  if(all(sapply(visres, function(x) all(is.na(x))))) visres <- NA else visres <- visres[!is.na(visres)]
  if(all(sapply(conts, function(x) all(is.na(x))))) conts <- NA else conts <- conts[!is.na(conts)]
  pares[["Methods"]] <- estmets
  pares[["Contribs"]] <- conts

  return(list(solute_data = solute_data, pares = pares, visres = visres))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Estimate critical temperature and pressure of solvent mixtures using various methods
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Determine the critical parameters of a solvent mixture and its current state
#'
#' @description Uses various empirical approaches to estimate the critical tempearatures and pressures
#' of binary and ternary mixtures given their mole or mass fractions. See \strong{Details} for further information.
#'
#' @param solv A \code{character} vector of two or three solvent names to calculate critical parameters for. For a list of solvents,
#' see \code{\link{show_solv}}.
#' @param fracs Fractions corresponding to solvents given in \code{solv}. Must sum to 1. Fraction units are specified in the
#' eponymous argument, \code{units}.
#' @param pres,temp \strong{Optional} pressure and temperature (in bar and degrees Celsius, respectively), to
#' at which to determine whether the physical state of the binary mixture is supercritical or not.
#' @param units The units of solvent fractions specified in \code{fracs}. One of \code{"mass"} (default) or \code{"mol"}.
#' @param tc,pc Methods to use for parameter estimation specified by \code{character} values. Currently implemented methods
#' include \code{"KAY"} (Kay, 1938), \code{"LI"} (Li, 1971),
#' \code{"FECP"} (First Extended Chueh-Prausnitz; Najafi et al., 2014, 2015) \strong{for critical temperature only},
#' \code{"HECP"} (He et al., 2017),
#' and variations of the Tang method \code{"TANG1"}, \code{"TANG2"}, \code{"TANG3"}, and \code{"TANG4"} (Tang et al., 2025).
#' @param getdf A \code{logical} specifying whether to compile the final predictions from all methods into a \code{data.frame}.
#' @param silent A \code{logical} that suppresses information printed to the console when \code{TRUE} (defaults to \code{FALSE}).
#'
#' @return
#' A list with the following elements:
#' \describe{
#' \item{\code{$fractions}: }{Mole and mass fractions for all solvents provided in \code{solv}.}
#' \item{\code{$results}: }{A \code{list} of estimated parameters for each of the methods specified in \code{tc} and \code{pc}.}
#' \item{\code{$results_df}: }{A \code{data.frame} compilation of \code{$results} described above (only when \code{getdf} is \code{TRUE}).}
#' \item{\code{$global_means}: }{Global averages and standard deviations of parameters obtained from all estimation methods.}
#' \item{\code{$statement}: }{\strong{Optional} descriptive text which specifies whether the examined solvent mixture
#' is supercritical at given reference pressure and temperature.}
#' }
#'
#' @details
#' The function implements empirical methods of Kay (1938), Li (1971), as well as various extensions of the Chueh-Prausnitz method
#' introduced by Najafi et al. (2014, 2015), He et al. (2017), and Tang et al. (2025). Comprehensive descriptions of methods
#' can be found in the source publications.
#'
#' @export
#'
#' @references
#' He, M., Liu, Y., Liu, X. (2017), 'Prediction of critical temperature and critical pressure of multicomponent mixtures', \emph{Fluid Phase Equilibria} \strong{441}, pp. 2-8, DOI: \url{http://dx.doi.org/10.1016/j.fluid.2016.11.017}.
#'
#' Kay, W.B. (1938), 'Liquid-Vapor Phase Equilibrium Relations in the Ethane-n-Heptane System', \emph{Industrial & Engineering Chemistry} \strong{30} (4), pp. 459-465, DOI: \url{https://doi.org/10.1021/ie50340a023}.
#'
#' Li, C.C. (1971), 'Critical temperature estimation for simple mixtures', \emph{The Canadian Journal of Chemical Engineering} \strong{49} (5), pp. 709-710, DOI: \url{https://doi.org/10.1002/cjce.5450490529}.
#'
#' Najafi, H., Maghbooli, B., Sobati, M.A. (2014), 'Prediction of true critical temperature of multi-component mixtures: An extension to Chueh and Prausnitz method', \emph{Fluid Phase Equilibria} \strong{363}, pp. 1-17, DOI: \url{http://dx.doi.org/10.1016/j.fluid.2013.10.054}.
#'
#' Najafi, H., Maghbooli, B., Sobati, M.A. (2015), 'Prediction of true critical temperature of multi-component mixtures: Extending fast estimation methods', \emph{Fluid Phase Equilibria} \strong{392}, pp. 104-126, DOI: \url{http://dx.doi.org/10.1016/j.fluid.2015.02.001}.
#'
#' Tang, B., Dong, X., Zhao, Y., Gong, M. (2025), 'Prediction for Critical Temperature and Critical Pressure of Mixtures by Improved Empirical Correlations', \emph{International Journal of Thermophysics} \strong{46}, article 83, DOI: \url{https://doi.org/10.1007/s10765-025-03560-2}.
#'
#' @examples
#' res <- iscrit_gen(solv = c("CO2", "Hexane", "Methanol"),
#' fracs = c(0.216, 0.196, 0.588),
#' pres = 400,
#' temp = 45,
#' units = "mol")
#'
#' @seealso \code{\link{iscrit_etoh}}, \code{\link{iscrit_demo}}
#'
#' @importFrom utils combn
iscrit_gen <- function(solv, fracs, pres = NA, temp = NA, units = "mass", tc = "all",
                   pc = "all", getdf = TRUE, silent = FALSE) {

  #Preliminary checks
  defsolv <- unlist(solv_db[,c("Solvent","Abbreviation")], use.names = FALSE)
  if(any(!solv %in% defsolv)) stop(paste0("The following solvent(s) was/were not recognized: ", paste0("'", solv[!solv %in% defsolv], "'", collapse = ", "), "! ",
                                          "Solvents must be one of: ", paste0("'", defsolv, "'", collapse = ", "), "."))
  if(length(c(solv,fracs))<=2) stop("There must be at least two solvents specified in 'solv'!")
  if(length(solv)>3) stop("Currently only binary or ternary mixtures are supported!")
  defmets_pc <- c("KAY", "LI", "HECP", "TANG1", "TANG2", "TANG3", "TANG4", "none")
  defmets_tc <- c("KAY", "LI", "HECP", "FECP", "TANG1", "TANG2", "TANG3", "TANG4", "none") #"SECP"
  if(any(tc %in% "none")) tc <- "none" else if(any(tc %in% "all")) tc <- defmets_tc[!defmets_tc %in% "none"]
  if(any(pc %in% "none")) pc <- "none" else if(any(pc %in% "all")) pc <- defmets_pc[!defmets_pc %in% "none"]
  if(!all(pc %in% defmets_pc)) stop(paste0("Critical pressure estimation method ('pc') not recognized. Possible values are:" , paste0("'", defmets_pc, "'", collapse = ", "), "!"))
  if(!all(tc %in% defmets_tc)) stop(paste0("Critical temperature estimation method ('tc') not recognized. Possible values are:" , paste0("'", defmets_tc, "'", collapse = ", "), "!"))
  if(any(!is.numeric(fracs)) | length(fracs) != length(solv)) stop("The number of solvents named in 'solv' must be of equal length to the fractions in 'fracs'!")
  if(sum(fracs)>1 | sum(fracs) < 1) {
    stop("Solvent fractions 'fracs' must sum to unity!")
  } #else if(sum(fracs)<1 & length(fracs)==length(solv)-1) fracs <- c(fracs, 1-sum(fracs))
  defunits <- c("mass", "mol")
  if(!any(defunits %in% units) | length(units)!=1) stop(paste0("Argument 'units' must be of length 1 and one of the following: ", paste0("'", defunits, "'", collapse = ", "))) else {

    if(!all(is.na(c(pres, temp))) & any(is.na(c(pres, temp)))) {
      stop("If reference pressure 'pres' is provided, temperature 'temp' must be given as well (and vice versa)!")
    } else {

      if(min(temp)<(-20) | max(temp)>200) stop("Minimum and maximum allowed temperature ('temp') is between -20 and 200 degrees celsius!")
      if(min(pres)<5 | max(pres)>1000) stop("Minimum and maximum allowed pressure ('pres') is between 5 and 1000 bar!")
      if(length(pres)==1 & length(temp)==1) {
        pres <- rep(pres, length(solv))
        temp <- rep(temp, length(solv))
      }
    }
  }
  if(any(!is.logical(c(getdf,silent)))) stop("Arguments 'getdf' and 'silent' must be logical!")

  #Begin processing
  #Retrieve RMMs, densities, critical temperatures, pressures, and volumes
  solv_inds <- pmin(match(solv, solv_db[,"Solvent"]), match(solv,solv_db[,"Abbreviation"]), na.rm = TRUE)
  solv <- solv_db[solv_inds, "Solvent"]
  nslv <- length(solv)
  prms <- setNames(lapply(c("MW", "Tb", "Tc", "Pc", "Vc", "Omega", "RHO"), function(x) {
    res <- if(x=="RHO") {
      sapply(seq_along(solv), function(y) {
        cursolv <- solv_dmass[[solv[y]]]
        cursolv[rownames(cursolv)==pres[y], colnames(cursolv)==temp[y]]
      })
    } else solv_db[solv_inds,x, drop = TRUE]
    return(setNames(res, solv))
  }), c("mw", "tb", "tc", "pc", "vc", "omega", "rho"))

  #Convert units
  prms[["vc"]] <- prms[["vc"]]*1000 #from L/mol to cm3/mol
  #prms[["pc"]] <- prms[["pc"]]*0.1 #from bar to MPa

  #Compile mole, volume, and mass fractions
  if(!silent) cat("\nCalculating mass and mole fractions for mixture ", paste0(solv, collapse = " : "), " (",
                  paste0(round(fracs, 2), collapse = " : "), "; ", units, ")...", sep = "")
  frc <- conv_fracs(solv, fracs, units)
  frc <- lapply(frc, function(x) setNames(x, solv))

  #Create list of output data
  outs <- list()

  #Get partial contributions of acentric factor, boiling point, critical temperature for TANG methods
  if(any(grepl("^TANG", c(tc,pc)))) {
    tang <- list()
    tngpar <- c(rep("omega",2), "tc", "tb")
    for(i in 1:4) {
      if(any(c(tc,pc) %in% paste0("TANG",i))) {
        addfac <- if(i %in% c(1,2)) {
          if(min(prms[["omega"]])>=0.0955 & i==2) 0 else 0.4
        } else 0
        tang[[i]] <- (frc[["mol"]]*(prms[[tngpar[i]]] + addfac))/sum(frc[["mol"]]*(prms[[tngpar[i]]] + addfac))
      }
    }
  }

  #Calculate the same for LI
  if(any(c(tc,pc) %in% "LI")) li <- (frc[["mol"]]*prms[["vc"]])/sum(frc[["mol"]]*prms[["vc"]])

  #Calculate critical temperature and pressure
  outnms <- c("Tc", "Pc")
  metlst <- setNames(list(tc,pc), c("tc","pc"))
  for(i in c("tc","pc")) {
    if(!silent) cat("\nCalculating critical ", ifelse(i=="tc", "temperatures", "pressures"), " using method(s): ", paste0(tc, collapse = ", "), "...", sep = "")
    for(j in metlst[[i]]) {
      if(j=="KAY") {
        outs[[j]][scap(i)] <- sum(frc[["mol"]]*prms[[i]])
      } else if(grepl("^TANG|LI", j)) {
        curtang <- if(grepl("LI", j)) li else tang[[as.numeric(substr(j, nchar(j), nchar(j)))]]
        if(i=="tc") {
          outs[[j]][scap(i)] <- sum(curtang*prms[[i]])
        } else {
          wm <- sum(frc[["mol"]]*prms[["omega"]])
          #zi <- curtang
          tcm <- sum(curtang*prms[["tc"]])
          tcm_star <- sum(frc[["mol"]]*prms[["tc"]])
          pcm_star <- sum(frc[["mol"]]*prms[[i]])
          outs[[j]][scap(i)] <- pcm_star*(1 + (5.808 + 4.93*wm)*(tcm/tcm_star - 1))
          #outs[[j]][paste0(i,"_alt")] <- pcm_star*(1 + (5.808 + 4.93*wm) * ((tcm-tcm_star)/tcm_star))
        }
      } else if(any(c("HECP", "FECP") %in% j)) {
        #Get any available interaction parameters
        solv_cmb <- combn(solv, 2, simplify = FALSE)
        condnm <- paste0(j, "_", scap(i))
        cur <- lapply(1:3, function(x) if(x==1) solv_conds[[condnm]] else if(x==2) solv_coefs[[condnm]] else solv_ints[[grep(paste0("^", condnm, "*"), names(solv_ints))]])
        names(cur) <- c("conds", "coefs", "ints")
        tijs <- sapply(solv_cmb, function(x) cur[["ints"]][rownames(cur[["ints"]]) %in% x[1], colnames(cur[["ints"]])==x[2]])

        #Estimate any missing interaction parameters ('tijs')
        if(any(is.na(tijs))) {
          miss_tijs <- which(is.na(tijs))
          if(!silent) cat("\n", length(miss_tijs), " interaction parameter(s) was/were not available for method ", j, " and will be estimated.", sep = "")
          miss_solv <- solv_cmb[miss_tijs]
          id_var <- ifelse(j=="HECP", "ID", "Priority")

          #Calculate conditions
          int_scores <- lapply(miss_solv, function(x) {
            conds <- cur[["conds"]][rownames(cur[["conds"]]) %in% x,]
            res <- sapply(seq(ncol(conds)), function(y) {

              #Check for -10 flags (if none of the flagged compounds present, nullify group contribution)
              if(any(cur[["conds"]][,y] == -10) & !any(conds == -10)) {
                nullify_cond <- TRUE
              } else {
                nullify_cond <- FALSE
                conds[which(conds[,y] == -10),y] <- -5
              }
              #Check for -5 flags (base contribution upon mole fraction)
              flag_5 <- which(conds[,y] == -5)
              if(length(flag_5)) {
                misind <- solv %in% rownames(conds)[flag_5]
                addval <- sum(frc[["mol"]][misind])
              } else addval <- 0

              #Calculate final scores
              conds[conds[,y] %in% c(-5,-10),y] <- 0
              fin_score <- if(!nullify_cond) sum(conds[,y]) + addval else 0
              if(fin_score < 0) fin_score <- 0 else if(fin_score > 1) fin_score <- 1
              return(fin_score)
            })
            return(res)
          })

          #Determine group ID using maximum score and priority (where available)
          #Retrieve coefficients for interaction parameter estimation
          if(j=="HECP") {
            coefs <- lapply(int_scores, function(x) {
              best_group <- which.max(x)
              unlist(cur[["coefs"]][best_group,LETTERS[1:5]])
            })

          } else if(j=="FECP") {
            coefs <- lapply(int_scores, function(x) {
              best_scores <- which(x == max(x))
              best_group <- cur[["coefs"]][which(cur[["coefs"]][,id_var] %in% best_scores),]
              unlist(best_group[which.min(best_group[,id_var]),LETTERS[1:5]])
            })
          }

          #Estimate interaction parameters
          tijs[miss_tijs] <- sapply(seq_along(miss_solv), function(x) {
            gamma_ij <- abs(diff(prms[["tc"]][miss_solv[[x]]])/sum(prms[["tc"]][miss_solv[[x]]]))
            res <- (sum(prms[["tc"]][miss_solv[[x]]])*(coefs[[x]]["A"] +
                                            coefs[[x]]["B"]*gamma_ij+
                                            coefs[[x]]["C"]*gamma_ij^2 +
                                            coefs[[x]]["D"]*gamma_ij^3 +
                                            coefs[[x]]["E"]*gamma_ij^4))/2
            return(res)
          })
        }

        #Finally, calculate critical parameters
        thetas <- setNames((frc[["mol"]]*(prms[["vc"]])^(2/3))/sum(frc[["mol"]]*(prms[["vc"]])^(2/3)), solv)
        outs[[j]][scap(i)] <- sum(thetas*prms[[i]])+2*sum(sapply(seq_along(solv_cmb), function(x) prod(c(thetas[solv_cmb[[x]]],tijs[x]))))*1

      } else if(j=="none") if(!silent) cat("\nCritical ", ifelse(i=="tc", "temperature", "pressure")," calculation was skipped since method is set to 'none'.", sep = "")
    }
  }

  #Compile and return results
  #Get global averages
  dfouts <- data.frame(sapply(outs, "length<-", max(lengths(outs))))
  global_means <- rowMeans(dfouts, na.rm = TRUE)
  global_sd <- sqrt(rowVars(dfouts, na.rm = TRUE))

  if(!any(is.na(c(pres, temp)))) {
    newpres <- pres[1]/10
    newtemp <- temp[1]+273.15
    mix_state <- unname(ifelse(newpres > global_means["Pc"] & newtemp > global_means["Tc"], "supercritical", "NOT supercritical"))

    statement <- paste0("Global averages: Pc = ", round(global_means["Pc"],2), " \u00b1 ",
                        round(global_sd["Pc"],2),
                        " MPa, Tc = ", round(global_means["Tc"],2), " \u00b1 ",
                        round(global_sd["Tc"],2), " K. At ",
                        newpres, " MPa and ", newtemp, " K (",
                        pres[1], " bar and ", temp[1], " C), the mixture physical state is ", mix_state, ".")
  } else statement <- "The physical state of the system was not determined since no reference pressure and temperature were provided."


  if(getdf) {
    finres <- cbind.data.frame(Method = names(outs), as.data.frame(bind_rows(outs)))
  } else finres <- cat("\nData frame of results was not generated since 'getdf' is set to FALSE...")

  if(!silent) cat("\nDONE!")
  return(list(fractions = frc, results = outs, results_df = finres,
              global_means = list(Mean = global_means, SD = global_sd), statement = statement))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Convert between mass and mole fractions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @noRd
conv_fracs <- function(solv, fracs, units){
  solv_inds <- pmin(match(solv, solv_db[,"Solvent"]), match(solv,solv_db[,"Abbreviation"]), na.rm = TRUE)
  mw <- solv_db[solv_inds, "MW", drop = TRUE]

  frc <- list()
  if(units == "mass") {
    frc[["mass"]] <- fracs
    frc[["mol"]] <- sapply(seq_along(frc[["mass"]]), function(x) (frc[["mass"]][x]/mw[x])/sum(frc[["mass"]]/mw))
  } else if(units == "mol") {
    frc[["mol"]] <- fracs
    frc[["mass"]] <- sapply(seq_along(frc[["mol"]]), function(x) (frc[["mol"]][x]*mw[x])/sum(frc[["mol"]]*mw))
  }
  return(frc)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Determine critical parameters of binary CO2-Ethanol mixtures
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Determine critical parameters of binary CO2-Ethanol mixtures
#'
#' @description A more accurate determination of critical parameters specific to
#' CO2-Ethanol binary mixtures using the Chueh-Prausnitz and Redlich-Kister methods.
#' These cover the entire CO2 molar fraction range of 0-1, while Redlich-Kister also reduces
#' overestimation of the Chueh-Prausnitz method at high molar fractions of >0.5 CO2.
#'
#' @param fracs A \code{numeric} value specifying the molar fraction of CO2 in the binary mixture.
#' @param units One of \code{"mass"} (default) or \code{"mol"}. Specifies the units of \code{fracs}.
#' @param pres,temp \strong{Optional} pressure and temperature (in bar and degrees Celsius, respectively), to
#' at which to determine whether the physical state of the binary mixture is supercritical or not.
#' @param method One of \code{"chueh"} (Chueh-Prausnitz method), \code{"redlich"} (Redlich-Kister method),
#' or \code{"both"}.
#'
#' @return A \code{list} including the \code{$results} of critical pressure and temperature estimations for each
#' specified method, and a \code{$statement} summarizing the physical state of the system at the optionally provided
#' pressure (\code{pres}) and temperature (\code{temp}).
#' @export
#'
#' @details
#' The calculations required for the Chueh-Prausnitz and Redlich-Kister methods may be found both in the source
#' publications cited below. They are also summarized and optimized coefficients given in Sun et al. (2022), with
#' experimental data sourced from Gil et al. (2012).
#'
#' @references
#' Chueh, P.L., Prausnitz, J.M. (1967), 'Vapor-Liquid Equilibria at High Pressures: Calculation of Critical Temperatures, Volumes, and Pressures of Nonpolar Mixtures', \emph{AIChE Journal} \strong{13} (6), pp. 1107-1113, DOI: \url{https://doi.org/10.1002/aic.690130613}.
#'
#' Gil, L., Blanco, S.T., Rivas, C., Laga, E., Fernandez, J., Artal, M., Velasco, I. (2012), 'Experimental determination of the critical loci for {n-C6H14 or CO2 + alkan-1-ol} mixtures. Evaluation of their critical and subcritical behavior using PC-SAFT EoS', \emph{Journal of Supercritical Fluids} \strong{71}, pp. 26-44, DOI: \url{https://www.doi.org/10.1016/j.supflu.2012.07.008}.
#'
#' Redlich, O., Kister, A.T. (1948), 'Algebraic Representation of Thermodynamic Properties and the Classification of Solutions', \emph{Industrial & Engineering Chemistry} \strong{40} (2), pp. 345-348, DOI: \url{https://doi.org/10.1021/ie50458a036}.
#'
#' Sun, R., Tian, H., Wu, Z., Shi, L., Shu, G. (2022), 'Comparative Study on Critical Points of Carbon Dioxide-Based Binary Mixtures', \emph{International Journal of Thermophysics} \strong{43}, article 122, DOI: \url{https://doi.org/10.1007/s10765-022-03048-3}.
#'
#' @examples
#' res <- iscrit_etoh(0.8, "mol", pres = 400, temp = 45, "redlich")
#' res[["statement"]]
#'
#' #For visualization of both methods
#' iscrit_demo()
#'
#' @seealso \code{\link{iscrit_gen}}
iscrit_etoh <- function(fracs, pres = NA, temp = NA, units = "mass", method = "redlich"){
  if(length(fracs)!=1) stop("Only the CO2 mass or mole fraction must be given in 'fracs'!")
  if(!all(units %in% c("mass", "mol"))) stop("The fractions 'units' must be one of 'mass' or 'mol'!")
  if(!all(method %in% c("chueh", "redlich", "both"))) stop("The 'method' must be one of 'chueh', 'redlich', or 'both'!")
  if(any(method %in% "both")) method <- c("chueh", "redlich")
  pnt <- c(pres, temp)
  if(!all(is.na(pnt)) & any(is.na(pnt))) stop("If reference pressure 'pres' is provided, temperature 'temp' must be given as well (and vice versa)!")
  if(!any(is.na(pnt))) {
    newpres <- pnt[1]/10
    newtemp <- pnt[2]+273.15
  }

  #Convert fractions
  fracs <- c(fracs, 1-fracs)
  fracs_mol <- if(units=="mass") conv_fracs(c("CarbonDioxide","Ethanol"), fracs, units)[["mol"]] else fracs

  #Properties of CO2 and EtOH
  props <- c("MW","RHOc", "Tc", "Pc", "Omega")
  co2 <- unlist(solv_db[solv_db[,"Abbreviation"]=="CO2", props])
  etoh <- unlist(solv_db[solv_db[,"Abbreviation"]=="EtOH", props])
  names(co2) <- names(etoh) <- c("mw", "rhoc", "tc", "pc", "omega")

  #Additional properties
  co2[c("frac", "vc")] <- c(fracs_mol[1], (co2["mw"]*1000)/(co2["rhoc"]*co2["mw"])) #mL/mol
  etoh[c("frac", "vc")] <- c(fracs_mol[2], (etoh["mw"]*1000)/(etoh["rhoc"]*etoh["mw"])) #mL/mol
  R <- 8.314 #J/(mol K)

  prms <- setNames(rbind.data.frame(co2, etoh), names(co2))

  #Begin calculation
  outs <- list()
  statement <- c()

  for(i in method) {
    if(i=="chueh") {

    #Get surface fraction
    theta <- unlist((prms["frac"] * prms["vc"]^(2/3))/sum(prms[,"frac"]*prms[,"vc"]^(2/3)))
    theta <- setNames(theta, c("co2", "etoh"))

    #Coefficients of Tc and Vc
    cftc <- c(A = -0.0076, B = 0.287, C = -1.343, D = 5.443, E = -3.038)
    cfvc <- c(A = -0.4957, B = 17.1185, C = -168.56, D = 587.05, E = -698.89)

    #Get Tc
    dT <- abs(diff(prms[,"tc"])/sum(prms[,"tc"]))
    psaiT <- cftc["A"] + cftc["B"] * dT + cftc["C"] * dT^2 + cftc["D"] * dT^3 + cftc["E"] * dT^4
    tau_ij <- 0.5 * sum(prms["tc"]) * psaiT
    outs[[toupper(i)]]["Tc"] <- Tc <- sum(theta*prms["tc"]) + 2 * theta["co2"] * theta["etoh"] * tau_ij

    #Get Vc
    dV <- abs(diff(prms[,"vc"]^(2/3))/sum(prms[,"vc"]^(2/3)))
    psaiV <- cfvc["A"] + cfvc["B"] * dV + cfvc["C"] * dV^2 + cfvc["D"] * dV^3 + cfvc["E"] * dV^4
    v_ij <- 0.5 * sum(prms["vc"]) * psaiV
    outs[[toupper(i)]]["Vc"] <- Vc <- sum(theta*prms["vc"]) + 2 * theta["co2"] * theta["etoh"] * v_ij

    #Get Pc
    omega_star <- unlist(0.0867 - 0.0125 * prms["omega"] + 0.011 * prms["omega"]^2)
    bees <- unlist(omega_star * R * prms["tc"]/prms["pc"])
    bm <- sum(prms["frac"]*bees)
    omega_a <- unlist(prms["pc"] * prms["vc"] * (prms["vc"]+bees) * (R * prms["tc"]/(prms["vc"] - bees) - prms["pc"]) / (R * prms["tc"])^2)
    ays <- unlist(omega_a * R^2 * prms["tc"]^2.5 / prms["pc"])

    #Optimized interaction parameters kij
    kij <- 0.20

    #Final calculation of Pc
    Tc12 <- (1 - kij) * sqrt(prod(prms["tc"]))
    a12 <- sum(omega_a) * R * Tc12^1.5 * sum(prms["vc"]) / (4 * (0.291 - 0.04 * sum(prms["omega"])))
    am <- sum(unlist(prms["frac"]^2)*ays) + 2 * prod(prms["frac"]) * a12
    outs[[toupper(i)]]["Pc"] <- Pc <- R * Tc / (Vc - bm) - am / (sqrt(Tc) * Vc * (Vc + bm)) #MPa


    } else if(i=="redlich") {

      #Coefficients for Tc and Pc (from Sun et al., 2022)
      cftc <- c(a1 = 115.507, a2 = -45.356, a3 = -129.584)
      cfpc <- c(a1 = 29.491, a2 = 22.023, a3 = -6.559, a4 = -27.863)

      #Final calculation
      outs[[toupper(i)]]["Tc"] <- Tc <- sum(prms["frac"]*prms["tc"]) + sum(cftc * prod(prms["frac"]) * (2*prms["frac"][1,] - 1)^(seq_along(cftc)-1))
      outs[[toupper(i)]]["Pc"] <- Pc <- sum(prms["frac"]*prms["pc"]) + sum(cfpc * prod(prms["frac"]) * (2*prms["frac"][1,] - 1)^(seq_along(cfpc)-1))
    }

    #Compile statement
    if(!any(is.na(pnt))) {
      mix_state <- unname(ifelse(newpres > Pc & newtemp > Tc, "supercritical", "NOT supercritical"))
      goodnms <- c(chueh = "Chueh-Prausnitz", redlich = "Redlich-Kister")
      statement <- append(statement, paste0("Based on the ", goodnms[i], " method , Pc = ", round(Pc,2),
                          " MPa and Tc = ", round(Tc,2), " K. At ",
                          newpres, " MPa and ", newtemp, " K (",
                          pnt[1], " bar and ", pnt[2], " C), the mixture physical state is ", mix_state, ".\n"))
    }
  }
  if(length(statement)>0) cat(statement) else statement <- "The physical state of the system was not determined since no reference pressure and temperature were provided."
  return(list(results = outs, statement = statement))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Test the Chueh-Prausnitz and Redlich-Kister methods
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname iscrit_etoh
#' @export
iscrit_demo <- function() {
  interval <- seq(0, 1, by = 0.01)
  method_names <- c(CHUEH = "Chueh-Prausnitz", REDLICH = "Redlich-Kister")

  #Experimental data (from Gil et al., 2012)
  df <- as.data.frame(matrix(c(
    0.0000, 514.25, 6.168,
    0.0500, 507.29, 7.309,
    0.1000, 501.54, 8.064,
    0.1500, 493.23, 9.052,
    0.1998, 486.84, 9.579,
    0.2504, 480.48, 10.509,
    0.3000, 474.58, 11.056,
    0.3496, 466.35, 11.865,
    0.3999, 458.40, 12.578,
    0.4496, 451.14, 13.390,
    0.5002, 436.22, 14.086,
    0.5505, 428.65, 14.439,
    0.6001, 413.32, 14.929,
    0.6504, 400.57, 15.077,
    0.7199, 374.38, 14.400,
    0.7506, 368.18, 13.842,
    0.8002, 348.47, 12.429,
    0.8504, 336.96, 11.088,
    0.9000, 326.90, 9.817,
    0.9501, 319.07, 8.605,
    1.0000, 304.21, 7.383
  ), ncol = 3, byrow = TRUE))

  df <- setNames(cbind.data.frame(rep("Experimental", nrow(df)), df),  c("Method", "Fraction", "Tc", "Pc"))

  res <- plotlist <- list()
  #df <- setNames(as.data.frame(matrix(ncol = 4)), c("Method", "Fraction", "Tc", "Pc"))
  for(i in seq_along(interval)) res[[i]] <- iscrit_etoh(interval[i], units = "mol", method = "both")[["results"]]
  for(i in names(res[[1]])) {
    df <- rbind.data.frame(df, setNames(cbind.data.frame(rep(method_names[i], length(interval)), interval,
                                                    Reduce("rbind.data.frame", lapply(res, function(x) x[[i]][c("Tc", "Pc")]))),
                                          c("Method", "Fraction", "Tc", "Pc")))
  }
  df <- df[complete.cases(df),]

  for(i in c("Tc", "Pc")) {
    plotlist[[i]] <- ggplot(data = df, aes(x = .data[["Fraction"]], y = .data[[i]], colour = .data[["Method"]], lty = .data[["Method"]])) +
      geom_line(data = df[df[,"Method"]!="Experimental",], linewidth = 1) +
      geom_point(data = df[df[,"Method"]=="Experimental",], shape = 23, size = 3.5, stroke = 1.7, fill = "white") +
      labs(colour = "Method: ", lty = "Method: ", x = expression(CO[2]~mole~fraction), y = if(i=="Tc") "Critical Temperature (K)" else "Critical Pressure (MPa)") +
      scale_x_continuous(breaks = scales::breaks_pretty(n = 8)) +
      scale_y_continuous(breaks = scales::breaks_pretty(n = 8)) +
      scale_linetype_manual(values = c(4,1,1)) +
      scale_colour_manual(values = c("darkred", "darkorange", "darkorange")) +
      #theme_bw(base_size = 16) +
      theme(panel.background = element_blank(),
            axis.text = element_text(size = 14, colour = "black"),
            axis.title = element_text(size = 16, colour = "black"),
            legend.text = element_text(size = 14, colour = "black"),
            legend.title = element_text(size = 16, colour = "black"),
            legend.position = "top",
            axis.line = element_line(linewidth = 1))
  }
  print(plotlist)
  return(list(data = df, plots = plotlist))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Optimize HSP-based solubility enhancement for various solvent-solute systems
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Optimize HSP-based solubility enhancement for SFE
#'
#' @description Part of the \code{\link{sfe_mod}} workflow. Assesses requirement and optimizes
#' co-solvent choice for Supercritical CO2 Extraction (SFE) from Hansen Solubility Parameters (HSPs)
#' and critical temperature of any solute.
#'
#' @param cp The \code{numeric} critical temperature of the solute (\strong{in Kelvin}).
#' @param hsp_vals A named \code{numeric} vector of length 3-5 containing HSPs for the solute.
#' Names must be: \code{"dD"}, \code{"dP"}, \code{"dHB"}, optionally also including \code{"dP_low"} and/or \code{"dHB_low"}.
#' @param modif A \code{character} vector of one or more modifiers (i.e. co-solvents) to evaluate.
#' May also be a \code{list} where each element of \code{length > 1} includes two or more solvents to be \strong{blended}.
#' For a list of modifiers, see \code{\link{sfe_mod}}.
#' @param modfracs Either \code{NA} (default) or a \code{list} of numeric vectors specifying the \strong{volume} percentages
#' for solvent blends included in \code{modif}. Overall length must match the number of blends provided.
#' Each volume percentage must be \code{>0} and \code{<100}, and the vector length must either equal the number of corresponding
#' blend elements (in which case the sum must equal to 100) or one less (in which case the fraction of the last element
#' is automatically determined to yield a total percentage of 100).
#' @param pres,temps A \code{numeric} vector of one or more pressure (75-700 bar) and temperature (31-70) values
#' at which to evaluate co-solvents.
#' @param vfrac The volume fraction of co-solvent(s) to use (between 0-1). Defaults to 0.1 (10% co-solvent).
#' @param silent A \code{logical} indicating whether to show user updates when executing the function
#' (\code{FALSE} by default).
#'
#' @return A \code{list} containing the following elements:
#' \describe{
#' \item{\code{Modifiers}}{A \code{character} vector of modifier names used for the evaluation.}
#' \item{\code{Volume_Fraction}}{The \code{numeric} volume fraction (between 0 and 1) used for the evaluation.}
#' \item{\code{SoluteHSP_vs_Temp}}{A \code{list} of 3 \code{data.frame} objects showing the variation of dD, dP, and dHB HSP parameters with temperature (as set by the \code{temps} argument).}
#' \item{\code{SolventBlend_HSPs}}{A \strong{nested} \code{list} of \code{data.frame} objects showing the variation of HSP parameters with pressure and temperature \strong{for each chosen modifier}.}
#' \item{\code{Ra}}{A \code{list} of \code{data.frame} objects showing the calculated HSP distances (\eqn{R_a}) for selected pressure and temperature values (\code{pres} and \code{temps}).}
#' \item{\code{Miscib_Enhancement}}{A \code{list} of \code{data.frame} objects showing the \strong{percentage Miscibility Enhancement} for pure CO2 and CO2-modifier mixtures.}
#' \item{\code{Best_Modifier}}{A \code{data.frame} showing the modifier providing the highest Miscibility Enhancement for the solute at every combination of pressure and temperature.}
#' \item{\code{Modifier_Ranking}}{A \strong{named} vector showing the percentage occurrence of each modifier in the \code{Best_Modifier} output list element.}
#' }
#' @export
#'
#' @examples
#' #Using limonene as an example
#' lim_cp <- 628.4626
#' lim_hsp <- c(dD = 14.00539, dP = 0, dHB = 1.7012, dP_low = 2.0448, dHB_low = 1.2838)
#'
#' #With simple pure co-solvents
#' modif <- c("Methanol", "Ethanol", "Hexane")
#' res <- hsp_optim(lim_cp, lim_hsp, modif, silent = TRUE)
#'
#' #Including some co-solvent blends
#' modif <- list(c("Ethanol", "Water"), "Ethanol", "Methanol", c("Ethanol", "Methanol"))
#' modfracs <- list(c(50,50), 34)
#' res <- hsp_optim(lim_cp, lim_hsp, modif, modfracs, silent = TRUE)
#'
#' @seealso \code{\link{sfe_mod}}
hsp_optim <- function(cp, hsp_vals, modif, modfracs = NA, pres = seq_last(80, 500, 20),
                      temps = seq_last(32, 65, 3), vfrac = 0.10,
                      silent = FALSE) {

  #Preliminary checks
  if(!is.numeric(cp) | length(cp)!=1) stop("The critical point 'cp' must be a single numeric value!")
  if(!all(sapply(list(pres,temps), is.vector)) | !all(sapply(list(pres,temps), is.numeric))) {
    stop("Input temperatures ('temps') and pressures ('pres') at which to evaluate solute HSP must both be numeric vectors!")
  } else {
    if(!silent) cat("\nRounding SFE temperature and pressure constraints to whole numbers...")
    temps <- round(temps, 0)
    pres <- round(pres, 0)
    if(min(temps)<31 | max(temps)>200) stop("Minimum and maximum allowed SFE temperature ('temps') is between 31 and 200 degrees celsius!")
    if(min(pres)<75 | max(pres)>1000) stop("Minimum and maximum allowed SFE pressure ('pres') is between 75 and 1000 bar!")
  }
  if(vfrac > 0.40) warning("A modifier volume fraction in excess of 0.40 is not recommended for SFE!") else if(!is.numeric(vfrac) & (vfrac < 0.01 | vfrac > 1)) {
    stop("Volumetric fraction of SFE modifier (by flow rate) must be provided as a single numeric value between 0.01 and 1.00!")
  }

  allowed_mods <- unname(unlist(show_solv()[!show_solv()[["Solvent"]] %in% c("CarbonDioxide","Hydrogen"), c("Solvent","Abbreviation")]))
  if(any(modif %in% "all")) modif <- allowed_mods[allowed_mods %in% show_solv()[,c("Solvent")]]
  if(!is.list(modif)) modif <- as.list(modif)

  #Check for any unidentified modifiers (not included in 'solv_db')
  modif_raw <- unique(unlist(modif))
  if(!all(modif_raw %in% allowed_mods)) {
    modif_culprit <- modif_raw[which(!modif_raw %in% allowed_mods)]
    stop(paste0("The following SFE modifiers were not recognized: ", paste0(modif_culprit, collapse = ", "), "!"))
  }

  #Check whether modifier input argument is a list or vector
  #Check the number of solvent blends and corresponding fractions given
  blends <- which(lengths(modif)>1)
  fracs <- which(!is.na(modfracs))

  blen <- length(modif[blends])
  blens <- lengths(modif[blends])

  fraclen <- length(modfracs[fracs])
  fraclens <- lengths(modfracs[fracs])

  if(blen>0) {
    if(blen!=fraclen) stop("The number of elements in modifier fractions 'modfracs' must be equal to that of solvent blends specified in 'modif'!")
    if(any(!unique(blens-fraclens) %in% c(0,1))) stop("The lengths of modifier fractions 'modfracs' provided for blends must be either equal or one less than the number of blend elements!")
    if(any(unlist(modfracs[fracs])>=100 | unlist(modfracs[fracs]) <= 0)) stop("All modifier fraction 'modfracs' for blends must be >0 and <100!")

    lesschk <- which(blens-fraclens==1)
    if(length(lesschk)>0) modfracs[lesschk] <- lapply(modfracs[lesschk], function(x) c(x,100-sum(x)))
    if(any(sapply(modfracs, sum)!=100)) stop("Final sums of blend percentages must be equal to 100!")

    #Reorder modifiers and their volume fractions such that blends come first
    modif <- modif[c(blends, seq_along(modif)[-blends])]
    modfracs <- c(modfracs, rep(100, length(modif)-blen))
  } else modfracs <- rep(100, length(modif))

  modabbs <- lapply(modif, function(x) solv_db[solv_db[,"Solvent"] %in% x,"Abbreviation"])

  #Create modifier blend names (and abbreviated names)
  modif_nms <- modabb_nms <- c()
  for(i in seq_along(modif)) {
    if(length(modif[[i]])==1) {
      modif_nms[i] <- modif[[i]]
      modabb_nms[i] <- modabbs[[i]]
    } else {
      modif_nms[i] <- paste0(paste0(modif[[i]], collapse = ":"), " (", paste0(modfracs[[i]], collapse = ":"), ")")
      modabb_nms[i] <- paste0(paste0(modabbs[[i]], collapse = ":"), " (", paste0(modfracs[[i]], collapse = ":"), ")")
    }
  }

  #Other checks
  if(!is.numeric(hsp_vals) | !any(length(hsp_vals) %in% c(3,4,5))) stop("A numeric vector of length 3-5 is expected for HSP values ('hsp_vals')!")
  if(!is.numeric(cp)) stop("A numeric value of solute critical temperature must be provided!")

  #Begin processing
  if(!silent) cat("\nCalculating SFE miscibility enhancement using a ", vfrac, " volume fraction of the following modifiers: ", paste0("'", modif_nms, "'", collapse = ", "), ".", sep = "")

  #Optimization of SFE
  #Format solute HSP data
  if(length(hsp_vals)>3) {
    #dpl <- grep("dP_low", names(hsp_vals))
    #dhbl <- grep("dHB_low", names(hsp_vals))
    #dpchk <- length(dpl)
    #dhbchk <- length(dhbl)
    hsp_vals <- hsp_vals[c(grep("dD", names(hsp_vals)),
                           grep("dP_low", names(hsp_vals)),
                           grep("dHB_low", names(hsp_vals)))] #c(1,4,5)
  }
  names(hsp_vals) <- c("dD", "dP", "dHB")

  #Calculate the temperature influence on HSPs of solute
  #Check that the critical temperature of solute does not exceed the maximum SFE set temperature
  if(!silent) cat("\nCalculating temperature influence of HSP values of solute...")

  ref_df <- solv_mv[["CarbonDioxide"]][as.character(pres),as.character(temps), drop = FALSE]
  if((cp-273.15) < max(as.numeric(colnames(ref_df)))) warning("The solute critical temperature exceeds maximum SFE set value! Resulting pressure influence not considered.")

  solute_hsplist <- list(ref_df, ref_df, ref_df) #Create a reference matrix
  names(solute_hsplist) <- names(hsp_vals)

  for(i in names(hsp_vals)) {
    redt_ref <- 298.15/cp
    redt_new <- (as.numeric(colnames(solute_hsplist[[i]]))+273.15)/cp
    hsp_new <- ((1-redt_new)/(1-redt_ref))*hsp_vals[i]
    hsp_new[which(hsp_new<0)] <- 0

    for(j in rownames(solute_hsplist[[i]])){
      solute_hsplist[[i]][j,] <- hsp_new
    }
  }

  #Load CO2 and modifier Molar Volume (MV) data in mL/mol
  #Load Solvent DB
  solvent_db <- cbind.data.frame(ID = unlist(solv_db[,c("Solvent", "Abbreviation")]),
                                 do.call(rbind.data.frame, replicate(2, solv_db[,!colnames(solv_db) %in% c("Solvent", "Abbreviation")], simplify = FALSE)))
  rownames(solvent_db) <- NULL

  #Retrieve molar volumes and adjust data resolution according to requirements
  if(!silent) cat("\nProcessing molar volumes...")

  solvent_mv <- lapply(solv_mv, function(x) x[as.character(pres),as.character(temps), drop = FALSE])

  #Compile final MV data
  molfracs <- mvs <- hsps <- slmv <- list()
  molfracs[["CO2"]] <- 100
  mvs[["CO2"]] <- solvent_db[solvent_db[,"ID"]=="CO2", "MV"]
  hsps[["CO2"]] <- setNames(as.numeric(solvent_db[solvent_db[,"ID"]=="CO2", c("dD", "dP", "dHB")]), c("dD", "dP", "dHB"))
  slmv[["CO2"]] <- solvent_mv[["CarbonDioxide"]]

  for(i in seq_along(modfracs)) {
    #Convert volume fraction to MOLE FRACTION
    ssub <- solvent_db[solvent_db[,"ID"] %in% modif[[i]],]
    res <- modfracs[[i]]/100*ssub[,"RHO"]/ssub[,"MW"]
    molfracs[[modif_nms[i]]] <- curmol <- res/sum(res)*100
    #Calculate average molar volume based on MOLE FRACTION
    mvs[[modif_nms[i]]] <- sum(curmol/100*ssub[,"MV"])
    #Calculate average HSP based on VOLUME FRACTION
    hsps[[modif_nms[i]]] <- colSums(modfracs[[i]]/100*ssub[,c("dD","dP","dHB")])
    #Calculate Average Molar Volume of solvent blends at different pressures and temperatures based on MOLE FRACTION
    slmv[[modif_nms[i]]] <- Reduce("+", lapply(seq_along(modif[[i]]), function(x) curmol[x]/100*solvent_mv[[modif[[i]][x]]]))
  }

  #Element-wise calculate the temperature-pressure influence on HSPs of CO2
  if(!silent) cat("\nCalculating the temperature and pressure influence on HSP of solvent(s)...")
  solvent_hsplist <- list()

  for(i in names(slmv)) {
    solvent_hsplist[[i]][["dD"]] <- apply(slmv[[i]], c(1,2), function(x) hsps[[i]]["dD"]/(mvs[[i]]/x)^-1.25)

    solvent_hsplist[[i]][["dP"]] <- apply(slmv[[i]], c(1,2), function(x) hsps[[i]]["dP"]/(mvs[[i]]/x)^-0.5)

    solvent_hsplist[[i]][["dHB"]] <- slmv[[i]]
    for(j in 1:ncol(slmv[[i]])) {
      t_val <- as.numeric(colnames(solvent_hsplist[[i]][["dHB"]])[j])
      solvent_hsplist[[i]][["dHB"]][,j] <-  hsps[[i]]["dHB"]/exp((-1.32*10^-3)*(298.15-(t_val+273.15))-sapply((mvs[[i]]/solvent_hsplist[[i]][["dHB"]][,j])^-0.5, function(x) log(x, base=exp(1))))
    }
  }

  #Element-wise calculate the HSPs of each CO2-modifier system using the Linear Blend Rule
  blend_hsplist <- list()
  blend_hsplist[["CO2"]] <- solvent_hsplist[["CO2"]]
  co2_frac <- 1 - vfrac

  for(i in names(solvent_hsplist)[!names(solvent_hsplist) %in% "CO2"]) {
    for(j in names(hsp_vals)) {
      blend_hsplist[[paste0("CO2 with ",i)]][[j]] <- co2_frac*(solvent_hsplist[["CO2"]][[j]])+vfrac*(solvent_hsplist[[i]][[j]]) #sqrt(co2_frac*(solvent_hsplist[["CO2"]][[j]])^2+vfrac*(solvent_hsplist[[i]][[j]])^2)
    }
  }

  #Element-wise calculate the Ra values for the pure CO2 versus the solute, and that of the CO2-modifier system(s). Lower Ra = better solubility.
  if(!silent) cat("\nCalculating Ra distances and Miscibility Enhancement (%)...")

  ra_list <- list()

  for(i in names(blend_hsplist)) {
    ra_list[[i]] <- sqrt(4*(solute_hsplist[["dD"]]-blend_hsplist[[i]][["dD"]])^2+(solute_hsplist[["dP"]]-blend_hsplist[[i]][["dP"]])^2+(solute_hsplist[["dHB"]]-blend_hsplist[[i]][["dHB"]])^2)
  }

  #Element-wise calculate the Miscibility Enhancement (%) using the Ra values of the pure CO2 and the CO2-modifier system.
  miscib_list <- list()
  for(i in names(ra_list)[!names(ra_list) %in% "CO2"]) {
    miscib_list[[i]] <- (1-(ra_list[[i]]/ra_list[["CO2"]]))*100
  }

  #Determine the best miscibility improvement at each temperature-pressure combination
  if(length(modif)>1) {

    miscib_array <- array(unlist(lapply(miscib_list, as.matrix)), c(dim(miscib_list[[1]]),length(miscib_list)))
    miscib_max <- as.data.frame(apply(miscib_array, c(1,2), max, na.rm = TRUE))
    miscib_best <- as.data.frame(apply(miscib_array, c(1,2), which.max))

    colnames(miscib_best) <- colnames(miscib_list[[1]])
    rownames(miscib_best) <- rownames(miscib_list[[1]])
    miscib_best <- apply(miscib_best, c(1,2), function(x) { res <- modabb_nms[x];return(res)})
    miscib_best[miscib_max < 0] <- "CO2"

    #Determine which solvent occurs most often
    best_ranking <- table(unlist(miscib_best))/(dim(miscib_best)[1]*dim(miscib_best)[2])*100
    best_overall <- names(which.max(best_ranking))
  }

  #Return results
  res <- list("Modifiers" = modif_nms,
              "Volume_Fraction" = vfrac,
              "SoluteHSP_vs_Temp"= solute_hsplist,
              "SolventBlend_HSPs"= blend_hsplist,
              "Ra" = ra_list,
              "Miscib_Enhancement" = miscib_list)

  if(length(modif)>1) {
    res <- append(res, list("Best_Modifier" = miscib_best,
                            "Modifier_Ranking" = best_ranking))
  }
  if(!silent) cat("\nDONE!")
  return(res)
}
