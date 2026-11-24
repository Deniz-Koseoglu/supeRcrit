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
plot_gcm <- function(solute, smarts, hlight = TRUE, draw_plot = TRUE, plot_title = TRUE, method = NULL, order = 1L) {

  #ALTERNATE PLOTTING OPTION
  #grid::grid.raster(rcdk::view.image.2d(solute$Molfile, depictor = rcdk::get.depictor(sma = gcm_smartstr[1,"SMARTS"]))) #or graphics::rasterImage()

  #Preliminary checks
  if(!is.vector(smarts) | !is.character(smarts)) stop("The 'smarts' argument must be a character vector of SMARTS sub-structures!")
  if(!is.list(solute) | !all(c("IDs","Molfile") %in% names(solute))) stop("The solute argument must be a list output from function 'mol_find' containing at least 'IDs' and 'Molfile'!")
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
  draw_mol <- depict::parse_smiles(paste0(solute_ids["SMILES"], " ", solute_ids["Name"], " (", solute_ids["CAS"], ")"))

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

  if(hlight) {
    # Highlight by ATOM INDEX, not by re-matching SMARTS through the depictor. depict::match_smarts uses a
    # different aromaticity model than the engine (it does NOT de-aromatize ring carbonyls), so it silently
    # missed de-aromatized groups (e.g. caffeine's ring C=O / pyrimidinedione N's looked "uncovered" even
    # though they ARE assigned). draw_mol is a CDK AtomContainer; cast to IAtomContainer so the engine
    # matcher / fragmenter resolve, then colour atoms by index.
    draw_iface <- rJava::.jcast(draw_mol, "org/openscience/cdk/interfaces/IAtomContainer")
    hl <- function(dep, idx, col) {                       # colour a set of 0-based atom indices on draw_mol
      idx <- unique(idx[!is.na(idx)]); if(!length(idx)) return(dep)
      hs <- rJava::.jnew("java.util.HashSet")
      for(a in idx) { atm <- tryCatch(rJava::.jcall(draw_mol, "Lorg/openscience/cdk/interfaces/IAtom;", "getAtom", as.integer(a)), error = function(e) NULL)
        if(!is.null(atm)) rJava::.jcall(hs, "Z", "add", rJava::.jcast(atm, "java.lang.Object")) }
      tryCatch(depict::highlight_atoms(dep, hs, col), error = function(e) dep)
    }
    if(!is.null(method)) {
      # MAXIMALLY ROBUST: take the EXACT group->atom assignment the fragmentation produced for this group
      # ORDER (order 1 = the disjoint first-order partition; 2/3 = the correction groups). One colour per
      # group, by atom index — no SMARTS re-matching. A higher order with no spatial groups -> no image.
      ga <- tryCatch(.frag_group_atoms(draw_iface, method, order), error = function(e) list())
      if(order > 1L && !length(ga)) return(NULL)
      for(k in seq_along(ga)) dep_obj <- hl(dep_obj, ga[[k]], jcol_vals[[((k - 1L) %% length(jcol_vals)) + 1L]])
    } else {
      # Fallback (caller supplied no method): engine-match each passed group SMARTS and colour its atoms.
      for(i in seq_along(vis_vec))
        dep_obj <- hl(dep_obj, unlist(tryCatch(.group_matches(vis_vec[i], draw_iface), error = function(e) list())),
                      jcol_vals[[((i - 1L) %% length(jcol_vals)) + 1L]])
    }
  }

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
#FUNCTION: Physical-plausibility (applicability-domain) guard for GCM outputs
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Values outside these bounds are non-physical for any real condensed-phase organic, so a GCM that emits
# them is extrapolating out of its applicability domain (uncovered/atypical group composition). We void
# them to NA and flag it (see gcm_domain_flags) rather than hand back an impossible number such as dD=0,
# Pc=380 bar, or Tb>1500 K. Bounds are DELIBERATELY GENEROUS - they catch gross extrapolation, not ordinary
# method error: empirically <0.25% of clean-method (SP/NL/JR/HKR-criticals) predictions are voided, versus
# ~11% of the known-pathological HKR dD tail (alkynes/perfluoroalkanes) and the HKR_STW Pc perfluoro blow-up.
# This complements the structural coverage flag (which only catches UNcovered atoms): ~82% of the voided
# compounds are fully covered but PARAMETRICALLY out of domain, which coverage alone cannot detect.
# Floors/ceilings are the TRUE physical extremes of real matter, NOT the typical-organic range, so the
# envelope can never clip a correct prediction of a real value (chemical out-of-domain, e.g. non-organics,
# is caught separately by coverage/element flags). Calibrated against the experimental test-set extremes:
# Tb floor < D2 (23.6 K), Tc floor < D2 (38 K), Pc ceiling > water/H2O2 (218-220 bar). Everything voided
# beyond these is a genuine GCM blow-up (Tb>1500 overprediction, perfluoro Pc 256-380, HKR dD 0/65).
.gcm_phys_bounds <- list(Tb = c(20,1500), Tb_corr = c(20,1500), Tc = c(30,2000), Pc = c(1,250),
                         Vc = c(30,6000), dD = c(10,30), dP = c(0,35), dHB = c(0,55))
# Void out-of-range elements of a named GCM result vector; record the voided names in attribute "phys_oob".
.guard_physical <- function(res) {
  oob <- character(0)
  for(nm in names(res)) {
    b <- .gcm_phys_bounds[[nm]]
    if(!is.null(b) && !is.na(res[[nm]]) && is.finite(res[[nm]]) && (res[[nm]] < b[1] || res[[nm]] > b[2])) {
      oob <- c(oob, nm); res[[nm]] <- NA_real_
    }
  }
  if(length(oob)) attr(res, "phys_oob") <- oob
  res
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Calculate GCM-based boiling point
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @noRd
gcm_bp <- function(contribs, method = NA) {

  #Preliminary checks
  if(length(contribs)<4 | !all(c("results", "solute", "smarts", "method") %in% names(contribs))) stop("Argument 'contribs' must be output from the function 'gcm_contribs'!")
  #Retrieve solute info
  solute <- contribs[["solute"]]
  if(!is.list(solute) | !all(c("IDs","Molfile") %in% names(solute))) stop("The solute argument must be a list output from function 'mol_find' containing at least 'IDs' and 'Molfile'!")
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

    # Marrero-Gani Tb = a*ln(sum of contributions): only physical when the sum > 1 (=> Tb > 0). For
    # very small/exotic molecules the sum can be <=1 (or <=0 with NA-contribution groups), which would
    # otherwise yield a NEGATIVE (impossible) boiling point. Return NA (out-of-domain) instead.
    bpoint["Tb"] <- if(is.finite(total_cont) && total_cont > 1)
      ifelse(any(grepl("_STW", method)), 244.5165, 244.7889)*log(total_cont) else NA_real_
  }
  return(.guard_physical(bpoint))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Calculate GCM-based critical parameters
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @noRd
gcm_crit <- function(bp = NA, contribs, method = NA) {

  #Preliminary checks
  if(length(contribs)<4 | !all(c("results", "solute", "smarts", "method") %in% names(contribs))) stop("Argument 'contribs' must be output from the function 'gcm_contribs'!")
  #Retrieve solute info
  solute <- contribs[["solute"]]
  if(!is.list(solute) | !all(c("IDs","Molfile") %in% names(solute))) stop("The solute argument must be a list output from function 'mol_find' containing at least 'IDs' and 'Molfile'!")
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
    # Tb/Tc use a*ln(sum-of-contributions); this is only defined (and physical) when the summed
    # contribution exceeds 1 (otherwise ln<=0 gives a non-physical <=0 K). Such cases (very small
    # molecules, or substituents with small/negative contributions) are outside the model's domain,
    # so return NA rather than an impossible negative temperature.
    if(is.na(bp)) critres["Tb"] <- if(isTRUE(total_cont["Tb"] > 1)) univcon[1]*log(total_cont["Tb"]) else NA_real_
    critres["Tc"] <- if(isTRUE(total_cont["Tc"] > 1)) univcon[2]*log(total_cont["Tc"]) else NA_real_
    critres["Pc"] <- (univcon[4] + total_cont["Pc"])^-2 + univcon[3]
    critres["Vc"] <- univcon[5] + total_cont["Vc"]
  }
  return(.guard_physical(critres))
}

#~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Calculate HSP
#~~~~~~~~~~~~~~~~~~~~~~~
#' @noRd
gcm_hsp <- function(contribs, method = NA) {

  #Preliminary checks
  if(length(contribs)<4 | !all(c("results", "solute", "smarts", "method") %in% names(contribs))) stop("Argument 'contribs' must be output from the function 'gcm_contribs'!")
  #Retrieve method
  if(any(is.na(method))) method <- contribs[["method"]]
  posmets <- c("SP08", "SP12", "SP_ENK", "HKR_STW", "HKR_SIM")
  if(!any(method %in% posmets)) stop(paste0("Incorrect HSP GCM method ('method') chosen! Possible values: ", paste0("'", posmets, "'", collapse = ", "), "!"))

  #Retrieve data frame with group contributions
  cdata <- contribs[["results"]]

  #Set coefficients based on which SP method (2008 or 2012) is used
  # SP_ENK = Enekvist et al. (2021) new HSP GCM (simultaneous regression, Eqs 13-15): single additive
  # constants for dD/dP/dHB, no power form and no separate low-value (<3) equation.
  cf <- if(any(grepl("SP_ENK", method))) c(16.4677, 1, 7.1519, 6.7601)
  else if(any(grepl("SP08", method))) c(17.3231, 1, 7.3548, 7.9793, 2.7467, 1.3720)
  else if(any(grepl("SP12", method))) c(959.11, 0.4126, 7.6134, 7.7003, 2.6560, 1.3720)
  else if(any(grepl("HKR_", method))) c(0,1,0,0) #Effectively no coefficients

  #Calculate HSPs (NOTE: group contribution sums take into account both first- and second-order groups!)
  hspres <- c()
  hspres["dD"] <- (sum(cdata[,"Sum_dD_cont"], na.rm = TRUE)+cf[1])^cf[2] #Units of MPa^0.5
  hspres["dP"] <- sum(cdata[,"Sum_dP_cont"], na.rm = TRUE)+cf[3]
  hspres["dHB"] <- sum(cdata[,"Sum_dHB_cont"], na.rm = TRUE)+cf[4]

  # Stefanis-Panayiotou LOW-VALUE equation. The paper states Eqs (A.3)/(A.4) for dP/dHB "are valid ONLY for
  # Hansen solubility parameter values greater than 3 MPa^0.5"; when the regular value is < 3 it must be
  # REPLACED by the low-value equation (Eqs A.5/A.6): the SAME molecule re-summed with the SEPARATE low-value
  # first- + second-order contributions (cols dP_low_cont/dHB_low_cont, Tables A.5/A.6) plus the low constant
  # (cf[5]/cf[6]). Groups absent from the low tables contribute 0 (standard additive convention). SP_ENK has
  # no low-value form. (Earlier this value was computed but never substituted, and a fallback wrongly mixed
  # in the REGULAR contribution for groups lacking a low value - both fixed.)
  if(any(grepl("^SP", method)) && !any(grepl("SP_ENK", method))) {
    for(i in c("dP","dHB")) {
      if(isTRUE(hspres[i] < 3)) {
        addval <- if(i=="dP") cf[5] else cf[6]
        hspres[i] <- sum(cdata[, paste0(i, "_low_cont")] * cdata[, "Occurrences"], na.rm = TRUE) + addval
      }
    }
  }

  #Making negative values 0
  hspres[which(hspres<0)] <- 0

  # Physical-plausibility guard (see .guard_physical). Bites mainly on HKR HSP, which is an UNcalibrated
  # raw group-sum (cf above = c(0,1,0,0), no intercept): in-domain molecules already sum to a physical HSP
  # (mean dD ~16), but out-of-domain structures (alkynes, perfluoroalkanes, diazo, ...) sum to non-physical
  # values the negative->0 clamp turns into an impossible dD=0 (or over-count to ~65). SP/SP_ENK carry an
  # intercept so they are essentially untouched (<0.1% voided). dD/dP/dHB are guarded; the SP low-value
  # *_low elements are not in the bounds table and pass through unchanged.
  return(.guard_physical(hspres))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Slonimskii (1970) intrinsic atomic volume via the sphere-cap formula
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Faithful re-implementation of Slonimskii, Askadskii & Kitaigorodskii (1970) eqs (2)+(3): each atom's
# intrinsic volume = full vdW sphere minus the spherical caps cut off by every covalent bond. This computes
# every Table-3 increment from first principles (verified: CH2=17.07, aromatic CH=14.71, quaternary C=5.01,
# carbonyl O=5.83, etc.) so it generalises to any bonding environment. Returns A^3/molecule (NA if any element
# lacks a tabulated radius -> out of domain). Radii = Table 1; bond lengths = Table 2.
.slon_bondlen <- function(si, sj, ord, barom, ai, aj) {
  p <- paste(sort(c(si, sj)), collapse = "-")
  if (barom) { if (p == "C-C") return(1.40); if (grepl("N", p)) return(1.37); if (grepl("O", p)) return(1.36); return(1.40) }
  if (ord == "DOUBLE") { if (grepl("O", p)) return(1.28); return(1.34) }
  if (ord == "TRIPLE") { if (grepl("N", p)) return(1.16); return(1.20) }
  if (grepl("H", p)) return(1.08)                                  # C-H / O-H / N-H
  if (p == "C-C") return(if (ai || aj) 1.48 else 1.54)             # 1.48 if a carbon is aromatic
  if (p == "C-O") return(if ((si == "C" && ai) || (sj == "C" && aj)) 1.36 else 1.50)
  if (p == "C-N") return(1.37); if (p == "C-Cl") return(1.77); if (p == "C-F") return(1.34)
  1.54
}
.slon_intrinsic <- function(mol) {
  RAD <- c(C = 1.80, H = 1.17, O = 1.36, N = 1.57, Cl = 1.78, F = 1.50)
  atoms <- rcdk::get.atoms(mol); n <- length(atoms)
  sym <- vapply(atoms, rcdk::get.symbol, character(1))
  if (any(!sym %in% names(RAD))) return(NA_real_)                  # element without a Slonimskii radius
  arA <- vapply(atoms, function(a) tryCatch(rJava::.jcall(a, "Z", "getFlag", 32L), error = function(e) FALSE), logical(1))
  hsh <- vapply(atoms, function(a) rJava::.jcall(a, "I", "hashCode"), integer(1)); idx <- setNames(seq_len(n), hsh)
  caps <- numeric(n)
  for (b in rcdk::get.bonds(mol)) {
    ba <- rcdk::get.atoms(b)
    i <- idx[[as.character(rJava::.jcall(ba[[1]], "I", "hashCode"))]]; j <- idx[[as.character(rJava::.jcall(ba[[2]], "I", "hashCode"))]]
    ord <- rJava::.jcall(rJava::.jcall(b, "Lorg/openscience/cdk/interfaces/IBond$Order;", "getOrder"), "S", "toString")
    barom <- tryCatch(rJava::.jcall(b, "Z", "getFlag", 32L), error = function(e) FALSE)
    d <- .slon_bondlen(sym[i], sym[j], ord, barom, arA[i], arA[j]); Ri <- RAD[sym[i]]; Rj <- RAD[sym[j]]
    hi <- Ri - (Ri^2 + d^2 - Rj^2) / (2 * d); caps[i] <- caps[i] + (1/3) * pi * hi^2 * (3 * Ri - hi)
    hj <- Rj - (Rj^2 + d^2 - Ri^2) / (2 * d); caps[j] <- caps[j] + (1/3) * pi * hj^2 * (3 * Rj - hj)
  }
  sum((4/3) * pi * RAD[sym]^3 - caps)
}

# Zhao (2003) VABC computed directly from the structure (element counts + bond/ring corrections), so ZHAO
# needs no group decomposition. V = Sum(atomic vols) - 5.92*NB - 14.7*RA - 3.8*RNR. NA if any element lacks a
# Zhao atomic volume. (The "Br=40.48" row in the legacy table is an OCR typo for Boron; corrected to B here.)
.zhao_vabc <- function(mol) {
  ZV <- c(H=7.24, C=20.58, N=15.60, O=14.71, F=13.31, Cl=22.45, Br=26.52, I=32.52, P=24.43, S=24.43,
          As=26.52, B=40.48, Si=38.79, Se=28.73, Te=36.62)
  sym <- vapply(rcdk::get.atoms(mol), rcdk::get.symbol, character(1))
  if (any(!sym %in% names(ZV))) return(NA_real_)
  rd <- tryCatch(rcdk::eval.desc(mol, "org.openscience.cdk.qsar.descriptors.molecular.SmallRingDescriptor"),
                 error = function(e) NULL)
  RA  <- if (!is.null(rd)) as.numeric(rd[["nAromRings"]]) else 0
  RNR <- if (!is.null(rd)) as.numeric(rd[["nSmallRings"]]) - RA else 0
  sum(ZV[sym]) - 5.92 * length(rcdk::get.bonds(mol)) - 14.7 * RA - 3.8 * RNR
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Calculate van der Waals Volume
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @noRd
gcm_vdw <- function(contribs, method = NA) {

  #Preliminary checks
  if(length(contribs)<4 | !all(c("results", "solute", "smarts", "method") %in% names(contribs))) stop("Argument 'contribs' must be output from the function 'gcm_contribs'!")
  #Retrieve method
  if(any(is.na(method))) method <- contribs[["method"]]
  posmets <- c("ZHAO", "SLON", "BND")
  if(!any(method %in% posmets)) stop(paste0("Incorrect HSP GCM method ('method') chosen! Possible values: ", paste0("'", posmets, "'", collapse = ", "), "!"))

  #Retrieve data frame with group contributions
  cdata <- contribs[["results"]]
  res <- c()

  #Retrieve auxiliary data
  mol <- contribs[["solute"]][[2]]
  numbonds <- length(get.bonds(mol)) #Total bonds (mol_find makes H explicit, so C-H bonds are included)

  #Summed atomic/group contributions
  VDW_res <- sum(cdata[,"Sum_VDW_cont"], na.rm = TRUE)
  #ZHAO (2003) VABC: V = sum(atomic) - 5.92*NB - 14.7*RA - 3.8*RNR  (NB=bonds, RA=aromatic rings,
  #RNR=non-aromatic rings). Previously the -5.92*NB term was computed but never assigned (no-op) and the
  #ring terms were missing entirely -> ~2x over-estimate; fixed here.
  if(any(method %in% "ZHAO")) {
    # ZHAO VABC computed directly from the structure (element counts + bond/ring corrections); no group
    # decomposition needed. NA when an element has no Zhao atomic volume -> out of domain.
    VDW_res <- .zhao_vabc(mol)
  } else if(any(method %in% "SLON")) {
    #SLON (Slonimskii 1970): the intrinsic volume is the sphere-cap sum computed directly from molecular
    #geometry (eqs 2+3), NOT a group/atom-increment sum. Compute it from the structure, ignoring the
    #(legacy) results table. NA when an element has no tabulated radius (Br/I/S/P/...) -> out of domain.
    VDW_res <- .slon_intrinsic(mol)
  }
  #Convert to SI m3/mole. Bondi tabulates cm3/mole directly; ZHAO/SLON give Angstrom^3/molecule -> *0.602 (=N_A*1e-24) = cm3/mole.
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
#' \item \strong{For HSP} (\code{hsp}): \code{"SP08"}, \code{"SP12"} (Stefanis & Panayiotou 2008-2012), or
#' \code{"SP_ENK"} (Enekvist et al. 2021 - SP group scheme re-regressed simultaneously with new group values
#' and a dedicated conjugated keto-hydrazone group, targeted at organic pigments). The units for these
#' parameters are MPa^(1/2).
#' \item \strong{For all of the above}: \code{"HKR_STW"}, \code{"HKR_SIM"} (Hukkerikar 2012).
#' \item \strong{For van der Waals volume} (\code{vdw}): \code{"ZHAO"} (Zhao 2003), \code{"BND"} (Bondi 1964), or \code{"SLON"} (Slonimskii 1970).
#' The units are L/mole (i.e. SI units).
#' }
#' @param hlight A \code{logical} indicating whether substructures are highlighted (see \code{\link{plot_gcm}}).
#' @param simplicity A \code{character} string indicating whether overlapping sub-structures should be allowed (\code{"auto"} by default).
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
#' @seealso \code{\link{mol_find}}, \code{\link{sfe_mod}}, \code{\link{plot_gcm}}
#'
est_gcm <- function(solute, tb = NA, crit = NA, hsp = NA, vdw = NA, hlight = TRUE, simplicity = "auto", gorder = 0, silent = FALSE) {

  #Preliminary checks
  gcm_conds <- list(tb = tb, crit = crit, hsp = hsp, vdw = vdw)
  cond_nms <- unlist(gcm_conds[sapply(gcm_conds, function(x) !is.numeric(x) & !is.na(x))])

  if(all(is.na(gcm_conds))) stop("No GCMs were specified! At least one of 'tb', 'crit', 'hsp', and/or 'vdw' must be provided.") else metlst <- gcm_conds[!is.na(gcm_conds)]
  if(is.numeric(hsp) & length(hsp)!=3) stop("When numeric, 'hsp' must be of length 3!")

  # Validate method strings up front so an unrecognized / non-GCM method ("manual", "none", "", a typo, or a
  # database "experimental" marker) fails here with a clear message, instead of a cryptic "get1index" crash
  # deep inside gcm_contribs_route/plot_gcm. Each GCM property has its own set of valid methods (mirrors the
  # 'posmets' checks in gcm_bp/gcm_crit/gcm_hsp/gcm_vdw). Numeric values (direct/experimental) are exempt.
  posmets_by_par <- list(
    tb   = c("JR", "JR_corr", "SB", "SB_corr", "NL04", "HKR_STW", "HKR_SIM"),
    crit = c("JR", "JR_corr", "NL07", "NL07_robust", "HKR_STW", "HKR_SIM"),
    hsp  = c("SP08", "SP12", "SP_ENK", "HKR_STW", "HKR_SIM"),
    vdw  = c("ZHAO", "SLON", "BND"))
  for(.par in names(metlst)) {
    .m <- metlst[[.par]]
    if(!is.numeric(.m) && !is.na(.m) && !(.m %in% posmets_by_par[[.par]]))
      stop(sprintf("Unknown %s GCM method '%s'. Valid methods: %s. A non-GCM value (e.g. \"manual\"/\"none\") must be supplied as a numeric value or via an experimental override, not as a method name.",
                   .par, as.character(.m), paste0("'", posmets_by_par[[.par]], "'", collapse = ", ")), call. = FALSE)
  }

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
  visres <- pares <- conts <- covmap <- list()
  estmets <- c()
  phys_oob_all <- character(0)   # parameter ids voided by the physical-plausibility guard (.guard_physical)

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
      #Get group contributions: Müller engine for migrated methods (HKR), legacy chain otherwise
      contribs <- gcm_contribs_route(solute_data, metlst[[i]], simplicity = simplicity,
                                     gorder = unique(gorder[names(gorder)==metlst[[i]]]), silent = TRUE)
      conts[[curnm]] <- contribs[["results"]]
      covmap[[curnm]] <- contribs[["coverage"]]      # engine coverage (covered/n_heavy); NULL for legacy path
      #Create plot of identified groups
      # Render one depiction per available group ORDER (1st-order partition + any 2nd/3rd-order correction
      # groups), so the UI can switch between them. visres[[curnm]] becomes a named list "1"/"2"/"3" -> image.
      vis_imgs <- list()
      for (.o in 1:3) { .im <- plot_gcm(solute_data, contribs[["smarts"]], hlight, draw_plot = FALSE,
                                        plot_title = TRUE, method = metlst[[i]], order = .o)
        if (!is.null(.im)) vis_imgs[[as.character(.o)]] <- .im }
      visres[[curnm]] <- if (length(vis_imgs)) vis_imgs else NA
      #Calculate parameter(s)
      res <- if(i=="tb") gcm_bp(contribs)
      else if(i=="crit") gcm_crit(tb_forcrit, contribs)
      else if(i=="hsp") gcm_hsp(contribs)
      else if(i=="vdw") gcm_vdw(contribs)
      phys_oob_all <- c(phys_oob_all, attr(res, "phys_oob"))   # non-physical params voided to NA

      if(i!="tb") pares[[curnm]] <- res
      #Subset tb for critical parameter calculation
      if(i=="tb") tb_forcrit <- res[length(res)] # & !is.numeric(crit)
      estmets[curnm] <- metlst[[i]]
    } else if(!is.na(metlst[[i]])) {
      if(i=="tb") tb_forcrit <- tb
      if(i=="crit" & is.numeric(crit)) pares[[curnm]] <- c("Tb" = tb_forcrit, "Tc" = crit)
      if(i!="crit") estmets[curnm] <- "experimental" else estmets["Tc"] <- "experimental"
      visres[[curnm]] <- conts[[curnm]] <- NA
    } else next
  }

  if(all(sapply(visres, function(x) all(is.na(x))))) visres <- NA else visres <- visres[!is.na(visres)]
  if(all(sapply(conts, function(x) all(is.na(x))))) conts <- NA else conts <- conts[!is.na(conts)]
  pares[["Methods"]] <- estmets
  pares[["Contribs"]] <- conts
  #Domain flags: which estimated parameters use a detected group that has NO calibrated contribution
  #(e.g. Nannoolal leaves Tc/Pc/Vc unparameterised for dienes/amides/...). The estimate is still
  #produced (best-effort) but flagged so the UI can warn the user it is uncalibrated/rough.
  pares[["Domain"]] <- gcm_domain_flags(conts, covmap, phys_oob = phys_oob_all)

  return(list(solute_data = solute_data, pares = pares, visres = visres))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Flag estimated parameters whose detected groups lack a calibrated contribution
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Out-of-domain flags for GCM-estimated parameters
#' @description For each estimated parameter, returns the detected group(s) (if any) that have NO
#'   calibrated contribution for that parameter in the source method. Such estimates are computed
#'   best-effort (the missing group contributes 0) but should be treated as rough/uncalibrated.
#' @param conts The \code{Contribs} list from \code{\link{est_gcm}} (group-contribution tables keyed by
#'   parameter group: \code{Tb}, \code{Critical}, \code{HSP}, \code{VDW}).
#' @return A named \code{list}: parameter id (e.g. \code{"Vc"}) -> character vector of uncalibrated
#'   group names. Parameters that are fully in-domain are absent from the list.
#' @param phys_oob Optional \code{character} vector of parameter ids (e.g. \code{"Pc"}, \code{"dD"}) whose
#'   estimate was voided to \code{NA} by the physical-plausibility guard (\code{.guard_physical}) - i.e. the
#'   method produced a non-physical value (out of applicability domain). These are merged into the flags.
#' @keywords internal
gcm_domain_flags <- function(conts, coverage = NULL, phys_oob = NULL) {
  empty <- (!is.list(conts) || !length(conts))
  if(empty && !length(phys_oob)) return(list())
  if(empty) conts <- list()
  pm <- list(Tb = c("Tb","Tb_cont"), Tb_corr = c("Tb","Tb_cont"),
             Tc = c("Critical","Tc_cont"), Pc = c("Critical","Pc_cont"), Vc = c("Critical","Vc_cont"),
             dD = c("HSP","dD_cont"), dP = c("HSP","dP_cont"), dHB = c("HSP","dHB_cont"),
             VDW = c("VDW","Vm_cont"))
  out <- list()
  for(p in names(pm)) {
    grp <- pm[[p]][1]; df <- conts[[grp]]; col <- pm[[p]][2]
    if(is.null(df) || !is.data.frame(df) || !all(c(col,"Group","Occurrences") %in% names(df))) next
    occ <- suppressWarnings(as.numeric(df[["Occurrences"]]))
    base <- suppressWarnings(as.numeric(df[[col]]))
    # (a) detected FIRST-ORDER group(s) with NO calibrated contribution -> out-of-domain. A second-order
    #     (Type 2/3) group with *** is merely a missing CORRECTION (skipped, treated 0), NOT out-of-domain,
    #     so it must not flag (e.g. SP >C{H or C}-C= has no dP, C6-ring no dHB; the compound is still predictable).
    is_first <- if("Type" %in% names(df)) suppressWarnings(as.numeric(df[["Type"]])) == 1 else rep(TRUE, nrow(df))
    is_first[is.na(is_first)] <- TRUE
    bad <- df[["Group"]][is.na(base) & !is.na(occ) & occ > 0 & is_first]
    # (b) INCOMPLETE first-order atom coverage: the molecule has atom(s) outside the method's group set,
    #     so the estimate silently omits their contribution -> out-of-domain (engine path only; covmap[grp]).
    cov <- if(is.list(coverage)) coverage[[grp]] else NULL
    if(is.list(cov) && length(cov$covered) && length(cov$n_heavy) &&
       !is.na(cov$covered) && !is.na(cov$n_heavy) && cov$covered < cov$n_heavy)
      bad <- c(bad, sprintf("<%d/%d atoms uncovered>", cov$n_heavy - cov$covered, cov$n_heavy))
    if(length(bad)) out[[p]] <- unique(as.character(bad))
  }
  # (c) parameters voided by the physical-plausibility guard: the method emitted a non-physical value and
  #     the estimate was set to NA (out of applicability domain). Coverage may be complete (parametric OOD).
  for(p in unique(phys_oob)) out[[p]] <- unique(c(out[[p]], "<non-physical value (out of applicability domain)>"))
  out
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
  if(abs(sum(fracs) - 1) > 0.001) {
    stop("Solvent fractions 'fracs' must sum to unity!")
  } else if(abs(sum(fracs) - 1) > 0) {
    # Normalize to exactly 1 for numerical stability
    fracs <- fracs / sum(fracs)
  }
  defunits <- c("mass", "mol")
  if(!any(defunits %in% units) | length(units)!=1) stop(paste0("Argument 'units' must be of length 1 and one of the following: ", paste0("'", defunits, "'", collapse = ", "))) else {

    if(!all(is.na(c(pres, temp))) & any(is.na(c(pres, temp)))) {
      stop("If reference pressure 'pres' is provided, temperature 'temp' must be given as well (and vice versa)!")
    } else if(!all(is.na(c(pres, temp)))) {
      # Only validate/expand a reference (pres, temp) when one is actually supplied. The default call
      # (pres = temp = NA, i.e. no reference condition) must NOT run these range checks: min(NA) is NA and
      # `if(NA)` errors ("missing value where TRUE/FALSE needed"), which crashed the default mode.
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
    #Extract dD (always present)
    dD_idx <- grep("dD", names(hsp_vals))
    #For dP: prefer dP_low if present, otherwise use dP
    dP_low_idx <- grep("dP_low", names(hsp_vals))
    dP_idx <- if(length(dP_low_idx) > 0) dP_low_idx else grep("^dP$", names(hsp_vals))
    #For dHB: prefer dHB_low if present, otherwise use dHB
    dHB_low_idx <- grep("dHB_low", names(hsp_vals))
    dHB_idx <- if(length(dHB_low_idx) > 0) dHB_low_idx else grep("^dHB$", names(hsp_vals))
    hsp_vals <- hsp_vals[c(dD_idx, dP_idx, dHB_idx)]
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
