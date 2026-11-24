# Curated set of common, readily-available liquid-liquid-extraction / process solvents for automatic
# solvent screening (the COSMO-SAC analogue of an AMS-style solvent database), grounded in the GSK
# solvent sustainability guide (Alder et al., Green Chem. 2016, 18, 3879) and restricted to solvents
# one would realistically use. Each entry carries a sustainability band and hazard/application flags so
# the screening can gauge risk and warn about unsuitable choices.
#
# Columns:
#   name, smiles, class
#   gsk   1-4 sustainability band, GSK-guide style: 1 Recommended, 2 Some issues, 3 Hazardous,
#         4 Highly Hazardous (higher = worse). Directional, from the GSK guide + common references.
#   ich   ICH Q3C residual-solvent class: 1 = avoid, 2 = limit, 3 = low toxicity (NA if not classified).
#   food  TRUE if generally food/GRAS-acceptable (e.g. water, ethanol, acetone, ethyl acetate, limonene).
#   hazard short free-text flag for the most safety-relevant concern (or "" if none notable).

#' Curated common extraction / process solvents with sustainability data
#'
#' Returns a data frame of common, readily-available organic process solvents used as the default
#' candidate set for automatic liquid-liquid-extraction solvent screening (\code{\link{csac_lle}}).
#' The list and sustainability bands are grounded in GSK's solvent sustainability guide
#' (Alder et al., Green Chem. 2016) and augmented with ICH Q3C residual-solvent classes and
#' food/GRAS acceptability, so a screen can flag hazardous or application-unsuitable choices.
#'
#' @return data frame: \code{name}, \code{smiles}, \code{class}, \code{gsk} (1 Recommended ... 4 Highly
#'   Hazardous), \code{ich} (Q3C class 1/2/3), \code{ppm} (ICH Q3C residual-solvent concentration limit,
#'   ppm; Class 3 = 5000, NA if unclassified), \code{food} (logical), \code{hazard} (text).
#' @references Alder, C. M. et al. Updating and further expanding GSK's solvent sustainability guide.
#'   Green Chem. 2016, 18, 3879. ICH Q3C(R8) Residual Solvents (concentration limits).
#' @seealso \code{\link{csac_lle}}, \code{\link{lle_solvent_band}}.
#' @export
lle_solvents <- function() {
  r <- function(name, smiles, class, gsk, ich, food, hazard = "")
    data.frame(name = name, smiles = smiles, class = class, gsk = gsk, ich = ich,
               food = food, hazard = hazard, stringsAsFactors = FALSE)
  df <- rbind(
    # --- water & alcohols (mostly recommended / food-grade) ---
    r("Water", "O", "aqueous", 1L, NA, TRUE, ""),
    r("Methanol", "CO", "alcohol", 2L, 2L, FALSE, "toxic if ingested"),
    r("Ethanol", "CCO", "alcohol", 1L, 3L, TRUE, ""),
    r("1-Propanol", "CCCO", "alcohol", 1L, 3L, TRUE, ""),
    r("Isopropanol", "CC(C)O", "alcohol", 1L, 3L, TRUE, ""),
    r("1-Butanol", "CCCCO", "alcohol", 1L, 3L, TRUE, ""),
    r("2-Butanol", "CCC(C)O", "alcohol", 1L, 3L, FALSE, ""),
    r("Isobutanol", "CC(C)CO", "alcohol", 1L, 3L, FALSE, ""),
    r("tert-Butanol", "CC(C)(C)O", "alcohol", 2L, 3L, FALSE, ""),
    r("1-Pentanol", "CCCCCO", "alcohol", 1L, NA, FALSE, ""),
    r("1-Octanol", "CCCCCCCCO", "alcohol", 1L, NA, TRUE, ""),
    r("Benzyl alcohol", "OCc1ccccc1", "alcohol", 1L, NA, TRUE, ""),
    r("Ethylene glycol", "OCCO", "alcohol", 2L, 2L, FALSE, "toxic if ingested"),
    r("Glycerol", "OCC(O)CO", "green", 1L, NA, TRUE, ""),
    # --- esters / carbonates (recommended, several food-grade) ---
    r("Methyl acetate", "COC(C)=O", "ester", 1L, 3L, FALSE, "flammable"),
    r("Ethyl acetate", "CCOC(C)=O", "ester", 1L, 3L, TRUE, "flammable"),
    r("Isopropyl acetate", "CC(C)OC(C)=O", "ester", 1L, 3L, TRUE, "flammable"),
    r("n-Butyl acetate", "CCCCOC(C)=O", "ester", 1L, 3L, TRUE, "flammable"),
    r("Dimethyl carbonate", "COC(=O)OC", "ester", 1L, NA, FALSE, "flammable"),
    r("Propylene carbonate", "CC1COC(=O)O1", "ester", 1L, NA, FALSE, ""),
    r("Ethyl lactate", "CCOC(=O)C(C)O", "green", 1L, NA, TRUE, ""),
    # --- ketones ---
    r("Acetone", "CC(C)=O", "ketone", 1L, 3L, TRUE, "highly flammable"),
    r("Methyl ethyl ketone", "CCC(C)=O", "ketone", 2L, 3L, FALSE, "highly flammable"),
    r("Methyl isobutyl ketone", "CC(=O)CC(C)C", "ketone", 2L, 3L, FALSE, "flammable"),
    r("Cyclohexanone", "O=C1CCCCC1", "ketone", 2L, 3L, FALSE, ""),
    # --- ethers (THF/dioxane/ethers flagged) ---
    r("MTBE", "COC(C)(C)C", "ether", 2L, 3L, FALSE, "highly flammable"),
    r("2-Methyltetrahydrofuran", "CC1CCCO1", "ether", 2L, NA, FALSE, "peroxide-forming"),
    r("Tetrahydrofuran", "C1CCOC1", "ether", 3L, 2L, FALSE, "peroxide-forming"),
    r("Cyclopentyl methyl ether", "COC1CCCC1", "ether", 2L, NA, FALSE, "flammable"),
    r("Diethyl ether", "CCOCC", "ether", 3L, 3L, FALSE, "extremely flammable; peroxide-forming"),
    r("Diisopropyl ether", "CC(C)OC(C)C", "ether", 3L, NA, FALSE, "strong peroxide-former"),
    r("1,4-Dioxane", "C1COCCO1", "ether", 3L, 2L, FALSE, "suspected carcinogen; peroxide-forming"),
    r("Anisole", "COc1ccccc1", "aromatic", 1L, 3L, FALSE, ""),
    # --- alkanes / cycloalkanes ---
    r("n-Pentane", "CCCCC", "alkane", 2L, 3L, FALSE, "highly flammable"),
    r("n-Hexane", "CCCCCC", "alkane", 3L, 2L, FALSE, "neurotoxic (peripheral neuropathy)"),
    r("n-Heptane", "CCCCCCC", "alkane", 2L, 3L, FALSE, "flammable"),
    r("n-Octane", "CCCCCCCC", "alkane", 2L, NA, FALSE, "flammable"),
    r("Isooctane", "CC(C)CC(C)(C)C", "alkane", 2L, NA, FALSE, "flammable"),
    r("Cyclohexane", "C1CCCCC1", "alkane", 2L, 2L, FALSE, "flammable"),
    r("Methylcyclohexane", "CC1CCCCC1", "alkane", 2L, 2L, FALSE, "flammable"),
    # --- aromatics ---
    r("Toluene", "Cc1ccccc1", "aromatic", 2L, 2L, FALSE, "suspected reproductive toxicant"),
    r("o-Xylene", "Cc1ccccc1C", "aromatic", 2L, 2L, FALSE, ""),
    r("p-Xylene", "Cc1ccc(C)cc1", "aromatic", 2L, 2L, FALSE, ""),
    r("Ethylbenzene", "CCc1ccccc1", "aromatic", 3L, 2L, FALSE, "possible carcinogen"),
    r("Chlorobenzene", "Clc1ccccc1", "aromatic", 3L, 2L, FALSE, ""),
    r("Benzene", "c1ccccc1", "aromatic", 4L, 1L, FALSE, "known human carcinogen \u2014 avoid"),
    # --- chlorinated (mostly hazardous) ---
    r("Dichloromethane", "ClCCl", "chlorinated", 3L, 2L, FALSE, "suspected carcinogen"),
    r("Chloroform", "ClC(Cl)Cl", "chlorinated", 3L, 2L, FALSE, "suspected carcinogen"),
    r("1,2-Dichloroethane", "ClCCCl", "chlorinated", 4L, 1L, FALSE, "carcinogen \u2014 avoid"),
    r("Carbon tetrachloride", "ClC(Cl)(Cl)Cl", "chlorinated", 4L, 1L, FALSE, "carcinogen, ozone-depleting \u2014 avoid"),
    # --- nitriles / nitro ---
    r("Acetonitrile", "CC#N", "nitrile", 3L, 2L, FALSE, "toxic; supply-constrained"),
    r("Nitromethane", "C[N+](=O)[O-]", "nitro", 3L, NA, FALSE, "explosive risk"),
    # --- dipolar aprotic (amides / sulfoxides) ---
    r("DMF", "CN(C)C=O", "amide", 3L, 2L, FALSE, "reprotoxic \u2014 restricted (REACH)"),
    r("N,N-Dimethylacetamide", "CN(C)C(C)=O", "amide", 3L, 2L, FALSE, "reprotoxic \u2014 restricted"),
    r("NMP", "CN1CCCC1=O", "amide", 3L, 2L, FALSE, "reprotoxic \u2014 restricted"),
    r("DMSO", "CS(C)=O", "sulfoxide", 1L, NA, FALSE, "skin penetrant (carries solutes)"),
    r("Sulfolane", "O=S1(=O)CCCC1", "sulfoxide", 2L, NA, FALSE, ""),
    # --- amines ---
    r("Triethylamine", "CCN(CC)CC", "amine", 3L, 3L, FALSE, "corrosive; flammable"),
    r("Pyridine", "c1ccncc1", "amine", 3L, 2L, FALSE, "toxic; malodorous"),
    r("Morpholine", "C1COCCN1", "amine", 3L, NA, FALSE, "corrosive"),
    # --- acids / misc ---
    r("Acetic acid", "CC(O)=O", "acid", 1L, 3L, TRUE, "corrosive"),
    r("Formic acid", "OC=O", "acid", 2L, 3L, TRUE, "corrosive"),
    r("Carbon disulfide", "S=C=S", "misc", 4L, NA, FALSE, "extremely flammable; neurotoxic \u2014 avoid"),
    # --- bio-based / green ---
    r("D-Limonene", "CC(=C)C1CCC(C)=CC1", "green", 1L, NA, TRUE, "skin sensitiser"),
    r("p-Cymene", "Cc1ccc(C(C)C)cc1", "green", 1L, NA, TRUE, ""),
    r("gamma-Valerolactone", "CC1CCC(=O)O1", "green", 1L, NA, FALSE, "")
  )
  # ICH Q3C(R8) residual-solvent concentration limits (ppm). Class 1 & 2 are compound-specific
  # (Option-1 limits); Class 3 solvents share a generic 5000 ppm (0.5%) limit; NA = not classified.
  q3c <- c(
    "Benzene" = 2, "1,2-Dichloroethane" = 5, "Carbon tetrachloride" = 4,
    "Methanol" = 3000, "Acetonitrile" = 410, "Chlorobenzene" = 360, "Chloroform" = 60,
    "Cyclohexane" = 3880, "Dichloromethane" = 600, "N,N-Dimethylacetamide" = 1090,
    "DMF" = 880, "1,4-Dioxane" = 380, "Ethylbenzene" = 3690, "Ethylene glycol" = 620,
    "n-Hexane" = 290, "Methylcyclohexane" = 1180, "NMP" = 530, "Nitromethane" = 50,
    "Pyridine" = 200, "Sulfolane" = 160, "Tetrahydrofuran" = 720, "Toluene" = 890,
    "o-Xylene" = 2170, "p-Xylene" = 2170)
  df$ppm <- unname(q3c[df$name])
  df$ppm[is.na(df$ppm) & !is.na(df$ich) & df$ich == 3L] <- 5000  # Class-3 generic limit
  df
}

#' Human-readable label / colour / short note for a GSK sustainability band
#'
#' @param band integer band 1-4 (see \code{\link{lle_solvents}}).
#' @return list with \code{label}, \code{colour} (hex), \code{short}.
#' @seealso \code{\link{lle_solvents}}.
#' @export
lle_solvent_band <- function(band) {
  b <- c("Recommended", "Some issues", "Hazardous", "Highly Hazardous")
  col <- c("#2e7d32", "#f9a825", "#e65100", "#b71c1c")
  i <- pmin(pmax(as.integer(band), 1L), 4L)
  list(label = b[i], colour = col[i], short = b[i])
}
