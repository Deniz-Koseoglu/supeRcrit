#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# GC-EOS / GCA-EOS PARAMETER REGISTRY
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Group-contribution equation-of-state parameter sets, transcribed verbatim from
# the primary literature. Each set is a self-contained list (pure-group params,
# binary interaction + nonrandomness matrices, component critical data, and the
# molecular group decomposition) that the GC-EOS core consumes directly.
#
# Data layer ONLY: no thermodynamics here. The numerical engine (volume root,
# fugacity coefficients, flash) lives in the C++ core and its R wrappers.
#
# Transcription protocol: values are taken from the PDF embedded TEXT LAYER
# (poppler pdf_text, which yields exact digit strings) and the column/group
# alignment is verified against a high-resolution render of the same page. Visual
# transcription of the digits alone is NOT reliable for these tables and must not
# be used. Every set records its source so values can be re-checked.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FUNCTION: Retrieve the consolidated GC-EOS / GCA-EOS parameter set
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Retrieve the consolidated GC-EOS or GCA-EOS parameter set
#'
#' @description
#' Returns the consolidated group-contribution equation-of-state parameter set for
#' the requested \code{model}: \code{"gceos"} (the classic non-associating GC-EOS,
#' Skjold-Jorgensen / Fornari) or \code{"gcaeos"} (the associating GCA-EOS variant).
#' The returned list bundles everything the numerical core needs: pure-group energy
#' and surface parameters, the binary interaction (\eqn{k}) and nonrandomness
#' (\eqn{\alpha}) matrices, the high-molecular-weight overrides, the SMARTS patterns
#' used to decompose molecules into groups, and (for GCA-EOS) the association
#' parameters.
#'
#' These objects are the data layer of the GC-EOS module - validated numbers and
#' provenance only, no thermodynamics (the numerical engine consumes them).
#'
#' @details
#' \strong{Model parameters.} The attractive term uses, per group \eqn{i}: a
#' reference temperature \eqn{T_i^*} (K), a surface-area parameter \eqn{q_i}, and the
#' pure-group attractive energy \eqn{g_i^*} with temperature coefficients \eqn{g_i'},
#' \eqn{g_i''}:
#' \deqn{g_{ii} = g_i^*\,[\,1 + g_i'\,(T/T_i^* - 1) + g_i''\,\ln(T/T_i^*)\,].}
#' Unlike-pair energies follow \eqn{g_{ij} = k_{ij}\,(g_{ii} g_{jj})^{1/2}} with
#' \deqn{k_{ij} = k_{ij}^*\,[\,1 + k_{ij}'\,\ln(T/T_{ij}^*)\,],\qquad T_{ij}^* = (T_i^* + T_j^*)/2,}
#' and the density-dependent NRTL nonrandomness uses the (generally asymmetric)
#' \eqn{\alpha_{ij}}.
#'
#' \strong{Matrix conventions.} \code{kstar} and \code{kprime} are symmetric
#' (\eqn{k_{ij}^* = k_{ji}^*}); \code{alpha} is the full, possibly asymmetric matrix
#' (\eqn{\alpha_{ij}} in row \eqn{i}, column \eqn{j}; zero diagonal). \code{NA} marks
#' an interaction reported as "n.a." (not available) in the source.
#'
#' @param model Character scalar: \code{"gceos"} (default), \code{"gcaeos"},
#'   \code{"gceos-hmw"}, \code{"diaz2005"}, or \code{"ganan2011"}.
#'   \code{"gceos-hmw"} applies Fornari (2007) Table 4 re-adjusted CO2 /
#'   aromatic-paraffin / olefin interactions for \strong{high-molecular-weight}
#'   compounds (squalene, triacylglycerides, fatty-acid esters): use it for MW >~ 300
#'   (the original \code{"gceos"} badly under-predicts CO2 solubility in heavy
#'   compounds and is also the most accurate general set for terpene alcohols and
#'   esters), and \code{"gceos"} for light molecules. \code{"gcaeos"} is the
#'   associating set for water / free fatty acids / alcohols (the only set with the
#'   Wertheim association term; essential for FFA - association cuts the CO2-phase
#'   solubility error ~5x vs the non-associating sets).
#'   \code{"diaz2005"} and \code{"ganan2011"} are \strong{molecule-specific terpene
#'   sets} that also carry their own component critical data (\code{components}) and
#'   group decompositions (\code{decomp}): Diaz et al. (2005) for limonene/linalool
#'   (the most accurate set for citrus deterpenation), and Ganan & Brignole (2011)
#'   for the Ganan 2013 fractionation columns
#'   (ocimene/ocimenone/pinene/thujone/menthone/menthol/caryophyllene). For these
#'   two, build a component with \code{groups = ps$decomp[name, ]; dc =
#'   ps$components[name, "dc"]}.
#'
#' @return A named \code{list} with elements:
#' \describe{
#'   \item{\code{groups}}{\code{data.frame} of pure-group parameters
#'     (\code{Tstar}, \code{q}, \code{gstar}, \code{gprime}, \code{gpprime}).}
#'   \item{\code{kstar}, \code{kprime}}{Symmetric group-by-group binary interaction
#'     and temperature-coefficient matrices.}
#'   \item{\code{alpha}}{Full (asymmetric) nonrandomness matrix.}
#'   \item{\code{hmw_groups}, \code{hmw_binary}}{High-molecular-weight overrides.}
#'   \item{\code{group_index}}{Named integer vector mapping group name to index.}
#'   \item{\code{smarts}}{SMARTS patterns for molecular decomposition (or
#'     \code{NULL} until built).}
#'   \item{\code{meta}}{Provenance and notes.}
#' }
#' Errors if the requested set has not been built yet.
#' @export
#'
#' @examples
#' ps <- gceos_params("gceos")
#' head(ps$groups)
#' ps$kstar["CH3", "COH"]
gceos_params <- function(model = c("gceos", "gcaeos", "gceos-hmw", "gceos-fae", "diaz2005", "ganan2011")) {
  model <- match.arg(model)
  obj <- switch(model,
    gceos          = if (exists("gceos",  mode = "list")) gceos  else NULL,
    gcaeos         = if (exists("gcaeos", mode = "list")) gcaeos else NULL,
    `gceos-hmw`    = .gceos_hmw(),
    `gceos-fae`    = .gceos_paramset_fae(),
    diaz2005       = .gceos_paramset_diaz2005(),
    ganan2011      = .gceos_paramset_ganan2011()
  )
  if (is.null(obj))
    stop("The '", model, "' parameter set is not available in this build yet.", call. = FALSE)
  obj
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# INTERNAL: high-molecular-weight (HMW) GC-EOS parameter set
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Fornari (2007), Fluid Phase Equilibria 262:187-209, Table 4: GC-EOS dispersive
# parameters RE-ADJUSTED (Pereda et al. [9,10], Vazquez et al. [11]) to represent
# phase equilibria of CO2 with HIGH-molecular-weight paraffins / olefins /
# aromatics (squalene, triacylglycerides, tocopherol, heavy fatty compounds). The
# shipped 'gceos' table holds Fornari's ORIGINAL values (his Tables 1-3), which he
# shows (Fig. 3) badly UNDER-predict the bubble pressure / over-predict CO2
# solubility in heavy compounds. This variant applies the Table 4 overrides.
# Fornari ships both sets deliberately: use 'gceos-hmw' for MW >~ 300 (squalene,
# glycerides, tocopherol), and the original 'gceos' for light molecules (the
# re-adjusted CH=C/CO2 parameters were fit to squalene and must NOT be used for
# light olefins). Still current: post-2007 GCA-EOS work (Sanchez, Prieto, Ortega
# 2025) builds on this same dispersive base and adds association on top.
.gceos_hmw <- function() {
  base <- if (exists("gceos", mode = "list")) gceos else
    stop("The 'gceos' parameter set is not available in this build yet.", call. = FALSE)
  # revised CH=C group parameters (fitted to squalene + CO2 VLE, Vazquez 2007)
  base$groups["CH=C", "gstar"]  <- 421650
  base$groups["CH=C", "gprime"] <- -1.3756
  # revised binary interactions: kstar/kprime symmetric, alpha (i,j)/(j,i)
  set <- function(b, i, j, ks, kp, aij, aji) {
    b$kstar[i, j]  <- b$kstar[j, i]  <- ks
    b$kprime[i, j] <- b$kprime[j, i] <- kp
    b$alpha[i, j]  <- aij; b$alpha[j, i] <- aji; b
  }
  base <- set(base, "CO2", "CH3",   0.898,  0.000,   4.683,   4.683)   # [9]
  base <- set(base, "CO2", "CH2",   0.874,  0.000,   4.683,   4.683)   # [9]
  base <- set(base, "CO2", "CH=C",  0.882,  0.022, -14.247, -14.247)   # [10/11]
  base <- set(base, "H2",  "CH3",   0.845, -0.115,  -1.000,  -1.000)   # [10]
  base <- set(base, "H2",  "CH2",   0.845, -0.115,  -1.000,  -1.000)   # [10]
  base <- set(base, "AC",  "CH3",   0.854,  0.000,   0.000,   0.000)   # [9]
  base <- set(base, "AC",  "CH2",   0.854,  0.000,   0.000,   0.000)   # [9]
  base <- set(base, "AC",  "CH=CH", 1.100,  0.000,   0.000,   0.000)   # [9]
  base <- set(base, "ACH", "CH3",   0.937,  0.020,  -0.794,  -0.794)   # [9]
  base <- set(base, "ACH", "CH2",   1.136,  0.020,  -2.678,  -2.678)   # [9]
  base <- set(base, "ACH", "CH=CH", 1.100,  0.000,   0.000,   0.000)   # [9]
  base$meta <- list(set = "gceos-hmw",
    source = "Fornari (2007), Fluid Phase Equilibria 262:187-209, Table 4 (HMW re-adjusted CO2/aromatic-paraffin/olefin interactions; Pereda, Vazquez)",
    note = "GC-EOS dispersive parameters re-adjusted for high-MW compounds (squalene, triacylglycerides, tocopherol). For MW >~ 300; use 'gceos' for light molecules.")
  base
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# INTERNAL: fatty-acid ester (FAEE/FAME) + CO2 set, as used by Espinosa 2002
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Espinosa, Diaz & Brignole (2002), IECR 41:1516, model CO2 + fatty-acid methyl/
# ethyl esters with the ester-group params in their Tables 1-2 (already in the
# shipped 'gceos' table: CH2COO/CH3COO g* and the ester binaries) BUT take the
# CO2-paraffin/olefin dispersive interactions from the PLAPIQUI base set (ref 37,
# Espinosa et al. 2000 FPE 172:129 -> Gonzalez Prieto/Ortega 'gcaeos' table), NOT
# Fornari's (2007) consolidated values. Fornari's CO2-CH2/CH3 (0.814/0.892, no
# T-dependence, alpha 3.369) make the ester+CO2 mixture too immiscible -> the
# mixture-critical pressure comes out ~2-3 MPa too HIGH, worst at low T. Swapping
# in the PLAPIQUI CO2-CH2/CH3 (0.910/0.9185, k'=0.0469, alpha -21/-26) reproduces
# Espinosa Fig 2-4 across all isotherms (methyl myristate apices 10.1/13.1/15.9/
# 18.6 MPa @313-343K vs his 10.2/13.1/16.2/18.7). CO2-CH=CH stays 1.0 (= both sets).
.gceos_paramset_fae <- function() {
  base <- if (exists("gceos", mode = "list")) gceos else
    stop("The 'gceos' parameter set is not available in this build yet.", call. = FALSE)
  set <- function(b, i, j, ks, kp, aij, aji) {
    b$kstar[i, j]  <- b$kstar[j, i]  <- ks
    b$kprime[i, j] <- b$kprime[j, i] <- kp
    b$alpha[i, j]  <- aij; b$alpha[j, i] <- aji; b
  }
  # Espinosa, Bermudez, Foco & Fornari (2000) FPE 172:129, Table 2 (= ref 37 of Espinosa 2002),
  # the EXACT CO2-paraffin dispersive params Espinosa 2002 used (NOT Fornari 2007's gceos values,
  # NOT Ortega's later readjustment). kij'=0, alpha symmetric = 4.683.
  base <- set(base, "CO2", "CH3",   0.898, 0, 4.683, 4.683)   # Espinosa 2000 Table 2
  base <- set(base, "CO2", "CH2",   0.874, 0, 4.683, 4.683)   # Espinosa 2000 Table 2
  # CO2-CH=CH (olefin): this binary is in NONE of the GC-EOS sources - Skjold-Jorgensen 1988 has no
  # olefin group at all, Espinosa 2000 didn't add one, and Fornari 2007 left CO2-CH=CH "n.a.". So it
  # was never published; Espinosa 2002 must have used a default or fit it. We FIT it to the Bharath
  # EPA/DHA-ethyl-ester VLE -> 0.90 (k'=0, alpha=0). This is the one empirical binary in the set; all
  # others are literature: CO2-ester 1.115/0.094/-1.615, ester-CH3/CH2 0.869, ester-CH=CH 1.006/0/
  # -0.876 (Espinosa 2000 Table 2); CH=CH pure-group from Fornari 2007 (gceos base).
  # CO2-CH=CH = 0.948 (k'=0, alpha=0): the published GC-EOS CO2-olefin binary (PLAPIQUI binary table,
  # same source as the CH=CH group params; = the Diaz 2005 CO2-olefin value). This is the parameter
  # that was "n.a." in Fornari 2007 and which we had to placeholder - now sourced.
  base <- set(base, "CO2", "CH=CH", 0.948, 0, 0, 0)
  base$meta <- list(set = "gceos-fae",
    source = "Espinosa 2002 (IECR 41:1516) esters + Espinosa 2000 (FPE 172:129, ref 37) CO2-paraffin + CO2-CH=CH=0.92 fit to Bharath VLE (both branches)",
    note = paste("For CO2 + fatty-acid methyl/ethyl esters. Reproduces Espinosa 2002 Fig 2 (saturated)",
      "exactly. CO2-CH=CH=0.92 (unpublished; fit to balance liquid+vapor of EPA/DHA). dc per ester",
      "fitted to the binary VLE (Espinosa's method)."))
  base
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# INTERNAL: Diaz et al. (2005) limonene/linalool/CO2 set
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Source: Diaz, Espinosa & Brignole (2005), J. Supercrit. Fluids 35:49-61,
# Appendix A, Tables A.1-A.4. Verified against a 400-dpi render of page 10-11.
.gceos_paramset_diaz2005 <- function() {

  # Group order is fixed and shared by every matrix below.
  grp <- c("CH2", "C=CH", "CH=CH2", "CH3(B)", "CYCH2", "CYCH",
           "ACH", "C=CH2", "ACCH3", "COH", "CO2")
  ng <- length(grp)

  ## --- Table A.2: pure-group parameters ---
  groups <- data.frame(
    Tstar   = c(600,    600,    600,    600,    600,    600,    600,    600,    600,    512.6,  304.2),
    q       = c(0.54,   0.676,  1.176,  0.7891, 0.54,   0.228,  0.4,    0.988,  0.968,  0.7138, 1.261),
    gstar   = c(356080, 546780, 337980, 316910, 466550, 466550, 723210, 323440, 506290, 1207500, 531890),
    gprime  = c(-0.8755, -1.0966, -0.6764, -0.9274, -0.6062, -0.6062, -0.606, -0.6328, -0.8013, -0.6441, -0.578),
    gpprime = rep(0, ng),
    row.names = grp,
    stringsAsFactors = FALSE
  )

  ## --- Table A.3: binary interaction parameters (packed) ---
  # As printed: k*_ij above the diagonal, k'_ij (temperature dependence) below it,
  # row-major in the fixed group order above.
  packed <- matrix(c(
    #CH2    C=CH   CH=CH2 CH3(B) CYCH2  CYCH   ACH    C=CH2  ACCH3  COH    CO2
    1,     0.977, 0.977, 1,     1,     1,     1.041, 1,     0.975, 0.682, 0.874,  # CH2
    0,     1,     1.094, 0.977, 1,     1,     1,     1.094, 1.040, 0.773, 1,      # C=CH
    0,     0,     1,     0.977, 1,     1,     0.984, 1.094, 1.034, 1.04,  0.948,  # CH=CH2
    0,     0,     0,     1,     1,     1,     1.041, 1,     0.975, 0.715, 0.898,  # CH3(B)
    0,     0,     0,     0,     1,     1,     0.98,  1.040, 0.994, 0.719, 0.928,  # CYCH2
    0,     0,     0,     0,     0,     1,     0.98,  1.040, 0.994, 0.719, 0.928,  # CYCH
    0.094, 0,     0,     0.094, 0,     0,     1,     1.040, 1.007, 0.826, 1.060,  # ACH
    0,     0,     0,     0,     0,     0,     0,     1,     1.040, 0.769, 1.057,  # C=CH2
    0,     0,     0,     0,     0,     0,     0,     0,     1,     0.774, 0.938,  # ACCH3
    0,     0,     0,     0,     0,     0,     0,     0,     0,     1,     0.785,  # COH
    0,     0,     0,     0,     0.21,  0.21,  0,     0,     0,     0,     1       # CO2
  ), nrow = ng, byrow = TRUE, dimnames = list(grp, grp))

  # Unpack into symmetric k* (upper triangle + diagonal) and symmetric k' (lower).
  kstar  <- matrix(0, ng, ng, dimnames = list(grp, grp))
  kprime <- matrix(0, ng, ng, dimnames = list(grp, grp))
  diag(kstar) <- 1
  for (i in 1:ng) for (j in 1:ng) {
    if (j > i) { kstar[i, j]  <- kstar[j, i]  <- packed[i, j] }  # above diagonal -> k*
    if (j < i) { kprime[i, j] <- kprime[j, i] <- packed[i, j] }  # below diagonal -> k'
  }

  ## --- Table A.4: binary nonrandomness parameters (full, asymmetric) ---
  alpha <- matrix(c(
    #CH2     C=CH    CH=CH2 CH3(B)  CYCH2   CYCH    ACH     C=CH2   ACCH3   COH     CO2
    0,      0,      0,     0,      0,      0,      0.392,  0,      0,      1.471,  4.683,   # CH2
    0,      0,      0,     0,      0,      0,      0,      0,      0,      1.376,  0,       # C=CH
    0,      0,      0,     0,      0,      0,      0,      0,      0,      0,      0,       # CH=CH2
    0,      0,      0,     0,      0,      0,      0.392,  0,      0,      1.471,  4.683,   # CH3(B)
    0,      0,      0,     0,      0,      0,      0,      0,      0,      1.471,  0,       # CYCH2
    0,      0,      0,     0,      0,      0,      0,      0,      0,      1.471,  0,       # CYCH
    0.392,  0,      0,     0.392,  0,      0,      0,      0,      0,      4.091, -6.888,   # ACH
    0,      0,      0,     0,      0,      0,      0,      0,      0,      0.603,  0,       # C=CH2
    0,      0,      0,     0,      0,      0,      0,      0,      0,      0.784, -14.93,   # ACCH3
    10.22,  1.376,  0,     10.22,  10.22,  10.22,  20.74,  0.603,  20.74,  0,      0.22,   # COH
    4.683,  0,      0,     4.683,  0,      0,     -3.329,  0,     -3.94,  -1.18,   0       # CO2
  ), nrow = ng, byrow = TRUE, dimnames = list(grp, grp))

  ## --- Table A.1: component critical data and fitted critical diameter ---
  components <- data.frame(
    Tc = c(304.2, 660.0, 630.5),
    Pc = c(7.280, 2.750, 2.388),
    Tb = c(NA,    449.65, 471.15),
    MW = c(44.00, 136.2, 154.4),
    dc = c(3.1289, 5.6027, 6.1712),
    row.names = c("CO2", "limonene", "linalool"),
    stringsAsFactors = FALSE
  )

  ## --- Table A.2 (decomposition columns): nu_j^i, count of group j in molecule i ---
  decomp <- rbind(
    CO2      = c(CH2 = 0, `C=CH` = 0, `CH=CH2` = 0, `CH3(B)` = 0, CYCH2 = 0, CYCH = 0, ACH = 0, `C=CH2` = 0, ACCH3 = 0, COH = 0, CO2 = 1),
    limonene = c(0,        0,          0,            1,            3,         1,        1,       1,           1,         0,       0),
    linalool = c(2,        1,          1,            3,            0,         0,        0,       0,           0,         1,       0)
  )
  colnames(decomp) <- grp

  list(
    groups     = groups,
    kstar      = kstar,
    kprime     = kprime,
    alpha      = alpha,
    components = components,
    decomp     = decomp,
    meta = list(
      set    = "diaz2005",
      source = "Diaz, Espinosa & Brignole (2005), J. Supercrit. Fluids 35:49-61, Appendix A (Tables A.1-A.4)",
      note   = "GC-EOS (non-associating). Verified against 400-dpi page render. dc in cm/mol; Tstar,Tc,Tb in K; Pc in MPa."
    )
  )
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# INTERNAL: Ganan & Brignole (2011) terpene GC-EOS set
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The fitted terpene parameter set behind the Ganan & Brignole (2013) counter-
# current fractionation columns (Tagetes minuta, sage, peppermint). Transcribed
# from Ganan & Brignole (2011), J. Supercrit. Fluids 58:58-67, Tables 4 (critical
# data + dc), 5 (group decompositions), 6 (pure-group params), 7 (binary k*/k'),
# 8 (nonrandomness alpha). The ketone group CH2C=O carries the ocimenone-specific
# surface q = 0.88 (Table 6 footnote), the relevant value for the T. minuta system.
.gceos_paramset_ganan2011 <- function() {
  grp <- c("CH3","CH2","CH","CH3(B)","CYCH2","CYCH","CH=CH2","C=CH2","C=CH",
           "CH2C=O","CHOH","CO2")
  ng <- length(grp)

  ## --- Table 6: pure-group parameters ---
  groups <- data.frame(
    Tstar   = c(600,600,600,600,600,600,600,600,600,600,512.6,304.2),
    q       = c(0.848,0.540,0.228,0.789,0.540,0.228,1.176,0.988,0.676,0.88,0.908,1.261),
    gstar   = c(316910,356080,356080,316910,466550,466550,337980,323440,546780,888410,1207500,531890),
    gprime  = c(-0.9274,-0.8755,-0.8755,-0.9274,-0.6062,-0.6062,-0.6764,-0.6328,-1.0966,-0.7018,-0.6441,-0.578),
    gpprime = rep(0, ng),
    row.names = grp, stringsAsFactors = FALSE
  )

  ## --- Table 7: k* (above diagonal) / k' (below diagonal); n.a. -> default ---
  packed <- matrix(c(
    #CH3   CH2    CH    CH3B  CYCH2 CYCH  C=CH2a C=CH2 C=CH  KET   CHOH  CO2
    1,    1,    1,    1,    1,    1,    0.977,1,    0.977,0.834,0.715,0.898,  # CH3
    0,    1,    1,    1,    1,    1,    0.977,1,    0.977,0.834,1,    0.898,  # CH2
    0,    0,    1,    1,    1,    1,    1,    1,    0.977,0.834,0.682,0.898,  # CH
    0,    0,    0,    1,    1,    1,    1,    1,    0.977,0.834,1,    0.928,  # CH3(B)
    0,    0,    0,    0,    1,    1,    1,    1.04, 1,    0.870,0.722,0.928,  # CYCH2
    0,    0,    0,    0,    0,    1,    1,    1.04, 1,    0.870,0.722,0.928,  # CYCH
    0,    0,    0,    0,    0,    0,    1,    1,    1.094,1,    1,    0.948,  # CH=CH2
    0,    0,    0,    0,    0,    0,    0,    1,    1.094,1,    1,    1.057,  # C=CH2
    0,    0,    0,    0,    0,    0,    0,    0,    1,    0.975,1,    1,      # C=CH
    0.084,0.084,0.084,0.084,0.097,0.097,0,    0,    0,    1,    0.953,1.025,  # CH2C=O
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    1,    0.985,  # CHOH
    0,    0,    0,    0,    0.210,0.21, 0,    0,    0,    0.108,0,    1       # CO2
  ), nrow = ng, byrow = TRUE, dimnames = list(grp, grp))
  kstar <- kprime <- matrix(0, ng, ng, dimnames = list(grp, grp)); diag(kstar) <- 1
  for (i in 1:ng) for (j in 1:ng) {
    if (j > i) kstar[i,j]  <- kstar[j,i]  <- packed[i,j]
    if (j < i) kprime[i,j] <- kprime[j,i] <- packed[i,j]
  }

  ## --- Table 8: nonrandomness alpha (full asymmetric); n.a. -> 0 ---
  alpha <- matrix(c(
    #CH3   CH2   CH    CH3B  CYCH2 CYCH  CH=CH2 C=CH2 C=CH  KET   CHOH  CO2
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0.854,1.471,4.683,  # CH3
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0.854,0,    4.683,  # CH2
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0.854,1.471,4.683,  # CH
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0.854,0,    4.683,  # CH3(B)
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0.854,1.471,0,      # CYCH2
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0.854,1.417,0,      # CYCH
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,      # CH=CH2
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,      # C=CH2
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,      # C=CH
    5.146,5.146,5.146,5.146,5.146,5.146,0,    0,    0,    0,    0,    0.17,   # CH2C=O
    10.22,0,    10.22,0,    10.22,10.22,0,    0,    0,    0,    0,   -0.39,   # CHOH
    4.683,4.683,4.683,4.683,0,    0,    0,    0,    0,    0.17, 0.468,0       # CO2
  ), nrow = ng, byrow = TRUE, dimnames = list(grp, grp))

  ## --- Table 4: component critical data + fitted dc ---
  components <- data.frame(
    Tc = c(304.2, 643.3, 640.8, 644.0, 680.5, 686.6, 685.0, 746.3),
    Pc = c(72.8,  24.5,  26.0,  27.6,  28.8,  26.0,  20.0,  20.1),
    MW = c(44.01, 136.23,152.23,136.23,152.23,154.25,156.27,204.35),
    dc = c(3.1289,5.8985,5.3948,5.3167,5.4496,5.8165,5.9772,6.5205),
    row.names = c("CO2","ocimene","ocimenone","a-pinene","a-thujone",
                  "menthone","menthol","caryophyllene"),
    stringsAsFactors = FALSE
  )

  ## --- Table 5: group decompositions nu_j^i ---
  decomp <- matrix(0, nrow = nrow(components), ncol = ng,
                   dimnames = list(rownames(components), grp))
  set <- function(nm, ...) { v <- c(...); decomp[nm, names(v)] <<- v }
  set("CO2",           CO2=1)
  set("ocimene",       CH3=3, CH2=1, `CH=CH2`=1, `C=CH`=2)
  set("ocimenone",     CH3=3, `CH=CH2`=1, `C=CH`=1, `CH2C=O`=1)
  set("a-pinene",      CH3=1, `CH3(B)`=2, CYCH2=2, CYCH=2, `C=CH`=1)
  set("a-thujone",     CH3=3, CH=1, CYCH2=1, CYCH=2, `CH2C=O`=1)
  set("menthone",      CH3=3, CH=1, CYCH2=2, CYCH=2, `CH2C=O`=1)
  set("menthol",       CH3=3, CH=1, CYCH2=3, CYCH=2, CHOH=1)
  set("caryophyllene", CH3=1, `CH3(B)`=2, CYCH2=5, CYCH=2, `C=CH2`=1, `C=CH`=1)

  list(
    groups = groups, kstar = kstar, kprime = kprime, alpha = alpha,
    components = components, decomp = decomp,
    meta = list(set = "ganan2011",
      source = "Ganan & Brignole (2011), J. Supercrit. Fluids 58:58-67, Tables 4-8",
      note = "GC-EOS terpene set behind the Ganan 2013 columns. CH2C=O uses the ocimenone q=0.88. dc in cm/mol; Tstar,Tc in K; Pc in bar.")
  )
}
