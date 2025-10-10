#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Generate Full Factorial Design (FFD)
#Available for 2-3 levels at 2-5 factors
#3-level designs necessarily include 3 center points
#Hence, 'add_cpts' only adds additional center points
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Generate a Full Factorial experimental design
#'
#' @description Generates a Full Factorial design with 2-3 levels and 2-5 factors.
#' Three-level designs include 3 center points by default.
#'
#' @param levels The number of factor levels to use (between 2-3).
#' @param factors The number of factors to use (between 2-5). The value must equal to the length of \code{fnames} and \code{flims}.
#' @param cpts The number of \strong{additional} (to the default 3) center points to add to the design (0 by default).
#' @param fnames A \code{character} vector of factor names of length 2-5.
#' @param flims A \code{list} of numeric vectors containing the lower and upper factor level limits \strong{in the same order as given in \code{fnames}}.
#' @param randomize A \code{logical} indicating whether or not the run order of the design is randomized (\code{TRUE} by default).
#' @param export Either \code{"none"} (default) or a path to export the generated design .TAB file to.
#'
#' @return A \code{list} containing a \code{data.frame} with the generated design as well as a short description (\code{$description}).
#' @export
#'
#' @examples
#' #Generate experimental design with 3 levels, 3 factors, and 3 additional center points (6 in total)
#' doe_res <- doe_ffd(levels = 3,
#'                    factors = 3,
#'                    cpts = 3,
#'                    fnames = c("Pressure", "Temperature", "Flow"),
#'                    flims = list(c(100, 300), c(35, 65), c(2, 4)))
#'
#' @seealso \code{\link{doe_export}}, \code{\link{doe_frfd}}, \code{\link{doe_ccd}}, \code{\link{doe_bbd}}, \code{\link{doe_tm}}
doe_ffd <- function(levels = 2, factors = 3, cpts = 0, fnames, flims, randomize = TRUE, export = "none") {

  #Preliminary checks
  if(length(fnames)!=length(flims) | length(fnames)!=factors | length(flims)!=factors) stop("Length of factor/limits, and/or the number of factors specified for the design do not match!")
  if(!randomize) cat("\nRun order randomization is turned off. It is highly recommended that DOE runs are randomized!")

  if(levels<2 | levels>3) stop("Only 2- and 3-level FFDs are available at present!")
  if(((factors<2 | factors>3) & levels==3) | (factors>5 & levels==2)) stop("Only FFDs for 2 to 5 (or 3 for 3-level FFD) factors are available at present!")
  if(levels==3) cat("\nNote that 3-level FFD designs must contain 3 center points by default! Only ADDITIONAL center points are provided in 'cpts'.")

  #Load FFD data
  loadlev <- if(levels %in% 2:3) levels else if(levels>3) 2
  doe_df <- doe_base[[grep(paste0("FD_",loadlev), names(doe_base))]]

  #Modify data according to 'levels' and 'factors'
  colsub <- c("Standard_Order", LETTERS[1:factors])
  rnum <- 1:(levels^factors)
  doe_df <- doe_df[doe_df[,"Standard_Order"] %in% rnum, colsub]

  #Add coded center points
  if(cpts>0) doe_df <- add_cpts(doe_df, cpts)

  #Add actual variable values (uncoded)
  doe_df <- uncode_levels(doe_df, "FFD", levels, factors, flims, fnames)

  #Randomize runs
  if(randomize) doe_df <- doe_df[sample(1:nrow(doe_df)),]

  #Put results into list and optionally export
  doe <- list(doe_df)
  names(doe) <- paste0("FFD_", levels,"sup", factors, "_ResV")

  #Add function call
  doe[["call"]] <- match.call()

  #Export
  export_name <- paste0("A ", levels, "^", factors, " FFD (FULL FACTORIAL) DESIGN OF RESOLUTION V WITH ", nrow(doe_df), " RUNS AND ", cpts, " ADDED CENTER POINTS")
  if(export!="none" & dir.exists(export)) doe_export(doe, export_name, expath = export)

  return(list(doe = doe, description = export_name))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION 3H: General Fractional Factorial Design (FrFD)
#Only 2-level designs available for 2-5 factors
#p-value is required and cannot exceed 'factors' (i.e. k)
#p-value must be 1 for 2-4 factors, and 1-2 for 5 factors
#If an 'aliasing' vector is provided, its' length must equal the p-value
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Generate a Fractional Factorial experimental design
#'
#' @description Generates a 2-level Fractional Factorial experimental design.
#'
#' @param factors The number of factors to use (between 2-5). The value must equal to the length of \code{fnames} and \code{flims}.
#' @param p A numeric value describing the fraction of the Full Factorial design used. Must be 1 when \code{factors} are between 2-4, and 1-2 for 5 \code{factors}.
#' @param cpts The number of center points to add to the design (0 by default).
#' @param aliasing Either \code{"default"} or a \code{character} vector containing the aliasing patterns to use.
#' @param fnames A \code{character} vector of factor names of length 2-5.
#' @param flims A \code{list} of numeric vectors containing the lower and upper factor level limits \strong{in the same order as given in \code{fnames}}.
#' @param randomize A \code{logical} indicating whether or not the run order of the design is randomized (\code{TRUE} by default).
#' @param export Either \code{"none"} (default) or a path to export the generated design .TAB file to.
#'
#' @return A \code{list} containing a \code{data.frame} with the generated design as well as a short description (\code{$description}).
#' @export
#'
#' @examples
#' #Fractional Factorial (var. 3, 2^5-2)
#' doe_res <- doe_frfd(factors = 5,
#'                     p = 2,
#'                     cpts = 0,
#'                     aliasing = "default",
#'                     fnames = c("Pressure", "Temperature", "Flow", "Time", "PartSize"),
#'                     flims = list(c(100, 300), c(35, 65), c(2, 4), c(90, 240), c(0.2, 1.0)))
#'
#' @seealso \code{\link{doe_export}}, \code{\link{doe_ffd}}, \code{\link{doe_ccd}}, \code{\link{doe_bbd}}, \code{\link{doe_tm}}
#'
#' @importFrom utils as.roman
#'
doe_frfd <- function(factors = 3, p = 1, cpts = 0, aliasing = "default", fnames, flims, randomize = TRUE, export = "none") {

  #Preliminary checks
  if(length(fnames)!=length(flims) | length(fnames)!=factors | length(flims)!=factors) stop("Length of factor/limits, and/or the number of factors specified for the design do not match!")
  if(!randomize) cat("\nRun order randomization is turned off. It is highly recommended that DOE runs are randomized!")

  levels <- 2
  if(levels>2) stop("Only 2-level FrFDs are currently available!")

  if(factors<2 | factors>5) stop("Only FrFDs for 2 to 5 factors are available at present!")
  if(((factors %in% 2:4) & p!=1) | (factors==5 & !p %in% 1:2)) stop("'p' must be 1 for 'factors' between 2-4, or between 1-2 for 5 factors!")
  if(p!=length(aliasing) & aliasing!="default") stop("The vector specified for aliased factor calculations ('aliasing') has to be of the same length as 'p'!")

  #Load FFD data
  loadlev <- if(levels %in% 2:3) levels else if(levels>3) 2
  doe_df <- doe_base[[grep(paste0("FD_",loadlev), names(doe_base))]]

  #Modify data according to 'levels' and 'factors'
  colsub <- c("Standard_Order", LETTERS[1:(factors-p)])
  rnum <- 1:(levels^(factors-p))
  doe_df <- doe_df[doe_df[,"Standard_Order"] %in% rnum, colsub]

  #Determine aliasing
  if(aliasing=="default") aliasing <- if(factors %in% 3:5 & p==1) paste0(LETTERS[1:(factors-1)], collapse = "") else if(factors==5 & p==2) c("AB","AC")

  #Get defining relation of a design and add associated column to 'doe_df'
  drel <- defrel(doe_df, factors, aliasing)
  doe_df <- drel[["data"]]
  def_rel <- drel[["def_rel"]]

  #Determine design resolution (length of shortest word)
  doe_res <- as.roman(min(sapply(def_rel, nchar)))

  #Add coded center points
  if(cpts>0) doe_df <- add_cpts(doe_df, cpts)

  #Add actual variable values (uncoded)
  doe_df <- uncode_levels(doe_df, "FrFD", levels, factors, flims, fnames)

  #Get confounding pattern
  alias_df <- confound(doe_df, def_rel)

  #Randomize runs
  if(randomize) doe_df <- doe_df[sample(1:nrow(doe_df)),]

  #Put results into list and optionally export
  doe <- list(doe_df, alias_df)
  names(doe) <- c(paste0("FrFD_2sup", factors, "-", p, "_Res_", doe_res), "CONFOUNDING PATTERN")

  #Add function call
  doe[["call"]] <- match.call()

  #Export
  export_name <- paste0("A ", levels, "^", factors, "-", p, " FrFD (FRACTIONAL FACTORIAL) SCREENING DESIGN OF RESOLUTION ", doe_res, " WITH ", nrow(doe_df), " RUNS AND ", cpts, " ADDED CENTER POINTS")
  if(export!="none" & dir.exists(export)) doe_export(doe, export_name, expath = export)

  return(list(doe = doe, description = export_name))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION 3I: Generate Central Composite Designs (CCC, CCF)
#'CCC' and 'CCF' designs require 5 and 3 factor levels, respectively
#Designs can have 2-4 factors
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Generate a Central Composite experimental design
#'
#' @description Generates either a Central Composite Circumscribed (CCC) or Central Composite Face-Centered (CCF) experimental design.
#' Currently available for 2-4 factors.
#'
#' @param design Type of design to generate. One of \code{"CCC"} (default) or \code{"CCF"}.
#' @param levels The number of factor levels to use (5 and 3 when \code{design} is \code{"CCC"} and \code{"CCF"}, respectively).
#' @param factors The number of factors to use (between 2-4). The value must equal to the length of \code{fnames} and \code{flims}.
#' @param cpts The number of center points to add to the design (0 by default).
#' @param fnames A \code{character} vector of factor names of length 2-4.
#' @param flims A \code{list} of numeric vectors containing the lower and upper factor level limits \strong{in the same order as given in \code{fnames}}.
#' @param randomize A \code{logical} indicating whether or not the run order of the design is randomized (\code{TRUE} by default).
#' @param export Either \code{"none"} (default) or a path to export the generated design .TAB file to.
#'
#' @return A \code{list} containing a \code{data.frame} with the generated design as well as a short description (\code{$description}).
#' @export
#'
#' @examples
#' #Central Composite Circumscribed for 4 factors (standard CCD)
#' doe_res <- doe_ccd(design = "CCC",
#' levels = 5,
#' factors = 3,
#' cpts = 6,
#' fnames = c("Pressure", "Temperature", "Co-Solvent"),
#' flims = list("hard"=c(100, 345), "hard"=c(35, 75), c(2, 8)))
#'
#' @seealso \code{\link{doe_export}}, \code{\link{doe_frfd}}, \code{\link{doe_ffd}}, \code{\link{doe_bbd}}, \code{\link{doe_tm}}
doe_ccd <- function(design = "CCC", levels = 3, factors = 3, cpts = 0, fnames, flims, randomize = TRUE, export = "none") {

  #Preliminary checks
  if(length(fnames)!=length(flims) | length(fnames)!=factors | length(flims)!=factors) stop("Length of factor/limits, and/or the number of factors specified for the design do not match!")
  if(!randomize) cat("\nRun order randomization is turned off. It is highly recommended that DOE runs are randomized!")

  if(design=="CCC" & levels!=5) {
    cat("\nCentral Composite Circumscribed (CCC) designs require 5 factor levels! Correcting...")
    levels <- 5
  } else if(design=="CCF" & levels!=3) {
    cat("\nCentral Composite Face-Centered (CCF) designs require 3 factor levels! Correcting...")
    levels <- 3
  }
  if(factors<2 | factors>4)  stop("Central Composite Designs (CCC and CCF) are available for 2 to 4 factors only!")
  if(cpts!=4 & factors==2) cat("\nNote that 4 center points are recommended for Central Composite designs with 2 factors! Currently set to ", cpts, ".", sep = "")
  if(cpts!=6 & (factors %in% 3:4)) cat("\nNote that 6 center points are recommended for Central Composite designs with 3-4 factors! Currently set to ", cpts, ".", sep = "")

  #Load FFD data
  loadlev <- if(levels %in% 2:3) levels else if(levels>3) 2
  doe_df <- doe_base[[grep(paste0("FD_",loadlev), names(doe_base))]]

  #Modify data according to the factor number (this generates the full factorial portion of the design)
  colsub <- c("Standard_Order", LETTERS[1:factors])
  rnum <- 1:(2^factors)
  doe_df <- doe_df[doe_df[,"Standard_Order"] %in% rnum, colsub]

  #Add coded star points
  doe_df <- add_stars(doe_df, factors, design)

  #Add coded center points
  if(cpts>0) doe_df <- add_cpts(doe_df, cpts)

  #Add actual variable values (uncoded)
  doe_df <- uncode_levels(doe_df, design, levels, factors, flims, fnames)

  #Randomize runs
  if(randomize) doe_df <- doe_df[sample(1:nrow(doe_df)),]

  #Put results into list and optionally export
  doe <- list(doe_df)
  names(doe) <- paste0(design,"_", levels,"sup", factors, "_with_", 2*factors, "_starpoints_and_", cpts, "_centerpoints")

  #Add function call
  doe[["call"]] <- match.call()

  #Export
  export_name <- paste0("A ", levels, "^", factors, " ", design, " (CENTRAL COMPOSITE) OPTIMIZATION DESIGN WITH ", 2*factors, " STAR POINTS AND ", cpts, " CENTER POINTS FOR A TOTAL OF ", nrow(doe_df), " RUNS")
  if(export!="none" & dir.exists(export)) doe_export(doe, export_name, expath = export)

  return(list(doe = doe, description = export_name))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION 3J: Generate Box-Behnken Design (BBD)
#Necessarily required 3 factor levels
#Available for 3-4 factors at present
#BBD designs necessarily include 3 center points
#Hence, 'add_cpts' only adds additional center points
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Generate a Box-Behnken experimental design
#'
#' @description Generates a Box-Behnken experimental design for 3-4 factors and 3 factor levels.
#'
#' @param factors The number of factors to use (between 3-4). The value must equal to the length of \code{fnames} and \code{flims}.
#' @param cpts The number of \strong{additional} center points to add to the design (0 by default). BBD designs always have 3 center points by default.
#' @param fnames A \code{character} vector of factor names of length 3-4.
#' @param flims A \code{list} of numeric vectors containing the lower and upper factor level limits \strong{in the same order as given in \code{fnames}}.
#' @param randomize A \code{logical} indicating whether or not the run order of the design is randomized (\code{TRUE} by default).
#' @param export Either \code{"none"} (default) or a path to export the generated design .TAB file to.
#'
#' @return A \code{list} containing a \code{data.frame} with the generated design as well as a short description (\code{$description}).
#' @export
#'
#' @examples
#' #Box-Behnken Design for 3 factors with 2 additional center points
#' doe_res <- doe_bbd(factors = 3,
#'                    cpts = 2,
#'                    fnames = c("Pressure", "Temperature", "Flow"),
#'                    flims = list(c(100, 300), c(35, 65), c(2, 4)))
#'
#' @seealso \code{\link{doe_export}}, \code{\link{doe_frfd}}, \code{\link{doe_ffd}}, \code{\link{doe_ccd}}, \code{\link{doe_tm}}
doe_bbd <- function(factors = 3, cpts = 0, fnames, flims, randomize = TRUE, export = "none") {

  #Preliminary checks
  if(length(fnames)!=length(flims) | length(fnames)!=factors | length(flims)!=factors) stop("Length of factor/limits, and/or the number of factors specified for the design do not match!")
  if(!randomize) cat("\nRun order randomization is turned off. It is highly recommended that DOE runs are randomized!")

  levels <- 3
  if(levels>3) stop("BBDs only available for 3 factor levels!")

  if(factors<3 | factors>4) stop("Only BBDs for 3 or 4 factors are available at present!")
  cat("\nNote that BBD designs must contain 3 center points by default! Only ADDITIONAL center points are provided in 'cpts'.")

  #Load data
  doe_df <- doe_base[[paste0("BBD_3_", factors)]]

  #Add coded center points
  if(cpts>0) doe_df <- add_cpts(doe_df, cpts)

  #Add actual variable values (uncoded)
  doe_df <- uncode_levels(doe_df, "BBD", levels, factors, flims, fnames)

  #Randomize runs
  if(randomize) doe_df <- doe_df[sample(1:nrow(doe_df)),]

  #Put results into list and optionally export
  doe <- list(doe_df)
  names(doe) <- paste0("BBD_3sup", factors)

  #Add function call
  doe[["call"]] <- match.call()

  #Export
  export_name <- paste0("A 3^", factors, " BBD (BOX-BEHNKEN) OPTIMIZATION DESIGN WITH ", nrow(doe_df), " RUNS")
  if(export!="none" & dir.exists(export)) doe_export(doe, export_name, expath = export)

  return(list(doe = doe, description = export_name))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Generate Taguchi Design (TM)
#Additional center points ('add_cpts') are NOT supported
#2-4 factor levels for 3-5 factors
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Generate a Taguchi experimental design
#'
#' @description Generates a Taguchi Method experimental design with 2-4 factor levels and 3-5 factors.
#'
#' @param levels The number of factor levels to use (between 2-4).
#' @param factors The number of factors to use (between 3-5). The value must equal to the length of \code{fnames} and \code{flims}.
#' @param fnames A \code{character} vector of factor names of length 3-5.
#' @param flims A \code{list} of numeric vectors containing the lower and upper factor level limits \strong{in the same order as given in \code{fnames}}.
#' @param randomize A \code{logical} indicating whether or not the run order of the design is randomized (\code{TRUE} by default).
#' @param export Either \code{"none"} (default) or a path to export the generated design .TAB file to.
#'
#' @return A \code{list} containing a \code{data.frame} with the generated design as well as a short description (\code{$description}).
#' @export
#'
#' @examples
#' #Taguchi Design for 3 levels and 3 factors
#' doe_res <- doe_tm(levels = 3,
#'                   factors =3,
#'                   fnames = c("Pressure", "Temperature", "Flow"),
#'                   flims = list(c(100, 300), c(35, 65), c(2, 4)))
#'
#' @seealso \code{\link{doe_export}}, \code{\link{doe_frfd}}, \code{\link{doe_ffd}}, \code{\link{doe_ccd}}, \code{\link{doe_bbd}}
doe_tm <- function(levels = 2, factors = 3, fnames, flims, randomize = TRUE, export = "none") {

  #Preliminary checks
  if(length(fnames)!=length(flims) | length(fnames)!=factors | length(flims)!=factors) stop("Length of factor/limits, and/or the number of factors specified for the design do not match!")
  if(!randomize) cat("\nRun order randomization is turned off. It is highly recommended that DOE runs are randomized!")

  if(!all(is.numeric(c(levels,factors))) | length(c(levels,factors))!=2) stop("Arguments 'factors' and 'levels' must both be numeric values!")
  if(levels<2 | levels>4) stop("For Taguchi designs, 2-4 factor 'levels' are currently supported!")
  if(factors<3 | factors>5) stop("For Taguchi designs, 3-5 'factors' are currently supported!")

  #Load data
  doe_df <- doe_base[[grep(paste0("TM_", levels,"_.*", factors), names(doe_base))]][,c("Standard_Order", LETTERS[1:factors])]

  #Add actual variable values (uncoded)
  doe_df <- uncode_levels(doe_df, "TM", levels, factors, flims, fnames)

  #Randomize runs
  if(randomize) doe_df <- doe_df[sample(1:nrow(doe_df)),]

  #Put results into list and optionally export
  doe <- list(doe_df)
  names(doe) <- paste0("Taguchi_", levels, "sup", factors)

  #Add function call
  doe[["call"]] <- match.call()

  #Export
  export_name <- paste0("A ", levels, "^", factors, " TAGUCHI DESIGN WITH ", nrow(doe_df), " RUNS")
  if(export!="none" & dir.exists(export)) doe_export(doe, export_name, expath = export)

  return(list(doe = doe, description = export_name))
}
