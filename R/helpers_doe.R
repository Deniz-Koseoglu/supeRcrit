#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Add coded star-points (for CCD designs)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Add star points to an experimental design
#'
#' @description Part of the \strong{experimental design generation} workflow.
#'
#' @param input A \code{data.frame} containing the \strong{coded} input factors and their levels. Columns (factor names) must be named alphabetically.
#' @param factors The number of factors (\strong{between 2 and 4}).
#' @param design The type of Central Composite experimental design.
#' Currently supported designs are Circumscribed (\code{"CCC"}, default) or face-centered (\code{"CCF"}).
#'
#' @return The experimental design \code{data.frame} including the newly added star points.
#' @export
#'
#' @examples
#' #Load data
#' doe_base <- load_internal("doe_base")
#'
#' #Get experimental design data.frame
#' doe_df <- doe_base[["FD_3"]]
#'
#' #Add center points
#' res <- add_stars(doe_df, 3, "CCC")
#'
#' @seealso \code{\link{doe_frfd}}, \code{\link{doe_ffd}}, \code{\link{doe_ccd}}, \code{\link{doe_bbd}}, \code{\link{doe_tm}}
add_stars <- function(input, factors, design = "CCC") {
  #Preliminary checks
  if(!is.data.frame(input) | !any(colnames(input) %in% "Standard_Order")) stop("Input data must contain coded factors and their levels!")

  cat("\nAdding star points...")

  #Calculate alpha value (for star points, which ensure ROTATABILITY)
  #Coded
  alpha_cmax <- if(design=="CCC") (2^factors)^0.25 else if(design=="CCF") 1
  alpha_cmin <- -alpha_cmax

  #Add coded star point portion of the design
  staref <- c(4, 6, 8)
  names(staref) <- 2:4
  starvec <- rep(0, staref[as.character(factors)])

  star_df <- data.frame("Standard_Order" = seq(max(input[,"Standard_Order"])+1, max(input[,"Standard_Order"])+length(starvec), by = 1))
  substvals <- 1:2
  codecols <- colnames(input)[!colnames(input) %in% "Standard_Order"]
  for(i in seq_along(codecols)) {
    if(i>1) substvals <- substvals+2
    subvec <- starvec
    subvec[substvals] <- c(alpha_cmin, alpha_cmax)
    star_df[,codecols[i]] <- subvec
  }
  #Merge starpoints with main df
  output <- rbind.data.frame(input, star_df)
  return(output)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Add coded center points to design matrix
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Add center points to an experimental design.
#'
#' @description Part of the \strong{experimental design generation} workflow.
#'
#' @param input A \code{data.frame} containing the \strong{coded} input factors and their levels. Columns (factor names) must be named alphabetically.
#' @param cpts The number of center points to add (defaults to 3).
#'
#' @return The experimental design \code{data.frame} including the newly added center points.
#' @export
#'
#' @examples
#' #Load data
#' doe_base <- load_internal("doe_base")
#'
#' #Get experimental design data.frame
#' doe_df <- doe_base[["FD_3"]]
#'
#' #Add center points
#' res <- add_cpts(doe_df, 3)
#'
#' @seealso \code{\link{doe_frfd}}, \code{\link{doe_ffd}}, \code{\link{doe_ccd}}, \code{\link{doe_bbd}}, \code{\link{doe_tm}}
add_cpts <- function(input, cpts = 3) {
  #Preliminary checks
  if(!is.data.frame(input) | !any(colnames(input) %in% "Standard_Order")) stop("Input data must contain coded factors and their levels!")
  if(cpts<=0) stop("The number of center points ('cpts') to be added must be greater than zero!")
  if(length(which(LETTERS %in% colnames(input)))==0) stop("No coded factors (denoted by capital letters) found in the input data!")

  cat("\nAdding center points...")

  #Add coded center points
  centvec <- rep(0, cpts)
  cent_df <- data.frame("Standard_Order" = seq(max(input[,"Standard_Order"])+1, max(input[,"Standard_Order"])+length(centvec), by = 1))
  #Merge center points with main df
  lettcols <- LETTERS[which(LETTERS %in% colnames(input))]
  cent_df[,lettcols] <- rep(list(centvec),length(lettcols))
  output <- rbind.data.frame(input, cent_df)
  return(output)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Add uncoded factor levels to design matrix
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Get uncoded factor levels of an experimental design
#'
#' @description Part of the \strong{experimental design generation} workflow.
#' \strong{This function manual is hidden from the Index.}
#'
#' @param input A \code{data.frame} containing the \strong{coded} input factors and their levels. Columns (factor names) must be named alphabetically.
#' @param design The type of design as a \code{character} value. Possible values are \code{"CCC"}, \code{"CCF"}, \code{"FFD"}, \code{"FrFD"}.
#' @param levels The number of factor levels as a single \code{numeric} value.
#' @param factors The number of factors as a single \code{numeric} value.
#' @param flims A \code{list} of vectors (each of length 2) containing the lower and upper limits of factors.
#' The number of list elements must match \code{factors}. Hard limits are specified by naming the corresponding element \code{"hard"}.
#' @param fnames A \code{character} vector of factor names. Must be of equal length to \code{factors}.
#'
#' @return Returns the input \code{data.frame} with added columns containing the \strong{uncoded} factor levels.
#'
#' @export
#' @keywords internal
#'
#' @seealso \code{\link{doe_frfd}}, \code{\link{doe_ffd}}, \code{\link{doe_ccd}}, \code{\link{doe_bbd}}, \code{\link{doe_tm}}
uncode_levels <- function(input, design, levels, factors, flims, fnames) {

  cat("\nCalculating uncoded factor levels...")

  if(design %in% c("CCC","CCF")) {
    #Calculate alpha value (for star points, which ensure ROTATABILITY)
    #Coded
    alpha_cmax <- if(design=="CCC") (2^factors)^0.25 else if(design=="CCF") 1
    alpha_cmin <- -alpha_cmax

    #Uncoded (based on whether or not the values are hard limits)
    lvals <- list()
    if(is.null(names(flims))) names(flims) <- rep("soft", length(flims))

    for(i in seq_along(flims)) {
      lvl_seq <- seq(min(flims[[i]]), max(flims[[i]]), length.out = 3)
      denom <- if(names(flims)[i]=="hard") alpha_cmax else 1
      lvl_dist <- diff(lvl_seq)[1]/denom

      lvals[[fnames[i]]] <- if(names(flims)[i]=="hard") {
        c(min(flims[[i]]), lvl_seq[2]-lvl_dist, lvl_seq[2], lvl_seq[2]+lvl_dist, max(flims[[i]]))
      } else c(lvl_seq[2]-lvl_dist*abs(alpha_cmin), lvl_seq, lvl_seq[2]+lvl_dist*alpha_cmax)
    }
    lvals <- lapply(lvals, function(x) x[order(x, decreasing = FALSE)])

    #Remove star-point levels if design is CCF
    if(design=="CCF") lvals <- lapply(lvals, function(x) x[c(-1,-5)])

    #Detect duplicate values in levels
    for(i in fnames) if(any(table(lvals[[i]])>1)) warning(paste0("Duplicated levels (likely zeroes) detected for factor:", i, "!"))
  } else {
    lvals <- list()
    #Determine number of levels required based on whether or not centerpoints were added
    lval_length <- if(design %in% c("FFD","FrFD") & any(rowSums(input[,!colnames(input) %in% "Standard_Order"])==0) & levels==2) 3 else levels #any(apply(input, 1, function(x) all(x[-1]==0))) or any(rowSums(input[,!colnames(input) %in% "Standard_Order"])==0) checks for center points
    for(i in seq_along(fnames)) lvals[[fnames[i]]] <- seq(min(flims[[i]]), max(flims[[i]]), length.out = lval_length)
  }

  #Add actual variable values (uncoded)
  for(i in seq_along(fnames)) {
    names(lvals[[fnames[i]]]) <- old <- unique(input[,LETTERS[i]])[order(unique(input[,LETTERS[i]]))]
    input[,paste0(LETTERS[i], "_", fnames[i])] <- lvals[[fnames[i]]][match(input[,LETTERS[i]], old, nomatch = 0)]
  }
  return(input)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Get the defining relation of a design
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Get the defining relation of a Fractional Factorial design
#'
#' @description Part of the \strong{experimental design generation} workflow.
#' \strong{This function manual is hidden from the Index.}
#'
#' @param input A \code{data.frame} containing the \strong{coded} input factors and their levels. Columns (factor names) must be named alphabetically.
#' Only Fractional Factorial designs are supported at present.
#' @param factors The number of factors included in the design.
#' @param aliasing A \code{character} string defining the aliasing pattern of the design.
#'
#' @return A \code{list} containing the original input \code{data.frame} (\code{$data}) and the defining relation character string (\code{$def_rel}).
#'
#' @export
#' @keywords internal
#'
#' @seealso \code{\link{doe_frfd}}, \code{\link{doe_ffd}}, \code{\link{doe_ccd}}, \code{\link{doe_bbd}}, \code{\link{doe_tm}}
defrel <- function(input, factors, aliasing) {
  #Preliminary checks
  whichlet <- which(LETTERS %in% colnames(input))
  if(length(whichlet)==0) stop("No coded factors (denoted by capital letters) found in the input data!")

  cat("\nGetting defining relation...")

  #Calculate aliased columns and get defining relation(s)
  alias_facs <- LETTERS[(max(whichlet)+1):factors]
  alias_splits <- lapply(aliasing, function(x) strsplit(x, "")[[1]])
  def_rel <- c()

  for(i in seq_along(alias_facs)) {
    input[,alias_facs[i]] <- apply(input[,alias_splits[[i]]], 1, prod)
    def_rel[paste0("I_",i)] <- paste0(c(aliasing[i], alias_facs[i]), collapse="") #[i] works for "aliasing" because its length must equal that of "alias_facs"!
  }
  return(list(data = input, def_rel = def_rel))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Get the confounding pattern of a design from defining relation and factor number
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Get the confounding pattern of a design
#'
#' @description Part of the \strong{experimental design generation} workflow.
#' \strong{This function manual is hidden from the Index.}
#'
#' @param input A \code{data.frame} containing the \strong{coded} input factors and their levels. Columns (factor names) must be named alphabetically.
#' @param def_rel A \code{character} string specifying the \strong{defining relation} of a design.
#'
#' @return A two-column \code{data.frame} containing a list of main, interaction, and/or quadratic effects and their aliasing pattern (i.e. confounding pattern) generated from the defining relation.
#'
#' @references
#' NIST/SEMATECH (2013), 'Engineering Statistics Handbook', section 5.3.3.4.3, available at: \url{https://www.itl.nist.gov/div898/handbook/index.htm} (accessed 29.09.2024).
#'
#' @export
#' @keywords internal
#'
#' @seealso \code{\link{defrel}}, \code{\link{doe_frfd}}, \code{\link{doe_ffd}}, \code{\link{doe_ccd}}, \code{\link{doe_bbd}}, \code{\link{doe_tm}}
confound <- function(input, def_rel) {
  #Preliminary checks
  whichlet <- which(LETTERS %in% colnames(input))
  if(length(whichlet)==0) stop("No coded factors (denoted by capital letters) found in the input data!")
  if(!all(unlist(strsplit(def_rel,"")) %in% LETTERS[whichlet])) stop("The defining relation does not fit with coded factor letters!")

  cat("\nFinding confounding pattern...")

  #Get all main effects and two-factor interactions (ignoring higher-order interactions!)
  all_effs <- as.vector(outer(LETTERS[whichlet], LETTERS[whichlet], paste0))
  all_effs <- sapply(all_effs, function(x) { res <- strsplit(x, "")[[1]];
  if(any(table(res)>1)) res <- NULL else res <- res[order(res, decreasing = FALSE)];
  res <- paste(res, collapse = "");
  return(res)})
  all_effs <- all_effs[!all_effs==""]
  all_effs <- c(LETTERS[whichlet], unique(all_effs))

  #Assemble a data frame of all effects with the defining relation (or relations for 2^5-2 design)
  output <- data.frame("Effect"=all_effs)

  for(i in seq_along(def_rel)) {
    #Determine confounded/aliased main and 2nd-order effects
    aliased <- sapply(all_effs, function(x) { res <- paste0(x, def_rel[i]);
    res <- strsplit(res, "")[[1]];
    res <- res[order(res, decreasing = FALSE)];
    res <- res[!res %in% res[which(duplicated(res))]];
    res <- paste0(res, collapse = "");
    return(res)})
    output[,paste0("I=",def_rel[i])] <- aliased
  }
  return(output)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Export generated design data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Export generated experimental design
#'
#' @description Part of the \strong{experimental design generation} workflow.
#'
#' @param input Output from one of the experimental design generator functions: \code{\link{doe_ffd}}, \code{\link{doe_frfd}}, \code{\link{doe_ccd}}, \code{\link{doe_bbd}}, or \code{\link{doe_tm}}.
#' @param export_name An \strong{optional} title to paste as the first line of the exported .TAB file.
#' @param expath The directory path into which to export the output .TAB file.
#' @param silent A \code{logical} specifying whether console output should be suppressed (\code{FALSE} by default).
#'
#' @return A .TAB file containing the experimental design \code{data.frame} is exported to the path specified in \code{expath}.
#' @export
#'
#' @seealso \code{\link{doe_frfd}}, \code{\link{doe_ffd}}, \code{\link{doe_ccd}}, \code{\link{doe_bbd}}, \code{\link{doe_tm}}
#'
#' @importFrom data.table fwrite
#'
doe_export <- function(input, export_name = NA, expath = getwd(), silent = FALSE) {
  #Preliminary checks
  if(!is.list(input)) stop("The input data must be a list output from one of the DOE generator functions!")
  if(!dir.exists(expath)) stop("The export directory does not exist!")
  #if(!is.character(export_name)) stop("An export file title must be given as a single character string!")

  #Export data
  expath <- paste0(expath, "/", names(input)[1], "_", format(Sys.time(), "%Y-%m-%d %Hhr %Mmin %Ssec"), ".tab")
  if(!silent) cat("\nExporting design to: ", expath, "...", sep = "")

  if(!is.na(export_name) & is.character(export_name)) cat(export_name, "\n", file = expath, sep = "", append = TRUE)
  for(i in seq_along(input)) {
    if(i==1) {
      heading <- "DESIGN MATRIX"
    } else if(i>1) heading <- toupper(names(input)[i])
    cat(heading, "\n", file = expath, sep="", append = TRUE)
    if(names(input)[i]=="call") cat(format(input[[i]]), file = expath, sep = "", append = TRUE) else fwrite(input[[i]], expath, sep = "\t", na = "", append = TRUE, col.names = TRUE)
    cat("\n", file = expath, append = TRUE)
  }
}
