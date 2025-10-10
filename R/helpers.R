#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Check which elements can be numeric
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Check which elements of vector can or cannot be numeric
#' @description Finds elements which can (or cannot) be coerced to \code{numeric} format.
#' @param vec Character vector to evaluate
#'
#' @return A \code{logical} vector indicating whether numeric conversion is suitable
#' @export
#'
can_numeric <- function(vec) {
  if(!is.character(vec)) stop("The input must be a character vector!")
  grepl('^(?=.)([+-]?([0-9]*)(\\.([0-9]+))?)$', vec, perl = TRUE)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Supplement function arguments (named vectors) with default values
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Supplement function arguments (named vectors) with default values
#'
#' @description Compares an input \strong{named} vector of parameters with a default vector,
#' supplementing with default parameters where necessary.
#' \strong{This function manual is hidden from the Index}.
#'
#' @param defpars A \strong{named} vector of default parameters (numeric or character).
#' @param pars A \strong{named} vector of input parameters where all names must correspond to those of \code{defpars}.
#' @param parlb The label for parameters. If \code{NA} (default), defaults to \code{"pars"}.
#'
#' @return The input vector supplemented with default parameters (if necessary).
#'
#' @export
#' @keywords internal
#'
supp_pars <- function(defpars, pars = "default", parlb = NA) {

  #Preliminary checks
  mlst <- list(pars, defpars)
  if(!any(pars %in% "default") & any(unlist(lapply(mlst, function(x) is.null(names(x)))))) stop("Arguments 'pars' and 'defpars' must both be named vectors!")
  if(!all(sapply(mlst, is.atomic))) stop("Arguments 'pars' and 'defpars' must both be atomic vectors!")
  if(is.na(parlb)) parlb <- "pars"

  #Processing
  posnms <- paste0("'", names(defpars), "'", collapse = ", ")
  if(!any(pars %in% "default")) {
    if(is.null(names(pars))) stop(paste0("The vector '", parlb, "' must be named! Possible names are: " , posnms, "."))
    if(!all(names(defpars) %in% names(pars))) {
      if(length(which(!names(pars) %in% names(defpars)))>0) cat("\nSome of the names provided to '", parlb, "' were not recognized. Possible names are: " , posnms, ".", sep = "")
      abs_pars <- which(!names(defpars) %in% names(pars))
      if(length(abs_pars)>0) cat("\nAdding the following missing parameters to '", parlb, "': ", paste0("'", names(defpars)[abs_pars], "'", collapse = ", "),  "...", sep = "")
      pars <- append(pars, defpars[abs_pars])[names(defpars)]
    }
  } else pars <- defpars
  return(pars)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Split vector at specific position(s)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Split vector at specific position(s)
#'
#' @description Splits an input atomic vector into two or more vectors at specific indices.
#' Function adapted from a StackOverflow answer (see \strong{References}).
#'
#' @param x An \code{atomic} vector to split.
#' @param pos A \code{numeric} position (or vector thereof) at which to split the input vector \code{x}.
#'
#' @return A \code{list} of output vectors resulting from splitting \code{x} at position(s) given in \code{pos}.
#' @export
#'
#' @references
#' \href{StackOverflow answer}{https://stackoverflow.com/questions/16357962/r-split-numeric-vector-at-position} by \strong{flodel}.
#'
#' @examples
#' splitAt(seq(1,5,1), c(2,4))
splitAt <- function(x, pos) { unname(split(x, cumsum(seq_along(x) %in% pos))) }

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Generate a sequence which includes the last number
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Get sequence including the last number
#'
#' @param x,y Starting and ending points of the sequence.
#' @param z Increment of the sequence.
#'
#' @return A \code{numeric} vector containing the sequence inclusive of the last number.
#' @export
#'
#' @examples
#' seq_last(80,310,20)
seq_last <- function(x,y,z) {
  seqn <- c(seq(x,y,z), y)
  seqn <- seqn[!duplicated(seqn)]
  return(seqn)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Capitalize first letter of every word in string
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Capitalize first letter of every word in string
#'
#' @param x A \code{character} string.
#'
#' @return The input character string with every word (separated by a space) capitalized.
#' @export
#'
#' @examples
#' scap("every word of this sentence should now be capitalized")
scap <- function(x) { s <- tolower(x); s <- strsplit(s, " ")[[1]]; paste(toupper(substring(s, 1,1)), substring(s, 2), sep="", collapse=" ") }


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Flatten nested lists
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Flatten nested lists
#'
#' @description Flattens nested lists into a one-level list.
#'
#' @param x A nested \code{list} object.
#'
#' @return A one-level list created from the input list \code{x}.
#' @export
#'
flattenlist <- function(x){
  morelists <- sapply(x, function(xprime) class(xprime)[1]=="list")
  out <- c(x[!morelists], unlist(x[morelists], recursive=FALSE))
  if(sum(morelists)){
    Recall(out)
  } else return(out)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Calculate row-wise variances
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Calculate row-wise variances
#'
#' @param x A \code{data.frame} for which to calculate row-wise variances.
#' @param na.rm A \code{logical} indicating whether to ignore \code{NA} values (\code{FALSE} by default).
#'
#' @return A \code{numeric} vector of variances (one value per row of \code{x}).
#' @export
#'
rowVars <- function(x, na.rm = FALSE) {
  # Vectorised version of variance filter
  rowSums((x - rowMeans(x, na.rm=na.rm))^2, na.rm=na.rm) / (ncol(x) - 1)
}
