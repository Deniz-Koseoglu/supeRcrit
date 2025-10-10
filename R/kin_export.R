#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Export the results of one or more kinetic models and their summaries
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Export the results of one or more kinetic models and their summaries
#'
#' @description Compiles and exports the results and plots of Broken-and-Intact Cells (BIC) or Two-Site Kinetic Desorption
#' (TWS) model into a specified directory.
#'
#' @param modres The output of \code{\link{bicmod}} or \code{\link{ktsmod}}.
#' @param sumres \strong{Optional} results summarizing \strong{several} kinetic models output from function \code{\link{kin_splot}}.
#' @param expath An \strong{existing} directory where the results should be exported. Defaults to the working directory.
#' @param which_mod A \code{character} vector specifying which model results should be exported. Either \code{"all"} (default)
#' or any of \code{"sim"} (simple BIC), \code{"ct"} (characteristic times BIC), \code{"cmp"} (complete BIC), and/or \code{"tws"}
#' (two-site kinetic desorption).
#' @param plotpars Either \code{"default"} or a \strong{named} vector containing one or more of the
#' following parameters: \code{"w"} (width, defaults to 10 inches), \code{"h"} (height, defaults to 12 inches),
#' \code{"psize"} (point size, default to 12), \code{"dpi"} (resolution, defaults to 300 dpi).
#' @param plot_format The plot format. One of: \code{"png"} (default) or \code{"pdf"}.
#' @param silent A \code{logical}. When \code{FALSE} (default), additional information is printed in the console.
#'
#' @return Results as a .CSV files and plots as either .PDF or .PNG files are output into a new directory in \code{expath}.
#' @export
#'
#' @seealso \code{\link{bicmod}}, \code{\link{ktsmod}}, \code{\link{kin_splot}}, \code{\link{show_pars}}
#'
#' @importFrom stats setNames
#' @importFrom data.table fwrite
#' @importFrom ggplot2 ggsave
#'
kin_export <- function(modres, sumres = NA, expath = getwd(), which_mod = "all", plotpars = "default", plot_format = "png",
                       silent = FALSE) {

  #Set the delimiter
  fwsep <- "\t"

  #Preliminary checks
  singlechk <- all(c("data", "plots", "input", "call") %in% names(modres))
  if(!singlechk) {
    prelchk <- all(sapply(modres, function(x) is.list(x) & all(c("data","plots","input", "call") %in% names(x))))
    if(!prelchk) stop("The data structure of kinetic model results was not recognised (must be output from functions 'ktsmod' or 'bicmod')!")
  } else modres <- list(modres)

  singlesumchk <- all(c("plot", "summary", "model_type") %in% names(sumres))
  if(!singlesumchk) {
    prelsumchk <- all(sapply(sumres, function(x) is.list(x) & all(c("plot", "summary", "model_type") %in% names(x))))
    if(!prelsumchk) stop("The data structure of kinetic model SUMMARY ('sumres') results was not recognised (must be output from function 'kin_splot')!")
  } else sumres <- list(sumres)

  if(!dir.exists(expath)) stop("The selected export directory doesn't exist!")
  if(!plot_format %in% c("png","pdf")) stop("The output format for plots must be one of: 'png', 'pdf'!")
  #if(!all(is.na(sumres)) & !all(c("plot","summary") %in% names(sumres))) stop("When provided, input data 'sumres' must be output from function 'kin_splot'!")
  defmods <- c("sim", "ct", "cmp", "tws")
  if(any(which_mod %in% "all")) which_mod <- defmods else if(!all(which_mod %in% c(defmods)))
    stop(paste0("Model names given in 'which_mod' not recognized! Possible names are: ", paste0("'", defmods, "'", collapse = ", "),"!"))

  #Set up plot parameters
  defpars <- c(w = 10, h = 12, psize = 12, dpi = 300)
  plotpars <- supp_pars(pars = plotpars, defpars = defpars, parlb = "plotpars")

  #Prepare/create export directory
  expath <- paste0(expath, "/KIN_", format(Sys.time(), "%Y-%m-%d %Hhr %Mmin %Ssec"))
  if(!silent) cat("\nExport path: ", expath, "...")
  expath_plots <- paste0(expath, "/Plots")

  #Create output dirs
  invisible(sapply(c(expath, expath_plots), dir.create))

  #Exporting data to a single .CSV
  if(!silent) cat("\nExporting data...")
  fpath <- paste0(expath, "/Kinetic_Model_Results.tab")

  #Export individual model results
  pltlst <- list()
  modnms <- c("sim" = "Simple BIC model", "ct" = "BIC model based on characteristic times",
              "cmp" = "Complete BIC model", "tws" = "Two-site kinetic desoprtion model")
  modnms <- modnms[names(modnms) %in% which_mod]

  for(i in seq_along(modres)) {

    curmod <- modres[[i]]
    modchk <- which(names(curmod) %in% names(modnms))

    if(i>1) cat("\n\n", file = fpath, sep = "", append = TRUE)
    cat("INDIVIDUAL KINETIC MODEL RESULTS: MODEL ", i, file = fpath, sep = "", append = TRUE)
    cat("\nOriginal input dataset\n", file = fpath, sep = "", append = TRUE)
    data.table::fwrite(curmod[["data"]], file = fpath, na = "", sep = fwsep, col.names = TRUE, append = TRUE)

    for(j in modchk) {
      mname <- names(curmod)[j]
      modsub <- curmod[[mname]]
      modesc <- modnms[[mname]]
      cat("\n", toupper(modesc), " RESULTS", file = fpath, sep = "", append = TRUE)
      cat("\nProcessed input data with model predictions\n", file = fpath, sep = "", append = TRUE)
      data.table::fwrite(modsub[["ordt"]], file = fpath, na = "", sep = fwsep, col.names = TRUE, append = TRUE)

      cat("\nModel output data (for plotting)\n", file = fpath, sep = "", append = TRUE)
      data.table::fwrite(modsub[["mdt"]], file = fpath, na = "", sep = fwsep, col.names = TRUE, append = TRUE)

      cat("\nModel parameters\n", file = fpath, sep = "", append = TRUE)
      modpar_desc <- paste0("The following parameters were modeled: ", paste0(modsub[["fit_pars"]], collapse = ", "))
      cat(modpar_desc, "\n", file = fpath, sep = fwsep, append = TRUE)
      cat(names(modsub[["mod_pars"]]), file = fpath, sep = fwsep, append = TRUE)
      cat("\n", file = fpath, append = TRUE)
      cat(modsub[["mod_pars"]], file = fpath, sep = fwsep, append = TRUE)

      cat("\n\nModel errors (AARD, RMSE, R2)\n", file = fpath, sep = "", append = TRUE)
      cat(names(modsub[["resid"]]), file = fpath, sep = fwsep, append = TRUE)
      cat("\n", file = fpath, append = TRUE)
      cat(modsub[["resid"]], file = fpath, sep = fwsep, append = TRUE)
    }

    #Export function call
    cat("\n\nFunction call\n", file = fpath, append = TRUE)
    cat(format(curmod[["call"]]), file = fpath, sep = "", append = TRUE)

    #Add plots to collective list
    pltlst <- c(pltlst, setNames(curmod[["plots"]], paste0(names(curmod[["plots"]]), "_model", i)))
  }

  #Export model summary results and plots (if any)
  if(!all(is.na(sumres))) {

    for(i in seq_along(sumres)) {
      cursum <- sumres[[i]]
      #Export data
      cat("\n\nMULTI-MODEL SUMMARY RESULTS", file = fpath, sep = "", append = TRUE)
      cat("\nModel Type ", i, ": ", modnms[cursum[["model_type"]]], "\n", file = fpath, sep = "", append = TRUE)
      data.table::fwrite(cursum[["summary"]], file = fpath, na = "", sep = fwsep, col.names = TRUE, append = TRUE)

      #Add plot to output list
      pltlst[[paste0(cursum[["model_type"]], "_summary_plot")]] <- cursum[["plot"]]
    }
  }

  #Finally, export plots
  if(!silent) cat("\nExporting plots...")
  plotsave <- lapply(seq_along(pltlst), function(x) ggplot2::ggsave(paste0(expath_plots, "/", names(pltlst)[x], ".", plot_format), pltlst[[x]],
                                                                    height = plotpars["h"], width = plotpars["w"], units = "in", pointsize = plotpars["psize"],
                                                                    dpi = plotpars["dpi"], device = plot_format))
  if(!silent) cat("\nDONE!")
}
