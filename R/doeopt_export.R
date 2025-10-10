#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Export DOE analysis results (visualisations and data)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Export DOE analysis results
#'
#' @description Export the results and visualisations of \code{\link{doe_analyze}} workflow as .CSV and .PDF (or .PNG) files.
#'
#' @param input The output of function \code{\link{doe_analyze}}.
#' @param expath The \strong{existing} export folder path.
#' @param plotpars A \strong{named} \code{numeric} vector of plotting parameters, including:
#' width (\code{["w"]}, in inches), height (\code{["h"]}), point size (\code{["psize"]}), and dots per inch or DPI (\code{["dpi"]}).
#' Initially set to \code{"default"}, i.e. \code{c(w = 10, h = 12, psize = 12, dpi = 300)}.
#' @param plot_format Format of plots to export. One of \code{"png"} (default) or \code{"pdf"}.
#' @param silent Should console output be silenced? Defaults to \code{FALSE}).
#'
#' @return A .CSV file and any accompanying graphics are expoted to \code{expath}.
#' @export
#'
#' @seealso \code{\link{doe_analyze}}
#'
#' @importFrom ggplot2 ggsave
#' @importFrom data.table fwrite
#'
doeopt_export <- function(input, expath = getwd(), plotpars = "default", plot_format = "png", silent = FALSE) {

  #Set the delimiter
  fwsep <- "\t"

  #Preliminary checks
  if(!all(c("models","results","plots","statements","call") %in% names(input))|!is.list(input)) stop("The 'input' must originate from function 'doe_analyze'!")
  modres <- input[["results"]]
  plotres <- input[["plots"]]
  statements <- input[["statements"]]
  funcall <- input[["call"]]

  if(inherits(funcall, "call")) funcall <- format(funcall)
  if(length(modres)!=length(plotres) | length(modres)!=2 | !identical(names(modres), names(plotres))) stop("Both 'plotres' and 'modres' must be lists of length 2 with identical names!")
  if(!dir.exists(expath)) stop("The selected export directory doesn't exist!")
  if(!plot_format %in% c("png","pdf")) stop("The output format for plots must be one of: 'png', 'pdf'!")

  #Set up plot parameters
  defpars <- c(w = 10, h = 12, psize = 12, dpi = 300)
  plotpars <- supp_pars(pars = plotpars, defpars = defpars, parlb = "plotpars")

  #Prepare/create export directory
  expath <- paste0(expath, "/DOE_Analysis_", format(Sys.time(), "%Y-%m-%d %Hhr %Mmin %Ssec"))
  if(!silent) cat("\nExport path: ", expath, "...")
  expath_plots <- paste0(expath, "/Plots")

  #Create output dirs
  invisible(sapply(c(expath, expath_plots), dir.create))

  #Export plots
  if(!silent) cat("\nExporting plots...")
  for(i in names(plotres)) {
    lapply(seq_along(plotres[[i]]), function(x) ggsave(paste0(expath_plots, "/", names(plotres[[i]])[x],".",plot_format), plotres[[i]][[x]],
                                                       height = plotpars["h"], width = plotpars["w"], units = "in", pointsize = plotpars["psize"],
                                                       dpi = plotpars["dpi"], device = plot_format))
  }

  #Exporting data to a single .CSV
  if(!silent) cat("\nExporting data...")
  fpath <- paste0(expath, "/DOE_Analysis_Results.tab")
  initres <- modres[["initial"]]
  finres <- modres[["final"]]

  cat("DESIGN OF EXPERIMENTS (DOE) OPTIMIZATION OF THE RESPONSE: ", initres[["Misc"]][["Response"]], ", VIA THE FOLLOWING ",  nrow(initres[["Model_Data"]]), "-RUN DESIGN:", file = fpath, sep = "", append = TRUE)

  #Export original input dataset (not the model input dataset!)
  if("Orig_Data" %in% names(initres)) {
    desc_suffix <- if("Factor_Names" %in% names(initres[["Misc"]])) paste0(") AND UNCODED FACTORS (", paste0(initres[["Misc"]][["Factor_Names"]], collapse = ", "), ")") else ""
    doe_desc <- paste0("ORIGINAL DATASET WITH CODED FACTORS (", paste0(LETTERS[LETTERS %in% colnames(initres[["Orig_Data"]])], collapse= ", "), desc_suffix)
    cat("\n", doe_desc, "\n", file = fpath, sep = "", append = TRUE)
    fwrite(initres[["Orig_Data"]], file = fpath, na = "", sep = fwsep, col.names = TRUE, append = TRUE)
  }

  #Export title of the initial model dataset
  term_titles <- c("FIRST-ORDER TERMS", "TWO-WAY INTERACTIONS", "QUADRATIC TERMS")
  order_list <- c(1, 1.5, 2)
  mod_order <- initres[["Model_Metrics"]][["Order"]]
  order_desc_suffix <- paste0(term_titles[1:which(order_list==mod_order)], collapse = ", ")
  order_desc <- paste0("STARTING MODEL INPUT DATASET INCLUDING ", order_desc_suffix)
  cat("\n", order_desc, "\n", file = fpath, sep = "", append = TRUE)

  #Export the initial model dataset
  fwrite(initres[["Model_Data"]], file = fpath, na = "", sep = fwsep, col.names = TRUE, append = TRUE)

  #Loop through initial and final model results
  for(i in names(modres)) {

    #Export inestimable factors, if any
    cat("\n", toupper(i), " MODEL RESULTS", file = fpath, sep = "", append = TRUE)
    if(i == "initial") cat("\n", statements["Inestimable"], file = fpath, sep = "", append = TRUE)
    cat("\n", statements[paste0("Mod_",i)], file = fpath, sep = "", append = TRUE)
    cat("\n", statements[paste0("Eq_",i)],"\n", file = fpath, sep = "", append = TRUE)

    #Export model results
    cat("\nESTIMATED COEFFICIENTS, ANOVA AND OTHER STATISTICS\n", file = fpath, sep = "", append = TRUE)
    fwrite(modres[[i]][["Model_Results"]], file = fpath, na = "", sep = fwsep, col.names = TRUE, append = TRUE)

    #Export model data with residuals
    cat("\nCODED ", toupper(i), " MODEL DATA WITH RESIDUALS INCLUDED\n", sep = "", file = fpath, append = TRUE)
    fwrite(modres[[i]][["Model_Data"]], file = fpath, na = "", sep = fwsep, col.names = TRUE, append = TRUE)

    #Export model metrics
    cat("\nKEY METRICS FOR MODEL: ", toupper(i), "\n", file = fpath, sep = "", append = TRUE)
    metr <- modres[[i]][["Model_Metrics"]]
    cat(c("Order", "Lack-of-Fit P", "R^2", "Adj. R^2", "Resid. Stnd. Error", "F", "F DOF 1", "F DOF 2"), file = fpath, sep = fwsep, append = TRUE)
    cat("\n", file = fpath, append = TRUE)
    cat(c(metr[["Order"]], metr[["LoF_Pvalue"]], metr[["R2"]], metr[["Adj_R2"]], metr[["Residual_Stnd_Error"]],
          metr[["F_Statistic"]], metr[["F_DOF_1"]], metr[["F_DOF_2"]]), file = fpath, sep = fwsep, append = TRUE)
    cat("\n", file = fpath, append = TRUE)

    #Export metric-based observations/statements
    cat("\nMETRIC-BASED OBSERVATIONS ABOUT MODEL: ", toupper(i), file = fpath, sep = "", append = TRUE)
    cat("\n", statements[paste0("LoF_",i)], file = fpath, sep = "", append = TRUE)
    cat("\n", statements[paste0("R2_vs_adjR2_",i)], "\n", file = fpath, sep = "", append = TRUE)

    #Export Classical Optimization results
    cat("\nGENERAL-PURPOSE OPTIMIZATION RESULTS FOR MODEL: ", toupper(i), file = fpath, sep="", append = TRUE)
    cat("\nCODED VALUES\n", file = fpath, append = TRUE)
    fwrite(metr[["Trad_Opt"]][["coded"]], file = fpath, na = "", sep = fwsep, col.names = TRUE, append = TRUE) #CHECK THIS OUT LATER!

    if("decoded" %in% names(metr[["Trad_Opt"]])) {
      cat("\nDECODED VALUES\n", file = fpath, append = TRUE)
      fwrite(metr[["Trad_Opt"]][["decoded"]], file = fpath, na = "", sep = fwsep, col.names = TRUE, append = TRUE)
      for(j in 1:2) cat("\n", statements[paste0("TradOpt_", j,"_", i)], file = fpath, sep = "", append = TRUE)
    }
    cat("\n", file = fpath, append = TRUE)

    #Export Canonical Analysis results
    if("Canonical_Analysis" %in% names(metr)) {

      cat("\nCODED CANONICAL ANALYSIS VALUES FOR MODEL: ", toupper(i), "\n", file = fpath, sep = "", append = TRUE)
      cat(names(metr[["Canonical_Analysis"]][["xs"]]), file = fpath, sep = fwsep, append = TRUE)
      cat("\n", file = fpath, append = TRUE)
      cat(metr[["Canonical_Analysis"]][["xs"]], file = fpath, sep = fwsep, append = TRUE)
      cat("\n", file = fpath, append = TRUE)

      cat("\nCODED CANONICAL ANALYSIS EIGEN VALUES FOR MODEL: ", toupper(i), "\n", file = fpath, sep = "", append = TRUE)
      cat(names(metr[["Canonical_Analysis"]][["xs"]]), file = fpath, sep = fwsep, append = TRUE)
      cat("\n", file = fpath, append = TRUE)
      cat(metr[["Canonical_Analysis"]][["eigen"]][["values"]], file = fpath, sep = fwsep, append = TRUE)
      cat("\n", file = fpath, append = TRUE)

      if("xs_decoded" %in% names(metr[["Canonical_Analysis"]])) {
        cat("\nDECODED CANONICAL ANALYSIS VALUES FOR MODEL: ", toupper(i), "\n", file = fpath, sep = "", append = TRUE)
        cat(names(metr[["Canonical_Analysis"]][["xs_decoded"]]), file = fpath, sep = fwsep, append = TRUE)
        cat("\n", file = fpath, append = TRUE)
        cat(metr[["Canonical_Analysis"]][["xs_decoded"]], file = fpath, sep = fwsep, append = TRUE)
        cat("\n", file = fpath, append = TRUE)
        for(j in 1:5) { #seq(length(statements[grep("Optim_", names(statements))[1]]))
          cat("\n", statements[paste0("Optim_", j, "_", i)], file = fpath, sep = "", append = TRUE)
        }
      }

    } else if("Steepest_Ascent_Linear" %in% names(metr)) {
      cat("\nSTEEPEST ASCENT RUN RESULTS FOR MODEL (LINEAR): ", toupper(i), "\n", file = fpath, sep = "", append = TRUE)
      cat(names(metr[["Steepest_Ascent_Linear"]]), file = fpath, sep = fwsep, append = TRUE)
      cat("\n", file = fpath, append = TRUE)
      cat(metr[["Steepest_Ascent_Linear"]], file = fpath, sep = fwsep, append = TRUE)
    }
    cat("\n", file = fpath, append = TRUE)

    if("Steepest_Ascent" %in% names(metr)) {
      cat("\nSTEEPEST ASCENT RUN RESULTS FOR SECOND-ORDER MODEL: ", toupper(i), "\n", file = fpath, sep = "", append = TRUE)
      if(is.list(metr[["Steepest_Ascent"]])) {
        fwrite(metr[["Steepest_Ascent"]], file = fpath, na = "", sep = fwsep, col.names = TRUE, append = TRUE)
      } else cat(metr[["Steepest_Ascent"]], file = fpath, sep = "", append = TRUE)
    }

    #Export statements about procedures for removal of insignificant terms (ONLY FOR THE INITIAL MODEL!)
    if(i=="initial") {
      cat("\nPROCEDURES APPLIED TO INITIAL MODEL FOR REMOVAL OF INSIGNIFICANT TERMS", file = fpath, append = TRUE)
      if("p_cutoff" %in% names(statements)) cat("\n", statements["p_cutoff"], file = fpath, sep = "", append = TRUE)
      if("Stepwise" %in% names(statements)) cat("\n", statements["Stepwise"], file = fpath, sep = "", append = TRUE)
      if("Notrim" %in% names(statements))  cat("\n", statements["Notrim"], file = fpath, sep = "", append = TRUE)
      cat("\n", statements["Signif_Effs"], file = fpath, sep = "", append = TRUE)
      cat("\n", statements["H_Principle"], file = fpath, sep = "", append = TRUE)
    }
    replicate(2, cat("\n", file = fpath, append = TRUE))
  }

  #Finally, optionally export the function call
  if(is.character(funcall)) {
    cat("FUNCTION CALL\n", file = fpath, append = TRUE)
    cat(funcall, file = fpath, sep = "", append = TRUE)
  }
  if(!silent) cat("\nDONE!")
}
