#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Data exporting function for GCM parameter estimation and HSP-based solubility enhancement results
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Export results of HSP-based miscibility enhancement function
#'
#' @description Exports the output of function \code{\link{sfe_mod}} as a .CSV file along any additional data of class \code{"ggplot"}.
#'
#' @param input The output of function \code{\link{sfe_mod}}.
#' @param addres Additional output of class \code{"ggplot"}. Defaults to \code{NA}.
#' @param plotpars Either \code{"default"} or a \strong{named} vector containing one or more of the
#' following parameters: \code{"w"} (width, defaults to 10 inches),
#' \code{"h"} (height, defaults to 12 inches),
#' \code{"psize"} (point size, default to 12),
#' \code{"res"} (raster resolution, defaults to 800 px, only applies when \code{plot_format} is \code{"png"}),
#' \code{"dpi"} (vector resolution, defaults to 300 dpi, only applies when \code{plot_format} is \code{"pdf"}).
#' @param plot_format The plot format. One of: \code{"png"} (default) or \code{"pdf"}.
#' @param expath The export directory as a \code{character} value. The path must exist on the system.
#' @param silent A \code{logical}. When \code{FALSE} (default), additional information is printed in the console.
#'
#' @return A folder with a timestamp is created in the output directory \code{expath}, into which the output
#' \strong{.CSV} file is exported. Any plots are exported into an eponymous sub-folder.
#' @export
#'
#' @seealso \code{\link{sfe_mod}}
#'
#' @importFrom grDevices dev.off pdf png
#' @importFrom grid grid.raster
#' @importFrom data.table fwrite
#' @importFrom ggplot2 ggsave
#'
hsp_export <- function(input, addres = NA, plotpars = "default", plot_format = "png", expath = getwd(), silent = FALSE) {

  #Set the delimiter
  fwsep <- "\t"

  #Preliminary checks
  if(!dir.exists(expath)) stop("The output directory does not exist!")
  if(!all(c("solute_ids", "parameters", "sfe", "gcm_vis", "call") %in% names(input))) stop("The 'input' data must be output from function 'sfe_mod'!")
  if(!all(is.na(addres)) & !all(sapply(addres, function(x) "ggplot" %in% class(x)))) stop("When provided, additional results ('addres') must be of class 'ggplot'!")

  #Set up plot parameters
  defpars <- c(w = 10, h = 12, psize = 12, res = 800, dpi = 300)
  plotpars <- supp_pars(pars = plotpars, defpars = defpars, parlb = "plotpars")

  #Unpack input data
  solute_ids <- input[["solute_ids"]]
  pares <- input[["parameters"]]
  optres <- input[["sfe"]]
  visres <- input[["gcm_vis"]]

  #Create output directory
  expath <- paste0(expath, "/HSP_", solute_ids["Name"], "_", solute_ids["CAS"], "_", format(Sys.time(), "%Y-%m-%d %Hhr %Mmin %Ssec"))
  if(!silent) cat("\nExport path: ", expath, "...")
  exfile <- paste0(expath, "/", solute_ids["Name"], "_GCM_and_HSP_Optim_Results.tab")
  expath_plots <- paste0(expath, "/Plots")

  #Create output dirs
  invisible(sapply(c(expath, expath_plots), dir.create))

  #Retrieve items to export
  if(is.list(pares)) {
    hsp_vec <- pares[["HSP"]]
    crit_vec <- pares[["Critical"]]
    method_vec <- pares[["Methods"]]
    contrib_list <- pares[["Contribs"]]
    names(contrib_list) <- paste0(names(contrib_list), " GCM contibutions (method: ", method_vec, ")")
  }

  if(is.list(optres)) {
    modif_vec <- optres[["Modifiers"]]
    vfrac <- optres[["Volume_Fraction"]]
    hsp_list <- optres[["SoluteHSP_vs_Temp"]]
    blend_list <- optres[["SolventBlend_HSPs"]]
    ra_list <- optres[["Ra"]]
    miscib_list <- optres[["Miscib_Enhancement"]]
  }

  #Export structure visualizations
  if(!all(is.na(visres))) {
    if(!silent) cat("\nExporting plots...")
    for(i in seq_along(visres)) {
      if(plot_format=="png") {
        png(filename = paste0(expath_plots, "/", names(visres)[i], if(is.list(pares)) paste0("_", method_vec[i]) else "", "_", solute_ids["Name"], "_", solute_ids["CAS"], "_GCM.png"),
            type = "cairo", units="in",
            height = plotpars["h"], width = plotpars["w"],
            pointsize = plotpars["psize"], res = plotpars["res"])
      } else if(plot_format=="pdf") {
        pdf(file = paste0(expath_plots, "/", names(visres)[i], if(is.list(pares)) paste0("_", method_vec[i]) else "", "_", solute_ids["Name"], "_", solute_ids["CAS"], "_GCM.pdf"),
            height = plotpars["h"], width = plotpars["w"],
            pointsize = plotpars["psize"])
      }
      grid.raster(visres[[i]])
      dev.off()
    }
  }

  #Export ggplots
  if(!all(is.na(addres))) {
    if(!silent) cat("\nExporting additional plots...")
    #addres <- addres[["plots"]]
    lapply(seq_along(addres), function(x) ggsave(paste0(expath_plots, "/", names(addres)[x],".",plot_format), addres[[x]],
                                                 height = plotpars["h"], width = plotpars["w"], units = "in", pointsize = plotpars["psize"],
                                                 dpi = plotpars["dpi"], device = plot_format))
  }

  #Export data into a .CSV
  if(!silent) cat("\nExporting data...")

  #Title
  cat("SOLUTE: ", solute_ids["Name"], " (", solute_ids["MF"], "; CAS: ", solute_ids["CAS"], "; MW: ",  round(as.numeric(solute_ids["MW"]),2), ")", file = exfile, sep="", append = TRUE)

  #GCM group contributions
  if(is.list(pares)) {

    cat("\nGCM FRAGMENTATION RESULTS", file = exfile, append = TRUE)
    for(i in seq_along(contrib_list)) {
      cat("\n", names(contrib_list)[i], "\n", file = exfile, append = TRUE)
      contrib_list[[i]][grep("^=.*", contrib_list[[i]][,"Group"]), "Group"] <- paste0("'", contrib_list[[i]][grep("^=.*", contrib_list[[i]][,"Group"]), "Group"]) #Add an "'" to rows beginning with "=" to avoid problems with displaying in Excel
      fwrite(contrib_list[[i]], file = exfile, na = "",  sep = fwsep, col.names = TRUE, append = TRUE)
    }
    cat("\n", file = exfile, append = TRUE)

    #GCM calculation results (Tb, critical parameters)
    cat("\nBP AND CRITICAL PARAMETER CALCULATION RESULTS FROM GCM\n", file = exfile, append = TRUE)
    cat(names(crit_vec), file = exfile, sep = fwsep, append = TRUE)
    cat("\n", file = exfile, append = TRUE)
    cat(crit_vec, file = exfile, sep = fwsep, append = TRUE)
    cat("\n", file = exfile, append = TRUE)

    #GCM calculation results (HSP)
    cat("\nHANSEN SOLUBILITY PARAMETER (HSP) CALCULATION RESULTS FROM GCM\n", file = exfile, append = TRUE)
    cat(names(hsp_vec), file = exfile, sep = fwsep, append = TRUE)
    cat("\n", file = exfile, append = TRUE)
    cat(hsp_vec, file = exfile, sep = fwsep, append = TRUE)
    cat("\n", file = exfile, append = TRUE)

    #Methods used for GCM
    cat("\nGCM METHODS UTILIZED\n", file = exfile, append = TRUE)
    cat(names(method_vec), file = exfile, sep = fwsep, append = TRUE)
    cat("\n", file = exfile, append = TRUE)
    cat(method_vec, file = exfile, sep = fwsep, append = TRUE)
    replicate(2, cat("\n", file = exfile, append = TRUE))
  } else cat("\nGCM-BASED ESTIMATION OF BOILING POINT AND OTHER PARAMETERS WAS NOT CARRIED OUT.\n", file = exfile, append = TRUE)

  #Retrieve and format SFE optimization data for exporting
  if(is.list(optres)) {

    for(i in seq_along(hsp_list)) {
      hsp_list[[i]] <- hsp_list[[i]][1,]
      colnames(hsp_list[[i]]) <- paste0(colnames(hsp_list[[i]]), " degC")
    }

    for(i in seq_along(blend_list)) {
      for(j in seq_along(blend_list[[i]])) {
        colnames(blend_list[[i]][[j]]) <- paste0(colnames(blend_list[[i]][[j]]), " degC")
        blend_list[[i]][[j]] <- cbind.data.frame("'"=paste0(rownames(blend_list[[i]][[j]]), " bar"), blend_list[[i]][[j]])
      }
    }

    for(i in seq_along(ra_list)) {
      colnames(ra_list[[i]]) <- paste0(colnames(ra_list[[i]]), " degC")
      ra_list[[i]] <- cbind.data.frame("'"=paste0(rownames(ra_list[[i]]), " bar"), ra_list[[i]])
    }

    for(i in seq_along(miscib_list)) {
      colnames(miscib_list[[i]]) <- paste0(colnames(miscib_list[[i]]), " degC")
      miscib_list[[i]] <- cbind.data.frame("'"=paste0(rownames(miscib_list[[i]]), " bar"), miscib_list[[i]])
    }

    if(length(modif_vec)>1) {
      miscib_best <- optres[["Best_Modifier"]]
      miscib_rank <- optres[["Modifier_Ranking"]]
      colnames(miscib_best) <- paste0(colnames(miscib_best), " degC")
      miscib_best <- cbind.data.frame("'"=paste0(rownames(miscib_best), " bar"), miscib_best)
    }

    #Export SFE optimization data
    cat("\nSFE MODIFIER OPTIMIZATION RESULTS", file = exfile, append = TRUE)
    cat("\nSELECTION OF THE BEST scCO2 SFE MODIFIER AMONGST: ", paste0(modif_vec, collapse = ", "), file = exfile, append = TRUE)

    cat("\nSOLUTE HSP VERSUS TEMPERATURE\n", file = exfile, append = TRUE)
    for(i in seq_along(hsp_list)) {
      cat(names(hsp_list)[i], "\n", file = exfile, sep = "", append = TRUE)
      fwrite(hsp_list[[i]], file = exfile, na = "", sep = fwsep, col.names = TRUE, append = TRUE)
      cat("\n", file = exfile, append = TRUE)
    }

    cat("\nCO2-MODIFIER BLEND HSP VERSUS TEMPERATURE AND PRESSURE (MODIFIER VOL. FRACTION: ", vfrac, ")", file = exfile, sep = "", append = TRUE)
    for(i in seq_along(blend_list)) {
      cat("\n", names(blend_list)[i], file = exfile, sep = "", append = TRUE)

      for(j in seq_along(blend_list[[i]])) {
        cat("\n", names(blend_list[[i]])[j], "\n", file = exfile, append = TRUE)
        fwrite(blend_list[[i]][[j]], file = exfile, na = "", sep = fwsep, col.names = TRUE, append = TRUE)
      }
      cat("\n", file = exfile, append = TRUE)
    }

    cat("\n", file = exfile, append = TRUE)
    cat("\nHSP DISTANCE Ra BETWEEN SOLUTE AND SOLVENT BLENDS (LOWER = BETTER MISCIBILITY)", file = exfile, sep = fwsep, append = TRUE)

    for(i in seq_along(ra_list)) {
      cat("\n", names(ra_list)[i], "\n", file = exfile, sep = "", append = TRUE)
      fwrite(ra_list[[i]], file = exfile, na = "", sep = fwsep, col.names = TRUE, append = TRUE)
    }

    cat("\n", file = exfile, append = TRUE)
    cat("\nMISCIBILITY ENHANCEMENT (%) WITH DIFFERENT SOLVENT BLENDS (HIGHER = BETTER)", file = exfile, append = TRUE)

    for(i in seq_along(miscib_list)) {
      cat("\n", names(miscib_list)[i], "\n", file = exfile, sep = "", append = TRUE)
      fwrite(miscib_list[[i]], file = exfile, na = "", sep = fwsep, col.names = TRUE, append = TRUE)
    }
    cat("\n", file = exfile, append = TRUE)

    if(length(modif_vec)>1) {
      cat("\nBEST SOLVENT OUT OF SELECTION: ", paste0(modif_vec, collapse = ", "), file = exfile, sep = "", append = TRUE)
      cat("\n", file = exfile, append = TRUE)
      #cat("\n", names(miscib_best)[i], "\n", file = exfile, sep = fwsep, append = TRUE)
      fwrite(miscib_best, file = exfile, na = "", sep = fwsep, col.names = TRUE, append = TRUE)

      cat("\nSOLVENT RANKING (HIGHER = BETTER)\n", file = exfile, append = TRUE)
      cat(names(miscib_rank), file = exfile, sep = fwsep, append = TRUE)
      cat("\n", file = exfile, append = TRUE)
      cat(miscib_rank, file = exfile, sep = fwsep, append = TRUE)
    }
  } else cat("\nSFE MODIFIER OPTIMIZATION WAS NOT CARRIED OUT.", file = exfile, append = TRUE)
  if(!silent) cat("\nDONE!")
}
