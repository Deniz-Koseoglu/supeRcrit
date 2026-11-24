#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Data exporting function for supercritical CO2 co-solvent solubility-screening results
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Export results of co-solvent solubility optimisation
#'
#' @description Exports the output of function \code{\link{solub_optim}} as a \strong{single},
#' well-formatted, tab-delimited \strong{.tab} file gathering every result table into one document:
#' the screening conditions, the co-solvent ranking, the best-co-solvent map, the pure-CO2 baseline and,
#' for each evaluated co-solvent (and blend), the predicted solubility and enhancement grids over the
#' pressure/temperature domain. It is the solubility-mode counterpart of \code{\link{hsp_export}} and
#' works with every solubility engine (Makarov ML, CSP-2021 and openCOSMO-RS-Phi / COSMO).
#'
#' @param input The output of \code{\link{solub_optim}}, or the app-side \code{rv$sol_scan} list that
#' wraps it (a list with a \code{$scan} element plus \code{$ranking}/\code{$blend_ranking}). Both are
#' accepted: when a \code{$scan} element is present it is treated as the \code{solub_optim} output and
#' the sibling \code{$ranking}/\code{$blend_ranking} tables are exported alongside it.
#' @param solute An optional \strong{named} character vector describing the solute, recognising the names
#' \code{"Name"}, \code{"SMILES"}, \code{"CAS"}, \code{"MW"} and \code{"Tm"}. Missing entries are omitted
#' from the header. Defaults to \code{NULL}.
#' @param engine An optional \code{character} label of the prediction engine used (e.g.
#' \code{"openCOSMO-RS-Phi"}), printed in the screening-conditions block. Defaults to \code{NULL}.
#' @param cosolvent_labels An optional \strong{named} \code{character} vector mapping the internal
#' co-solvent identifiers (the names of \code{input$scan$enhancement} and the entries of
#' \code{input$scan$best}) to human-readable display labels. Identifiers absent from the mapping are
#' printed verbatim. Defaults to \code{NULL} (identifiers used as-is).
#' @param expath The export directory as a \code{character} value. The path must exist on the system.
#' Defaults to \code{getwd()}.
#' @param silent A \code{logical}. When \code{FALSE} (default), progress information is printed to the
#' console.
#'
#' @return Invisibly returns the full path of the single \strong{.tab} file written into \code{expath}.
#' @export
#'
#' @seealso \code{\link{solub_optim}}, \code{\link{hsp_export}}
#'
#' @importFrom data.table fwrite
#'
solub_export <- function(input, solute = NULL, engine = NULL, cosolvent_labels = NULL,
                         expath = getwd(), silent = FALSE) {

  #Set the delimiter
  fwsep <- "\t"

  #Preliminary checks
  if (!dir.exists(expath)) stop("The output directory does not exist!")
  if (is.null(input) || !is.list(input)) stop("'input' must be the output of function 'solub_optim()'!")
  scan <- if (!is.null(input[["scan"]])) input[["scan"]] else input
  req_el <- c("pure", "solubility", "enhancement", "best", "domain", "temp_C", "pres_bar")
  if (!all(req_el %in% names(scan)))
    stop("The 'input' data must be output from function 'solub_optim()' (or an 'rv$sol_scan' list)!")
  ranking <- input[["ranking"]]
  blend_ranking <- input[["blend_ranking"]]

  #Map an internal co-solvent identifier to a display label (identity when no mapping supplied)
  disp <- function(k) {
    k <- as.character(k)
    if (is.na(k) || !nzchar(k)) return(k)
    if (identical(k, "CO2")) return("None (pure CO2)")
    if (!is.null(cosolvent_labels) && k %in% names(cosolvent_labels)) unname(cosolvent_labels[[k]]) else k
  }

  #Solute identity fields (all optional)
  sid <- function(f) if (!is.null(solute) && f %in% names(solute) && !is.na(solute[[f]]) && nzchar(as.character(solute[[f]]))) as.character(solute[[f]]) else NA_character_
  sname <- sid("Name"); if (is.na(sname)) sname <- "solute"

  #Format a pressure x temperature grid into a labelled, exportable data.frame
  fmt_grid <- function(m, digfun = NULL) {
    if (is.null(m) || !length(m)) return(NULL)
    df <- as.data.frame(m, check.names = FALSE, stringsAsFactors = FALSE)
    if (!is.null(digfun)) df[] <- lapply(df, function(col) if (is.numeric(col)) digfun(col) else col)
    colnames(df) <- paste0(colnames(m), " \u00b0C")
    lab <- data.frame(rownames(m), stringsAsFactors = FALSE); names(lab) <- "Pressure (bar)"
    cbind(lab, df)
  }

  #Create the single output file
  safe <- gsub("[^A-Za-z0-9._-]+", "_", sname)
  exfile <- paste0(expath, "/", safe, "_CoSolvent_Solubility_Results.tab")
  if (!silent) cat("\nExport file: ", exfile, "...", sep = "")

  #~~ Header: solute identity ~~
  hdr <- paste0("SOLUTE: ", sname)
  extra <- c(if (!is.na(sid("SMILES"))) paste0("SMILES: ", sid("SMILES")),
             if (!is.na(sid("CAS")))    paste0("CAS: ", sid("CAS")),
             if (!is.na(sid("MW")))     paste0("MW: ", round(as.numeric(sid("MW")), 2)),
             if (!is.na(sid("Tm")))     paste0("Tm: ", round(as.numeric(sid("Tm")), 2), " K"))
  if (length(extra)) hdr <- paste0(hdr, " (", paste(extra, collapse = "; "), ")")
  cat(hdr, file = exfile, append = FALSE)

  #~~ Screening conditions ~~
  cat("\n\nSCREENING CONDITIONS\n", file = exfile, append = TRUE)
  xfrac <- if (!is.null(input[["x"]])) input[["x"]] else scan[["cosolvent_x"]]
  n_out <- if (!is.null(input[["n_out"]])) input[["n_out"]]
           else if (!is.null(scan[["domain"]])) sum(!scan[["domain"]], na.rm = TRUE) else NA
  cond <- c(if (!is.null(engine)) paste0("Prediction engine\t", engine),
            paste0("Temperature grid (\u00b0C)\t", paste(scan[["temp_C"]], collapse = "; ")),
            paste0("Pressure grid (bar)\t", paste(scan[["pres_bar"]], collapse = "; ")),
            if (!is.null(xfrac)) paste0("Co-solvent mole fraction\t", xfrac),
            paste0("Co-solvents screened\t", paste(vapply(names(scan[["enhancement"]]), disp, character(1)), collapse = "; ")),
            if (!is.na(n_out)) paste0("Grid points outside applicability domain\t", n_out))
  cat(paste(cond, collapse = "\n"), file = exfile, append = TRUE)

  #~~ Co-solvent ranking ~~
  if (!is.null(ranking) && nrow(ranking)) {
    rk <- data.frame(
      `Co-solvent` = vapply(ranking[["Modifier"]], disp, character(1)),
      `Mean enhancement (x)` = round(as.numeric(ranking[["MeanEnh"]]), 3),
      `Max enhancement (x)`  = round(as.numeric(ranking[["MaxEnh"]]), 3),
      `Mean solubility (mole fraction)` = signif(as.numeric(ranking[["MeanSol"]]), 4),
      check.names = FALSE, stringsAsFactors = FALSE)
    cat("\n\nCO-SOLVENT RANKING (higher mean enhancement = better)\n", file = exfile, append = TRUE)
    fwrite(rk, file = exfile, na = "", sep = fwsep, col.names = TRUE, append = TRUE)
  }

  #~~ Best co-solvent at each condition ~~
  if (!is.null(scan[["best"]])) {
    best_disp <- scan[["best"]]
    best_disp[] <- vapply(as.vector(scan[["best"]]), disp, character(1))
    cat("\n\nBEST CO-SOLVENT AT EACH CONDITION (highest enhancement; pressure bar \u00d7 temperature \u00b0C)\n",
        file = exfile, append = TRUE)
    fwrite(fmt_grid(best_disp), file = exfile, na = "", sep = fwsep, col.names = TRUE, append = TRUE)
  }

  #~~ Pure-CO2 baseline solubility ~~
  if (!is.null(scan[["pure"]])) {
    cat("\n\nPURE scCO2 SOLUBILITY BASELINE (mole fraction; pressure bar \u00d7 temperature \u00b0C)\n",
        file = exfile, append = TRUE)
    fwrite(fmt_grid(scan[["pure"]], function(v) signif(v, 4)), file = exfile, na = "", sep = fwsep,
           col.names = TRUE, append = TRUE)
  }

  #~~ Per-co-solvent solubility grids ~~
  if (length(scan[["solubility"]])) {
    cat("\n\nSOLUBILITY WITH EACH CO-SOLVENT (mole fraction; pressure bar \u00d7 temperature \u00b0C)",
        file = exfile, append = TRUE)
    for (nm in names(scan[["solubility"]])) {
      cat("\n\n", disp(nm), "\n", file = exfile, sep = "", append = TRUE)
      fwrite(fmt_grid(scan[["solubility"]][[nm]], function(v) signif(v, 4)), file = exfile, na = "",
             sep = fwsep, col.names = TRUE, append = TRUE)
    }
  }

  #~~ Per-co-solvent enhancement grids ~~
  if (length(scan[["enhancement"]])) {
    cat("\n\nSOLUBILITY ENHANCEMENT WITH EACH CO-SOLVENT (\u00d7 relative to pure CO2; pressure bar \u00d7 temperature \u00b0C)",
        file = exfile, append = TRUE)
    for (nm in names(scan[["enhancement"]])) {
      cat("\n\n", disp(nm), "\n", file = exfile, sep = "", append = TRUE)
      fwrite(fmt_grid(scan[["enhancement"]][[nm]], function(v) round(v, 3)), file = exfile, na = "",
             sep = fwsep, col.names = TRUE, append = TRUE)
    }
  }

  #~~ Applicability domain ~~
  if (!is.null(scan[["domain"]])) {
    dom <- scan[["domain"]]; dom[] <- ifelse(as.vector(scan[["domain"]]), "In-domain", "Out-of-domain")
    cat("\n\nAPPLICABILITY DOMAIN (whether each condition is within the model's reliable range)\n",
        file = exfile, append = TRUE)
    fwrite(fmt_grid(dom), file = exfile, na = "", sep = fwsep, col.names = TRUE, append = TRUE)
  }

  #~~ Blend ranking (optional) ~~
  if (!is.null(blend_ranking) && nrow(blend_ranking)) {
    bk <- data.frame(
      `Solvent blend` = vapply(blend_ranking[["Modifier"]], disp, character(1)),
      `Mean enhancement (x)` = round(as.numeric(blend_ranking[["MeanEnh"]]), 3),
      `Max enhancement (x)`  = round(as.numeric(blend_ranking[["MaxEnh"]]), 3),
      `Mean solubility (mole fraction)` = signif(as.numeric(blend_ranking[["MeanSol"]]), 4),
      check.names = FALSE, stringsAsFactors = FALSE)
    cat("\n\nSOLVENT BLEND RANKING (higher mean enhancement = better)\n", file = exfile, append = TRUE)
    fwrite(bk, file = exfile, na = "", sep = fwsep, col.names = TRUE, append = TRUE)
  }

  cat("\n", file = exfile, append = TRUE)
  if (!silent) cat("\nDONE!")
  invisible(exfile)
}
