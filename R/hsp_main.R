#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Main workflow for co-solvent evaluation for SFE
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Compare HSP-based utility of various co-solvents for SFE
#'
#' @description Uses group contribution methods to estimate the boiling point, critical temperature, and/or Hansen Solubility Parameters
#' of a solute given its SMILES string and MOL file (for example, output from \code{\link{mol_find}}). The estimated parameters are then
#' used to calculate \strong{solubility parameter distance \eqn{R_a}} between the solute and both pure carbon dioxide and its volumetric
#' mixture with a given co-solvent. These distances may be compared for various co-solvents to determine the optimal solvent throughout
#' a range of supercritical CO2 pressures and temperatures.
#'
#' @param solute Information about the solute as required by function \code{\link{mol_find}}.
#' @param tb,crit,hsp All are \code{character} values specifying the methods to use for GCMs (see \code{\link{est_gcm}}).
#' @param gorder A \code{numeric} value denoting the maximum order of groups to be considered. Defaults to \code{0}, which
#' considers all available group orders. May also be a \strong{named} vector - see \code{\link{est_gcm}} for details.
#' @param modif A \code{character} value, vector, or \code{list} indicating which co-solvent(s) to evaluate alongside pure CO2.
#' Each element of vector must be either a solvent \strong{name} or its \strong{abbreviation}. For possible values,
#' consult the output of \code{\link{show_solv}}. \strong{Mixtures of co-solvents} may
#' also be evaluated if a \code{list} is provided, where a mixture may be specified in each list element as a vector of length 2
#' or more containing the \strong{names of solvents}.
#' @param modfracs An \strong{optional} \code{list} of solvent mixture volume percentages to use for solvent mixtures specified in
#' \code{modif}. Percentages given in each list element must either be less than or add up to 100 and contain an equal number of
#' percentage values to the number of solvents in the corresponding mixture (or one less value, in which case the remainder is
#' made up to a total of 100). The total number of list elements must equal the number of mixtures specified in \code{modif}.
#' @param hlight A \code{logical} indicating whether substructures are highlighted (see \code{\link{plot_gcm}}).
#' @param simplicity A \code{character} string indicating whether overlapping sub-structures should be allowed (\code{"auto"} by default).
#' See \code{\link{sub_smarts}}.
#' @param pres,temps Both are \code{numeric} values \strong{or sequences} of pressures (75-1000 bar) and temperatures (32-200 Celsius)
#' at which to evaluate solute and solvent HSPs. Defaults are \code{seq(80,300,20)} and \code{seq(32,65,3)} for pressure and
#' temperature, respectively.
#' @param vfrac The volume fraction of co-solvent to use (defaults to \code{0.10}, or 10%).
#' @param silent A \code{logical}. When \code{FALSE} (default), additional information is printed in the console.
#'
#' @details
#' The workflow is based on various group contribution methods (see \code{\link{est_gcm}} for relevant sources), the
#' Hansen Solubility Theory (Hansen, 2007), and the work of Tirado et al. (2018, 2019) and Diego & Calvo (2019). Given the SMILES string
#' and molecular geometry (MOL file) of a solute, the boiling point, critical temperature, and Hansen Solubility Parameters
#' (HSPs) are estimated via GCMs. For pure CO2 and co-solvents, these values as well as the molar volume (mL/mol)
#' were compiled from CoolProp (Bell et al., 2014). The influence of temperature on \strong{solute} HSPs is then calculated
#' from \strong{reduced temperatures} via the following equation where \eqn{T_{r2}} and \eqn{T_{r1}} are given by
#' \eqn{T_{actual}/T_{critical}} at the desired temperature and 298.15 K, respectively:
#' \deqn{\delta_2 = (\frac{1-T_{r2}}{1-T_{r1}})^{0.34}\times\delta_1}
#' The effects of temperature on dispersion (\eqn{\delta_d}), polarity (\eqn{\delta_p}), and hydrogen bonding (\eqn{\delta_{HB}})
#' HSPs are calculated via the following equations:
#' \deqn{\delta_{d\text{ }ref}/\delta_d = (V_{ref}/V)^{-1.25}}
#' \deqn{\delta_{p\text{ }ref}/\delta_p = (V_{ref}/V)^{-0.5}}
#' \deqn{\delta_{HB\text{ }ref}/\delta_{HB} = exp[-1.32\times10^{-3}\times(T_{ref}-T)-ln(V_{ref}/V)^{-0.5}]}
#' The HSP distance \eqn{R_a} between the solvent (pure CO2 and/or CO2+co-solvent mixture, subscript 1) and solute
#' (subscript 2) is then derived using:
#' \deqn{R_a = \sqrt{4\times(\delta_{d1} - \delta_{d2})^2 + (\delta_{p1} - \delta_{p2})^2 + (\delta_{HB1} - \delta_{HB2})^2}}
#' The above calculations are repeated for every combination of pressure and temperature. The miscibility enhancement (\eqn{ME}, in %)
#' is finally derived as the percentage ratio between the solute \eqn{R_a} to that of CO2+co-solvent and pure CO2:
#' \deqn{ME\text{ }(%) = (1-(R_{a\text{ }scCO_2\text{ }+\text{ }co-solvent}/R_{a\text{ }pure\text{ }scCO_2}))\times 100}
#' Marking the miscibility enhancement values at all combinations of pressure and temperature allows the determination of the best-suited
#' co-solvent for any given solute.
#'
#' @references
#' Bell, Ian H., Wronski, Jorrit, Quoilin, Sylvain, Lemort, Vincent (2014), 'Pure and Pseudo-pure Fluid Thermophysical Property Evaluation and the Open-Source Thermophysical Property Library CoolProp', \emph{Industrial & Engineering Chemistry Research} \strong{53} (6), pp. 2498-2508, DOI: \url{https://doi.org/10.1021/ie4033999}.
#'
#' Hansen, Charles M. (2007), \emph{Hansen Solubility Parameters: A User's Handbook (2nd edition)}, CRC Press, London, United Kingdom.
#'
#' Tirado, Diego F., Tenorio, Maria Jose, Cabanas, Albertina, Calvo, Lourdes (2018), 'Prediction of the best cosolvents to solubilise fatty acids in supercritical CO2 using the Hansen solubility theory', \emph{Chemical Engineering Science} \strong{190}, pp. 14-20, DOI: \url{https://www.doi.org/10.1016/j.ces.2018.06.017}.
#'
#' Tirado, Diego F., Rousset, Amandine, Calvo, Lourdes (2019), 'The Selective Supercritical Extraction of High-value Fatty Acids from Tetraselmis suecica using the Hansen Solubility Theory', \emph{Chemical Engineering Transactions} \strong{75}, pp. 133-138, DOI: \url{https://www.doi.org/10.3303/CET1975023}.
#'
#' Tirado, Diego F., Calvo, Lourdes (2019), 'The Hansen theory to choose the best cosolvent for supercritical CO2 extraction of beta-carotene from \emph{Dunaliella salina}', \emph{The Journal of Supercritical Fluids} \strong{145}, pp. 211-218, DOI: \url{https://www.doi.org/10.1016/j.supflu.2018.12.013}.
#'
#' @return A named \code{list} with the following elements:
#' \describe{
#' \item{solute_ids}{Solute information as retrieved by \code{\link{mol_find}}.}
#' \item{parameters}{Solute parameters (boiling point, critical parameters, and HSPs)
#'  estimated by GCM via function \code{\link{est_gcm}}.}
#' \item{sfe}{Results of miscibility enhancement assessment as output from \code{\link{hsp_optim}}.}
#' \item{gcm_vis}{A \code{list} of raster visualisation included as part of \code{\link{est_gcm}} output.}
#' \item{call}{The function \code{call}.}
#' }
#' @export
#'
#' @examples
#' #Retrieve molecule (beta-carotene)
#' mol <- c("CC1=C(C(CCC1)(C)C)C=CC(=CC=CC(=CC=CC=C(C)C=CC=C(C)C=CC2=C(CCCC2(C)C)C)C)C",
#' "7235-40-7", "Beta-carotene")
#'
#' #Assess SFE miscibility enhancement with various co-solvents
#' optres <- sfe_mod(solute = mol,
#' tb = "SB_corr",
#' crit = "NL07_robust",
#' hsp = "SP12")
#'
#' @seealso \code{\link{mol_find}}, \code{\link{est_gcm}}, \code{\link{plot_gcm}}, \code{\link{hsp_optim}}, \code{\link{miscomp}}, \code{\link{compare_gcm}}, \code{\link{show_solv}}
sfe_mod <- function(solute, tb, crit, hsp, gorder = 0, modif = "all", modfracs = NA, hlight = TRUE, simplicity = "auto",
                    pres = seq_last(80, 400, 20), temps = seq_last(32, 70, 3), vfrac = 0.10,
                    silent = FALSE) {

  #Record function call for later exporting
  cl_rec <- match.call()

  ##Preliminary checks
  #if(any(modif %in% "none")) stop("The function has no task to carry out! No SFE modifiers were set...")

  #Estimate boiling points, critical parameters, and HSP via GCM
  gcmres <- est_gcm(solute, tb, crit, hsp, vdw = NA, hlight, simplicity, gorder, silent)

  #Unpack results and get parameters for HSP-based miscibility optimization
  solute_data <- gcmres[["solute_data"]]
  pares <- gcmres[["pares"]]
  visres <- gcmres[["visres"]]
  crit_foropt <- pares[["Critical"]][["Tc"]]
  hsp_foropt <- pares[["HSP"]]

  #Use the critical temperature Tc and HSP parameters of solute to obtain miscibility enhancement in various CO2-Cosolvent mixtures versus pure CO2
  if(!any(modif %in% "none")) {
    optres <- hsp_optim(crit_foropt, hsp_foropt, modif, modfracs, pres, temps, vfrac, silent = silent)
  } else optres <- NA

  #Return results
  return(list(solute_ids = solute_data[["IDs"]], parameters = pares, sfe = optres, gcm_vis = visres, call = cl_rec))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Compare miscibility enhancements with two or more compounds
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Compare HSP-based miscibility enhancement for two or more solutes
#'
#' @description Uses the \code{\link{sfe_mod}} framework to visually compare the miscibility enhancement provided by a chosen co-solvent
#' for at least two solutes at various pressures (up to 6) and a set temperature.
#'
#' @param sols A \code{list} where each element provides information for a solute as required by \code{\link{mol_find}}.
#' @param tb,crit,hsp All \code{character} values of GCM methods to use. For available methods, see \code{\link{est_gcm}}.
#' @param modif A \code{character} value or vector of the co-solvent (or solvent blend) to use.
#' For possible values, see \code{\link{sfe_mod}}.
#' @param modfracs A vector or \code{list} of co-solvent volume fractions to use when a \strong{solvent blend} is specified
#' in \code{modif}.
#' @param pres A \code{numeric} vector of \strong{up to 6} pressures (in bar) to evaluate miscibility enhancement at.
#' @param pres_comp A \code{numeric} pressure value to use for comparative plots between solutes. Must also be present in \code{pres}.
#' @param cols Either \code{"default"} or a \strong{named} vector of colours to use for up to six values of pressure (given in \code{pres}).
#' Possible names are \code{"one"}, \code{"two"}, \code{"three"}, \code{"four"}, \code{"five"}, and/or \code{"six"}.
#' @param plt_title A \code{logical} indicating whether titles should be added to plots.
#' @param temp The single \code{numeric} temperature value to use (between 40-70 Celsius).
#' @param vfrac A \code{numeric} vector of volume fractions of co-solvent to evaluate. Defaults to \code{seq(0.05, 0.40, 0.05)}.
#' @param simplicity A \code{character} string indicating whether overlapping sub-structures should be allowed (\code{"auto"} by default).
#' See \code{\link{sub_smarts}}.
#' @param draw A \code{logical} indicating whether generated plots should be printed automatically (\code{TRUE} by default).
#' @param silent A \code{logical}. When \code{FALSE} (default), additional information is printed in the console.
#'
#' @return A named \code{list} containing the output \code{data.frame} (\code{$data}) with miscibility enhancement
#' at all values of pressure and co-solvent volume fractions, and corresponding visualizations (\code{$plots}).
#' @export
#'
#' @examples
#' #Get information about solute 1 (beta-carotene)
#' mol1 <- c("CC1=C(C(CCC1)(C)C)C=CC(=CC=CC(=CC=CC=C(C)C=CC=C(C)C=CC2=C(CCCC2(C)C)C)C)C",
#' "7235-40-7", "Beta-carotene")
#'
#' #Get information about solute 2 (xanthohumol)
#' mol2 <- c(hspex[24,c("SMILES", "CAS", "Name")])
#'
#' #Compare miscibility enhancement with ethanol as a co-solvent
#' gcm_comp <- miscomp(sols = list(mol1, mol2))
#'
#' @seealso \code{\link{sfe_mod}}
#'
#' @importFrom webchem is.smiles
#' @importFrom scales breaks_pretty
#' @import ggplot2
#'
miscomp <- function(sols, tb = "SB_corr", crit = "NL07_robust", hsp = "SP12",
                    modif = "Ethanol", modfracs = NA, pres = seq(100,600,100), pres_comp = pres, cols = "default",
                    plt_title = TRUE, temp = 40, vfrac = seq(0.05, 0.40, 0.05), simplicity = "auto",
                    draw = TRUE, silent = FALSE) {

  #Preliminary checks
  if(!is.list(sols)) {
    if(!any(sapply(sols, webchem::is.smiles))) stop("The input 'sols' must include a SMILES string!") else sols <- list(sols)
  } else {
    if(suppressWarnings(any(sapply(sols, function(x) !any(sapply(x, webchem::is.smiles)))))) stop("Each element of the input ('sols') must include a SMILES string!")
  }

  if(!is.numeric(tb) & !is.list(tb)) tb <- as.list(rep(tb,length(sols))) else if(is.list(tb) & length(tb)!=length(sols)) stop("The length of 'tb' must equal that of 'sols'!")
  if(!is.numeric(crit) & !is.list(crit)) crit <- as.list(rep(crit,length(sols))) else if(is.list(crit) & length(crit)!=length(sols)) stop("The length of 'crit' must equal that of 'sols'!")
  if(!is.numeric(hsp) & !is.list(hsp)) hsp <- as.list(rep(hsp,length(sols))) else if(is.list(hsp) & length(hsp)!=length(sols)) stop("The length of 'hsp' must equal that of 'sols'!")

  if(length(sols)==1 & length(pres)==1) stop("At least one input vector/list among pressure ('pres') and/or solutes ('sols') must be high than 1 in length!")
  if(length(plt_title)==1) plt_title <- rep(plt_title,2) else if(length(plt_title)>2) stop("The length of 'plt_title' must be either 1 or 2!")
  if(length(vfrac)<2) stop("At least two volume fractions must be included in 'vfrac' for visualization!")
  if(temp<40 & temp>70|length(temp)!=1) stop("The temperature parameter 'temp' must be a single numeric value between 40 and 70!")
  if(length(sols)>6) stop("The maximum number of solutes to be compared ('sols') is currently limited to 6!")
  if(length(pres)>6|!is.numeric(pres)) stop("The pressure parameter 'pres' must be a numeric vector of length 1-6!")
  if(!all(pres_comp %in% pres)) stop("The pressure(s) used for solute comparison ('pres_comp') must also be included in 'pres'!")
  defmod <- unname(unlist(show_solv()[,c("Solvent","Abbreviation")]))
  if(!all(modif %in% defmod)) {
    stop(paste0("The modified ('modif') must be ONE OR MORE (if using a solvent blend) of the following: ",
                paste0("'", defmod, "'", collapse = ", "), "!"))
  } else if(length(modif)>1) {
    if(!is.list(modif)) modif <- list(modif)
    if(!is.list(modfracs)) modfracs <- list(modfracs)
  }

  #Set up plot colours
  defcols <- c(one = "black", two = "blue", three = "darkgreen", four = "darkorange", five = "darkred", six = "purple")
  cols <- supp_pars(pars = cols, defpars = defcols, parlb = "cols")

  #Calculate or compile boiling points, critical parameters, and HSPs for all solutes
  #Also calculate and compile Ra and miscibility enhancements for all solutes at each volume fraction of the chosen modifier
  miscib <- as.data.frame(matrix(ncol = 3, nrow = 0))
  for(i in seq_along(sols)) {
    if(!silent) cat("\nWorking on compound", i, "of", length(sols),"...")
    if(!silent) cat("\nCalculating HSPs and other parameters...")
    gcmpars <- est_gcm(sols[[i]], tb[[i]], crit[[i]], hsp[[i]], vdw = NA, simplicity = simplicity, silent = TRUE)
    soldata <- gcmpars[["solute_data"]][["IDs"]]
    crit_opt <- gcmpars[["pares"]][["Critical"]][["Tc"]]
    hsp_opt <- gcmpars[["pares"]][["HSP"]]

    cat("\nCalculating miscibility enhancement...")
    optres <- lapply(vfrac, function(x) {
      res <- hsp_optim(crit_opt, hsp_opt, modif, modfracs, pres, temp, vfrac = x, silent = TRUE)[["Miscib_Enhancement"]][[1]]
      res <- data.frame(rep(soldata[["Name"]],nrow(res)), rep(x, nrow(res)), as.character(rownames(res)), miscib_enh = res)
      return(res)
    })
    cat("\nCompiling data...")
    optres <- do.call(rbind.data.frame, optres)
    miscib <- rbind.data.frame(miscib, optres)
  }
  colnames(miscib) <- c("name", "vfrac", "pres", "miscib")
  rownames(miscib) <- NULL

  #Create plots of the results
  pltlst <- list()

  #Each solute at different pressures
  if(length(pres)>1) {
    for(i in unique(miscib[,"name"])) {
      pltlst[[i]] <- ggplot(data = miscib[miscib[,"name"]==i,], aes(x = .data[["vfrac"]], y = .data[["miscib"]],
                                                                    shape = .data[["pres"]], #grp = .data[["pres"]]
                                                                    col = .data[["pres"]])) +
        geom_line() + geom_point(size = 2) +
        labs(title = if(plt_title[1]) paste0("Miscibility enhancement of ", tolower(i), " (", min(pres), "-", max(pres), " bar) in ", tolower(modif)) else NULL,
             x = paste0(modif, " volume fraction"),
             y = "Miscibility enhancement (%)") +
        scale_x_continuous(breaks = scales::breaks_pretty(n = 6)) +
        scale_y_continuous(breaks = scales::breaks_pretty(n = 6)) +
        scale_colour_manual(values = unname(cols[1:length(pres)]),
                            labels = sort(unique(miscib[,"pres"])),
                            breaks = sort(pres)) +
        scale_shape_manual(values = c(0:2,5:7),
                           labels = sort(unique(miscib[,"pres"])),
                           breaks = sort(pres)) +
        theme(aspect.ratio = 1,
              panel.background = element_blank(),
              axis.text = element_text(colour = "black", size = 12),
              axis.title = element_text(colour = "black", size = 13),
              axis.line = element_line(colour = "black"),
              legend.title = element_blank(),
              legend.position = "right", #c(0.93, 0.15)
              legend.text = element_text(size = 11))
    }
  }

  #Multiple solutes at the same pressure(s)
  if(length(sols)>1) {
    for(i in seq_along(pres_comp)) {
      pltlst[[paste0("pres_",pres_comp[i])]] <- ggplot(data = miscib[miscib[,"pres"]==pres_comp[i],], aes(x = .data[["vfrac"]], y = .data[["miscib"]],
                                                                                                          grp = .data[["name"]], shape = .data[["name"]],
                                                                                                          col = .data[["name"]])) +
        geom_line() + geom_point(size = 2) +
        labs(title = if(plt_title[2]) paste0("Miscibility enhancement of various solutes in ", tolower(modif), " at ", pres_comp[i], " bar") else NULL,
             x = paste0(modif, " volume fraction"),
             y = "Miscibility enhancement (%)") +
        scale_x_continuous(breaks = scales::breaks_pretty(n = 6)) +
        scale_y_continuous(breaks = scales::breaks_pretty(n = 6)) +
        scale_colour_manual(values = unname(cols[1:length(sols)]),
                            labels = sort(unique(miscib[,"name"])),
                            breaks = sort(unique(miscib[,"name"]))) +
        scale_shape_manual(values = c(0:2,5:7),
                           labels = sort(unique(miscib[,"name"])),
                           breaks = sort(unique(miscib[,"name"]))) +
        theme(aspect.ratio = 1,
              panel.background = element_blank(),
              axis.text = element_text(colour = "black", size = 12),
              axis.title = element_text(colour = "black", size = 13),
              axis.line = element_line(colour = "black"),
              legend.title = element_blank(),
              legend.position = "right", #c(0.93, 0.15)
              legend.text = element_text(size = 11))
    }
  }

  return(list(data = miscib, plots = pltlst))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Compare the estimated values from various GCM methods
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Compare estimated parameters from various GCM methods
#'
#' @description Compares the boiling points, critical parameters (temperature, pressure, volume),
#' and/or Hansen Solubility Parameters (HSPs) estimated for a solute by various Group Contribution Methods (GCMs).
#'
#' @param solute A (optionally named) \code{character} vector of solute information as provided to \code{\link{mol_find}}.
#' @param gorder A \code{numeric} value or vector denoting the maximum order of groups to be considered.
#' Defaults to \code{0}, which considers all group orders. Must either be of \strong{length 1} or a \strong{named vector}
#' with names corresponding to available GCM methods (see \code{\link{est_gcm}} for a full list).
#' @param simplicity A \code{character} string indicating whether overlapping sub-structures should be allowed (\code{"auto"} by default).
#' See \code{\link{sub_smarts}}.
#'
#' @return A \code{data.frame} containing the type of estimated \code{parameter}, the \code{method} used,
#' the level of \code{simplicity} of matching GCM SMARTS substructures, and various estimated parameters including
#' boiling points (\code{"Tb"}, \code{"Tb_corr"}), critical temperature (\code{"Tc"}), pressure (\code{"Pc"}),
#' and volume (\code{"Vc"}), as well as dispersion (\code{"dD"}), polarity (\code{"dP"}, \code{"dP_low"}),
#' and hydrogen bonding (\code{"dHB"}, \code{"dHB_low"}) components of HSPs.
#'
#' @export
#'
#' @examples
#' #Limonene
#' mol <- c("CC1=CCC(CC1)C(=C)C", "5989-54-8", "Limonene")
#' res <- compare_gcm(mol)
#'
#' #Linoleic acid
#' mol2 <- c("CCCCCC=CCC=CCCCCCCCC(=O)O", "60-33-3", "Linoleic Acid")
#' res <- compare_gcm(mol2)
#'
#' @seealso \code{\link{mol_find}}, \code{\link{sfe_mod}}, \code{\link{sub_smarts}}
#'
compare_gcm <- function(solute, gorder = 0, simplicity = "auto") {
  if(length(gorder)>1 & is.null(names(gorder))) {
    stop("When group order 'gorder' is of length > 1, names corresponding to available GCM methods must be provided!")
  } else if(length(gorder)>1 & !all(names(gorder) %in% gcm_opts[,"Method"])) {
    stop(paste0("The following method names in 'gorder' were not recognized: ",
                paste0("'", names(gorder)[!names(gorder) %in% gcm_opts[,"Method"]], "'", collapse = ", "), "."))
  } else if(length(gorder)==1) gorder <- setNames(rep(gorder, nrow(gcm_opts)), gcm_opts[,"Method"])

  mets <- cbind.data.frame(parameter = c(rep("tb",7), rep("crit",5), rep("hsp",4), rep("vdw",3)),
                           method = c("JR", "JR_corr", "SB", "SB_corr", "NL04", "HKR_STW", "HKR_SIM",
                                      "JR", "NL07", "NL07_robust", "HKR_STW", "HKR_SIM",
                                      "SP08", "SP12",  "HKR_STW", "HKR_SIM",
                                      "BND", "SLON", "ZHAO"),
                           simplicity = c(rep(simplicity,19)))
  mets[,"order"] <- sapply(mets[,"method"], function(x) {
    input_order <- gorder[names(gorder) %in% x]
    cur_order <- gcm_opts[gcm_opts[,"Method"]==x,"Order"]
    if(cur_order < input_order | input_order==0) cur_order else input_order
  })

  solute <- mol_find(solute)
  for(i in seq(nrow(mets))) {
    cat(if(i==1)"\n" else "\r", "Estimating values using method: ", mets[i, "method"], "...", sep = "")
    par <- mets[i,"parameter"]
    grps <- define_grps(sub_smarts(solute, method = mets[i,"method"], simplicity = simplicity, gorder = gorder[names(gorder) %in% mets[i,"method"]], silent = TRUE))
    contribs <- gcm_contribs(solute, grps)
    if(par=="tb") {
      tbres <- gcm_bp(contribs)
      mets[i,"Tb"] <- tbres[["Tb"]]
      if(any(names(tbres) %in% "Tb_corr")) mets[i,"Tb_corr"] <- tbres[["Tb_corr"]]
    } else if(par=="crit") {
      tb_grps <- define_grps(sub_smarts(solute, method = "SB_corr", simplicity = simplicity, gorder = gorder[names(gorder) %in% mets[i,"method"]], silent = TRUE))
      tb_contribs <- gcm_contribs(solute, tb_grps)
      tb <- gcm_bp(tb_contribs)[["Tb_corr"]]
      mets[i,c("Tc","Pc","Vc")] <- unname(gcm_crit(tb, contribs)[c("Tc","Pc","Vc")])
    } else if(par=="hsp") {
      hspres <- gcm_hsp(contribs)
      mets[i,c("dD","dP","dHB")] <- unname(hspres[c("dD", "dP", "dHB")])
      for(j in c("dP_low", "dHB_low")) if(any(names(hspres) %in% j)) mets[i,"dP_low"] <- hspres[[j]]
    } else if(par=="vdw") {
      vdwres <- gcm_vdw(contribs)
      mets[i,"VDW"] <- vdwres[["VDW"]]
    }
  }
  return(mets)
}
