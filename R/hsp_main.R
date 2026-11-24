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
#' @param pres,temps Both are \code{numeric} values \strong{or sequences} of pressures (75-1000 bar) and temperatures (32-200 Celsius)
#' at which to evaluate solute and solvent HSPs. Defaults are \code{seq(80,300,20)} and \code{seq(32,65,3)} for pressure and
#' temperature, respectively.
#' @param vfrac The volume fraction of co-solvent to use (defaults to \code{0.10}, or 10%).
#' @param silent A \code{logical}. When \code{FALSE} (default), additional information is printed in the console.
#' @param crit_value,hsp_values Optional experimental override. When both are supplied — a measured critical
#' temperature \code{crit_value} (in K) and Hansen parameters \code{hsp_values} as \code{c(dD, dP, dHB)} —
#' these are used directly and the GCM estimation (\code{tb}/\code{crit}/\code{hsp}) is skipped. Used when a
#' compound's measured properties are loaded from the experimental database. \code{NULL} (default) uses GCM.
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
                    silent = FALSE, crit_value = NULL, hsp_values = NULL) {

  #Record function call for later exporting
  cl_rec <- match.call()

  ##Preliminary checks
  #if(any(modif %in% "none")) stop("The function has no task to carry out! No SFE modifiers were set...")

  # Experimental override (PARTIAL-friendly): use a measured critical temperature (crit_value, K) and/or
  # Hansen parameters (hsp_values = c(dD, dP, dHB)) directly when supplied, and fall back to the GCM ONLY
  # for the part(s) not provided. This must not be all-or-nothing: a compound can have experimental HSP
  # but no measured Tc (e.g. caffeine, whose experimental Tc is NA). In that case the GCM still runs for
  # the critical part, but a KNOWN-VALID placeholder method is used for any experimentally-supplied part
  # (its GCM result is discarded), so a non-GCM method string ("experimental"/"") carried in from the
  # database never reaches est_gcm — which would otherwise crash with "get1index" on the empty method.
  have_crit <- !is.null(crit_value) && isTRUE(is.finite(suppressWarnings(as.numeric(crit_value)[1])))
  have_hsp  <- !is.null(hsp_values) && length(hsp_values) >= 3 &&
               all(is.finite(suppressWarnings(as.numeric(hsp_values[1:3]))))
  if (have_crit && have_hsp) {
    # Fully experimental: no GCM decomposition needed.
    solute_data <- mol_find(solute)
    hsp_foropt  <- stats::setNames(as.numeric(hsp_values[1:3]), c("dD", "dP", "dHB"))
    crit_foropt <- as.numeric(crit_value)[1]
    pares  <- list(Critical = c(Tc = crit_foropt), HSP = hsp_foropt,
                   Methods = c(Tc = "experimental", HSP = "experimental"))
    visres <- NA
  } else {
    # GCM for whatever is NOT supplied experimentally; valid placeholder methods for any supplied part.
    gcmres <- est_gcm(solute, tb,
                      if (have_crit) "JR" else crit,
                      if (have_hsp)  "SP12" else hsp,
                      vdw = NA, hlight, simplicity, gorder, silent)
    solute_data <- gcmres[["solute_data"]]
    pares <- gcmres[["pares"]]
    visres <- gcmres[["visres"]]
    if (have_crit) {
      crit_foropt <- as.numeric(crit_value)[1]
      pares[["Critical"]][["Tc"]] <- crit_foropt
      if (!is.null(pares[["Methods"]])) pares[["Methods"]][["Tc"]] <- "experimental"
    } else crit_foropt <- pares[["Critical"]][["Tc"]]
    if (have_hsp) {
      hsp_foropt <- stats::setNames(as.numeric(hsp_values[1:3]), c("dD", "dP", "dHB"))
      pares[["HSP"]] <- hsp_foropt
      if (!is.null(pares[["Methods"]])) pares[["Methods"]][["HSP"]] <- "experimental"
    } else hsp_foropt <- pares[["HSP"]]
  }

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
#' The length of each element must either be 1 or equal to that of \code{sols}.
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
#' @param draw A \code{logical} indicating whether generated plots should be printed automatically (\code{TRUE} by default).
#' @param silent A \code{logical}. When \code{FALSE} (default), additional information is printed in the console.
#' @param crit_values,hsp_values Optional per-solute experimental overrides, each a \code{list} the same length
#' as \code{sols}. A non-\code{NULL} element supplies that solute's measured critical temperature (K) /
#' Hansen parameters \code{c(dD, dP, dHB)}, used directly instead of the GCM estimate (\code{NULL} elements
#' fall back to GCM). Used when comparing compounds loaded from the experimental database.
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
                    draw = TRUE, silent = FALSE, crit_values = NULL, hsp_values = NULL) {

  #Preliminary checks
  if(!is.list(sols)) {
    if(!any(sapply(sols, webchem::is.smiles))) stop("The input 'sols' must include a SMILES string!") else sols <- list(sols)
  } else {
    if(suppressWarnings(any(sapply(sols, function(x) !any(sapply(x, webchem::is.smiles)))))) stop("Each element of the input ('sols') must include a SMILES string!")
  }

  if(!is.numeric(tb) & !is.list(tb)) {
    tb <- if(length(tb)==1) as.list(rep(tb,length(sols))) else if(length(tb)==length(sols)) as.list(tb) else stop("The length of 'tb' must equal to that of 'sols'!")
  } else if(is.list(tb) & length(tb)!=length(sols)) stop("The length of 'tb' must equal that of 'sols'!")

  if(!is.numeric(crit) & !is.list(crit)) {
    crit <- if(length(crit)==1) as.list(rep(crit,length(sols))) else if(length(crit)==length(sols)) as.list(crit) else stop("The length of 'crit' must equal to that of 'sols'!")
  } else if(is.list(crit) & length(crit)!=length(sols)) stop("The length of 'crit' must equal that of 'sols'!")

  if(!is.numeric(hsp) & !is.list(hsp)) {
    hsp <- if(length(hsp)==1) as.list(rep(hsp,length(sols))) else if(length(hsp)==length(sols)) as.list(hsp) else stop("The length of 'hsp' must equal to that of 'sols'!")
  } else if(is.list(hsp) & length(hsp)!=length(sols)) stop("The length of 'hsp' must equal that of 'sols'!")

  if(!is.numeric(hsp) & !is.list(hsp)) hsp <- as.list(rep(hsp,length(sols))) else if(is.list(hsp) & length(hsp)!=length(sols)) stop("The length of 'hsp' must equal that of 'sols'!")

  if(length(sols)==1 & length(pres)==1) stop("At least one input vector/list among pressure ('pres') and/or solutes ('sols') must be high than 1 in length!")
  if(length(plt_title)==1) plt_title <- rep(plt_title,2) else if(length(plt_title)>2) stop("The length of 'plt_title' must be either 1 or 2!")
  if(length(vfrac)<2) stop("At least two volume fractions must be included in 'vfrac' for visualization!")
  # 'temp' may be a single value or a VECTOR of temperatures (the returned data gains a 'temp' column).
  temp <- sort(unique(as.numeric(temp)))
  if(!length(temp) || any(!is.finite(temp))) stop("The temperature parameter 'temp' must be one or more finite numeric values!")
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
  miscib <- NULL
  for(i in seq_along(sols)) {
    if(!silent) cat("\nWorking on compound", i, "of", length(sols),"...")
    if(!silent) cat("\nCalculating HSPs and other parameters...")
    # Per-solute experimental override (measured Tc + HSP loaded from the database): skip GCM for this solute.
    exp_i <- !is.null(crit_values) && length(crit_values) >= i && !is.null(crit_values[[i]]) &&
             !is.null(hsp_values) && length(hsp_values) >= i && !is.null(hsp_values[[i]]) &&
             isTRUE(is.finite(suppressWarnings(as.numeric(crit_values[[i]])[1]))) &&
             all(is.finite(suppressWarnings(as.numeric(hsp_values[[i]])[1:3])))
    if(exp_i) {
      soldata  <- mol_find(sols[[i]])[["IDs"]]
      crit_opt <- as.numeric(crit_values[[i]])[1]
      hsp_opt  <- stats::setNames(as.numeric(hsp_values[[i]])[1:3], c("dD","dP","dHB"))
    } else {
      gcmpars <- est_gcm(sols[[i]], tb[[i]], crit[[i]], hsp[[i]], vdw = NA, simplicity = simplicity, silent = TRUE)
      soldata <- gcmpars[["solute_data"]][["IDs"]]
      crit_opt <- gcmpars[["pares"]][["Critical"]][["Tc"]]
      hsp_opt <- gcmpars[["pares"]][["HSP"]]
    }

    cat("\nCalculating miscibility enhancement...")
    optres <- lapply(vfrac, function(x) {
      # Miscib_Enhancement[[1]] is a pressures x temperatures matrix; melt it so every temperature yields
      # its own rows (the source of the new 'temp' column that lets callers compare across temperatures).
      me <- hsp_optim(crit_opt, hsp_opt, modif, modfracs, pres, temp, vfrac = x, silent = TRUE)[["Miscib_Enhancement"]][[1]]
      Pr <- as.character(rownames(me)); Tc <- as.numeric(colnames(me))
      do.call(rbind, lapply(seq_along(Tc), function(ti)
        data.frame(name = soldata[["Name"]], vfrac = x, pres = Pr, temp = Tc[ti],
                   miscib = as.numeric(me[, ti]), stringsAsFactors = FALSE)))
    })
    cat("\nCompiling data...")
    optres <- do.call(rbind, optres)
    miscib <- rbind(miscib, optres)
  }
  colnames(miscib) <- c("name", "vfrac", "pres", "temp", "miscib")
  rownames(miscib) <- NULL

  #Create plots of the results (at the first temperature; multi-temperature comparison is handled by the
  # Shiny app, which builds temperature-aware plots reactively from the returned $data).
  pltlst <- list()
  miscib_p <- miscib[miscib[,"temp"] == temp[1], , drop = FALSE]

  #Each solute at different pressures
  if(length(pres)>1) {
    for(i in unique(miscib_p[,"name"])) {
      pltlst[[i]] <- ggplot(data = miscib_p[miscib_p[,"name"]==i,], aes(x = .data[["vfrac"]], y = .data[["miscib"]],
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
      pltlst[[paste0("pres_",pres_comp[i])]] <- ggplot(data = miscib_p[miscib_p[,"pres"]==pres_comp[i],], aes(x = .data[["vfrac"]], y = .data[["miscib"]],
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
#' @param methods An optional \code{character} vector of method codes to restrict the comparison to (e.g.
#' \code{c("JR", "NL07", "HKR_STW")}). \code{NULL} (the default) compares all available methods. Method codes
#' must match the \code{method} column of the returned table (see \code{\link{est_gcm}} for the full list).
#'
#' @return A \code{data.frame} containing the type of estimated \code{parameter}, the \code{method} used,
#' the level of \code{simplicity} of matching GCM SMARTS substructures, and various estimated parameters including
#' the boiling point (\code{"Tb"}; for the corrected methods \code{JR_corr}/\code{SB_corr} this column already
#' holds the corrected value), critical temperature (\code{"Tc"}), pressure (\code{"Pc"}),
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
#' @seealso \code{\link{mol_find}}, \code{\link{sfe_mod}}
#'
compare_gcm <- function(solute, gorder = 0, simplicity = "auto", methods = NULL) {
  if(length(gorder)>1 & is.null(names(gorder))) {
    stop("When group order 'gorder' is of length > 1, names corresponding to available GCM methods must be provided!")
  } else if(length(gorder)>1 & !all(names(gorder) %in% gcm_opts[,"Method"])) {
    stop(paste0("The following method names in 'gorder' were not recognized: ",
                paste0("'", names(gorder)[!names(gorder) %in% gcm_opts[,"Method"]], "'", collapse = ", "), "."))
  } else if(length(gorder)==1) gorder <- setNames(rep(gorder, nrow(gcm_opts)), gcm_opts[,"Method"])

  mets <- cbind.data.frame(parameter = c(rep("tb",7), rep("crit",5), rep("hsp",4), rep("vdw",3), "chrastil"),
                           method = c("JR", "JR_corr", "SB", "SB_corr", "NL04", "HKR_STW", "HKR_SIM",
                                      "JR", "NL07", "NL07_robust", "HKR_STW", "HKR_SIM",
                                      "SP08", "SP12",  "HKR_STW", "HKR_SIM",
                                      "BND", "SLON", "ZHAO", "CHRASTIL"),
                           simplicity = c(rep(simplicity,20)))

  # Optional method selection: keep only the requested methods (the comparison table is otherwise very
  # crowded). 'methods' is a character vector of method codes (see the 'method' column above / est_gcm).
  if(!is.null(methods)) {
    methods <- as.character(methods)
    valid_mets <- unique(mets[,"method"])
    unknown <- setdiff(methods, valid_mets)
    if(length(unknown)) stop(paste0("Unrecognized method(s) in 'methods': ",
        paste0("'", unknown, "'", collapse = ", "), ". Available: ", paste0(valid_mets, collapse = ", "), "."))
    mets <- mets[mets[,"method"] %in% methods, , drop = FALSE]
    if(nrow(mets)==0) stop("No methods left to compare after applying the 'methods' filter.")
    rownames(mets) <- NULL
  }

  mets[,"order"] <- sapply(mets[,"method"], function(x) {
    if(!x %in% gcm_opts[,"Method"]) return(0)        # e.g. CHRASTIL: first-order groups only
    input_order <- gorder[names(gorder) %in% x]
    cur_order <- gcm_opts[gcm_opts[,"Method"]==x,"Order"]
    if(cur_order < input_order | input_order==0) cur_order else input_order
  })

  solute <- mol_find(solute)
  for(i in seq(nrow(mets))) {
    cat(if(i==1)"\n" else "\r", "Estimating values using method: ", mets[i, "method"], "...", sep = "")
    par <- mets[i,"parameter"]
    g_i <- gorder[names(gorder) %in% mets[i,"method"]]; if(length(g_i)==0) g_i <- 0   # CHRASTIL not in gcm_opts
    contribs <- gcm_contribs_route(solute, mets[i,"method"], simplicity = simplicity, gorder = g_i, silent = TRUE)
    if(par=="tb") {
      tbres <- gcm_bp(contribs)
      # Fold the JR/SB correction into Tb: the corrected-method rows (JR_corr / SB_corr) report the
      # corrected boiling point directly in the Tb column, so the separate (redundant) Tb_corr column
      # is no longer produced.
      mets[i,"Tb"] <- if(any(names(tbres) %in% "Tb_corr")) tbres[["Tb_corr"]] else tbres[["Tb"]]
    } else if(par=="crit") {
      tb_contribs <- gcm_contribs_route(solute, "SB_corr", simplicity = simplicity, gorder = g_i, silent = TRUE)
      tb <- gcm_bp(tb_contribs)[["Tb_corr"]]
      mets[i,c("Tc","Pc","Vc")] <- unname(gcm_crit(tb, contribs)[c("Tc","Pc","Vc")])
    } else if(par=="hsp") {
      hspres <- gcm_hsp(contribs)
      mets[i,c("dD","dP","dHB")] <- unname(hspres[c("dD", "dP", "dHB")])
      for(j in c("dP_low", "dHB_low")) if(any(names(hspres) %in% j)) mets[i,"dP_low"] <- hspres[[j]]
    } else if(par=="vdw") {
      vdwres <- gcm_vdw(contribs)
      mets[i,"VDW"] <- vdwres[["VDW"]]
    } else if(par=="chrastil") {
      chres <- gcm_chrastil(contribs)
      mets[i,c("k","A","B")] <- unname(chres[c("k","A","B")])
    }
  }
  return(mets)
}
