#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Calculate COM for one or more extraction times
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Calculate COM for an SFE or SWE extraction process
#'
#' @description Calculates the Cost of Manufacturing (COM) and its accompanying components including Cost of Labour (COL),
#' Cost of Raw Materials (CRM), Cost of Ulitities (CUT) and Fixed Costs (FCI). See \strong{Details} for further information.
#'
#' @param input A named \code{character} vector of length 2, or a \code{data.frame} with 2 columns
#' containing the temporal (\strong{element 1}) and response (\strong{element 2}) components of the extraction process. When
#' a \code{data.frame} is provided, the input is treated like an extraction curve, and COM is calculated separately for each row.
#' @param invars A \code{character} vector containing names of the 2 input variables (temporal and response).
#' By default, the names provided in \code{input} are used.
#' @param pltlab The plot title. By default, \code{invars[1]} is used.
#' @param cosol_loss A \strong{single} \code{numeric} value between 0.001 and 0.99 denoting the \strong{fraction}
#' of co-solvent lost during post-processing (e.g. evaporation). Defaults to 0.05.
#' @param gen,crm,cut,col,fci \strong{Named} numeric vectors of input parameters required for \strong{general calculations} (\code{gen}),
#' \strong{Cost of Raw Materials} (\code{crm}), \strong{Cost of Labour} (\code{col}), \strong{Cost of Utilities} (\code{cut}),
#' and/or \strong{Fixed Costs} (\code{fci}). Some parameters are mandatory, while others are optional.
#' For an exhaustive list of available \strong{input} and \strong{output} parameters, use \code{show_pars("com")}.
#' @param auxpr,auxfr \strong{Identically named} \code{numeric} vectors containing the
#' \strong{price per unit mass} of any auxiliary materials used in the process
#' (e.g. for pre- or post-processing), and the \strong{fraction of said material
#' required relative to the mass of raw material} (>0).
#' Both default to \code{NA} (no auxiliary materials used).
#' @param taxrate The \code{fraction of gross profit} to be removed as tax to calculate \strong{net profit}.
#' @param flowpar The parameters of temperature and pressure at which flow rate is given.
#' Either \auto{"auto"} (default) or a \code{list} of length 2, where the first element specifies the pressure and temperature at which flow is
#' measured for the main solvent (CO2 or water depending on \code{comode}), while the second element specifies these values for the co-solvent (
#' either CO2 or ethanol when \code{comode} is set to \code{"swe"} or \code{"sfe"}, respectively). \strong{Note} that when \code{comode} is
#' \code{"sfe"}, only the temperature of ethanol is included in the second element, as pressure is disregarded in density calculations.
#' Used for conversion of volumetric flow to mass flow when \code{!mass_flow}.
#' If set to \code{"auto"}, uses a temperature and pressure of 10 degrees Celsius and 60 bar for CO2, and 25 degrees Celsius
#' for ethanol.
#' @param draw A \code{logical} switch specifying whether to plot the results.
#' Only works if \strong{an extraction curve is provided as } \code{input}.
#' @param mass_flow A \code{logical} specifying whether the \code{["flow"]} component of \code{gen} is provided in \strong{mass (g/min)}
#' or \strong{volumetric (mL/min)} units.
#' @param comode Which extraction process to calculate COM for?
#' One of: \code{"sfe"} (Supercritical Fluid Extraction) or \code{"swe"} (Subcritical Water Extraction).
#' @param use_coefs A \code{logical} denoting whether to use pre-defined coefficients to calculate COM (\code{TRUE}) from
#' CUT, CRM, FCI, and COL, or whether to sue a simple sum instead (\code{FALSE}, \strong{default}).
#' @param export An \strong{existing} folder path into which the results are exported as a .CSV file and accompanying graphics (if any).
#' Defaults to \code{"none"} (the results are not exported).
#'
#' @details
#' The calculation of Cost of Manufacturing (COM) is based on the formula provided by Turton et al. (1998)
#' when \code{use_coefs} is \code{TRUE}, and combined the Cost of Labour (\eqn{COL}), Cost of Utilities (\eqn{CUT}),
#' Cost of Raw Materials (\eqn{CRM}), and Fixed Costs (\eqn{FCI}). In the current implementation, the Cost of Waste Management
#' (\eqn{CWT}) normally included in this method has been omitted since neither supercritical CO2 and subcritical water extraction
#' do not produce significant amounts of waste.
#' \deqn{COM = 0.304 \times FCI + 2.73 \times COL + 1.23 \times (CRM + CUT)}
#' Alternatively (and by default), COM is calculated as a simple sum of \strong{monthly} CUT, CRM, COL, and FCI.
#' The various COM constituents are calculated as below.
#'
#' The monthly COL is calculated using a simplified formula incorporating the average monthly wage (\eqn{W_{pay}}),
#' the number of daily work shifts (\eqn{W_{sh}}), and the number of personnel required (\eqn{W_{pers}}):
#' \deqn{COL = W_{sh} \times W_{pers} \times W_{pay}}
#'
#' The monthly CRM encompasses a sum of various costs associated with raw materials, calculated in multiple steps. First,
#' the number of \strong{monthly} extraction cycles \eqn{N_{ex}} is calculated using \eqn{W_{sh}}, the number of work hours per shift
#' (\eqn{W_{hr}}), extraction time (\eqn{T_{ex}}), batch exchange time (\eqn{T_{aux}}), and the number of work days per month (\eqn{W_{days}}).
#' \deqn{N_{ex} = (W_{sh}\times W_{hr})/((T_{ex} + T_{aux})/60) \times W_{days}}
#'
#' Any volumetric flow rates are converted to mass flow using CO2 (for SFE) or water (for SWE) densities at process conditions specified in
#' \code{crm} or \code{flowpar}. The equations incorporate fluid density \eqn{\rho} calculated by the Bender Equation (\code{\link{bendens}}) or
#' the IAPWS R6-95 formulation (\code{\link{h2o_dens}}).
#' \deqn{F_{mass} = \rho/1000 \times F_{vol}}
#' The density of main and co-solvent mixtures is calculated using the Linear Blend Rule (see \code{\link{etoh_dens}}).
#'
#' Next, the main solvent usage (\eqn{SL_{main}}, water for SWE or a supercritical fluid like CO2 for SFE) and co-solvent usage (\eqn{SL_{aux}})
#' are calculated via the equations below. \strong{Currently only EtOH for SFE and CO2 for SWE are supported as co-solvents}.
#' The units are either \strong{L} or \strong{kg} depending on whether \code{mass_flow} is \code{TRUE}. For CO2, the equation
#' depends on whether a recovery tank pressure was specified - if it was, the system is assumed to be equipped with a CO2
#' recirculation/recovery system, incorporating the extractor volume \eqn{V_{ex}}. If not, the flow rate is simply converted
#' to kg/min and multiplied by extraction time (the same equation is used for SWE). In equations below, density \eqn{\rho} is assumed to be in
#' units of \strong{g/L}.
#' \deqn{SL = \rho/1000\times V_{ex} \text{ for CO2 as a main (SFE) or a co-solvent (SWE) with a recovery system}}
#' \deqn{SL = F_{mass}/1000 \times T_{ex} \times 0.05 \text{ for EtOH co-solvent (SFE) assuming a 5% loss per batch}}
#' \deqn{SL = \rho_{CO2}/1000 \times (F_{aux}/(F_{aux} + F_{main})) \times V_{ex} \text{ for CO2 as a co-solvent for SWE}}
#' \deqn{SL = F_{mass}/1000 \times T_{ex} \text{ for all solvents where a recovery system is not present}}
#'
#' Finally, the requirement (\eqn{REQ}, kg), \eqn{N_{ex}}, and the price \eqn{PR} of \strong{each} material \eqn{i} (including solvents)
#' are combined as follows and the results summed to obtain the monthly CRM:
#' \deqn{CRM = \sum{REQ_i \times N_{ex} \times PR_i}}
#'
#' Similarly, all main power requirements (\eqn{PWRM_i}, in \strong{kWh}) are multiplied by \eqn{N_{ex}}, \eqn{T_{ex}}
#' (in \strong{hr}), and cost per kWh (\eqn{PR_{kWh}}) to obtain main power requirement \eqn{PW_{main}}:
#' \deqn{PW_{main} = PWRM_i \times N_{ex} \times T_{ex} \times PR_{kWh}}
#'
#' Power requirements for \strong{drying}, \strong{comminution}, and \strong{evaporation}
#' (termed \eqn{PW_{aux}} when summed) are calculated
#' by multiplying the respective power requirement \eqn{PWRA_i} with the monthly consumption of the
#' associated material(s) \eqn{REQ_i} divided by the respective processing capacity of required equipment
#' \eqn{CAP_i}:
#' \deqn{PW_{aux} = \sum{PWRA_i \times (REQ_i/CAP_i)}}
#'
#' The main and auxiliary power usage is finally summed to obtain \eqn{CUT}:
#' \deqn{CUT = PW_{main} + PW_{aux}}
#'
#' The FCI is either used as-provided in the input data or incorporates the \strong{depreciation fraction} that is deducted yearly
#' from the CAPEX.
#'
#' Once COM is calculated, the Specific Cost \eqn{SC} (USD/kg) of extract production is calculated by dividing monthly \eqn{COM} by the monthly
#' yield of extract \eqn{Y_{month}} (kg). Sales Volume \eqn{SV} (USD/month) is then obtained by multiplying \eqn{Y_{month}} by sales price
#' \eqn{SP_{kg}} (USD/kg). The manufacturing cost (\eqn{SC \times Y_{month}}) is then subtracted from \eqn{SV} to obtain the Gross Profit
#' (\eqn{GPr}, USD/month), which may then be corrected for taxes to get Net Profit (\eqn{NPr}).
#' \deqn{SC = COM/Y_{month}}
#' \deqn{SV = Y_{month} \times SP_{kg}}
#' \deqn{GPr = SV - SC \times Y_{month}}
#' \deqn{NPr = GPr - taxes}
#'
#' Finally, the Profit Margin (\eqn{MRG}, %) and Payback Period (\eqn{PBK}, yr) are obtained as follows:
#' \deqn{MRG = NPr/SV \times 100}
#' \deqn{PBK = CAPEX/(NPr \times 12)}
#'
#' @return A named \code{list} containing the input parameters (\code{$input}), the calculated output values (\code{$output})
#' its simplified form (\code{$simple_output}), extra parameters including FCI and COL (\code{$extra}),
#' and the function call (\code{$call}). Optionally, specific cost and payback time plots of the results are also included
#' when \code{draw} is \code{TRUE} and an extraction curve is provided as \code{input} (\code{$plots}).
#' Any plot points removed due to economic \strong{non-}viability are also noted in element \code{$rm_state}.
#' @export
#'
#' @references
#' Turton, R., Bailie, R.C., Whiting, W.B., Shaeiwitz, J.A. (1998), \emph{Analysis, Synthesis and Design of Chemical Process, PTR}, Prentice Hall, Upper Saddle River, NJ, USA.
#'
#' @examples
#' #Calculate COM for a single process
#' comin <- c(time = 180, yield = 7)
#'
#' #Calculate COM throughout an extraction curve and plot the results
#' comin <- data.frame(time = c(5,9,11,15,19,21,23,25,30,40,60,80,100,120,140,160,180),
#' yield = c(0.80, 2.18, 2.79, 3.40, 3.86, 4.17, 4.47, 4.63, 4.93,
#'           5.24, 5.70, 6.00, 6.31, 6.46, 6.77, 6.92, 7.00))
#'
#' comres <- calcom(input = comin,
#' gen = c(volex = 30, load = 9, pres = 300, temp = 40, flow = 3000,
#'         extime = 30, csol_flow = 300, dilfac = 2, pr_sale = 90),
#' crm = c(bh = 155, id = 15.5, pr_mat = 0.5, pr_msol = 0.8, pr_csol = 2,
#' recp = 60, rect = 10, sept = 55),
#' cut = c(pw_main = 25, pr_kwh = 0.13, pw_dry = 2, pw_com = 1, pw_evap = 13.3,
#' cap_dry = 4.16, cap_com = 20, cap_evap = 60),
#' col = c(oper = 1, whr = 8, shifts = 3, wage = 940, wdays = 24),
#' fci = c(capex = 150000, maint = 250, other = 1210),
#' auxpr = c(oil = 1.52),
#' auxfr = c(oil = 1),
#' pltlab = "Time (min)")
#'
#' @seealso \code{\link{show_pars}}, \code{\link{bendens}}, \code{\link{h2o_dens}}, \code{\link{com_export}}
#'
#' @importFrom stats setNames na.omit complete.cases
#' @importFrom scales breaks_pretty
#' @import ggplot2
#'
calcom <- function(input, invars = c(names(input)[1], names(input)[2]), pltlab = invars[1], cosol_loss = 0.05,
                   gen, crm, auxpr = NA, auxfr = NA, cut, col, fci, taxrate = NA, flowpar = "auto",
                   draw = TRUE, mass_flow = FALSE, comode = "sfe", use_coefs = FALSE, export = "none") {

  #Preliminary checks
  if(length(invars)!=2) stop("Argument 'invars' must be a character vector or list of length 2!")
  if(!all(invars %in% names(input))) stop("At least one name in 'invars' is not present in the input data!")
  if(!is.data.frame(input)) input <- as.data.frame(t(input))
  if(!any(c("sfe","swe") %in% comode)) stop("Argument 'comode' must be one of 'sfe' or 'swe'!")
  if(!dir.exists(export) & export!="none") stop("The provided export directory does not exist!")
  if(!all(sapply(c(mass_flow,draw), is.logical))) stop("Arguments 'mass_flow' and 'draw' must both be logical!")
  if(!is.numeric(cosol_loss)|cosol_loss<0.001|cosol_loss>0.99) stop("The degree of co-solvent loss should be between 0.001 and 0.99!")
  if(length(mass_flow)==1) mass_flow <- rep(mass_flow,2) #First for main solvent, second for co-solvent

  #Checking main parameters
  pars <- list(gen = gen, crm = crm, cut = cut, col = col, fci = fci)

  auxnms <- list(gen = c("csol_flow", "dilfac", "pr_sale"), #if(comode=="sfe") "sept" else NULL
                 crm = c("recp", "rect", "sept", "pr_csol"),
                 cut = c("pw_dry", "pw_com", "pw_aux", "pw_evap", "cap_dry", "cap_com", "cap_evap"),
                 fci = c("depr", "maint", "other"))

  gnms <- list(gen = c("volex","load","pres", "temp", "flow", "extime", auxnms[["gen"]]),
               crm = c("bh", "id", "pr_mat", "pr_msol", auxnms[["crm"]]),
               cut = c("pw_main", "pr_kwh", auxnms[["cut"]]),
               col = c("oper", "whr", "shifts", "wage", "wdays"),
               fci = c("capex", auxnms[["fci"]]))

  for(i in names(pars)) {
    if(i!="col") pars[[i]][auxnms[[i]][!auxnms[[i]] %in% names(pars[[i]])]] <- NA
    gnchk <- gnms[[i]][which(!gnms[[i]] %in% names(pars[[i]]))]
    if(length(gnchk)>0) stop(paste0("The following necessary named element(s) is/are missing from argument '",
                                    i, "': " , paste0("'", gnchk, "'", collapse = ", "), "!"))
    gnchk2 <- names(pars[[i]])[!names(pars[[i]]) %in% gnms[[i]]]
    if(length(gnchk2)>0) stop(paste0("The following named elements in argument '", i, "' were not recognized: ",
                                     paste0("'", gnchk2, "'", collapse = ", "), "!"))
  }

  #Deploy list elements
  list2env(pars, envir = environment())

  #Checking auxiliary materials
  if(!any(is.na(auxpr)) & !any(is.na(auxfr))) {

    if(length(auxpr)!=length(auxfr)|!is.numeric(auxpr)|!is.numeric(auxfr)) stop("Auxiliary material inputs 'auxpr' and 'auxfr' must be numeric vectors of equal length!")
    else if(is.null(names(auxpr))|is.null(names(auxfr))|!all(names(auxpr) %in% names(auxfr))) stop("The names of 'auxpr' and 'auxfr' must be the same!")

    auxna <- FALSE
    auxmatnms <- names(auxpr)
    auxfr <- auxfr[names(auxpr)]
    if(any(auxfr <= 0)) stop("The required fraction of each 'auxfr' element must be >0!")
  } else auxna <- TRUE

  #Additional checks
  if(comode=="sfe" & !all(c("recp","rect") %in% names(crm))) cat("\nNo recovery pressure ('recp') and/or temperature ('rect') set for CO2. Are you sure?")
  usedvol <- pi*(crm["id"]/2)^2*crm["bh"]/1000
  if(usedvol > gen["volex"]) stop("The used volume cannot exceed extractor volume!")
  if((gen["pres"]>1000|gen["temp"]>826) & comode=="sfe") {
    stop("Maximum supported operating pressure and temperature for SFE is currently 1000 bar and 826 degC, respectively!")
  } else if((gen["pres"]>220|gen["temp"]>373) & comode=="swe") stop("Maximum supported operating pressure and temperature
                                                for SWE is currently 220 bar and 373 degC, respectively!")

  pwnms <- c("dry", "com", "evap")
  for(i in pwnms) {
    cutnms <- paste0(c("pw","cap"), "_", i)
    if(!all(cutnms %in% names(cut))) stop(paste0("The following parameter pair is incomplete present in 'cut': ",
                                                 paste0("'", cutnms, "'", collapse = ", "), "!"))
  }

  if(!is.list(flowpar) & length(flowpar)==2 & is.numeric(flowpar)) flowpar <- list(flowpar)
  if(!any(flowpar %in% "auto") & ((!is.na(gen["co2"]) & length(flowpar)!=2)| #No adjustable density for EtOH
                                  unique(lengths(flowpar))!=2)) stop("When not set to 'auto', argument 'flowpar' must be a numeric list
                                                                     of length 1 or 2, with each element a numeric vector of length 2!")
  recvec <- crm[c("recp","rect","sept")]
  if(!any(is.na(c(recvec)))) {
    if(crm[["recp"]]>gen[["pres"]]) stop("Extraction pressure cannot be higher than the CO2 recovery pressure!")
  }

  if(!all(is.na(recvec)) & any(is.na(recvec))) stop("When recovery pressure 'recp' is provided, recovery temperature 'rect'
                                                    and final separator temperature 'sept' are also required!")

  if(!is.na(fci[["depr"]]) & (fci[["depr"]] >= 1|fci[["depr"]]<0)) stop("Equipment depreciation fraction 'depr' cannot be more than 1 or less than 0!")


  #Create master output list to populate later
  finres <- list()

  #Begin COM calculations - COMPREHENSIVE DATA REPORTED ONLY FOR THE MAXIMUM EXTRACTION TIME!
  #STEP 1: Calculate S/M ratio
  #Convert flows to mass (g/min) if necessary
  #Main solvent flow (CO2 or water)
  sfechk <- comode=="sfe"

  if(!mass_flow[1]) {
    #Set up flow parameters
    if(any(flowpar %in% "auto")) {
      flowpar_main <- if(comode=="sfe") c(60,10) else c(gen["pres"], gen["temp"]) #if(!is.na(crm["recp"])) c(crm[["recp"]], crm[["rect"]]) else c(60,10)
    } else flowpar_main <- flowpar[[1]]

    #Calculate CO2 or water density
    main_dens <- if(sfechk) bendens(flowpar_main[1], flowpar_main[2])[[1]] else h2o_dens(flowpar_main[1], flowpar_main[2])
    main_flow <- main_dens/1000*gen[["flow"]]

  } else main_flow <- gen[["flow"]]

  #Co-solvent flow (EtOH or CO2)
  cur_csol <- gen[["csol_flow"]]
  if(!is.na(cur_csol)) {
    if(!mass_flow[2]) {
      #Set up flow parameters
      if(any(flowpar %in% "auto")) {
        flowpar_csol <- if(!sfechk) c(60,10) else 25 #if(!sfechk & !is.na(crm[["recp"]])) c(crm[["recp"]], crm[["rect"]]) else if(!sfechk & is.na(crm[["recp"]])) c(60,10) #if(sfechk) c(1.01325, 25) else
      } else flowpar_csol <- flowpar[[2]]

      #Calculate CO2 density for SWE (EtOH density is a constant value at STP)
      csol_dens <- if(!sfechk) bendens(flowpar_csol[1], flowpar_csol[2])[[1]] else etoh_dens(flowpar_csol[1])[[1]]
      csol_flow <- csol_dens/1000*cur_csol

    } else csol_flow <- cur_csol
  } else csol_flow <- NA

  #Retrieve input data
  tm <- input[,invars[1]]
  resp <- input[,invars[2]]

  #Compile input parameters
  cat("\nCompiling input data...")
  desc_com <- show_pars("com")
  inpars <- na.omit(c(gen, crm, cosol_loss = cosol_loss, cut, col, fci))
  inpars <- setNames(cbind.data.frame(names(inpars), inpars), c("parameter", "value"))
  inpars[,c("units", "type", "description")] <- desc_com[["input"]][match(inpars[,"parameter"], desc_com[["input"]][,"parameter"]),
                                                                    c("units", "type", "description")]
  inpars[inpars[,"parameter"] %in% c("flow", "csol_flow"),"units"] <- ifelse(mass_flow, "g/min", "mL/min") #Specify flow units

  #Add auxiliary material prices and required fractions
  if(!auxna) {
    auxdf <- do.call(cbind.data.frame, list(parameter = c(paste0("auxprice_", auxmatnms),
                                                          paste0("auxfrac_", auxmatnms)),
                                            value = c(auxpr, auxfr),
                                            units = c(rep("USD/kg", length(auxpr)), rep("none", length(auxfr))),
                                            type = "crm_aux",
                                            description = c(paste0("Price of auxiliary material: ", auxmatnms),
                                                            paste0("Required fraction of auxiliary material: ", auxmatnms))))
    inpars <- rbind.data.frame(inpars, auxdf)
  }

  rownames(inpars) <- NULL
  finres[["input"]] <- inpars

  #Compile detailed output data
  #Create output data frame
  cat("\nCompiling detailed output data...")
  detout <- cbind.data.frame(desc_com[["output"]][,c("parameter", "description", "units")], setNames(as.data.frame(matrix(nrow = nrow(desc_com[["output"]]), ncol = nrow(input))), paste0(tm, "_min")))

  #Calculate S/M ratio
  dlst <- list()
  dlst[["sm"]] <- main_flow*tm/(gen[["load"]]*1000)

  #Calculate number of extraction cycles (monthly)
  whrd <- col[["whr"]]*col[["shifts"]]
  dlst[["cycles"]] <- cycles <- floor(whrd/((tm+gen[["extime"]])/60))*col[["wdays"]]

  #Calculate main solvent and co-solvent usage per batch
  if(comode=="sfe") {
    dlst[["msol_loss"]] <- msol_loss <- if(!is.na(crm[["recp"]])) bendens(crm[["recp"]], crm[["sept"]])[[1]]/1000*gen[["volex"]] else main_flow/1000*tm
    dlst[["csol_loss"]] <- csol_loss <- csol_flow/1000*tm*cosol_loss
  } else {
    dlst[["msol_loss"]] <- msol_loss <- main_flow/1000*tm
    dlst[["csol_loss"]] <- csol_loss <- if(!is.na(crm[["recp"]])) bendens(crm[["recp"]], crm[["sept"]])[[1]]/1000*((csol_flow/(csol_flow+main_flow))*gen[["volex"]]) else csol_flow/1000*tm
  }

  #Calculate main material requirements and costs (monthly)
  lossnms <- c("raw", "msol","csol")
  for(i in lossnms) {
    losschk <- which(lossnms %in% i)
    req_arg <- list(gen[["load"]], msol_loss, csol_loss)[[losschk]]
    cost_arg <- unname(crm[c("pr_mat","pr_msol","pr_csol")])[losschk]

    dlst[[paste0(i,"_req")]] <- cureq <- req_arg * cycles
    dlst[[paste0(i,"_cost")]] <- cureq * cost_arg
  }

  #Calculate auxiliary material requirements and cost (monthly)
  if(!auxna) {
    auxreqs <- auxcosts <- as.data.frame(matrix(NA, ncol = length(tm), nrow = length(auxpr)))
    for(i in seq_along(auxpr)) {
      auxreqs[i,] <- (dlst[["raw_req"]]*(resp/100))*auxfr[i]
      auxcosts[i,] <- auxreqs[i,] * auxpr[i]
    }
    dlst[["aux_req"]] <- unname(colSums(auxreqs, na.rm = TRUE))
    dlst[["aux_cost"]] <- unname(colSums(auxcosts, na.rm = TRUE))
  } else dlst[["aux_req"]] <- dlst[["aux_cost"]] <- 0

  #Calculate main power requirements and costs
  dlst[["pwm_req"]] <- pwm_req <- cut[["pw_main"]]*(tm/60)*cycles
  dlst[["pwm_cost"]] <- pwm_req*cut[["pr_kwh"]]

  #Ditto for auxiliary power
  pwnms <- c("dry", "com", "evap")
  aux_req <- sapply(pwnms, function(x) {
    matype <- if(x=="evap" & sfechk) dlst[["csol_req"]]/cosol_loss else if(x=="evap" & !sfechk) dlst[["msol_req"]] else dlst[["raw_req"]]
    res <- cut[[paste0("pw_",x)]]*(matype/cut[[paste0("cap_",x)]])
    return(res)
  })

  #Individual aux power requirements
  for(i in seq_along(pwnms)) {
    dlst[[paste0("pw",pwnms[i],"_req")]] <- curreq <- aux_req[i]
    dlst[[paste0("pw",pwnms[i],"_cost")]] <- curreq*cut[["pr_kwh"]]
  }

  #Total aux power requirement
  aux_req <- if(length(tm)>1) rowSums(aux_req) else sum(aux_req)
  dlst[["pwa_req"]] <- pwa_req <- sapply(aux_req, function(x) sum(c(x, cut[["pw_aux"]]), na.rm = TRUE))
  dlst[["pwa_cost"]] <- pwa_req * cut[["pr_kwh"]]

  #Calculate CRM, CUT, COL, FCI, and COM
  base_fci <- sum(fci[c("maint","other")], na.rm = TRUE)
  FCI <- if(is.na(fci[["depr"]])) sum(c(base_fci, (fci[["capex"]]*fci[["depr"]])/12), na.rm = TRUE)  else base_fci
  COL <- col[["wage"]] * col[["oper"]] * col[["shifts"]]
  dlst[["crm"]] <- CRM <- unname(colSums(do.call(rbind.data.frame, dlst[c("raw_cost","msol_cost","csol_cost","aux_cost")]), na.rm = TRUE)) #Reduce("+", dlst[c("raw_cost","msol_cost","csol_cost","aux_cost")])
  dlst[["cut"]] <- CUT <- unname(colSums(do.call(rbind.data.frame, dlst[c("pwm_cost","pwa_cost")]), na.rm = TRUE))
  dlst[["com"]] <- COM <- if(use_coefs) 1.23*(CUT+CRM) + 2.73*COL + 0.304*FCI else CRM + CUT + FCI + COL

  #Calculate specific cost and other economic indicators necessary for Payback Period
  dlst[["extract"]] <- dlst[["raw_req"]]*(resp/100)
  if(!is.na(gen[["dilfac"]]) & gen[["dilfac"]]>0) dlst[["extract"]] <- dlst[["extract"]] * gen[["dilfac"]]
  dlst[["sc"]] <- COM/dlst[["extract"]]
  dlst[["svol"]] <- svol <- gen[["pr_sale"]] * dlst[["extract"]]
  dlst[["mgp"]] <- svol - dlst[["sc"]] * dlst[["extract"]]
  if(!is.na(taxrate)) dlst[["mnp"]] <- curpr <- dlst[["mgp"]] * (1-taxrate/100) else curpr <- dlst[["mgp"]]
  dlst[["pmargin"]] <- curpr/svol*100
  dlst[["payback"]] <- fci[["capex"]]/(curpr*12)

  #Populate detailed output data frame
  detcols <- grep("_min", colnames(detout))
  for(i in names(dlst)) detout[detout[,"parameter"]==i, detcols] <- dlst[[i]]
  detout <- detout[complete.cases(detout),]
  rownames(detout) <- NULL
  finres[["output"]] <- detout

  #Compile simplified output
  cat("\nCompiling simplified output data...")
  mnpchk <- "mnp" %in% detout[,"parameter"]
  simcols <- c("cycles", "cut", "crm", "com", "svol", "extract", "sc", if(mnpchk) "mnp" else "mgp", "pmargin", "payback")
  simout <- cbind.data.frame(tm, resp, as.data.frame(t(detout[c(match(simcols, detout[,"parameter"], nomatch = 0),0),
                                                              detcols, drop = FALSE]))) #which(detout[,"parameter"] %in% simcols)
  rownames(simout) <- NULL
  colnames(simout) <- c(invars[1], invars[2], "monthly_cycles", "CUT", "CRM", "COM", "sales_volume", "extract_made", "SC_per_kg", "monthly_profit", "margin_percent", "payback_yr")

  #Remove economically non-viable points of the extraction curve
  finres[["simple_output"]] <- NA
  rmchk <- which(simout[,"payback_yr"]<0)
  if(length(rmchk)>0 & length(tm)>1) {
    finres[["rm_state"]] <- rm_state <- paste0("Results at the following extraction time(s) were removed as not economically viable: ",
                                               paste0(simout[rmchk,invars[1]], " min", collapse = ","),".")
    cat("\n",rm_state, sep = "")
    simout <- simout[-rmchk,]
  }
  finres[["simple_output"]] <- simout

  #Create plots if an extraction curve is provided
  if(length(tm)>1) {
    cat("\nGenerating plots...")
    pltlst <- list()

    yvar <- c("SC_per_kg", "payback_yr")
    plt_title <- c("Specific Cost plot", "Payback time plot")
    plt_ylab <- c("Specific Cost (USD/kg)", "Payback time (yr)")
    pltnms <- c("sc","payback")

    for(i in 1:2) {
      minxv <- simout[which.min(simout[,yvar[i]]),invars[1]]

      pltlst[[pltnms[i]]] <- ggplot(data = simout, aes(x = .data[[invars[1]]], y = .data[[yvar[i]]])) +
        geom_hline(yintercept = min(simout[,yvar[i]]), lty = 2, colour = "darkgreen", linewidth = 0.8) +
        geom_point(pch = 16, colour = "darkred") + geom_line(colour = "darkred") +
        geom_point(data = simout[which.min(simout[,yvar[i]]),], colour = "darkgreen", size = 2) +
        geom_label(data = simout[which.min(simout[,yvar[i]]),], nudge_x = 1*(max(tm)-minxv),
                   aes(label = round(.data[[yvar[i]]],2)), colour = "white", fontface = "bold", fill = "darkgreen") +
        scale_x_continuous(breaks = scales::breaks_pretty(n = 6)) +
        scale_y_continuous(breaks = scales::breaks_pretty(n = 6)) +
        labs(title = plt_title[i], x = pltlab, y = plt_ylab[i]) +
        theme(aspect.ratio = 1,
              panel.background = element_blank(),
              axis.text = element_text(colour = "black", size = 12),
              axis.title = element_text(colour = "black", size = 13),
              axis.line = element_line(colour = "black"),
              legend.title = element_blank(),
              #legend.key = element_rect(fill = "grey90", colour = "black"),
              legend.position.inside = c(0.93, 0.15),
              legend.text = element_text(size = 11))
    }
    finres[["plots"]] <- pltlst
    if(draw) print(pltlst)
  }

  #Include additional parameters
  finres[["extra"]] <- c(fci = FCI, col = COL)

  #Save function call
  finres[["call"]] <- match.call()

  #Export results
  if(export!="none") {
    cat("\nExporting results...")
    com_export(comres = finres, expath = export, silent = FALSE)
  }
  return(finres)
}
