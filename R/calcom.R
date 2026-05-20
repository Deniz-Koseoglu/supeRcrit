#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Calculate COM for one or more extraction times
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Calculate COM for an SFE or SWE extraction process
#'
#' @description Calculates the Cost of Manufacturing (COM) and its accompanying components including Cost of Labour (COL),
#' Cost of Raw Materials (CRM), Cost of Ulitities (CUT) and Fixed Manufacturing Costs (FMC). See \strong{Details} for further information.
#'
#' @param input A named \code{character} vector of length 2, or a \code{data.frame} with 2 columns
#' containing the temporal (\strong{element 1}) and response (\strong{element 2}) components of the extraction process. When
#' a \code{data.frame} is provided, the input is treated like an extraction curve, and COM is calculated separately for each row.
#' @param invars A \code{character} vector containing names of the 2 input variables (temporal and response).
#' By default, the names provided in \code{input} are used.
#' @param pltlab The plot title. By default, \code{invars[1]} is used.
#' @param cosol_loss A \strong{single} \code{numeric} value between 0.001 and 0.99 denoting the \strong{fraction}
#' of co-solvent lost during post-processing (e.g. evaporation). Defaults to 0.05.
#' @param msol_loss_frac A \strong{single} \code{numeric} value between 0 and 1 denoting the \strong{fraction}
#' of main solvent (CO2) lost per recirculation pass in CC-SFE mode. Only used when \code{comode = "cc_sfe"}.
#' Represents system leakage, valve losses, and incomplete recovery. Defaults to 0.01 (1%).
#' @param feed_density A \strong{single} \code{numeric} value denoting the feed liquid density in g/mL,
#' used for volumetric-to-mass conversion of the feed flow rate in CC-SFE mode. Only used when
#' \code{comode = "cc_sfe"} and \code{mass_flow[3] = FALSE} (volumetric feed flow). Defaults to 1.
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
#' Used for conversion of volumetric flow to mass flow when \code{!mass_flow}.
#' If set to \code{"auto"} (default), uses the temperature and pressure specified for the recovery line in \code{crm[c("recp","rect")]}.
#' @param draw A \code{logical} switch specifying whether to plot the results.
#' Only works if \strong{an extraction curve is provided as } \code{input}.
#' @param mass_flow A \code{logical} vector specifying whether flow rates are in \strong{mass (g/min)}
#' or \strong{volumetric (mL/min)} units. Length 1 (applied to all), 2 (main, co-solvent),
#' or 3 (main, co-solvent, feed for CC-SFE).
#' @param comode Which extraction process to calculate COM for?
#' One of: \code{"sfe"} (Supercritical Fluid Extraction from solid material),
#' \code{"cc_sfe"} (Counter-Current Supercritical CO2 Extraction from liquid feed),
#' or \code{"swe"} (Subcritical Water Extraction).
#' In \code{"cc_sfe"} mode, \code{gen[["load"]]} is interpreted as the feed flow rate
#' (mL/min or g/min) instead of batch load (kg), and \code{gen[["extime"]]} is ignored.
#' @param use_coefs A \code{logical} denoting whether to use pre-defined coefficients to calculate COM (\code{TRUE}) from
#' CUT, CRM, FMC, and COL, or whether to sue a simple sum instead (\code{FALSE}, \strong{default}).
#' @param export An \strong{existing} folder path into which the results are exported as a .CSV file and accompanying graphics (if any).
#' Defaults to \code{"none"} (the results are not exported).
#' @param response_unit The unit of the primary response variable. One of: \code{"percent"} (default, yield as percentage of raw material),
#' \code{"g"} (absolute mass in grams), or \code{"kg"} (absolute mass in kilograms). When \code{"g"} or \code{"kg"} is specified,
#' the yield percentage is calculated based on the dry mass of raw material (accounting for \code{moisture_content}).
#' @param moisture_content The moisture content of the raw material as a percentage (0-100). Defaults to 0.
#' Used only when any \code{response_unit} (primary or additional) is \code{"g"} or \code{"kg"} to calculate the dry mass for yield percentage conversion.
#' @param response_name A \code{character} string specifying the name of the primary response. Defaults to \code{"Primary"}.
#' @param additional_responses A \code{list} of up to 3 additional response specifications. Each element should be a named list containing:
#' \itemize{
#'   \item \code{column}: The column name in \code{input} containing response values (required for data.frame input)
#'   \item \code{value}: A single numeric value (required for single-value input instead of \code{column})
#'   \item \code{name}: The name for this response (defaults to column name or "Response_N")
#'   \item \code{pr_sale}: Sale price in USD/kg (required)
#'   \item \code{response_unit}: One of \code{"percent"}, \code{"g"}, or \code{"kg"} (defaults to \code{"percent"})
#'   \item \code{dilfac}: Dilution factor (defaults to 1)
#' }
#' Defaults to \code{NULL} (no additional responses).
#'
#' @details
#' The calculation of Cost of Manufacturing (COM) is based on the formula provided by Turton et al. (1998)
#' when \code{use_coefs} is \code{TRUE}, and combined the Cost of Labour (\eqn{COL}), Cost of Utilities (\eqn{CUT}),
#' Cost of Raw Materials (\eqn{CRM}), and Fixed Manufacturing Costs (\eqn{FMC}). In the current implementation, the Cost of Waste Management
#' (\eqn{CWT}) normally included in this method has been omitted since neither supercritical CO2 and subcritical water extraction
#' do not produce significant amounts of waste.
#' \deqn{COM = 0.304 \times FMC + 2.73 \times COL + 1.23 \times (CRM + CUT)}
#' Alternatively (and by default), COM is calculated as a simple sum of \strong{monthly} CUT, CRM, COL, and FMC.
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
#' The FMC is either used as-provided in the input data or incorporates the \strong{depreciation fraction} that is deducted yearly
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
#' its simplified form (\code{$simple_output}), extra parameters including FMC and COL (\code{$extra}),
#' and the function call (\code{$call}). Optionally, specific cost and payback time plots of the results are also included
#' when \code{draw} is \code{TRUE} and an extraction curve is provided as \code{input} (\code{$plots}).
#' Any plot points removed due to economic \strong{non-}viability are also noted in element \code{$rm_state}.
#' When multiple responses are provided, \code{$response_details} contains a list of details for each response including
#' name, extract amount, sales volume, sale price, dilution factor, and response unit.
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
calcom <- function(input, invars = c(names(input)[1], names(input)[2]), pltlab = invars[1], cosol_loss = 0.05, msol_loss_frac = 0.01,
                   gen, crm, auxpr = NA, auxfr = NA, cut, col, fci, taxrate = NA, flowpar = "auto",
                   draw = TRUE, mass_flow = FALSE, comode = "sfe", use_coefs = FALSE,
                   coefs = c(fci = 1, col = 1, crm = 1, cut = 1), export = "none",
                   response_unit = "percent", moisture_content = 0, response_name = "Primary",
                   additional_responses = NULL, feed_density = 1) {

  #Preliminary checks
  if(length(invars)!=2) stop("Argument 'invars' must be a character vector or list of length 2!")
  if(!all(invars %in% names(input))) stop("At least one name in 'invars' is not present in the input data!")
  if(!is.data.frame(input)) input <- as.data.frame(t(input))
  if(!any(c("sfe","swe","cc_sfe") %in% comode)) stop("Argument 'comode' must be one of 'sfe', 'swe', or 'cc_sfe'!")
  if(!dir.exists(export) & export!="none") stop("The provided export directory does not exist!")
  if(!all(sapply(c(mass_flow,draw), is.logical))) stop("Arguments 'mass_flow' and 'draw' must both be logical!")
  if(!is.numeric(cosol_loss)|cosol_loss<0.001|cosol_loss>0.99) stop("The degree of co-solvent loss should be between 0.001 and 0.99!")
  sfechk <- comode %in% c("sfe", "cc_sfe")
  ccchk  <- comode == "cc_sfe"
  if(ccchk && (!is.numeric(msol_loss_frac)|msol_loss_frac<0|msol_loss_frac>1)) stop("Argument 'msol_loss_frac' must be between 0 and 1!")
  if(length(mass_flow)==1) mass_flow <- rep(mass_flow, if(ccchk) 3 else 2) #Main solvent, co-solvent, [feed for CC-SFE]
  if(ccchk && length(mass_flow)==2) mass_flow <- c(mass_flow, FALSE) #Default feed flow to volumetric
  if(!response_unit %in% c("percent", "g", "kg")) stop("Argument 'response_unit' must be one of 'percent', 'g', or 'kg'!")
  if(is.na(moisture_content)) moisture_content <- 0
  if(!is.numeric(moisture_content) | moisture_content < 0 | moisture_content > 100) stop("Argument 'moisture_content' must be a number between 0 and 100!")
  if(!is.character(response_name) || length(response_name) != 1) stop("Argument 'response_name' must be a single character string!")

  #Validate additional_responses if provided
  if(!is.null(additional_responses)) {
    if(!is.list(additional_responses)) stop("Argument 'additional_responses' must be a list!")
    if(length(additional_responses) > 3) stop("A maximum of 3 additional responses is supported!")

    is_single_input <- nrow(input) == 1

    for(i in seq_along(additional_responses)) {
      ar <- additional_responses[[i]]
      if(!is.list(ar)) stop(paste0("Additional response ", i, " must be a list!"))

      #Check for required pr_sale
      if(is.null(ar$pr_sale) || !is.numeric(ar$pr_sale)) {
        stop(paste0("Additional response ", i, " must have a numeric 'pr_sale' (sale price)!"))
      }

      #Check for column or value depending on input type
      if(is_single_input) {
        if(is.null(ar$value) || !is.numeric(ar$value)) {
          stop(paste0("Additional response ", i, " must have a numeric 'value' for single-value input!"))
        }
      } else {
        if(is.null(ar$column) || !ar$column %in% names(input)) {
          stop(paste0("Additional response ", i, " must have a valid 'column' name present in the input data!"))
        }
      }

      #Validate response_unit if provided
      if(!is.null(ar$response_unit) && !ar$response_unit %in% c("percent", "g", "kg")) {
        stop(paste0("Additional response ", i, " 'response_unit' must be one of 'percent', 'g', or 'kg'!"))
      }

      #Validate dilfac if provided
      if(!is.null(ar$dilfac) && (!is.numeric(ar$dilfac) || ar$dilfac < 0)) {
        stop(paste0("Additional response ", i, " 'dilfac' must be a non-negative number!"))
      }
    }
  }

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
  if(!ccchk) {
    usedvol <- pi*(crm["id"]/2)^2*crm["bh"]/1000
    if(usedvol > gen["volex"]) stop("The used volume cannot exceed extractor volume!")
  }
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
  if(!ccchk && !any(is.na(c(recvec)))) {
    if(crm[["recp"]]>gen[["pres"]]) stop("Extraction pressure cannot be higher than the CO2 recovery pressure!")
  }

  if(!ccchk && !all(is.na(recvec)) & any(is.na(recvec))) stop("When recovery pressure 'recp' is provided, recovery temperature 'rect'
                                                    and final separator temperature 'sept' are also required!")

  if(!is.na(fci[["depr"]]) & (fci[["depr"]] >= 1|fci[["depr"]]<0)) stop("Equipment depreciation fraction 'depr' cannot be more than 1 or less than 0!")


  #Create master output list to populate later
  finres <- list()

  #Begin COM calculations - COMPREHENSIVE DATA REPORTED ONLY FOR THE MAXIMUM EXTRACTION TIME!
  #STEP 1: Calculate S/M ratio
  #Convert flows to mass (g/min) if necessary
  #Main solvent flow (CO2 or water)

  if(!mass_flow[1]) {
    #Set up flow parameters
    if(any(flowpar %in% "auto")) {
      #Use recovery line parameters if available, otherwise use sensible defaults
      if(!is.na(crm[["recp"]]) && !is.na(crm[["rect"]])) {
        flowpar_main <- c(crm[["recp"]], crm[["rect"]])
      } else {
        #Default: 60 bar, 10 C for SFE; 1 bar, 25 C for SWE
        flowpar_main <- if(sfechk) c(60, 10) else c(1, 25)
      }
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
        #Use recovery line parameters if available, otherwise use sensible defaults
        if(!is.na(crm[["recp"]]) && !is.na(crm[["rect"]])) {
          flowpar_csol <- c(crm[["recp"]], crm[["rect"]])
        } else {
          #Default: 60 bar, 10 C for CO2 co-solvent in SWE; for SFE EtOH, density is calculated at 25 C
          flowpar_csol <- c(60, 10)
        }
      } else if(comode!="sfe") flowpar_csol <- flowpar[[2]]

      #Calculate CO2 density for SWE (EtOH density is a constant value at STP)
      csol_dens <- if(!sfechk) bendens(flowpar_csol[1], flowpar_csol[2])[[1]] else etoh_dens(25)[[1]]
      csol_flow <- csol_dens/1000*cur_csol

    } else csol_flow <- cur_csol
  } else csol_flow <- NA

  #CC-SFE: Convert feed flow to mass (g/min) if volumetric
  if(ccchk) {
    feed_flow_raw <- gen[["load"]]
    if(!mass_flow[3]) {
      feed_flow_mass <- feed_flow_raw * feed_density  #mL/min * g/mL = g/min
    } else {
      feed_flow_mass <- feed_flow_raw
    }
  }

  #Retrieve input data
  tm <- input[,invars[1]]
  resp <- input[,invars[2]]

  #Helper function to convert response to percentage if needed
  convert_response_to_pct <- function(response_values, resp_unit, moist_content, load_kg, verbose = TRUE) {
    if(resp_unit != "percent") {
      # Get dry mass of raw material (accounting for moisture content)
      dry_mass_kg <- load_kg * (1 - moist_content / 100)

      # Convert response to kg if in grams
      if(resp_unit == "g") {
        resp_kg <- response_values / 1000
      } else {
        resp_kg <- response_values  # Already in kg
      }

      # Calculate percentage yield based on dry mass
      result <- (resp_kg / dry_mass_kg) * 100

      if(verbose) {
        cat(paste0("\nResponse converted from ", resp_unit, " to %: ",
                   "dry mass = ", round(dry_mass_kg, 3), " kg, ",
                   "moisture = ", moist_content, "%"))
      }
      return(result)
    }
    return(response_values)
  }

  #Convert primary response to percentage if provided in absolute mass units
  if(ccchk) {
    cc_ref_kg <- feed_flow_mass * 60 / 1000
    resp <- convert_response_to_pct(resp, response_unit, moisture_content, cc_ref_kg, verbose = TRUE)
  } else {
    resp <- convert_response_to_pct(resp, response_unit, moisture_content, gen[["load"]], verbose = TRUE)
  }

  #Process additional responses if provided
  additional_resp_data <- NULL
  n_additional <- 0

  if(!is.null(additional_responses) && length(additional_responses) > 0) {
    n_additional <- length(additional_responses)
    additional_resp_data <- list()

    for(i in seq_along(additional_responses)) {
      ar <- additional_responses[[i]]

      #Get response values
      if(nrow(input) == 1) {
        ar_values <- ar$value
      } else {
        ar_values <- input[, ar$column]
      }

      #Get settings with defaults
      ar_name <- if(!is.null(ar$name)) ar$name else if(!is.null(ar$column)) ar$column else paste0("Response_", i)
      ar_unit <- if(!is.null(ar$response_unit)) ar$response_unit else "percent"
      ar_dilfac <- if(!is.null(ar$dilfac)) ar$dilfac else 1
      ar_pr_sale <- ar$pr_sale

      #Convert to percentage if needed
      cat(paste0("\nProcessing additional response '", ar_name, "'..."))
      ar_values_pct <- convert_response_to_pct(ar_values, ar_unit, moisture_content,
        if(ccchk) cc_ref_kg else gen[["load"]], verbose = TRUE)

      additional_resp_data[[i]] <- list(
        name = ar_name,
        values = ar_values,
        values_pct = ar_values_pct,
        response_unit = ar_unit,
        dilfac = ar_dilfac,
        pr_sale = ar_pr_sale
      )
    }
  }

  #Compile input parameters
  cat("\nCompiling input data...")
  desc_com <- show_pars("com")
  inpars <- na.omit(c(gen, crm, cosol_loss = cosol_loss, cut, col, fci))
  inpars <- setNames(cbind.data.frame(names(inpars), inpars), c("parameter", "value"))
  inpars[,c("units", "type", "description")] <- desc_com[["input"]][match(inpars[,"parameter"], desc_com[["input"]][,"parameter"]),
                                                                    c("units", "type", "description")]
  inpars[inpars[,"parameter"] %in% c("flow", "csol_flow"),"units"] <- ifelse(mass_flow[1:2], "g/min", "mL/min") #Specify flow units

  #CC-SFE: Adjust input parameter labels
  if(ccchk) {
    load_idx <- which(inpars[,"parameter"] == "load")
    if(length(load_idx) > 0) {
      feed_unit <- if(mass_flow[3]) "g/min" else "mL/min"
      inpars[load_idx, "description"] <- "Feed flow rate"
      inpars[load_idx, "units"] <- feed_unit
    }
    #Add feed density row right after load (CC-SFE only)
    fd_row <- data.frame(parameter = "feed_density", value = feed_density,
                         units = "g/mL", type = "gen",
                         description = "Feed density", stringsAsFactors = FALSE)
    inpars <- rbind.data.frame(inpars[1:load_idx, , drop = FALSE], fd_row,
                               inpars[(load_idx + 1):nrow(inpars), , drop = FALSE])
    rownames(inpars) <- NULL
  }

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

  #Calculate S/M ratio (main solvent, i.e. CO2 or water)
  dlst <- list()
  if(ccchk) {
    dlst[["sm"]] <- main_flow / feed_flow_mass
  } else {
    dlst[["sm"]] <- main_flow*tm/(gen[["load"]]*1000)
  }

  #Calculate S/M ratio (co-solvent, if applicable)
  if(!is.na(csol_flow) && csol_flow > 0) {
    if(ccchk) {
      dlst[["sm_csol"]] <- csol_flow / feed_flow_mass
    } else {
      dlst[["sm_csol"]] <- csol_flow*tm/(gen[["load"]]*1000)
    }
  } else {
    dlst[["sm_csol"]] <- NA
  }

  #Calculate monthly throughput
  whrd <- col[["whr"]]*col[["shifts"]]
  if(ccchk) {
    monthly_minutes <- whrd * col[["wdays"]] * 60
    dlst[["cycles"]] <- cycles <- NA
    cc_monthly_feed_kg <- feed_flow_mass * monthly_minutes / 1000
  } else {
    dlst[["cycles"]] <- cycles <- floor(whrd/((tm+gen[["extime"]])/60))*col[["wdays"]]
  }

  #Calculate main solvent and co-solvent usage per batch (or per month for CC-SFE)
  if(ccchk) {
    #CC-SFE: Recirculating system. Solvent loss = fraction of total monthly CO2 throughput
    #msol_loss_frac accounts for system leakage, valve losses, incomplete recovery
    monthly_co2_kg <- main_flow/1000 * monthly_minutes
    dlst[["msol_loss"]] <- msol_loss <- monthly_co2_kg * msol_loss_frac
    dlst[["csol_loss"]] <- csol_loss <- if(!is.na(csol_flow) && csol_flow > 0) csol_flow/1000 * monthly_minutes * cosol_loss else NA
  } else if(comode=="sfe") {
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
    if(ccchk) {
      req_arg <- list(cc_monthly_feed_kg, msol_loss, csol_loss)[[losschk]]
    } else {
      req_arg <- list(gen[["load"]], msol_loss, csol_loss)[[losschk]]
    }
    cost_arg <- unname(crm[c("pr_mat","pr_msol","pr_csol")])[losschk]

    if(ccchk) {
      dlst[[paste0(i,"_req")]] <- cureq <- req_arg
      dlst[[paste0(i,"_cost")]] <- cureq * cost_arg
    } else {
      dlst[[paste0(i,"_req")]] <- cureq <- req_arg * cycles
      dlst[[paste0(i,"_cost")]] <- cureq * cost_arg
    }
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
  if(ccchk) {
    dlst[["pwm_req"]] <- pwm_req <- cut[["pw_main"]] * whrd * col[["wdays"]]
  } else {
    dlst[["pwm_req"]] <- pwm_req <- cut[["pw_main"]]*(tm/60)*cycles
  }
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

  #Calculate total electricity consumption and cost (for display purposes)
  dlst[["elec_cons"]] <- pwm_req + pwa_req
  dlst[["elec_cost"]] <- dlst[["elec_cons"]] * cut[["pr_kwh"]]

  #Calculate CRM, CUT, COL, FMC, and COM
  base_fci <- sum(fci[c("maint","other")], na.rm = TRUE)
  FCI <- if(!is.na(fci[["depr"]])) base_fci + (fci[["capex"]] * fci[["depr"]])/12 else base_fci
  COL <- col[["wage"]] * col[["oper"]] * col[["shifts"]]
  dlst[["crm"]] <- CRM <- unname(colSums(do.call(rbind.data.frame, dlst[c("raw_cost","msol_cost","csol_cost","aux_cost")]), na.rm = TRUE)) #Reduce("+", dlst[c("raw_cost","msol_cost","csol_cost","aux_cost")])
  dlst[["cut"]] <- CUT <- unname(colSums(do.call(rbind.data.frame, dlst[c("pwm_cost","pwa_cost")]), na.rm = TRUE))
  dlst[["com"]] <- COM <- if(use_coefs) {
    1.23*(CUT+CRM) + 2.73*COL + 0.304*FCI
  } else {
    coefs[["crm"]]*CRM + coefs[["cut"]]*CUT + coefs[["fci"]]*FCI + coefs[["col"]]*COL
  }

  #Calculate specific cost and other economic indicators necessary for Payback Period
  #PRIMARY RESPONSE
  dlst[["extract"]] <- primary_extract <- dlst[["raw_req"]]*(resp/100)
  if(!is.na(gen[["dilfac"]]) & gen[["dilfac"]]>0) dlst[["extract"]] <- primary_extract <- primary_extract * gen[["dilfac"]]
  dlst[["svol"]] <- primary_svol <- gen[["pr_sale"]] * primary_extract

  #Store primary response details for multi-response output
  response_details <- list()
  response_details[[1]] <- list(
    name = response_name,
    extract = primary_extract,
    svol = primary_svol,
    pr_sale = gen[["pr_sale"]],
    dilfac = if(!is.na(gen[["dilfac"]])) gen[["dilfac"]] else 1,
    response_unit = response_unit
  )

  #ADDITIONAL RESPONSES - Calculate extract and sales volume for each
  total_svol <- primary_svol
  total_extract <- primary_extract

  if(!is.null(additional_resp_data) && length(additional_resp_data) > 0) {
    cat("\nCalculating revenue from additional responses...")

    for(i in seq_along(additional_resp_data)) {
      ar <- additional_resp_data[[i]]

      #Calculate extract amount for this response
      ar_extract <- dlst[["raw_req"]] * (ar$values_pct / 100)
      if(ar$dilfac > 0) ar_extract <- ar_extract * ar$dilfac

      #Calculate sales volume for this response
      ar_svol <- ar$pr_sale * ar_extract

      #Store in detailed list
      dlst[[paste0("extract_", i)]] <- ar_extract
      dlst[[paste0("svol_", i)]] <- ar_svol

      #Store response details
      response_details[[i + 1]] <- list(
        name = ar$name,
        extract = ar_extract,
        svol = ar_svol,
        pr_sale = ar$pr_sale,
        dilfac = ar$dilfac,
        response_unit = ar$response_unit
      )

      #Add to totals
      total_svol <- total_svol + ar_svol
      total_extract <- total_extract + ar_extract

      cat(paste0("\n  ", ar$name, ": extract = ", round(mean(ar_extract), 3), " kg, ",
                 "sales = $", round(mean(ar_svol), 2)))
    }
  }

  #Store total sales volume (overwrite primary-only svol)
  dlst[["svol_total"]] <- total_svol
  dlst[["extract_total"]] <- total_extract

  #Calculate overall specific cost based on total extract
  dlst[["sc"]] <- COM / total_extract

  #Calculate profit and margins based on TOTAL revenue
  dlst[["mgp"]] <- total_svol - COM  # Gross profit = Total Revenue - Total Cost
  if(!is.na(taxrate)) dlst[["mnp"]] <- curpr <- dlst[["mgp"]] * (1-taxrate/100) else curpr <- dlst[["mgp"]]
  dlst[["pmargin"]] <- curpr/total_svol*100
  dlst[["payback"]] <- fci[["capex"]]/(curpr*12)

  #Store response details in final results
  finres[["response_details"]] <- response_details

  #Populate detailed output data frame
  detcols <- grep("_min", colnames(detout))
  for(i in names(dlst)) {
    if(i %in% detout[,"parameter"]) {
      detout[detout[,"parameter"]==i, detcols] <- dlst[[i]]
    }
  }

  #Update svol and extract in detout to use totals if we have multiple responses
  if(n_additional > 0) {
    if("svol" %in% detout[,"parameter"]) {
      detout[detout[,"parameter"]=="svol", detcols] <- total_svol
    }
    if("extract" %in% detout[,"parameter"]) {
      detout[detout[,"parameter"]=="extract", detcols] <- total_extract
    }
  }

  detout <- detout[complete.cases(detout),]
  rownames(detout) <- NULL

  #CC-SFE: Adjust detailed output labels
  if(ccchk) {
    det_fixes <- list(
      msol_loss = list(description = "CO2 loss per month", units = "kg/month"),
      csol_loss = list(description = "Co-solvent loss per month", units = "kg/month"),
      sm        = list(description = "S/F mass ratio (CO2)"),
      sm_csol   = list(description = "S/F mass ratio (co-solvent)")
    )
    for(pname in names(det_fixes)) {
      idx <- which(detout[,"parameter"] == pname)
      if(length(idx) > 0) {
        for(col in names(det_fixes[[pname]])) {
          detout[idx, col] <- det_fixes[[pname]][[col]]
        }
      }
    }

    #Add CC-SFE production overview rows (not in desc_pars)
    det_cols <- grep("_min", colnames(detout))
    cc_prod_rows <- data.frame(
      parameter = c("cc_op_time", "cc_feed_processed"),
      description = c("Monthly operating time", "Monthly feed processed"),
      units = c("hours", "kg"),
      stringsAsFactors = FALSE
    )
    for(dc in det_cols) cc_prod_rows[[colnames(detout)[dc]]] <- NA
    cc_prod_rows[1, det_cols] <- round(monthly_minutes / 60, 1)
    cc_prod_rows[2, det_cols] <- round(cc_monthly_feed_kg, 2)
    detout <- rbind(cc_prod_rows, detout)  #Prepend so they appear first
    rownames(detout) <- NULL
  }

  finres[["output"]] <- detout

  #Compile simplified output
  cat("\nCompiling simplified output data...")
  mnpchk <- "mnp" %in% detout[,"parameter"]

  #Update dlst svol/extract for backward compatibility
  if(n_additional > 0) {
    dlst[["svol"]] <- total_svol
    dlst[["extract"]] <- total_extract
  }

  simcols <- c(if(!ccchk) "cycles", "cut", "crm", "com", "svol", "extract", "sc", if(mnpchk) "mnp" else "mgp", "pmargin", "payback")
  simout <- cbind.data.frame(tm, resp, as.data.frame(t(detout[c(match(simcols, detout[,"parameter"], nomatch = 0),0),
                                                              detcols, drop = FALSE]))) #which(detout[,"parameter"] %in% simcols)
  rownames(simout) <- NULL
  colnames(simout) <- c(invars[1], invars[2], if(!ccchk) "monthly_cycles", "CUT", "CRM", "COM", "sales_volume", "extract_made", "SC_per_kg", "monthly_profit", "margin_percent", "payback_yr")

  #If we have multiple responses, update sales_volume and extract_made to use totals
  if(n_additional > 0) {
    simout[,"sales_volume"] <- total_svol
    simout[,"extract_made"] <- total_extract
  }

  #Flag economically non-viable points (negative payback) but do NOT remove them
  finres[["simple_output"]] <- NA
  rmchk <- which(simout[,"payback_yr"]<0)
  if(length(rmchk)>0 & length(tm)>1) {
    finres[["rm_state"]] <- rm_state <- paste0("Note: The following extraction time(s) have negative payback (not economically viable at current prices): ",
                                               paste0(simout[rmchk,invars[1]], " min", collapse = ", "),".")
    cat("\n",rm_state, sep = "")
    # Previously removed non-viable points; now we keep them for display purposes
    # simout <- simout[-rmchk,]
  }
  finres[["simple_output"]] <- simout

  #Create organized tables for structured output (matching Shiny UI)
  cat("\nCompiling structured tables...")
  tables <- list()

  #Helper function to create a table row
  make_row <- function(desc, val, unit) {
    data.frame(Description = desc, Value = val, Unit = unit, stringsAsFactors = FALSE)
  }

  #1. Production Overview table
  prod_rows <- list()
  if(ccchk) {
    prod_rows[[length(prod_rows) + 1]] <- make_row("Monthly operating time", round(monthly_minutes / 60, 1), "hours")
    prod_rows[[length(prod_rows) + 1]] <- make_row("Monthly feed processed", round(cc_monthly_feed_kg, 2), "kg")
  } else {
    prod_rows[[length(prod_rows) + 1]] <- make_row("Extraction cycles per month", round(mean(dlst[["cycles"]]), 2), "none")
  }
  prod_rows[[length(prod_rows) + 1]] <- make_row(if(ccchk) "S/F mass ratio (CO2)" else "S/M mass ratio (CO2)", round(mean(dlst[["sm"]]), 2), "none")

  #Add co-solvent S/M ratio if applicable
  if(!all(is.na(dlst[["sm_csol"]])) && any(dlst[["sm_csol"]] > 0, na.rm = TRUE)) {
    prod_rows[[length(prod_rows) + 1]] <- make_row(if(ccchk) "S/F mass ratio (co-solvent)" else "S/M mass ratio (co-solvent)", round(mean(dlst[["sm_csol"]], na.rm = TRUE), 2), "none")
  }

  #Add per-response extract breakdown if multiple responses
  if(n_additional > 0) {
    #Add header row
    prod_rows[[length(prod_rows) + 1]] <- make_row("--- Extract produced (monthly) by response ---", "", "")

    #Add each response
    for(i in seq_along(response_details)) {
      resp_detail <- response_details[[i]]
      prod_rows[[length(prod_rows) + 1]] <- make_row(
        paste0("  ", resp_detail$name),
        round(mean(resp_detail$extract), 2),
        "kg/month"
      )
    }
    #Add total
    prod_rows[[length(prod_rows) + 1]] <- make_row("  Total", round(mean(total_extract), 2), "kg/month")
  } else {
    prod_rows[[length(prod_rows) + 1]] <- make_row("Extract produced (monthly)", round(mean(dlst[["extract"]]), 2), "kg/month")
  }
  tables[["production"]] <- do.call(rbind, prod_rows)

  #2. Cost of Raw Materials (CRM) table
  crm_rows <- list(
    make_row(if(ccchk) "CO2 loss per month" else "Main solvent loss per cycle",
             round(mean(dlst[["msol_loss"]]), 4), if(ccchk) "kg/month" else "kg/cycle"),
    make_row(if(ccchk) "Co-solvent loss per month" else "Co-solvent loss per batch",
             round(mean(dlst[["csol_loss"]]), 4), if(ccchk) "kg/month" else "kg/cycle"),
    make_row("Raw material requirement (monthly)", round(mean(dlst[["raw_req"]]), 2), "kg/month"),
    make_row("Main solvent requirement (monthly)", round(mean(dlst[["msol_req"]]), 2), "kg/month"),
    make_row("Co-solvent requirement (monthly)", round(mean(dlst[["csol_req"]]), 2), "kg/month"),
    make_row("Raw material cost (monthly)", round(mean(dlst[["raw_cost"]]), 2), "USD/month"),
    make_row("Main solvent cost (monthly)", round(mean(dlst[["msol_cost"]]), 2), "USD/month"),
    make_row("Co-solvent cost (monthly)", round(mean(dlst[["csol_cost"]]), 2), "USD/month"),
    make_row("Auxiliary material cost (monthly)", round(mean(dlst[["aux_cost"]]), 2), "USD/month"),
    make_row("Cost of Raw Materials (CRM)", round(mean(dlst[["crm"]]), 2), "USD/month")
  )
  tables[["crm"]] <- do.call(rbind, crm_rows)

  #3. Cost of Utilities (CUT) table
  cut_rows <- list(
    make_row("Main power consumption (monthly)", round(mean(dlst[["pwm_req"]]), 2), "kWh/month"),
    make_row("Main power cost (monthly)", round(mean(dlst[["pwm_cost"]]), 2), "USD/month"),
    make_row("Drying power consumption (monthly)", round(mean(dlst[["pwdry_req"]]), 2), "kWh/month"),
    make_row("Drying power cost (monthly)", round(mean(dlst[["pwdry_cost"]]), 2), "USD/month"),
    make_row("Compression power consumption (monthly)", round(mean(dlst[["pwcom_req"]]), 2), "kWh/month"),
    make_row("Compression power cost (monthly)", round(mean(dlst[["pwcom_cost"]]), 2), "USD/month"),
    make_row("Evaporation power consumption (monthly)", round(mean(dlst[["pwevap_req"]]), 2), "kWh/month"),
    make_row("Evaporation power cost (monthly)", round(mean(dlst[["pwevap_cost"]]), 2), "USD/month"),
    make_row("Total electricity consumption (monthly)", round(mean(dlst[["elec_cons"]]), 2), "kWh/month"),
    make_row("Total electricity cost (monthly)", round(mean(dlst[["elec_cost"]]), 2), "USD/month"),
    make_row("Cost of Utilities (CUT)", round(mean(dlst[["cut"]]), 2), "USD/month")
  )
  tables[["cut"]] <- do.call(rbind, cut_rows)

  #4. Financial Results table (combined summary)
  fin_rows <- list(
    make_row("Cost of Raw Materials (CRM)", round(mean(dlst[["crm"]]), 2), "USD/month"),
    make_row("Cost of Utilities (CUT)", round(mean(dlst[["cut"]]), 2), "USD/month"),
    make_row("Cost of Labor (COL)", round(COL, 2), "USD/month"),
    make_row("Fixed Costs (FMC)", round(FCI, 2), "USD/month"),
    make_row("Cost of Manufacturing (COM)", round(mean(dlst[["com"]]), 2), "USD/month"),
    make_row("Specific extract cost", round(mean(dlst[["sc"]]), 2), "USD/kg"),
    make_row("Sales volume (monthly)", round(mean(total_svol), 2), "USD/month"),
    make_row("Monthly gross profit (pre-tax)", round(mean(dlst[["mgp"]]), 2), "USD/month")
  )
  if(!is.na(taxrate)) {
    fin_rows[[length(fin_rows) + 1]] <- make_row("Monthly profit (after tax)", round(mean(dlst[["mnp"]]), 2), "USD/month")
  }
  fin_rows[[length(fin_rows) + 1]] <- make_row("Profit margin", round(mean(dlst[["pmargin"]]), 2), "percent")
  fin_rows[[length(fin_rows) + 1]] <- make_row("Payback period", round(mean(dlst[["payback"]]), 2), "years")

  #Add weighted average price if multiple responses
  if(n_additional > 0) {
    weighted_avg_price <- mean(total_svol) / mean(total_extract)
    fin_rows[[length(fin_rows) + 1]] <- make_row("Weighted average price", round(weighted_avg_price, 2), "USD/kg")
  }
  tables[["financial"]] <- do.call(rbind, fin_rows)

  #5. Per-response financial tables (only if multiple responses)
  if(n_additional > 0) {
    tables[["per_response"]] <- list()
    com_total <- mean(dlst[["com"]])
    total_profit <- mean(dlst[["mgp"]])  # Total gross profit
    overall_payback <- mean(dlst[["payback"]])  # Overall payback period

    for(i in seq_along(response_details)) {
      resp_detail <- response_details[[i]]
      resp_extract <- mean(resp_detail$extract)
      resp_svol <- mean(resp_detail$svol)

      #Calculate proportional cost share
      extract_share <- resp_extract / mean(total_extract)
      resp_cost <- com_total * extract_share

      #Sole production cost: COM / this response's extract alone
      sole_prod_cost <- com_total / resp_extract

      resp_profit <- resp_svol - resp_cost
      resp_margin <- (resp_profit / resp_svol) * 100

      #Contribution to payback: percentage of total profit this response provides
      profit_contribution_pct <- if(total_profit > 0) (resp_profit / total_profit) * 100 else 0

      resp_rows <- list(
        make_row("Extract produced (monthly)", round(resp_extract, 2), "kg/month"),
        make_row("Sale price", round(resp_detail$pr_sale, 2), "USD/kg"),
        make_row("Sole production cost", round(sole_prod_cost, 2), "USD/kg"),
        make_row("Sales volume (monthly)", round(resp_svol, 2), "USD/month"),
        make_row("Cost share (proportional)", round(resp_cost, 2), "USD/month"),
        make_row("Gross profit", round(resp_profit, 2), "USD/month"),
        make_row("Profit margin", round(resp_margin, 2), "percent"),
        make_row("Contribution to payback", round(profit_contribution_pct, 1), "percent")
      )
      tables[["per_response"]][[resp_detail$name]] <- do.call(rbind, resp_rows)
    }
  }

  #6. Performance Overview (time-series data) - only for multi-value input
  if(length(tm) > 1) {
    perf_data <- data.frame(
      Time = tm,
      Yield = round(resp, 2),
      Monthly_Profit = round(simout$monthly_profit, 2),
      Margin = round(simout$margin_percent, 2),
      Payback = round(simout$payback_yr, 2),
      Specific_Cost = round(simout$SC_per_kg, 2)
    )
    colnames(perf_data) <- c("Time (min)", "Yield (%)", "Monthly Profit ($)", "Margin (%)", "Payback (years)", "Specific Cost ($/kg)")
    tables[["performance"]] <- perf_data
  }

  finres[["tables"]] <- tables

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
