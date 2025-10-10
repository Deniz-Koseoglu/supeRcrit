#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Construct the SIMPLIFIED, CHARACTERISTIC TIMES, and COMPLETE BIC models
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Construct BIC models of varying complexity
#'
#' @description Constructs various Broken and Intact Cells (BIC) kinetic models from
#' overall extraction curves (OECs) of supercritical CO2, subcritical water, or any
#' other process. Consult \strong{References} and the \strong{See Also} section for further information.
#' \strong{This workflow is currently a Work in Progress (WIP)}.
#'
#' @param oec A \code{data.frame} of OEC data. Must include all \code{oec_vars}.
#' @param oec_vars A \strong{named} \code{character} vector of column names included in \code{oec}.
#' These \strong{must} include the extraction time (\code{"x"}) and response (\code{"y"}) with appropriate \code{units}.
#' A third parameters indicating the solvent usage (\code{"slv"}) is \strong{optional} and may be provided as an
#' \strong{alternative} to an explicit flow rate normally provided in \code{pars["flow"]} - this is useful when the
#' flow rate is uneven throughout the extraction process.
#' @param pars A named \code{numeric} vector of input parameters for the model, divided into \strong{mandatory} and
#' \strong{optional} parameters. \strong{Mandatory} parameters include pressure (\code{"pres"}; bar),
#' temperature (\code{"temp"}; degC), mass of material loaded (\code{"mass_in"}; g), moisture content
#' (\code{"moisture"}; %), diameter (\code{"D"}; m) and length (\code{"L"}; m) of the extraction vessel,
#' the fraction of ethanol co-solvent (\code{"etoh"}), the real (\code{"dr"}; g/L) and apparent (\code{"dp"}; g/L)
#' densities of the raw material, and the number of observations/OEC points corresponding to the end of the CER (\code{"n"}).
#' The \strong{optional} variables default to \code{NA} and include the flow rate of solvent (\code{"flow"}; units set in \code{units}),
#' and the maximum extractable material fraction (\code{"cu"}), which \strong{must be included unless} \code{modtype} is set to \code{"cu"}.
#' @param opt_est Either \code{"default"} or a \strong{named} \code{numeric} vector of initial parameter estimates
#' for iterative optimization. May include any of grinding efficiency \eqn{r} (\code{"r"}), the product of the solid phase
#' mass transfer coefficient (\eqn{k_s}) and the specific area between intact and broken cells (\eqn{a_s}) \eqn{k_sa_s} (\code{"ksas"}),
#' external mass transfer coefficient \eqn{\theta_e} (\code{"thetaf"}), extraction duration of FER \eqn{t_i} (\code{"ti"}),
#' the fluid phase mass transfer coefficient \eqn{k_f} (\code{"kf"}), and/or the relative amount of solvent expended
#' at the end of CER \eqn{q_m} (\code{"qc"}). An additional parameter \code{"c3"} is related to the maximum extractable material fraction
#' \eqn{c_u} and is \strong{only required} when \code{modtype} is set to \code{"cu"}.
#' Any values not specified will be set to the following defaults:
#' \code{c(r = 0.4, ksas = 10e-5, qc = 400, thetaf = 1, ti = 30, kf = 0.001, c3 = 0.15)}.
#' @param etoh_frac The single \code{numeric} fraction of ethanol co-solvent (between 0-0.99). Defaults to \code{0}.
#' Must be non-zero if \strong{flow rate} is not provided in \code{pars} and \code{pars["etoh"]} is \strong{non-zero}.
#' @param flowpar Either \code{NA} (default) or a \code{numeric} vector of length 2 providing temperature and pressure
#' at which flow rate of CO2 is measured.
#' @param ro_co2 The supercritical CO2 density (in g/L). If not provided (\code{NA}; default),
#' it is calculated via the Bender Equation of State (see \code{\link{bendens}}).
#' @param tmax Maximum x-axis value (time, min) to use for model predictions \strong{exclusively} for \code{modtype = "ct"}.
#' Setting to \code{NA} defaults to 120% of the maximum experimental value.
#' @param qmax Maximum x-axis value (solvent expended, kg/kg insoluble solid) to use for model predictions for all models except
#' \code{modtype = "ct"}. Setting to \code{NA} defaults to 120% of the maximum experimental value.
#' @param cumulative A \code{logical} switch specifying whether the \strong{response} and/or \strong{solvent consumption}
#' values provided in \code{oec} are cumulative or not (defaults to \code{FALSE}).
#' @param mass_flow A \code{logical} indicating whether the flow rate provided in \code{pars} is \strong{mass}
#' or \strong{volumetric} (\code{FALSE}; default).
#' @param draw A \code{logical} switch. Should generated plots be plotted? Defaults to \code{TRUE}.
#' @param aggreg A string specifying how the "best" results of non-linear optimization (\code{nlsm}) should be chosen.
#' One of \code{"aard"} (default; value with minimum Average Absolute Relative Deviation is chosen) or \code{"mean"}
#' (the arithmetic mean of all results is taken).
#' @param modtype Specifies the type(s) of BIC models to generate. Either \code{"all"} (default) or one or more of:
#' \code{"sim"} (simplified), \code{"ct"} (characteristic times), \code{"cmp3"} (complete with 3 OEC regions),
#' and/or \code{"cmp2"} (complete with 2 OEC regions). A separate option \code{"cu"} is used when the
#' extractable material fraction \eqn{c_u} is to be estimated.
#' @param units A \strong{named} \code{character} vector of length 1 or 2 specifying the units of \code{"flow"}
#' (one of \code{"mL/min"}, the \strong{default} \code{"g/min"}, \code{"kg/h"}, \code{"L/h"}, or \code{"none"}) and/or
#' the response \code{"resp"} (one of \code{"g"}, the \strong{default} \code{"percent"}, \code{"permille"}, \code{"ppm"},
#' or \code{"ppb"}). Where not provided, default values are used.
#' @param silent Should console output be silenced? Defaults to \code{FALSE}).
#'
#' @return The output takes two forms. When \code{modtype} includes \code{"cu"} (for estimation of the maximum extractable fraction \eqn{c_u}),
#' the output is a \code{list} of the estimated \eqn{c_u} value (\code{$cu}) and the iteratively derived final estimate of the related constant
#' \strong{c3} (\code{$mod_coefs}).
#' Alternatively, when \code{modtype} includes any of \code{c("sim","ct","cmp3","cmp2")}, output is a \code{list} containing
#' the following elements:
#' \enumerate{
#' \item \strong{$data}: A \code{data.frame} of original \code{oec} input data with added values of Solvent-Material (S/M) ratio \code{$q},
#' as well as fractional yields \code{$e} (g/g insoluble solid) and \code{$estar} (g/g total dry solid used for \code{modtype == "ct"}).
#' \item \strong{$sim, $ct, and/or $cmp}: Each a \code{list} of results for the eponymous BIC model, with output structure outlined in
#' the documentation of \code{\link{bic_sm}}, \code{\link{bic_ct}}, and \code{\link{bic_cmp}}, respectively.
#' \item \strong{$plots}: A \code{list} of plot objects of class \code{"ggplot"}.
#' \item \strong{$input}: A named \code{numeric} vector of input parameters. In addition to those provided in argument \code{pars},
#' some calculated parameters are also included. These are the specific surface area per unit volume of extraction bed \eqn{a_0} (\code{"a0"};
#' 1/m), the apparent porosity \eqn{\varepsilon} (\code{"porosity"}), the CO2 to insoluble solid ratio in the extraction bed \eqn{\gamma}
#' (\code{"gamma"}; kg/kg), the number of experimental points \eqn{m} (\code{"m"}), total \strong{dry} mass of material \eqn{N} (\code{"Ng"}; g),
#' mass of \strong{insoluble} material \eqn{N_m} (\code{"Nm"}; g), the ratio of solute to insoluble material \eqn{x_u} (\code{"xu"}; kg/kg),
#' and the apparent extract solubility \eqn{y_s} (\code{"ys"}; g/g).
#' \item \strong{$call}: The function call.
#' }
#' @export
#'
#' @details
#' This workflow derives and visualizes three different Broken and Intact Cells (BIC) kinetic models based on the work of
#' Sovova (2005, 2012, 2017) and previously utilized by Rizza (2014) and many other authors. These include the \strong{simplified},
#' \strong{characteristic times}, and \strong{complete} models - descriptions of associated parameters may be found in the descriptions of
#' \code{\link{bic_sm}}, \code{\link{bic_ct}}, and \code{\link{bic_cmp}}, respectively, and their linked functions.
#'
#' @references
#' Rizza, C.S. (2014), \emph{Experiments and Modeling of Supercritical CO2 Extraction of Lipids from Microalgae}, MSc thesis, Universita Degli Studi Di Padova, Dipartimento Di Ingegneria Industriale.
#'
#' Sovova, H. (2005), 'Mathematical model for supercritical fluid extraction of natural products and extraction curve evaluation', \emph{The Journal of Supercritical Fluids} \strong{33} (1), pp. 35-52, DOI: \url{https://doi.org/10.1016/j.supflu.2004.03.005}.
#'
#' Sovova, H. (2012), 'Steps of supercritical fluid extraction of natural products and their characteristic times', \emph{The Journal of Supercritical Fluids} \strong{66}, pp. 73-79, DOI: \url{https://doi.org/10.1016/j.supflu.2011.11.004}.
#'
#' Sovova, H. (2017), 'Broken-and-intact cell model for supercritical fluid extraction: Its origin and limits', \emph{The Journal of Supercritical Fluids} \strong{129}, pp. 3-8, DOI: \url{https://doi.org/10.1016/j.supflu.2017.02.014}.
#'
#' @examples
#' bic_res <- bicmod(oec = sfex[[2]][["data"]],
#' oec_vars = c(x = "Time_min", y = "Yield_g", slv = "Solvent_mL"),
#' pars = c(pres = 300,
#'          cu = 0.165,
#'          temp = 45,
#'          flow = NA,
#'          mass_in = 0.5125,
#'          moisture = 8.6,
#'          D = 0.015,
#'          L = 0.015,
#'          etoh = 0.5,
#'          dr = 1554,
#'          dp = 0.0004,
#'          n = 2),
#' opt_est = "default",
#' flowpar = c(1.01325, 25),
#' etoh_frac = 0.06, #Required when CO2 flow is not provided but 'etoh' is non-zero
#' ro_co2 = NA,
#' tmax = NA,
#' qmax = NA,
#' cumulative = FALSE,
#' mass_flow = FALSE,
#' draw = TRUE,
#' units = c(flow = "none", resp = "g"),
#' modtype = "all")
#'
#' @seealso \code{\link{bendens}}, \code{\link{etoh_dens}}, \code{\link{bic_sm}}, \code{\link{bic_ct}}, \code{\link{bic_cmp}}, \code{\link{show_pars}}, \code{\link{kin_plot}}, \code{\link{kin_splot}}, \code{\link{kin_export}}
#'
#' @importFrom stats complete.cases setNames
#' @importFrom pracma polyfit
#'
bicmod <- function(oec, oec_vars, pars, opt_est = "default", etoh_frac = 0, flowpar = rep(NA,2), ro_co2 = NA, tmax = NA, qmax = NA,
                   cumulative = FALSE, mass_flow = FALSE, draw = TRUE, aggreg = "aard", modtype = "all",
                   units = "default", silent = FALSE) {

  #Preliminary checks
  modtps <- c("sim", "cu", "ct", "cmp3", "cmp2")
  if(any(modtype %in% "all")) modtype <- modtps[!modtps %in% "cu"] else if(any(modtype %in% "cu")) {
    modtype <- "cu"
    cat("Argument 'modtype' includes 'cu' estimation. Cancelling building of all other models...\n")
  }
  if(!any(c("aard","mean") %in% aggreg)) stop("The fitted parameter aggregation setting 'aggreg' may only be one of: 'aard', 'mean'!")
  if(!all(modtype %in% modtps)) stop(paste0("BIC model type not recognised! Must be 'all' or one or more of: ", paste0("'", modtps, "'", collapse = ", "),"."))
  if(length(flowpar)!=2|!is.numeric(flowpar) & !all(is.na(flowpar))) stop("Pressure and temperature of flow rate measurements 'flowpar' must be a numeric vector of length 2!")
  if(!all(names(oec_vars) %in% c("x","y","slv"))) stop("Argument 'oec_vars' must be a named vector! Possible names are: 'x', 'y', and 'slv' (optional).")
  if(!is.data.frame(oec)) stop("Argument 'oec' must be a data.frame!") else if(!all(oec_vars %in% colnames(oec))) stop("Undefined column names included in 'oec_vars'!")
  if(!is.numeric(etoh_frac)|etoh_frac > 0.99) stop("Ethanol fraction must either be zero or a fraction <0.99!")
  #if(!is.numeric(cu)|cu > 1) stop("Maximum extractable fraction 'cu' must either be zero or a fraction less than or equal to 1!")
  if(!all(sapply(c(cumulative,mass_flow),is.logical))) stop("Arguments 'cumulative' and 'mass_flow' must be logical!")
  parnms <- c("pres", "temp", "mass_in", "moisture", "D", "L", "etoh", "dr", "dp", "n")
  if(!all(parnms %in% names(pars))) stop(paste0("Vector of parameters must include names: ", paste0("'", parnms, "'", collapse = ", "), "!"))
  if(!any(names(pars) %in% "flow")) pars <- c(pars, flow = NA)
  if(!any(names(pars) %in% "cu")) pars <- c(pars, cu = NA)
  if(!any(names(opt_est) %in% "c3")) opt_est <- c(opt_est, c3 = NA)

  #Set up units
  if(!silent) cat("\nSetting up units...")
  posun_flow <- c("mL/min", "g/min", "kg/h", "L/h", "none")
  posun_resp <- c("g", "percent", "permille", "ppm", "ppb")
  defunits <- c(flow = "g/min", resp = "percent")
  units <- supp_pars(defunits, units, parlb = "units")

  if(!all(c("flow","resp") %in% names(units))) {
    stop("The 'units' vector must be named with the following: 'flow' and 'resp'!")
  } else if(!any(posun_flow %in% units["flow"])|!any(posun_resp %in% units["resp"])) {
    stop(paste0("Unrecognised values in the 'units' argument!
                Accepted units are: ", paste0("'", posun_flow, "'", collapse = ", "), " for flow;",
                paste0("'", posun_resp, "'", collapse = ", "), " for response."))
  } else if(mass_flow & grepl("^mL|^L",units["flow"])) stop("When 'mass_flow' is TRUE, the flow 'units' must not be volumetric!")
  if(!is.na(pars["flow"]) & "slv" %in% names(oec_vars)) {
    cat("When flow rate is provided, solvent expenditure data in 'oec' is ignored...\n")
    oec <- oec[,!colnames(oec) %in% oec_vars["slv"]]
    names(oec_vars) <- names(oec_vars)[!names(oec_vars) %in% "slv"]
  }
  if(is.na(pars["flow"]) & pars["etoh"]!=0 & etoh_frac==0) stop("When CO2 flow rate is not provided and EtOH co-solvent flow is non-zero, EtOH flow fraction ('etoh_frac') MUST be provided!")
  if(any(names(oec_vars) %in% "slv") & !silent) cat("Note that when total expended solvent is given instead of a flow rate, the units must be either mL or g based on the 'mass_flow' logical switch!\n")

  #Set up initial parameter estimates
  if(!silent) cat("\nSetting up input variable and initial parameter estimates...")
  defest <- c(r = 0.4, ksas = 10e-5, qc = 400, thetaf = 1, ti = 30, kf = 0.001, c3 = 0.15)
  opt_est <- supp_pars(pars = opt_est, defpars = defest, parlb = "opt_est")

  est_cu <- any(modtype %in% "cu")
  if(est_cu & is.na(opt_est["c3"])) stop("When 'cu' is to be estimated, an initial estimate of constant 'c3' has to be provided in 'opt_est'!")
  if(!est_cu & is.na(pars["cu"])) stop("Unless 'cu' is to be estimated, it must be specified in 'pars'!")

  #Remove missing values
  mischk <- length(which(is.na(oec[,oec_vars["y"]])))
  if(mischk > 0) {
    cat("A total of", mischk, "missing data points were removed from input data...\n", sep = "")
    oec <- oec[complete.cases(oec[,oec_vars]),]
  }

  #Retrieve and/or calculate input variables for the simplified and complete BIC models
  t <- oec[,oec_vars["x"]] #CUMULATIVE extraction time (min)
  m <- length(t) #Number of experimental points
  m_in <- pars[["mass_in"]] #Mass of WET raw material (g)

  funit_conv <- c("percent" = 100, "permille" = 1e3, "ppm" = 1e6, "ppb" = 1e9)
  mex <- if(units["resp"]!="g") oec[,oec_vars["y"]]/funit_conv[units["resp"]]*m_in else oec[,oec_vars["y"]] #Mass of extract (g)

  slv_tfac <- if(grepl("/h$", units["flow"])) 60 else 1
  slv_mfac <- if(grepl("^kg/", units["flow"])) 1000 else 1
  uslv <- if(any(names(oec_vars) %in% "slv")) oec[,oec_vars["slv"]] else (t/slv_tfac)*(pars["flow"]*slv_mfac) #Volume or mass of solvent expended

  moisture <- pars[["moisture"]] #Percentage moisture in raw material (%)
  etoh <- pars[["etoh"]] #Flow rate of EtOH (mL/min), if any
  pres <- pars[["pres"]] #Extraction pressure (bar)
  temp <- pars[["temp"]] #Extraction temperature (degC)
  n <- pars[["n"]] #Point corresponding to the end of the CER

  #Extra checks for 'cumulative' condition
  cumulcnd <- if(cumulative) c(any(diff(mex)<0), any(diff(uslv)<0), " NOT ", " ") else c(all(diff(mex)>0), all(diff(uslv)>0), " ", " NOT ")
  cumulog <- as.logical(cumulcnd[1:2])
  if(cumulog[1]) warning(paste0("The response values provided are likely", cumulcnd[3], "cumulative and were", cumulcnd[4], "converted."))
  if(cumulog[2] & is.na(pars["flow"])) warning(paste0("The solvent consumption values provided are likely", cumulcnd[3], "cumulative and were", cumulcnd[4], "converted."))

  if(cumulative) {
    if(cumulog[1]) mex <- cumsum(mex)
    if(cumulog[2] & any(names(oec_vars) %in% "slv")) uslv <- cumsum(uslv)
  }

  #Convert data to cumulative if necessary
  if(!cumulative) {
    if(!cumulog[1]) mex <- cumsum(mex) #Cumulative extract (g)
    if(!cumulog[2] & any(names(oec_vars) %in% "slv")) uslv <- cumsum(uslv) #Cumulative solvent expended
  }

  co2_flow <- if(is.na(pars["flow"])) uslv[m]/t[m] else pars[["flow"]]/slv_tfac/slv_mfac #Flow rate of CO2 (g/min or mL/min)

  D <- pars[["D"]] #Inner diameter of vessel (m)
  L <- pars[["L"]] #Length of vessel (m)
  dp <- pars[["dp"]] #[m] particle diameter
  dr <- pars[["dr"]] #[kg/m3] Sample real density
  cu <- pars[["cu"]] #Initial content of solute in raw material (kg/kg solid)

  #Calculation of the solid dried mass
  Ng <- m_in - (m_in * moisture / 100) #[g] total dried mass(oil+insoluble solid)
  N <- Ng / 1000 #[kg] total dried mass

  if(!est_cu) {
    Nmg <- (1 - cu) * Ng #[g] mass of insoluble solid
    Nm <- Nmg / 1000 #[kg] mass of insoluble solid
    xu <- cu * N / Nm #[kg/kg] concentration of oil in the untreated solid (oil/insoluble solid)
  } else Nmg <- Nm <- xu <- NA

  #Computation of the CO2 density
  if(is.na(ro_co2)) ro_co2 <- bendens(pres, temp)[["rho"]] #[kg/m3] CO2 density

  #Calculate fraction of CO2 and EtOH, and final density of solvent
  etoh_chk <- c(pars["etoh"]>0 & etoh_frac==0, etoh_frac>0)
  if(any(etoh_chk)) {
    if(etoh_chk[1]) {
      co2_vf <- if(units["flow"]!="mL/min") co2_flow/ro_co2 else co2_flow
      co2_frac <- co2_vf/(co2_vf+etoh)
    } else if(etoh_chk[2]) co2_frac <- 1 - etoh_frac

    #Computation of EtOH and total CO2+EtOH density
    ro_mix <- etoh_dens(temp, co2_frac, ro_co2)
    ro_etoh <- ro_mix[["etoh"]]
    ro_fin <- ro_mix[["co2_etoh"]]
  } else ro_fin <- ro_co2

  #Calculate bed porosity
  da <- (m_in/1000) / (pi * D^2 / 4 * L) # [kg/m3] Sample apparent density
  porosity <- 1 - da / dr # [-] bed porosity

  #Calculate the specific area per unit volume of extraction bed
  a0 <- 6 * (1 - porosity) / dp # [1/m] specific area per unit volume of extraction bed
  gam <- ro_fin * porosity / (dr * (1 - porosity)) # [-] CO2 to solid ratio in the bed

  #Calculate relative amount of passed solvent
  if(mass_flow) {
    mass_slv <- uslv/1000 #Cumulative mass of solvent passed (kg)
    #qaver <- co2_flow/60/1000 #Convert g/min to kg/s
  } else {
    pres_flow <- if(is.na(flowpar[1])) pres else flowpar[1] #[bar] Pressure of flow rate measurement
    temp_flow <- if(is.na(flowpar[2])) temp else flowpar[2] #[°C] Temperature of flow rate measurement
    ro_co2f <- bendens(pres_flow, temp_flow)[["rho"]] # [kg/m3] CO2 density of flow rate measurement
    mass_slv <- uslv*(ro_co2f/1000)/1000 #Cumulative mass of solvent passed (kg)
    #qaver <- co2_flow*(ro_co2f/1000)/60/1000 #Else convert mL/min to kg/s
  }
  qser <- mass_slv/(t*60)
  qser[1] <- 0
  qaver <- mean(qser[-1], na.rm = TRUE) #[kg/s] Average CO2 flow rate
  q <- if(!est_cu) mass_slv/Nm else mass_slv/N #[kg/kg] Relative amount of the passed solvent (kg/kg insoluble solid)

  #Calculate cumulative yields
  if(!est_cu) {
    yield <- mex/Nmg #[g/g] yield = oil/insoluble solid
    yieldstar <- yield * Nm/N #[g/g(%)] yield = oil/solid
  } else yield <- mex/Ng

  #Calculate apparent extract solubility
  if(!est_cu) {
    p <- pracma::polyfit(q[1:n], yield[1:n], 1)
    ys <- if(!any(names(pars) %in% "ys")) p[1] else pars["ys"] # [g/g] solubility (oil/CO2)
  } else ys <- NA

  #Create output list
  modres <- list()

  #Set q and t limits
  if(is.na(tmax)) tmax <- round(max(t, na.rm = TRUE)*1.2, -1)
  if(is.na(qmax)) qmax <- round(max(q)*1.2,-1) #Maximum x-axis value (kg/kg solvent expended) to use to model predictions

  if(!silent) cat("\nDeriving model(s)...")

  #SIMPLIFIED MODEL
  modres[["sim"]] <- bic_sm(yield, q, n, xu, qaver, Nm, porosity, ys, opt_est[["c3"]], Ng, est_cu = if(est_cu) TRUE else FALSE,
                            maxq = qmax, r0 = opt_est[["r"]], ksas0 = opt_est[["ksas"]], qc0 = opt_est[["qc"]])
  if(est_cu) return(modres[["sim"]])

  #SIMPLIFIED MODEL BASED ON CHARACTERISTIC TIMES
  if(any(modtype %in% "ct")) {
    modres[["ct"]] <- bic_ct(yieldstar, t, n, qaver, cu, N, Nm, ys, thetaf0 = opt_est[["thetaf"]], ti0 = opt_est[["ti"]], maxt = tmax)
  }

  #COMPLETE MODEL WITH 2 or 3 EXTRACTION CURVE REGIONS (CER, FER, DC)
  if(any(c("cmp3", "cmp2") %in% modtype)) {
    modres[["cmp"]] <- bic_cmp(yield, q, n, a0, gam, porosity, qaver, Nm, ys, xu,
                               r0 = modres[["sim"]][["mod_pars"]][["r"]], ksas0 = modres[["sim"]][["mod_pars"]][["ksas"]],
                               kf0 = opt_est[["kf"]], qc0 = opt_est[["qc"]],
                               maxq = qmax)
  }

  #COMPILE PLOTS
  if(!silent) cat("\nGenerating plots...")
  pltlst <- list()
  plt_titles <- c("sim" = "Simple BIC model", "ct" = "BIC characteristic times model", "cmp" = "Complete BIC model")
  time_suffix <- " (time-based)"
  time_titles <- setNames(paste0(plt_titles[c("sim","cmp")], time_suffix), c("sim","cmp"))

  for(i in names(modres)) {
    pltlst[[i]] <- kin_plot(pts = modres[[i]][["ordt"]], mod = modres[[i]][["mdt"]],
                          grp = c(mod = "model", reg = "period"), cols = "default",
                          pltlabs = c(title = plt_titles[[i]], x = if(i=="ct") "Time (min)" else "q (kg/kg)", y = "e (kg/kg)"))

    if(i!="ct") {
      pltlst[[paste0(i, "_time")]] <- kin_plot(pts = modres[[i]][["ordt"]], mod = modres[[i]][["mdt"]],
                                               ptvars = c("t","y"),
                                               grp = c(mod = "model", reg = "period"), cols = "default",
                                               pltlabs = c(title = time_titles[[i]], x = "Time (min)", y = "e (kg/kg)"))
    }
  }
  modres[["plots"]] <- pltlst
  isnest <- any(sapply(modres[["plots"]], function(x) class(x)=="list" & length(x)>1))
  if(isnest) {
    modres[["plots"]] <- flattenlist(modres[["plots"]])
    names(modres[["plots"]]) <- sapply(names(modres[["plots"]]), function(x) paste0(gsub(".*\\.", "", x), if(grepl("_time", x)) "_time" else ""))
  }
  if(draw) print(modres[["plots"]])

  #COMPILE MODEL INPUT PARAMETERS
  modres[["data"]] <- cbind.data.frame(oec, q = q, e = yield, estar = yieldstar)
  modres[["input"]] <- c(pres = pres, temp = temp, D = D, L = L, flow = qaver, etoh = etoh, dp = dp,
                         da = da, dr = dr, a0 = a0, porosity = porosity, gamma = gam,
                         n = n, m = m, mass_in = m_in, moisture = moisture, Ng = Ng,
                         Nm = Nm, cu = cu, xu = xu, ys = ys)

  #Retrieve function call
  modres[["call"]] <- match.call()
  if(!silent) cat("\nDONE!")
  return(modres)
}
