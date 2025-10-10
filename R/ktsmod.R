#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Build and plot two-site kinetic desorption model for SWE processes
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Construct two-site kinetic desorption models
#'
#' @description Derives, evaluates, and visualizes the results of two-site kinetic desorption (TWS) models.
#' Useful for subcritical water extraction (SWE) process evaluation. See \strong{Details} for further information.
#'
#' @param oec A \code{data.frame} of OEC data. Must include all \code{oec_vars}.
#' @param oec_vars A \strong{named} \code{character} vector of column names included in \code{oec}.
#' These \strong{must} include the extraction time (\code{"x"}) and response (\code{"y"}) with appropriate \code{units}.
#' A third parameters indicating the solvent usage (\code{"slv"}) is \strong{optional} and may be provided as an
#' \strong{alternative} to an explicit flow rate normally provided in \code{pars["flow"]} - this is useful when the
#' flow rate is uneven throughout the extraction process.
#' @param pars A named \code{numeric} vector of input parameters for the model. \strong{Mandatory} parameters
#' include the pressure (\code{"pres"}; bar), temperature (\code{"temp"}; degrees Celsius), mass of raw material
#' loaded (\code{"m_in"}; g), and the maximum possible yield (\code{"c0"}; units identical to those given in \code{units}).
#' \strong{Optional} parameters include the fraction of easily-desorbed solute (\code{"f"}) and the flow rate of water
#' (\code{"flow"}; units given in \code{units}).
#' @param units A \strong{named} \code{character} vector of length 1 or 2 specifying the units of \code{"flow"}
#' (one of \code{"mL/min"}, the \strong{default} \code{"g/min"}, \code{"kg/h"}, \code{"L/h"}, or \code{"none"}) and/or
#' the response \code{"resp"} (one of \code{"g"}, the \strong{default} \code{"percent"}, \code{"permille"}, \code{"ppm"},
#' or \code{"ppb"}). Where not provided, default values are used.
#' @param opt_est Either \code{"default"} or a \strong{named} \code{numeric} vector of initial parameter estimates
#' for iterative optimization. May include any of the first-order rate constants \eqn{k_1} and \eqn{k_2} (\code{"k1"}
#' and \code{"k2"}), and/or the fraction of easily-desorbed solute \eqn{F} (\code{"f"}, \strong{only required if
#' it is not explicitly provided in \code{pars}}). If not provided, the following default values are used:
#' \code{c(k1 = 0.1, k2 = 0.1, f = 0.5)}.
#' @param plot_units A \strong{named} \code{character} vector of length 2 specifying which units to use for the x- and y-axis
#' of the model plot(s). Possible values are \code{c("time", "q")} for element \code{["x"]} and
#' \code{c("abs", "cc0")} for element \code{["y"]}.
#' @param flowpar Either \code{NA} (default) or a \code{numeric} vector of length 2 providing temperature and pressure
#' at which flow rate of water is measured.
#' @param ro_h2o The subcritical water density (in g/L) at extraction conditions. If not provided (\code{NA}; default),
#' it is calculated via the IAPWS-95 formulation (see \code{\link{h2o_dens}}).
#' @param tmax,qmax Maximum x-axis value (time, min for \code{tmax} and S/M ratio for \code{qmax}) to use for model predictions.
#' Setting to \code{NA} defaults to 120% of the maximum experimental value.
#' @param cumulative A \code{logical} switch specifying whether the \strong{response} and/or \strong{solvent consumption}
#' values provided in \code{oec} are cumulative or not (defaults to \code{FALSE}).
#' @param mass_flow A \code{logical} indicating whether the flow rate provided in \code{pars} is \strong{mass}
#' or \strong{volumetric} (\code{FALSE}; default).
#' @param draw A \code{logical} switch. Should generated plots be plotted? Defaults to \code{TRUE}.
#' @param optmet The method of iterative curve fitting to use for estimating \eqn{k_1}, \eqn{k_2}, and (optionally) \eqn{F}.
#' One of \code{"nlopt"} (Non-Linear Optimization via \code{\link[nloptr]{nloptr}}; default) or \code{"nlrob"} (Robust Fitting
#' via \code{\link[robustbase]{nlrob}}).
#'
#' @return A named \code{list} with the following elements:
#' \enumerate{
#' \item \strong{$data}: A \code{data.frame} including time, response, S/M ratio (\code{$q}), mass yield of extract (\code{"yield_g"}),
#' and fractional yield relative to the total amount of extractable solute (\code{"yield_cc0"}).
#' \item \strong{$tws}: The summarized TWS model output from function \code{\link{tws_cmp}}.
#' \item \strong{$plots}: The model visualisation (plot) of class \code{"ggplot"}.
#' \item \strong{$input}: A list of input model parameters including pressure (\code{"pres"}), temperature
#' (\code{"temp"}), flow rate (\code{"flow"} \strong{in kg/s}), the maximum possible yield (\code{"c0"}),
#' mass of loaded material (\code{"m_in"}), and the number of experimental observations (\code{"m"}).
#' \item \strong{$call}: The function call.
#' }
#' @export
#'
#' @details
#' The TWS model is generated given the vector of time data \eqn{t} (e.g. extraction time), first-order rate constants
#' \eqn{k_1} and \eqn{k_2} (in \eqn{min^{-1}}) describing the "fast" and "slow" component of kinetic desorption, and
#' the fraction \eqn{F} of easily-desorbed target compound(s). The equation describing fractional yield \eqn{e}
#' (ratio between extract collected at time \eqn{t} and the maximum amount of extractable material) is:
#' \deqn{e = 1 - [F \times e^{-k_1t}] - [(1 - F) \times e^{-k_2t}]}
#' See \strong{References} for example applications of the TWS model for subcritical water extraction (SWE) studies.
#'
#' @references
#' Abidin, Z.Z., Samadi, M., Biak, D.R.A., Yunus, R. (2024), 'Mathematical Modelling Of Extraction Of Oil From \emph{Aquilaria malacenssis} Wood Employing Subcritical Conditions', \emph{Journal of Applied Science and Engineering} \strong{27} (12), pp. 3725-3738, DOI: \url{http://dx.doi.org/10.6180/jase.202412_27(12).0012}.
#'
#' Anepankul, T., Goto, M., Sasaki, M., Pavasant, P., Shotipruk, A. (2007), 'Extraction of anti-cancer damnacanthal from roots of \emph{Morinda citrifolia} by subcritical water', \emph{Separation and Purification Technology} \strong{55}, pp. 343-349, DOI: \url{http://dx.doi.org/10.1016/j.seppur.2007.01.004}.
#'
#' Jamaludin, R., Kim, D.-S., Salleh, L.M., Lim, S.-B. (2021), 'Kinetic Study of SubcriticalWater Extraction of Scopoletin, Alizarin, and Rutin from \emph{Morinda citrifolia}', \emph{Foods} \strong{2021} (10), article 2260, DOI: \url{https://doi.org/10.3390/foods10102260}.
#'
#' Pereira, D.T.V., Tarone, A.G., Cazarin, C.B.B., Barbero, G.F., Martinez, J. (2019), 'Pressurized liquid extraction of bioactive compounds from grape marc', \emph{Journal of Food Engineering} \strong{240}, pp. 105-113, DOI: \url{https://doi.org/10.1016/j.jfoodeng.2018.07.019}.
#'
#' @examples
#' twosite_res <- ktsmod(oec = swex[["duba1"]][["data"]],
#' oec_vars = c(x = "Time_min", y = "Yield_100C"),
#' pars = c(pres = 15, temp = 100, flow = 2, c0 = 77, m_in = 2, f = 0.24),
#' opt_est = "default",
#' units = c(flow = "mL/min", resp = "permille"),
#' plot_units = c(x = "q", y = "abs"), #"time" "q" "abs" "cc0"
#' cumulative = TRUE,
#' mass_flow = FALSE,
#' flowpar = rep(NA,2),
#' ro_h2o = NA,
#' tmax = NA,
#' qmax = NA,
#' optmet = "nlopt")
#'
#' @seealso \code{\link{tws_cmp}}, \code{\link{kin_splot}}, \code{\link{show_pars}}, \code{\link{moderr}}, \code{\link{bicmod}}
#'
#' @importFrom stats complete.cases
#'
ktsmod <- function(oec, oec_vars, pars, units = "default", opt_est = "default",
                   plot_units = c(x = "time", y = "cc0"), flowpar = rep(NA,2), ro_h2o = NA, tmax = NA, qmax = NA,
                   cumulative = TRUE, mass_flow = FALSE, draw = TRUE, optmet = "nlopt") {

  #Preliminary checks
  if(!all(c("x","y") %in% names(plot_units))|!is.character(plot_units)) stop("Argument 'plot_units' must be a character vector of length 2!")
  if(!any(c("time", "q") %in% plot_units["x"])|!any(c("abs", "cc0") %in% plot_units["y"]))
    stop("Argument 'plot_units' may only have the following values: 'time', 'q' for element ['x'] and 'abs', 'cc0' for element ['y']!")
  if(length(flowpar)!=2|!is.numeric(flowpar) & !all(is.na(flowpar))) stop("Pressure and temperature of flow rate measurements 'flowpar' must be a numeric vector of length 2!")
  if(!any(names(pars) %in% "f")) pars["f"] <- NA
  if(!all(c("c0", "m_in", "pres", "temp") %in% names(pars))) stop("Pressure ('pres', bar), temperature ('temp', degC),
                                                                  initial concentration of solute in raw material ('c0')
                                                                  and mass loaded ('m_in', in g) must be provided!")
  #Set up units
  posun_flow <- c("mL/min", "g/min", "kg/h", "L/h", "none")
  posun_resp <- c("g", "percent", "permille", "ppm", "ppb")
  defunits <- c(flow = "mL/min", resp = "permille")
  units <- supp_pars(defunits, units, parlb = "units")

  if(!all(c("flow","resp") %in% names(units))) {
    stop("The 'units' vector must be named with the following: 'flow' and 'resp'!")
  } else if(!any(posun_flow %in% units["flow"])|!any(posun_resp %in% units["resp"])) {
    stop(paste0("Unrecognised values in the 'units' argument!
                Accepted units are: ", paste0("'", posun_flow, "'", collapse = ", "), " for flow;",
                paste0("'", posun_resp, "'", collapse = ", "), " for response."))
  } else if(mass_flow & grepl("^mL|^L",units["flow"])) stop("When 'mass_flow' is TRUE, the flow 'units' must not be volumetric!")

  if(!is.na(pars["flow"]) & "slv" %in% names(oec_vars)) {
    cat("When flow rate is provided, solvent expenditure data in 'oec' is ignored.../n")
    oec <- oec[,!colnames(oec) %in% oec_vars["slv"]]
  }
  if(is.na(pars["flow"]) & !any(names(oec_vars) %in% "slv")) stop("When water flow rate is not provided, data on expended solvent MUST be included in 'oec'!")

  #Set up initial parameter estimates
  defest <- c(k1 = 0.1, k2 = 0.1)
  if(is.na(pars["f"])) defest <- append(defest, c(f = 0.5))
  opt_est <- supp_pars(pars = opt_est, defpars = defest, parlb = "opt_est")

  #Remove missing values
  mischk <- length(which(is.na(oec[,oec_vars["y"]])))
  if(mischk > 0) {
    cat("A total of", mischk, "missing data points were removed from input data...\n")
    oec <- oec[complete.cases(oec[,oec_vars]),]
  }

  #Retrieve or calculate input variables
  pres <- pars[["pres"]] #Extraction pressure (bar)
  temp <- pars[["temp"]] #Extraction temperature (degC)
  t <- oec[,oec_vars["x"]] #Cumulative extraction time (min)
  m_in <- pars[["m_in"]] #Mass of raw material (g)
  m <- length(t) #Number of experimental points
  f <- pars[["f"]] #Fraction of easily extractable solute
  c0 <- pars[["c0"]] #Initial content of solute in the raw material (kg/kg)
  yield <- oec[,oec_vars["y"]]
  cc0 <- yield/c0

  funit_conv <- c("percent" = 100, "permille" = 1e3, "ppm" = 1e6, "ppb" = 1e9)
  mex <- if(units["resp"]!="g") yield/funit_conv[units["resp"]]*m_in else yield #Mass of extract (g)

  slv_tfac <- if(grepl("/h$", units["flow"])) 60 else 1
  slv_mfac <- if(grepl("^kg/", units["flow"])) 1000 else 1
  uslv <- if(any(names(oec_vars) %in% "slv")) oec[,oec_vars["slv"]] else (t/slv_tfac)*(pars["flow"]*slv_mfac) #Volume or mass of solvent expended

  #Extra checks for 'cumulative' condition
  cumulcnd <- if(cumulative) c(any(diff(mex)<0), any(diff(uslv)<0), " NOT ", " ") else c(all(diff(mex)>0), all(diff(uslv)>0), " ", " NOT ")
  cumulog <- as.logical(cumulcnd[1:2])
  if(cumulog[1]) warning(paste0("The response values provided are likely", cumulcnd[3], "cumulative and were", cumulcnd[4], "converted."))
  if(cumulog[2]) warning(paste0("The solvent consumption values provided are likely", cumulcnd[3], "cumulative and were", cumulcnd[4], "converted."))

  if(cumulative) {
    if(cumulog[1]) {
      yield <- cumsum(yield)
      mex <- cumsum(mex)
    }
    if(cumulog[2] & any(names(oec_vars) %in% "slv")) uslv <- cumsum(uslv)
  }

  #Convert data to cumulative if necessary
  if(!cumulative) {
    if(!cumulog[1]) {
      yield <- cumsum(yield) #Cumulative yield (variable units)
      mex <- cumsum(mex) #Cumulative extract (g)
    }
    if(!cumulog[2] & any(names(oec_vars) %in% "slv")) uslv <- cumsum(uslv) #Cumulative solvent expended
  }

  #h2o_flow <- if(is.na(pars["flow"])) uslv[m]/t[m] else pars[["flow"]]/slv_tfac/slv_mfac #Flow rate of water (g/min or mL/min)

  #Computation of the water density
  if(is.na(ro_h2o)) ro_h2o <- h2o_dens(pres, temp) #[kg/m3] Water density at extraction conditions

  #Calculate relative amount of passed solvent
  if(mass_flow) {
    mass_slv <- uslv/1000 #Cumulative mass of solvent passed (kg)
  } else {
    pres_flow <- if(is.na(flowpar[1])) pres else flowpar[1] #[bar] Pressure of flow rate measurement
    temp_flow <- if(is.na(flowpar[2])) temp else flowpar[2] #[°C] Temperature of flow rate measurement
    ro_h2of <- h2o_dens(pres_flow, temp_flow) # [kg/m3] Water density of flow rate measurement
    mass_slv <- uslv*(ro_h2of/1000)/1000 #Cumulative mass of solvent passed (kg)
  }
  qser <- mass_slv/(t*60)
  qser[1] <- 0
  qaver <- mean(qser, na.rm = TRUE) #[kg/s] Average water flow rate
  q <- mass_slv/(m_in/1000) #[kg/kg] Relative amount of the passed solvent (kg/kg insoluble solid)

  #Set up maximum modeling limits for time t (min) and relative mass of solvent q (kg/kg)
  if(is.na(tmax)) tmax <- round(max(t, na.rm = TRUE)*1.2, -1)
  if(is.na(qmax)) qmax <- round(max(q)*1.2,-1) #Maximum x-axis value (kg/kg solvent expended) to use to model predictions

  #Build the two-site kinetic desorption model
  modres <- list()
  est_f <- if(is.na(f)) TRUE else FALSE

  modres[["tws"]] <- tws_cmp(x = t, y = yield, c0 = c0, q = q, f = if(!est_f) f else NA,
                             k1_0 = opt_est[["k1"]], k2_0 = opt_est[["k2"]],
                             f0 = if(est_f) opt_est[["f"]] else NA, est_f = est_f, maxt = tmax, maxq = qmax,
                             optmet = optmet) #"nlopt" "nlrob"

  #Create and optionally print model plot
  qchk <- plot_units["x"]=="q"
  ccchk <- plot_units["y"]=="cc0"
  modres[["plots"]] <- kin_plot(pts = modres[["tws"]][["ordt"]], mod = modres[["tws"]][["mdt"]],
                                ptvars = c(x = if(qchk) "q" else "x", y = if(ccchk) "cc0" else "y"),
                                grp = c(mod = "model", reg = NA), cols = "default",
                                pltlabs = c(title = "Two-site kinetic desorption model",
                                            x = if(qchk) "q (kg/kg)" else "Time (min)",
                                            y = if(ccchk) "e (kg/kg)" else paste0("Yield (", units[["resp"]], ")")))

  if(draw) print(modres[["plots"]])

  #COMPILE MODEL INPUT PARAMETERS
  modres[["data"]] <- cbind.data.frame(oec, q = q, yield_g = mex, yield_cc0 = cc0)
  modres[["input"]] <- c(pres = pres, temp = temp, flow = qaver, c0 = c0, mass_in = m_in, m = m)
  modres[["call"]] <- match.call()

  return(modres)
}
