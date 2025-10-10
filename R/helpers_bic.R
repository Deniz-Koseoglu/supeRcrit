#' @title Show kinetic model parameters and their description
#'
#' @description Displays names, units, and descriptions of kinetic model and Cost of Manufacturing
#' calculation parameters implemented in \pkg{supeRcrit}.
#'
#' @param type The type of model to show parameters for. One or more of: \code{"bic"} (BIC model),
#' \code{"ts"} (two-site kinetic desorption model),
#' and/or \code{"com"} (Cost of Manufacturing).
#'
#' @return A \code{data.frame} with types, names, units, and descriptions of model parameters.
#' @export
#'
#' @examples
#' #Show parameter list for the Broken and Intact Cells (BIC) model
#' show_pars("bic")
show_pars <- function(type) {
  if(!any(c("bic", "ts", "com") %in% type)) stop("Unrecognised 'type' argument! Must be one of: 'bic', 'ts', and/or 'com'!")
  if(length(type)!=1|!is.character(type)) stop("The 'type' of parameters to display must be a single character string!")
nmvec <- c(bic = "BIC", ts = "twosite", com = "COM")
if(!any(type %in% names(nmvec))) stop(paste0("The type of model to show parameters for is incorrect! Possible values are: ", paste0(names(nmvec), collapse = ", "),"!"))
else return(desc_pars[[nmvec[type]]])
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Load internal data (mainly useful for function examples)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Load internal package data where necessary
#'
#' @param dtname Name of dataset. One of \code{c("doe_base", "doe_lst1", "doe_lst2", "gcm_cnt", "gcm_int",
#' "gcm_smarts", "solv_dmass", "solv_dmol", "solv_mv")}
#'
#' @return The selected internal data.
#' @export
#'
#' @examples
#' \dontrun{
#' load_internal("doe_lst1")
#' }
load_internal <- function(dtname) {
  int_nms <- c("doe_base", "doe_lst1", "doe_lst2", "gcm_cnt", "gcm_int",
               "gcm_smarts", "solv_dmass", "solv_dmol", "solv_mv", "solv_iscrit")
  if(!all(dtname %in% int_nms)) stop(paste0("Internal dataset name must be one of: ", paste0(int_nms, collapse = ", "), "!"))
  else return(eval(parse(text = dtname)))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Run segmented regression to calculate the breakpoint where CER (Constant Extraction Rate) ends
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Run segmented regression to estimate CER endpoint
#'
#' @description Estimates the number of observation along an extraction curve
#' using \code{\link[segmented]{segreg}} or \code{\link[segmented]{stepreg}}.
#' May be used in preparation for the \code{\link{bicmod}}.
#'
#' @param input A \code{data.frame} containing the time \code{x} and \code{y} values of the extraction curve.
#' @param x,y Both are \code{character} column names identifying x- and y-values in the \code{input} data.
#' @param plt A \code{logical} switch specifying whether to plot segmented regression results (defaults to \code{TRUE}).
#' @param segmode Segmentation mode to use. One of: \code{"step"} (default) or \code{"seg"}.
#'
#' @return A single \code{numeric} value showing the observation number at which the CER likely ends.
#' @export
#'
#' @examples
#' oec_bp(input = sfex[[1]][["data"]], x = "time_min", y = "yield_g")
#'
#' @seealso \code{\link{bicmod}}
#'
#' @importFrom stats as.formula
#' @importFrom graphics points
#' @importFrom segmented seg stepreg
#'
oec_bp <- function(input, x, y, plt = TRUE, segmode = "step") {
  if(!any(c("step","seg") %in% segmode)) stop("Argument 'segmode' must be one of: 'seg' or 'step'!")

  #Make formula
  segform <- as.formula(paste0(y, " ~ seg(", x, ")", collapse = ""))

  #Segmented
  if(segmode=="seg") {
    seg <- segmented::segreg(segform, data = input, npsi = 1) #Estimate a singular breakpoint
    n_time <- seg[["psi"]][1,"Est."]
  } else {
    seg <- segmented::stepreg(segform, data = input, npsi = 1) #In stepmented mode
    n_time <- seg[["psi.rounded"]][1]
  }
  n_time <- which.min(abs(input[,x]-n_time)) #Get closest time point from result

  if(plt) {
    plot(seg)
    if(segmode=="seg") points(seg)
  }
  return(n_time)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Calculate CO2 and CO2+EtOH density using Bender Equation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Calculate CO2 density using Bender EoS
#'
#' @description Uses the Bender Equation of State to calculate CO2 density (g/L or g/mL) and specific enthalpy (kJ/kg)
#' between 1-1000 bar and -50 to 300 degress Celsius.
#' Part of the \code{bicmod} workflow.
#'
#' @param pres,temp Pressure (in bar) and temperature (in degrees Celsius) for which to return the CO2 density.
#' @param units Units required for output density as a \code{character} value. One of: \code{"g/L"} or \code{"g/mL"}.
#'
#' @return A named \code{numeric} vector containing the density (\code{["rho"]}) in chosen \code{units} and the specific enthalpy (\code{"ent"}).
#' @export
#'
#' @examples
#' bendens(300, 45)
#' bendens(800, 70, "g/mL")
#'
#' @details
#' For the complete Bender Equation of State used to calculate density and enthalpy herein, see \strong{Eqs. 1.1-1.4} in Rizza (2014).
#'
#' @references
#' Bender, E. (1975), 'Equations of state for ethylene and propylene', \emph{Cryogenics} \strong{15} (11), pp. 667-673, DOI: \url{https://doi.org/10.1016/0011-2275(75)90100-9}.
#'
#' Ghazouani, J., Chouaieb, O., Bellagi, A. (2005), 'Evaluation of the parameters of the Bender equation of state for low acentric factor fluids and carbon dioxide', \emph{Thermochimica Acta} \strong{432} (1), pp. 10-19, DOI: \url{https://doi.org/10.1016/j.tca.2004.11.008}.
#'
#' Rizza, C.S. (2014), \emph{Experiments and Modeling of Supercritical CO2 Extraction of Lipids from Microalgae}, MSc thesis, Universita Degli Studi Di Padova, Dipartimento Di Ingegneria Industriale.
#'
#' Sievers, U. (1984), 'Die thermodynamischen Eigenschaften von Kohlendioxid', \emph{Forsch Ing-Wes} \strong{50}, p. 192, DOI: \url{https://doi.org/10.1007/BF02560841}.
#'
#' @seealso \code{\link{bicmod}}
#'
#' @importFrom stats setNames
bendens <- function(pres, temp, units = "g/L") {
  #Preliminary checks
  if(!all(is.numeric(c(pres,temp)))|length(c(pres,temp))!=2) stop("Input data 'pres' and 'temp' must both be numeric values!")
  if(pres<1|pres>1000) stop("Pressure must be between 1 and 1000 bar!")
  if(-50>=temp|temp>300) stop("Temperature must be between -50 and 300 degress Celsius!")
  if(!any(c("g/L","g/mL") %in% units)) stop("Arguments 'units' must be one of 'g/L' or 'g/mL'!")

  #Convert pressure and temperature to MPa and Kelvin, respectively
  pres <- pres/10
  temp <- temp + 273.15

  #Define Bender equation coefficients
  #For density (Eq. 1.1 in Rizza, 2014)
  R <- 0.188918
  a <- c(0.22488558, 0.13717965e3, 0.14430214e5, 0.29630491e7, 0.20606039e9, 0.45554393e-1, 0.77042840e2, 0.40602371e5, 0.40029509, -0.39436077e3,
         0.12115286, 0.10783386e3, 0.43962336e2, -0.36505545e8, 0.19490511e11, -0.29186718e13, 0.24358627e8, -0.37546530e11, 0.11898141e14, 0.50000000e1)
  abs <- c(Bn = a[1]*temp - a[2] - a[3]/temp - a[4]/temp^2 - a[5]/temp^3,
           Cn = a[6]*temp + a[7] + a[8]/temp,
           Dn = a[9]*temp + a[10],
           En = a[11]*temp + a[12],
           Fn = a[13],
           Gn = a[14]/temp^2 + a[15]/temp^3 + a[16]/temp^4,
           Hn = a[17]/temp^2 + a[18]/temp^3 + a[19]/temp^4)

  #Additional coefficients for enthalpy (Eq. 1.4 in Rizza, 2014)
  h0 <- 0
  t0 <- 298.15
  tb <- 1000
  es <- c(0.1853128e-3, -0.8552719e-2, 0.1450667, -1.068975, 5.219996, 1.833078, -0.5882021)
  der <- c(Bn = a[1]*temp + a[3]/temp + 2*a[4]/temp^2 + 3*a[5]/temp^3,
           Cn = a[6]*temp - a[8]/temp,
           Dn = a[9]*temp,
           En = a[11]*temp,
           Fn = 0,
           Gn = -2*a[14]/temp^2 - 3*a[15]/temp^3 - 4*a[16]/temp^4,
           Hn = -2*a[17]/temp^2 - 3*a[18]/temp^3 - 4*a[19]/temp^4)

  #Calculate density
  ac <- c(0,1)
  e <- 0.000001
  rhox <- function(x) {
    R*temp*x + abs["Bn"]*x^2 + abs["Cn"]*x^3 + abs["Dn"]*x^4 + abs["En"]*x^5 + abs["Fn"]*x^6 + (abs["Gn"]+abs["Hn"]*x^2)*x^3*exp(-a[20]*x^2) - pres
  }
  fs <- sapply(ac, rhox)

  while(abs(ac[2]-ac[1]) > e) {
    rho <- mean(ac, na.rm = TRUE)
    fb <- rhox(rho)
    if(fs[1]*fb <= 0) {
      ac[2] <- rho
      fs[2] <- fb
    } else {
      ac[1] <- rho
      fs[1] <- fb
    }
  }

  #Calculate enthalpy from density and temperature
  h <- h0 + R*tb*(-es[1]/3 * ((temp/tb)^(-3) - (t0/tb)^(-3)) -
                    es[2]/2 * ((temp/tb)^(-2) - (t0/tb)^(-2)) -
                    es[3] * ((temp/tb)^(-1) - (t0/tb)^(-1)) +
                    es[4] * log(temp/t0) + es[5] * ((temp/tb)-(t0/tb)) +
                    es[6]/2 * ((temp/tb)^2 - (t0/tb)^2) +
                    es[7]/3 * ((temp/tb)^3 - (t0/tb)^3)) +
    (2*abs["Bn"] - der["Bn"])*rho + 1/2*(3*abs["Cn"]-der["Cn"])*rho^2 +
    1/3*(4*abs["Dn"]-der["Dn"])*rho^3 + 1/4*(5*abs["En"]-der["En"])*rho^4 +
    1/5*(6*abs["Fn"]-der["Fn"])*rho^5 + (abs["Gn"]+abs["Hn"]*rho^2) * rho^2 * exp(-a[20]*rho^2) +
    (abs["Gn"]-der["Gn"]) * 1/(2*a[20]) * (1-exp(-a[20]*rho^2)) +
    (abs["Hn"]-der["Hn"]) * 1/(2*a[20]^2) * (1-(a[20]*rho^2+1) * exp(-a[20]*rho^2)) +
    R*(temp-t0)

  if(units=="g/L") rho <- rho*1000
  finres <- unname(c(rho,h))

  return(setNames(finres, c("rho","ent")))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION B: Calculate EtOH density or that + CO2
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Calculate density of ethanol and its mixtures with CO2
#'
#' @description Calculates the density of pure ethanol at specific temperatures.
#' The density of CO2-ethanol mixtures may also be calculated via the Linear Blend Rule, but density of CO2 must be provided
#' (for example, from the \code{\link{bendens}} function).
#' Part of the \code{bicmod} workflow.
#'
#' @param temp Temperature for which to calculate density (in degrees Celsius).
#' @param co2_frac The volume fraction of CO2 (from 0 to 0.99).
#' @param co2_rho Density of CO2 (\strong{in g/L}).
#'
#' @details
#' In this implementation ethanol is considered an incompressible fluid and the following equation (Poling et al., 2008) is
#' used to calculate density \eqn{\rho} in g/L:
#'
#' \deqn{\rho = C_1/C_2^{1+(1-T/C_3)^{C_4}} \times RMM_{EtOH}}
#'
#'  In this equation, \eqn{C_1} to \eqn{C_4} are constants: \eqn{C_1 = 1.6288}, \eqn{C_2 = 0.27469}, \eqn{C_3 = 514},
#'  \eqn{C_4 = 0.23178}, \eqn{T} is temperature in Kelvin, and \eqn{RMM_{EtOH}} is the molar mass of ethanol (46.068 g/mol).
#'  The same expression is listed and described in \strong{Eq. 3.14} (Rizza, 2014).
#'
#'  Density adjustment for CO2-Ethanol mixtures is carried out using the Linear Blend Rule. For example, when 6% ethanol is used
#'  as co-solvent (see also \strong{Eq. 3.13} in Rizza, 2014):
#'
#'  \eqn{1/\rho_f = 0.94/\rho_{CO2} + 0.06/\rho_{EtOH}}
#'
#' @references
#' Poling, B.E., Thomson, G.H., Friend, D.G., Rowley, R.L., Wilding, W.V. (2008), 'Physical and Chemical Data', In: Perry, R.H., Green, D.W. (eds.), \emph{Perry's Chemical Engineers Handbook}, McGraw-Hill, chapter 2.
#'
#' Rizza, C.S. (2014), \emph{Experiments and Modeling of Supercritical CO2 Extraction of Lipids from Microalgae}, MSc thesis, Universita Degli Studi Di Padova, Dipartimento Di Ingegneria Industriale.
#'
#' @return A named \code{numeric} vector containing densities of pure ethanol (\code{["etoh"]}) and its mixture with CO2 (\code{["co2_etoh"]}).
#' @export
#'
#' @examples
#' etoh_dens(55)
#' etoh_dens(55, 0.90, 880)
#'
#' @seealso \code{\link{bicmod}}
etoh_dens <- function(temp, co2_frac = 0, co2_rho = 0) { #Density MUST BE IN kg/m3 (g/L)!
  #Preliminary checks
  if(!is.numeric(temp)|length(temp)!=1) stop("Temperature 'temp' must be a single numeric value!")
  if(!is.numeric(co2_frac)|co2_frac<0|co2_frac>0.99) stop("The CO2 fraction 'co2_frac' must be between 0 and 0.99!")

  #Get pure EtOH density
  cost <- c(1.6288, 0.27469, 514, 0.23178)
  pm_etoh <- 46.068 #kg/kmol
  rho_etoh <- pm_etoh * cost[1]/cost[2]^(1+(1-(temp+273.15)/cost[3])^cost[4]) #kg/m3 (g/L)

  #If any CO2 is present, also calculate overall density via the Linear Blend rule
  if(co2_frac > 0 & co2_frac < 1 & co2_rho > 0) {
    rho_mix <- unname(1/(co2_frac/co2_rho + (1-co2_frac)/rho_etoh))
  } else rho_mix <- NA
  return(c(etoh = rho_etoh, co2_etoh = rho_mix))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Simplified and complete BIC model for SFE processes
#FUNCTION A1: Second extraction period of the SIMPLIFIED model
#The CER period (1st period) has no adjustable parameters
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Equation: Second extraction period of the simplified BIC model
#'
#' @description One of the equations used for the \strong{Broken and Intact Cells (BIC)} model workflow in \code{\link{bicmod}}.
#' See also \strong{Details} for more information.
#' \strong{This function manual is hidden from the Index.}
#'
#' @param c1,c2 Constants \eqn{C_1} and \eqn{C_2}.
#' @param x A \code{numeric} value or vector containing the input data (relative amount of solvent passed \eqn{q}; kg/kg insoluble solid).
#' @param xu Weight fraction (concentration) \eqn{x_u} in the untreated solid.
#'
#' @return The single \code{numeric} result of the evaluated equation - specifically, the extraction yield \eqn{e} (g/g insoluble solid).
#'
#' @details
#' The relevant equation describes the \strong{second period} of a \strong{simplified} BIC model (Rizza, 2014, \strong{Eq. 3.19}):
#' \deqn{e = x_u \times (1-C_1 \times exp(-C_2 \times q)) \text{ for } q > q_c}
#' Here, \eqn{e} is extraction yield, \eqn{x_u} is the fraction of solute in untreated solid, \eqn{q_c} is the relative
#' amount of solvent passed at the end of the Constant Extraction Period (CER; kg/kg of \strong{insoluble solid}),
#' while \eqn{C_1} and \eqn{C_2} are constants.
#'
#' Note that the CER period is considered completely linear such that \eqn{e = qy_s} where \eqn{y_s} is extract solubility
#' (in g/g; slope of best-fit line).
#'
#' @export
#' @keywords internal
#'
#' @references
#' Rizza, C.S. (2014), \emph{Experiments and Modeling of Supercritical CO2 Extraction of Lipids from Microalgae}, MSc thesis, Universita Degli Studi Di Padova, Dipartimento Di Ingegneria Industriale.
#'
#' Sovova, H. (2005), 'Mathematical model for supercritical fluid extraction of natural products and extraction curve evaluation', \emph{The Journal of Supercritical Fluids} \strong{33} (1), pp. 35-52, DOI: \url{https://doi.org/10.1016/j.supflu.2004.03.005}.
#'
#' Sovova, H. (2017), 'Broken-and-intact cell model for supercritical fluid extraction: Its origin and limits', \emph{The Journal of Supercritical Fluids} \strong{129}, pp. 3-8, DOI: \url{https://doi.org/10.1016/j.supflu.2017.02.014}.
#'
#' @export
#' @keywords internal
#'
#' @seealso \code{\link{bicmod}}, \code{\link{show_pars}}
fer_sim_eq <- function(c1, c2, x, xu) {
  res <- xu * (1 - c1 * exp(-c2 * x))
  return(res)
}


#' @title RMSE: Second extraction period of the simplified BIC model
#'
#' @description Calculates Round-Mean-Squared-Error (RMSE) for the BIC model equation in \code{\link{fer_sim_eq}}.
#' One of the equations used for the \strong{Broken and Intact Cells (BIC)} model workflow in \code{\link{bicmod}}.
#' Part of the \code{\link{bicmod}} workflow. \strong{This function manual is hidden from the Index.}
#'
#' @param alpha A \code{numeric} vector of length 2 containing constants \eqn{C_1} and \eqn{C_2} from \code{\link{fer_sim_eq}}.
#' @param x A \code{numeric} value or vector containing the input data (relative amount of solvent passed \eqn{q}; kg/kg insoluble solid).
#' @param xu Weight fraction (concentration) \eqn{x_u} in the untreated solid.
#' @param input A vector of \strong{observed} extraction yields \eqn{e}.
#'
#' @return The RMSE as a single \code{numeric} value \eqn{RMSE = \sum{\sqrt{(x_{model}-x_{observed})^2}/m}}.
#'
#' @references
#' Rizza, C.S. (2014), \emph{Experiments and Modeling of Supercritical CO2 Extraction of Lipids from Microalgae}, MSc thesis, Universita Degli Studi Di Padova, Dipartimento Di Ingegneria Industriale.
#'
#' Sovova, H. (2005), 'Mathematical model for supercritical fluid extraction of natural products and extraction curve evaluation', \emph{The Journal of Supercritical Fluids} \strong{33} (1), pp. 35-52, DOI: \url{https://doi.org/10.1016/j.supflu.2004.03.005}.
#'
#' Sovova, H. (2017), 'Broken-and-intact cell model for supercritical fluid extraction: Its origin and limits', \emph{The Journal of Supercritical Fluids} \strong{129}, pp. 3-8, DOI: \url{https://doi.org/10.1016/j.supflu.2017.02.014}.
#'
#' @export
#' @keywords internal
#'
#' @seealso \code{\link{fer_sim_eq}}, \code{\link{bicmod}}
fer_sim_err <- function(alpha, x, xu, input) {
  if(length(x)!=length(input)|!all(sapply(list(x,input), is.numeric))) stop("Arguments 'x' and 'input' must be numeric vectors of equal length!")
  res <- fer_sim_eq(alpha[1], alpha[2], x, xu)
  err <- sum(sqrt((res-input)^2)/length(input))
  return(err)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION A2: Same as A1 with 'cu' (solute content in the untreated solid, kg/kg of solid) as an additional parameter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Equation: Second extraction period of the simplified BIC model ('cu' estimation)
#'
#' @description A modification of the simplified BIC model equation in \code{\link{fer_sim_eq}} used to estimate
#' \eqn{c_u}, the solute content of the untreated solid (kg/kg).
#' Part of the \code{\link{bicmod}} workflow. \strong{This function manual is hidden from the Index.}
#'
#' @param c1,c2,c3 Constants \eqn{C_1}, \eqn{C_2}, and \eqn{C_3}.
#' @param x A \code{numeric} value or vector containing the input data (relative amount of solvent passed \eqn{q}; kg/kg insoluble solid).
#' @param Ng Total dry mass \eqn{N} (solute + insoluble material; \strong{g}).
#'
#' @return The single \code{numeric} result of the evaluated equation - specifically, the extraction yield \eqn{e} (g/g insoluble solid).
#'
#' @references
#' Rizza, C.S. (2014), \emph{Experiments and Modeling of Supercritical CO2 Extraction of Lipids from Microalgae}, MSc thesis, Universita Degli Studi Di Padova, Dipartimento Di Ingegneria Industriale.
#'
#' Sovova, H. (2005), 'Mathematical model for supercritical fluid extraction of natural products and extraction curve evaluation', \emph{The Journal of Supercritical Fluids} \strong{33} (1), pp. 35-52, DOI: \url{https://doi.org/10.1016/j.supflu.2004.03.005}.
#'
#' Sovova, H. (2017), 'Broken-and-intact cell model for supercritical fluid extraction: Its origin and limits', \emph{The Journal of Supercritical Fluids} \strong{129}, pp. 3-8, DOI: \url{https://doi.org/10.1016/j.supflu.2017.02.014}.
#'
#' @export
#' @keywords internal
#'
#' @seealso \code{\link{fer_sim_eq}}, \code{\link{show_pars}}, \code{\link{bicmod}}
fer_sim_cu <- function(c1, c2, c3, x, Ng) {
  cu <- c3 / (1 + c3)
  N <- Ng / 1000
  Nmg <- (1 - cu) * Ng
  Nm <- Nmg / 1000
  y <- c3 * (1 - c1 * exp(-c2 * x)) * Nm / N
  return(y)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION B: Relative amount of expended solvent at the end of the CER
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Equation: Relative amount of expended solvent at the end of the CER (simplified BIC model)
#'
#' @description Part of the \code{\link{bicmod}} workflow. \strong{This function manual is hidden from the Index.}
#'
#' @param x A \code{numeric} vector of estimates of possible relative amounts of solvent expended at the end of the CER (\eqn{q}; kg/kg CO2).
#' @param xu Weight fraction (concentration) in the untreated solid \eqn{x_u}.
#' @param c1,c2 Constants \eqn{C_1} and \eqn{C_2}.
#' @param ys Extract solubility in CO2 (\eqn{y_s}; g/g).
#'
#' @return The single \code{numeric} result of the evaluated equation - specifically, relative amount of expended solvent at the
#' end of the CER (\eqn{q_m}; kg/kg insoluble solid).
#'
#' @details
#' The following equation calculates the relative amount of passed solvent (CO2) at the end of the Constant Extraction Rate (CER) period
#' (Rizza, 2014):
#' \deqn{q_m = x_u \times (1 - C_1 \times \exp{(-C_2 \times q)}) - q * y_s}
#'
#' @references
#' Rizza, C.S. (2014), \emph{Experiments and Modeling of Supercritical CO2 Extraction of Lipids from Microalgae}, MSc thesis, Universita Degli Studi Di Padova, Dipartimento Di Ingegneria Industriale.
#'
#' Sovova, H. (2005), 'Mathematical model for supercritical fluid extraction of natural products and extraction curve evaluation', \emph{The Journal of Supercritical Fluids} \strong{33} (1), pp. 35-52, DOI: \url{https://doi.org/10.1016/j.supflu.2004.03.005}.
#'
#' Sovova, H. (2017), 'Broken-and-intact cell model for supercritical fluid extraction: Its origin and limits', \emph{The Journal of Supercritical Fluids} \strong{129}, pp. 3-8, DOI: \url{https://doi.org/10.1016/j.supflu.2017.02.014}.
#'
#' @export
#' @keywords internal
#'
#' @seealso \code{\link{bicmod}}, \code{\link{show_pars}}
solv_sim <- function(x, xu, c1, c2, ys) {
  qsim <- xu * (1 - c1 * exp(-c2 * x)) - x * ys
  return(qsim)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION C: First extraction period (CER) of the CHARACTERISTIC TIMES model
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Equation: First extraction period (CER) of the characteristic times BIC model
#'
#' @description Part of the \code{\link{bicmod}} workflow. \strong{This function manual is hidden from the Index.}
#'
#' @param delta External mass transfer resistance (\eqn{\theta_e}; dimeonsionless).
#' @param t1 Extraction time \eqn{t_1} at the end of the CER period (s).
#' @param ys Extract solubility in CO2 (\eqn{y_s}; g/g).
#' @param qspec Specific solvent flow rate (\eqn{q' \text{ in } kg_{solvent}/kg_{solid}/s}).
#'
#' @return The single \code{numeric} result of the evaluated equation - specifically, the extraction yield (g/g insoluble solid).
#'
#' @details
#' The equation estimates the extraction yield \eqn{e'} at the end of the CER for the Characteristic Times BIC model
#' (see also \strong{Eqn. 3.32} in Rizza, 2014):
#' \deqn{e' = y_s \times q' \times (1 - \exp{(-1/\theta_e)}) \text{ for } t \leq t_1}
#'
#' @references
#' Rizza, C.S. (2014), \emph{Experiments and Modeling of Supercritical CO2 Extraction of Lipids from Microalgae}, MSc thesis, Universita Degli Studi Di Padova, Dipartimento Di Ingegneria Industriale.
#'
#' Sovova, H. (2005), 'Mathematical model for supercritical fluid extraction of natural products and extraction curve evaluation', \emph{The Journal of Supercritical Fluids} \strong{33} (1), pp. 35-52, DOI: \url{https://doi.org/10.1016/j.supflu.2004.03.005}.
#'
#' Sovova, H. (2017), 'Broken-and-intact cell model for supercritical fluid extraction: Its origin and limits', \emph{The Journal of Supercritical Fluids} \strong{129}, pp. 3-8, DOI: \url{https://doi.org/10.1016/j.supflu.2017.02.014}.
#'
#' @export
#' @keywords internal
#'
#' @seealso \code{\link{bicmod}}, \code{\link{show_pars}}
cer_ct_eq <- function(delta, t1, ys, qspec) {
  res <- ys * qspec * t1 * (1 - exp(-1 / delta))
  return(res)
}

#' @title RMSE: First extraction period (CER) of the characteristic times BIC model
#'
#' @description Part of the \code{\link{bicmod}} workflow. \strong{This function manual is hidden from the Index.}
#'
#' @param delta External mass transfer resistance \eqn{\theta_e} (dimeonsionless).
#' @param t1 Extraction time at the end of the CER period \eqn{t_1} (s).
#' @param y1 A \code{numeric} vector of \strong{observed} yields \eqn{e} (g/g insoluble solid) up to the end of the CER period.
#' @param ys Extract solubility in CO2 (\eqn{y_s}; g/g).
#' @param qspec Specific solvent flow rate (\eqn{q' \text{ in } kg_{solvent}/kg_{solid}/s}).
#'
#' @return The RMSE as a single \code{numeric} value \eqn{RMSE = \sum{\sqrt{(x_{model}-x_{observed})^2}/m}}.
#'
#' @export
#' @keywords internal
#'
#' @seealso \code{\link{cer_ct_eq}}, \code{\link{bicmod}}, \code{\link{show_pars}}
cer_ct_err <- function(delta, t1, y1, ys, qspec) {
  y <- cer_ct_eq(delta, t1, ys, qspec)
  err <- 100 * sum(abs((y - y1) / y1)) / length(y1)
  return(err)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION D1: Second extraction period (FER) of the CHARACTERISTIC TIMES model
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Equation: Second extraction period (FER) of the characteristic times BIC model
#'
#' @description Part of the \code{\link{bicmod}} workflow. \strong{This function manual is hidden from the Index.}
#'
#' @param G Initial fraction of solute in open (broken) cells \eqn{G}.
#' @param ti Characteristic time of the solid phase mass transfer (\eqn{t_i}; s).
#' @param t2 A \code{numeric} vector of extraction time (\eqn{t}; s).
#' @param cu Solute content in the untreated solid (\eqn{c_u}; kg/kg), i.e. maximum possible yield as a \strong{fraction}.
#' @param t1 Extraction time at the end of the CER (\eqn{t_1}; s).
#'
#' @return The single \code{numeric} result of the evaluated equation - specifically, the extraction yield (\eqn{e'}; g/g insoluble solid).
#'
#' @details
#' The Falling Extraction Rate (FER) period of the Characteristic Times BIC model is defined as (see also \strong{Eq. 3.33} in Rizza, 2014):
#' \deqn{e' = c_u \times (1 - (1 - G) \times \exp{(-(-t - t_1)/t_i)}) \text { for } t > t_1}
#'
#' @references
#' Rizza, C.S. (2014), \emph{Experiments and Modeling of Supercritical CO2 Extraction of Lipids from Microalgae}, MSc thesis, Universita Degli Studi Di Padova, Dipartimento Di Ingegneria Industriale.
#'
#' Sovova, H. (2005), 'Mathematical model for supercritical fluid extraction of natural products and extraction curve evaluation', \emph{The Journal of Supercritical Fluids} \strong{33} (1), pp. 35-52, DOI: \url{https://doi.org/10.1016/j.supflu.2004.03.005}.
#'
#' Sovova, H. (2017), 'Broken-and-intact cell model for supercritical fluid extraction: Its origin and limits', \emph{The Journal of Supercritical Fluids} \strong{129}, pp. 3-8, DOI: \url{https://doi.org/10.1016/j.supflu.2017.02.014}.
#'
#' @export
#' @keywords internal
#'
#' @seealso \code{\link{bicmod}}, \code{\link{show_pars}}
fer_ct_eq <- function(G, ti, t2, cu, t1) {
  res <- cu * (1 - (1 - G) * exp(-(t2 - t1[length(t1)]) / ti))
  return(res)
}

#' @title RMSE: Second extraction period (FER) of the characteristic times BIC model
#'
#' @description Part of the \code{\link{bicmod}} workflow. \strong{This function manual is hidden from the Index.}
#'
#' @param zeta A \code{numeric} vector of length 2 containing the characteristic time of solid phase mass transfer \eqn{t_i} (s) and
#' the initial fraction of solute in open (broken) cells \eqn{G}, \strong{in that order}.
#' @param t2 Extraction time at the end of the second extraction period (\eqn{t}; s).
#' @param y2 A \code{numeric} vector of \strong{observed} yields \eqn{e'} after the CER.
#' @param cu Solute content in the untreated solid (\eqn{c_u}; kg/kg), i.e. maximum possible yield as a \strong{fraction}.
#' @param t1 Extraction time at the end of the CER (\eqn{t_1}; s).
#'
#' @return The RMSE as a single \code{numeric} value \eqn{RMSE = \sum{\sqrt{(x_{model}-x_{observed})^2}/m}}.
#'
#' @export
#' @keywords internal
#'
#' @seealso \code{\link{fer_ct_eq}}, \code{\link{bicmod}}, \code{\link{show_pars}}
fer_ct_err <- function(zeta, t2, y2, cu, t1) {
  y <- fer_ct_eq(zeta[2], zeta[1], t2, cu, t1)
  err <- 100 * sum(abs((y - y2) / y2)) / length(y2)
  return(err)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION D2: Same as D1 with 'cu' as an additional parameter
#The CER for CHARACTERISTIC TIMES model does not change to estimate 'cu'
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Equation: Second extraction period (FER) of the characteristic times BIC model ('cu' estimation)
#'
#' @description Part of the \code{\link{bicmod}} workflow. \strong{This function manual is hidden from the Index.}
#'
#' @param zeta A \code{numeric} vector of length 2 containing the characteristic time of solid phase mass transfer \eqn{t_i} (s) and
#' the initial fraction of solute in open (broken) cells (\eqn{G}), in that order.
#' @param t2 Extraction time at the end of the second extraction period (\eqn{t}; s).
#' @param y2 A \code{numeric} vector of \strong{observed} yields after the CER \eqn{e'}.
#' @param t1 Extraction time at the end of the CER (\eqn{t_1}; s).
#'
#' @return The RMSE as a single \code{numeric} value \eqn{RMSE = \sum{\sqrt{(x_{model}-x_{observed})^2}/m}}.
#'
#' @references
#' Rizza, C.S. (2014), \emph{Experiments and Modeling of Supercritical CO2 Extraction of Lipids from Microalgae}, MSc thesis, Universita Degli Studi Di Padova, Dipartimento Di Ingegneria Industriale.
#'
#' Sovova, H. (2005), 'Mathematical model for supercritical fluid extraction of natural products and extraction curve evaluation', \emph{The Journal of Supercritical Fluids} \strong{33} (1), pp. 35-52, DOI: \url{https://doi.org/10.1016/j.supflu.2004.03.005}.
#'
#' Sovova, H. (2017), 'Broken-and-intact cell model for supercritical fluid extraction: Its origin and limits', \emph{The Journal of Supercritical Fluids} \strong{129}, pp. 3-8, DOI: \url{https://doi.org/10.1016/j.supflu.2017.02.014}.
#'
#' @export
#' @keywords internal
#'
#' @seealso \code{\link{fer_ct_eq}}, \code{\link{fer_ct_err}}, \code{\link{bicmod}}, \code{\link{show_pars}}
fer_ct_err_cu <- function(zeta, t2, y2, t1) {
  y <- fer_ct_eq(zeta[2], zeta[1], t2, zeta[3], t1)
  err <- 100 * sum(abs((y - y2) / y2)) / length(y2)
  return(err)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION E1: First extraction period (CER) of the COMPLETE model
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Equation: First extraction period (CER) of the complete BIC model
#'
#' @description Part of the \code{\link{bicmod}} workflow. \strong{This function manual is hidden from the Index.}
#'
#' @param iota Fluid phase mass transfer coefficient (\eqn{k_f}; 1/s).
#' @param q1 Relative amount of passed solvent at the end of the CER (\eqn{q \text{ in } kg_{solvent}/kg_{insoluble solid}}).
#' @param a0 Specific surface area per unit volume of extraction bed (\eqn{a_0}; 1/m).
#' @param gam Solvent to matrix ratio in the bed (\eqn{\gamma \text{ in } kg_{solvent}/kg_{insoluble solid}}).
#' @param porosity Bed porosity (\eqn{\varepsilon}; dimensionless).
#' @param qaver Average solvent flow rate (\eqn{\dot{Q}}; kg/s).
#' @param Nm Mass of insoluble material (\eqn{N_m}; g).
#' @param ys Extract solubility in CO2 (\eqn{y_s}; g/g).
#'
#' @return The single \code{numeric} result of the evaluated equation - specifically, the extraction yield \eqn{e} (g/g insoluble solid).
#'
#' @details
#' The Constant Extraction Rate (CER) period of the complete Sovova (2005) model is calculated as follows (see also \strong{Eq. 3.27} in
#' Rizza, 2014):
#' \deqn{e = q \times y_s \times (1 - \exp{(-1/\theta_e)}) \text{ for } 0 \leq q \leq q_m}
#'
#' In the above equation, \eqn{q_m} is the relative amount of passed solvent at the end of the CER
#' and the external mass transfer resistance \eqn{\theta_e} is defined as (see also \strong{Eq 3.23} in Rizza, 2014):
#' \deqn{\theta_e = (\varepsilon \times \dot{Q})/\gamma \times k_f \times a_0 \times N_m}
#'
#' @references
#' Rizza, C.S. (2014), \emph{Experiments and Modeling of Supercritical CO2 Extraction of Lipids from Microalgae}, MSc thesis, Universita Degli Studi Di Padova, Dipartimento Di Ingegneria Industriale.
#'
#' Sovova, H. (2005), 'Mathematical model for supercritical fluid extraction of natural products and extraction curve evaluation', \emph{The Journal of Supercritical Fluids} \strong{33} (1), pp. 35-52, DOI: \url{https://doi.org/10.1016/j.supflu.2004.03.005}.
#'
#' Sovova, H. (2017), 'Broken-and-intact cell model for supercritical fluid extraction: Its origin and limits', \emph{The Journal of Supercritical Fluids} \strong{129}, pp. 3-8, DOI: \url{https://doi.org/10.1016/j.supflu.2017.02.014}.
#'
#' @export
#' @keywords internal
#'
#' @seealso \code{\link{bicmod}}, \code{\link{show_pars}}
cer_cmp_eq <- function(iota, q1, a0, gam, porosity, qaver, Nm, ys) {
  res <- q1 * ys * (1 - exp(-1 / (porosity * qaver / (gam * iota * a0 * Nm))))
  return(res)
}

#' @title RMSE: First extraction period (CER) of the complete BIC model
#'
#' @description Part of the \code{\link{bicmod}} workflow. \strong{This function manual is hidden from the Index.}
#'
#' @param iota Fluid phase mass transfer coefficient (\eqn{k_f}; 1/s).
#' @param q1 Relative amount of passed solvent at the end of the CER (\eqn{q \text{ in } kg_{solvent}/kg_{insoluble solid}}).
#' @param y1 Yield (\eqn{e}; g/g insoluble solid) at the end of the CER.
#' @param a0 Specific surface area per unit volume of extraction bed (\eqn{a_0}; 1/m).
#' @param gam Solvent to matrix ratio in the bed (\eqn{\gamma \text{ in } kg_{solvent}/kg_{insoluble solid}}).
#' @param porosity Bed porosity (\eqn{\varepsilon}; dimensionless).
#' @param qaver Average solvent flow rate (\eqn{\dot{Q}}; kg/s).
#' @param Nm Mass of insoluble material (\eqn{N_m}; g).
#' @param ys Extract solubility in CO2 (\eqn{y_s}; g/g).
#'
#' @return The RMSE as a single \code{numeric} value \eqn{RMSE = \sum{\sqrt{(x_{model}-x_{observed})^2}/m}}.
#'
#' @references
#' Rizza, C.S. (2014), \emph{Experiments and Modeling of Supercritical CO2 Extraction of Lipids from Microalgae}, MSc thesis, Universita Degli Studi Di Padova, Dipartimento Di Ingegneria Industriale.
#'
#' Sovova, H. (2005), 'Mathematical model for supercritical fluid extraction of natural products and extraction curve evaluation', \emph{The Journal of Supercritical Fluids} \strong{33} (1), pp. 35-52, DOI: \url{https://doi.org/10.1016/j.supflu.2004.03.005}.
#'
#' Sovova, H. (2017), 'Broken-and-intact cell model for supercritical fluid extraction: Its origin and limits', \emph{The Journal of Supercritical Fluids} \strong{129}, pp. 3-8, DOI: \url{https://doi.org/10.1016/j.supflu.2017.02.014}.
#'
#' @export
#' @keywords internal
#'
#' @seealso \code{\link{bicmod}}, \code{\link{show_pars}}
cer_cmp_err <- function(iota, q1, y1, a0, gam, porosity, qaver, Nm, ys) {
  y <- cer_cmp_eq(iota, q1, a0, gam, porosity, qaver, Nm, ys)
  err <- 100 * sum(abs((y - y1) / y1)) / length(y1)
  return(err)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION E2: Same as E1 with 'cu' as an additional parameter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Equation: First extraction period (CER) of the complete BIC model ('cu' estimation)
#'
#' @description Part of the \code{\link{bicmod}} workflow. \strong{This function manual is hidden from the Index.}
#'
#' @param iota Fluid phase mass transfer coefficient (\eqn{k_f}; 1/s).
#' @param q1 Relative amount of passed solvent at the end of the CER (\eqn{q \text{ in } kg_{solvent}/kg_{insoluble solid}}).
#' @param y1 Yield (\eqn{e}; g/g insoluble solid) at the end of the CER.
#' @param a0 Specific surface area per unit volume of extraction bed (\eqn{a_0}; 1/m).
#' @param gam Solvent to matrix ratio in the bed (\eqn{\gamma \text{ in } kg_{solvent}/kg_{insoluble solid}}).
#' @param porosity Bed porosity (\eqn{\varepsilon}; dimensionless).
#' @param qaver Average solvent flow rate (\eqn{\dot{Q}}; kg/s).
#' @param Ng Total dry mass of solute + insoluble material (\eqn{N}; g).
#' @param ys Extract solubility in CO2 (\eqn{y_s}; g/g).
#' @param xup Weight fraction (concentration) in the untreated solid \eqn{x_u}.
#'
#' @return The RMSE as a single \code{numeric} value \eqn{RMSE = \sum{\sqrt{(x_{model}-x_{observed})^2}/m}}.
#'
#' @references
#' Rizza, C.S. (2014), \emph{Experiments and Modeling of Supercritical CO2 Extraction of Lipids from Microalgae}, MSc thesis, Universita Degli Studi Di Padova, Dipartimento Di Ingegneria Industriale.
#'
#' Sovova, H. (2005), 'Mathematical model for supercritical fluid extraction of natural products and extraction curve evaluation', \emph{The Journal of Supercritical Fluids} \strong{33} (1), pp. 35-52, DOI: \url{https://doi.org/10.1016/j.supflu.2004.03.005}.
#'
#' Sovova, H. (2017), 'Broken-and-intact cell model for supercritical fluid extraction: Its origin and limits', \emph{The Journal of Supercritical Fluids} \strong{129}, pp. 3-8, DOI: \url{https://doi.org/10.1016/j.supflu.2017.02.014}.
#'
#' @export
#' @keywords internal
#'
#' @seealso \code{\link{bicmod}}, \code{\link{show_pars}}
cer_cmp_cu <- function(iota, q1, y1, a0, gam, porosity, qaver, Ng, ys, xup) {
  cu <- xup / (1 + xup)
  N <- Ng / 1000
  Nmg <- (1 - cu) * Ng
  Nm <- Nmg / 1000
  y <- cer_cmp_eq(iota, q1, a0, gam, porosity, qaver, Nm, ys) * Nm / N
  err <- 100 * sum(abs((y - y1) / y1)) / length(y1)
  return(err)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION F1: Second (FER) and third (DC) periods of the COMPLETE model
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Equation: Second (FER) and third (DC) periods of the complete BIC model
#'
#' @description Part of the \code{\link{bicmod}} workflow. \strong{This function manual is hidden from the Index.}
#'
#' @param r Grinding efficiency (\eqn{r}; fraction of broken cells).
#' @param ksas The product of the solid phase mass transfer coefficient (\eqn{k_s}; 1/s)
#' and the specific area between intact and broken cells (\eqn{a_s}; 1/m), in 1/m/s.
#' @param x1,x2,x3 Relative amounts of expended solvent spanning the CER, FER, and DC, respectively (\eqn{q_m}, \eqn{q_n}, \eqn{q},
#' respectively; kg/kg insoluble solid). The first two parameters are optional (\code{NA} by default).
#' @param kf Fluid phase mass transfer coefficient (\eqn{k_f}; 1/s).
#' @param xu Weight fraction (concentration) in the untreated solid \eqn{x_u}.
#' @param ys Extract solubility in CO2 (\eqn{y_s}; g/g).
#' @param porosity Bed porosity (\eqn{\varepsilon}; dimensionless).
#' @param qaver Average solvent flow rate (\eqn{\dot{Q}}; kg/s).
#' @param gam Solvent to matrix ratio in the bed (\eqn{\gamma \text{ in } kg_{solvent}/kg_{insoluble solid}}).
#' @param Nm Mass of insoluble material (\eqn{N_m}; g).
#' @param a0 Specific surface area per unit volume of extraction bed (\eqn{a_0}; 1/m).
#' @param out One of \code{"final"} (default) or \code{"all"}. Determines which results are reported in the output (see \strong{Value}).
#'
#' @return When \code{out} is set to \code{"final"}, returns a \code{numeric} value/vector of yields spanning the DC region.
#' If \code{out} is \code{"all"}, a \code{list} is returned containing the following named elements:
#' \enumerate{
#' \item \strong{$pars}: A named \code{numeric} vector of parameters calculated as part of the evaluation.
#' \item \strong{$y1; $y2; $y3}: Vectors of calculated yields (kg/kg insoluble solid) spanning the CER, FER, and DC regions of the
#' extraction curve, respectively.
#' }
#'
#' @details
#' The Falling Extraction Rate (FER) and subsequent Diffusion-Controlled (DC) periods are defined by the following equations
#' (see also \strong{Eqs. 3.28} and \strong{3.29} in Rizza, 2014):
#' \deqn{e \text{ (FER)} = q \times y_s - r \times x_u \times \theta_e \times \exp{(\beta/\theta_e \times \ln{\{1 + 1/r \times (\exp{[\frac{q - q_m}{\gamma \times \theta_i}]}-1)\}}-1/\theta_e)} \text{ for } q_m \leq q< q_n}
#' \deqn{e \text{ (DC)} = x_u \times [1 - \beta \times \ln{\{1 + (1 - r) \times [\exp{(1/\beta)}-1] \times \exp{(\frac{q - q_m}{\gamma \times \theta_i})}\}}] \text{ for } q \geq q_n}
#'
#' Herein, the coefficient \eqn{\beta} is used to simplify equation expression and is defined as
#' (see also \strong{Eq. 3.26} in Rizza, 2014):
#' \deqn{\beta = (\gamma \times \theta_i \times y_s)/x_u}
#'
#' The internal and external mass transfer resistances (\eqn{\theta_i} and \eqn{\theta_e}, respectively) are defined as
#' (see also \strong{Eqs. 3.22} and \strong{3.23} in Rizza, 2014):
#' \deqn{\theta_i = ((1-\varepsilon) \times \dot{Q})/(\gamma \times k_sa_s \times N_m)}
#' \deqn{\theta_e = (\varepsilon \times \dot{Q})/\gamma \times k_f \times a_0 \times N_m}
#'
#' Finally, the relative amounts of solvent expended at the end of the CER and FER (\eqn{q_m} and \eqn{q_n}, respectively) is calculated via
#' (see also \strong{Eqs. 3.24} and \strong{3.25} in Rizza, 2014):
#' \deqn{q_m = (r \times x_u \times \theta_e)/y_s}
#' \deqn{q_n = q_m + \gamma \times \theta_i \times \ln{[1 - r + r \times \exp{(1/\beta)}]}}
#'
#' @references
#' Rizza, C.S. (2014), \emph{Experiments and Modeling of Supercritical CO2 Extraction of Lipids from Microalgae}, MSc thesis, Universita Degli Studi Di Padova, Dipartimento Di Ingegneria Industriale.
#'
#' Sovova, H. (2005), 'Mathematical model for supercritical fluid extraction of natural products and extraction curve evaluation', \emph{The Journal of Supercritical Fluids} \strong{33} (1), pp. 35-52, DOI: \url{https://doi.org/10.1016/j.supflu.2004.03.005}.
#'
#' Sovova, H. (2017), 'Broken-and-intact cell model for supercritical fluid extraction: Its origin and limits', \emph{The Journal of Supercritical Fluids} \strong{129}, pp. 3-8, DOI: \url{https://doi.org/10.1016/j.supflu.2017.02.014}.
#'
#' @export
#' @keywords internal
#'
#' @seealso \code{\link{bicmod}}, \code{\link{show_pars}}
fer_cmp_eq <- function(r, ksas, x3, kf, xu, ys, porosity, qaver, gam, Nm, a0, out = "final", x1 = NA, x2 = NA) { #"final" "all"
  if(!any(c("final","all") %in% out)) stop("Argument 'out' must be one of: 'final' or 'all'!")

  thetae <- porosity * qaver / (gam * kf * a0 * Nm)
  qm <- r * xu * thetae / ys
  thetai <- (1 - porosity) * qaver / (gam * ksas * Nm)
  betam <- gam * thetai * ys / xu
  qn <- qm + gam * thetai * log(1 - r + r * exp(1 / betam))

  y1 <- if(!any(is.na(x1))) x1 * ys * (1 - exp(-1 / thetae)) else NA
  y2 <- if(!any(is.na(x2))) ys * x2 - r * xu * thetae * exp((betam / thetae) * log(1 + (1 / r) * (exp((x2 - qm) / (gam * thetai)) - 1)) - 1 / thetae) else NA
  y3 <- xu * (1 - betam * log(1 + (1 - r) * (exp(1 / betam) - 1) * exp(-(x3 - qm) / (gam * thetai))))
  res <- if(out=="all") list(pars = c(thetae = thetae, thetai = thetai, qm = qm, betam = betam, qn = qn),
                             y1 = y1, y2 = y2, y3 = y3) else y3
  return(res)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION F2: Same as F1 with 'cu' as an additional parameter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Equation: Second (FER) and third (DC) periods of the complete BIC model ('cu' estimation)
#'
#' @description Part of the \code{\link{bicmod}} workflow. \strong{This function manual is hidden from the Index.}
#'
#' @param r Grinding efficiency \eqn{r} (fraction of broken cells).
#' @param ksas The product of the solid phase mass transfer coefficient (\eqn{k_s}; 1/s).
#' and the specific area between intact and broken cells (\eqn{a_s}; 1/m), in 1/m/s.
#' @param xu Weight fraction (concentration) in the untreated solid \eqn{x_u}.
#' @param x Relative amount of expended solvent spanning the DC (\eqn{q}; kg/kg insoluble solid).
#' @param kf Fluid phase mass transfer coefficient (\eqn{k_f}; 1/s).
#' @param ys Extract solubility in CO2 (\eqn{y_s}; g/g).
#' @param porosity Bed porosity (\eqn{\varepsilon}; dimensionless).
#' @param qaver Average solvent flow rate (\eqn{\dot{Q}}; kg/s).
#' @param gam Solvent to matrix ratio in the bed (\eqn{\gamma \text{ in } kg_{solvent}/kg_{insoluble solid}}).
#' @param a0 Specific surface area per unit volume of extraction bed (\eqn{a_0}; 1/m).
#' @param Ng Total dry mass \eqn{N} (solute + insoluble material; g).
#'
#' @return The single \code{numeric} result of the evaluated equation - specifically, the extraction yield \eqn{e} (g/g insoluble solid).
#'
#' @references
#' Rizza, C.S. (2014), \emph{Experiments and Modeling of Supercritical CO2 Extraction of Lipids from Microalgae}, MSc thesis, Universita Degli Studi Di Padova, Dipartimento Di Ingegneria Industriale.
#'
#' Sovova, H. (2005), 'Mathematical model for supercritical fluid extraction of natural products and extraction curve evaluation', \emph{The Journal of Supercritical Fluids} \strong{33} (1), pp. 35-52, DOI: \url{https://doi.org/10.1016/j.supflu.2004.03.005}.
#'
#' Sovova, H. (2017), 'Broken-and-intact cell model for supercritical fluid extraction: Its origin and limits', \emph{The Journal of Supercritical Fluids} \strong{129}, pp. 3-8, DOI: \url{https://doi.org/10.1016/j.supflu.2017.02.014}.
#'
#' @export
#' @keywords internal
#'
#' @seealso \code{\link{bicmod}}, \code{\link{show_pars}}
fer_cmp_cu <- function(r, ksas, xu, x, kf, ys, porosity, qaver, gam, a0, Ng) {
  cu <- xu / (1 + xu)
  N <- Ng / 1000
  Nmg <- (1 - cu) * Ng
  Nm <- Nmg / 1000
  y <- fer_cmp_eq(r, ksas, x, kf, xu, ys, porosity, qaver, gam, Nm, a0) * Nm / N
  return(y)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION G: Relative amount of expended solvent considering only the CER and DC
# Computation of the relative amount of passed solvent at the end of the first extraction period of the complete model
# when only the first and the third periods are considered:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Equation: Relative amount of expended solvent considering only the CER and DC of the complete BIC model
#'
#' @description Part of the \code{\link{bicmod}} workflow. \strong{This function manual is hidden from the Index.}
#'
#' @param x A \code{numeric} vector of relative amounts of expended solvent (\eqn{q}; kg/kg insoluble solid) to evaluate.
#' @param ys Extract solubility in CO2 (\eqn{y_s}; g/g).
#' @param thetae  External material (mass) transport resistance \eqn{\theta_e}.
#' @param xu Weight fraction (concentration) in the untreated solid \eqn{x_u}.
#' @param betam Coefficient that simplifies the form of the equation (\eqn{\beta}; see \strong{Eq. 3.26 in Rizza (2014)} for definition).
#' @param r Grinding efficiency \eqn{r} (fraction of broken cells).
#' @param qm Relative amount of expended solvent (\eqn{q_m}; kg/kg insoluble solid) at the end of the CER.
#' @param gam Solvent to matrix ratio in the bed (\eqn{\gamma \text{ in } kg_{solvent}/kg_{insoluble solid}}).
#' @param thetai Internal material (mass) transfer resistance \eqn{\theta_i}.
#'
#' @return The single \code{numeric} result of the evaluated equation - specifically, the relative amount of expended solvent
#' \eqn{q} (kg/kg insoluble solid).
#'
#' @details
#' The equation used to calculate the relative amount of expended solvent \eqn{q_{cmp}} considering only the Constant Extraction Rate (CER) and
#' Diffusion-Controlled (DC) periods of the overall extraction curve is (Rizza, 2014):
#'
#' \deqn{q_{cmp} = x_u \times [1 - \beta \times \ln{\{1 + (1 - r) \times [\exp{(1/\beta)}-1] \times \exp{(\frac{q - q_m}{\gamma \times \theta_i})}\}}] - q \times y_s \times (1 - \exp{(-1/\theta_e)})}
#'
#' This is a combination of \strong{Eqs. 3.27} and \strong{3.29} as presented by Rizza (2014).
#'
#' @references
#' Rizza, C.S. (2014), \emph{Experiments and Modeling of Supercritical CO2 Extraction of Lipids from Microalgae}, MSc thesis, Universita Degli Studi Di Padova, Dipartimento Di Ingegneria Industriale.
#'
#' Sovova, H. (2005), 'Mathematical model for supercritical fluid extraction of natural products and extraction curve evaluation', \emph{The Journal of Supercritical Fluids} \strong{33} (1), pp. 35-52, DOI: \url{https://doi.org/10.1016/j.supflu.2004.03.005}.
#'
#' Sovova, H. (2017), 'Broken-and-intact cell model for supercritical fluid extraction: Its origin and limits', \emph{The Journal of Supercritical Fluids} \strong{129}, pp. 3-8, DOI: \url{https://doi.org/10.1016/j.supflu.2017.02.014}.
#'
#' @export
#' @keywords internal
#'
#' @seealso \code{\link{bicmod}}, \code{\link{show_pars}}
solv_cmp <- function(x, ys, thetae, xu, betam, r, qm, gam, thetai) {
  qcmp <- xu * (1 - betam * log(1 + (1 - r) * (exp(1 / betam) - 1) * exp(-(x - qm) / (gam * thetai)))) - x * ys * (1 - exp(-1 / thetae))
  return(qcmp)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION H: Derive the SIMPLIFIED BIC MODEL (Rizza, 2014)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Build a simplified BIC model
#'
#' @description Derives, summarises, and visualises the results of the simplified Broken-and-Intact Cells (BIC) model of Sovova (2005).
#' Part of the \code{\link{bicmod}} workflow.
#'
#' @param yield A \code{numeric} vector of yields (\eqn{e}; g/g insoluble solid).
#' @param q A \code{numeric} vector of relative amount of expended solvent (\eqn{q}; kg/kg insoluble solid).
#' @param n Period corresponding to the end of the CER (\eqn{n}; in \strong{number of experimental points}).
#' @param xu Weight fraction (concentration) in the untreated solid \eqn{x_u}. Defaults to \code{NA}.
#' Must be provided only when \code{est_cu} is \code{FALSE}.
#' @param qaver Average solvent flow rate (\eqn{\dot{Q}}; kg/s).
#' @param Nm Mass of insoluble material (\eqn{N_m}; g). Must be provided only when \code{est_cu} is \code{FALSE}.
#' @param porosity Bed porosity (\eqn{\varepsilon}; dimensionless).
#' @param ys Extract solubility in CO2 (\eqn{y_s}; g/g). May be estimated using \code{\link{oec_bp}}. Defaults to \code{NA}.
#' Must be provided only when \code{est_cu} is \code{FALSE}.
#' @param c3 An initial estimate of the asymptotic extraction yield at infinite time (\eqn{c_u}; fraction) to be provided
#' \strong{only} when \code{est_cu} is \code{TRUE}.
#' @param Ng Total dry mass \eqn{N} (solute + insoluble material; g). Defaults to \code{NA}. Must be provided when \code{est_cu} is \code{TRUE}.
#' @param est_cu A \code{logical} indicating whether solute content in the untreated solid (kg/kg), i.e. maximum possible yield,
#' should be estimated (\code{FALSE} by default).
#' @param maxq Maximum x-axis value (solvent expended, kg/kg insoluble solid) to use for model predictions.
#' Defaults to 120% of the maximum \code{q} value.
#' @param r0  Initial estimate of the grinding efficiency \eqn{r} (fraction of broken cells).
#' @param ksas0 Initial estimate of the product of the solid phase mass transfer coefficient (\eqn{k_s}; 1/s)
#' and the specific area between intact and broken cells (\eqn{a_s}; 1/m), in 1/m/s.
#' @param qc0 Initial estimate of solvent expended at the end of the CER (\eqn{q_m}; kg/kg CO2).
#' @param modpts A single \code{numeric} value specifying how many points should be modelled throughout the OEC curve range (\code{100} by default).
#' @param nlsm A \code{character} vector of non-linear optimization methods to use for convergence of the model.
#' One or more of: \code{"MM"}, \code{"tau"}, \code{"CM"}, and/or \code{"mtl"} (\strong{see \code{\link[robustbase]{nlrob}} for details}).
#' @param aggreg A string specifying how the "best" results of non-linear optimization (\code{nlsm}) should be chosen.
#' One of \code{"aard"} (default; value with minimum Average Absolute Relative Deviation is chosen) or \code{"mean"}
#' (the arithmetic mean of all results is taken).
#' @param const_flow A \code{logical} specifying whether the flow rate is constant throughout the extraction.
#' If set to \code{TRUE}, the solvent-material ratio (S/M) is \strong{not} converted to extraction time (min).
#'
#' @return A named \code{list} containing the following elements:
#' \enumerate{
#' \item \strong{$ordt}: A \code{data.frame} containing the original input data including the temporal variable \code{$t},
#' the Solvent-Solute (S/M) ratio \code{$x}, the \strong{actual} fractional yield (\code{$y}; g/g insoluble solid), as well as
#' the \strong{modeled} yield (\code{$mod_y}).
#' \item \strong{$mdt}: A \code{data.frame} of modeled data including the model type (\code{$model}),
#' the region of the extraction curve (\code{$period}), as well as S/M ratio \code{$x}, fractional yield \code{$y}, and time (\code{$t})
#' up to \code{maxq}.
#' \item \strong{$mod_pars}: The model parameters including the constants \eqn{C_1} and \eqn{C_2} (\code{"c1"} and \code{"c2"}),
#' the relative amount of expended solvent (kg/kg insoluble solid) at the end of the CER (\eqn{q_m}; \code{"qm"}),
#' the grinding efficiency (\eqn{r}; \code{"r"}), the product of the solid phase mass transfer coefficient \eqn{k_s}
#' and the specific area between intact and broken cells \eqn{a_s} (\eqn{k_sa_s}; \code{"ksas"}), and the initial
#' fraction of solute in broken cells (\eqn{G}; \code{"G"}).
#' See \code{\link{show_pars}} for detailed description of these parameters.
#' \item \strong{$fit_pars}: A \code{character} vector of which parameters in \code{$mod_pars} were iteratively fit to the model.
#' \item \strong{$resid}: A named \code{numeric} vector of various error values for the model, including Average Absolute Relative Deviation
#' AARD (\code{"aard"}), round mean squared error RMSE (\code{"rmse"}), and the R2 value (\code{"r2"}).
#' }
#' @export
#'
#' @references
#' Rizza, C.S. (2014), \emph{Experiments and Modeling of Supercritical CO2 Extraction of Lipids from Microalgae}, MSc thesis, Universita Degli Studi Di Padova, Dipartimento Di Ingegneria Industriale.
#'
#' Sovova, H. (2005), 'Mathematical model for supercritical fluid extraction of natural products and extraction curve evaluation', \emph{The Journal of Supercritical Fluids} \strong{33} (1), pp. 35-52, DOI: \url{https://doi.org/10.1016/j.supflu.2004.03.005}.
#'
#' Sovova, H. (2017), 'Broken-and-intact cell model for supercritical fluid extraction: Its origin and limits', \emph{The Journal of Supercritical Fluids} \strong{129}, pp. 3-8, DOI: \url{https://doi.org/10.1016/j.supflu.2017.02.014}.
#'
#' @seealso \code{\link{bicmod}}, \code{\link{show_pars}}, \code{\link{oec_bp}}, \code{\link{moderr}}
#'
#' @importFrom stats as.formula coef uniroot
bic_sm <- function(yield, q, n, xu = NA, qaver, Nm = NA, porosity, ys = NA, c3 = NA, Ng = NA, est_cu = FALSE,
                   maxq = round(max(q)*1.2,-1), r0 = 0.4, ksas0 = 0.0001, qc0 = 400, aggreg = "aard", modpts = 100,
                   nlsm = c("MM","tau","CM","mtl"), const_flow = TRUE) {

  #Preliminary checks
  modnms <- c("MM","tau","CM","mtl")
  if(any(nlsm %in% "all")) nlsm <- modnms
  if(!all(nlsm %in% modnms)) stop(paste0("At least one NLS fitting method not recognised! Possible values are: ", paste0("'", modnms, "'", collapse = ", "), "."))
  if(est_cu & any(is.na(c(c3,Ng)))) stop("When 'cu' parameter estimation is required ('est_cu' set to TRUE), parameters 'c3' and 'Ng' must be provided!")
  if(!est_cu & any(is.na(c(xu,Nm,ys)))) stop("Unless 'cu' parameter estimation is required ('est_cu' set to TRUE), parameters 'xu', 'Nm', 'ys' must all be provided!")
  m <- length(q) #Number of experimental data points

  #SIMPLIFIED MODEL
  #Adjustable parameters of the simplified model:
  #r_new = first estimation of the grinding efficiency
  #ksas_new = first estimation of the product ks*as (ks = internal mass transfer coefficient ,
  #as = specific area between the regions of broken and intact cells)

  #nlrob() approach - WE HAVE A WINNER! Works with methods "MM", "tau", "CM" (less well), and "mtl"
  set.seed(42)
  aardvec <- rep(NA,4)
  resmat <- as.data.frame(matrix(nrow = 4, ncol = 2, dimnames = list(c(), c("c1","c2"))))
  if(est_cu) resmat[,"c3"] <- rep(NA,nrow(resmat))
  sd_formula <- if(est_cu) as.formula("yield ~ fer_sim_cu(c1, c2, c3, q, Ng)") else as.formula("yield ~ fer_sim_eq(c1, c2, q, xu)")
  optlims <- list(lower = c(c1 = r0/20, c2 = ksas0/20, c3 = if(est_cu) c3/20 else NULL),
                  upper = c(c1 = r0*20, c2 = ksas0*20, c3 = if(est_cu) 1 else NULL))
  optdata <- data.frame(yield = yield, q = q, if(est_cu) rep(Ng, length(yield)) else rep(xu, length(yield)))
  colnames(optdata)[3] <- if(est_cu) "Ng" else "xu"
  for(i in seq_along(nlsm)) {
    optres <- try(robustbase::nlrob(sd_formula, #start = c(c1 = alpha[1], c2 = alpha[2]),
                                    #psi = .Mwgt.psi1("huber", cc = 1.345),
                                    data = optdata,
                                    lower = optlims[["lower"]],
                                    upper = optlims[["upper"]], method = nlsm[i]))
    errchk <- inherits(optres, "try-error")
    if(!errchk) {
      resmat[i,] <- cfs <- coef(optres)
      e_opt <- xu * (1 - cfs[1] * exp(-cfs[2] * q[n:m]))
      aardvec[i] <- moderr(pred = e_opt, act = yield[n:m])[["aard"]]
    }
  }

  if(aggreg == "aard") {
    min_aard <- which(aardvec == min(aardvec, na.rm = TRUE))
    c1 <- resmat[min_aard,"c1"]
    c2 <- resmat[min_aard,"c2"]
  } else if(aggreg == "mean") {
    c1 <- mean(resmat[,"c1"], na.rm = TRUE)
    c2 <- mean(resmat[,"c2"], na.rm = TRUE)
  }

  if(est_cu) {
    c3 <- mean(resmat[,"c3"], na.rm = TRUE)
    cu_est <- c3 / (1 + c3)
    return(list(cu = cu_est, mod_coefs = c(c1 = c1, c2 = c2, c3 = c3)))
  }

  #Fitting for c(1) and c(2)
  qm <- uniroot(solv_sim, interval = c(qc0/20, qc0*20), tol = 1e-10, maxiter = 10000, xu = xu, c1 = c1, c2 = c2, ys = ys)[["root"]]

  #Creation of the vector of the horizontal axis
  xdt_cer <- seq(0, qm, length.out = modpts)
  xdt_fer <- seq(qm + 0.1, maxq, length.out = modpts)

  #Calculation of the yield at each q of the horizontal axis
  ydt_cer <- xdt_cer * ys # [g/g] calculated yield of the CER
  ydt_fer <- fer_sim_eq(c1, c2, xdt_fer, xu) # [g/g] calculated yield of the FER

  #Calculation of model predictions for input data points
  prind <- if(qm > q[n]) c(n,n+1) else c(n-1,n)
  cer_pred <- q[1:prind[1]] * ys #[g/g] calculated yield of the CER
  fer_pred <- fer_sim_eq(c1, c2, q[prind[2]:m], xu) #[g/g] calculated yield of the FER

  #Calculation of the first estimation of r and ksas from the simplified model
  r_new <- 1 - c1 * exp(-c2 * qm / 2) # [-] first estimation of r
  ksas_new <- (1 - r_new) * (1 - porosity) * (qaver) * c2 / Nm # [1/(s*m)] first estimation of ksas

  #Calculation of G with the simplified model
  eqc <- qm * ys
  G <- eqc / xu

  #Calculation of the residual function of the SIMPLIFIED MODEL
  y_fer <- yield[n:m] # [g/g] experimental yield of the FER
  q_fer <- q[n:m]
  e_fer <- fer_sim_eq(c1, c2, q_fer, xu)
  resid <- moderr(pred = e_fer, act = y_fer)

  #Compile output data
  cer_dt <- cbind.data.frame(model = rep("sim",length(xdt_cer)), period = rep("cer",length(xdt_cer)), x = xdt_cer, y = ydt_cer)
  fer_dt <- cbind.data.frame(model = rep("sim",length(xdt_fer)), period = rep("fer", length(xdt_fer)), x = xdt_fer, y = ydt_fer)
  mod_dt <- rbind.data.frame(cer_dt, fer_dt)

  if(const_flow){
    #Add time equivalent (for plotting)
    mod_dt[,"t"] <- mod_dt[,"x"]*Nm/qaver/60 #Extraction time (min)

    #Also derive time for original q data
    ort <- q*Nm/qaver/60

    ordt <- data.frame(t = ort, x = q, y = yield, mod_y = c(cer_pred,fer_pred))
  } else ordt <- data.frame(x = q, y = yield, mod_y = c(cer_pred,fer_pred))

  return(list(ordt = ordt,
              mdt = mod_dt,
              mod_pars = c(c1 = c1, c2 = c2, qm = qm, r = r_new, ksas = ksas_new, G = G),
              fit_pars = c("c1", "c2", "qm"), resid = resid))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION I: Derive the SIMPLIFIED MODEL BASED ON CHARACTERISTIC TIMES (Rizza, 2014)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Derive the simplified BIC model based on characteristic times
#'
#' @description Derives, summarises, and visualises the results of the characteristic times Broken-and-Intact Cells (BIC)
#' model of Sovova (2005).
#' Part of the \code{\link{bicmod}} workflow.
#'
#' @param yield Fractional yield of extract (g/g total dry solid).
#' @param t A \code{numeric} vector of times at which \code{yield} was recorded (s).
#' @param n Period corresponding to the end of the CER (in \strong{number of experimental points}).
#' @param qaver Average solvent flow rate (kg/s).
#' @param cu Solute content in the untreated solid (kg/kg), i.e. maximum possible yield as a \strong{fraction}.
#' @param N Total dry mass (\strong{kg}).
#' @param Nm Mass of insoluble material (g).
#' @param ys Extract solubility in CO2 (g/g).
#' @param modpts A single \code{numeric} value specifying how many points should be modelled throughout the OEC curve range (\code{100} by default).
#' @param thetaf0 Initial estimate of the external mass transfer resistance. Defaults to \code{1}.
#' @param ti0 Initial estimate of the characteristic time of the solid phase mass transfer (s). Defaults to \code{30} s.
#' @param maxt  Maximum x-axis value (time, s) to use for model predictions.
#' Defaults to 120% of the maximum \code{t} value.
#'
#' @return A named \code{list} containing the following elements:
#' \enumerate{
#' \item \strong{$ordt}: A \code{data.frame} containing the original input data including the time variable \code{$x},
#' the \strong{actual} fractional yield (\code{$y}; g/g \strong{total dry solid}), as well as the \strong{modeled} yield (\code{$mod_y}.
#' \item \strong{$mdt}: A \code{data.frame} of modeled data including the model type (\code{$model}),
#' the region of the extraction curve (\code{$period}), as well as the time (\code{$t}) up to \code{maxt} and fractional yield \code{$y}.
#' \item \strong{$mod_pars}: The model parameters including the external mass transfer resistance (\eqn{\theta_e}; \code{"thetaf"}),
#' extraction time/duration of the FER (\eqn{t_i}; \code{"ti"}), extraction time (\eqn{t'}) and yield (\eqn{e'}; g/g total dry solid)
#' at the end of CER (\code{"tprime"} and \eqn{"eprime"}), and the initial fraction of solute in broken cells (\eqn{G}; \code{"G"}).
#' (\eqn{k_sa_s}; \code{"ksas"}). See \code{\link{show_pars}} for detailed description of these parameters.
#' \item \strong{$fit_pars}: A \code{character} vector of which parameters in \code{$mod_pars} were iteratively fit to the model.
#' \item \strong{$resid}: A named \code{numeric} vector of various error values for the model, including Average Absolute Relative Deviation
#' AARD (\code{"aard"}), round mean squared error RMSE (\code{"rmse"}), and the R2 value (\code{"r2"}).
#' }
#' @export
#'
#' @details
#' For a detailed description of this variant of the Broken and Intact Cells (BIC) model, refer to \code{\link{bicmod}}.
#'
#' @references
#' Rizza, C.S. (2014), \emph{Experiments and Modeling of Supercritical CO2 Extraction of Lipids from Microalgae}, MSc thesis, Universita Degli Studi Di Padova, Dipartimento Di Ingegneria Industriale.
#'
#' Sovova, H. (2005), 'Mathematical model for supercritical fluid extraction of natural products and extraction curve evaluation', \emph{The Journal of Supercritical Fluids} \strong{33} (1), pp. 35-52, DOI: \url{https://doi.org/10.1016/j.supflu.2004.03.005}.
#'
#' Sovova, H. (2017), 'Broken-and-intact cell model for supercritical fluid extraction: Its origin and limits', \emph{The Journal of Supercritical Fluids} \strong{129}, pp. 3-8, DOI: \url{https://doi.org/10.1016/j.supflu.2017.02.014}.
#'
#' @seealso \code{\link{bicmod}}, \code{\link{show_pars}}, \code{\link{moderr}}
#'
#' @importFrom stats setNames optimize
bic_ct <- function(yield, t, n, qaver, cu, N, Nm, ys, modpts = 100, thetaf0 = 1, ti0 = 30, maxt = round(max(t)*1.2,-1)) {

  #SIMPLIFIED MODEL BASED ON CHARACTERISTIC TIMES
  #Adjustable parameters of the model based on characteristic times:
  #G = initial fraction of extract in open cells
  #thetaf = external mass transfer resistance
  #ti = characteristic time of the internal mass transfer
  #eprime = yield at the end of the CER
  #tprime = time at the end of the CER
  m <- length(t)

  t1 <- t[2:n] #[min] time of the CER
  t2 <- t[n:m] #[min] time of the second extraction period
  qspec <- qaver/N*60 #[1/min] specific flow rate (CO2/( solid*s))

  #First extraction period (CER)
  by1 <- yield[2:n] #[g/g] experimental yield of the CER
  #by1 <- y1 * Nm / N #[g/g] experimental yield of the CER
  #unires1 <- suppressWarnings(pracma::newtonRaphson(cer_ct_err, x0 = thetaf0, maxiter = 5000, tol = 1e-10, t1 = t1, y1 = by1, ys = ys, qspec = qspec)[["root"]])
  #unires2 <- pracma::halley(cer_ct_err, x0 = thetaf0, maxiter = 5000, tol = 1e-10, t1 = t1, y1 = by1, ys = ys, qspec = qspec)[["root"]]
  unires <- optimize(cer_ct_err, interval = c(thetaf0/100, thetaf0*100), tol = 1e-10, t1 = t1, y1 = by1, ys = ys, qspec = qspec) #FIND A WAY TO INCORPOPATE INITIAL ESTIMATE!
  thetaf <- unires[["minimum"]]
  res_tm1 <- unires[["objective"]]

  #Second extraction period (FER)
  by2 <- yield[n:m] #[g/g] experimental yield of the FER
  #by2 <- y2 * Nm / N #[g/g] experimental yield of the FER
  zeta0 <- c(ti0, 0.1) #initial value of ti
  ink <- pracma::fminsearch(fer_ct_err, zeta0, maxiter = 1000, tol = 1e-10, t1 = t1, t2 = t2, y2 = by2, cu = cu) #[min] characteristic time of the FER
  ti <- ink[["xmin"]][1] #[min] Extraction time of the FER (i.e. solid mass transfer)
  G <- ink[["xmin"]][2] #Initial fraction of solute in open (broken) cells
  res_tm2 <- ink[["fmin"]]
  eprime <- G * cu #[g/g] yield at the end of the CER
  tprime <- eprime / (ys * qspec * (1 - exp(-1 / thetaf))) #[min] time at the end of the CER

  #Compile model curve data
  xdt_cer <- seq(0, tprime, length.out = modpts) #[min] time vector for the horizontal axis (CER)
  ydt_cer <- cer_ct_eq(thetaf, xdt_cer, ys, qspec) #[g/g] calculated yield (CER)
  xdt_fer <- seq(tprime + 0.1, maxt, length.out = modpts) #[min] calculated time for the horizontal axis (FER)
  ydt_fer <- fer_ct_eq(G, ti, xdt_fer, cu, tprime) #[g/g] calculated yield (FER)

  #Calculation of model predictions for input data points
  prind <- if(tprime > t[n]) c(n,n+1) else c(n-1,n)
  cer_pred <- cer_ct_eq(thetaf, t[1:prind[1]], ys, qspec) #[g/g] calculated yield of the CER
  fer_pred <- fer_ct_eq(G, ti, t[prind[2]:m], cu, tprime) #[g/g] calculated yield of the FER

  #Calculate errors of the SIMPLIFIED MODEL BASED ON CHARACTERISTIC TIMES
  err1 <- moderr(pred = cer_pred, act = yield[1:prind[1]])
  err2 <- moderr(pred = fer_pred, act = yield[prind[2]:m])
  resid <- setNames(c(sqrt(res_tm2^2 + res_tm1^2),
                      sqrt(err1["rmse"]*err2["rmse"]),
                      mean(c(err1["r2"], err2["r2"]), na.rm = TRUE)), c("aard", "rmse", "r2"))

  #Compile output data
  cer_dt <- cbind.data.frame(model = rep("ct",length(xdt_cer)), period = rep("cer",length(xdt_cer)), x = xdt_cer, y = ydt_cer)
  fer_dt <- cbind.data.frame(model = rep("ct",length(xdt_fer)), period = rep("fer", length(xdt_fer)), x = xdt_fer, y = ydt_fer)
  mod_dt <- rbind.data.frame(cer_dt, fer_dt)

  return(list(ordt = data.frame(x = t, y = yield, mod_y = c(cer_pred,fer_pred)), mdt = mod_dt,
              mod_pars = c(thetaf = thetaf, ti = ti, tprime = tprime, eprime = eprime, G = G), #qspec = qspec
              fit_pars = c("ti","G","thetaf"), resid = resid))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION J: Derive the COMPLETE MODEL with 3 or 2 extraction periods (Rizza, 2014)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Derive the complete BIC model
#'
#' @description Derives, summarises, and visualises the results of the complete Broken-and-Intact Cells (BIC) model of Sovova (2005).
#' Part of the \code{\link{bicmod}} workflow.
#'
#' @param yield A \code{numeric} vector of yields (g/g insoluble solid).
#' @param q A \code{numeric} vector of relative amounts of expended solvent (kg/kg insoluble solid).
#' @param n Period corresponding to the end of the CER (in \strong{number of experimental points}).
#' @param a0 Specific surface area per unit volume of extraction bed (1/m).
#' @param gam Solvent to matrix ratio in the bed (\eqn{kg_{solvent}/kg_{insoluble solid}}).
#' @param porosity Bed porosity (dimensionless).
#' @param qaver Average solvent flow rate (kg/s).
#' @param Nm Mass of insoluble material (g).
#' @param ys Extract solubility in CO2 (g/g).
#' @param xu Weight fraction (concentration) in the untreated solid.
#' @param r0 Initial estimate of the grinding efficiency (fraction of broken cells).
#' @param ksas0 Initial estimate of the product of the solid phase mass transfer coefficient (\eqn{k_s}; 1/s)
#' and the specific area between intact and broken cells (\eqn{a_s}; 1/m), in 1/m/s.
#' @param kf0 An initial estimate of the fluid phase mass transfer coefficient (1/s). Defaults to \code{0.001}.
#' @param qc0 Initial estimate of solvent expended at the end of the CER (kg/kg CO2). Defaults to \code{400}.
#' @param modpts A single \code{numeric} value specifying how many points should be modelled throughout the OEC curve range (\code{100} by default).
#' @param maxq Maximum x-axis value (solvent expended, kg/kg insoluble solid) to use for model predictions.
#' Defaults to 120% of the maximum \code{q} value.
#' @param nlsm A \code{character} vector of non-linear optimization methods to use for convergence of the model.
#' One or more of: \code{"MM"}, \code{"tau"}, \code{"CM"}, and/or \code{"mtl"} (\strong{see \code{\link[robustbase]{nlrob}} for details}).
#' @param aggreg A string specifying how the "best" results of non-linear optimization (\code{nlsm}) should be chosen.
#' One of \code{"aard"} (default; value with minimum Average Absolute Relative Deviation is chosen) or \code{"mean"}
#' (the arithmetic mean of all results is taken).
#' @param const_flow A \code{logical} specifying whether the flow rate is constant throughout the extraction.
#' If set to \code{TRUE}, the solvent-material ratio (S/M) is \strong{not} converted to extraction time (min).
#'
#' @return A named \code{list} containing the following elements:
#' \enumerate{
#' \item \strong{$ordt}: A \code{data.frame} containing the original input data including the temporal variable \code{$t},
#' the Solvent-Solute (S/M) ratio \code{$x}, the \strong{actual} fractional yield (\code{$y}; g/g insoluble solid), as well as yields
#' \strong{modeled} via the 3-period and 2-period models (\code{$y_cmp3} and \code{$y_cmp2}, respectively).
#' \item \strong{$mdt}: A \code{data.frame} of modeled data including the model type (\code{$model}),
#' the region of the extraction curve (\code{$period}), as well as S/M ratio \code{$x}, fractional yield \code{$y}, and time (\code{$t})
#' up to \code{maxq}.
#' \item \strong{$mod_pars}: The model parameters including the external mass transfer resistance (\eqn{\theta_e}; \code{"thetae"}),
#' the product of the fluid phase mass transfer coefficient \eqn{k_f} and specific surface area per unit volume of extraction bed \eqn{a_0}
#' (\eqn{k_fa_0}; \code{"kfa0"}), the relative amount of expended solvent (kg/kg insoluble solid) at the end of the CER (\eqn{q_m}; \code{"qm"}),
#' FER for the 3-period model (\eqn{q_n}; \code{"qn"}), and DC for the 2-period model (\eqn{q_{cmp}}; \code{"qs"}),
#' coefficient \eqn{\beta} (\code{"beta"}), initial fraction of solute in broken cells (\eqn{G}; \code{"G"}),
#' the fluid mass transfer coefficient (\eqn{k_f}; \code{"kf"}), grinding efficiency (\eqn{r}; \code{"r"}), as well as the product
#' of the solid phase mass transfer coefficient \eqn{k_s} and the specific area between intact and broken cells \eqn{a_s}
#' (\eqn{k_sa_s}; \code{"ksas"}). See \code{\link{show_pars}} for detailed description of these parameters.
#' \item \strong{$fit_pars}: A \code{character} vector of which parameters in \code{$mod_pars} were iteratively fit to the model.
#' \item \strong{$resid}: A named \code{numeric} vector of various error values for the model, including Average Absolute Relative Deviation
#' AARD (\code{"aard"}), round mean squared error RMSE (\code{"rmse"}), and the R2 value (\code{"r2"}).
#' }
#' @export
#'
#' @details
#' For a detailed description of this variant of the Broken and Intact Cells (BIC) model, refer to \code{\link{bicmod}}.
#'
#' @references
#' Rizza, C.S. (2014), \emph{Experiments and Modeling of Supercritical CO2 Extraction of Lipids from Microalgae}, MSc thesis, Universita Degli Studi Di Padova, Dipartimento Di Ingegneria Industriale.
#'
#' Sovova, H. (2005), 'Mathematical model for supercritical fluid extraction of natural products and extraction curve evaluation', \emph{The Journal of Supercritical Fluids} \strong{33} (1), pp. 35-52, DOI: \url{https://doi.org/10.1016/j.supflu.2004.03.005}.
#'
#' Sovova, H. (2017), 'Broken-and-intact cell model for supercritical fluid extraction: Its origin and limits', \emph{The Journal of Supercritical Fluids} \strong{129}, pp. 3-8, DOI: \url{https://doi.org/10.1016/j.supflu.2017.02.014}.
#'
#' @seealso \code{\link{bicmod}}, \code{\link{show_pars}}, \code{\link{moderr}}
#'
#' @importFrom stats coef setNames
bic_cmp <- function(yield, q, n, a0, gam, porosity, qaver, Nm, ys, xu, r0, ksas0, kf0 = 0.001, qc0 = 400,
                    modpts = 100, maxq = round(max(q)*1.2,-1), nlsm = c("MM","tau","CM","mtl"), aggreg = "aard", const_flow = TRUE) {

  #Preliminary checks
  modnms <- c("MM","tau","CM","mtl")
  if(any(nlsm %in% "all")) nlsm <- modnms
  if(!all(nlsm %in% modnms)) stop(paste0("At least one NLS fitting method not recognised! Possible values are: ", paste0("'", modnms, "'", collapse = ", "), "."))

  #COMPLETE MODEL
  #Adjustable parameters of the complete model:
  #kf = external mass transfer coefficient
  #r = grinding efficiency
  #ksas = internal MT coefficient multiplied by specific area between the regions of intact and broken cells
  #kf will be estimated using the equation of the CER,
  #r and ksas will be evaluated using the equations of the FER and DC periods.
  #r1 and ksas1, the results of the simplified model fitting, are used as initial values of the second fitting.
  #First extraction period
  m <- length(yield)
  q1 <- q[2:n]
  y1 <- yield[2:n]

  #APPROACHES FOR UNIVARIATE ROOT FINDING
  kf <- suppressWarnings(pracma::newtonRaphson(cer_cmp_err, x0 = kf0, maxiter = 5000, tol = 1e-08, q1 = q1, y1 = y1, a0 = a0, gam = gam, porosity = porosity, qaver = qaver, Nm = Nm, ys = ys)[["root"]])
  #kf2 <- pracma::halley(cer_cmp_err, x0 = iota0, maxiter = 5000, tol = 1e-08, q1 = q1, y1 = y1, a0 = a0, gam = gam, porosity = porosity, qaver = qaver, Nm = Nm, ys = ys)[["root"]]
  #kf <- mean(c(kf1,kf2), na.rm = TRUE) #[1/s] external MT coefficient
  kfa0 <- kf * a0

  #Second and third extraction period
  q2 <- q[n:m]
  y2 <- yield[n:m]
  csi <- c(r0, ksas0) #Initial values of r and ksas from the simplified model fitting

  set.seed(42)
  aardvec <- rep(NA,4)
  resmat <- as.data.frame(matrix(nrow = 4, ncol = 2, dimnames = list(c(), c("r","ksas"))))
  for(i in seq_along(nlsm)) {
    optres <- suppressWarnings(try(robustbase::nlrob(y2 ~ fer_cmp_eq(r, ksas, q2, kf, xu, ys, porosity, qaver, gam, Nm, a0),
                                                     #psi = .Mwgt.psi1("huber", cc = 1.345),
                                                     data = data.frame(y2 = y2, q2 = q2,
                                                                       kf = rep(kf, length(q2)),
                                                                       xu = rep(xu, length(q2)),
                                                                       ys = rep(ys, length(q2)),
                                                                       porosity = rep(porosity, length(q2)),
                                                                       qaver = rep(qaver, length(q2)),
                                                                       gam = rep(gam, length(q2)),
                                                                       Nm = rep(Nm, length(q2)),
                                                                       a0 = rep(a0, length(q2))),
                                                     lower = c(r = csi[1]/2, ksas = csi[2]/5),
                                                     upper = c(r = csi[1]*2, ksas = csi[2]*5), method = nlsm[i]), silent = TRUE))
    errchk <- inherits(optres, "try-error")
    if(!errchk) {
      resmat[i,] <- cfs <- coef(optres)

      #Computation of prediction errors
      r_opt <- resmat[i,"r"]
      ksas_opt <- resmat[i,"ksas"]
      optpred <- fer_cmp_eq(r_opt, ksas_opt, q2, kf, xu, ys, porosity, qaver, gam, Nm, a0)
      aardvec[i] <- moderr(pred = optpred, act = y2, m = length(y2))[["aard"]]
    }
  }

  if(aggreg=="aard") {
    min_aard <- which(aardvec == min(aardvec, na.rm = TRUE))
    r <- resmat[min_aard,"r"] #[-] grinding efficiency
    ksas <- resmat[min_aard,"ksas"] #[1/(s*m)] product ks*as
  } else if(aggreg == "mean") {
    r <- mean(resmat[,"r"], na.rm = TRUE) #[-] grinding efficiency
    ksas <- mean(resmat[,"ksas"], na.rm = TRUE) #[1/(s*m)] product ks*as
  }

  cmperr <- fer_cmp_eq(r, ksas, q2, kf, xu, ys, porosity, qaver, gam, Nm, a0, out = "all", x1 = q1, x2 = q1)
  qm <- cmperr[["pars"]][["qm"]] #q at the end of the CER
  thetai <- cmperr[["pars"]][["thetai"]] #[-] internal mass transfer resistance
  thetae <- (porosity * qaver) / (gam * kf * a0 * Nm) #[-] external mass transfer resistance
  betam <- cmperr[["pars"]][["betam"]] #Coefficient
  qn <- cmperr[["pars"]][["qn"]] #q at the end of the FER

  #Computation of prediction errors
  err1 <- moderr(pred = cmperr[["y1"]], act = y1, m = length(y1)) #[-] yield of the CER
  err2 <- moderr(pred = cmperr[["y2"]], act = y1, m = length(y1)) #[-] yield of the FER
  err3 <- moderr(pred = cmperr[["y3"]], act = y2, m = length(y2)) #[-] yield of the DC
  resid <- setNames(c(sqrt(err1["aard"]^2 + err3["aard"]^2),
                      sqrt(err1["rmse"]*err3["rmse"]), #NOT SURE IF VALID
                      mean(c(err1["r2"], err3["r2"]), na.rm = TRUE)), c("aard", "rmse", "r2"))

  #Compile plot data for 3-period complete model
  #Creation of the vector of the horizontal axis
  xdt_cer <- seq(0, qm, length.out = modpts) #[kg/kg] q of the CER
  xdt_fer <- seq(qm + 0.1, qn, length.out = modpts) #[kg/kg] q of the FER
  xdt_dc <- seq(qn + 0.1, maxq, length.out = modpts) #[kg/kg] q of the DC

  #Computation of the yield at each q of the horizontal axis
  cmpres <- fer_cmp_eq(r, ksas, xdt_dc, kf, xu, ys, porosity, qaver, gam, Nm, a0, out = "all", x1 = xdt_cer, x2 = xdt_fer)
  ydt_cer <- cmpres[["y1"]] #[g/g] yield of the CER
  ydt_fer <- cmpres[["y2"]] #[g/g] yield of the FER
  ydt_dc <- cmpres[["y3"]] #[g/g] yield of the DC

  #Complete model with 2 equations (only CER and DC periods considered)
  qs <- pracma::newtonRaphson(solv_cmp, x0 = qc0, maxiter = 5000, tol = 1e-08, ys = ys, thetae = thetae, xu = xu, betam = betam, r = r, qm = qm, gam = gam, thetai = thetai)[["root"]]
  #qs <- uniroot(solv_cmp, interval = c(0, maxq), tol = 1e-8, maxiter = 1000, ys = ys, thetae = thetae, xu = xu, betam = betam, r = r, qm = qm, gam = gam, thetai = thetai)[["root"]]

  #Calculation of G with the COMPLETE MODEL
  G <- (qs * ys * (1 - exp(-1 / thetae))) / xu

  #Compile plot data for 2-period complete model
  xdt_cer2 <- seq(0, qs, length.out = modpts)
  xdt_dc2 <- seq(qs, maxq, length.out = modpts)
  cmpres2 <- fer_cmp_eq(r, ksas, xdt_dc2, kf, xu, ys, porosity, qaver, gam, Nm, a0, out = "all", x1 = xdt_cer2, x2 = NA)
  ydt_cer2 <- cmpres2[["y1"]]
  ydt_dc2 <- cmpres2[["y3"]]

  #Calculation of model predictions for input data points (3-period model)
  cmpred <- fer_cmp_eq(r, ksas, q[q>=qn], kf, xu, ys, porosity, qaver, gam, Nm, a0, out = "all", x1 = q[q<qm], x2 = q[q>=qm & q<qn])
  pred_cer1 <- cmpred[["y1"]]
  pred_fer1 <- cmpred[["y2"]]
  pred_dc1 <- cmpred[["y3"]]

  #Ditto for 2-period model
  prind <- if(qs > q[n]) c(n,n+1) else c(n-1,n)
  cmpred2 <- fer_cmp_eq(r, ksas, q[prind[2]:m], kf, xu, ys, porosity, qaver, gam, Nm, a0, out = "all", x1 = q[1:prind[1]], x2 = NA)
  pred_cer2 <- cmpred2[["y1"]]
  pred_dc2 <- cmpred2[["y3"]]

  #Compile output data
  cer_dt_cmp3 <- cbind.data.frame(model = rep("cmp3", length(xdt_cer)), period = rep("cer",length(xdt_cer)), x = xdt_cer, y = ydt_cer)
  fer_dt_cmp3 <- cbind.data.frame(model = rep("cmp3", length(xdt_fer)), period = rep("fer", length(xdt_fer)), x = xdt_fer, y = ydt_fer)
  dc_dt_cmp3 <- cbind.data.frame(model = rep("cmp3", length(xdt_dc)), period = rep("dc", length(xdt_dc)), x = xdt_dc, y = ydt_dc)
  cer_dt_cmp2 <- cbind.data.frame(model = rep("cmp2", length(xdt_cer2)), period = rep("cer",length(xdt_cer2)), x = xdt_cer2, y = ydt_cer2)
  fer_dt_cmp2 <- cbind.data.frame(model = rep("cmp2", length(xdt_dc2)), period = rep("dc", length(xdt_dc2)), x = xdt_dc2, y = ydt_dc2)
  mod_dt <- do.call(rbind.data.frame, list(cer_dt_cmp3, fer_dt_cmp3, dc_dt_cmp3, cer_dt_cmp2, fer_dt_cmp2))
  colnames(mod_dt) <- colnames(cer_dt_cmp3)

  if(const_flow){
    #Add time equivalent (for plotting)
    mod_dt[,"t"] <- mod_dt[,"x"]*Nm/qaver/60 #Extraction time (min)

    #Also derive time for original q data
    ort <- q*Nm/qaver/60

    ordt <- data.frame(t = ort, x = q, y = yield, y_cmp3 = c(pred_cer1, pred_fer1, pred_dc1), y_cmp2 = c(pred_cer2, pred_dc2))
  } else ordt <- data.frame(x = q, y = yield, y_cmp3 = c(pred_cer1, pred_fer1, pred_dc1), y_cmp2 = c(pred_cer2, pred_dc2))

  return(list(ordt = ordt,
              mdt = mod_dt,
              mod_pars = c(thetae = thetae, kfa0 = kfa0, qm = qm, qn = qn, qs = qs, beta = betam, G = G, kf = kf, r = r, ksas = ksas),
              fit_pars = c("kf","r","ksas"),
              resid = resid))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION K: Create kinetic model plots
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Create kinetic model plots
#'
#' @description Creates a plot of BIC or other kinetic model showing extraction curve points and up to three regions of the
#' modeled curve including the Constant Extraction Rate (CER), Falling Extraction Rate (FER), and the Diffusion-Controlled (DC) regions.
#' Part of the \code{\link{bicmod}} workflow.
#'
#' @param pts A \code{data.frame} of original Overall Extraction Curve (OEC) points (including both temporal and signal variables).
#' @param mod A \code{data.frame} of the model curve to plot alongside \code{pts}.
#' @param ptvars,modvars A \code{character} vector of length 2 specifying column names of the temporal and signal variables
#' in \code{pts} and \code{mod}, respectively. If \code{modvars} is not provided, it is set to be equal to \code{ptvars}.
#' @param grp An \strong{optional} named \code{character} vector of length 2 specifying column names with grouping variables
#' for \strong{model type} (\code{"mod"}) and \strong{OEC region} (\code{"reg"}).
#' @param cols Either \code{"default"} or a named \code{character} vector of plot colours for points (\code{"id"}),
#' model curve without sub-division into regions (\code{"mod"}), as well as with sub-division into CER (\code{"cer"}),
#' FER (\code{"fer"}), and DC (\code{"dc"}) regions of the extraction curve.
#' @param pltlabs An \strong{optional} named \code{character} vector of labels for the temporal (\code{"x"}) and
#' signal (\code{"y"}) variables to display in plots. Defaults to \code{ptvars}.
#' @param draw A \code{logical} switch specifying whether the generated plot(s) should be printed? Defaults to \code{FALSE}.
#'
#' @return A \code{list} of one or more plots of class \code{"ggplot"}.
#' @export
#'
#' @seealso \code{\link{bicmod}}, \code{\link{ktsmod}}, \code{\link{show_pars}}
#'
#' @import ggplot2
#'
kin_plot <- function(pts, mod, ptvars = c("x","y"), modvars = NA, grp = c(mod = "model", reg = "period"),
                     cols = "default", pltlabs = c(x = ptvars[1], y = ptvars[2]), draw = FALSE) {

  #Preliminary checks
  if(any(sapply(list(pts,mod), function(x) !is.data.frame(x)))) stop("Input data 'pts' and 'mod' must both be data frames!")
  if(any(is.na(modvars)) & is.character(ptvars)) modvars <- ptvars
  if(!all(ptvars %in% colnames(pts))|!all(modvars %in% colnames(mod))) stop("At least some of the column names provided in 'ptvars' and/or 'modvars' were not recognised!")
  if(length(ptvars)!=2|length(modvars)!=2) stop("Where provided, 'ptvars' and 'modvars' must be character vectors of length 2!")
  if(!all(names(grp) %in% c("mod","reg"))) stop("Argument 'grp' must be a named vector of grouping variables for model ('mod') and region of the extraction curve ('reg')!")
  if(!is.na(grp["reg"])) {
    if(!all(mod[,grp["reg"]] %in% c("cer","fer","dc"))) stop("When an extraction curve region grouping is provided, currently the only supported group names are: 'cer', 'fer', and 'dc'!")
  }
  if(!all(is.na(pltlabs)) & !all(names(pltlabs) %in% c("title","x","y"))) stop("The vector of custom plot labels 'pltlabs' must include one or more of the following names: 'title', 'x', or 'y'!")

  #Set up plot colours
  defcols <- c(id = "black", mod = "navy", cer = "red", fer = "blue", dc = "darkgreen")
  cols <- supp_pars(pars = cols, defpars = defcols, parlb = "cols")

  #Create plots
  if(!is.na(grp["mod"])) {
    mod <- split(mod, f = mod[,grp["mod"]])
  } else {
    mod <- list(mod)
    names(mod) <- "model"
  }

  #Make base plot of experimental data
  baseplt <- ggplot2::ggplot(data = pts, aes(x = .data[[ptvars[1]]], y = .data[[ptvars[2]]])) +
    geom_point(pch = 16, colour = cols["id"]) +
    scale_x_continuous(breaks = scales::breaks_pretty(n = 6)) +
    scale_y_continuous(breaks = scales::breaks_pretty(n = 6)) +
    theme(aspect.ratio = 1,
          panel.background = element_blank(),
          axis.text = element_text(colour = "black", size = 12),
          axis.title = element_text(colour = "black", size = 13),
          axis.line = element_line(colour = "black"),
          legend.title = element_blank(),
          #legend.key = element_rect(fill = "grey90", colour = "black"),
          legend.position.inside = c(0.93, 0.15),
          legend.text = element_text(size = 11))

  #Create output plot list
  pltlst <- list()
  for(i in seq_along(mod)) {
    #Add model curve (optionally grouped by extraction period)
    if(is.na(grp["reg"])) {
      curplt <- baseplt + geom_path(data = mod[[i]], colour = cols["mod"])
    } else {
      curplt <- baseplt + geom_path(data = mod[[i]], aes(colour = .data[[grp["reg"]]])) +
        scale_colour_manual(values=cols[c("cer","fer","dc")],
                            labels = c("cer" = "CER", "fer" = "FER", "dc" = "DC"),
                            breaks = c("cer","fer","dc"))
    }

    #Add  plot labels
    if(!all(is.na(pltlabs))) curplt <- curplt + labs(title = if("title" %in% names(pltlabs)) pltlabs["title"] else waiver(),
                                                     x = if("x" %in% names(pltlabs)) pltlabs["x"] else waiver(),
                                                     y = if("y" %in% names(pltlabs)) pltlabs["y"] else waiver())
    pltlst[[names(mod)[i]]] <- curplt
  }

  #Optionally draw the plots
  if(draw) print(pltlst)
  return(pltlst)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Predict response for BIC model (Sovova)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Predict response using built BIC models
#'
#' @description Predicts responses from new temporal data using BIC (Broken-and-Intact Cells) models
#' built via \code{\link{bicmod}}.
#'
#' @param input A BIC model output from \code{\link{bicmod}}.
#' @param newdata A \code{numeric} value or vector of new data (extraction time or solvent-to-insoluble solid ratio)
#' to make predictions for.
#' @param units Specifies the units of the input data (\code{newdata}). One of \code{"time"} (extraction time in minutes)
#' or the default \code{"sm"} (solvent-to-insoluble solid material ratio, dimensionless).
#' @param get_yields A \code{logical} specifying whether mass and percentage yields should be calculated from the default
#' prediction format (g/g insoluble solid, i.e. \strong{fractional yield}).
#'
#' @return A \code{list} of length 3 containing the model \code{$predictions}, a helpful \code{$unit_chart} showing the units
#' pertaining to the predictions, and a \code{$description} providing some additional details about the predictions (such as the
#' process conditions which they are valid for).
#'
#' @export
#'
#' @seealso \code{\link{bicmod}}
predict_bic <- function(input, newdata, units = "sm", get_yields = TRUE) {

  #Preliminary checks
  modtypes <- c("sim", "ct", "cmp")
  modnms <- setNames(c("Simple", "Characteristic Times", "Complete"), modtypes)
  if(!all(c("plots", "data", "input", "call") %in% names(input)) | !any(modtypes %in% names(input))) stop("The 'input' data must originate from function 'bicmod'!")
  if(!any(c("sm","time") %in% units)) stop("The 'units' of 'newdata' must be one of 'time' or 'sm' (ratio of solvent to insoluble solid)!")

  #Retrieve input parameters
  inpr <- input[["input"]]

  #Begin processing
  finres <- preds <- list()
  for(i in which(names(input) %in% modtypes)) {
    #Get necessary data from current model
    curmod <- input[[i]]
    curnm <- names(input)[i]

    cat("\nWorking on model type: '", curnm, "' (", modnms[curnm], ")...", sep = "")

    #Determine whether time data is available
    timecol <- if(curnm=="ct") "x" else "t"
    time_chk <- any(colnames(curmod[["ordt"]]) %in% timecol)
    if(!time_chk & units == "time") {
      cat("\nNo predictions were made for this model type since no extraction time data is available! Skipping...")
      next
    } else {
      #Convert units where necessary
      data_sm <- if(units == "time") 60*newdata*inpr["flow"]/inpr["Nm"] else if(units == "sm") newdata
      data_time <- if(units == "time") newdata else if(units == "sm") newdata*inpr["Nm"]/inpr["flow"]/60
    }

    #Get modeled parameters
    modpr <- curmod[["mod_pars"]]

    #Get initial predictions
    if(curnm == "sim") {
      preds[[curnm]] <- unname(sapply(data_sm, function(x) if(modpr["qm"] < x) fer_sim_eq(modpr["c1"], modpr["c2"], x, inpr["xu"]) else x * inpr["ys"]))
    } else if(curnm == "ct") {
      if(units == "sm") {
        cat("\nNo predictions were made for the characteristic times model since input data is a solvent-material ratio (check 'units').")
        preds[[curnm]] <- rep(NA, length(data_time))
      } else {
        qspec <- inpr["flow"]/(inpr["Ng"]/1000)*60
        preds[[curnm]] <- unname(sapply(data_time, function(x) if(modpr["tprime"] < x) fer_ct_eq(modpr["G"], modpr["ti"], x, inpr["cu"], modpr["tprime"]) else cer_ct_eq(modpr["thetaf"], x, inpr["ys"], qspec)))
      }
    } else if(curnm == "cmp") {
      #Three-period model
      preds_cmp3 <- fer_cmp_eq(modpr["r"], modpr["ksas"],
                               data_sm[data_sm >= modpr["qn"]], modpr["kf"], inpr["xu"], inpr["ys"],
                               inpr["porosity"], inpr["flow"], inpr["gamma"], inpr["Nm"], inpr["a0"], out = "all",
                               x1 = data_sm[data_sm < modpr["qm"]], x2 = data_sm[data_sm >= modpr["qm"] & data_sm < modpr["qn"]])

      preds[[paste0(curnm,"3")]] <- unname(sort(unlist(preds_cmp3[!names(preds_cmp3) %in% "pars"])))

      #Ditto for 2-period model
      preds_cmp2 <- fer_cmp_eq(modpr["r"], modpr["ksas"],
                               data_sm[data_sm > modpr["qs"]], modpr["kf"], inpr["xu"], inpr["ys"],
                               inpr["porosity"], inpr["flow"], inpr["gamma"], inpr["Nm"], inpr["a0"], out = "all",
                               x1 = data_sm[data_sm <= modpr["qs"]], x2 = NA)
      preds[[paste0(curnm, "2")]] <- unname(sort(unlist(preds_cmp2[!names(preds_cmp2) %in% c("pars","y2")])))
    }
    curpreds <- preds[grepl(curnm, names(preds))]

    #Convert q (kg/kg insoluble solid) to S/M (kg/kg total mass loaded)
    sm <- inpr["flow"]*1000*60*data_time/inpr["mass_in"]

    #Compile results into a data.frame
    nmlist <- if(!grepl("cmp", curnm)) curnm else c("cmp2", "cmp3")
    for(j in nmlist) {
      finres[[j]] <- data.frame(t = data_time, q = data_sm, sm = sm, yield = preds[[j]])
      #Optionally decode responses, converting yield into mass AND percentage yields
      if(get_yields) {
        if(j==nmlist[1]) cat("\nConverting fractional yield into mass and percentage yield...")
        yield_mass <- (if(j=="ct") preds[[j]]*(inpr["Ng"]/1000)/inpr["Nm"] else preds[[j]])*(inpr["Nm"]*1000) #[g] Absolute yield
        yield_percent <- yield_mass/inpr["mass_in"]*(1-inpr["moisture"]/100)*100 #[% dry weight] Relative yield
        finres[[j]] <- cbind.data.frame(finres[[j]], yield_mass = yield_mass, yield_percent = yield_percent)
      }
    }
  }

  #Create a chart of units for reference
  modnum <- length(modtypes)
  unit_chart <- data.frame(model_type = modtypes,
                           t = rep("min", modnum),
                           q = rep("kg/kg insoluble solid", modnum),
                           sm = rep("dimensionless (S/M ratio)", modnum),
                           yield = c("g/g insoluble solid", "g/g total solid", "g/g insoluble solid"),
                           yield_mass = rep("g", modnum),
                           yield_percent = rep("percent dry weight", modnum))

  unit_chart <- unit_chart[unit_chart[,"model_type"] %in% modtypes, colnames(unit_chart) %in% colnames(finres[[1]])]

  #Compile statement about parameters for which the model is valid
  parnms <- c(pres = "Pressure", temp = "Temperature", flow = "Flow rate", cu = "Extractable fraction",
              etoh = "Co-solvent concentration", D = "Extractor diameter", L = "Extractor height")
  parunits <- c(pres = "bar", temp = "C", flow = "g/min", cu = "", etoh = "percent CO2 flow",
                D = "m", L = "m")
  whichnms <- which(names(inpr) %in% names(parnms))
  statepars <- inpr[names(inpr)[whichnms]]
  finfilter <- match(names(parnms), names(statepars))
  if("flow" %in% names(statepars)) statepars["flow"] <- round(statepars["flow"]*1000*60, 2)

  statement <- paste0("\nPredictions are valid for the following process parameters:",
                      paste0("\n", parnms[finfilter], " of ", statepars[names(parnms[finfilter])],
                             " ", parunits[finfilter], ".", collapse = ""))
  statement <- append(statement, paste0("\nRefer to the unit chart for units of input and predicted data."))

  return(list(predictions = finres, unit_chart = unit_chart, description = statement))
}
