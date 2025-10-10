#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Calculate the residual part of the IAPWS-95 formulation of water properties
#Modified from function 'IAPWS95.residual' in package 'CHNOSZ'
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @noRd
h2o_resid <- function(delta, tau) {
  #The residual part in the IAPWS-95 formulation
  #From Table 6.2 of Wagner and Pruss, 2002
  c <- c(rep(NA,7),rep(1,15),rep(2,20),rep(3,4),4,rep(6,4),rep(NA,5))
  d <- c(1,1,1,2,2,3,4,1,1,1,2,2,3,4,
         4,5,7,9,10,11,13,15,1,2,2,2,3,4,
         4,4,5,6,6,7,9,9,9,9,9,10,10,12,
         3,4,4,5,14,3,6,6,6,3,3,3,NA,NA)
  t <- c(-0.5,0.875,1,0.5,0.75,0.375,1,4,6,12,1,5,4,2,
         13,9,3,4,11,4,13,1,7,1,9,10,10,3,
         7,10,10,6,10,10,1,2,3,4,8,6,9,8,
         16,22,23,23,10,50,44,46,50,0,1,4,NA,NA)
  n <- c( 0.12533547935523E-1, 0.78957634722828E1 ,-0.87803203303561E1 ,
          0.31802509345418   ,-0.26145533859358   ,-0.78199751687981E-2,
          0.88089493102134E-2,-0.66856572307965   , 0.20433810950965   ,
          -0.66212605039687E-4,-0.19232721156002   ,-0.25709043003438   ,
          0.16074868486251   ,-0.40092828925807E-1, 0.39343422603254E-6,
          -0.75941377088144E-5, 0.56250979351888E-3,-0.15608652257135E-4,
          0.11537996422951E-8, 0.36582165144204E-6,-0.13251180074668E-11,
          -0.62639586912454E-9,-0.10793600908932   , 0.17611491008752E-1,
          0.22132295167546   ,-0.40247669763528   , 0.58083399985759   ,
          0.49969146990806E-2,-0.31358700712549E-1,-0.74315929710341   ,
          0.47807329915480   , 0.20527940895948E-1,-0.13636435110343   ,
          0.14180634400617E-1, 0.83326504880713E-2,-0.29052336009585E-1,
          0.38615085574206E-1,-0.20393486513704E-1,-0.16554050063734E-2,
          0.19955571979541E-2, 0.15870308324157E-3,-0.16388568342530E-4,
          0.43613615723811E-1, 0.34994005463765E-1,-0.76788197844621E-1,
          0.22446277332006E-1,-0.62689710414685E-4,-0.55711118565645E-9,
          -0.19905718354408   , 0.31777497330738   ,-0.11841182425981   ,
          -0.31306260323435E2 , 0.31546140237781E2 ,-0.25213154341695E4 ,
          -0.14874640856724   , 0.31806110878444)
  alpha <- c(rep(NA,51),20,20,20,NA,NA)
  beta <- c(rep(NA,51),150,150,250,0.3,0.3)
  gamma <- c(rep(NA,51),1.21,1.21,1.25,NA,NA)
  epsilon <- c(rep(NA,51),1,1,1,NA,NA)
  a <- c(rep(NA,54),3.5,3.5)
  b <- c(rep(NA,54),0.85,0.95)
  B <- c(rep(NA,54),0.2,0.2)
  C <- c(rep(NA,54),28,32)
  D <- c(rep(NA,54),700,800)
  A <- c(rep(NA,54),0.32,0.32)

  #From Table 6.5
  i1 <- 1:7
  i2 <- 8:51
  i3 <- 52:54
  i4 <- 55:56

  #Deriviatives of distance function
  ldelta <- function(i) { theta(i)^2 + B[i] * ((delta-1)^2)^a[i] }
  theta <- function(i) { (1-tau) + A[i] * ((delta-1)^2)^(1/(2*beta[i])) }
  psi <- function(i) { exp ( -C[i]*(delta-1)^2 - D[i]*(tau-1)^2 ) }
  ddelta_ddelta <- function(i) { (delta-1) * ( A[i]*theta(i)*2/beta[i]*((delta-1)^2)^(1/(2*beta[i])-1) + 2*B[i]*a[i]*((delta-1)^2)^(a[i]-1) ) }
  ddelta_bi_ddelta <- function(i) { b[i]*ldelta(i)^(b[i]-1)*ddelta_ddelta(i) }

  #Derivatives of exponential function
  dpsi_ddelta <- function(i) { -2*C[i]*(delta-1)*psi(i) }

  # Dimensionless Helmholtz free energy and derivatives
  res <- sum(n[i1]*d[i1]*delta^(d[i1]-1)*tau^t[i1]) +
    sum(n[i2]*exp(-delta^c[i2])*(delta^(d[i2]-1)*tau^t[i2]*(d[i2]-c[i2]*delta^c[i2]))) +
    sum(n[i3]*delta^d[i3]*tau^t[i3] *
          exp( -alpha[i3]*(delta-epsilon[i3])^2 - beta[i3]*(tau-gamma[i3])^2) *
          (d[i3]/delta - 2 * alpha[i3]*(delta-epsilon[i3]))) +
    sum(n[i4] * (ldelta(i4)^b[i4] * (psi(i4)+delta*dpsi_ddelta(i4)) + ddelta_bi_ddelta(i4)*delta*psi(i4)))

  return(res)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Member of auxiliary functions used to calculate water properties
#Modified from function 'IAPWS95' in package 'CHNOSZ'
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @noRd
h2o_aux <- function (temp = 298.15, rho = 1000)  {

  temp_crit <- 647.096
  rho_crit <- 322
  R <- 0.46151805
  delta <- rho/rho_crit
  tau <- temp_crit/temp

  res <- (1 + delta * h2o_resid(delta, tau)) * rho * R * temp/1000
  return(res)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Member of auxiliary function used to calculate water properties
#Modified from function 'WP02.auxiliary' in package 'CHNOZS'
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @noRd
h2o_prop <- function (property = "rho_lq", temp = 298.15) {

  #Preliminary checks
  if(length(property)!=1) stop("Only one property may be calculated at a time!")
  if(!any(c("rho_lq", "rho_vap", "psat") %in% property)) stop(paste0("Property not recognised: ", property,"!"))

  #Critical properties of water
  temp_crit <- 647.096 #Kelvin
  pres_crit <- 22.064 #In MPa
  rho_crit <- 322 #In g/L

  V <- 1 - temp/temp_crit

  if (property == "psat") {
    a1 <- -7.85951783
    a2 <- 1.84408259
    a3 <- -11.7866497
    a4 <- 22.6807411
    a5 <- -15.9618719
    a6 <- 1.80122502
    ln_psigma_pcrit <- temp_crit/temp * (a1 * V + a2 * V^1.5 + a3 * V^3 + a4 * V^3.5 + a5 * V^4 + a6 * V^7.5)
    out <- pres_crit * exp(ln_psigma_pcrit)

  } else if (property == "rho_lq") {
    b1 <- 1.99274064
    b2 <- 1.09965342
    b3 <- -0.510839303
    b4 <- -1.75493479
    b5 <- -45.5170352
    b6 <- -674694.45
    out <- rho_crit * (1 + b1 * V^(1/3) + b2 * V^(2/3) + b3 * V^(5/3) + b4 * V^(16/3) + b5 * V^(43/3) + b6 * V^(110/3))

  } else if (property == "rho_vap") {
    c1 <- -2.0315024
    c2 <- -2.6830294
    c3 <- -5.38626492
    c4 <- -17.2991605
    c5 <- -44.7586581
    c6 <- -63.9201063
    out <- rho_crit * exp(c1 * V^(2/6) + c2 * V^(4/6) + c3 * V^(8/6) + c4 * V^(18/6) + c5 * V^(37/6) + c6 * V^(71/6))
  }
  return(out)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Calculation of water density in the subcritical region
#Modified from function 'rho.IAPWS95' in package 'CHNOSZ'
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Calculate water density in the subcritical region
#'
#' @description Calculates the density of pure water in the subcritical region (<374 degC, <221 bar)
#' via the International Association for the Properties of Water and Steam formulation IAPWS-95 (Wagner & Pruß, 2002).
#'
#' @param temp Water temperature in degrees Celsius. Defaults to \code{25}.
#' @param pres Water pressure in bar. Defaults to \code{1}.
#'
#' @return The water density (in g/L) as a single \code{numeric} value.
#' @export
#'
#' @references
#' Wagner, W., Pruß, A. (2002), 'The IAPWS Formulation 1995 for the Thermodynamic Properties of Ordinary Water Substance for General and Scientific Use', \emph{J. Phys. Chem. Ref. Data} \strong{31}, pp. 387-535, DOI: \url{https://doi.org/10.1063/1.1461829}.
#'
#' @examples
#' h2o_dens(200, 40)
#'
#' @importFrom stats uniroot
#'
h2o_dens <- function (temp = 25, pres = 1) {

  dP <- function(rho, temp, pres_mpa) h2o_aux(rho = rho, temp = temp) - pres_mpa
  temp <- temp + 273.15
  pres_mpa <- pres/10
  temp_crit <- 647.096
  pres_crit <- 22.064
  pres_sat <- h2o_prop("psat", temp)
  if(pres_mpa <= 0.9999 * pres_sat) stop("Set pressure 'pres' is below the saturation pressure at temperature 'temp'!")
  if(temp > temp_crit|pres_mpa > pres_crit) stop("Calculation currently only implemented for subcritical water region (<374 degC, <220 bar)!")
  if(temp <= 273.15|pres_mpa <= 0.1) stop("Temperature and pressure cannot be equal to or lower than 0 degC and 1 bar, respectively!")

  #Begin processing
  rho0 <- h2o_prop("rho_lq", temp)
  interval <- c(rho0 * 0.95, rho0 * 1.05)
  pres_init <- c()
  for(i in seq_along(interval)) pres_init[i] <- h2o_aux(rho = interval[i], temp = temp)
  extendint <- if(all(pres_init < pres_mpa)) "downX" else if(all(pres_init > pres_mpa)) "upX" else "yes"

  #Calculate density
  rho <- try(uniroot(dP, interval, extendInt = extendint, temp = temp, pres_mpa = pres_mpa)[["root"]], silent = TRUE)
  if(!is.numeric(rho)) stop("Failed to calculate density of water at given temperature and pressure!") else return(rho)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Two-site kinetic desorption model equations
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Equation: Two-site kinetic desorption model
#'
#' @description Derives two-site kinetic desorption (TWS) model predictions. Part of the \code{\link{ktsmod}} workflow.
#'
#' @param t A \code{numeric} vector of extraction time (usually in minutes) at which yield or another response was recorded.
#' @param k1,k2 First-order rate constants \eqn{k_1} and \eqn{k_2} describing the "fast" and "slow" component
#' of target compound desorption, respectively.
#' @param f The fraction of total extractable analyte that desorbs at a faster rate (i.e. is easily accessible for the solvent).
#'
#' @return A \code{numeric} vector of model predictions.
#' @export
#' @keywords internal
#'
#' @seealso \code{\link{tws_cmp}}, \code{\link{ktsmod}}
tws_eq <- function(t, k1, k2, f) {
  res <- 1-f*exp(-k1*t)-(1-f)*exp(-k2*t)
  return(res)
}

#' @title RMSE: Two-site kinetic desorption model
#'
#' @description Calculate the Round Mean Squared Error (RMSE) of the two-site kinetic desorption (TWS) model.
#' Part of the \code{\link{ktsmod}} workflow. \strong{This function manual is hidden from the Index}.
#'
#' @param pars Initial estimates of first-order rate constants \eqn{k_1} and \eqn{k_2} as well as an \strong{optional}
#' estimate for fraction \eqn{F} of easily-desorbed extract.
#' @param tm Vector of extraction time. Must be equal in length to the response vector \code{input}.
#' @param input Vector of \strong{actual} response values. Must be equal in length to \code{tm}.
#' @param f An explicit value of \eqn{F}. If provided (\code{NA} by default), any estimate included in \code{pars} is ignored.
#'
#' @return The RMSE as a single \code{numeric} value \eqn{RMSE = \sum{\sqrt{(x_{model}-x_{observed})^2}/m}}.
#'
#' @export
#' @keywords internal
#'
#' @seealso \code{\link{tws_eq}}, \code{\link{tws_cmp}}, \code{\link{ktsmod}}
tws_err <- function(pars, input, tm, f = NA) { #t, k1, k2, f
  res <- if(is.na(f)) 1-pars[3]*exp(-pars[1]*tm)-(1-pars[3])*exp(-pars[2]*tm) else 1-f*exp(-pars[1]*tm)-(1-f)*exp(-pars[2]*tm)
  err <- sum(sqrt((res-input)^2)/length(res))
  return(err)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Calculate model errors (AARD, RMSE, R2)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Calculate kinetic model errors (AARD, RMSE, R2)
#'
#' @description Calculates the Average Absolute Relative Deviation (AARD), Round-Mean Squared Error (RMSE), and the R2 (R-Squared) value
#' of any model given predicted and actual values alongside the number of recorded observations (data points).
#'
#' @param act,pred Both \code{numeric} vectors of actual and model-predicted values \strong{of equal length}.
#' @param m \strong{Optional} number of data points (by default, equals to the length of \code{act}).
#'
#' @return A named \code{numeric} vector containing the calculated AARD (\code{"aard"}), RMSE (\code{"rmse"}), and R2 (\code{"r2"}) values.
#' @export
#'
#' @details
#' The Average Absolute Relative Deviation (\eqn{AARD}) is defined as:
#' \deqn{AARD = 100 \times \sum{\vert (x_{pred} - x_{act})/x_{act}\vert}/m}
#'
#' The RMSE and R2 value are defined by:
#' \deqn{RMSE = \sum{\sqrt{(x_{pred}-x_{act})^2}/m}}
#' \deqn{R2 = 1 - [\sum{(x_{act} - x_{pred})^2}]/[\sum{(x_{act} - \bar{x}_{act})^2}]}
#'
#' In all the above equations, \eqn{m} is the number of observations, while \eqn{x_{pred}} and \eqn{x_{act}} are predicted
#' and actual response values, respectively. An overline denotes the mean value.
#'
#' @seealso \code{\link{bicmod}}, \code{\link{ktsmod}}, \code{\link{bic_sm}}, \code{\link{bic_ct}}, \code{\link{bic_cmp}}
moderr <- function(act, pred, m = length(act)) {
  #Preliminary checks
  if(length(act)!=length(pred)) stop("Input data must be of equal length!")
  if(length(m)!=1|!is.numeric(act)) stop("Argument 'm' must be the number of experimental points!")

  #Get error metrics
  aard <- 100 * sum(abs((pred - act) / act)) / m
  rmse <- sum(sqrt((pred-act)^2) / m)
  r2 <- if(m>1) 1 - (sum((pred-act)^2)/sum((pred-mean(act))^2)) else NA

  return(c(aard = aard, rmse = rmse, r2 = r2))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Build two-site kinetic model
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Build two-site kinetic desorption model
#'
#' @description Generates and summarizes a two-site kinetic desorption (TWS) model. Part of the \code{\link{ktsmod}} workflow.
#'
#' @param x,y Both \code{numeric} vectors of time (\eqn{t}; min) and response (\eqn{e_t}; e.g. yield), respectively. Must be of equal length.
#' Missing data is not allowed.
#' @param c0 The maximum possible yield \eqn{e_0}. Must have \strong{units identical to those of \code{y}}.
#' @param q An \strong{optional} \code{numeric} vector of Solvent-Material (S/M) ratio \eqn{q} to be included in the final output.
#' @param f An \strong{explicit} value of the fraction of easily-desorbed extract \eqn{F} (between 0 and 1).
#' When provided (\code{NA} by default), iterative curve fitting of this parameter is not carried out (\code{est_f} is ignored).
#' @param k1_0,k2_0 \strong{Mandatory} initial estimates for first-order rate constants \eqn{k_1} and \eqn{k_2}.
#' @param f0 \strong{Optional} initial estimate for \eqn{F}.
#' @param est_f A \code{logical} indicating whether \eqn{F} should be estimated via iterative curve fitting (\code{TRUE} by default).
#' @param modpts The number of points to model between 0 and \code{maxq} (or \code{maxt}). Defaults to \code{100}.
#' @param aggreg A string specifying how the "best" results of non-linear optimization (\code{nlsm}) should be chosen.
#' One of \code{"aard"} (default; value with minimum Average Absolute Relative Deviation is chosen) or \code{"mean"}
#' (the arithmetic mean of all results is taken).
#' @param maxt,maxq Maximum x-axis value (solvent expended, kg/kg insoluble solid for \code{maxq}, time for \code{maxt}) to use
#' for model predictions. Defaults to 120% of the maximum experimental value.
#' @param optmet The method of iterative curve fitting to use for estimating \eqn{k_1}, \eqn{k_2}, and (optionally) \eqn{F}.
#' One of \code{"nlopt"} (Non-Linear Optimization via \code{\link[nloptr]{nloptr}}; default) or \code{"nlrob"} (Robust Fitting
#' via \code{\link[robustbase]{nlrob}}).
#'
#' @return A \code{list} with the following elements:
#' \enumerate{
#' \item \strong{$ordt}: A \code{data.frame} containing the original input data including the extraction time (\code{x}),
#' \strong{optionally} the S/M ratio (\code{q}), the actual response (\code{y}, usually yield) and associated model
#' predictions (\code{"pred_y"}), as well as the actual and predicted fractional yield (\code{"cc0"} and \code{"pred_cc0"})
#' defined as the ratio of yield at time \eqn{e_t} and the maximum possible yield \eqn{e_0}.
#' \item \strong{$mdt}: A \code{data.frame} of modeled data including the model type (\code{$model}), the extraction time
#' (\code{"x"}), the \strong{optional} S/M ratio (\code{"q"}), the response (\code{"y"}), and the fraction of maximum
#' attainable yield (\code{"cc0"}).
#' \item \strong{$mod_pars}: A named \code{numeric} vector of input and estimated model parameters including the first-order
#' rate constants \eqn{k_1} and \eqn{k_2} (\code{c("k1","k2")}), the fraction of easily desorbed solute \eqn{F} (\code{"f"}),
#' and the maximum possible yield of extractable material \eqn{e_0} (\code{"c0"}).
#' \item \strong{$fit_pars}: A \code{character} vector of which parameters in \code{$mod_pars} were iteratively fit to the model.
#' \item \strong{$resid}: A named \code{numeric} vector of various error values for the model, including Average Absolute Relative Deviation
#' AARD (\code{"aard"}), round mean squared error RMSE (\code{"rmse"}), and the R2 value (\code{"r2"}).
#' }
#' @export
#'
#' @seealso \code{\link{ktsmod}}, \code{\link{tws_eq}}, \code{\link{moderr}}
#'
#' @importFrom stats as.formula coef sd
#' @importFrom robustbase nlrob
#' @importFrom nloptr nloptr
#'
tws_cmp <- function(x, y, c0, q = NA, f = NA, k1_0 = 0.5, k2_0 = 0.5, f0 = NA, est_f = TRUE, modpts = 100,
                    aggreg = "aard", maxt = round(max(x)*1.2,-1), maxq = round(max(q)*1.2,-1), optmet = "nlopt") {

  #Preliminary checks
  qchk <- any(is.na(q))
  if(qchk) maxq <- NA
  if(!any(c("aard","mean") %in% aggreg)) stop("The fitted parameter aggregation setting 'aggreg' may only be one of: 'aard', 'mean'!")
  if(!any(c("nlopt","nlrob") %in% optmet)|length(optmet)!=1) stop("The non-linear fitting method 'optmet' must be one of: 'nlopt' or 'nlrob'!")
  if(all(is.na(c(f,f0)))) stop("One of 'f' or 'f0' must be provided!")
  if(est_f & is.na(f0)) stop("The initial estimate 'f0' must be provided if 'est_f' is TRUE!")
  if(any(sapply(list(x,y), function(z) !is.atomic(z)|!is.numeric(z)))) stop("Both 'x' and 'y' input data must be atomic numeric vectors!")
  if(length(x)!=length(y)) stop("Input data vectors must be of equal length!")

  chkf <- if(is.na(f)) f0 else f
  if(chkf > 1 | chkf < 0) stop("The F value (easily exctractable solute fraction) must be between 0 and 1!")

  #modnms <- c("MM","tau","CM","mtl")
  #if(any(nlsm %in% "all")) nlsm <- modnms
  #if(!all(nlsm %in% modnms)) stop(paste0("At least one NLS fitting method not recognised! Possible values are: ", paste0("'", modnms, "'", collapse = ", "), "."))

  #Build the model
  m <- length(x)
  cc0 <- y/c0

  aardvec <- rep(NA,4)
  resmat <- as.data.frame(matrix(nrow = 4, ncol = 2, dimnames = list(c(), c("k1","k2"))))
  if(est_f) resmat[,"f"] <- rep(NA,nrow(resmat))

  if(optmet == "nlrob") {

    sd_formula <- as.formula("cc0 ~ tws_eq(t, k1, k2, f)")
    optlims <- list(lower = c(k1 = k1_0/200, k2 = k2_0/200, f = if(est_f) f0/200 else NULL),
                    upper = c(k1 = k1_0*200, k2 = k2_0*200, f = if(est_f) 1 else NULL))
    optdata <- data.frame(cc0 = cc0, t = x)
    if(!est_f) optdata[,"f"] <- rep(f, length(y))

    #FITTING ROUND 1
    set.seed(42)
    nlsm <- c("MM","tau","CM","mtl")
    for(i in seq_along(nlsm)) {
      optres <- suppressWarnings(try(robustbase::nlrob(sd_formula,
                                                       data = optdata,
                                                       lower = optlims[["lower"]],
                                                       upper = optlims[["upper"]],
                                                       method = nlsm[i],
                                                       tol = 1e-10), silent = TRUE))
      errchk <- inherits(optres, "try-error")
      if(!errchk) resmat[i,] <- coef(optres)
    }

    #Get mean and SD values
    k1_rd2 <- mean(resmat[,"k1"], na.rm = TRUE)
    k2_rd2 <- mean(resmat[,"k2"], na.rm = TRUE)
    f_rd2 <- if(est_f) mean(resmat[,"f"], na.rm = TRUE) else NA
    k1_sd <- sd(resmat[,"k1"], na.rm = TRUE)
    k2_sd <- sd(resmat[,"k2"], na.rm = TRUE)
    f_sd <- if(est_f) sd(resmat[,"f"], na.rm = TRUE) else NA

    optlims_rd2 <- list(lower = c(k1 = k1_rd2-k1_sd, k2 = max(k2_rd2-k2_sd, 0), f = if(est_f) f_rd2-f_sd else NULL),
                        upper = c(k1 = k1_rd2+k1_sd, k2 = k2_rd2+k2_sd, f = if(est_f) min(c(f_rd2+f_sd, 1)) else NULL))

    #FITTING ROUND 2
    for(i in seq_along(nlsm)) {
      optres <- suppressWarnings(try(robustbase::nlrob(sd_formula,
                                                       data = optdata,
                                                       lower = optlims_rd2[["lower"]],
                                                       upper = optlims_rd2[["upper"]],
                                                       method = nlsm[i],
                                                       tol = 1e-10), silent = TRUE))
      errchk <- inherits(optres, "try-error")
      if(!errchk) {
        resmat[i,] <- cfs <- coef(optres)
        optpred <- 1-cfs[3]*exp(-cfs[1]*x)-(1-cfs[3])*exp(-cfs[2]*x)
        aardvec[i] <- moderr(pred = optpred[2:m], act = cc0[2:m], m = m-1)[["aard"]]
      }
    }
  } else if(optmet == "nlopt") {

    x0 <- c(k1_0, k2_0, if(est_f) f0 else NULL)
    lb <- c(0, 0, if(est_f) 0 else NULL)
    ub <- c(1, 1, if(est_f) 1 else NULL)

    set.seed(42)
    nlsm <- c("NLOPT_LN_COBYLA", "NLOPT_LN_BOBYQA", "NLOPT_LN_NEWUOA", "NLOPT_LN_PRAXIS") #"NLOPT_LN_PRAXIS", "NLOPT_LN_NELDERMEAD"
    for(i in seq_along(nlsm)) {
      optres <- suppressWarnings(try(nloptr::nloptr(x0 = x0,
                                                    eval_f = tws_err,
                                                    f = if(est_f) NA else f,
                                                    tm = x,
                                                    input = cc0,
                                                    lb = lb,
                                                    ub = ub,
                                                    opts = list("algorithm" = nlsm[i],
                                                                "xtol_rel" = 1.0e-15,
                                                                "maxeval" = 50000)), silent = TRUE))
      errchk <- inherits(optres, "try-error")
      if(!errchk) {
        resmat[i,] <- cfs <- optres[["solution"]]
        if(!est_f) cfs[3] <- f
        optpred <- 1-cfs[3]*exp(-cfs[1]*x)-(1-cfs[3])*exp(-cfs[2]*x)
        aardvec[i] <- moderr(pred = optpred[2:m], act = cc0[2:m], m = m-1)[["aard"]]
      }
    }
  }

  #Get final estimated parameter values
  if(aggreg == "aard") {
    min_aard <- which(aardvec == min(aardvec, na.rm = TRUE))
    k1 <- resmat[min_aard,"k1"]
    k2 <- resmat[min_aard,"k2"]
    finf <- if(est_f) resmat[min_aard,"f"] else f
  } else if(aggreg == "mean") {
    #Based on simple mean
    k1 <- mean(resmat[,"k1"], na.rm = TRUE)
    k2 <- mean(resmat[,"k2"], na.rm = TRUE)
    finf <- if(est_f) mean(resmat[,"f"], na.rm = TRUE) else f
  }

  #Computation of the residual function and other error metrics
  pred_cc0 <- tws_eq(t = x, k1 = k1, k2 = k2, f = finf)
  pred_y <- pred_cc0*c0

  #Get AARD, RMSE, and R2
  resid <- moderr(pred = pred_cc0[2:m], act = cc0[2:m], m = m-1)

  #Compile model data
  xdt <- seq(0, maxt, length.out = modpts)
  if(!qchk) qdt <- seq(0, maxq, length.out = length(xdt))
  c0dt <- tws_eq(t = xdt, k1 = k1, k2 = k2, f = finf)
  ydt <- c0dt*c0

  #Compile output data
  ordt <- data.frame(x = x, q = if(qchk) NULL else q, y = y, cc0 = cc0, pred_y = pred_y, pred_cc0 = pred_cc0)
  mod_dt <- cbind.data.frame(model = rep("tws", length(xdt)), x = xdt, q = if(qchk) NULL else qdt,
                             y = ydt, cc0 = c0dt)
  mod_pars <- c(c0 = c0, f = finf, k1 = k1, k2 = k2)
  fit_pars <- c("k1", "k2", if(est_f) "f" else NULL)
  return(list(ordt = ordt, mdt = mod_dt, mod_pars = mod_pars, fit_pars = fit_pars, resid = resid))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Predict response for KTS desorption models
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Predict responses using built KTS desorption models
#'
#' @description Predicts the response for new input values using two-site kinetic desorption models
#' built via \code{\link{ktsmod}}.
#'
#' @param input The output of \code{\link{ktsmod}} containing model information.
#' @param newdata A single \code{numeric} value or a vector of new input data to made predictions for
#' (extraction time, in minutes).
#' @param get_yields A \code{logical} value specifying whether the default yield should be converted into mass
#' and percentage yield. Defaults to \code{TRUE}.
#' @param moisture An optional \code{numeric} value showing the moisture content of the raw material
#' in percent fresh weight. Only used to correct percentage yield predictions when \code{get_yields == TRUE}. Defaults to \code{NA}.
#'
#' @return A \code{list} of length 3 containing the \code{$predictions} as a \code{data.frame}, the \code{$input} parameters,
#' and a \code{$description} outlining which process parameters the model is valid for along with other useful information.
#' @export
#'
#' @seealso \code{\link{ktsmod}}
predict_kts <- function(input, newdata, get_yields = TRUE, moisture = NA) {
  #Preliminary checks
  if(!all(c("tws", "plots", "data", "input", "call") %in% names(input))) stop("The 'input' must originate from function 'ktsmod'!")
  if(!is.numeric(newdata)) stop("New input data ('newdata') must be a numeric value/vector!")
  if(!is.logical(get_yields)) stop("Argument 'get_yields' must be logical!")
  if(!is.numeric(moisture) & !is.na(moisture)) stop("If provided, argument 'moisture' must be a numeric value!")

  #Retrieve model output and input parameters
  inpars <- input[["input"]]
  outpars <- input[["tws"]][["mod_pars"]]

  cat("\nPredicting response...")
  #Compile predictions
  pred_cc0 <- tws_eq(t = newdata, k1 = outpars[["k1"]], k2 = outpars[["k2"]], f = outpars[["f"]])
  pred_y <- pred_cc0*outpars[["c0"]]

  df <- setNames(cbind.data.frame(newdata, inpars[["flow"]]*1000*(newdata*60)/inpars[["mass_in"]],
                         pred_y, pred_cc0), c("t", "sm", paste0("yield_", input[["units"]]["response"]), "yield_cc0"))

  #Optionally append extra yields (mass and percentage) using information about fractional yield units
  if(get_yields) {
    cat("\nConverting fractional yield into mass and percentage yield...")
    yunit <- input[["units"]]["response"]
    divider <- c(percent = 100, permille = 1000, ppm = 1000000, ppb = 1000000000)

    moisture_factor <- if(!is.na(moisture)) moisture/100 else 0

    yield_mass <- if(yunit == "g") pred_y else pred_y/divider[yunit] * inpars[["mass_in"]] * (1+moisture_factor)
    yield_percent <- if(yunit == "g") pred_y/inpars[["mass_in"]]* (1-moisture_factor) else pred_y / (divider[yunit]/100)
    df <- cbind.data.frame(df, yield_g = yield_mass, yield_percent = yield_percent)
  }

  #Compile statement about parameters for which the model is valid
  parnms <- c(pres = "Pressure", temp = "Temperature", flow = "Flow rate", c0 = "Extractable fraction")
  parunits <- c(pres = "bar", temp = "C", flow = "g/min", c0 = "percent")
  whichnms <- which(names(inpars) %in% names(parnms))
  statepars <- inpars[names(parunits)[whichnms]]
  if("flow" %in% names(statepars)) statepars["flow"] <- round(statepars["flow"]*1000*60, 2)

  statement <- paste0("\nPredictions are valid for the following process parameters:",
                      paste0("\n", parnms[whichnms], " of ", statepars,
                             " ", parunits[whichnms], ".", collapse = ""))
  statement <- append(statement, paste0("\nThe units of flow rate and response are ",
                                        paste0(input[["units"]], collapse = " and "), ", respectively.",
                                        if(get_yields) paste0("\nThe calculated percentage yield was calculated on a ",
                                                              if(is.na(moisture)) "wet" else "dry", " weight basis.") else ""))

  cat("\nDONE!")
  return(list(predictions = df, input = inpars, description = statement))
}
