#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Combine two or more models with same DOE but different responses into a Desirability Function
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Combine models with different responses into a Desirability Function
#'
#' @description Calculates individual and overall desirability function from two or more models (linear or quadratic)
#' output from the \code{\link{doe_analyze}} function.
#' See \strong{Details} and \strong{References} for further information.
#'
#' @param mods A \code{list} of DoE model results output from function \code{\link{doe_analyze}}.
#' @param dsrng A named \code{list} of \code{numeric} vectors of length 2 or 3 specifying lower and upper response limits as well as
#' (\strong{optionally}) the target for each model in \code{mods}. The target can \strong{only} be specified if the
#' corresponding element of \code{obj} is set to \code{"trg"}. The list elements must have names identical to those of responses.
#' @param frng An optional \strong{named} \code{list} of \code{numeric} vectors of length 2 containing lower and upper limits of
#' all factors. Names must correspond to those of \strong{coded} factors (i.e. capital letters).
#' If set to the \code{"default"} value, uses the full range of each factor.
#' @param obj Either a single string or \code{character} vector denoting the objective of desirability function.
#' Each element must be one of: \code{"min"} (response minimization), \code{"max"} (maximization), or \code{"trg"}
#' (a specific response target). When a string is given, the value applies to all models in \code{mods}.
#' @param dtype A \code{character} string specifying whether \code{"coded"} (default) or \code{"uncoded"} factor values should be
#' used for processing. \strong{Note} that ranges in \code{frng} must be provided based on this option.
#' @param wts A \code{list} of desirability weights between \strong{0.1 and 10} where each element corresponds to a model
#' in \code{mods} and its associated \code{obj} and must either be a single \code{numeric} value or a vector of length
#' 2 when the corresponding \code{obj} is set to \code{"trg"} (in such cases, the \strong{first and second} elements are weights
#' used for values \strong{lower and higher} than the target, respectively).
#' @param spts A \code{numeric} vector of length 2 providing the number of desirability optimization starting points
#' to be randomly generated (\strong{element 1}), and those to be retrieved from the original model \code{data.frame} (\strong{element 2}).
#' Defaults to \code{c(100,10)}.
#' @param modbase A \code{character} string specifying which model from each element of \code{mods} should be
#' used for processing. One of \code{"initial"} or \code{"final"} (default).
#' @param optmet optimization method. One of \code{"optim"} (uses the eponymous \code{\link[stats]{optim}} function) or
#' \code{"nlopt"} (uses the \strong{NLopt} library for non-linear optimization).
#' @param kmed The \strong{optional} number of clusters to use for Partitioning Around Medoids (PAM) clustering.
#' May be set to a specific integer, \code{"auto"} for automatic choice of appropriate cluster number, or \code{NA}
#' (\strong{default}, clustering \strong{not run}).
#' @param export An \strong{existing} folder path where a .CSV of the results should be exported.
#' @param silent A \code{logical} specifying whether useful updates in the console during processing should be suppressed.
#' Defaults to \code{FALSE}.
#'
#' @details
#' This function retrieves two or more responses from various DoE-derived linear and/or quadratic models analyzed by \code{\link{doe_analyze}}
#' and transforms each response into a desirability function using the approach of Derringer and Suich (1980) for simultaneous
#' optimization of several responses. The overall desirability is then calculated and optimized by non-linear techniques including
#' \code{\link[stats]{optim}} and the \strong{NLopt} library.
#'
#' The process allows for calculation of one of three types of desirability functions including:
#' \enumerate{
#' \item \strong{Larger-The-Best (LTB)}: A maximization function.
#' \item \strong{Smaller-The-Best (STB)}: A minimization function.
#' \item \strong{Nominal-The-Best (NTB)}: A function targeting a specific optimal response value.
#' }
#'
#' The response \eqn{\hat{Y}_i} as well as its minimum (\eqn{Y_{i\ast}}) and maximum (\eqn{Y_{i}^{\ast}}) acceptable values are incorporated
#' into the calculation of a desirability \eqn{d_i} where \eqn{0 \leq d_i \leq 1}. A weight \eqn{0.1 \leq r1 \leq 10} is also added
#' to modify the relative influence of the response value on desirability. When \eqn{r<1}, the desirability increases at a lower rate
#' above \eqn{Y_{i\ast}}, decreasing the relative importance of the response. The reverse is true when \eqn{r>1}, rapidly increasing
#' desirability above \eqn{Y_{i\ast}} and increasing importance of the response. The overall desirability equations for
#' \strong{one-sided transformations} is:
#'
#' \deqn{d_i = 0 \text{ if } \hat{Y}_i \leq Y_{i\ast}}
#' \deqn{d_i = [(\hat{Y}_i - Y_{i\ast})/(Y_i^\ast - Y_{i\ast})]^{r1} \text{ if } Y_{i\ast} < \hat{Y}_i < Y_i^\ast}
#' \deqn{d_i = 1 \text{ if } \hat{Y}_i \geq Y_i^\ast}
#'
#' Upper, lower, or both limits can be set for each response.
#'
#' For two-sided transformations required by the \strong{NLB} approach, the value \eqn{\hat{Y}_i} has both a \strong{minimum and maximum}
#' constraint and the value chosen for \eqn{c_i} is based on maximum desirability provided within the constraints of \eqn{Y_{i\ast}}
#' and \eqn{Y_{i}^{\ast}}. The relative importance of each side of this target value is adjusted by factors \eqn{r1} and \eqn{r2}.
#' The associated equations are as follows:
#'
#' \deqn{d_i = ((\hat{Y}_i - Y_{i\ast})/(c_i - Y_{i\ast}))^{r1} \text{ if } Y_{i\ast} \leq \hat{Y}_i \leq c_i}
#' \deqn{d_i = ((\hat{Y}_i - Y_i^\ast)/(c_i - Y_i^\ast))^{r2} \text{ if } c_i < \hat{Y}_i \leq Y_i^\ast}
#' \deqn{d_i = 0 \text{ if } \hat{Y}_i < Y_{i\ast} \text{ or } \hat{Y}_i > Y_i^\ast}
#'
#' The \strong{overall} desirability \eqn{OD} may then be calculated by taking the \strong{geometric mean} of individual response desirability values:
#'
#' \deqn{OD = (d_1 \times d_2 \times ... \times d_k)^{1/k}}
#'
#' Both individual and overall desirability may be optimized using \strong{non-linear techniques}.
#'
#' @references
#' Cardoso, R.P., da Motta Reis, J.S., Silva, D.E.W., de Barros, J.G.M., Sampaio, N.A.S. (2023), 'How to perform a simultaneous optimization with several response variables', \emph{Management and Administrative Professional Review} \strong{14} (1), pp. 564-578, DOI: \url{http://dx.doi.org/10.7769/gesec.v14i1.1536}.
#'
#' Cojocaru, C., Khayet, M., Zakrzewska-Trznadel, G., Jaworska, A. (2009), 'Modeling and multi-response optimization of pervaporation of organic aqueous solutions using desirability function approach', \emph{Journal of Hazardous Materials} \strong{167}, pp. 52-63, DOI: \url{http://dx.doi.org/10.1016/j.jhazmat.2008.12.078}.
#'
#' Derringer, G., Suich, R. (1980), 'Simultaneous Optimization of Several Response Variables', \emph{Journal of Quality Technology} \strong{12} (4), pp. 214-219, DOI: \url{https://doi.org/10.1080/00224065.1980.11980968}.
#'
#' @return A named \code{list} containing the following elements:
#' \enumerate{
#' \item \strong{$factor_lims}: A \code{data.frame} summarizing the names, objectives, and lower/upper limits of all factors from \code{frng}.
#' \item \strong{$response_lims}: A \code{data.frame} with names, desirability function objectives, lower/upper limits, and weights of all
#' responses from \code{dsrng}.
#' \item \strong{$mod_sums}: A \code{data.frame} summarizing key performance metrics from models.
#' \item \strong{$orig_data}: The original input \code{data.frame} used to build the models, including coded and uncoded factor levels,
#' responses, as well as corresponding individual and overall desirability values.
#' \item \strong{$output_data}: The results of local desirability optimization from all starting points specified in \code{spts},
#' including individual and overall desirabilities.
#' \item \strong{$unique_solutions}: A \code{data.frame} of statistically unique solutions derived from \code{$output_data}.
#' \item \strong{$call}: The function call.
#' }
#' @export
#'
#' @examples
#' #Calculate overall desirability among 3 responses
#' doe_lst1 <- load_internal("doe_lst1")
#'
#' desires <- doe_desir(mods = doe_lst1,
#'                      dsrng = list(CarnosicAcid_mgg = c(0,150),
#'                      Carnosol_mgg = c(0,65), ExtYield = c(1,7)),
#'                      frng = list(B = c(40,60), A = c(10,30), C = c(1,3)),
#'                      obj = c("max", "max", "max"),
#'                      dtype = "uncoded",
#'                      wts = rep(1,3),
#'                      spts = c(100,10),
#'                      modbase = "final",
#'                      optmet = "nlopt",
#'                      kmed = "auto",
#'                      export = "none",
#'                      silent = FALSE)
#'
#' @seealso \code{\link{doe_analyze}}, \code{\link{get_meq}}
#'
#' @importFrom stats optim setNames
#' @importFrom cluster pam
#' @importFrom factoextra fviz_nbclust
#' @importFrom nloptr nloptr
#'
doe_desir <- function(mods, dsrng, frng = "default", obj = rep("max", length(mods)), dtype = "coded",
                      wts = lapply(obj, function(x) if(x=="trg") rep(1,2) else 1), spts = c(100,10),
                      modbase = "final", optmet = "nlopt", kmed = NA, export = "none", #optype = "single" "multi"
                      silent = FALSE) {

  #Preliminary checks
  if(!is.logical(silent)) stop("Argument 'silent' must be logical!")
  if(export!="none" & !dir.exists(export)) stop("The chosen export directory does not exist!")
  if(!dtype %in% c("coded","uncoded")) stop("Data type 'dtype' must be one of 'coded' or 'uncoded'!")
  if(!optmet %in% c("optim", "nlopt")) stop("Optimization method 'optmet' must be one of 'optim' or 'nlopt'!")
  if((!is.na(kmed) & kmed!="auto" & !is.numeric(kmed))|length(kmed)!=1) stop("K-medoids control 'kmed' must
                                                                                either be a single numeric value,
                                                                                NA, or set to 'default'!")
  #Extract data from model list
  if(!silent) cat("\nPreparing data...")
  prepres <- desir_prep(mods, modbase)
  #list2env(prepres, envir = environment())
  doe_df <- prepres[["doe_df"]]
  eqs <- prepres[["eqs"]]
  c_facs <- prepres[["c_facs"]]
  uc_facs <- prepres[["uc_facs"]]
  c_tot <- prepres[["c_tot"]]
  uc_tot <- prepres[["uc_tot"]]
  cf_lst <- prepres[["cf_lst"]]
  ucf_lst <- prepres[["ucf_lst"]]
  resps <- prepres[["resps"]]

  relfacs <- if(dtype=="coded") c_facs else uc_facs #Factors relevant for various specific operations

  #Preliminary checks (continued)
  if(length(spts)!=2|!is.numeric(spts)|spts[2] > nrow(doe_df)|any(spts < 0)|all(spts <= 0))
    stop("The starting points of optimization ('spts') must be a numeric vector of length 2.
         The second element must not exceed the number of model data points.")

  trgchk <- which(obj %in% "trg")
  if(!is.list(dsrng)|length(dsrng)!=length(mods)|unique(lengths(dsrng))!=2|!all(sapply(dsrng, is.numeric))|!all(names(dsrng) %in% colnames(doe_df))) {
    stop("Argument 'dsrng' must be a list of numeric vectors of length 2 containing minimum and maximum limits for responses!")
  } else if(length(trgchk)>0) {
    for(i in trgchk) if(length(dsrng[[i]])!=3) stop("When any objective ('obj') is set to target mode ('trg'),
                                                    the corresponding response limit vector ('dsrng') must be of length 3!")
  }

  if(!any(frng %in% "default")) {

    if(!is.list(frng)) {
      stop("The permissible factor ranges 'frng' must be a list of numeric vectors!")
    } else if(!all(sapply(frng, is.numeric))|unique(lengths(frng))!=2) {
      stop("Each element of 'frng' must be a numeric vector of length 2!")
    }

    if(!all(names(frng) %in% c_facs)) { #relfacs
      #stop("Argument 'frng' must be named with factor names (coded or uncoded)!")
      misnms <- which(!names(frng) %in% c_facs)
      if(!silent) cat("\nRemoving the following ", length(misnms), " unrecognized names from 'frng': ", names(frng)[misnms], "...", sep = "")
      frng <- frng[-misnms]
    } else if(length(frng)!=length(c_facs)) {
      misfacs <- which(!c_facs %in% names(frng))
      for(i in misfacs) frng[c_facs[i]] <- range(doe_df[,relfacs[i]])
    }
  } else if(length(frng)==1 & frng=="default") {
    if(!silent) cat("\nSetting default factor limits since specific ranges were not provided...")
    frng <- setNames(lapply(relfacs, function(x) range(doe_df[,x])), c_facs)
  }

  if(!all(obj %in% c("min", "max", "trg"))) stop("All elements of 'obj' must be one of: 'min', 'max', or 'trg'!")
  if(length(obj)!=1 & length(obj)!=length(mods)) {
    stop("Vector of objectives 'obj' must either be of length 1 or that of 'mods'!")
  } else if(length(obj)==1) obj <- rep(obj, length(mods))

  if(!is.list(wts)) wts <- as.list(wts)
  if(any(sapply(wts, function(x) !is.numeric(x)))) stop("The desirability function weights ('wts') must be numeric!")
  if(any(unlist(wts) < 0.1|unlist(wts) > 10)) stop("Desirability weights 'wts' must all be between 0.1 and 10!")
  if(!any(c(1,length(mods)) %in% length(wts))) {
    stop("The length of 'wts' must either equal 1 or that of 'mods'!")
  } else if(length(wts)==1) wts <- rep(wts, length(mods))

  if(length(trgchk)>0) {
    wts <- lapply(seq_along(wts), function(x) if(obj[x]=="trg") {
      if(length(wts[x])==1) wts[x] <- rep(wts[x],2)
    } else wts[x])
  }

  #Reorder named arguments where necessary to match the order of responses and factors in 'mods'
  respmatch <- match(resps, names(dsrng))
  dsrng <- dsrng[respmatch]
  obj <- obj[respmatch]
  wts <- wts[respmatch]
  frng <- frng[match(c_facs, names(frng))]

  #Begin processing
  #Create master output list
  finres <- list()

  #STEP 1A: Optionally recode factor limits
  if(dtype=="uncoded") {
    if(!silent) cat("\nRecoding factor limits...")
    bckp_nms <- names(frng)
    names(frng) <- uc_facs[match(names(frng), c_facs)]
    frng <- doe_decode(frng, doe_df, c_facs, uc_facs, task = "code", reassemble = TRUE)[["newvals"]]
    names(frng) <- bckp_nms
  }

  #STEP 1B: Compile data frames of factor and response constraints
  if(!silent) cat("\nCompiling model summary data...")
  finres[c("factor_lims","response_lims")] <- desir_sumlims(doe_df, c_facs, uc_facs, resps, obj, frng, dsrng, wts)

  #STEP 1C: Compile a data frame of various model performance metrics
  finres[["mod_sums"]] <- desir_sumods(mods)

  #STEP 2A: Calculate and append individual desirability values to the main data frame
  dsrng <- lapply(dsrng, sort)
  for(i in seq_along(resps)) {
    doe_df[,paste0("di_", resps[i])] <- desir_calc(input = doe_df[,resps[i]], goal = obj[i], lim = dsrng[[i]], wt = wts[[i]])
  }

  #STEP 2B: Calculate overall desirability
  doe_df[,"DO"] <- sapply(seq(nrow(doe_df)), function(x) { vals <- doe_df[x,grep("di_", colnames(doe_df))] #d_overall(doe_df[,c(11:13)])
  return(prod(vals)^(1/length(vals)))
  })
  finres[["orig_data"]] <- doe_df

  #STEP 3: Maximize overall desirability via model equations and individual desirability functions
  #Generate random sampling and results data frame (with coded and uncoded factor values)
  if(!silent) cat("\nOptimizing factor values via method: '", optmet, "'...")
  out_df <- desir_sample(doe_df, c_facs, uc_facs, resps, spts, frng)

  #Get unique list of parameter vectors
  parlst <- lapply(seq(nrow(out_df)), function(x) {
    #res <- c()
    #if(optype=="multi") {
    #  for(i in cf_lst) res <- append(res, unname(unlist(out_df[x,paste0("str_",i)])))
    #} else if(optype=="single") {
    res <- unlist(out_df[x,grep(paste0("^str_",LETTERS,"$", collapse = "|"), colnames(out_df))])
    names(res) <- gsub("str_", "", names(res), fixed = TRUE)
    #}
    return(res)
  })

  #Run optimization function
  eqs_opt <- eqs[["coded"]]
  lbds <- unlist(lapply(frng, function(x) x[[1]]))
  ubds <- unlist(lapply(frng, function(x) x[[2]]))

  for(i in seq_along(parlst)) {
    cat_prefix <- if(i==1) "\n" else "\r"
    if(!silent) cat(cat_prefix, "Working on starting parameter set ", i, " of ", length(parlst), "...", sep = "")
    curpar <- parlst[[i]]

    if(optmet=="optim") {
      opnm <- "par" #c("par","value")
      optres <- suppressWarnings(try(optim(par = curpar, fn = desir_opt, method = "L-BFGS-B", lower = lbds,
                                           upper = ubds, eqs = eqs_opt, dsrng = dsrng, wts = wts, obj = obj,
                                           optype = "single", opt = "max", control = list(fnscale = -1, maxit = 1000)), silent = TRUE))
    } else if(optmet=="nlopt") {
      opnm <- "solution" #c("solution","objective")
      set.seed(42)
      nlpm <- c("NLOPT_LN_COBYLA", "NLOPT_LN_BOBYQA", "NLOPT_LN_PRAXIS") # "NLOPT_LN_NEWUOA"
      slst <- list()
      ovec <- c()
      for(j in seq_along(nlpm)) {
        optres <- suppressWarnings(try(nloptr::nloptr(x0 = curpar,
                                                      eval_f = desir_opt,
                                                      lb = lbds,
                                                      ub = ubds,
                                                      eqs = eqs_opt,
                                                      dsrng = dsrng,
                                                      wts = wts,
                                                      obj = obj,
                                                      optype = "single",
                                                      opt = "min",
                                                      eqtype = "coded",
                                                      opts = list("algorithm" = nlpm[j],
                                                                  "xtol_rel" = 1.0e-15,
                                                                  "maxeval" = 50000)), silent = TRUE))
        if(length(nlpm)>1 & !inherits(optres, "try-error")) {
          slst[[j]] <- optres[[opnm]]
          ovec[j] <- -optres[["objective"]]
        } else next
      }
      optres[[opnm]] <- slst[[which(ovec==max(ovec))[1]]] #Add the results with the highest overall desirability back to the optimization results
    }
    errchk <- inherits(optres, "try-error")
    if(errchk) next else {
      optpar <- optres[[opnm]]
      newres <- desir_opt(optpar, eqs_opt, dsrng, wts, obj, opt = "none", optype = "single")
      out_df[i,c(names(curpar),names(newres[["resp"]]), names(newres[["di"]]), "DO")] <- c(optpar, newres[["resp"]], newres[["di"]], newres[["do"]])
    }
  }

  #Decode the optimized output factors
  if(!silent) cat("\nDecoding optimized factor values...")
  out_df[,uc_facs] <- doe_decode(out_df[,c_facs], doe_df, c_facs, uc_facs, task = "decode")[["newvals"]]

  #Optionally run k-medoids clustering on the optimization data and plot the results
  desir_cols <- grep("di_|^DO$",colnames(out_df))
  if(!is.na(kmed)) {
    #Determine the optimum number of clusters
    clust_df <- out_df[,desir_cols]
    if(kmed=="auto") {
      kmed <- fviz_nbclust(clust_df, cluster::pam, method = "silhouette")[["data"]]
      kmed <- kmed[which.max(kmed[,"y"]),"clusters"]
    }
    #PAM clustering (k-medoids)
    pamres <- pam(clust_df, kmed)
    out_df[,"Clustering"] <- pamres[["clustering"]]
  }
  finres[["output_data"]] <- out_df <- out_df[order(out_df[,"DO"], decreasing = TRUE),]
  finres[["unique_solutions"]] <- out_df[!duplicated(round(out_df[,desir_cols],2)),]

  #Save function call
  finres[["call"]] <- match.call()

  #STEP 5: Export results
  if(export!="none") {
    if(!silent) cat("\nExporting results...")
    desir_export(input = finres, expath = export, detailed = FALSE, silent = FALSE)
  }
  return(finres)
}
