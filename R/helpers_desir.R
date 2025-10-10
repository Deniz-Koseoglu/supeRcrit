#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Extract key data from a list of models
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Extract key data from a list of screening or RSM models
#'
#' @description Part of the \code{\link{doe_desir}} workflow.
#' \strong{This function manual is hidden from the Index}.
#'
#' @param mods A \code{list} of \strong{2 or more} outputs from function \code{\link{doe_analyze}}.
#' @param modbase Which model to retrieve data from for desirability functions? One of \code{"initial"} or \code{"final"} (default), which
#' represent the full model and that with insignificant effects removed, respectively.
#' @param eqbase Which model to retrieve the equation from? One of \code{"initial"} or \code{"final"} (default).
#'
#' @return A named \code{list} containing the following elements:
#' \enumerate{
#' \item \strong{doe_df}: A \code{data.frame} containing the coded and uncoded factor values as well as responses used to build \strong{all} models in \code{mods}.
#' \item \strong{resps}: A \code{character} vector of response variable names used to build \code{mods}.
#' \item \strong{c_tot}: A \code{character} vector of \strong{total} coded factors.
#' \item \strong{uc_tot}: A \code{character} vector of \strong{total un}coded factors.
#' \item \strong{c_facs}: A \code{character} vector of coded factors utilized in \code{mods}.
#' \item \strong{uc_facs}: A \code{character} vector of \strong{un}coded factors utilized in \code{mods}.
#' \item \strong{cf_lst}: A named \code{list} of coded factor names used for \strong{each} model contained in \code{mods}.
#' \item \strong{ucf_lst}: The \strong{un}coded factor equivalent to \strong{cf_lst}.
#' \item \strong{eqs}: A named \code{list} containing both \code{$raw} (for labeling) and \code{$coded} (for calculations) model equations
#' for the models in \code{mods}.
#' }
#'
#' @export
#' @keywords internal
#'
#' @seealso \code{\link{doe_desir}}, \code{\link{get_meq}}
desir_prep <- function(mods, modbase = "final", eqbase = "final") {

  #Preliminary checks
  if(!is.list(mods)|!all(c("models", "results", "plots", "statements", "call") %in% unique(unlist(lapply(mods, names)))))
    stop("The input data 'mods' must be a LIST of 2 or more models output from function 'doe_analyze'!")
  if(!any(c("initial","final") %in% c(modbase,eqbase))) stop("Argument 'modbase' must be one of 'initial' or 'final'!")

  #Retrieve original data frame used to construct models
  #Also get the number and names of coded and uncoded factors
  doe_df <- mods[[1]][["results"]][["initial"]][["Orig_Data"]] #ORIGINAL INPUT DATASET FOR MODELS
  uc_tot <- mods[[1]][["models"]][["initial"]][["realnames"]] #TOTAL UNCODED FACTORS
  c_tot <- LETTERS[seq(length(uc_tot))] #TOTAL CODED FACTORS
  c_facs <- uc_facs <- resps <- c()
  for(i in seq_along(mods)) {
    cdt <- mods[[i]][["models"]][[modbase]][["codenames"]][,"data"]
    c_facs <- sort(unique(append(c_facs, cdt[which(nchar(cdt)==1)]))) #CODED FACTORS
    uc_facs <- unique(append(uc_facs, mods[[i]][["models"]][[modbase]][["realnames"]])) #UNCODED FACTORS
    resps[i] <- mods[[i]][["results"]][[modbase]][["Misc"]][["Response"]]
  }
  uc_facs <- uc_facs[match(uc_facs, uc_tot[uc_tot %in% uc_facs])]

  cf_lst <- lapply(mods, function(x) {        #MODEL-WISE CODED FACTORS (AS LIST)
    df <- x[["models"]][[modbase]][["model"]]
    return(colnames(df)[grep(paste0("^", LETTERS, "$", collapse = "|"), colnames(df))])
  })

  ucf_lst <- lapply(cf_lst, function(x) uc_facs[which(c_facs %in% x)]) #MODEL-WISE UNCODED FACTORS (AS LIST)

  #Retrieve model equations (coded, for convenience of optimization)
  eq_raw <- get_meq(mods, mtype = eqbase, eqtype = "raw")
  eq_coded <- get_meq(mods, mtype = eqbase, eqtype = "coded")

  #Cleaning up original input data frame
  fin_df <- cbind.data.frame(doe_df[,1,drop = FALSE], doe_df[,c(c_facs, uc_facs)])
  fin_df <- cbind.data.frame(fin_df, doe_df[,!colnames(doe_df) %in% colnames(fin_df)])
  return(list(doe_df = doe_df, resps = resps, c_tot = c_tot, uc_tot = uc_tot,
              c_facs = c_facs, uc_facs = uc_facs, cf_lst = cf_lst, ucf_lst = ucf_lst, eqs = list(raw = eq_raw, coded = eq_coded)))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Retrieve model equation(s)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Retrieve linear or quadratic model equation
#'
#' @description Retrieves the coded or uncoded model equation from the output of function \code{\link{doe_analyze}}.
#' Part of the \code{\link{doe_desir}} workflow.
#'
#' @param mod The output of function \code{\link{doe_analyze}}.
#' @param mtype Which model to retrieve the equation from? One of \code{"initial"} or \code{"final"} (default).
#' @param eqtype Which type of equation to retrieve from the model of type \code{mtype}? One of \code{"coded"} (default) or
#' \code{"raw"}.
#'
#' @return A named string containing the \strong{raw} or \strong{coded} equation for \code{mod}.
#' @export
#'
#' @examples
#' doe_lst1 <- load_internal("doe_lst1")
#' get_meq(doe_lst1[[1]])
#'
#' @seealso \code{\link{doe_desir}}, \code{\link{desir_prep}}, \code{\link{doe_analyze}}
get_meq <- function(mod, mtype = "final", eqtype = "coded") {

  #Preliminary checks
  if(!any(c("initial", "final") %in% mtype)) stop("Model type for equation retrieval must be one of 'initial' or 'final'!")
  if(!any(c("raw", "coded") %in% eqtype)) stop("Equation type must be one of 'raw' or 'coded'!")

  res <- c()
  smodchk <- all(c("models", "results", "plots", "statements", "call") %in% names(mod))
  loopseq <- if(smodchk) "results" else seq_along(mod)
  for(i in loopseq) {
    x <- if(smodchk) mod[[i]] else mod[[i]][["results"]]
    res <- append(res, x[[mtype]][["Misc"]][["Equation"]][[eqtype]])
  }
  names(res) <- if(smodchk) paste0(eqtype, "_equation") else names(mod)
  return(res)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Summarize key metrics of several DOE models
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Summarize key metrics of several DOE models
#'
#' @description Summarizes performance metrics of 2 or more models output from function \code{\link{doe_analyze}}.
#' Part of the \code{\link{doe_desir}} workflow.
#'
#' @param mods A \code{list} of \strong{2 or more} models output from function \code{\link{doe_analyze}}.
#' @param rm_eqs Should model equations be removed from the final output? Defaults to \code{FALSE} (equations are kept).
#'
#' @return A \code{data.frame} with each row containing key information about a single model, including the \code{$response}
#' name, the \code{$model_type} and \code{$order}, performance metrics including the R2 (\code{$r2}), adjusted R2 (\code{$adj_r2}),
#' residual standard error (\code{$stnd_error}), the F statistic (\code{$F}), degrees of freedom (\code{$DOF_1} and \code{$DOF_2}), and
#' the Lack-of-Fit test p-value (\code{$lof_pvalue}). When \code{rm_eqs} is \code{FALSE}, model equations (\code{model_equation})
#' are also included.
#'
#' @export
#'
#' @seealso \code{\link{doe_desir}}, \code{\link{doe_analyze}}
desir_sumods <- function(mods, rm_eqs = FALSE) {
  msum_df <- lapply(mods, function(x) {
    res <- list()
    eqs <- c()
    tvec <- c("initial", "final")
    for(i in tvec) {
      res[[i]] <- unlist(x[["results"]][[i]][["Model_Metrics"]]
                         [c("Order", "R2", "Adj_R2", "Residual_Stnd_Error", "F_Statistic", "F_DOF_1", "F_DOF_2", "LoF_Pvalue")])
      eqs <- append(eqs, x[["results"]][[i]][["Misc"]][["Equation"]][["raw"]])
    }
    res <- cbind.data.frame(x[["results"]][["initial"]][["Misc"]][["Response"]], tvec, do.call(rbind.data.frame, res), eqs)
    colnames(res) <- c("response", "model_type", "order", "r2", "adj_r2", "stnd_error", "F", "DOF_1", "DOF_2", "lof_pvalue", "model_equation")
    return(res)
  })
  msum_df <- do.call(rbind.data.frame, msum_df)
  #msum_df <- msum_df[order(msum_df[,"model_type"]),]
  rownames(msum_df) <- NULL
  if(rm_eqs) msum_df <- msum_df[,!colnames(msum_df) %in% "model_equation"]
  return(msum_df)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Summarize desirability optimization factor and response limits into 2 data frames
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Summarize desirability optimization factor and response limits
#'
#' @description Summarizes desirability-related metrics and response limits.
#' Part of the \code{\link{doe_desir}} workflow.
#' \strong{This function manual is hidden from the Index}.
#'
#' @param doe_df A \code{data.frame} containing coded and uncoded factors as well as all responses.
#' @param c_facs,uc_facs Both are \code{character} vectors of coded and uncoded factor names, respectively.
#' @param resps A \code{character} vector of response names.
#' @param obj A \code{character} vector of desirability function objectives corresponding to \code{resps}.
#' For possible values, see \code{\link{doe_desir}}.
#' @param frng A named \code{list} of numeric vectors (length 2-3) with names corresponding to those of \strong{coded} factors.
#' Each element must contain minimum, maximum, and (optionally) target values of factors (see \code{\link{doe_desir}}).
#' @param dsrng A named \code{list} with each element named after a response in \code{resps} and containing minimum
#' and maximum limits for that response.
#' @param wts A \code{list} of desirability function weights with length identical to that of \code{resps}.
#'
#' @return A \code{list} of length 2 containing \code{data.frame} objects summarizing the lower and upper limits of
#' factors (\code{[[1]]}) and responses (\code{[[2]]}).
#'
#' @export
#' @keywords internal
#'
#' @seealso \code{\link{doe_desir}}
#'
#' @importFrom stats setNames
#'
desir_sumlims <- function(doe_df, c_facs, uc_facs, resps, obj, frng, dsrng, wts) {
  fac_df <- data.frame(codename = c_facs, name = uc_facs, goal = rep("in_range", length(c_facs)),
                       lowlim = sapply(frng, function(x) x[1]), uplim = sapply(frng, function(x) x[2]),
                       row.names = NULL)
  for(i in c("lowlim", "uplim")) fac_df[,paste0(i, "_uncoded")] <- doe_decode(setNames(fac_df[,i], c_facs), doe_df, c_facs, uc_facs, task = "decode")[["newvals"]]

  resp_df <- data.frame(name = resps, goal = obj,
                        lower_lim = sapply(dsrng, function(x) x[1]),
                        target = sapply(dsrng, function(x) if(length(x)>2) x[2] else NA),
                        upper_lim = sapply(dsrng, function(x) if(length(x)>2) x[3] else x[2]),
                        lower_wt = sapply(wts, function(x) x[1]),
                        upper_wt = sapply(wts, function(x) if(length(x)>1) x[2] else NA),
                        row.names = NULL)

  #Remove columns with ALL NAs
  resp_df <- resp_df[,colSums(is.na(resp_df))<nrow(resp_df)]
  return(list(fac_df, resp_df))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Random generation/sampling of starting points for desirability optimization
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Generation/sampling of starting points for desirability optimization
#'
#' @description Randomly generates and/or samples starting points for desirability function optimization.
#' Part of the \code{\link{doe_desir}} workflow.
#' \strong{This function manual is hidden from the Index}.
#'
#' @param orig A \code{data.frame} containing coded and uncoded factors as well as all responses.
#' @param c_facs,uc_facs Both are \code{character} vectors of coded and uncoded factor names, respectively.
#' @param resps A \code{character} vector of response names.
#' @param spts A \code{numeric} vector of length 2 providing the number of desirability optimization starting points
#' to be randomly generated (\strong{element 1}), and those to be retrieved from the original model \code{data.frame}
#' (\strong{element 2}).
#' @param frng A \strong{named} \code{list} of \code{numeric} vectors of length 2 containing lower and upper limits of
#' all factors. Names must correspond to those of \strong{coded} factors (i.e. capital letters).
#'
#' @return A \code{data.frame} containing the coded and uncoded starting points to use for desirability optimization.
#'
#' @export
#' @keywords internal
#'
#' @seealso \code{\link{doe_desir}}, \code{\link{desir_opt}}
#'
#' @importFrom stats runif setNames
#' @importFrom pracma randsample
#'
desir_sample <- function(orig, c_facs, uc_facs, resps, spts, frng) {

  #Create data frame of randomly generated and sampled factor data
  out_df <- as.data.frame(matrix(NA, ncol = 2 + length(c_facs)*4 + length(resps)*2, nrow = sum(spts)))
  colnames(out_df) <- c("type", paste0("str_", c(c_facs,uc_facs)), c_facs, uc_facs, resps, paste0("di_",resps), "DO")
  out_df[,"type"] <- c(rep("random", spts[1]), rep("design", spts[2]))

  for(i in c_facs) out_df[,paste0("str_",i)] <- c(runif(spts[1], min = frng[[i]][1], max = frng[[i]][2]), #Random sampling
                                                  orig[randsample(seq(nrow(orig)), spts[2]), i]) #Points taken from the design data frame

  #Also add decoded factor values
  out_df[,paste0("str_",uc_facs)] <- doe_decode(setNames(out_df[,paste0("str_",c_facs)], c_facs), orig, c_facs, uc_facs, task = "decode")[["newvals"]]
  return(out_df)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Calculate desirability function based on goal
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Calculate desirability function based on goal
#'
#' @description A wrapper for the \code{\link[desirability2]{d_min}}, \code{\link[desirability2]{d_min}}, and
#' \code{\link[desirability2]{d_target}} for generating desirability functions based on a specific goal of
#' maximization, minimization, or specific target for 2 or more response variables.
#' Part of the \code{\link{doe_desir}} workflow.
#' \strong{This function manual is hidden from the Index}.
#'
#' @param input A \code{numeric} vector of \strong{response} values from which to calculate desirability.
#' @param goal The desirability function objective. One of \code{"min"} (minimize; \strong{default}), \code{"max"} (maximize),
#' or \code{"trg"} (target).
#' @param lim A \code{numeric} vector of length 2 containing the minimum and maximum values of response in \code{input}.
#' @param wt A \code{numeric} value or vector of length 2 containing the desirability weight(s) to use.
#' Possible values range from 0.1 to 10 (defaults to \code{1}).
#' Two weights must be given if \code{goal} is set to \code{"trg"}.
#'
#' @return A \code{numeric} vector of desirability values derived from \code{input}.
#'
#' @export
#' @keywords internal
#'
#' @seealso \code{\link{doe_desir}}
#'
#' @importFrom desirability2 d_min d_max d_target
desir_calc <- function(input, goal = "min", lim = range(input), wt = 1) {

  #Preliminary checks
  if(!is.atomic(input)|!is.numeric(input)) stop("The input must be an atomic numeric vector!")
  if(!any(c("min","trg","max") %in% goal)) stop("The 'goal' must be one of 'min', 'max', or 'trg'!")
  lim <- sort(lim)

  #Get individual desirability
  out <- if(goal=="min") {
    d_min(input, low = lim[1], high = lim[2], scale = wt[1])
  } else if(goal=="max") {
    d_max(input, low = lim[1], high = lim[2], scale = wt[1])
  } else if(goal=="trg") {
    d_target(input, low = lim[1], target = lim[2], high = lim[3],
             scale_low = wt[1], scale_high = wt[2])
  }
  return(out)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Optimization of overall desirability (maximization)
#Suitable for both 'optim' and 'nlopt'
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Maximization of overall desirability
#'
#' @description Part of the \code{\link{doe_desir}} workflow.
#' \strong{This function manual is hidden from the Index}.
#'
#' @param pars A \code{numeric} vector of factor levels.
#' @param eqs A named \code{character} vector of \strong{coded} equations (see \code{\link{get_meq}}) for each response.
#' @param dsrng A named \code{list} with each element named after a response and containing minimum
#' and maximum limits for that response.
#' @param wts A \code{list} of desirability function weights with length identical to the number of responses.
#' @param obj A \code{character} vector of desirability function objectives of length identical to the number of responses.
#' @param eqtype Which type of equation to retrieve from the model of type \code{mtype}? One of \code{"coded"} (default) or
#' \code{"raw"}.
#' @param opt The objective of optimization (determines the nature of function output).
#' One of \code{"min"}, \code{"max"}, or \code{"none"} (default).
#' @param optype A \strong{deprecated} character value (either the default \code{"single"} or \code{"multi"})
#'
#' @return If \code{opt != "none"}, the final desirability is returned as a single \code{numeric} value.
#' Otherwise, a \code{list} containing optimized responses (\code{$resp}), corresponding desirability function
#' values (\code{$di}), and the overall desirability calculated via a geometric mean (\code{$do}) is returned.
#'
#' @export
#' @keywords internal
#'
#' @seealso \code{\link{doe_desir}}, \code{\link{get_meq}}
#'
#' @importFrom stringr str_extract_all str_replace_all fixed
#' @importFrom stats setNames
#'
desir_opt <- function(pars, eqs, dsrng, wts, obj, eqtype = "coded", opt = "min", optype = "single") {

  #Preliminary checks
  if(!any(c("single", "multi") %in% optype)) stop("The optimization type 'optype' must be either 'single' or 'multi'!")
  if(!any(c("min","max","none") %in% opt)) stop("The optimization mode 'opt' must be one of 'min', 'max', or 'none'!")
  if(!any(c("raw","coded") %in% eqtype)) stop("The equation type 'eqtype' must either be 'raw' or 'coded'!")
  cchk <- eqtype=="coded"
  regpat <- if(cchk) "x\\[[:digit:]\\]" else paste0(LETTERS, collapse = "|")

  #Extract unique model terms from equations
  termlst <- lapply(str_extract_all(eqs, regpat), function(x) {
    res <- sort(unique(x))
    inds <- if(cchk) as.numeric(gsub("^.*([0-9]+).*$", "\\1", res)) else which(LETTERS %in% res)
    letts <- LETTERS[inds]
    codes <- paste0("x[", inds, "]")
    return(list(inds = inds, letts = letts, codes = codes))
  })
  indl <- lapply(termlst, function(x) x[["inds"]])
  letl <- lapply(termlst, function(x) x[["letts"]])
  codl <- lapply(termlst, function(x) x[["codes"]])

  #Rename model terms if a 'raw' equation is provided
  #if(!cchk) eqs <- sapply(seq_along(eqs), function(x) str_replace_all(eqs[x], subl[[x]]))
  if(cchk) eqs <- sapply(seq_along(eqs), function(x) res <- str_replace_all(eqs[x], fixed(setNames(letl[[x]], codl[[x]]))))

  #Derive indices to invoke from 'pars'
  if(optype=="multi") {
    trsum <- cumsum(lengths(indl))
    parlst <- splitAt(pars, trsum[-length(trsum)]+1)
    parlst <- lapply(seq_along(parlst), function(x) setNames(parlst[[x]], letl[[x]]))
  } else if(optype=="single") {
    parlst <- rep(list(setNames(pars, sort(unique(unlist(letl))))),length(indl))
  }

  #Get responses and individual desirabilities
  resps <- dis <- c()
  for(i in seq_along(parlst)) {
    list2env(as.list(parlst[[i]]), envir = environment())
    resps[i] <- curesp <- eval(parse(text = eqs[[i]]))
    dis[i] <- desir_calc(resps[i], goal = obj[i], lim = dsrng[[i]], wt = wts[[i]])
  }

  #Get overall desirability
  do <- prod(dis)^(1/length(dis))

  res <- if(opt=="max") do else if(opt=="min") -do else if(opt=="none") list(resp = setNames(resps, names(dsrng)),
                                                                             di = setNames(dis, paste0("di_",names(dsrng))), do = do)
  return(res)
}
