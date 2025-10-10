#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Path of steepest ascent
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Find path of steepest ascent
#'
#' @description Part of the \code{\link{doe_analyze}} workflow. Calculates the path of steepest ascent.
#' \strong{This function manual is hidden from the Index}.
#'
#' @param mod_in Model of class \code{"lm"}.
#' @param c_facs,uc_facs Coded and uncoded factors as \code{character} vectors.
#' @param respname Response name as a \code{character} string.
#'
#' @return A \code{data.frame} containing the path of steepest ascent.
#'
#' @seealso \code{\link{doe_analyze}}
#'
#' @export
#' @keywords internal
#'
#' @importFrom stats approx predict setNames uniroot
#'
steepath <- function(mod_in, uc_facs, c_facs, respname = NA){

  #Preliminary checks
  if(!inherits(mod_in, "lm")) stop("The input model 'mod_in' must be of class 'lm'!")
  if(length(uc_facs)!=length(c_facs)|!is.character(c(c_facs,uc_facs))) stop("Coded and uncoded factor names must be character vectors of equal length!")
  if((!is.na(respname) & !is.character(respname))|length(respname)!=1) stop("The response variable name, where provided, must be a single character string!")

  dist = abs(seq(0, 5, by = 0.5))
  mod_sum <- aug_sum(mod_in)

  iden = diag(rep(1, length(mod_sum[["b"]])))
  rng = range(eigen(mod_sum[["B"]])[["values"]])

  soln = function(gam) {
    -0.5 * solve(mod_sum[["B"]] - gam * iden, mod_sum[["b"]])
  }

  deldist = function(gam, d) {
    xx = soln(gam)
    sqrt(sum(xx^2)) - d
  }

  find.pt = function(d, bd) {
    if (abs(d) < 0.01)
      return(0 * mod_sum[["b"]])
    gamma = uniroot(deldist, bd, d)$root
    soln(gamma)
  }

  incr.out = function(bd, inc, mind) {
    while (deldist(bd, mind) > 0) {
      bd = bd + inc
      inc = 2 * inc
    }
    bd
  }

  mind = min(dist[dist > 0.009])
  bds = c(rng[2] + 0.001, incr.out(rng[2] + 5, 2, mind))
  path = t(sapply(dist, find.pt, bds))
  #cat(paste("Path of steepest ascent from ridge analysis:\n"))

  #Remove main factors that are not present in results (if any)
  mainchk <- c_facs %in% colnames(path)
  c_facs <- c_facs[mainchk]
  uc_facs <- uc_facs[mainchk]

  path = newdata = as.data.frame(round(path, 3))
  md = mod_in[["orig_df"]]

  for (vn in names(md)[names(md) %in% uc_facs]) if (is.null(newdata[[vn]])) {
    v = md[[vn]]
    if (is.factor(v))
      newdata[[vn]] = factor(levels(v)[1], levels = levels(v))
    else newdata[[vn]] = mean(v)
  }
  yhat = predict(mod_in, newdata = newdata)
  path <- cbind.data.frame(path, yhat)
  if(!is.na(respname)) path <- setNames(path, c(c_facs, respname))

  #Trim the coded variable to be within range of the design
  for(i in seq_along(uc_facs)) {
    path <- path[path[,i] >= min(md[,c_facs[i]]) & path[,i] <= max(md[,c_facs[i]]),]
  }

  #Decode via linear interpolation
  lvls <- as.data.frame(sapply(seq_along(uc_facs), function(x) {
    res <- unique(md[,c(c_facs[x],uc_facs[x])])
    res <- approx(x = res[,1], y = res[,2], xout = path[,x], method = "linear")[["y"]]
  }))
  path <- cbind.data.frame(setNames(lvls, uc_facs), path)

  return(path)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Canonical Analysis for DOE LM RESULTS
#NOTE: This function requires that the hierarchy principle is adhered to and all
#main effects which are part of two-way or quadratic interactions are included!
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Canonical Analysis (CA) of linear/quadratic models
#'
#' @description Carries out canonical analysis of RSM results.
#' Part of the \code{\link{doe_analyze}} workflow.
#' \strong{This function manual is hidden from the Index}.
#'
#' @param object A model of class \code{lm}.
#' @param augs A \code{list} containing the matrices of first- and second-order coefficients necessary to carry out CA.
#' @param threshold The canonical analysis threshold. Defaults to 10% of the maximum Eigen value.
#'
#' @return A named \code{list} of length 2 containing the \strong{coded} factor value estimates (\code{$xs})
#' and the \strong{eigen decomposition} (\code{$eigen}).
#'
#' @export
#' @keywords internal
#'
#' @seealso \code{\link{doe_analyze}}
canon <- function (object, augs, threshold = 0.1 * max.eigen)
{
  if(!inherits(object, "lm"))
    stop("Not an 'lm' model!")
  if (augs[["order"]] == 1)
    stop("Canonical analysis is not possible for first-order models")

  #Calculate eigenvectors and values
  EA <- eigen(augs[["B"]])
  max.eigen <- max(abs(EA[["values"]]))
  active <- which(abs(EA[["values"]]) >= threshold)

  if (length(active) == 0)
    stop("threshold is greater than the largest |eigenvalue|!")
  if ((nzero <- length(EA[["values"]]) - length(active)) > 0)
    message("Near-stationary-ridge situation detected -- stationary point altered\n",
            " Change 'threshold' if this is not what you intend")

  U <- EA[["vectors"]][, active, drop = FALSE]
  laminv <- 1/EA[["values"]][active]
  xs <- as.vector(-0.5 * U %*% diag(laminv, ncol = ncol(U)) %*%
                    t(U) %*% augs[["b"]])
  names(xs) <- names(augs[["b"]])
  dimnames(EA[["vectors"]]) <- list(names(augs[["b"]]), NULL)
  if (length(active) < nrow(U)) {
    EA[["values"]][-active] = 0
  }
  list(xs = xs, eigen = EA)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Augmented Summary for LM models that includes Canonical Analysis Optimization
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Generate an augmented summary of linear/quadratic models
#'
#' @description Returns an augmented summary of a linear or quadratic model (i.e. model of order 1, 1.5, or 2).
#' Part of the \code{\link{doe_analyze}} workflow. \strong{This function manual is hidden from the Index}.
#'
#' @param linmod A linear model of class \code{lm}.
#' @param thres_coeff The fraction (between 0.01 and 1) of maximum eigenvalue to use for canonical analysis (see \code{\link{canon}}).
#' @param codenames An \strong{optional} \code{data.frame} containing disambiguation of factor names.
#' Generated from function \code{\link{twoway}}.
#'
#' @return The augmented summary of the input model \code{linmod} (class \code{"summary.lm"}).
#'
#' @export
#' @keywords internal
#'
#' @seealso \code{\link{doe_analyze}}
aug_sum <- function(linmod, thres_coeff = 0.1, codenames = NA) {
  #Preliminary checks
  realchk <- "codenames" %in% names(linmod)
  if(!inherits(linmod, "lm")) stop("A linear model object (lm) must be provided!")
  if(any(is.na(codenames)) & !realchk) {
    cat("\nWhere factor name disambiguation is not provided, coefficient names are used instead!")
  } else if(is.data.frame(codenames) & !realchk) {
    linmod[["codenames"]] <- codenames
  }

  #Create dataframe for results (eventually to be appended to summary object)
  sumaugs <- list()

  #STEP 1: First-Order Terms, b and B dataframes
  coeffs <- linmod[["coefficients"]]
  coeffs <- coeffs[!names(coeffs) %in% "(Intercept)"]
  names(coeffs) <- nm <- if(!any(is.na(codenames)) | realchk) linmod[["codenames"]][,"data"] else names(linmod[["coefficients"]])[!names(linmod[["coefficients"]]) %in% "(Intercept)"]

  #Determine first-order terms
  i.fo <- which(sapply(nm, nchar)==1)

  #Derive b dataframe
  k <- length(i.fo)
  sumaugs[["b"]] <- coeffs[i.fo]

  #Set model order
  sumaugs[["order"]] = 1

  #Derive B dataframe
  fonm <- names(sumaugs[["b"]])
  sumaugs[["B"]] <- matrix(0, k, k)
  dimnames(sumaugs[["B"]]) = list(fonm, fonm)

  #CHECK IF ANY MAIN EFFECTS THAT ARE INCLUDED IN TWO-WAY INTERACTIONS AND/OR QUADRATIC TERMS ARE ABSENT! IF SO, ADD THEM.
  #Note that this routine is irrelevant if the hierarchy principle is adhered to (like in the doe_optim function...)
  unique_effs <- unique(unlist(strsplit(nm, "")))
  missing_check <- !all(unique_effs %in% colnames(sumaugs[["B"]]))

  if(missing_check) {
    #Update fonm
    fonm <- unique_effs[order(unique_effs)]

    missing_effs <- unique_effs[which(!unique_effs %in% colnames(sumaugs[["B"]]))]

    for(i in missing_effs) {
      bckp_rownames <- rownames(sumaugs[["B"]])
      sumaugs[["B"]] <- rbind.data.frame(sumaugs[["B"]], rep(0, ncol(sumaugs[["B"]])))
      rownames(sumaugs[["B"]]) <- c(bckp_rownames, i)
      sumaugs[["B"]][,i] <- rep(0, nrow(sumaugs[["B"]]))
    }

    sumaugs[["B"]] <- sumaugs[["B"]][order(rownames(sumaugs[["B"]])), order(colnames(sumaugs[["B"]]))]

  }

  #STEP 2: Two-Way Interactions (NOTE: ALL TERMS IN TWIs MUST BE INCLUDED AS FO TERMS AS WELL!)
  i.twi = which(unlist(lapply(strsplit(nm, ""), function(x) any(x != x[[1]]) & length(x)==2)))

  #Check model has at least 1 TWI and >1 FO term
  if ((k > 1) & (length(i.twi) > 0)) {
    btwi = coeffs[i.twi]

    #Update model order
    sumaugs[["order"]] = 1.5

    #Update B dataframe
    twi.lab <- names(coeffs[i.twi])
    names(twi.lab) = NULL

    for (i in seq_along(twi.lab)) {
      vn = strsplit(twi.lab[i], "")[[1]]
      idx = match(vn, fonm)
      if (!is.na(btwi[i]) & !any(is.na(idx)))
        sumaugs[["B"]][idx[1], idx[2]] <- sumaugs[["B"]][idx[2], idx[1]] <- btwi[i]/2 #Why is this divided by 2?
      else twi.lab[i] <- paste(twi.lab[i], "@", sep = "") #What's the purpose of this line?
    }
  }

  #STEP 3: Quadratic Terms
  i.pq <- which(unlist(lapply(strsplit(nm, ""), function(x) all(x == x[[1]]) & length(x)==2)))

  if (length(i.pq) > 0) {
    #Update model order
    sumaugs[["order"]] = 2


    pq.lab <- names(coeffs[i.pq])
    names(pq.lab) <- NULL
    vn <- sapply(pq.lab, function(x) strsplit(x, "")[[1]][1])

    for (i in seq_along(vn)) sumaugs[["B"]][vn[i], vn[i]] <- coeffs[i.pq[i]]
  }

  #STEP 4: Create augmented model summary
  SUMM <- summary(linmod)

  #Perform canonical analysis (optional based on model order, not possible for 1st order models; also not possible if hierarchy principle was not adhered to in the model)
  if (sumaugs$order > 1 & !missing_check) {

    EA <- eigen(sumaugs[["B"]])
    max.eigen <- max(abs(EA[["values"]]))

    SUMM[["canonical"]] = suppressMessages(canon(linmod, augs = sumaugs, threshold = thres_coeff * max.eigen))
  } else SUMM[["sa_lin"]] = sumaugs[["b"]]/sqrt(sum(sumaugs[["b"]]^2))

  #Add sumaugs to summary object
  for(i in names(sumaugs)) {
    SUMM[[i]] <- sumaugs[[i]]
  }

  #Add codenames and realnames, where present
  if("codenames" %in% names(linmod)) SUMM[["codenames"]] <- linmod[["codenames"]][,"data"]
  if("realnames" %in% names(linmod)) SUMM[["realnames"]] <- linmod[["realnames"]]

  #Return augmented summary
  return(SUMM)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Get all two-way interactions from a model
#Potentially add functionality to get higher-order interactions?
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Get all two-way interactions from a linear/RSM model
#'
#' @description Obtains all interaction and quadratic terms of a linear or quadratic model given labels of coded factors
#' alongside model order. Part of the \code{\link{doe_analyze}} workflow.
#'
#' @param c_facs A \code{character} vector of \strong{coded} factors. \strong{Elements must all be capital letters}.
#' @param mod_order The \code{numeric} model order, either one of 1, 1.5, or 2.
#'
#' @return A \code{data.frame} of 3 columns containing the main, interaction, and/or quadratic terms (depending on \code{mod_order}) in
#' 3 different representations (data, formula, and label).
#' @export
#'
#' @examples
#' twoway(c("A","B","C"), 2)
twoway <- function(c_facs, mod_order = 2) {
  #Preliminary checks
  if(!all(c_facs %in% LETTERS)) stop("Coded factors must be named as capital letters of the Latin alphabet!")

  if(mod_order>1) {
    all_effs <- as.vector(outer(c_facs, c_facs, paste0))

    all_effs <- lapply(all_effs, function(x) { res <- strsplit(x, "")[[1]];
    if(any(table(res)>1) & mod_order < 2) res <- NULL else res <- res[order(res, decreasing = FALSE)];
    res_stnd <- paste(res, collapse = "");
    res_label <- if(length(res)==1) res_stnd
    else if(length(res)==2 & length(unique(res))==2) paste0(res, collapse = "*") #paste0("I(",paste0(res,collapse = "*"), ")")
    else if(length(unique(res))==1) paste0(unique(res),"^2")
    res_formula <- if(length(res)==1) res_stnd
    else if(length(res)>1) paste0("I(", res_label, ")")
    return(c(res_stnd, res_formula, res_label))})

    all_effs <- do.call(rbind.data.frame, c(lapply(c_facs, rep, 3), all_effs))
    all_effs <- unique(all_effs)
  } else all_effs <- do.call(rbind.data.frame, lapply(c_facs, rep, 3))
  colnames(all_effs) <- c("data","formula","label")
  return(all_effs)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Load and prepare input data for model building
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Prepare DoE data for modeling and analysis
#'
#' @description An auto-process function that performs necessary formatting and categorizing of an input DoE \code{data.frame}.
#' Part of the \code{\link{doe_analyze}} workflow.
#'
#' @param doe A \code{data.frame} containing all coded factors (named with \strong{capital letters}), their uncoded equivalents
#' with names as specified in \code{uc_facs}, the response variable as specified in \code{resp_var}, and the run order as provided
#' in \code{time_var}.
#' @param time_var,resp_var Character values specifying the column names in \code{doe}
#' corresponding to the run order and response variables, respectively.
#' @param uc_facs A character vector of column names in \code{doe} specifying \strong{uncoded} factors.
#' @param which_facs String specifying which type of factors to process in preparation for modeling.
#' One of \code{"coded"} (default) or \code{"uncoded"}.
#' @param mod_order The \code{numeric} model order, either one of 1, 1.5, or 2.
#'
#' @return A named \code{list} containing the original input \code{doe} (\code{$orig_df}), processed input ready for modeling
#' (\code{$minput_df}), a sub-list of coded and uncoded factor names (\code{$all_facs}), as well as a \code{data.frame} of
#' all main, interaction, and/or quadratic effects as appropriate for the set \code{mod_order} (\code{$all_effs}).
#' @export
#'
#' @seealso \code{\link{doe_analyze}}, \code{\link{twoway}}
#'
#' @importFrom stats setNames
#'
doe_prep <- function(doe, time_var, resp_var, uc_facs = NA, which_facs = "coded", mod_order = 2) {

  #Preliminary checks
  if(!is.data.frame(doe)) stop("Input DOE results 'doe' must be a data frame!")
  if(!any(c("coded", "uncoded") %in% which_facs) | length(which_facs)!=1) stop("Argument 'which_facs' must be one of: 'coded', 'uncoded'!")
  if(any(which_facs %in% "uncoded") & any(is.na(uc_facs))) stop("Uncoded factors were not provided!") else if(any(which_facs %in% "uncoded") & is.character(uc_facs) & !all(uc_facs %in% colnames(doe))) stop("Some or all uncoded factor names were not found in the 'doe' input data!")
  if(mod_order < 1 | mod_order > 2) stop("Argument 'mod_order' must be a numeric value equal to one of: 1, 1.5, or 2!")
  if(!all(c(time_var,resp_var) %in% colnames(doe))) stop("The order ('time_var') and response ('resp_var') variable column names must be present in the input 'doe'!")
  #Retrieve coded factors (MUST be denoted by letters of the Latin alphabet)
  c_facs <- LETTERS[LETTERS %in% colnames(doe)]
  if(length(c_facs)==0) stop("Coded factors must be denoted by capital Latin alphabet letters in 'doe'!")
  if(is.character(uc_facs) & length(c_facs)!=length(uc_facs)) stop("The length of coded factors must be equal to that of uncoded factors (if these are provided)!")

  #Data pre-processing
  #In case a character vector was provided (i.e. only coded and not actual factor levels)
  all_facs <- if(!any(is.na(uc_facs)) & is.character(uc_facs) & length(uc_facs)==length(c_facs)) list(c_facs, uc_facs) else list(c_facs)
  names(all_facs) <- if(length(all_facs)==1) "coded" else c("coded", "uncoded")

  #Get two-way interactions
  all_effs <- twoway(c_facs, mod_order)

  #Compile the model input data USING CODED OR UNCODED FACTORS
  minput_df <- if(which_facs=="coded") doe[, c_facs] else setNames(doe[, uc_facs], c_facs) #EVEN WHEN UNCODED FACTORS ARE CHOSEN, THEIR COLNAMES ARE CHANGED TO THOSE OF CODED FACTORS!
  minput_df <- cbind.data.frame(doe[,c(time_var, resp_var)], minput_df)
  colnames(minput_df)[1:2] <- c(time_var, resp_var)

  if(mod_order>1) {
    #Split the terms into individual factor names/letters
    eff_pairs <- lapply(all_effs[,"data"], function(x) strsplit(x, "")[[1]])
    eff_pairs <- eff_pairs[which(lapply(eff_pairs, length)>1)]
    for(i in seq_along(eff_pairs)) minput_df[,paste0(eff_pairs[[i]], collapse = "")] <- apply(minput_df[,eff_pairs[[i]]], 1, prod)
  } else eff_pairs <- NA

  return(list(orig_df = doe, minput_df = minput_df, all_facs = all_facs, all_effs = all_effs))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Build the model, determine inestimable factors, and trim the model if necessary
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Build a linear/quadratic model from DoE data
#'
#' @description Builds a linear or quadratic (i.e. RSM) model from input data.
#' Part of the \code{\link{doe_analyze}} workflow. \strong{This function manual is hidden from the Index}.
#'
#' @param input A \code{data.frame} of DoE input data. Must contain coded factors, as well as uncoded factors and the response variable as
#' specified in \code{uc_facs} and \code{resp_var}, respectively.
#' @param resp_var A character string specifying the name of the response variable included in \code{input}.
#' @param effs Main, interaction, and/or quadratic model effects to evaluate (as output by \code{\link{doe_prep}}).
#' @param orig_data An \strong{optional} \code{data.frame} containing the original input data to be appended to the model output
#' (mainly for reference). Defaults to \code{NA}.
#' @param uc_facs A character vector of \strong{uncoded} factor names.
#'
#' @return A linear or quadratic model of class \code{lm}.
#'
#' @export
#' @keywords internal
#'
#' @seealso \code{\link{doe_analyze}}, \code{\link{doe_prep}}
#'
#' @importFrom stats as.formula lm
#'
doe_build <- function(input, resp_var, effs, orig_data = NA, uc_facs = NA) {

  #Preliminary checks
  if(!all(c("data","formula") %in% colnames(effs))) stop("The 'effs' argument must be a data.frame including columns called: 'data', 'formula'!")

  #Generate formula and build the model
  effs_bckp <- effs
  effs_formula <- paste0(effs[,"formula"], collapse="+")
  mod_formula <- as.formula(paste0(resp_var, "~", effs_formula))

  init_model <- lm(mod_formula, data = input)

  #Create a vector for statements about the model(s) and analysis
  statements <- c()

  #Determine if any factors were not estimated due to singularities etc.
  inest_effs <- which(is.na(init_model[["coefficients"]]))

  attempt <- 1
  while(length(inest_effs)>0) {
    #Trimming formula
    effs <- effs[!effs[,"formula"] %in% names(inest_effs),]
    effs_formula <- paste0(effs[,"formula"], collapse = "+")
    mod_formula <- as.formula(paste0(resp_var, "~", effs_formula))

    #Trimming model input DF
    input <- input[,!colnames(input) %in% inest_effs]

    #Rebuilding the model without problematic terms
    init_model <- lm(mod_formula, data = input)

    inest_effs <- which(is.na(init_model[["coefficients"]]))
    if(length(inest_effs)>0) attempt <- attempt + 1
  }

  inest_effs <- effs_bckp[!effs_bckp[,"formula"] %in% effs[,"formula"],]
  #Add a coefficient name disambiguation to the model output
  coeffs <- init_model[["coefficients"]]
  coeffs <- coeffs[!names(coeffs) %in% "(Intercept)"]
  init_model[["codenames"]] <- effs[effs[,"formula"] %in% gsub(" ", "", names(coeffs)),]

  #Optionally add real (uncoded) factor names
  if(is.character(uc_facs)) {
    maineffs <- effs[which(sapply(effs[,"data"], nchar)==1), "data"]
    init_model[["realnames"]] <- uc_facs[which(maineffs %in% gsub(" ","",names(coeffs)))]
  }

  if(is.data.frame(orig_data)) init_model[["orig_df"]] <- orig_data
  if(length(inest_effs)==0) inest_effs <- "none"
  return(list(input_df = input, model = init_model, est_effs = effs, inest_effs = inest_effs, attempts = attempt))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Carry out a Lack-of-Fit test on a model
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Carry out a Lack-of-Fit test on a model
#'
#' @description Part of the \code{\link{doe_analyze}} workflow. \strong{This function manual is hidden from the Index}.
#'
#' @param input A \code{data.frame} containing the response variable (\code{resp_var}), coded and uncoded factors.
#' @param resp_var The \code{character} name of the response variable. Must be included in both \code{input} and \code{model}.
#' @param model A linear or quadratic model of class \code{lm}.
#' @param effs A \code{data.frame} of main, interaction, and/or quadratic effects,
#' in the same format output as part of \code{\link{doe_prep}}.
#'
#' @return An ANOVA \code{list} object of class \code{c("anova","data.frame")}. The output is structured identically to that of
#' \code{\link[stats]{anova}}.
#'
#' @export
#' @keywords internal
#'
#' @seealso \code{\link{doe_analyze}}
#'
#' @importFrom stats anova as.formula lm
#'
doe_lof <- function(input, resp_var, model, effs) {
  #Preliminary checks
  if(!all(c("data","formula") %in% colnames(effs))) stop("The 'effs' argument must be a data.frame with 2 columns called: 'data', 'formula'!")
  if(!all(c(resp_var,effs[,"data"]) %in% colnames(input))) stop("The response variable ('resp_var') and all effects ('effs') must be present in the 'input' data.frame!")
  if(!inherits(model, "lm")) stop("The 'model' must be of class 'lm'!")

  #Carry out Lack-of-Fit test
  lof <- factor(apply(sapply(input[,effs[,"data"]], function(x) x), 1, paste, collapse = " "))
  outer.model <- lm(as.formula(paste0(resp_var, "~lof")), data = input)
  lof <- anova(model, outer.model)
  return(lof)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Determine insignificant model effects using Stepwise Regression and/or p-value cutoff
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Determine insignificant model effects
#'
#' @description Determines insignificant effects of linear or quadratic models using Stepwise Regression and/or p-value cutoff.
#' Part of the \code{\link{doe_analyze}} workflow. \strong{This function manual is hidden from the Index}.
#'
#' @param model A linear or quadratic model of class \code{lm}.
#' @param facs A \code{character} vector of factor names included in the \code{model}.
#' @param resp_var A string specifying the response variable name.
#' @param trim_method Which method(s) of pruning insignificant effects should be applied?
#' One or more of: \code{"stepwise"} (stepwise regression, default), \code{"p_cutoff"} (p-value cutoff), or \code{"both"}.
#' @param p_cutoff The p-value cutoff to use when \code{trim_method} includes \code{"p_cutoff"} (between 0 and 1).
#' Values of 0.01-0.05 are usually appropriate based on the desired level of confidence.
#'
#' @return A named \code{list} of length 3 containing \code{data.frame} objects with significant effects (\code{$signif}),
#' as well as those removed by either stepwise regression (\code{$removed_step}) and/or p-value cutoff (\code{$removed_p}).
#'
#' @export
#' @keywords internal
#'
#' @seealso \code{\link{doe_analyze}}
#'
#' @importFrom stats as.formula step
#'
doe_insig <- function(model, facs, resp_var, trim_method="stepwise", p_cutoff = NA) {
  #Preliminary checks
  if(!inherits(model, "lm")) stop("The 'model' must be of class 'lm'!")
  if(!any(c("stepwise", "p_cutoff", "both") %in% trim_method)) stop("Argument 'trim_method' must be one of: 'stepwise', 'p_cutoff', 'both'!")
  if("both" %in% trim_method) trim_method <- c("stepwise", "p_cutoff")
  if("p_cutoff" %in% trim_method & any(is.na(p_cutoff))) stop("The 'p_cutoff' value must be provided when a p-value cutoff is included in 'trim_method'!")

  #Get an augmented summary of the model
  modsum <- aug_sum(model)

  #Get model coefficients
  coeffs <- modsum[["coefficients"]]
  coeffs <- coeffs[!rownames(coeffs) %in% "(Intercept)",]

  #Run stepwise regression and determine insignificant factors (using scope of main effects only through to full model with all effects)
  if(any(trim_method %in% "stepwise")) {
    min_effs <- paste0(facs, collapse = "+")
    minmod_formula <- as.formula(paste0(resp_var, "~", min_effs))
    mod_formula <- model[["terms"]]
    input <- model[["model"]]
    step_res <- step(model, scope = list(lower = minmod_formula, upper = mod_formula), direction = "both", trace = 0)
    facs_step <- rownames(summary(step_res)[["coefficients"]])
    insignif_step <- rownames(coeffs)[-which(rownames(coeffs) %in% facs_step)]
  } else insignif_step <- c()

  #Determine insignificant factors based on p-value cutoff
  insignif_pval <- if(any(trim_method %in% "p_cutoff")) rownames(coeffs)[which(coeffs[,"Pr(>|t|)"]>p_cutoff)] else c()

  #Use both stepwise regression and p-value cutoff to compile a final vector of significant factors
  signif_effs <- rownames(coeffs)[!rownames(coeffs) %in% unique(c(insignif_step, insignif_pval))]
  res <- list(signif = signif_effs, removed_step = insignif_step, removed_p = insignif_pval)
  res <- lapply(res, function(x) model[["codenames"]][model[["codenames"]][,"formula"] %in% gsub(" ","",x),])
  return(res)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Apply the Hierarchy Principle (optionally, usually) to add some potentially significant effects back to the model
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Apply the Hierarchy Principle to a list of model effects
#'
#' @description Applies the Hierarchy Principle to add main effects which are currently present as part of
#' any interaction and/or quadratic effects.
#' Part of the \code{\link{doe_analyze}} workflow. \strong{This function manual is hidden from the Index}.
#'
#' @param effs A \code{data.frame} of model effects, e.g. output as part of \code{\link{doe_prep}}.
#'
#' @return A named \code{list} containing the original effects (\code{$old_effs}), new updated effects (\code{$new_effs}),
#' and the difference between the two, i.e. added effects (\code{$added_effs}, if any).
#'
#' @export
#' @keywords internal
#'
#' @seealso \code{\link{doe_analyze}}
doe_hrp <- function(effs) {

  #Preliminary checks
  if(!all(c("data","formula") %in% colnames(effs))) stop("The 'effs' argument must be a data.frame with 2 columns called: 'data', 'formula'!")

  effs_dt <- effs[,"data"]

  splits <- strsplit(effs_dt, "")
  hp_list <- list()
  hp_list[["mains"]] <- effs_dt[which(sapply(splits, length)==1)]
  hp_list[["uniques"]] <- unique(unlist(splits[which(sapply(splits, length)>1)]))
  hp_list <- lapply(hp_list, function(x) x[order(x)])

  signif_maxlen <- which.max(sapply(hp_list, length))
  signif_minlen <- which.min(sapply(hp_list, length))

  hp_effs <- hp_list[[signif_maxlen]]
  effs_bckp <- effs_dt
  effs_dt <- unique(c(hp_effs, effs_dt))
  added_effs <- if(!all(hp_effs %in% effs_bckp)) setdiff(hp_effs, effs_bckp) else "none"

  mainchk <- which(!effs_dt %in% effs_bckp & nchar(effs_dt)==1)
  res <- list(old_effs = effs_bckp, new_effs = effs_dt, added_effs = added_effs)

  if(length(mainchk)>0) {
    for(i in mainchk) {
      #Find the last row where nchar is 1
      ncrle <- rle(nchar(effs[["data"]]))
      ncrle[["clen"]] <- cumsum(ncrle[["lengths"]])
      lastchar <- ncrle[["clen"]][which(ncrle[["values"]]==1)[1]]
      effs <- rbind.data.frame(effs[seq(lastchar),], rep(effs_dt[i],3), effs[(lastchar+1):nrow(effs),])
    }
  }
  res <- lapply(res, function(x) effs[effs[,"data"] %in% x,])
  return(res)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Code or decode one or more DOE model values from the results
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title (De)code one or more model results
#'
#' @description Uses linear interpolation to (de)code any number of provided factor values.
#' Part of the \code{\link{doe_analyze}} workflow. \strong{This function manual is hidden from the Index}.
#'
#' @param input A named \code{numeric} vector or \code{data.frame} containing values to (de)code.
#' Names must match either \code{c_facs} or \code{uc_facs} based on \code{task}.
#' @param modf A \code{data.frame} of model input data containing \strong{both} \code{c_facs} and \code{uc_facs}.
#' @param c_facs,uc_facs Character vectors of coded and uncoded factor names included in \code{modf}, respectively.
#' @param task String specifying what should be done with \code{input} values. One of: \code{"code"} or \code{"decode"} (default).
#' @param reassemble Should the \strong{new} (coded or decoded) values be converted into a list format? Defaults to \code{FALSE}.
#'
#' @return A named \code{list} containing the input minima (\code{$olmins}, either coded or decoded as appropriate for the given \code{task}),
#' input maxima (\code{$olmaxes}), corresponding \strong{output} minima and maxima (\code{$mins} and \code{$maxes}),
#' and the output factor values (\code{$newvals}).
#'
#' @export
#' @keywords internal
#'
#' @seealso \code{\link{doe_analyze}}
doe_decode <- function(input, modf, c_facs, uc_facs, task = "decode", reassemble = FALSE) {

  #Preliminary checks
  if(!all(c(c_facs,uc_facs) %in% colnames(modf))) stop("All coded and uncoded factors ('c_facs' and 'uc_facs') must be present in the input 'df'!")
  listflag <- vecflag <- FALSE
  if(is.list(input) & !is.data.frame(input)) {
    if(length(unique(lengths(input)))!=1) {
      stop("When 'input' is a list, its elements must be equal in length!")
    } else if(!is.null(names(input[[1]]))) { #COMBINE LIST ROW-WISE
      bckp_nms <- names(input[[1]])
      input <- do.call(rbind.data.frame, input)
      colnames(input) <- bckp_nms
    } else input <- do.call(cbind.data.frame, input) #COMBINE LIST COLUMN-WISE
    listflag <- TRUE
  } else if(is.vector(input)) {
    input <- as.data.frame(t(input))
    vecflag <- TRUE
  }

  if(!all(names(input) %in% colnames(modf))) stop("All the input factor names must be present in input data frame!")

  if((task=="decode" & !any(c_facs %in% names(input)))|
     task=="code" & !any(uc_facs %in% names(input))) stop("None of the necessary factors for (de)coding are included in the 'input' data!")

  #Begin (de)coding input data
  fc <- if(task=="decode") which(c_facs %in% names(input)) else which(uc_facs %in% names(input))
  oldfacs <- if(task=="decode") c_facs[fc] else uc_facs[fc]
  newfacs <- if(task=="decode") uc_facs[fc] else c_facs[fc]

  old_min <- old_max <- new_min <- new_max <- new_vals <- c()

  #Decoding optimum values and appending to canonical analysis results
  new_vals <- data.frame()
  for(i in oldfacs) {
    for(j in seq(nrow(input))) {
      old_val <- input[j,i]
      old_min[i] <- min(range(modf[,i], na.rm = TRUE), na.rm = TRUE)
      old_max[i] <- max(range(modf[,i], na.rm = TRUE), na.rm = TRUE)

      newsub <- which(oldfacs %in% i)
      new_min[i] <- min(range(modf[,newfacs[newsub]], na.rm = TRUE), na.rm = TRUE)
      new_max[i] <- max(range(modf[,newfacs[newsub]], na.rm = TRUE), na.rm = TRUE)
      new_vals[j,i] <- (((old_val - old_min[i])*(new_max[i]-new_min[i]))/(old_max[i]-old_min[i]))+new_min[i]
    }
  }
  if(reassemble & listflag) new_vals <- as.list(new_vals)
  if(vecflag) new_vals <- unlist(new_vals)
  return(list(olmins = old_min, olmaxes = old_max, mins = new_min, maxes = new_max, newvals = new_vals))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Optionally optimize the model for either minimum or maximum response
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Optimize the model for either minimum or maximum response
#'
#' @description Uses \code{\link[stats]{optim}} to both minimize and maximize the response variable in a linear/quadratic model.
#'
#' @param model A linear or quadratic model of class \code{lm}.
#' @param mod_df A \code{data.frame} of model input data containing \strong{both} \code{c_facs} and \code{uc_facs}.
#' @param resp_var A string specifying the response variable name.
#' @param effs A \code{data.frame} of model effects, e.g. output as part of \code{\link{doe_prep}}.
#' @param c_facs,uc_facs Character vectors of coded and uncoded factor names included in \code{model} (and \code{mod_df}), respectively.
#'
#' @return The \code{model} summary object of class \code{"summary.lm"}, including a \code{$tradopt} list element
#'
#' @seealso \code{\link{doe_analyze}}
#'
#' @importFrom stats optim predict setNames
#'
doe_optim <- function(model, mod_df, resp_var, effs, c_facs, uc_facs = NA) {

  #Remove main effects which are not present in the model
  mainchk <- c_facs %in% colnames(model[["model"]])
  c_facs <- c_facs[mainchk]
  if(!all(is.na(uc_facs))) uc_facs <- uc_facs[mainchk]

  #Get an augmented summary of the model
  modsum <- aug_sum(model)

  #Optimize model predictions (either maximize or minimize response)
  opt_doe <- function(pars, optmod) {
    newdt <- setNames(as.data.frame(t(pars)), c_facs)
    predict(optmod, newdata = newdt)
  }

  opt_bounds <- range(mod_df[,c_facs], na.rm = TRUE)

  #Minimize response
  min_resp <- optim(rep(0,length(c_facs)), opt_doe, method = "L-BFGS-B", lower = min(opt_bounds), upper = max(opt_bounds), control = list(fnscale=1), optmod = model)

  #Maximize response
  max_resp <- optim(rep(0,length(c_facs)), opt_doe, method = "L-BFGS-B", lower = min(opt_bounds), upper = max(opt_bounds), control = list(fnscale=-1), optmod = model)

  #Determine the maximum AND minimum predicted value
  modsum[["tradopt"]][["coded"]] <- setNames(rbind.data.frame(c(min_resp[["par"]], min_resp[["value"]]), c(max_resp[["par"]], max_resp[["value"]])), c(c_facs,resp_var))

  #(Optionally) Decode the optimization results
  if(!any(is.na(uc_facs)) & identical(opt_bounds, range(model[["model"]][,c_facs], na.rm = TRUE))) {
    #Decoding optimum values (obtained via optim())
    modsum[["tradopt"]][["decoded"]] <- doe_decode(modsum[["tradopt"]][["coded"]], mod_df, c_facs, uc_facs, task = "decode")[["newvals"]]
  }
  return(modsum)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Summarize the model results
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Create a DOE model analysis summary
#'
#' @description Compiles a complete analysis of DoE model results.
#' Part of the \code{\link{doe_analyze}} workflow. \strong{This function manual is hidden from the Index}.
#'
#' @param model A model built using \code{\link{doe_build}}.
#' @param mod_sum Summary of \code{model} obtained using \code{\link{aug_sum}}.
#' @param mod_anova The results of \code{anova(model)}.
#' @param mod_lof The results of Lack-of-Fit testing obtained via \code{\link{doe_lof}}.
#' @param mod_df The initial \code{data.frame} used as model input.
#' @param code_names A \code{logical} indicating whether coefficient names should be switched with codenames
#' contained in \code{mod_sum}.
#'
#' @return A named \code{list} containing a complete DoE model analysis. For further details, see \code{\link{doe_analyze}}.
#'
#' @export
#' @keywords internal
#'
#' @seealso \code{\link{doe_analyze}}, \code{\link{doe_build}}, \code{\link{aug_sum}}, \code{\link{doe_lof}}
doe_summary <- function(model, mod_sum, mod_anova, mod_lof, mod_df, code_names = TRUE) {

  #Fix the row order of ANOVA objects
  mod_anova <- mod_anova[c(nrow(mod_anova),seq(nrow(mod_anova)-1)),]

  sumres <- list()

  #Compile results from the revised and final models in a list
  #Compile coefficients and other general results
  mod_coeff <- mod_sum[["coefficients"]]

  #Optionally switch coefficient names to codenames
  if(code_names) rownames(mod_coeff) <- c("Intercept", mod_sum[["codenames"]])

  mod_coeff <- cbind.data.frame(rownames(mod_coeff), mod_coeff[,"Estimate"], mod_sum[["aliased"]], mod_coeff[,!colnames(mod_coeff) %in% "Estimate"])
  mod_coeff[,"Signif_Level"] <- as.numeric(as.character(cut(mod_coeff[,ncol(mod_coeff)],
                                                            c(0, 0.001, 0.01, 0.05, 0.10, 1.00),
                                                            c(0.999, 0.99, 0.95, 0.90, 0.00))))
  mod_coeff <- do.call(cbind.data.frame, list(mod_coeff,
                                              mod_anova[["Sum Sq"]],
                                              mod_anova[["Mean Sq"]],
                                              mod_anova[["F value"]]))
  rownames(mod_coeff) <- NULL
  colnames(mod_coeff) <- c("Term", "Estimate", "Aliased", "Stnd_Error", "t_value", "p_value", "Signif_Level", "Sum_Sq_ANOVA", "Mean_Sq_ANOVA", "F_value_ANOVA")

  sumres[["Model_Results"]] <- mod_coeff

  #Get model input data with residuals
  sumres[["Model_Data"]] <- cbind.data.frame(mod_df, "Residual" = mod_sum[["residuals"]])

  #Get original data
  if("orig_df" %in% names(model)) sumres[["Orig_Data"]] <- model[["orig_df"]]

  #Get model performance metrics
  mod_metrics <- list("Order" = mod_sum[["order"]],
                      "R2" =  mod_sum[["r.squared"]],
                      "Adj_R2" =  mod_sum[["adj.r.squared"]],
                      "Residual_Stnd_Error" =  mod_sum[["sigma"]],
                      "F_Statistic" =  unname(mod_sum[["fstatistic"]][1]),
                      "F_DOF_1" =  unname(mod_sum[["fstatistic"]][2]),
                      "F_DOF_2" =  unname(mod_sum[["fstatistic"]][3]),
                      "LoF_Pvalue" = mean(mod_lof[["Pr(>F)"]], na.rm = TRUE))

  if(mod_sum[["order"]]>1) mod_metrics[["Canonical_Analysis"]] <- mod_sum[["canonical"]] else mod_metrics[["Steepest_Ascent_Linear"]] <- mod_sum[["sa_lin"]]
  if(mod_sum[["order"]]==2 & "steepest_ascent" %in% names(mod_sum)) mod_metrics[["Steepest_Ascent"]] <- mod_sum[["steepest_ascent"]]

  #General-purpose optimization results
  mod_metrics[["Trad_Opt"]] <- mod_sum[["tradopt"]]

  sumres[["Model_Metrics"]] <- mod_metrics

  #Miscellaneous info
  misc_info <- list()
  factors <- rownames(attr(model[["terms"]], "factors"))
  terms <- attr(model[["terms"]], "term.labels")
  misc_info[["Response"]] <- factors[!factors %in% terms]
  misc_info[["Runs"]] <- nrow(mod_df)
  if("realnames" %in% names(mod_sum)) misc_info[["Factor_Names"]] <- mod_sum[["realnames"]]

  #Add model equation
  #Construct data frame from which to compile model equation
  eq_cfs <- model[["coefficients"]]
  intloc <- grep("Intercept", names(eq_cfs))
  yint <- eq_cfs[[intloc]]
  cfs <- unname(eq_cfs[-intloc])
  eqs <- c()
  eq_trms <- model[["codenames"]][,"label"]
  eqs["raw"] <- paste0(round(yint,3), " + ", paste0(round(cfs,3), "*", eq_trms, collapse = " + "))
  eqs["coded"] <- paste0(yint, " + ", paste0(cfs, "*", eq_trms, collapse = " + "))
  for(i in seq_along(LETTERS)) eqs["coded"] <- gsub(LETTERS[i], paste0("x[",i,"]"), eqs["coded"], fixed = TRUE)
  eqs <- sapply(eqs, function(x) gsub("\\+[[:space:]]\\-", "- ", x))
  misc_info[["Equation"]] <- eqs #data.frame(type = c("raw","coded"), equation = c(eq_raw, eq_coded))

  sumres[["Misc"]] <- misc_info
  return(sumres)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Compile a vector of statements about the workflow
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Compile a list of statements about the DoE analysis
#'
#' @description Generates a set of descriptive statements about DoE analysis results.
#' Part of the \code{\link{doe_analyze}} workflow. \strong{This function manual is hidden from the Index}.
#'
#' @param initres,finres Results of the \strong{initial} and \strong{final} DoE models built as part of \code{\link{doe_analyze}}.
#' @param signif Results of \code{\link{doe_insig}}.
#' @param hrp Results of \code{\link{doe_hrp}}.
#' @param sums Results of \code{\link{aug_sum}}.
#' @param lofs Results of \code{\link{doe_lof}}.
#' @param misc Additional results retrieved from the output of \code{\link{doe_summary}}.
#' @param labs A \code{character} vector of length 2 containing labels for the initial and final models. Defaults to \code{c("initial", "final")}.
#' @param print_res A \code{logical} indicating whether the generated statements should be printed into the console. Defaults to \code{FALSE}.
#'
#' @return A named \code{character} vector of descriptive statements about DoE analysis.
#'
#' @export
#' @keywords internal
#'
#' @seealso \code{\link{doe_analyze}}
doe_state <- function(initres = NA, finres = NA, signif = NA, hrp = NA, sums = NA, lofs = NA, misc = NA, labs = c("initial","final"), print_res = TRUE) {

  #Create a vector for statements about the model(s) and analysis
  statements <- c()

  #PART 1: INITIAL MODEL RESULTS
  #Inestimable effects (initial model)
  if(!any(is.na(initres))) {
    statements["Inestimable"] <- if(nrow(initres[["inest_effs"]])>0) paste0("NOTE: Model was unable to estimate ", nrow(initres[["inest_effs"]]), " coefficients after ", initres[["attempts"]], "attempts : ", paste0(initres[["inest_effs"]][,"data"], collapse=", "), ". These were removed and the model revised...")
    else "All model coefficients successfully estimated on the first attempt."
  }

  #Initial model order and terms
  if(!any(is.na(sums)) & any(names(sums) %in% labs[1])) {
    statements[paste0("Mod_",labs[1])] <- paste0("Initial model is of order ", sums[[labs[1]]][["order"]], " with the following terms: ", paste0(sums[[labs[1]]][["codenames"]], collapse = ", "), ".")
    statements[paste0("Eq_", labs[1])] <- paste0("The model equation is: ", misc[[labs[1]]][["Misc"]][["Equation"]][["raw"]])

    #R2 assessment (initial model)
    init_r2 <- sums[[labs[1]]][["r.squared"]]
    init_adjr2 <- sums[[labs[1]]][["adj.r.squared"]]
    init_r2_diff <- init_r2 - init_adjr2
    rel_r2_init <- if(init_r2_diff < 0) "higher" else "lower"
    statements[paste0("R2_vs_adjR2_",labs[1])] <- paste0("Adjusted R^2 of the initial model (", round(init_adjr2,2),") is ", rel_r2_init, " than R^2 (",  round(init_r2,2),") by ", round(init_r2_diff, 2), ".")

  }

  #Initial model Lack-of-Fit test results
  if(!any(is.na(lofs)) & any(names(lofs) %in% labs[1])) {
    statements[paste0("LoF_",labs[1])] <- paste0("ANOVA Lack-of-Fit testing of the initial model revealed a p value of ", round(mean(lofs[[labs[1]]][["Pr(>F)"]], na.rm = TRUE), 4), ".")
  }

  #Results of trimming out insignificant variables (initial model)
  if(!any(is.na(signif))) {
    stepchk <- nrow(signif[["removed_step"]])>0
    pchk <- nrow(signif[["removed_p"]])>0
    if(stepchk) statements["Stepwise"] <- paste0("Stepwise regression based on AIC removed ", nrow(signif[["removed_step"]]), " factors from the initial model: ", paste0(signif[["removed_step"]][,"data"], collapse = ", "), ".")
    if(pchk) statements["p_cutoff"] <- paste0("A p-value cutoff removed ", nrow(signif[["removed_p"]]), " factors from the initial model: ", paste0(signif[["removed_p"]][,"data"], collapse = ", "), ".")
    if(!any(c(stepchk,pchk))) {
      statements["Notrim"] <- "The initial model was not truncated via either a p-value cutoff or stepwise regression."
      statements["Signif_Effs"] <- "No effects were removed from the model."
    } else {
      trim_type <- paste0(c("stepwise regression", "p-value cutoff")[which(c(stepchk,pchk))], collapse = " and ")
      statements["Signif_Effs"] <- paste0("After removing insignificant effects via ", trim_type, " method(s), the following ", nrow(signif[["signif"]]), " were left in the model: ", paste0(signif[["signif"]][,"data"], collapse = ", "), ".")
    }
  } else statements["Signif_Effs"] <- "No effects were removed from the model since all were significant."

  #Hierarchy Principle
  if(!any(is.na(hrp))) {
    statements["H_Principle"] <- if(nrow(hrp[["added_effs"]])>0) {
      paste0("The following main effects were added to the model upon applying the Hierarchy Principle: ", paste0(hrp[["added_effs"]][,"data"], collapse = ", "), ". The full list of terms included is now: ", paste0(hrp[["new_effs"]][,"data"], collapse = ", "), ".")
    } else "No effects were added to the model upon applying the Hierarchy Principle."
  }

  #PART 2: FINAL MODEL RESULTS
  #Final model order and terms
  if(!any(is.na(sums)) & any(names(sums) %in% labs[2])) {
    statements[paste0("Mod_",labs[2])] <- paste0("Final model is of order ", sums[[labs[2]]][["order"]], " with the following terms: ", paste0(sums[[labs[2]]][["codenames"]], collapse = ", "), ".")
    statements[paste0("Eq_", labs[2])] <- paste0("The model equation is: ", misc[[labs[2]]][["Misc"]][["Equation"]][["raw"]])

    #R2 assessment (final model)
    fin_r2 <- sums[[labs[2]]][["r.squared"]]
    fin_adjr2 <- sums[[labs[2]]][["adj.r.squared"]]
    fin_r2_diff <- fin_r2 - fin_adjr2
    rel_r2_fin <- if(fin_r2_diff < 0) "higher" else "lower"
    statements[paste0("R2_vs_adjR2_",labs[2])] <- paste0("Adjusted R^2 of the final model (", round(fin_adjr2,2),") is ", rel_r2_fin, " than R^2 (",  round(fin_r2,2),") by ", round(fin_r2_diff, 2), ".")
  }

  #Initial model Lack-of-Fit test results
  if(!any(is.na(lofs)) & any(names(lofs) %in% labs[2])) {
    statements[paste0("LoF_",labs[2])] <- paste0("ANOVA Lack-of-Fit testing of the final model revealed a p value of ", round(mean(lofs[[labs[2]]][["Pr(>F)"]], na.rm = TRUE), 4), ".")
  }

  #PART 3: FINAL MODEL OPTIMIZATION
  if(!any(is.na(sums))) {

    for(i in seq_along(sums)) {

      modlab <- names(sums)[i]
      modsum <- sums[[i]]

      #Canonical Analysis Optimization
      if("canonical" %in% names(modsum)) {

        facs <- names(modsum[["canonical"]][["xs"]])
        eigen_vals <- round(modsum[["canonical"]][["eigen"]][["values"]],3)
        coded <- round(modsum[["canonical"]][["xs"]], 3)
        decoded <- if("xs_decoded" %in% names(modsum[["canonical"]])) round(modsum[["canonical"]][["xs_decoded"]], 3) else rep("-", length(facs))
        out_range <- modsum[["canonical"]][["outside_range"]]
        canon_pred <- round(modsum[["canonical"]][["predicted"]], 3)

        statements[paste0("Optim_1_", modlab)] <- paste0("The ", modlab, " model yielded the following stationary point (uncoded values in brackets if available): ",
                                                         paste0(facs, " = ", coded, " (", decoded, ")", collapse = ", "), ", with eigenvalues: ",
                                                         paste0(facs, " = ", eigen_vals, collapse = ", "), ". ")
        if("outside_range" %in% names(modsum[["canonical"]])) statements[paste0("Optim_2_", modlab)] <- paste0("NOTE: ", length(which(out_range)), " of ", length(out_range), " optimum decoded CA values are out of range!")
        statements[paste0("Optim_3_", modlab)] <- paste0("NOTE: ", length(which(eigen_vals>0)), " of ", length(facs), " CA eigen values are positive!")
        statements[paste0("Optim_4_", modlab)] <- paste0("The stationary point found by canonical analysis is a ", if(all(eigen_vals < 0)) "maximum" else if(all(eigen_vals > 0)) "minimum" else "saddle point", ".")
        statements[paste0("Optim_5_", modlab)] <- paste0("Model prediction at the stationary point yields the response value: ", canon_pred, ".")
      }

      #Classical Optimization (via optim())
      if("tradopt" %in% names(modsum)) {

        desc <- c("MINIMUM", "MAXIMUM")
        for(j in 1:2) {
          coded <- modsum[["tradopt"]][["coded"]][j,]
          facs <- rownames(modsum[["coefficients"]])[which(rownames(modsum[["coefficients"]]) %in% names(coded))]
          resp <- round(coded[!names(coded) %in% facs],3)
          coded <- round(coded[names(coded) %in% facs],3)
          decoded <- if("decoded" %in% names(modsum[["tradopt"]])) round(modsum[["tradopt"]][["decoded"]][j,], 3) else rep("-", length(facs))

          statements[paste0("TradOpt_", j, "_", modlab)] <- paste0("The ", modlab, " model yielded the following ", desc[j], " response value: ", resp,
                                                                   ", obtained via the following parameter values (uncoded values in brackets if available): ",
                                                                   paste0(facs, " = ", coded, " (", decoded, ")", collapse = ", "), ".")
        }
      }
    }
  }

  #Optionally print statements
  if(print_res) cat(paste0("\n", statements), sep = "")
  return(statements)
}
#END OF DATA PROCESSING FUNCTIONS

#BEGINNING OF VISUALISATION FUNCTIONS. The following have been shelved for now:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Box Plots of response/residual versus coded/uncoded factor levels
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Create Box plots from DoE analysis results
#'
#' @description Part of the \code{\link{doe_analyze}} workflow. \strong{This function manual is hidden from the Index}.
#'
#' @param mod_df The \code{data.frame} used as DoE model input.
#' @param resids A \code{numeric} vector of model residuals.
#' @param resp_var A \code{character} string specifying the response variable name.
#' @param facs A named \code{list} of length 2 containing the \code{$coded} and \code{$uncoded} factor names.
#' @param cols Plot colours. Either \code{"default"}, or a \strong{named} \code{numeric} vector of any of the following colours:
#' outlier outline (\code{["outcol"]}), outlier fill (\code{["outfill"]}), box fill (\code{["outcol"]}).
#' @param asprat The plot aspect ratio. Defaults to 1.
#' @param modlab The model name/label as a \code{character} string (none by default).
#'
#' @return A \code{list} of \code{ggplot} box plot objects.
#'
#' @export
#' @keywords internal
#'
#' @seealso \code{\link{doe_analyze}}
#'
#' @importFrom data.table as.data.table melt
#' @import ggplot2
#'
doe_box <- function(mod_df, resids, resp_var, facs, cols = "default", asprat = 1, modlab = "") {

  #Preliminary checks
  modlab <- scap(modlab)
  if(!resp_var %in% colnames(mod_df)) stop("The response variable 'resp_var' must be present in the 'mod_df' data!")
  if(!is.list(facs) | length(facs)>2) stop("Factors ('facs') must be a list of length 1 or 2 (containing coded and, optionally, uncoded factor names!")
  if(!is.vector(resids) | !is.atomic(resids) | length(resids)!=nrow(mod_df)) stop("Model residuals 'resids' must be an atomic vector!")

  #if(is.null(names(cols)) & !any(cols %in% "default")) stop("The colour vector 'cols' must be named! Possible names are: 'outcol', 'outfill', 'box'.")
  #defcols <- c(outcol = "black", outfill = "#DE0000", box = "#3BB826")
  #if(any(cols %in% "default")) cols <- defcols else {
  #  abs_cols <- which(!names(defcols) %in% names(cols))
  #  cols <- append(cols, defcols[abs_cols])[names(defcols)]
  #}

  #Set up plot colours
  defcols <- c(outcol = "black", outfill = "#DE0000", box = "#3BB826")
  cols <- supp_pars(pars = cols, defpars = defcols, parlb = "cols")

  molten_does <- res_plots <- list()

  #Final model residual analysis
  box_dfs <- rep(list(mod_df, cbind.data.frame(mod_df, "Residual" = resids)),
                 each = length(facs)) #If both coded and uncoded factors were provided, length(all_facs)==2, otherwise 1!
  box_idvar <- rep(c(resp_var, "Residual"), each = length(facs))
  molten_does <- list()
  box_facs <- rep(facs, 2) #all_facs are duplicated here because they need to be repeated for response and residuals

  for(i in seq_along(box_facs)) {

    molten_does[[i]] <- suppressWarnings(data.table::melt(data.table::as.data.table(box_dfs[[i]]), measure.vars = box_facs[[i]], id.vars = box_idvar[i], variable.name = "Factor", value.name = "Value"))
    molten_does[[i]] <- as.data.frame(molten_does[[i]])
    molten_does[[i]][,"Value"] <- as.character(molten_does[[i]][,"Value"])
    fac_lvls <- as.numeric(unique(molten_does[[i]][,"Value"]))
    fac_lvls <- fac_lvls[order(fac_lvls, decreasing = FALSE)]
    molten_does[[i]][,"Value"] <- factor(molten_does[[i]][,"Value"], levels = as.character(fac_lvls))

    #Define vars for new ggplot3.4.0 aes (after aes_string deprecation)
    aes_xvar <- "Value"

    res_plots[[paste0(modlab, "Model_", box_idvar[i], "_vs_",scap(names(box_facs)[i]))]] <- ggplot(molten_does[[i]], aes(x = .data[[aes_xvar]], y = .data[[box_idvar[i]]])) +
      stat_boxplot(geom = "errorbar", width = 0.2) +
      geom_boxplot(outlier.shape = 21, outlier.color = cols[1],
                   outlier.fill = cols[2], outlier.size = 3, fill = cols[3]) +
      facet_wrap(~Factor, scales = "free") +
      labs(x = "Factor Level", y = scap(box_idvar[i]), title = paste0(modlab, " Model ", scap(box_idvar[i])," vs. ", scap(names(box_facs)[i]), " Factors")) +
      theme(aspect.ratio = asprat,
            panel.background = element_blank(),
            panel.grid.major.y = element_line(colour = "grey60", linetype = 3),
            plot.title = element_text(colour = "black"),
            axis.line = element_line(colour = "black", linetype = 1),
            axis.text = element_text(size = 11, colour = "black"),
            axis.title = element_text(size = 12, colour = "black"),
            strip.background = element_rect(fill = "grey90"),
            strip.text = element_text(size = 12, colour = "black"))
  }
  return(res_plots)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: EDA Plots (Exploratory Data Analysis)
#Includes: Normal Q-Q, Frequency Histogram, Box, Run Order plots
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Create Exploratory Data Analysis (EDA) plots from DoE analysis results
#'
#' @description Part of the \code{\link{doe_analyze}} workflow. \strong{This function manual is hidden from the Index}.
#'
#' @param mod_df The input \code{data.frame} used to build the model via \code{\link{doe_build}}.
#' @param mod_res Model summary results obtained from \code{\link{doe_summary}}.
#' @param time_var,resp_var Strings specifying the run order and response variable names, respectively.
#' @param cent_id A \code{character} identifier for center point highlighting. Alternatively, if set to \code{NA} (default),
#' center points are \strong{not} highlighted in the plots.
#' @param cols Plot colours. Either \code{"default"}, or a \strong{named} \code{numeric} vector of any of the following colours:
#' outlier outline (\code{"outcol"}), outlier fill (\code{"outfill"}), point/box fill (\code{"fill"}), center point fill (\code{"cp"}).
#' @param asprat The plot aspect ratio. Defaults to 1.
#' @param modlab The model name/label as a \code{character} string (none by default).
#'
#' @return A \code{list} of \code{ggplot} EDA plot objects.
#'
#' @export
#' @keywords internal
#'
#' @seealso \code{\link{doe_analyze}}
#'
#' @importFrom gridExtra arrangeGrob
#' @importFrom stats qnorm
#' @import ggplot2
#'
doe_eda <- function(mod_df, mod_res, time_var, resp_var, cent_id = NA, cols = "default", asprat = 1, modlab = "") {

  #Preliminary checks
  modlab <- scap(modlab)
  if(length(cent_id)!=1) stop("Argument 'cent_id' must be a character value of length 1 (or set to NA)!")
  if(!resp_var %in% colnames(mod_df)) stop("The response variable 'resp_var' must be present in the 'mod_df' data!")

  #Set up plot colours
  defcols <- c(outcol = "black", outfill = "#DE0000", fill = "#3BB826", cp = "#E7972D")
  cols <- supp_pars(pars = cols, defpars = defcols, parlb = "cols")

  #EDA Plots for response and final model residual
  eda_plots <- gridded_plots <- list()
  eda_data <- mod_res[["Model_Data"]]

  if(is.character(cent_id) & !is.na(cent_id)) {
    group_col <- unique(names(mod_df)[which(mod_df == cent_id, arr.ind=T)[, "col"]])
    eda_data <- cbind.data.frame(eda_data, mod_df[,group_col])
    colnames(eda_data)[ncol(eda_data)] <- group_col
  }

  for(i in c(resp_var, "Residual")) {

    eda_var <- paste(scap(modlab), "Model,", i)

    #Normal Q-Q Plot
    eda_plots[[paste0(modlab, "_QQ")]] <- ggplot(eda_data, aes(sample = .data[[i]])) +
      geom_qq(distribution = stats::qnorm, pch = 21, colour = cols[1], fill = cols[3], size = 3) +
      geom_qq_line(distribution = stats::qnorm, colour = cols[2], lty=1) +
      labs(title = paste0("Normal Q-Q for ", eda_var), x = "Theoretical Quantiles", y = "Sample Quantiles")

    #Frequency Histogram
    eda_plots[[paste0(modlab, "_Hist")]] <- ggplot(eda_data, aes(x = .data[[i]])) +
      geom_histogram(bins = 7, colour = cols[1], fill = cols[3]) + #binwidth = round(max(range(eda_data[,eda_var], na.rm=FALSE))/7, 0)
      labs(title = paste0("Histogram - ", eda_var), x = i, y = "Frequency")

    #Box Plot
    eda_plots[[paste0(modlab, "_Box")]] <- ggplot(eda_data, aes(x = .data[[i]])) +
      stat_boxplot(geom = "errorbar", width = 0.2) +
      geom_boxplot(width = 0.4, outlier.shape = 21, outlier.color = cols[1],
                   outlier.fill = cols[2], outlier.size = 3, fill = cols[3]) +
      scale_y_continuous(limits = c(-0.5, 0.5)) +
      labs(title = paste0("Box Plot of ", eda_var), x = i, y = "Arbitrary")

    #Run Order (i.e. Time) Plot
    eda_plots[[paste0(modlab, "_Time")]] <- ggplot(eda_data, aes(x = .data[[time_var]], y = .data[[i]])) +
      geom_point(pch = 21, colour = cols[1], fill = cols[3], size = 3) +
      labs(title = paste0("Run Order Plot - ", eda_var), x = "Actual Run Order", y = i)

    #Add center points (optional)
    if(is.character(cent_id) & !is.na(cent_id)) {

      #Define vars for new ggplot3.4.0 aes (after aes_string deprecation)
      aes_grpvar <- "Group"

      eda_plots[[paste0(modlab, "_Time")]] <- eda_plots[[paste0(modlab, "_Time")]] + geom_point(data = eda_data[eda_data[,group_col]==cent_id,], aes(fill = .data[[aes_grpvar]], colour = .data[[aes_grpvar]]), pch = 21, size = 3.5) +
        scale_fill_manual(values=c("CP" = cols[4]), labels = c("Center Points")) +
        scale_colour_manual(values=c("CP" = cols[1]), labels = c("Center Points"))
    }

    #Add theme to EDA plots
    for(j in seq_along(eda_plots)) {
      eda_plots[[j]] <- eda_plots[[j]] +
        theme(aspect.ratio = asprat,
              plot.margin = unit(rep(0.2, 4), "cm"),
              panel.background = element_blank(),
              legend.title = element_blank(),
              legend.key = element_rect(fill = "grey90", colour = "black"),
              legend.position.inside = c(0.85, 0.95),
              legend.text = element_text(size = 11),
              panel.grid.major = element_line(colour = "grey60", linetype = 3),
              plot.title = element_text(colour = "black", size = 12.5),
              axis.line = element_line(colour = "black", linetype = 1),
              axis.text = element_text(size = 11, colour = "black"),
              axis.title = element_text(size = 12, colour = "black"))
    }

    #Additional edits for box plot only
    eda_plots[[paste0(modlab, "_Box")]] <- eda_plots[[paste0(modlab, "_Box")]] +
      theme(axis.text.y = element_text(size = 11, colour = "transparent"),
            axis.line.y = element_line(colour = "transparent"),
            axis.title.y = element_text(size = 12, colour = "transparent"),
            axis.ticks.y = element_line(colour = "transparent"))

    #Arrange in 2x2 rectangular grid
    gridded_plots[[eda_var]] <- do.call(gridExtra::arrangeGrob, c(eda_plots, ncol=2)) #Suitable to save with ggsave() later
  }
  return(gridded_plots)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Residuals versus predicted values for models
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Create a residuals plot from DoE analysis results
#'
#' @description Part of the \code{\link{doe_analyze}} workflow. \strong{This function manual is hidden from the Index}.
#'
#' @param model The linear or quadratic model built using \code{\link{doe_build}}.
#' @param resp_var A \code{character} string specifying the response variable name.
#' @param resids A \code{numeric} vector of model residuals.
#' @param cols Plot colours. Either \code{"default"}, or a \strong{named} \code{numeric} vector of any of the following colours:
#' colour (\code{"col"}), fill (\code{"fill"}), line colour (\code{"line"}), and center point fill (\code{"cp"}).
#' @param asprat The plot aspect ratio. Defaults to 1.
#' @param modlab The model name/label as a \code{character} string (none by default).
#'
#' @return A \code{list} of \code{ggplot} residual plot objects.
#'
#' @export
#' @keywords internal
#'
#' @seealso \code{\link{doe_analyze}}
#'
#' @importFrom stats predict
#' @import ggplot2
#'
doe_resid <- function(model, resp_var, resids, cols = "default", asprat = 1, modlab = "") {

  #Preliminary checks
  modlab <- scap(modlab)
  if(!inherits(model, "lm")) stop("The input 'model' must be of class 'lm'!")
  if(!is.vector(resids) | !is.atomic(resids)) stop("Model residuals 'resids' must be an atomic vector!")

  #Set up plot colours
  defcols <- c(col = "black", fill = "#DE0000", line = "#3BB826", cp = "#E7972D")
  cols <- supp_pars(pars = cols, defpars = defcols, parlb = "cols")

  #Residuals versus predicted values
  resid_vs_pred_df <- cbind.data.frame(predict(model), resids)
  resid_colname <- paste0("Predicted_", resp_var, collapse = "")
  colnames(resid_vs_pred_df) <- c(resid_colname, "Residual")

  res_plot <- list()

  #Define vars for new ggplot3.4.0 aes (after aes_string deprecation)
  aes_yvar <- "Residual"

  res_plot[[paste0(modlab, "Model_Residual_vs_Predicted")]] <- ggplot(resid_vs_pred_df, aes(x = .data[[resid_colname]], y = .data[[aes_yvar]])) +
    geom_point(pch = 21, colour = cols[1], fill = cols[3], size = 3) +
    geom_hline(yintercept = 0, lwd = 1, colour = cols[2]) +
    labs(title = paste0(modlab, " Model Predicted ", resp_var, " vs Residuals"), x = paste0("Predicted ", resp_var), y = "Residuals") +
    theme(aspect.ratio = asprat,
          panel.background = element_blank(),
          panel.grid.major.y = element_line(colour = "grey50", linetype = 2),
          panel.grid.minor.y = element_line(colour = "grey60", linetype = 3),
          plot.title = element_text(colour = "black"),
          axis.line = element_line(colour = "black", linetype = 1),
          axis.text = element_text(size = 11, colour = "black"),
          axis.title = element_text(size = 12, colour = "black"))
  return(res_plot)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Model predicted versus actual values
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Create a Predicted-vs-Actual plot from DoE analysis results
#'
#' @description Part of the \code{\link{doe_analyze}} workflow. \strong{This function manual is hidden from the Index}.
#'
#' @param model Model results obtained from \code{\link{doe_build}}.
#' @param resp_var A \code{character} string specifying the response variable name.
#' @param cols Plot colours. Either \code{"default"}, or a \strong{named} \code{numeric} vector of any of the following colours:
#' colour (\code{"col"}), fill (\code{"fill"}), line colour (\code{"line"}).
#' @param asprat The plot aspect ratio. Defaults to 1.
#' @param modlab The model name/label as a \code{character} string (none by default).
#'
#' @return A \code{list} of \code{ggplot} plot objects.
#'
#' @export
#' @keywords internal
#'
#' @seealso \code{\link{doe_analyze}}
#'
#' @importFrom stats as.formula predict
#' @import ggplot2
#' @import ggpmisc
#'
doe_pred <- function(model, resp_var, cols = "default", asprat = 1, modlab = "") {

  #Preliminary checks
  modlab <- scap(modlab)
  if(!inherits(model, "lm")) stop("The input 'model' must be of class 'lm'!")

  #Set up plot colours
  defcols <- c(col = "black", fill = "#3BB826", line = "#E7972D")
  cols <- supp_pars(pars = cols, defpars = defcols, parlb = "cols")

  #Predicted versus actual response values
  resp_vs_pred_df <- cbind.data.frame(predict(model), model[["model"]][,resp_var])
  resid_colname <- paste0("Predicted_", resp_var, collapse = "")
  colnames(resp_vs_pred_df) <- c(resid_colname, resp_var)
  lin_formula <- as.formula("y~x")

  #Define global variables to pass R CMD note check
  rr.label <- eq.label <- p.value.label <- NULL

  res_plot <- list()
  res_plot[[paste0(modlab, "Model_", resp_var, "_vs_Predicted")]] <- ggplot(resp_vs_pred_df, aes(x = .data[[resp_var]], y = .data[[resid_colname]])) +
    geom_smooth(method = "lm", formula = lin_formula, se=TRUE, colour = cols[3], fill=cols[3], alpha=0.1, linewidth=0.8) +
    geom_point(pch = 21, colour = cols[1], fill = cols[2], size = 3) +

    ggpmisc::stat_poly_eq(aes(label = paste(after_stat(eq.label), after_stat(rr.label),
                                            after_stat(p.value.label), sep = "*plain(\";\")~")),
                          label.x = "left", label.y = 0.95,
                          formula = lin_formula, parse = TRUE, size = 4, alpha = 1) +

    labs(title = paste0(modlab, " Model Predicted ", resp_var, " vs Actual ", resp_var), x = paste0("Predicted ", resp_var), y = paste0("Actual ", resp_var)) +
    theme(aspect.ratio = asprat,
          panel.background = element_blank(),
          panel.grid.major.y = element_line(colour = "grey60", linetype = 3),
          panel.grid.minor.y = element_line(colour = "grey60", linetype = 3),
          plot.title = element_text(colour = "black"),
          axis.line = element_line(colour = "black", linetype = 1),
          axis.text = element_text(size = 11, colour = "black"),
          axis.title = element_text(size = 12, colour = "black"))
  return(res_plot)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Cook's Distance Plot
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Create a Cook's Distance plot from DoE analysis results
#'
#' @description Part of the \code{\link{doe_analyze}} workflow. \strong{This function manual is hidden from the Index}.
#'
#' @param model Model results obtained from \code{\link{doe_build}}.
#' @param type A \code{numeric} identifier \strong{between 1-4} specifying the equation used to calculate the distance limit
#' above which significance is noted. One of \strong{1:} 4/n (default), \strong{2:} 4/(n-p-1),
#' \strong{3:} 1/(n-p-1), \strong{4:} 3 * average Cook's Distance;
#' \strong{p and n are the number of model coefficients and DoE data points, respectively}.
#' @param plt_title Plot title as a \code{character} string. Defaults to \code{NA} (no title is displayed).
#' @param cols Plot colours. Either \code{"default"}, or a \strong{named} \code{numeric} vector of any of the following colours:
#'
#' @param asprat The plot aspect ratio. Defaults to 1.
#' @param modlab The model name/label as a \code{character} string (none by default).
#'
#' @return A \code{list} of \code{ggplot} box plot objects.
#'
#' @export
#' @keywords internal
#'
#' @seealso \code{\link{doe_analyze}}
#'
#' @importFrom stats cooks.distance rstandard
#' @importFrom scales breaks_pretty
#' @importFrom ggrepel geom_label_repel
#' @import ggplot2
#'
doe_cooks <- function(model, type = 1, plt_title = NA, cols = "default", asprat = 1, modlab = "") {

  #Preliminary checks
  if(!any(class(model) %in% "lm")) stop("The 'model' provided must be of class 'lm'!")
  if(type < 0 | type > 4) stop("Argument 'type' must be 1, 2, 3, or 4!")
  type <- as.character(type)

  #Set up plot colours
  defcols <- c(main = "darkred", line = "#505252")
  cols <- supp_pars(pars = cols, defpars = defcols, parlb = "cols")

  #Prepare data
  df <- model[["model"]]
  rnm_df <- rownames(df)
  df[,"st_resid"] <- rstandard(model)
  df[,"cook_dist"] <- cooks.distance(model)
  df[,"outlier"] <- ifelse(abs(df[,"st_resid"]) > 3, rnm_df, NA)

  p <- length(model[["coefficients"]])
  n <- nrow(df)
  h <- switch(type, "1" = 4/n, "2" = 4/(n-p-1),
              "3" = 1/(n-p-1), "4" = 3 * mean(df[,"cook_dist"]))
  df[,"cooks_switch"] <- ifelse(df[,"cook_dist"] > h, rnm_df, NA)

  plotres <- list()

  #Create the plot
  pltname <- paste0("Cooks_Distance_Plot", ifelse(modlab=="", "", paste0("_", scap(modlab), "_Model", collapse = "")))
  plotres[[pltname]] <- ggplot(data = df, aes(x = 1:n, y = .data[["cook_dist"]], ymin = 0,
                                              ymax = .data[["cook_dist"]])) +
    geom_point(size = 1.5, shape=1) +
    geom_hline(yintercept = h,
               color = cols["main"], linetype = 2) +
    geom_linerange(color = cols["line"]) +
    scale_x_continuous(breaks = scales::breaks_pretty(n = if(n <= 10) n else n/2)) +
    scale_y_continuous(breaks = scales::breaks_pretty(n = 5)) +
    ggrepel::geom_label_repel(data = df,aes(label = !!sym("cooks_switch")),
                              na.rm = TRUE,
                              max.overlaps = 20,
                              color = cols["main"]) +
    geom_label(data = data.frame(h = h, maxn = n), #nudge_x = 1*(n-1),
               aes(x = n, y = h, ymin = NULL, ymax = NULL, label = round(h,2)), colour = "white", fontface = "bold", fill = cols["main"]) +
    labs(x = "Observation", y = "Cook's Distance",
         title = if(!is.na(plt_title)) plt_title else waiver()) +
    theme(aspect.ratio = asprat,
          panel.background = element_blank(),
          axis.text = element_text(colour = "black", size = 12),
          axis.title = element_text(colour = "black", size = 13),
          axis.line = element_line(colour = "black"))
  return(plotres)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Calculate the Pseudo-Standard Error (PSE), Margin of Error (ME), and Simultaneous Margin of Error (SME)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Calculate PSE, ME, and SME from model coefficients
#'
#' @description Part of the \code{\link{doe_analyze}} workflow. \strong{This function manual is hidden from the Index}.
#'
#' @param effs A \code{numeric} vector of model \strong{coefficients}.
#' @param method The method to use for calculating PSE, ME, and SME. One of \code{"zahn"}, \code{"daniel"}, \code{"lenth"},
#' or \code{"juanpena"}.
#' @param conf The \code{numeric} confidence level (defaults to 0.05).
#'
#' @return A named \code{list} containing the Pseudo-Standard Error (\code{$pse}), (Simultaneous) Margin of Error at \code{conf}
#' (\code{$me} and \code{$sme}), the \code{$method} used, and the chosen \code{conf} (\code{$alpha}).
#'
#' @export
#' @keywords internal
#'
#' @seealso \code{\link{doe_analyze}}, \code{\link{doe_pareto}}
#'
#' @importFrom stats median qnorm quantile rnorm
#'
doe_pseme <- function(effs, method = "zahn", conf = 0.05) {

  #Preliminary checks
  if(!is.numeric(effs)|is.null(names(effs))) stop("The effects ('effs') must be provided as a named numeric vector!")
  psemet <- c("daniel", "lenth", "juanpena", "zahn")
  if(!any(psemet %in% method)|length(method)!=1) stop(paste0("The 'method' must be one of: ", paste0("'", psemet, "'", collapse = ", "), "!"))
  if(!is.null(names(effs))) effs <- effs[!grepl("Intercept", names(effs))]

  #Begin processing
  #Define function
  get_pse <- function(effs, method) {
    n.effs <- length(effs)
    abs.effs <- abs(effs)
    MAD <- median(sort(abs.effs))

    if(method == "daniel") {
      pse <- sort(abs.effs)[floor(0.683 * n.effs + 0.5)]
    } else if(method == "lenth") {
      s0 <- 1.5 * median(abs.effs)
      pse <- 1.5 * median(abs.effs[abs.effs <= 2.5 * s0])
    } else if(method == "juanpena") {
      while(n.effs != (n.effs <- max(which(sort(abs.effs) <= 3.5 * MAD)))) MAD <- median(sort(abs.effs)[seq_len(n.effs)])
      pse <- MAD/0.6578
    } else if(method == "zahn") {
      m <- floor(0.683 * n.effs + 0.5)
      q <- (seq_len(m) - 0.375)/(n.effs + 0.25)
      z <- qnorm((1 + q)/2)
      zeff <- z/sum(z^2)
      pse <- sum(zeff * sort(abs.effs)[seq_len(m)])
    }
    return(pse)
  }

  #Calculate the ME and SME from PSE
  pse <- get_pse(effs, method = method)
  n.effs <- length(effs)
  n.sets <- ceiling(40000/n.effs)
  X <- matrix(rnorm(n.effs * n.sets), nrow = n.effs)
  abst <- apply(X, 2, function(x) abs(x)/get_pse(x, method = method))
  max.abst <- apply(abst, 2, max)
  me <- pse * quantile(abst, 1 - conf)
  sme <- pse * quantile(max.abst, 1 - conf)

  return(list(pse = pse, me = me, sme = sme, method = method, alpha = conf))
}

#~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Pareto plot
#~~~~~~~~~~~~~~~~~~~~~
#' @title Create a Pareto plot from DoE analysis results
#'
#' @description Part of the \code{\link{doe_analyze}} workflow. \strong{This function manual is hidden from the Index}.
#'
#' @param model Model results obtained from \code{\link{doe_build}}.
#' @param conf The confidence limit to use for function \code{\link{doe_pseme}}.
#' @param method Method to use for PSE, ME, and SME estimation as detailed in function \code{\link{doe_pseme}}.
#' @param plt_title Either set to \code{NA} or the plot title as a \code{character} string.
#' @param cols Plot colours. Either \code{"default"}, or a \strong{named} \code{numeric} vector of any of the following colours:
#'
#' @param asprat The plot aspect ratio. Defaults to 1.
#' @param modlab The model name/label as a \code{character} string (none by default).
#'
#' @return A \code{list} of \code{ggplot} box plot objects.
#'
#' @export
#' @keywords internal
#'
#' @seealso \code{\link{doe_analyze}}, \code{\link{doe_pseme}}
#'
#' @importFrom stats coef reorder
#' @importFrom scales breaks_pretty
#' @import ggplot2
#'
doe_pareto <- function(model, conf = 0.05, method = "zahn", plt_title = NA, cols = "default", asprat = 1, modlab = "") {

  #Preliminary checks
  if(!any(class(model) %in% "lm")) stop("The 'model' provided must be of class 'lm'!")

  #Set up plot colours
  defcols <- c(pos = "#00BFC4", neg = "#F8766D")
  cols <- supp_pars(pars = cols, defpars = defcols, parlb = "cols")

  #Prepare data
  effs <- coef(model)[!grepl("Intercept", names(coef(model)))]
  names(effs) <- model[["codenames"]][match(gsub(" ", "", names(effs), fixed = TRUE),
                                            gsub(" ", "", model[["codenames"]][,"formula"], fixed = TRUE)),"label"]
  estim <- effs * 2

  pseres <- doe_pseme(estim, method = method, conf = conf)
  pse <- pseres[["pse"]]
  me <- pseres[["me"]]
  sme <- pseres[["sme"]]

  df <- data.frame(effects = names(effs), estimates = estim, abs_estimates = abs(estim),
                   cols = ifelse(effs > 0, cols["pos"], cols["neg"]))
  df <- df[order(df[,"abs_estimates"]),]

  plotres <- list()

  #Create Pareto plot
  pltname <- paste0("Pareto_Plot", ifelse(modlab=="", "", paste0("_", scap(modlab), "_Model", collapse = "")))
  plotres[[pltname]] <- ggplot(df,
                               aes(x = reorder(!!sym("effects"),
                                               !!sym("abs_estimates")),
                                   y = !!sym("abs_estimates"),
                                   fill= !!sym("cols"))) +
    geom_bar(stat = "identity") +
    geom_hline(yintercept = me, linetype=2) +
    annotate("text", x = -Inf, y = me, hjust = -0.2, vjust = -0.5,
             label = "ME", fontface = "italic", size = 2.8) +
    geom_hline(yintercept = sme, linetype=2) +
    coord_flip() +
    scale_fill_manual(values = unname(cols),
                      labels = c("Negative","Positive")) +
    annotate("text", x = -Inf, y = sme, hjust = -0.2, vjust = -0.5,
             label = "SME",fontface = "italic", size=2.8) +
    labs(title = if(!is.na(plt_title)) plt_title else waiver(), x = "", y = "Absolute effects",
         caption = paste0("ME = ", round(me, 2), ", ", "SME = ", round(sme, 2))) +
    scale_y_continuous(breaks = scales::breaks_pretty(n = 6)) +
    theme(aspect.ratio = asprat,
          panel.background = element_blank(),
          axis.text = element_text(colour = "black", size = 12),
          axis.title = element_text(colour = "black", size = 13),
          axis.line = element_line(colour = "black"),
          legend.title = element_blank(),
          legend.position = "right")
  return(plotres)
}
#END OF PLOTTING FUNCTIONS
