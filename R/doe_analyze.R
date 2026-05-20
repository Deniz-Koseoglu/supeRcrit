#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Combine functions into a singular workflow for model building
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Build, prune, analyze, and summarize RSM models from DoE
#'
#' @description A complete workflow for modeling and analysing DoE design responses that includes the following elements:
#' \enumerate{
#' \item Building of the \strong{initial} linear or quadratic (RSM) model.
#' \item Carry out Lack-of-Fit (LoF) testing.
#' \item Removing insignificant effects via Stepwise Regression and/or p-value cutoff.
#' \item Building the \strong{final} simplified model.
#' \item Minimizing and/or maximizing the response using \code{\link[stats]{optim}} and Canonical Analysis.
#' \item Compiling summaries, descriptive statements, and plots for all of the above.
#' For further information, see \strong{Details} and \strong{References}.
#' }
#'
#' @param doe A \code{data.frame} containing DoE factors (\strong{coded and uncoded}), response (\code{resp_var}),
#' and temporal variable (e.g. run order, \code{time_var}). \strong{Coded factor names are assumed to be capital letters ordered alphabetically}.
#' @param uc_facs A \code{character} vector of \strong{uncoded} factor names, in order of appearance in \code{doe}.
#' If not provided (\code{NA}, default), uncoded factors are not included in the analysis.
#' @param cent_id A unique \code{character} identifier for center points to be highlighted in Exploratory Data Analysis (EDA) plots.
#' Defaults to \code{NA} (i.e. no center points are highlighted).
#' @param resp_var,time_var Single \code{character} strings specifying the response and run order (temporal) variables as they appear in \code{doe}.
#' @param mod_order The order of model to build. One of: \code{1} (first-order, linear), \code{1.5} (first order with interaction terms),
#' or \code{2} (quadratic polynomial, suitable for RSM).
#' @param canon_thres A threshold specific to Canonical Analysis (CA). When set to \code{"auto"}, defaults to 10% of the maximum eigen value.
#' @param p_cutoff The p-value cutoff to use when pruning the initial model. Only used when \code{trim_method} includes \code{"p_cutoff"}.
#' @param trim_method A \code{character} string specifying which method(s) to use for removing \strong{in}significant terms
#' from the initial model. One of \code{"stepwise"} (Stepwise Regression, default), \code{"p_cutoff"} (p-value cutoff),
#' or \code{"both"}.
#' @param which_facs A \code{character} string specifying which factors to use for model building. One of \code{"coded"} (default)
#' or \code{"uncoded"}.
#' @param export An \strong{existing} export folder path where plots and results are exported as .PDF (or .PNG) and .CSV files
#' via function \code{\link{doeopt_export}}.
#' @param asprat Aspect ratio of generated plots. When set to \code{"default"}, a ratio of \code{1} is applied.
#' @param verbose Should detailed updates be provided during processing? Defaults to \code{TRUE}.
#'
#' @return A complex \code{list} including the following elements:
#' \enumerate{
#' \item \strong{$models}: The results of \strong{initial and final} models of class \code{"lm"}.
#' \item \strong{$results}: Summarized key results of \strong{initial and final} models, including a \code{data.frame} of
#' model coefficients, aliasing, standard errors, t- and p-values, significance level, ANOVA sum of squares and F-values (\code{$Model_Results}).
#' Other elements include a \code{data.frame} of main, interaction, and/or quadratic effects and residuals for each observation (\code{$Model_Data}),
#' the original input \code{data.frame} (\code{$Orig_Data}), a \code{list} of performance metrics such as R2, F-statistic, LoF test results,
#' and Canonical Analysis (\code{$Model_Metrics}). Finally, a summary of key input data such as the number of observations and both
#' \strong{coded and uncoded} model equations are also included.
#' \item \strong{$plots}: A list of \code{ggplot}-class plots describing the results and including Box, EDA, Pareto, Cook's Distance, and other
#' plots.
#' \item \strong{$statements}: A vector of descriptive statements about the results.
#' \item \strong{$call}: The function call.
#' }
#' @export
#'
#' @details
#' The key goal of SFE experimental design is process optimization, commonly achieved via Response Surface Methodology (RSM) which
#' calculates both the interaction and quadratic terms and approximates the shape of the local response surface  (Sharif et al., 2014; Yolmeh et al., 2017).
#' Because a natural product SFE rate almost always decreases along a single smooth curvature seen in a typical OEC,
#' RSM is especially suitable as it predicts a local response surface by a single 2nd-order (i.e. quadratic) polynomial function,
#' thus failing to describe piecewise smooth or jagged response trajectories in a single experiment. An example model containing linear,
#' two-way, and quadratic interaction terms for 3 factors is shown below.
#' \deqn{y = \beta_0 + \beta_2x_2 + \beta_3x_3 + \beta_{12}x_1x_2 + \beta_{13}x_1x_3 + \beta_{23}x_2x_3 + \beta_{11}x_1^2 + \beta_{22}x_2^2 + \beta_{33}x_3^2 + \epsilon}
#' At least a 3-level design is required to estimate quadratic interactions. This requirement is not met by 2-level designs with
#' center points, which are positioned within the experimental space such that all quadratic effects are aliased (NIST/Sematech, 2013).
#' Three-level FFDs are useful but are not rotatable and become prohibitive due to the large number of runs required for more than 3 factors.
#' Instead, rotatable Box-Wilson Central Composite (CCD) or Box-Behnken (BBD) designs are used, where response is dependent only on the
#' distance of factor levels from the center of the experimental space (and not on direction); thus, points at an identical distance from
#' the center of the experimental space exhibit the same prediction error/variance  (Sharif et al., 2014).
#' Cubic terms are seldom included when interpreting RSM. An ANOVA table should then be examined for goodness of fit
#' (consistently high R2 and adjusted R2) and insignificant terms of low-magnitude exhibiting a high p value (e.g. >0.05),
#' Probability plots can be used as a complementary technique. Insignificant effects may then be separated systematically
#' using stepwise regression and/or Pareto charts (Kiratu, 2015), and the simplified ANOVA model re-examined iteratively.
#' Once the simplest model that explains most of the response variability is attained, the residual distribution should be examined to
#' validate the normality and uniformity of variance assumptions (Yolmeh et al., 2017). Transformation of response data (e.g. via a
#' natural logarithm or a Box-Cox approach) is useful for mitigating violations of these assumptions. The residuals can also be checked
#' for curvature via if a 2nd-order model was not built. Once the residual distribution is satisfactory, the
#' optimal factor settings can be determined by plotting the model coefficients/terms against factor levels. In case of RSM designs,
#' a contour plot(s) for each response is used to both optimize each response separately and/or locate a compromise between multiple responses.
#' The current workflow incorporates linear and quadratic model building, lack-of-fit testing, systematic removal of insignificant effects,
#' visualisation of important factors and optimization of factor values to maximize or minimize the response via Canonical Analysis (CA).
#'
#' @references
#' NIST/SEMATECH (2013), 'Engineering Statistics Handbook', available at: \url{https://www.itl.nist.gov/div898/handbook/index.htm} (accessed 29.09.2024).
#'
#' Kiratu, J., Raynie, D.E. (2015), 'Aiding the Development of Extraction Procedures with Response Surface Methodology', \emph{LCGC North America} \strong{33} (7), pp. 104-111.
#'
#' Sharif, K.M., Rahman, M.M., Azmir, J., Mohamed, A., Jahurul, M.H.A., Sahena, F., Zaidul, I.S.M. (2014), 'Experimental design of supercritical fluid extraction – A review', \emph{Journal of Food Engineering} \strong{124}, pp. 105-116, DOI: \url{https://doi.org/10.1016/j.jfoodeng.2013.10.003}.
#'
#' Yolmeh, M., Jafari, S.M. (2017), 'Applications of Response Surface Methodology in the Food Industry Processes', \emph{Food Bioprocess. Technol.} \strong{10}, pp. 413-433, DOI: \url{https://www.doi.org/10.1007/s11947-016-1855-2}.
#'
#' @examples
#' #Maximizing extraction yield of spearmint (SFE)
#' doe_optres <- doe_analyze(doe = doex[["ccd3"]][["data"]],
#' uc_facs = c("P_bar", "T_degC", "EtOH_gmin"),
#' cent_id = NA,
#' resp_var = "ExtYield",
#' time_var = "Actual_Order",
#' mod_order = 2,
#' canon_thres = "auto",
#' p_cutoff = 0.10,
#' trim_method = "both",
#' which_facs = "coded",
#' export = "none",
#' verbose = TRUE)
#'
#' @seealso \code{\link{doeopt_export}}
#'
#' @importFrom stats anova predict setNames
#'
doe_analyze <- function(doe, uc_facs = NA, cent_id = NA, resp_var, time_var, mod_order = 1.5,
                        canon_thres = "auto", p_cutoff = 0.10, trim_method = "stepwise",
                        which_facs = "coded", export = "none",
                        asprat = "default", verbose = TRUE) {

  #Generate function call
  cl_rec <- match.call()

  #Preliminary checks
  if(!any(c("stepwise", "p_cutoff", "both", "none") %in% trim_method)) stop("Argument 'trim_method' must be one of: 'stepwise', 'p_cutoff', 'both', 'none'!")
  if(!dir.exists(export) & !any(export %in% "none")) stop("The provided 'export' directory does not exist!")

  #Define Aspect Ratio parameters
  defasp <- setNames(rep(1,6), c("box","eda","resid","pred","cooks","pareto"))
  asprat <- supp_pars(pars = asprat, defpars = defasp, parlb = "asprat")

  #Create lists to assign various results from the initial and final models
  mods <- mod_sums <- mod_anovas <- mod_lofs <- mod_dfs <- mod_res <- mod_plots <- setNames(vector("list",2), c("initial","final"))

  #Order data according to actual run order
  doe <- doe[order(doe[,time_var], decreasing = FALSE),]

  #Prepare data for model building
  cat("\nPreparing data for model building...")
  prepres <- doe_prep(doe, time_var, resp_var, uc_facs, which_facs, mod_order)
  init_input <- mod_dfs[["initial"]] <- prepres[["minput_df"]]
  all_facs <- prepres[["all_facs"]]
  all_effs <- prepres[["all_effs"]]

  #Check if a useful model can even be built (it cannot if the number of effects is equal to or larger than the number of observations)
  if(nrow(prepres[["orig_df"]])<=nrow(all_effs)) stop("No useful model is possible since the number of observations is equal to or lower than the number of effects! Try a lower-order model?")

  #Build initial model and determine if any factors were not estimated due to singularities etc.
  cat("\nBuilding initial model...")
  init_modres <- doe_build(input = init_input, resp_var = resp_var, effs = all_effs, orig_data = doe,
                           uc_facs = if(is.character(uc_facs)) uc_facs[which(all_facs[["coded"]] %in% colnames(init_input))] else uc_facs)
  rev_input <- init_modres[["input_df"]]
  init_model <- mods[["initial"]] <- init_modres[["model"]]
  est_effs <- init_modres[["est_effs"]]

  #Carry out a Lack-of-Fit test
  cat("\nCarrying out Lack-of-Fit testing of the initial model...")
  init_lof <- mod_lofs[["initial"]] <- doe_lof(rev_input, resp_var, init_model, est_effs)

  #Run stepwise regression and p-value cutoff to determine insignificant factors (using scope of main effects only through to full model with all effects)
  cat("\nTrimming insignificant factors from the initial model using methods: ", paste0("'", trim_method, "'", collapse = ", "),"...", sep = "")
  signif_res <- doe_insig(init_model, all_facs[["coded"]], resp_var, trim_method, p_cutoff)
  signif_effs <- signif_res[["signif"]]

  #Check if model is too poor to trim
  if(nrow(signif_effs)<2) {
    cat("\nModel trimming resulted in only ", nrow(signif_effs), " remaining! Cancelling trimming.", sep = "")
    signif_effs <- est_effs
  } else if(all(is.nan(init_lof[,"Pr(>F)"]))) {
    cat("\nThe initial model does not have enough degrees of freedom for a Lack-of-Fit test! Model factors were not trimmed!")
    signif_effs <- est_effs
  }

  #Apply the Hierarchy Principle (if any main effects are deemed insignificant but ARE included in significant two-way or quadratic interactions, add them back into the model!)
  cat("\nApplying the Hierarchy Principle to the initial model...")
  hrp_res <- doe_hrp(signif_effs)
  fin_effs <- hrp_res[["new_effs"]]
  fin_input <- rev_input[,c(time_var,resp_var,fin_effs[,"data"])]

  #Build final model
  cat("\nBuilding final model...")
  fin_modres <- doe_build(fin_input, resp_var, fin_effs, orig_data = doe,
                          uc_facs = if(is.character(uc_facs)) uc_facs[which(all_facs[["coded"]] %in% colnames(fin_input))] else uc_facs)
  fin_input <- mod_dfs[["final"]] <- fin_modres[["input_df"]]
  fin_model <- mods[["final"]] <- fin_modres[["model"]]
  fin_est_effs <- fin_modres[["est_effs"]]

  #Carry out a Lack-of-Fit test
  cat("\nCarrying out Lack-of-Fit testing of the final model...")
  fin_lof <- mod_lofs[["final"]] <- doe_lof(fin_input, resp_var, fin_model, fin_est_effs)

  #Compile summaries and ANOVA tables for the initial and final models
  #Also carry out additional calculations and summarize the results
  #These include: Canonical Analysis, Optimization etc.
  cat("\nCompiling summaries and plots for the initial and final models...")
  replicate(2, cat("\n"))
  for(i in seq_along(mods)) {
    mod_anovas[[i]] <- anova(mods[[i]])
    mod_sums[[i]] <- aug_sum(mods[[i]])
    if(which_facs == "coded" & !any(is.na(uc_facs)) & mod_sums[[i]][["order"]]>1) {
      if(any(names(mod_sums[[i]]) %in% "canonical")) {

        dcres <- doe_decode(mod_sums[[i]][["canonical"]][["xs"]], doe, all_facs[["coded"]], uc_facs)

        mod_sums[[i]][["canonical"]][["xs_decoded"]] <- dcres[["newvals"]]
        mod_sums[[i]][["canonical"]][["outside_range"]] <- dcres[["newvals"]] < dcres[["mins"]] | dcres[["newvals"]] > dcres[["maxes"]]
        mod_sums[[i]][["canonical"]][["rng"]] <- setNames(cbind.data.frame(names(dcres[["olmins"]]),
                                                                           dcres[["olmins"]], dcres[["olmaxes"]],
                                                                           dcres[["mins"]], dcres[["maxes"]]), c("facs", "old_min", "old_max",
                                                                                                                 "new_min", "new_max"))
      }
      steeptry <- try(steepath(mods[[i]], uc_facs, all_facs[["coded"]], respname = resp_var), silent = TRUE)
      errchk <- inherits(steeptry, "try-error")
      mod_sums[[i]][["steepest_ascent"]] <-  if(!errchk) {
        steepath(mods[[i]], uc_facs, all_facs[["coded"]], respname = resp_var)
      } else "Path of steepest ascent could not be estimated."
    }
    if(any(names(mod_sums[[i]]) %in% "canonical")) mod_sums[[i]][["canonical"]][["predicted"]] <- predict(mods[[i]], newdata = as.data.frame(t(mod_sums[[i]][["canonical"]][["xs"]])))

    mod_sums[[i]][["tradopt"]] <- doe_optim(mods[[i]], doe, resp_var, all_effs, all_facs[["coded"]], uc_facs)[["tradopt"]]

    #Summarize all model results
    mod_res[[i]] <- doe_summary(mods[[i]], mod_sums[[i]], mod_anovas[[i]], mod_lofs[[i]], mod_dfs[[i]])

    #Create model visualisations
    #Exploratory graphs of response distribution
    #Box Plots of response/residual versus coded/uncoded factor levels
    box_res <- doe_box(doe, resids = mod_res[[i]][["Model_Data"]][,"Residual"], resp_var, all_facs, asprat = asprat["box"], modlab = names(mod_res)[i])

    #EDA Plots including Normal Q-Q, Frequency Histogram, Box, Run Order plots
    eda_res <- doe_eda(doe, mod_res[[i]], time_var, resp_var, cent_id = cent_id, asprat = asprat["eda"], modlab = names(mod_res)[i]) #use grid.arrange() to print

    #Residuals versus predicted values
    resid_res <- doe_resid(mods[[i]], resp_var, mod_res[[i]][["Model_Data"]][,"Residual"], asprat = asprat["resid"], modlab = names(mod_res)[i])

    #Predicted versus Actual response values
    pred_res <- doe_pred(mods[[i]], resp_var, asprat = asprat["pred"], modlab = names(mod_res)[i])

    cooks_res <- doe_cooks(mods[[i]], plt_title = paste0("Cook's Distance Plot - ", scap(names(mod_res)[i]), " Model"),
                           asprat = asprat["cooks"], modlab = names(mod_res)[i])

    pareto_res <- doe_pareto(mods[[i]], plt_title = paste0("Pareto Plot - ", scap(names(mod_res)[i]), " Model"),
                             asprat = asprat["pareto"], modlab = names(mod_res)[i])

    #Compile a complete plot_list
    mod_plots[[i]] <- c(box_res, eda_res, resid_res, pred_res, cooks_res, pareto_res)
  }

  #Retrieve and (optionally) print statements about the models
  statements <- doe_state(init_modres, fin_modres, signif_res, hrp_res, mod_sums, mod_lofs, mod_res, print_res = verbose)

  #Compile TOTAL RESULTS
  totres <- list(models = mods, results = mod_res, plots = mod_plots, statements = statements, call = cl_rec)

  #Finally, export data and visualisations
  if(export!="none" & dir.exists(export)) {
    cat("\nExporting DOE analysis results...")
    doeopt_export(totres, expath = export, silent = !verbose)
  }
  return(totres)
}
