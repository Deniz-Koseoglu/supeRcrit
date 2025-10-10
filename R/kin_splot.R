#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Visualize and summarize several kinetic extraction models
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Visually compare several kinetic extraction models
#'
#' @description Summarizes the results of between two and five kinetic models both visually and in a table.
#'
#' @param m A \code{list} of \strong{2-5} output objects from function \code{\link{bicmod}} \strong{OR} \code{\link{ktsmod}}.
#' @param which_mod A string specifying which type of models to summarize and plot. \strong{One} of \code{"sim"} (simplified BIC),
#' \code{"ct"}, \code{"cmp2"} or \code{"cmp3"} (complete BIC with 2 or 3 OEC regions, respectively), \code{"tws"} (two-site kinetic desorption).
#' @param mvars A \strong{named} \code{character} vector of column names specifying \code{"x"} and \code{"y"} values in all results in \code{m}.
#' Defaults to \code{c(x = "x", y = "y")}.
#' @param cols Either \code{"default"} or a \strong{named} \code{character} vector of point/line colours for models \code{"one"},
#' \code{"two"}, \code{"three"}, \code{"four"}, and/or \code{"five"}. Unspecified colours are set to the following defaults:
#' \code{c(one = "darkgreen", two = "darkorange", three = "darkred", four = "blue", five = "purple")}.
#' @param leglab A \code{character} vector of legend labels for models in the same order as in argument \code{m}.
#' @param axlab A \strong{named} character vector of plot labels for the x-axis (\code{"x"}), y-axis (\code{"y"}),
#' and the title (\code{"title"}).
#'
#' @return A \code{list} containing the following results:
#' \enumerate{
#' \item \strong{$plot}: A summary plot containing experimental points and modeled curves for all models of type \code{which_mod}
#' in argument \code{m}.
#' \item \strong{$summary}: A \code{data.frame} summarizing key input and calculated parameters of evaluated models, as well as three standard
#' error metrics (see \code{\link{moderr}}).
#' See/use \code{\link{bicmod}}, \code{\link{ktsmod}}, and \code{\link{show_pars}} for parameter descriptions.
#' \item \strong{$model_type}: A \code{character} string stating the value of \code{which_mod} (the summarized model type) for user information.
#' }
#' @export
#'
#' @examples
#' #Prepare input data
#' bicdt <- list(sfex[["rizza1"]][["data"]],
#' sfex[["rizza2"]][["data"]],
#' sfex[["rizza3"]][["data"]])
#'
#' #Estimates of extractable solute fraction
#' cuvals <- c(0.165, 0.21, 0.08)
#'
#' #Generate several BIC models
#' biclst <- list()
#' for(i in seq_along(bicdt)) {
#'   biclst[[i]] <- bicmod(oec = bicdt[[i]],
#'                         oec_vars = c(x = "Time_min", y = "Yield_g", slv = "Solvent_mL"),
#'                         pars = c(pres = 300,
#'                                  cu = cuvals[i],
#'                                  temp = 45,
#'                                  flow = NA,
#'                                  mass_in = 0.5125,
#'                                  moisture = 8.6,
#'                                  D = 0.015,
#'                                  L = 0.015,
#'                                  etoh = 0.5,
#'                                  dr = 1554,
#'                                  dp = 0.0004,
#'                                  n = 2),
#'                         opt_est = "default",
#'                         flowpar = c(1.01325, 25),
#'                         etoh_frac = 0.06, #For when CO2 flow not provided but 'etoh' is >0
#'                         ro_co2 = NA,
#'                         tmax = NA,
#'                         qmax = NA,
#'                         cumulative = FALSE,
#'                         mass_flow = FALSE,
#'                         draw = FALSE,
#'                         units = c(flow = "none", resp = "g"),
#'                         modtype = "all") #"cu"
#' }
#'
#' #Summarize the results
#' bic_summary <- kin_splot(m = biclst,
#'                          which_mod = "cmp3",
#'                          mvars = c(x = "x", y = "y"), #c("x", "y")
#'                          leglab = c("S. obliquus", "N. salina", "C. protothecoides"),
#'                          axlab = c(title = "BIC models", x = "q (kg/kg)", y = "e (kg/kg)"))
#'
#' @seealso \code{\link{bicmod}}, \code{\link{ktsmod}}, \code{\link{show_pars}}, \code{\link{moderr}}
#'
#' @importFrom scales breaks_pretty
#' @import ggplot2
#'
kin_splot <- function(m, which_mod, mvars = c(x = "x", y = "y"), cols = "default", leglab = NA, axlab = c(title = NA, x = NA, y = NA)) {

  #Assign model name to retrieve from data based on 'which_mod'
  which_cat <- if(grepl("^cmp", which_mod)) "cmp" else which_mod

  #Preliminary checks
  modnms <- c("sim", "ct", "cmp2", "cmp3", "tws")
  if(!any(modnms %in% which_mod)|length(which_mod)!=1) stop(paste0("Argument 'which_mod' must be one of:", paste("'", modnms, "'", collapse = ", "),"!"))
  prelchk <- all(sapply(m, function(x) is.list(x) & all(c("data","plots","input", "call", which_cat) %in% names(x))))
  if(!prelchk) stop("The data structure of kinetic model results was not recognised (must be output from functions 'ktsmod' or 'bicmod')!")

  #Create list of model data
  mdlst <- lapply(m, function(x) x[[which_cat]])

  #Preliminary checks (continued)
  if(!all(is.na(axlab)) & !all(names(axlab) %in% c("title","x","y"))) stop("The vector of custom plot labels 'axlab' must include all of the following names: 'title', 'x', or 'y'!")
  if(!all(c("x","y") %in% names(mvars))) stop("The vector of data variable names to plot ('mvars') must be a vector of length 2 with names 'x' and 'y'!")
  if(!all(sapply(mdlst, function(x) all(mvars %in% c(colnames(x[["ordt"]]),colnames(x[["mdt"]])))))) {
    stop("Not all model variables to plot ('mvars') are present in the input data!")
  }
  if(length(m)>5) stop("Currently, a maximum of 5 models may be combined!")
  if(!any(is.na(leglab)) & length(leglab)!=length(m)) stop("If provided, the length of legend labels ('leglab') must be equal the number of models!")

  #Set up plot colours
  defcols <- c(one = "darkgreen", two = "darkorange", three = "darkred", four = "blue", five = "purple")
  cols <- supp_pars(pars = cols, defpars = defcols, parlb = "cols")

  cols <- cols[seq(length(m))]
  if(any(is.na(leglab))) leglab <- names(defcols)[seq(length(m))]

  #Combine model plot data, errors, and key metrics
  pltlab <- paste0(which_mod, "_", leglab)
  for(i in seq_along(mdlst)) {
    curlab <- pltlab[i]
    curmod <- mdlst[[i]]
    ptapp <- cbind.data.frame(model = curlab, curmod[["ordt"]][,mvars])
    modapp <- cbind.data.frame(model = curlab, curmod[["mdt"]][curmod[["mdt"]][,"model"]==which_mod, mvars])
    errapp <- cbind.data.frame(model = curlab, data.frame(t(curmod[["mod_pars"]])), data.frame(t(curmod[["resid"]])))
    ptdata <- if(i==1) ptapp else rbind.data.frame(ptdata, ptapp)
    mdata <- if(i==1) modapp else rbind.data.frame(mdata, modapp)
    errdata <- if(i==1) errapp else rbind.data.frame(errdata, errapp)
  }

  #Create a combined model plot
  grpvar <- "model"
  xvar <- mvars[["x"]]
  yvar <- mvars[["y"]]

  resplot <- ggplot(data = ptdata, aes(shape = .data[[grpvar]])) +
    geom_point(data = ptdata, aes(x = .data[[xvar]], y = .data[[yvar]], colour = .data[[grpvar]]), size = 2) +
    geom_path(data = mdata, aes(x = .data[[xvar]], y = .data[[yvar]], colour = .data[[grpvar]])) +
    scale_x_continuous(breaks = scales::breaks_pretty(n = 6)) +
    scale_y_continuous(breaks = scales::breaks_pretty(n = 6)) +
    scale_shape_manual(values = 15:19,
                       labels = leglab,
                       breaks = pltlab) +
    scale_colour_manual(values = unname(cols),
                        labels = leglab,
                        breaks = pltlab) +
    labs(title = if("title" %in% names(axlab)) axlab["title"] else waiver(),
         x = if("x" %in% names(axlab)) axlab["x"] else waiver(),
         y = if("y" %in% names(axlab)) axlab["y"] else waiver()) +
    theme(aspect.ratio = 1,
          panel.background = element_blank(),
          axis.text = element_text(colour = "black", size = 12),
          axis.title = element_text(colour = "black", size = 13),
          axis.line = element_line(colour = "black"),
          legend.title = element_blank(),
          #legend.key = element_rect(fill = "grey90", colour = "black"),
          legend.position.inside = c(0.93, 0.20),
          legend.text = element_text(size = 11))

  return(list(plot = resplot, summary = errdata, model_type = which_cat))
}
