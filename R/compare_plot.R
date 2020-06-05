#' Shows plots for the comparison of estimates
#'
#' Function \code{compare_plot} is a generic function used to produce two plots 
#' for all indicators or a selection of indicators. The first plot is a scatter 
#' plot of estimates to compare and the second is a line plot with these estimates.
#' @param model an object of type "emdi","model", representing point and MSE
#' estimates.
#' @param direct optional, an object of type "emdi","direct", representing point
#' and MSE estimates. If the input argument \code{model} is of type "model","ebp",
#' \code{direct} is required. If the input argument \code{model} is of type 
#' "model","fh", the \code{direct} component is already included in the input 
#' argument \code{model}.
#' @param indicator optional character vector that selects which indicators
#' shall be returned: (i) all calculated indicators ("all");
#' (ii) each indicator name: "Mean", "Quantile_10", "Quantile_25", "Median",
#' "Quantile_75", "Quantile_90", "Head_Count",
#' "Poverty_Gap", "Gini", "Quintile_Share" or the function name/s of
#' "custom_indicator/s"; (iii) groups of indicators: "Quantiles", "Poverty",
#' "Inequality" or "Custom". If two of these groups are selected, only the first
#' one is returned. Note, additional custom indicators can be
#' defined as argument for the EBP approaches (see also \code{\link{ebp}})
#' and do not appear in groups of indicators even though these might belong to
#' one of the groups. If the \code{model} argument is of type "model","fh", 
#' indicator can be set to "all", "FH" or "Direct". Defaults to "all".
#' @param MSE optional logical. If \code{TRUE}, the MSE estimates of the direct 
#' and model-based estimates are compared via boxplots and ordered scatter plots. 
#' Defaults to \code{FALSE}.
#' @param CV optional logical. If \code{TRUE}, the coefficient of variation 
#' estimates of the direct and model-based estimates are compared via boxplots 
#' and ordered scatter plots. Defaults to \code{FALSE}.
#' @param label argument that enables to customize title and axis labels. There 
#' are three options to label the evaluation plots: (i) original labels ("orig"), 
#' (ii) axis labels but no title ("no_title"), (iii) neither axis 
#' labels nor title ("blank").
#' @param color a vector with two elements. The first color determines
#' the color of the line in the scatter plot and the color for the direct 
#' estimates in the line plot. The second color specifies the color of the line
#' for the model-based estimates.
#' @param shape a numeric vector with two elements. The first shape determines
#' the shape of the points in the line plot for the direct estimates and the 
#' second shape for the model-based estimates. The options are numbered from 
#' 0 to 25. 
#' @param line_type a character vector with two elements. The first line type 
#' determines the type of the line for the direct estimates and the 
#' second type for the model-based estimates. The options are: "twodash", 
#' "solid", "longdash", "dotted", "dotdash", "dashed" and "blank".
#' @param gg_theme \code{\link[ggplot2]{theme}} list from package \pkg{ggplot2}.
#' For using this argument, package \pkg{ggplot2} must be loaded via 
#' \code{library(ggplot2)}. See also Example 2.
#' @param ... further arguments passed to or from other methods.
#' @return A scatter plot and a line plot comparing direct and model-based
#' estimators for each selected indicator obtained by \code{\link[ggplot2]{ggplot}}.
#' If the input arguments MSE and CV are set to TRUE two extra plots are created, 
#' respectively: the MSE/CV estimates of the direct and model-based estimates are 
#' compared by boxplots and scatter plots.
#' @details Since all of the comparisons need a direct estimator, the plots are 
#' only created for in-sample domains. For the new package version (2.0.0) the 
#' order of the input arguments direct and model has been changed. In this 
#' version (1.1.6), it is still possible to use the old order because the 
#' arguments are swapped internally. From the next package version on it will no 
#' longer be possible.
#' @seealso \code{\link{emdiObject}}, \code{\link{direct}}, \code{\link{ebp}}, 
#' \code{\link{fh}}
#' @export

compare_plot <- function(model, direct,  indicator = "all", MSE = FALSE, 
                         CV = FALSE, label = "orig", color = c("blue", "lightblue3"), 
                         shape = c(16, 16), line_type = c("solid", "solid"), 
                         gg_theme = NULL, ...) UseMethod("compare_plot")



#' Shows plots for the comparison of estimates
#'
#' For all indicators or a selection of indicators two plots are returned. The
#' first plot is a scatter plot of estimates to compare and the second is a line
#' plot with these estimates.
#' @param model an object of type "emdi","model", representing point and MSE
#' estimates
#' @param direct optional, an object of type "emdi","direct", representing point
#' and MSE estimates. If the input argument \code{model} is of type "model","ebp",
#' \code{direct} is required. If the input argument \code{model} is of type 
#' "model","fh", the \code{direct} component is already included in the input 
#' argument \code{model}..
#' @param indicator optional character vector that selects which indicators
#' shall be returned: (i) all calculated indicators ("all");
#' (ii) each indicator name: "Mean", "Quantile_10", "Quantile_25", "Median",
#' "Quantile_75", "Quantile_90", "Head_Count",
#' "Poverty_Gap", "Gini", "Quintile_Share" or the function name/s of
#' "custom_indicator/s"; (iii) groups of indicators: "Quantiles", "Poverty",
#' "Inequality" or "Custom".If two of these groups are selected, only the first
#' one is returned. Defaults to "all". Note, additional custom indicators can be
#' defined as argument for model-based approaches (see also \code{\link{ebp}})
#' and do not appear in groups of indicators even though these might belong to
#' one of the groups.
#' @return A scatter plot and a line plot comparing direct and model-based
#' estimators for each selected indicator obtained by \code{\link[ggplot2]{ggplot}}.
#' @keywords internal

compare_plot_ebp <- function(model, direct, indicator = "all", MSE = FALSE,
                             CV = FALSE, label = "orig",
                             color = c("blue", "lightblue3"),
                             shape = c(16, 16), line_type = c("solid", "solid"),
                             gg_theme = NULL) {
  
  Model_based <- NULL
  Direct <- NULL
  ID <- NULL
  value <- NULL
  Method <- NULL


  ind_direct <- point_emdi(object = direct, indicator = indicator)$ind
  selected_direct <- colnames(ind_direct)[-1]
  colnames(ind_direct) <- c("Domain", paste0(colnames(ind_direct)[-1], "_Direct"))

  ind_model <- point_emdi(object = model, indicator = indicator)$ind
  selected_model <- colnames(ind_model)[-1]
  colnames(ind_model) <- c("Domain", paste0(colnames(ind_model)[-1], "_Model"))
  smp_size <- (table(direct$framework$smp_domains_vec))

  compare_plot_check2(ind_direct, ind_model)

  Data <- merge(ind_direct, ind_model, by = "Domain" )

  matcher <- match(Data$Domain, names(smp_size))
  Data$smp_size <- as.numeric(smp_size)[matcher]

  if (MSE == TRUE || CV == TRUE ) {
    
    precisions_direct <- mse_emdi(object = direct, indicator = indicator, CV = TRUE)
    colnames(precisions_direct$ind) <- c("Domain", paste0(colnames(precisions_direct$ind)[-1], "_Direct_MSE"))
    colnames(precisions_direct$ind_cv) <- c("Domain", paste0(colnames(precisions_direct$ind_cv)[-1], "_Direct_CV"))

    precisions_model <- mse_emdi(object = model, indicator = indicator, CV = TRUE)
    colnames(precisions_model$ind) <- c("Domain", paste0(colnames(precisions_model$ind)[-1], "_Model_MSE"))
    colnames(precisions_model$ind_cv) <- c("Domain", paste0(colnames(precisions_model$ind_cv)[-1], "_Model_CV"))

    if (MSE == TRUE) {
      Data <- merge(Data, precisions_direct$ind, id = "Domain")
      Data <- merge(Data, precisions_model$ind, id = "Domain")
    }
    if (CV == TRUE) {
      Data <- merge(Data, precisions_direct$ind_cv, id = "Domain")
      Data <- merge(Data, precisions_model$ind_cv, id = "Domain")
      Data$smp_size2 <- Data$smp_size
    }

  }

  selected_indicators <- selected_model[selected_model %in% selected_direct]

  compare_plots(object = Data, type = "unit", selected_indicators = selected_indicators,
                MSE = MSE, CV = CV, label = label, color = color,
                shape = shape, line_type = line_type, gg_theme = gg_theme)

}


#' Shows plots for the comparison of estimates
#'
#' For all indicators or a selection of indicators two plots are returned. The
#' first plot is a scatter plot of estimates to compare and the second is a line
#' plot with these estimates.
#' @param model an object of type "emdi","model", representing point and MSE
#' estimates.
#' @param direct optional, an object of type "emdi","direct", representing point
#' and MSE estimates.
#' @param indicator optional character vector that selects which indicators
#' shall be returned: (i) all calculated indicators ("all");
#' (ii) each indicator name: "Mean", "Quantile_10", "Quantile_25", "Median",
#' "Quantile_75", "Quantile_90", "Head_Count",
#' "Poverty_Gap", "Gini", "Quintile_Share" or the function name/s of
#' "custom_indicator/s"; (iii) groups of indicators: "Quantiles", "Poverty",
#' "Inequality" or "Custom".If two of these groups are selected, only the first
#' one is returned. Defaults to "all". Note, additional custom indicators can be
#' defined as argument for model-based approaches (see also \code{\link{ebp}})
#' and do not appear in groups of indicators even though these might belong to
#' one of the groups.
#' @return A scatter plot and a line plot comparing direct and model-based
#' estimators for each selected indicator obtained by \code{\link[ggplot2]{ggplot}}.
#' @keywords internal

compare_plot_fh <- function(model, direct, indicator = "all", MSE = FALSE, CV = FALSE,
                            label = "orig", color = c("blue", "lightblue3"),
                             shape = c(16, 16), line_type = c("solid", "solid"),
                             gg_theme = NULL) {

  
  Model_based <- NULL
  Direct <- NULL
  ID <- NULL
  value <- NULL
  Method <- NULL

  

  Data <- point_emdi(object = model, indicator = "all")$ind
  Data <- Data[!is.na(Data$Direct), ]
  selected_indicators <- colnames(Data)[!(colnames(Data) %in% c('Domain', 'Direct'))]
  colnames(Data) <- c("Domain", "FH_Direct",
                            paste0(colnames(Data)[!(colnames(Data) %in% c('Domain', 'Direct'))], "_Model"))
  if ("FH_Bench" %in% selected_indicators) {
    Data$FH_Bench_Direct <- Data$FH_Direct
  }
  if ('FH_Bench' %in% indicator & !("FH_Bench" %in% selected_indicators)) {
    warning('emdi object does not contain benchmarked fh estimates. Only 
            FH estimates are compared with direct. See also help(benchmark_fh).')
  }
  
  if (!(any(indicator == "all") || any(indicator == "direct") || any(indicator == "Direct"))) {
    selected_indicators <- selected_indicators[selected_indicators %in% indicator]
  }

  if (is.null(model$MSE)) {
    Data$smp_size <- NULL
  } 
  
  # Streichen, wenn es nichts kaputt macht
  #else {
  #  Data$smp_size <- -model$MSE$Direct[model$MSE$Out == 0]
  #}
  

  if (MSE == TRUE || CV == TRUE) {
    all_precisions <- mse_emdi(object = model, indicator = "all", CV = TRUE)
    colnames(all_precisions$ind) <- c("Domain", paste0(c("FH_Direct", "FH_Model"), "_MSE"))
    colnames(all_precisions$ind_cv) <- c("Domain", paste0(c("FH_Direct", "FH_Model"), "_CV"))
    combined <- merge(all_precisions$ind, all_precisions$ind_cv, id = "Domain")
    combined <- combined[!is.na(combined$FH_Direct_MSE), ]

    Data <- merge(Data, combined, id = "Domain")
    Data$smp_size <- -Data$FH_Direct_MSE
    Data$smp_size2 <- -Data$FH_Direct_CV
  }

  #compare_plot_check2(ind_model, ind_direct)
  
  if (model$framework$N_dom_unobs > 0) {
    cat("Please not that since all of the comparisons need a direct estimator, 
  the plots are only created for in-sample domains. \n \n")
  }
  
  compare_plots(object = Data, type = "area", selected_indicators = selected_indicators,
                MSE = MSE, CV = CV, label = label, color = color,
                shape = shape, line_type = line_type, gg_theme = gg_theme)

}


#' Shows plots for the comparison of estimates
#'
#' Method \code{compare_plot.emdi} produces two plots for all indicators or a 
#' selection of indicators for objects of type "emdi". The
#' first plot is a scatter plot of estimates to compare and the second is a line
#' plot with these estimates.
#' @param model an object of type "emdi","model", representing point and MSE
#' estimates.
#' @param direct optional, an object of type "emdi","direct", representing point
#' and MSE estimates. If the input argument \code{model} is of type "model","ebp",
#' \code{direct} is required. If the input argument \code{model} is of type 
#' "model","fh", the \code{direct} component is already included in the input 
#' argument \code{model}.
#' @param indicator optional character vector that selects which indicators
#' shall be returned: (i) all calculated indicators ("all");
#' (ii) each indicator name: "Mean", "Quantile_10", "Quantile_25", "Median",
#' "Quantile_75", "Quantile_90", "Head_Count",
#' "Poverty_Gap", "Gini", "Quintile_Share" or the function name/s of
#' "custom_indicator/s"; (iii) groups of indicators: "Quantiles", "Poverty",
#' "Inequality" or "Custom". If two of these groups are selected, only the first
#' one is returned. Note, additional custom indicators can be
#' defined as argument for the EBP approaches (see also \code{\link{ebp}})
#' and do not appear in groups of indicators even though these might belong to
#' one of the groups. If the \code{model} argument is of type "model","fh", 
#' indicator can be set to "all", "FH" or "Direct". Defaults to "all".
#' @param MSE optional logical. If \code{TRUE}, the MSE estimates of the direct 
#' and model-based estimates are compared via boxplots and ordered scatter plots.
#' @param CV optional logical. If \code{TRUE}, the coefficient of variation 
#' estimates of the direct and model-based estimates are compared via boxplots 
#' and ordered scatter plots.
#' @param label argument that enables to customize title and axis labels. There 
#' are three options to label the evaluation plots: (i) original labels ("orig"), 
#' (ii) axis labels but no title ("no_title"), (iii) neither axis 
#' labels nor title ("blank").
#' @param color a vector with two elements. The first color determines
#' the color of the line in the scatter plot and the color for the direct 
#' estimates in the line plot. The second color specifies the color of the line
#' for the model-based estimates.
#' @param shape a numeric vector with two elements. The first shape determines
#' the shape of the points in the line plot for the direct estimates and the 
#' second shape for the model-based estimates. The options are numbered from 
#' 0 to 25. 
#' @param line_type a character vector with two elements. The first line type 
#' determines the type of the line for the direct estimates and the 
#' second type for the model-based estimates. The options are: "twodash", 
#' "solid", "longdash", "dotted", "dotdash", "dashed" and "blank".
#' @param gg_theme \code{\link[ggplot2]{theme}} list from package \pkg{ggplot2}.
#' For using this argument, package \pkg{ggplot2} must be loaded via 
#' \code{library(ggplot2)}. See also Example 2.
#' @param ... further arguments passed to or from other methods.
#' @return A scatter plot and a line plot comparing direct and model-based
#' estimators for each selected indicator obtained by \code{\link[ggplot2]{ggplot}}.
#' @details Since all of the comparisons need a direct estimator, the plots are 
#' only created for in-sample domains. For the new package version (2.0.0) the 
#' order of the input arguments direct and model has been changed. In this 
#' version (1.1.6), it is still possible to use the old order because the 
#' arguments are swapped internally. From the next package version on it will no 
#' longer be possible.
#' @seealso \code{\link{emdiObject}}, \code{\link{direct}}, \code{\link{ebp}}, 
#' \code{\link{fh}} 
#' @examples
#' \dontrun{
#' # Examples for comparisons of direct estimates and models of type ebp
#' 
#' # Loading data - population and sample data
#' data("eusilcA_pop")
#' data("eusilcA_smp")
#'   
#' # Generation of two emdi objects
#' emdi_model <- ebp(fixed = eqIncome ~ gender + eqsize + cash + 
#' self_empl + unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + 
#' fam_allow + house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop,
#' pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district",
#' threshold = function(y){0.6 * median(y)}, L = 50, MSE = TRUE,
#' na.rm = TRUE, cpus = 1)
#' 
#' emdi_direct <- direct(y = "eqIncome", smp_data = eusilcA_smp,
#' smp_domains = "district", weights = "weight", threshold = 11161.44,
#' var = TRUE, boot_type = "naive", B = 50, seed = 123, na.rm = TRUE)
#' 
#' # Example 1: Receive first overview
#' compare_plot(model = emdi_model, direct = emdi_direct)
#' 
#' # Example 2: Change plot theme
#' library(ggplot2)
#' compare_plot(emdi_model, emdi_direct, indicator = "Median",
#' gg_theme = theme(axis.line = element_line(size = 3, colour = "grey80"),
#' plot.background = element_rect(fill = "lightblue3"),
#' legend.position = "none"))
#' 
#' # Example for comparison of direct estimates and models of type fh
#' 
#' # Loading data - population and sample data
#' data("eusilcA_popAgg")
#' data("eusilcA_smpAgg")
#'
#' # Combine sample and population data -------------------------------------------
#' combined_data <- combine_data(pop_data = eusilcA_popAgg, pop_domains = "Domain",
#'                               smp_data = eusilcA_smpAgg, smp_domains = "Domain")
#'
#' # Generation of the emdi object
#' fh_std <- fh(fixed = Mean ~ cash + self_empl, vardir = "Var_Mean",
#'              combined_data = combined_data, domains = "Domain", method = "ml", 
#'              MSE = TRUE)
#' # Example 3: Receive first overview
#' compare_plot(fh_std) 
#' 
#' # Example 4: Compare also MSE and CV estimates 
#' compare_plot(fh_std, MSE = TRUE, CV = TRUE)                       
#' }
#' @export
#' @importFrom reshape2 melt
#' @importFrom ggplot2 geom_point geom_smooth geom_line geom_boxplot
#' @importFrom ggplot2 aes xlim ylim scale_shape_manual scale_linetype_manual coord_flip
#' @importFrom ggplot2 scale_color_manual scale_fill_manual


compare_plot.emdi <- function(model = NULL, direct = NULL, indicator = "all",
                              MSE = FALSE, CV = FALSE, label = "orig",
                            color = c("blue", "lightblue3"),
                            shape = c(16, 16), line_type = c("solid", "solid"),
                            gg_theme = NULL, ...) {
  
  compare_plot_check(model = model, indicator = indicator,
                    label = label, color = color, shape = shape,
                  line_type = line_type, gg_theme = gg_theme)

   if (inherits(model, "direct") & inherits(direct, "model")){
     model_orig <- model
     direct_orig <- direct
     model <- direct_orig
     direct <- model_orig
     warning("Please note that for the new package version (2.0.0) the order of the input 
         arguments direct and model has been changed. In this version (1.1.6), it is 
         still possible to use the old order because the arguments are swapped 
         internally. From the next version on it will no longer be possible.")
   }
  
  if(is.null(model) & inherits(direct, "fh") ){
      model <- direct
  } 
  
  if((inherits(direct, "fh") & inherits(model, "ebp")) |
    (inherits(direct, "ebp") & inherits(model, "fh"))) {
    stop(paste0("It is not possible to compare the point and MSE estimates of a
model of type 'model','fh', to the point and MSE estimates of a 'model','ebp' object.")) 
  }
   if((is.null(model) & inherits(direct, "direct")) |
      (is.null(direct) & inherits(model, "direct"))) {
     stop(paste0("If the direct argument is of type 'emdi','direct', the input 
                 argument model is required.")) 
   } 
   if(inherits(model, "fh")  & inherits(direct, "direct")) {
      stop(paste0("It is not possible to compare the point and MSE estimates of a
model of type 'model','fh', to the point and MSE estimates of a 'direct' object."))
   } else if(inherits(model, "fh")){
     compare_plot_fh(model = model, direct = model, indicator = indicator,
                     MSE = MSE, CV = CV,
                     label = label, color = color, shape = shape,
                     line_type = line_type, gg_theme = gg_theme)
   }
    
  if((inherits(model, "ebp") & is.null(direct)) | 
     (inherits(direct, "ebp") & is.null(model))) {
      stop(paste0("If the model is of type 'model','ebp', the input argument 
                  direct is required."))
    } else if(inherits(model, "ebp") & inherits(direct, "direct")) {
      compare_plot_ebp(model = model, direct = direct, indicator = indicator,
                       MSE = MSE, CV = CV,
                       label = label, color = color, shape = shape,
                       line_type = line_type, gg_theme = gg_theme) 
    }
   
}



define_evallabel <- function(type, label, indi){
  if (!inherits(label, "list")) {
    if (label == "orig") {

      if (type == "unit") {
        label <- list(scatter = c(title = indi,
                                  y_lab = "Model-based",
                                  x_lab = "Direct"),
                      line = c(title = indi,
                               y_lab = "Value",
                               x_lab = "Domain (ordered by sample size)"),
                      boxplot_MSE = c(title = indi,
                               y_lab = "MSE",
                               x_lab = ""),
                      ordered_MSE = c(title = indi,
                               y_lab = "MSE",
                               x_lab = "Domain (ordered by sample size)"),
                      boxplot_CV = c(title = indi,
                                  y_lab = "CV",
                                  x_lab = ""),
                      ordered_CV = c(title = indi,
                                  y_lab = "CV",
                                  x_lab = "Domain (ordered by sample size)"))
      } else if (type == "area") {
        label <- list(scatter = c(title = indi,
                                  y_lab = "Model-based",
                                  x_lab = "Direct"),
                      line = c(title = indi,
                               y_lab = "Value",
                               x_lab = "Domain (ordered by decreasing MSE of Direct)"),
                      boxplot_MSE = c(title = indi,
                                      y_lab = "",
                                      x_lab = "MSE"),
                      ordered_MSE = c(title = indi,
                                      y_lab = "MSE",
                                      x_lab = "Domain (ordered by decreasing MSE of Direct)"),
                      boxplot_CV = c(title = indi,
                                     y_lab = "CV",
                                     x_lab = ""),
                      ordered_CV = c(title = indi,
                                     y_lab = "CV",
                                     x_lab = "Domain (ordered by decreasing CV of Direct)"))
      }

    } else if (label == "blank") {
      label <- list(scatter = c(title = "",
                               y_lab = "",
                               x_lab = ""),
                    line = c(title = "",
                               y_lab = "",
                               x_lab = ""),
                    boxplot_MSE = c(title = "",
                             y_lab = "",
                             x_lab = ""),
                    ordered_MSE = c(title = "",
                             y_lab = "",
                             x_lab = ""),
                    boxplot_CV = c(title = "",
                             y_lab = "",
                             x_lab = ""),
                    ordered_CV = c(title = "",
                             y_lab = "",
                             x_lab = ""))
    } else if (label == "no_title") {

      if (type == "unit") {
        label <- list(scatter = c(title = "",
                                  y_lab = "Model-based",
                                  x_lab = "Direct"),
                      line = c(title = "",
                               y_lab = "Value",
                               x_lab = "Domain (ordered by sample size)"),
                      boxplot_MSE = c(title = "",
                                      y_lab = "MSE",
                                      x_lab = ""),
                      ordered_MSE = c(title = "",
                                      y_lab = "MSE",
                                      x_lab = "Domain (ordered by sample size)"),
                      boxplot_CV = c(title = "",
                                     y_lab = "CV",
                                     x_lab = ""),
                      ordered_CV = c(title = "",
                                     y_lab = "CV",
                                     x_lab = "Domain (ordered by sample size)"))
      } else if (type == "area") {
        label <- list(scatter = c(title = "",
                                  y_lab = "Model-based",
                                  x_lab = "Direct"),
                      line = c(title = "",
                               y_lab = "Value",
                               x_lab = "Domain (ordered by decreasing MSE of Direct)"),
                      boxplot_MSE = c(title = "",
                                      y_lab = "",
                                      x_lab = "MSE"),
                      ordered_MSE = c(title = "",
                                      y_lab = "MSE",
                                      x_lab = "Domain (ordered by decreasing MSE of Direct)"),
                      boxplot_CV = c(title = "",
                                     y_lab = "CV",
                                     x_lab = ""),
                      ordered_CV = c(title = "",
                                     y_lab = "CV",
                                     x_lab = "Domain (ordered by decreasing CV of Direct)"))
      }

    }

  }

  #else if (inherits(label, "list")) {

  #  if (!any(names(label) %in% c("scatter", "line"))) {
  #    stop("List elements must have following names even though not
  #         all must be included: scatter and line Every list element must
  #         have the elements title, y_lab and x_lab.")
  #  }
  #  for (i in names(label)) {
  #    if (!all(names(label[[i]]) == c("title", "y_lab", "x_lab"))) {
  #      stop("Every list element must have the elements title,
  #           y_lab and x_lab in this order.")
  #    }
  #    }

  #  if (type == "unit") {
  #    orig_label <- list(scatter = c(title = indi,
  #                              y_lab = "Model-based",
  #                              x_lab = "Direct"),
  #                  line = c(title = indi,
  #                           y_lab = "Value",
  #                           x_lab = "Domain (ordered by sample size)"),
  #                  boxplot_MSE = c(title = indi,
  #                                  y_lab = "Value",
  #                                  x_lab = "Domain (ordered by sample size)"),
  #                  ordered_MSE = c(title = indi,
  #                                  y_lab = "Value",
  #                                  x_lab = "Domain (ordered by sample size)"),
  #                  boxplot_CV = c(title = indi,
  #                                 y_lab = "Value",
  #                                 x_lab = "Domain (ordered by sample size)"),
  #                  ordered_CV = c(title = indi,
  #                                 y_lab = "Value",
  #                                 x_lab = "Domain (ordered by sample size)"))
  #  } else if (type == "area") {
  #    orig_label <- list(scatter = c(title = indi,
  #                              y_lab = "Model-based",
  #                              x_lab = "Direct"),
  #                  line = c(title = indi,
  #                           y_lab = "Value",
  #                           x_lab = "Domain (ordered by sample size)"),
  #                  boxplot_MSE = c(title = indi,
  #                                  y_lab = "",
  #                                  x_lab = "MSE"),
  #                  ordered_MSE = c(title = indi,
  #                                  y_lab = "MSE",
  #                                  x_lab = "Domain (ordered by increasing MSE of Direct)"),
  #                  boxplot_CV = c(title = indi,
  #                                 y_lab = "CV",
  #                                 x_lab = ""),
  #                  ordered_CV = c(title = indi,
  #                                 y_lab = "Value",
  #                                 x_lab = "Domain (ordered by increasing CV of Direct)"))
  #  }


  #  if (any(names(label) == "scatter")) {
  #    label$scatter <- label$scatter
  #  } else {
   #   label$scatter <- orig_label$scatter
  #  }
  #  if (any(names(label) == "line")) {
  #    label$line <- label$line
  #  } else {
  #    label$line <- orig_label$line
  #  }
  #    }

  #if (any(!(names(label) %in%  c("scatter", "line")))) {
  #  warning("One or more list elements are not called scatter or line. The
  #           changes are for this/these element(s) is/are not done. Instead the
  #          original labels are used.")
  #}

  return(label)
    }


compare_plots <- function(object, type, selected_indicators, MSE, CV, label, color,
                          shape, line_type, gg_theme,...) {

  Model_based <- NULL
  Direct <- NULL
  ID <- NULL
  value <- NULL
  Method <- NULL
  slope <- NULL
  intercept <- NULL
  area <- NULL
  
  
  if (MSE == FALSE & CV == FALSE) {
    plotList <- vector(mode = "list", length = length(selected_indicators) * 2)
    names(plotList) <- paste(rep(c("scatter", "line"), length(selected_indicators)),
                             rep(selected_indicators, each = 2), sep = "_")
  } else if ((MSE == TRUE | CV == TRUE) & !(MSE == TRUE & CV == TRUE)) {
    plotList <- vector(mode = "list", length = length(selected_indicators) * 4)
    names(plotList) <- paste(rep(c("scatter", "line"), length(selected_indicators)),
                             rep(selected_indicators, each = 4), sep = "_")
  } else if (MSE == TRUE & CV == TRUE) {
    plotList <- vector(mode = "list", length = length(selected_indicators) * 6)
    names(plotList) <- paste(rep(c("scatter", "line"), length(selected_indicators)),
                             rep(selected_indicators, each = 6), sep = "_")
  }

  #scatter line
  for (ind in selected_indicators) {

    label_ind <- define_evallabel(type = type, label = label, indi = ind)

    if (is.null(object$smp_size)) {
      data_tmp <- data.frame(Direct = object[, paste0(ind, "_Direct")],
                             Model_based = object[, paste0(ind, "_Model")])
      label_ind$line["x_lab"] <- "Domains (unordered)"
    } else {
      data_tmp <- data.frame(Direct = object[, paste0(ind, "_Direct")],
                             Model_based = object[, paste0(ind, "_Model")],
                             smp_size = object$smp_size)
      data_tmp <- data_tmp[order(data_tmp$smp_size), ]
      data_tmp$smp_size <- NULL
    }
    
    data_tmp$ID <- seq_along(object$Domain)
    data_shaped <- melt(data_tmp, id.vars = "ID")
    names(data_shaped) <- c("ID", "Method", "value")
    

    print((plotList[[paste("scatter", ind, sep = "_")]] <- 
             ggplot(data_tmp, aes(x = Direct, y = Model_based)) + 
             geom_point(shape = shape[1]) +
             geom_smooth(method = lm, se = FALSE, inherit.aes = FALSE, 
                         lty = line_type[1],
                         aes(colour = "Reg. line", x = Direct, y = Model_based)) + 
             geom_abline(mapping = aes(colour = "Intersection", 
                                       slope = slope, intercept = intercept),
                         data.frame(slope = 1, intercept = 0), 
                         lty = line_type[2]) +
             xlim(min(min(data_tmp$Direct), min(data_tmp$Model_based)), 
                  max(max(data_tmp$Direct), max(data_tmp$Model_based))) +
             ylim(min(min(data_tmp$Direct), min(data_tmp$Model_based)), 
                  max(max(data_tmp$Direct), max(data_tmp$Model_based))) +
             ggtitle(label_ind$scatter["title"]) + 
             ylab(label_ind$scatter["y_lab"]) + 
             xlab(label_ind$scatter["x_lab"]) +
             scale_color_manual(name = "",values = c("Intersection" = color[2], 
                                                     "Reg. line" = color[1])) +  
             gg_theme))
    
    cat("Press [enter] to continue")
    line <- readline()


    print((plotList[[paste("line", ind, sep = "_")]] <-
             ggplot(data = data_shaped, aes(x = ID,
                                            y = value, group = Method,
                                            colour = Method)) +
             geom_line(aes(linetype = Method), size = 0.7) +
             geom_point(aes(color = Method, shape = Method), size = 2) +
             scale_shape_manual(values = c(shape[1], shape[2]),
                                breaks = c("Direct", "Model_based"),
                                labels = c("Direct", "Model-based")) +
             scale_linetype_manual(values = c(line_type[1], line_type[2]),
                                   breaks = c("Direct", "Model_based"),
                                   labels = c("Direct", "Model-based")) +
             scale_color_manual(values = c(color[1], color[2]),
                                breaks = c("Direct", "Model_based"),
                                labels = c("Direct", "Model-based")) +
             scale_fill_manual(name = "Method",
                               breaks = c("Direct", "Model_based"),
                               labels = c("Direct", "Model-based")) +
             xlab(label_ind$line["x_lab"]) + ylab(label_ind$line["y_lab"]) +
             ggtitle(label_ind$line["title"]) + gg_theme))

    if (MSE == TRUE) {

      data_tmp2 <- data.frame(Direct = object[, paste0(ind, "_Direct_MSE")],
                              Model_based = object[, paste0(ind, "_Model_MSE")],
                              smp_size = object$smp_size)

      data_tmp2 <- data_tmp2[order(data_tmp2$smp_size, decreasing = TRUE), ]
      data_tmp2$smp_size <- NULL
      data_tmp2$ID <- seq_along(object$Domain)
      data_shaped <- melt(data_tmp2, id.vars = "ID")
      names(data_shaped) <- c("ID", "Method", "value")
      data_shaped$area <- rep(1:NROW(data_tmp2$Direct), 2)

      cat("Press [enter] to continue")
      line <- readline()

      print((plotList[[paste("boxplot", "MSE", ind, sep = "_")]] <- ggplot(data_shaped,
                                                                        aes(x = Method, y = value,
                                                                            fill = Method)) +
               geom_boxplot() +
               coord_flip() +
               labs(title = label_ind$boxplot_MSE["title"],
                    x = label_ind$boxplot_MSE["x_lab"],
                    y = label_ind$boxplot_MSE["y_lab"]) +
               scale_fill_manual(name = "Method",
                                 values = color)))

      cat("Press [enter] to continue")
      line <- readline()

      print((plotList[[paste("ordered", "MSE", ind, sep = "_")]] <- ggplot(data_shaped,
                                                                        aes(x = area,
                                                                            y = value,
                                                                            colour = Method)) +
               geom_point(aes(color = Method, shape = Method)) +
               labs(title = label_ind$ordered_MSE["title"],
                                   x = label_ind$ordered_MSE["x_lab"],
                                   y = label_ind$ordered_MSE["y_lab"]) +
               scale_color_manual(values = color)) +
        scale_shape_manual(values = c(shape[1], shape[2])))
    }

    if (CV == TRUE) {

      data_tmp3 <- data.frame(Direct = object[, paste0(ind, "_Direct_CV")],
                              Model_based = object[, paste0(ind, "_Model_CV")],
                              smp_size = object$smp_size2)

      data_tmp3 <- data_tmp3[order(data_tmp3$smp_size, decreasing = TRUE), ]
      data_tmp3$smp_size <- NULL
      data_tmp3$ID <- seq_along(object$Domain)
      data_shaped <- melt(data_tmp3, id.vars = "ID")
      names(data_shaped) <- c("ID", "Method", "value")
      data_shaped$area <- rep(1:NROW(data_tmp3$Direct), 2)

      cat("Press [enter] to continue")
      line <- readline()

      print((plotList[[paste("boxplot", "CV", ind, sep = "_")]] <- ggplot(data_shaped,
                                                                        aes(x = Method, y = value,
                                                                            fill = Method)) +
               geom_boxplot() +
               coord_flip() +
               labs(title = label_ind$boxplot_CV["title"],
                    x = label_ind$boxplot_CV["x_lab"],
                    y = label_ind$boxplot_CV["y_lab"]) +
               scale_fill_manual(name = "Method",
                                 values = color)))

      cat("Press [enter] to continue")
      line <- readline()

      data_shaped

      print((plotList[[paste("ordered", "CV", ind, sep = "_")]] <- ggplot(data_shaped,
                                                                        aes(x = area,
                                                                            y = value,
                                                                            colour = Method)) +
               geom_point(aes(color = Method, shape = Method)) +
               labs(title = label_ind$ordered_CV["title"],
                                   x = label_ind$ordered_CV["x_lab"],
                                   y = label_ind$ordered_CV["y_lab"]) +
               scale_color_manual(values = color))+
        scale_shape_manual(values = c(shape[1], shape[2])))
    }


    if (!ind == tail(selected_indicators, 1)) {
      cat("Press [enter] to continue")
      line <- readline()
    }
  }
  invisible(plotList)
}

