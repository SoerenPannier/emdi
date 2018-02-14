#' Shows plots for the comparison of direct and model-based estimates
#'
#' For all indicators or a selection of indicators two plots are returned. The 
#' first plot is a scatter plot of the direct and model-based point estimates 
#' and the second is a line plots with both point estimates. 
#' @param direct an object of type "emdi","direct", representing point and MSE
#' estimates. 
#' @param model an object of type "emdi","model", representing point and MSE
#' estimates. 
#' @param indicator optional character vector that selects which indicators
#' shall be returned: (i) all calculated indicators ("all");
#' (ii) each indicator name: "Mean" "Quantile_10", "Quantile_25", "Median",
#' "Quantile_75", "Quantile_90", "Head_Count", 
#' "Poverty_Gap", "Gini", "Quintile_Share" or the function name/s of 
#' "custom_indicator/s"; (iii) groups of indicators: "Quantiles", "Poverty", 
#' "Inequality" or "Custom".If two of these groups are selected, only the first
#' one is returned. Defaults to "all". Note, additional custom indicators can be 
#' defined as argument for model-based approaches (see also \code{\link{ebp}}) 
#' and do not appear in groups of indicators even though these might belong to 
#' one of the groups.
#' @param label argument that enables to customize title and axis labels. There 
#' are four options to label the evaluation plots: (i) original labels ("orig"), 
#' (ii) axis lables but no title ("no_title"), (iii) neither axis 
#' labels nor title ("blank").  
#' @param color a character vector with two elements. The first color determines
#' the color of the line in the scatter plot and the color for the direct 
#' estimates in the line plot. The second color specifies the color of the line
#' for the model-based estimates.
#' @param shape a character vector with two elements. The first shape determines
#' the shape of the points in the line plot for the direct estimates and the 
#' second shape for the model-based estimates. 
#' @param line_type a character vector with two elements. The first line type 
#' determines the type of the line for the direct estimates and the 
#' second type for the model-based estimates.
#' @param gg_theme \code{\link[ggplot2]{theme}} list from package \pkg{ggplot2}.
#' @return A scatter plot and a line plot comparing direct and model-based 
#' estimators for each selected indicator obtained by \code{\link{ggplot}}.
#' @seealso \code{\link{emdiObject}}, \code{\link{direct}}, \code{\link{ebp}}, 
#' @examples
#' \dontrun{
#' # Loading data - population and sample data
#' data("eusilcA_pop")
#' data("eusilcA_smp")
#'   
#' # Generation of two emdi objects
#' emdi_model <- ebp(fixed = eqIncome ~ gender + eqsize + cash + 
#' self_empl + unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + 
#' fam_allow + house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop,
#' pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district",
#' threshold = function(y){0.6 * median(y)}, L = 50, MSE = TRUE, B = 50,
#' na.rm = TRUE, cpus = 1)
#' 
#' emdi_direct <- direct(y = "eqIncome", smp_data = eusilcA_smp,
#' smp_domains = "district", weights = "weight", threshold = 10989.28,
#' var = TRUE, boot_type = "naive", B = 50, seed = 123, na.rm = TRUE)
#' 
#' # Receive first overview
#' evaluate_plot(direct = emdi_direct, model = emdi_model)
#' }
#' @export
#' @importFrom reshape2 melt
#' @importFrom ggplot2 aes geom_point geom_smooth coord_fixed geom_line


evaluate_plot <- function(direct, model, indicator = "all", label = "orig", 
                          color = c("blue", "lightblue3"),
                          shape = c(16, 16), line_type = c("solid", "solid"),
                          gg_theme = NULL) {
  
  Direct <- NULL
  Model_based <- NULL
  ID <- NULL
  value <- NULL
  Method <- NULL
  pchisq <- NULL
  
  
  ind_direct <- point_emdi(object = direct, indicator = indicator)$ind 
  selected_direct <- colnames(ind_direct)[-1]
  colnames(ind_direct) <- c("Domain", paste0(colnames(ind_direct)[-1], "_Direct"))
  
  
  precisions_direct <- mse_emdi(object = direct, indicator = indicator, CV = TRUE)
  cv_direct <- precisions_direct$ind_cv  
  
  
  ind_model <- point_emdi(object = model, indicator = indicator)$ind 
  selected_model <- colnames(ind_model)[-1]
  colnames(ind_model) <- c("Domain", paste0(colnames(ind_model)[-1], "_Model"))
  smp_size <- (table(direct$framework$smp_domains_vec))
   
  Data <- merge(ind_direct, ind_model, by = "Domain")
  matcher <- match(Data$Domain, names(smp_size))
  Data$smp_size <- as.numeric(smp_size)[matcher]
  selection_indicators <- selected_model %in% selected_direct
  selected_indicators <- selected_direct[selection_indicators]
  
  
  for (ind in selected_indicators) {
    
    label <- define_evallabel(label = label, indicator = ind)
    
    data_tmp <- data.frame(Direct = Data[, paste0(ind, "_Direct")],
                           Model_based = Data[, paste0(ind, "_Model")],
                           smp_size = Data$smp_size)
    
    print(ggplot(data_tmp, aes(x = Direct, y = Model_based)) + 
            geom_point() +
            geom_smooth(method = lm, color = color[1], 
                        se = FALSE
            ) + geom_abline(intercept = 0, slope = 1, size = 1, 
                            color = color[2]) +
            xlim(min(min(data_tmp$Direct), min(data_tmp$Model_based)), 
                     max(max(data_tmp$Direct), max(data_tmp$Model_based))) +
            ylim(min(min(data_tmp$Direct), min(data_tmp$Model_based)), 
                 max(max(data_tmp$Direct), max(data_tmp$Model_based))) +
            ggtitle(label$scatter["title"]) + 
            ylab(label$scatter["y_lab"]) + 
      xlab(label$scatter["x_lab"]) + gg_theme)
    cat("Press [enter] to continue")
    line <- readline()
    
    data_tmp <- data_tmp[order(data_tmp$smp_size), ]
    data_tmp$smp_size <- NULL
    data_tmp$ID <- seq_along(ind_direct$Domain)
    data_shaped <- melt(data_tmp, id.vars = "ID")
    names(data_shaped) <- c("ID", "Method", "value")
    
    print(ggplot(data = data_shaped, aes(x = ID, 
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
            xlab(label$line["x_lab"]) + ylab(label$line["y_lab"]) + 
            ggtitle(label$line["title"]) + gg_theme)
    #cat("Press [enter] to continue")
    #line <- readline()
    
    if (!ind == tail(selected_indicators,1)) {
      cat("Press [enter] to continue")
      line <- readline()
    }
  }
}

define_evallabel <- function(label, indicator){
  if (!inherits(label, "list")) {
    if (label == "orig") {
      label <- list(scatter = c(title = indicator, 
                               y_lab = "Model-based", 
                               x_lab = "Direct"),
                    line = c(title = indicator,
                               y_lab = "Value", 
                               x_lab = "Domain (ordered by sample size)"))
    } else if (label == "blank") {
      label <- list(scatter = c(title = "", 
                               y_lab = "", 
                               x_lab = ""),
                    line = c(title = "",
                               y_lab = "", 
                               x_lab = ""))
    } else if (label == "no_title") {
      label <- list(scatter = c(title = "", 
                                y_lab = "Model-based", 
                                x_lab = "Direct"),
                    line = c(title = "",
                             y_lab = "Value", 
                             x_lab = "Domain (ordered by sample size)"))
    }
    
  } else if (inherits(label, "list")) {
    
    if (!any(names(label) %in% c("scatter", "line"))) {
      stop("List elements must have following names even though not 
           all must be included: scatter and line Every list element must 
           have the elements title, y_lab and x_lab.")
    }
    for (i in names(label)) {
      if (!all(names(label[[i]]) == c("title", "y_lab", "x_lab"))) {
        stop("Every list element must have the elements title, 
             y_lab and x_lab in this order.")
      }
      }

    orig_label <- list(scatter = c(title = indicator, 
                                   y_lab = "Model-based", 
                                   x_lab = "Direct"),
                       line = c(title = indicator,
                                y_lab = "Value", 
                                x_lab = "Domain (ordered by sample size)"))
    
    if (any(names(label) == "scatter")) {
      label$scatter <- label$scatter
    } else {
      label$scatter <- orig_label$scatter
    }
    if (any(names(label) == "line")) {
      label$line <- label$line
    } else {
      label$line <- orig_label$line
    }
      }
  
  if (any(!(names(label) %in%  c("scatter", "line")))) {
    warning("One or more list elements are not called scatter or line. The 
             changes are for this/these element(s) is/are not done. Instead the 
            original labels are used.")
  }
  
  return(label)
    }



#' Goodness of fit diagnostic of direct and model-based estimates
#'
#' For all indicators or a selection of indicators a goodness of fit test 
#' following \cite{Brown et al. (2001)} is conducted. The returned object 
#' is suitable for printing  with the \code{print.evaluate.emdi} method. 
#' @param direct an object of type "emdi","direct", representing point and MSE
#' estimates. 
#' @param model an object of type "emdi","model", representing point and MSE
#' estimates. 
#' @param indicator optional character vector that selects which indicators
#' shall be returned: (i) all calculated indicators ("all");
#' (ii) each indicator name: "Mean" "Quantile_10", "Quantile_25", "Median",
#' "Quantile_75", "Quantile_90", "Head_Count", 
#' "Poverty_Gap", "Gini", "Quintile_Share" or the function name/s of 
#' "custom_indicator/s"; (iii) groups of indicators: "Quantiles", "Poverty", 
#' "Inequality" or "Custom".If two of these groups are selected, only the first
#' one is returned. Defaults to "all". Note, additional custom indicators can be 
#' defined as argument for model-based approaches (see also \code{\link{ebp}}) 
#' and do not appear in groups of indicators even though these might belong to 
#' one of the groups.  
#' @return an object of type "evaluate.emdi" containing the results of the 
#' goodness of fit test.
#' @references
#' Brown, G., Chambers, R., Heady, P. and Heasmann, D. (2001). Evaluation of 
#' Small Area Estimation Methods - An Application to Unemployment Estimates from 
#' the UK LFS. In Proceedings of Statistics Canada Symposium 2001: Achieving Data 
#' Quality in a Statistical Agency: A Methodological Perspective. Statistics 
#' Canada. \cr \cr
#' @seealso \code{\link{emdiObject}}, \code{\link{direct}}, \code{\link{ebp}}, 
#' @examples
#' \dontrun{
#' # Loading data - population and sample data
#' data("eusilcA_pop")
#' data("eusilcA_smp")
#'   
#' # Generation of two emdi objects
#' emdi_model <- ebp(fixed = eqIncome ~ gender + eqsize + cash + 
#' self_empl + unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + 
#' fam_allow + house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop,
#' pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district",
#' threshold = function(y){0.6 * median(y)}, L = 50, MSE = TRUE, B = 50,
#' na.rm = TRUE, cpus = 1)
#' 
#' emdi_direct <- direct(y = "eqIncome", smp_data = eusilcA_smp,
#' smp_domains = "district", weights = "weight", threshold = 10989.28,
#' var = TRUE, boot_type = "naive", B = 50, seed = 123, na.rm = TRUE)
#' 
#' # Receive first overview
#' evaluate_test(direct = emdi_direct, model = emdi_model)
#' }
#' @importFrom stats pchisq
#' @export


evaluate_test <- function(direct, model, indicator = "all") {
  
  ind_direct <- point_emdi(object = direct, indicator = indicator)$ind 
  selected_direct <- colnames(ind_direct)[-1]
  colnames(ind_direct) <- c("Domain", paste0(colnames(ind_direct)[-1], "_Direct"))
  
  precisions_direct <- mse_emdi(object = direct, indicator = indicator, CV = TRUE)
  cv_direct <- precisions_direct$ind_cv  
  mse_direct <- precisions_direct$ind
  colnames(mse_direct) <- c("Domain", paste0(colnames(mse_direct)[-1], "_Var"))
  smp_size <- as.numeric(table(direct$framework$smp_domains_vec))
  
  ind_model <- point_emdi(object = model, indicator = indicator)$ind 
  selected_model <- colnames(ind_model)[-1]
  colnames(ind_model) <- c("Domain", paste0(colnames(ind_model)[-1], "_Model"))
  
  precisions_model <- mse_emdi(object = model, indicator = indicator, CV = TRUE)
  cv_model <- precisions_model$ind_cv  
  mse_model <- precisions_model$ind
  colnames(mse_model) <- c("Domain", paste0(colnames(mse_model)[-1], "_MSE"))
  
  
  data <- Reduce(function(...) merge(...), list(ind_direct, mse_direct,
                                                ind_model, mse_model))
  
  selection_indicators <- selected_model %in% selected_direct
  selected_indicators <- selected_direct[selection_indicators]
  df <- length(data$Domain)
  
  W <- NULL
  p_value <- NULL
  
  for (ind in selected_indicators) {
    
    W_tmp <- sum((data[, paste0(ind, "_Direct")] - data[, paste0(ind, "_Model")])^2 / 
                   (data[, paste0(ind, "_Var")] + data[, paste0(ind, "_MSE")]), na.rm = TRUE)
    p_value_tmp <- 1 - pchisq(W_tmp, df)
    
    W <- c(W, W_tmp)
    p_value <- c(p_value, p_value_tmp)
  }
  
  brown_test <- data.frame(Indicator = selected_indicators, W = W, 
                           p_value = round(p_value,3) 
                           #row.names = ""
  )
  
  evaluate <- list(brown_test = brown_test)
  
  class(evaluate) <- "evaluate.emdi"
  evaluate
}


#' Prints a evaluate.emdi object
#'
#' The results of the goodness of fit test are printed.
#' @param x an object of type "evaluate.emdi", generally resulting
#' from applying evaluate_test to two objects of type "emdi", "direct" and
#' "emdi", "model".
#' @param ... optional arguments passed to print.default; see the documentation on
#' that method functions.
#' @seealso
#' \code{\link{evaluate_test}}
#' @export

print.evaluate.emdi <- function(x,...) {
  cat("Goodness of fit test\n")
  cat("\n")
  print(x$brown_test)
}
