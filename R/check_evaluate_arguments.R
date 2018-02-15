evaluate_plot_check <- function(direct, model, indicator, label, color, shape, 
                                line_type, gg_theme) {
  
  if (!all(inherits(direct, which = TRUE, c("emdi", "direct")))) {
    stop('First object needs to be of class emdi, direct.')
  }
  if (!all(inherits(model, which = TRUE, c("emdi", "model")))) {
    stop('First object needs to be of class emdi, model.')
  }
  if (is.null(indicator) || !(indicator == "all" || indicator == "Quantiles" 
                              || indicator == "quantiles"
                              || indicator == "Poverty" || indicator == "poverty" 
                              || indicator == "Inequality" || indicator == "inequality" 
                              || indicator == "Custom" || indicator == "custom" 
                              || indicator %in% names(direct$ind[-1]))) {
    stop(paste0("The argument indicator is set to ", indicator, ". The argument 
                only allows to be set to all, a name of estimated indicators or 
                indicator groups as described in help(estimators.emdi)."))
  }
  
  if (is.null(label) || (!(label == "orig" || label == "no_title" || label == "blank"))) {
    stop("label can be one of the following characters 'orig', 
         'no_title' or 'blank'.")
  }
  if (length(color) != 2 || !is.vector(color)) {
    stop("color needs to be a vector of length 2 defining the two colors for the 
          scatter and line plot. See also help(evaluate_plot).")
  }
  if (length(shape) != 2 || !is.vector(shape)) {
    stop("shape needs to be a vector of length 2 defining the two shapes for the 
          estimates in the line plots. See also help(evaluate_plot).")
  }
  if (length(line_type) != 2 || !is.vector(shape)) {
    stop("line_type needs to be a vector of length 2 defining the types for the 
          lines in the line plots. See also help(evaluate_plot).")
  }
  if (!all(line_type %in% c("twodash", "solid", "longdash", "dotted", "dotdash", 
                            "dashed","blank"))) {
    stop("An element in argument line_type is not a valid option. 
                See help(evaluate_plot) for valid options.")
  }
}



evaluate_test_check <- function(direct, model, indicator, label, color, shape, 
                                line_type, gg_theme) {
  
  if (!all(inherits(direct, which = TRUE, c("emdi", "direct")))) {
    stop('First object needs to be of class emdi, direct.')
  }
  if (!all(inherits(model, which = TRUE, c("emdi", "model")))) {
    stop('First object needs to be of class emdi, model.')
  }
  if (is.null(indicator) || !(indicator == "all" || indicator == "Quantiles" 
                              || indicator == "quantiles"
                              || indicator == "Poverty" || indicator == "poverty" 
                              || indicator == "Inequality" || indicator == "inequality" 
                              || indicator == "Custom" || indicator == "custom" 
                              || indicator %in% names(direct$ind[-1]))) {
    stop(paste0("The argument indicator is set to ", indicator, ". The argument 
                only allows to be set to all, a name of estimated indicators or 
                indicator groups as described in help(estimators.emdi)."))
  }
  
  if (is.null(direct$MSE)) {
    stop('No MSE estimates in the emdi direct object. These are needed for the 
         calculation of the test statistics. Generate a direct object with 
         variance estimates.')
  }
  
  if (is.null(model$MSE)) {
    stop('No MSE estimates in the emdi model object. These are needed for the 
         calculation of the test statistics. Generate a direct object with 
         MSE estimates.')
  }
}