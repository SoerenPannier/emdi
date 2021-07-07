compare_plot_check <- function(model, indicator, label, color, shape, 
                                line_type, gg_theme) {
  

  if (is.null(indicator) || !all(indicator == "all" | indicator == "Quantiles" 
                              | indicator == "quantiles"
                              | indicator == "Poverty" | indicator == "poverty" 
                              | indicator == "Inequality" | indicator == "inequality" 
                              | indicator == "Custom" | indicator == "custom" 
                              | indicator %in% names(model$ind[-1]))) {
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
          scatter and line plot. See also help(compare_plot).")
  }
  if (length(shape) != 2 || !is.vector(shape)) {
    stop("shape needs to be a vector of length 2 defining the two shapes for the 
          estimates in the line plots. See also help(compare_plot).")
  }
  if (length(line_type) != 2 || !is.vector(shape)) {
    stop("line_type needs to be a vector of length 2 defining the types for the 
          lines in the line plots. See also help(compare_plot).")
  }
  if (!all(line_type %in% c("twodash", "solid", "longdash", "dotted", "dotdash", 
                            "dashed","blank"))) {
    stop("An element in argument line_type is not a valid option. 
                See help(compare_plot) for valid options.")
  }

}

compare_plot_check2 <- function(ind_direct, ind_model) {
  if (!any(ind_direct$Domain %in% ind_model$Domain)) {
    stop("Domain identifiers between direct and model estimates never match.
         Please verify you are comparing estimates obtained on the same sample.")
  }
  if (!all(ind_direct$Domain %in% ind_model$Domain)) {
    warning(paste("Not all domains contained in the direct",
                  "estimation have been found in the",
                  "model estimation. Following plots will",
                  "only contain results for estimates available",
                  "in both objects."))
  }
  if (!all(ind_model$Domain %in% ind_direct$Domain )) {
    message(paste("Not all domains contained in the model",
                  "estimation have been found in the",
                  "direct estimation. Following plots will",
                  "only contain results for estimates available",
                  "in both objects. \n"))
  }
}

