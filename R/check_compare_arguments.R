compare_plot_check <- function(model, indicator, label, color, shape,
                               line_type, gg_theme, level) {
  if (inherits(model, "ebp_tf")) {
    if (is.null(indicator) || !all(indicator == "all" | indicator == "Quantiles" |
                                   indicator == "quantiles" |
                                   indicator == "Poverty" |
                                   indicator == "poverty" |
                                   indicator == "Inequality" |
                                   indicator == "inequality" |
                                   indicator == "Custom" | indicator == "custom" |
                                   indicator %in% names(model$ind_Domain[-1]) |
                                   indicator %in% names(model$ind_Subdomain[-1]) )) {
      stop(strwrap(prefix = " ", initial = "",
                   paste0("The argument indicator is set to ", indicator, ". The
                        argument only allows to be set to all, a name of
                        estimated indicators or indicator groups as described
                        in help(estimators.emdi).")))
    }
  } else if (inherits(model, "fh_tf") && level == "domain"){
    if(!(indicator %in% c("Mean", "Head_Count", "Poverty_Gap", "Gini",
                          "Quintile_Share", "Quantile_10", "Quantile_25",
                          "Median", "Quantile_75", "Quantile_90"))){
      stop(strwrap(prefix = " ", initial = "",
                   paste0("The argument indicator is set to ", indicator, ". The
                        argument only allows to be set to a name of indicators
                        in the direct object for the comparison with the 'fh_tf'
                        model at domain level. ")))
    }
  } else if (inherits(model, "fh_tf") && level == "subdomain"){
    if(!(indicator %in% c("Direct", "all", "FH_TF"))){
      stop(strwrap(prefix = " ", initial = "",
                   paste0("The argument indicator is set to ", indicator, ". The
                        argument only allows to be set either all, Direct or
                          FH_TF for the fh_tf model at subdomain level.")))
    }
  }
    else{
    if (is.null(indicator) || !all(indicator == "all" | indicator == "Quantiles" |
                                   indicator == "quantiles" |
                                   indicator == "Poverty" |
                                   indicator == "poverty" |
                                   indicator == "Inequality" |
                                   indicator == "inequality" |
                                   indicator == "Custom" | indicator == "custom" |
                                   indicator %in% names(model$ind[-1]))) {
      stop(strwrap(prefix = " ", initial = "",
                   paste0("The argument indicator is set to ", indicator, ". The
                        argument only allows to be set to all, a name of
                        estimated indicators or indicator groups as described
                        in help(estimators.emdi).")))
    }
  }


  #____________________________Rachael__________________________________________
  if (any(inherits(model, which = TRUE, c("ebp_tf", "fh_tf")))) {
    if (is.null(level)) {
      stop("For 'ebp_tf' or 'fh_tf' models, please specify the 'level' argument.
           'level' can take either 'domain' or 'subdomain' arguments")
    }else if (!level %in% c("domain", "subdomain")) {
      stop("'level' can take either 'domain' or 'subdomain' arguments.")
    }
  } else {
    level <- NULL
  }
  #_______________________________________________________________________________

  if (is.null(label) || (!(label == "orig" || label == "no_title" ||
    label == "blank"))) {
    stop(strwrap(prefix = " ", initial = "",
                 "label can be one of the following characters 'orig',
                 'no_title' or 'blank'."))
  }
  if (length(color) != 2 || !is.vector(color)) {
    stop(strwrap(prefix = " ", initial = "",
                 "color needs to be a vector of length 2 defining the two
                 colors for the scatter and line plot. See also
                 help(compare_plot)."))
  }
  if (length(shape) != 2 || !is.vector(shape)) {
    stop(strwrap(prefix = " ", initial = "",
                 "shape needs to be a vector of length 2 defining the two
                 shapes for the estimates in the line plots. See also
                 help(compare_plot)."))
  }
  if (length(line_type) != 2 || !is.vector(shape)) {
    stop(strwrap(prefix = " ", initial = "",
                 "line_type needs to be a vector of length 2 defining the types
                 for the lines in the line plots. See also
                 help(compare_plot)."))
  }
  if (!all(line_type %in% c(
    "twodash", "solid", "longdash", "dotted", "dotdash",
    "dashed", "blank"
  ))) {
    stop(strwrap(prefix = " ", initial = "",
                 "An element in argument line_type is not a valid option.
                 See help(compare_plot) for valid options."))
  }
}

compare_plot_check2 <- function(ind_direct, ind_model) {
  if (!any(ind_direct$Domain %in% ind_model$Domain)) {
    stop(strwrap(prefix = " ", initial = "",
                 "Domain identifiers between direct and model estimates never
                 match. Please verify you are comparing estimates obtained on
                 the same sample."))
  }
  if (!all(ind_direct$Domain %in% ind_model$Domain)) {
    warning(strwrap(prefix = " ", initial = "",
                    "Not all domains contained in the direct
                    estimation have been found in the model estimation.
                    Following plots will only contain results for estimates
                    available in both objects."
    ))
  }
  if (!all(ind_model$Domain %in% ind_direct$Domain)) {
    message(strwrap(prefix = " ", initial = "",
                    "Not all domains contained in the model estimation have
                    been found in the direct estimation. Following plots will
                    only contain results for estimates available
                    in both objects."))
  }
}
