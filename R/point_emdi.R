# #' Presents point estimates of an emdiObject
# #'
# #' Function \code{point_emdi} presents point estimates for regional
# #' disaggregated indicators. This method enables to select for which indicators
# #' the point estimates shall be returned. The returned object is suitable for
# #' printing with the \code{print.point.emdi} method.
# #' @param object an object of type "emdi", representing point and
# #' MSE estimates.
# #' @param indicator optional character vector that selects which indicators
# #' shall be returned: (i) all calculated indicators ("all");
# #' (ii) each indicator name: "Mean" "Quantile_10", "Quantile_25", "Median",
# #' "Quantile_75", "Quantile_90", "Head_Count", "Poverty_Gap", "Gini", 
# #' "Quintile_Share" or the function name/s of "custom_indicator/s"; 
# #' (ii) groups of indicators: "Quantiles", "Poverty" or 
# #' "Inequality". Defaults to "all". Note, additional custom indicators can be 
# #' defined as argument for model-based approaches (\code{link{ebp}}) and do not 
# #' appear in groups of indicators even though these might belong to one of the 
# #' groups.  
# #' @return
# #' an object of type "point.emdi" with point estimates per domain
# #' obtained from \code{emdiObject$ind}. These objects contain two elements,
# #' one data frame \code{ind} and a character naming the indicator or indicator
# #' group \code{ind_name}.
# #' @seealso \code{\link{emdiObject}}, \code{\link{ebp}}
# #' @examples
# #' # Loading data
# #' data("Xoutsamp_AuxVar")
# #' data("incomedata")
# #'
# #' # generate emdi object
# #' set.seed(100); ebp <- ebp(income~educ1, Xoutsamp_AuxVar, "provlab",
# #' incomedata, "provlab", 4500, "no", L=2, MSE=TRUE, B=2)
# #'
# #' # choose Gini coefficient
# #' point(ebp, indicator = "gini")
# # #' @export


point_emdi <- function(object, indicator = "all") {
  if (is.null(object$ind)) {
    stop('No estimates in object: method point not applicable')
  } 
  if ((ncol(object$ind) == 11) && any(indicator == "Custom" || indicator == "custom")) {
    stop('No individual indicators are defined. Either select other indicators or
         define custom indicators and generate a new emdi object. See also 
         help(direct) or help(ebp).')
  }

  if (any(indicator == "Quantiles") || any(indicator == "quantiles")) {
    indicator = c(indicator[!(indicator == "Quantiles"|| indicator == "quantiles")],
                  "Quantile_10", "Quantile_25", "Median", "Quantile_75", "Quantile_90")
  }
  if (any(indicator == "poverty") || any(indicator == "Poverty")) {
    indicator = c(indicator[!(indicator == "poverty"|| indicator == "Poverty")], "Head_Count", "Poverty_Gap")
  }

  if (any(indicator == "inequality") || any(indicator == "Inequality")) {
    indicator = c(indicator[!(indicator == "inequality" || indicator == "Inequality")],
                  "Gini", "Quintile_Share")
  }

  if (any(indicator == "custom") || any(indicator == "Custom")) {
     indicator = c(indicator[!(indicator == "custom" || indicator == "Custom")],
                  colnames(object$ind[-c(1,2,3,4,5,6,7,8,9,10,11)]))
  }

  

  if (any(indicator == "all") || any(indicator == "All" )) {
    ind <- object$ind
    ind_name <- "All indicators"
  } else {
    selection <- colnames(object$ind[-1]) %in% indicator
    ind <- object$ind[,c(T, selection)]
    ind_name <- paste(unique(indicator), collapse = ", ")
  }

  point_emdi <- list(ind = ind, ind_name = ind_name)
  class(point_emdi) <- "point.emdi"

  return(point_emdi)
}

# #' Prints point.emdi objects
# #'
# #' @param object an object of type "point.emdi".
# #' @export

print.point.emdi <- function(x,...) {
  cat(paste0("Point estimates: ", x$ind_name, "\n"))
  print(x$ind)
}
