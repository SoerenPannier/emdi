# #' Presents MSE estimates of an emdiObject
# #'
# #' Function \code{mse_emdi} presents MSE estimates for regional
# #' disaggregated indicators. This method enables to select for which
# #' indicators the MSE estimates shall be returned. Furthermore, coefficients of
# #' variation can be calculated per selected indicator per domain. The returned
# #' object is suitable for printing with the \code{print.mse.emdi} method.
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
# #' @param CV logical. If TRUE, coefficients of variation for selected indicators
# #' per domain are added to the data frame of MSE estimates. Defaults to FALSE.
# #' @return
# #' an object of type "mse.emdi" with MSE estimates per domain
# #' obtained from \code{emdiObject$MSE}. These objects contain two elements,

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
# #' # choose Gini coefficient and CV
# #' mse(ebp, indicator = "gini", CV = TRUE)
# #' @export

mse_emdi <- function(object, indicator = "all", CV = FALSE) {

  if (is.null(object$MSE)) {
    stop('No MSE estimates in object: method mse not applicable')
  }
  if((ncol(object$ind)==11) && any(indicator=="Custom"||indicator=="custom")){
    stop('No individual indicators are defined. Either select other indicators or
         define custom indicators and generate a new emdi object. See also help(ebp).')
  }

  # Calculation of CVs
  all_cv <- sqrt(object$MSE[,-1]) / object$ind[,-1]

  if(any(indicator == "Quantiles")||any(indicator == "quantiles"))
  {
    indicator = c(indicator[!(indicator == "Quantiles"||indicator == "quantiles")], "Quantile_10", "Quantile_25", "Median", "Quantile_75", "Quantile_90")
  }
  if(any(indicator == "poverty")||any(indicator == "Poverty"))
  {
    indicator = c(indicator[!(indicator == "poverty"||indicator == "Poverty")], "Head_Count", "Poverty_Gap")
  }
  if(any(indicator == "inequality")||any(indicator == "Inequality"))
  {
    indicator = c(indicator[!(indicator == "inequality"||indicator == "Inequality")], "Gini", "Quintile_Share")
  }
  if(any(indicator == "custom")||any(indicator == "Custom")){
     indicator = c(indicator[!(indicator == "custom"||indicator == "Custom")],
                   colnames(object$ind[-c(1,2,3,4,5,6,7,8,9,10,11)]))
  }

  if (any(indicator == "all") || any(indicator == "All" )) {
    ind <- object$MSE
    ind_cv <- cbind(Domain = object$MSE[,1], all_cv)
    ind_name <- "All indicators"
  }
  else {
    selection = colnames(object$MSE[-1]) %in% indicator
    ind <- object$MSE[,c(T, selection)]
    ind_cv <- data.frame(Domain = object$MSE[,1], all_cv[, selection])
    colnames(ind_cv) = colnames(ind)
    ind_name <- paste(unique(indicator), collapse = ", ")
  }

  if (CV == FALSE) {
  mse_emdi <- list(ind = ind, ind_name = ind_name)
  } else {
  mse_emdi <- list(ind = ind, ind_cv = ind_cv, ind_name = ind_name)
  }

  class(mse_emdi) <- "mse.emdi"

  return(mse_emdi)
}

# #' Prints mse.emdi objects
# #'
# #' @param object an object of type "mse.emdi".
# #' @export


print.mse.emdi <- function(x,...) {

  cat(paste0("MSE estimates: ", x$ind_name, "\n"))
  print(x$ind)
  cat(paste0("CV estimates: ", x$ind_name, "\n"))
  print(x$ind_cv)
}
