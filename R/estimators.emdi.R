#' Presents point, MSE and CV estimates
#'
#' Function \code{estimators} is a generic function used to present point and
#' mean squared error (MSE) estimates and calculated coefficients of variation
#' (CV).
#' @param object an object for which point and/or MSE estimates and/or
#' calculated CV's are desired.
#' @param indicator optional character vector that selects which indicators
#' shall be returned: (i) all calculated indicators ("all");
#' (ii) each indicator name: "Mean" "Quantile_10", "Quantile_25", "Median",
#' "Quantile_75", "Quantile_90", "Head_Count",
#' "Poverty_Gap", "Gini", "Quintile_Share" or the function name/s of
#' "custom_indicator/s"; (iii) groups of indicators: "Quantiles", "Poverty",
#' "Inequality" or "Custom". If two of these groups are selected, only the first
#' one is returned. Note, additional custom indicators can be
#' defined as argument for model-based approaches (see also \code{\link{ebp}})
#' and do not appear in groups of indicators even though these might belong to
#' one of the groups. If the \code{model} argument is of type "model","fh", 
#' indicator can be set to "all", "FH" or "Direct". Defaults to "all".
#' @param MSE optional logical. If \code{TRUE}, MSE estimates for selected indicators
#' per domain are added to the data frame of point estimates. Defaults to
#' \code{FALSE}.
#' @param CV optional logical. If \code{TRUE}, coefficients of variation for selected
#' indicators per domain are added to the data frame of point estimates.
#' Defaults to \code{FALSE}.
#' @param ... arguments to be passed to or from other methods, e.g. indicator.
#' @return
#' The return of \code{estimators} depends on the class of its argument. The
#' documentation of particular methods gives detailed information about the
#' return of that method.
#' @export

estimators <- function(object, indicator, MSE, CV, ...) UseMethod("estimators")


#' Presents point, MSE and/or CV estimates of an emdiObject
#'
#' Method \code{estimators.emdi} presents point and MSE estimates for regional
#' disaggregated indicators. Coefficients of variation are calculated
#' using these estimators. This method enables to select for which indicators
#' the estimates shall be returned. The returned object is suitable for
#' printing with the \code{print.estimators.emdi} method.
#' @param object an object of type "emdi", representing point and,
#' if chosen, MSE estimates.
#' @param indicator optional character vector that selects which indicators
#' shall be returned: (i) all calculated indicators ("all");
#' (ii) each indicator name: "Mean", "Quantile_10", "Quantile_25", "Median",
#' "Quantile_75", "Quantile_90", "Head_Count",
#' "Poverty_Gap", "Gini", "Quintile_Share" or the function name/s of
#' "custom_indicator/s"; (iii) groups of indicators: "Quantiles", "Poverty",
#' "Inequality" or "Custom". If two of these groups are selected, only the first
#' one is returned. Note, additional custom indicators can be
#' defined as argument for model-based approaches (see also \code{\link{ebp}})
#' and do not appear in groups of indicators even though these might belong to
#' one of the groups. If the \code{model} argument is of type "model","fh", 
#' indicator can be set to "all", "FH" or "Direct". Defaults to "all".
#' @param MSE optional logical. If \code{TRUE}, MSE estimates for selected indicators
#' per domain are added to the data frame of point estimates. Defaults to
#' \code{FALSE}.
#' @param CV optional logical. If \code{TRUE}, coefficients of variation for selected
#' indicators per domain are added to the data frame of point estimates.
#' Defaults to \code{FALSE}.
#' @param ... other parameters that can be passed to function \code{estimators}.
#' @return
#' an object of type "estimators.emdi" with point and/or MSE
#' estimates and/or calculated CV's per domain obtained from
#' \code{emdiObject$ind} and, if chosen, \code{emdiObject$MSE}. These objects
#' contain two elements, one data frame \code{ind} and a character naming the
#' indicator or indicator group \code{ind_name}.
#' @seealso \code{\link{emdiObject}}, \code{\link{direct}}, \code{\link{ebp}}, 
#' \code{\link{fh}}
#' @examples
#' \dontrun{
#' # Loading data - population and sample data
#' data("eusilcA_pop")
#' data("eusilcA_smp")
#'
#' # Generate emdi object with additional indicators; here via function ebp()
#' emdi_model <- ebp(fixed = eqIncome ~ gender + eqsize + cash +
#' self_empl + unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent +
#' fam_allow + house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop,
#' pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district",
#' threshold = 11064.82, transformation = "box.cox",
#' L = 50, MSE = TRUE, B = 50, custom_indicator =
#' list(my_max = function(y, threshold){max(y)},
#' my_min = function(y, threshold){min(y)}), na.rm = TRUE, cpus = 1)
#'
#' # Example 1: Choose Gini coefficient, MSE and CV
#' estimators(emdi_model, indicator = "Gini", MSE = TRUE, CV = TRUE)
#'
#' # Example 2: Choose custom indicators without MSE and CV
#' estimators(emdi_model, indicator = "Custom")
#' }
#' @export

estimators.emdi <- function(object, indicator = "all", MSE = FALSE, CV = FALSE, ...) {

  estimators_check(object = object, indicator = indicator,
                   MSE = MSE, CV = CV)

  # Only point estimates
  all_ind <- point_emdi(object = object, indicator = indicator)
  selected <- colnames(all_ind$ind)[-1]

  if ( MSE == TRUE || CV == TRUE ) {
    all_precisions <- mse_emdi(object = object, indicator = indicator, CV = TRUE)
    colnames(all_precisions$ind) <- paste0(colnames(all_precisions$ind), "_MSE")
    colnames(all_precisions$ind_cv) <- paste0(colnames(all_precisions$ind_cv), "_CV")
    combined <- data.frame(all_ind$ind, all_precisions$ind, all_precisions$ind_cv)
    endings <- c("","_MSE", "_CV")[c(TRUE,MSE,CV)]

    combined <- combined[,c("Domain",paste0(rep(selected,each = length(endings)),
                                           endings))]
  } else {
    combined <- all_ind$ind
  }

  estimators_emdi <- list(ind = combined, ind_name = all_ind$ind_name)

  class(estimators_emdi) <- "estimators.emdi"

  return(estimators_emdi)
}

#' Prints estimators.emdi objects
#'
#' @param x an object of type "estimators.emdi".
#' @param ... further arguments passed to or from other methods.
#' @export

print.estimators.emdi <- function(x,...) {
  cat(paste0("Indicator/s: ", x$ind_name, "\n"))
  print(x$ind)
}


# Tail/head functions ----------------------------------------------------------

#' Returns the first part of predicted indicators and, if chosen, of MSE and
#' CV estimators.
#'
#' @param x an object of type "estimators.emdi", representing
#' point estimators and, if chosen, MSE and/or CV estimates for selected
#' indicators.
#' @param n a single integer. If positive, it determines the number of rows for
#' the data frame. If negative, all but the n last rows of
#' elements of the object.
#' @param addrownums if there are no row names, create them from the row numbers.
#' @param ... arguments to be passed to or from other methods.
#' @return Selected rows of the object of type "estimators.emdi".
#' @seealso \code{\link{estimators.emdi}}
#' @examples
#' \dontrun{
#' # Loading data - population and sample data
#' data("eusilcA_pop")
#' data("eusilcA_smp")
#'
#' # Generate emdi object with deleting missing values; here via function ebp()
#' emdi_model <- ebp(fixed = eqIncome ~ gender + eqsize + cash +
#' self_empl + unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent +
#' fam_allow + house_allow + cap_inv + tax_adj,
#' pop_data = eusilcA_pop, pop_domains = "district",
#' smp_data = eusilcA_smp, smp_domains = "district",
#' na.rm = TRUE)
#'
#' # Example: Choose first lines of the Gini coefficient, MSE and CV
#' head(estimators(emdi_model, indicator = c("Gini", "Head_Count")))
#' }
#' @importFrom utils head
#' @export
head.estimators.emdi <- function(x, n = 6L, addrownums=NULL, ...) {
  head(x$ind, n = n, addrownums = addrownums, ...)
}

#' Returns the last part of predicted indicators and, if chosen, of MSE and
#' CV estimators.
#'
#' @param x an object of type "estimators.emdi", representing
#' point estimators and, if chosen, MSE and/or CV estimates for selected
#' indicators.
#' @param n a single integer. If positive, it determines the number of rows for
#' the data frame. If negative, all but the n first rows of
#' elements of the object.
#' @param addrownums if there are no row names, create them from the row numbers.
#' @param ... arguments to be passed to or from other methods.
#' @return Selected rows of the object of type "estimators.emdi".
#' @seealso \code{\link{estimators.emdi}}
#' @examples
#' \dontrun{
#' # Loading data - population and sample data
#' data("eusilcA_pop")
#' data("eusilcA_smp")
#'
#' # Generate emdi object with deleting missing values; here via function ebp()
#' emdi_model <- ebp(fixed = eqIncome ~ gender + eqsize + cash +
#' self_empl + unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent +
#' fam_allow + house_allow + cap_inv + tax_adj,
#' pop_data = eusilcA_pop, pop_domains = "district",
#' smp_data = eusilcA_smp, smp_domains = "district",
#' na.rm = TRUE)
#'
#' # Example: Choose last lines of the Gini coefficient, MSE and CV
#' tail(estimators(emdi_model, indicator = c("Gini", "Head_Count")))
#' }
#' @importFrom utils tail
#' @export

tail.estimators.emdi <- function(x, n = 6L, addrownums=NULL, ...) {
  tail(x$ind, n = n, addrownums = addrownums, ...)
}


#' Transforms estimators.emdi objects into a matrix object
#'
#' @param x an object of type "estimators.emdi".
#' @param ... further arguments passed to or from other methods.
#' @export

as.matrix.estimators.emdi <- function(x,...) {
  as.matrix(x$ind[,-1])
}

#' Transforms estimators.emdi objects into a dataframe object
#'
#' @param x an object of type "estimators.emdi".
#' @param ... further arguments passed to or from other methods.
#' @export

as.data.frame.estimators.emdi <- function(x,...) {
  as.data.frame(x$ind, ...)
}

#' Subsets an estimators.emdi object
#'
#' @param x an object of type "estimators.emdi".
#' @param ... further arguments passed to or from other methods.
#' @return Selected subsets of the object of type "estimators.emdi".
#' @seealso \code{\link{estimators.emdi}}
#' @examples
#' \dontrun{
#' # Loading data - population and sample data
#' data("eusilcA_pop")
#' data("eusilcA_smp")
#'
#' # Generate emdi object with deleting missing values; here via function ebp()
#' emdi_model <- ebp( fixed = eqIncome ~ gender + eqsize + cash +
#' self_empl + unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent +
#' fam_allow + house_allow + cap_inv + tax_adj,
#' pop_data = eusilcA_pop, pop_domains = "district",
#' smp_data = eusilcA_smp, smp_domains = "district",
#' na.rm = TRUE)
#'
#' # Example: Choose last lines of the Gini coefficient, MSE and CV
#' subset(estimators(emdi_model, indicator = "Gini"),
#'        Domain %in% c("Wien", "Wien Umgebung"))
#' }
#' @export

subset.estimators.emdi <- function(x, ...) {
  x <- as.data.frame(x)
  subset(x = x,  ...)
}






