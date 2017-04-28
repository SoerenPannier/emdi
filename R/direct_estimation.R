#' Direct estimation of disaggregated indicators
#'
#' Function \code{direct} estimates indicators only based on sample information. 
#' The variance is estimated via a naive or calibrated bootstrap. The estimation
#' is adapted from the estimation of direct indicators in package 
#' \pkg{laeken}. 
#'
#' @param y a character indicating the variable that is used for estimating the 
#' indicators.
#' @param smp_data survey data containing the above variable as well as sampling
#' domains, and weights if selected.
#' @param smp_domains a character string containing the name of a variable
#' that indicates domains in the sample data. The variable can be numeric or a
#' factor.
#' @param weights a character containing the name of a variable for 
#' the sampling weights in the sample data. This argument is optional and defaults
#' to \code{NULL}. 
#' @param design a character containing the name of a variable for different 
#' strata for stratified sampling designs. This argument is optional and defaults
#' to \code{NULL}. 
#' @param threshold a number defining a threshold. Alternatively, a threshold may 
#' be defined as a \code{function} of \code{y} and \code{weights} returning a 
#' numeric value. Such a function will be evaluated once for the point 
#' estimation and in each iteration of the parametric bootstrap. A threshold is
#' needed for calculation e.g. of head count ratios and poverty gaps. The 
#' argument defaults to \code{NULL}. In this case the threshold is set to 60\% 
#' of the median of the variable that is selected as y similary to the 
#' At-risk-of-poverty rate used in the EU (see also \cite{Social Protection Committee 2001}). 
#' However, any desired threshold can be chosen.
#' @param var if TRUE, estimates for the variance are calcualted using a 
#' naive or calibrated bootstrap. Defaults to \code{FALSE}.
#' @param boot_type a character containing the name of the bootstrap specification. 
#' Either a \code{"naive"} or a \code{"calibrate"} bootstrap can be used. See 
#' also \code{\link[laeken]{bootVar}}. Defaults to \code{naive}.
#' @param B a number determining the number of bootstrap populations for the 
#' bootstrap variance. Defaults to \code{NULL}.
#' @param seed an integer to set the seed for the random number generator. Random 
#' number generation is used in the bootstrap approach. If no seed is set, seed
#' is chosen randomly.
#' @param X_calib a numeric matrix including calibration variables if the calibrated 
#' bootstrap is chosen. Defaults to NULL.
#' @param totals a numeric vector providing the population totals if the calibrated 
#' bootstrap is chosen.. Defaults to \code{NULL}. In this case, the sampling 
#' weights are used to calculate the totals.
#' @param custom_indicator a list of functions containing the indicators to be
#' calculated additionaly. Such functions must and must only depend on the
#' target variable \code{y}, the \code{weights} and the threshold (numeric value)
#' \code{threshold}. Defaults to \code{NULL}.
#' @param na.rm if TRUE, observations with \code{NA} values are deleted from the 
#' sample data. Defaults to \code{FALSE}. 
#' @return An object of class "emdi" that provides direct estimators for regional
#' disaggregated indicators and optionally corresponding variance estimates. Generic
#' functions such as \code{\link{estimators}}, \code{\link{print}}, 
#' and \code{\link{summary}} have methods that can be used
#' to obtain further information. See \code{\link{emdiObject}} for descriptions
#' of components of objects of class "emdi".
#' @details The set of predefined indicators includes the mean, median, four further quantiles
#' (10\%, 25\%, 75\% and 90\%), head count ratio, poverty gap, Gini coefficient
#' and the quintile share ratio.
#' @references
#' Alfons, A. and Templ, M. (2013). Estimation of Social Exclusion Indicators
#' from complex Surveys: The R package laeken. Journal of Statistical Software, 
#' 54(15), 1-25.  \cr \cr
#' Social Protection Committee (2001). Report on indicators in the field of
#' poverty and social exclusions, Technical Report, European Union.
#' @seealso \code{\link{emdiObject}}, \code{\link[nlme]{lme}},
#' \code{\link{estimators.emdi}}, \code{\link{print.emdi}}, 
#' \code{\link{summary.emdi}}
#' @examples
#' \dontrun{
#' # Loading sample data
#' data("eusilcA_smp")
#'
#' # Example without weights and naive bootstrap
#' emdi_direct <- direct(y = "eqIncome", smp_data = eusilcA_smp, 
#' smp_domains = "district", weights = "weight", threshold = 11064.82, var = TRUE, 
#' boot_type = "naive", B = 50, seed = 123, X = NULL, totals = NULL, na.rm = TRUE)
#' 
#' #' # Example with custom indicators
#' emdi_direct <- direct(y="eqIncome", smp_data=eusilcA_smp, smp_domains="district", 
#' weights="weight", threshold=10859.24, var=TRUE, boot_type = "naive", B=50, 
#' seed=123, X = NULL, totals = NULL, custom_indicator = list( my_max = 
#' function(y, weights, threshold){max(y)}, my_min = 
#' function(y, weights, threshold){min(y)}), na.rm=TRUE)
#' }
#' @export
#' @importFrom boot boot
#' @importFrom Hmisc wtd.quantile
#' @importFrom MASS ginv
#' @importFrom stats aggregate runif weighted.mean


direct <- function(y, 
                   smp_data, 
                   smp_domains = NULL, 
                   weights = NULL, 
                   design = NULL,
                   threshold = NULL,
                   var = FALSE, 
                   boot_type = "naive", 
                   B = NULL,
                   seed = NULL,
                   X_calib = NULL, 
                   totals = NULL,
                   custom_indicator = NULL,
                   na.rm = FALSE){
  
  
  direct_check1(y = y, smp_data = smp_data)
  
  direct_check2(smp_domains = smp_domains, weights = weights, sort = sort, 
                threshold = threshold, var = var, boot_type = boot_type, 
                B = B, X = X_calib, totals = totals)
  
  # Save call ------------------------------------------------------------------
  call <- match.call()
  
  # Data manipulation and notational framework ---------------------------------
  
  # The function framework_dir can be found in script framework_direct.R
  framework <- framework_dir(y = y,
                             smp_data = smp_data, 
                             smp_domains = smp_domains, 
                             weights = weights, 
                             threshold = threshold, 
                             custom_indicator = custom_indicator,
                             na.rm = na.rm)
  
  result_point <- mapply( FUN = point_estim_direct, 
                          direct_estimator =  framework$indicator_list,
                          indicator_name =  framework$indicator_names,
                          MoreArgs = list(
                                framework = framework,  
                                boot_type = boot_type, 
                                B = B,
                                seed = seed,
                                X_calib = X_calib, 
                                totals = totals
                          ),
                  SIMPLIFY = F
  )


  if(var == TRUE){
    warnlist <- character()
    envir <- environment()
    res <- mapply(FUN = direct_variance,
                  direct_estimator =  framework$indicator_list,
                  indicator_name =  framework$indicator_names,
                  indicator = result_point,
                  MoreArgs = list(
                           y = framework$y, 
                           weights = framework$weights, 
                           smp_data = framework$smp_data,  
                           smp_domains = framework$smp_domains_vec,
                           design = design,
                           bootType = boot_type, 
                           B = B,
                           seed = seed,
                           X_calib = X_calib, 
                           totals = totals,
                           threshold = framework$threshold,
                           envir = envir
                           ),
                  SIMPLIFY = F
    )
    if(length(warnlist) >0){
      warning(paste0("For the following domains at least one bootstrap failed ",
                     ", this may be due to a very small sample size. For these domains ",
                     "no MSE estimate is displayed. ", paste0(unique(warnlist), collapse = ", ")))
    }
  } else {
    res <- result_point
  }
  ####### Putting together the emdi object
  

  ind <- cbind(res[[1]]$valueByDomain$Domain, 
              as.data.frame(lapply(res, function(erg){erg$valueByDomain[,2]}))
  )
  if(!var) {
    MSE <- NULL 
    colnames(ind) <- c("Domain", framework$indicator_names)
    } else {
      MSE <- cbind(res[[1]]$varByDomain$Domain, 
              as.data.frame(lapply(res, function(erg){erg$varByDomain[,2]}))
              )
      colnames(MSE) <- colnames(ind) <- c("Domain", framework$indicator_names)
    }
  
  direct_out <- list(
    ind = ind, 
    MSE = MSE,
    framework=framework[c("N_dom_smp",
                         "N_smp",
                         "smp_domains",
                         "smp_domains_vec")],
    call = call)
  
  class(direct_out) <- c("emdi", "direct")
  return(direct_out)
}












