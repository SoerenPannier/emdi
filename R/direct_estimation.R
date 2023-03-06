#' Direct Estimation of Disaggregated Indicators
#'
#' Function \code{direct} estimates indicators only based on sample information.
#' The variance is estimated via a naive or calibrated bootstrap. The estimation
#' is adapted from the estimation of direct indicators in package
#' \pkg{laeken}.
#'
#' @param y a character string indicating the variable that is used for
#' estimating the indicators. The variable must be contained in the sample data.
#' @param smp_data survey data containing variable y as well as sampling
#' domains, and weights if selected.
#' @param smp_domains a character containing the name of a variable
#' that indicates domains in the sample data. The variable must be numeric or a
#' factor.
#' @param weights a character string containing the name of a variable for
#' the sampling weights in the sample data. This argument is optional and
#' defaults to \code{NULL}.
#' @param design a character string containing the name of a variable for
#' different strata for stratified sampling designs. This argument is optional
#' and defaults to \code{NULL}.
#' @param threshold a number defining a threshold. Alternatively, a threshold
#' may be defined as a \code{function} of \code{y} and \code{weights} returning
#' a numeric value. Such a function will be evaluated once for the point
#' estimation and in each iteration of the parametric bootstrap. See Example 2
#' for using a function as threshold.  A threshold is needed for calculation
#' e.g. of head count ratios and poverty gaps. The argument defaults to
#' \code{NULL}. In this case, the threshold is set to 60\% of the median of the
#' variable that is selected as \code{y} similarly to the at-risk-of-poverty
#' rate used in the EU (see also \cite{Social Protection Committee 2001}).
#' However, any desired threshold can be chosen.
#' @param var if \code{TRUE}, estimates for the variance are calculated using a
#' naive or calibrated bootstrap. Defaults to \code{FALSE}.
#' @param boot_type a character string containing the name of the bootstrap
#' specification. Either a \code{"naive"} or a \code{"calibrate"} bootstrap can
#' be used. See also \code{\link[laeken]{bootVar}}. Defaults to \code{naive}.
#' @param B a number determining the number of bootstrap populations for the
#' bootstrap variance. Defaults to \code{50}.
#' @param seed an integer to set the seed for the random number generator.
#' Random number generation is used in the bootstrap approach. If seed is set to
#' \code{NULL}, seed is chosen randomly. Defaults to \code{123}.
#' @param X_calib a numeric matrix including calibration variables if the
#' calibrated bootstrap is chosen. Defaults to NULL.
#' @param totals a numeric vector providing the population totals if the
#' calibrated bootstrap is chosen. If a vector is chosen, the length of the
#' vector needs to equal the number of columns in X_calib. Defaults to
#' \code{NULL}. In this case, the sampling weights are used to calculate the
#' totals.
#' @param custom_indicator a list of functions containing the indicators to be
#' calculated additionally. Such functions must and must only depend on the
#' target variable \code{y}, the \code{weights} and the
#' \code{threshold} (numeric value) (see Example 3). Defaults to \code{NULL}.
#' @param na.rm if \code{TRUE}, observations with \code{NA} values are deleted
#' from the sample data. Defaults to \code{FALSE}.
#' @return An object of class "direct", "emdi" that provides direct estimators
#' for regional disaggregated indicators and optionally corresponding variance
#' estimates. Several generic functions have methods for the
#' returned object. For a full list and descriptions of the components of
#' objects of class "emdi", see \code{\link{emdiObject}}.
#' @details The set of predefined indicators includes the mean, median, four
#' further quantiles (10\%, 25\%, 75\% and 90\%), head count ratio, poverty gap,
#' Gini coefficient and the quintile share ratio.
#' @references
#' Alfons, A. and Templ, M. (2013). Estimation of Social Exclusion Indicators
#' from Complex Surveys: The \R Package \pkg{laeken}. Journal of
#' Statistical Software, 54(15), 1-25.  \cr \cr
#' Social Protection Committee (2001). Report on Indicators in the Field of
#' Poverty and Social Exclusions, Technical Report, European Union.
#' @seealso \code{\link{emdiObject}}, \code{\link[nlme]{lme}},
#' \code{\link{estimators.emdi}}, \code{\link{emdi_summaries}}
#' @examples
#' \donttest{
#' # Loading sample data
#' data("eusilcA_smp")
#'
#' # Example 1: With weights and naive bootstrap
#' emdi_direct <- direct(
#'   y = "eqIncome", smp_data = eusilcA_smp,
#'   smp_domains = "district", weights = "weight", threshold = 11064.82,
#'   var = TRUE, boot_type = "naive", B = 50, seed = 123, X_calib = NULL,
#'   totals = NULL, na.rm = TRUE
#' )
#'
#' # Example 2: With function as threshold
#' emdi_direct <- direct(
#'   y = "eqIncome", smp_data = eusilcA_smp,
#'   smp_domains = "district", weights = "weight", threshold =
#'     function(y, weights) {
#'       0.6 * laeken::weightedMedian(y, weights)
#'     }, na.rm = TRUE
#' )
#'
#' # Example 3: With custom indicators
#' emdi_direct <- direct(
#'   y = "eqIncome", smp_data = eusilcA_smp,
#'   smp_domains = "district", weights = "weight", threshold = 10859.24,
#'   var = TRUE, boot_type = "naive", B = 50, seed = 123, X_calib = NULL,
#'   totals = NULL, custom_indicator = list(my_max = function(y) {
#'     max(y)
#'   }, my_min = function(y) {
#'     min(y)
#'   }),
#'   na.rm = TRUE
#' )
#' }
#' @export
#' @importFrom boot boot
#' @importFrom MASS ginv
#' @importFrom stats aggregate runif weighted.mean


direct <- function(y,
                   smp_data,
                   smp_domains,
                   weights = NULL,
                   design = NULL,
                   threshold = NULL,
                   var = FALSE,
                   boot_type = "naive",
                   B = 50,
                   seed = 123,
                   X_calib = NULL,
                   totals = NULL,
                   custom_indicator = NULL,
                   na.rm = FALSE) {
  direct_check(
    y = y, smp_data = smp_data, smp_domains = smp_domains,
    weights = weights, design = design, threshold = threshold,
    var = var, boot_type = boot_type,
    B = B, seed = seed, X_calib = X_calib, totals = totals,
    na.rm = na.rm, custom_indicator = custom_indicator
  )

  # Save call ------------------------------------------------------------------
  call <- match.call()

  # Data manipulation and notational framework ---------------------------------

  # The function framework_dir can be found in script framework_direct.R
  framework <- framework_dir(
    y = y,
    smp_data = smp_data,
    smp_domains = smp_domains,
    weights = weights,
    threshold = threshold,
    custom_indicator = custom_indicator,
    na.rm = na.rm
  )

  result_point <- mapply(
    FUN = point_estim_direct,
    direct_estimator = framework$indicator_list,
    indicator_name = framework$indicator_names,
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

  warnlist <- character()
  if (var == TRUE) {
    envir <- environment()
    res <- mapply(
      FUN = direct_variance,
      direct_estimator = framework$indicator_list,
      indicator_name = framework$indicator_names,
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
    if (length(warnlist) > 0) {
      warning(strwrap(prefix = " ", initial = "",
                      paste0("For the following domains at least one bootstrap
                             failed, this may be due to a very small sample
                             size. For these domains the variance estimation is
                             based on a reduced number of bootstrap iteration.
                             To see the number of successful bootstrap
                             iterations for each domain and indicator, have a
                             look at the value successful_bootstraps in the
                             returned object."
                             )))
    }
  } else {
    res <- result_point
  }
  ####### Putting together the emdi object


  ind <- cbind(
    res[[1]]$valueByDomain$Domain,
    as.data.frame(lapply(res, function(erg) {
      erg$valueByDomain[, 2]
    }))
  )
  if (!var) {
    MSE <- NULL
    colnames(ind) <- c("Domain", framework$indicator_names)
  } else {
    MSE <- cbind(
      res[[1]]$varByDomain$Domain,
      as.data.frame(lapply(res, function(erg) {
        erg$varByDomain[, 2]
      }))
    )
    colnames(MSE) <- colnames(ind) <- c("Domain", framework$indicator_names)
  }

  if (length(warnlist) > 0) {
    failed_BS <- do.call(rbind, lapply(strsplit(warnlist, split = ":_:"), t))
    colnames(failed_BS) <- c("Domain", "Indicator")
    failed_BS <- as.data.frame(failed_BS)
    failed_BS$Domain <- factor(failed_BS$Domain,
      levels = levels(framework$smp_domains_vec)
    )
    failed_BS$Indicator <- factor(failed_BS$Indicator,
      levels = framework$indicator_names
    )
    sucInd <- B - table(failed_BS$Domain, failed_BS$Indicator)
  } else {
    failed_BS <- matrix(
      data = 0,
      nrow = length(levels(framework$smp_domains_vec)),
      ncol = length(framework$indicator_names),
      dimnames = list(
        levels(framework$smp_domains_vec),
        framework$indicator_names
      )
    )
    sucInd <- B - failed_BS
  }

  direct_out <- list(
    ind = ind,
    MSE = MSE,
    framework = framework[c(
      "N_dom_smp",
      "N_smp",
      "smp_domains",
      "smp_data",
      "smp_domains_vec"
    )],
    call = call,
    successful_bootstraps = sucInd
  )

  class(direct_out) <- c("direct", "emdi")
  return(direct_out)
}
