#' Function for Fay-Herriot model
#'
#' This function conducts the estimation of a Fay-Herriot model.
#'
#' @param fixed a two-sided linear formula object describing the
#' fixed-effects part of the nested error linear regression model with the
#' dependent variable on the left of a ~ operator and the explanatory
#' variables on the right, separated by + operators.
#' @param vardir a character string indicating the name of the variable containing
#' the domain-specific sampling variances of the direct estimators that are
#' included in \cr \code{combined_data}.
#' @param combined_data a data set containing the direct estimates,
#' the sampling variances, the explanatory variables and the domains.
#' @param domains a character string indicating the domain variable that is
#' included in \cr \code{combined_data}. If \code{NULL}, the domains are numbered
#' consecutively.
#' @param correlation a character string determining the correlation structure.
#' @param corMatrix  proximity matrix.
#' @param time a character string indicating the name of the variable containing
#' the time variable that is included in \code{combined_data}.
#' @param predType (character) one or more in c("\code{linear}", "\code{reblup}",
#'  "\code{reblupbc}")
#' @param c (numeric) scalar; a multiplyer constant used in the bias correction. 
#' Default is to make no correction for realisations of direct estimator within
#' c = 1 times the standard deviation of direct estimator.
#' @param MSE (character) the type of the MSE. Available are "\code{pseudo}"
#' and "\code{robustboot}".
#' @param B numeric value that determines the number of bootstrap iterations.
#' @return fitted robust FH model.
#' @import saeRobust 
#' @importFrom formula.tools lhs
#' @importFrom stats median model.frame model.matrix model.response optimize
#' @importFrom stats pnorm rnorm
#' @export


fh_robust <- function(fixed, vardir, combined_data, domains = NULL, correlation = "no",
                     corMatrix = NULL, time = NULL, predType = "reblup", c = 1,
                     MSE = "pseudo", B = NULL) {
  
  
  # Save function call ---------------------------------------------------------
  call <- match.call()
  
  # Notational framework -------------------------------------------------------
  framework <- framework_FH(combined_data = combined_data, fixed = fixed,
                            vardir = vardir, domains = domains, transformation = "no",
                            eff_smpsize = NULL)
  
  
  
  # Standard EBLUP -------------------------------------------------------------
  eblup <- eblup_robust(framework = framework, predType = predType, 
                        vardir = vardir,combined_data = combined_data, c = c, 
                        correlation = correlation, corMatrix = corMatrix,
                        time = time)
  
  if (MSE == TRUE) {
    # MSE
    MSE_data <- wrapper_MSE(combined_data = combined_data,
                            vardir = vardir, eblup = eblup,
                            mse_type = mse_type,
                            predType = predType, framework = framework, B = B)
    MSE = MSE_data$MSE_data
    MSE_method = MSE_data$MSE_method
  } else {
    MSE <- NULL
    MSE_method <- "no mse estimated"
  }
  
  
    
  out <- list(ind = eblup$EBLUP_data,
                MSE = MSE_data$MSE,
                model = list(coefficients = eblup$coefficients,
                             variance = eblup$variance,
                             random_effects = as.matrix(eblup$random_effects),
                             real_residuals = as.matrix(eblup$real_res),
                             std_real_residuals = as.matrix(eblup$std_real_res),
                             model_select = NULL,
                             correlation = correlation,
                             W = eblup$W,
                             nTime = eblup$nTime,
                             scores = eblup$scores,
                             iterations = eblup$iterations,
                             maxIter = eblup$maxIter,
                             maxIterParam = eblup$maxIterParam,
                             maxIterRe = eblup$maxIterRe),
                framework = framework[c("direct", "vardir", "N_dom_smp",
                                        "N_dom_unobs")],
                method = list(method = "robustified ML",
                              MSE_method = MSE_method),
                fixed = fixed,
                call = call,
                successful_bootstraps = NULL)
  
  class(out) <- c("emdi", "model", "fh")
  
  return(out)
  
}
