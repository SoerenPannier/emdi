#' Standard Fay-Herriot model for disaggregated indicators
#'
#' Function \code{fh} estimates indicators using the Fay-Herriot approach by
#' \cite{Fay and Herriot (1979)}. Point estimates of indicators are
#' empirical best linear unbiased predictors (EBLUPs). Additionally, mean squared
#' error (MSE) estimation can be conducted which depends on the chosen estimation
#' approach for the variance of the random effect. For this estimation, six different
#' approaches are provided (see also \code{method}). Three different transformation
#' types for the dependent variable can be chosen.
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
#' @param method a character string describing the method for the estimation of
#' the variance of the random effects. Methods that can be chosen
#' (i) restricted maximum likelihood (REML) method ("\code{reml}"),
#' (ii) maximum likelihood method ("\code{ml}"),
#' (iii) adjusted REML following \cite{Li and Lahiri (2010)} ("\code{amrl}"),
#' (iv) adjusted ML following \cite{Li and Lahiri (2010)} ("\code{ampl}"),
#' (v) adjusted REML following \cite{Yoshimori and Lahiri (2014)}
#' ("\code{amrl_yl}"),
#' (vi) adjusted ML following \cite{Yoshimori and Lahiri (2014)}
#' ("\code{ampl_yl}").
#'  Defaults to "\code{reml}".
#' @param interval interval for the estimation of sigmau2.
#' @param transformation a character that determines the type of transformation
#' and back-transformation. Methods that can be chosen
#' (i) no transformation ("\code{no}")
#' (ii) log transformation with naive back-transformation, i.e. simply taking the
#' exponential ("\code{log_naive}"),
#' (iii) log transformation with crude back-transformation ("\code{log_crude}"),
#' (iv) log transformation with Slud-Maiti back-transformation ("\code{log_SM}")
#' and (v) arcsin transformation with naive back-transformation ("\code{arcsin}")
#' @param backtransformation a character that determines the type of bracktransformation
#' @param eff_smpsize Effective sample size.
#' @param MSE if \code{TRUE}, MSE estimates are calculated. Defaults
#' to \code{FALSE}.
#' @param mse_type a character string determining the estimation method of the MSE.
#' Methods that can be chosen
#' (i) analytical MSE depending on the estimation method of the variance of the
#' random effect ("\code{analytical}"),
#' (ii) a jackknife MSE ("\code{jackknife}"),
#' (ii) a weighted jackknife MSE ("\code{weighted_jackknife}"),
#' (iii) and a bootstrap ("\code{boot}"). The latter three options are of interest
#' when the arcsin transformation is selected.
#' @param B numeric value that determines the number of bootstrap iterations.
#' @param alpha a numeric value that determines the confidence level for the
#' confidence intervals.
#' @return fitted FH model.
#' @import saeRobust
#' @import formula.tools 
#' @importFrom stats median model.frame model.matrix model.response optimize
#' @importFrom stats pnorm rnorm
#' @export


fh <- function(fixed, vardir, combined_data, domains, method = "reml",
               interval = c(0, 1000), k = 10000, c = 1, transformation = "no",
               backtransformation = NULL, eff_smpsize = NULL,
               correlation = "no", corMatrix = NULL, time = NULL,
               Ci = NULL, tol = NULL, maxit = NULL,
               MSE = FALSE, mse_type = "analytical", B = NULL, alpha = 0.05) {


  # Save function call ---------------------------------------------------------
  call <- match.call()

  # Notational framework -------------------------------------------------------
  framework <- framework_FH(combined_data = combined_data, fixed = fixed,
                            vardir = vardir, domains = domains,
                            transformation = transformation,
                            eff_smpsize = eff_smpsize,
                            Ci = Ci, tol = tol, maxit = maxit)

  if (!(method == "reblup" | method == "reblupbc")) {
    # Estimate sigma u -----------------------------------------------------------
    sigmau2 <- wrapper_estsigmau2(framework = framework, method = method,
                                  interval = interval)
    
    
    if (method != "moment") {
      # Standard EBLUP -------------------------------------------------------------
      eblup <- eblup_FH(framework = framework, sigmau2 = sigmau2,
                        combined_data = combined_data)
      
      
      # Criteria for model selection -----------------------------------------------
      criteria <- model_select(framework = framework, sigmau2 = sigmau2,
                               real_res = eblup$real_res)
    } else if (method == "moment") {
      # Standard EBLUP -------------------------------------------------------------
      eblup <- eblup_YL(framework = framework, sigmau2 = sigmau2,
                        combined_data = combined_data)
    }
    
    
    
    if (transformation == "no") {
      
      # Analytical MSE
      if (MSE == TRUE) {
        MSE_data <- wrapper_MSE(framework = framework, combined_data = combined_data,
                                sigmau2 = sigmau2, vardir = vardir, Ci = Ci, eblup = eblup,
                                transformation = transformation, method = method,
                                interval = interval, mse_type = mse_type)
        MSE <- MSE_data$MSE_data
        MSE_method <- MSE_data$MSE_method
      } else {
        MSE <- NULL
        MSE_method <- "no mse estimated"
      }
      
      
      # Shrinkage factor
      Gamma <- data.frame(Domain = framework$data[[framework$domains]],
                          Gamma = eblup$gamma)
      
      if (method != "moment") {
        out <- list(ind = eblup$EBLUP_data,
                    MSE = MSE,
                    transform_param = NULL,
                    model = list(coefficients = eblup$coefficients,
                                 variance = sigmau2,
                                 random_effects = eblup$random_effects,
                                 real_residuals = eblup$real_res,
                                 std_real_residuals = eblup$std_real_res,
                                 gamma = Gamma,
                                 model_select = criteria,
                                 correlation = correlation),
                    framework = framework[c("direct", "vardir", "N_dom_smp",
                                            "N_dom_unobs")],
                    transformation = list(transformation = transformation,
                                          backtransformation = backtransformation),
                    method = list(method = method,
                                  MSE_method = MSE_method),
                    fixed = fixed,
                    call = call,
                    successful_bootstraps = NULL
        )
        
      } else if (method == "moment") {
        out <- list(ind = eblup$EBLUP_data,
                    MSE = MSE,
                    transform_param = NULL,
                    model = list(coefficients = eblup$coefficients,
                                 variance = sigmau2$sigmau_YL,
                                 random_effects = eblup$random_effects,
                                 real_residuals = eblup$real_res,
                                 std_real_residuals = eblup$std_real_res,
                                 gamma = Gamma,
                                 model_select = NULL,
                                 correlation = correlation),
                    framework = framework[c("direct", "vardir", "N_dom_smp",
                                            "N_dom_unobs")],
                    transformation = list(transformation = transformation,
                                          backtransformation = backtransformation),
                    method = list(method = method,
                                  MSE_method = MSE_method),
                    fixed = fixed,
                    call = call,
                    successful_bootstraps = NULL
        )
        
      }
      
      
    } else if (transformation != "no") {
      
      # Shrinkage factor
      Gamma <- data.frame(Domain = framework$data[[framework$domains]],
                          Gamma = as.numeric(eblup$gamma))
      
      # Back-transformation
      result_data <- backtransformed(framework = framework,
                                     sigmau2 = sigmau2, eblup = eblup,
                                     transformation = transformation,
                                     backtransformation = backtransformation,
                                     combined_data = combined_data,
                                     method = method, interval = interval,
                                     MSE = MSE,
                                     mse_type = mse_type,
                                     B = B, alpha = alpha)
      
      out <- list(ind = result_data$EBLUP_data,
                  MSE = result_data$MSE_data,
                  transform_param = NULL,
                  model = list(coefficients = eblup$coefficients,
                               variance = sigmau2,
                               random_effects = eblup$random_effects[, 1],
                               real_residuals = eblup$real_res[, 1],
                               std_real_residuals = eblup$std_real_res[, 1],
                               gamma = Gamma,
                               model_select = criteria, 
                               correlation = correlation),
                  framework = framework[c("direct", "vardir", "N_dom_smp",
                                          "N_dom_unobs")],
                  transformation = list(transformation = transformation,
                                        backtransformation = backtransformation),
                  method = list(method = method,
                                MSE_method = result_data$MSE_method),
                  fixed = fixed,
                  call = call,
                  successful_bootstraps = NULL)
    }
  } else if (method == "reblup" | method == "reblupbc") {
    
    # Standard EBLUP -----------------------------------------------------------
    eblup <- eblup_robust(framework = framework, vardir = vardir, combined_data = combined_data,
                          method = method, k = k, c = c, 
                          correlation = correlation, corMatrix = corMatrix,
                          time = time)
    
    # MSE ----------------------------------------------------------------------
    if (MSE == TRUE) {
      MSE_data <- wrapper_MSE(framework = framework, combined_data = combined_data,
                              vardir = vardir, eblup = eblup,
                              mse_type = mse_type, method = method, B = B)
      MSE <- MSE_data$MSE_data
      MSE_method <- MSE_data$MSE_method
    } else {
      MSE <- NULL
      MSE_method <- "no mse estimated"
    }
    
    
    out <- list(ind = eblup$EBLUP_data,
                MSE = MSE,
                transform_param = NULL,
                model = list(coefficients = eblup$coefficients,
                             variance = eblup$variance,
                             random_effects = as.matrix(eblup$random_effects),
                             real_residuals = as.matrix(eblup$real_res),
                             std_real_residuals = as.matrix(eblup$std_real_res),
                             model_select = NULL,
                             correlation = correlation,
                             W = eblup$W,
                             n_time = eblup$nTime,
                             k = k,
                             c = c,
                             scores = eblup$scores,
                             iterations = eblup$iterations,
                             max_iter = eblup$maxIter,
                             max_iter_param = eblup$maxIterParam,
                             max_iter_re = eblup$maxIterRe),
                framework = framework[c("direct", "vardir", "N_dom_smp",
                                        "N_dom_unobs")],
                transformation = list(transformation = transformation,
                                      backtransformation = backtransformation),
                method = list(method = method,
                              MSE_method = MSE_method),
                fixed = fixed,
                call = call,
                successful_bootstraps = NULL)
    

  }
  

  class(out) <- c("emdi", "model", "fh")

  return(out)

}
