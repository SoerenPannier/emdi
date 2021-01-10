#' Extract model coefficients of emdi objects
#'
#' Method \code{coef.emdi} extracts the model coefficients from an emdi 
#' object.
#' 
#' @param object an object of type "emdi".
#' @param ... additional arguments that are not used in this method.
#' @return For class ebp, a matrix with the extracted coefficients by domains 
#' is returned. For class fh, a vector of the extracted coefficients is returned. 
#' For class direct, the method is not applicable.
#' @seealso \code{\link{ebp}}, \code{\link{fh}}
#' @aliases coefficients
#' @examples
#' \donttest{
#' # Example for class ebp
#' emdi_model <- ebp(fixed = eqIncome ~ gender + eqsize + cash + self_empl + 
#' unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + fam_allow + 
#' house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop, 
#' pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district", 
#' na.rm = TRUE)
#' 
#' coef(emdi_model)
#' }
#' @export
#' @method coef emdi
#' @importFrom stats coef coefficients

coef.emdi <- function(object, ...) {
  
  if(!inherits(object, "emdi")){
    stop('First object needs to be of class emdi.')
  }
  
  if(inherits(object, "ebp")){
    coef(object$model)
  } else if(inherits(object, "direct")){
    cat("For an object of class direct no fixed effects are available.")
  } else if (inherits(object, "fh")) {
    fixed_effects <- object$model$coefficients$coefficients
    names(fixed_effects) <- row.names(object$model$coefficients)
    fixed_effects
  }
}

#' Confidence intervals for emdi objects
#'
#' Method \code{confint.emdi} computes confidence intervals for direct estimates, 
#' empirical best linear unbiased and empirical best predictors.  
#' 
#' @param object an object of type "emdi".
#' @param parm equivalent to indicator argument in other methods. Optional character 
#' vector that selects which indicators shall be returned: (i) all calculated 
#' indicators ("all"); (ii) each indicator name: "Mean", "Quantile_10", 
#' "Quantile_25", "Median", "Quantile_75", "Quantile_90", "Head_Count",
#' "Poverty_Gap", "Gini", "Quintile_Share" or the function name/s of
#' "custom_indicator/s"; (iii) groups of indicators: "Quantiles", "Poverty",
#' "Inequality" or "Custom". If two of these groups are selected, only the first
#' one is returned. Note, additional custom indicators can be
#' defined as argument for model-based approaches (see also \code{\link{ebp}})
#' and do not appear in groups of indicators even though these might belong to
#' one of the groups. If the \code{model} argument is of type "model","fh", 
#' indicator can be set to "all", "Direct", FH", or "FH_Bench" (if emdi 
#' object is overwritten by function benchmark). Defaults to "all".
#' @param level the confidence level required. Defaults to 0.95.
#' @param ... additional arguments that are not used in this method.
#' @return For emdi objects, a data frame with the point estimator and lower and
#' upper bounds is returned.
#' @details The confidence intervals are constructed using following equation 
#' (exemplarily for the 95\% interval): 
#' \ifelse{html}{\out{CI<sub>d</sub> = y&#770;<sub>d</sub> +/- 1.96 &#8730; MSE(y&#770;<sub>d</sub>)}}{\deqn{CI_{d} = \hat{ind}_{d} +/- 1.96 \sqrt{MSE(\hat{y}_{d})}}}
#' @seealso \code{\link{direct}}, \code{\link{ebp}}, \code{\link{fh}}, 
#' \code{\link[stats]{confint}}
#' @examples
#' \donttest{
#' # Example for class ebp
#' emdi_model <- ebp(fixed = eqIncome ~ gender + eqsize + cash + self_empl + 
#' unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + fam_allow + 
#' house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop, 
#' pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district", 
#' na.rm = TRUE)
#' 
#' confint(emdi_model)
#' }
#' @export
#' @importFrom stats confint

confint.emdi <- function(object, parm = 'all', level = 0.95,  ...) {
  
  if(!inherits(object, "emdi")){
    stop('First object needs to be of class emdi.')
  }
  
  estimators_check(object = object, indicator = indicator,
                   MSE = FALSE, CV = FALSE)
  
  if(is.null(object$MSE)) {
    print('No MSE estimates are available. Confidence intervals can only be 
          computed when MSE estimates are available')
  } else {
    
    all_ind <- point_emdi(object = object, indicator = indicator)$ind
    selected <- colnames(all_ind)[-1]
    all_precisions <- mse_emdi(object = object, indicator = indicator, CV = FALSE)$ind
    
    a <- (1 - level)/2
    a <- 1 - a
    fac <- qnorm(a)
    lower <- all_ind[-1] - fac * sqrt(all_precisions[-1])
    colnames(lower) <- paste0(colnames(lower), "_lower")
    upper <- all_ind[-1] + fac * sqrt(all_precisions[-1])
    colnames(upper) <- paste0(colnames(upper), "_upper")
    
    combined <- data.frame(all_ind[1], lower, all_ind[-1], upper)
    endings <- c("_lower","", "_upper")
    
    combined <- combined[,c("Domain", paste0(rep(selected,each = length(endings)),
                                             endings))]
    cat(paste0("Lower and upper bounds for ", level*100, "% confidence level:\n"))
    return(combined)
  }
  
}

#' Extract the AIC from a model fit of an emdi object
#'
#' Method \code{extractAIC.emdi} extracts the Akaike Information Criterion from 
#' a model fit of an emdi object.
#' 
#' @param fit an object of type "emdi".
#' @param ... additional arguments that are not used in this method.
#' @return For objects of class fh a single number is returned. For class direct 
#' and ebp no AIC value is available.
#' @seealso \code{\link{fh}}, \code{\link[stats]{extractAIC}}
#' @examples
#' \donttest{
#' # Example for class fh
#' combined_data <- combine_data(pop_data = eusilcA_popAgg, pop_domains = "Domain", 
#'                               smp_data = eusilcA_smpAgg, smp_domains = "Domain")
#' 
#' fh_std <- fh(fixed = Mean ~ cash + self_empl, vardir = "Var_Mean", 
#' combined_data = combined_data, domains = "Domain", method = "ml", 
#' MSE = TRUE)
#' 
#' extractAIC(fh_std)
#' }
#' @export
#' @importFrom stats extractAIC

extractAIC.emdi <- function(fit, ...) {
  
  if(!inherits(fit, "emdi")){
    stop('First object needs to be of class emdi.')
  }
  
  if(inherits(fit, "ebp")){
    cat("For an object of class ebp no AIC value is available.")
  } else if(inherits(fit, "direct")){
    cat("For an object of class direct no AIC value is available.")
  } else if (inherits(fit, "fh")) {
    fit$model$model_select$AIC
  }
}

#' Extract fitted values of emdi objects
#'
#' Method \code{fitted.emdi} extracts the model fitted values from an emdi 
#' object.
#' 
#' @param object an object of type "emdi".
#' @param ... additional arguments that are not used in this method.
#' @return For class ebp XXXXXXXXXXXXX is returned. 
#' For class fh a vector containing the fitted values is returned. For class 
#' direct no model fitted values are available.
#' @seealso \code{\link{ebp}}, \code{\link{fh}}, \code{\link[stats]{fitted}}
#' @aliases fitted.values
#' @examples
#' \donttest{
#' # Example for class fh
#' combined_data <- combine_data(pop_data = eusilcA_popAgg, pop_domains = "Domain", 
#'                               smp_data = eusilcA_smpAgg, smp_domains = "Domain")
#'                               
#' fh_std <- fh(fixed = Mean ~ cash + self_empl, vardir = "Var_Mean", 
#'           combined_data = combined_data, domains = "Domain", method = "ml", 
#'           MSE = TRUE)
#' 
#' fitted(fh_std)
#' }
#' @export
#' @method fitted emdi
#' @importFrom stats fitted fitted.values

fitted.emdi <- function(object, ...) {
  
  if(!inherits(object, "emdi")){
    stop('First object needs to be of class emdi.')
  }
  
  if(inherits(object, "ebp")){
    object$model$fitted
  } else if(inherits(object, "direct")){
    cat("For an object of class direct no model fitted values are available.")
  } else if (inherits(object, "fh")) {
    object$model$fitted
  }
}

#' Extract the model formula of an emdi object
#'
#' Method \code{formula.emdi} extracts the model formula of an emdi object.
#' 
#' @param x an object of type "emdi".
#' @param ... additional arguments that are not used in this method.
#' @return Two-sided linear formula object describing the fixed-effects part of 
#' the regression model with the dependent variable on the left of a ~ operator 
#' and the explanatory variables on the right, separated by + operators. Method 
#' is not defined for emdi objects of class direct.
#' @seealso \code{\link{ebp}}, \code{\link{fh}}, \code{\link[stats]{formula}}
#' @examples
#' \donttest{
#' # Example for class fh
#' combined_data <- combine_data(pop_data = eusilcA_popAgg, pop_domains = "Domain", 
#'                               smp_data = eusilcA_smpAgg, smp_domains = "Domain")
#'                               
#' fh_std <- fh(fixed = Mean ~ cash + self_empl, vardir = "Var_Mean", 
#'           combined_data = combined_data, domains = "Domain", method = "ml", 
#'           MSE = TRUE)
#' 
#' formula(fh_std)
#' }
#' @export
#' @importFrom stats formula

formula.emdi <- function(x, ...) {
  
  if(!inherits(x, "emdi")){
    stop('First object needs to be of class emdi.')
  }
  
  if(inherits(x, "ebp")){
    x$fixed
  } else if(inherits(x, "direct")){
    cat("Object of class direct does not contain a model formula.")
  } else if (inherits(x, "fh")) {
    x$fixed
  }
}

#' Extract log-Likelihood of emdi objects
#'
#' Method \code{logLik.emdi} extracts the log-Likelihood from an emdi 
#' object.
#' 
#' @param object an object of type "emdi".
#' @param ... additional arguments that are not used in this method.
#' @return For class ebp and fh, a vector containing the log-Likelihood. For 
#' class direct, the method is not applicable.
#' @seealso \code{\link{ebp}}, \code{\link{fh}}
#' @examples
#' \donttest{
#' # Example for class ebp
#' emdi_model <- ebp(fixed = eqIncome ~ gender + eqsize + cash + self_empl + 
#' unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + fam_allow + 
#' house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop, 
#' pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district", 
#' na.rm = TRUE)
#' 
#' logLik(emdi_model)
#' }
#' @export
#' @method logLik emdi
#' @importFrom stats logLik

logLik.emdi <- function(object, ...) {
  
  if(!inherits(object, "emdi")){
    stop('First object needs to be of class emdi.')
  }
  
  if(inherits(object, "ebp")){
    object$model$logLik
  } else if(inherits(object, "direct")){
    cat("For an object of class direct no fixed effects are available.")
  } else if (inherits(object, "fh")) {
    object$model$model_select$loglike
  }
}

#' Extract the number of `observations´ from a fit of an emdi object
#'
#' Method \code{nobs.emdi} extracts the number of `observations´ from a 
#' fit of an emdi object.
#' 
#' @param object an object of type "emdi".
#' @param ... additional arguments that are not used in this method.
#' @return Single number.
#' @seealso \code{\link{direct}}, \code{\link{ebp}},
#' \code{\link{fh}}, \code{\link[stats]{nobs}}
#' @examples
#' \donttest{
#' # Example for class ebp
#' emdi_model <- ebp(fixed = eqIncome ~ gender + eqsize + cash + self_empl + 
#' unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + fam_allow + 
#' house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop, 
#' pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district", 
#' na.rm = TRUE)
#' 
#' nobs(emdi_model)
#' }
#' @export
#' @importFrom stats nobs
 
nobs.emdi <- function(object, ...) {

if(!inherits(object, "emdi")){
  stop('First object needs to be of class emdi.')
}

if(inherits(object, "ebp")){
  N_obs <- object$framework$N_smp
} else if(inherits(object, "direct")){
  N_obs <- object$framework$N_smp
} else if (inherits(object, "fh")) {
  N_obs <- object$framework$N_dom_smp
}
  N_obs
}

#' Predictions from emdi objects
#'
#' Method \code{predict.emdi} extracts the direct estimates, the empirical
#' best linear unbiased or empirical best predictors for all domains from an emdi 
#' object.
#' 
#' @param object an object of type "emdi".
#' @param ... additional arguments that are not used in this method.
#' @return Data frame with domain predictors.
#' @seealso \code{\link{direct}}, \code{\link{ebp}}, \code{\link{fh}}
#' @examples
#' \donttest{
#' # Example for class ebp
#' emdi_model <- ebp(fixed = eqIncome ~ gender + eqsize + cash + self_empl + 
#' unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + fam_allow + 
#' house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop, 
#' pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district", 
#' na.rm = TRUE)
#' 
#' predict(emdi_model)
#' }
#' @export
#' @method predict emdi

predict.emdi <- function(object, ...) {
  
  if(!inherits(object, "emdi")){
    stop('First object needs to be of class emdi.')
  }
  
  object$ind
}


#' Extract residuals of emdi objects
#'
#' Method \code{residuals.emdi} extracts the residuals from an emdi 
#' object.
#' 
#' @param object an object of type "emdi".
#' @param ... additional arguments that are not used in this method.
#' @return For class ebp, a vector containing the residuals is returned. 
#' For class fh, a data frame containing the realized and standardized realized 
#' residuals. For class direct, the method is not applicable.
#' @seealso \code{\link{ebp}}, \code{\link{fh}}
#' @aliases resid
#' @examples
#' \donttest{
#' # Example for class ebp
#' emdi_model <- ebp(fixed = eqIncome ~ gender + eqsize + cash + self_empl + 
#' unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + fam_allow + 
#' house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop, 
#' pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district", 
#' na.rm = TRUE)
#' 
#' residuals(emdi_model)
#' }
#' @export
#' @method residuals emdi
#' @importFrom stats residuals resid

residuals.emdi <- function(object, ...) {
  
  if(!inherits(object, "emdi")){
    stop('First object needs to be of class emdi.')
  }
  
  if(inherits(object, "ebp")){
    as.numeric(object$model$residuals[, 2])
  } else if(inherits(object, "direct")){
    cat("For an object of class direct no fixed effects are available.")
  } else if (inherits(object, "fh")) {
    data.frame(real_residuals = object$model$real_residuals, 
               std_real_residuals = object$model$std_real_residuals)
  }
}




#' Constructs a terms object from an emdi object
#'
#' Method \code{terms.emdi} constructs a terms object from an emdi object.
#' 
#' @param x an object of type "emdi".
#' @param ... additional arguments that are not used in this method.
#' @return For classes ebp and fh a \code{\link[stats]{terms.object}} is returned. 
#' For class direct no terms object is available.
#' @seealso \code{\link{ebp}}, \code{\link{fh}}, \code{\link[stats]{terms}}, 
#' \code{\link[stats]{terms.object}}
#' @examples
#' \donttest{
#' # Example for class ebp
#' emdi_model <- ebp(fixed = eqIncome ~ gender + eqsize + cash + self_empl + 
#' unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + fam_allow + 
#' house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop, 
#' pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district", 
#' na.rm = TRUE)
#' 
#' terms(emdi_model)
#' }
#' @export
#' @importFrom stats aov terms

terms.emdi <- function(x, ...) {
  
  if(!inherits(x, "emdi")){
    stop('First object needs to be of class emdi.')
  }
  
  if(inherits(x, "ebp")){
    terms(aov(x$fixed, x$framework$smp_data))
  } else if(inherits(x, "direct")){
    cat("For class direct no terms object is available.")
  } else if (inherits(x, "fh")) {
    terms(aov(x$fixed, x$framework$combined_data))
  }
}


