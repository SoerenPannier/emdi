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
