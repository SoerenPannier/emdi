#' Extract the number of `observations´ from a model fit of an emdi object
#'
#' Method \code{nobs.emdi} extracts the number of `observations´ from a model 
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

#' Extract the model formula of an emdi object
#'
#' Method \code{formula.emdi} extracts the model formula of an emdi object.
#' 
#' @param object an object of type "emdi".
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

formula.emdi <- function(object, ...) {
  
  if(!inherits(object, "emdi")){
    stop('First object needs to be of class emdi.')
  }
  
  if(inherits(object, "ebp")){
    object$fixed
  } else if(inherits(object, "direct")){
    cat("Object of class direct does not contain a model formula.")
  } else if (inherits(object, "fh")) {
    object$fixed
  }
}