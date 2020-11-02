#' Extract the number of `observations´ from a model fit of an emdi object
#'
#' Method \code{nobs.emdi} extracts the number of `observations´ from a model 
#' fit of an emdi object.
#' 
#' @param object an object of type "emdi".
#' @param ... additional arguments that are not used in this method.
#' @return Data frame used to fit the model. For classes direct and ebp the 
#' sample data is returned. For class fh the combined data set is returned.
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