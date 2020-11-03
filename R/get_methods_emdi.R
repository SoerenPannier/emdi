#' Extract emdi object data
#'
#' Method \code{getData.emdi} extracts the data frame used to fit the model.
#' 
#' @param object an object of type "emdi".
#' @param ... additional arguments that are not used in this method.
#' @return Data frame used to fit the model. For classes direct and ebp the 
#' sample data is returned. For class fh the combined data set is returned.
#' @seealso \code{\link{direct}}, \code{\link{ebp}},
#' \code{\link{fh}}, \code{\link[nlme]{getData}}
#' @examples
#' \donttest{
#' # Example for class direct
#' emdi_direct <- direct(y = "eqIncome", smp_data = eusilcA_smp, 
#' smp_domains = "district", weights = "weight", threshold = 11064.82, var = TRUE, 
#' boot_type = "naive", B = 50, seed = 123, X_calib = NULL, totals = NULL, 
#' na.rm = TRUE)
#' 
#' getData(emdi_direct)
#' }
#' @export
#' @importFrom nlme getData

getData.emdi <- function(object, ...) {
  
  if(!inherits(object, "emdi")){
    stop('First object needs to be of class emdi.')
  }
  
  if(inherits(object, "ebp")){
    data <- object$framework$smp_data
  } else if(inherits(object, "direct")){
    data <- object$framework$smp_data
  } else if (inherits(object, "fh")) {
    data <- object$framework$combined_data
  }
  data
}

#' Extract grouping factors from an emdi object
#'
#' Method \code{getGroups.emdi} extracts grouping factors from an emdi object.
#' 
#' @param object an object of type "emdi".
#' @param ... additional arguments that are not used in this method.
#' @return A vector containing the grouping factors.
#' @seealso \code{\link{direct}}, \code{\link{ebp}},
#' \code{\link{fh}}, \code{\link[nlme]{getGroups}}
#' @examples
#' \donttest{
#' # Example for class direct
#' emdi_direct <- direct(y = "eqIncome", smp_data = eusilcA_smp, 
#' smp_domains = "district", weights = "weight", threshold = 11064.82, var = TRUE, 
#' boot_type = "naive", B = 50, seed = 123, X_calib = NULL, totals = NULL, 
#' na.rm = TRUE)
#' 
#' getGroups(emdi_direct)
#' }
#' @export
#' @importFrom nlme getGroups

getGroups.emdi <- function(object, ...) {
  
  if(!inherits(object, "emdi")){
    stop('First object needs to be of class emdi.')
  }
  
  if(inherits(object, "ebp")){
    groups <- object$framework$smp_domains_vec
  } else if(inherits(object, "direct")){
    groups <- object$framework$smp_domains_vec
  } else if (inherits(object, "fh")) {
    groups <- object$ind$Domain
  }
  groups
}

#' Extract response variable from an emdi object
#'
#' Method \code{getResponse.emdi} extracts the response variable from an emdi 
#' object.
#' 
#' @param object an object of type "emdi".
#' @param ... additional arguments that are not used in this method.
#' @return Vector containing the response variable. 
#' @seealso \code{\link{direct}}, \code{\link{ebp}}, \code{\link{fh}}, \code{\link[nlme]{getResponse}}
#' @examples
#' \donttest{
#' # Example for class ebp
#' emdi_model <- ebp(fixed = eqIncome ~ gender + eqsize + cash + self_empl + 
#' unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + fam_allow + 
#' house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop, 
#' pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district", 
#' na.rm = TRUE)
#' 
#' getResponse(emdi_model)
#' }
#' @export
#' @importFrom nlme getResponse

getResponse.emdi <- function(object, ...) {
  
  if(!inherits(object, "emdi")){
    stop('First object needs to be of class emdi.')
  }
  
  if(inherits(object, "ebp")){
    response <- object$framework$response
  } else if(inherits(object, "direct")){
    response <- object$framework$y_vec
  } else if (inherits(object, "fh")) {
    response <- object$framework$direct
  }
  response
}


