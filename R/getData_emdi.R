#' Extract emdi Object Data
#'
#' Extracts the data frame used to fit the model.
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




