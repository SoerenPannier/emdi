#' Compare predictions of emdi objects
#'
#' Method \code{comparePred.emdi} compares predictions of emdi objects.
#' 
#' @param object1 an object of type "emdi".
#' @param object2 an object of type "emdi".
#' @param MSE if \code{TRUE}, MSE estimates are calculated. Defaults to \code{FALSE}.
#' @param ... additional arguments that are not used in this method.
#' @return Data frame containing the point estimates of both emdi objects. If 
#' column names are duplicated, the suffixes "_1" and "_2" are added to their names.
#' "_1" and "_2" standing for object1 and object2, respectively. If \code{MSE} is 
#' set to \code{TRUE}, the data frame contains the MSE estimates of the emdi objects.
#' @seealso \code{\link{direct}}, \code{\link{ebp}}, \code{\link{fh}}
#' @examples
#' \donttest{
#' # Example for class ebp
#' emdi_model_1 <- ebp(fixed = eqIncome ~ gender + eqsize + cash + self_empl + 
#' unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + fam_allow + 
#' house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop, 
#' pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district", 
#' na.rm = TRUE)
#' 
#' emdi_model_2 <- ebp(fixed = eqIncome ~ gender + eqsize + cash + self_empl + 
#' unempl_ben + age_ben + surv_ben, pop_data = eusilcA_pop, 
#' pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district", 
#' na.rm = TRUE)
#' 
#' comparePred(emdi_model_1, emdi_model_2)
#' }
#' @export
#' @method comparePred emdi
#' @importFrom nlme comparePred
 
comparePred.emdi <- function(object1, object2, MSE = FALSE, ...) {
  
  if(!inherits(object1, "emdi") || !inherits(object2, "emdi")){
    stop('Both objects need to be of class emdi.')
  }
  
  if ((length(object1$ind$Domain) == length(object2$ind$Domain)) &&
      (!all(as.character(object1$ind$Domain) %in% 
           as.character(object2$ind$Domain)))) {
    stop('It is only possible to compare emdi objects with the same domains.')
  }
  
  if ((length(object1$ind$Domain) < length(object2$ind$Domain)) &&
    !all(as.character(object1$ind$Domain) %in% 
           as.character(object2$ind$Domain))) {
    stop('The first object contains domains that are
         not contained in the second object. It is only possible to compare 
         emdi objects with the same domains.')
  }
  
  if ((length(object2$ind$Domain) < length(object1$ind$Domain)) &&
      !all(as.character(object2$ind$Domain) %in% 
           as.character(object1$ind$Domain))) {
    stop('The second object contains domains that are
         not contained in the first object. It is only possible to compare 
         emdi objects with the same domains.')
  }
  
  if ((MSE == TRUE) && (is.null(object1$MSE) || is.null(object2$MSE))) {
    stop('If MSE is set to TRUE, both emdi objects need to contain MSE estimates.')
  }
  
  
  if(MSE == FALSE){
    object1data <- get("ind", object1)
    object2data <- get("ind", object2)
  } else if(MSE == TRUE){
    object1data <- get("MSE", object1)
    object2data <- get("MSE", object2)
  }
  
  if(inherits(object1, "fh")){
    object1data <- object1data[ , -4] # remove column Out
  }
  
  if(inherits(object2, "fh")){
    object2data <- object2data[ , -4] # remove column Out
  }
  
  order_direct_ebp <- c("Domain", "Mean_1", "Mean_2", "Head_Count_1", "Head_Count_2",
                        "Poverty_Gap_1", "Poverty_Gap_2", "Gini_1", "Gini_2",
                        "Quintile_Share_1", "Quintile_Share_2", "Quantile_10_1",
                        "Quantile_10_2", "Quantile_25_1", "Quantile_25_2",
                        "Median_1", "Median_2", "Quantile_75_1", "Quantile_75_2",
                        "Quantile_90_1", "Quantile_90_2")
  
  if( (inherits(object1, "ebp") && inherits(object2, "ebp")) ||
     (inherits(object1, "direct") && inherits(object2, "direct")) ||
     (inherits(object1, "direct") && inherits(object2, "ebp")) ||
     (inherits(object1, "ebp") && inherits(object2, "direct")) ){
    
    if(dim(object1data)[2] > 11){
      colnames(object1data)[12:dim(object1data)[2]] <- 
        paste0(names(object1data[12:dim(object1data)[2]]), "_1")
    }
    
    if(dim(object2data)[2] > 11){
      colnames(object2data)[12:dim(object2data)[2]] <- 
        paste0(names(object2data[12:dim(object2data)[2]]), "_2")
    }
    
    data <- merge(object1data, object2data, by.x = "Domain", by.y = "Domain", 
                  suffixes = c("_1","_2"))
    
    if(dim(data)[2] == 21){
      data <- data[, order_direct_ebp]
    } else if(dim(data)[2] > 21){
      custom_indicators <- colnames(data)[(which(!colnames(data) %in% order_direct_ebp))]
      data <- data[, c(order_direct_ebp, custom_indicators)]
    }
    
  } else if(inherits(object1, "fh") && inherits(object2, "fh")){
    data <- merge(object1data, object2data, by.x = "Domain", by.y = "Domain", 
                  suffixes = c("_1","_2"))
    data <- data[, c("Domain", "Direct_1", "Direct_2", "FH_1", "FH_2")]
  } else if((inherits(object1, "direct") && inherits(object2, "fh")) ||
            (inherits(object1, "fh") && inherits(object2, "direct"))){
    data <- merge(object1data, object2data, by.x = "Domain", by.y = "Domain", 
                  all = TRUE, suffixes = c("_1","_2"))
  } else if((inherits(object1, "fh") && inherits(object2, "ebp")) ||
            (inherits(object1, "ebp") && inherits(object2, "fh"))){
    data <- merge(object1data, object2data, by.x = "Domain", by.y = "Domain", 
                  all = TRUE, suffixes = c("_1","_2"))
  }
  data
}

#' Extract fixed effects from an emdi object
#'
#' Method \code{fixef.emdi} extracts the fixed effects from an emdi 
#' object.
#' 
#' @param object an object of type "emdi".
#' @param ... additional arguments that are not used in this method.
#' @return For class ebp and fh a vector containing the fixed effects is 
#' returned. For class direct no fixed effects are available.
#' @seealso \code{\link{ebp}}, \code{\link{fh}}
#' @aliases fixed.effects
#' @examples
#' \donttest{
#' # Example for class ebp
#' emdi_model <- ebp(fixed = eqIncome ~ gender + eqsize + cash + self_empl + 
#' unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + fam_allow + 
#' house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop, 
#' pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district", 
#' na.rm = TRUE)
#' 
#' fixef(emdi_model)
#' }
#' @export
#' @method fixef emdi
#' @importFrom nlme fixef fixed.effects

fixef.emdi <- function(object, ...) {
  
  if(!inherits(object, "emdi")){
    stop('First object needs to be of class emdi.')
  }
  
  if(inherits(object, "ebp")){
    object$model$coefficients$fixed
  } else if(inherits(object, "direct")){
    cat("For an object of class direct no fixed effects are available.")
  } else if (inherits(object, "fh")) {
    fixed_effects <- object$model$coefficients$coefficients
    names(fixed_effects) <- row.names(object$model$coefficients)
    fixed_effects
  }
}

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

#' Extract grouping formula from an emdi object
#'
#' Method \code{getGroupsFormula.emdi} extracts the grouping formula from an 
#' emdi object.
#' 
#' @param object an object of type "emdi".
#' @param ... additional arguments that are not used in this method.
#' @return A one-sided formula.
#' @seealso \code{\link{direct}}, \code{\link{ebp}},
#' \code{\link{fh}}, \code{\link[nlme]{getGroupsFormula}}
#' @examples
#' \donttest{
#' # Example for class ebp
#' emdi_model <- ebp(fixed = eqIncome ~ gender + eqsize + cash + self_empl + 
#' unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + fam_allow + 
#' house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop, 
#' pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district", 
#' na.rm = TRUE)
#' 
#' getGroupsFormula(emdi_model)
#' }
#' @export
#' @importFrom nlme getGroupsFormula

getGroupsFormula.emdi <- function(object, ...) {
  
  if(!inherits(object, "emdi")){
    stop('First object needs to be of class emdi.')
  }
  
  if(inherits(object, "ebp")){
    groups_formula <- eval(parse(text = paste("~", object$framework$smp_domains)))
  } else if(inherits(object, "direct")){
    groups_formula <- eval(parse(text = paste("~", object$framework$smp_domains)))
  } else if (inherits(object, "fh")) {
    groups_formula <- eval(parse(text = paste("~", object$framework$domains)))
  }
  groups_formula
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
    response <- makeXY(object$fixed, object$framework$smp_data)$y
  } else if(inherits(object, "direct")){
    response <- object$framework$smp_data[, object$call$y]
  } else if (inherits(object, "fh")) {
    response <- object$framework$direct
  }
  response
}

#' Extract random effects of emdi objects
#'
#' Method \code{ranef.emdi} extracts the random effects from an emdi 
#' object.
#' 
#' @param object an object of type "emdi".
#' @param ... additional arguments that are not used in this method.
#' @return For class ebp and fh, a vector containing the estimated random effects 
#' at domain level is returned. 
#' For class direct, the method is not applicable.
#' @seealso \code{\link{ebp}}, \code{\link{fh}}
#' @aliases random.effects
#' @examples
#' \donttest{
#' # Example for class ebp
#' emdi_model <- ebp(fixed = eqIncome ~ gender + eqsize + cash + self_empl + 
#' unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + fam_allow + 
#' house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop, 
#' pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district", 
#' na.rm = TRUE)
#' 
#' ranef(emdi_model)
#' }
#' @export
#' @method ranef emdi
#' @importFrom nlme ranef random.effects

ranef.emdi <- function(object, ...) {
  
  if(!inherits(object, "emdi")){
    stop('First object needs to be of class emdi.')
  }
  
  if(inherits(object, "ebp")){
    ranef(object$model)
  } else if(inherits(object, "direct")){
    cat("For an object of class direct no random effects are available.")
  } else if (inherits(object, "fh")) {
    random_effects <- object$model$random_effects
    row.names(random_effects) <- object$ind$Domain
    random_effects
  }
}









