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
    groups_formula <- eval(parse(text = paste("~", obje-ct$framework$smp_domains)))
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
    response <- object$framework$response
  } else if(inherits(object, "direct")){
    response <- object$framework$y_vec
  } else if (inherits(object, "fh")) {
    response <- object$framework$direct
  }
  response
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


#' Extract residuals of emdi objects
#'
#' Method \code{residuals.emdi} extracts the residuals from an emdi 
#' object.
#' 
#' @param object an object of type "emdi".
#' @param ... additional arguments that are not used in this method.
#' @return For class ebp a vector containing the residuals is returned. 
#' For class fh a data frame containing the realized and standardized realized 
#' residuals. For class direct no residuals are available.
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

#' Extract random effects of emdi objects
#'
#' Method \code{ranef.emdi} extracts the random effects from an emdi 
#' object.
#' 
#' @param object an object of type "emdi".
#' @param ... additional arguments that are not used in this method.
#' @return For class ebp a vector containing the residuals is returned. 
#' For class fh a data frame containing the realized and standardized realized 
#' residuals. For class direct no residuals are available.
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
    cat("For an object of class direct no fixed effects are available.")
  } else if (inherits(object, "fh")) {
    object$model$random_effects
  }
}



#' Extract model coefficients of emdi objects
#'
#' Method \code{coef.emdi} extracts the model coefficients from an emdi 
#' object.
#' 
#' @param object an object of type "emdi".
#' @param ... additional arguments that are not used in this method.
#' @return For class ebp a vector containing the residuals is returned. 
#' For class fh a data frame containing the realized and standardized realized 
#' residuals. For class direct no residuals are available.
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


#' Extract log-Likelihood of emdi objects
#'
#' Method \code{logLik.emdi} extracts the log-Likelihood from an emdi 
#' object.
#' 
#' @param object an object of type "emdi".
#' @param ... additional arguments that are not used in this method.
#' @return For class ebp and fh, a vector containing the log-Likelihood. For 
#' class direct, no residuals are available.
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


#' Predictions from emdi objects
#'
#' Method \code{predict.emdi} extracts the small area predictions from an emdi 
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






