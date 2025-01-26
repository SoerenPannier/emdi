# Extract Model Coefficients of emdi Objects -----------------------------------

#' @aliases coefficients
#' @export
#' @method coef ebp
#' @importFrom stats coef coefficients

coef.ebp <- function(object, weights = FALSE, ...) {
  throw_class_error(object, "ebp")
  if (isFALSE(weights)) {
    coef(object$model)
  } else {
    object$model$coefficients_weighted
  }
}


#' @aliases coefficients
#' @export
#' @method coef fh
#' @importFrom stats coef coefficients

coef.fh <- function(object, ...) {
  throw_class_error(object, "fh")
  fixed_effects <- object$model$coefficients$coefficients
  names(fixed_effects) <- row.names(object$model$coefficients)
  fixed_effects
}

#' @aliases coefficients
#' @export
#' @method coef fh_tf
#' @importFrom stats coef coefficients

coef.fh_tf <- function(object, ...) {
  throw_class_error(object, "fh_tf")
  fixed_effects <- object$model$coefficients$coefficients
  names(fixed_effects) <- row.names(object$model$coefficients)
  fixed_effects
}

# Confidence Intervals of an emdi Object ---------------------------------------

#' @export
#' @method confint ebp
#' @importFrom nlme intervals
#' @importFrom stats confint

confint.ebp <- function(object, parm = NULL, level = 0.95, ...) {
  throw_class_error(object, "ebp")
  if (!is.null(parm)) {
    confidence_intervals <- intervals(object$model, level = level)$fixed
    subset(confidence_intervals, rownames(confidence_intervals) %in% parm)
  } else {
    intervals(object$model, level = level)$fixed
  }
}

#' @export
#' @method confint fh
#' @importFrom stats confint
confint.fh <- function(object, parm = NULL, level = 0.95, ...) {
  throw_class_error(object, "fh")
  coefmat <- object$model$coefficients

  coefs <- coefmat[, 1]
  stds <- coefmat[, 2]
  dist <- qnorm(p = (1 - level) / 2, 0, stds)
  ret_value <- data.frame(
    lower = coefs + dist,
    est. = coefs,
    upper = coefs + abs(dist),
    row.names = row.names(coefmat)
  )
  if (is.null(parm)) {
    as.matrix(ret_value)
  } else {
    as.matrix(ret_value[parm, ])
  }
}

#' @export
#' @method confint fh_tf
#' @importFrom stats confint
confint.fh_tf <- function(object, parm = NULL, level = 0.95, ...) {
  throw_class_error(object, "fh_tf")
  coefmat <- object$model$coefficients

  coefs <- coefmat[, 1]
  stds <- coefmat[, 2]
  dist <- qnorm(p = (1 - level) / 2, 0, stds)
  ret_value <- data.frame(
    lower = coefs + dist,
    est. = coefs,
    upper = coefs + abs(dist),
    row.names = row.names(coefmat)
  )
  if (is.null(parm)) {
    as.matrix(ret_value)
  } else {
    as.matrix(ret_value[parm, ])
  }
}

# Extract the AIC from a Model Fit of an emdi Object ---------------------------
#' @export
#' @method extractAIC fh
#' @importFrom stats extractAIC

extractAIC.fh <- function(fit, ...) {
  throw_class_error(fit, "fh")
  if (!is.null(fit$model$model_select$AIC)) {
    message(strwrap(prefix = " ", initial = "",
                    paste0("Estimation approach used is ",
                           fit$method$method, ": ",
                           round(fit$model$model_select$AIC, 5)
                           )))
    invisible(fit$model$model_select$AIC)
  } else {
    message(strwrap(prefix = " ", initial = "",
                    paste0("No AIC is returned for estimation approach ",
                           fit$method$method, "."
                           )))
  }
}

# Extracts family object of emdi object ----------------------------------------
#' @export
#' @method family ebp
#' @importFrom stats family gaussian

family.ebp <- function(object, ...) {
  throw_class_error(object, "ebp")
  gaussian(link = "identity")
}


#' @export
#' @method family fh
#' @importFrom stats family gaussian

family.fh <- function(object, ...) {
  throw_class_error(object, "fh")
  gaussian(link = "identity")
}

#' @export
#' @method family fh_tf
#' @importFrom stats family gaussian

family.fh_tf <- function(object, ...) {
  throw_class_error(object, "fh_tf")
  gaussian(link = "identity")
}

# Extract fitted values of emdi objects ----------------------------------------

#' @aliases fitted.values
#' @export
#' @method fitted ebp
#' @importFrom stats fitted fitted.values

fitted.ebp <- function(object, ...) {
  throw_class_error(object, "ebp")
  fitted(object$model, ...)
}


#' @aliases fitted.values
#' @export
#' @method fitted fh
#' @importFrom stats fitted fitted.values

fitted.fh <- function(object, ...) {
  throw_class_error(object, "fh")
  object$model$fitted
}

#' @aliases fitted.values
#' @export
#' @method fitted fh_tf
#' @importFrom stats fitted fitted.values

fitted.fh_tf <- function(object, ...) {
  throw_class_error(object, "fh_tf")
  object$model$fitted
}

# Extract the model formula of an emdi object ----------------------------------

#' @export
#' @method formula ebp
#' @importFrom stats formula

formula.ebp <- function(x, ...) {
  throw_class_error(x, "ebp")
  x$fixed
}


#' @export
#' @method formula fh
#' @importFrom stats formula

formula.fh <- function(x, ...) {
  throw_class_error(x, "fh")
  x$fixed
}


#' @export
#' @method formula fh_tf
#' @importFrom stats formula

formula.fh_tf <- function(x, ...) {
  throw_class_error(x, "fh_tf")
  x$fixed
}


# Extract log-Likelihood of emdi objects ---------------------------------------
#' @export
#' @method logLik ebp
#' @importFrom stats logLik

logLik.ebp <- function(object, ...) {
  throw_class_error(object, "ebp")
  message(strwrap(prefix = " ", initial = "",
                  paste0("Estimation approach used is reml: ",
                         round(object$model$logLik, 5))))
  invisible(object$model$logLik)
}


#' @export
#' @method logLik fh
#' @importFrom stats logLik

logLik.fh <- function(object, ...) {
  throw_class_error(object, "fh")
  if (!is.null(object$model$model_select$loglike)) {
    message(strwrap(prefix = " ", initial = "",
                    paste0("Estimation approach used is ",
                           object$method$method, ":",
                           round(object$model$model_select$loglike, 5)
                           )))
    invisible(object$model$model_select$loglike)
  } else {
    message(paste0(
      "No likelihood is returned for estimation approach ",
      object$method$method, "."
    ))
  }
}

#' @export
#' @method logLik fh_tf
#' @importFrom stats logLik

logLik.fh_tf <- function(object, ...) {
  throw_class_error(object, "fh_tf")
  object$model$loglike
}

# Extract the number of `observations´ from a fit of an emdi object -----------
#' @export
#' @method nobs ebp
#' @importFrom stats nobs

nobs.ebp <- function(object, ...) {
  throw_class_error(object, "ebp")
  N_obs <- object$framework$N_smp
  N_obs
}

# Extract the number of `observations´ from a fit of an emdi object
#' @export
#' @method nobs fh
#' @importFrom stats nobs

nobs.fh <- function(object, ...) {
  throw_class_error(object, "fh")
  N_obs <- object$framework$N_dom_smp
  N_obs
}

#' @export
#' @method nobs fh_tf
#' @importFrom stats nobs

nobs.fh_tf <- function(object, ...) {
  throw_class_error(object, "fh_tf")
  N_obs <- object$framework$N_in_sub
  N_obs
}

#' Predictions from emdi Objects
#'
#' Method \code{predict.emdi} extracts the direct estimates, the empirical
#' best linear unbiased or empirical best predictors for all domains from an
#' emdi object.
#'
#' @param object an object of type "emdi".
#' @param ... additional arguments that are not used in this method.
#' @return Data frame with domain predictors.
#' @details For a better selection of prediction results, it is referred to use
#' the generic function \code{\link{estimators}}. The methods for object of
#' class "emdi" allows to select among the indicators of interest.
#' @seealso \code{\link{direct}}, \code{\link{ebp}}, \code{\link{fh}}
#' @examples
#' \donttest{
#' # Example for class ebp
#' emdi_model <- ebp(
#'   fixed = eqIncome ~ gender + eqsize + cash + self_empl +
#'     unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + fam_allow +
#'     house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop,
#'   pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district",
#'   na.rm = TRUE
#' )
#'
#' predict(emdi_model)
#' }
#' @export
#' @method predict emdi
#' @importFrom stats predict

predict.emdi <- function(object, ...) {
  if(any(inherits(object, which = TRUE, c("ebp_tf", "fh_tf")))){
    object$ind_Domain
    object$ind_Subdomain
  }else{
#-------------------------------------------------------------------------------
    object$ind # Originally, only object$ind was in the function
#-------------------------------------------------------------------------------
  }
}


# Extract residuals of emdi objects --------------------------------------------

#' @aliases resid
#' @export
#' @method residuals ebp
#' @importFrom stats residuals resid

residuals.ebp <- function(object, ...) {
  throw_class_error(object, "ebp")
  residuals(object$model, ...)
}


#' @aliases resid
#' @export
#' @method residuals fh
#' @importFrom stats residuals resid

residuals.fh <- function(object, ...) {
  throw_class_error(object, "fh")
  type <- ""
  try(type <- list(...)[[1]], silent = TRUE)
  if (type == "standardized") {
    object$model$std_real_residuals
  } else {
    object$model$real_residuals
  }
}

#' @aliases resid
#' @export
#' @method residuals fh_tf
#' @importFrom stats residuals resid

residuals.fh_tf <- function(object, ...) {
  throw_class_error(object, "fh_tf")
  type <- ""
  try(type <- list(...)[[1]], silent = TRUE)
  if (type == "standardized") {
    object$model$std_real_residuals
  } else {
    object$model$real_residuals
  }
}

# Extract residual standard deviation of emdi objects --------------------------

#' @export
#' @importFrom stats sigma

sigma.ebp <- function(object, ...) {
  throw_class_error(object, "ebp")
  object$model$sigma
}


# Constructs a terms object from an emdi object --------------------------------

#' @export
#' @method terms ebp
#' @importFrom stats aov terms

terms.ebp <- function(x, ...) {
  throw_class_error(x, "ebp")
  terms(aov(x$fixed, x$framework$smp_data))
}

#' @export
#' @method terms fh
#' @importFrom stats aov terms

terms.fh <- function(x, ...) {
  throw_class_error(x, "fh")
  terms(aov(x$fixed, x$framework$combined_data))
}

#' @export
#' @method terms fh_tf
#' @importFrom stats aov terms

terms.fh_tf <- function(x, ...) {
  throw_class_error(x, "fh_tf")
  terms(aov(x$fixed, x$framework$orig_data))
}

# Extract variance-covariance matrix of the main parameters of emdi objects ----

#' @export
#' @method vcov ebp
#' @importFrom stats vcov

vcov.ebp <- function(object, ...) {
  throw_class_error(object, "ebp")
  vcov(object$model, ...)
}


# Extract variance-covariance matrix of the main parameters of emdi objects ----

#' @export
#' @method vcov fh
#' @importFrom stats vcov

vcov.fh <- function(object, ...) {
  throw_class_error(object, "fh")
  object$model$beta_vcov
}

#' @export
#' @method vcov fh_tf
#' @importFrom stats vcov

vcov.fh_tf <- function(object, ...) {
  throw_class_error(object, "fh_tf")
  object$model$beta_vcov
}
