#' Extract fixed effects from an emdi object
#'
#' Methods \code{fixef.ebp} and \code{fixef.fh} extract the 
#' fixed effects from an emdi object of class "ebp" or "fh".
#' 
#' @param object an object of type "emdi", depending on the used method either "ebp" or "fh".
#' @param ... additional arguments that are not used in this method.
#' @return For classes ebp and fh a vector containing the fixed effects is 
#' returned. 
#' @details The alias \code{fixed.effects} can also be used instead of \code{fixef}. 
#' The generic function \code{fixef} is imported from package nlme and 
#' re-exported to make the S3-methods available, even though the nlme package 
#' itself is not loaded or attached. For default documentation, 
#' see \code{\link[nlme]{fixed.effects}}.
#' @seealso \code{\link{ebp}}, \code{\link{fh}}, \code{\link[nlme]{fixed.effects}}
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
#' @aliases fixed.effects
#' @name fixef
#' @importFrom nlme fixef fixed.effects
#' @export fixed.effects
#' @export fixef
fixef

#' @export fixef.ebp
#' @export
#' @rdname fixef
fixef.ebp <- function(object, ...) {
  throw_class_error(object, "ebp")
  object$model$coefficients$fixed
}

#' @export fixed.effects.ebp
#' @export
#' @rdname fixef
fixed.effects.ebp <- function(object, ...) {
  throw_class_error(object, "ebp")
  object$model$coefficients$fixed
}

#' @export fixef.fh
#' @export
#' @rdname fixef
fixef.fh <- function(object, ...) {
  throw_class_error(object, "fh")
  fixed_effects <- object$model$coefficients$coefficients
  names(fixed_effects) <- row.names(object$model$coefficients)
  fixed_effects
}

#' @export fixed.effects.fh
#' @export
#' @rdname fixef
fixed.effects.fh <- function(object, ...) {
  throw_class_error(object, "fh")
  fixed_effects <- object$model$coefficients$coefficients
  names(fixed_effects) <- row.names(object$model$coefficients)
  fixed_effects
}

#' Extract emdi object data
#'
#' Methods \code{getData.direct}, \code{getData.ebp} and \code{getData.fh} extract 
#' the data frame used to fit the model.
#' 
#' @param object an object of type "emdi", depending on the method either "direct", 
#' "ebp" or "fh".
#' @param ... additional arguments that are not used in this method.
#' @return Data frame used to fit the model. For classes direct and ebp the 
#' (untranformed) sample data is returned. For class fh the combined data set is returned.
#' @details The generic function \code{getData} is imported from package nlme and 
#' re-exported to make the S3-methods available, even though the nlme package 
#' itself is not loaded or attached. For default documentation, 
#' see \code{\link[nlme]{getData}}.
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
#' @name getData
#' @importFrom nlme getData
#' @export getData
getData

#' @export getData.direct
#' @export
#' @rdname getData
getData.direct <- function(object, ...) {
  throw_class_error(object, "direct")
  object$framework$smp_data
}

#' @export getData.ebp
#' @export 
#' @rdname getData
getData.ebp <- function(object, ...) {
  throw_class_error(object, "ebp")
  if (object$transformation != "no") {
    cat('The untransformed sample data set of the ebp object is returned. \n \n')
  }
  object$framework$smp_data
}

#' @export getData.fh
#' @export 
#' @rdname getData
getData.fh <- function(object, ...) {
  throw_class_error(object, "fh")
  cat('The combined data set (combined_data) of the fh object is returned. \n \n')
  object$framework$combined_data
}

#' Extract grouping factors from an emdi object
#'
#' Methods \code{getGroups.direct}, \code{getGroups.ebp} and \code{getGroups.fh} 
#' extract grouping factors from an emdi object.
#' 
#' @param object an object of type "emdi", depending on the method either "direct", 
#' "ebp" or "fh".
#' @param ... additional arguments that are not used in this method.
#' @return A vector containing the grouping factors.
#' @details The generic function \code{getGroups} is imported from package nlme and 
#' re-exported to make the S3-methods available, even though the nlme package 
#' itself is not loaded or attached. For default documentation, 
#' see \code{\link[nlme]{getGroups}}.
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
#' @name getGroups
#' @importFrom nlme getGroups
#' @export getGroups
getGroups

#' @export getGroups.direct
#' @export
#' @rdname getGroups
getGroups.direct <- function(object, ...) {
  throw_class_error(object, "direct")
  object$framework$smp_domains_vec
}

#' @export getGroups.ebp
#' @export
#' @rdname getGroups
getGroups.ebp <- function(object, ...) {
  throw_class_error(object, "ebp")
  object$framework$smp_domains_vec
}

#' @export getGroups.fh
#' @export
#' @rdname getGroups
getGroups.fh <- function(object, ...) {
  throw_class_error(object, "fh")
  object$ind$Domain
}


#' Extract grouping formula from an emdi object
#'
#' Methods \code{getGroupsFormula.direct}, \code{getGroupsFormula.ebp} and 
#' \code{getGroupsFormula.fh} extract the grouping formula from an 
#' emdi object.
#' 
#' @param object an object of type "emdi", depending on the method either "direct", 
#' "ebp" or "fh".
#' @param ... additional arguments that are not used in this method.
#' @return A one-sided formula.
#' @details The generic function \code{getGroupsFormula} is imported from package nlme and 
#' re-exported to make the S3-methods available, even though the nlme package 
#' itself is not loaded or attached. For default documentation, 
#' see \code{\link[nlme]{getGroupsFormula}}.
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
#' @name getGroupsFormula
#' @importFrom nlme getGroupsFormula
#' @export getGroupsFormula
getGroupsFormula

#' @export getGroupsFormula.direct
#' @export
#' @rdname getGroupsFormula
getGroupsFormula.direct <- function(object, ...) {
  throw_class_error(object, "direct")
  eval(parse(text = paste("~", object$framework$smp_domains)))
}

#' @export getGroupsFormula.ebp
#' @export
#' @rdname getGroupsFormula
getGroupsFormula.ebp <- function(object, ...) {
  throw_class_error(object, "ebp")
  eval(parse(text = paste("~", object$framework$smp_domains)))
}

#' @export getGroupsFormula.fh
#' @export
#' @rdname getGroupsFormula
getGroupsFormula.fh <- function(object, ...) {
  throw_class_error(object, "fh")
  eval(parse(text = paste("~", object$framework$domains)))
}

#' Extract response variable from an emdi object
#'
#' Methods \code{getResponse.direct}, \code{getResponse.ebp} and \code{getResponse.fh} 
#' extract the response variable from an emdi object.
#' 
#' @param object an object of type "emdi", depending on the method either "direct", 
#' "ebp" or "fh".
#' @param ... additional arguments that are not used in this method.
#' @return Vector containing the response variable.
#' @details The generic function \code{getResponse} is imported from package nlme and 
#' re-exported to make the S3-methods available, even though the nlme package 
#' itself is not loaded or attached. For default documentation, 
#' see \code{\link[nlme]{getResponse}}. 
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
#' @name getResponse
#' @importFrom nlme getResponse
#' @export getResponse
getResponse

#' @export getResponse.direct
#' @export
#' @rdname getResponse
getResponse.direct <- function(object, ...) {
  throw_class_error(object, "direct")  
  object$framework$smp_data[, object$call$y]
  
}

#' @export getResponse.ebp
#' @export
#' @rdname getResponse
getResponse.ebp <- function(object, ...) {
  throw_class_error(object, "ebp")
  makeXY(object$fixed, object$framework$smp_data)$y
}

#' @export getResponse.fh
#' @export
#' @rdname getResponse
getResponse.fh <- function(object, ...) {
  throw_class_error(object, "fh")
  object$framework$direct
}


#' Extract variance-covariance matrix from an emdi object
#'
#' Methods \code{getVarCov.ebp} and \code{getVarCov.fh} extract the 
#' variance-covariance matrix from a fitted model of class ebp or fh.
# 
#' @param obj an object of type "emdi", either "ebp" or "fh".
#' @param individuals vector of levels of the in-sample domains can be specified 
#' for the types "\code{conditional}" or "\code{marginal}".
#' @param type a character that determines the type of variance-covariance matrix. 
#' Types that can be chosen
#' (i) random-effects variance-covariance matrix ("\code{random.effects}"),
#' (ii) conditional variance-covariance matrix ("\code{conditional}"), 
#' (iii) marginal variance-covariance matrix ("\code{marginal}"). Defaults to 
#' "\code{random.effects}".
#' @param ... additional arguments that are not used in this method.
#' @return A variance-covariance matrix or a list of variance-covariance matrices, 
#' if more than one individual is selected. For method \code{getVarCov.ebp}, the 
#' dimensions of the matrices are 1 x 1 for type "\code{random.effects}" and 
#' number of in-sample domains x number of in-sample domains for types 
#' "\code{conditional}" and "\code{marginal}". For method \code{getVarCov.fh}, 
#' for all types the dimensions of the matrices are 1 x 1. For type "\code{marginal}" 
#' the diagonal elements of the variance covariances matrices are returned for the chosen 
#' individual. Please note, if the correlation argument of the fh object is set to 
#' spatial, the variance covariance matrix has non-zero off-diagonal elements, 
#' because the assumption of independency of the error terms does not hold. For 
#' the non-spatial models the off-diagonal elements are zero. 
#' @details The generic function \code{getVarCov} is imported from package nlme and 
#' re-exported to make the S3-methods available, even though the nlme package 
#' itself is not loaded or attached. For default documentation, 
#' see \code{\link[nlme]{getVarCov}}. 
#' @seealso \code{\link{ebp}}, \code{\link{fh}}, \code{\link[nlme]{getVarCov}}
#' @examples
#' \donttest{
#' # Example for class fh
#' combined_data <- combine_data(pop_data = eusilcA_popAgg, pop_domains = "Domain",
#'                              smp_data = eusilcA_smpAgg, smp_domains = "Domain")
#'
#' fh_std <- fh(fixed = Mean ~ cash + self_empl, vardir = "Var_Mean",
#'              combined_data = combined_data, domains = "Domain", method = "ml", 
#'              MSE = TRUE)
#' 
#' getVarCov(fh_std)
#' }
#' @name getVarCov
#' @importFrom nlme getVarCov
#' @export getVarCov
getVarCov

#' @export getVarCov.ebp
#' @export
#' @rdname getVarCov

getVarCov.ebp <- function(obj, individuals, type = "random.effects", ...) {
  throw_class_error(obj, "ebp")
  
  if (is.null(type) || !(type == "random.effects" 
                         || type == "conditional" 
                         || type == "marginal")) {
    stop("The three options for type are ''random.effects'', ''conditional'' 
         or ''marginal''.")
  }
  
  getVarCov(obj$model, individuals = individuals, type = type)
  
}

#' @export getVarCov.fh
#' @export
#' @rdname getVarCov

getVarCov.fh <- function(obj, individuals = 1, type = "random.effects", ...) {
  throw_class_error(obj, "fh")
  
  if (is.null(type) || !(type == "random.effects" 
                         || type == "conditional" 
                         || type == "marginal")) {
    stop("The three options for type are ''random.effects'', ''conditional'' 
         or ''marginal''.")
  }
  
  if (type == "random.effects"){
    if (obj$model$correlation == "spatial"){
      result <- list(varmat = matrix(obj$model$variance$variance, nrow = 1, ncol = 1, 
                                     dimnames = list(c("(Intercept)"), c("(Intercept)"))),
                     std.dev = sqrt(obj$model$variance$variance))
    } else {
      result <- list(varmat = matrix(obj$model$variance, nrow = 1, ncol = 1, 
                                     dimnames = list(c("(Intercept)"), c("(Intercept)"))),
                     std.dev = sqrt(obj$model$variance))
    }
    
    class(result) <- c("getVarCov.fh", "VarCov_random")
  } else {
    result <- list()
    
    for(i in individuals){
      
      if (!(is.numeric(i) || is.character(i))) {
        stop("individuals must be numeric or a character specifying a level of a 
         in-sample domain.")
      }
      if (is.numeric(i)) {
        i <- as.character(obj$ind$Domain[obj$ind$Out == 0][i])
      }
      if (!(i %in% obj$ind$Domain[obj$ind$Out == 0])) {
        stop(paste0("No variance-covariance matrix is available. Individual '",
                    i, "' is not contained in the sample and therefore not used for the model fitting."))
      }
      
      if (type == "conditional"){
        D <- diag(1, obj$framework$N_dom_smp)
        
        if (obj$method$method == "me"){
          Beta.hat.tCiBeta.hat <- NULL
          for(j in seq_len(obj$framework$N_dom_smp)){
            Beta.hat.tCiBeta.hat[j] <- 
              t(obj$model$coefficients$coefficients)%*%obj$framework$Ci[,,j]%*%obj$model$coefficients$coefficients
          }
          V <- matrix(diag(obj$framework$vardir) + diag(as.numeric(Beta.hat.tCiBeta.hat)), 
                      nrow = obj$framework$N_dom_smp, ncol = obj$framework$N_dom_smp,
                      dimnames = list(obj$ind$Domain[obj$ind$Out== 0], 
                                      obj$ind$Domain[obj$ind$Out== 0]))
          result[[as.character(i)]] <- list(varmat = matrix(V[i, i], 
                                                            nrow = 1, ncol = 1, dimnames = list(c("1"), c("1"))),
                                            std.dev = sqrt(V[i, i]),
                                            domain = i)
        } else {
          V <- matrix(diag(obj$framework$vardir), 
                      nrow = obj$framework$N_dom_smp, ncol = obj$framework$N_dom_smp,
                      dimnames = list(obj$ind$Domain[obj$ind$Out== 0], 
                                      obj$ind$Domain[obj$ind$Out== 0]))
          result[[as.character(i)]] <- list(varmat = matrix(V[i, i], 
                                                            nrow = 1, ncol = 1, dimnames = list(c("1"), c("1"))),
                                            std.dev = sqrt(V[i, i]),
                                            domain = i)
        }
        class(result) <- c("getVarCov.fh", "VarCov_conditional")
      } else if (type == "marginal"){
        D <- diag(1, obj$framework$N_dom_smp)
        
        if (obj$model$correlation == "spatial"){
          Wt <- t(obj$framework$W)
          A <- solve((D - obj$model$variance$correlation*Wt)%*%
                       (D - obj$model$variance$correlation*obj$framework$W))
          G <- obj$model$variance$variance*A
          # Total variance-covariance matrix 
          V <- matrix(G + D*obj$framework$vardir,
                      nrow = obj$framework$N_dom_smp, ncol = obj$framework$N_dom_smp,
                      dimnames = list(obj$ind$Domain[obj$ind$Out== 0], 
                                      obj$ind$Domain[obj$ind$Out== 0]))
          result[[as.character(i)]] <- list(varmat = matrix(V[i, i], 
                                                            nrow = 1, ncol = 1, 
                                                            dimnames = list(c("1"), c("1"))),
                                            std.dev = sqrt(V[i, i]),
                                            domain = i,
                                            correlation = "spatial")
        } else if (obj$method$method == "me") {
          Beta.hat.tCiBeta.hat <- NULL
          for(j in seq_len(obj$framework$N_dom_smp)){
            Beta.hat.tCiBeta.hat[j] <- 
              t(obj$model$coefficients$coefficients)%*%obj$framework$Ci[,,j]%*%obj$model$coefficients$coefficients
          }
          # Total variance-covariance matrix - only values on the diagonal due to
          # independence of error terms
          V <- matrix(obj$model$variance * D%*%t(D) + diag(obj$framework$vardir) + 
            diag(as.numeric(Beta.hat.tCiBeta.hat)), 
            nrow = obj$framework$N_dom_smp, ncol = obj$framework$N_dom_smp,
            dimnames = list(obj$ind$Domain[obj$ind$Out== 0], 
                            obj$ind$Domain[obj$ind$Out== 0]))
          result[[as.character(i)]] <- list(varmat = matrix(V[i, i], 
                                                            nrow = 1, ncol = 1, 
                                                            dimnames = list(c("1"), c("1"))),
                                            std.dev = sqrt(V[i, i]),
                                            domain = i,
                                            correlation = "no")
        } else {
          # Total variance-covariance matrix - only values on the diagonal due to
          # independence of error terms
          V <- matrix(obj$model$variance * D%*%t(D) + diag(as.numeric(obj$framework$vardir)),
                      nrow = obj$framework$N_dom_smp, ncol = obj$framework$N_dom_smp,
                      dimnames = list(obj$ind$Domain[obj$ind$Out== 0], 
                                      obj$ind$Domain[obj$ind$Out== 0]))
          result[[as.character(i)]] <- list(varmat = matrix(V[i, i], 
                                                            nrow = 1, ncol = 1, 
                                                            dimnames = list(c("1"), c("1"))),
                                            std.dev = sqrt(V[i, i]),
                                            domain = i,
                                            correlation = "no")
        }
        
        class(result) <- c("getVarCov.fh", "VarCov_marginal")
      }
    } 
  }
  result
}

#' @export
print.getVarCov.fh <- function(x, ...) {
  
  if(inherits(x, "VarCov_random")){
    cat("Random effects variance covariance matrix\n")
    print(x$varmat)
    cat("  Standard Deviations:", round(x$std.dev, 2),"\n")
  } else if(inherits(x, "VarCov_conditional")){
    for (i in names(x)){
      cat("domain", as.character(x[[i]]$domain), "\n")
      cat("Conditional variance covariance matrix\n")
      print(x[[i]]$varmat)
      cat("  Standard Deviations:", round(x[[i]]$std.dev, 2),"\n")
    }
  } else if(inherits(x, "VarCov_marginal")){
    for (i in names(x)){
      cat("domain", as.character(x[[i]]$domain), "\n")
      cat("Marginal variance covariance matrix\n")
      print(x[[i]]$varmat)
      cat("  Standard Deviations:", round(x[[i]]$std.dev, 2),"\n")
    }
    if (x[[1]]$correlation == "spatial"){
      cat("\n")
      cat('Please note, if the correlation argument of the fh object is set to 
spatial, the variance covariance matrix has non-zero off-diagonal elements, 
because the assumption of independency of the error terms does not hold. The diagonal 
elements are returned.')
    }
  }
}

#' Confidence intervals on coefficients of an emdi object
#'
#' Methods \code{intervals.ebp} and \code{intervals.fh} provide the approximate 
#' confidence intervals on the coefficients (fixed effects) of an emdi object.
#' 
#' @param object an object of type "emdi", depending on the method either "ebp" 
#' or "fh".
#' @param level an optional numeric value with the confidence level for the intervals. 
#' Defaults to 0.95.
#' @param parm vector of names to specify which parameters are to be given confidence 
#' intervals. If \code{NULL}, all parameters are taken into account. Defaults to \code{NULL}.
#' @param ... additional arguments that are not used in this method.
#' @return A matrix with rows corresponding to the parameters and columns 
#' containing the lower confidence limits (lower), the 
#' estimated values (est.), and upper confidence limits (upper).
#' @details The generic function \code{intervals} is imported from package nlme and 
#' re-exported to make the S3-methods available, even though the nlme package 
#' itself is not loaded or attached. For default documentation, 
#' see \code{\link[nlme]{intervals}}. 
#' @seealso \code{\link{direct}}, \code{\link{ebp}}, \code{\link{fh}}, \code{\link[nlme]{intervals}}
#' @examples
#' \donttest{
#' # Example for class ebp
#' emdi_model <- ebp(fixed = eqIncome ~ gender + eqsize + cash + self_empl + 
#' unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + fam_allow + 
#' house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop, 
#' pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district", 
#' na.rm = TRUE)
#' 
#' intervals(emdi_model)
#' }
#' @name intervals
#' @importFrom nlme intervals
#' @export intervals
intervals

#' @export intervals.ebp
#' @export
#' @rdname intervals 
intervals.ebp <- function(object, level = 0.95, parm = NULL, ...) {
  throw_class_error(object, "ebp")
  if (!is.null(parm)) {
    confidence_intervals <- intervals(object$model, level = level)$fixed
    subset(confidence_intervals, rownames(confidence_intervals) %in% parm)
  } else {
    intervals(object$model, level = level)$fixed
  }
  
}

#' @export intervals.fh
#' @export
#' @rdname intervals
intervals.fh <- function(object, level = 0.95, parm = NULL, ...) {
  throw_class_error(object, "fh")
  coefmat <- object$model$coefficients
  
  coefs <- coefmat[,1]
  stds <- coefmat[,2]
  dist <- qnorm(p = (1 - level) / 2, 0, stds)
  ret_value <- data.frame(lower = coefs + dist,
                          est. = coefs,         
                          upper = coefs + abs(dist),
                          row.names = row.names(coefmat))
  if (is.null(parm)) {
    as.matrix(ret_value)  
  } else {
    as.matrix(ret_value[parm, ])
  }
}

#' Extract random effects of emdi objects
#'
#' Methods \code{ranef.ebp} and \code{ranef.fh} extract the 
#' fixed effects from an emdi object of class "ebp" or "fh".
#'
#' @param object an object of type "emdi", depending on the used method either "ebp" or "fh".
#' @param ... additional arguments that are not used in this method.
#' @return A vector containing the estimated random effects at domain level is returned.
#' @details The alias \code{random.effects} can also be used instead of \code{ranef}. 
#' The generic function \code{ranef} is imported from package nlme and 
#' re-exported to make the S3-methods available, even though the nlme package 
#' itself is not loaded or attached. For default documentation, 
#' see \code{\link[nlme]{random.effects}}.
#' @seealso \code{\link{ebp}}, \code{\link{fh}}, \code{\link[nlme]{random.effects}}
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
#' @name ranef
#' @aliases random.effects
#' @importFrom nlme ranef random.effects
#' @export random.effects
#' @export ranef
ranef

#' @export ranef.ebp
#' @export
#' @rdname ranef
ranef.ebp <- function(object, ...) {
  throw_class_error(object, "ebp")
  ranef(object$model)
}

#' @export random.effects.ebp
#' @export
#' @rdname ranef
random.effects.ebp <- function(object, ...) {
  throw_class_error(object, "ebp")
  ranef(object$model)
}

#' @export ranef.fh
#' @export
#' @rdname ranef
ranef.fh <- function(object, ...) {
  throw_class_error(object, "fh")
  random_effects <- object$model$random_effects
  row.names(random_effects) <- object$ind$Domain[object$ind$Out == 0]
  random_effects
}

#' @export random.effects.fh
#' @export
#' @rdname ranef
random.effects.fh <- function(object, ...) {
  throw_class_error(object, "fh")
  random_effects <- object$model$random_effects
  row.names(random_effects) <- object$ind$Domain[object$ind$Out == 0]
  random_effects
}

