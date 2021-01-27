# Extract fixed effects from an emdi object ------------------------------------
#' @aliases fixed.effects
#' @export
#' @method fixef ebp
#' @importFrom nlme fixef fixed.effects

fixef.ebp <- function(object, ...) {
  throw_class_error(object, "ebp")
  object$model$coefficients$fixed
}

#' @aliases fixed.effects
#' @export
#' @method fixef fh
#' @importFrom nlme fixef fixed.effects

fixef.fh <- function(object, ...) {
  throw_class_error(object, "fh")
  fixed_effects <- object$model$coefficients$coefficients
  names(fixed_effects) <- row.names(object$model$coefficients)
  fixed_effects
}


# Extract emdi object data -----------------------------------------------------
#' @importFrom nlme getData
#' @export getData
getData

#' @export getData.direct
#' @export
getData.direct <- function(object, ...) {
  throw_class_error(object, "direct")
  object$framework$smp_data
}

#' @export getData.ebp
#' @export
getData.ebp <- function(object, ...) {
  throw_class_error(object, "ebp")
  if (object$transformation != "no") {
    cat('The untransformed sample data set of the ebp object is returned. \n \n')
  }
  object$framework$smp_data
}

#' @export getData.fh
#' @export 
getData.fh <- function(object, ...) {
  throw_class_error(object, "fh")
  cat('The combined data set (combined_data) of the fh object is returned. \n \n')
  object$framework$combined_data
}

# Extract grouping factors from an emdi object ---------------------------------

#' @export
#' @method getGroups direct
#' @importFrom nlme getGroups

getGroups.direct <- function(object, ...) {
  throw_class_error(object, "direct")
  object$framework$smp_domains_vec
}


#' @export
#' @method getGroups ebp
#' @importFrom nlme getGroups

getGroups.ebp <- function(object, ...) {
  throw_class_error(object, "ebp")
  object$framework$smp_domains_vec
}


#' @export
#' @method getGroups fh
#' @importFrom nlme getGroups

getGroups.fh <- function(object, ...) {
  throw_class_error(object, "fh")
  object$ind$Domain
}


# Extract grouping formula from an emdi object ---------------------------------

#' @export
#' @method getGroupsFormula direct
#' @importFrom nlme getGroupsFormula

getGroupsFormula.direct <- function(object, ...) {
  throw_class_error(object, "direct")
  eval(parse(text = paste("~", object$framework$smp_domains)))
}


#' @export
#' @method getGroupsFormula ebp
#' @importFrom nlme getGroupsFormula

getGroupsFormula.ebp <- function(object, ...) {
  throw_class_error(object, "ebp")
  eval(parse(text = paste("~", object$framework$smp_domains)))
}


#' @export
#' @method getGroupsFormula fh
#' @importFrom nlme getGroupsFormula

getGroupsFormula.fh <- function(object, ...) {
  throw_class_error(object, "fh")
  eval(parse(text = paste("~", object$framework$domains)))
}

# Extract response variable from an emdi object

#' @export
#' @method getResponse direct
#' @importFrom nlme getResponse

getResponse.direct <- function(object, ...) {
  throw_class_error(object, "direct")  
  object$framework$smp_data[, object$call$y]
  
}


#' @export
#' @method getResponse ebp
#' @importFrom nlme getResponse

getResponse.ebp <- function(object, ...) {
  throw_class_error(object, "ebp")
  makeXY(object$fixed, object$framework$smp_data)$y
}


#' @export
#' @method getResponse fh
#' @importFrom nlme getResponse

getResponse.fh <- function(object, ...) {
  throw_class_error(object, "fh")
  object$framework$direct
}

#' Extract variance-covariance matrix from a fitted model of class ebp
#'
#' Method \code{getVarCov.ebp} extracts the variance-covariance matrix from a fitted 
#' model of class ebp.
# 
#' @param obj an object of type "ebp".
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
#' if more than one individual is selected. The dimensions of the matrices are 
#' 1 x 1 for type "\code{random.effects}" and number of in-sample domains x number 
#' of in-sample domains for types "\code{conditional}" and "\code{marginal}".  
#' @seealso \code{\link{ebp}}, \code{\link[nlme]{getVarCov}}
#' @examples
#' \donttest{
#' # Example for class ebp
#' emdi_model <- ebp(fixed = eqIncome ~ gender + eqsize + cash + self_empl + 
#' unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + fam_allow + 
#' house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop, 
#' pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district", 
#' na.rm = TRUE)
#' 
#' getVarCov(emdi_model)
#' }
#' @export
#' @method getVarCov ebp
#' @importFrom nlme getVarCov

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

#' Extract variance-covariance matrix from a fitted model of class fh
#'
#' Method \code{getVarCov.fh} extracts the variance-covariance matrix from a fitted 
#' model of class fh.
# 
#' @param obj an object of type "fh".
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
#' if more than one individual is selected. For all types the dimensions of the 
#' matrices are 1 x 1. For type "\code{marginal}" the diagonal elements of the 
#' variance covariances matrices are returned for the chosen 
#' individual. Please note, if the correlation argument of the fh object is set to 
#' spatial, the variance covariance matrix has non-zero off-diagonal elements, 
#' because the assumption of independency of the error terms does not hold. For 
#' the non-spatial models the off-diagonal elements are zero.
#' @seealso \code{\link{fh}}, \code{\link[nlme]{getVarCov}}
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
#' @export
#' @method getVarCov fh
#' @importFrom nlme getVarCov

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

#' @export
#' @method intervals ebp
#' @importFrom nlme intervals

intervals.ebp <- function(object, level = 0.95, parm = NULL, ...) {
  throw_class_error(object, "ebp")
  if (!is.null(parm)) {
    confidence_intervals <- intervals(object$model, level = level)$fixed
    subset(confidence_intervals, rownames(confidence_intervals) %in% parm)
  } else {
    intervals(object$model, level = level)$fixed
  }
  
}

#' @export
#' @method intervals fh
#' @importFrom nlme intervals

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

# Extract random effects of emdi objects ---------------------------------------

#' @aliases random.effects
#' @export
#' @method ranef ebp
#' @importFrom nlme ranef random.effects

ranef.ebp <- function(object, ...) {
  throw_class_error(object, "ebp")
  ranef(object$model)
}


#' @aliases random.effects
#' @export
#' @method ranef fh
#' @importFrom nlme ranef random.effects

ranef.fh <- function(object, ...) {
  throw_class_error(object, "fh")
  random_effects <- object$model$random_effects
  row.names(random_effects) <- object$ind$Domain[object$ind$Out == 0]
  random_effects
}

