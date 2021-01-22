#' Compare predictions of emdi objects
#'
#' Method \code{comparePred.emdi} compares predictions of two emdi objects.
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

#' @export
#' @method getData direct
#' @importFrom nlme getData

getData.direct <- function(object, ...) {
  throw_class_error(object, "direct")
  object$framework$smp_data
}

#' @export
#' @method getData ebp
#' @importFrom nlme getData

getData.ebp <- function(object, ...) {
  throw_class_error(object, "ebp")
  if (object$transformation != "no"){
    cat('The untransformed sample data set of the ebp object is returned. \n \n')
  }
  object$framework$smp_data
}

#' @export
#' @method getData fh
#' @importFrom nlme getData

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
#' @return A variance-covariance matrix or a list of variance-covariance matrices. 
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
#' @return A variance-covariance matrix or a list of variance-covariance matrices. 
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
        i <- obj$ind$Domain[obj$ind$Out == 0][i]
      }
      if (!(i %in% obj$ind$Domain[obj$ind$Out == 0])) {
        stop(paste0("No variance-covariance matrix is available. Individual '",
                    i, "' is not contained in the sample and therefore not used for the model fitting."))
      }
      
      if (type == "conditional"){
        D <- diag(1, obj$framework$N_dom_smp)
        
        if (obj$method$method == "me"){
          Beta.hat.tCiBeta.hat <- NULL
          for(i in seq_len(obj$framework$N_dom_smp)){
            Beta.hat.tCiBeta.hat[i] <- 
              t(obj$model$coefficients$coefficients)%*%obj$framework$Ci[,,i]%*%obj$model$coefficients$coefficients
          }
          V <- diag(obj$framework$vardir) + diag(as.numeric(Beta.hat.tCiBeta.hat))
          result[[as.character(i)]] <- list(varmat = matrix(V[i, i], 
                                                            nrow = 1, ncol = 1, dimnames = list(c("1"), c("1"))),
                                            std.dev = sqrt(V[i, i]),
                                            domain = i)
        } else {
          result[[as.character(i)]] <- list(varmat = matrix(obj$framework$vardir[obj$ind$Domain == i], 
                                                            nrow = 1, ncol = 1, dimnames = list(c("1"), c("1"))),
                                            std.dev = sqrt(obj$framework$vardir[obj$ind$Domain == i]),
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
          for(i in seq_len(obj$framework$N_dom_smp)){
            Beta.hat.tCiBeta.hat[i] <- 
              t(obj$model$coefficients$coefficients)%*%obj$framework$Ci[,,i]%*%obj$model$coefficients$coefficients
          }
          # Total variance-covariance matrix - only values on the diagonal due to
          # independence of error terms
          V <- obj$model$variance * D%*%t(D) + diag(obj$framework$vardir) + 
            diag(as.numeric(Beta.hat.tCiBeta.hat))
          result[[as.character(i)]] <- list(varmat = matrix(V[i, i], 
                                                            nrow = 1, ncol = 1, 
                                                            dimnames = list(c("1"), c("1"))),
                                            std.dev = sqrt(V[i, i]),
                                            domain = i)
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
                                            domain = i)
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
  row.names(random_effects) <- object$ind$Domain
  random_effects
}

