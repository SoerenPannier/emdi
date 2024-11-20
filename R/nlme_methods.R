#' Extract Fixed Effects from an emdi Object
#'
#' Methods \code{fixef.ebp}, \code{fixef.ebp_tf}, \code{fixef.fh} and
#' \code{fixef.fh_tf} extract the fixed effects from an emdi object of class
#' "ebp", "ebp_tf", "fh" or "fh_tf".
#'
#' @param object an object of type "emdi", depending on any of the used method:
#' "ebp", "ebp_tf", "fh" or "fh_tf".
#' @param ... additional arguments that are not used in this method.
#' @return For classes "ebp", "ebp_tf", "fh" and "fh_tf" a vector containing
#' the fixed effects is returned.
#' @details The alias \code{fixed.effects} can also be used instead of
#' \code{fixef}. The generic function \code{fixef} is imported from package
#' nlme and re-exported to make the S3-methods available, even though the nlme
#' package itself is not loaded or attached. For default documentation,
#' see \code{\link[nlme]{fixed.effects}}.
#' @seealso \code{\link{ebp}}, \code{\link{ebp_tf}}, \code{\link{fh}},
#' \code{\link{fh_tf}}, \code{\link[nlme]{fixed.effects}}
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

#____________________________ ebp_tf____________________________________________
#' @export fixef.ebp_tf
#' @export
#' @rdname fixef
fixef.ebp_tf <- function(object, ...) {
  throw_class_error(object, "ebp_tf")
  object$model$coefficients$fixed
}

#' @export fixed.effects.ebp_tf
#' @export
#' @rdname fixef
fixed.effects.ebp_tf <- function(object, ...) {
  throw_class_error(object, "ebp_tf")
  object$model$coefficients$fixed
}
#_______________________________________________________________________________

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

#' @export fixef.fh_tf
#' @export
#' @rdname fixef
fixef.fh_tf <- function(object, ...) {
  throw_class_error(object, "fh_tf")
  fixed_effects <- object$model$coefficients$coefficients
  names(fixed_effects) <- row.names(object$model$coefficients)
  fixed_effects
}

#' @export fixed.effects.fh_tf
#' @export
#' @rdname fixef
fixed.effects.fh_tf <- function(object, ...) {
  throw_class_error(object, "fh_tf")
  fixed_effects <- object$model$coefficients$coefficients
  names(fixed_effects) <- row.names(object$model$coefficients)
  fixed_effects
}

#' Extract emdi Object Data
#'
#' Methods \code{getData.direct}, \code{getData.ebp}, \code{getData.ebp_tf},
#' \code{getData.fh} and \code{getData.fh_tf} extract the data frame
#' used to fit the model.
#'
#' @param object an object of type "emdi", depending on any of the used method:
#' "ebp", "ebp_tf", "fh" or "fh_tf".
#' @param ... additional arguments that are not used in this method.
#' @return Data frame used to fit the model. For classes "direct", "ebp" and
#' "ebp_tf" the (untransformed) sample data is returned. For class "fh" and
#' "fh_tf" the combined data set is returned.
#' @details The generic function \code{getData} is imported from package nlme
#' and re-exported to make the S3-methods available, even though the nlme
#' package itself is not loaded or attached. For default documentation,
#' see \code{\link[nlme]{getData}}.
#' @seealso \code{\link{direct}}, \code{\link{ebp}}, \code{\link{ebp_tf}},
#' \code{\link{fh}}, \code{\link{fh_tf}}, \code{\link[nlme]{getData}}
#' @examples
#' \donttest{
#' # Example for class direct
#' emdi_direct <- direct(
#'   y = "eqIncome", smp_data = eusilcA_smp,
#'   smp_domains = "district", weights = "weight", threshold = 11064.82,
#'   var = TRUE, boot_type = "naive", B = 50, seed = 123, X_calib = NULL,
#'   totals = NULL, na.rm = TRUE
#' )
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
    message(strwrap(prefix = " ", initial = "",
                    "The untransformed sample data set of the ebp object
                    is returned."))
  }
  object$framework$smp_data
}
#__________________________________ebp_tf_______________________________________
#' @export getData.ebp_tf
#' @export
#' @rdname getData
getData.ebp_tf <- function(object, ...) {
  throw_class_error(object, "ebp_tf")
  if (object$transformation != "no") {
    message(strwrap(prefix = " ", initial = "",
                    "The untransformed sample data set of the ebp_tf object
                    is returned."))
  }
  object$framework$smp_data
}
#_______________________________________________________________________________
#' @export getData.fh
#' @export
#' @rdname getData
getData.fh <- function(object, ...) {
  throw_class_error(object, "fh")
  message(strwrap(prefix = " ", initial = "",
                  "The combined data set (combined_data) of the fh object
                  is returned."))
  object$framework$combined_data
}

#' @export getData.fh_tf
#' @export
#' @rdname getData
getData.fh_tf <- function(object, ...) {
  throw_class_error(object, "fh_tf")
  message(strwrap(prefix = " ", initial = "",
                  "The combined data set (combined_data) of the fh_tf object
                  is returned."))
  object$framework$orig_data
}
#' Extract Grouping Factors from an emdi Object
#'
#' Methods \code{getGroups.direct}, \code{getGroups.ebp}, \code{getGroups.ebp_tf},
#' \code{getGroups.fh} and \code{getGroups.fh_tf} extract grouping factors from
#' an emdi object.
#'
#' @param object an object of type "emdi", depending on any of the used method:
#' "ebp", "ebp_tf", "fh" or "fh_tf".
#' @param ... additional arguments that are not used in this method.
#' @return A vector containing the grouping factors.
#' @details The generic function \code{getGroups} is imported from package nlme
#' and re-exported to make the S3-methods available, even though the nlme
#' package itself is not loaded or attached. For default documentation,
#' see \code{\link[nlme]{getGroups}}.
#' @seealso \code{\link{direct}}, \code{\link{ebp}}, \code{\link{ebp_tf}},
#' \code{\link{fh}}, \code{\link{fh_tf}}, \code{\link[nlme]{getGroups}}
#' @examples
#' \donttest{
#' # Example for class direct
#' emdi_direct <- direct(
#'   y = "eqIncome", smp_data = eusilcA_smp,
#'   smp_domains = "district", weights = "weight", threshold = 11064.82,
#'   var = TRUE, boot_type = "naive", B = 50, seed = 123, X_calib = NULL,
#'   totals = NULL, na.rm = TRUE
#' )
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

#__________________________ebp_tf_______________________________________________
#' @export getGroups.ebp
#' @export
#' @rdname getGroups
getGroups.ebp_tf <- function(object, ...) {
  throw_class_error(object, "ebp_tf")
  cat("Domains: \n")
  if(is.factor(object$ind_Domain$Domain)){
    print(object$ind_Domain$Domain)
  } else (print(as.factor(object$ind_Domain$Domain)))

  cat("\n")
  cat("SubDomains: \n")
  if(is.factor(object$ind_Subdomain$Subdomain)){
    print(object$ind_Domain$Domain)
  } else (print(as.factor(object$ind_Subdomain$Subdomain)))
  cat("\n")
}
#_______________________________________________________________________________
#' @export getGroups.fh
#' @export
#' @rdname getGroups
getGroups.fh <- function(object, ...) {
  throw_class_error(object, "fh")
  object$ind$Domain
}

#' @export getGroups.fh_tf
#' @export
#' @rdname getGroups
getGroups.fh_tf <- function(object, ...) {
  throw_class_error(object, "fh_tf")
  cat("Domains: \n")
  if(is.factor(object$ind_Domain$Domain)){
    print(object$ind_Domain$Domain)
  } else (print(as.factor(object$ind_Domain$Domain)))

  cat("\n")
  cat("SubDomains: \n")
  if(is.factor(object$ind_Subdomain$Subdomain)){
    print(object$ind_Domain$Domain)
  } else (print(as.factor(object$ind_Subdomain$Subdomain)))
  cat("\n")
}

#' Extract Grouping Formula from an emdi Object
#'
#' Methods \code{getGroupsFormula.direct}, \code{getGroupsFormula.ebp},
#' \code{getGroupsFormula.ebp_tf}, \code{getGroupsFormula.fh} and
#' \code{getGroupsFormula.fh_tf} extract the grouping formula from an
#' emdi object.
#'
#' @param object an object of type "emdi", depending on any of the used method:
#' "ebp", "ebp_tf", "fh" or "fh_tf".
#' @param ... additional arguments that are not used in this method.
#' @return A one-sided formula.
#' @details The generic function \code{getGroupsFormula} is imported from
#' package nlme and re-exported to make the S3-methods available, even though
#' the nlme package itself is not loaded or attached. For default documentation,
#' see \code{\link[nlme]{getGroupsFormula}}.
#' @seealso \code{\link{direct}}, \code{\link{ebp}}, \code{\link{ebp_tf}},
#' \code{\link{fh}}, \code{\link{fh_tf}}, \code{\link[nlme]{getGroupsFormula}}
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
#_____________________________ebp_tf____________________________________________
#' @export getGroupsFormula.ebp_tf
#' @export
#' @rdname getGroupsFormula
getGroupsFormula.ebp_tf <- function(object, ...) {
  throw_class_error(object, "ebp_tf")
  #eval(parse(text = paste("~", object$framework$smp_domains)))
  eval(parse(text = paste("~", object$call$smp_domains, "+", object$call$smp_subdomains)))
}

#_______________________________________________________________________________
#' @export getGroupsFormula.fh
#' @export
#' @rdname getGroupsFormula
getGroupsFormula.fh <- function(object, ...) {
  throw_class_error(object, "fh")
  eval(parse(text = paste("~", object$framework$domains)))
}

#' @export getGroupsFormula.fh_tf
#' @export
#' @rdname getGroupsFormula
getGroupsFormula.fh_tf <- function(object, ...) {
  throw_class_error(object, "fh_tf")
  eval(parse(text = paste("~", object$call$domains, "+", object$call$subdomains)))
}

#' Extract Response Variable from an emdi Object
#'
#' Methods \code{getResponse.direct}, \code{getResponse.ebp},
#' \code{getResponse.ebp_tf}, \code{getResponse.fh} and
#' \code{getResponse.fh_tf} extract the response variable from an emdi object.
#'
#' @param object an object of type "emdi", depending on any of the used method:
#' "ebp", "ebp_tf", "fh" or "fh_tf".
#' @param ... additional arguments that are not used in this method.
#' @return Vector containing the response variable.
#' @details The generic function \code{getResponse} is imported from package
#' nlme and re-exported to make the S3-methods available, even though the nlme
#' package itself is not loaded or attached. For default documentation,
#' see \code{\link[nlme]{getResponse}}.
#' @seealso \code{\link{direct}}, \code{\link{ebp}}, \code{\link{ebp_tf}},
#' \code{\link{fh}}, \code{\link{fh_tf}}, \code{\link[nlme]{getResponse}}
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

#____________________________ebp_tf_____________________________________________
#' @export getResponse.ebp_tf
#' @export
#' @rdname getResponse
getResponse.ebp_tf <- function(object, ...) {
  throw_class_error(object, "ebp_tf")
  makeXY(object$fixed, object$framework$smp_data)$y
}
#_______________________________________________________________________________
#' @export getResponse.fh
#' @export
#' @rdname getResponse
getResponse.fh <- function(object, ...) {
  throw_class_error(object, "fh")
  object$framework$direct
}

#' @export getResponse.fh_tf
#' @export
#' @rdname getResponse
getResponse.fh_tf <- function(object, ...) {
  throw_class_error(object, "fh_tf")
  object$framework$y_ij # If transformation is used, it is given in transformed scale
}

#' Extract Variance-covariance Matrix from an emdi Object
#'
#' Methods \code{getVarCov.ebp}, \code{getVarCov.ebp_tf}, \code{getVarCov.fh}
#' and \code{getVarCov.fh_tf} extract the variance-covariance matrix from a
#' fitted model of class "ebp", "ebp_tf", "fh" or "fh_tf".
#' @param obj an object of type "emdi", depending on any of the used method:
#' "ebp", "ebp_tf", "fh" or "fh_tf".
#' @param individuals vector of levels of the in-sample domains can be
#' specified for the types "\code{conditional}" or "\code{marginal}".
#' @param type a character that determines the type of variance-covariance
#' matrix. Types that can be chosen
#' (i) random-effects variance-covariance matrix ("\code{random.effects}"),
#' (ii) conditional variance-covariance matrix ("\code{conditional}"),
#' (iii) marginal variance-covariance matrix ("\code{marginal}"). Defaults to
#' "\code{random.effects}".
#' @param ... additional arguments that are not used in this method.
#' @return A variance-covariance matrix or a list of variance-covariance
#' matrices, if more than one individual is selected. For method
#' \code{getVarCov.ebp}, the dimensions of the matrices are 1 x 1 for type
#' "\code{random.effects}" and number of in-sample domains x number of
#' in-sample domains for types "\code{conditional}" and "\code{marginal}".
#' For method \code{getVarCov.ebp_tf}, the dimensions of the matrices are
#' number of observations x number of observations in in-sample domains for all
#' types: "\code{random.effects}", "\code{conditional}" and "\code{marginal}".
#' For method \code{getVarCov.fh}, for all types the dimensions of the matrices are
#' 1 x 1. For type "\code{marginal}" the diagonal elements of the variance
#' covariances matrices are returned for the chosen individual. Please note, if
#' the correlation argument of the "fh" object is set to spatial, the variance
#' covariance matrix has non-zero off-diagonal elements, because the assumption
#' of independence of the error terms does not hold. For the non-spatial models,
#' the off-diagonal elements are zero. For method \code{getVarCov.fh_tf},
#' the dimensions of the matrices are number of in-sample subdomains x number
#' of in-sample subdomains for all types: "\code{random.effects}",
#' "\code{conditional}" and "\code{marginal}".
#' @details The generic function \code{getVarCov} is imported from package nlme
#' and re-exported to make the S3-methods available, even though the nlme
#' package itself is not loaded or attached. For default documentation,
#' see \code{\link[nlme]{getVarCov}}.
#' @seealso \code{\link{ebp}}, \code{\link{ebp_tf}},
#' \code{\link{fh}}, \code{\link{fh_tf}}, \code{\link[nlme]{getVarCov}}
#' @examples
#' \donttest{
#' # Example for class fh
#' combined_data <- combine_data(
#'   pop_data = eusilcA_popAgg,
#'   pop_domains = "Domain",
#'   smp_data = eusilcA_smpAgg,
#'   smp_domains = "Domain"
#' )
#'
#' fh_std <- fh(
#'   fixed = Mean ~ cash + self_empl, vardir = "Var_Mean",
#'   combined_data = combined_data, domains = "Domain",
#'   method = "ml", MSE = TRUE
#' )
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

getVarCov.ebp <- function(obj, individuals = 1, type = "random.effects", ...) {
  throw_class_error(obj, "ebp")

  if (is.null(type) || !(type == "random.effects" ||
    type == "conditional" ||
    type == "marginal")) {
    stop(strwrap(prefix = " ", initial = "",
                 "The three options for type are ''random.effects'',
                 ''conditional'' or ''marginal''."))
  }

  getVarCov(obj$model, individuals = individuals, type = type)
}
#___________________________ebp_tf______________________________________________
#' @export getVarCov.ebp_tf
#' @export
#' @rdname getVarCov

getVarCov.ebp_tf <- function(obj, individuals = 1, type = "random.effects", ...) {
  throw_class_error(obj, "ebp_tf")

  if (is.null(type) || !(type == "random.effects" ||
                         type == "conditional" ||
                         type == "marginal")) {
    stop(strwrap(prefix = " ", initial = "",
                 "The three options for type are ''random.effects'',
                 ''conditional'' or ''marginal''."))
  }
  # Estimated error variance
  sigmae2est <- obj$model$sigma^2
  # VarCorr(fit2) is the estimated random error variance
  sigmau2_1est <- as.numeric(nlme::VarCorr(obj$model)[2, 1])
  sigmau2_2est <- as.numeric(nlme::VarCorr(obj$model)[4, 1])

  result <- list()
  for (i in individuals) {
    if (!(is.numeric(i) || is.character(i))) {
      stop("individuals must be numeric or a character specifying a level of
             a in-sample subdomain.")
    }
    if (is.numeric(i)) {
      in_dom <- unique(obj$framework$smp_data[, obj$framework$smp_domains])

      if (!is.element(obj$ind_Domain$Domain[i], in_dom)) {
        stop(strwrap(prefix = " ", initial = "",
                     paste0("No variance-covariance matrix is available.
                            Individual '", i, "' is not contained in the sample
                            and therefore not used for the model fitting.")))
      } else if(is.element(obj$ind_Domain$Domain[i], in_dom)){
        i <- as.character(obj$ind_Domain$Domain[i])
      }}


      subdoms_i <- unique(obj$framework$smp_data[obj$framework$smp_data[[obj$call$smp_domains]] == i,
                                                   obj$framework$smp_subdomains])
      n_ik <- table(obj$framework$smp_data[[obj$call$smp_subdomains]])

      con_i <- diag(sigmae2est, n_ik[[subdoms_i[1]]])
      mar_i <- sigmau2_2est + con_i

      for(k in subdoms_i[2:length(subdoms_i)]){
        con_inext <- diag(sigmae2est, n_ik[[k]])
        mar_inext <- sigmau2_2est + con_inext
        con_i <- adiag(con_i, con_inext)
        mar_i <- adiag(mar_i, mar_inext)
      }
      mar_i <- mar_i + sigmau2_1est
      rand_i <- mar_i - con_i


    if(type == "random.effects"){
      result[[i]] <- list(V = rand_i, i = i, v_dom = sigmau2_1est,
                          v_sub = sigmau2_2est)
      class(result) <- c("getVarCov.ebp_tf", "VarCov_random")
    } else if(type == "marginal"){
      result[[i]] <- list(V = mar_i, i = i)
      class(result) <- c("getVarCov.ebp_tf", "VarCov_marginal")
    } else if (type == "conditional"){
      result[[i]] <- list(V = con_i, i = i)
      class(result) <- c("getVarCov.ebp_tf", "VarCov_conditional")
    }
  }
  result
}

#' @export
print.getVarCov.ebp_tf <- function(x, ...) {
  if (inherits(x, "VarCov_random")) {
    for (i in names(x)) {
      cat("domain", as.character(x[[i]]$i), "\n")
      cat("Random effects variance covariance\n")
      print(x[[i]]$V)
      cat("Domain level:", round(x[[i]]$v_dom, 4), "\t",
          "Subdomain level:", round(x[[i]]$v_sub, 4), "\n")
    }
  } else if (inherits(x, "VarCov_conditional")) {
    for (i in names(x)) {
      cat("domain", as.character(x[[i]]$i), "\n")
      cat("Conditional variance covariance matrix\n")
      print(x[[i]]$V)
    }
  } else if (inherits(x, "VarCov_marginal")) {
    for (i in names(x)) {
      cat("domain", as.character(x[[i]]$i), "\n")
      cat("Marginal variance covariance matrix\n")
      print(x[[i]]$V)
    }
  }
}

#_______________________________________________________________________________
#' @export getVarCov.fh
#' @export
#' @rdname getVarCov

getVarCov.fh <- function(obj, individuals = 1, type = "random.effects", ...) {
  throw_class_error(obj, "fh")

  if (is.null(type) || !(type == "random.effects" ||
    type == "conditional" ||
    type == "marginal")) {
    stop(strwrap(prefix = " ", initial = "",
                 "The three options for type are ''random.effects'',
                 ''conditional'' or ''marginal''."))
  }

  if (type == "random.effects") {
    if (obj$model$correlation == "spatial") {
      result <- list(
        varmat = matrix(obj$model$variance$variance,
          nrow = 1,
          ncol = 1,
          dimnames = list(
            c("(Intercept)"),
            c("(Intercept)")
          )
        ),
        std.dev = sqrt(obj$model$variance$variance)
      )
    } else {
      result <- list(
        varmat = matrix(obj$model$variance,
          nrow = 1, ncol = 1,
          dimnames = list(
            c("(Intercept)"),
            c("(Intercept)")
          )
        ),
        std.dev = sqrt(obj$model$variance)
      )
    }

    class(result) <- c("getVarCov.fh", "VarCov_random")
  } else {
    result <- list()

    for (i in individuals) {
      if (!(is.numeric(i) || is.character(i))) {
        stop("individuals must be numeric or a character specifying a level of
             a in-sample domain.")
      }
      if (is.numeric(i)) {
        i <- as.character(obj$ind$Domain[obj$ind$Out == 0][i])
      }
      if (!(i %in% obj$ind$Domain[obj$ind$Out == 0])) {
        stop(strwrap(prefix = " ", initial = "",
                     paste0("No variance-covariance matrix is available.
                            Individual '", i, "' is not contained in the sample
                            and therefore not used for the model fitting."
                            )))
      }

      if (type == "conditional") {
        D <- diag(1, obj$framework$N_dom_smp)

        if (obj$method$method == "me") {
          Beta.hat.tCiBeta.hat <- NULL
          for (j in seq_len(obj$framework$N_dom_smp)) {
            Beta.hat.tCiBeta.hat[j] <-
              t(obj$model$coefficients$coefficients) %*%
              obj$framework$Ci[, , j] %*% obj$model$coefficients$coefficients
          }
          V <- matrix(diag(obj$framework$vardir) +
            diag(as.numeric(Beta.hat.tCiBeta.hat)),
          nrow = obj$framework$N_dom_smp,
          ncol = obj$framework$N_dom_smp,
          dimnames = list(
            obj$ind$Domain[obj$ind$Out == 0],
            obj$ind$Domain[obj$ind$Out == 0]
          )
          )
          result[[as.character(i)]] <- list(
            varmat = matrix(V[i, i],
              nrow = 1,
              ncol = 1,
              dimnames =
                list(
                  c("1"),
                  c("1")
                )
            ),
            std.dev = sqrt(V[i, i]),
            domain = i
          )
        } else {
          V <- matrix(diag(obj$framework$vardir),
            nrow = obj$framework$N_dom_smp,
            ncol = obj$framework$N_dom_smp,
            dimnames = list(
              obj$ind$Domain[obj$ind$Out == 0],
              obj$ind$Domain[obj$ind$Out == 0]
            )
          )
          result[[as.character(i)]] <- list(
            varmat = matrix(V[i, i],
              nrow = 1,
              ncol = 1,
              dimnames =
                list(
                  c("1"),
                  c("1")
                )
            ),
            std.dev = sqrt(V[i, i]),
            domain = i
          )
        }
        class(result) <- c("getVarCov.fh", "VarCov_conditional")
      } else if (type == "marginal") {
        D <- diag(1, obj$framework$N_dom_smp)

        if (obj$model$correlation == "spatial") {
          Wt <- t(obj$framework$W)
          A <- solve((D - obj$model$variance$correlation * Wt) %*%
            (D - obj$model$variance$correlation * obj$framework$W))
          G <- obj$model$variance$variance * A
          # Total variance-covariance matrix
          V <- matrix(G + D * obj$framework$vardir,
            nrow = obj$framework$N_dom_smp,
            ncol = obj$framework$N_dom_smp,
            dimnames = list(
              obj$ind$Domain[obj$ind$Out == 0],
              obj$ind$Domain[obj$ind$Out == 0]
            )
          )
          result[[as.character(i)]] <- list(
            varmat = matrix(V[i, i],
              nrow = 1,
              ncol = 1,
              dimnames =
                list(
                  c("1"),
                  c("1")
                )
            ),
            std.dev = sqrt(V[i, i]),
            domain = i,
            correlation = "spatial"
          )
        } else if (obj$method$method == "me") {
          Beta.hat.tCiBeta.hat <- NULL
          for (j in seq_len(obj$framework$N_dom_smp)) {
            Beta.hat.tCiBeta.hat[j] <-
              t(obj$model$coefficients$coefficients) %*%
              obj$framework$Ci[, , j] %*% obj$model$coefficients$coefficients
          }
          # Total variance-covariance matrix - only values on the diagonal due
          # to independence of error terms
          V <- matrix(obj$model$variance * D %*% t(D) +
            diag(obj$framework$vardir) +
            diag(as.numeric(Beta.hat.tCiBeta.hat)),
          nrow = obj$framework$N_dom_smp, ncol = obj$framework$N_dom_smp,
          dimnames = list(
            obj$ind$Domain[obj$ind$Out == 0],
            obj$ind$Domain[obj$ind$Out == 0]
          )
          )
          result[[as.character(i)]] <- list(
            varmat = matrix(V[i, i],
              nrow = 1,
              ncol = 1,
              dimnames =
                list(
                  c("1"),
                  c("1")
                )
            ),
            std.dev = sqrt(V[i, i]),
            domain = i,
            correlation = "no"
          )
        } else {
          # Total variance-covariance matrix - only values on the diagonal due
          # to independence of error terms
          V <- matrix(obj$model$variance * D %*% t(D) +
            diag(as.numeric(obj$framework$vardir)),
          nrow = obj$framework$N_dom_smp,
          ncol = obj$framework$N_dom_smp,
          dimnames = list(
            obj$ind$Domain[obj$ind$Out == 0],
            obj$ind$Domain[obj$ind$Out == 0]
          )
          )
          result[[as.character(i)]] <- list(
            varmat = matrix(V[i, i],
              nrow = 1, ncol = 1,
              dimnames =
                list(
                  c("1"),
                  c("1")
                )
            ),
            std.dev = sqrt(V[i, i]),
            domain = i,
            correlation = "no"
          )
        }

        class(result) <- c("getVarCov.fh", "VarCov_marginal")
      }
    }
  }
  result
}

#' @export
print.getVarCov.fh <- function(x, ...) {
  if (inherits(x, "VarCov_random")) {
    cat("Random effects variance covariance matrix\n")
    print(x$varmat)
    cat("  Standard Deviations:", round(x$std.dev, 2), "\n")
  } else if (inherits(x, "VarCov_conditional")) {
    for (i in names(x)) {
      cat("domain", as.character(x[[i]]$domain), "\n")
      cat("Conditional variance covariance matrix\n")
      print(x[[i]]$varmat)
      cat("  Standard Deviations:", round(x[[i]]$std.dev, 2), "\n")
    }
  } else if (inherits(x, "VarCov_marginal")) {
    for (i in names(x)) {
      cat("domain", as.character(x[[i]]$domain), "\n")
      cat("Marginal variance covariance matrix\n")
      print(x[[i]]$varmat)
      cat("  Standard Deviations:", round(x[[i]]$std.dev, 2), "\n")
    }
    if (x[[1]]$correlation == "spatial") {
      cat("\n")
      cat(strwrap(prefix = " ", initial = "",
                  "Please note, if the correlation argument of the fh object is
                  set to spatial, the variance covariance matrix has non-zero
                  off-diagonal elements, because the assumption of independence
                  of the error terms does not hold. The diagonal elements are
                  returned."))
    }
  }
}

#' @export getVarCov.fh_tf
#' @export
#' @rdname getVarCov

getVarCov.fh_tf <- function(obj, individuals = 1, type = "random.effects", ...) {
  throw_class_error(obj, "fh_tf")

  if (is.null(type) || !(type == "random.effects" ||
                         type == "conditional" ||
                         type == "marginal")) {
    stop(strwrap(prefix = " ", initial = "",
                 "The three options for type are ''random.effects'',
                 ''conditional'' or ''marginal''."))
  }

  result <- list()
  for (i in individuals) {
    if (!(is.numeric(i) || is.character(i))) {
      stop("individuals must be numeric or a character specifying a level of
             a in-sample subdomain.")
    }
    if (is.numeric(i)) {
      in_dom <- unique(obj$framework$data[obj$framework$data$ObsSub == "yes",
                                          obj$call$domains])

      if (!is.element(obj$ind_Domain$Domain[i], in_dom)) {
        stop(strwrap(prefix = " ", initial = "",
                     paste0("No variance-covariance matrix is available.
                            Individual '", i, "' is not contained in the sample
                            and therefore not used for the model fitting.")))
      } else if(is.element(obj$ind_Domain$Domain[i], in_dom)){
        i <- as.character(obj$ind_Domain$Domain[i])
      }}

      n_i <- table(obj$framework$smp_data[[obj$call$domains]])[[i]]
      subdom_i <- unique(obj$framework$smp_data[obj$framework$smp_data[[obj$call$domains]] == i,
                                                 obj$call$subdomains])
      con_V_i <- diag(obj$framework$vare[subdom_i])
      dimnames(con_V_i) <- list(subdom_i, subdom_i)

      ran_V_i <- obj$model$variances[["Domain"]] + diag(rep(obj$model$variances[["Subdomain"]], n_i))

      mar_V_i <- con_V_i + ran_V_i
      dimnames(mar_V_i) <- list(subdom_i, subdom_i)

    if(type == "random.effects"){
      # For fh_tf random effects differs between domains
      result[[i]] <- list(V = ran_V_i, i = i,
                          v_dom = obj$model$variances[["Domain"]],
                          v_sub = obj$model$variances[["Subdomain"]])
      class(result) <- c("getVarCov.fh_tf", "VarCov_random")
    } else if(type == "marginal"){
      result[[i]] <- list(V = mar_V_i, i = i)
      class(result) <- c("getVarCov.fh_tf", "VarCov_marginal")
    } else if (type == "conditional"){
      result[[i]] <- list(V = con_V_i, i = i)
      class(result) <- c("getVarCov.fh_tf", "VarCov_conditional")
    }
  }

  result
}

#' @export
print.getVarCov.fh_tf <- function(x, ...) {
  if (inherits(x, "VarCov_random")) {
    for (i in names(x)) {
      cat("domain", as.character(x[[i]]$i), "\n")
      cat("Random effects variance covariance\n")
      print(x[[i]]$V)
      cat("Domain level:", round(x[[i]]$v_dom, 4), "\t",
          "Subdomain level:", round(x[[i]]$v_sub, 4), "\n")
    }
  } else if (inherits(x, "VarCov_conditional")) {
    for (i in names(x)) {
      cat("domain", as.character(x[[i]]$i), "\n")
      cat("Conditional variance covariance matrix\n")
      print(x[[i]]$V)
    }
  } else if (inherits(x, "VarCov_marginal")) {
    for (i in names(x)) {
      cat("domain", as.character(x[[i]]$i), "\n")
      cat("Marginal variance covariance matrix\n")
      print(x[[i]]$V)
    }
  }
}
#' Confidence Intervals on Coefficients of an emdi Object
#'
#' Methods \code{intervals.ebp}, \code{intervals.ebp_tf}, \code{intervals.fh}
#' and \code{intervals.fh_tf} provide the approximate confidence intervals on
#' the coefficients (fixed effects) of an emdi object.
#'
#' @param object an object of type "emdi", depending on any of the used method:
#' "ebp", "ebp_tf", "fh" or "fh_tf".
#' @param level an optional numeric value with the confidence level for the
#' intervals. Defaults to 0.95.
#' @param parm vector of names to specify which parameters are to be given
#' confidence intervals. If \code{NULL}, all parameters are taken into account.
#' Defaults to \code{NULL}.
#' @param ... additional arguments that are not used in this method.
#' @return A matrix with rows corresponding to the parameters and columns
#' containing the lower confidence limits (lower), the
#' estimated values (est.), and upper confidence limits (upper).
#' @details The generic function \code{intervals} is imported from package nlme
#' and re-exported to make the S3-methods available, even though the nlme
#' package itself is not loaded or attached. For default documentation,
#' see \code{\link[nlme]{intervals}}.
#' @seealso \code{\link{direct}}, \code{\link{ebp}}, \code{\link{ebp_tf}},
#' \code{\link{fh}}, \code{\link{fh_tf}}, \code{\link[nlme]{intervals}}
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
#____________________________________ebp_tf_____________________________________
#' @export intervals.ebp_tf
#' @export
#' @rdname intervals
intervals.ebp_tf <- function(object, level = 0.95, parm = NULL, ...) {
  throw_class_error(object, "ebp_tf")
  if (!is.null(parm)) {
    confidence_intervals <- intervals(object$model, level = level)$fixed
    subset(confidence_intervals, rownames(confidence_intervals) %in% parm)
  } else {
    intervals(object$model, level = level)$fixed
  }
}
#_______________________________________________________________________________
#' @export intervals.fh
#' @export
#' @rdname intervals
intervals.fh <- function(object, level = 0.95, parm = NULL, ...) {
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

#' @export intervals.fh_tf
#' @export
#' @rdname intervals
intervals.fh_tf <- function(object, level = 0.95, parm = NULL, ...) {
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

#' Extract Random Effects of emdi Objects
#'
#' Methods \code{ranef.ebp}, \code{ranef.ebp_tf},  \code{ranef.fh} and
#' \code{ranef.fh_tf} extract the fixed effects from an emdi object of class
#' "ebp", "ebp_tf", "fh" or "fh_tf".
#'
#' @param object an object of type "emdi", depending on any of the used method:
#' "ebp", "ebp_tf", "fh" or "fh_tf".
#' @param ... additional arguments that are not used in this method.
#' @return A vector containing the estimated random effects at domain level is
#' returned.
#' @details The alias \code{random.effects} can also be used instead of
#' \code{ranef}. The generic function \code{ranef} is imported from package
#' nlme and re-exported to make the S3-methods available, even though the nlme
#' package itself is not loaded or attached. For default documentation,
#' see \code{\link[nlme]{random.effects}}.
#' @seealso \code{\link{ebp}}, \code{\link{ebp_tf}}, \code{\link{fh}},
#' \code{\link{fh_tf}}, \code{\link[nlme]{random.effects}}
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
#_________________________________ebp_tf________________________________________
#' @export ranef.ebp_tf
#' @export
#' @rdname ranef
ranef.ebp_tf <- function(object, ...) {
  throw_class_error(object, "ebp_tf")
  ranef(object$model)
}

#' @export random.effects.ebp_tf
#' @export
#' @rdname ranef
random.effects.ebp_tf <- function(object, ...) {
  throw_class_error(object, "ebp_tf")
  ranef(object$model)
}
#_______________________________________________________________________________
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

#' @export ranef.fh_tf
#' @export
#' @rdname ranef
ranef.fh_tf <- function(object, ...) {
  throw_class_error(object, "fh_tf")
  cat(paste0("Level: ", object$call$domains), "\n")
  print(object$model$random_effects_Domain)
  cat("\n")
  cat(paste0("Level: ", object$call$subdomains), "\n")
  print(object$model$random_effects_Subdomain)
  cat("\n")
}

#' @export random.effects.fh_tf
#' @export
#' @rdname ranef
random.effects.fh_tf <- function(object, ...) {
  throw_class_error(object, "fh_tf")
  cat(paste0("Level: ", object$call$domains), "\n")
  print(object$model$random_effects_Domain)
  cat("\n")
  cat(paste0("Level: ", object$call$subdomains), "\n")
  print(object$model$random_effects_Subdomain)
  cat("\n")
}