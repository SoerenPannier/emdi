# #' Estimates optimal parameter for Box-Cox transformations
# #'
# #' Function \code{optimal_parameter} estimates the optimal parameter lambda
# #' that is neccessary to determine Box-Cox transformations.
# #'
# #' @param generic_opt a wrapper function that conducts the selected optimization
# #' \code{method} in order to specify the optimal lambda for Box-Cox
# #' transformations.
# #' @param fixed a two-sided linear formula object describing the
# #' fixed-effects part of the nested error linear regression model with the
# #' dependent variable on the left of a ~ operator and the explanatory
# #' variables on the right, separated by + operators. The argument corresponds
# #' to the argument \code{fixed} in function \code{\link[nlme]{lme}}.
# #' @param smp_data a data frame that needs to comprise all variables named in
# #' \code{fixed} and \code{smp_domains}.
# #' @param smp_domains a character string containing the name of a variable
# #' that indicates domains in the sample data. The variable can be numeric or a
# #' factor.
# #' @param transformation a character string. Three different transformation
# #' types for the dependent variable can be chosen (i) no transformation ("no");
# #' (ii) log transformation ("log"); (iii) Box-Cox transformation ("box.cox").
# #' @param interval a numeric vector containing a lower and upper limit
# #' determining an interval for the estimation of the optimal parameter. Defaults
# #' to c(-1,2).
# #' @return a scalar returning the optimal lambda.
# #' @examples
# #' # Loading data
# #' data("Xoutsamp_AuxVar")
# #' data("incomedata")
# #'
# #' opt_par <- optimal_parameter(generic_opt, income~educ1, incomedata,
# #' "provlab", "box.cox")
# #' @export
# #' @import nlme
# #' @importFrom FNN KL.divergence
# #' @importFrom moments skewness

optimal_parameter <- function(generic_opt,
                              fixed,
                              smp_data,
                              smp_domains,
                              transformation,
                              interval=c(-1,2)) {

  if(transformation != "no" &&
     transformation != "log") {
    # no lambda -> no estimation -> no optmimization

    # Estimation of optimal lambda parameters
    optimal_parameter <- optimize(generic_opt,
                                  fixed          = fixed,
                                  smp_data       = smp_data,
                                  smp_domains    = smp_domains,
                                  transformation = transformation,
                                  interval       = interval,
                                  maximum        = FALSE
                                  )$minimum

  } else {
    optimal_parameter <- NULL
  }

  return(optimal_parameter)
} # End optimal parameter


# Internal documentation -------------------------------------------------------

# Function generic_opt provides estimation method reml to specifiy
# the optimal parameter lambda. Here its important that lambda is the
# first argument because generic_opt is given to optimize. Otherwise,
# lambda is missing without default.


# External documentation -------------------------------------------------------

# #' Selects estimation method for optimal parameter in Box-Cox transformations
# #'
# #' Function \code{generic_opt} selects the estimation method reml for the optimal
# #' lambda that is neccessary to conduct Box-Cox transformations.
# #'
# #' @param lambda a number that determines the Box-Cox transformation.
# #' @param fixed a two-sided linear formula object describing the
# #' fixed-effects part of the nested error linear regression model with the
# #' dependent variable on the left of a ~ operator and the explanatory
# #' variables on the right, separated by + operators. The argument corresponds
# #' to the argument \code{fixed} in function \code{\link[nlme]{lme}}.
# #' @param smp_data a data frame that needs to comprise all variables named in
# #' \code{fixed} and \code{smp_domains}.
# #' @param smp_domains a character string containing the name of a variable
# #' that indicates domains in the sample data. The variable can be numeric or a
# #' factor.
# #' @param transformation a character string. Three different transformation
# #' types for the dependent variable can be chosen (i) no transformation ("no");
# #' (ii) log transformation ("log"); (iii) Box-Cox transformation ("box.cox").
# #' @return The return is a log likelihood.
# #' @examples
# #' log_likelihood <- generic_opt(0.0589, income~educ1, incomedata, "provlab",
# #' "box.cox")
# #' @export
# #' @import nlme
# #' @importFrom FNN KL.divergence
# #' @importFrom moments skewness


generic_opt <- function(lambda,
                        fixed,
                        smp_data,
                        smp_domains,
                        transformation
                        ) {


  #Definition of optimization function for finding the optimal lambda
  #Preperation to easily implement further methods here
  optimization <- if(T) {
        reml(fixed          = fixed,
             smp_data       = smp_data,
             smp_domains    = smp_domains,
             transformation = transformation,
             lambda         = lambda
             )
        }
      return(optimization)
}



# REML method ------------------------------------------------------------------

reml <- function(fixed          = fixed,
                 smp_data       = smp_data,
                 smp_domains    = smp_domains,
                 transformation = transformation,
                 lambda         = lambda
                 ) {

  sd_transformed_data <- std_data_transformation(fixed          = fixed,
                                                 smp_data       = smp_data,
                                                 transformation = transformation,
                                                 lambda         = lambda
                                                 )
  model_REML <- NULL
  try(model_REML <- lme(fixed     = fixed,
                        data      = sd_transformed_data,
                        random    = as.formula(paste0("~ 1 | as.factor(", smp_domains, ")")),
                        method    = "REML",
                        keep.data = FALSE), silent=TRUE)
  if(is.null(model_REML)){
    stop("For some lambda in the interval, the likelihood does not converge.
         Choose another interval. See also help(ebp).")
  } else {
    model_REML <- model_REML
  }
  

  log_likelihood <- -logLik(model_REML)

  return(log_likelihood)
}


