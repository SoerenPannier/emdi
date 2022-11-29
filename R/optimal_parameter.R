optimal_parameter <- function(generic_opt,
                              fixed,
                              smp_data,
                              smp_domains,
                              transformation,
                              interval) {
  if (transformation != "no" &&
    transformation != "log") {
    # no lambda -> no estimation -> no optmimization

    if (transformation == "box.cox" && any(interval == "default")) {
      interval <- c(-1, 2)
    } else if (transformation == "dual" && any(interval == "default")) {
      interval <- c(0, 2)
    } else if (transformation == "log.shift" && any(interval == "default")) {
      # interval = c(min(smp_data[paste(fixed[2])]),
      # max(smp_data[paste(fixed[2])]))
      span <- range(smp_data[paste(fixed[2])])
      if ((span[1] + 1) <= 1) {
        lower <- abs(span[1]) + 1
      } else {
        lower <- 0
      }

      upper <- diff(span) / 2

      interval <- c(lower, upper)
    }

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

generic_opt <- function(lambda,
                        fixed,
                        smp_data,
                        smp_domains,
                        transformation) {


  # Definition of optimization function for finding the optimal lambda
  # Preperation to easily implement further methods here
  optimization <- if (TRUE) {
    reml(
      fixed = fixed,
      smp_data = smp_data,
      smp_domains = smp_domains,
      transformation = transformation,
      lambda = lambda
    )
  }
  return(optimization)
}



# REML method ------------------------------------------------------------------

reml <- function(fixed = fixed,
                 smp_data = smp_data,
                 smp_domains = smp_domains,
                 transformation = transformation,
                 lambda = lambda) {
  sd_transformed_data <- std_data_transformation(
    fixed = fixed,
    smp_data = smp_data,
    transformation =
      transformation,
    lambda = lambda
  )


  model_REML <- NULL
  try(model_REML <- lme(
    fixed = fixed,
    data = sd_transformed_data,
    random =
      as.formula(paste0(
        "~ 1 | as.factor(",
        smp_domains, ")"
      )),
    method = "REML",
    keep.data = FALSE
  ), silent = TRUE)
  if (is.null(model_REML)) {
    stop(strwrap(prefix = " ", initial = "",
                 "The likelihood does not converge. One reason could be that
                 the interval for the estimation of an optimal transformation
                 parameter is not appropriate. Try another interval. See also
                 help(ebp)."))
  } else {
    model_REML <- model_REML
  }


  log_likelihood <- -logLik(model_REML)

  return(log_likelihood)
}
