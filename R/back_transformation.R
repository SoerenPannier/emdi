backtransformed <- function(framework, sigmau2, eblup, transformation,
                            backtransformation,
                            combined_data, method, vardir,
                            interval, B, MSE, mse_type) {

  # Conduct backtransformation
  backtransformed_estims <- if (transformation == "log") {
    log_bt(framework, sigmau2, combined_data,
      eblup = eblup,
      method, MSE, backtransformation
    )
  } else if (transformation == "arcsin") {
    arcsin_bt(
      sigmau2 = sigmau2, combined_data = combined_data,
      framework = framework, eblup = eblup, vardir = vardir,
      mse_type = mse_type,
      method = method, interval = interval, MSE = MSE,
      B = B, backtransformation = backtransformation
    )
  }

  # Create return data frame

  # Define return data frames with direct estimators
  # These are independent from the backtransformation
  eblup_data <- data.frame(Domain = combined_data[[framework$domains]])
  eblup_data$Direct <- NA
  eblup_data$Direct[framework$obs_dom] <- framework$direct_orig

  mse_data <- data.frame(Domain = combined_data[[framework$domains]])
  mse_data$Direct <- NA
  mse_data$Direct[framework$obs_dom] <- framework$vardir_orig

  # Add backtransformed FH estimates
  eblup_data$FH <- backtransformed_estims$point_backtransformed
  mse_data$FH <- backtransformed_estims$mse_backtransformed


  # Add indicator for out-of-sample domains
  eblup_data$Out <- 1
  eblup_data$Out[framework$obs_dom] <- 0
  if (MSE == TRUE) {
    mse_data$Out <- 1
    mse_data$Out[framework$obs_dom] <- 0
  }

  back_out <- list(
    eblup_data = eblup_data,
    mse_data = mse_data,
    MSE_method = backtransformed_estims$mse_method
  )

  return(back_out)
}


# Backtransformation for log transformatio

log_bt <- function(framework, sigmau2, combined_data, eblup = eblup,
                   method, MSE, backtransformation) {
  log_backtransformed <- if (backtransformation == "bc_crude") {
    bc_crude(
      framework = framework, sigmau2 = sigmau2,
      combined_data = combined_data,
      eblup = eblup, method = method, MSE = MSE
    )
  } else if (backtransformation == "bc_sm") {
    bc_sm(
      framework = framework, sigmau2 = sigmau2,
      combined_data = combined_data,
      eblup = eblup, method = method, MSE = MSE
    )
  }


  return(log_backtransformed)
}

bc_crude <- function(framework, sigmau2, combined_data, eblup = eblup,
                     method, MSE) {

  # Crude means that we do not add 0.5 * sigmau2 * (1 - gamma) but the MSE as
  # approximation of the term. This enables to get estimates also for
  # out-of-sample estimates. Following Rao 2003, Neves et al. 2013

  # Backtransformation point estimates
  estim_mse <- analytical_mse(
    framework = framework, sigmau2 = sigmau2,
    combined_data = combined_data,
    method = method
  )

  # Available for all domains - in- and out-of-sample
  point_backtransformed <- exp(eblup$eblup_data$FH + 0.5 *
    estim_mse$mse_data$FH)

  # Selection if MSE is returned or not
  if (MSE == TRUE) {
    # The MSE is backtransformed following Rao 2003, p. 133
    mse_backtransformed <- exp(eblup$eblup_data$FH + 0.5 *
      estim_mse$mse_data$FH)^2 * estim_mse$mse_data$FH
    mse_method <- estim_mse$MSE_method
  } else {
    mse_backtransformed <- NULL
    mse_method <- "no mse estimated"
  }

  bc_crude_out <- list(
    point_backtransformed = point_backtransformed,
    mse_backtransformed = mse_backtransformed,
    mse_method = mse_method
  )

  return(bc_crude_out)
}

bc_sm <- function(framework, sigmau2, combined_data, eblup = eblup,
                  method, MSE) {
  # Slud and Maiti show the correct backtransformation for the log case which
  # is adding 0.5 * sigmau2 * (1-gamma) to the point estimator on the
  # transformed scale and backtransform the term by the exponential
  point_backtransformed <- rep(NA, framework$M)
  point_backtransformed[framework$obs_dom == TRUE] <-
    exp(eblup$eblup_data$FH[framework$obs_dom == TRUE] +
      (0.5 * sigmau2 * (1 - eblup$gamma)))

  if (MSE == TRUE) {
    mse_backtransformed <- slud_maiti(
      framework = framework, sigmau2 = sigmau2,
      eblup = eblup,
      combined_data = combined_data
    )$FH
    mse_method <- "slud-maiti"
  } else {
    mse_backtransformed <- NULL
    mse_method <- "no mse estimated"
  }

  bc_sm_out <- list(
    point_backtransformed = point_backtransformed,
    mse_backtransformed = mse_backtransformed,
    mse_method = mse_method
  )

  return(bc_sm_out)
}

# Backtransformation for arcsin transformation
arcsin_bt <- function(sigmau2 = sigmau2, combined_data = combined_data,
                      framework = framework, eblup = eblup, vardir = vardir,
                      mse_type = mse_type,
                      method = method, interval = interval, MSE = MSE,
                      B = B, backtransformation = backtransformation) {
  point_backtransformed <- arcsin_point(
    framework, sigmau2, eblup,
    backtransformation
  )

  if (MSE == TRUE) {
    mse_backtransformed <- arcsin_mse(
      sigmau2 = sigmau2,
      combined_data = combined_data,
      framework = framework, eblup = eblup,
      vardir = vardir,
      eblup_corr = point_backtransformed,
      mse_type = mse_type,
      method = method, interval = interval,
      B = B,
      backtransformation = backtransformation
    )
    mse_method <- mse_backtransformed$mse_method
  } else {
    mse_backtransformed <- NULL
    mse_method <- "no mse estimated"
  }

  arcsin_bt_out <- list(
    point_backtransformed = point_backtransformed,
    mse_backtransformed = mse_backtransformed$mse_backtransformed,
    mse_method = mse_method
  )

  return(arcsin_bt_out)
}

arcsin_mse <- function(sigmau2 = sigmau2, combined_data = combined_data,
                       framework = framework, eblup = eblup, vardir = vardir,
                       eblup_corr = eblup_corr, mse_type = mse_type,
                       method = method, interval = interval,
                       B = B, backtransformation = backtransformation) {
  if (mse_type == "boot") {
    mse_backtransformed <- boot_arcsin_2(
      sigmau2 = sigmau2, combined_data = combined_data,
      framework = framework, eblup = eblup,
      eblup_corr = eblup_corr,
      method = method, interval = interval,
      B = B, backtransformation = backtransformation
    )
    mse_backtransformed <- mse_backtransformed[[2]]$MSE
    mse_method <- "bootstrap"
  } else if (mse_type == "jackknife" || mse_type == "weighted_jackknife") {
    transformation <- "arcsin"
    jack_mse <- wrapper_MSE(
      framework = framework, combined_data = combined_data,
      sigmau2 = sigmau2, vardir = vardir, eblup = eblup,
      transformation = transformation, method = method,
      interval = interval, mse_type = mse_type
    )

    mse_backtransformed <- rep(NA, framework$M)
    mse_backtransformed[framework$obs_dom] <- 2 *
      sin(eblup$eblup_data$FH[framework$obs_dom]) *
      cos(eblup$eblup_data$FH[framework$obs_dom]) *
      jack_mse$mse_data$FH[framework$obs_dom]

    mse_method <- jack_mse$MSE_method
  }

  arcsin_mse_out <- list(
    mse_backtransformed = mse_backtransformed,
    mse_method = mse_method
  )

  return(arcsin_mse_out)
}

arcsin_point <- function(framework, sigmau2, eblup, backtransformation) {
  point_backtransformed <- if (backtransformation == "naive") {
    arcsin_naive(eblup)
  } else if (backtransformation == "bc") {
    var <- rep(NA, framework$M)
    var[framework$obs_dom] <- sigmau2 * (1 - eblup$gamma)
    mu <- eblup$eblup_data$FH

    arcsin_bc(framework, mu, var)
  }

  return(point_backtransformed)
}

arcsin_naive <- function(eblup) {
  point_backtransformed <- eblup$eblup_data$FH

  # Truncate the estimates on the transformed scale
  point_backtransformed[point_backtransformed < 0] <- 0
  point_backtransformed[point_backtransformed > (pi / 2)] <- (pi / 2)

  # Naively backtransform with the inverse of the transformation
  point_backtransformed <- (sin(point_backtransformed))^2

  return(point_backtransformed)
}

arcsin_bc <- function(framework, mu, var) {

  # Use integral to solve the formula in Slud and Maiti
  int_value <- NULL

  # Can this be vectorized?
  for (i in seq_len(framework$M)) {
    if (framework$obs_dom[i] == TRUE) {

      # Parameters for integration
      mu_dri <- mu[i]
      var_dri <- as.numeric(var[i])

      int_value <- c(int_value, integrate(integrand,
        lower = 0,
        upper = pi / 2,
        mu_dri,
        sqrt(var_dri)
      )$value)
    } else {
      # Naive backtransformation for out-of-sample domains
      int_value <- c(int_value, (sin(mu[i]))^2)
    }
  }

  return(int_value)
}


integrand <- function(x, mean, sd) {
  sin(x)^2 * dnorm(x, mean = mean, sd = sd)
}
