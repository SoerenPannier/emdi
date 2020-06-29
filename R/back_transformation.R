backtransformed <- function(framework, sigmau2, eblup, transformation,
                            backtransformation,
                            combined_data, method, vardir,
                            interval, B, MSE, mse_type) {


  # Define return data frames with direct estimators
  # These are independent from the backtransformation
  EBLUP_data <- data.frame(Domain = combined_data[[framework$domains]])
  EBLUP_data$Direct <- NA
  EBLUP_data$Direct[framework$obs_dom == TRUE] <- framework$direct_orig

  MSE_data <- data.frame(Domain = combined_data[[framework$domains]])
  MSE_data$Direct <- NA
  MSE_data$Direct[framework$obs_dom == TRUE] <- framework$vardir_orig

  if (transformation == "log") {
    if (backtransformation == "bc_crude") {
      estim_MSE <- analytical_mse(framework = framework, sigmau2 = sigmau2,
                                  combined_data = combined_data,
                                  method = method)

      # Crude means that we do not add 0.5 * sigmau2 * (1 - gamma) but the MSE as approximation
      # of the term. This enables to get estimates also for out-of-sample estimates.
      # Following Rao 2003, p. 133
      EBLUP_data$FH <- exp(eblup$EBLUP_data$FH + 0.5 * estim_MSE$MSE_data$FH)

      if (MSE == TRUE) {
        # The MSE is backtransformed following Rao 2003, p. 133
        MSE_data$FH <- exp(eblup$EBLUP_data$FH)^2 * estim_MSE$MSE_data$FH
        MSE_method <- estim_MSE$MSE_method
      } else {
        MSE_data <- NULL
        MSE_method <- "no mse estimated"
      }

    } else if (backtransformation == "bc_sm") {
      estim_MSE <- analytical_mse(framework = framework, sigmau2 = sigmau2,
                                  combined_data = combined_data,
                                  method = method)

      # Slud and Maiti show the correct backtransformation for the log case which
      # is adding 0.5 * sigmau2 * (1-gamma) to the point estimator on the transformed
      # scale and backtransform the term by the exponential
      EBLUP_data$FH[framework$obs_dom == TRUE] <- exp(eblup$EBLUP_data$FH[framework$obs_dom == TRUE] + (0.5 * sigmau2 * (1 - eblup$gamma)))
      # If we want estimates for the out-of-sample domain, we need to use the crude
      # estimation
      #EBLUP_data$EBLUP[framework$obs_dom == FALSE] <- exp(eblup$EBLUP_data$EBLUP[framework$obs_dom == FALSE] + 0.5 * estim_MSE$MSE_data$MSE[framework$obs_dom == FALSE])
      EBLUP_data$FH[framework$obs_dom == FALSE] <- NA


      if (MSE == TRUE) {
        SM_MSE <- slud_maiti(framework = framework, sigmau2 = sigmau2,
                             eblup = eblup, combined_data = combined_data)
        MSE_data$FH[framework$obs_dom == TRUE] <- SM_MSE$FH[framework$obs_dom == TRUE]
        # MSE_data$MSE[framework$obs_dom == FALSE] <-  exp(eblup$EBLUP_data$EBLUP[framework$obs_dom == FALSE])^2 * estim_MSE$MSE_data$MSE[framework$obs_dom == FALSE]
       # MSE_data$FHE[framework$obs_dom == FALSE] <- NA
        MSE_method <- "slud-maiti"
      } else {
        MSE_data <- NULL
        MSE_method <- "no mse estimated"
      }
    }
  } else if (transformation == "arcsin") {
    if (backtransformation == "naive") {
      EBLUP_data$FH <- eblup$EBLUP_data$FH

      EBLUP_data$FH[EBLUP_data$FH < 0] <- 0
      EBLUP_data$FH[EBLUP_data$FH > (pi / 2)] <- (pi / 2)

      EBLUP_data$FH <- (sin(EBLUP_data$FH))^2


      if (MSE == TRUE) {
        if (mse_type == "boot") {
          tmp_out <- boot_arcsin_2(sigmau2 = sigmau2, combined_data = combined_data,
                                   framework = framework, eblup = eblup,
                                   eblup_corr = EBLUP_data$FH,
                                   method = method, interval = interval,
                                   B = B, backtransformation = backtransformation)
          MSE_boot <- tmp_out[[2]]
          MSE_data$FH <- MSE_boot$MSE
          MSE_method <- "bootstrap"

        } else if (mse_type == "jackknife" | mse_type == "weighted_jackknife") {

          if (mse_type  == "jackknife") {
            jack_mse <- wrapper_MSE(framework = framework, combined_data = combined_data,
                                    sigmau2 = sigmau2, vardir = vardir, eblup = eblup,
                                    transformation = transformation, method = method,
                                    interval = interval, mse_type = mse_type)
          } else if (mse_type == "weighted_jackknife") {
            jack_mse <- wrapper_MSE(framework = framework, combined_data = combined_data,
                                    sigmau2 = sigmau2, vardir = vardir, eblup = eblup,
                                    transformation = transformation, method = method,
                                    interval = interval, mse_type = mse_type)
          }


          # Following which paper??
          back_jack_mse <- 2 * sin(eblup$EBLUP_data$FH[eblup$EBLUP_data$Out == 0]) * cos(eblup$EBLUP_data$FH[eblup$EBLUP_data$Out == 0]) * jack_mse$MSE_data$FH[jack_mse$MSE_data$Out == 0]

          MSE_data$FH[framework$obs_dom == TRUE] <- back_jack_mse
          MSE_data$FH[framework$obs_dom == FALSE] <- NA
          MSE_method <- jack_mse$MSE_method
        }
      } else {
        MSE_data <- NULL
        MSE_method <- "no mse estimated"
      }

    } else if (backtransformation == "bc") {

      # Use integral to solve the formula in Slud and Maiti
      int_value <- NULL
      
      for (i in seq_len(framework$m)) {

        mu_dri <- eblup$EBLUP_data$FH[eblup$EBLUP_data$Out == 0]
        mu_dri <- mu_dri[i]

        Var_dri <- sigmau2 * (1 - eblup$gamma)
        Var_dri <- as.numeric(Var_dri[i])

        integrand <- function(x, mean, sd){sin(x)^2 * dnorm(x, mean = mu_dri,
                                                            sd = sqrt(Var_dri))}

        upper_bound <- min(mean(framework$direct) + 10 * sd(framework$direct),
                           mu_dri + 100 * sqrt(Var_dri))
        lower_bound <- max(mean(framework$direct) - 10 * sd(framework$direct),
                           mu_dri - 100 * sqrt(Var_dri))

        int_value <- c(int_value, integrate(integrand,
                                            lower = 0,
                                            upper = pi/2)$value)
      }

      EBLUP_data$FH[eblup$EBLUP_data$Out == 0] <- int_value
      EBLUP_data$FH[eblup$EBLUP_data$Out == 1] <- (sin(eblup$EBLUP_data$FH[eblup$EBLUP_data$Out == 1]))^2


      if (MSE == TRUE) {
        if (mse_type == "boot") {
          tmp_out <- boot_arcsin_2(sigmau2 = sigmau2, combined_data = combined_data,
                                   framework = framework, eblup = eblup,
                                   eblup_corr = EBLUP_data$FH,
                                   method = method, interval = interval,
                                   B = B, backtransformation = backtransformation)
          MSE_boot <- tmp_out[[2]]

          MSE_data$FH <- MSE_boot$MSE
          MSE_method <- "bootstrap"

        } else if (mse_type == "jackknife" | mse_type == "weighted_jackknife") {

          if (mse_type == "jackknife") {
            jack_mse <- wrapper_MSE(framework = framework, combined_data = combined_data,
                                    sigmau2 = sigmau2, vardir = vardir, eblup = eblup,
                                    transformation = transformation, method = method,
                                    interval = interval, mse_type = mse_type)
          } else if (mse_type == "weighted_jackknife") {
            jack_mse <- wrapper_MSE(framework = framework, combined_data = combined_data,
                                    sigmau2 = sigmau2, vardir = vardir, eblup = eblup,
                                    transformation = transformation, method = method,
                                    interval = interval, mse_type = mse_type)
          }


          # Following which paper??
          back_jack_mse <- 2 * sin(eblup$EBLUP_data$FH[eblup$EBLUP_data$Out == 0]) * cos(eblup$EBLUP_data$FH[eblup$EBLUP_data$Out == 0]) * jack_mse$MSE_data$FH[jack_mse$MSE_data$Out == 0]

          MSE_data$FH[framework$obs_dom == TRUE] <- back_jack_mse
          MSE_data$FH[framework$obs_dom == FALSE] <- NA
          MSE_method <- jack_mse$MSE_method
        }

      } else {
        MSE_data <- NULL
        MSE_method <- "no mse estimated"
      }

      }

  }


  EBLUP_data$Out[framework$obs_dom == TRUE] <- 0
  EBLUP_data$Out[framework$obs_dom == FALSE] <- 1
  if (MSE == TRUE) {
    MSE_data$Out[framework$obs_dom == TRUE] <- 0
    MSE_data$Out[framework$obs_dom == FALSE] <- 1
  }



  back_out <- list(EBLUP_data = EBLUP_data,
                   MSE_data = MSE_data, MSE_method = MSE_method)

  return(back_out)
  }
