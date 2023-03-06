# Internal documentation -------------------------------------------------------

# MSE estimation - parametric bootstrap procedure

# Function parametric_bootstrap conducts the MSE estimation defined in function
# mse_estim (see below)
# The parametric boostrap approach can be find in Molina and Rao (2010) p. 376

parametric_bootstrap <- function(framework,
                                 point_estim,
                                 fixed,
                                 transformation,
                                 interval = c(-1, 2),
                                 L,
                                 B,
                                 boot_type,
                                 parallel_mode,
                                 cpus) {
  message("\r", "Bootstrap started                                            ")
  if (boot_type == "wild") {
    res_s <- residuals(point_estim$model)
    fitted_s <- fitted(point_estim$model, level = 1)
  } else {
    res_s <- NULL
    fitted_s <- NULL
  }

  start_time <- Sys.time()
  if (cpus > 1) {
    cpus <- min(cpus, parallel::detectCores())
    parallelMap::parallelStart(
      mode = parallel_mode,
      cpus = cpus, show.info = FALSE
    )

    if (parallel_mode == "socket") {
      parallel::clusterSetRNGStream()
    }
    parallelMap::parallelLibrary("nlme")
    mses <- simplify2array(parallelMap::parallelLapply(
      xs              = seq_len(B),
      fun             = mse_estim_wrapper,
      B               = B,
      framework       = framework,
      lambda          = point_estim$optimal_lambda,
      shift           = point_estim$shift_par,
      model_par       = point_estim$model_par,
      gen_model       = point_estim$gen_model,
      fixed           = fixed,
      transformation  = transformation,
      interval        = interval,
      L               = L,
      res_s           = res_s,
      fitted_s        = fitted_s,
      start_time      = start_time,
      boot_type       = boot_type
    ))
    parallelMap::parallelStop()
  } else {
    mses <- simplify2array(lapply(
      X = seq_len(B),
      FUN = mse_estim_wrapper,
      B = B,
      framework = framework,
      lambda = point_estim$optimal_lambda,
      shift = point_estim$shift_par,
      model_par = point_estim$model_par,
      gen_model = point_estim$gen_model,
      fixed = fixed,
      transformation = transformation,
      interval = interval,
      L = L,
      res_s = res_s,
      fitted_s = fitted_s,
      start_time = start_time,
      boot_type = boot_type
    ))
  }

  message("\r", "Bootstrap completed", "\n")
  if (.Platform$OS.type == "windows") {
    flush.console()
  }

  mses <- apply(mses, c(1, 2), mean)
  if(is.null(framework$aggregate_to_vec)){
    mses <- data.frame(Domain = unique(framework$pop_domains_vec), mses)
  }else{
    mses <- data.frame(Domain = unique(framework$aggregate_to_vec), mses)
  }

  return(mses)
}




# mse_estim (only internal) ----------------------------------------------------

# The mse_estim function defines all parameters and estimations which have to
# be replicated B times for the Parametric Bootstrap Approach.
# See Molina and Rao (2010) p. 376

mse_estim <- function(framework,
                      lambda,
                      shift,
                      model_par,
                      gen_model,
                      res_s,
                      fitted_s,
                      fixed,
                      transformation,
                      interval,
                      L,
                      boot_type) {



  # The function superpopulation returns an income vector and a temporary
  # variable that passes the random effect to generating bootstrap populations
  # in bootstrap_par.

  if (boot_type == "wild") {
    superpop <- superpopulation_wild(
      framework = framework,
      model_par = model_par,
      gen_model = gen_model,
      lambda = lambda,
      shift = shift,
      transformation = transformation,
      res_s = res_s,
      fitted_s = fitted_s
    )
  } else {
    superpop <- superpopulation(
      framework = framework,
      model_par = model_par,
      gen_model = gen_model,
      lambda = lambda,
      shift = shift,
      transformation = transformation
    )
  }
  pop_income_vector <- superpop$pop_income_vector

  if (inherits(framework$threshold, "function")) {
    framework$threshold <-
      framework$threshold(y = pop_income_vector)
  }

  if(is.null(framework$aggregate_to_vec) != TRUE){
    N_dom_pop_tmp <- framework$N_dom_pop_agg
    pop_domains_vec_tmp <- framework$aggregate_to_vec
    pop_weights_vec <- framework$pop_data[[framework$pop_weights]]
  } else {
    N_dom_pop_tmp <- framework$N_dom_pop
    pop_domains_vec_tmp <- framework$pop_domains_vec
    pop_weights_vec <- rep(1, nrow(framework$pop_data))
  }

  # True indicator values
  true_indicators <- matrix(
    nrow = N_dom_pop_tmp,
    data = unlist(lapply(framework$indicator_list,
      function(f, threshold) {
        matrix(
          nrow = N_dom_pop_tmp,
          data =
            unlist(mapply(
              y = split(pop_income_vector, pop_domains_vec_tmp),
              pop_weight = split(pop_weights_vec, pop_domains_vec_tmp),
              f,
              threshold = framework$threshold
            )),
          byrow = TRUE
        )
      },
      threshold = framework$threshold
    ))
  )

  colnames(true_indicators) <- framework$indicator_names

  # The function bootstrap_par returns a sample that can be given into the
  # point estimation to get predictors of the indicators that can be compared
  # to the "truth".

  if (boot_type == "wild") {
    bootstrap_sample <- bootstrap_par_wild(
      fixed = fixed,
      transformation = transformation,
      framework = framework,
      model_par = model_par,
      lambda = lambda,
      shift = shift,
      vu_tmp = superpop$vu_tmp,
      res_s = res_s,
      fitted_s = fitted_s
    )
  } else {
    bootstrap_sample <- bootstrap_par(
      fixed = fixed,
      transformation = transformation,
      framework = framework,
      model_par = model_par,
      lambda = lambda,
      shift = shift,
      vu_tmp = superpop$vu_tmp
    )
  }

  framework$smp_data <- bootstrap_sample

  # Prediction of indicators with bootstap sample.
  bootstrap_point_estim <- as.matrix(point_estim(
    fixed = fixed,
    transformation =
      transformation,
    interval = interval,
    L = L,
    framework = framework
  )[[1]][, -1])

  return((bootstrap_point_estim - true_indicators)^2)
} # End mse_estim


# Superpopulation function -----------------------------------------------------

# The model parameter from the nested error linear regression model are
# used to contruct a superpopulation model.
superpopulation_wild <- function(framework, model_par, gen_model, lambda,
                                 shift, transformation, res_s, fitted_s) {
  # rescaling the errors
  res_s <- sqrt(model_par$sigmae2est) * (res_s - mean(res_s)) / sd(res_s)

  # superpopulation random effect
  vu_tmp <- rnorm(framework$N_dom_pop, 0, sqrt(model_par$sigmau2est))
  vu_pop <- rep(vu_tmp, framework$n_pop)

  # income without individual errors
  Y_pop_b <- gen_model$mu_fixed + vu_pop

  indexer <- vapply(Y_pop_b,
    function(x) {
      which.min(abs(x - fitted_s))
    },
    FUN.VALUE = integer(1)
  )

  # superpopulation individual errors
  eps <- res_s[indexer]
  wu <- sample(c(-1, 1), size = length(eps), replace = TRUE)
  eps <- abs(eps) * wu

  #  superpopulation income vector
  Y_pop_b <- Y_pop_b + eps

  Y_pop_b <- back_transformation(
    y = Y_pop_b,
    transformation = transformation,
    lambda = lambda,
    shift = shift
  )
  Y_pop_b[!is.finite(Y_pop_b)] <- 0

  return(list(pop_income_vector = Y_pop_b, vu_tmp = vu_tmp))
}

superpopulation <- function(framework, model_par, gen_model, lambda, shift,
                            transformation) {
  # superpopulation individual errors
  eps <- vector(length = framework$N_pop)
  eps[framework$obs_dom] <- rnorm(
    sum(framework$obs_dom), 0,
    sqrt(model_par$sigmae2est)
  )
  eps[!framework$obs_dom] <- rnorm(
    sum(!framework$obs_dom), 0,
    sqrt(model_par$sigmae2est +
      model_par$sigmau2est)
  )
  # superpopulation random effect
  vu_tmp <- rnorm(framework$N_dom_pop, 0, sqrt(model_par$sigmau2est))
  vu_pop <- rep(vu_tmp, framework$n_pop)
  #  superpopulation income vector
  Y_pop_b <- gen_model$mu_fixed + eps + vu_pop

  Y_pop_b <- back_transformation(
    y = Y_pop_b,
    transformation = transformation,
    lambda = lambda,
    shift = shift
  )
  Y_pop_b[!is.finite(Y_pop_b)] <- 0

  return(list(pop_income_vector = Y_pop_b, vu_tmp = vu_tmp))
}

# Bootstrap function -----------------------------------------------------------

bootstrap_par <- function(fixed,
                          transformation,
                          framework,
                          model_par,
                          lambda,
                          shift,
                          vu_tmp) {
  # Bootstrap sample individual error term
  eps <- rnorm(framework$N_smp, 0, sqrt(model_par$sigmae2est))
  # Bootstrap sample random effect
  vu_smp <- rep(vu_tmp[framework$dist_obs_dom], framework$n_smp)
  # Extraction of design matrix
  X_smp <- model.matrix(fixed, framework$smp_data)
  # Constant part of income vector for bootstrap sample
  mu_smp <- X_smp %*% model_par$betas
  # Transformed bootstrap income vector
  Y_smp_b <- mu_smp + eps + vu_smp
  # Back transformation of bootstrap income vector
  Y_smp_b <- back_transformation(
    y = Y_smp_b,
    transformation = transformation,
    lambda = lambda,
    shift = shift
  )
  Y_smp_b[!is.finite(Y_smp_b)] <- 0

  # Inclusion of bootstrap income vector into sample data
  bootstrap_smp <- framework$smp_data
  bootstrap_smp[paste(fixed[2])] <- Y_smp_b

  return(bootstrap_sample = bootstrap_smp)
}

bootstrap_par_wild <- function(fixed,
                               transformation,
                               framework,
                               model_par,
                               lambda,
                               shift,
                               vu_tmp,
                               res_s,
                               fitted_s) {
  # rescaling sample individual error term
  res_s <- sqrt(model_par$sigmae2est) * (res_s - mean(res_s)) / sd(res_s)
  # Bootstrap sample individual error term
  ws <- sample(c(-1, 1), size = length(res_s), replace = TRUE)
  eps <- abs(res_s) * ws

  # Bootstrap sample random effect
  vu_smp <- rep(vu_tmp[framework$dist_obs_dom], framework$n_smp)

  # Extraction of design matrix
  X_smp <- model.matrix(fixed, framework$smp_data)

  # Transformed bootstrap income vector
  Y_smp_b <- X_smp %*% model_par$betas + eps + vu_smp

  # Back transformation of bootstrap income vector
  Y_smp_b <- back_transformation(
    y = Y_smp_b,
    transformation = transformation,
    lambda = lambda,
    shift = shift
  )
  Y_smp_b[!is.finite(Y_smp_b)] <- 0

  # Inclusion of bootstrap income vector into sample data
  bootstrap_smp <- framework$smp_data
  bootstrap_smp[paste(fixed[2])] <- Y_smp_b

  return(bootstrap_sample = bootstrap_smp)
}

# progress for mse_estim (only internal) ----------

mse_estim_wrapper <- function(i,
                              B,
                              framework,
                              lambda,
                              shift,
                              model_par,
                              gen_model,
                              fixed,
                              transformation,
                              interval,
                              L,
                              res_s,
                              fitted_s,
                              start_time,
                              boot_type,
                              seedvec) {
  tmp <- mse_estim(
    framework = framework,
    lambda = lambda,
    shift = shift,
    model_par = model_par,
    gen_model = gen_model,
    res_s = res_s,
    fitted_s = fitted_s,
    fixed = fixed,
    transformation = transformation,
    interval = interval,
    L = L,
    boot_type = boot_type
  )

  if (i %% 10 == 0) {
    if (i != B) {
      delta <- difftime(Sys.time(), start_time, units = "secs")
      remaining <- (delta / i) * (B - i)
      remaining <- unclass(remaining)
      remaining <- sprintf(
        "%02d:%02d:%02d:%02d",
        remaining %/% 86400, # days
        remaining %% 86400 %/% 3600, # hours
        remaining %% 3600 %/% 60, # minutes
        remaining %% 60 %/% 1
      ) # seconds)

      message("\r", i, " of ", B, " Bootstrap iterations completed \t
              Approximately ", remaining, " remaining \n")
      if (.Platform$OS.type == "windows") flush.console()
    }
  }
  return(tmp)
}
