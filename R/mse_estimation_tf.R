# Internal documentation -------------------------------------------------------

# MSE estimation - parametric bootstrap procedure

# Function parametric_bootstrap_tf conducts the MSE estimation defined in function
# mse_estim_tf (see below)
# The parametric boostrap approach can be found in Molina and Rao (2010) p. 376

parametric_bootstrap_tf <- function(framework,
                                    point_ebp_tf,
                                    fixed,
                                    transformation,
                                    interval = c(-1, 2),
                                    L,
                                    B,
                                    parallel_mode,
                                    cpus) {
  message("\r", "Bootstrap started                                            ")

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
    # Load necessary libraries on worker processes
    parallelMap::parallelLibrary("nlme")

    mse_results <- simplify2array(parallelMap::parallelLapply(
      xs              = seq_len(B),
      fun             = mse_estim_tf_wrapper,
      B               = B,
      framework       = framework,
      lambda          = point_ebp_tf$optimal_lambda,
      shift           = point_ebp_tf$shift_par,
      model_par_tf    = point_ebp_tf$model_par_tf,
      gen_model_tf    = point_ebp_tf$gen_model_tf,
      fixed           = fixed,
      transformation  = transformation,
      interval        = interval,
      L               = L,
      start_time      = start_time
    ))
    parallelMap::parallelStop()
  } else {
    mse_results <- simplify2array(lapply(
      X = seq_len(B),
      FUN = mse_estim_tf_wrapper,
      B = B,
      framework = framework,
      lambda = point_ebp_tf$optimal_lambda,
      shift = point_ebp_tf$shift_par,
      model_par_tf = point_ebp_tf$model_par_tf,
      gen_model_tf = point_ebp_tf$gen_model_tf,
      fixed = fixed,
      transformation = transformation,
      interval = interval,
      L = L,
      start_time = start_time
    ))
  }

  message("\r", "Bootstrap completed", "\n")
  if (.Platform$OS.type == "windows") {
    flush.console()
  }

  mses <- apply(mse_results[[1]], c(1, 2), mean)
  mses_subdom <- apply(mse_results[[2]], c(1,2), mean)

  mses <- data.frame(Domain = unique(framework$pop_domains_vec), mses)
  mses_subdom <- data.frame(Subdomain = unique(framework$pop_subdomains_vec), mses_subdom)

  return(list(mses=mses, mses_subdom=mses_subdom))
}





# mse_estim_tf (only internal) ----------------------------------------------------

# The mse_estim_tf function defines all parameters and estimations which have to
# be replicated B times for the Parametric Bootstrap Approach.
# See Molina and Rao (2010) p. 376
mse_estim_tf <- function(framework,
                      lambda,
                      shift,
                      model_par_tf,
                      gen_model_tf,
                      fixed,
                      transformation,
                      interval,
                      L) {

  # The function superpopulation_tf returns an income vector and a temporary
  # variable that passes the random effect to generating bootstrap populations
  # in bootstrap_par_tf.

    superpop <- superpopulation_tf(
      framework = framework,
      model_par_tf = model_par_tf,
      gen_model_tf = gen_model_tf,
      lambda = lambda,
      shift = shift,
      transformation = transformation
    )

  pop_income_vector <- superpop$pop_income_vector

  if (inherits(framework$threshold, "function")) {
    framework$threshold <-
      framework$threshold(y = pop_income_vector)
  }

  N_subdom_pop_tmp <- framework$N_subdom_pop
  pop_subdomains_vec_tmp <- framework$pop_subdomains_vec
  N_dom_pop_tmp <- framework$N_dom_pop
  pop_domains_vec_tmp <- framework$pop_domains_vec


  if(!is.null(framework$pop_weights)) {
    pop_weights_vec <- framework$pop_data[[framework$pop_weights]]
  }else{
    pop_weights_vec <- rep(1, nrow(framework$pop_data))
  }

  # True indicator values
  true_indicators_subdom <- matrix(
    nrow = N_subdom_pop_tmp,
    data = unlist(lapply(framework$indicator_list,
      function(f, threshold) {
        matrix(
          nrow = N_subdom_pop_tmp,
          data =
            unlist(mapply(
              y = split(pop_income_vector, pop_subdomains_vec_tmp),
              pop_weights = split(pop_weights_vec, pop_subdomains_vec_tmp),
              f,
              threshold = framework$threshold
            )),
          byrow = TRUE
        )
      },
      threshold = framework$threshold
    ))
  )

  colnames(true_indicators_subdom) <- framework$indicator_names

  # True indicator values
  true_indicators_dom <- matrix(
    nrow = N_dom_pop_tmp,
    data = unlist(lapply(framework$indicator_list,
                         function(f, threshold) {
                           matrix(
                             nrow = N_dom_pop_tmp,
                             data =
                               unlist(mapply(
                                 y = split(pop_income_vector, pop_domains_vec_tmp),
                                 pop_weights = split(pop_weights_vec, pop_domains_vec_tmp),
                                 f,
                                 threshold = framework$threshold
                               )),
                             byrow = TRUE
                           )
                         },
                         threshold = framework$threshold
    ))
  )

  colnames(true_indicators_dom) <- framework$indicator_names

  # The function bootstrap_par_tf returns a sample that can be given into the
  # point estimation to get predictors of the indicators that can be compared
  # to the "truth".

    bootstrap_sample <- bootstrap_par_tf(
      fixed = fixed,
      transformation = transformation,
      framework = framework,
      model_par_tf = model_par_tf,
      lambda = lambda,
      shift = shift,
      vu_tmp1 = superpop$vu_tmp1,
      vu_tmp2 = superpop$vu_tmp2
    )


  framework$smp_data <- bootstrap_sample

  bootstrap_point_ebp_tf <- point_ebp_tf(
    fixed = fixed,
    transformation = transformation,
    interval = interval,
    L = L,
    framework = framework
  )

  bootstrap_point_ebp_tf_dom <- as.matrix(bootstrap_point_ebp_tf[[1]][, -1])
  bootstrap_point_ebp_tf_subdom <- as.matrix(bootstrap_point_ebp_tf[[2]][, -1])



  mse_dom <- (bootstrap_point_ebp_tf_dom - true_indicators_dom)^2
  mse_subdom <- (bootstrap_point_ebp_tf_subdom - true_indicators_subdom)^2

  return(list(mse_dom = mse_dom, mse_subdom = mse_subdom))
} # End mse_estim_tf


# Superpopulation_tf function -----------------------------------------------------

# The model parameter from the nested error linear regression model are
# used to construct a superpopulation_tf model.


superpopulation_tf <- function(framework, model_par_tf, gen_model_tf, lambda, shift,
                            transformation) {
  # superpopulation individual errors
  eps <- vector(length = framework$N_pop)
  eps[framework$obs_subdom] <- rnorm(
    sum(framework$obs_subdom), 0,
    sqrt(model_par_tf$sigmae2est)
  )
  eps[!framework$obs_subdom & framework$obs_dom] <- rnorm(
    sum(!framework$obs_subdom & framework$obs_dom), 0,
    sqrt(model_par_tf$sigmae2est + model_par_tf$sigmau2_2est)
  )
  eps[!framework$obs_dom] <- rnorm(
    sum(!framework$obs_dom), 0,
    sqrt(model_par_tf$sigmae2est + model_par_tf$sigmau2_1est +
           model_par_tf$sigmau2_2est)
  )

  # superpopulation random effect
  vu_tmp1 <- rnorm(framework$N_dom_pop, 0, sqrt(model_par_tf$sigmau2_1est))
  vu_tmp2 <- rnorm(framework$N_subdom_pop, 0, sqrt(model_par_tf$sigmau2_2est))
  vu_pop1 <- rep(vu_tmp1, framework$n_pop)
  vu_pop2 <- rep(vu_tmp2, framework$ndt_pop)

  #  superpopulation income vector
  Y_pop_b <- gen_model_tf$mu_fixed + eps + vu_pop1 + vu_pop2

  Y_pop_b <- back_transformation(
    y = Y_pop_b,
    transformation = transformation,
    lambda = lambda,
    shift = shift
  )
  Y_pop_b[!is.finite(Y_pop_b)] <- 0

  return(list(pop_income_vector = Y_pop_b, vu_tmp1 = vu_tmp1, vu_tmp2 = vu_tmp2))
}

# Bootstrap function -----------------------------------------------------------

bootstrap_par_tf <- function(fixed,
                          transformation,
                          framework,
                          model_par_tf,
                          lambda,
                          shift,
                          vu_tmp1,
                          vu_tmp2) {
  # Bootstrap sample individual error term
  eps <- rnorm(framework$N_smp, 0, sqrt(model_par_tf$sigmae2est))
  # Bootstrap sample random effect
  vu_smp1 <- rep(vu_tmp1[framework$dist_obs_dom], framework$n_smp)
  vu_smp2 <- rep(vu_tmp2[framework$dist_obs_subdom], framework$ndt_smp)
  # Extraction of design matrix
  X_smp <- model.matrix(fixed, framework$smp_data)
  # Constant part of income vector for bootstrap sample
  mu_smp <- X_smp %*% model_par_tf$betas
  # Transformed bootstrap income vector
  Y_smp_b <- mu_smp + eps + vu_smp1 + vu_smp2
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



# progress for mse_estim_tf (only internal) ----------

mse_estim_tf_wrapper <- function(i,
                                 B,
                                 framework,
                                 lambda,
                                 shift,
                                 model_par_tf,
                                 gen_model_tf,
                                 fixed,
                                 transformation,
                                 interval,
                                 L,
                                 start_time) {
  tmp <- mse_estim_tf(
    framework = framework,
    lambda = lambda,
    shift = shift,
    model_par_tf = model_par_tf,
    gen_model_tf = gen_model_tf,
    fixed = fixed,
    transformation = transformation,
    interval = interval,
    L = L
  )

  if (i %% 10 == 0 && i != B) {
    delta <- difftime(Sys.time(), start_time, units = "secs")
    remaining <- (delta / i) * (B - i)
    remaining <- unclass(remaining)
    remaining <- sprintf(
      "%02d:%02d:%02d:%02d",
      remaining %/% 86400, # days
      remaining %% 86400 %/% 3600, # hours
      remaining %% 3600 %/% 60, # minutes
      remaining %% 60 %/% 1
    )

    message("\r", i, " of ", B, " Bootstrap iterations completed \t
            Approximately ", remaining, " remaining \n")
    if (.Platform$OS.type == "windows") flush.console()
  }
  return(tmp)
}


