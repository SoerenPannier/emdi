# Internal documentation -------------------------------------------------------

# MSE estimation - parametric bootstrap procedure

# Function parametric_bootstrap_tf conducts the MSE estimation defined in function
# mse_estim_tf (see below)
# The parametric boostrap approach can be found in Molina and Rao (2010) p. 376

parametric_bootstrap_tf <- function(framework_ebp_tf,
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

    mse_results <- parallelMap::parallelLapply(
      xs              = seq_len(B),
      fun             = mse_estim_tf_wrapper,
      B               = B,
      framework_ebp_tf       = framework_ebp_tf,
      lambda          = point_ebp_tf$optimal_lambda,
      shift           = point_ebp_tf$shift_par,
      model_par_tf    = point_ebp_tf$model_par_tf,
      gen_model_tf    = point_ebp_tf$gen_model_tf,
      fixed           = fixed,
      transformation  = transformation,
      interval        = interval,
      L               = L,
      start_time      = start_time
    )
    parallelMap::parallelStop()
  } else {
    mse_results <- lapply(
      X = seq_len(B),
      FUN = mse_estim_tf_wrapper,
      B = B,
      framework_ebp_tf = framework_ebp_tf,
      lambda = point_ebp_tf$optimal_lambda,
      shift = point_ebp_tf$shift_par,
      model_par_tf = point_ebp_tf$model_par_tf,
      gen_model_tf = point_ebp_tf$gen_model_tf,
      fixed = fixed,
      transformation = transformation,
      interval = interval,
      L = L,
      start_time = start_time
    )
  }

  message("\r", "Bootstrap completed", "\n")
  if (.Platform$OS.type == "windows") {
    flush.console()
  }


  # Extract bootstrap results for domain and subdomain separately
  mse_dom_list <- lapply(mse_results, `[[`, "mse_dom")
  mse_subdom_list <- lapply(mse_results, `[[`, "mse_subdom")

  # Convert to 3D arrays (dom/subdom x indicators x B)
  mse_dom_array <- simplify2array(mse_dom_list)
  mse_subdom_array <- simplify2array(mse_subdom_list)

  # Average over B (3rd dimension)
  mse_dom_mean <- apply(mse_dom_array, c(1, 2), mean)
  mse_subdom_mean <- apply(mse_subdom_array, c(1, 2), mean)

  mse_dom_df <- data.frame(Domain = unique(framework_ebp_tf$pop_domains_vec),
                           mse_dom_mean)
  mse_subdom_df <- data.frame(Subdomain =
                                unique(framework_ebp_tf$pop_subdomains_vec),
                              mse_subdom_mean)

  return(list(
    mses_dom = mse_dom_df,
    mses_subdom = mse_subdom_df
  ))


}





# mse_estim_tf (only internal) ----------------------------------------------------

# The mse_estim_tf function defines all parameters and estimations which have to
# be replicated B times for the Parametric Bootstrap Approach.
# See Molina and Rao (2010) p. 376
mse_estim_tf <- function(framework_ebp_tf,
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
      fixed = fixed,
      framework_ebp_tf = framework_ebp_tf,
      model_par_tf = model_par_tf,
      gen_model_tf = gen_model_tf,
      lambda = lambda,
      shift = shift,
      transformation = transformation
    )

  pop_income_vector <- superpop$pop_income_vector

  if (inherits(framework_ebp_tf$threshold, "function")) {
    framework_ebp_tf$threshold <-
      framework_ebp_tf$threshold(y = pop_income_vector)
  }

  N_subdom_pop_tmp <- framework_ebp_tf$N_subdom_pop
  pop_subdomains_vec_tmp <- framework_ebp_tf$pop_subdomains_vec
  N_dom_pop_tmp <- framework_ebp_tf$N_dom_pop
  pop_domains_vec_tmp <- framework_ebp_tf$pop_domains_vec


  if(!is.null(framework_ebp_tf$pop_weights)) {
    pop_weights_vec <- framework_ebp_tf$pop_data[[framework_ebp_tf$pop_weights]]
  }else{
    pop_weights_vec <- rep(1, nrow(framework_ebp_tf$pop_data))
  }

  # True indicator values - subdomain
  true_indicators_subdom <- matrix(
    nrow = N_subdom_pop_tmp,
    data = unlist(lapply(framework_ebp_tf$indicator_list,
      function(f, threshold) {
        matrix(
          nrow = N_subdom_pop_tmp,
          data =
            unlist(mapply(
              y = split(pop_income_vector, pop_subdomains_vec_tmp),
              pop_weights = split(pop_weights_vec, pop_subdomains_vec_tmp),
              f,
              threshold = framework_ebp_tf$threshold
            )),
          byrow = TRUE
        )
      },
      threshold = framework_ebp_tf$threshold
    ))
  )

  colnames(true_indicators_subdom) <- framework_ebp_tf$indicator_names

  # True indicator values - domain
  true_indicators_dom <- matrix(
    nrow = N_dom_pop_tmp,
    data = unlist(lapply(framework_ebp_tf$indicator_list,
                         function(f, threshold) {
                           matrix(
                             nrow = N_dom_pop_tmp,
                             data =
                               unlist(mapply(
                                 y = split(pop_income_vector, pop_domains_vec_tmp),
                                 pop_weights = split(pop_weights_vec, pop_domains_vec_tmp),
                                 f,
                                 threshold = framework_ebp_tf$threshold
                               )),
                             byrow = TRUE
                           )
                         },
                         threshold = framework_ebp_tf$threshold
    ))
  )

  colnames(true_indicators_dom) <- framework_ebp_tf$indicator_names

  # The function bootstrap_par_tf returns a sample that can be given into the
  # point estimation to get predictors of the indicators that can be compared
  # to the "truth".

    bootstrap_sample <- bootstrap_par_tf(
      fixed = fixed,
      transformation = transformation,
      framework_ebp_tf = framework_ebp_tf,
      model_par_tf = model_par_tf,
      lambda = lambda,
      shift = shift,
      vu_tmp1 = superpop$vu_tmp1,
      vu_tmp2 = superpop$vu_tmp2
    )


  framework_ebp_tf$smp_data <- bootstrap_sample

  bootstrap_point_ebp_tf <- point_ebp_tf(
    fixed = fixed,
    transformation = transformation,
    interval = interval,
    L = L,
    framework_ebp_tf = framework_ebp_tf
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


superpopulation_tf <- function(fixed, framework_ebp_tf, model_par_tf,
                               gen_model_tf, lambda, shift,transformation) {
  # superpopulation individual errors
  eps <- vector(length = framework_ebp_tf$N_pop)
  eps[framework_ebp_tf$obs_subdom] <- rnorm(
    sum(framework_ebp_tf$obs_subdom), 0,
    sqrt(model_par_tf$sigmae2est)
  )
  eps[!framework_ebp_tf$obs_subdom & framework_ebp_tf$obs_dom] <- rnorm(
    sum(!framework_ebp_tf$obs_subdom & framework_ebp_tf$obs_dom), 0,
    sqrt(model_par_tf$sigmae2est + model_par_tf$sigmau2_2est)
  )
  eps[!framework_ebp_tf$obs_dom] <- rnorm(
    sum(!framework_ebp_tf$obs_dom), 0,
    sqrt(model_par_tf$sigmae2est + model_par_tf$sigmau2_1est +
           model_par_tf$sigmau2_2est)
  )

  # Generate random effects
  # domain RE
  vu_tmp1 <- rnorm(framework_ebp_tf$N_dom_pop, 0, sqrt(model_par_tf$sigmau2_1est))
  # subdomain RE
  vu_tmp2 <- rnorm(framework_ebp_tf$N_subdom_pop, 0, sqrt(model_par_tf$sigmau2_2est))

  # Domain-level: create a lookup table
  unique_dom_pop <- unique(framework_ebp_tf$pop_domains_vec)
  dom_re_pop <- data.frame(idD = unique_dom_pop, vu1 = vu_tmp1)

  # Subdomain-level: create another lookup table
  unique_subdom_pop <- unique(framework_ebp_tf$pop_subdomains_vec)
  subdom_re_pop <- data.frame(idSub = unique_subdom_pop,
                             vu2 = vu_tmp2)

  # Merge into population data
  pop_data <- framework_ebp_tf$pop_data
  pop_data$row_index <- seq_len(nrow(pop_data))
  pop_data <- merge(pop_data, dom_re_pop,
                    by.x = framework_ebp_tf$pop_domains,
                    by.y = "idD", all.x = TRUE)
  pop_data <- merge(pop_data, subdom_re_pop,
                    by.x = framework_ebp_tf$pop_subdomains,
                    by.y = "idSub", all.x = TRUE)


  pop_data[[paste0(fixed[2])]] <- seq_len(nrow(pop_data))
  X_pop <- model.matrix(fixed, pop_data)
  mu_fixed <- X_pop %*% model_par_tf$betas
  Y_pop_b <- mu_fixed + eps + pop_data$vu1 + pop_data$vu2

  Y_pop_b <- back_transformation(
    y = Y_pop_b,
    transformation = transformation,
    lambda = lambda,
    shift = shift
  )
  Y_pop_b[!is.finite(Y_pop_b)] <- 0

  bootstrap_pop <- pop_data
  bootstrap_pop[paste(fixed[2])] <- Y_pop_b

  bootstrap_pop <- bootstrap_pop[order(bootstrap_pop$row_index),]

  return(list(pop_income_vector = bootstrap_pop[paste(fixed[2])][,1],
              vu_tmp1 = vu_tmp1, vu_tmp2 = vu_tmp2))
}

# Bootstrap function -----------------------------------------------------------

bootstrap_par_tf <- function(fixed,
                          transformation,
                          framework_ebp_tf,
                          model_par_tf,
                          lambda,
                          shift,
                          vu_tmp1,
                          vu_tmp2) {
  # Bootstrap sample individual error term
  eps <- rnorm(framework_ebp_tf$N_smp, 0, sqrt(model_par_tf$sigmae2est))

  # Bootstrap sample random effect
  vu_smp1 <- vu_tmp1[framework_ebp_tf$dist_obs_dom]
  vu_smp2 <- vu_tmp2[framework_ebp_tf$dist_obs_subdom]

   # Domain-level: create a lookup table
  unique_dom_smp <- unique(framework_ebp_tf$smp_domains_vec)
  dom_re_smp <- data.frame(idD = unique_dom_smp, vu1 = vu_smp1)

  # Subdomain-level: create another lookup table
  unique_subdom_smp <- unique(framework_ebp_tf$smp_subdomains_vec)
  subdom_re_smp <- data.frame(idSub = unique_subdom_smp,
                              vu2 = vu_smp2)

  # Merge into smp data
  smp_data <- framework_ebp_tf$smp_data
  smp_data$row_index <- seq_len(nrow(smp_data))
  smp_data <- merge(smp_data, dom_re_smp,
                    by.x = framework_ebp_tf$smp_domains,
                    by.y = "idD", all.x = TRUE)
  smp_data <- merge(smp_data, subdom_re_smp,
                    by.x = framework_ebp_tf$smp_subdomains,
                    by.y = "idSub", all.x = TRUE)


  # Extraction of design matrix
  X_smp <- model.matrix(fixed, smp_data)
  # Constant part of income vector for bootstrap sample
  mu_smp <- X_smp %*% model_par_tf$betas

  # Transformed bootstrap income vector
  Y_smp_b <- mu_smp + eps + smp_data$vu1 + smp_data$vu2

  # Back transformation of bootstrap income vector
  Y_smp_b <- back_transformation(
    y = Y_smp_b,
    transformation = transformation,
    lambda = lambda,
    shift = shift
  )
  Y_smp_b[!is.finite(Y_smp_b)] <- 0

  # Inclusion of bootstrap income vector into sample data
  bootstrap_smp <- smp_data
  bootstrap_smp[paste(fixed[2])] <- Y_smp_b

  bootstrap_smp <- bootstrap_smp[order(bootstrap_smp$row_index),]
  bootstrap_smp$row_index <- NULL


  return(bootstrap_sample = bootstrap_smp)
}



# progress for mse_estim_tf (only internal) ----------

mse_estim_tf_wrapper <- function(i,
                                 B,
                                 framework_ebp_tf,
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
    framework_ebp_tf = framework_ebp_tf,
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


