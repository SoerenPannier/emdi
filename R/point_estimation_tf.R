# Internal documentation -------------------------------------------------------

# Point estimation function - twofold

# This function implements the transformation of data, estimation of the twofold
# nested error linear regression model and the monte-carlo approximation to
# predict the desired indicators. If the weighted version of the approach is
# used, then additional estimation steps are taken in order to calculate
# weighted regression coefficients before the monte-carlo approximation. See
# corresponding functions below.


point_ebp_tf <- function(framework,
                        fixed,
                        transformation,
                        interval,
                        L,
                        keep_data = FALSE) {

  # Transformation of data -----------------------------------------------------

  # Estimating the optimal parameter by optimization
  # Optimal parameter function returns the minimum of the optimization
  # functions from generic_opt; the minimum is the optimal lambda.
  # The function can be found in the script optimal_parameter.R
  optimal_lambda <- optimal_parameter(
    generic_opt = generic_opt,
    fixed = fixed,
    smp_data = framework$smp_data,
    smp_domains = framework$smp_domains,
    transformation = transformation,
    interval = interval
  )
  #browser()
  # Data_transformation function returns transformed data and shift parameter.
  # The function can be found in the script transformation_functions.R
  transformation_par <- data_transformation(
    fixed = fixed,
    smp_data = framework$smp_data,
    transformation = transformation,
    lambda = optimal_lambda
  )
  shift_par <- transformation_par$shift

  #print(dim(transformation_par$transformed_data))
  #print(head(transformation_par$transformed_data))

  #random_effects_formula <- as.formula(paste("~ 1 |", framework$smp_domains, "/", framework$smp_subdomains))
  #print(random_effects_formula)

  # Model estimation, model parameter and parameter of generating model --------

  # Estimation of the nested error linear regression model
  # See Molina and Rao (2010) p. 374
  # lme function is included in the nlme package which is imported.

 # browser()
  mixed_model_tf <- nlme::lme(
    fixed = fixed,
    data = transformation_par$transformed_data,
    random =
      as.formula(paste0(
        "~ 1 | " , framework$smp_domains, "/", framework$smp_subdomains)),
   #random = random_effects_formula,
    method = "REML",
    keep.data = keep_data
  )


  # Function model_par_tf extracts the needed parameters theta from the nested
  # error linear regression model. It returns the beta coefficients (betas),
  # sigmae2est, sigmau2est and the random effect (rand_eff).

  est_par_tf <- model_par_tf(
    mixed_model_tf = mixed_model_tf,
    framework = framework,
    fixed = fixed,
    transformation_par = transformation_par
  )

  # Function gen_model_tf calculates the parameters in the generating model.
  # See Molina and Rao (2010) p. 375 (20)
  # The function returns sigmav2est and the constant part mu.
  gen_par_tf <- gen_model_tf(
    model_par_tf = est_par_tf,
    fixed = fixed,
    framework = framework
  )

  # Monte-Carlo approximation --------------------------------------------------
  if (inherits(framework$threshold, "function")) {
    framework$threshold <-
      framework$threshold(
        y =
          as.numeric(framework$smp_data[[paste0(fixed[2])]])
      )
  }

  # The monte-carlo function returns a data frame of desired indicators.
  indicator_prediction <- monte_carlo(
    transformation = transformation,
    L = L,
    framework = framework,
    lambda = optimal_lambda,
    shift = shift_par,
    model_par_tf = est_par_tf,
    gen_model_tf = gen_par_tf
  )

  #mixed_model_tf$coefficients_weighted <- if (!is.null(framework$weights)) {
   # as.numeric(est_par_tf$betas)
 # } else {
  #  NULL
  #}
  #names(mixed_model_tf$coefficients_weighted) <- if (!is.null(framework$weights)) {
    #rownames(est_par_tf$betas)
  #} else {
   # NULL
  #}
  return(list(
    ind = indicator_prediction,
    optimal_lambda = optimal_lambda,
    shift_par = shift_par,
    model_par_tf = est_par_tf,
    gen_model_tf = gen_par_tf,
    model = mixed_model_tf
  ))
} # End point estimation function


# All following functions are only internal ------------------------------------

# Functions to extract and calculate model parameter----------------------------

# Function model_par_tf extracts the needed parameters theta from the nested
# error linear regression model. It returns the beta coefficients (betas),
# sigmae2est, sigmau2est and the random effect (rand_eff).

#browser()
model_par_tf <- function(framework,
                      mixed_model_tf,
                      fixed,
                      transformation_par) {
    # fixed parametersn
    betas <- nlme::fixed.effects(mixed_model_tf)
    # Estimated error variance
    sigmae2est <- mixed_model_tf$sigma^2
    # VarCorr(fit2) is the estimated random error variance
    sigmau2_1est <- as.numeric(nlme::VarCorr(mixed_model_tf)[2, 1])
    sigmau2_2est <- as.numeric(nlme::VarCorr(mixed_model_tf)[4, 1])
    # Random effect: vector with zeros for all domains, filled with
    #_________________________________________________________________________________________
    rand_eff1 <- rep(0, length(unique(framework$pop_domains_vec)))
    # random effect for in-sample domains (dist_obs_dom)
    rand_eff1[framework$dist_obs_dom] <- (random.effects(mixed_model_tf)[[1]][[1]])
    rand_eff2<- rep(0, length(unique(framework$pop_subdomains_vec)))
    # random effect for in-sample sub-domains (dist_obs_subdom)
    rand_eff2[framework$dist_obs_subdom] <- (random.effects(mixed_model_tf)[[2]][[1]])
    #_________________________________________________________________________________________

    return(list(
      betas = betas,
      sigmae2est = sigmae2est,
      sigmau2_1est = sigmau2_1est,
      sigmau2_2est = sigmau2_2est,
      rand_eff1 = rand_eff1,
      rand_eff2 = rand_eff2
    ))

} # End model_par_tf



# Function gen_model_tf calculates the parameters in the generating model.
# See Molina and Rao (2010) p. 375 (20)
gen_model_tf <- function(fixed,
                      framework,
                      model_par_tf) {
    # Parameter for calculating variance of new random effect
         ##gamma <- model_par_tf$sigmau2est / (model_par_tf$sigmau2est +
         ##model_par_tf$sigmae2est / framework$n_smp)
    #____________________________19.08.2024____________________________________________
    gamma_dt <- model_par_tf$sigmau2_2est / (model_par_tf$sigmau2_2est +
                                            (model_par_tf$sigmae2est / framework$ndt_smp))
    gamma_dl <- (1-gamma_dt)*framework$ndt_smp
    #unique_dom <- unique(smp_domains_vec) #defined in framework
    gamma_d <- numeric(framework$N_dom_smp)

    for (i in seq_along(framework$unique_dom_smp)) {
      d <- framework$unique_dom_smp[i]

      # Find corresponding domains with similar integer parts
      matching_domains <- grep(paste0("^", d, "\\."), framework$unique_subdom_smp, value = TRUE)

      # If there are matching domains, sum their values
      if (length(matching_domains) > 0) {
        gamma_d[i] <- sum(gamma_dl[matching_domains], na.rm = TRUE)
      }
    }

    phi_d <- model_par_tf$sigmau2_1est/(model_par_tf$sigmae2est+
                                       (model_par_tf$sigmau2_1est*gamma_d))

    coef_var <-  1+gamma_dt*(gamma_dt-2)
    domain_labels <- names(coef_var)
    # Initialize result vector
    coef_var_prod <- numeric(length(framework$N_subdom_smp))

    # Loop through each domain in phi_d
    for (d in names(phi_d)) {
      # Find corresponding indices in coef_var
      indices_coef_var <- grep(d, framework$unique_subdom_smp)

      # Multiply corresponding values
      coef_var_prod[indices_coef_var] <- coef_var[indices_coef_var] * phi_d[d]

    }

    #____________________Edited 19.08.2024____________________________________________________
    # Variance of new random effect
    sigmav2est_sampled_dt <- model_par_tf$sigmae2est * coef_var_prod +
      model_par_tf$sigmau2_2est * (1 - gamma_dt)

    sigmav2est_nonsampled_dt <- model_par_tf$sigmae2est * phi_d +
      model_par_tf$sigmau2_2est
    # Random effect in constant part of y for in-sample households
    rand_eff1_pop <- rep(model_par_tf$rand_eff1, framework$n_pop)
    rand_eff2_pop <- rep(model_par_tf$rand_eff2, framework$ndt_pop)
    # Model matrix for population covariate information
    framework$pop_data[[paste0(fixed[2])]] <- seq_len(nrow(framework$pop_data))
    X_pop <- model.matrix(fixed, framework$pop_data)

    # Constant part of predicted y
    mu_fixed <- X_pop %*% model_par_tf$betas
    mu <- mu_fixed + rand_eff1_pop + rand_eff2_pop

    return(list(sigmav2est_sampled_dt = sigmav2est_sampled_dt,
                sigmav2est_nonsampled_dt = sigmav2est_nonsampled_dt,
                mu = mu, mu_fixed = mu_fixed))

} # End gen_model_tf


# Monte-Carlo approximation ----------------------------------------------------

# The function approximates the expected value (Molina and Rao (2010)
# p.372 (6)). For description of monte-carlo simulation see Molina and
# Rao (2010) p. 373 (13) and p. 374-375
monte_carlo <- function(transformation,
                        L,
                        framework,
                        lambda,
                        shift,
                        model_par_tf,
                        gen_model_tf) {

  # Preparing matrices for indicators for the Monte-Carlo simulation

  if(!is.null(framework$aggregate_to_vec)){
    N_dom_pop_tmp <- framework$N_dom_pop_agg
    pop_domains_vec_tmp <- framework$aggregate_to_vec
  } else {
    N_dom_pop_tmp <- framework$N_dom_pop
    pop_domains_vec_tmp <- framework$pop_domains_vec
  }

  ests_mcmc <- array(dim = c(
    N_dom_pop_tmp,
    L,
    length(framework$indicator_names)
  ))

  for (l in seq_len(L)) {

    # Errors in generating model: individual error term and random effect
    # See below for function errors_gen.
    errors <- errors_gen(
      framework = framework,
      model_par_tf = model_par_tf,
      gen_model_tf = gen_model_tf
    )

    # Prediction of population vector y
    # See below for function prediction_y.
    population_vector <- prediction_y(
      transformation = transformation,
      lambda = lambda,
      shift = shift,
      gen_model_tf = gen_model_tf,
      errors_gen = errors,
      framework = framework
    )

    if(!is.null(framework$pop_weights)){
      pop_weights_vec <- framework$pop_data[[framework$pop_weights]]
    }else{
      pop_weights_vec <- rep(1, nrow(framework$pop_data))
    }

    # Calculation of indicators for each Monte Carlo population
    ests_mcmc[, l, ] <-
      matrix(
        nrow = N_dom_pop_tmp,
        data = unlist(lapply(framework$indicator_list,
          function(f, threshold) {
            matrix(
              nrow = N_dom_pop_tmp,
              data = unlist(mapply(
                y = split(population_vector, pop_domains_vec_tmp),
                pop_weights = split(pop_weights_vec, pop_domains_vec_tmp),
                f,
                threshold = framework$threshold
              )), byrow = TRUE
            )
          },
          threshold = framework$threshold
        ))
      )
  } # End for loop


  # Point estimations of indicators by taking the mean

  point_estimates <- data.frame(
    Domain = unique(pop_domains_vec_tmp),
    apply(ests_mcmc, c(3), rowMeans)
  )
  colnames(point_estimates) <- c("Domain", framework$indicator_names)
  return(point_estimates)
} # End Monte-Carlo


# The function errors_gen returns error terms of the generating model.
# See Molina and Rao (2010) p. 375 (20)

errors_gen <- function(framework, model_par_tf, gen_model_tf) {
  # individual error term in generating model epsilon
  epsilon <- rnorm(framework$N_pop, 0, sqrt(model_par_tf$sigmae2est))

  # empty vector for new random effect in generating model
  vu <- vector(length = framework$N_pop)
  # new random effect for out-of-sample domains
  vu[!framework$obs_dom] <- rep(
    rnorm(
      framework$N_dom_unobs,
      0,
      sqrt((model_par_tf$sigmau2_1est + model_par_tf$sigmau2_2est))
    ),
    framework$n_pop[!framework$dist_obs_dom]
  )
  # new random effect for out-sample-subdomains
  vu[!framework$obs_subdom] <- rep(
    rnorm(
      framework$N_subdom_unobs,
      0,
      sqrt(gen_model_tf$sigmav2est_nonsampled_dt)
    ),
    framework$ndt_pop[!framework$dist_obs_subdom]
  )
  # new random effect for in-sample-subdomains
  vu[framework$obs_subdom] <- rep(
    rnorm(
      rep(1, framework$N_subdom_smp),
      0,
      sqrt(gen_model_tf$sigmav2est_sampled_dt)
    ),
    framework$ndt_pop[framework$dist_obs_subdom]
  )

  return(list(epsilon = epsilon, vu = vu))
} # End errors_gen

# The function prediction_y returns a predicted income vector which can be used
# to calculate indicators. Note that a whole income vector is predicted without
# distinction between in- and out-of-sample domains.
prediction_y <- function(transformation,
                         lambda,
                         shift,
                         gen_model_tf,
                         errors_gen,
                         framework) {

  # predicted population income vector
  y_pred <- gen_model_tf$mu + errors_gen$epsilon + errors_gen$vu

  # back-transformation of predicted population income vector
  y_pred <- back_transformation(
    y = y_pred,
    transformation = transformation,
    lambda = lambda,
    shift = shift
  )
  y_pred[!is.finite(y_pred)] <- 0

  return(y_pred)
} # End prediction_y
