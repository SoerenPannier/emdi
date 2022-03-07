# Internal documentation -------------------------------------------------------

# Point estimation function

# This function implements the transformation of data, estimation of the nested
# error linear regression model and the monte-carlo approximation to predict
# the desired indicators. If the weighted version of the approach is used, then
# additional estimation steps are taken in order to calculate weighted regression
# coefficients before the monte-carlo approximation. See corresponding functions below.


point_estim <- function (framework,
                         fixed,
                         transformation,
                         interval,
                         L,
                         keep_data  = FALSE
) {
  
  # Transformation of data -------------------------------------------------------
  
  # Estimating the optimal parameter by optimization
  # browser()
  # Optimal parameter function returns the minimum of the optimization
  # functions from generic_opt; the minimum is the optimal lambda.
  # The function can be found in the script optimal_parameter.R
  optimal_lambda <- optimal_parameter(generic_opt    = generic_opt,
                                      fixed          = fixed,
                                      smp_data       = framework$smp_data,
                                      smp_domains    = framework$smp_domains,
                                      transformation = transformation,
                                      interval       = interval
  )
  
  # Data_transformation function returns transformed data and shift parameter.
  # The function can be found in the script transformation_functions.R
  # browser()
  transformation_par <- data_transformation(fixed          = fixed,
                                            smp_data       = framework$smp_data,
                                            transformation = transformation,
                                            lambda         = optimal_lambda
  )
  shift_par <- transformation_par$shift
  
  # Model estimation, model parameter and parameter of generating model --------
  
  # Estimation of the nested error linear regression model
  # See Molina and Rao (2010) p. 374
  # lme function is included in the nlme package which is imported.
  
  mixed_model <- nlme::lme(fixed  = fixed,
                           data   = transformation_par$transformed_data ,
                           random = as.formula(paste0("~ 1 | as.factor(", 
                                                      framework$smp_domains, ")")),
                           method = "REML",
                           keep.data = keep_data)
  
  
  # Function model_par extracts the needed parameters theta from the nested
  # error linear regression model. It returns the beta coefficients (betas),
  # sigmae2est, sigmau2est and the random effect (rand_eff).
  
  est_par <- model_par(mixed_model = mixed_model,
                       framework   = framework,
                       fixed       = fixed,
                       transformation_par = transformation_par
  )
  
  # Function gen_model calculates the parameters in the generating model.
  # See Molina and Rao (2010) p. 375 (20)
  # The function returns sigmav2est and the constant part mu.
  gen_par <- gen_model(model_par   = est_par,
                       fixed       = fixed,
                       framework   = framework
  )
  
  # Monte-Carlo approximation --------------------------------------------------
  if(inherits(framework$threshold, "function")) {
    framework$threshold <- 
      framework$threshold(y = as.numeric(framework$smp_data[[paste0(fixed[2])]]))
  }
  
  # The monte-carlo function returns a data frame of desired indicators.
  indicator_prediction <- monte_carlo(transformation = transformation,
                                      L              = L,
                                      framework      = framework,
                                      lambda         = optimal_lambda,
                                      shift          = shift_par,
                                      model_par      = est_par,
                                      gen_model      = gen_par
  )
  
  mixed_model$coefficients_weighted <- if(!is.null(framework$weights)) {
    as.numeric(est_par$betas)
  } else {NULL}
  names(mixed_model$coefficients_weighted) <-if(!is.null(framework$weights)) {
    rownames(est_par$betas)
  } else {NULL}
  return(list(ind            = indicator_prediction,
              optimal_lambda = optimal_lambda,
              shift_par      = shift_par,
              model_par      = est_par,
              gen_model      = gen_par,
              model          = mixed_model
  )
  )
} # End point estimation function


# All following functions are only internal ------------------------------------

# Functions to extract and calculate model parameter----------------------------

# Function model_par extracts the needed parameters theta from the nested
# error linear regression model. It returns the beta coefficients (betas),
# sigmae2est, sigmau2est and the random effect (rand_eff).

model_par <- function(framework,
                      mixed_model,
                      fixed,
                      transformation_par) {
  # browser()
  if(is.null(framework$weights)) {
    # fixed parametersn
    betas <- nlme::fixed.effects(mixed_model)
    # Estimated error variance
    sigmae2est <- mixed_model$sigma^2
    # VarCorr(fit2) is the estimated random error variance
    sigmau2est <- as.numeric(nlme::VarCorr(mixed_model)[1,1])
    # Random effect: vector with zeros for all domains, filled with
    # browser()
    rand_eff <- rep(0, length(unique(framework$pop_domains_vec)))
    # random effect for in-sample domains (dist_obs_dom)
    rand_eff[framework$dist_obs_dom] <- (random.effects(mixed_model)[[1]])
    
    return(list(betas      = betas,
                sigmae2est = sigmae2est,
                sigmau2est = sigmau2est,
                rand_eff   = rand_eff
    )
    )
  } else {
    # fixed parameters
    betas <- nlme::fixed.effects(mixed_model)
    # Estimated error variance
    sigmae2est<-mixed_model$sigma^2
    # VarCorr(fit2) is the estimated random error variance
    sigmau2est <- as.numeric(nlme::VarCorr(mixed_model)[1,1])
    
    # Calculations needed for pseudo EB
    
    weight_sum    <- rep(0, framework$N_dom_smp)
    mean_dep      <- rep(0, framework$N_dom_smp)
    mean_indep    <- matrix(0 ,nrow = framework$N_dom_smp, ncol = length(betas)) 
    delta2        <- rep(0,framework$N_dom_smp)
    gamma_weight  <- rep(0,framework$N_dom_smp)
    num           <- matrix(0, nrow = length(betas), ncol = 1)
    den           <- matrix(0, nrow = length(betas), ncol = length(betas))
    
    for (d in 1:framework$N_dom_smp){
      domain  <- as.character(unique(framework$smp_domains_vec)[d])
      
      # Domain means of of the dependent variable
      dep_smp       <- transformation_par$transformed_data[[as.character(mixed_model$terms[[2]])]][
        framework$smp_domains_vec == domain]
      weight_smp    <- transformation_par$transformed_data[[as.character(framework$weights)]][
        framework$smp_domains_vec == domain]
      weight_sum[d] <- sum(weight_smp)
      indep_smp     <- model.matrix(fixed, framework$smp_data[framework$smp_domains_vec == domain,])
      
      # weighted mean of the dependent variable
      mean_dep[d] <- sum(weight_smp * dep_smp) / weight_sum[d]
      
      # weighted means of the auxiliary information
      for (k in 1:length(betas)){
        mean_indep[d,k] <- sum(weight_smp * indep_smp[,k]) / weight_sum[d]      
      }
      
      delta2[d]       <- sum(weight_smp^2) / (weight_sum[d]^2)
      gamma_weight[d] <- sigmau2est / (sigmau2est + sigmae2est * delta2[d])
      weight_smp_diag <- if(length(weight_smp) == 1) {
        matrix(weight_smp, ncol = 1, nrow = 1)
      } else {
        diag(weight_smp)
      }
      dep_var_ast     <- dep_smp - gamma_weight[d] * mean_dep[d]
      indep_weight    <- t(indep_smp) %*% weight_smp_diag
      indep_var_ast   <- indep_smp - matrix(rep(gamma_weight[d] * mean_indep[d,], framework$n_smp[d]),
                            nrow = framework$n_smp[d], byrow = TRUE)
      
      num <- num + (indep_weight %*% dep_var_ast)
      den <- den + (indep_weight %*% indep_var_ast)
      
    }
    
    
    betas    <- solve(den) %*% num
    # Random effect: vector with zeros for all domains, filled with
    rand_eff <- rep(0, length(unique(framework$pop_domains_vec)))
    # random effect for in-sample domains (dist_obs_dom)
    rand_eff[framework$dist_obs_dom] <- gamma_weight * (mean_dep - mean_indep %*% betas)
    
    
    return(list(betas      = betas,
                sigmae2est = sigmae2est,
                sigmau2est = sigmau2est,
                rand_eff   = rand_eff,
                gammaw     = gamma_weight,
                delta2     = delta2
    )
    )
  }
  
} # End model_par



# Function gen_model calculates the parameters in the generating model.
# See Molina and Rao (2010) p. 375 (20)
gen_model <- function(fixed,
                      framework,
                      model_par) {
  if(is.null(framework$weights)) {
    # Parameter for calculating variance of new random effect
    gamma <- model_par$sigmau2est / (model_par$sigmau2est +
                                       model_par$sigmae2est / framework$n_smp)
    # Variance of new random effect
    sigmav2est <- model_par$sigmau2est * (1 - gamma)
    # Random effect in constant part of y for in-sample households
    rand_eff_pop <- rep(model_par$rand_eff, framework$n_pop)
    # Model matrix for population covariate information
    framework$pop_data[[paste0(fixed[2])]] = seq_len(nrow(framework$pop_data))
    X_pop <- model.matrix(fixed, framework$pop_data)
    
    # Constant part of predicted y
    mu_fixed <- X_pop %*% model_par$betas
    mu <- mu_fixed + rand_eff_pop
    
    return(list(sigmav2est = sigmav2est, mu = mu, mu_fixed = mu_fixed))
  } else {
    # Parameter for calculating variance of new random effect
    gamma <- model_par$gammaw 
    # Variance of new random effect
    sigmav2est <- model_par$sigmau2est * (1 - gamma) 
    # Random effect in constant part of y for in-sample households
    rand_eff_pop <- rep(model_par$rand_eff, framework$n_pop) ####### change
    # Model matrix for population covariate information
    framework$pop_data[[paste0(fixed[2])]] <- seq_len(nrow(framework$pop_data))
    X_pop <- model.matrix(fixed, framework$pop_data)
    
    # Constant part of predicted y
    mu_fixed <- X_pop %*% model_par$betas
    mu <- mu_fixed + rand_eff_pop
    
    
    return(list(sigmav2est = sigmav2est, mu = mu, mu_fixed = mu_fixed))
  }
  
} # End gen_model


# Monte-Carlo approximation ----------------------------------------------------

# The function approximates the expected value (Molina and Rao (2010)
# p.372 (6)). For description of monte-carlo simulation see Molina and
# Rao (2010) p. 373 (13) and p. 374-375
monte_carlo <- function(transformation,
                        L,
                        framework,
                        lambda,
                        shift,
                        model_par,
                        gen_model) {
  
  # Preparing matrices for indicators for the Monte-Carlo simulation
  #   quant10s = quant25s = mediane = quant75s = quant90s = ginis = qsrs = pgaps =
  #     hcrs = means <- matrix(nrow=framework$N_dom_pop, ncol=L)
  
  ests_mcmc <- array(dim = c(framework$N_dom_pop,L,length(framework$indicator_names)))
  
  for (l in seq_len(L)) {
    
    # Errors in generating model: individual error term and random effect
    # See below for function errors_gen.
    errors <- errors_gen(framework = framework,
                         model_par = model_par,
                         gen_model = gen_model
    )
    
    # Prediction of population vector y
    # See below for function prediction_y.
    population_vector <- prediction_y(transformation = transformation,
                                      lambda         = lambda,
                                      shift          = shift,
                                      gen_model      = gen_model,
                                      errors_gen     = errors,
                                      framework      = framework
    )
    
    # Calculation of indicators for each Monte Carlo population
    ests_mcmc[,l,] <- matrix(nrow=framework$N_dom_pop, data = unlist(lapply(
      framework$indicator_list, function(f, threshold){matrix(nrow=framework$N_dom_pop, 
                                                              data = unlist(tapply(
                                                                population_vector,
                                                                framework$pop_domains_vec, 
                                                                f, 
                                                                threshold = framework$threshold,
                                                                simplify = TRUE)),byrow = TRUE)}, 
      threshold = framework$threshold)))
    
  } # End for loop
  
  
  # Point estimations of indicators by taking the mean
  
  point_estimates <- data.frame(Domain = unique(framework$pop_domains_vec) , 
                                apply(ests_mcmc, c(3), rowMeans))
  colnames(point_estimates) <- c("Domain", framework$indicator_names)
  return(point_estimates)
  
} # End Monte-Carlo


# The function errors_gen returns error terms of the generating model.
# See Molina and Rao (2010) p. 375 (20)

errors_gen <- function(framework, model_par, gen_model) {
  # individual error term in generating model epsilon
  epsilon <- rnorm(framework$N_pop, 0, sqrt(model_par$sigmae2est))
  
  # empty vector for new random effect in generating model
  vu <- vector(length = framework$N_pop)
  # new random effect for out-of-sample domains
  vu[!framework$obs_dom] <- rep(rnorm(framework$N_dom_unobs,
                                      0,
                                      sqrt(model_par$sigmau2est)
  ),
  framework$n_pop[!framework$dist_obs_dom]
  )
  # new random effect for in-sample-domains
  vu[framework$obs_dom]  <- rep(rnorm(rep(1, framework$N_dom_smp),
                                      0,
                                      sqrt(gen_model$sigmav2est)
  ),
  framework$n_pop[framework$dist_obs_dom]
  )
  
  return(list(epsilon = epsilon, vu = vu))
  
} # End errors_gen

# The function prediction_y returns a predicted income vector which can be used
# to calculate indicators. Note that a whole income vector is predicted without
# distinction between in- and out-of-sample domains.
prediction_y <- function(transformation,
                         lambda,
                         shift,
                         gen_model,
                         errors_gen,
                         framework) {
  
  # predicted population income vector
  y_pred <- gen_model$mu + errors_gen$epsilon + errors_gen$vu
  
  # back-transformation of predicted population income vector
  y_pred <- back_transformation(y              = y_pred,
                                transformation = transformation,
                                lambda         = lambda,
                                shift          = shift
  )
  y_pred[!is.finite(y_pred)] <- 0
  
  return(y_pred)
} # End prediction_y


