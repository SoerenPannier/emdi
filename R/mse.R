prasad_rao <- function(framework, sigmau2, combined_data) {

  g1 <- rep(0, framework$m)
  g2 <- rep(0, framework$m)
  g3 <- rep(0, framework$m)
  mse <- rep(0, framework$m)
  # Inverse of total variance
  Vi <- 1/(sigmau2 + framework$vardir)
  # Shrinkage factor
  Bd <- framework$vardir/(sigmau2 + framework$vardir)
  # Squared inverse of total variance
  SumAD2 <- sum(Vi^2)
  # X'Vi
  XtVi <- t(Vi * framework$model_X)
  # (X'ViX)^-1
  Q <- solve(XtVi %*% framework$model_X)

  # 2 divided by squared inverse of total variance
  VarA <- 2/SumAD2

  for (d in 1:framework$m) {
    # Variance due to random effects: vardir * gamma
    g1[d] <- framework$vardir[d] * (1 - Bd[d])
    # Covariate for single domain
    xd <- matrix(framework$model_X[d, ], nrow = 1, ncol = framework$p)
    # Variance due to the estimation of beta
    g2[d] <- (Bd[d]^2) * xd %*% Q %*% t(xd)
    # Variance due to the estimation of the variance of the random effects
    g3[d] <- (Bd[d]^2) * VarA/(sigmau2 + framework$vardir[d])
    # Prasad-Rao estimator
    mse[d] <- g1[d] + g2[d] + 2 * g3[d]
  }

  MSE_data <- data.frame(Domain = combined_data[[framework$domains]])
  MSE_data$Direct <- NA
  MSE_data$Direct[framework$obs_dom == TRUE] <- framework$vardir

  # Small area MSE
  MSE_data$FH[framework$obs_dom == TRUE] <- mse
  MSE_data$Out[framework$obs_dom == TRUE] <- 0


  if (!all(framework$obs_dom == TRUE)) {
    h <- rep(0, framework$M - framework$m)
    mse_out <- rep(0, framework$M - framework$m)
    # Covariates for out-of-sample domains
    pred_data_tmp <- combined_data[framework$obs_dom == FALSE,]
    pred_data_tmp <- data.frame(pred_data_tmp, helper = rnorm(1,0,1))
    lhs(framework$formula) <- quote(helper)
    pred_data <- makeXY(formula = framework$formula, data = pred_data_tmp)
    pred_X <- pred_data$x

    for (d_out in 1:(framework$M - framework$m)) {
      xd_out <- matrix(pred_X[d_out, ], nrow = 1, ncol = framework$p)
      h[d_out] <- xd_out %*% Q %*% t(xd_out)
      mse_out[d_out] <- sigmau2 + h[d_out]
    }

    MSE_data$FH[framework$obs_dom == FALSE] <- mse_out
    MSE_data$Out[framework$obs_dom == FALSE] <- 1
  }


  return(MSE_data)
  }


datta_lahiri <- function(framework, sigmau2, combined_data) {


  g1 <- rep(0, framework$m)
  g2 <- rep(0, framework$m)
  g3 <- rep(0, framework$m)
  mse <- rep(0, framework$m)
  Vi <- 1/(sigmau2 + framework$vardir)
  Bd <- framework$vardir/(sigmau2 + framework$vardir)
  SumAD2 <- sum(Vi^2)
  XtVi <- t(Vi * framework$model_X)
  Q <- solve(XtVi %*% framework$model_X)

  VarA <- 2/SumAD2
  b <- (-1) * sum(diag(Q %*% (t((Vi^2) * framework$model_X) %*% framework$model_X)))/SumAD2
  for (d in 1:framework$m) {
    g1[d] <- framework$vardir[d] * (1 - Bd[d])
    xd <- matrix(framework$model_X[d, ], nrow = 1, ncol = framework$p)
    g2[d] <- (Bd[d]^2) * xd %*% Q %*% t(xd)
    g3[d] <- (Bd[d]^2) * VarA/(sigmau2 + framework$vardir[d])
    mse[d] <- g1[d] + g2[d] + 2 * g3[d] - b * (Bd[d]^2)
  }

  MSE_data <- data.frame(Domain = combined_data[[framework$domains]])
  MSE_data$Direct <- NA
  MSE_data$Direct[framework$obs_dom == TRUE] <- framework$vardir

  # Small area MSE
  MSE_data$FH[framework$obs_dom == TRUE] <- mse
  MSE_data$Out[framework$obs_dom == TRUE] <- 0

  if (!all(framework$obs_dom == TRUE)) {
    h <- rep(0, framework$M - framework$m)
    mse_out <- rep(0, framework$M - framework$m)
    # Covariates for out-of-sample domains
    pred_data_tmp <- combined_data[framework$obs_dom == FALSE,]
    pred_data_tmp <- data.frame(pred_data_tmp, helper = rnorm(1,0,1))
    lhs(framework$formula) <- quote(helper)
    pred_data <- makeXY(formula = framework$formula, data = pred_data_tmp)
    pred_X <- pred_data$x

    for (d_out in 1:(framework$M - framework$m)) {
      xd_out <- matrix(pred_X[d_out, ], nrow = 1, ncol = framework$p)
      h[d_out] <- xd_out %*% Q %*% t(xd_out)
      mse_out[d_out] <- sigmau2 + b + h[d_out]
    }

    MSE_data$FH[framework$obs_dom == FALSE] <- mse_out
    MSE_data$Out[framework$obs_dom == FALSE] <- 1
  }

  return(MSE_data)
}


li_lahiri <- function(framework, sigmau2, combined_data, method) {


    prasad_rao <- prasad_rao(framework = framework, sigmau2 = sigmau2,
                             combined_data = combined_data)
    mse <- prasad_rao$FH[framework$obs_dom == TRUE]
    X <- framework$model_X
    psi <- matrix(c(framework$vardir), framework$m, 1)
    Y <- matrix(c(framework$direct), framework$m, 1)
    Z.area <- diag(1, framework$m)
    I <- diag(1, framework$m)
    # Shrinkage factor
    Bd <- framework$vardir/(sigmau2 + framework$vardir)
    #V is the variance covariance matrix
    V <- sigmau2 * Z.area%*%t(Z.area) + I * psi[,1]
    Vi <- solve(V)
    Xt <- t(X)
    XVi <- Xt%*%Vi
    Q <- solve(XVi%*%X)
    P <- Vi - (Vi%*%X%*%Q%*%XVi)
    b.s <- Q%*%XVi%*%Y

    if (method == "ampl") {
      Bias <- (sum(diag(P - Vi)) + (2/sigmau2)) / sum(diag(Vi^2))
      for (d in 1:framework$m) {
        # Adjusted mse
        mse[d] <- mse[d] - (Bd[d]^2) * Bias
      }
    } else if (method == "amrl") {
      Bias <- (2/sigmau2) / sum(diag(Vi^2))
      for (d in 1:framework$m) {
        # Adjusted mse
        mse[d] <- mse[d] - (Bd[d]^2) * Bias
      }
    }

    MSE_data <- prasad_rao
    MSE_data$FH[framework$obs_dom == TRUE] <- mse
    MSE_data$Out[framework$obs_dom == TRUE] <- 0


    if (!all(framework$obs_dom == TRUE)) {
      MSE_data$FH[framework$obs_dom == FALSE] <- NA
      MSE_data$Out[framework$obs_dom == FALSE] <- 1

    cat("Please note that only for in-sample-domains a correction following
        Li and Lahiri (2010) is implemented. For the out-of-sample domains,
        no estimate for the MSE is returned. For the reference see help(FH_AK).")
    }

    return(MSE_data)
}


yoshimori_lahiri <- function(framework, sigmau2, combined_data, method) {

  prasad_rao <- prasad_rao(framework = framework, sigmau2 = sigmau2,
                           combined_data = combined_data)
  mse <- prasad_rao$FH[framework$obs_dom == TRUE]
  X <- framework$model_X
  psi <- matrix(c(framework$vardir), framework$m, 1)
  Y <- matrix(c(framework$direct), framework$m, 1)
  Z.area <- diag(1, framework$m)
  I <- diag(1, framework$m)
  # Shrinkage factor
  Bd <- framework$vardir/(sigmau2 + framework$vardir)
  #V is the variance covariance matrix
  V <- sigmau2 * Z.area%*%t(Z.area) + I * psi[,1]
  Vi <- solve(V)
  Xt <- t(X)
  XVi <- Xt%*%Vi
  Q <- solve(XVi%*%X)
  P <- Vi - (Vi%*%X%*%Q%*%XVi)
  b.s <- Q%*%XVi%*%Y

  if (method == "ampl_yl") {
    Bias <- (sum(diag(P - Vi))) / sum(diag(Vi^2))
    for (d in 1:framework$m) {
      # Adjusted mse
      mse[d] <- mse[d] - (Bd[d]^2) * Bias
    }
  } else if (method == "amrl_yl") {
      mse <- mse
  }

  MSE_data <- prasad_rao
  MSE_data$FH[framework$obs_dom == TRUE] <- mse
  MSE_data$Out[framework$obs_dom == TRUE] <- 0


  if (!all(framework$obs_dom == TRUE)) {
    MSE_data$FH[framework$obs_dom == FALSE] <- NA
    MSE_data$Out[framework$obs_dom == FALSE] <- 1

    cat("Please note that only for in-sample-domains a correction following
        Yoshimori and Lahiri (2014) is implemented. For the out-of-sample domains,
        no estimate for the MSE is returned. For the reference see help(FH_AK).")
  }

  return(MSE_data)
}


slud_maiti <- function(framework, sigmau2, eblup, combined_data) {

  # MSE estimation
  nu <- framework$model_X%*%eblup$coefficients$coefficients
  gamma <- eblup$gamma
  tau <- sigmau2 + framework$vardir
  # Variance of beta
  X <- framework$model_X
  psi <- matrix(c(framework$vardir), framework$m, 1)
  Y <- matrix(c(framework$direct), framework$m, 1)
  Z.area <- diag(1, framework$m)
  I <- diag(1, framework$m)
  #V is the variance covariance matrix
  V <- sigmau2 * Z.area%*%t(Z.area) + I * psi[,1]
  Vi <- solve(V)
  Xt <- t(X)
  XVi <- Xt%*%Vi
  Q <- solve(XVi%*%X)
  Var.beta <- Q
  # Variance of sigmau2
  # Identity matrix mxm
  D <- diag(1, framework$m)
  Deriv1 <- solve((sigmau2 * D) + diag(c(framework$vardir), framework$m))
  ### Inverse of fisher information matrix. That is var. sigma2u
  Var.sigma <- ((1/2) * sum(diag(Deriv1%*%Deriv1)))^(-1)

  tmp <- NULL
  for (i in 1:framework$m) {
    tmp[i] <- (t(framework$model_X)[,i]%*%Q%*%framework$model_X[i,])/(tau[i]^2)
  }

  mse <- NULL
  for (j in 1:framework$m) {
    mse[j] <- exp(2 * (nu[j,1] + sigmau2)) * (1 - exp(-gamma[j] * framework$vardir[j])) +
      ((framework$vardir[j]^2)/tau[j]^2) * exp(2 * nu[j,1] + sigmau2 * (1 + gamma[j])) * (t(framework$model_X)[,j]%*%Q%*%framework$model_X[j,]) +
      Var.sigma * ((framework$vardir[j]^2)/tau[j]^2) * exp(2 * nu[j,1] + sigmau2 * (1 + gamma[j])) *
      ((1/4) * (1 + 3 * gamma[j])^2 + (1/tau[j])) - exp(2 * (nu[j,1] + sigmau2)) *
      (2 * (1 - exp(-gamma[j] * framework$vardir[j])) * (t(framework$model_X)[,j]%*%Q%*%framework$model_X[j,]) - Var.sigma *
         (2 + (((framework$vardir[j]^2)/tau[j]^2) - 2) * exp(-gamma[j] * framework$vardir[j])) * sum(tmp) +
         Var.sigma * (2 + (((2 * (framework$vardir[j]^2))/(tau[j]^2)) - 2) * exp(-gamma[j] * framework$vardir[j]) -
                        ((framework$vardir[j]^2)/(tau[j]^3)) * exp(-gamma[j] * framework$vardir[j]) * (1 + ((framework$vardir[j]^2)/(2 * tau[j])))))
  }


  MSE_data <- data.frame(Domain = combined_data[[framework$domains]])
  MSE_data$Direct <- NA
  MSE_data$Direct[framework$obs_dom == TRUE] <- framework$vardir
  MSE_data$FH[framework$obs_dom == TRUE] <- mse
  MSE_data$Out[framework$obs_dom == TRUE] <- 0

  if (!all(framework$obs_dom == TRUE)) {
    MSE_data$FH[framework$obs_dom == FALSE] <- NA
    MSE_data$Out[framework$obs_dom == FALSE] <- 1

    cat("Please note that a MSE is only returned for in-sample domains.
        For more information see help(FH_AK).")
  }

  return(MSE_data)

  }



analytical_mse <- function(framework, sigmau2, combined_data,
                           method) {

    if (method == "reml") {
      MSE_data <- prasad_rao(framework = framework, sigmau2 = sigmau2,
                             combined_data = combined_data)
      MSE_method <- "prasad-rao"
    } else if (method == "amrl" | method == "ampl") {
      MSE_data <- li_lahiri(framework = framework, sigmau2, combined_data,
                            method = method)
      MSE_method <- "li-lahiri"
    } else if (method == "ampl_yl") {
      MSE_data <- yoshimori_lahiri(framework = framework, sigmau2, combined_data,
                            method = method)
      MSE_method <- "yoshimori-lahiri"
    } else if (method == "amrl_yl") {
      MSE_data <- yoshimori_lahiri(framework = framework, sigmau2, combined_data,
                                   method = method)
      MSE_method <- "prasad-rao"
    } else if (method == "ml") {
      MSE_data <- datta_lahiri(framework = framework, sigmau2, combined_data)
      MSE_method <- "datta-lahiri"
    }

  mse_out <- list(MSE_data = MSE_data,
                  MSE_method = MSE_method)

  return(mse_out)
  }


boot_arcsin <- function(sigmau2, vardir, combined_data, framework,
                        eblup, B, method,
                        interval, alpha) {


  M <- framework$M
  m <- framework$m
  vardir <- framework$vardir
  eff_smpsize <- framework$eff_smpsize
  x <- framework$model_X

  Li <- rep(NA, M)
  Ui <- rep(NA, M)

  ### Bootstrap
    ti <- matrix(NA, M, B)
    boots_est <- matrix(NA, M, B)
    boots_par <- matrix(NA, M, B)
    in_sample <- framework$obs_dom == TRUE
    out_sample <- framework$obs_dom == FALSE

    for (b in 1:B){

      #set.seed(b)

      v_boot <- rnorm(M, 0, sqrt(sigmau2))
      e_boot <- rnorm(m, 0, sqrt(vardir))

      # Get covariates for all domains
      pred_data_tmp <- combined_data
      pred_data_tmp <- data.frame(pred_data_tmp, helper = rnorm(1,0,1))
      lhs(framework$formula) <- quote(helper)
      pred_data <- makeXY(formula = framework$formula, data = pred_data_tmp)

      pred_X <- pred_data$x


      Xbeta_boot <- pred_X %*% eblup$coefficients$coefficients


      ## Theta under transformation
      theta <- Xbeta_boot[, 1] + v_boot

      ## Truncation
      true_value_boot <- Xbeta_boot + v_boot
      true_value_boot[true_value_boot < 0] <- 0
      true_value_boot[true_value_boot > (pi / 2)] <- (pi / 2)

      ## Back-transformation
      true_value_boot <- (sin(true_value_boot))^2
      boots_par[,b] <- true_value_boot
      boots_par[,b] <- Xbeta_boot + v_boot

      ystar <- Xbeta_boot[in_sample] + v_boot[in_sample] + e_boot

      ## Estimation of beta_boot
      framework2 <- framework
      framework2$direct <- ystar
      sigmau2_boot <- wrapper_estsigmau2(framework = framework2, method = method,
                                    interval = interval)


      ## Computation of the coefficients'estimator (Bstim)
      D <- diag(1, m)
      V <- sigmau2_boot*D%*%t(D) + diag(as.numeric(vardir))
      Vi <- solve(V)
      Q <- solve(t(x)%*%Vi%*%x)
      Beta.hat_boot <- Q%*%t(x)%*%Vi%*%ystar

      ## Computation of the EBLUP
      res <- ystar - c(x%*%Beta.hat_boot)
      Sigma.u <- sigmau2_boot*D
      u.hat <- Sigma.u%*%t(D)%*%Vi%*%res

      ## Small area mean
      est_mean_boot <- x%*%Beta.hat_boot+D%*%u.hat
      Bi <- as.numeric(vardir)/(sigmau2_boot + as.numeric(vardir))
      ti[in_sample, b] <- (theta[in_sample] - est_mean_boot)/sqrt(as.numeric(vardir)*(1 - Bi))

      ## Synthetic prediction for out-of-sample
      pred_out_boot <- pred_X%*%Beta.hat_boot
      ti[out_sample, b]<-(theta[out_sample] - pred_out_boot[out_sample])/sqrt(sigmau2_boot)

      print(b)
    } # End of bootstrap runs

    qi <- matrix(NA, M, 2)

    for (i in 1:M) {
      qi[i,1] <- quantile(ti[i,], prob = alpha/2, na.rm = T)
      qi[i,2] <- quantile(ti[i,], prob = (1 - alpha/2), na.rm = T)
    }

    Li <- matrix(NA, M, 1)
    Ui <- matrix(NA, M, 1)

    Di <- rep(NA, M)
    Di[in_sample] <- vardir
    Di[out_sample] <- (1 / (4 * mean(combined_data[in_sample, eff_smpsize])))
    Bi.tot <- as.numeric(Di) / (sigmau2 + as.numeric(Di))


    Li <- (eblup$EBLUP_data$FH + qi * sqrt(Di * (1 - Bi.tot)))[,1]
    Ui <- (eblup$EBLUP_data$FH + qi * sqrt(Di * (1 - Bi.tot)))[,2]

    ### Truncation
    Li[Li < 0] <- 0
    Ui[Ui > (pi / 2)] <- (pi / 2)

    ### Back-transformation
    Li <- (sin(Li))^2
    Ui <- (sin(Ui))^2

    conf_int <- data.frame(Li = Li, Ui = Ui)
}

boot_arcsin_2 <- function(sigmau2, vardir, combined_data, framework,
                          eblup, eblup_corr, B, method,
                          interval, alpha) {


  # Gonzales Bootstrap fuer arcsin
  M <- framework$M
  m <- framework$m
  vardir <- framework$vardir
  eff_smpsize <- framework$eff_smpsize
  x <- framework$model_X

  ### Bootstrap
  in_sample <- framework$obs_dom == TRUE
  out_sample <- framework$obs_dom == FALSE

  # Ergebnissmatrixen
  true_value_boot <- matrix(NA, ncol = B, nrow = M)
  est_value_boot  <- matrix(NA, ncol = B, nrow = M)

  for (b in 1:B){

    #set.seed(b)

    v_boot <- rnorm(M, 0, sqrt(sigmau2))
    e_boot <- rnorm(m, 0, sqrt(vardir))

    # Get covariates for all domains
    pred_data_tmp <- combined_data
    pred_data_tmp <- data.frame(pred_data_tmp, helper = rnorm(1,0,1))
    lhs(framework$formula) <- quote(helper)
    pred_data <- makeXY(formula = framework$formula, data = pred_data_tmp)

    pred_X <- pred_data$x

    Xbeta_boot <- pred_X %*% eblup$coefficients$coefficients

    ## True Value for bootstraps
    ## Truncation
    true_value_boot_trans <- Xbeta_boot + v_boot
    true_value_boot_trans[true_value_boot_trans < 0] <- 0
    true_value_boot_trans[true_value_boot_trans > (pi / 2)] <- (pi / 2)

    ## Back-transformation
    ## true values without correction
    true_value_boot[,b] <- (sin(true_value_boot_trans))^2

    ystar_trans <- Xbeta_boot[in_sample] + v_boot[in_sample] + e_boot

    ## Estimation of sigmau2_boot auf transformierter Ebene
    framework2 <- framework
    framework2$direct <- ystar_trans
    sigmau2_boot <- wrapper_estsigmau2(framework = framework2, method = method,
                                       interval = interval)

    ## Computation of the coefficients'estimator (Bstim)
    D <- diag(1, m)
    V <- sigmau2_boot*D%*%t(D) + diag(as.numeric(vardir))
    Vi <- solve(V)
    Q <- solve(t(x)%*%Vi%*%x)
    Beta.hat_boot <- Q%*%t(x)%*%Vi%*%ystar_trans

    ## Computation of the EBLUP
    res <- ystar_trans - c(x%*%Beta.hat_boot)
    Sigma.u <- sigmau2_boot*D
    u.hat <- Sigma.u%*%t(D)%*%Vi%*%res

    ## Estimated Small area mean on transformed scale for the out and in sample values
    est_mean_boot_trans <- x%*%Beta.hat_boot+D%*%u.hat
    pred_out_boot_trans <- pred_X%*%Beta.hat_boot

    est_value_boot_trans <- rep(NA, M)
    est_value_boot_trans[in_sample] <- est_mean_boot_trans
    est_value_boot_trans[out_sample]<- pred_out_boot_trans[out_sample]

    gamma_trans <- as.numeric(vardir)/(sigmau2_boot + as.numeric(vardir))
    est_value_boot_trans_var <- sigmau2_boot * gamma_trans
    est_value_boot_trans_var_<- rep(0, M)
    est_value_boot_trans_var_[in_sample] <- est_value_boot_trans_var

    # backtransformation
    int_value <- NULL
    for (i in 1:M) {

      if(in_sample[i] == T){
        mu_dri <- est_value_boot_trans
        # Get value of first domain
        mu_dri <- mu_dri[i]

        Var_dri <- est_value_boot_trans_var_
        Var_dri <- as.numeric(Var_dri[i])

        integrand <- function(x, mean, sd){sin(x)^2 * dnorm(x, mean = mu_dri,
                                                            sd = sqrt(Var_dri))}

        upper_bound <- min(mean(framework$direct) + 10 * sd(framework$direct),
                           mu_dri + 100 * sqrt(Var_dri))
        lower_bound <- max(mean(framework$direct) - 10 * sd(framework$direct),
                           mu_dri - 100 * sqrt(Var_dri))

        int_value <- c(int_value, integrate(integrand, lower = 0, upper = pi/2)$value)
      }else{
        int_value <- c(int_value, (sin(est_value_boot_trans[i]))^2)
      }
    }

    est_value_boot[,b] <- int_value

    print(b)
  } # End of bootstrap runs

  # KI
  Li <- rep(NA, M)
  Ui <- rep(NA, M)

  for(ii in 1:M){
    Li[ii] <- eblup_corr[ii] + quantile(est_value_boot[ii,] - true_value_boot[ii,], 0.025)
    Ui[ii] <- eblup_corr[ii] + quantile(est_value_boot[ii,] - true_value_boot[ii,], 0.975)
  }

  conf_int <- data.frame(Li = Li, Ui = Ui)

  Quality_MSE<-function(estimator, TrueVal, B){
    RMSE<-rep(NA,dim(estimator)[1])
    for(ii in 1:dim(estimator)[1]){
      RMSE[ii]<-(1/B*sum(((estimator[ii,]-TrueVal[ii,]))^2))
    }
    RMSE
  }

  mse <- Quality_MSE(est_value_boot, true_value_boot, B)

  MSE_data <- data.frame(Domain = combined_data[[framework$domains]])
  MSE_data$Var <- NA
  MSE_data$Var[framework$obs_dom == TRUE] <- framework$vardir

  # Small area MSE
  MSE_data$MSE <- mse
  MSE_data$Out[framework$obs_dom == TRUE] <- 0
  MSE_data$Out[framework$obs_dom == FALSE] <- 1

  return(list(conf_int, MSE_data))

}



jiang_jackknife <- function(framework, combined_data, sigmau2, eblup, transformation,
                            vardir, method, interval) {


  # this MSE estimator can leed to negative values
  m <- framework$m
  jack_sigmau2 <- vector(length = m)
  diff_jack_eblups <- data.frame(row.names = 1:m)
  diff_jack_g1 <- data.frame(row.names = 1:m)

  g1 <- rep(0, framework$m)
  jack_mse <- rep(0, framework$m)
  # Inverse of total variance
  Vi <- 1/(sigmau2 + framework$vardir)
  # Shrinkage factor
  Bd <- framework$vardir/(sigmau2 + framework$vardir)


  for (d in 1:framework$m) {
    # Variance due to random effects: vardir * gamma
    g1[d] <- framework$vardir[d] * (1 - Bd[d])
  }

  for (domain in 1:m) {
    print(domain)


    data_insample <- combined_data[framework$obs_dom,]
    data_tmp <- data_insample[-domain,]

    # Framework with temporary data
    framework_tmp <- framework_FH(combined_data = data_tmp, fixed = framework$formula,
                              vardir = vardir, domains = framework$domains,
                              transformation = transformation,
                              eff_smpsize = framework$eff_smpsize)
    # Estimate sigma u
    sigmau2_tmp <- wrapper_estsigmau2(framework = framework_tmp, method = method,
                                  interval = interval)
    jack_sigmau2[domain] <- sigmau2_tmp

    Vi_tmp <- 1/(sigmau2_tmp + framework$vardir)
    # Shrinkage factor
    Bd_tmp <- framework$vardir/(sigmau2_tmp + framework$vardir)

    g1_tmp <- rep(0, framework$m)
    for (d_tmp in 1:framework$m) {
      g1_tmp[d_tmp] <- framework$vardir[d_tmp] * (1 - Bd_tmp[d_tmp])
    }

    # G1
    diff_jack_g1[, paste0(domain)] <- g1_tmp - g1

    # Standard EBLUP
    framework_insample <- framework_FH(combined_data = data_insample, fixed = framework$formula,
                                       vardir = vardir, domains = framework$domains,
                                       transformation = transformation,
                                       eff_smpsize = framework$eff_smpsize)
    eblup_tmp <- eblup_FH(framework = framework_insample, sigmau2 = sigmau2_tmp,
                      combined_data = data_insample)
    diff_jack_eblups[, paste0(domain)] <- eblup_tmp$EBLUP_data$FH - eblup$EBLUP_data$FH[eblup$EBLUP_data$Out == 0]
  }

  jack_mse <- g1 - ((m - 1)/m) * rowSums(diff_jack_g1) + ((m - 1)/m) * rowSums(diff_jack_eblups^2)


  MSE_data <- data.frame(Domain = combined_data[[framework$domains]])
  MSE_data$Direct <- NA
  MSE_data$Direct[framework$obs_dom == TRUE] <- framework$vardir

  # Jackknife MSE
  MSE_data$FH[framework$obs_dom == TRUE] <- jack_mse
  MSE_data$Out[framework$obs_dom == TRUE] <- 0


  if (!all(framework$obs_dom == TRUE)) {
    MSE_data$FH[framework$obs_dom == FALSE] <- NA
    MSE_data$Out[framework$obs_dom == FALSE] <- 1

    cat("Please note that the jackknife MSE is only available for in-sample
        domains.")
  }

  mse_out <- list(MSE_data = MSE_data,
                  MSE_method = "jackknife")

  return(mse_out)
}

chen_weighted_jackknife <- function(framework, combined_data, sigmau2, eblup, transformation,
                                    vardir, method, interval) {

  # implementiert nach:
  # Chen S., Lahiri P. (2002), A Weighted Jackknife MSPE Estimator in Small-Area Estimation,
  # "Proceeding of the Section on Survey Research Methods", American Statistical Association,
  # pp. 473-477.

  # hier wurden erstmal die weights aus d-1/d festgelegt, es gibt auch andere Optionen

  m <- framework$m
  jack_sigmau2 <- vector(length = m)
  diff_jack_eblups <- data.frame(row.names = 1:m)
  diff_jack_g1 <- data.frame(row.names = 1:m)
  diff_jack_g2 <- data.frame(row.names = 1:m)

  g1 <- rep(0, framework$m)
  jack_mse <- rep(0, framework$m)
  jack_mse_weighted <- rep(0, framework$m)
  # Inverse of total variance
  Vi <- 1/(sigmau2 + framework$vardir)
  # Shrinkage factor
  Bd <- framework$vardir/(sigmau2 + framework$vardir)


  for (d in 1:framework$m) {
    # Variance due to random effects: vardir * gamma
    g1[d] <- framework$vardir[d] * (1 - Bd[d])
  }


  Nenner <- rep(0, framework$m)
  for (d in 1:framework$m) {
    Nenner[d] <- (framework$model_X[d,] %*% framework$model_X[d,]) /
      (framework$vardir[d] + sigmau2)
  }


  g2 <- rep(0, framework$m)
  for (d in 1:framework$m) {
    # Variance due to beta estimation
    g2[d] <- (Bd[d])^2 * framework$model_X[d,] %*% framework$model_X[d,] * (sum(Nenner))^(-1)
  }

  for (domain in 1:m) {
    print(domain)


    data_insample <- combined_data[framework$obs_dom,]
    data_tmp <- data_insample[-domain,]

    # Framework with temporary data
    framework_tmp <- framework_FH(combined_data = data_tmp, fixed = framework$formula,
                                  vardir = vardir, domains = framework$domains,
                                  transformation = transformation,
                                  eff_smpsize = framework$eff_smpsize)
    # Estimate sigma u
    sigmau2_tmp <- wrapper_estsigmau2(framework = framework_tmp, method = method,
                                      interval = interval)
    jack_sigmau2[domain] <- sigmau2_tmp

    Vi_tmp <- 1/(sigmau2_tmp + framework$vardir)
    # Shrinkage factor
    Bd_tmp <- framework$vardir/(sigmau2_tmp + framework$vardir)

    g1_tmp <- rep(0, framework$m)
    for (d_tmp in 1:framework$m) {
      g1_tmp[d_tmp] <- framework$vardir[d_tmp] * (1 - Bd_tmp[d_tmp])
    }

    Nenner_tmp <- rep(0, framework$m)
    for (d_tmp in 1:framework$m) {
      Nenner_tmp[d_tmp] <- (framework$model_X[d_tmp,] %*% framework$model_X[d_tmp,]) /
        (framework$vardir[d_tmp] + sigmau2_tmp)
    }

    g2_tmp <- rep(0, framework$m)
    for (d_tmp in 1:framework$m) {
      g2_tmp[d_tmp] <- (Bd_tmp[d_tmp])^2 * framework$model_X[d_tmp,] %*% framework$model_X[d_tmp,] * (sum(Nenner_tmp))^(-1)
    }

    #G1
    diff_jack_g1[, paste0(domain)] <- g1_tmp - g1
    # negative Werte koennen erhalten werden, wenn diff_jack_g1 sehr gross ist

    #G1
    diff_jack_g2[, paste0(domain)] <- g1_tmp + g2_tmp - (g1 + g2)

    # Standard EBLUP
    framework_insample <- framework_FH(combined_data = data_insample, fixed = framework$formula,
                                       vardir = vardir, domains = framework$domains,
                                       transformation = transformation,
                                       eff_smpsize = framework$eff_smpsize)
    eblup_tmp <- eblup_FH(framework = framework_insample, sigmau2 = sigmau2_tmp,
                          combined_data = data_insample)
    diff_jack_eblups[, paste0(domain)] <- eblup_tmp$EBLUP_data$FH - eblup$EBLUP_data$FH[eblup$EBLUP_data$Out == 0]
  }

  w_u  <- c()
  v_wj <- c()

  for(i in 1:nrow(framework$model_X)){
    w_u[i] <- 1 - t(framework$model_X[i,]) %*% solve(t(framework$model_X) %*% framework$model_X) %*% framework$model_X[i,]
    v_wj[i] <- w_u[i] * (jack_sigmau2[i] - sigmau2) * (jack_sigmau2[i] - sigmau2)
  }

  jack_mse <- g1 - ((m - 1)/m) * rowSums(diff_jack_g1) + ((m - 1)/m) * rowSums(diff_jack_eblups^2)
  jack_mse_weighted <- g1 + g2 - ((m - 1)/m) * rowSums(diff_jack_g2) + ((m - 1)/m) * rowSums(diff_jack_eblups^2)

  jack_mse <- g1 - w_u * rowSums(diff_jack_g1) + w_u * rowSums(diff_jack_eblups^2)
  jack_mse_weighted <- g1 + g2 - w_u * rowSums(diff_jack_g2) + w_u * rowSums(diff_jack_eblups^2)


  # bias correction for negative values:

  neg_values <- which(jack_mse_weighted < 0)

  jack_mse_weighted_neg <- c()

  if (length(neg_values) >0 ) {

    Sig_d<- (sigmau2 + framework$vardir)* diag(m)
    Sig_d_inv <- solve(Sig_d)
    L_d  <- framework$vardir/((sigmau2 + framework$vardir)^2) * diag(m)
    b_wj <- w_u * (jack_sigmau2 - sigmau2)

    # bias fuer REML ist NULL (nur fuer REML durchfuehrbar)
    app_bias_correction <-
      sum(b_wj) * (framework$vardir^2 / (framework$vardir + sigmau2)^2) - #ueberprueft
      diag(L_d %*% Sig_d %*% t(L_d) * sum(v_wj))

    jack_mse_weighted_neg <- g1 + g2 + w_u * rowSums(diff_jack_eblups^2) - app_bias_correction
  }

  for(i in 1:m){
    if(i %in% neg_values){
      jack_mse_weighted[i] <- jack_mse_weighted_neg[i]
    }
  }

  jack_mse_weighted

  MSE_data <- data.frame(Domain = combined_data[[framework$domains]])
  MSE_data$Direct <- NA
  MSE_data$Direct[framework$obs_dom == TRUE] <- framework$vardir

  # Jackknife MSE
  MSE_data$FH[framework$obs_dom == TRUE] <- jack_mse_weighted
  MSE_data$Out[framework$obs_dom == TRUE] <- 0


  if (!all(framework$obs_dom == TRUE)) {
    MSE_data$FH[framework$obs_dom == FALSE] <- NA
    MSE_data$Out[framework$obs_dom == FALSE] <- 1

    cat("Please note that the jackknife MSE is only available for in-sample
        domains.")
  }

  mse_out <- list(MSE_data = MSE_data,
                  MSE_method = "weighted jackknife")

  return(mse_out)
}





wrapper_MSE <- function(framework, combined_data, sigmau2, vardir, eblup,
                        transformation, method, interval, mse_type) {
  MSE_data <- if (mse_type == "analytical") {
    analytical_mse(framework = framework, sigmau2 = sigmau2,
                   combined_data = combined_data, method = method)
  } else if (mse_type == "jackknife") {
    jiang_jackknife(framework = framework, combined_data = combined_data,
                    sigmau2 = sigmau2, vardir = vardir, eblup = eblup,
                    transformation = transformation, method = method,
                    interval = interval)
  } else if (mse_type == "weighted_jackknife"){
    chen_weighted_jackknife(framework = framework, combined_data = combined_data,
                            sigmau2 = sigmau2, eblup = eblup, vardir = vardir,
                            transformation = transformation, method = method,
                            interval = interval)
  }

  return(MSE_data)
}
