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

prasad_rao_spatial <- function(framework, sigmau2, combined_data, method) {
  
  g1 <- rep(0, framework$m)
  g2 <- rep(0, framework$m)
  g3 <- rep(0, framework$m)
  g4 <- rep(0, framework$m)
  mse.help <- rep(0, framework$m)
  mse <- rep(0, framework$m)
  
  
  # Auxiliary calculations
  D <- diag(1, framework$m)
  Xt <- t(framework$model_X)
  Wt <- t(framework$W)
  A <- solve((D - sigmau2$rho*Wt)%*%(D - sigmau2$rho*framework$W))
  G <- sigmau2$sigmau2*A
  # Total variance
  V<- G + D*framework$vardir
  # Inverse of total variance
  Vi<-solve(V)
  # Inverse of X'ViX
  XtVi<-Xt%*%Vi
  Q <- solve(t(framework$model_X)%*%Vi%*%framework$model_X)
  
  Ga <- G - G%*%Vi%*%G
  
  # g1
  for (d in 1:framework$m){
    g1[d]<-Ga[d,d]} # g1sp contains the diagonal elements of Ga

  Gb <- G%*%Vi%*%framework$model_X
  Xa <- matrix(0,1,framework$p)
  
  # g2
  for (d in 1:framework$m){
    Xa[1,] <- framework$model_X[d,]-Gb[d,]
    g2[d] <- Xa%*%Q%*%t(Xa)}
  
  # Auxiliary calculations for g3
  derRho<-2*sigmau2$rho*Wt%*%framework$W-framework$W-Wt
  Amat <- (-1)*sigmau2$sigmau2*(A%*%derRho%*%A)
  P <- Vi-t(XtVi)%*%Q%*%XtVi
  PA <- P%*%A
  PAmat <- P%*%Amat
  
  Idev<-matrix(0,2,2)
  Idev[1,1] <- (0.5)*sum(diag((PA%*%PA)))
  Idev[1,2] <- (0.5)*sum(diag((PA%*%PAmat)))
  Idev[2,1] <- Idev[1,2]
  Idev[2,2] <- (0.5)*sum(diag((PAmat%*%PAmat)))
  Idevi <- solve(Idev)
 
  ViA <- Vi%*%A
  ViAmat<-Vi%*%Amat
  
  #g3
  l1 <- ViA - sigmau2$sigmau2*ViA%*%ViA
  l1t<-t(l1)
  l2<-ViAmat - sigmau2$sigmau2*ViAmat%*%ViA
  l2t<-t(l2)
  L<-matrix(0,2,framework$m)
  
  # Anfang
  # Calculation of g3
  for (d in 1:framework$m)
  {
    L[1,]<-l1t[d,]
    L[2,]<-l2t[d,]
    g3[d] <- sum(diag(L%*%V%*%t(L)%*%Idevi))
  }
  mse.help <- g1 + g2 + 2*g3

  ######## Bias correction of Singh et al
  psi <- diag(c(framework$vardir),framework$m)
  D12aux <- (-1)*(A%*%derRho%*%A)
  D22aux <- 2*sigmau2$sigmau2*A%*%derRho%*%A%*%derRho%*%A-2*sigmau2$sigmau2*A%*%Wt%*%framework$W%*%A
  D <- (psi%*%Vi%*%D12aux%*%Vi%*%psi)*(Idevi[1,2]+Idevi[2,1])+psi%*%Vi%*%D22aux%*%Vi%*%psi*Idevi[2,2]
  
  for (d in 1:framework$m){
    g4[d]<-(0.5)*D[d,d]
  }
  
  # Computation of estimated MSE of Singh et al
  mse <- mse.help - g4
  
  if (method == "ml"){
    # Calculate bML
    QXtVi <- Q%*%XtVi
    ViX   <- Vi%*%framework$model_X
    h1    <- (-1)*sum(diag(QXtVi%*%A%*%ViX))
    h2    <- (-1)*sum(diag(QXtVi%*%Amat%*%ViX))
    h     <- matrix(c(h1,h2),nrow=2,ncol=1)
    bML   <- (Idevi%*%h)/2 
    tbML  <- t(bML)
    
    # Calculate gradient of g1d
    GVi     <- G%*%Vi   # G<-A*Ci
    GViA   <- GVi%*%A
    GViAmat <- GVi%*%Amat
    ViA    <- Vi%*%A
    dg1_dA  <- A   - 2*GViA   + sigmau2$sigmau2*GViA%*%ViA
    dg1_dp  <- Amat - 2*GViAmat + sigmau2$sigmau2*GViAmat%*%ViA
    gradg1d <- matrix(0,nrow=2,ncol=1) 
    
    bMLgradg1 <- rep(0,framework$m)   
    for (d in 1:framework$m)
    {
      gradg1d[1,1] <- dg1_dA[d,d]
      gradg1d[2,1] <- dg1_dp[d,d]
      bMLgradg1[d] <- tbML%*%gradg1d             
    }
    mse <- mse - bMLgradg1
  }
  
  
  
  MSE_data <- data.frame(Domain = combined_data[[framework$domains]])
  MSE_data$Direct <- NA
  MSE_data$Direct[framework$obs_dom == TRUE] <- framework$vardir
  
  # Small area MSE
  MSE_data$FH[framework$obs_dom == TRUE] <- mse
  MSE_data$Out[framework$obs_dom == TRUE] <- 0
  
  
  #if (!all(framework$obs_dom == TRUE)) {
   # h <- rep(0, framework$M - framework$m)
   # mse_out <- rep(0, framework$M - framework$m)
    # Covariates for out-of-sample domains
   # pred_data_tmp <- combined_data[framework$obs_dom == FALSE,]
   # pred_data_tmp <- data.frame(pred_data_tmp, helper = rnorm(1,0,1))
   # lhs(framework$formula) <- quote(helper)
   # pred_data <- makeXY(formula = framework$formula, data = pred_data_tmp)
   # pred_X <- pred_data$x
    
   # for (d_out in 1:(framework$M - framework$m)) {
   #   xd_out <- matrix(pred_X[d_out, ], nrow = 1, ncol = framework$p)
   #   h[d_out] <- xd_out %*% Q %*% t(xd_out)
   #   mse_out[d_out] <- sigmau2 + h[d_out]
   # }
    
    MSE_data$FH[framework$obs_dom == FALSE] <- NA
    MSE_data$Out[framework$obs_dom == FALSE] <- 1
 # }
  
  
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

  if (framework$correlation == "spatial"){
    MSE_data <- prasad_rao_spatial(framework = framework, sigmau2, combined_data, method)
    MSE_method <- "prasad-rao-Singh"
    } else if (method == "reml") {
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

nonparametricboot_spatial <- function(sigmau2, combined_data, framework, vardir,
                          eblup, B, transformation, method) {
  
  
  g1 <- rep(0, framework$m)
  g2 <- rep(0, framework$m)
  mse <- rep(0, framework$M)
  
  BCoef.boot <- eblup$coefficients$coefficients         
  rho.boot   <- sigmau2$rho
  sigma2.boot <- sigmau2$sigmau2
  
  D <- diag(1,framework$m)
  Xt <- t(framework$model_X)
  Wt <- t(framework$W)
  W <- framework$W
  DrhoW <- D - rho.boot*W
  DrhoWt <- t(DrhoW)
  Ar <- solve(DrhoWt%*%DrhoW)
  Gr <- sigmau2$sigmau2*Ar
  Vr <- Gr + D*framework$vardir
  Vri <- solve(Vr)
  Qr <- solve(t(framework$model_X)%*%Vri%*%framework$model_X)
  
  # Residual vectors
  res <- framework$direct - framework$model_X%*%BCoef.boot
  vstim <- Gr%*%Vri%*%res
  
  # Calculate covariance matrices of residual vectors
  VG <- Vr-Gr
  P <- Vri-Vri%*%framework$model_X%*%Qr%*%t(framework$model_X)%*%Vri
  Ve <- VG%*%P%*%VG
  Vu <- DrhoW%*%Gr%*%P%*%Gr%*%DrhoWt
  
  XtVri <- Xt%*%Vri
  Qr <- solve(XtVri%*%framework$model_X)
  
  # Calculate g1 and g2
  
  Ga <- Gr - Gr%*%Vri%*%Gr
  Gb <- Gr%*%t(XtVri)
  Xa <- matrix(0,1,framework$p)
  
  for (d in 1:framework$m) {
    g1[d]<-Ga[d,d]
    Xa[1,]<-framework$model_X[d,] - Gb[d,]
    g2[d] <- Xa%*%Qr%*%t(Xa)
  }
  
  # Square roots of covariance matrices
  VecVe0 <- eigen(Ve)$vectors
  VecVe <- VecVe0[,1:(framework$m - framework$p)]
  ValVe0 <- eigen(Ve)$values
  Valve <- diag(sqrt(1/ValVe0[1:(framework$m - framework$p)]))
  Vei05 <- VecVe%*%Valve%*%t(VecVe)
  
  VecVu0 <- eigen(Vu)$vectors
  VecVu <- VecVu0[,1:(framework$m - framework$p)]
  ValVu0 <- 1/(eigen(Vu)$values)
  ValVu <- diag(sqrt(ValVu0[1:(framework$m - framework$p)]))
  Vui05 <- VecVu%*%ValVu%*%t(VecVu)
  
  # Standardize residual vectors
  ustim <- as.vector(Vui05%*%((DrhoW)%*%vstim))
  estim <- as.vector(Vei05%*%(res-vstim))
  sdu <- sqrt(sigma2.boot)
  
  u.std <- rep(0,framework$m)
  for (d in 1:framework$m){
    u.std[d] <- (sdu*(ustim[d]-mean(ustim)))/sqrt(mean((ustim-mean(ustim))^2))
    }
  
  e.std <- rep(0,framework$m)
  for (d in 1:framework$m){
    e.std[d] <- (estim[d]-mean(estim))/sqrt(mean((estim-mean(estim))^2))
    }
  
  # Create results matrices
  bootstrapMSE <- matrix(0, framework$M, B)
  estTheta <- matrix(0, framework$M, B)
  trueTheta <- matrix(0, framework$M, B)
  difmse.npb <- matrix(0,framework$M,1)
  #difmse.npb1=matrix(0,nrow(b1),1)
  #difg3Spat.npb1=matrix(0,nrow(b1),1)
  g1.help <- matrix(0,framework$m,1)
  g2.help <- matrix(0,framework$m,1)
  difg1.npb <- matrix(0,framework$m,1)
  difg2.npb <- matrix(0,framework$m,1)
  difg3.npb <- matrix(0,framework$m,1)
  difmse.npbBC <- matrix(0,framework$M,1)
  
  # Bootstrap algorithm
  b <- 1
  while (b<=B) 
  {
    
    # Bootstrap data
    u.boot <- sample(u.std,framework$m,replace=TRUE)
    e.samp <- sample(e.std,framework$m,replace=TRUE)
    e.boot <- sqrt(framework$vardir)*e.samp
    v.boot <- solve(D-rho.boot*W)%*%u.boot
    
    if (all(framework$obs_dom == TRUE)) {
      data_tmp <- data.frame(Domain = combined_data[[framework$domains]])
      data_tmp$theta.boot[framework$obs_dom == TRUE] <- as.numeric(framework$model_X%*%BCoef.boot + v.boot) 
    } else {
      data_tmp <- data.frame(Domain = combined_data[[framework$domains]])
      data_tmp$theta.boot[framework$obs_dom == TRUE] <- as.numeric(framework$model_X%*%BCoef.boot + v.boot) 
      # Prediction (Out-of-sample)
      pred_data_tmp <- combined_data[framework$obs_dom == FALSE,]
      pred_data_tmp <- data.frame(pred_data_tmp, helper = rnorm(1,0,1))
      lhs(framework$formula) <- quote(helper)
      pred_data <- makeXY(formula = framework$formula, data = pred_data_tmp)
      pred_X <- pred_data$x
      pred_y <- pred_X %*% BCoef.boot
      data_tmp$theta.boot[framework$obs_dom == FALSE] <- as.numeric(pred_y)
    }
    
    #theta.boot1=Xpop1%*%Bstim.boot
    direct.boot <- as.numeric(data_tmp$theta.boot[framework$obs_dom == TRUE] + e.boot)
    combined_data$direct.boot[framework$obs_dom == TRUE] <- direct.boot
    combined_data$direct.boot <- as.numeric(combined_data$direct.boot)
    # Estimation procedure
    Terms <- terms(framework$formula)
    formula.tmp <- as.formula(paste("direct.boot ~", paste(attr(Terms, "term.labels"), collapse="+", "- 1"))) 
    framework.boot <- framework_FH(combined_data = combined_data,
                                   fixed = formula.tmp,
                                   vardir = vardir,
                                   domains = framework$domains,
                                   transformation = transformation,
                                   eff_smpsize = framework$eff_smpsize,
                                   correlation = framework$correlation,
                                   corMatrix = framework$W,
                                   Ci = framework$Ci,
                                   tol = framework$tol,
                                   maxit = framework$maxit)
    
    sigmau2.boot <- wrapper_estsigmau2(framework = framework.boot, 
                                       method = method,
                                       interval = interval)
    
    eblupSFH.boot <- eblup_SFH(framework = framework.boot, 
                               sigmau2 = sigmau2.boot,
                               combined_data = combined_data) 
      
    
    # Generate a new sample if estimators are not satisfactory
    if (sigmau2.boot$convergence==FALSE | sigmau2.boot$sigmau2 <0 | 
        sigmau2.boot$rho <(-1) | sigmau2.boot$rho>1)
    next
    
    cat("b =",b,"\n")
    
   # Bstim.ML.boot <- results.SpFH.boot$fit$estcoef[,1]                                   
    rho.tmp.boot <- sigmau2.boot$rho
    sigma2.tmp.boot <- sigmau2.boot$sigmau2                                        
    thetaSFH.boot <-  eblupSFH.boot$EBLUP_data$FH   
    
    # Nonparametric bootstrap estimator of g3
    Bstim.sblup <- Qr%*%XtVri%*%direct.boot
    
    if (all(framework$obs_dom == TRUE)) {
      data_tmp$thetaSFH.sblup.boot[framework$obs_dom == TRUE] <- framework$model_X%*%Bstim.sblup + Gr%*%Vri%*%
        (direct.boot-framework$model_X%*%Bstim.sblup) 
    } else {
    data_tmp$thetaSFH.sblup.boot[framework$obs_dom == TRUE] <- framework$model_X%*%Bstim.sblup + Gr%*%Vri%*%
      (direct.boot-framework$model_X%*%Bstim.sblup)
    
    # Prediction (Out-of-sample)
    pred_data_tmp <- combined_data[framework$obs_dom == FALSE,]
    pred_data_tmp <- data.frame(pred_data_tmp, helper = rnorm(1,0,1))
    lhs(framework$formula) <- quote(helper)
    pred_data <- makeXY(formula = framework$formula, data = pred_data_tmp)
    pred_X <- pred_data$x
    pred_y <- pred_X %*% Bstim.sblup
    data_tmp$thetaSFH.sblup.boot[framework$obs_dom == FALSE] <- pred_y
    }
    thetaSFH.boot.in <-  eblupSFH.boot$EBLUP_data$FH[eblupSFH.boot$EBLUP_data$Out == 0]
    difg3.npb[,1] <- difg3.npb[,1] + (thetaSFH.boot.in - data_tmp$thetaSFH.sblup.boot[framework$obs_dom == TRUE])^2
    
    # Naive nonparametric bootstrap MSE
    estTheta[,b] <- thetaSFH.boot # estimated values
    trueTheta[,b] <- data_tmp$theta.boot # true values
    bootstrapMSE[, b] <- (thetaSFH.boot - data_tmp$theta.boot)^2
    difmse.npb[,1] <- difmse.npb[,1] + (thetaSFH.boot - data_tmp$theta.boot)^2
    #difmse.npb1[,1]<-difmse.npb1[,1]+(thetaEBLUPSpat.boot1[,1]-theta.boot1)^2
    
    # g1 and g2 for each bootstrap sample
    A <- solve((D - rho.tmp.boot*Wt)%*%(D - rho.tmp.boot*W))
    G <- sigma2.tmp.boot*A
    V <- G + D*framework$vardir
    Vi<-solve(V)
    XtVi<-Xt%*%Vi
    Q<-solve(XtVi%*%framework$model_X)
    
    Ga <- G-G%*%Vi%*%G
    Gb <- G%*%Vi%*%framework$model_X
    Xa <- matrix(0,1,framework$p)
    
    for (d in 1:framework$m){
      g1.help[d] <- Ga[d,d] 
      Xa[1,] <- framework$model_X[d,] - Gb[d,]
      g2.help[d] <- Xa%*%Q%*%t(Xa)
    }
    
    difg1.npb <- difg1.npb + g1.help
    difg2.npb <- difg2.npb + g2.help
    
    b <- b + 1
  }
  # Final naive nonparametric bootstrap MSE estimator
  mse.npb <- difmse.npb[,1]/B
  
  # Final bias-corrected nonparametric bootstrap MSE estimator
  g3.npb <- difg3.npb/B
  g1.npb  <-difg1.npb/B
  g2.npb <- difg2.npb/B
  mse.npbBC <- 2*(g1 + g2) - difg1.npb[,1]/B - difg2.npb[,1]/B + difg3.npb[,1]/B
  
 
  
  MSE_data <- data.frame(Domain = combined_data[[framework$domains]])
  MSE_data$Var <- NA
  MSE_data$Var[framework$obs_dom == TRUE] <- framework$vardir
  
  # Small area MSE
  MSE_data$MSE <- mse.npb
  MSE_data$MSE.BC <- NA
  MSE_data$MSE.BC[framework$obs_dom == TRUE] <- mse.npbBC
  MSE_data$Out[framework$obs_dom == TRUE] <- 0
  MSE_data$Out[framework$obs_dom == FALSE] <- 1
  
  MSE_data <- list(MSE_data = MSE_data,
                  MSE_method = "non-parametric bootstrap",
                  bootstrapMSE = bootstrapMSE,
                  estTheta = estTheta,
                  trueTheta = trueTheta)

}

parametricboot_spatial <- function(sigmau2, combined_data, framework, vardir,
                                      eblup, B, transformation, method) {
  
  
  g1 <- rep(0, framework$m)
  g2 <- rep(0, framework$m)
  mse <- rep(0, framework$m)
  
  BCoef.boot <- eblup$coefficients$coefficients         
  rho.boot   <- sigmau2$rho
  sigma2.boot <- sigmau2$sigmau2
  
  D <- diag(1,framework$m)
  Xt <- t(framework$model_X)
  Wt <- t(framework$W)
  W <- framework$W
  DrhoW <- D - rho.boot*W
  DrhoWt <- t(DrhoW)
  Ar <- solve(DrhoWt%*%DrhoW)
  Gr <- sigmau2$sigmau2*Ar
  Vr <- Gr + D*framework$vardir
  Vri <- solve(Vr)
  Qr <- solve(t(framework$model_X)%*%Vri%*%framework$model_X)
  
  # Residual vectors
  res <- framework$direct - framework$model_X%*%BCoef.boot
  vstim <- Gr%*%Vri%*%res
 
  XtVri <- Xt%*%Vri
  Qr <- solve(XtVri%*%framework$model_X)
  
  # Calculate g1 and g2
  
  Ga <- Gr - Gr%*%Vri%*%Gr
  Gb <- Gr%*%t(XtVri)
  Xa <- matrix(0,1,framework$p)
  
  for (d in 1:framework$m) {
    g1[d]<-Ga[d,d]
    Xa[1,]<-framework$model_X[d,] - Gb[d,]
    g2[d] <- Xa%*%Qr%*%t(Xa)
  }
  
  # Create results matrices
  
  difmse.pb <- matrix(0,framework$m,1)
  #difmse.npb1=matrix(0,nrow(b1),1)
  #difg3Spat.npb1=matrix(0,nrow(b1),1)
  g1.help <- matrix(0,framework$m,1)
  g2.help <- matrix(0,framework$m,1)
  difg1.pb <- matrix(0,framework$m,1)
  difg2.pb <- matrix(0,framework$m,1)
  difg3.pb <- matrix(0,framework$m,1)
  difmse.pbBC <- matrix(0,framework$m,1)
  
  # Bootstrap algorithm
  for (b in 1:B){
    
    # Bootstrap data
    u.boot     <-rnorm(framework$m,0,sqrt(sigma2.boot))
    v.boot     <-solve(D - rho.boot*W)%*%u.boot
    theta.boot <- framework$model_X%*%BCoef.boot + v.boot
    e.boot     <-rnorm(framework$m,0,sqrt(framework$vardir))
    direct.boot <- theta.boot + e.boot
    combined_data$direct.boot <- direct.boot
    
    # Estimation procedure
    Terms <- terms(framework$formula)
    formula.tmp <- as.formula(paste("direct.boot ~", paste(attr(Terms, "term.labels"), collapse="+"), "- 1"))
    framework.boot <- framework_FH(combined_data = combined_data,
                                   fixed = formula.tmp,
                                   vardir = vardir,
                                   domains = framework$domains,
                                   transformation = transformation,
                                   eff_smpsize = framework$eff_smpsize,
                                   correlation = framework$correlation,
                                   corMatrix = framework$W,
                                   Ci = framework$Ci,
                                   tol = framework$tol,
                                   maxit = framework$maxit)
    
    sigmau2.boot <- wrapper_estsigmau2(framework = framework.boot, 
                                       method = method,
                                       interval = interval)
    
    eblupSFH.boot <- eblup_SFH(framework = framework.boot, 
                               sigmau2 = sigmau2.boot,
                               combined_data = combined_data) 
    
    
    # Generate a new sample if estimators are not satisfactory
     if (sigmau2.boot$convergence==FALSE | sigmau2.boot$sigmau2 <0 | 
         sigmau2.boot$rho <(-1) | sigmau2.boot$rho>1)
     next
    
    cat("b =",b,"\n")
    
    # Bstim.ML.boot <- results.SpFH.boot$fit$estcoef[,1]                                   
    rho.tmp.boot <- sigmau2.boot$rho
    sigma2.tmp.boot <- sigmau2.boot$sigmau2  
    BCoef.tmp.boot <- eblupSFH.boot$coefficients$coefficients  
    thetaSFH.boot <-  eblupSFH.boot$EBLUP_data$FH   
    
    # Nonparametric bootstrap estimator of g3
    Bstim.sblup <- Qr%*%XtVri%*%direct.boot
    thetaSFH.sblup.boot <- framework$model_X%*%Bstim.sblup + Gr%*%Vri%*%
      (direct.boot-framework$model_X%*%Bstim.sblup)
    
    difg3.pb[,1] <- difg3.pb[,1] + (thetaSFH.boot - thetaSFH.sblup.boot)^2
    
    # Naive parametric bootstrap MSE
    difmse.pb[,1] <- difmse.pb[,1] + (thetaSFH.boot - theta.boot)^2
    #difmse.npb1[,1]<-difmse.npb1[,1]+(thetaEBLUPSpat.boot1[,1]-theta.boot1)^2
    
    # g1 and g2 for each bootstrap sample
    A <- solve((D - rho.tmp.boot*Wt)%*%(D - rho.tmp.boot*W))
    G <- sigma2.tmp.boot*A
    V <- G + D*framework$vardir
    Vi<-solve(V)
    XtVi<-Xt%*%Vi
    Q<-solve(XtVi%*%framework$model_X)
    
    Ga <- G-G%*%Vi%*%G
    Gb <- G%*%Vi%*%framework$model_X
    Xa <- matrix(0,1,framework$p)
    
    for (d in 1:framework$m){
      g1.help[d] <- Ga[d,d] 
      Xa[1,] <- framework$model_X[d,] - Gb[d,]
      g2.help[d] <- Xa%*%Q%*%t(Xa)
    }
    
    difg1.pb <- difg1.pb + g1.help
    difg2.pb <- difg2.pb + g2.help
  }
  # Final naive nonparametric bootstrap MSE estimator
  mse.pb <- difmse.pb[,1]/B
  
  # Final bias-corrected nonparametric bootstrap MSE estimator
  g1.pb  <-difg1.pb/B
  g2.pb <- difg2.pb/B
  g3.pb <- difg3.pb/B
  
  mse.pbBC <- 2*(g1 + g2) - difg1.pb[,1]/B - difg2.pb[,1]/B + difg3.pb[,1]/B
  
  
  
  MSE_data <- data.frame(Domain = combined_data[[framework$domains]])
  MSE_data$Var <- NA
  MSE_data$Var[framework$obs_dom == TRUE] <- framework$vardir
  
  # Small area MSE
  MSE_data$MSE <- mse.pb
  MSE_data$MSE.BC <- mse.pbBC
  MSE_data$Out[framework$obs_dom == TRUE] <- 0
  MSE_data$Out[framework$obs_dom == FALSE] <- 1
  
  MSE_data <- list(MSE_data = MSE_data,
                   MSE_method = "parametric bootstrap")
  
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
                              correlation = framework$correlation,
                              corMatrix = framework$corMatrix,
                              eff_smpsize = framework$eff_smpsize, Ci = NULL, 
                              tol = NULL, maxit = NULL)
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
                                       correlation = framework$correlation,
                                       corMatrix = framework$corMatrix,
                                       eff_smpsize = framework$eff_smpsize, Ci = NULL, 
                                       tol = NULL, maxit = NULL)
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
                                  correlation = framework$correlation, 
                                  corMatrix = framework$corMatrix,
                                  eff_smpsize = framework$eff_smpsize, Ci = NULL, 
                                  tol = NULL, maxit = NULL)
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
                                       eff_smpsize = framework$eff_smpsize, 
                                       correlation = framework$correlation,
                                       corMatrix = framework$corMatrix, 
                                       Ci = NULL, tol = NULL, maxit = NULL)
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

### Jackknife MSE estimator (Ybarra-Lohr model)

jiang_jackknife_yl  <- function(framework, combined_data, sigmau2, eblup, method,
                                transformation, vardir, Ci) {
  
  
  # this MSE estimator can leed to negative values
  m <- framework$m
  jack_sigmau2 <- vector(length = m)
  diff_jack_eblups <- data.frame(row.names = 1:m)
  diff_jack_g1 <- data.frame(row.names = 1:m)
  g1 <- rep(0, framework$m)
  jack_mse <- rep(0, framework$m)
  gamma_tmp <- matrix(0,framework$m, framework$m)
  sigmau2_tmp<- matrix(0,framework$m)
  jack_sigmau2<- matrix(0,framework$m)
  # Inverse of total variance
  #Vi <- 1/(sigmau2 + framework$vardir)
  # Shrinkage factor
  Bd <- 1- eblup$gamma
  
  g1_tmp <- matrix(0,framework$m, framework$m)
  for (d in 1:framework$m) {
    # Variance due to random effects: vardir * gamma
    g1[d] <- framework$vardir[d] * (1 - Bd[d])
  }
  
  for (domain in 1:m) {
    print(domain)
    
    
    data_insample <- combined_data[framework$obs_dom,]
    data_tmp <- data_insample[-domain,]
    Ci_tmp <- Ci[,,-domain]
    
    # Framework with temporary data
    framework_tmp <- framework_FH(combined_data = data_tmp,
                                  fixed = framework$formula,
                                  vardir = vardir, 
                                  domains = framework$domains,
                                  transformation = "no",
                                  eff_smpsize = framework$eff_smpsize,
                                  correlation = framework$correlation,
                                  corMatrix = framework$corMatrix, 
                                  Ci = Ci_tmp,
                                  tol = framework$tol, 
                                  maxit = framework$maxit)
    

    # Estimate sigma u
    sigmau2_tmp<- wrapper_estsigmau2(framework = framework_tmp, method = method)
    jack_sigmau2[domain] <- sigmau2_tmp$sigmau_YL
    
    #Vi_tmp <- 1/(sigmau2_tmp + framework$vardir)
    
    # Shrinkage factor
    #Bd_tmp <- 1- eblup_tmp$gamma
    
    framework_insample <- framework_FH(combined_data = data_insample, 
                                       fixed = framework$formula,
                                       vardir = vardir, 
                                       domains = framework$domains,
                                       transformation = "no",
                                       eff_smpsize = framework$eff_smpsize,
                                       correlation = framework$correlation,
                                       corMatrix = framework$corMatrix, 
                                       Ci = Ci,
                                       tol = framework$tol, 
                                       maxit = framework$maxit)
    
    
    thbCihb<-NULL
    for(i in 1:framework_insample$m){
      thbCihb[i]<-t(sigmau2_tmp$betahatw)%*%framework_insample$Ci[,,i]%*%sigmau2_tmp$betahatw
    }
    
    gamma_tmp[, domain] <- (sigmau2_tmp$sigmau_YL+thbCihb)/
      (sigmau2_tmp$sigmau_YL+thbCihb+ framework_insample$vardir)
    
    
    
    for (d_tmp in 1:framework$m) {
      g1_tmp[d_tmp,] <- framework$vardir[d_tmp] * gamma_tmp[d_tmp,] #(1 - Bd_tmp[d_tmp])
    }
    
    
    # G1
    diff_jack_g1[, paste0(domain)] <- g1_tmp[, domain] - g1
    
    # Standard EBLUP
    
    eblup_tmp <- eblup_YL(framework = framework_insample, sigmau2 = sigmau2_tmp,
                          combined_data = data_insample) #data_insample framework_insample
    
    diff_jack_eblups[, paste0(domain)] <- eblup_tmp$EBLUP_data$FH - 
      eblup$EBLUP_data$FH[eblup$EBLUP_data$Out == 0]
    
    
  }
  
  jack_mse <- g1 - ((m - 1)/m) * rowSums(diff_jack_g1) + 
    ((m - 1)/m) * rowSums(diff_jack_eblups^2)
  
  
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
                  MSE_method = "jackknife",
                  g1 = g1,
                  diff_jack_g1 = jack_sigmau2,
                  diff_jack_eblups = diff_jack_eblups)
  
  return(mse_out)
  }


################################################################################
### MSE estimators taken from saeRobust
pseudo <- function(framework, combined_data, eblup, mse_type, method){
  MSE <- saeRobust::mse(object = eblup$eblupobject, type = mse_type, predType = method)
  MSE_data <- data.frame(Domain = combined_data[[framework$domains]])
  MSE_data$Direct <- NA
  MSE_data$Direct[framework$obs_dom == TRUE] <- framework$vardir
  if (is.element("reblup", method)) MSE_data$FH[framework$obs_dom == TRUE] <- MSE$pseudo
  if (is.element("reblupbc", method)) MSE_data$FH[framework$obs_dom == TRUE] <- MSE$pseudobc
  MSE_data$Out[framework$obs_dom == TRUE] <- 0
  
  MSE_data <- list(MSE_data = MSE_data,
                   MSE_method = "pseudo linearization")
}

robustboot <- function(framework, combined_data, eblup, mse_type, B, method){
  MSE <- saeRobust::mse(object = eblup$eblupobject, type = mse_type, predType = method, B = B)
  MSE_data <- data.frame(Domain = combined_data[[framework$domains]])
  MSE_data$Direct <- NA
  MSE_data$Direct[framework$obs_dom == TRUE] <- framework$vardir
  if (is.element("reblup", method)) MSE_data$FH[framework$obs_dom == TRUE] <- MSE$boot
  if (is.element("reblupbc", method)) MSE_data$FH[framework$obs_dom == TRUE] <- MSE$bootbc
  MSE_data$Out[framework$obs_dom == TRUE] <- 0
  MSE_data <- list(MSE_data = MSE_data,
                   MSE_method = "bootstrap")
}




wrapper_MSE <- function(framework, combined_data, sigmau2, vardir, Ci, eblup,
                        transformation, method, interval, mse_type, 
                        B = NULL) {
  MSE_data <- if (mse_type == "analytical") {
    analytical_mse(framework = framework, sigmau2 = sigmau2,
                   combined_data = combined_data, method = method)
  } else if (mse_type == "jackknife") {
    if (method == "moment") {
      jiang_jackknife_yl(framework = framework, combined_data = combined_data,
                         sigmau2 = sigmau2, vardir = vardir, Ci = Ci, eblup = eblup,
                         transformation = transformation, method = method)
    } else {
      jiang_jackknife(framework = framework, combined_data = combined_data,
                      sigmau2 = sigmau2, vardir = vardir, eblup = eblup,
                      transformation = transformation, method = method,
                      interval = interval) 
    }
  } else if (mse_type == "weighted_jackknife"){
    chen_weighted_jackknife(framework = framework, combined_data = combined_data,
                            sigmau2 = sigmau2, eblup = eblup, vardir = vardir,
                            transformation = transformation, method = method,
                            interval = interval)
  } else if (mse_type == "pseudo"){
    pseudo(framework = framework, combined_data = combined_data, eblup = eblup, 
           mse_type = "pseudo", method = method)
  } else if (mse_type == "boot") {
    robustboot(framework = framework, combined_data = combined_data, eblup = eblup, 
               mse_type = "boot", method = method,
               B = B)
  } else if (mse_type == "spatialnonparboot"){
    nonparametricboot_spatial(framework = framework, combined_data = combined_data,
                              sigmau2 = sigmau2, eblup = eblup, B = B, 
                              method = method,
                              vardir = vardir, transformation = transformation) 
  } else if (mse_type == "spatialparboot"){
    parametricboot_spatial(framework = framework, combined_data = combined_data,
                              sigmau2 = sigmau2, eblup = eblup, B = B, 
                              method = method,
                              vardir = vardir, transformation = transformation) 
  }

  return(MSE_data)
}
