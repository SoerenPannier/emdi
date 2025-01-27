point_fh_tf <- function(fixed, vardir, domains, subdomains, nunits, trafo,
                        maxit, eff_n, framework){

  # Estimate var_v and var_u (based on sampled data)----------------------------
  varv0 <- var(framework$smp_data[[fixed[[2]]]])
  varu0 <- var(framework$smp_data[[fixed[[2]]]])

  dif.ml <- ml_NR_diff(varv = varv0, varu = varu0, vare = framework$vare,
                       yranr = framework$y_ij,
                       no = framework$no, X = framework$X, m = framework$m)
  varv1 <- varv0
  varu1 <- varu0

  iter <- 0
  while((abs(dif.ml[1]) > 0.0001 | abs(dif.ml[2]) > 0.0001) & iter < maxit){
    iter <- iter + 1
    varv1 <- varv1+dif.ml[1]
    varu1 <- varu1+dif.ml[2]
    dif.ml <- ml_NR_diff(varv = varv1, varu = varu1, vare = framework$vare,
                         yranr = framework$y_ij,
                         no = framework$no, X = framework$X, m = framework$m)
  }

  if(iter >= maxit) {
    warning(strwrap(prefix = " ", initial = "",
                    paste0("There is a convergence problem in the variance components estimation process.
           You may try to increase the number of the maximum iteration.")))}

  varv <- varv1
  varu <- varu1
  if(varv <= 0){
    varv <- 0.00001
    warning(strwrap(prefix = " ", initial = "",
                    paste0("The estimated variance of the domain level random
                           effect is negative. Therefore, it is replaced by 0.0001")))}

  if(varu <= 0){
    varu <- 0.00001
    warning(strwrap(prefix = " ", initial = "",
                    paste0("The estimated variance of the subdomain level random
                           effect is negative. Therefore, it is replaced by 0.00001")))
  }

  beta <- beta_tilde(varv = varv, varu = varu, vare = framework$vare,
                     yranr = framework$y_ij, no = framework$no, m = framework$m,
                     X = framework$X)
  beta_tild <- beta$beta

  # Obtain EBLUP at subarea level
  gamma_ij <- varu/(varu + framework$vare)
  gamma1_i <- numeric()
  y_bar_igamma_prep <- numeric()
  X_bar_igamma_prep <- matrix(ncol = framework$p, nrow = framework$m)

  for(i in 1:framework$m){
    if (i == 1){
      a <- 1
      b <- framework$no[1]
    } else {
      a <- a + framework$no[i - 1]
      b <- b + framework$no[i]
    }
    gamma1_i[i] <- sum(gamma_ij[a:b])
    y_bar_igamma_prep[i] <- sum((gamma_ij * framework$y_ij)[a:b])
    if (framework$p == 1) {X_bar_igamma_prep[i, ] <- sum((gamma_ij * framework$X)[a:b, ])}
    else{
      if(b == a) {X_bar_igamma_prep[i, ] <- ((gamma_ij * framework$X)[a:b, ])}
      else{X_bar_igamma_prep[i, ] <- colSums((gamma_ij * framework$X)[a:b, ])}
    }
  }
  names(gamma1_i) <- unique(framework$smp_data[[domains]])
  gamma2_i <- varv/(varv + (varu/gamma1_i))

  y_bar_igamma <- (1/gamma1_i)*y_bar_igamma_prep
  X_bar_igamma <- (1/gamma1_i)*X_bar_igamma_prep
  rownames(X_bar_igamma) <- names(gamma1_i)
  v_tilde_i <- gamma2_i * (y_bar_igamma - X_bar_igamma %*% beta_tild)

  v_tilde_i_sub <- rep(v_tilde_i, times = framework$no)
  gamma2_i_sub <- rep(gamma2_i, times = framework$no)
  y_bar_igamma_sub <- rep(y_bar_igamma, times = framework$no)

  X_bar_igamma_sub <- do.call("rbind", replicate(framework$no[1], X_bar_igamma[1, ], simplify = FALSE))
  for (i in 2:framework$m) {
    X_bar_igamma_sub_i <- do.call("rbind", replicate(framework$no[i], X_bar_igamma[i, ], simplify = FALSE))
    X_bar_igamma_sub <- rbind(X_bar_igamma_sub, X_bar_igamma_sub_i)
  }

  u_tilde_ij <- gamma_ij * (framework$y_ij - framework$X %*% beta_tild) - gamma2_i_sub * gamma_ij *
    (y_bar_igamma_sub - X_bar_igamma_sub %*% beta_tild)

  # EBLUP for mu_ij for sampled sub-areas
  mu_ij_smp <- framework$X %*% beta_tild + v_tilde_i_sub + u_tilde_ij

  # EBLUP for mu_ij for non-sampled sub-areas & in sample areas
  no_smp_data <- framework$data[framework$data$ObsSub == "no", ]
  no_dom <- framework$DomName[which(!is.element(framework$DomName, unique(framework$smp_data[[domains]])))]
  vec <- which(is.element(no_smp_data[[domains]], no_dom))

  if(length(no_dom) > 0){
    no_sub_no_area_data <- no_smp_data[vec, ]
    no_sub_in_area_data <- no_smp_data[-vec, ]
  } else{
    no_sub_in_area_data <- no_smp_data
  }

  x_var <- c(labels(terms(fixed)))
  X_no_sub_in_area <- data.frame(Intercept = rep(1, length(no_sub_in_area_data[[subdomains]])))
  X_no_sub_in_area <- cbind(X_no_sub_in_area, no_sub_in_area_data[, x_var])
  Dom_X_no_sub_in_area <- no_sub_in_area_data[[domains]]
  rownames(X_no_sub_in_area) <- no_sub_in_area_data[[subdomains]]

  v_tilde_no <- numeric()
  for (k in 1:length(no_sub_in_area_data[[subdomains]])) {
    v_tilde_no[k] <- v_tilde_i[which(rownames(v_tilde_i) == Dom_X_no_sub_in_area[k])]
  }

  mu_ij_no_sub_in_area <- as.matrix(X_no_sub_in_area) %*% beta_tild + v_tilde_no

  if(length(no_dom) > 0){
    X_no_sub_no_area <- data.frame(Intercept = rep(1, length(no_sub_no_area_data[[subdomains]])))
    X_no_sub_no_area <- cbind(X_no_sub_no_area, no_sub_no_area_data[, x_var])
    rownames(X_no_sub_no_area) <- no_sub_no_area_data[[subdomains]]

    mu_ij_no_sub_no_area <- as.matrix(X_no_sub_no_area) %*% beta_tild

    mu_ij_no_smp_dat <- data.frame(mu_ij = c(mu_ij_no_sub_no_area, mu_ij_no_sub_in_area),
                                   SubDomain = c(rownames(mu_ij_no_sub_no_area),
                                                 rownames(mu_ij_no_sub_in_area)))
  } else{
    mu_ij_no_smp_dat <- data.frame(mu_ij = c(mu_ij_no_sub_in_area),
                                   SubDomain = c(rownames(mu_ij_no_sub_in_area)))
  }


  mu_ij_smp_dat <- data.frame(mu_ij = mu_ij_smp, SubDomain = rownames(mu_ij_smp))

  mu_ij <- rbind(mu_ij_smp_dat, mu_ij_no_smp_dat)

  data <- merge(framework$data, mu_ij, by.x = subdomains, by.y = "SubDomain")
  EBLUP_ij <- data.frame(Subdomain = data[[subdomains]], EBLUP = data$mu_ij)
  ################################################################################
  # Preparation for back transformation
  gamma_ij <- data.frame(SubArea = names(gamma_ij),
                         gamma_ij = gamma_ij)
  gamma_i <- data.frame(Area = names(gamma2_i),
                        gamma_i = gamma2_i)
  gamma_i0 <- data.frame(Area = names(gamma1_i),
                         gamma_i0 = gamma1_i)
  y_bar_igamma <- data.frame(Area = names(y_bar_igamma),
                             y_bar_igamma = y_bar_igamma)
  X_bar_igamma_x_beta <- data.frame(Area = y_bar_igamma$Area,
                                    X_bar_igamma_x_beta = X_bar_igamma %*% beta_tild)

  data <- merge(data, gamma_ij, by.x = subdomains,
                by.y = "SubArea", all.x = TRUE, all.y = FALSE)
  data <- merge(data, gamma_i, by.x = domains,
                by.y = "Area", all.x = TRUE, all.y = FALSE)
  data <- merge(data, gamma_i0, by.x = domains,
                by.y = "Area", all.x = TRUE, all.y = FALSE)
  data <- merge(data,  y_bar_igamma, by.x = domains,
                by.y = "Area", all.x = TRUE, all.y = FALSE)
  data <- merge(data,  X_bar_igamma_x_beta, by.x = domains,
                by.y = "Area", all.x = TRUE, all.y = FALSE)

  data$gamma_ij <- ifelse(is.na(data$gamma_ij), 0, data$gamma_ij)
  data$gamma_i0 <- ifelse(is.na(data$gamma_i0) & data$gamma_ij == 0, 0, data$gamma_i0)
  data$gamma_i <- ifelse(is.na(data$gamma_i), 0, data$gamma_i)

  ################################################################################
  # Back transformation at sub-area level
  if(trafo == "arcsin"){
    bt_eblup <- arcsin_bt_fh_tf(data = data, eblup = "mu_ij", subdomains = subdomains,
                                varv = varv, varu = varu)
    data <- merge(data, bt_eblup, by = subdomains, all.x = TRUE, all.y = FALSE)
    EBLUP_ij <- data[, c(subdomains, "bt_mu_ij")]
    colnames(EBLUP_ij) <- c("Subdomain", "EBLUP")
  } else if(trafo == "log"){

    bt_eblup <- log_bt_fh_tf(data = data, eblup = "mu_ij", subdomains = subdomains,
                             varv = varv, varu = varu)
    data <- merge(data, bt_eblup, by = subdomains, all.x = TRUE, all.y = FALSE)
    EBLUP_ij <- data[, c(subdomains, "bt_mu_ij")]
    colnames(EBLUP_ij) <- c("Subdomain", "EBLUP")
  }
  ################################################################################
  # Obtain area level EBLUP by aggregating back-transformed at sub-area level

  if(trafo == "log" | trafo == "arcsin"){
    mu_i <- numeric()
    eblup <- "bt_mu_ij"
    for (i in 1:framework$M) {
      dat <- data[data[[domains]] == framework$DomName[i], c(eblup, "ObsSub", nunits)]
      N_i <- sum(dat[[nunits]])
      mu_i[i] <- sum(dat[[eblup]] * dat[[nunits]])/N_i
    }
    EBLUP_i <- data.frame(Domain = framework$DomName, EBLUP = mu_i)
  }
  else {
    mu_i <- numeric()
    eblup <- "mu_ij"
    for (i in 1:framework$M) {
      dat <- data[data[[domains]] == framework$DomName[i], c(eblup, "ObsSub", nunits)]
      N_i <- sum(dat[[nunits]])
      mu_i[i] <- sum(dat[[eblup]] * dat[[nunits]])/N_i
    }
    EBLUP_i <- data.frame(Domain = framework$DomName, EBLUP = mu_i)
  }

  ##############################################################################
  # Get residuals and predicted random effects
  #all.equal(names(y_ij), rownames(X))
  residuals <- data.frame(SubArea = names(framework$y_ij),
                          y_ij = framework$y_ij,
                          xbeta = framework$X %*% beta_tild)
  residuals <- merge(residuals, data[, c(domains, subdomains)],
                     by.x = "SubArea", by.y = subdomains, all.x = T, all.y = F)
  residuals <- merge(residuals, data.frame(Area = rownames(v_tilde_i),
                                           v_tilde = v_tilde_i),
                     by.x = domains, by.y = "Area", all.x = T, all.y = F)
  residuals <- merge(residuals, data.frame(SubArea = rownames(u_tilde_ij),
                                           u_tilde = u_tilde_ij),
                     by = "SubArea", all.x = T, all.y = F)
  #residuals <- merge(residuals, )
  mar_y_hat <- residuals$xbeta
  con_y_hat <- residuals$xbeta + residuals$v_tilde + residuals$u_tilde
  mar_r2 <- var(mar_y_hat)/var(framework$y_ij)
  con_r2 <- var(con_y_hat)/var(framework$y_ij)
  R2 <- c(mar_r2, con_r2)
  names(R2) <- c("Marginal_R2", "Conditional_R2")
  residuals$resid <- residuals$y_ij - con_y_hat

  residuals <- merge(residuals, data.frame(SubArea = names(framework$vare),
                                           vare = framework$vare),
                     by = "SubArea", all.x = T, all.y = F)
  residuals$std_resid <- residuals$resid / residuals$vare

  # beta coefficient
  tvalue <- beta_tild / beta$beta_std
  pvalue <- 2 * pnorm(abs(tvalue), lower.tail = FALSE)

  beta_coef <- data.frame(
    coefficients = beta_tild,
    std.error = beta$beta_std,
    t.value = tvalue,
    p.value = pvalue
  )

  loglike <- (-0.5) * (sum(log(2 * pi * (varv + varu + framework$vare)) +
                             ((residuals$y_ij - residuals$xbeta)^2) / (varv + varu + framework$vare)))
  ##############################################################################

  return(list(EBLUP_Area = EBLUP_i,
              EBLUP_SubArea = EBLUP_ij,
              varu = varu,
              varv = varv,
              vare = framework$vare,
              beta = beta_coef,
              beta_vcov = beta$Q,
              loglike = loglike,
              fitted = mar_y_hat,
              v_tilde = v_tilde_i,
              u_tilde = u_tilde_ij,
              resid = residuals$resid,
              std_resid = residuals$std_resid,
              gammas = data[, c(domains, subdomains, "gamma_ij", "gamma_i0", "gamma_i")],
              R2 = R2))
}

ml_NR_diff <- function(varv, varu, vare, yranr, no, X, m){
  a <- 1
  b <- no[1]
  if (is.vector(X)) {X_i <- X[a:b]}
  else{X_i <- X[a:b,]}
  J <- matrix(1,ncol=no[1],nrow=no[1])
  V <- Var_y(varv = varv, varu = varu, vare = vare, no = no, m = m)
  V_i <- V[a:b, a:b]

  beta_coef <- beta_tilde(varv = varv, varu = varu, vare = vare,
                          yranr = yranr, no = no, m = m, X = X)
  beta_til <- beta_coef$beta
  inv_Vi <- beta_coef$inv_V[a:b, a:b]

  score_v_term1 <- sum(diag(inv_Vi %*% J))
  score_u_term1 <- sum(diag(inv_Vi))
  score_v_term2 <-t(yranr[a:b] - X_i %*% beta_til)%*%(-inv_Vi %*% J %*% inv_Vi)%*%(yranr[a:b] -X_i %*% beta_til)
  score_u_term2 <- t(yranr[a:b] - X_i %*% beta_til)%*%(-inv_Vi %*% inv_Vi)%*%(yranr[a:b] - X_i %*% beta_til)
  score_v <- - 0.5 * (score_v_term1 + score_v_term2)
  score_u <- - 0.5 * (score_u_term1 + score_u_term2)

  H_hatv <- 0.5 * sum(diag(inv_Vi %*% J %*% inv_Vi %*% J))
  H_hatu <- 0.5 * sum(diag(inv_Vi %*% inv_Vi))
  H_hatvu <- 0.5 * sum(diag(inv_Vi  %*% J %*% inv_Vi))
  for(i in 2:m){
    a <- a + no[i - 1]
    b <- b + no[i]
    if (is.vector(X)) {X_i <- X[a:b]}
    else{X_i <- X[a:b,]}
    J <- matrix(1, ncol = no[i], nrow = no[i])
    V_i <- V[a:b, a:b]
    inv_Vi <- beta_coef$inv_V[a:b, a:b]

    score_v_term1 <- sum(diag(inv_Vi %*% J))
    score_u_term1 <- sum(diag(inv_Vi))
    score_v_term2 <-t(yranr[a:b] - X_i %*% beta_til)%*%(-inv_Vi %*% J %*% inv_Vi)%*%(yranr[a:b] -X_i %*% beta_til)
    score_u_term2 <- t(yranr[a:b] - X_i %*% beta_til)%*%(-inv_Vi %*%inv_Vi)%*%(yranr[a:b] - X_i %*% beta_til)
    score_v <- score_v -0.5*(score_v_term1 + score_v_term2)
    score_u <- score_u -0.5*(score_u_term1 + score_u_term2)

    H_hatv <- H_hatv + 0.5*sum(diag(inv_Vi %*% J %*% inv_Vi %*% J))
    H_hatu <- H_hatu + 0.5*sum(diag(inv_Vi %*% inv_Vi))
    H_hatvu <- H_hatvu + 0.5*sum(diag(inv_Vi %*% J %*% inv_Vi))
  }
  # score elements (S(theta^(r)) for theta = (sigma_v, sigma_u)) in Eq. 6.17 (Morales et al., 2021)
  score <- c(score_v, score_u)

  # Fisher information matrix (F(\theta^(r))) = - (hessian matrix)^(-1)  (Eq. 6.21 Morales et al., 2021)
  Inv_F <- solve(matrix(c(H_hatv, H_hatvu,
                          H_hatvu, H_hatu),
                        nrow = 2, ncol = 2, byrow = T))
  NR_diff <- Inv_F %*% score

  return(NR_diff)
}

#-------------------------------------------------------------------------------
# Get variance of y: V(y)  with sub-area as unit (N x N matrix)
Var_y <- function(varv, varu, vare, no, m) {
  R <- diag(vare, ncol = sum(no))
  Z <- cbind(c(rep(1, no[1])), diag(1, ncol = no[1], nrow = no[1]))
  G <- diag(c(varv, rep(varu, no[1])))
  for (i in 2:m) {
    Znext <- cbind(c(rep(1,no[i])),diag(1,ncol=no[i], nrow=no[i]))
    Gnext <- diag(c(varv, rep(varu, no[i])))
    Z <- adiag(Z, Znext)
    G <- adiag(G, Gnext)
  }
  V <- R + Z %*% G %*% t(Z)
  return(V)
}

#
# Under/For Eq. 2.6: beta_tilde(delta) =
# (sum_i^m(t(X_i) %*% solve(V_i) %*% X_i))^-1 * sum(t(X_i) %*% solve(V_i) %*% y_i)
beta_tilde <- function(varv, varu, vare, yranr, no, m, X){
  V <- Var_y(varv = varv, varu = varu, vare = vare, no = no, m = m)
  inv_V <- solve(V)
  Q <- solve(t(X) %*% inv_V %*% X)
  beta <- Q %*% (t(X) %*% inv_V %*% yranr)
  beta_std <- sqrt(diag(Q))
  return(list(beta = beta,
              beta_std = beta_std,
              V = V,
              inv_V = inv_V,
              Q = Q))
}