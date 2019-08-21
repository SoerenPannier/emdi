model_select <- function(framework, sigmau2, real_res) {

  m <- nrow(framework$model_X)
  p <- ncol(framework$model_X)

  # Criteria for model selection
  loglike <- (-0.5) * (sum(log(2 * pi * (sigmau2 + framework$vardir)) +
                             (real_res^2)/(sigmau2 + framework$vardir)))
  AIC <- (-2) * loglike + 2 * (p + 1)
  BIC <- (-2) * loglike + (p + 1) * log(framework$m)

  # Calculation R2
  P <- framework$model_X%*%solve(t(framework$model_X)%*%framework$model_X)%*%t(framework$model_X)
  SSE <- as.numeric(t(framework$direct)%*%(diag(1,length(framework$direct))-P)%*%framework$direct)
  MSE <- SSE/(m-p)
  one <- matrix(1,m,1)
  SST <- as.numeric(t(framework$direct)%*%(diag(1,length(framework$direct))-(1/m)*one%*%t(one))%*%framework$direct)
  MST <- SST/(m - 1)
  R2_regular <- 1-(MSE/MST)

  barD <- sum(framework$vardir)/m
  hii <- NULL
  for (i in 1:m)
  {
    hii[i] <- as.numeric(t(framework$model_X[i,])%*%solve(t(framework$model_X)%*%framework$model_X)%*%framework$model_X[i,])
  }
  Dw <- sum((1 - hii) * framework$vardir)/(m - p)
  hxbMSE <- (2 * MSE)/(1 + exp((2 * Dw)/MSE))
  hxbMST <- (2 * MST)/(1 + exp((2 * barD)/MST))
  AdjR2 <- 1 - (hxbMSE/hxbMST)

  criteria <- data.frame(loglike = loglike,
                         AIC = AIC,
                         BIC = BIC,
                         R2 = R2_regular,
                         AdjR2 = AdjR2)
  return(criteria)
  }
