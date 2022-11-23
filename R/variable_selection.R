model_select <- function(framework, sigmau2, method, interval,
                         eblup, B, vardir = vardir, transformation,
                         combined_data) {
  m <- nrow(framework$model_X)
  p <- ncol(framework$model_X)
  D <- diag(1, framework$m)

  res <- eblup$real_res + eblup$random_effects

  if (method != "me") {
    # Criteria for model selection
    if (framework$correlation == "spatial") {
      Vi <- solve(eblup$V)
      loglike <- (-0.5) * (m * log(2 * pi) +
        determinant(eblup$V, logarithm = TRUE)$modulus +
        t(res) %*% Vi %*% res)
      AIC <- (-2) * loglike + 2 * (p + 2)
      BIC <- (-2) * loglike + (p + 2) * log(m)
    } else {
      loglike <- (-0.5) * (sum(log(2 * pi * (sigmau2 + framework$vardir)) +
        (res^2) / (sigmau2 + framework$vardir)))
      AIC <- (-2) * loglike + 2 * (p + 1)
      BIC <- (-2) * loglike + (p + 1) * log(m)
      KIC <- (-2) * loglike + 3 * (p + 1)
    }

    if ((framework$correlation == "no" &&
      transformation == "no") && !is.null(B)) {
      # Criteria following Marhuenda et al. (2014)
      # bootstrap components
      B1 <- vector(length = B)
      B3 <- vector(length = B)
      B5 <- vector(length = B)
      loglike.boot <- vector(length = B)
      loglikestar.boot <- vector(length = B)

      for (b in seq_len(B)) {
        W1 <- rnorm(n = m, mean = 0, sd = 1)
        u.boot <- sqrt(sigmau2) * W1
        W2 <- rnorm(n = m, mean = 0, sd = 1)
        e.boot <- sqrt(framework$vardir) * W2
        direct.boot <- framework$model_X %*%
          eblup$coefficients$coefficients + u.boot + e.boot
        combined_data$direct.boot[framework$obs_dom == TRUE] <- direct.boot
        combined_data$direct.boot <- as.numeric(combined_data$direct.boot)

        # Estimation procedure
        formula.tmp <- update(framework$formula, direct.boot ~ .)
        framework.boot <- framework_FH(
          combined_data = combined_data,
          fixed = formula.tmp,
          vardir = vardir,
          domains = framework$domains,
          transformation = "no",
          eff_smpsize = framework$eff_smpsize,
          correlation = framework$correlation,
          corMatrix = framework$W,
          Ci = framework$Ci,
          tol = framework$tol,
          maxit = framework$maxit
        )

        sigmau2.boot <- wrapper_estsigmau2(
          framework = framework.boot,
          method = method, interval = interval
        )

        eblup.boot <- eblup_FH(
          framework = framework.boot,
          sigmau2 = sigmau2.boot,
          combined_data = combined_data
        )

        B1[b] <-
          sum(((sigmau2 + framework$vardir) +
            ((framework$model_X %*%
              (eblup$coefficients$coefficients -
                eblup.boot$coefficients$coefficients))^2) -
            ((direct.boot - framework$model_X %*%
              eblup.boot$coefficients$coefficients)^2)) /
            (sigmau2.boot + framework$vardir))

        loglike.boot[b] <-
          -(framework$m / 2 * log(2 * pi)) -
          (1 / 2 * sum(log(sigmau2.boot + framework$vardir))) -
          (1 / 2 * sum((framework$direct - framework$model_X %*%
            eblup.boot$coefficients$coefficients)^2 /
            (sigmau2.boot + framework$vardir)))
        res.boot <- eblup.boot$real_res + eblup.boot$random_effects

        loglikestar.boot[b] <-
          (-0.5) * (sum(log(2 * pi * (sigmau2.boot + framework$vardir)) +
            (res.boot^2) / (sigmau2.boot + framework$vardir)))

        B3[b] <- sum(((sigmau2.boot + framework$vardir) +
          (framework$model_X %*%
            (eblup$coefficients$coefficients -
              eblup.boot$coefficients$coefficients))^2) /
          (sigmau2 + framework$vardir))

        B5[b] <- sum((log(sigmau2.boot + framework$vardir) +
          (((direct.boot - framework$model_X %*%
            eblup.boot$coefficients$coefficients)^2)) /
            (sigmau2.boot + framework$vardir)))
      }

      AICc <- (-2) * loglike + mean(B1)
      AICb1 <- (-2) * loglike - 2 * (mean(loglike.boot) -
                                       mean(loglikestar.boot))
      AICb2 <- (-2) * loglike - 4 * (mean(loglike.boot) - loglike)
      B2 <- sum(log(sigmau2 + framework$vardir)) + mean(B3) - mean(B5)
      KICc <- AICc + B2
      KICb1 <- AICb1 + B2
      KICb2 <- AICb2 + B2
    }
  }
  # Calculation R2
  P <- framework$model_X %*% solve(t(framework$model_X) %*%
    framework$model_X) %*% t(framework$model_X)
  SSE <- as.numeric(t(framework$direct) %*%
    (diag(1, length(framework$direct)) - P) %*%
    framework$direct)
  MSE <- SSE / (m - p)
  one <- matrix(1, m, 1)
  SST <- as.numeric(t(framework$direct) %*%
    (diag(1, length(framework$direct)) -
      (1 / m) * one %*% t(one)) %*% framework$direct)
  MST <- SST / (m - 1)
  R2_regular <- 1 - (MSE / MST)

  barD <- sum(framework$vardir) / m
  hii <- NULL
  for (i in seq_len(m)) {
    hii[i] <- as.numeric(t(framework$model_X[i, ]) %*%
      solve(t(framework$model_X) %*%
        framework$model_X) %*% framework$model_X[i, ])
  }
  Dw <- sum((1 - hii) * framework$vardir) / (m - p)
  hxbMSE <- (2 * MSE) / (1 + exp((2 * Dw) / MSE))
  hxbMST <- (2 * MST) / (1 + exp((2 * barD) / MST))
  FH_R2 <- 1 - (hxbMSE / hxbMST)

  if (!all(framework$obs_dom == TRUE)) {
    message(strwrap(prefix = " ", initial = "",
                    "Please note that the model selection criteria are only
                    computed based on the in-sample domains."))
  }

  if (framework$correlation == "spatial" || transformation != "no") {
    criteria <- data.frame(
      loglike = loglike,
      AIC = AIC,
      BIC = BIC,
      AdjR2 = R2_regular,
      FH_R2 = FH_R2
    )
  } else if (framework$correlation == "no" && transformation == "no" &&
    !(B > 1) && method != "me") {
    criteria <- data.frame(
      loglike = loglike,
      AIC = AIC,
      BIC = BIC,
      KIC = KIC,
      AdjR2 = R2_regular,
      FH_R2 = FH_R2
    )
  } else if (framework$correlation == "no" && transformation == "no" &&
    (B > 1) && method != "me") {
    criteria <- data.frame(
      loglike = loglike,
      AIC = AIC, AICc = AICc,
      AICb1 = AICb1, AICb2 = AICb2,
      BIC = BIC,
      KIC = KIC, KICc = KICc,
      KICb1 = KICb1, KICb2 = KICb2,
      AdjR2 = R2_regular,
      FH_R2 = FH_R2
    )
  } else if (method == "me") {
    criteria <- data.frame(
      AdjR2 = R2_regular,
      FH_R2 = FH_R2
    )
  }
  return(criteria)
}
