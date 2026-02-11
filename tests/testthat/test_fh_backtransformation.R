# Compare results of new functions with the


# Load aggregated data ---------------------------------------------------------
data("eusilcA_popAgg")
data("eusilcA_smpAgg")

eusilcA_smpAgg <- eusilcA_smpAgg[c(1:30, 35:70, 75:94), ]

# Combine sample and population data -------------------------------------------
combined_data <- combine_data(pop_data = eusilcA_popAgg, pop_domains = "Domain",
                              smp_data = eusilcA_smpAgg, smp_domains = "Domain")




fh_point_bt <- read.csv('FH/fh_backtransform_point.csv')
fh_mse_bt <- read.csv('FH/fh_backtransform_mse.csv')



test_that("Do the FH log backtransformations work?", {

  fixed = Mean ~ eqsize + cash + self_empl
  vardir = "Var_Mean"
  combined_data = combined_data
  domains = "Domain"
  interval = c(0, 0.1518748)
  transformation = "log"
  eff_smpsize = NULL
  method = "ml"
  correlation = 'no'
  corMatrix = NULL
  Ci = NULL
  tol = NULL
  maxit = NULL
  MSE = TRUE

  # Single elements needed in optimal_parameter()
  framework <- framework_FH(combined_data = combined_data, fixed = fixed,
                            vardir = vardir, domains = domains,
                            transformation = transformation,
                            eff_smpsize = eff_smpsize, correlation = correlation,
                            corMatrix = corMatrix, Ci = Ci, tol = tol,
                            maxit = maxit)

  sigmau2 <- wrapper_estsigmau2(framework = framework, method = method,
                                interval = interval)
  expect_equal(round(sigmau2, 6), 0.008423)
  # Why is the result rounded? Where do we do this?
  sigmau2 <- 0.008423

  eblup <- eblup_FH(framework = framework, sigmau2 = sigmau2,
                    combined_data = combined_data)

  log_bt_crude <- log_bt(framework, sigmau2, combined_data, eblup = eblup,
                       method, MSE, backtransformation = 'bc_crude')
  log_bt_sm <- log_bt(framework, sigmau2, combined_data, eblup = eblup,
                         method, MSE, backtransformation = 'bc_sm')


  expect_equal(fh_point_bt$log_crude, log_bt_crude$point_backtransformed)
  expect_equal(fh_point_bt$log_sm, log_bt_sm$point_backtransformed)
  expect_equal(fh_mse_bt$log_crude, log_bt_crude$mse_backtransformed)
  expect_equal(fh_mse_bt$log_sm, log_bt_sm$mse_backtransformed)
})

test_that("Do the FH arcsin backtransformations work?", {

  fixed = MTMED ~ eqsize + cash + self_empl
  vardir = "Var_MTMED"
  combined_data = combined_data
  domains = "Domain"
  interval = c(0, 0.1124019)
  transformation = "arcsin"
  eff_smpsize = "n"
  method = "ml"
  correlation = 'no'
  corMatrix = NULL
  Ci = NULL
  tol = NULL
  maxit = NULL
  MSE = TRUE
  B = 20

  # Single elements needed in optimal_parameter()
  framework <- framework_FH(combined_data = combined_data, fixed = fixed,
                            vardir = vardir, domains = domains,
                            transformation = transformation,
                            eff_smpsize = eff_smpsize, correlation = correlation,
                            corMatrix = corMatrix, Ci = Ci, tol = tol,
                            maxit = maxit)

  sigmau2 <- wrapper_estsigmau2(framework = framework, method = method,
                                interval = interval)

  eblup <- eblup_FH(framework = framework, sigmau2 = sigmau2,
                    combined_data = combined_data)

  arcsin_naive_jk <- arcsin_bt(sigmau2 = sigmau2, combined_data = combined_data,
                            framework = framework, eblup = eblup, vardir = vardir,
                            mse_type = 'jackknife',
                            method = method, interval = interval, MSE = MSE,
                            B = B, backtransformation = 'naive')

  set.seed(123)
  arcsin_naive_boot <- arcsin_bt(sigmau2 = sigmau2, combined_data = combined_data,
                         framework = framework, eblup = eblup, vardir = vardir,
                         mse_type = 'boot',
                         method = method, interval = interval, MSE = MSE,
                         B = B, backtransformation = 'naive')

  set.seed(123)
  arcsin_bc_boot <- arcsin_bt(sigmau2 = sigmau2, combined_data = combined_data,
                              framework = framework, eblup = eblup, vardir = vardir,
                              mse_type = 'boot',
                              method = method, interval = interval, MSE = MSE,
                              B = B, backtransformation = 'bc')


  expect_equal(fh_point_bt$arcsin_naive_jk, arcsin_naive_jk$point_backtransformed)
  expect_equal(fh_point_bt$arcsin_naive_boot, arcsin_naive_boot$point_backtransformed)
  expect_equal(fh_point_bt$arcsin_bc_boot, arcsin_bc_boot$point_backtransformed)

  expect_equal(fh_mse_bt$arcsin_naive_jk, arcsin_naive_jk$mse_backtransformed)
  expect_equal(fh_mse_bt$arcsin_naive_boot, arcsin_naive_boot$mse_backtransformed)
  expect_equal(fh_mse_bt$arcsin_bc_boot, arcsin_bc_boot$mse_backtransformed)

})

test_that("Do the FH logit backtransformations work?", {

  fixed = MTMED ~ eqsize + cash + self_empl
  vardir = "Var_MTMED"
  combined_data = combined_data
  domains = "Domain"
  interval = c(0, 0.1124019)
  transformation = "logit"
  eff_smpsize = "n"
  method = "ml"
  correlation = 'no'
  corMatrix = NULL
  Ci = NULL
  tol = NULL
  maxit = NULL
  MSE = TRUE
  B = 20

  # Single elements needed in optimal_parameter()
  framework <- framework_FH(combined_data = combined_data, fixed = fixed,
                            vardir = vardir, domains = domains,
                            transformation = transformation,
                            eff_smpsize = eff_smpsize, correlation = correlation,
                            corMatrix = corMatrix, Ci = Ci, tol = tol,
                            maxit = maxit)

  sigmau2 <- wrapper_estsigmau2(framework = framework, method = method,
                                interval = interval)

  eblup <- eblup_FH(framework = framework, sigmau2 = sigmau2,
                    combined_data = combined_data)

  set.seed(123)
  logit_naive_boot <- arcsin_bt(sigmau2 = sigmau2, combined_data = combined_data,
                                 framework = framework, eblup = eblup, vardir = vardir,
                                 mse_type = 'boot',
                                 method = method, interval = interval, MSE = MSE,
                                 B = B, backtransformation = 'naive')

  set.seed(123)
  logit_bc_boot <- arcsin_bt(sigmau2 = sigmau2, combined_data = combined_data,
                              framework = framework, eblup = eblup, vardir = vardir,
                              mse_type = 'boot',
                              method = method, interval = interval, MSE = MSE,
                              B = B, backtransformation = 'bc')


  expect_equal(round(sigmau2, 7), 0.1123531)
  expect_equal(fh_point_bt$logit_naive_boot, logit_naive_boot$point_backtransformed)
  expect_equal(fh_point_bt$logit_bc_boot, logit_bc_boot$point_backtransformed)

  expect_equal(fh_mse_bt$logit_naive_boot, logit_naive_boot$mse_backtransformed)
  expect_equal(fh_mse_bt$logit_bc_boot, logit_bc_boot$mse_backtransformed)

})


