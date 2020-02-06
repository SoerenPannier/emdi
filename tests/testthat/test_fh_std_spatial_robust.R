# Test if the same variance, EBLUP and MSE results are obtained as with 
# the sae package

# Load needed package
install.packages("sae")
library("sae")

########################## Standard Fay-Herriot model ##########################

# The data that is used for testing is the data from the sae package. 
load("FH/milk.RData")

test_that("Does the fh function in emdi return the same variance, EBLUP and MSE 
          estimates as the function mseFH of package sae?",{
  
  ############################ REML variance estimation ########################
  # Estimation with fh of emdi
  milk$var <- milk$SD^2
  fh_reml <- fh(fixed = yi ~ as.factor(MajorArea), vardir = "var",
                         combined_data = milk, domains = "SmallArea",
                         method = "reml", interval = c(0, 1000), MSE = TRUE)
  
  # Estimation with mseFH of sae (benchmark)
  fh_reml_sae <- mseFH(milk$yi ~ as.factor(milk$MajorArea), vardir = milk$SD^2, 
                         method = "REML", MAXITER = 100, PRECISION = 0.0001)
  
  # Comparison
  # Variance
  expect_equal(fh_reml$model$variance, fh_reml_sae$est$fit$refvar, tolerance = 0.0001)
  # EBLUP
  expect_equal(fh_reml$ind$FH, as.vector(fh_reml_sae$est$eblup), tolerance = 0.0001)
  # MSE
  expect_equal(fh_reml$MSE$FH, fh_reml_sae$mse, tolerance = 0.00001)
  
  ############################ ML variance estimation ##########################
  # Estimation with fh of emdi
  fh_ml <- fh(fixed = yi ~ as.factor(MajorArea), vardir = "var",
                combined_data = milk, domains = "SmallArea",
                method = "ml", interval = c(0, 1000), MSE = TRUE)
  
  # Estimation with mseFH of sae (benchmark)
  fh_ml_sae <- mseFH(milk$yi ~ as.factor(milk$MajorArea), vardir = milk$SD^2, 
                       method = "ML", MAXITER = 100, PRECISION = 0.0001)
  
  # Comparison
  # Variance
  expect_equal(fh_ml$model$variance, fh_ml_sae$est$fit$refvar, tolerance = 0.0001)
  # EBLUP
  expect_equal(fh_ml$ind$FH, as.vector(fh_ml_sae$est$eblup), tolerance = 0.0001)
  # MSE
  expect_equal(fh_ml$MSE$FH, fh_ml_sae$mse, tolerance = 0.0001)
})

# Test if the same variance, correlation parameter, EBLUP and MSE results are 
# obtained as with the sae package

########################## Spatial Fay-Herriot model ###########################
# The data that is used for testing is the data from the sae package. 
# Data set
load("FH/grapes.RData")
# Proximity matrix
load("FH/grapesprox.RData")

# Analytical MSE 

test_that("Does the fh function in emdi return the same variance, correlation 
           parameter, EBLUP and MSE estimates as the function mseSFH of package 
           sae?",{
  
  ############################ REML variance estimation ########################
  # Estimation with fh of emdi
  grapes$Domain <- c(1:274)
  fh_spatial_reml_analytical <- fh(fixed = grapehect ~ area + workdays - 1, 
                                   vardir = "var", tol = 0.0001, maxit = 100, 
                                   combined_data = grapes, domains = "Domain", 
                                   method = "reml", correlation = "spatial", 
                                   corMatrix = as.matrix(grapesprox), MSE = TRUE, 
                                   mse_type = "analytical")
  
  # Estimation with mseSFH of sae (benchmark)
  fh_spatial_reml_analytical_sae <- mseSFH(grapehect ~ area + workdays - 1, 
                                           vardir = var, proxmat = grapesprox, 
                                           method = "REML", MAXITER = 100, 
                                           PRECISION = 0.0001, data = grapes)
       
      
  # Comparison
  # Variance
  expect_equal(fh_spatial_reml_analytical$model$variance$sigmau2, 
               fh_spatial_reml_analytical_sae$est$fit$refvar)
  # Correlation parameter
  expect_equal(fh_spatial_reml_analytical$model$variance$rho, 
               fh_spatial_reml_analytical_sae$est$fit$spatialcorr)
  # EBLUP
  expect_equal(fh_spatial_reml_analytical$ind$FH, 
               as.vector(fh_spatial_reml_analytical_sae$est$eblup))
  # MSE
  expect_equal(fh_spatial_reml_analytical$MSE$FH, 
               fh_spatial_reml_analytical_sae$mse)
            
  ############################ ML variance estimation ##########################
  # Estimation with fh of emdi
  fh_spatial_ml_analytical <- fh(fixed = grapehect ~ area + workdays - 1, 
                                   vardir = "var", tol = 0.0001, maxit = 100, 
                                   combined_data = grapes, domains = "Domain", 
                                   method = "ml", correlation = "spatial", 
                                   corMatrix = as.matrix(grapesprox), MSE = TRUE, 
                                   mse_type = "analytical")
  
  # Estimation with mseSFH of sae (benchmark)
  fh_spatial_ml_analytical_sae <- mseSFH(grapehect ~ area + workdays - 1, 
                                           vardir = var, proxmat = grapesprox, 
                                           method = "ML", MAXITER = 100, 
                                           PRECISION = 0.0001, data = grapes)
  
  
  # Comparison
  # Variance
  expect_equal(fh_spatial_ml_analytical$model$variance$sigmau2, 
               fh_spatial_ml_analytical_sae$est$fit$refvar)
  # Correlation parameter
  expect_equal(fh_spatial_ml_analytical$model$variance$rho, 
               fh_spatial_ml_analytical_sae$est$fit$spatialcorr)
  # EBLUP
  expect_equal(fh_spatial_ml_analytical$ind$FH, 
               as.vector(fh_spatial_ml_analytical_sae$est$eblup))
  # MSE
  expect_equal(fh_spatial_ml_analytical$MSE$FH, 
               fh_spatial_ml_analytical_sae$mse)
})

# Nonparametric bootstrap MSE 

test_that("Does the fh function in emdi return the same variance, correlation 
          parameter, EBLUP and MSE estimates as the function npbmseSFH of package 
          sae?",{
            
  ############################ REML variance estimation ########################
  # Estimation with fh of emdi
  grapes$Domain <- c(1:274)
  fh_spatial_reml_npb <- fh(fixed = grapehect ~ area + workdays - 1, 
                            vardir = "var", tol = 0.0001, maxit = 100, 
                            combined_data = grapes, domains = "Domain", 
                            method = "reml", correlation = "spatial", 
                            corMatrix = as.matrix(grapesprox), MSE = TRUE, 
                            mse_type = "spatialnonparboot", B = 3, seed = 123)
            
  # Estimation with npbmseSFH of sae (benchmark)
  set.seed(123)
  fh_spatial_reml_npb_sae <- npbmseSFH(grapehect ~ area + workdays - 1, 
                                       vardir = var, proxmat = grapesprox, 
                                       method = "REML", MAXITER = 100, 
                                       PRECISION = 0.0001, B = 3, data = grapes)
  
  # Comparison
  # Variance
  expect_equal(fh_spatial_reml_npb$model$variance$sigmau2, 
               fh_spatial_reml_npb_sae$est$fit$refvar)
  # Correlation parameter
  expect_equal(fh_spatial_reml_npb$model$variance$rho, 
               fh_spatial_reml_npb_sae$est$fit$spatialcorr)
  # EBLUP
  expect_equal(fh_spatial_reml_npb$ind$FH, 
               as.vector(fh_spatial_reml_npb_sae$est$eblup))
  # MSE
  expect_equal(fh_spatial_reml_npb$MSE$MSE, 
               fh_spatial_reml_npb_sae$mse$mse, tolerance = 0.000001)
  # MSE bias corrected
  expect_equal(fh_spatial_reml_npb$MSE$MSE.BC, 
               fh_spatial_reml_npb_sae$mse$msebc, tolerance = 0.000001)
})

# Parametric bootstrap MSE 

test_that("Does the fh function in emdi return the same variance, correlation 
          parameter, EBLUP and MSE estimates as the function pbmseSFH of package 
          sae?",{
            
  ############################ REML variance estimation ########################
  # Estimation with fh of emdi
  grapes$Domain <- c(1:274)
  fh_spatial_reml_pb <- fh(fixed = grapehect ~ area + workdays - 1, 
                           vardir = "var", tol = 0.0001, maxit = 100, 
                           combined_data = grapes, domains = "Domain", 
                           method = "reml", correlation = "spatial", 
                           corMatrix = as.matrix(grapesprox), MSE = TRUE, 
                           mse_type = "spatialparboot", B = 3, seed = 123)
            
  # Estimation with npbmseSFH of sae (benchmark)
  set.seed(123)
  fh_spatial_reml_pb_sae <- pbmseSFH(grapehect ~ area + workdays - 1, 
                                     vardir = var, proxmat = grapesprox, 
                                     method = "REML", MAXITER = 100, 
                                     PRECISION = 0.0001, B = 3, data = grapes)
            
  # Comparison
  # Variance
  expect_equal(fh_spatial_reml_pb$model$variance$sigmau2, 
               fh_spatial_reml_pb_sae$est$fit$refvar)
  # Correlation parameter
  expect_equal(fh_spatial_reml_pb$model$variance$rho, 
               fh_spatial_reml_pb_sae$est$fit$spatialcorr)
  # EBLUP
  expect_equal(fh_spatial_reml_pb$ind$FH, 
               as.vector(fh_spatial_reml_pb_sae$est$eblup))
  # MSE
  expect_equal(fh_spatial_reml_pb$MSE$MSE, 
               fh_spatial_reml_pb_sae$mse$mse)
  # MSE bias corrected
  expect_equal(fh_spatial_reml_pb$MSE$MSE.BC, 
               fh_spatial_reml_pb_sae$mse$msebc)
  
  ############################ ML variance estimation ##########################
  # Estimation with fh of emdi
  grapes$Domain <- c(1:274)
  fh_spatial_ml_pb <- fh(fixed = grapehect ~ area + workdays - 1, 
                         vardir = "var", tol = 0.0001, maxit = 100, 
                         combined_data = grapes, domains = "Domain", 
                         method = "ml", correlation = "spatial", 
                         corMatrix = as.matrix(grapesprox), MSE = TRUE, 
                         mse_type = "spatialparboot", B = 3, seed = 123)
  
  # Estimation with npbmseSFH of sae (benchmark)
  set.seed(123)
  fh_spatial_ml_pb_sae <- pbmseSFH(grapehect ~ area + workdays - 1, 
                                   vardir = var, proxmat = grapesprox, 
                                   method = "ML", MAXITER = 100, 
                                   PRECISION = 0.0001, B = 3, data = grapes)
  
  # Comparison
  # Variance
  expect_equal(fh_spatial_ml_pb$model$variance$sigmau2, 
               fh_spatial_ml_pb_sae$est$fit$refvar)
  # Correlation parameter
  expect_equal(fh_spatial_ml_pb$model$variance$rho, 
               fh_spatial_ml_pb_sae$est$fit$spatialcorr)
  # EBLUP
  expect_equal(fh_spatial_ml_pb$ind$FH, 
               as.vector(fh_spatial_ml_pb_sae$est$eblup))
  # MSE
  expect_equal(fh_spatial_ml_pb$MSE$MSE, 
               fh_spatial_ml_pb_sae$mse$mse)
  # MSE bias corrected
  expect_equal(fh_spatial_ml_pb$MSE$MSE.BC, 
               fh_spatial_ml_pb_sae$mse$msebc)
})


# Test if the same variance, EBLUP and MSE results are obtained as with 
# the saeRobust package

# Load needed package
install.packages("saeRobust")
library("saeRobust")

########################## Robust area-level model #############################


# Pseudo MSE

test_that("Does the fh function in emdi return the same variance, EBLUP and MSE 
          estimates as the functions rfh and mse (pseudo) of package saeRobust?",{
  
  ############################ REBLUP model fitting ############################
            
  # Estimation with fh of emdi
  grapes$Domain <- c(1:274)
  fh_robust <- fh(fixed = grapehect ~ area + workdays - 1, vardir = "var",
                          combined_data = grapes, domains = "Domain",
                          method = "reblup", tol = 1e-06, maxit = 100, k = 1.345,
                          MSE = TRUE, mse_type = "pseudo")
           
  # Estimation with fitRFH of saeRobust (benchmark)
  fh_robust_saeRobust <- saeRobust::rfh(grapehect ~ area + workdays - 1, data = grapes, 
                         samplingVar = "var")
  mse_robust_saeRobust <- saeRobust::mse(fh_robust_saeRobust, type = "pseudo", 
                              predType = "reblup")         
  # Comparison
  # Variance
  expect_equal(fh_robust$model$variance, fh_robust_saeRobust$variance)
  # EBLUP
  expect_equal(fh_robust$ind$FH, fh_robust_saeRobust$reblup)
  # MSE
  expect_equal(fh_robust$MSE$FH, mse_robust_saeRobust$pseudo)
            
  ############################ REBLUPBC model fitting ##########################
  
  # Estimation with fh of emdi
  grapes$Domain <- c(1:274)
  fh_robustbc <- fh(fixed = grapehect ~ area + workdays - 1, vardir = "var",
                  combined_data = grapes, domains = "Domain",
                  method = "reblupbc", tol = 1e-06, maxit = 100, k = 1.345, 
                  c = 2, MSE = TRUE, mse_type = "pseudo")
  
  # Estimation with fitRFH of saeRobust (benchmark)
  fh_robust_saeRobust <- saeRobust::rfh(grapehect ~ area + workdays - 1, data = grapes, 
                             samplingVar = "var")
  fh_robustbc_saeRobust <- predict(fh_robust_saeRobust, type = "reblupbc", c = 2)
  mse_robustbc_saeRobust <- saeRobust::mse(fh_robust_saeRobust, type = "pseudo", 
                              predType = "reblupbc", c = 2)         
  # Comparison
  # Variance
  expect_equal(fh_robustbc$model$variance, fh_robust_saeRobust$variance)
  # EBLUP
  expect_equal(fh_robustbc$ind$FH, fh_robustbc_saeRobust$reblupbc)
  # MSE
  expect_equal(fh_robustbc$MSE$FH, mse_robustbc_saeRobust$pseudo)
  
})

# Bootstrap MSE

test_that("Does the fh function in emdi return the same variance, EBLUP and MSE 
          estimates as the functions rfh and mse (boot) of package saeRobust?",{
            
  ############################ REBLUP model fitting ############################
            
  # Estimation with fh of emdi
  grapes$Domain <- c(1:274)
  fh_robust_boot <- fh(fixed = grapehect ~ area + workdays - 1, vardir = "var",
                          combined_data = grapes, domains = "Domain",
                          method = "reblup", tol = 1e-06, maxit = 100, k = 1.345,
                          MSE = TRUE, mse_type = "boot", B = 3, seed = 123)
            
  # Estimation with fitRFH of saeRobust (benchmark)
  fh_robust_boot_saeRobust <- saeRobust::rfh(grapehect ~ area + workdays - 1, data = grapes,
                                  samplingVar = "var")
  set.seed(123)
  mse_robust_boot_saeRobust <- saeRobust::mse(fh_robust_boot_saeRobust, type = "boot", B = 3, 
                                   predType = "reblup")         
  # Comparison
  # Variance
  expect_equal(fh_robust_boot$model$variance, fh_robust_boot_saeRobust$variance)
  # EBLUP
  expect_equal(fh_robust_boot$ind$FH, fh_robust_boot_saeRobust$reblup)
  # MSE
  expect_equal(fh_robust_boot$MSE$FH, mse_robust_boot_saeRobust$boot)
            
  ############################ REBLUPBC model fitting ##########################
            
  # Estimation with fh of emdi
  grapes$Domain <- c(1:274)
  fh_robustbc_boot <- fh(fixed = grapehect ~ area + workdays - 1, vardir = "var", 
                         combined_data = grapes, domains = "Domain", 
                         method = "reblupbc", tol = 1e-06, maxit = 100, k = 1.345, 
                         c = 2, MSE = TRUE, mse_type = "boot", B = 3, seed = 123)
            
  # Estimation with fitRFH of saeRobust (benchmark)
  fh_robust_boot_saeRobust <- saeRobust::rfh(grapehect ~ area + workdays - 1, data = grapes, 
                                  samplingVar = "var")
  fh_robustbc_boot_saeRobust <- predict(fh_robust_boot_saeRobust, 
                                        type = "reblupbc", c = 2)
  set.seed(123)
  mse_robustbc_boot_saeRobust <- saeRobust::mse(fh_robust_boot_saeRobust, type = "boot", B = 3, 
                                predType = "reblupbc", c = 2)         
  # Comparison
  # Variance
  expect_equal(fh_robustbc_boot$model$variance, fh_robust_boot_saeRobust$variance)
  # EBLUP
  expect_equal(fh_robustbc_boot$ind$FH, fh_robustbc_boot_saeRobust$reblupbc)
  # MSE
  expect_equal(fh_robustbc_boot$MSE$FH, mse_robustbc_boot_saeRobust$bootbc)
            
})

######################## Robust spatial area-level model #######################

# Pseudo MSE

test_that("Does the fh function in emdi return the same variance, correlation 
           parameter, EBLUP and MSE estimates as the functions rfh and mse 
          (pseudo) of package saeRobust?",{
            
  ############################ REBLUP model fitting ############################
            
  # Estimation with fh of emdi
  grapes$Domain <- c(1:274)
  fh_robust_spatial <- fh(fixed = grapehect ~ area + workdays - 1, vardir = "var",
                          combined_data = grapes, domains = "Domain",
                          method = "reblup", tol = 1e-06, maxit = 100, k = 1.345,
                          correlation = "spatial", corMatrix = grapesprox,
                          MSE = TRUE, mse_type = "pseudo")
            
  # Estimation with fitRFH of saeRobust (benchmark)
  fh_robust_spatial_saeRobust <- rfh(grapehect ~ area + workdays - 1, 
                                     data = grapes, samplingVar = "var", 
                                     corSAR1(as.matrix(grapesprox)))
  mse_robust_spatial_saeRobust <- mse(fh_robust_spatial_saeRobust, 
                                      type = "pseudo", predType = "reblup")         
  # Comparison
  # Variance
  expect_equal(fh_robust_spatial$model$variance[2], 
               fh_robust_spatial_saeRobust$variance[2])
  # Correlation parameter
  expect_equal(fh_robust_spatial$model$variance[1], 
               fh_robust_spatial_saeRobust$variance[1])
  # EBLUP
  expect_equal(fh_robust_spatial$ind$FH, fh_robust_spatial_saeRobust$reblup)
  # MSE
  expect_equal(fh_robust_spatial$MSE$FH, mse_robust_spatial_saeRobust$pseudo)
            
  ############################ REBLUPBC model fitting ##########################
            
  # Estimation with fh of emdi
  grapes$Domain <- c(1:274)
  fh_robust_spatial_bc <- fh(fixed = grapehect ~ area + workdays - 1, vardir = "var",
                             combined_data = grapes, domains = "Domain",
                             method = "reblupbc", tol = 1e-06, maxit = 100, 
                             k = 1.345, c = 2, 
                             correlation = "spatial", corMatrix = grapesprox, 
                             MSE = TRUE, mse_type = "pseudo")
            
  # Estimation with fitRFH of saeRobust (benchmark)
  fh_robust_spatial_saeRobust <- rfh(grapehect ~ area + workdays - 1, 
                                     data = grapes, samplingVar = "var",
                                     corSAR1(as.matrix(grapesprox)))
  fh_robust_spatial_bc_saeRobust <- predict(fh_robust_spatial_saeRobust, 
                                            type = "reblupbc", c = 2)
  mse_robust_spatial_bc_saeRobust <- mse(fh_robust_spatial_saeRobust, 
                                         type = "pseudo", predType = "reblupbc", 
                                         c = 2)         
  # Comparison
  # Variance
  expect_equal(fh_robust_spatial_bc$model$variance[2], 
               fh_robust_spatial_saeRobust$variance[2])
  # Correlation parameter
  expect_equal(fh_robust_spatial_bc$model$variance[1], 
               fh_robust_spatial_saeRobust$variance[1])
  # EBLUP
  expect_equal(fh_robust_spatial_bc$ind$FH, 
               fh_robust_spatial_bc_saeRobust$reblupbc)
  # MSE
  expect_equal(fh_robust_spatial_bc$MSE$FH, 
               mse_robust_spatial_bc_saeRobust$pseudo)
            
})

# Bootstrap MSE

test_that("Does the fh function in emdi return the same variance, correlation 
           parameter, EBLUP and MSE estimates as the functions rfh and mse 
          (boot) of package saeRobust?",{
            
  ############################ REBLUP model fitting ############################
            
  # Estimation with fh of emdi
  grapes$Domain <- c(1:274)
  fh_robust_spatial_boot <- fh(fixed = grapehect ~ area + workdays - 1, 
                               vardir = "var", combined_data = grapes, 
                               domains = "Domain", method = "reblup", 
                               tol = 1e-06, maxit = 100, k = 1.345,
                               correlation = "spatial", corMatrix = grapesprox,
                               MSE = TRUE, mse_type = "boot", B = 3, seed = 123)
            
  # Estimation with fitRFH of saeRobust (benchmark)
  fh_robust_spatial_boot_saeRobust <- rfh(grapehect ~ area + workdays - 1, 
                                          data = grapes, samplingVar = "var",
                                          corSAR1(as.matrix(grapesprox)))
  set.seed(123)
  mse_robust_spatial_boot_saeRobust <- mse(fh_robust_spatial_boot_saeRobust, 
                                           type = "boot", B = 3, 
                                           predType = "reblup")         
  # Comparison
  # Variance
  expect_equal(fh_robust_spatial_boot$model$variance[2], 
               fh_robust_spatial_boot_saeRobust$variance[2])
  # Correlation parameter
  expect_equal(fh_robust_spatial_boot$model$variance[1], 
               fh_robust_spatial_boot_saeRobust$variance[1])
  # EBLUP
  expect_equal(fh_robust_spatial_boot$ind$FH, 
               fh_robust_spatial_boot_saeRobust$reblup)
  # MSE
  expect_equal(fh_robust_spatial_boot$MSE$FH, 
               mse_robust_spatial_boot_saeRobust$boot)
            
  ############################ REBLUPBC model fitting ##########################
            
  # Estimation with fh of emdi
  grapes$Domain <- c(1:274)
  fh_robust_spatial_bc_boot <- fh(fixed = grapehect ~ area + workdays - 1, 
                                  vardir = "var", combined_data = grapes, 
                                  domains = "Domain", method = "reblupbc", 
                                  tol = 1e-06, maxit = 100, k = 1.345, c = 2, 
                                  correlation = "spatial", corMatrix = grapesprox,
                                  MSE = TRUE, mse_type = "boot", 
                                  B = 3, seed = 123)
            
  # Estimation with fitRFH of saeRobust (benchmark)
  fh_robust_spatial_boot_saeRobust <- rfh(grapehect ~ area + workdays - 1, 
                                          data = grapes, samplingVar = "var",
                                          corSAR1(as.matrix(grapesprox)))
  fh_robust_spatial_bc_boot_saeRobust <- predict(fh_robust_spatial_boot_saeRobust, 
                                                 type = "reblupbc", c = 2)
  set.seed(123)
  mse_robust_spatial_bc_boot_saeRobust <- mse(fh_robust_spatial_boot_saeRobust, 
                                              type = "boot", B = 3, 
                                              predType = "reblupbc", c = 2)         
  # Comparison
  # Variance
  expect_equal(fh_robust_spatial_bc_boot$model$variance[2],
               fh_robust_spatial_boot_saeRobust$variance[2])
  # Correlation parameter
  expect_equal(fh_robust_spatial_bc_boot$model$variance[1],
               fh_robust_spatial_boot_saeRobust$variance[1])
  # EBLUP
  expect_equal(fh_robust_spatial_bc_boot$ind$FH, 
               fh_robust_spatial_bc_boot_saeRobust$reblupbc)
  # MSE
  expect_equal(fh_robust_spatial_bc_boot$MSE$FH, 
               mse_robust_spatial_bc_boot_saeRobust$bootbc)
            
})
