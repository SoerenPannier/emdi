# Test if variance estimation (and resulting EBLUPs and MSEs) remains the same

# Loading data - population and sample data
load("FH/eusilcA_popAgg.RData")
load("FH/eusilcA_smpAgg.RData")

# Combine sample and population data 
combined_data <- combine_data(pop_data = eusilcA_popAgg, pop_domains = "Domain",
                               smp_data = eusilcA_smpAgg, smp_domains = "Domain")

test_that("Does the variance estimation (and resulting EBLUPs and MSEs) 
           return the same results as the ones obtained in simulation studies?", {
            
  ################################# ML #########################################
            # ML model fitting (current version) 
            fh_ML <- fh(fixed = Mean ~ cash + self_empl, vardir = "Var_Mean",
                        combined_data = combined_data, domains = "Domain",
                        method = "ml", interval = c(0, 10000000), MSE = TRUE)
            # ML model fitting status quo (benchmark)
            ML_modelfit <- read.csv("FH/ML_modelfit.csv", sep = ",")  
            
            # Compare results from current version and benchmark
            # EBLUP
            expect_equal(fh_ML$ind[, c("Domain","FH")], 
                         ML_modelfit[, c("Domain","FH")])
            expect_equal(fh_ML$ind$FH, ML_modelfit$FH)
            # MSE
            expect_equal(fh_ML$MSE$FH, ML_modelfit$MSE)
            # Variance
            expect_equal(fh_ML$model$variance, ML_modelfit$variance[1])
            
 ################################ REML #########################################          
            # REML model fitting (current version) 
            fh_REML <- fh(fixed = Mean ~ cash + self_empl, vardir = "Var_Mean",
                          combined_data = combined_data, domains = "Domain",
                          method = "reml", interval = c(0, 10000000), MSE = TRUE)
            
            # REML model fitting status quo (benchmark)
            REML_modelfit <- read.csv("FH/REML_modelfit.csv", sep = ",")  
  
            # Compare results from current version and benchmark
            # EBLUP
            expect_equal(fh_REML$ind[, c("Domain","FH")], 
                         REML_modelfit[, c("Domain","FH")])
            # MSE
            expect_equal(fh_REML$MSE$FH, REML_modelfit$MSE)
            # Variance
            expect_equal(fh_REML$model$variance, REML_modelfit$variance[1])
            
################################# AMPL #########################################
            # AMPL model fitting (current version) 
            fh_AMPL <- fh(fixed = Mean ~ cash + self_empl, vardir = "Var_Mean",
                          combined_data = combined_data, domains = "Domain",
                          method = "ampl", interval = c(0, 10000000), MSE = TRUE)
            
            # AMPL model fitting status quo (benchmark)
            AMPL_modelfit <- read.csv("FH/AMPL_modelfit.csv", sep = ",")  
            
            # Compare results from current version and benchmark
            # EBLUP
            expect_equal(fh_AMPL$ind[, c("Domain","FH")], 
                         AMPL_modelfit[, c("Domain","FH")])
            # MSE
            expect_equal(fh_AMPL$MSE$FH, AMPL_modelfit$MSE)
            # Variance
            expect_equal(fh_AMPL$model$variance, AMPL_modelfit$variance[1])
            
################################# AMRL #########################################
            # AMRL model fitting (current version) 
            fh_AMRL <- fh(fixed = Mean ~ cash + self_empl, vardir = "Var_Mean",
                          combined_data = combined_data, domains = "Domain",
                          method = "amrl", interval = c(0, 10000000), MSE = TRUE)
            
            # AMRL model fitting status quo (benchmark)
            AMRL_modelfit <- read.csv("FH/AMRL_modelfit.csv", sep = ",")  
            
            # Compare results from current version and benchmark
            # EBLUP
            expect_equal(fh_AMRL$ind[, c("Domain","FH")], 
                         AMRL_modelfit[, c("Domain","FH")])
            # MSE
            expect_equal(fh_AMRL$MSE$FH, AMRL_modelfit$MSE)
            # Variance
            expect_equal(fh_AMRL$model$variance, AMRL_modelfit$variance[1])
            
################################# AMPL_YL ######################################
            # AMPL_YL model fitting (current version) 
            fh_AMPL_YL <- fh(fixed = Mean ~ cash + self_empl, vardir = "Var_Mean",
                             combined_data = combined_data, domains = "Domain",
                             method = "ampl_yl", interval = c(0, 10000000), MSE = TRUE)
            
            # AMPL model fitting status quo (benchmark)
            AMPL_YL_modelfit <- read.csv("FH/AMPL_YL_modelfit.csv", sep = ",")  
            
            # Compare results from current version and benchmark
            # EBLUP
            expect_equal(fh_AMPL_YL$ind[, c("Domain","FH")], 
                         AMPL_YL_modelfit[, c("Domain","FH")])
            # MSE
            expect_equal(fh_AMPL_YL$MSE$FH, AMPL_YL_modelfit$MSE)
            # Variance
            expect_equal(fh_AMPL_YL$model$variance, AMPL_YL_modelfit$variance[1])
            
################################# AMRL_YL ######################################
            # AMRL_YL model fitting (current version) 
            fh_AMRL_YL <- fh(fixed = Mean ~ cash + self_empl, vardir = "Var_Mean",
                             combined_data = combined_data, domains = "Domain",
                             method = "amrl_yl", interval = c(0, 10000000), MSE = TRUE)
            
            # AMRL_YL model fitting status quo (benchmark)
            AMRL_YL_modelfit <- read.csv("FH/AMRL_YL_modelfit.csv", sep = ",")  

            # Compare results from current version and benchmark
            # EBLUP
            expect_equal(fh_AMRL_YL$ind[, c("Domain","FH")], 
                         AMRL_YL_modelfit[, c("Domain","FH")])
            # MSE
            expect_equal(fh_AMRL_YL$MSE$FH, AMRL_YL_modelfit$MSE)
            # Variance
            expect_equal(fh_AMRL_YL$model$variance, AMRL_YL_modelfit$variance[1]) 
            })
