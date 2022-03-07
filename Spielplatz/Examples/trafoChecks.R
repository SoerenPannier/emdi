# Load package

# Install the package
install.packages("emdi")
library(emdi)
install.packages('C:/Users/Ann-Kristin/Documents/emdi_2.0.3.tar.gz', repos = NULL, type="source")
library("emdi")

# The ggplot2 package is loaded additionally since we use theme_set for a better
# representation of the plots in the paper but it is otherwise not needed to 
# load the package manually. 
library(ggplot2)
# The laeken package is loaded since we estimate the poverty line in the 
# application with the weightedMedian function from this package.
library(laeken)

# Code in Section Data sets ----------------------------------------------------

# Load sample data set
data("eusilcA_smp")
data('eusilcA_pop')
eusilcA_smp2 <- eusilcA_smp
eusilcA_smp2$eqIncome <- eusilcA_smp2$eqIncome - 1000


# Box-Cox with default  --------------------------------------------------------
emdi_bc <- ebp(fixed = eqIncome ~ gender + eqsize + cash + self_empl +
                   unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + fam_allow + 
                   house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop, 
                 pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district",
                 threshold = 10885.33, MSE = FALSE, transformation = 'box.cox', 
                 interval = 'default',
                 custom_indicator = list(my_max = function(y, threshold){max(y)},
                                         my_min = function(y, threshold){min(y)}))  
summary(emdi_bc)
qqnorm(emdi_bc)

# Box-Cox with default and negative values in y  -------------------------------
emdi_bc <- ebp(fixed = eqIncome ~ gender + eqsize + cash + self_empl +
                 unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + fam_allow + 
                 house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop, 
               pop_domains = "district", smp_data = eusilcA_smp2, smp_domains = "district",
               threshold = 10885.33, MSE = FALSE, transformation = 'box.cox', 
               interval = 'default',
               custom_indicator = list(my_max = function(y, threshold){max(y)},
                                       my_min = function(y, threshold){min(y)}))  
summary(emdi_bc)
qqnorm(emdi_bc)


# Box-Cox with interval  -------------------------------------------------------
emdi_bc <- ebp(fixed = eqIncome ~ gender + eqsize + cash + self_empl +
                 unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + fam_allow + 
                 house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop, 
               pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district",
               threshold = 10885.33, MSE = TRUE, B = 2, transformation = 'box.cox', 
               interval = c(0.8, 2),
               custom_indicator = list(my_max = function(y, threshold){max(y)},
                                       my_min = function(y, threshold){min(y)}))  
summary(emdi_bc)


# Dual with default  --------------------------------------------------------
emdi_dual <- ebp(fixed = eqIncome ~ gender + eqsize + cash + self_empl +
                 unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + fam_allow + 
                 house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop, 
               pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district",
               threshold = 10885.33, MSE = FALSE, transformation = 'dual', 
               interval = 'default',
               custom_indicator = list(my_max = function(y, threshold){max(y)},
                                       my_min = function(y, threshold){min(y)}))  
summary(emdi_dual)
qqnorm(emdi_dual)

# Dual with default and negative values in y  ----------------------------------
emdi_dual <- ebp(fixed = eqIncome ~ gender + eqsize + cash + self_empl +
                   unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + fam_allow + 
                   house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop, 
                 pop_domains = "district", smp_data = eusilcA_smp2, smp_domains = "district",
                 threshold = 10885.33, MSE = FALSE, transformation = 'dual', 
                 interval = 'default',
                 custom_indicator = list(my_max = function(y, threshold){max(y)},
                                         my_min = function(y, threshold){min(y)}))  
summary(emdi_dual)


# Dual with interval  -------------------------------------------------------
emdi_dual <- ebp(fixed = eqIncome ~ gender + eqsize + cash + self_empl +
                 unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + fam_allow + 
                 house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop, 
               pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district",
               threshold = 10885.33, MSE = TRUE, B = 2, transformation = 'dual', 
               interval = c(0.8, 2),
               custom_indicator = list(my_max = function(y, threshold){max(y)},
                                       my_min = function(y, threshold){min(y)}))  
summary(emdi_dual)

# Dual with interval with negative value ---------------------------------------
# Expect error
emdi_dual <- ebp(fixed = eqIncome ~ gender + eqsize + cash + self_empl +
                   unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + fam_allow + 
                   house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop, 
                 pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district",
                 threshold = 10885.33, MSE = FALSE, B = 2, transformation = 'dual', 
                 interval = c(-1, 2),
                 custom_indicator = list(my_max = function(y, threshold){max(y)},
                                         my_min = function(y, threshold){min(y)}))  


# Log-shift with default  --------------------------------------------------------
emdi_logShift <- ebp(fixed = eqIncome ~ gender + eqsize + cash + self_empl +
                   unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + fam_allow + 
                   house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop, 
                 pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district",
                 threshold = 10885.33, MSE = FALSE, transformation = 'log.shift', 
                 interval = 'default',
                 custom_indicator = list(my_max = function(y, threshold){max(y)},
                                         my_min = function(y, threshold){min(y)}))  
summary(emdi_logShift)
qqnorm(emdi_logShift)

# Log-shift with interval  -----------------------------------------------------
emdi_logShift <- ebp(fixed = eqIncome ~ gender + eqsize + cash + self_empl +
                       unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + fam_allow + 
                       house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop, 
                     pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district",
                     threshold = 10885.33, MSE = FALSE, transformation = 'log.shift', 
                     interval = c(0, 20000),
                     custom_indicator = list(my_max = function(y, threshold){max(y)},
                                             my_min = function(y, threshold){min(y)}))  
summary(emdi_logShift)



# Test of plot function --------------------------------------------------------

plot(emdi_bc)
plot(emdi_dual)
plot(emdi_logShift)


# Test individual label
plot(emdi_bc, label = list(qq_res = c(title = "",
                                               y_lab = "Quantiles of pearson residuals",
                                               x_lab = "Theoretical quantiles"),
                                    qq_ran = c(title = "",
                                               y_lab = "Quantiles of random effects",
                                               x_lab = "Theoretical quantiles"),
                                    d_res = c(title = "",
                                              y_lab = "Density",
                                              x_lab = "Pearson residuals"),
                                    d_ran = c(title = "",
                                              y_lab = "Density",
                                              x_lab = "Standardized random effects"),
                                    cooks = c(title = "",
                                              y_lab = "Cook's Distance",
                                              x_lab = "Index"),
                                    box_cox = c(title = "TesT",
                                                   y_lab = "Log-Likelihood",
                                                   x_lab = "expression(lambda)")))


plot(emdi_dual, label = list(qq_res = c(title = "",
                                      y_lab = "Quantiles of pearson residuals",
                                      x_lab = "Theoretical quantiles"),
                           qq_ran = c(title = "",
                                      y_lab = "Quantiles of random effects",
                                      x_lab = "Theoretical quantiles"),
                           d_res = c(title = "",
                                     y_lab = "Density",
                                     x_lab = "Pearson residuals"),
                           d_ran = c(title = "",
                                     y_lab = "Density",
                                     x_lab = "Standardized random effects"),
                           cooks = c(title = "",
                                     y_lab = "Cook's Distance",
                                     x_lab = "Index"),
                           opt_lambda = c(title = "TesT",
                                       y_lab = "Log-Likelihood",
                                       x_lab = "expression(lambda)")))

