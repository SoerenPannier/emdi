my_threshold <- function(y){
  0.6 * median(y)
}

my_rmpg <- function(y, threshold){
  rmpg(inc = y)$value
}

my_indicators <- list(my_rmpg = my_rmpg)


data("eusilcA_pop")
data("eusilcA_smp")

set.seed(100)
emdi_model <- ebp(fixed = eqIncome ~ gender + eqsize + cash + self_empl + 
                     unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + 
                     rent + fam_allow + house_allow + cap_inv + tax_adj,
                   pop_data = eusilcA_pop,
                   pop_domains = "district",
                   smp_data = eusilcA_smp,
                   smp_domains = "district",
                   na.rm = TRUE,
                   L = 5, 
                   B = 5, 
                   MSE = T, 
                   threshold = my_threshold ,
                   custom_indicator = my_indicators
)
estimators(emdi_model, indicator = "custom")
