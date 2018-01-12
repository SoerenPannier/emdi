devtools::install_github("soerenpannier/emdi")
library(emdi)


# Datensätze im Paket ----------------------------------------------------------
data("eusilcA_pop")
data("eusilcA_smp")



# Beispiele im Paket -----------------------------------------------------------

# Direct
0.6 * laeken::weightedMedian(eusilcA_smp$eqIncome, weights = eusilcA_smp$weight)
emdi_direct <- direct(y = "eqIncome", 
                      smp_data = eusilcA_smp, 
                      smp_domains = "district", 
                      weights = "weight", 
                      threshold = 11064.82, 
                      var = TRUE, 
                      boot_type = "naive", 
                      B = 5, 
                      seed = 123, 
                      X_calib = NULL, 
                      totals = NULL, 
                      na.rm = TRUE)

# Gibt Warnungen zurück

emdi_direct <- direct(y = "eqIncome", smp_data = eusilcA_smp, smp_domains = "district", 
                      weights = "weight", threshold = 11064.82, var = TRUE, 
                      boot_type = "naive", B = 50, 
                      seed = 123, X_calib = NULL, totals = NULL, custom_indicator = list( my_max = 
                      function(y, weights, threshold){max(y)}, my_min = 
                      function(y, weights, threshold){min(y)}), na.rm = TRUE)

# Wie gibt man threshold mit Gewichten rein?
emdi_direct_varth <- direct(y = "eqIncome", 
                      smp_data = eusilcA_smp, 
                      smp_domains = "district", 
                      weights = "weight", 
                      threshold = function(y, weights){0.6 * laeken::weightedMedian(eusilcA_smp$eqIncome, weights = eusilcA_smp$weight)},#11064.82, 
                      var = TRUE, 
                      boot_type = "naive", 
                      B = 5, 
                      seed = 123, 
                      X_calib = NULL, 
                      totals = NULL, 
                      na.rm = TRUE)

# Information about data
print(emdi_direct)
summary(emdi_direct)



# Check error messages
plot(emdi_direct)


# Choose indicators
estimators(object = emdi_direct, MSE = F, CV = F, indicator = "all")
head(estimators(object = emdi_direct, MSE = F, CV = T, indicator = c("Head_Count","Poverty_Gap")))
tail(estimators(object = emdi_direct, MSE = T, CV = T, indicator = c("Head_Count","Poverty_Gap")))



# Map plot

# Are the domains defined equally in census and shape file?
unique(eusilcA_pop$district)
class(eusilcA_pop$district)
length(unique(eusilcA_pop$district))

# Load shape file
load_shapeaustria()

# How is the variable defined?
class(shape_austria_dis$NAME_2)
length(unique(shape_austria_dis$NAME_2))

# Match data and shape variable
mapping_table <- data.frame(unique(eusilcA_pop$district), 
                            unique(shape_austria_dis$NAME_2))

map_plot(object = emdi_direct, 
         MSE = TRUE, 
         CV = FALSE, 
         map_obj = shape_austria_dis,
         indicator = c("Gini"), 
         map_dom_id = "NAME_2", 
         map_tab = mapping_table,
         return_data = FALSE)


# Export to excel
write.excel(emdi_direct, file="excel_output.xlsx")



# Model-based ------------------------------------------------------------------
set.seed(100)
emdi_model <- ebp( fixed = eqIncome ~ gender + eqsize + cash + self_empl + 
                     unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + 
                     rent + fam_allow + house_allow + cap_inv + tax_adj,
                   pop_data = eusilcA_pop,
                   pop_domains = "district",
                   smp_data = eusilcA_smp,
                   smp_domains = "district",
                   na.rm = TRUE,
                   L = 5, B=5, MSE = T,boot_type = "wild"
)

set.seed(100)
emdi_model_varth <- ebp( fixed = eqIncome ~ gender + eqsize + cash + self_empl + 
                     unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + 
                     rent + fam_allow + house_allow + cap_inv + tax_adj,
                   pop_data = eusilcA_pop,
                   pop_domains = "district",
                   smp_data = eusilcA_smp,
                   smp_domains = "district",
                   na.rm = TRUE,
                   threshold = function(y){0.6 * median(y)},
                   L = 5, B = 5, MSE = T
)


emdi_model2 <- ebp( fixed = eqIncome ~ gender + eqsize + cash + self_empl + 
                     unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + 
                     rent + fam_allow + house_allow + cap_inv + tax_adj,
                   pop_data = eusilcA_pop,
                   pop_domains = "district",
                   smp_data = eusilcA_smp,
                   smp_domains = "district",
                   threshold = 10722.66,
                   transformation = "no",
                   L= 50,
                   MSE = TRUE,
                   B = 50,
                   custom_indicator = list( my_max = function(y, threshold){max(y)},
                                            my_min = function(y, threshold){min(y)}
                   ),  
                   na.rm = TRUE, 
                   cpus = 1
)


# Information about data and model
print(emdi_model)
summary(emdi_model)
plot(emdi_model)

print(emdi_model2)
summary(emdi_model2)
plot(emdi_model2)

# Choose indicators
estimators(object = emdi_model2, MSE = F, CV = F, indicator = "all")
head(estimators(object = emdi_model2, MSE = F, CV = T, indicator = c("Head_Count","Poverty_Gap")))
tail(estimators(object = emdi_model2, MSE = T, CV = T, indicator = c("Head_Count","Poverty_Gap")))



# Map plot function
mapping_table <- data.frame(unique(eusilcA_pop$district), 
                            unique(shape_austria_dis$NAME_2))


map_plot(object = emdi_model2, MSE = TRUE, CV = FALSE, map_obj = shape_austria_dis,
    indicator = c("Gini"), map_dom_id = "NAME_2", map_tab = mapping_table)


# Export to excel
write.excel(emdi_model2, file="excel_output.xlsx")

