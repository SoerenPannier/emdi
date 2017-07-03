# Datensätze im Paket ----------------------------------------------------------
data("eusilcA_pop")
data("eusilcA_smp")



# Beispiele im Paket -----------------------------------------------------------

# Direct
0.6 * laeken::weightedMedian(eusilcA_smp$eqIncome, weights = eusilcA_smp$weight)

# Teste mit Skalaren
# numeric
num_skalar <- 5
# character
char_skalar <- "Hallo"
# integer 
ind_skalar <- 5L
# logical 
log_skalar <- TRUE

# Teste mit Vektoren
# numeric
num_vek <- c(10,10)
# character
char_vek <- c("Hallo", "Tschüss")
# integer 
ind_vek <- c(5L, 10L)
# logical 
log_vek <- c(TRUE, FALSE)

# Teste mit matrix
# numeric
num_mat <- matrix(10,10, 11)
# integer 
ind_mat <- matrix(4L ,5L, 10L)
# logical 
log_mat <- matrix(FALSE, 5, 10)

# Text data_frame
data <- data.frame(Skalar = c(num_skalar, char_skalar, log_skalar, ind_skalar), 
                   Vek = c(num_vek, ind_vek))
data_test <- eusilcA_smp
data_test$district_char <- as.character(data_test$district)


list_poss <- list(num_skalar,
                  char_skalar,
                  ind_skalar,
                  log_skalar,
                  num_vek,
                  char_vek,
                  ind_vek,
                  log_vek, 
                  num_mat,
                  ind_mat, 
                  log_mat,
                  data, 
                  data_test, 
                  NULL)


emdi_direct <- direct(y = "eqIncome", 
                      smp_data = eusilcA_smp, 
                      smp_domains = "district", 
                      weights = "weight", 
                      design = NULL,
                      threshold = function(y, weights){0.6 * laeken::weightedMedian(eusilcA_smp$eqIncome, weights = eusilcA_smp$weight)}, 
                      var = TRUE, 
                      boot_type = "calibrate", 
                      B = 2, 
                      seed = 123, 
                      #X_calib = as.matrix(as.numeric(eusilcA_smp$gender)),
                      X_calib = cbind(as.numeric(list_poss[[13]]$gender),
                                          as.numeric(list_poss[[13]]$state)),
                      totals = NULL, 
                      na.rm = TRUE,
                      custom_indicator = list(my_max = function(y, weights, threshold){max(y)}, 
                                              my_min = function(y, weights, threshold){min(y)}))


# Try all possibilities above for y
# We always get: y must be a character indicating the variable that is used for 
# estimating the indicators. See also help(direct).
# char_skalar hat keine eigene Fehlermeldung, sondern undefined colums 
# selected, aber dies sagt eigentlich auch, was falsch ist
# Ich weiß grad keine Lösung, das anders abzufangen


# Try all possibilities above for smp_data
# We always get: Smp_data must be a data frame containing the variable y.
# See also help(direct).
# data hat keine eigene Fehlermeldung, sondern undefined colums 
# selected, aber dies sagt eigentlich auch, was falsch ist
# Ich weiß grad keine Lösung, das anders abzufangen


# Try all possibilities above for smp_domains
# We always get: Smp_domains must be a single character containing the name of a 
# variable indicating domains in the sample data. See also help(direct). 
# char_skalar hat keine eigene Fehlermeldung, sondern undefined columns selected 


# Try all possibilities above for weights
# We always get: Weights must be a single character containing the name of a variable 
# for the sampling weights in the sample data. See also help(direct).
# char_skalar hat keine eigene Fehlermeldung, sondern undefined columns selected 

# Try all possibilities above for design
# We always get: Design must be a single character containing the name of a variable 
# for the sampling design in the sample data. See also help(direct).
# char_skalar hat keine eigene Fehlermeldung, sondern undefined columns selected 


# Try all possibilities above for threshold
# It works fine for num_skalar and ind_skalar, but this is ok even though 5 does 
# not make sense.
# Otherwise we get: threshold needs to be a single number or a function of y and 
# weights. If it is NULL 60% of the median of the target variable is selected 
# as threshold. See also help(direct).

# Excursus: How to check if function for threshold has arguments y and weights?
threshold = function(y, weights){0.6 * laeken::weightedMedian(eusilcA_smp$eqIncome, 
                                                              weights = eusilcA_smp$weight)}

attributes(alist(y = , weights = ))$names

all(attributes(formals(threshold))$names == c("y", "weights")) 


# Try all possibilities above for var
# It works for log_skalar, thats how it should be. 
# Otherwise we get: Var must be a logical value. Set Var to TRUE or FALSE. See also
# help(direct).

# Try all possibilities above for boot_type
# We always get: If var is set to TRUE, boot_type "naive" or "calibrate" needs to be 
# selected. See also help(direct). 

# Try all possibilities above for B 
# It works for num_skalar and ind_skalar, which is fine. 
# Otherwise, we get: If var is set to TRUE, a single number for the number of
# bootstrap sample needs to be chosen. See also help(direct).

# Try all possibilities above for seed
# It works for num_skalar and ind_skalar, which is fine. 
# Otherwise we get: Seed must be a single number or NULL as initialisation of 
# the RNG. See also help(direct).  


# Try all possibilities above for X_calib

 



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
         return_data = F)


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

