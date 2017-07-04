# Datensätze im Paket ----------------------------------------------------------
data("eusilcA_pop")
data("eusilcA_smp")



# Beispiele im Paket -----------------------------------------------------------

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



# Direct #######################################################################
0.6 * laeken::weightedMedian(eusilcA_smp$eqIncome, weights = eusilcA_smp$weight)


for (i in 1:14) {
  
emdi_directNEU <- direct(y = "eqIncome", 
                      smp_data = eusilcA_smp, 
                      smp_domains = "district", 
                      weights = "weight", 
                      design = NULL,
                      #threshold = function(y, weights){0.6 * laeken::weightedMedian(eusilcA_smp$eqIncome, weights = eusilcA_smp$weight)}, 
                      threshold = threshold1,
                      var = FALSE, 
                      boot_type = "naive", 
                      B = 2, 
                      seed = 123, 
                      #X_calib = as.matrix(as.numeric(eusilcA_smp$gender)),
                      X_calib = cbind(as.numeric(list_poss[[13]]$gender),
                                          as.numeric(list_poss[[13]]$state)),
                      totals = char_skalar,
                      #X_calib = list_poss[[10]],
                      #totals = NULL, 
                      na.rm = TRUE,
                      custom_indicator = list( my_max = function(weights, threshold){max(y)}, 
                                              my_min = function(weights, threshold){min(y)}))

}

# Only default values
direct_default <- direct(y = "eqIncome",                  
               smp_data = eusilcA_smp, 
               smp_domains = "district", 
               var = FALSE)

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
threshold1 = function(y, weights){0.6 * laeken::weightedMedian(eusilcA_smp$eqIncome, 
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
# When boot type is calibrate, we get: If boot_type is set to 'calibrate', 
# X_calib must be a numeric matrix which colums have the same length as the 
# colums in the sample data. See also help(direct).
# for all possibilities.
# Does only work when it is a numeric matric which colums lengths equals the 
# colum length of the sample data.
# When boot type is naive, it does not matter how X_calib is specified. 


# Try all possibilities above for totals
# It works for num_skalar and ind_skalar when X_calib has one colum and 
# it works for num_vek and ind_vek when x_calib has two colums, which is fine
# Otherwise, we get: If boot_type is set to 'calibrate', totals can be NULL or must be 
# a numeric vector which length equals to the number of colums in 
# X_calib. See also help(direct).
# When boot type is naive, it does not matter how totals is specified. 
 
# Try all possibilities above for na.rm
# It works for log_skalar which is fine
# Otherwise, we get: na.rm needs to be a logical value. Set na.rm to TRUE or 
# FALSE. See also help(direct).

# Try all possibilities above for custom_indicators
# It works for NULL which is fine
# Otherwise, we get: Additional indicators need to be added in argument 
# custom_indicator as a list of functions. For help see Example 3 in help(direct).
# Problem: The functions itself are not checked! 

# Works now, also with checking elements!!!
# But this warning is added automatically:
#Error in direct_check(y = y, smp_data = smp_data, smp_domains = smp_domains,  : 
# Functions for custom indicators need to have exactly the following 
# three arguments: y, weights threshold; even though weights might 
# not be needed and a threshold might not be 
#included in the indicator. For help see Example 3 in help(direct). 
#In addition: Warning message:
# In names(formals(custom_indicator[[i]])) == c("y", "weights", "threshold") :
# longer object length is not a multiple of shorter object length


# Methods for direct

# Information about data
# These two function only have one argument and this is the emdi object, so no
# further arguments for checking
print(emdi_direct)
summary(emdi_direct)



# Check error messages
# This method is not available for an emdi object of class direct and it return 
# a suitable error message. 
plot(emdi_direct)


# Choose indicators
# 1. Case: No MSE estimation conducted.
estimators(object = emdi_direct, MSE = F, CV = F, indicator = names(emdi_direct$ind[-1]))


# Try all possibilities above for custom_indicators
# We get for all: indicator is a character vector that can only contain the 
# names of estimated indicators or 'all' or indicator groups as described in 
# help(estimators.emdi).

head(estimators(object = emdi_direct, MSE = F, CV = F, indicator = list_poss[[6]]))
tail(estimators(object = emdi_direct, MSE = T, CV = FALSE, indicator = c("Gini", "Mean")))



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
         #map_obj = NULL,
         indicator = "poverty", 
         map_dom_id = "NAME_2", 
         map_tab = mapping_table,
         #col = c("white", "black"), 
         return_data = FALSE)



# Try all possibilities: map_obj
# If it is NULL: No Map Object has been provided. An artificial polygone is 
# used for visualization. For real visualization give an object of 
# class SpatialPolygonsDataFrame to the argument map_obj.
# Otherwise we get: map_obj is not of class SpatialPolygonsDataFrame from the 
# sp package

# Try all possibilities: map_dom_id
# We get in all cases:  A domain ID needs to be given by argument map_dom_id. 
# This argument needs to be a single character that indicates a variable
# in map_obj. Thus, it also needs to be contained in map_obj. 
# See also help(map_plot). 


# Try all possibilities: map_tab
# We get in most cases: If the IDs in the data object and shape file differ a 
# mapping table needs to be used. This table needs to be a data frame with 
# two colums. See also help(map_plot). 
# If a data frame with two colums in given but it does not fit, it comes: 
# Domains of map_tab and Map object do not match. Check map_tab 
# If the domains do not match and map_tab is still NULL, we get an unspecific
# error. 

# Try all possibilities: col
# For num_vek it works 10 and 10 seem to specify colours and for ind_vek but e.g. 
# for char_vek we get invalid color name 'Hallo'  which is understandable, the 
# same for TRUE. 
# Otherwise we get: col needs to be a vector of length 2 defining the starting 
# and upper color of the map-plot


# Try all possibilities: return_data
# Wet get: return_data needs to be a logical value. Set na.rm to TRUE or FALSE. See 
# also help(direct).



# Export to excel
write.excel(emdi_direct, file = "excel_output.xlsx")



# Model-based ------------------------------------------------------------------

# Add possibility
list_poss[[15]] <- hallo ~ es + geht + noch

# Check arguments in model
emdi_model <- ebp(fixed = eqIncome ~ gender + eqsize + cash + self_empl + 
                  unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + 
                    rent + fam_allow + house_allow + cap_inv + tax_adj,
                   pop_data = eusilcA_pop,
                   pop_domains = "district",
                   smp_data = eusilcA_smp,
                   smp_domains = "district",
                   threshold = function(y){0.6 * median(y)},
                   transformation = "box.cox",
                   interval = c(-1,2),
                   L = 50,
                   MSE = TRUE,
                   boot_type = "parametric",
                   B = 50,
                   seed = 100,
                   custom_indicator = list( my_max = function(y, threshold){max(y)},
                                            my_min = function(y, threshold){min(y)}),  
                   na.rm = TRUE, 
                   cpus = 5
)


# Try possibilities: fixed
# We always get: Fixed must be a formula object. See also help(ebp).
# If it is formula but the variables are not in the data sets, we get:
# Both the variable name in pop_domains and the explanatory variables
# in argument fixed need to be contained in pop_data. 


# Try possibilities: pop_data
# If it is not a data frame, we get: Pop_data must be a data frame containing 
# population data. See also help(ebp).
# If it is a data frame but does not contain the variables in fixed:
# Both the variable name in pop_domains and the explanatory variables
# in argument fixed need to be contained in pop_data.
# You can use smp data for pop data but this cannot be controlled.


# Try possibilities: pop_domains
# We get if it is not a character: Pop_domains must be a single character containing 
# the name of a numeric or factor variable indicating domains in the population 
# data. See also help(ebp).
# If it is a single character but it does not fit: Both the variable name in 
# pop_domains and the explanatory variables in argument fixed need to be 
# contained in pop_data. 
# There is no plausible error when as pop_domains e.g. cash is chosen.


# Try possibilities: smp_data
# We get: Smp_data must be a data frame containing sample data. See also help(ebp). 
# And if it is a data frame without the right variables: The variable name 
# in smp_domains and the variables in argument fixed need to be contained in 
# smp_data. 


# Try possibilities: smp_domains
# We get if it is not a character: Smp_domains must be a single character 
# containing the name of a numeric or factor variable indicating domains 
# in the sample data. See also help(ebp). 
# If it is a single character that does not fit:
# The variable name in smp_domains and the variables
# in argument fixed need to be contained in smp_data.


# Try possibilities: threshold
# For NULL, num_skalar and ind_skalar it works, but this is fine
# Otherwise we get:  threshold needs to be a single number or a function of y. 
# If it is NULL 60% of the median is selected as threshold. See also help(ebp).
# Added that argument of function in threshold is checked.

# Try possibilities: transformation
# We always get: The three options for transformation are ''no'', ''log'' or 
# ''box.cox''.

# Try possibilities: interval
# If it is ind_skalar, it works but this is fine. 
# We always get: interval needs to be a vector of length 2 
# defining a lower and upper limit for the estimation of the optimal 
# transformation parameter. The value of the lower limit needs to be 
# smaller than the upper limit. See also help(ebp). 


# Try possibilities: L
# If it is num_skalar or ind_skalar it works but this is fine. 
# Otherwise we get: L needs to be a single number determining the
# number of Monte-Carlo simulations. See also help(ebp). 


# Try possibiltiies: MSE
# If it is log_skalar, it works but this it fine. 
# Otherwise we get: MSE must be a logical value. Set MSE to TRUE or FALSE. See 
# also help(ebp). 


# Try possibilities: boot_type
# We always get: The two bootstrap procedures are ''parametric'' or ''wild''. 

# Try possibilities: B
# It is it num_skalar or ind_skalar it works but this is fine. 
# Otherwise we get:  If MSE is set to TRUE, a single number for the number of
# bootstrap sample needs to be chosen. See also help(ebp). 


# Try possibilities: seed
# If it is NULL, num_skalar or ind_skalar it works but this is fine
# Otherwise we get:  Seed must be a single number or NULL as initialisation 
# of the RNG. See also help(ebp). 


# Try possibilities: custom_indicator
# If it is not a list:  Additional indicators need to be added in argument 
# custom_indicator as a list of functions. For help see Example 3 in help(direct).
# If it is a list but elements are not functions: The elements of the list need 
# to be functions. These Functions for custom indicators need to have exactly 
# the following two arguments: y, threshold; even though a threshold might not 
# included in the indicator. See also help(ebp). 
# If list elements are functions but with wrong arguments: Functions for custom 
# indicators need to have exactly the following two arguments: y, threshold; 
# even though a threshold might not included in the indicator. See also help(ebp). 



# Information about data and model
print(emdi_model)
summary(emdi_model)



plot(emdi_model, label = "orig", color = c("green","yellow"))


# Choose indicators
estimators(object = emdi_model, MSE = F, CV = F, indicator = "all")
head(estimators(object = emdi_model, MSE = T, CV = T, indicator = c("Gini", "Median")))
tail(estimators(object = emdi_model, MSE = T, CV = T, indicator = c("Head_Count","Poverty_Gap")))



# Map plot function
mapping_table <- data.frame(unique(eusilcA_pop$district), 
                            unique(shape_austria_dis$NAME_2))


map_plot(object = emdi_model, MSE = TRUE, CV = FALSE, map_obj = shape_austria_dis,
    indicator = "Gini", map_dom_id = "NAME_2", map_tab = mapping_table)


# Export to excel
write.excel(emdi_model2, file = "excel_output.xlsx")

