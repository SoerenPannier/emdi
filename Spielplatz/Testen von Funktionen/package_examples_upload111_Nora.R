
remove.packages("emdi")

# Direct function
# Example 1: Without weights and naive bootstrap
emdi_direct <- direct(y = "eqIncome", smp_data = eusilcA_smp, 
smp_domains = "district", weights = "weight", threshold = 11064.82, var = TRUE, 
boot_type = "naive", B = 50, seed = 123, X = NULL, totals = NULL, na.rm = TRUE)
# klappt



# Example 2: With function as threshold
emdi_direct <- direct(y = "eqIncome", smp_data = eusilcA_smp, 
smp_domains = "district", weights = "weight", threshold = 
function(y, weights){0.6 * laeken::weightedMedian(y, weights)}, na.rm = TRUE)


# Example 3: With custom indicators
emdi_direct <- direct(y = "eqIncome", smp_data = eusilcA_smp, 
smp_domains = "district", weights = "weight", threshold = 10859.24, 
var = TRUE, boot_type = "naive", B = 50, seed = 123, X = NULL, totals = NULL, 
custom_indicator = list( my_max = function(y, weights, threshold){max(y)}, 
my_min = function(y, weights, threshold){min(y)}), na.rm = TRUE)

#TEST direct- andere Argumente:
emdi_direct <- direct(y = "eqIncome", smp_data = eusilcA_smp, 
                      smp_domains = "district")
emdi_direct <- direct(y = "eqIncome", smp_data = eusilcA_smp, 
                      smp_domains = "district", weights = "weight", threshold = 
                        function(y, weights){
                          0.6 * laeken::weightedMedian(y, weights)}, 
                      na.rm = FALSE)
totals <- rep(10000,2)
X_calib <- cbind(rep(0.5,1000), rep(0.5,1000))
emdi_direct <- direct(y = "eqIncome", smp_data = eusilcA_smp, 
                      smp_domains = "district", weights = "weight", threshold = 11064.82, var = TRUE, 
                      boot_type = "calibrate", X_calib  = X_calib  ,  B = 50, seed = 123, totals = totals, na.rm = TRUE)

# EBP function
# Example 1: With default setting but na.rm=TRUE
emdi_model <- ebp(fixed = eqIncome ~ gender + eqsize + cash + self_empl + 
unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + fam_allow + 
house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop,
pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district", 
na.rm = TRUE, interval = c(-5,2))

# Example 2: With MSE, two additional indicators and function as threshold
emdi_model <- ebp(fixed = eqIncome ~ gender + eqsize + cash + 
self_empl + unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + 
fam_allow + house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop,
pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district",
threshold = function(y){0.6 * median(y)}, transformation = "log", 
L = 50, MSE = TRUE, boot_type = "wild", B = 50, custom_indicator = 
list( my_max = function(y, threshold){max(y)},
my_min = function(y, threshold){min(y)}), na.rm = TRUE, cpus = 1)



# Function estimators
# generate emdi object with additional indicators; here via function ebp()
emdi_model <- ebp(fixed = eqIncome ~ gender + eqsize + cash + 
self_empl + unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + 
fam_allow + house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop,
pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district",
threshold = 11064.82, transformation = "box.cox", 
L = 50, MSE = TRUE, B = 50, custom_indicator = 
list( my_max = function(y, threshold){max(y)},
my_min = function(y, threshold){min(y)}), na.rm = TRUE, cpus = 1)

# TEST: Parameter
emdi_model <- ebp(fixed = eqIncome ~ gender + eqsize + cash + self_empl + 
                    unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + fam_allow + 
                    house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop,
                  pop_domains = "district", 
                  smp_data = eusilcA_smp, smp_domains = "district" 
                  ,na.rm = TRUE, interval = c(-1,2), MSE = TRUE)

emdi_model <- ebp(fixed = eqIncome ~ gender + eqsize + cash + self_empl + 
                    unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + fam_allow + 
                    house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop,
                  pop_domains = "district", 
                  smp_data = eusilcA_smp, smp_domains = "district" 
                  ,na.rm = TRUE, interval = c(-1,2), MSE = TRUE, B= 1)

# Example 1: Choose Gini coefficient and MSE and CV
estimators(emdi_model, indicator = "Gini", MSE = TRUE, CV = TRUE)

# Example 2: Choose custom indicators without MSE and CV
estimators(emdi_model, indicator = "Custom")

# Functions head, tail, subset, as.data.frame, as.matrix

# generate emdi object with deleting missing values; here via function ebp()
emdi_model <- ebp( fixed = eqIncome ~ gender + eqsize + cash + 
self_empl + unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + 
fam_allow + house_allow + cap_inv + tax_adj,
pop_data = eusilcA_pop, pop_domains = "district",
smp_data = eusilcA_smp, smp_domains = "district",
na.rm = TRUE)

# Example: Choose first lines of the Gini coefficient, MSE and CV
head(estimators(emdi_model, indicator = c("Gini", "Head_Count")))


# Example: Choose last lines of the Gini coefficient, MSE and CV
tail(estimators(emdi_model, indicator = c("Gini", "Head_Count")))

# Example: Choose last lines of the Gini coefficient, MSE and CV
subset(estimators(emdi_model, indicator = "Gini"), 
Domain %in% c("Wien", "Wien Umgebung"))


as.data.frame(estimators(emdi_model))
as.matrix(estimators(emdi_model))


# Test loading shape file
load_shapeaustria()


# Map plot function
# generate emdi object with additional indicators; here via function ebp()
emdi_model <- ebp(fixed = eqIncome ~ gender + eqsize + cash + 
self_empl + unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + 
fam_allow + house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop,
pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district",
threshold = 11064.82, transformation = "box.cox", L = 50, MSE = TRUE, B = 50, 
custom_indicator = list( my_max = function(y, threshold){max(y)},
my_min = function(y, threshold){min(y)}), na.rm = TRUE, cpus = 1)


# Load shape file
load_shapeaustria()
 
# Create mapping table such that variables that indicate domains correspond
# in population data and shape file
mapping_table <- data.frame(unique(eusilcA_pop$district), 
unique(shape_austria_dis$NAME_2))

# Example 1: Create map plot for mean indicator - point and MSE estimates 
# but no CV
map_plot(object = emdi_model, MSE = TRUE, CV = FALSE, 
map_obj = shape_austria_dis, indicator = c("Mean"), map_dom_id = "NAME_2", 
map_tab = mapping_table)


# Example 2:
# Now we are creating plots for the Mean and the Median with both their precision
# measures, while forcing coloring ranges of both indicators to be the same
 
# First, define appropriate scales for each indicator and measure and save it 
# in a nested list as seen below
#' 
my_scale_points <- list(Mean   = list(ind = c(0, 75000),
                                    MSE = c(45000, 15000000),
                                    CV  = c(0, 0.5)
                                   ),
                        Median = list(ind = c(0, 75000),
                                      MSE = c(45000, 15000000),
                                      CV  = c(0, 0.5)
                                      )
                       )
# When done so, this list may be used as an argument to map_plot 
  
map_plot(object = emdi_model, MSE = TRUE, CV = TRUE, 
map_obj = shape_austria_dis, indicator = c("Mean", "Median"), 
map_dom_id = "NAME_2", map_tab = mapping_table, 
scale_points = my_scale_points)

# Example 3:
# In the simple case, that all plots shall use the same color range,
# the procedure from example 2 may be abbreviated to:
map_plot(object = emdi_model, MSE = FALSE, CV = FALSE, 
map_obj = shape_austria_dis, indicator = c("Mean", "Median"), 
map_dom_id = "NAME_2", map_tab = mapping_table, 
scale_points = c(0, 75000))


# Plot function
# With default setting but na.rm = TRUE; with Box-Cox transformation
emdi_model <- ebp(fixed = eqIncome ~ gender + eqsize + cash + self_empl + 
unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + fam_allow + 
house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop,
pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district", 
na.rm = TRUE)

# Example 1: Creation of default diagnostic plots
plot(emdi_model)

# Example 2: Creation of diagnostic plots without labels and titles, different colors 
# and without Cook's distance plot.
plot(emdi_model, label = "no_title", color = c("red", "yellow"), cooks = FALSE)
 
# Example 3: Creation of diagnostic plots where labels and title differs for 
# residual plot
plot(emdi_model, label = list(qq_res = c(title = "Pearson resid.", 
y_lab = "Quant.", x_lab = "Theo. Quant.")), color = c("red", "yellow"), 
cooks = FALSE)


# Summary function
# Example with two additional indicators
emdi_model <- ebp(fixed = eqIncome ~ gender + eqsize + cash + 
self_empl + unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + 
fam_allow + house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop,
pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district",
threshold = function(y){0.6 * median(y)}, L = 50, MSE = TRUE, B = 50, 
custom_indicator = list( my_max = function(y, threshold){max(y)},
my_min = function(y, threshold){min(y)}), na.rm = TRUE, cpus = 1)

 
# Receive first overview
sum_model <- summary(emdi_model)


# Function write excel

# Example 1: Export estimates for all indicators and uncertainty measures and 
# diagnostics to excel
write.excel(emdi_model, file = "excel_output_all.xlsx", indicator = "all", 
MSE = TRUE, CV = TRUE)



# Example 2: Single excel sheets for point, MSE and CV estimates
write.excel(emdi_model, file = "excel_output_all_split.xlsx", indicator = "all", 
MSE = F, CV = F, split = TRUE)

# Only for testing
emdi_model <- ebp(eqIncome ~ gender, eusilcA_pop, "district", eusilcA_smp, "district")


