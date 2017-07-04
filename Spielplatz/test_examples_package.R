################################################################################
# Test examples from package since most are dontrun!                            
################################################################################

# Ebp function
emdi_model <- ebp(fixed = eqIncome ~ gender + eqsize + cash + self_empl + 
                  unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + fam_allow + 
                  house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop,
                  pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district", 
                  na.rm = TRUE)
# funktioniert

emdi_model <- ebp(fixed = eqIncome ~ gender + eqsize + cash + 
                  self_empl + unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + 
                  fam_allow + house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop,
                  pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district",
                  threshold = function(y){0.6 * median(y)}, transformation = "log", 
                  L= 50, MSE = TRUE, boot_type = "wild", B = 50, custom_indicator = 
                  list( my_max = function(y, threshold){max(y)},
                  my_min = function(y, threshold){min(y)}), na.rm = TRUE, cpus = 1)
# funktioniert


# Direct estimation
emdi_direct <- direct(y = "eqIncome", smp_data = eusilcA_smp, 
                      smp_domains = "district", weights = "weight", 
                      threshold = 11064.82, var = TRUE, 
                      boot_type = "naive", B = 50, seed = 123, X = NULL, 
                      totals = NULL, na.rm = TRUE)
# funktioniert


emdi_direct <- direct(y="eqIncome", smp_data=eusilcA_smp, smp_domains="district", 
                      weights="weight", 
                      threshold=function(y, weights) {0.6 * Hmisc::wtd.quantile(eusilcA_smp$eqIncome, weights=eusilcA_smp$weight, probs=0.5)}, 
                      var=TRUE, bootType = "naive", B=50, 
                      seed=123, X = NULL, totals = NULL, custom_indicator = list( my_max = 
                      function(y, weights, threshold){max(y)}, my_min = 
                      function(y, weights, threshold){min(y)}), na.rm=TRUE)
# Warnings


# estimators
estimators(emdi_model, indicator = "Gini", MSE = TRUE, CV = TRUE)
estimators(emdi_model, indicator = "Custom")

# map_plot
# Load shape file
load_shapeaustria()

# Create mapping table such that variables that indicate domains correspond
# in population data and shape file
mapping_table <- data.frame(unique(eusilcA_pop$district), 
unique(shape_austria_dis$NAME_2))
 
# Create map plot for mean indicator - point and MSE estimates but no CV
map_plot(object = emdi_model, MSE = TRUE, CV = FALSE, 
map_obj = shape_austria_dis, indicator = c("Mean"), map_dom_id = "NAME_2", 
map_tab = mapping_table)
# hat geklappt


# plot function
# Creation of default diagnostic plots
plot(emdi_model)

# Creation of diagnostic plots without labels and titles, different colors 
# and without Cook's distance plot.
plot(emdi_model, label="no_title", color=c("red", "yellow"), cooks = FALSE)

# summary function
# Example with two additional indicators
emdi_model <- ebp(fixed = eqIncome ~ gender + eqsize + cash + 
self_empl + unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + 
fam_allow + house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop,
pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district",
threshold = function(y){0.6 * median(y)}, L= 50, MSE = TRUE, B = 50, 
custom_indicator = list( my_max = function(y, threshold){max(y)},
my_min = function(y, threshold){min(y)}), na.rm = TRUE, cpus = 1)

# Receive first overview
summary(emdi_model)

# write.excel
# Export estimates for all indicators and uncertainty measures and 
# diagnostics to excel
write.excel(emdi_model, file ="excel_output_all.xlsx", indicator = "all", 
MSE = TRUE, CV = TRUE)
 
# Single excel sheets for point, MSE and CV estimates
write.excel(emdi_model, file ="excel_output_all_split.xlsx", indicator = "all", 
MSE = TRUE, CV = TRUE, split=TRUE)
