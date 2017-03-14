######################################################################################
# data_transformation(fixed, smp_data, transformation = box.cox, lambda = )

# Loading data - sample data
data("eusilcA_smp")

# Transform dependent variable in sample data with Box-Cox transformation
transform_data <- data_transformation(eqIncome ~ gender + eqsize + cash +
                                        self_empl + unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent +
                                        fam_allow + house_allow + cap_inv + tax_adj, eusilcA_smp, "box.cox", 0.7)

# erst verwirrend, dass lambda schon fix ist 


######################################################################################
# ebp


# manche default values angegeben, aber nicht alle

rm(list = ls())

# Loading data - population and sample data
data("eusilcA_pop")
data("eusilcA_smp") # Fehler, vorher 2mal pop

# Example with default setting but na.rm=TRUE
set.seed(100); emdi_model <- ebp( fixed = eqIncome ~ gender + eqsize + cash +
                                    self_empl + unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent +
                                    fam_allow + house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop,
                                  pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district",
                                  L= 1, na.rm = TRUE)
emdi_model

# Example with default setting but na.rm=TRUE und MSE
set.seed(100); emdi_model <- ebp( fixed = eqIncome ~ gender + eqsize + cash +
                                    self_empl + unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent +
                                    fam_allow + house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop,
                                  pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district",
                                  L= 1, na.rm = TRUE, MSE = TRUE, B = 2)
emdi_model
summary(emdi_model)

# Example with two additional indicators
set.seed(100); emdi_model <- ebp( fixed = eqIncome ~ gender + eqsize + cash +
                                    self_empl + unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + fam_allow + house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop,
                                  pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district",
                                  pov_line = 10722.66, transformation = "box.cox", L= 1, MSE = TRUE, B = 1,
                                  custom_indicator = list( my_max = function(y, pov_line){max(y)},
                                                           my_min = function(y, pov_line){min(y)}), na.rm = TRUE, cpus = 1)
emdi_model
summary(emdi_model)
plot(emdi_model)


estimators(object = emdi_model, CV = T)
call("MSE")
call
framework
emdiObject
emdi_model$model

rm(list = ls())

#########################################################################################
# estimators.emdi
# Loading data - population and sample data
data("eusilcA_pop")
data("eusilcA_smp")
# generate emdi object with additional indicators; here via function ebp()
set.seed(100); emdi_model <- ebp( fixed = eqIncome ~ gender + eqsize + cash +
                                    self_empl + unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent +
                                    fam_allow + house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop,
                                  pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district",
                                  pov_line = 10722.66, transformation = "box.cox", L= 1, MSE = TRUE, B = 1,
                                  custom_indicator = list( my_max = function(y, pov_line){max(y)},
                                                           my_min = function(y, pov_line){min(y)}), na.rm = TRUE, cpus = 1)
# choose Gini coefficient and MSE and CV
estimators(emdi_model, indicator = "Gini", MSE = TRUE, CV = TRUE)
# choose custom indicators without MSE and CV
estimators(emdi_model, indicator = "Custom", MSE = TRUE)

rm(list = ls())

#############################################################################################
# head.estimators.emdi

head(emdi_model$MSE$Poverty_Gap, n = 2, addrownums = TRUE)
# Beispiel fehlt in Beschreibung. Wäre etwas leicher. addrownums? TRUE/FALSE richtig? egal welche Zahlen man eingibt, immer gleiches Ergebnis

rm(list = ls())

############################################################################################
# map_plot

## Not run:
# Loading data - population and sample data
data("eusilcA_pop")
data("eusilcA_smp")
# generate emdi object with additional indicators; here via function ebp()
set.seed(100); emdi_model <- ebp( fixed = eqIncome ~ gender + eqsize + cash +
                                    self_empl + unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent +
                                    fam_allow + house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop,
                                  pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district",
                                  pov_line = 10722.66, transformation = "box.cox", L= 1, MSE = TRUE, B = 1,
                                  custom_indicator = list( my_max = function(y, pov_line){max(y)},
                                                           my_min = function(y, pov_line){min(y)}), na.rm = TRUE, cpus = 1)
# Load shape file
load(system.file("shapes/shape_austria_dis.RData", package="emdi"))
# Create mapping table such that variables that indicate domains correspond
# in population data and shape file
mapping_table <- data.frame(unique(eusilcA_pop$district),
                            unique(shape_austria_dis$NAME_2))
# when rgeos is not available, polygon geometry computations in
# maptools depends on the package gpclib,
# which has a restricted licence. It is disabled by default;
# to enable gpclib, type gpclibPermit()
library(rgeos)
library(maptools)
gpclibPermit()
# Create map plot for mean indicator - point and MSE estimates but no CV
map_plot(object = emdi_model, MSE = TRUE, CV = TRUE,
         map_obj = shape_austria_dis, indicator = c("Mean", "Median"), map_dom_id = "NAME_2",
         map_tab = mapping_table)

rm(list = ls())

###############################################################################################
# plot.emdi

## Not run:
# Loading data - population and sample data
data("eusilcA_pop")
data("eusilcA_smp")
# Example with default setting but na.rm=TRUE; with Box-Cox transformation
set.seed(100); emdi_model <- ebp( fixed = eqIncome ~ gender + eqsize + cash +
                                    self_empl + unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent +
                                    fam_allow + house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop,
                                  pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district",
                                  L= 1, na.rm = TRUE, MSE = TRUE)
# Creation of default diagnostic plots
plot(emdi_model, MSE = TRUE)
# Creation of diagnostic plots without labels and titles, different colors
# and without Cook's distance plot.
plot(emdi_model, color=c("red", "lightblue"))
## End(Not run)


rm(list = ls())

###############################################################################################
# print.emdi

# Loading data - population and sample data
data("eusilcA_pop")
data("eusilcA_smp")
# Example with default setting but na.rm=TRUE; with Box-Cox transformation
set.seed(100); emdi_model <- ebp( fixed = eqIncome ~ gender + eqsize + cash +
                                    self_empl + unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent +
                                    fam_allow + house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop,
                                  pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district",
                                  L= 1, na.rm = TRUE)

print(estimators(emdi_model), MSE = TRUE)
## Wenn MSE nicht im emdi_model enthalten ist, bei print aber MSE = TRUE eingestellt wird, wird der MSE nicht ausgegeben. Evtl. Fehlermeldung oder Hinweis an dieser Stelle

print(estimators(emdi_model))
print(emdi_model$ind)
print(summary(emdi_model))

rm(list = ls())

###############################################################################################
# tail.estimators.emdi

# Loading data - population and sample data
data("eusilcA_pop")
data("eusilcA_smp")

set.seed(100); emdi_model <- ebp( fixed = eqIncome ~ gender + eqsize + cash +
                                    self_empl + unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent +
                                    fam_allow + house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop,
                                  pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district",
                                  L= 1)
tail(emdi_model$ind, n = 5, addrownums = NULL)

rm(list = ls())

###############################################################################################
# write.excel

# Loading data - population and sample data
data("eusilcA_pop")
data("eusilcA_smp")

# Example with two additional indicators
set.seed(100)
emdimodel <- ebp(fixed = eqIncome ~ gender + eqsize + cash +
                                    self_empl + unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent +
                                    fam_allow + house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop,
                                  pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district",
                                  pov_line = 10722.66, transformation = "box.cox", L= 2, MSE = TRUE, B = 2,
                                  custom_indicator = list( my_max = function(y, pov_line){max(y)},
                                                           my_min = function(y, pov_line){min(y)}), na.rm = TRUE, cpus = 1)
# Export estimates for all indicators and uncertainty measures and
# diagnostics to excel
write.excel(emdimodel, file ="excel_output_all.xlsx", indicator = "all", MSE = TRUE, CV = TRUE)


# Single excel sheets for point, MSE and CV estimates
write.excel(emdi_model, file ="excel_output_all_split.xlsx", indicator = "all",
            MSE = TRUE, CV = TRUE, split=TRUE)
