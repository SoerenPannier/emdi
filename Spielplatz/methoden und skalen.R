data("eusilcA_pop")
data("eusilcA_smp")

# generate emdi object with additional indicators; here via function ebp()
emdi_model <- ebp( fixed = eqIncome ~ gender + eqsize + cash + 
                     self_empl + unempl_ben + age_ben + 
                     surv_ben + sick_ben + dis_ben + rent + 
                     fam_allow + house_allow + cap_inv + 
                     tax_adj, pop_data = eusilcA_pop,
                   pop_domains = "district", smp_data = eusilcA_smp, 
                   smp_domains = "district",
                   threshold = 10722.66, transformation = "box.cox", 
                   L= 5, MSE = TRUE, B = 5,
                   custom_indicator = list( my_max = function(y, threshold){max(y)},
                                            my_min = function(y, threshold){min(y)}),
                   na.rm = TRUE, cpus = 1)

# Load shape file
load_shapeaustria()

# Create mapping table such that variables that indicate domains correspond
# in population data and shape file
mapping_table <- data.frame(unique(eusilcA_pop$district), 
                            unique(shape_austria_dis$NAME_2))

map_plot(object = emdi_model, MSE = TRUE, CV = TRUE, 
         map_obj = shape_austria_dis, indicator = c("Mean"), map_dom_id = "NAME_2", 
         map_tab = mapping_table)

#"Mean"     "Mean_MSE" "Mean_CV" 

scaleset <- list("Mean" = list(
                                ind = c(0,10000), 
                                 MSE = c(1000,  100000), 
                                 CV = c(0,10)
    )
  )

map_plot(object = emdi_model, MSE = TRUE, CV = TRUE, 
         map_obj = shape_austria_dis, indicator = c("Mean", "Gini"), map_dom_id = "NAME_2", 
         map_tab = mapping_table, scale_points = scaleset)


fgs <- estimators(emdi_model, indicator = c("Mean", "Gini"), MSE = TRUE)
as.matrix(fgs)
as.data.frame(fgs)



