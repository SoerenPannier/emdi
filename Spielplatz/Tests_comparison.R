# Load package
install.packages("emdi")
library(emdi)


# Code in Section Data sets ----------------------------------------------------
# Load data sets
data("eusilcA_smp")
data("eusilcA_pop")


# Generate object of class "emdi","direct" -------------------------------------
emdi_direct <- direct(y = "eqIncome", smp_data = eusilcA_smp,
  smp_domains = "district", weights = "weight", threshold = 10989.28,
  var = TRUE, boot_type = "naive", B = 50, seed = 123, na.rm = TRUE)



# Generate object of class "emdi","model" --------------------------------------
emdi_model <- ebp(fixed = eqIncome ~ gender + eqsize + cash + self_empl +
  unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + fam_allow + 
  house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop, 
  pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district",
  MSE = TRUE, seed = 100, 
  custom_indicator = list(my_max = function(y, threshold){max(y)},
                          my_min = function(y, threshold){min(y)}))






evaluate <- function(direct, model, indicator = "all", color = "blue") {
  
  obs_dom <- emdi_model$ind$Domain %in% emdi_direct$ind$Domain
  
  
  Mean_direct <- emdi_direct$ind[, c("Domain", "Mean")]
  Mean_direct$smp_size <- as.numeric(table(emdi_direct$framework$smp_domains_vec))
  Mean_model <- emdi_model$ind[, c("Domain", "Mean")]

  
  Mean_data <- merge(Mean_direct, Mean_model, by = "Domain")
  
  summary(lm(Mean.x ~ Mean.y, data = Mean_data))
  ggplot(Mean_data, aes(x = Mean.x, y = Mean.y)) + 
    geom_point() +
    geom_smooth(method = lm, color = color, 
                #se = FALSE
                ) + coord_fixed() 
  
  
  Mean_data <- Mean_data[order(Mean_data$smp_size), ]
  
  ggplot(data = Mean_data, aes(x = Mean.x, y = Mean.y, group=1)) +
    geom_line(color="red")+
    geom_point()
  
  
}
