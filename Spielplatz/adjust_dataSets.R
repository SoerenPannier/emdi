
adjust_dataSets <- function(pop, smp) {
  
  library(plyr)
  
  fixed = eqIncome ~ gender + eqsize + py010n + py050n + py090n + py100n + 
    py110n + py120n + py130n + hy040n + hy050n + hy070n + hy090n + hy145n + 
    region 
  
  mod_vars <- gsub(" ", "",unlist(strsplit(paste(fixed[3]), "[+]")), 
                   fixed = TRUE)
  data_vars <- c(as.character(fixed[2]), mod_vars, "sub_2")
  
  
  # Umbennenung einiger Variablen
  
  # Population
  eusilcA_pop <- eusilcA_pop[, data_vars]
  
  
  eusilcA_pop <- rename(eusilcA_pop, c("py010n"="cash", "py050n"="self_empl", 
                                       "py090n"="unempl_ben", "py100n"="age_ben",
                                       "py110n"="surv_ben", "py120n"="sick_ben",
                                       "py130n"="dis_ben", "hy040n"="rent",
                                       "hy050n"="fam_allow", "hy070n"="house_allow",
                                       "hy090n"="cap_inv", "hy145n"="tax_adj",
                                       "sub_2"="district", "region" = "state"))
  
  # Sample
  eusilcA_smp <- eusilcA_smp[, data_vars]
  
  eusilcA_smp <- rename(eusilcA_smp, c("py010n"="cash", "py050n"="self_empl", 
                                       "py090n"="unempl_ben", "py100n"="age_ben",
                                       "py110n"="surv_ben", "py120n"="sick_ben",
                                       "py130n"="dis_ben", "hy040n"="rent",
                                       "hy050n"="fam_allow", "hy070n"="house_allow",
                                       "hy090n"="cap_inv", "hy145n"="tax_adj",
                                       "region"="state", "sub_2"="district"))
  
  return(list(smp = eusilcA_smp, pop = eusilcA_pop))
  }

