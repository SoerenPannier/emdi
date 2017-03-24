################################################################################
# Test examples from package since most are dontrun!                            
################################################################################

# Ebp function
emdi_model <- ebp(fixed = eqIncome ~ gender + eqsize + cash + self_empl + 
                  unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + fam_allow + 
                  house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop,
                  pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district", 
                  na.rm = TRUE)
