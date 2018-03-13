# Test if reproducability is ensured when the MSE estimation is parallelized


data("eusilcA_pop")
data("eusilcA_smp")

test_that("Are the estimates reproducable when using parallelization?", {
  
  
  # Generate emdi model object when using cpus = 2
  emdi_model2 <- ebp(fixed = eqIncome ~ gender + eqsize +  
                        cash + self_empl + unempl_ben + age_ben + surv_ben + 
                        sick_ben +  dis_ben + rent + fam_allow + house_allow + 
                        cap_inv + tax_adj, pop_data = eusilcA_pop, 
                      pop_domains = "district", smp_data = eusilcA_smp, 
                      smp_domains = "district", threshold = 11161.44,
                      MSE = TRUE, seed = 100, cpus = 2)
  
  # Generate a second emdi model object when using cpus = 2
  emdi_model22 <- ebp(fixed = eqIncome ~ gender + eqsize + cash + self_empl + 
                        unempl_ben + age_ben + surv_ben + sick_ben +  dis_ben + 
                        rent + fam_allow + house_allow + cap_inv + tax_adj,
                      pop_data = eusilcA_pop, pop_domains = "district", 
                      smp_data = eusilcA_smp, smp_domains = "district", 
                      threshold = 11161.44, MSE = TRUE, seed = 100, cpus = 2)

  # Check equality
  expect_equal(emdi_model2$MSE, emdi_model22$MSE)
})



