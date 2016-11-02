# # Comparison of original ebp and ebp in package
# 
# # Auskommentiert, da Problematik zwischen 64bit und 32bit Betriebssystem,
# # Intern weiternutzen
# 
# # The data that is used for testing is the data from the sae package. 
# load("./incomedata.RData")
# load("./incomedata_woTeruel.RData")
# load("./Xoutsamp_AuxVar.RData")
# 
# 
# 
# test_that("Does ebp return the same results with other data (no out of
#           sample domain)?", {
# 
#   # Box Cox 
#   ebp_point_bc <- read.csv2("./ebp_point_bc.csv", sep=",")
#   ebp_MSE_bc <- read.csv2("./ebp_MSE_bc.csv", sep=",")  
#   ebp_optpar_bc <- read.csv2("./ebp_optpar_bc.csv", sep=",")  
#   ebp_shift_bc <- read.csv2("./ebp_shift_bc.csv", sep=",")  
#   
#   # ebp function with domains as factors instead of numerics
#   # method cannot be chosen anymore --> REML for all tests
#   set.seed(100); ebp_bc <- ebp(fixed = income~educ1, 
#                                pop_data = Xoutsamp_AuxVar,
#                                pop_domains = "provlab", 
#                                smp_data = incomedata,
#                                smp_domains = "provlab", 
#                                pov_line = 4282.081,
#                                transformation = "box.cox",
#                                interval=c(-1,2), 
#                                L=2, 
#                                MSE=TRUE, 
#                                B=2, 
#                                custom_indicator = NULL, 
#                                na.rm = F)
# 
#   expect_equal(ebp_bc$transform_param$optimal_lambda,
#                as.numeric(as.character(ebp_optpar_bc[,"Optpar"])))
#   expect_equal(ebp_bc$transform_param$shift_par,
#                as.numeric(as.character(ebp_shift_bc)))
#   # compare 10% quantile
#   expect_equal(ebp_bc$ind[,"Quantile_10"],
#                as.numeric(as.character(ebp_point_bc[,"quant10"])))
#   # compare HCR
#   expect_equal(ebp_bc$ind[,"Head_Count"],
#                as.numeric(as.character(ebp_point_bc[,"hcr"])))
#   # compare 10% quantile MSE
#   expect_equal(ebp_bc$MSE[,"Quantile_10"],
#                as.numeric(as.character(ebp_MSE_bc[,"quant10"])))
#   # compare 10% quantile MSE
#   expect_equal(ebp_bc$MSE[,"Head_Count"],
#                as.numeric(as.character(ebp_MSE_bc[,"hcr"])))
# 
#   # No transformation
#   ebp_point_no <- read.csv2("./ebp_point_no.csv", sep=",")
#   ebp_MSE_no <- read.csv2("./ebp_MSE_no.csv", sep=",")  
# 
#   set.seed(100); ebp_no <- ebp(fixed  = income~educ1,
#                                pop_data = Xoutsamp_AuxVar, 
#                                pop_domains = "provlab",
#                                smp_data = incomedata,
#                                smp_domains = "provlab",
#                                pov_line = 4282.081, 
#                                transformation = "no",
#                                interval=c(-1,2), 
#                                L=2, 
#                                MSE=TRUE, 
#                                B=2,
#                                custom_indicator = NULL, 
#                                na.rm = F)
# 
#   # Optpar and shift are NULL in the original code
#   expect_equal(ebp_no$transform_param$optimal_lambda,
#                NULL)
#   expect_equal(ebp_no$transform_param$shift_par,
#                NULL)
#   # compare 10% quantile
#   expect_equal(ebp_no$ind[,"Quantile_10"],
#                as.numeric(as.character(ebp_point_no[,"quant10"])))
#   # compare HCR
#   expect_equal(ebp_no$ind[,"Head_Count"],
#                as.numeric(as.character(ebp_point_no[,"hcr"])))
#   # compare 10% quantile MSE
#   expect_equal(ebp_no$MSE[,"Quantile_10"],
#                as.numeric(as.character(ebp_MSE_no[,"quant10"])))
#   # compare 10% quantile MSE
#   expect_equal(ebp_no$MSE[,"Head_Count"],
#                as.numeric(as.character(ebp_MSE_no[,"hcr"])))
# 
#   # Log transformation
#   ebp_point_log <- read.csv2("./ebp_point_log.csv", sep=",")
#   ebp_MSE_log <- read.csv2("./ebp_MSE_log.csv", sep=",")  
#   ebp_shift_log <- read.csv2("./ebp_shift_log.csv", sep=",") 
#   
#   
#   set.seed(100); ebp_log <- ebp(fixed = income~educ1, 
#                                 pop_data = Xoutsamp_AuxVar, 
#                                 pop_domains = "provlab", 
#                                 smp_data = incomedata,
#                                 smp_domains = "provlab", 
#                                 pov_line = 4282.081, 
#                                 transformation = "log",
#                                 interval=c(-1,2), 
#                                 L=2, 
#                                 MSE=TRUE, 
#                                 B=2,
#                                 custom_indicator = NULL, 
#                                 na.rm = F)
#   
#   # Optpar is NULL in the original code
#   expect_equal(ebp_log$transform_param$optimal_lambda,
#                NULL)
#   expect_equal(ebp_log$transform_param$shift_par,
#                as.numeric(as.character(ebp_shift_log)))
#   
#   # compare 10% quantile
#   expect_equal(ebp_log$ind[,"Quantile_10"],
#                as.numeric(as.character(ebp_point_log[,"quant10"])))
#   # compare HCR
#   expect_equal(ebp_log$ind[,"Head_Count"],
#                as.numeric(as.character(ebp_point_log[,"hcr"])))
#   # compare 10% quantile MSE
#   expect_equal(ebp_log$MSE[,"Quantile_10"],
#                as.numeric(as.character(ebp_MSE_log[,"quant10"])))
#   # compare 10% quantile MSE
#   expect_equal(ebp_log$MSE[,"Head_Count"],
#                as.numeric(as.character(ebp_MSE_log[,"hcr"])))
# })
# 
# 
# test_that("Does ebp return the same results with other data (with out of
#           sample domain)?", {
# 
#   # Box Cox with REML
#   ebp_point_bc_out <- read.csv2("./ebp_point_bc_out.csv", sep=",")
#   ebp_MSE_bc_out <- read.csv2("./ebp_MSE_bc_out.csv", sep=",")  
#   ebp_optpar_bc_out <- read.csv2("./ebp_optpar_bc_out.csv", sep=",")  
#   ebp_shift_bc_out <- read.csv2("./ebp_shift_bc_out.csv", sep=",")  
#               
#   set.seed(100); ebp_bc_wo <- ebp(fixed = income~educ1, 
#                                   pop_data = Xoutsamp_AuxVar,
#                                   pop_domains = "provlab", 
#                                   smp_data = incomedata_woTeruel,
#                                   smp_domains = "provlab",
#                                   pov_line = 4282.081,
#                                   transformation = "box.cox", 
#                                   interval=c(-1,2), 
#                                   L=2, 
#                                   MSE=TRUE, 
#                                   B=2, 
#                                   custom_indicator = NULL, 
#                                   na.rm = F)
# 
#   expect_equal(ebp_bc_wo$transform_param$optimal_lambda,
#                as.numeric(as.character(ebp_optpar_bc_out[,"Optpar"])))
#   expect_equal(ebp_bc_wo$transform_param$shift_par,
#                as.numeric(as.character(ebp_shift_bc_out)))
#   # compare 10% quantile
#   expect_equal(ebp_bc_wo$ind[,"Quantile_10"],
#                as.numeric(as.character(ebp_point_bc_out[,"quant10"])))
#   # compare HCR
#   expect_equal(ebp_bc_wo$ind[,"Head_Count"],
#                as.numeric(as.character(ebp_point_bc_out[,"hcr"])))
#   # compare 10% quantile MSE
#   expect_equal(ebp_bc_wo$MSE[,"Quantile_10"],
#                as.numeric(as.character(ebp_MSE_bc_out[,"quant10"])))
#   # compare 10% quantile MSE
#   expect_equal(ebp_bc_wo$MSE[,"Head_Count"],
#                as.numeric(as.character(ebp_MSE_bc_out[,"hcr"])))
# })

