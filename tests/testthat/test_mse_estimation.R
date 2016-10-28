# Test if mse estimation runs on its own and returns the same values

# Load needed data
load("./incomedata.RData")
load("./incomedata_woTeruel.RData")
load("./Xoutsamp_AuxVar.RData")

# Test if point_estim runs on its own by comparing results to ebp_neue

test_that("Does parametric_bootstrap return the benchmark results?", {

  
  # Conduct single steps
  framework <- notation(income~educ1,
                        Xoutsamp_AuxVar, 
                        "provlab", 
                        incomedata,
                        "provlab", 
                        4282.081,
                        custom_indicator = NULL, 
                        na.rm = TRUE)

  set.seed(100); point <- point_estim(framework,
                                      income~educ1,
                                      "box.cox",
                                      interval=c(-1,2),
                                      L=2
                                      )
  
  MSE <- parametric_bootstrap(framework,
                              point_estim = point,
                              income~educ1,
                              "box.cox",
                              interval=c(-1,2),
                              L=2,
                              B=2,
                              cpus=1)
  
  # Get benchmark results
  ebp_MSE_bc <- read.csv2("./ebp_MSE_bc.csv", sep=",")  

  
  # Compare MSE estimates
  expect_equal(MSE[,"Quantile_10"],
               as.numeric(as.character(ebp_MSE_bc[,"quant10"])))

})

