# Test if point estimation runs on its own and returns the same values

# Load needed data
load("EBP_TF/sample_data.RData")
load("EBP_TF/population_data.RData")

test_that("Does monte_carlo function give benchmark results?", {
  suppressWarnings(RNGversion("3.5.0"))
  # Single elements needed in monte_carlo()
  framework_ebp_tf <- framework_ebp_tf(income ~ educ1,
                        population_data,
                        "provlab",
                        "prov_gen",
                        sample_data,
                        "provlab",
                        "prov_gen",
                        5741.0157,
                        custom_indicator = NULL,
                        na.rm = TRUE,
                        pop_weights = NULL)
  # Fixed optimal parameter and shift (benchmark values)
  ebp_optpar_bc <- read.csv2("EBP_TF/ebp_optpar_bc.csv", sep = ",",
                             stringsAsFactors = TRUE)
  ebp_shift_bc  <- read.csv2("EBP_TF/ebp_shift_bc.csv", sep = ",",
                             stringsAsFactors = TRUE)

  lambda <- as.numeric(as.character(ebp_optpar_bc[,"Optpar"]))
  shift  <- as.numeric(as.character(ebp_shift_bc))

  # Conduct transformation using the optimal parameter
  transformation_par <- data_transformation(fixed          = income ~ educ1,
                                            smp_data       = framework_ebp_tf$smp_data,
                                            transformation = "box.cox",
                                            lambda         = lambda
  )

  # Conduct regression using transformed data
  mixed_model_tf <- nlme::lme(fixed  = income~educ1,
                     data   = transformation_par$transformed_data ,
                     random =
                       as.formula(paste0(
                         "~ 1 | " , framework_ebp_tf$smp_domains, "/", framework_ebp_tf$smp_subdomains)),
                     method = "REML")

  # Get model parameter
  est_par_tf <- model_par_tf(mixed_model_tf = mixed_model_tf,
                       framework_ebp_tf   = framework_ebp_tf
  )

  # Get parameter for the generating model
  gen_par_tf <- gen_model_tf(model_par_tf   = est_par_tf,
                       fixed       = income~educ1,
                       framework_ebp_tf   = framework_ebp_tf
  )

  set.seed(100)
  point <- monte_carlo_tf(transformation = "box.cox",
                                      L = 2,
                                      framework_ebp_tf,
                                      lambda = lambda,
                                      shift = shift,
                                      model_par_tf = est_par_tf,
                                      gen_model_tf = gen_par_tf
                                      )

  set.seed(100)
  point2 <- point_ebp_tf(framework_ebp_tf,
                        fixed = income ~ educ1,
                        transformation = "box.cox",
                        interval = "default",
                        L = 2,
                        keep_data = FALSE)


  # Load benchmark point estimates for domain level
  ebp_tf_point_dom_bc <- read.csv2("EBP_TF/ebp_tf_point_dom_bc.csv", sep = ",",
                            stringsAsFactors = TRUE)

  # compare 10% quantile
  expect_equal(point[["point_estimates"]][,"Quantile_10"],
               as.numeric(as.character(ebp_tf_point_dom_bc[,"Quantile_10"])))
  expect_equal(point2$ind_Domain[,"Quantile_10"],
               as.numeric(as.character(ebp_tf_point_dom_bc[,"Quantile_10"])))

  # compare HCR
  expect_equal(point[["point_estimates"]][,"Head_Count"],
               as.numeric(as.character(ebp_tf_point_dom_bc[,"Head_Count"])))
  expect_equal(point2$ind_Domain[,"Head_Count"],
               as.numeric(as.character(ebp_tf_point_dom_bc[,"Head_Count"])))

  # compare Gini
  expect_equal(point[["point_estimates"]][,"Gini"],
               as.numeric(as.character(ebp_tf_point_dom_bc[,"Gini"])))
  expect_equal(point2$ind_Domain[,"Gini"],
               as.numeric(as.character(ebp_tf_point_dom_bc[,"Gini"])))

  # Load benchmark point estimates for subdomain level
  ebp_tf_point_subdom_bc <- read.csv2("EBP_TF/ebp_tf_point_subdom_bc.csv", sep = ",",
                                   stringsAsFactors = TRUE)

  # compare 10% quantile
  expect_equal(point[["point_estimates_subdom"]][,"Quantile_10"],
               as.numeric(as.character(ebp_tf_point_subdom_bc[,"Quantile_10"])))
  expect_equal(point2$ind_Subdomain[,"Quantile_10"],
               as.numeric(as.character(ebp_tf_point_subdom_bc[,"Quantile_10"])))

  # compare HCR
  expect_equal(point[["point_estimates_subdom"]][,"Head_Count"],
               as.numeric(as.character(ebp_tf_point_subdom_bc[,"Head_Count"])))
  expect_equal(point2$ind_Subdomain[,"Head_Count"],
               as.numeric(as.character(ebp_tf_point_subdom_bc[,"Head_Count"])))

  # compare Gini
  expect_equal(point[["point_estimates_subdom"]][,"Gini"],
               as.numeric(as.character(ebp_tf_point_subdom_bc[,"Gini"])))
  expect_equal(point2$ind_Subdomain[,"Gini"],
               as.numeric(as.character(ebp_tf_point_subdom_bc[,"Gini"])))
})



