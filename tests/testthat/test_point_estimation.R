# Test if point estimation runs on its own and returns the same values


# Load needed data
load("EBP/incomedata.RData")
load("EBP/incomedata_woTeruel.RData")
load("EBP/Xoutsamp_AuxVar.RData")



test_that("Does monte_carlo function give benchmark results?", {
  suppressWarnings(RNGversion("3.5.0"))
  # Single elements needed in monte_carlo()
  framework <- framework_ebp(income ~ educ1,
                        Xoutsamp_AuxVar,
                        "provlab",
                        incomedata,
                        "provlab",
                        4282.081,
                        custom_indicator = NULL,
                        na.rm = TRUE,
                        pop_weights = NULL,
                        weights = NULL)
  # Fixed optimal parameter and shift (benchmark values)
  ebp_optpar_bc <- read.csv2("EBP/ebp_optpar_bc.csv", sep = ",",
                             stringsAsFactors = TRUE)
  ebp_shift_bc  <- read.csv2("EBP/ebp_shift_bc.csv", sep = ",",
                             stringsAsFactors = TRUE)

  lambda <- as.numeric(as.character(ebp_optpar_bc[,"Optpar"]))
  shift  <- as.numeric(as.character(ebp_shift_bc))

  # Conduct transformation using the optimal parameter
  transformation_par <- data_transformation(fixed          = income ~ educ1,
                                            smp_data       = framework$smp_data,
                                            transformation = "box.cox",
                                            lambda         = lambda
  )

  # Conduct regression using transformed data
  mixed_model <- lme(fixed  = income~educ1,
                     data   = transformation_par$transformed_data ,
                     random = as.formula(paste0("~ 1 | as.factor(", framework$smp_domains, ")")),
                     method = "REML")

  # Get model parameter
  est_par <- model_par(mixed_model = mixed_model,
                       framework   = framework
  )

  # Get parameter for the generating model
  gen_par <- gen_model(model_par   = est_par,
                       fixed       = income~educ1,
                       framework   = framework
  )

  set.seed(100)
  point <- monte_carlo(transformation = "box.cox",
                                      L = 2,
                                      framework = framework,
                                      lambda = lambda,
                                      shift = shift,
                                      model_par = est_par,
                                      gen_model = gen_par
                                      )

  set.seed(100)
  point2 <- point_estim(framework = framework,
                        fixed = income ~ educ1,
                        transformation = "box.cox",
                        interval = "default",
                        L = 2,
                        keep_data = FALSE)

  # Load benchmark point estimates
  ebp_point_bc <- read.csv2("EBP/ebp_point_bc.csv", sep = ",",
                            stringsAsFactors = TRUE)

  # compare 10% quantile
  expect_equal(point[,"Quantile_10"],
               as.numeric(as.character(ebp_point_bc[,"quant10"])))
  expect_equal(point2$ind[,"Quantile_10"],
               as.numeric(as.character(ebp_point_bc[,"quant10"])))

  # compare HCR
  expect_equal(point[,"Head_Count"],
               as.numeric(as.character(ebp_point_bc[,"hcr"])))
  expect_equal(point2$ind[,"Head_Count"],
               as.numeric(as.character(ebp_point_bc[,"hcr"])))

  # compare Gini
  expect_equal(point[,"Gini"],
               as.numeric(as.character(ebp_point_bc[,"gini"])))
  expect_equal(point2$ind[,"Gini"],
               as.numeric(as.character(ebp_point_bc[,"gini"])))

})


test_that("Does monte_carlo function give benchmark results? Using weights
          and pop_weights", {
  suppressWarnings(RNGversion("3.5.0"))
  # join synthetic population weights
  Xoutsamp_AuxVar$pop_weights <- rep(2, nrow(Xoutsamp_AuxVar))
  Xoutsamp_AuxVar$domain[Xoutsamp_AuxVar$domain == 5 | Xoutsamp_AuxVar$domain == 34] <- 1

  # Single elements needed in monte_carlo()
  framework <- framework_ebp(fixed = income ~ educ1,
                             pop_data = Xoutsamp_AuxVar, pop_domains = "provlab",
                             smp_data = incomedata, smp_domains = "provlab",
                             threshold = 4282.081,
                             custom_indicator = NULL,
                             na.rm = TRUE,
                             aggregate_to = "domain",
                             weights = "weight",
                             pop_weights = "pop_weights")

  # Conduct transformation using the optimal parameter
  transformation_par <- data_transformation(fixed          = income ~ educ1,
                                            smp_data       = framework$smp_data,
                                            transformation = "log"
  )
  shift <- 0

  # Conduct regression using transformed data
  mixed_model <- lme(fixed  = income~educ1,
                     data   = transformation_par$transformed_data ,
                     random = as.formula(paste0("~ 1 | as.factor(", framework$smp_domains, ")")),
                     method = "REML")

  # Get model parameter
  est_par <- model_par(mixed_model = mixed_model,
                       framework   = framework,
                       transformation_par = transformation_par,
                       fixed = income~educ1
  )

  # Get parameter for the generating model
  gen_par <- gen_model(model_par   = est_par,
                       fixed       = income~educ1,
                       framework   = framework
  )

  set.seed(100)
  point <- monte_carlo(transformation = "log",
                       L = 2,
                       framework = framework,
                       shift = shift,
                       model_par = est_par,
                       gen_model = gen_par
  )

  set.seed(100)
  point2 <- point_estim(framework = framework,
                        fixed = income ~ educ1,
                        transformation = "log",
                        interval = "default",
                        L = 2,
                        keep_data = FALSE
  )

  # Load benchmark point estimates
  ebp_point_bc <- read.csv2("EBP/ebp_point_bc2.csv", sep = ",",
                            stringsAsFactors = TRUE)

  # compare 10% quantile
  expect_equal(point[,"Quantile_10"],
               as.numeric(as.character(ebp_point_bc[,"Quantile_10"])))
  expect_equal(point2$ind[,"Quantile_10"],
               as.numeric(as.character(ebp_point_bc[,"Quantile_10"])))

  # compare HCR
  expect_equal(point[,"Head_Count"],
               as.numeric(as.character(ebp_point_bc[,"Head_Count"])))
  expect_equal(point2$ind[,"Head_Count"],
               as.numeric(as.character(ebp_point_bc[,"Head_Count"])))

  # compare Gini
  expect_equal(point[,"Gini"],
               as.numeric(as.character(ebp_point_bc[,"Gini"])))
  expect_equal(point2$ind[,"Gini"],
               as.numeric(as.character(ebp_point_bc[,"Gini"])))

})


