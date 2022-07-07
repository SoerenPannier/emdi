# Test if point estimation runs on its own and returns the same values


# Load needed data
data("eusilcA_smp")
eusilcA_smp$eqIncome_int <- as.integer(eusilcA_smp$eqIncome)

test_that("Does the direct estimation in emdi return the point and variance
          estimates when a naive bootstrap is used?", {
  suppressWarnings(RNGversion("3.5.0"))

  # Direct estimation with naive bootstrap
  direct_all_naive <- direct(y = 'eqIncome_int',
                             weights = 'weight',
                             smp_data = eusilcA_smp,
                             smp_domains = 'state',
                             var = TRUE,
                             threshold = function(y, weights){
                               0.6 * wtd.quantile(x = y, weights = weights,
                                                  probs = 0.5)},
                             boot_type = "naive",
                             X_calib = NULL,
                             totals = NULL,
                             B = 5,
                             seed = 123,
                             na.rm = TRUE)



  # HCR from laeken package (benchmark)
  arpr_naive_value <- read.csv2("Direct/arpr_naive_value.csv", sep = ",")
  arpr_naive_var <- read.csv2("Direct/arpr_naive_var.csv", sep = ",")


  # Compare HCR from direct and benchmark
  expect_equal(as.numeric(arpr_naive_value$value)/100,
               direct_all_naive$ind$Head_Count)
  expect_equal(as.numeric(arpr_naive_var$var)/10000,
               direct_all_naive$MSE$Head_Count)

})


test_that("Does the direct estimation in emdi return the point and variance
          estimates when a calibrated bootstrap is used?", {
            suppressWarnings(RNGversion("3.5.0"))
            # Direct estimation with naive bootstrap
            direct_all_cali <-  direct(y = "eqIncome_int",
                                       smp_data = eusilcA_smp,
                                       smp_domains = "state",
                                       weights = "weight",
                                       # with weights
                                       threshold = 10859.24,
                                       var = TRUE,
                                       boot_type = "calibrate",
                                       X_calib = as.matrix(eusilcA_smp$eqsize),
                                       totals = NULL,
                                       B = 5,
                                       seed = 123,
                                       na.rm = TRUE)


            # Gini from laeken package (benchmark)
            gini_cali_value <- read.csv2("Direct/gini_cali_value.csv", sep = ",")
            gini_cali_var <- read.csv2("Direct/gini_cali_var.csv", sep = ",")


            expect_equal(as.numeric(gini_cali_value$value)/100,
                         direct_all_cali$ind$Gini)
            expect_equal(as.numeric(gini_cali_var$var)/10000,
                         direct_all_cali$MSE$Gini)

          })



