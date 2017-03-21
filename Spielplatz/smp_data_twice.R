# Check if it is necessary that emdi object returns the data twice

# For this check if the data in model is the same as sample data in framework
# Firstly, keep.data equals true in point estimation
emdi_model <- ebp(fixed = eqIncome ~ gender + eqsize + cash + self_empl + 
                    unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + 
                    rent + fam_allow + house_allow + cap_inv + tax_adj,
                  pop_data = eusilcA_pop,
                  pop_domains = "district",
                  smp_data = eusilcA_smp,
                  smp_domains = "district",
                  L = 50,
                  MSE = TRUE, B =50,
                  custom_indicator = list(my_max = function(y, threshold){max(y)},
                                          my_min = function(y, threshold){min(y)})
)


# Transformed data set
class(emdi_model$model$data)
head(emdi_model$model$data)


# Original data set
class(emdi_model$framework$smp_data)
head(emdi_model$framework$smp_data)

# The original data is needed for the graphical diagnostics
# The data of the model object is used for function r.squaredGLMM.lme
# However, we need to check if it is important that the transformed data is 
# used or if we can overwrite model$data with framework$smp_data

# Use model$data as data
x <- emdi_model$model
# This is the object for which the data is used
mmRE_modeldata <- mmRE

# Use original sample data as data
emdi_model$model$data <- emdi_model$framework$smp_data
x <- emdi_model$model
mmRe_originaldata <- mmRE

# Compare the two components
all.equal(mmRE_modeldata, mmRe_originaldata)
# The component using the data does not differ when different data sets are used

test_r.GLMM(x)


test_r.GLMM <- function (x) 
{
  VarFx <- var(fitted(x, level = 0L))
  mmRE <- model.matrix(x$modelStruct$reStruct, data = x$data[rownames(x$fitted), 
                                                             , drop = FALSE])
  n <- nrow(mmRE)
  sigma2 <- x$sigma^2
  reStruct <- x$modelStruct$reStruct
  if ((m <- length(reStruct)) > 1L) {
    nams <- names(reStruct)
    for (i in seq.int(m)) attr(reStruct[[i]], "Dimnames")[[2L]] <- paste(nams[[i]], 
                                                                         attr(reStruct[[i]], "Dimnames")[[2L]], sep = ".")
  }
  varRe <- sum(sapply(reStruct, function(z) {
   # browser()
    sig <- nlme::pdMatrix(z) * sigma2
    mm1 <- mmRE[, rownames(sig), drop = FALSE]
    sum(MuMIn:::matmultdiag(mm1 %*% sig, ty = mm1))/n
  }))
  varTot <- sum(VarFx, varRe)
  res <- c(VarFx, varTot)/(varTot + sigma2)
  names(res) <- c("R2m", "R2c")
  res
}


# Check of the marginal and conditional R squared depend on transformation type
emdi_model_no <- ebp(fixed = eqIncome ~ gender + eqsize + cash + self_empl + 
                    unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + 
                    rent + fam_allow + house_allow + cap_inv + tax_adj,
                  pop_data = eusilcA_pop,
                  pop_domains = "district",
                  smp_data = eusilcA_smp,
                  smp_domains = "district",
                  L = 50,
                  transformation = "no",
                  MSE = TRUE, B =50,
                  custom_indicator = list(my_max = function(y, threshold){max(y)},
                                          my_min = function(y, threshold){min(y)})
)


summary(emdi_model_no)


