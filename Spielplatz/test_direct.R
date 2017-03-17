## New Function direct

library(laeken)
library(emdi)
data(eusilc)

# 1. with weights and sampling domains (naive bootstrap)

direct_all_naive <- direct(y="eqIncome",
                           smp_data=eusilc, 
                           smp_domains="db040", 
                           weights="rb050", 
                           # without weights
                           #threshold = 10848.8, 
                           # with weights
                           threshold=10859.24, 
                           var=TRUE,
                           bootType = "naive",
                           X = NULL, 
                           totals = NULL, 
                           B=50,  
                           seed=123, 
                           na.rm=TRUE)


# laeken indicators that fit to above setting
arpr_all <- arpr(eusilc$eqIncome, breakdown = eusilc$db040, weights=eusilc$rb050)
arpr_all_naive <- variance("eqIncome", 
                           weights="rb050", 
                           data=eusilc, 
                           breakdown = "db040", 
                           indicator=arpr_all, 
                           R=50, 
                           bootType="naive",
                           X = NULL,  
                           seed=123)

# Check correspondence: difference in variance can occur due to different poverty
# lines
all.equal(arpr_all$valueByStratum, direct_all_naive$ind[, c("Domain","Head_Count")])
all.equal(arpr_all_naive$varByStratum, direct_all_naive$MSE[, c("Domain","Head_Count")])


gini_all <- gini(eusilc$eqIncome, breakdown = eusilc$db040, weights=eusilc$rb050)
gini_all_naive <- variance("eqIncome", 
                           weights="rb050", 
                           data=eusilc,  
                           breakdown = "db040",
                           bootType="naive", 
                           indicator=gini_all, 
                           R=50, 
                           seed=123)
# Check correspondence
all.equal(gini_all$valueByStratum, direct_all_naive$ind[, c("Domain","Gini")])
all.equal(gini_all_naive$varByStratum, direct_all_naive$MSE[, c("Domain","Gini")])


qsr_all <- qsr(eusilc$eqIncome,breakdown = eusilc$db040, weights=eusilc$rb050)
qsr_all_naive <- variance("eqIncome", 
                          weights="rb050", 
                          data=eusilc,  
                          breakdown = "db040",
                          bootType="naive", 
                          X = NULL, 
                          indicator=qsr_all, 
                          R=50, 
                          seed=123)

# Check correspondence
all.equal(qsr_all$valueByStratum, direct_all_naive$ind[, c("Domain","Quintile_Share")])
all.equal(qsr_all_naive$varByStratum, direct_all_naive$MSE[, c("Domain","Quintile_Share")])


# 2. with weights and sampling domains (calibrate bootstrap)

direct_all_cali <-  direct(y="eqIncome",
                           smp_data=eusilc, 
                           smp_domains="db040", 
                           weights="rb050", 
                           # without weights
                           #threshold = 10848.8, 
                           # with weights
                           threshold=10859.24, 
                           var=TRUE,
                           bootType = "calibrate",
                           X = as.matrix(eusilc$age), 
                           totals = NULL, 
                           B=50,  
                           seed=123, 
                           na.rm=TRUE)


# laeken indicators that fit to above setting
arpr_all_cali <- variance("eqIncome", 
                           weights="rb050", 
                           data=eusilc, 
                           breakdown = "db040", 
                           indicator=arpr_all, 
                           R=50, 
                           bootType="calibrate",
                           X = as.matrix(eusilc$age),  
                           seed=123)

# Check correspondence: difference in variance can occur due to different poverty
# lines
all.equal(arpr_all$valueByStratum, direct_all_cali$ind[, c("Domain","Head_Count")])
all.equal(arpr_all_cali$varByStratum, direct_all_cali$MSE[, c("Domain","Head_Count")])



gini_all_cali <- variance("eqIncome", 
                           weights="rb050", 
                           data=eusilc,  
                           breakdown = "db040",
                           bootType="calibrate", 
                           indicator=gini_all, 
                           R=50, 
                           X = as.matrix(eusilc$age),
                           seed=123)
# Check correspondence
all.equal(gini_all$valueByStratum, direct_all_cali$ind[, c("Domain","Gini")])
all.equal(gini_all_cali$varByStratum, direct_all_cali$MSE[, c("Domain","Gini")])


qsr_all <- qsr(eusilc$eqIncome,breakdown = eusilc$db040, weights=eusilc$rb050)
qsr_all_cali <- variance("eqIncome", 
                          weights="rb050", 
                          data=eusilc,  
                          breakdown = "db040",
                          bootType="calibrate", 
                          indicator=qsr_all, 
                          R=50, 
                          X = as.matrix(eusilc$age),
                          seed=123)

# Check correspondence
all.equal(qsr_all$valueByStratum, direct_all_cali$ind[, c("Domain","Quintile_Share")])
all.equal(qsr_all_cali$varByStratum, direct_all_cali$MSE[, c("Domain","Quintile_Share")])


# 3. without weights but with sampling domains (naive bootstrap)

direct_wow_naive <- direct(y="eqIncome",
                           smp_data=eusilc, 
                           smp_domains="db040", 
                           weights= NULL , 
                           # without weights
                           threshold = 10848.8, 
                           # with weights
                           #threshold=10859.24, 
                           var=TRUE,
                           bootType = "naive",
                           X = NULL, 
                           totals = NULL, 
                           B=50,  
                           seed=123, 
                           na.rm=TRUE)


# laeken indicators that fit to above setting
arpr_wow <- arpr(eusilc$eqIncome, breakdown = eusilc$db040, weights=NULL)
arpr_wow_naive <- variance("eqIncome", 
                           weights=NULL, 
                           data=eusilc, 
                           breakdown = "db040", 
                           indicator=arpr_wow, 
                           R=50, 
                           bootType="naive",
                           X = NULL,  
                           seed=123)

# Check correspondence: difference in variance can occur due to different poverty
# lines
all.equal(arpr_wow$valueByStratum, direct_wow_naive$ind[, c("Domain","Head_Count")])
all.equal(arpr_wow_naive$varByStratum, direct_wow_naive$MSE[, c("Domain","Head_Count")])


gini_wow <- gini(eusilc$eqIncome, breakdown = eusilc$db040, weights=NULL)
gini_wow_naive <- variance("eqIncome", 
                           weights=NULL, 
                           data=eusilc,  
                           breakdown = "db040",
                           bootType="naive", 
                           indicator=gini_wow, 
                           R=50, 
                           seed=123)
# Check correspondence
all.equal(gini_wow$valueByStratum, direct_wow_naive$ind[, c("Domain","Gini")])
all.equal(gini_wow_naive$varByStratum, direct_wow_naive$MSE[, c("Domain","Gini")])


qsr_wow <- qsr(eusilc$eqIncome,breakdown = eusilc$db040, weights=NULL)
qsr_wow_naive <- variance("eqIncome", 
                          weights=NULL, 
                          data=eusilc,  
                          breakdown = "db040",
                          bootType="naive", 
                          X = NULL, 
                          indicator=qsr_wow, 
                          R=50, 
                          seed=123)

# Check correspondence
all.equal(qsr_wow$valueByStratum, direct_wow_naive$ind[, c("Domain","Quintile_Share")])
all.equal(qsr_wow_naive$varByStratum, direct_wow_naive$MSE[, c("Domain","Quintile_Share")])


# 4. without weights but with sampling domains (calibrate bootstrap)

direct_wow_cali <-  direct(y="eqIncome",
                           smp_data=eusilc, 
                           smp_domains="db040", 
                           weights=NULL, 
                           # without weights
                           threshold = 10848.8, 
                           # with weights
                           #threshold=10859.24, 
                           var=TRUE,
                           bootType = "calibrate",
                           X = as.matrix(eusilc$age), 
                           totals = NULL, 
                           B=50,  
                           seed=123, 
                           na.rm=TRUE)


# laeken indicators that fit to above setting
arpr_wow_cali <- variance("eqIncome", 
                          weights=NULL, 
                          data=eusilc, 
                          breakdown = "db040", 
                          indicator=arpr_wow, 
                          R=50, 
                          bootType="calibrate",
                          X = as.matrix(eusilc$age),  
                          seed=123)

# Check correspondence: difference in variance can occur due to different poverty
# lines
all.equal(arpr_wow$valueByStratum, direct_wow_cali$ind[, c("Domain","Head_Count")])
all.equal(arpr_wow_cali$varByStratum, direct_wow_cali$MSE[, c("Domain","Head_Count")])



gini_wow_cali <- variance("eqIncome", 
                          weights=NULL, 
                          data=eusilc,  
                          breakdown = "db040",
                          bootType="calibrate", 
                          indicator=gini_wow, 
                          R=50, 
                          X = as.matrix(eusilc$age),
                          seed=123)
# Check correspondence
all.equal(gini_wow$valueByStratum, direct_wow_cali$ind[, c("Domain","Gini")])
all.equal(gini_wow_cali$varByStratum, direct_wow_cali$MSE[, c("Domain","Gini")])



qsr_wow_cali <- variance("eqIncome", 
                         weights=NULL, 
                         data=eusilc,  
                         breakdown = "db040",
                         bootType="calibrate", 
                         indicator=qsr_wow, 
                         R=50, 
                         X = as.matrix(eusilc$age),
                         seed=123)

# Check correspondence
all.equal(qsr_wow$valueByStratum, direct_wow_cali$ind[, c("Domain","Quintile_Share")])
all.equal(qsr_wow_cali$varByStratum, direct_wow_cali$MSE[, c("Domain","Quintile_Share")])





# Test ebp Quintile Share Ratio
qsr_laeken <- qsr(eusilc$eqIncome,breakdown = NULL, weights=NULL)




qsr_function2(eusilc$eqIncome)
qsr_function3(eusilc$eqIncome)



qsr_function2 <- function(y) {
  I1<-(y <= quantile(y,0.2,type = 7))
  I2<-(y > quantile(y,0.8,type = 7))
     cat("anzk",sum(I1), "anzg",sum(I2))
     (sum(1*y[I2])/sum(rep(1, length(y)))) /
       (sum(1*y[I1])/sum(rep(1, length(y))))
   }
  
qsr_function3 <- function(x) {
     weights <- rep.int(1, length(x))
     q <- incQuintile(x, weights)
     iq1 <- x <= q[1]
     iq4 <- x > q[2]
     (sum(weights[iq4] * x[iq4])/sum(weights[iq4]))/(sum(weights[iq1] *
                                                           x[iq1])/sum(weights[iq1]))
  
   }



# 5. With the data from the package emdi

data("eusilcA_smp")

direct_emdi <- direct(y="eqIncome",
                      smp_data=eusilcA_smp, 
                      smp_domains="district", 
                      weights= NULL , 
                      # without weights
                      threshold = 10848.8, 
                      # with weights
                      #threshold=10859.24, 
                      var=TRUE,
                      bootType = "naive",
                      X = NULL, 
                      totals = NULL, 
                      B=5,  
                      seed=123, 
                      na.rm=TRUE)


# laeken indicators that fit to above setting
eusilcA_smp$district <- droplevels(eusilcA_smp$district)
arpr_emdi <- arpr(eusilcA_smp$eqIncome, breakdown = eusilcA_smp$district, weights=NULL)
arpr_emdi_naive <- variance("eqIncome", 
                           weights=NULL, 
                           data=eusilcA_smp, 
                           breakdown = "district", 
                           indicator=arpr_emdi, 
                           R=50, 
                           bootType="naive",
                           X = NULL,  
                           seed=123)

# Check correspondence: difference in variance can occur due to different poverty
# lines
all.equal(arpr_wow$valueByStratum, direct_wow_naive$ind[, c("Domain","Head_Count")])
all.equal(arpr_wow_naive$varByStratum, direct_wow_naive$MSE[, c("Domain","Head_Count")])


gini_emdi <- gini(eusilcA_smp$eqIncome, breakdown = eusilcA_smp$district, weights=NULL)
gini_emdi_naive <- variance("eqIncome", 
                           weights=NULL, 
                           data=eusilcA_smp,  
                           breakdown = "district",
                           bootType="naive", 
                           indicator=gini_emdi, 
                           R=5, 
                           seed=123)


gini_comp <- data.frame(Domain = direct_emdi$ind$Domain,
                        Gini_emdi = direct_emdi$ind$Gini, 
                        Gini_laeken = gini_emdi$valueByStratum$value, 
                        Gini_MSE_emdi = direct_emdi$MSE$Gini, 
                        Gini_MSE_laeken = gini_emdi_naive$varByStratum$var)

# Check correspondence
all.equal(gini_wow$valueByStratum, direct_wow_naive$ind[, c("Domain","Gini")])
all.equal(gini_wow_naive$varByStratum, direct_wow_naive$MSE[, c("Domain","Gini")])


qsr_emdi <- qsr(eusilcA_smp$eqIncome,breakdown = eusilcA_smp$district, weights=NULL)
qsr_emdi_naive <- variance("eqIncome", 
                          weights=NULL, 
                          data=eusilcA_smp,  
                          breakdown = "district",
                          bootType="naive", 
                          X = NULL, 
                          indicator=qsr_emdi, 
                          R=50, 
                          seed=123)

# Check correspondence
all.equal(qsr_wow$valueByStratum, direct_wow_naive$ind[, c("Domain","Quintile_Share")])
all.equal(qsr_wow_naive$varByStratum, direct_wow_naive$MSE[, c("Domain","Quintile_Share")])






