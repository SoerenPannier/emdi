################################################################################
# Comparison of EBP in emdi with EBP in sae
################################################################################


# Load both packages
library(sae)
library(emdi)

# Load data from package emdi
data(eusilcA_smp)
data(eusilcA_pop)

# Define Head Count Ratio (HCR) for the ebBHF function corresponding to HCR in 
# emdi
povertyincidence <- function(y) {
  result <- mean(y < 11161.44)
  return(result)
}


# Define arguments needed for function ebBHF from the sae package
provincelabels <- unique(eusilcA_pop$district)
Xoutsamp_AuxVar <- eusilcA_pop[ ,c("district", "cash")]
rownames(Xoutsamp_AuxVar) <- NULL


# Get EBP estimates using ebBHF function
set.seed(123)
EB <- ebBHF(eqIncome ~ cash, dom = district,
selectdom = provincelabels, Xnonsample = Xoutsamp_AuxVar, MC = 5000,
constant = 0, indicator = povertyincidence, data = eusilcA_smp)

# Get EBP estimates using ebp function
EBP <- ebp(eqIncome ~ cash, pop_data = eusilcA_pop, pop_domains = "district", 
    smp_data = eusilcA_smp, smp_domains = "district", transformation = "log", 
    threshold = 11161.44, seed = 123, 
    L = 5000)



# Comparison of the nested error linear regression models
EB$fit$summary
summary(EBP$model)
# The models are exactly the same


# Comparison of estimates

# Order the return data frames in the same way
EB$eb$domain <- as.character(EB$eb$domain)
EBP$ind$Domain <- as.character(EBP$ind$Domain)

EB$eb <- EB$eb[order(EB$eb$domain),]
EBP$ind <- EBP$ind[order(EBP$ind$Domain),]

# Comparison of single values
EB$eb$eb
EBP$ind$Head_Count

# Mean relative difference
all.equal(EB$eb$eb, EBP$ind$Head_Count)

# Summary of the difference between the results
summary(EB$eb$eb - EBP$ind$Head_Count)
