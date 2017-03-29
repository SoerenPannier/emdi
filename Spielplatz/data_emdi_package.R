# Datensätze im Paket ----------------------------------------------------------

# Datensätze von Sören laden und dann im Paket abspeichern
# Old population
#load("C:/austria/mulilevel_eusilcP.RData") # Population

# New population
load("I:/LS-Rendtel/Forschung/emdi Package/austria/mulilevel_eusilcP_meanvar.RData") # Population
library(simFrame)
data("eusilcP")


# Check data set
table(my_eusilcP$sub_2)
summary(as.data.frame(table(as.factor(my_eusilcP$sub_2)))[,"Freq"])
sum(table(as.factor(my_eusilcP$sub_2)))
summary(as.data.frame(table(as.factor(my_eusilcP$region)))[,"Freq"])

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 37.0   232.5   423.0   611.0   638.2 11660.0
# Die Bevölkerung von Wien ist nun sehr groß, aber das macht ja auch irgendwie 
# Sinn
eusilcP_main <- eusilcP[eusilcP$main == 1,]
table(eusilcP_main$region)
summary(as.data.frame(table(as.factor(eusilcP_main$region)))[,"Freq"])


# Umbennen der Datensätze
eusilcA_pop <- my_eusilcP



# Variablenklassen anpassen
class(eusilcA_pop)
class(eusilcA_pop$eqIncome) # array
eusilcA_pop$eqIncome <- as.numeric(eusilcA_pop$eqIncome)
class(eusilcA_pop$eqIncome)


# Sample ziehen auf Haushaltsebene
set.seed(1)
sample_id <- sample(1:25000, 1000, replace=FALSE, prob=NULL)

############################### NEW ############################################
# Inverse Ziehungswahrscheinlichekeiten nach districts
invProb <- data.table(as.data.frame((floor((1000/9))/table(eusilcA_pop$sub_1))^-1))  
colnames(invProb) <- c("sub_1", "invProb")
eusilcA_pop <- merge(eusilcA_pop, invProb, by = "sub_1")

# Stratified sampling
sample_id2 <- stratified(eusilcA_pop, "sub_1", floor((1000/9)))

eusilcA_smp <- as.data.frame(sample_id2)
################################################################################

# Receive sample on houehold level corresponding to population data
eusilcA_smp <- eusilcA_pop[sample_id, ]

eusilcA_smp$eqIncome <- as.numeric(eusilcA_smp$eqIncome)
class(eusilcA_smp$eqIncome)


# Löschen aller nicht genutzten Variablen
fixed = eqIncome ~ gender + eqsize + py010n + py050n + py090n + py100n + py110n + py120n + py130n + hy040n + hy050n + hy070n + hy090n + hy145n + region 

mod_vars <- gsub(" ", "",unlist(strsplit(paste(fixed[3]), "[+]")), 
                 fixed = TRUE)
data_vars <- c(as.character(fixed[2]), mod_vars, "sub_2")

eusilcA_pop <- eusilcA_pop[, data_vars]
head(eusilcA_pop)
dim(eusilcA_pop)

#data_vars <- c(as.character(fixed[2]), mod_vars, "sub_0", "sub_1", "sub_2", "sub_3", "invProb")
eusilcA_smp <- eusilcA_smp[, data_vars]
head(eusilcA_smp)
dim(eusilcA_smp)


# Umbennenung einiger Variablen
names(eusilcA_pop)

library(plyr)
eusilcA_pop <- rename(eusilcA_pop, c("py010n"="cash", "py050n"="self_empl", 
                                     "py090n"="unempl_ben", "py100n"="age_ben",
                                     "py110n"="surv_ben", "py120n"="sick_ben",
                                     "py130n"="dis_ben", "hy040n"="rent",
                                     "hy050n"="fam_allow", "hy070n"="house_allow",
                                     "hy090n"="cap_inv", "hy145n"="tax_adj",
                                     "sub_2"="district", "region" = "state"))
head(eusilcA_pop)

names(eusilcA_smp)

library(plyr)
eusilcA_smp <- rename(eusilcA_smp, c("py010n"="cash", "py050n"="self_empl", 
                                     "py090n"="unempl_ben", "py100n"="age_ben",
                                     "py110n"="surv_ben", "py120n"="sick_ben",
                                     "py130n"="dis_ben", "hy040n"="rent",
                                     "hy050n"="fam_allow", "hy070n"="house_allow",
                                     "hy090n"="cap_inv", "hy145n"="tax_adj",
                                     "region"="state", "sub_2"="district"))
head(eusilcA_smp)


# Factor anstatt character
# counties gibt es nicht mehr
#eusilcA_pop$county <- factor(eusilcA_pop$county, levels=unique(eusilcA_pop$county))
#str(eusilcA_pop$county)

#eusilcA_smp$county <- factor(eusilcA_smp$county, levels=unique(eusilcA_pop$county))
#str(eusilcA_smp$county)


# Hinzufügen von konstantem Gewicht
#library(emdi)
#data("eusilcA_smp")
eusilcA_smp$weight <- 25000/nrow(eusilcA_smp) 


# Abspeichern der Datensätze im Datenordner des  Pakets
setwd("C:/Package Project/emdi_git")
save("eusilcA_pop", file = "./data/eusilcA_pop.RData")
save("eusilcA_smp", file = "./data/eusilcA_smp.RData")

devtools::use_data(eusilcA_pop, compress = "xz", overwrite = TRUE)
devtools::use_data(eusilcA_smp, compress = "xz", overwrite = TRUE)



# Beispiele im Paket -----------------------------------------------------------

# Direct
emdi_direct <- direct(y="eqIncome", smp_data=eusilcA_smp, smp_domains="district", 
                      weights="invProb", threshold=10859.24, var=TRUE, 
                      bootType = "naive", B=50, seed=123, X = NULL, 
                      totals = NULL, na.rm=TRUE)



print(emdi_direct)
summary(emdi_direct)



# Model-based 
emdi_model <- ebp( fixed = eqIncome ~ gender + eqsize + cash + self_empl + 
                     unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + 
                     rent + fam_allow + house_allow + cap_inv + tax_adj,
                   pop_data = eusilcA_pop,
                   pop_domains = "district",
                   smp_data = eusilcA_smp,
                   smp_domains = "district",
                   L = 5, B = 5,
                   na.rm = TRUE
)



emdi_model <- ebp( fixed = eqIncome ~ gender + eqsize + cash + self_empl + 
                     unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + 
                     rent + fam_allow + house_allow + cap_inv + tax_adj,
                   pop_data = eusilcA_pop,
                   pop_domains = "district",
                   smp_data = eusilcA_smp,
                   smp_domains = "district",
                   threshold = 10722.66,
                   transformation = "no",
                   L= 50,
                   MSE = TRUE,
                   B = 50,
                   custom_indicator = list( my_max = function(y, threshold){max(y)},
                                            my_min = function(y, threshold){min(y)}
                   ),  
                   na.rm = TRUE, 
                   cpus = 1
)


# Map plot function

# Are the domains defined equally in census and shape file?
unique(eusilcA_pop$district)
class(eusilcA_pop$district)
length(unique(eusilcA_pop$district))

# Load shape file
shape_austria_dis <- readRDS("C:/Package Project/Tmp_functions_data/old data/AUT_adm2.rds")
save("shape_austria_dis", file = "C:/Package Project/emdi_cran/inst/shapes/shape_austria_dis.RData")

load(system.file("shapes/shape_austria_dis.RData", package="emdi"))

# How is the variable defined?
class(shape_austria_dis$NAME_2)
length(unique(shape_austria_dis$NAME_2))


mapping_table <- data.frame(unique(eusilcA_pop$district), 
                            unique(shape_austria_dis$NAME_2))


map_plot(object = emdi_model, MSE = TRUE, CV = FALSE, map_obj = shape_austria_dis,
    indicator = c("Gini"), map_dom_id = "NAME_2", map_tab = mapping_table)



