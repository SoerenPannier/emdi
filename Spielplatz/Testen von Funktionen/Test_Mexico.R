# Check package with Mexican data

library(foreign)
library(readstata13)
library(emdi)

# Household data and sample data
setwd("C:/Package Project/daten_mexico")

# Households
census_mex <- read.dta13("Base_Censo2010_hog_Nacional.dta")
survey_mex <- read.dta13("Base_MCS_hog_Nacional.dta")

# Individuals
census_mexInd <- read.dta13("Base_Censo2010_per_Nacional.dta")
survey_mexInd <- read.dta13("Base_MCS_per_Nacional.dta")

# Explanations
setwd("H:/Ann-Kristin/Bases municipales 2015_01_27")
info_mex <- read.dta13("base_2donivel_Nacional_2010.dta")


# First overview
str(census_mex)
str(survey_mex)
head(census_mex)
head(survey_mex)


# Is the census complete? ------------------------------------------------------
sum(census_mex$factor)
# yes. Population in Mexico: 122 million which means appr. 4 people per household

# Which variable indicates the states and the municipalities within states?
str(info_mex)
# states = entidad, names of states=nom_ent, municipalities=mun, 
# name of municipalities=nom_mun, combination of both indicators=cve_mun

# Check if in census
table(census_mex$ent)
# 32 states
# Focus: Distrito Federal
info_mex$entidad[info_mex$nom_ent=="Distrito Federal"]
table(census_mex$mun[census_mex$ent=="09"])
unique(info_mex$nom_ent[info_mex$entidad=="09"])


info_mex$entidad[info_mex$nom_ent=="M?xico"]
table(census_mex$mun[census_mex$ent=="15"])
unique(info_mex$nom_ent[info_mex$entidad=="15"])


# Check if in survey
table(survey_mex$ent)
# 32 states
# no variables for municipalities

# income variable
summary(survey_mex$inglabpc)

# Specify the model 
model <- lm(survey_mex$inglabpc ~ survey_mex$pcocup + survey_mex$jnived + 
            survey_mex$clase_hog + survey_mex$pcpering +  
            survey_mex$bienes + survey_mex$actcom)
summary(model) # R2= 0.2461

# Try ebp for Mexcian states ---------------------------------------------------
set.seed(1)
emdi_mexico <- ebp(fixed = inglabpc ~ pcocup + jnived + clase_hog + 
                   pcpering + bienes + actcom,
                   pop_data = census_mex,
                   pop_domains = "ent",
                   smp_data = survey_mex, 
                   smp_domains = "ent",
                   transformation = "box.cox",
                   interval = c(0.25,0.3),
                   L = 1,
                   MSE = TRUE, 
                   B = 1,
                   na.rm = TRUE)


# Use methods for emdi objects
print(emdi_mexico)
summary(emdi_mexico)

plot(emdi_mexico)#, range=seq(0.15,0.4,by=0.03)) # does not work


# Selection of indicators
estimators(object = emdi_mexico, MSE = TRUE, CV = TRUE)
estimators(object = emdi_mexico, MSE = TRUE, CV = TRUE, indicator = "Poverty")
estimators(object = emdi_mexico, MSE = TRUE, CV = TRUE, indicator = "Custom")
estimators(object = emdi_mexico, MSE = TRUE, CV = TRUE, indicator = "Quantiles")


# Read in shapefile
library(sp) 
# 
shape_mexico <- readRDS("./MEX_adm1.rds")
plot(shape_mexico)
shape_mexico$ID_1
unique(census_mex$ent)

mapping_table <- data.frame(unique(census_mex$ent), shape_mexico$ID_1)


map_plot(emdi_mexico, indicator = "Mean", MSE = TRUE, CV = TRUE, 
         map_obj = shape_mexico, map_dom_id = "ID_1", map_tab = mapping_table)



# Try direct for Mexcian states ------------------------------------------------
# Loading sample data

direct_mexico <- direct(y = "inglabpc", 
                        smp_data = survey_mex,#[filt,], 
                        smp_domains = "ent", 
                        weights=NULL, threshold=900.8202, var = TRUE, 
                        bootType = "naive", B = 5, 
                        seed=123, X = NULL, totals = NULL, 
                        custom_indicator = list( my_max = function(y,weights, threshold){max(y)},
                                                 my_min = function(y, weights,threshold){min(y)})
                                                 ,na.rm=TRUE)


# Use methods for emdi objects
print(direct_mexico)
summary(direct_mexico)

# Selection of indicators
estimators(object = direct_mexico, MSE = TRUE, CV = TRUE)
estimators(object = direct_mexico, MSE = TRUE, CV = TRUE, indicator = "Poverty")
estimators(object = direct_mexico, MSE = TRUE, CV = TRUE, indicator = "Custom")
estimators(object = direct_mexico, MSE = TRUE, CV = TRUE, indicator = "Quantiles")


# Read in shapefile
library(sp) 
# 
shape_mexico <- readRDS("./MEX_adm1.rds")
plot(shape_mexico)
shape_mexico$ID_1
unique(census_mex$ent)

mapping_table <- data.frame(unique(survey_mex$ent), as.integer(unique(survey_mex$ent)))


map_plot(direct_mexico, indicator = "Median", MSE = TRUE, CV = TRUE, 
         map_obj = shape_mexico, map_dom_id = "ID_1", map_tab = mapping_table)



# Export to excel
write.excel(emdi_mexico, file ="excel_output.xlsx", 
            indicator = c("Mean", "Head_Count"), MSE = TRUE, CV = TRUE)

write.excel(direct_mexico, file ="excel_output_direct.xlsx", 
            indicator = c("Mean", "Head_Count"), MSE = TRUE, CV = TRUE)

