#==============================================================================
# SUBJECT:  Application Mexico Poverty mapping - Multidim. Poverty
# AUTHOR:   Timo Schmid and Nikos Tzavidis April 02, 2015
# LICENSE:  GPL Poverty mapping
#------------------------------------------------------------------------------
#==============================================================================

#--------------------------
# loading library
library(nlme)
library(foreign)
library(lme4)
library(MASS)
library(spdep)
library(lqmm)
library(reshape)
library(laeken)
library(formula.tools)
library(nnet)
library(readstata13)
rm(list=ls())
#--------------------------

#--------------------------
# loading data

# New household data
survey_data_hh <- read.dta("Base_MCS_per_EdoMex_all2.dta") 
census_hh <- read.dta("Base_Censo2010_per_EdoMex_all2.dta")

survey_data_hh$ictpc
#--------------------------
# Manipulating data

# 1) Create AREA IDs for Sample and Census

survey_data_hh<-data.frame(survey_data_hh,cluster_id=as.numeric(survey_data_hh$mun))
census_hh<-data.frame(census_hh,cluster_id=as.numeric(census_hh$mun))



# 2) Sorting of the data

survey_data_hh<-survey_data_hh[order(survey_data_hh$cluster_id), ] # Sorting of the data
census_hh<-census_hh[order(census_hh$cluster_id), ] # Sorting of the data


# Define data
survey_data<-survey_data_hh
#survey_data<-data.frame(survey_data,ic=as.numeric(survey_data$ic_rezedu)-1+as.numeric(survey_data$ic_asalud)-1+
                          as.numeric(survey_data$ic_segsoc)-1+as.numeric(survey_data$ic_cv)-1+as.numeric(survey_data$ic_sbv)-1+as.numeric(survey_data$ic_ali)-1)
#survey_data<-data.frame(survey_data,pov_ind=ceiling(survey_data$ic/3))
census<-census_hh
rm("survey_data_hh","census_hh")

#--------------------------
# Model selection


# Some variables were removed from the starting model because of missing values in survey/ census

#formula_full=cuadrantes~pcpering_2+pob_ind+ic_rezedu+ic_asalud+ic_cv+ic_sbv+edad+sexo+remesas+ayuotr+bengob+altitud+
#        tdep+actcom+bienes+jsexo+jedad+pcocup+oportunidades+idh_2010+tam_loc+clase_hog
 
#fit<-multinom(formula_full,data=survey_data)
#fit_red<-step(fit)

#formula<-formula(fit_red)

formula=ictpc~pcpering_2+pob_ind+ic_rezedu+ic_asalud+ic_cv+ic_sbv+edad+remesas+ayuotr+bengob+altitud+
  tdep+actcom+bienes+jedad+pcocup+idh_2010+tam_loc+clase_hog


# Complete cases census (adapt with covariates in formula)
census_new <- census[!(is.na(census$pcpering_2)),]
census_new <- census_new[!(is.na(census_new$pob_ind)),]
census_new <- census_new[!(is.na(census_new$ic_rezedu)),]
census_new <- census_new[!(is.na(census_new$ic_asalud)),]
census_new <- census_new[!(is.na(census_new$ic_cv)),]
levels(census_new$ic_cv)<-levels(survey_data$ic_cv)
census_new <- census_new[!(is.na(census_new$ic_sbv)),]
levels(census_new$ic_sbv)<-levels(survey_data$ic_sbv)
census_new <- census_new[!(is.na(census_new$edad)),]
census_new <- census_new[!(is.na(census_new$remesas)),]
census_new <- census_new[!(is.na(census_new$ayuotr)),]
census_new <- census_new[!(is.na(census_new$bengob)),]
census_new <- census_new[!(is.na(census_new$altitud)),]
census_new <- census_new[!(is.na(census_new$clase_hog)),]
census_new <- census_new[!(is.na(census_new$tam_loc)),]
census_new <- census_new[!(is.na(census_new$idh_2010)),]
census_new <- census_new[!(is.na(census_new$pcocup)),]
census_new <- census_new[!(is.na(census_new$jedad)),]
census_new <- census_new[!(is.na(census_new$bienes)),]
census_new <- census_new[!(is.na(census_new$actcom)),]
census_new <- census_new[!(is.na(census_new$tdep)),]

transform_data <- data_transformation(formula,survey_data, "box.cox", 0.7)
set.seed(100)
emdi_model <- ebp(fixed = formula, pop_data = census_new,
                                  pop_domains = "cluster_id", smp_data = survey_data, smp_domains = "cluster_id",
                                 transformation = "box.cox", L= 1, MSE = TRUE, B = 1,
                  custom_indicator = list( my_max = function(y, threshold){max(y)},
                                           my_min = function(y, threshold){min(y)}), na.rm = TRUE, cpus = 1)

summary(emdi_model)
plot(emdi_model)

estimators(object = emdi_model, CV = F, MSE = TRUE)
estimators(object = emdi_model, CV = TRUE)
emdi_model$model


# choose Gini coefficient and MSE and CV
estimators(emdi_model, indicator = "Gini", MSE = TRUE, CV = TRUE)
# choose custom indicators without MSE and CV
estimators(emdi_model, indicator = "Custom", MSE = TRUE)

head(emdi_model$MSE, n = 10, addrownums = TRUE)

plot(emdi_model, MSE = TRUE)
# Creation of diagnostic plots without labels and titles, different colors
# and without Cook's distance plot.
plot(emdi_model, color=c("blue", "lightblue"))
tail(emdi_model$ind, n = 5, addrownums = NULL)


############################################################################################
# map_plot

# Load shape file
readShapePoly("MEX_adm2")
#load(system.file("shapes/shape_austria_dis.RData", package="emdi"))
# Create mapping table such that variables that indicate domains correspond
# in population data and shape file
mapping_table <- data.frame(unique(census_new$cluster_id),
                            unique(MEX_adm2$NAME_2))
# when rgeos is not available, polygon geometry computations in
# maptools depends on the package gpclib,
# which has a restricted licence. It is disabled by default;
# to enable gpclib, type gpclibPermit()
library(rgeos)
library(maptools)
gpclibPermit()
# Create map plot for mean indicator - point and MSE estimates but no CV
map_plot(object = emdi_model, MSE = TRUE, CV = TRUE,
         map_obj = shape_austria_dis, indicator = c("Mean", "Median"), map_dom_id = "NAME_2",
         map_tab = mapping_table)


###############################################################################################
#--------------------------#--------------------------#--------------------------#--------------------------
# Load function
#source("I:/LS-Rendtel/Forschung/Multinomial SAE/Multinom_function_weight.r")


# Population in area
#Ni_area<-tapply(census$factor,census$cluster_id,sum)

#fit<-multinom_weight(formula,dataframe_sample=survey_data,saind=survey_data$cluster_id,
 #                         dataframe_pop_aux=census_new,Ni_area=Ni_area,
  #                        x.total_saind=census_new$cluster_id,B=100,MSE=TRUE)

#fit
