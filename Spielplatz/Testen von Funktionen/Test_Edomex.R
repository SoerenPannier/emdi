# Check functions in emdi with data from EDOMEX


setwd("C:/Package Project/daten_mexico/daten_mexico")
library(foreign)


# New household data
census_edomex <- read.dta("base_censo2010_hogares_15 20141024_all2.dta")
survey_edomex <- read.dta("base_MCS2010_hogares_15 20141024_all2.dta")

# Create numeric domain IDs for survey_mex and census since mun is character
survey_edomex <- data.frame(survey_edomex, domain_id=as.numeric(survey_edomex$mun))
unique(survey_edomex$domain_id)


# Create factor domain IDs for survey_mex data
survey_edomex$domain_id <- as.factor(survey_edomex$domain_id)
class(survey_edomex$domain_id)
levels(survey_edomex$domain_id) <- c(# ID 5
  "Almoloya de Juárez",
  # ID 8
  "Amatepec",
  # ID 9
  "Amecameca",
  # ID 10
  "Apaxco",
  # ID 11
  "Atenco",
  # ID 13
  "Atizapán de Zaragoza",
  # ID 14
  "Atlacomulco",
  # ID 18
  "Calimaya",
  # ID 20
  "Coacalco de Berriozábal",
  # ID 23
  "Coyotepec",
  # ID 24
  "Cuautitlán",
  # ID 25
  "Chalco",
  # ID 29
  "Chicoloapan",
  # ID 30
  "Chiconcuac",
  # ID 31
  "Chimalhuacán",
  # ID 33
  "Ecatepec de Morelos",
  # ID 35
  "Huehuetoca",
  # ID 37
  "Huixquilucan",
  # ID 39
  "Ixtapaluca",
  # ID 40
  "Ixtapan de la Sal",
  # ID 44
  "Jaltenco",
  # ID 45
  "Jilotepec",
  # ID 46
  "Jilotzingo",
  # ID 50
  "Juchitepec",
  # ID 51
  "Lerma",
  # ID 53
  "Melchor Ocampo",
  # ID 54
  "Metepec",
  # ID 56
  "Morelos",
  # ID 57
  "Naucalpan de Juárez",
  # ID 58
  "Nezahualcóyotl",
  # ID 60
  "Nicolás Romero",
  # ID 61
  "Nopaltepec",
  # ID 62
  "Ocoyoacac",
  # ID 64
  "El Oro",
  # ID 68
  "Ozumba",
  # ID 70
  "La Paz",
  # ID 71
  "Polotitlán",
  # ID 76
  "San Mateo Atenco",
  # ID 80
  "Sultepec",
  # ID 81
  "Tecámac",
  # ID 84
  "Temascalapa",
  # ID 91
  "Teoloyucan",
  # ID 94
  "Tepetlixpa",
  # ID 95
  "Tepotzotlán",
  # ID 96
  "Tequixquiac",
  # ID 99
  "Texcoco",
  # ID 103
  "Tlalmanalco",
  # ID 104
  "Tlalnepantla de Baz",
  # ID 106
  "Toluca",
  # ID 108
  "Tultepec",
  # ID 109
  "Tultitlán",
  # ID 112
  "Villa del Carbón",
  # ID 114
  "Villa Victoria",
  # ID 118
  "Zinacantepec",
  # ID 119
  "Zumpahuacán",
  # ID 120
  "Zumpango",
  # ID 121
  "Cuautitlán Izcalli",
  # ID 122
  "Valle de Chalco Solidaridad"
)


unique(survey_edomex$domain_id)




# Create numeric IDs for domains
census_edomex <- data.frame(census_edomex, domain_id=as.numeric(census_edomex$mun))

# Create factor domain IDs for census data
class(census_edomex$domain_id)
census_edomex$domain_id <- as.factor(census_edomex$domain_id)
unique(census_edomex$domain_id)

levels(census_edomex$domain_id) <- c("Acambay",
                              "Acolman",
                              "Aculco",
                              "Almoloya de Alquisiras",
                              "Almoloya de Juárez",
                              "Almoloya del Río",
                              "Amanalco",
                              "Amatepec",
                              "Amecameca",
                              "Apaxco",
                              "Atenco",
                              "Atizapán",
                              "Atizapán de Zaragoza",
                              "Atlacomulco",
                              "Atlautla",
                              "Axapusco",
                              "Ayapango",
                              "Calimaya",
                              "Capulhuac",
                              "Coacalco de Berriozábal",
                              "Coatepec Harinas",
                              "Cocotitlán",
                              "Coyotepec",
                              "Cuautitlán",
                              "Chalco",
                              "Chapa de Mota",
                              "Chapultepec",
                              "Chiautla",
                              "Chicoloapan",
                              "Chiconcuac",
                              "Chimalhuacán",
                              "Donato Guerra",
                              "Ecatepec de Morelos",
                              "Ecatzingo",
                              "Huehuetoca",
                              "Hueypoxtla",
                              "Huixquilucan",
                              "Isidro Fabela",
                              "Ixtapaluca",
                              "Ixtapan de la Sal",
                              "Ixtapan del Oro",
                              "Ixtlahuaca",
                              "Xalatlaco",
                              "Jaltenco",
                              "Jilotepec",
                              "Jilotzingo",
                              "Jiquipilco",
                              "Jocotitlán",
                              "Joquicingo",
                              "Juchitepec",
                              "Lerma",
                              "Malinalco",
                              "Melchor Ocampo",
                              "Metepec",
                              "Mexicaltzingo",
                              "Morelos",
                              "Naucalpan de Juárez",
                              "Nezahualcóyotl",
                              "Nextlalpan",
                              "Nicolás Romero",
                              "Nopaltepec",
                              "Ocoyoacac",
                              "Ocuilan",
                              "El Oro",
                              "Otumba",
                              "Otzoloapan",
                              "Otzolotepec",
                              "Ozumba",
                              "Papalotla",
                              "La Paz",
                              "Polotitlán",
                              "Rayón",
                              "San Antonio la Isla",
                              "San Felipe del Progreso",
                              "San Martín de las Pirámides",
                              "San Mateo Atenco",
                              "San Simón de Guerrero",
                              "Santo Tomás",
                              "Soyaniquilpan de Juárez",
                              "Sultepec",
                              "Tecámac",
                              "Tejupilco",
                              "Temamatla",
                              "Temascalapa",
                              "Temascalcingo",
                              "Temascaltepec",
                              "Temoaya",
                              "Tenancingo",
                              "Tenango del Aire",
                              "Tenango del Valle",
                              "Teoloyucan",
                              "Teotihuacán",
                              "Tepetlaoxtoc",
                              "Tepetlixpa",
                              "Tepotzotlán",
                              "Tequixquiac",
                              "Texcaltitlán",
                              "Texcalyacac",
                              "Texcoco",
                              "Tezoyuca",
                              "Tianguistenco",
                              "Timilpan",
                              "Tlalmanalco",
                              "Tlalnepantla de Baz",
                              "Tlatlaya",
                              "Toluca",
                              "Tonatico",
                              "Tultepec",
                              "Tultitlán",
                              "Valle de Bravo",
                              "Villa de Allende",
                              "Villa del Carbón",
                              "Villa Guerrero",
                              "Villa Victoria",
                              "Xonacatlán",
                              "Zacazonapan",
                              "Zacualpan",
                              "Zinacantepec",
                              "Zumpahuacán",
                              "Zumpango",
                              "Cuautitlán Izcalli",
                              "Valle de Chalco Solidaridad",
                              "Luvianos",
                              "San José del Rincón",
                              "Tonanitla"
)

unique(census_edomex$domain_id)




# Missing values have to be deleted
census_edomex <- census_edomex[!(is.na(census_edomex$bienes)),]
census_edomex <- census_edomex[!(is.na(census_edomex$actcom)),]
census_edomex <- census_edomex[!(is.na(census_edomex$pcocup)),]
census_edomex <- census_edomex[!(is.na(census_edomex$pcpering)),]
census_edomex <- census_edomex[!(is.na(census_edomex$jnived)),]
census_edomex <- census_edomex[!(is.na(census_edomex$clase_hog)),]



# Generate emdiObjects
ebp_edomex <- ebp( fixed = ictpc ~  pcocup + jnived + clase_hog + pcpering +
              bienes + actcom,
            pop_data = census_edomex,
            pop_domains = "domain_id",
            smp_data = survey_edomex,
            smp_domains = "domain_id",
            threshold = 903.04,
            transformation = "box.cox",
            L=5,
            MSE = T,
            B = 5,
            custom_indicator = list( my_max = function(y, threshold){max(y)},
                                     my_min = function(y, threshold){min(y)}
            ),
            na.rm=TRUE
)

direct_edomex <- direct(y = "inglabpc", 
                        smp_data = survey_edomex, 
                        smp_domains = "domain_id", 
                        weights=NULL, threshold=900.8202, var=TRUE, 
                        bootType = "naive", B=50, 
                        seed=123, X = NULL, totals = NULL, na.rm=TRUE)



# some generic functions are already implemented on the resulting object, an S3 class called emdi

summary(ebp_edomex)
print(ebp_edomex)

plot(ebp_edomex)


# some generic functions are already implemented on the resulting object, an S3 class called emdi
summary(direct_edomex)
print(direct_edomex)

# the estimators method extracts point estimations as well as, if set true, MSE and CV estimates
# the resulting class is S3 estimators.ebp
# methods like head and tail are yet to implement for this class
# estimators will extract all, some specifically defined estimators or predefined groups of estimators
estimators(object = ebp_edomex, MSE = F, CV = F, indicator = "all")
estimators(object = direct_edomex, MSE = T, CV = T, indicator = "all")



head(estimators(object = ebp_edomex, MSE = F, CV = T, indicator = "Custom"))

head(estimators(object = ebp_edomex, MSE = F, CV = T, indicator = c("Head_Count","Poverty_Gap")))
head(estimators(object = direct_edomex, MSE = F, CV = T, indicator = c("Head_Count","Poverty_Gap")))

tail(estimators(object = ebp_edomex, MSE = F, CV = T, indicator = c("Head_Count","Poverty_Gap")))
tail(estimators(object = direct_edomex, MSE = F, CV = T, indicator = c("Head_Count","Poverty_Gap")))

estimators(object = ebp_edomex, MSE = T, CV = T, indicator = c("Quantiles"))
estimators(object = direct_edomex, MSE = T, CV = T, indicator = c("Quantiles"))


estimators(object = ebp, MSE = F, CV = F, indicator = c("Inequality"))

selection <- estimators(object = ebp, MSE = T, CV = T, indicator = "Poverty")



#the write.excel method, writes the summary output as well as the chosen estimates into an excel file
write.excel(ebp_edomex, file ="excel_output.xlsx", indicator = "Poverty", MSE = T, CV = T)
write.excel(direct_edomex, file ="excel_output_direct.xlsx", indicator = "Poverty", MSE = T, CV = T)

write.excel(ebp, file ="excel_output_incl_MSE.xlsx", indicator = "all", MSE = T, CV = F)

write.excel(ebp, file ="excel_output_all.xlsx", indicator = "Poverty", MSE = T, CV = T)



write.excel(ebp, file ="excel_output_all_sep.xlsx", indicator = "all", MSE = T, CV = T, split = T)

# plot daten gives some graphs to analyse normal assumption, as well as if chosen,
# the estimation of the pover parameter of an underlying box-cox transformation
theme_set(theme_gray(base_size=18))
plot(ebp_edomex)


#the estimators can be linked to a spatial poygone, if no polygone is given an artificial one is created
map_plot(object = ebp, indicator = c("Poverty_Gap", "Head_Count"), MSE =  F, CV = F)

# for demonstration of the plot with a spatial polygone use an artificial dataset with a number of domains
# fitting to a polygone file

# reading in the spatial polygone
load("H:/Ann-Kristin/Paket/Example/shp_mex.RData")

#as the domainnames in the polygone do not match the ones from the artificial data,
#a mapping table needs to be provided
map_table <- data.frame(Domain = unique(census_edomex$domain_id), 
                        mun = sort(shp_mex$mun))

#' #when rgeos is not available, polygon geometry 	computations in maptools depends on the package gpclib,
#' #which has a restricted licence. It is disabled by default; to enable gpclib, type gpclibPermit()
gpclibPermit() 
theme_set(theme_gray(base_size=18))
map_plot(object = ebp_edomex, MSE = T, CV = T, map_obj = shp_mex,
         indicator = "Head_Count", map_dom_id = "mun", map_tab = map_table)


map_plot(object = direct_edomex, MSE = T, CV = T, map_obj = shp_mex,
         indicator = "Head_Count", map_dom_id = "mun", map_tab = map_table)









