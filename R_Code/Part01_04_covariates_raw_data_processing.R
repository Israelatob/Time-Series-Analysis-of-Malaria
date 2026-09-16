options(scipen = 999)
rm(list=ls(all=TRUE))

library(readxl)
library(dplyr)

# 1. SET WORKING DIRECTORY
# Note: Ensure "Additiona" isn't a typo for "Additional" in your actual folder name
setwd(r"(C:\Users\17034\Downloads\GSU PhD\Prof. Kirpich\Datasets\New datasets\Malaria\Final\Additional datasets\Covariates)")

# 2. LOAD DATA
load_assign <- function(path) {
  env <- new.env()
  load(path, envir = env)
  return(env[[ls(env)[1]]])
}

# We force the internal data into the names your Join expects
#malaria_data_all <- load_assign("malaria_data_all.RData")
gdp_summary      <- load_assign("gdp_summary.RData")
age_summary      <- load_assign("age_summary.RData")
den_summary      <- load_assign("den_summary.RData")
phy_summary      <- load_assign("phy_summary.RData")
rain_summary     <- load_assign("rain_summary.RData")
temp_summary     <- load_assign("temp_summary.RData")
elev_summary     <- load_assign("elev_summary.RData")
lat_summary     <- load_assign("latlong_summary.RData")
# 


# lat_summary <- read.csv("average-latitude-longitude-countries.csv") 
# 
# 
# temp_summary <- read_excel("Average Annual Temperature.xlsx", sheet = 1)
# 
# elev_summary <- read_excel("Elevation.xlsx", sheet = 1)
# colnames(elev_summary)[2] <- "Elevation"
# colnames(elev_summary)[1] <- "Country"
# elev_summary

#Physcian to population ratio
x <- phy_summary$median_phy
names(x) <- phy_summary$Country

phyd <- as.matrix(dist(x, method = "manhattan"))

rownames(phyd) <- names(x)
colnames(phyd) <- names(x)


phyd

phyd_normalize<- phyd/ max(phyd) * 1000
phyd_normalize

dim(phyd_normalize)


#GDP per capita
x <- gdp_summary$median_gdp
names(x) <- gdp_summary$Country

gdpd <- as.matrix(dist(x, method = "manhattan"))

rownames(gdpd) <- names(x)
colnames(gdpd) <- names(x)


gdpd

gdpd_normalize<- gdpd/ max(gdpd) * 1000
gdpd_normalize

dim(gdpd_normalize)


#Population Density 
x <- den_summary$median_den
names(x) <- den_summary$Country

dend <- as.matrix(dist(x, method = "manhattan"))

rownames(dend) <- names(x)
colnames(dend) <- names(x)


dend

dend_normalize<- dend/ max(dend) * 1000
dend_normalize

dim(dend_normalize)


#Mean age 
x <- age_summary$median_age
names(x) <- age_summary$Country

aged <- as.matrix(dist(x, method = "manhattan"))

rownames(aged) <- names(x)
colnames(aged) <- names(x)


aged

aged_normalize<- aged/ max(aged) * 1000
aged_normalize

dim(aged_normalize)

#Average rainfall 
x <- rain_summary$median_rain
names(x) <- rain_summary$Country

raind <- as.matrix(dist(x, method = "manhattan"))

rownames(raind) <- names(x)
colnames(raind) <- names(x)


raind

raind_normalize<- raind/ max(raind) * 1000
raind_normalize

dim(raind_normalize)


#Average temperature 
x <- temp_summary$median_temp
names(x) <- temp_summary$Country

tempd <- as.matrix(dist(x, method = "manhattan"))

rownames(tempd) <- names(x)
colnames(tempd) <- names(x)


tempd

tempd_normalize<- tempd/ max(tempd) * 1000
tempd_normalize

dim(tempd_normalize)



#Average Elevation 
x <- elev_summary$elevation
names(x) <- elev_summary$Country

elevd <- as.matrix(dist(x, method = "manhattan"))

rownames(elevd) <- names(x)
colnames(elevd) <- names(x)


elevd

elevd_normalize<- elevd/ max(elevd) * 1000
elevd_normalize

dim(elevd_normalize)


#Average latitude 
x <- lat_summary$Latitude
names(x) <- lat_summary$Country

latd <- as.matrix(dist(x, method = "manhattan"))

rownames(latd) <- names(x)
colnames(latd) <- names(x)


latd

latd_normalize<- latd/ max(latd) * 1000
latd_normalize

dim(latd_normalize)