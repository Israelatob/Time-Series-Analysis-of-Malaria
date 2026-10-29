options(scipen = 999)
rm(list = ls(all = TRUE))

library(readxl)
library(dplyr)

# ==============================================================================
# SETTINGS & PATH DECLARATIONS
# ==============================================================================

# Set working directory to project R_Code directory
base_dir <- "C:/Users/17034/Downloads/GSU PhD/Prof. Kirpich/Datasets/GitHub Files/Time-Series-Analysis-of-Malaria/R_Code"
if (dir.exists(base_dir)) setwd(base_dir)

# Define clean relative paths from R_Code
dir_data   <- file.path("..", "R_Data")
dir_plots  <- file.path("..", "Plots")
dir_output <- file.path("..", "R_Output")

# Ensure output directories exist
if (!dir.exists(dir_plots))  dir.create(dir_plots, recursive = TRUE)
if (!dir.exists(dir_output)) dir.create(dir_output, recursive = TRUE)

# ==============================================================================
# LOAD AUXILIARY DATA
# ==============================================================================

load_assign <- function(path) {
  if (!file.exists(path)) {
    warning("File not found: ", path)
    return(NULL)
  }
  env <- new.env()
  load(path, envir = env)
  return(env[[ls(env)[1]]])
}

# Load summaries directly from relative dir_data path
gdp_summary  <- load_assign(file.path(dir_data, "gdp_summary.RData"))
age_summary  <- load_assign(file.path(dir_data, "age_summary.RData"))
den_summary  <- load_assign(file.path(dir_data, "den_summary.RData"))
phy_summary  <- load_assign(file.path(dir_data, "phy_summary.RData"))
rain_summary <- load_assign(file.path(dir_data, "rain_summary.RData"))
temp_summary <- load_assign(file.path(dir_data, "temp_summary.RData"))
elev_summary <- load_assign(file.path(dir_data, "elev_summary.RData"))
lat_summary  <- load_assign(file.path(dir_data, "lat_summary.RData"))

# ==============================================================================
# COMPUTE PAIRWISE DISTANCE MATRICES
# ==============================================================================

# 1. Physician to population ratio
x <- phy_summary$median_phy
names(x) <- phy_summary$Country

phyd <- as.matrix(dist(x, method = "manhattan"))
rownames(phyd) <- names(x)
colnames(phyd) <- names(x)

phyd_normalize <- phyd / max(phyd) * 1000
cat("Phy Matrix Dimensions:", dim(phyd_normalize), "\n")

# 2. GDP per capita
x <- gdp_summary$median_gdp
names(x) <- gdp_summary$Country

gdpd <- as.matrix(dist(x, method = "manhattan"))
rownames(gdpd) <- names(x)
colnames(gdpd) <- names(x)

gdpd_normalize <- gdpd / max(gdpd) * 1000
cat("GDP Matrix Dimensions:", dim(gdpd_normalize), "\n")

# 3. Population Density 
x <- den_summary$median_den
names(x) <- den_summary$Country

dend <- as.matrix(dist(x, method = "manhattan"))
rownames(dend) <- names(x)
colnames(dend) <- names(x)

dend_normalize <- dend / max(dend) * 1000
cat("Density Matrix Dimensions:", dim(dend_normalize), "\n")

# 4. Median age 
x <- age_summary$median_age
names(x) <- age_summary$Country

aged <- as.matrix(dist(x, method = "manhattan"))
rownames(aged) <- names(x)
colnames(aged) <- names(x)

aged_normalize <- aged / max(aged) * 1000
cat("Age Matrix Dimensions:", dim(aged_normalize), "\n")

# 5. Average rainfall 
x <- rain_summary$median_rain
names(x) <- rain_summary$Country

raind <- as.matrix(dist(x, method = "manhattan"))
rownames(raind) <- names(x)
colnames(raind) <- names(x)

raind_normalize <- raind / max(raind) * 1000
cat("Rainfall Matrix Dimensions:", dim(raind_normalize), "\n")

# 6. Average temperature 
# Standardize 'country' to 'Country' if needed
if ("country" %in% names(temp_summary) && !"Country" %in% names(temp_summary)) {
  temp_summary <- temp_summary %>% rename(Country = country)
}

# Clean country string formatting
temp_summary <- temp_summary %>%
  mutate(
    Country = gsub("\u00A0", " ", Country),
    Country = trimws(as.character(Country))
  )

# Extract temperature vector
x <- temp_summary$median_temp
names(x) <- temp_summary$Country

# Compute Manhattan distance matrix
tempd <- as.matrix(dist(x, method = "manhattan"))
rownames(tempd) <- names(x)
colnames(tempd) <- names(x)

# Normalize
tempd_normalize <- tempd / max(tempd) * 1000

cat("Temp Matrix Dimensions:", dim(tempd_normalize), "| Max value:", max(tempd_normalize), "\n")

# 7. Average Elevation 
x <- elev_summary$elevation
names(x) <- elev_summary$Country

elevd <- as.matrix(dist(x, method = "manhattan"))
rownames(elevd) <- names(x)
colnames(elevd) <- names(x)

elevd_normalize <- elevd / max(elevd) * 1000
cat("Elevation Matrix Dimensions:", dim(elevd_normalize), "\n")

# 8. Average latitude 
x <- lat_summary$LAT
names(x) <- lat_summary$Country

latd <- as.matrix(dist(x, method = "manhattan"))
rownames(latd) <- names(x)
colnames(latd) <- names(x)

latd_normalize <- latd / max(latd) * 1000
cat("Latitude Matrix Dimensions:", dim(latd_normalize), "\n")

cat("\n✅ Distance matrix computations completed using relative paths.\n")


# Add this at the end of Part02_all_covariates_raw_data_processing.R
auxiliary_distance_matrices <- list(
  PHY  = phyd_normalize,
  GDP  = gdpd_normalize,
  DEN  = dend_normalize,
  AGE  = aged_normalize,
  RAIN = raind_normalize,
  TEMP = tempd_normalize,
  ELEV = elevd_normalize,
  LAT  = latd_normalize
)

save(auxiliary_distance_matrices, file = file.path(dir_data, "auxiliary_distance_matrices.RData"))