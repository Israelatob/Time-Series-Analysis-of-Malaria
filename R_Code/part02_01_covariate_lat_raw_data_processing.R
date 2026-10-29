# -----------------------------
# Clear environment
# -----------------------------
rm(list = ls(all = TRUE))

# -----------------------------
# Libraries
# -----------------------------
library(readr)   # REQUIRED for CSV files
library(dplyr)
library(tidyr)
library(writexl)

# -----------------------------
# Set working directory
# -----------------------------
setwd(
  "C:/Users/17034/Downloads/GSU PhD/Prof. Kirpich/Datasets/GitHub Files/Time-Series-Analysis-of-Malaria/R_Code"
)

# Verify working directory
cat("Working directory:", getwd(), "\n")

# -----------------------------
# Input & Output File Paths
# -----------------------------
# Input data
lat_input_file <- "../Raw_Data/country_centroids_all.csv"

# Output files
lat_rdata_path <- "../R_Data/lat_summary.RData"
lat_excel_path <- "../R_Output/lat_output.xlsx"

# -----------------------------
# Load Latitude/Longitude Data
# -----------------------------
lat_raw <- read_csv(lat_input_file)

lat_raw <- lat_raw %>%
  rename(Country = SHORT_NAME)

# -----------------------------
# Define Sub-Saharan Africa countries
# -----------------------------
countries_to_select <- c(
  "Angola","Benin","Botswana","Burkina Faso","Burundi","Cabo Verde",
  "Cameroon","Central African Republic","Chad","Comoros",
  "DRC","Congo","Côte d'Ivoire", "Djibouti","Equatorial Guinea","Eritrea","Eswatini",
  "Ethiopia",   "Gabon","Gambia","Ghana","Guinea","Guinea-Bissau","Kenya","Liberia",
  "Madagascar","Malawi","Mali","Mauritania","Mozambique","Namibia","Niger","Nigeria", 
  "Rwanda","Sao Tome and Principe","Senegal","Sierra Leone","Somalia","South Africa",
  "South Sudan", "Sudan","Tanzania","Togo","Uganda","Zambia","Zimbabwe"
)

# -----------------------------
# Clean and Filter Lat/Long Data
# -----------------------------
# 1. Standardize names to match your countries_to_select list
# 2. Filter for SSA countries
# 3. Keep essential columns and sort alphabetically
lat_summary <- lat_raw %>%
  mutate(Country = case_when(
    Country == "Cape Verde" ~ "Cabo Verde",
    Country == "Cote d'Ivoire" ~ "Côte d'Ivoire",
    Country == "Democratic Republic of the Congo" ~ "DRC",
    Country == "Swaziland" ~ "Eswatini",
    Country == "Republic of the Congo" ~ "Congo",
    TRUE ~ Country
  )) %>%
  filter(Country %in% countries_to_select) %>%
  select(Country, LAT) %>%  # Keeping essential columns
  arrange(Country)

# -----------------------------
# View Results
# -----------------------------
print("First few rows of processed SSA Latitude/Longitude data:")
print(head(lat_summary))

print(paste("Number of SSA countries matched:", nrow(lat_summary)))

# -----------------------------
# Create output directories if they don't exist
# -----------------------------
if (!dir.exists("../R_Data"))   dir.create("../R_Data", recursive = TRUE)
if (!dir.exists("../R_Output")) dir.create("../R_Output", recursive = TRUE)

# -----------------------------
# Save ONLY the summary outputs
# -----------------------------
# 1. Save RData containing ONLY lat_summary
save(lat_summary, file = lat_rdata_path)

# 2. Save Excel containing ONLY lat_summary
write_xlsx(
  list(
    "lat_summary" = lat_summary
  ),
  path = lat_excel_path
)

cat("\nSummary files successfully created:\n")
cat(" - RData: ", lat_rdata_path, "\n")
cat(" - Excel: ", lat_excel_path, "\n")