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
# Load Latitude/Longitude data
# -----------------------------
# Path to your CSV file
latlong_data_path <- "C:/Users/17034/Downloads/GSU PhD/Prof. Kirpich/Datasets/New datasets/Malaria/Final/Additional datasets/Covariates/average-latitude-longitude-countries.csv"

# Set working directory to the folder containing the file
if(file.exists(latlong_data_path)) {
  setwd(dirname(latlong_data_path))
}

# Load the CSV file
latlong_raw <- read_csv(latlong_data_path)

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
# 3. Sort alphabetically
latlong_summary_by_country <- latlong_raw %>%
  mutate(Country = case_when(
    Country == "Cape Verde" ~ "Cabo Verde",
    Country == "Cote d'Ivoire" ~ "Côte d'Ivoire",
    Country == "Tanzania, United Republic of" ~ "Tanzania",
    Country == "Congo, The Democratic Republic of the" ~ "DRC",
    Country == "Swaziland" ~ "Eswatini",
    Country == "Ethopia" ~ "Ethiopia",
    TRUE ~ Country
  )) %>%
  filter(Country %in% countries_to_select) %>%
  select(Country, Latitude, Longitude) %>%  # Keeping only essential columns
  arrange(Country)

# -----------------------------
# View Results
# -----------------------------
print("First few rows of processed SSA Latitude/Longitude data:")
print(head(latlong_summary_by_country))

print(paste("Number of SSA countries matched:", nrow(latlong_summary_by_country)))

# -----------------------------
# Save outputs
# -----------------------------
# Save as RData for modeling
save(latlong_summary_by_country, file = "latlong_summary.RData")
save(countries_to_select, file = "ssa_latlong_countries.RData")

# Save as Excel for external review
write_xlsx(
  list(
    "Lat_Long_Summary" = latlong_summary_by_country
  ),
  "latlong_output.xlsx"
)

print("Files 'latlong_summary.RData' and 'latlong_output.xlsx' saved successfully!")