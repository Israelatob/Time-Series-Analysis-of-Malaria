# -----------------------------
# Clear environment
# -----------------------------
rm(list = ls(all = TRUE))

# -----------------------------
# Libraries
# -----------------------------
library(readxl)
library(dplyr)
library(tidyr)
library(writexl)

# -----------------------------
# Load Elevation data
# -----------------------------
elev_data_path <- "C:/Users/17034/Downloads/GSU PhD/Prof. Kirpich/Datasets/New datasets/Malaria/Final/Additional datasets/Covariates/Elevation.xlsx"

if(file.exists(elev_data_path)) {
  setwd(dirname(elev_data_path))
}

# CHANGE: Load the "Average Elevation" sheet instead of sheet 1
# This sheet contains all 46 countries.
elev_raw <- read_excel(elev_data_path, sheet = "Average Elevation")

# -----------------------------
# Define Sub-Saharan Africa countries
# -----------------------------
countries_to_select <- c(
  "Angola","Benin","Botswana","Burkina Faso","Burundi","Cabo Verde",
  "Cameroon","Central African Republic","Chad","Comoros",
  "DRC","Congo","Côte d'Ivoire", "Djibouti","Equatorial Guinea","Eritrea","Eswatini",
  "Ethiopia", "Gabon","Gambia","Ghana","Guinea","Guinea-Bissau","Kenya","Liberia",
  "Madagascar","Malawi","Mali","Mauritania","Mozambique","Namibia","Niger","Nigeria", 
  "Rwanda","Sao Tome and Principe","Senegal","Sierra Leone","Somalia","South Africa",
  "South Sudan", "Sudan","Tanzania","Togo","Uganda","Zambia","Zimbabwe"
)

# -----------------------------
# Clean and Filter Elevation Data
# -----------------------------
elev_summary_by_country <- elev_raw %>%
  # 1. Standardize column names for this specific sheet
  rename(elevation = Elevation) %>% 
  # 2. Fix hidden characters and trim spaces
  mutate(Country = gsub("\u00A0", " ", Country), 
         Country = trimws(Country)) %>%
  # 3. Handle standard name variations
  mutate(Country = case_when(
    Country == "Democratic Republic of Congo" ~ "DRC",
    Country == "Congo, The Democratic Republic of the" ~ "DRC",
    Country == "Cote d'Ivoire" ~ "Côte d'Ivoire",
    Country == "Swaziland" ~ "Eswatini",
    TRUE ~ Country
  )) %>%
  # 4. Filter for your target list
  filter(Country %in% countries_to_select) %>%
  select(Country, elevation) %>%
  arrange(Country)

# -----------------------------
# View Results
# -----------------------------
print(paste("Total countries matched:", nrow(elev_summary_by_country)))
print(head(elev_summary_by_country))

# -----------------------------
# Save outputs
# -----------------------------
save(elev_summary_by_country, file = "elev_summary.RData")
write_xlsx(list("Elevation_Summary" = elev_summary_by_country), "elev_output.xlsx")

print("Files saved successfully! You should now have all 46 countries.")