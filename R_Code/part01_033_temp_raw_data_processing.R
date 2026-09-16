# -----------------------------
# Clear environment
# -----------------------------
rm(list = ls(all = TRUE))

# -----------------------------
# Libraries
# -----------------------------
library(readxl)  # REQUIRED for Excel files
library(dplyr)
library(tidyr)
library(writexl)

# -----------------------------
# Load Temperature data
# -----------------------------
# Path to your Excel file
temp_data_path <- "C:/Users/17034/Downloads/GSU PhD/Prof. Kirpich/Datasets/New datasets/Malaria/Final/Additional datasets/Covariates/Average Annual Temperature.xlsx"

# Set working directory to the folder containing the file
if(file.exists(temp_data_path)) {
  setwd(dirname(temp_data_path))
}

# Load the first sheet of the Excel file
# (Note: If your data is on a specific sheet, use sheet = "SheetName")
temp_raw <- read_excel(temp_data_path, sheet = 1)

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
# Clean and Filter Temperature Data
# -----------------------------
# 1. Standardize names found in the dataset to match your countries_to_select list
# 2. Filter for SSA countries
# 3. Sort alphabetically
temp_summary_by_country <- temp_raw %>%
  mutate(Country = case_when(
    Country == "Ethopia" ~ "Ethiopia",
    Country == "Cote D'Ivoire" ~ "Côte d'Ivoire",
    Country == "Cape Verde" ~ "Cabo Verde",
    Country == "Democratic Republic of Congo" ~ "DRC",
    Country == "Congo, The Democratic Republic of the" ~ "DRC",
    TRUE ~ Country
  )) %>%
  filter(Country %in% countries_to_select) %>%
  arrange(Country)

# -----------------------------
# View Results
# -----------------------------
print("First few rows of processed SSA Temperature data:")
print(head(temp_summary_by_country))

print(paste("Number of SSA countries matched:", nrow(temp_summary_by_country)))

# -----------------------------
# Save outputs
# -----------------------------
# Save as RData for modeling
save(temp_summary_by_country, file = "temp_summary.RData")
save(countries_to_select, file = "ssa_temp_countries.RData")

# Save as Excel for external review
write_xlsx(
  list(
    "Temperature_Summary" = temp_summary_by_country
  ),
  "temp_output.xlsx"
)

print("Files 'temp_summary.RData' and 'temp_output.xlsx' saved successfully!")