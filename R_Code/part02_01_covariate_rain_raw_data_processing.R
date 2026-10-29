# -----------------------------
# Clear environment
# -----------------------------
rm(list = ls(all = TRUE))

# -----------------------------
# Libraries
# -----------------------------
library(readr)
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
rain_input_file <- "../Raw_Data/Average Annual Rainfall.csv"

# Output files
rain_rdata_path <- "../R_Data/rain_summary.RData"
rain_excel_path <- "../R_Output/rain_output.xlsx"

# -----------------------------
# Load Rainfall Data
# -----------------------------
rain_raw <- read_csv(rain_input_file, skip = 4)

# -----------------------------
# Reshape and Clean
# -----------------------------
rain_long <- rain_raw %>%
  rename(Country = `Country Name`) %>%
  pivot_longer(
    cols = matches("^[0-9]{4}$"), 
    names_to = "Year", 
    values_to = "RAIN"
  ) %>%
  mutate(Year = as.numeric(Year))

# Standardize country names to match countries_to_select
rain_long$Country <- gsub("Congo, Dem. Rep.", "DRC", rain_long$Country)
rain_long$Country <- gsub("Congo, Rep.", "Congo", rain_long$Country)
rain_long$Country <- gsub("Cape Verde", "Cabo Verde", rain_long$Country)
rain_long$Country <- gsub("Cote d'Ivoire", "Côte d'Ivoire", rain_long$Country)
rain_long$Country <- gsub("Somalia, Fed. Rep.", "Somalia", rain_long$Country)
rain_long$Country <- gsub("Gambia, The", "Gambia", rain_long$Country)

# -----------------------------
# Define Sub-Saharan Africa countries
# -----------------------------
countries_to_select <- c(
  "Angola","Benin","Botswana","Burkina Faso","Burundi","Cabo Verde",
  "Cameroon","Central African Republic","Chad","Comoros",
  "DRC","Congo","Côte d'Ivoire", "Djibouti","Equatorial Guinea","Eritrea","Eswatini","Ethiopia",
  "Gabon","Gambia","Ghana","Guinea","Guinea-Bissau",
  "Kenya","Liberia","Madagascar","Malawi","Mali",
  "Mauritania","Mozambique","Namibia","Niger","Nigeria",
  "Rwanda","Sao Tome and Principe","Senegal",
  "Sierra Leone","Somalia","South Africa","South Sudan",
  "Sudan","Tanzania","Togo","Uganda","Zambia","Zimbabwe"
)

# -----------------------------
# Filter (2000-2023) and Reshape to Wide
# -----------------------------
rain_data_selected_fixed <- rain_long %>%
  filter(
    Country %in% countries_to_select,
    Year >= 2000,
    Year <= 2022
  ) %>%
  select(Country, Year, RAIN) %>%
  pivot_wider(names_from = Year, values_from = RAIN) %>%
  arrange(Country)

# -----------------------------
# Compute Summary Statistics by Country (Mean & Median)
# -----------------------------
year_cols <- grep("^[0-9]{4}$", names(rain_data_selected_fixed), value = TRUE)

rain_summary <- rain_data_selected_fixed %>%
  rowwise() %>%
  mutate(
    min_rain    = round(min(c_across(all_of(year_cols)), na.rm = TRUE), 3),
    max_rain    = round(max(c_across(all_of(year_cols)), na.rm = TRUE), 3),
    n_years     = sum(!is.na(c_across(all_of(year_cols)))),
    mean_rain   = round(mean(c_across(all_of(year_cols)), na.rm = TRUE), 3),
    median_rain = round(median(c_across(all_of(year_cols)), na.rm = TRUE), 3)
  ) %>%
  ungroup() %>%
  select(Country, n_years, min_rain, max_rain, mean_rain, median_rain)

# View the summary output
print(head(rain_summary))

# -----------------------------
# Create output directories if they don't exist
# -----------------------------
if (!dir.exists("../R_Data"))   dir.create("../R_Data", recursive = TRUE)
if (!dir.exists("../R_Output")) dir.create("../R_Output", recursive = TRUE)

# -----------------------------
# Save ONLY the summary outputs
# -----------------------------
# 1. Save RData containing ONLY rain_summary
save(rain_summary, file = rain_rdata_path)

# 2. Save Excel containing ONLY rain_summary
write_xlsx(
  list(
    "rain_summary" = rain_summary
  ),
  path = rain_excel_path
)

cat("\nSummary files successfully created:\n")
cat(" - RData: ", rain_rdata_path, "\n")
cat(" - Excel: ", rain_excel_path, "\n")