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
gdp_input_file <- "../Raw_Data/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_207559.csv"

# Output files
gdp_rdata_path <- "../R_Data/gdp_summary.RData"
gdp_excel_path <- "../R_Output/gdp_output.xlsx"

# -----------------------------
# Load World Bank GDP Data
# -----------------------------
gdp_raw <- read_csv(gdp_input_file, skip = 4)

# -----------------------------
# Reshape and Clean
# -----------------------------
gdp_long <- gdp_raw %>%
  rename(Country = `Country Name`) %>%
  pivot_longer(
    cols = matches("^[0-9]{4}$"), 
    names_to = "Year", 
    values_to = "GDP"
  ) %>%
  mutate(Year = as.numeric(Year))

# Standardize country names to match countries_to_select
gdp_long$Country <- gsub("Congo, Dem. Rep.", "DRC", gdp_long$Country)
gdp_long$Country <- gsub("Congo, Rep.", "Congo", gdp_long$Country)
gdp_long$Country <- gsub("Cote d'Ivoire", "Côte d'Ivoire", gdp_long$Country)
gdp_long$Country <- gsub("Somalia, Fed. Rep.", "Somalia", gdp_long$Country)
gdp_long$Country <- gsub("Gambia, The", "Gambia", gdp_long$Country)

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
gdp_data_selected_fixed <- gdp_long %>%
  filter(
    Country %in% countries_to_select,
    Year >= 2000,
    Year <= 2023
  ) %>%
  select(Country, Year, GDP) %>%
  pivot_wider(names_from = Year, values_from = GDP) %>%
  arrange(Country)

# -----------------------------
# Compute Summary Statistics by Country (Mean & Median)
# -----------------------------
year_cols <- grep("^[0-9]{4}$", names(gdp_data_selected_fixed), value = TRUE)

gdp_summary <- gdp_data_selected_fixed %>%
  rowwise() %>%
  mutate(
    min_gdp    = round(min(c_across(all_of(year_cols)), na.rm = TRUE), 3),
    max_gdp    = round(max(c_across(all_of(year_cols)), na.rm = TRUE), 3),
    n_years    = sum(!is.na(c_across(all_of(year_cols)))),
    mean_gdp   = round(mean(c_across(all_of(year_cols)), na.rm = TRUE), 3),
    median_gdp = round(median(c_across(all_of(year_cols)), na.rm = TRUE), 3)
  ) %>%
  ungroup() %>%
  select(Country, n_years, min_gdp, max_gdp, mean_gdp, median_gdp)

# View the summary output
print(head(gdp_summary))

# -----------------------------
# Create output directories if they don't exist
# -----------------------------
if (!dir.exists("../R_Data"))   dir.create("../R_Data", recursive = TRUE)
if (!dir.exists("../R_Output")) dir.create("../R_Output", recursive = TRUE)

# -----------------------------
# Save ONLY the summary outputs
# -----------------------------
# 1. Save RData containing ONLY gdp_summary
save(gdp_summary, file = gdp_rdata_path)

# 2. Save Excel containing ONLY gdp_summary
write_xlsx(
  list(
    "gdp_summary" = gdp_summary
  ),
  path = gdp_excel_path
)

cat("\nSummary files successfully created:\n")
cat(" - RData: ", gdp_rdata_path, "\n")
cat(" - Excel: ", gdp_excel_path, "\n")