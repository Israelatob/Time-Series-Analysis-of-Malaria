# # -----------------------------
# # Clear environment
# # -----------------------------
# rm(list = ls(all = TRUE))
# 
# # -----------------------------
# # Libraries
# # -----------------------------
# library(readr)
# library(dplyr)
# library(tidyr)
# library(writexl)
# 
# # -----------------------------
# # Load GDP data
# # -----------------------------
# gdp_data_path <- "C:/Users/17034/Downloads/GSU PhD/Prof. Kirpich/Datasets/New datasets/Malaria/Final/Additional datasets/Covariates/gdp-per-capita-worldbank.csv"
# 
# # Set working directory to the folder containing the CSV
# setwd(dirname(gdp_data_path))
# getwd()  # confirm
# 
# gdp_raw <- read_csv(gdp_data_path)
# 
# # -----------------------------
# # Rename and clean country names
# # -----------------------------
# gdp_raw <- gdp_raw %>%
#   rename(Country = Entity)
# 
# # Standardize country names
# gdp_raw$Country <- gsub("Cape Verde", "Cabo Verde", gdp_raw$Country)
# gdp_raw$Country <- gsub("Democratic Republic of Congo", "DRC", gdp_raw$Country)
# # gdp_raw$Country <- gsub("Republic Congo", "Congo", gdp_raw$Country)
# 
# # Rename GDP column to a simpler name
# gdp_raw <- gdp_raw %>%
#   rename(GDP = `GDP per capita`)
# 
# # -----------------------------
# # Define Sub-Saharan Africa countries (including islands)
# # -----------------------------
# countries_to_select <- c(
#   "Angola","Benin","Botswana","Burkina Faso","Burundi","Cabo Verde",
#   "Cameroon","Central African Republic","Chad","Comoros",
#   "DRC","Congo","Cote d'Ivoire", "Djibouti","Equatorial Guinea","Eritrea","Eswatini","Ethiopia",
#   "Gabon","Gambia","Ghana","Guinea","Guinea-Bissau",
#   "Kenya","Liberia","Madagascar","Malawi","Mali",
#   "Mauritania","Mozambique","Namibia","Niger","Nigeria",
#   "Rwanda","Sao Tome and Principe","Senegal",
#   "Sierra Leone","Somalia","South Africa","South Sudan",
#   "Sudan","Tanzania","Togo","Uganda","Zambia","Zimbabwe"
# )
# 
# # -----------------------------
# # Filter data (countries + years)
# # -----------------------------
# gdp_selected <- gdp_raw %>%
#   filter(
#     Country %in% countries_to_select,
#     Year >= 2000,
#     Year <= 2023
#   )
# 
# # -----------------------------
# # Reshape to wide format (countries as rows, years as columns)
# # -----------------------------
# gdp_wide <- gdp_selected %>%
#   select(Country, Year, GDP) %>%
#   pivot_wider(names_from = Year, values_from = GDP) %>%   # Years become columns
#   arrange(Country)
# 
# gdp_data_selected_fixed <- as.data.frame(gdp_wide)
# 
# 
# # -----------------------------
# # Compute summary statistics by country
# # -----------------------------
# year_cols <- grep("^[0-9]{4}$", names(gdp_data_selected_fixed), value = TRUE)
# 
# gdp_summary_by_country <- gdp_data_selected_fixed %>%
#   rowwise() %>%
#   mutate(
#     MIN_GDP = min(c_across(all_of(year_cols)), na.rm = TRUE),
#     MAX_GDP = max(c_across(all_of(year_cols)), na.rm = TRUE),
#     SUM_GDP = sum(c_across(all_of(year_cols)), na.rm = TRUE),
#     N = sum(!is.na(c_across(all_of(year_cols)))),
#     MEAN_GDP = ifelse(N > 0, SUM_GDP / N, NA)
#   ) %>%
#   ungroup() %>%
#   select(Country, MIN_GDP, MAX_GDP, SUM_GDP, N, MEAN_GDP)
# 
# 
# # View the first few rows
# head(gdp_summary_by_country)
# 
# # -----------------------------
# # Save outputs
# # -----------------------------
# save(gdp_data_selected_fixed, file = "gdp_data_selected_fixed.RData")
# save(countries_to_select, file = "ssa_gdp_countries.RData")
# 
# write_xlsx(
#   list(
#     "GDP_Data" = gdp_data_selected_fixed,
#     "Summary" = gdp_summary_by_country
#   ),
#   "gdp_output.xlsx"
# )
# 
# # Confirm working directory
# getwd()
# 
# API_NY.GDP.PCAP.CD_DS2_en_csv_v2_207559

# -----------------------------
# Libraries
# -----------------------------
library(readr)
library(dplyr)
library(tidyr)
library(writexl)

# -----------------------------
# Load Data
# -----------------------------
gdp_data_path <- "C:/Users/17034/Downloads/GSU PhD/Prof. Kirpich/Datasets/New datasets/Malaria/Final/Additional datasets/Covariates/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_207559.csv"

# Set working directory to the folder containing the CSV
setwd(dirname(gdp_data_path))
getwd()  # confirm

# Read the CSV (skip=4 is standard for World Bank files)
gdp_raw <- read_csv(gdp_data_path, skip = 4)

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

# Standardize country names to match your selection list
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
# Filter and Reshape back to Wide (This fulfills your 2000-2023 request)
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
# Compute summary statistics
# -----------------------------
year_cols <- grep("^[0-9]{4}$", names(gdp_data_selected_fixed), value = TRUE)

gdp_summary_by_country <- gdp_data_selected_fixed %>%
  rowwise() %>%
  mutate(
    min_gdp = min(c_across(all_of(year_cols)), na.rm = TRUE),
    max_gdp = max(c_across(all_of(year_cols)), na.rm = TRUE),
    sum_gdp = sum(c_across(all_of(year_cols)), na.rm = TRUE),
    n = sum(!is.na(c_across(all_of(year_cols)))),
    median_gdp = median(c_across(all_of(year_cols)), na.rm = TRUE),
    mean_gdp = ifelse(n > 0, sum_gdp / n, NA)
  ) %>%
  ungroup() %>%
  select(Country, min_gdp, max_gdp, sum_gdp, n, mean_gdp, median_gdp)



# View the first few rows
head(gdp_summary_by_country)

# -----------------------------
# Save outputs
# -----------------------------
save(gdp_data_selected_fixed, file = "gdp_data_selected_fixed.RData")
save(countries_to_select, file = "ssa_gdpcountries.RData")
save(gdp_summary_by_country, file = "gdp_summary.RData")

write_xlsx(
  list(
    "Physician_Data" = gdp_data_selected_fixed,
    "Summary" = gdp_summary_by_country
  ),
  "gdp_output.xlsx"
)
