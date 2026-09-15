#-----------------------------
#Clear environment
#-----------------------------
rm(list = ls(all = TRUE))


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
rain_data_path <- "C:/Users/17034/Downloads/GSU PhD/Prof. Kirpich/Datasets/New datasets/Malaria/Final/Additional datasets/Covariates/Average Annual Rainfall.csv"

# Set working directory to the folder containing the CSV
setwd(dirname(rain_data_path))
getwd()  # confirm

# Read the CSV (skip=4 is standard for World Bank files)
rain_raw <- read_csv(rain_data_path, skip = 4)

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

# Standardize country names to match your selection list
rain_long$Country <- gsub("Congo, Dem. Rep.", "DRC", rain_long$Country)
rain_long$Country <- gsub("Congo, Rep.", "Congo", rain_long$Country)
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
# Filter and Reshape back to Wide (This fulfills your 2000-2023 request)
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
# Compute summary statistics
# -----------------------------
year_cols <- grep("^[0-9]{4}$", names(rain_data_selected_fixed), value = TRUE)

rain_summary_by_country <- rain_data_selected_fixed %>%
  rowwise() %>%
  mutate(
    min_rain = min(c_across(all_of(year_cols)), na.rm = TRUE),
    max_rain = max(c_across(all_of(year_cols)), na.rm = TRUE),
    sum_rain = sum(c_across(all_of(year_cols)), na.rm = TRUE),
    n = sum(!is.na(c_across(all_of(year_cols)))),
    median_rain = median(c_across(all_of(year_cols)), na.rm = TRUE),
    mean_rain = ifelse(n > 0, sum_rain / n, NA)
  ) %>%
  ungroup() %>%
  select(Country, min_rain, max_rain, sum_rain, n, mean_rain, median_rain)


# -----------------------------
# Save outputs
# -----------------------------
save(rain_data_selected_fixed, file = "rain_data_selected_fixed.RData")
save(countries_to_select, file = "ssa_raincountries.RData")
save(rain_summary_by_country, file = "rain_summary.RData")

write_xlsx(
  list(
    "Rainfall_Data" = rain_data_selected_fixed,
    "Summary" = rain_summary_by_country
  ),
  "rain_output.xlsx"
)

