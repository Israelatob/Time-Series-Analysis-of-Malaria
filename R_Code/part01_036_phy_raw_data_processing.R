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
phy_data_path <- "C:/Users/17034/Downloads/GSU PhD/Prof. Kirpich/Datasets/New datasets/Malaria/Final/Additional datasets/Covariates/API_SH.MED.PHYS.ZS_DS2_en_csv_v2_1914.csv"

# Set working directory to the folder containing the CSV
setwd(dirname(phy_data_path))
getwd()  # confirm

# Read the CSV (skip=4 is standard for World Bank files)
phy_raw <- read_csv(phy_data_path, skip = 4)

# -----------------------------
# Reshape and Clean
# -----------------------------
phy_long <- phy_raw %>%
  rename(Country = `Country Name`) %>%
  pivot_longer(
    cols = matches("^[0-9]{4}$"), 
    names_to = "Year", 
    values_to = "Physician"
  ) %>%
  mutate(Year = as.numeric(Year))

# Standardize country names to match your selection list
phy_long$Country <- gsub("Congo, Dem. Rep.", "DRC", phy_long$Country)
phy_long$Country <- gsub("Congo, Rep.", "Congo", phy_long$Country)
phy_long$Country <- gsub("Cape Verde", "Cabo Verde", phy_long$Country)
phy_long$Country <- gsub("Cote d'Ivoire", "Côte d'Ivoire", phy_long$Country)
phy_long$Country <- gsub("Somalia, Fed. Rep.", "Somalia", phy_long$Country)
phy_long$Country <- gsub("Gambia, The", "Gambia", phy_long$Country)

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
phy_data_selected_fixed <- phy_long %>%
  filter(
    Country %in% countries_to_select,
    Year >= 2000,
    Year <= 2023
  ) %>%
  select(Country, Year, Physician) %>%
  pivot_wider(names_from = Year, values_from = Physician) %>%
  arrange(Country)

# -----------------------------
# Compute summary statistics
# -----------------------------
year_cols <- grep("^[0-9]{4}$", names(phy_data_selected_fixed), value = TRUE)

phy_summary_by_country <- phy_data_selected_fixed %>%
  rowwise() %>%
  mutate(
    min_phy= min(c_across(all_of(year_cols)), na.rm = TRUE),
    max_phy = max(c_across(all_of(year_cols)), na.rm = TRUE),
    sum_phy = sum(c_across(all_of(year_cols)), na.rm = TRUE),
    n = sum(!is.na(c_across(all_of(year_cols)))),
    median_phy = median(c_across(all_of(year_cols)), na.rm = TRUE),
    mean_phy = ifelse(n > 0, sum_phy / n, NA)
  ) %>%
  ungroup() %>%
    select(Country, min_phy, max_phy, sum_phy, n, mean_phy, median_phy)



# View the first few rows
head(phy_summary_by_country)

# -----------------------------
# Save outputs
# -----------------------------
save(phy_data_selected_fixed, file = "phy_data_selected_fixed.RData")
save(countries_to_select, file = "ssa_phycountries.RData")
save(phy_summary_by_country, file = "phy_summary.RData")

write_xlsx(
  list(
    "Physician_Data" = phy_data_selected_fixed,
    "Summary" = phy_summary_by_country
  ),
  "phy_output.xlsx"
)