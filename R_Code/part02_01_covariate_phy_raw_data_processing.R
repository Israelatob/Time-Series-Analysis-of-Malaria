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
phy_input_file <- "../Raw_Data/API_SH.MED.PHYS.ZS_DS2_en_csv_v2_1914.csv"

# Output files
phy_rdata_path <- "../R_Data/phy_summary.RData"
phy_excel_path <- "../R_Output/phy_output.xlsx"

# -----------------------------
# Load World Bank Physician Data
# -----------------------------
phy_raw <- read_csv(phy_input_file, skip = 4)

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

# Standardize country names to match countries_to_select
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
# Filter (2000-2023) and Reshape to Wide
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
# Compute Summary Statistics by Country (Mean & Median)
# -----------------------------
year_cols <- grep("^[0-9]{4}$", names(phy_data_selected_fixed), value = TRUE)

phy_summary <- phy_data_selected_fixed %>%
  rowwise() %>%
  mutate(
    min_phy    = round(min(c_across(all_of(year_cols)), na.rm = TRUE), 3),
    max_phy    = round(max(c_across(all_of(year_cols)), na.rm = TRUE), 3),
    n_years    = sum(!is.na(c_across(all_of(year_cols)))),
    mean_phy   = round(mean(c_across(all_of(year_cols)), na.rm = TRUE), 3),
    median_phy = round(median(c_across(all_of(year_cols)), na.rm = TRUE), 3)
  ) %>%
  ungroup() %>%
  select(Country, n_years, min_phy, max_phy, mean_phy, median_phy)

# View the summary output
print(head(phy_summary))

# -----------------------------
# Create output directories if they don't exist
# -----------------------------
if (!dir.exists("../R_Data"))   dir.create("../R_Data", recursive = TRUE)
if (!dir.exists("../R_Output")) dir.create("../R_Output", recursive = TRUE)

# -----------------------------
# Save ONLY the summary outputs
# -----------------------------
# 1. Save RData containing ONLY phy_summary
save(phy_summary, file = phy_rdata_path)

# 2. Save Excel containing ONLY phy_summary
write_xlsx(
  list(
    "phy_summary" = phy_summary
  ),
  path = phy_excel_path
)

cat("\nSummary files successfully created:\n")
cat(" - RData: ", phy_rdata_path, "\n")
cat(" - Excel: ", phy_excel_path, "\n")