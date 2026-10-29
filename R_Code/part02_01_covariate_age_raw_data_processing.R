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
age_input_file <- "../Raw_Data/median-age.csv"

# Output files
age_rdata_path <- "../R_Data/age_summary.RData"
age_excel_path <- "../R_Output/age_output.xlsx"

# -----------------------------
# Load Median Age Data
# -----------------------------
age_raw <- read_csv(age_input_file)

# Rename Entity to Country directly
age_raw <- age_raw %>%
  rename(Country = Entity)

# Standardize country names
age_raw$Country <- gsub("Cape Verde", "Cabo Verde", age_raw$Country)
age_raw$Country <- gsub("Democratic Republic of Congo", "DRC", age_raw$Country)
age_raw$Country <- gsub("Cote d'Ivoire", "Côte d'Ivoire", age_raw$Country)

# Rename age value column to AGE
age_val_col <- names(age_raw)[grep("age", names(age_raw), ignore.case = TRUE)][1]

age_raw <- age_raw %>%
  rename(AGE = all_of(age_val_col))

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
# Filter Data (Countries + Years 2000-2023)
# -----------------------------
age_selected <- age_raw %>%
  filter(
    Country %in% countries_to_select,
    Year >= 2000,
    Year <= 2023
  ) %>%
  filter(!is.na(AGE))

# -----------------------------
# Reshape to Wide Format (Countries as Rows, Years as Columns)
# -----------------------------
age_wide <- age_selected %>%
  select(Country, Year, AGE) %>%
  pivot_wider(names_from = Year, values_from = AGE) %>%
  arrange(Country)

age_data_selected_fixed <- as.data.frame(age_wide)

# -----------------------------
# Compute Summary Statistics by Country (Mean & Median)
# -----------------------------
year_cols <- grep("^[0-9]{4}$", names(age_data_selected_fixed), value = TRUE)

age_summary <- age_data_selected_fixed %>%
  rowwise() %>%
  mutate(
    min_age    = round(min(c_across(all_of(year_cols)), na.rm = TRUE), 3),
    max_age    = round(max(c_across(all_of(year_cols)), na.rm = TRUE), 3),
    n_years    = sum(!is.na(c_across(all_of(year_cols)))),
    mean_age   = round(mean(c_across(all_of(year_cols)), na.rm = TRUE), 3),
    median_age = round(median(c_across(all_of(year_cols)), na.rm = TRUE), 3)
  ) %>%
  ungroup() %>%
  select(Country, n_years, min_age, max_age, mean_age, median_age)

# -----------------------------
# Create output directories if they don't exist
# -----------------------------
if (!dir.exists("../R_Data"))   dir.create("../R_Data", recursive = TRUE)
if (!dir.exists("../R_Output")) dir.create("../R_Output", recursive = TRUE)

# -----------------------------
# Save ONLY the summary outputs
# -----------------------------
# 1. Save RData containing ONLY age_summary
save(age_summary, file = age_rdata_path)

# 2. Save Excel containing ONLY age_summary
write_xlsx(
  list(
    "age_summary" = age_summary
  ),
  path = age_excel_path
)

cat("\nSummary files successfully created:\n")
cat(" - RData: ", age_rdata_path, "\n")
cat(" - Excel: ", age_excel_path, "\n")