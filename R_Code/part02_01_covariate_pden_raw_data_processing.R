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
den_input_file <- "../Raw_Data/population-density.csv"

# Output files
den_rdata_path <- "../R_Data/den_summary.RData"
den_excel_path <- "../R_Output/den_output.xlsx"

# -----------------------------
# Load Population Density Data
# -----------------------------
den_raw <- read_csv(den_input_file)

# Rename Entity to Country directly
den_raw <- den_raw %>%
  rename(Country = Entity)

# Standardize country names to match countries_to_select
den_raw$Country <- gsub("Cape Verde", "Cabo Verde", den_raw$Country)
den_raw$Country <- gsub("Democratic Republic of Congo", "DRC", den_raw$Country)
den_raw$Country <- gsub("Cote d'Ivoire", "Côte d'Ivoire", den_raw$Country)

# Identify and rename population density column
den_val_col <- names(den_raw)[grep("density", names(den_raw), ignore.case = TRUE)][1]

den_raw <- den_raw %>%
  rename(Density = all_of(den_val_col))

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
den_selected <- den_raw %>%
  filter(
    Country %in% countries_to_select,
    Year >= 2000,
    Year <= 2023
  ) %>%
  filter(!is.na(Density))

den_wide <- den_selected %>%
  select(Country, Year, Density) %>%
  pivot_wider(names_from = Year, values_from = Density) %>%
  arrange(Country)

den_data_selected_fixed <- as.data.frame(den_wide)

# -----------------------------
# Compute Summary Statistics by Country (Mean & Median)
# -----------------------------
year_cols <- grep("^[0-9]{4}$", names(den_data_selected_fixed), value = TRUE)

den_summary <- den_data_selected_fixed %>%
  rowwise() %>%
  mutate(
    min_den    = round(min(c_across(all_of(year_cols)), na.rm = TRUE), 3),
    max_den    = round(max(c_across(all_of(year_cols)), na.rm = TRUE), 3),
    n_years    = sum(!is.na(c_across(all_of(year_cols)))),
    mean_den   = round(mean(c_across(all_of(year_cols)), na.rm = TRUE), 3),
    median_den = round(median(c_across(all_of(year_cols)), na.rm = TRUE), 3)
  ) %>%
  ungroup() %>%
  select(Country, n_years, min_den, max_den, mean_den, median_den)

# View summary preview
print(head(den_summary))

# -----------------------------
# Create output directories if they don't exist
# -----------------------------
if (!dir.exists("../R_Data"))   dir.create("../R_Data", recursive = TRUE)
if (!dir.exists("../R_Output")) dir.create("../R_Output", recursive = TRUE)

# -----------------------------
# Save ONLY the summary outputs
# -----------------------------
# 1. Save RData containing ONLY den_summary
save(den_summary, file = den_rdata_path)

# 2. Save Excel containing ONLY den_summary
write_xlsx(
  list(
    "den_summary" = den_summary
  ),
  path = den_excel_path
)

cat("\nSummary files successfully created:\n")
cat(" - RData: ", den_rdata_path, "\n")
cat(" - Excel: ", den_excel_path, "\n")