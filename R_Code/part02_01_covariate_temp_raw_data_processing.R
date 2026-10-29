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
temp_input_file <- "../Raw_Data/average-annual-surface-temperature.csv"

# Output files
temp_rdata_path <- "../R_Data/temp_summary.RData"
temp_excel_path <- "../R_Output/temp_output.xlsx"

# -----------------------------
# Load Raw Temperature CSV
# -----------------------------
temp_raw <- read.csv(temp_input_file, fileEncoding = "UTF-8", stringsAsFactors = FALSE)

# Rename Entity to Country directly
temp_raw <- temp_raw %>%
  rename(Country = Entity)

# Standardize column names to lowercase for consistency
names(temp_raw) <- tolower(names(temp_raw))

# Dynamically identify key columns
country_col <- "country"
year_col    <- names(temp_raw)[grep("year", names(temp_raw))][1]
temp_col    <- names(temp_raw)[grep("temp|value|surface", names(temp_raw))][1]

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
# Clean, Filter (2000-2023), and Summarize (Mean & Median)
# -----------------------------
temp_filtered <- temp_raw %>%
  rename(
    country     = all_of(country_col),
    year        = all_of(year_col),
    temperature = all_of(temp_col)
  ) %>%
  mutate(
    year = as.numeric(year),
    temperature = as.numeric(temperature),
    country = case_when(
      country == "Cote d'Ivoire" ~ "Côte d'Ivoire",
      country == "Cape Verde" ~ "Cabo Verde",
      country == "Democratic Republic of Congo" ~ "DRC",
      TRUE ~ country
    )
  ) %>%
  filter(country %in% countries_to_select) %>%
  filter(year >= 2000 & year <= 2023) %>%
  filter(!is.na(temperature))

# Calculate Mean and Median surface temperature (2000-2023) into `temp_summary`
temp_summary <- temp_filtered %>%
  group_by(country) %>%
  summarise(
    start_year       = min(year),
    end_year         = max(year),
    n_years          = n(),
    mean_temperature = round(mean(temperature, na.rm = TRUE), 3),
    median_temp      = round(median(temperature, na.rm = TRUE), 3),
    .groups          = "drop"
  ) %>%
  arrange(country)

# -----------------------------
# View Results
# -----------------------------
print("First few rows of processed SSA Temperature summary (temp_summary):")
print(head(temp_summary))

print(paste("Number of SSA countries summarized:", nrow(temp_summary)))

# -----------------------------
# Create output directories
# -----------------------------
if (!dir.exists("../R_Data")) {
  dir.create("../R_Data", recursive = TRUE)
}

if (!dir.exists("../R_Output")) {
  dir.create("../R_Output", recursive = TRUE)
}

# -----------------------------
# Save outputs
# -----------------------------

# 1. Save RData object `temp_summary`
save(
  temp_summary,
  file = temp_rdata_path
)

# 2. Save Excel file `temp_output.xlsx`
write_xlsx(
  list(
    "temp_output" = temp_summary
  ),
  path = temp_excel_path
)

# -----------------------------
# Verify output files
# -----------------------------
cat("\nFiles saved successfully:\n")

cat(
  "RData (temp_summary): ",
  normalizePath(
    temp_rdata_path,
    mustWork = TRUE
  ),
  "\n",
  sep = ""
)

cat(
  "Excel (temp_output.xlsx): ",
  normalizePath(
    temp_excel_path,
    mustWork = TRUE
  ),
  "\n",
  sep = ""
)