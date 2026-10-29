# -----------------------------
# Clear environment
# -----------------------------
rm(list = ls(all = TRUE))

# -----------------------------
# Libraries
# -----------------------------
library(readxl)
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
elev_input_file <- "../Raw_Data/Elevation.xlsx"

# Output files
elev_rdata_path <- "../R_Data/elev_summary.RData"
elev_excel_path <- "../R_Output/elev_output.xlsx"

# -----------------------------
# Load Elevation Data
# -----------------------------
elev_raw <- read_excel(elev_input_file, sheet = "Average Elevation")

# -----------------------------
# Define Sub-Saharan Africa countries
# -----------------------------
countries_to_select <- c(
  "Angola","Benin","Botswana","Burkina Faso","Burundi","Cabo Verde",
  "Cameroon","Central African Republic","Chad","Comoros",
  "DRC","Congo","Côte d'Ivoire", "Djibouti","Equatorial Guinea","Eritrea","Eswatini",
  "Ethiopia", "Gabon","Gambia","Ghana","Guinea","Guinea-Bissau","Kenya","Liberia",
  "Madagascar","Malawi","Mali","Mauritania","Mozambique","Namibia","Niger","Nigeria", 
  "Rwanda","Sao Tome and Principe","Senegal","Sierra Leone","Somalia","South Africa",
  "South Sudan", "Sudan","Tanzania","Togo","Uganda","Zambia","Zimbabwe"
)

# -----------------------------
# Clean and Filter Elevation Data
# -----------------------------
elev_summary <- elev_raw %>%
  # 1. Standardize column names for this specific sheet
  rename(elevation = Elevation) %>% 
  # 2. Fix hidden characters and trim spaces
  mutate(Country = gsub("\u00A0", " ", Country), 
         Country = trimws(Country)) %>%
  # 3. Handle standard name variations
  mutate(Country = case_when(
    Country == "Democratic Republic of Congo" ~ "DRC",
    Country == "Congo, The Democratic Republic of the" ~ "DRC",
    Country == "Cote d'Ivoire" ~ "Côte d'Ivoire",
    Country == "Swaziland" ~ "Eswatini",
    TRUE ~ Country
  )) %>%
  # 4. Filter for target list
  filter(Country %in% countries_to_select) %>%
  select(Country, elevation) %>%
  arrange(Country)

# -----------------------------
# View Results
# -----------------------------
print(paste("Total countries matched:", nrow(elev_summary)))
print(head(elev_summary))

# -----------------------------
# Create output directories if they don't exist
# -----------------------------
if (!dir.exists("../R_Data"))   dir.create("../R_Data", recursive = TRUE)
if (!dir.exists("../R_Output")) dir.create("../R_Output", recursive = TRUE)

# -----------------------------
# Save ONLY the summary outputs
# -----------------------------
# 1. Save RData containing ONLY elev_summary
save(elev_summary, file = elev_rdata_path)

# 2. Save Excel containing ONLY elev_summary
write_xlsx(
  list(
    "elev_summary" = elev_summary
  ),
  path = elev_excel_path
)

cat("\nSummary files successfully created:\n")
cat(" - RData: ", elev_rdata_path, "\n")
cat(" - Excel: ", elev_excel_path, "\n")