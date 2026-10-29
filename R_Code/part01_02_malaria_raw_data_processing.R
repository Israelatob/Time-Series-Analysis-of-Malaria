options(scipen = 999)
rm(list=ls(all=TRUE))

# Load required libraries
library(matrixStats)
library(xtable)
library(Matrix)
library(methods)
library(readxl)
library(rjson)

# -----------------------------
# Step 3: Set working directory
# -----------------------------
setwd(
  "C:/Users/17034/Downloads/GSU PhD/Prof. Kirpich/Datasets/GitHub Files/Time-Series-Analysis-of-Malaria/R_Code"
)

cat("Working directory:", getwd(), "\n")


# -----------------------------
# Step 4: Define file paths
# -----------------------------

# Input Excel workbook: R_Output folder
malaria_data_path <- "../R_Output/Malaria_TimeSeries_Bycountry.xlsx"

# Output consolidated dataset: R_Data folder
malaria_rdata_path <- "../R_Data/malaria_data_all.RData"


# -----------------------------
#: Verify input file
# -----------------------------
if (!file.exists(malaria_data_path)) {
  stop(
    "Input Excel file not found: ",
    normalizePath(malaria_data_path, mustWork = FALSE),
    "\nCheck that the previous script generated the workbook in R_Output."
  )
}


# -----------------------------
# Create output directory
# -----------------------------
rdata_dir <- dirname(malaria_rdata_path)

if (!dir.exists(rdata_dir)) {
  dir.create(
    rdata_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
}



# Read all sheets from the Excel file
sheet_names <- excel_sheets(malaria_data_path)

# Function to read and process each country's data
read_country_data <- function(sheet_name) {
  data <- read_excel(malaria_data_path, sheet = sheet_name)
  data$date <- as.Date(data$date)
  data$country <- sheet_name
  return(data)
}

# Read all countries' data
malaria_data_list <- lapply(sheet_names, read_country_data)
malaria_data_all <- do.call(rbind, malaria_data_list)

# Convert to data frame and ensure proper data types
malaria_data_all <- as.data.frame(malaria_data_all)
malaria_data_all$date <- as.Date(malaria_data_all$date)
malaria_data_all$year <- as.numeric(malaria_data_all$year)
malaria_data_all$population <- as.numeric(malaria_data_all$population)
malaria_data_all$cases <- as.numeric(malaria_data_all$cases)
malaria_data_all$deaths <- as.numeric(malaria_data_all$deaths)

# Save the complete dataset
save(malaria_data_all, file = malaria_rdata_path)