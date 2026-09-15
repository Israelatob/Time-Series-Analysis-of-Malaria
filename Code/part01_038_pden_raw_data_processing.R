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
# Load GDP data
# -----------------------------
den_data_path <- "C:/Users/17034/Downloads/GSU PhD/Prof. Kirpich/Datasets/New datasets/Malaria/Final/Additional datasets/Covariates/population-density.csv"

# Set working directory to the folder containing the CSV
setwd(dirname(den_data_path))
getwd()  # confirm

den_raw <- read_csv(den_data_path)

# -----------------------------
# Rename and clean country names
# -----------------------------
den_raw <- den_raw %>%
  rename(Country = Entity)

# Standardize country names
den_raw$Country <- gsub("Cape Verde", "Cabo Verde", den_raw$Country)
den_raw$Country <- gsub("Democratic Republic of Congo", "DRC", den_raw$Country)
den_raw$Country <- gsub("Cote d'Ivoire", "Côte d'Ivoire", den_raw$Country)

# Rename GDP column to a simpler name
den_raw <- den_raw %>%
  rename(Density = `Population density`)

# -----------------------------
# Define Sub-Saharan Africa countries (including islands)
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
# Filter data (countries + years)
# -----------------------------
den_selected <- den_raw %>%
  filter(
    Country %in% countries_to_select,
    Year >= 2000,
    Year <= 2023
  )

# -----------------------------
# Reshape to wide format (countries as rows, years as columns)
# -----------------------------
den_wide <- den_selected %>%
  select(Country, Year, Density) %>%
  pivot_wider(names_from = Year, values_from = Density) %>%   # Years become columns
  arrange(Country)

den_data_selected_fixed <- as.data.frame(den_wide)


# -----------------------------
# Compute summary statistics by country
# -----------------------------
year_cols <- grep("^[0-9]{4}$", names(den_data_selected_fixed), value = TRUE)

den_summary_by_country <- den_data_selected_fixed %>%
  rowwise() %>%
  mutate(
    min_den = min(c_across(all_of(year_cols)), na.rm = TRUE),
    max_den = max(c_across(all_of(year_cols)), na.rm = TRUE),
    sum_den = sum(c_across(all_of(year_cols)), na.rm = TRUE),
    n = sum(!is.na(c_across(all_of(year_cols)))),
    median_den = median(c_across(all_of(year_cols)), na.rm = TRUE),
    mean_den = ifelse(n > 0, sum_den / n, NA)
  ) %>%
  ungroup() %>%
  select(Country, min_den, max_den, sum_den, n, mean_den, median_den)

# View the first few rows
head(den_summary_by_country)

# -----------------------------
# Save outputs
# -----------------------------
save(den_data_selected_fixed, file = "den_data_selected_fixed.RData")
save(countries_to_select, file = "ssa_den_countries.RData")
save(den_summary_by_country, file = "den_summary.RData")

write_xlsx(
  list(
    "Density_Data" = den_data_selected_fixed,
    "Summary" = den_summary_by_country
  ),
  "den_output.xlsx"
)

