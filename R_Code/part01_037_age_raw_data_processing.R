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
age_data_path <- "C:/Users/17034/Downloads/GSU PhD/Prof. Kirpich/Datasets/New datasets/Malaria/Final/Additional datasets/Covariates/median-age.csv"

# Set working directory to the folder containing the CSV
setwd(dirname(age_data_path))
getwd()  # confirm

age_raw <- read_csv(age_data_path)

# -----------------------------
# Rename and clean country names
# -----------------------------
age_raw <- age_raw %>%
  rename(Country = Entity)

# Standardize country names
age_raw$Country <- gsub("Cape Verde", "Cabo Verde", age_raw$Country)
age_raw$Country <- gsub("Democratic Republic of Congo", "DRC", age_raw$Country)
age_raw$Country <- gsub("Cote d'Ivoire", "Côte d'Ivoire", age_raw$Country)

# Rename GDP column to a simpler name
age_raw <- age_raw %>%
  rename(AGE = `Median age, total`)

# -----------------------------
# Define Sub-Saharan Africa countries (including islands)
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
# Filter data (countries + years)
# -----------------------------
age_selected <- age_raw %>%
  filter(
    Country %in% countries_to_select,
    Year >= 2000,
    Year <= 2023
  )

# -----------------------------
# Reshape to wide format (countries as rows, years as columns)
# -----------------------------
age_wide <- age_selected %>%
  select(Country, Year, AGE) %>%
  pivot_wider(names_from = Year, values_from = AGE) %>%   # Years become columns
  arrange(Country)

age_data_selected_fixed <- as.data.frame(age_wide)

colnames(age_data_selected_fixed)
# -----------------------------
# Compute summary statistics by country
# -----------------------------
year_cols <- grep("^[0-9]{4}$", names(age_data_selected_fixed), value = TRUE)

age_summary_by_country <- age_data_selected_fixed %>%
  rowwise() %>%
  mutate(
    min_age = min(c_across(all_of(year_cols)), na.rm = TRUE),
    max_age = max(c_across(all_of(year_cols)), na.rm = TRUE),
    sum_age = sum(c_across(all_of(year_cols)), na.rm = TRUE),
    n = sum(!is.na(c_across(all_of(year_cols)))),
    median_age = median(c_across(all_of(year_cols)), na.rm = TRUE),
    mean_age = ifelse(n > 0, sum_age / n, NA)
  ) %>%
  ungroup() %>%
  select(Country, min_age, max_age, sum_age, n, mean_age, median_age)

# View the first few rows
head(age_summary_by_country)

# -----------------------------
# Save outputs
# -----------------------------
save(age_data_selected_fixed, file = "age_data_selected_fixed.RData")
save(countries_to_select, file = "ssa_agecountries.RData")
save(age_summary_by_country, file = "age_summary.RData")

write_xlsx(
  list(
    "GDP_Data" = age_data_selected_fixed,
    "Summary" = age_summary_by_country
  ),
  "age_output.xlsx"
)

