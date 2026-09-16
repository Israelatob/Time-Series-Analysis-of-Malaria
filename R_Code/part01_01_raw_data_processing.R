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
library(openxlsx)
library(stringr)
library(lubridate)

# -----------------------------
# Load data
# -----------------------------
mal_data_path <- "C:/Users/17034/Downloads/GSU PhD/Prof. Kirpich/Datasets/New datasets/Malaria/Final/Latest Malaria dataset/wmr2024_annex_4f.xlsx"
setwd(dirname(mal_data_path))

mal_raw <- read_excel(mal_data_path, skip = 2)

# -----------------------------
# Step 1: Fix Sparse Columns and Clean Footnotes
# -----------------------------
mal_clean <- mal_raw %>%
  rename(country = `WHO region`) %>%
  fill(country, .direction = "down") %>%
  mutate(country = str_remove_all(country, "[0-9,]+")) %>%
  mutate(country = str_trim(country))

# =========================================================
# STEP 2: Detect structure and reshape (Standardizing to Lowercase)
# =========================================================
# Check if "Year" (uppercase) exists in raw names to determine format
has_year_column <- "Year" %in% names(mal_clean)

if (has_year_column) {
  cat("Detected LONG format\n")
  mal_long <- mal_clean %>%
    # Rename specifically what we need, then force everything to lower
    rename(cases = `Cases Point`, deaths = `Deaths Point`, population = Population, year = Year) %>%
    rename_with(tolower) %>%
    mutate(
      year = as.numeric(year),
      cases = as.numeric(gsub("[^0-9.]", "", as.character(cases))),
      deaths = as.numeric(gsub("[^0-9.]", "", as.character(deaths))),
      population = as.numeric(gsub("[^0-9.]", "", as.character(population)))
    ) %>%
    select(country, year, cases, deaths, population)
  
} else {
  cat("Detected WIDE format\n")
  mal_long <- mal_clean %>%
    pivot_longer(
      cols = matches("Cases|Deaths|Population"),
      names_to = "variable",
      values_to = "value"
    ) %>%
    mutate(
      year = as.numeric(str_extract(variable, "\\d{4}")),
      measure = case_when(
        str_detect(variable, "Cases") ~ "cases",
        str_detect(variable, "Deaths") ~ "deaths",
        str_detect(variable, "Population") ~ "population"
      )
    ) %>%
    filter(!is.na(year)) %>%
    select(country, year, measure, value) %>%
    pivot_wider(names_from = measure, values_from = value) %>%
    rename_with(tolower) # Final insurance to make all lower
}

# -----------------------------
# Manual Country Fixes (using lowercase 'country')
# -----------------------------
mal_long <- mal_long %>%
  mutate(country = case_when(
    str_detect(country, "Democratic Republic of the Congo") ~ "DRC",
    str_detect(country, "Côte d’Ivoire") ~ "Côte d'Ivoire",
    str_detect(country, "United Republic of Tanzania") ~ "Tanzania",
    TRUE ~ country
  ))

# -----------------------------
# Sub-Saharan Africa countries list
# -----------------------------
countries_to_select <- c("Angola","Benin","Botswana","Burkina Faso","Burundi","Cabo Verde",
                         "Cameroon","Central African Republic","Chad","Comoros","DRC","Congo",
                         "Côte d'Ivoire", "Djibouti","Equatorial Guinea","Eritrea","Eswatini","Ethiopia",
                         "Gabon","Gambia","Ghana","Guinea","Guinea-Bissau","Kenya","Lesotho","Liberia",
                         "Madagascar","Malawi","Mali","Mauritania","Mauritius","Mozambique","Namibia",
                         "Niger","Nigeria","Rwanda","Sao Tome and Principe","Senegal","Seychelles",
                         "Sierra Leone","Somalia","South Africa","South Sudan","Sudan","Tanzania",
                         "Togo","Union of the Comoros", "Uganda","Zambia","Zimbabwe")

# -----------------------------
# Final dataset calculations (Standardizing to Lowercase)
# -----------------------------
mal_export <- mal_long %>%
  filter(country %in% countries_to_select) %>%
  filter(!is.na(year)) %>%
  filter(between(year, 2000, 2023)) %>%
  mutate(
    # Ensure values are numeric
    cases = as.numeric(cases),
    deaths = as.numeric(deaths),
    population = as.numeric(population),
    
    incidence_rate = ifelse(!is.na(cases) & !is.na(population) & population > 0,
                       (cases / population) * 100000, NA),
    death_rate = ifelse(!is.na(deaths) & !is.na(population) & population > 0,
                        (deaths / population) * 100000, NA)
  ) %>%
  arrange(country, year)

# -----------------------------
# EXPORT
# -----------------------------
wb <- createWorkbook()
countries_final <- sort(unique(mal_export$country))

for(i in seq_along(countries_final)) {
  c_name <- countries_final[i]
  
  country_data <- mal_export %>%
    filter(country == c_name) %>%
    mutate(date = ymd(paste0(year, "-12-31"))) %>%
    # Select using lowercase names
    select(date, country, year, population, cases, incidence_rate, deaths, death_rate) %>%
    mutate(incidence_rate = round(incidence_rate, 2),
           death_rate = round(death_rate, 5))
  
  # Clean sheet name
  safe_sheet_name <- substr(gsub("[^A-Za-z0-9]", "", c_name), 1, 30)
  addWorksheet(wb, safe_sheet_name)
  writeData(wb, safe_sheet_name, country_data)
}

saveWorkbook(wb, "Malaria_TimeSeries_Bycountry.xlsx", overwrite = TRUE)
cat("\n✅ SUCCESS: Full dataset exported correctly with lowercase names!\n")


