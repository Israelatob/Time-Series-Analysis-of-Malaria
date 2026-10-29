options(scipen = 999)
rm(list = ls(all = TRUE))

# Load required libraries
library(matrixStats)
library(xtable)
library(Matrix)
library(methods)
library(readxl)
library(rjson)

# ==============================================================================
# SETTINGS & PATH DECLARATIONS
# ==============================================================================

# Set working directory to project R_Code directory
base_dir <- "C:/Users/17034/Downloads/GSU PhD/Prof. Kirpich/Datasets/GitHub Files/Time-Series-Analysis-of-Malaria/R_Code"
if (dir.exists(base_dir)) setwd(base_dir)

# Define clean relative paths from R_Code
dir_data   <- file.path("..", "R_Data")
dir_plots  <- file.path("..", "Plots")
dir_output <- file.path("..", "R_Output")

# Create output directories if they do not exist
if (!dir.exists(dir_plots))  dir.create(dir_plots, recursive = TRUE)
if (!dir.exists(dir_output)) dir.create(dir_output, recursive = TRUE)

# ==============================================================================
# LOAD DATA
# ==============================================================================

# Load the .RData file relative to dir_data
malaria_data_path <- file.path(dir_data, "malaria_data_all.RData")
load(malaria_data_path)

# Get list of countries
countries <- unique(malaria_data_all$country)

# Function to analyze a specific country
analyze_country <- function(country_name) {
  country_data <- malaria_data_all[malaria_data_all$country == country_name, ]
  country_data <- country_data[order(country_data$date), ]
  return(country_data)
}

example_countries <- c("Benin", "Burkina Faso", "Central African Republic", 
                       "Côte d'Ivoire", "DRC", "Guinea",  
                       "Mali", "Mozambique", "Sierra Leone", "Uganda")

# ==============================================================================
# COMPARATIVE ANALYSIS PLOT
# ==============================================================================

pdf_output_path <- file.path(dir_plots, "Malaria_Comparative_Analysis.pdf")
pdf(pdf_output_path, height = 15, width = 20)

# Set up layout
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))

country_list <- example_countries

# Paul Tol color-blind friendly qualitative palette (12 colors)
tol12 <- c(
  "#332288", "#88CCEE", "#44AA99", "#117733",
  "#999933", "#DDCC77", "#CC6677", "#882255",
  "#AA4499", "#661100", "#6699CC", "#AA4466"
)

# Select as many colors as needed for the number of countries
country_colors <- setNames(tol12[1:length(country_list)], country_list)

# Helper: ensure sorted data for consistent trend lines
analyze_country_sorted <- function(country) {
  df <- malaria_data_all[malaria_data_all$country == country, ]
  df <- df[order(df$year), ]  # sort by year
  return(df)
}

# ------------------------------------------------------------------------------
# Plot 1: Cases comparison
# ------------------------------------------------------------------------------
y_min1 <- 0
y_max1 <- 35000000

plot(NULL, xlim = range(malaria_data_all$year), 
     ylim = c(y_min1, y_max1),
     main = "Malaria Cases - Country Comparison",
     xlab = "", ylab = "Number of Cases",
     cex.main = 2, cex.lab = 1.3, xaxt = "n")

# Add all years manually (tilted at 45°)
all_years <- sort(unique(malaria_data_all$year))
axis(1, at = all_years, labels = FALSE)
text(x = all_years, 
     y = par("usr")[3] - 0.05 * diff(par("usr")[3:4]), 
     labels = all_years, 
     srt = 45, adj = 1, xpd = TRUE, cex = 0.8)

# Add country trend lines
for (country in example_countries) {
  country_data <- analyze_country_sorted(country)
  lines(country_data$year, country_data$cases, 
        col = country_colors[country], lwd = 2)
  points(country_data$year, country_data$cases,
         col = country_colors[country],
         pch = 19, cex = 1.2)
}

legend("topright", legend = example_countries, 
       col = country_colors[example_countries], lwd = 3, cex = 1.0, inset = c(0.04, 0.3))
grid()
mtext("A", side = 3, line = 1, adj = 0, cex = 2, font = 2)

# ------------------------------------------------------------------------------
# Plot 2: Deaths comparison
# ------------------------------------------------------------------------------
y_min2 <- 0
y_max2 <- 120000

plot(NULL, xlim = range(malaria_data_all$year), 
     ylim = c(y_min2, y_max2),
     main = "Malaria Deaths - Country Comparison", xlab = "",
     ylab = "Number of Deaths",
     cex.main = 2, cex.lab = 1.3, xaxt = "n")

all_years <- sort(unique(malaria_data_all$year))
axis(1, at = all_years, labels = FALSE)
text(x = all_years, 
     y = par("usr")[3] - 0.05 * diff(par("usr")[3:4]), 
     labels = all_years, 
     srt = 45, adj = 1, xpd = TRUE, cex = 0.8)

for (country in example_countries) {
  country_data <- analyze_country(country)
  lines(country_data$year, country_data$deaths, 
        col = country_colors[country], lwd = 2)
  points(country_data$year, country_data$deaths,
         col = country_colors[country],
         pch = 19, cex = 1.2)
}
legend("topright", legend = example_countries, 
       col = country_colors[example_countries], lwd = 3, cex = 1.0, inset = c(0.2, 0.04))
grid()
mtext("B", side = 3, line = 1, adj = 0, cex = 2, font = 2)

# ------------------------------------------------------------------------------
# Plot 3: Incidence rate comparison
# ------------------------------------------------------------------------------
plot(NULL, xlim = range(malaria_data_all$year), 
     ylim = range(malaria_data_all$incidence_rate, na.rm = TRUE),
     main = "Incidence Rate - Country Comparison", xlab = "",
     ylab = "Incidence Rate (per 100,000)",
     cex.main = 2, cex.lab = 1.3, xaxt = "n")

all_years <- sort(unique(malaria_data_all$year))
axis(1, at = all_years, labels = FALSE)
text(x = all_years, 
     y = par("usr")[3] - 0.05 * diff(par("usr")[3:4]), 
     labels = all_years, 
     srt = 45, adj = 1, xpd = TRUE, cex = 0.8)

for (country in example_countries) {
  country_data <- analyze_country(country)
  lines(country_data$year, country_data$incidence_rate, 
        col = country_colors[country], lwd = 2)
  points(country_data$year, country_data$incidence_rate,
         col = country_colors[country],
         pch = 19, cex = 1.2)
}
legend("bottomleft", legend = example_countries, 
       col = country_colors[example_countries], lwd = 3, cex = 1.2, inset = c(0.05, 0.05))
grid()
mtext("C", side = 3, line = 1, adj = 0, cex = 2, font = 2)

# ------------------------------------------------------------------------------
# Plot 4: Death rate comparison
# ------------------------------------------------------------------------------
plot(NULL, xlim = range(malaria_data_all$year), 
     ylim = range(malaria_data_all$death_rate, na.rm = TRUE),
     main = "Death Rate - Country Comparison", xlab = "",
     ylab = "Death Rate (per 100,000)",
     cex.main = 2, cex.lab = 1.3, xaxt = "n")

all_years <- sort(unique(malaria_data_all$year))
axis(1, at = all_years, labels = FALSE)
text(x = all_years, 
     y = par("usr")[3] - 0.05 * diff(par("usr")[3:4]), 
     labels = all_years, 
     srt = 45, adj = 1, xpd = TRUE, cex = 0.8)

for (country in example_countries) {
  country_data <- analyze_country(country)
  lines(country_data$year, country_data$death_rate, 
        col = country_colors[country], lwd = 2)
  points(country_data$year, country_data$death_rate,
         col = country_colors[country],
         pch = 19, cex = 1.2)
}
legend("topright", legend = example_countries, 
       col = country_colors[example_countries], lwd = 3, cex = 1.2, inset = c(0.05, 0.05))
grid()
mtext("D", side = 3, line = 1, adj = 0, cex = 2, font = 2)

dev.off()

# ==============================================================================
# SUMMARY STATISTICS & OUTPUT
# ==============================================================================

summary_stats <- data.frame()

for (country in countries) {
  country_data <- analyze_country(country)
  
  if (nrow(country_data) > 0) {
    stats <- data.frame(
      Country = country,
      Total_Cases = sum(country_data$cases, na.rm = TRUE),
      Total_Deaths = sum(country_data$deaths, na.rm = TRUE),
      Avg_Incidence_Rate = mean(country_data$incidence_rate, na.rm = TRUE),
      Avg_Death_Rate = mean(country_data$death_rate, na.rm = TRUE),
      Peak_Cases_Year = country_data$year[which.max(country_data$cases)],
      Peak_Deaths_Year = country_data$year[which.max(country_data$deaths)]
    )
    summary_stats <- rbind(summary_stats, stats)
  }
}

# Save summary statistics to R_Output
csv_output_path <- file.path(dir_output, "malaria_summary_statistics.csv")
write.csv(summary_stats, csv_output_path, row.names = FALSE)

# Print top 10 countries by total cases
cat("Top 10 countries by total malaria cases:\n")
top_cases <- head(summary_stats[order(-summary_stats$Total_Cases), ], 10)
print(top_cases[, c("Country", "Total_Cases", "Total_Deaths")])

# Print top 10 countries by average incidence rate
cat("\nTop 10 countries by average incidence rate:\n")
top_incidence <- head(summary_stats[order(-summary_stats$Avg_Incidence_Rate), ], 10)
print(top_incidence[, c("Country", "Avg_Incidence_Rate", "Avg_Death_Rate")])

cat("\nAnalysis complete!\n")
cat("Generated graph saved to:", normalizePath(pdf_output_path, mustWork = FALSE), "\n")
cat("Summary statistics saved to:", normalizePath(csv_output_path, mustWork = FALSE), "\n")