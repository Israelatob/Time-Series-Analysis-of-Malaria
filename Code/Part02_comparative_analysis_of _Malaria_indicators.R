options(scipen = 999)
rm(list=ls(all=TRUE))

# Load required libraries
library(matrixStats)
library(xtable)
library(Matrix)
library(methods)
library(readxl)
library(rjson)

setwd("C:/Users/17034/Downloads/GSU PhD/Prof. Kirpich/Datasets/New datasets/Malaria/Final")
# Verify it worked
getwd()


# Load the .RData file
load("malaria_data_all.RData")

# Get list of countries
countries <- unique(malaria_data_all$country)


# Function to analyze a specific country
analyze_country <- function(country_name) {
  country_data <- malaria_data_all[malaria_data_all$country == country_name, ]
  country_data <- country_data[order(country_data$date), ]
  
  return(country_data)
}




example_countries  <- c("Benin", "Burkina Faso", "Central African Republic", 
                          "Côte d'Ivoire", "DRC", "Guinea",  
                          "Mali", "Mozambique", "Sierra Leone", "Uganda")



# Create directory for plots if it doesn't exist
if (!dir.exists("Plots")) {
  dir.create("Plots")
}


# Comparative analysis: Plot multiple countries together
pdf("Plots/Malaria_Comparative_Analysis.pdf", height = 15, width = 20)

# Set up layout
#layout(matrix(1:4, nrow = 2, ncol = 2))
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))


# #Colors for different countries
# country_colors <- c("Nigeria" = "red", "Angola" = "blue", "Benin" = "green", "Botswana" = "purple", "Burkina Faso" = "orange")
# country_colors <- unique(malaria_data_all$country)

# Create a named color vector for all 45 countries
#country_list <- unique(malaria_data_all$country)
country_list <- example_countries
#country_colors <- setNames(rainbow(length(country_list)), country_list)

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

# Plot 1: Cases comparison
y_min1 <- 0
y_max1 <- 35000000
#pdf("Plots/Malaria_Cases_Comparison.pdf", height = 10, width = 14)
plot(NULL, xlim = range(malaria_data_all$year), 
     ylim = c(y_min1, y_max1),
     main = "Malaria Cases - Country Comparison",
     xlab = "", ylab = "Number of Cases",
     cex.main = 2, cex.lab = 1.3, xaxt = "n") #xlab = "Year", ylim = range(malaria_data_all$cases, na.rm = TRUE),

# Add all years manually (tilted at 45°)
all_years <- sort(unique(malaria_data_all$year))
axis(1, at = all_years, labels = FALSE)  # add ticks only first
text(x = all_years, 
     y = par("usr")[3] - 0.05 * diff(par("usr")[3:4]),  # position slightly below axis
     labels = all_years, 
     srt = 45, adj = 1, xpd = TRUE, cex = 0.8)  # srt=45 for tilt

# Add country trend lines
for (country in example_countries) {
  country_data <- analyze_country_sorted(country)
  lines(country_data$year, country_data$cases, 
        col = country_colors[country], lwd = 2)
  
  # Overlay the individual data points on the line
  points(country_data$year, country_data$cases,
         col = country_colors[country],
         pch = 19,      # solid circle (can change to 16 or 17)
         cex = 1.2)     # point size
}

# Add legend and grid
legend("topright", legend = example_countries, 
       col = country_colors[example_countries], lwd = 3, cex = 1.0, inset = c(0.04, 0.3))
grid()
mtext("A", side = 3, line = 1, adj = 0, cex = 2, font = 2)
#dev.off()


# Plot 2: Deaths comparison
y_min2 <- 0
y_max2 <- 120000
#pdf("Plots/Malaria_Deaths_Comparison.pdf", height = 8, width = 14)
plot(NULL, xlim = range(malaria_data_all$year), 
     ylim = c(y_min2, y_max2),
     main = "Malaria Deaths - Country Comparison", xlab = "",
     ylab = "Number of Deaths",
     cex.main = 2, cex.lab = 1.3,xaxt = "n")#xlab = "Year", ylim = range(malaria_data_all$deaths, na.rm = TRUE),

# Add all years manually (tilted at 45°)
all_years <- sort(unique(malaria_data_all$year))
axis(1, at = all_years, labels = FALSE)  # add ticks only first
text(x = all_years, 
     y = par("usr")[3] - 0.05 * diff(par("usr")[3:4]),  # position slightly below axis
     labels = all_years, 
     srt = 45, adj = 1, xpd = TRUE, cex = 0.8)  # srt=45 for tilt

for (country in example_countries) {
  country_data <- analyze_country(country)
  lines(country_data$year, country_data$deaths, 
        col = country_colors[country], lwd = 2)
  
  # Overlay the individual data points on the line
  points(country_data$year, country_data$deaths,
         col = country_colors[country],
         pch = 19,      # solid circle (can change to 16 or 17)
         cex = 1.2)     # point size
}
legend("topright", legend = example_countries, 
       col = country_colors[example_countries], lwd = 3, cex = 1.0,inset = c(0.2, 0.04))
grid()
mtext("B", side = 3, line = 1, adj = 0, cex = 2, font = 2)
#dev.off()

# Plot 3: Incidence rate comparison
#pdf("Plots/Malaria_Incidence Rate_Comparison.pdf", height = 8, width = 14)
plot(NULL, xlim = range(malaria_data_all$year), 
     ylim = range(malaria_data_all$incidence_rate, na.rm = TRUE),
     main = "Incidence Rate - Country Comparison", xlab = "",
     ylab = "Incidence Rate (per 100,000)",
     cex.main = 2, cex.lab = 1.3, xaxt = "n")#xlab = "Year", 

# Add all years manually (tilted at 45°)
all_years <- sort(unique(malaria_data_all$year))
axis(1, at = all_years, labels = FALSE)  # add ticks only first
text(x = all_years, 
     y = par("usr")[3] - 0.05 * diff(par("usr")[3:4]),  # position slightly below axis
     labels = all_years, 
     srt = 45, adj = 1, xpd = TRUE, cex = 0.8)  # srt=45 for tilt


for (country in example_countries) {
  country_data <- analyze_country(country)
  lines(country_data$year, country_data$incidence_rate, 
        col = country_colors[country], lwd = 2)
  
  # Overlay the individual data points on the line
  points(country_data$year, country_data$incidence_rate,
         col = country_colors[country],
         pch = 19,      # solid circle (can change to 16 or 17)
         cex = 1.2)     # point size
}
legend("bottomleft", legend = example_countries, 
       col = country_colors[example_countries], lwd = 3, cex = 1.2, inset = c(0.05, 0.05))
grid()
mtext("C", side = 3, line = 1, adj = 0, cex = 2, font = 2)
#dev.off()

# Plot 4: Death rate comparison
#pdf("Plots/Malaria_Death Rate_Comparison.pdf", height = 8, width = 14)
plot(NULL, xlim = range(malaria_data_all$year), 
     ylim = range(malaria_data_all$death_rate, na.rm = TRUE),
     main = "Death Rate - Country Comparison", xlab = "",
     ylab = "Death Rate (per 100,000)",
     cex.main = 2, cex.lab = 1.3, xaxt = "n")#xlab = "Year", 

# Add all years manually (tilted at 45°)
all_years <- sort(unique(malaria_data_all$year))
axis(1, at = all_years, labels = FALSE)  # add ticks only first
text(x = all_years, 
     y = par("usr")[3] - 0.05 * diff(par("usr")[3:4]),  # position slightly below axis
     labels = all_years, 
     srt = 45, adj = 1, xpd = TRUE, cex = 0.8)  # srt=45 for tilt


for (country in example_countries) {
  country_data <- analyze_country(country)
  lines(country_data$year, country_data$death_rate, 
        col = country_colors[country], lwd = 2)
  # Overlay the individual data points on the line
  points(country_data$year, country_data$death_rate,
         col = country_colors[country],
         pch = 19,      # solid circle (can change to 16 or 17)
         cex = 1.2)     # point size
}
legend("topright", legend = example_countries, 
       col = country_colors[example_countries], lwd = 3, cex = 1.2, inset = c(0.05, 0.05))
grid()
mtext("D", side = 3, line = 1, adj = 0, cex = 2, font = 2)

dev.off()


# Generate summary statistics
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

# Save summary statistics
write.csv(summary_stats, "malaria_summary_statistics.csv", row.names = FALSE)

# Print top 10 countries by total cases
cat("Top 10 countries by total malaria cases:\n")
top_cases <- head(summary_stats[order(-summary_stats$Total_Cases), ], 10)
print(top_cases[, c("Country", "Total_Cases", "Total_Deaths")])

# Print top 10 countries by average incidence rate
cat("\nTop 10 countries by average incidence rate:\n")
top_incidence <- head(summary_stats[order(-summary_stats$Avg_Incidence_Rate), ], 10)
print(top_incidence[, c("Country", "Avg_Incidence_Rate", "Avg_Death_Rate")])


cat("Analysis complete! Check the 'Plots' folder for generated graphs.\n")
cat("Summary statistics saved as 'malaria_summary_statistics.csv'\n")











