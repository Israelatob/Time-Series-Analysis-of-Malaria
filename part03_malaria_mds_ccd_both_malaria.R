############################################################
## Malaria Time-Series Similarity, Clustering & Tanglegram
## Author: Israel Atobrhan
## Georgia State University  
## Date: 2024-06-04  (extended with tanglegram)
############################################################

rm(list = ls(all = TRUE))
options(scipen = 20)

############################################################
## ---- Packages ----
############################################################
required_pkgs <- c(
  "gtools", "dtw", "plotrix", "dendextend", "RColorBrewer",
  "ggrepel", "patchwork", "ggplot2", "dplyr"
)

for (pkg in required_pkgs) {
  if (!require(pkg, character.only = TRUE)) {
    stop("Package not installed: ", pkg)
  }
}

############################################################
## ---- Paths & Data ----
############################################################
setwd("C:/Users/17034/Downloads/GSU PhD/Prof. Kirpich/Datasets/New datasets/Malaria/Final")

data_file     <- "malaria_data_all.RData"
plots_dir     <- "Plots"
graph_suffix  <- "_malaria"
hclust_method <- "average"

load(data_file)
stopifnot(exists("malaria_data_all"))

if (!dir.exists(plots_dir)) dir.create(plots_dir)

cat("Data loaded:", dim(malaria_data_all), "\n")

############################################################
## ---- Countries & Pairs ----
############################################################
countries    <- sort(unique(malaria_data_all$country))
n_countries  <- length(countries)

pairs_frame <- as.data.frame(
  combinations(n_countries, 2, countries),
  stringsAsFactors = FALSE
)
colnames(pairs_frame) <- c("Country1", "Country2")

cat("Country pairs:", nrow(pairs_frame), "\n")

############################################################
## ---- Similarity / Distance Function ----
############################################################
calculate_distance <- function(c1, c2, metric,
                               method = c("ccd", "dtw")) {
  
  method <- match.arg(method)
  
  d1 <- malaria_data_all[malaria_data_all$country == c1, ]
  d2 <- malaria_data_all[malaria_data_all$country == c2, ]
  
  yrs <- intersect(d1$year, d2$year)
  if (length(yrs) < 3) return(NA_real_)
  
  x <- d1[d1$year %in% yrs, metric]
  y <- d2[d2$year %in% yrs, metric]
  
  keep <- !is.na(x) & !is.na(y)
  x <- x[keep]; y <- y[keep]
  
  if (length(x) < 3 || sd(x) == 0 || sd(y) == 0) return(NA_real_)
  
  ## ---- DTW: distance only (normalized) ----
  if (method == "dtw") {
    d <- dtw(x, y, keep = TRUE)$distance
    return(d)
  }
  
  ## ---- CCD: converted to distance ----
  if (method == "ccd") {
    acf_vals <- max(na.omit(ccf(x, y, plot = FALSE)$acf))
    #acf_vals <- acf_vals[-which.max(acf_vals)]   # remove lag 0
    #sim <- max(acf_vals, na.rm = TRUE)
    #sim <- max(min(sim, 1), -1)
    return(1 - acf_vals)
  }
}

############################################################
## ---- Compute Pairwise Distances ----
############################################################
metrics <- c("incidence_rate", "death_rate")
methods <- c("ccd", "dtw")

for (m in metrics) {
  for (s in methods) {
    pairs_frame[[paste0(m, "_", s)]] <- NA_real_
  }
}

for (i in seq_len(nrow(pairs_frame))) {
  for (m in metrics) {
    for (s in methods) {
      pairs_frame[i, paste0(m, "_", s)] <-
        calculate_distance(
          pairs_frame$Country1[i],
          pairs_frame$Country2[i],
          metric = m,
          method = s
        )
    }
  }
}

pairs_frame <- pairs_frame[complete.cases(pairs_frame), ]
cat("Valid pairs:", nrow(pairs_frame), "\n")

############################################################
## ---- Distance Matrix Builder ----
############################################################
build_distance_matrix <- function(dist_vec, pairs, countries) {
  
  mat <- matrix(0, length(countries), length(countries),
                dimnames = list(countries, countries))
  
  for (i in seq_len(nrow(pairs))) {
    d <- dist_vec[i]
    mat[pairs$Country1[i], pairs$Country2[i]] <- d
    mat[pairs$Country2[i], pairs$Country1[i]] <- d
  }
  
  ## scale for numerical stability
  mat <- mat / max(mat, na.rm = TRUE) * 1000
  mat
}

head(build_distance_matrix)
############################################################
## ---- Build Distance Matrices ----
############################################################
distance_matrices <- list()

for (m in metrics) {
  for (s in methods) {
    key <- paste0(m, "_", s)
    distance_matrices[[key]] <-
      build_distance_matrix(
        pairs_frame[[key]],
        pairs_frame,
        countries
      )
  }
}

save(distance_matrices, file = "distance_matrices.RData")

############################################################
## ---- Helper function: padded plot limits ----
############################################################
pad_limits <- function(x, pad = 0.15) {
  r <- range(x, na.rm = TRUE)
  d <- diff(r)
  c(r[1] - pad * d, r[2] + pad * d)
}



############################################################
## ---- MDS: Cases & Deaths (CCD - Side-by-Side) ----
############################################################

# --- DEFINE THE HELPER FUNCTION FIRST ---
create_mds_panel <- function(data, xlims, ylims, color, title, panel_label) {
  # Calculate aspect ratio from the data ranges to ensure true square
  x_range <- diff(xlims)
  y_range <- diff(ylims)
  
  # Determine which dimension needs to be expanded to make square
  if (x_range > y_range) {
    # Expand y limits to match x range
    y_mid <- mean(ylims)
    ylims <- y_mid + c(-1, 1) * x_range/2
  } else {
    # Expand x limits to match y range
    x_mid <- mean(xlims)
    xlims <- x_mid + c(-1, 1) * y_range/2
  }
  
  ggplot(data, aes(x, y, label = label)) +
    geom_point(pch = 19, colour = color, size = 3, stroke = 0) +
    geom_text_repel(
      size = 3.5, fontface = "bold", colour = color,
      max.overlaps = Inf, box.padding = 0.4, point.padding = 0.4,
      segment.colour = color, segment.size = 0.3,
      min.segment.length = 0, direction = "both", seed = 123
    ) +
    coord_fixed(ratio = 1, xlim = xlims, ylim = ylims, expand = FALSE) +
    labs(title = title) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5, margin = margin(b = 10)),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.3),
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      aspect.ratio = 1,
      plot.margin = margin(20, 5, 20, 5)
    ) +
    annotate("text", x = xlims[1] + 0.02 * diff(xlims), 
             y = ylims[2] - 0.02 * diff(ylims), 
             label = panel_label, size = 7, fontface = "bold", 
             hjust = 0, vjust = 1)
}

# Calculate MDS coordinates
mds_cases <- cmdscale(as.dist(distance_matrices$incidence_rate_ccd), k = 2)
mds_deaths <- cmdscale(as.dist(distance_matrices$death_rate_ccd), k = 2)

xlim_cases <- pad_limits(mds_cases[,1], pad = 0.15)
ylim_cases <- pad_limits(mds_cases[,2], pad = 0.15)
xlim_deaths <- pad_limits(mds_deaths[,1], pad = 0.15)
ylim_deaths <- pad_limits(mds_deaths[,2], pad = 0.15)

# Convert to data frames - USE THE SAME VARIABLE NAMES YOU CALCULATED
mds_cases_df <- data.frame(
  x = mds_cases[,1], y = mds_cases[,2], 
  label = rownames(mds_cases)
)
mds_deaths_df <- data.frame(
  x = mds_deaths[,1], y = mds_deaths[,2], 
  label = rownames(mds_deaths)
)

# REMOVE the conflicting line - you already defined mds_cases_df above
# mds_cases_ccd_df <- data.frame(x = mds_cases_ccd[,1], y = mds_cases_ccd[,2], label = rownames(mds_cases_ccd))
# mds_deaths_ccd_df <- data.frame(x = mds_deaths_ccd[,1], y = mds_deaths_ccd[,2], label = rownames(mds_deaths_ccd))

# Create panels with adjusted square limits
p1 <- create_mds_panel(
  data = mds_cases_df,      # Use mds_cases_df
  xlims = xlim_cases,       # Use xlim_cases
  ylims = ylim_cases,       # Use ylim_cases
  color = "royalblue3",
  title = "Multidimensional Scaling of Malaria Incidence Rate (CCD Distance)",
  panel_label = "A"
)

p2 <- create_mds_panel(
  data = mds_deaths_df,     # Use mds_deaths_df
  xlims = xlim_deaths,      # Use xlim_deaths
  ylims = ylim_deaths,      # Use ylim_deaths
  color = "red3",
  title = "Multidimensional Scaling of Malaria Death Rate (CCD Distance)",
  panel_label = "B"
)

# Combine with proper square layout
combined_ccd <- p1 + plot_spacer() + p2 +
  plot_layout(
    ncol = 3,
    widths = c(1, 0.005, 1),
    heights = 1
  )

# Save with proper dimensions
ggsave(
  filename = file.path(plots_dir, paste0("malaria_mds_ccd_both", graph_suffix, ".pdf")),
  plot = combined_ccd,
  width = 16,
  height = 8,
  units = "in",
  bg = "white",
  device = cairo_pdf
)









############################################################
## ---- Done ----
############################################################
cat("✅ Analysis complete. All plots saved in:", plots_dir, "\n")
