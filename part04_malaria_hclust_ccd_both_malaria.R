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
## ---- CCD Clustering: Side-by-Side ----
############################################################
pdf(file.path(plots_dir, paste0("malaria_hclust_ccd_both", graph_suffix, ".pdf")),
    width = 16, height = 10)

hc_cases_ccd  <- hclust(as.dist(distance_matrices$incidence_rate_ccd), method = hclust_method)
hc_deaths_ccd <- hclust(as.dist(distance_matrices$death_rate_ccd), method = hclust_method)

par(mfrow = c(1,2), mar = c(8,4,4,2))

plot(hc_cases_ccd,
     main = "CCD Clustering – Malaria Incidence Rate (Average)",
     sub  = "", cex = 0.7, hang = 1,
     xlab = "", ylab = "", xaxt = "n", yaxt = "n", asp = 1)
mtext("A", side = 3, line = 1, adj = 0, cex = 2, font = 2)

plot(hc_deaths_ccd,
     main = "CCD Clustering – Malaria Death Rate (Average)",
     sub  = "", cex = 0.7, hang = 0.5,
     xlab = "", ylab = "", xaxt = "n", yaxt = "n", asp = 1)
mtext("B", side = 3, line = 1, adj = 0, cex = 2, font = 2)

dev.off()


############################################################
## ---- Done ----
############################################################
cat("✅ Analysis complete. All plots saved in:", plots_dir, "\n")
