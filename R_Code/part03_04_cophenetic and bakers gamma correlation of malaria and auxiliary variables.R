# ==============================================================================
# FIGURE 4
# Comparisons of Malaria and Auxiliary Variables
#
# Outputs:
#   Plots/Figure4_Comparisons_Malaria_Auxiliary.pdf
# ==============================================================================

# --- PRE-FLIGHT ---
while (!is.null(dev.list())) dev.off()
graphics.off()
rm(list = ls(all = TRUE))

# ==============================================================================
# 1. LOAD LIBRARIES
# ==============================================================================

suppressPackageStartupMessages({
  library(dendextend)
  library(corrplot)
  library(dplyr)
  library(countrycode)
})

# ==============================================================================
# 2. SETTINGS & PATH DECLARATIONS
# ==============================================================================

# Set working directory to project R_Code directory
base_dir <- "C:/Users/17034/Downloads/GSU PhD/Prof. Kirpich/Datasets/GitHub Files/Time-Series-Analysis-of-Malaria/R_Code"
if (dir.exists(base_dir)) setwd(base_dir)

# Define clean relative paths from R_Code
dir_data  <- file.path("..", "R_Data")
dir_plots <- file.path("..", "Plots")

# Automatically create output Plots directory if it does not exist
if (!dir.exists(dir_plots)) {
  dir.create(dir_plots, recursive = TRUE)
}

hclust_method <- "average"

aux_files <- list(
  RAIN = "rain_summary.RData",
  AGE  = "age_summary.RData",
  DEN  = "den_summary.RData",
  PHY  = "phy_summary.RData",
  GDP  = "gdp_summary.RData",
  TEMP = "temp_summary.RData",
  ELEV = "elev_summary.RData",
  LAT  = "lat_summary.RData"
)

# ==============================================================================
# 3. HELPER FUNCTIONS
# ==============================================================================

load_obj <- function(path) {
  if (!file.exists(path)) return(NULL)
  env <- new.env()
  load(path, envir = env)
  if (length(ls(env)) == 0) return(NULL)
  get(ls(env)[1], envir = env)
}

clean_country <- function(x) {
  x <- trimws(as.character(x))
  standard_names <- suppressWarnings(
    countrycode(
      x,
      origin = "country.name",
      destination = "country.name"
    )
  )
  standard_names[is.na(standard_names)] <- x[is.na(standard_names)]
  standard_names
}

build_hclust <- function(input_obj) {
  # Option A: Input is already a distance matrix or square matrix
  if (is.matrix(input_obj) && nrow(input_obj) == ncol(input_obj)) {
    hc <- hclust(as.dist(input_obj), method = hclust_method)
    hc$labels <- clean_country(rownames(input_obj))
    return(hc)
  }
  
  # Option B: Input is a data frame (e.g., malaria incidence or raw summary)
  df <- as.data.frame(input_obj)
  char_cols <- which(sapply(df, function(x) is.character(x) | is.factor(x)))
  if (length(char_cols) == 0) return(NULL)
  
  country_col <- char_cols[1]
  countries   <- clean_country(df[, country_col])
  
  df$CleanedCountryTmp <- countries
  df <- df[!duplicated(df$CleanedCountryTmp), ]
  rownames(df) <- df$CleanedCountryTmp
  df$CleanedCountryTmp <- NULL
  
  nums <- df[, sapply(df, is.numeric), drop = FALSE]
  if (nrow(nums) < 3) return(NULL)
  
  scaled_nums <- scale(nums)
  
  hc <- hclust(dist(scaled_nums), method = hclust_method)
  hc$labels <- rownames(scaled_nums)
  hc
}

align_dends <- function(d1, d2) {
  common <- intersect(labels(d1), labels(d2))
  if (length(common) < 3) return(NULL)
  
  d1p <- prune(d1, setdiff(labels(d1), common))
  d2p <- prune(d2, setdiff(labels(d2), common))
  
  list(d1p, d2p)
}

# ==============================================================================
# 4. LOAD AND CLUSTER MALARIA DATA
# ==============================================================================

cat("Loading malaria and auxiliary datasets...\n")

all_objs <- list()

malaria_file <- file.path(dir_data, "Malaria_data_all.RData")

if (file.exists(malaria_file)) {
  mal <- load_obj(malaria_file)
  
  if ("country" %in% names(mal)) {
    if ("incidence_rate" %in% names(mal)) {
      all_objs$INC <- build_hclust(mal[, c("country", "incidence_rate")])
    }
    if ("death_rate" %in% names(mal)) {
      all_objs$MOR <- build_hclust(mal[, c("country", "death_rate")])
    }
  }
}

# ==============================================================================
# 5. LOAD AUXILIARY DISTANCE MATRICES
# ==============================================================================

aux_matrix_path <- file.path(dir_data, "auxiliary_distance_matrices.RData")

if (file.exists(aux_matrix_path)) {
  aux_mats <- load_obj(aux_matrix_path)
  
  for (nm in names(aux_mats)) {
    clust_res <- build_hclust(aux_mats[[nm]])
    if (!is.null(clust_res)) {
      all_objs[[nm]] <- clust_res
    }
  }
} else {
  warning("Precomputed distance matrix file not found at: ", aux_matrix_path)
}

# ==============================================================================
# 6. CONVERT TO DENDROGRAMS
# ==============================================================================

dends <- lapply(
  all_objs,
  function(hc) {
    hc$height <- (hc$height / max(hc$height)) * 1000
    as.dendrogram(hc)
  }
)

cat(
  "Successfully loaded clusters:",
  paste(names(dends), collapse = ", "),
  "\n"
)

# ==============================================================================
# 7. CALCULATE AGREEMENT MATRICES
# ==============================================================================

n <- length(dends)
labs <- names(dends)

cophenetic_mat <- matrix(0, n, n, dimnames = list(labs, labs))
bakers_mat     <- matrix(0, n, n, dimnames = list(labs, labs))

diag(cophenetic_mat) <- 1
diag(bakers_mat)     <- 1

for (i in 1:(n - 1)) {
  for (j in (i + 1):n) {
    aligned <- align_dends(dends[[i]], dends[[j]])
    
    if (!is.null(aligned)) {
      dl <- dendlist(aligned[[1]], aligned[[2]]) %>%
        untangle(method = "step2side")
      
      cophenetic_mat[i, j] <- cophenetic_mat[j, i] <- cor_cophenetic(dl[[1]], dl[[2]])
      bakers_mat[i, j]     <- bakers_mat[j, i]     <- cor_bakers_gamma(dl[[1]], dl[[2]])
    }
  }
}

# ==============================================================================
# 8. PUBLICATION LABELS
# ==============================================================================

pretty_names <- c(
  INC  = "Average Malaria Incidence Rate",
  MOR  = "Average Malaria Death Rate",
  RAIN = "Median Annual Rainfall",
  AGE  = "Median Population Age",
  DEN  = "Population Density",
  PHY  = "Physicians per 1000 People",
  GDP  = "GDP per Capita",
  TEMP = "Median Annual Temperature",
  ELEV = "Mean Elevation",
  LAT  = "Average Latitude"
)

plot_cophenetic_mat <- cophenetic_mat
plot_bakers_mat     <- bakers_mat

rownames(plot_cophenetic_mat) <- pretty_names[rownames(plot_cophenetic_mat)]
colnames(plot_cophenetic_mat) <- pretty_names[colnames(plot_cophenetic_mat)]
rownames(plot_bakers_mat)     <- pretty_names[rownames(plot_bakers_mat)]
colnames(plot_bakers_mat)     <- pretty_names[colnames(plot_bakers_mat)]

# ==============================================================================
# 9. GENERATE FIGURE 4 PDF
# ==============================================================================

cat("Saving Figure 4...\n")

fig4_pdf_path <- file.path(dir_plots, "Figure4_Comparisons_Malaria_Auxiliary.pdf")

pdf(
  fig4_pdf_path,
  width  = 18,
  height = 9
)

layout(matrix(c(1, 2), nrow = 1))

# PANEL A — COPHENETIC
par(mar = c(1, 1, 7, 3))
corrplot(
  plot_cophenetic_mat,
  method = "pie",
  type   = "full",
  tl.col = "black",
  tl.cex = 1.0,
  tl.srt = 90,
  cl.cex = 0.9,
  diag   = TRUE,
  title  = "Correlations for Trees (Cophenetic)",
  mar    = c(0, 0, 3, 0)
)
mtext("A", side = 3, line = 1, adj = 0, cex = 3.5, font = 2)

# PANEL B — BAKER
par(mar = c(1, 1, 7, 3))
corrplot(
  plot_bakers_mat,
  method = "pie",
  type   = "full",
  tl.col = "black",
  tl.cex = 1.0,
  tl.srt = 90,
  cl.cex = 0.9,
  diag   = TRUE,
  title  = "Correlations for Trees (Baker)",
  mar    = c(0, 0, 3, 0)
)
mtext("B", side = 3, line = 1, adj = 0, cex = 3.5, font = 2)

dev.off()

cat("\nFigure 4 successfully saved to:\n", normalizePath(fig4_pdf_path, mustWork = FALSE), "\n")