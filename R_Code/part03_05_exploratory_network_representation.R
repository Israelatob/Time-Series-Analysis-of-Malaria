# ==============================================================================
# FIGURE 5 + AGREEMENT TABLE
# Exploratory Network Representation & Agreement Rankings
#
# Outputs:
#   Plots/Figure5_Exploratory_Network_Representation.pdf
#   R_Output/Table1_Agreement_Rankings.xlsx
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
  library(igraph)
  library(dplyr)
  library(countrycode)
  library(writexl)
})

# ==============================================================================
# 2. SETTINGS & PATH DECLARATIONS
# ==============================================================================

# Set working directory to project R_Code directory
base_dir <- "C:/Users/17034/Downloads/GSU PhD/Prof. Kirpich/Datasets/GitHub Files/Time-Series-Analysis-of-Malaria/R_Code"
if (dir.exists(base_dir)) setwd(base_dir)

# Define clean relative paths from R_Code
dir_data   <- file.path("..", "R_Data")
dir_plots  <- file.path("..", "Plots")
dir_output <- file.path("..", "R_Output")

# Automatically create output directories if they do not exist
if (!dir.exists(dir_plots))  dir.create(dir_plots, recursive = TRUE)
if (!dir.exists(dir_output)) dir.create(dir_output, recursive = TRUE)

hclust_method     <- "average"
network_threshold <- 0.10

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
# 4. LOAD MALARIA DATA
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
# 8. NETWORK PREPARATION FUNCTION
# ==============================================================================

prep_net <- function(mat, thresh) {
  m <- mat
  m[m < thresh] <- 0
  diag(m) <- 0
  graph_from_adjacency_matrix(
    m,
    mode = "undirected",
    weighted = TRUE
  )
}

net_c <- prep_net(cophenetic_mat, network_threshold)
net_b <- prep_net(bakers_mat, network_threshold)

# ==============================================================================
# 9. COMMON NETWORK LAYOUT
# ==============================================================================

combined_graph <- igraph::union(net_c, net_b)
fixed_layout   <- layout_with_kk(combined_graph)

# ==============================================================================
# 10. FIGURE 5 (PDF OUTPUT)
# ==============================================================================

cat("Saving Figure 5...\n")

fig5_pdf_path <- file.path(dir_plots, "Figure5_Exploratory_Network_Representation.pdf")

pdf(
  fig5_pdf_path,
  width  = 18,
  height = 8
)

layout(matrix(c(1, 2), nrow = 1))

# PANEL A — COPHENETIC NETWORK
par(mar = c(1, 1, 7, 1))
plot(
  net_c,
  layout             = fixed_layout,
  edge.width         = E(net_c)$weight * 12,
  vertex.color       = "#D9EAF7",
  vertex.frame.color = "black",
  vertex.shape       = "circle",
  vertex.size        = 35,
  vertex.label.color = "black",
  vertex.label.cex   = 1.25,
  vertex.label.font  = 2,
  edge.color         = "gray50",
  edge.curved        = 0.15,
  main               = paste0("Graph of Links for r > ", network_threshold, " (Cophenetic)")
)
mtext("A", side = 3, line = 1, adj = 0, cex = 3.5, font = 2)

# PANEL B — BAKER NETWORK
par(mar = c(1, 1, 7, 1))
plot(
  net_b,
  layout             = fixed_layout,
  edge.width         = E(net_b)$weight * 12,
  vertex.color       = "#D9EAF7",
  vertex.frame.color = "black",
  vertex.shape       = "circle",
  vertex.size        = 35,
  vertex.label.color = "black",
  vertex.label.cex   = 1.25,
  vertex.label.font  = 2,
  edge.color         = "gray50",
  edge.curved        = 0.15,
  main               = paste0("Graph of Links for r > ", network_threshold, " (Baker)")
)
mtext("B", side = 3, line = 1, adj = 0, cex = 3.5, font = 2)

dev.off()

# ==============================================================================
# 11. AGREEMENT TABLE GENERATION
# ==============================================================================

cat("Creating agreement table...\n")

aux_vars <- c("RAIN", "AGE", "DEN", "PHY", "GDP", "TEMP", "ELEV", "LAT")

agreement_table <- data.frame(
  Variable       = aux_vars,
  Cophenetic_INC = round(cophenetic_mat["INC", aux_vars], 3),
  Baker_INC      = round(bakers_mat["INC", aux_vars], 3),
  Cophenetic_MOR = round(cophenetic_mat["MOR", aux_vars], 3),
  Baker_MOR      = round(bakers_mat["MOR", aux_vars], 3)
)

agreement_table$Variable <- c(
  "Rainfall",
  "Population Age",
  "Population Density",
  "Physicians per 1000 People",
  "GDP per Capita",
  "Temperature",
  "Elevation",
  "Latitude"
)

agreement_table <- agreement_table %>%
  arrange(desc(Cophenetic_INC))

print(agreement_table)

# ==============================================================================
# 12. SAVE TABLE AS EXCEL
# ==============================================================================

excel_file_path <- file.path(dir_output, "Table1_Agreement_Rankings.xlsx")

write_xlsx(
  agreement_table,
  excel_file_path
)

cat("\n======================================================================\n")
cat("SUCCESS: Figure 5 and Table 1 saved.\n")
cat("======================================================================\n")
cat("Figure PDF:", normalizePath(fig5_pdf_path, mustWork = FALSE), "\n")
cat("Excel Table:", normalizePath(excel_file_path, mustWork = FALSE), "\n")