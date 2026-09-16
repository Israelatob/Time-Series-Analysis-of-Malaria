# ==============================================================================
# FIGURE 5 + AGREEMENT TABLE
# Exploratory Network Representation
#
# Outputs:
#   Figure5_Exploratory_Network_Representation.pdf
#   Table1_Agreement_Rankings.xlsx
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
# 2. SETTINGS
# ==============================================================================

data_dir <- "C:/Users/17034/Downloads/GSU PhD/Prof. Kirpich/Datasets/New datasets/Malaria/Final/Bakers&Cophentic"

setwd(data_dir)

plot_dir <- file.path(data_dir, "Plots")

if (!dir.exists(plot_dir)) {
  dir.create(plot_dir)
}

hclust_method <- "average"

network_threshold <- 0.10

aux_files <- list(
  RAIN    = "rain_summary.RData",
  AGE     = "age_summary.RData",
  DEN     = "den_summary.RData",
  PHY     = "phy_summary.RData",
  GDP     = "gdp_summary.RData",
  TEMP    = "temp_summary.RData",
  ELEV    = "elev_summary.RData",
  LATLONG = "latlong_summary.RData"
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


build_hclust <- function(df) {
  
  df <- as.data.frame(df)
  
  char_cols <- which(
    sapply(df, function(x)
      is.character(x) | is.factor(x)
    )
  )
  
  if (length(char_cols) == 0) return(NULL)
  
  country_col <- char_cols[1]
  
  countries <- clean_country(
    df[, country_col]
  )
  
  df$CleanedCountryTmp <- countries
  
  df <- df[
    !duplicated(df$CleanedCountryTmp),
  ]
  
  rownames(df) <-
    df$CleanedCountryTmp
  
  df$CleanedCountryTmp <- NULL
  
  nums <- df[
    ,
    sapply(df, is.numeric),
    drop = FALSE
  ]
  
  if (nrow(nums) < 3) return(NULL)
  
  scaled_nums <- scale(nums)
  
  hc <- hclust(
    dist(scaled_nums),
    method = hclust_method
  )
  
  hc$labels <- rownames(scaled_nums)
  
  hc
}


align_dends <- function(d1, d2) {
  
  common <- intersect(
    labels(d1),
    labels(d2)
  )
  
  if (length(common) < 3) return(NULL)
  
  d1p <- prune(
    d1,
    setdiff(labels(d1), common)
  )
  
  d2p <- prune(
    d2,
    setdiff(labels(d2), common)
  )
  
  list(d1p, d2p)
}

# ==============================================================================
# 4. LOAD MALARIA DATA
# ==============================================================================

all_objs <- list()

malaria_file <- file.path(
  data_dir,
  "R_Data",
  "Malaria_data_all.RData"
)

if (file.exists(malaria_file)) {
  
  mal <- load_obj(malaria_file)
  
  if ("country" %in% names(mal)) {
    
    if ("incidence_rate" %in% names(mal)) {
      
      all_objs$INC <-
        build_hclust(
          mal[, c("country", "incidence_rate")]
        )
    }
    
    if ("death_rate" %in% names(mal)) {
      
      all_objs$MOR <-
        build_hclust(
          mal[, c("country", "death_rate")]
        )
    }
  }
}

# ==============================================================================
# 5. LOAD AUXILIARY VARIABLES
# ==============================================================================

for (nm in names(aux_files)) {
  
  f_path <- file.path(
    data_dir,
    "R_Data",
    aux_files[[nm]]
  )
  
  obj <- load_obj(f_path)
  
  if (!is.null(obj)) {
    
    clust_res <- build_hclust(obj)
    
    if (!is.null(clust_res)) {
      
      all_objs[[nm]] <- clust_res
    }
  }
}

all_objs <- all_objs[
  !sapply(all_objs, is.null)
]

# ==============================================================================
# 6. CONVERT TO DENDROGRAMS
# ==============================================================================

dends <- lapply(
  all_objs,
  function(hc) {
    
    hc$height <-
      (hc$height / max(hc$height)) * 1000
    
    as.dendrogram(hc)
  }
)

# ==============================================================================
# 7. CALCULATE AGREEMENT MATRICES
# ==============================================================================

n <- length(dends)

labs <- names(dends)

cophenetic_mat <- matrix(
  0,
  n,
  n,
  dimnames = list(labs, labs)
)

bakers_mat <- matrix(
  0,
  n,
  n,
  dimnames = list(labs, labs)
)

diag(cophenetic_mat) <- 1
diag(bakers_mat) <- 1

for (i in 1:(n - 1)) {
  
  for (j in (i + 1):n) {
    
    aligned <- align_dends(
      dends[[i]],
      dends[[j]]
    )
    
    if (!is.null(aligned)) {
      
      dl <-
        dendlist(
          aligned[[1]],
          aligned[[2]]
        ) %>%
        untangle(method = "step2side")
      
      cophenetic_mat[i, j] <-
        cophenetic_mat[j, i] <-
        cor_cophenetic(
          dl[[1]],
          dl[[2]]
        )
      
      bakers_mat[i, j] <-
        bakers_mat[j, i] <-
        cor_bakers_gamma(
          dl[[1]],
          dl[[2]]
        )
    }
  }
}

# ==============================================================================
# 8. NETWORK FUNCTION
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

net_c <- prep_net(
  cophenetic_mat,
  network_threshold
)

net_b <- prep_net(
  bakers_mat,
  network_threshold
)

# ==============================================================================
# 9. COMMON NETWORK LAYOUT
# ==============================================================================

combined_graph <-
  igraph::union(
    net_c,
    net_b
  )

fixed_layout <-
  layout_with_kk(
    combined_graph
  )

# ==============================================================================
# 10. FIGURE 5
# ==============================================================================

cat("Saving Figure 5...\n")

pdf(
  file.path(
    plot_dir,
    "Figure5_Exploratory_Network_Representation.pdf"
  ),
  width = 18,
  height = 8
)

layout(
  matrix(c(1, 2), nrow = 1)
)

# ------------------------------------------------------------------------------
# PANEL A — COPHENETIC NETWORK
# ------------------------------------------------------------------------------

par(
  mar = c(1, 1, 7, 1)
)

plot(
  net_c,
  layout = fixed_layout,
  edge.width = E(net_c)$weight * 12,
  vertex.color = "#D9EAF7",
  vertex.frame.color = "black",
  vertex.shape = "circle",
  vertex.size = 35,
  vertex.label.color = "black",
  vertex.label.cex = 1.25,
  vertex.label.font = 2,
  edge.color = "gray50",
  edge.curved = 0.15,
  main = paste0(
    "Graph of Links for r > ",
    network_threshold,
    " (Cophenetic)"
  )
)

mtext(
  "A",
  side = 3,
  line = 1,
  adj = 0,
  cex = 3.5,
  font = 2
)

# ------------------------------------------------------------------------------
# PANEL B — BAKER NETWORK
# ------------------------------------------------------------------------------

par(
  mar = c(1, 1, 7, 1)
)

plot(
  net_b,
  layout = fixed_layout,
  edge.width = E(net_b)$weight * 12,
  vertex.color = "#D9EAF7",
  vertex.frame.color = "black",
  vertex.shape = "circle",
  vertex.size = 35,
  vertex.label.color = "black",
  vertex.label.cex = 1.25,
  vertex.label.font = 2,
  edge.color = "gray50",
  edge.curved = 0.15,
  main = paste0(
    "Graph of Links for r > ",
    network_threshold,
    " (Baker)"
  )
)

mtext(
  "B",
  side = 3,
  line = 1,
  adj = 0,
  cex = 3.5,
  font = 2
)

dev.off()

# ==============================================================================
# 11. AGREEMENT TABLE
# ==============================================================================

cat("Creating agreement table...\n")

aux_vars <- c(
  "RAIN",
  "AGE",
  "DEN",
  "PHY",
  "GDP",
  "TEMP",
  "ELEV",
  "LATLONG"
)

agreement_table <- data.frame(
  
  Variable = aux_vars,
  
  Cophenetic_INC = round(
    cophenetic_mat[
      "INC",
      aux_vars
    ],
    3
  ),
  
  Baker_INC = round(
    bakers_mat[
      "INC",
      aux_vars
    ],
    3
  ),
  
  Cophenetic_MOR = round(
    cophenetic_mat[
      "MOR",
      aux_vars
    ],
    3
  ),
  
  Baker_MOR = round(
    bakers_mat[
      "MOR",
      aux_vars
    ],
    3
  )
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

agreement_table <-
  agreement_table %>%
  arrange(
    desc(Cophenetic_INC)
  )

print(agreement_table)

# ==============================================================================
# 12. SAVE AS EXCEL
# ==============================================================================

excel_file <- file.path(
  plot_dir,
  "Table1_Agreement_Rankings.xlsx"
)

write_xlsx(
  agreement_table,
  excel_file
)

cat("\n======================================================================\n")
cat("SUCCESS: Figure 5 and agreement table saved.\n")
cat("======================================================================\n")

cat(
  "Figure:",
  file.path(
    plot_dir,
    "Figure5_Exploratory_Network_Representation.pdf"
  ),
  "\n"
)

cat(
  "Excel:",
  excel_file,
  "\n"
)
