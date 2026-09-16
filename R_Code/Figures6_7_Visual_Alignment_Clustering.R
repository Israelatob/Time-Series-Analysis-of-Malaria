# ==============================================================================
# FIGURES 6 AND 7
# Visual Alignment of Hierarchical Clustering
#
# Figure 6: Malaria Incidence vs Temperature
# Figure 7: Malaria Death Rate vs Rainfall
#
# Outputs:
#   Figure6_Visual_Alignment_INC_TEMP.pdf
#   Figure7_Visual_Alignment_MOR_RAIN.pdf
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
  library(dplyr)
  library(countrycode)
})

# ==============================================================================
# 2. SETTINGS & PATHS
# ==============================================================================

data_dir <- "C:/Users/17034/Downloads/GSU PhD/Prof. Kirpich/Datasets/New datasets/Malaria/Final/Bakers&Cophentic"

setwd(data_dir)

plot_dir <- file.path(
  data_dir,
  "Plots"
)

if (!dir.exists(plot_dir)) {
  dir.create(plot_dir)
}

hclust_method <- "average"

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
    sapply(
      df,
      function(x)
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
    !duplicated(
      df$CleanedCountryTmp
    ),
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
    setdiff(
      labels(d1),
      common
    )
  )
  
  d2p <- prune(
    d2,
    setdiff(
      labels(d2),
      common
    )
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
          mal[
            ,
            c(
              "country",
              "incidence_rate"
            )
          ]
        )
    }
    
    if ("death_rate" %in% names(mal)) {
      
      all_objs$MOR <-
        build_hclust(
          mal[
            ,
            c(
              "country",
              "death_rate"
            )
          ]
        )
    }
  }
}

# ==============================================================================
# 5. LOAD AUXILIARY DATA
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

cat(
  "Successfully loaded clusters:",
  paste(names(dends), collapse = ", "),
  "\n"
)

# ==============================================================================
# 7. PUBLICATION LABELS
# ==============================================================================

pretty_names <- c(
  
  INC     = "Average Malaria Incidence Rate",
  MOR     = "Average Malaria Death Rate",
  RAIN    = "Median Annual Rainfall",
  AGE     = "Median Population Age",
  DEN     = "Population Density",
  PHY     = "Physicians per 1000 People",
  GDP     = "GDP per Capita",
  TEMP    = "Median Annual Temperature",
  ELEV    = "Mean Elevation",
  LATLONG = "Average Latitude"
)

# ==============================================================================
# 8. TANGLEGRAM FUNCTION
# ==============================================================================

save_tanglegram <- function(
    left_var,
    right_var,
    fig_num,
    outfile_name
) {
  
  if (!(left_var %in% names(dends))) {
    
    warning(
      "Missing dendrogram: ",
      left_var
    )
    
    return(NULL)
  }
  
  if (!(right_var %in% names(dends))) {
    
    warning(
      "Missing dendrogram: ",
      right_var
    )
    
    return(NULL)
  }
  
  aligned <- align_dends(
    dends[[left_var]],
    dends[[right_var]]
  )
  
  if (is.null(aligned)) {
    
    warning(
      "Insufficient common countries for ",
      left_var,
      " vs ",
      right_var
    )
    
    return(NULL)
  }
  
  cat(
    "Saving Figure ",
    fig_num,
    ": ",
    left_var,
    " vs ",
    right_var,
    "\n"
  )
  
  # --------------------------------------------------------------------------
  # COLOR AND FORMAT DENDROGRAMS
  # --------------------------------------------------------------------------
  
  d1 <- color_branches(
    aligned[[1]],
    k = 5
  )
  
  d2 <- color_branches(
    aligned[[2]],
    k = 5
  )
  
  d1 <- color_labels(
    d1,
    k = 5
  )
  
  d2 <- color_labels(
    d2,
    k = 5
  )
  
  d1 <- set(
    d1,
    "branches_lty",
    1
  )
  
  d2 <- set(
    d2,
    "branches_lty",
    1
  )
  
  dl <- dendlist(
    d1,
    d2
  )
  
  # --------------------------------------------------------------------------
  # CALCULATE COPHENETIC CORRELATION
  # --------------------------------------------------------------------------
  
  temp_dl <-
    dendlist(
      aligned[[1]],
      aligned[[2]]
    ) %>%
    untangle(
      method = "step2side"
    )
  
  cophenetic_value <-
    cor_cophenetic(
      temp_dl[[1]],
      temp_dl[[2]]
    )
  
  # --------------------------------------------------------------------------
  # OUTPUT
  # --------------------------------------------------------------------------
  
  outfile <- file.path(
    plot_dir,
    outfile_name
  )
  
  pdf(
    outfile,
    width = 16,
    height = 10
  )
  
  par(
    font = 2,
    mar = c(
      3,
      3,
      6,
      3
    )
  )
  
  tanglegram(
    
    dl,
    
    lab.cex = 1.2,
    
    lwd = 2.2,
    edge.lwd = 2.2,
    
    margin_inner = 14,
    
    common_subtrees_color_lines = TRUE,
    highlight_distinct_edges = TRUE,
    
    sort = TRUE,
    
    main_left =
      pretty_names[left_var],
    
    main_right =
      pretty_names[right_var],
    
    sub =
      paste0(
        "Cophenetic Correlation = ",
        round(
          cophenetic_value,
          3
        )
      )
  )
  
  dev.off()
  
  cat(
    "Saved:",
    outfile,
    "\n"
  )
}

# ==============================================================================
# 9. FIGURE 6
# MALARIA INCIDENCE VS TEMPERATURE
# ==============================================================================

save_tanglegram(
  
  left_var = "INC",
  
  right_var = "TEMP",
  
  fig_num = 6,
  
  outfile_name =
    "Figure6_Visual_Alignment_INC_TEMP.pdf"
)

# ==============================================================================
# 10. FIGURE 7
# MALARIA DEATH RATE VS RAINFALL
# ==============================================================================

save_tanglegram(
  
  left_var = "MOR",
  
  right_var = "RAIN",
  
  fig_num = 7,
  
  outfile_name =
    "Figure7_Visual_Alignment_MOR_RAIN.pdf"
)

# ==============================================================================
# COMPLETED
# ==============================================================================

cat("\n======================================================================\n")
cat("SUCCESS: Figures 6 and 7 saved.\n")
cat("======================================================================\n")