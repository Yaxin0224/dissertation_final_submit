# ============================================
# City Look Dissertation v2
# config.R - Project configuration file
# 
# Stores all project-level configuration parameters
# ============================================

# ============================================
# 1. API configuration
# ============================================

# Mapillary API Token
MAPILLARY_ACCESS_TOKEN <- "MLY|9922859457805691|cef02444f32c339cf09761b104ca4bb5"

# API endpoints
MAPILLARY_API_BASE <- "https://graph.mapillary.com"
MAPILLARY_IMAGE_ENDPOINT <- paste0(MAPILLARY_API_BASE, "/images")

# ============================================
# 2. Study area definition
# ============================================

# Inner London Boroughs
INNER_LONDON_BOROUGHS <- c(
  "Camden",
  "Greenwich", 
  "Hackney",
  "Hammersmith and Fulham",
  "Islington",
  "Kensington and Chelsea",
  "Lambeth",
  "Lewisham",
  "Southwark",
  "Tower Hamlets",
  "Wandsworth",
  "Westminster"
)

# ============================================
# 3. Sampling parameters
# ============================================

# LSOA sampling
N_LSOAS_PER_QUINTILE <- 4
TOTAL_LSOAS <- 20

# Image sampling (updated to actual collection)
TARGET_IMAGES_PER_LSOA <- 100  # actually collected 100 images per LSOA
MIN_IMAGES_PER_LSOA <- 60
MAX_IMAGES_PER_REQUEST <- 500  # request limit used in practice

# Image quality parameters
MIN_IMAGE_WIDTH <- 1024   # minimum width used in practice
MIN_IMAGE_HEIGHT <- 768   # minimum height used in practice

# Time range (updated to actual data coverage)
DATE_START <- "2014-01-01"  # earliest data in practice starts in 2014
DATE_END <- "2025-12-31"    # data includes 2025
# Note: although ranges are set, newer images are preferred in practice

# ============================================
# 4. Spatial parameters
# ============================================

# Search parameters
MIN_IMAGE_DISTANCE <- 20    # minimum distance between images (meters)
SEARCH_EXPANSION <- 0.2     # search expansion factor (20%)

# Coordinate systems
WGS84_EPSG <- 4326          # geographic coordinates (Mapillary uses)
BNG_EPSG <- 27700           # British National Grid (distance calculations)

# ============================================
# 5. GVI calculation parameters
# ============================================

# Semantic segmentation model
SEGMENTATION_MODEL <- "segformer" 

# Vegetation class definitions (adjust per model)
VEGETATION_CLASSES <- c(
  "tree",
  "grass", 
  "plant",
  "vegetation",
  "bush",
  "shrub"
)

# GVI calculation settings
GVI_THRESHOLD <- 0.01  # minimum GVI value (1%)
BATCH_SIZE <- 32       # batch size

# ============================================
# 6. File paths
# ============================================

# Use relative paths
PATHS <- list(
  # raw data
  imd_data = "data/raw/IMD 2019/IMD2019_London.xlsx",
  lsoa_boundaries_dir = "data/raw/LB_LSOA2021_shp/LB_shp/",
  
  # processed data
  selected_lsoas = "data/processed/selected_lsoas.csv",
  streetview_metadata = "data/processed/streetview_metadata_final.csv",  # final metadata
  download_summary = "data/processed/download_summary.csv",
  gvi_results = "data/processed/gvi_results.csv",
  
  # image storage
  mapillary_images = "data/raw/mapillary_images/",
  
  # outputs
  gvi_outputs = "output/gvi_results/",
  figures = "figures/",
  reports = "reports/",
  logs = "logs/"
)

# ============================================
# 7. Analysis parameters
# ============================================

# Statistical analysis
SIGNIFICANCE_LEVEL <- 0.05
CONFIDENCE_LEVEL <- 0.95

# Spatial analysis
SPATIAL_LAG_DISTANCE <- 500  # meters
MORAN_I_PERMUTATIONS <- 999

# ============================================
# 8. Visualization parameters
# ============================================

# Color schemes
COLOR_SCHEMES <- list(
  imd_quintiles = c("#2166ac", "#67a9cf", "#f7f7f7", "#fddbc7", "#b2182b"),
  gvi_gradient = c("#d73027", "#fc8d59", "#fee08b", "#d9ef8b", "#91cf60", "#1a9850"),
  borough = "Set3"
)

# Map settings
MAP_SETTINGS <- list(
  width = 10,
  height = 8,
  dpi = 300,
  format = "png"
)

# ============================================
# 9. System settings
# ============================================

# Parallel processing
USE_PARALLEL <- TRUE
N_CORES <- parallel::detectCores() - 1  # leave one core free

# Progress bar
SHOW_PROGRESS <- TRUE

# Logging level
LOG_LEVEL <- "INFO"  # DEBUG, INFO, WARNING, ERROR

# API rate limiting
API_DELAY <- 0.1  # delay between requests (seconds)

# ============================================
# 10. Project statistics (actual completion)
# ============================================

PROJECT_STATS <- list(
  total_lsoas = 20,
  images_per_lsoa = 100,
  total_images = 2000,
  collection_date = "2025-08-01",
  boroughs_covered = 12
)

# ============================================
# 11. Helper functions
# ============================================

# Check configuration integrity
check_config <- function() {
  issues <- c()
  
  # Ensure required directories exist
  required_dirs <- c("data", "data/raw", "data/processed", "output", "figures", "reports", "logs")
  for(dir in required_dirs) {
    if(!dir.exists(here::here(dir))) {
      dir.create(here::here(dir), recursive = TRUE, showWarnings = FALSE)
    }
  }
  
  # Check key files
  if(!file.exists(here::here(PATHS$selected_lsoas))) {
    issues <- c(issues, "selected_lsoas.csv not found")
  }
  
  if(!file.exists(here::here(PATHS$streetview_metadata))) {
    issues <- c(issues, "streetview_metadata_final.csv not found")
  }
  
  # Check image folder
  if(!dir.exists(here::here(PATHS$mapillary_images))) {
    issues <- c(issues, "mapillary_images folder not found")
  }
  
  if(length(issues) > 0) {
    cat("Configuration check found the following issues:\n")
    for(issue in issues) {
      cat("  -", issue, "\n")
    }
    return(FALSE)
  } else {
    cat("Configuration check passed!\n")
    return(TRUE)
  }
}

# Get configuration summary
get_config_summary <- function() {
  cat("=== City Look Project Configuration Summary ===\n")
  cat("Study area: Inner London (", length(INNER_LONDON_BOROUGHS), " boroughs)\n")
  cat("LSOA sample:", TOTAL_LSOAS, "LSOAs (", N_LSOAS_PER_QUINTILE, " per IMD quintile)\n")
  cat("Target images:", TARGET_IMAGES_PER_LSOA, "per LSOA\n")
  cat("Time range:", DATE_START, "to", DATE_END, "\n")
  cat("=============================\n")
}

# Run configuration check
if(interactive()) {
  check_config()
  get_config_summary()
}
