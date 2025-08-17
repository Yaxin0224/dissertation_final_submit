# ============================================
# City Look Dissertation v2
# 01_lsoa_selection.R - Inner London LSOA selection
# 
# Purpose: Select 20 LSOAs from Inner London for analysis
# Method: Stratified random sampling based on IMD, ensuring spatial dispersion
# Output: selected_lsoas.csv - final chosen 20 LSOAs
# ============================================

# Clear environment
rm(list = ls())

# Load required packages
library(tidyverse)
library(readxl)
library(sf)
library(tmap)

# Set random seed for reproducibility
set.seed(20250114)

# ============================================
# 1. Configuration parameters
# ============================================

# Definition of Inner London boroughs
INNER_LONDON_BOROUGHS <- c(
  "Camden", "Greenwich", "Hackney", 
  "Hammersmith and Fulham", "Islington", 
  "Kensington and Chelsea", "Lambeth", 
  "Lewisham", "Southwark", "Tower Hamlets", 
  "Wandsworth", "Westminster"
)

# Sampling parameters
N_LSOAS_PER_QUINTILE <- 4
TOTAL_LSOAS <- 20

# File paths (use relative paths)
IMD_FILE <- "data/raw/IMD 2019/IMD2019_London.xlsx"
LSOA_BOUNDARIES_DIR <- "data/raw/LB_LSOA2021_shp/LB_shp/"
OUTPUT_DIR <- "data/processed/"

# ============================================
# 2. Read and clean IMD data
# ============================================

cat("1. Reading IMD data...\n")

# Read IMD data
imd_data <- read_excel(IMD_FILE, sheet = "IMD 2019")

# Rename columns
imd_clean <- imd_data %>%
  rename(
    lsoa_code = `LSOA code (2011)`,
    lsoa_name = `LSOA name (2011)`,
    la_code = `Local Authority District code (2019)`,
    borough = `Local Authority District name (2019)`,
    imd_score = `Index of Multiple Deprivation (IMD) Score`,
    imd_rank = `Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)`,
    imd_decile = `Index of Multiple Deprivation (IMD) Decile (where 1 is most deprived 10% of LSOAs)`
  )

# Check the correct name for Kensington
kensington_variants <- unique(imd_clean$borough)[grep("Kensington", unique(imd_clean$borough))]
if(length(kensington_variants) > 0) {
  actual_kensington_name <- kensington_variants[1]
  INNER_LONDON_BOROUGHS[INNER_LONDON_BOROUGHS == "Kensington and Chelsea"] <- actual_kensington_name
  cat("  - Actual name for Kensington:", actual_kensington_name, "\n")
}

cat("  - IMD data contains", nrow(imd_clean), "LSOAs\n")

# ============================================
# 3. Filter Inner London LSOAs
# ============================================

cat("\n2. Filtering Inner London LSOAs...\n")

# Filter Inner London (exclude City of London)
inner_london_lsoas <- imd_clean %>%
  filter(
    borough %in% INNER_LONDON_BOROUGHS,
    borough != "City of London"
  )

cat("  - Inner London contains", nrow(inner_london_lsoas), "LSOAs\n")
cat("  - Covers", length(unique(inner_london_lsoas$borough)), "boroughs\n")

# ============================================
# 4. Read LSOA boundaries and validate
# ============================================

cat("\n3. Reading and validating LSOA boundary data...\n")

# Read and combine all borough LSOA boundary shapefiles
shp_files <- list.files(LSOA_BOUNDARIES_DIR, pattern = "\\.shp$", full.names = TRUE)

# Only read borough files for Inner London
inner_london_patterns <- gsub(" ", ".", INNER_LONDON_BOROUGHS)
inner_london_patterns <- paste0("(", paste(inner_london_patterns, collapse = "|"), ")")
relevant_shp_files <- shp_files[grepl(inner_london_patterns, shp_files, ignore.case = TRUE)]

# Read and combine
lsoa_boundaries <- NULL
for(shp_file in relevant_shp_files) {
  tryCatch({
    borough_data <- st_read(shp_file, quiet = TRUE)
    if(is.null(lsoa_boundaries)) {
      lsoa_boundaries <- borough_data
    } else {
      common_cols <- intersect(names(lsoa_boundaries), names(borough_data))
      lsoa_boundaries <- rbind(
        lsoa_boundaries[, common_cols],
        borough_data[, common_cols]
      )
    }
  }, error = function(e) {
    cat("    Warning: Unable to read", basename(shp_file), "\n")
  })
}

cat("  - Successfully read", nrow(lsoa_boundaries), "LSOA boundaries\n")

# Get LSOA codes present in the boundary file
available_lsoa_codes <- unique(lsoa_boundaries$lsoa21cd)

# ============================================
# 5. Compute Inner London-specific IMD quintiles
# ============================================

cat("\n4. Calculating IMD quintiles specific to Inner London...\n")

# Keep only LSOAs that exist in the boundaries file
inner_london_with_boundaries <- inner_london_lsoas %>%
  filter(lsoa_code %in% available_lsoa_codes)

cat("  - Number of LSOAs with boundary data:", nrow(inner_london_with_boundaries), "\n")

# Calculate quintiles
inner_london_quintiles <- inner_london_with_boundaries %>%
  mutate(
    inner_imd_quintile = ntile(imd_score, 5),
    deprivation_category = case_when(
      inner_imd_quintile == 1 ~ "1_Least Deprived (Inner London)",
      inner_imd_quintile == 2 ~ "2_Less Deprived (Inner London)",
      inner_imd_quintile == 3 ~ "3_Average (Inner London)",
      inner_imd_quintile == 4 ~ "4_More Deprived (Inner London)",
      inner_imd_quintile == 5 ~ "5_Most Deprived (Inner London)"
    )
  )

# Show quintile distribution
quintile_summary <- inner_london_quintiles %>%
  group_by(inner_imd_quintile, deprivation_category) %>%
  summarise(
    n_lsoas = n(),
    min_imd = min(imd_score),
    max_imd = max(imd_score),
    mean_imd = mean(imd_score),
    .groups = 'drop'
  )

print(quintile_summary)

# ============================================
# 6. Stratified random sampling (ensure spatial dispersion)
# ============================================

cat("\n5. Performing stratified random sampling...\n")

# Define spatially-dispersed sampling function
stratified_spatial_sampling <- function(data, n_per_group = 4) {
  
  selected_lsoas <- data %>%
    group_by(inner_imd_quintile) %>%
    group_modify(~ {
      quintile_data <- .x
      n_boroughs <- length(unique(quintile_data$borough))
      
      # Strategy 1: If enough boroughs, prioritize selecting from different boroughs
      if(n_boroughs >= n_per_group) {
        # First randomly select 1 from each borough
        borough_sample <- quintile_data %>%
          group_by(borough) %>%
          sample_n(1) %>%
          ungroup()
        
        # If fewer than n_per_group, fill remaining randomly
        if(nrow(borough_sample) < n_per_group) {
          remaining <- quintile_data %>%
            anti_join(borough_sample, by = "lsoa_code") %>%
            sample_n(n_per_group - nrow(borough_sample))
          
          borough_sample <- bind_rows(borough_sample, remaining)
        } else {
          # If more than n_per_group, randomly pick n_per_group
          borough_sample <- borough_sample %>%
            sample_n(n_per_group)
        }
        
        return(borough_sample)
        
      } else {
        # Strategy 2: If not enough boroughs, sample randomly
        # But try to ensure different boroughs when possible
        selected <- data.frame()
        remaining_data <- quintile_data
        
        # First select one from each borough
        for(b in unique(quintile_data$borough)) {
          borough_lsoas <- remaining_data %>% filter(borough == b)
          if(nrow(borough_lsoas) > 0 && nrow(selected) < n_per_group) {
            selected <- bind_rows(selected, borough_lsoas %>% sample_n(1))
            remaining_data <- remaining_data %>% 
              filter(lsoa_code != selected$lsoa_code[nrow(selected)])
          }
        }
        
        # If still not enough, randomly select from the remainder
        if(nrow(selected) < n_per_group && nrow(remaining_data) > 0) {
          n_needed <- n_per_group - nrow(selected)
          additional <- remaining_data %>% sample_n(min(n_needed, nrow(remaining_data)))
          selected <- bind_rows(selected, additional)
        }
        
        return(selected)
      }
    }) %>%
    ungroup()
  
  return(selected_lsoas)
}

# Execute sampling
selected_lsoas <- stratified_spatial_sampling(inner_london_quintiles, N_LSOAS_PER_QUINTILE)

# ============================================
# 7. Validate sampling results
# ============================================

cat("\n6. Validating sampling results...\n")

# Check sample counts per quintile
quintile_check <- selected_lsoas %>%
  count(inner_imd_quintile, deprivation_category)
print(quintile_check)

# Check borough distribution
borough_distribution <- selected_lsoas %>%
  count(borough, inner_imd_quintile) %>%
  pivot_wider(names_from = inner_imd_quintile, values_from = n, values_fill = 0)

cat("\nBorough distribution matrix:\n")
print(borough_distribution)

# Count number of covered boroughs
n_boroughs_covered <- length(unique(selected_lsoas$borough))
cat("\nCovered", n_boroughs_covered, "different boroughs\n")

# ============================================
# 8. Add extra info and save
# ============================================

# Add selection order and other useful info
selected_lsoas_final <- selected_lsoas %>%
  arrange(inner_imd_quintile, lsoa_code) %>%
  mutate(
    selection_id = row_number(),
    selection_label = paste0("LSOA_", str_pad(selection_id, 2, pad = "0"))
  ) %>%
  select(
    selection_id,
    selection_label,
    lsoa_code,
    lsoa_name,
    borough,
    inner_imd_quintile,
    deprivation_category,
    imd_score,
    imd_rank,
    everything()
  )

cat("\n7. Saving results...\n")

# Create output directory (if it doesn't exist)
if(!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
}

# Save as CSV (this is the final file)
output_file <- file.path(OUTPUT_DIR, "selected_lsoas.csv")
write_csv(selected_lsoas_final, output_file)
cat("  - Saved to:", output_file, "\n")

# Save detailed report
report_file <- file.path(OUTPUT_DIR, "lsoa_selection_report.txt")
sink(report_file)
cat("City Look Dissertation - LSOA Selection Report\n")
cat("Generated at:", Sys.time(), "\n")
cat("=====================================\n\n")

cat("1. Overall statistics\n")
cat("   - Total Inner London LSOAs:", nrow(inner_london_lsoas), "\n")
cat("   - LSOAs with boundary data:", nrow(inner_london_with_boundaries), "\n")
cat("   - Number of selected LSOAs:", nrow(selected_lsoas_final), "\n")
cat("   - Number of boroughs covered:", n_boroughs_covered, "\n\n")

cat("2. IMD quintile distribution\n")
print(quintile_check)
cat("\n")

cat("3. Borough distribution\n")
print(borough_distribution)
cat("\n")

cat("4. List of selected LSOAs\n")
selected_lsoas_final %>%
  select(selection_id, lsoa_code, borough, deprivation_category, imd_score) %>%
  print(n = 20)

sink()
cat("  - Report saved to:", report_file, "\n")

# ============================================
# 9. Create visualisation
# ============================================

if(!is.null(lsoa_boundaries)) {
  cat("\n8. Creating map visualization...\n")
  
  # Join selected LSOAs with boundary data
  selected_boundaries <- lsoa_boundaries %>%
    inner_join(selected_lsoas_final, by = c("lsoa21cd" = "lsoa_code"))
  
  # Create map
  tmap_mode("plot")
  
  # Thematic map
  map_selected <- tm_shape(lsoa_boundaries %>% 
                             filter(lsoa21cd %in% inner_london_with_boundaries$lsoa_code)) +
    tm_polygons(col = "grey90", border.col = "grey80", lwd = 0.5) +
    tm_shape(selected_boundaries) +
    tm_polygons(
      col = "deprivation_category",
      palette = c("#2166ac", "#67a9cf", "#f7f7f7", "#fddbc7", "#b2182b"),
      title = "IMD Quintile",
      border.col = "black",
      lwd = 1.5
    ) +
    tm_layout(
      title = "Selected LSOAs in Inner London",
      frame = FALSE,
      legend.outside = TRUE
    )
  
  # Save map
  map_file <- file.path("figures", "selected_lsoas_map.png")
  if(!dir.exists("figures")) dir.create("figures")
  tmap_save(map_selected, map_file, width = 10, height = 8, dpi = 300)
  cat("  - Map saved to:", map_file, "\n")
  
  # Verify number of LSOAs in the map
  cat("  - Number of LSOAs shown on the map:", nrow(selected_boundaries), "\n")
  
} else {
  cat("\nNote: LSOA boundary data did not load correctly\n")
}

# ============================================
# 10. Done
# ============================================

cat("\n========== LSOA selection complete ==========\n")
cat("Successfully selected", nrow(selected_lsoas_final), "LSOAs\n")
cat("Next step: run 02_streetview_download.R to download Street View images\n")

# Show final summary
cat("\nSummary of selected LSOAs:\n")
selected_lsoas_final %>%
  group_by(deprivation_category) %>%
  summarise(
    count = n(),
    boroughs = paste(unique(borough), collapse = ", "),
    .groups = 'drop'
  ) %>%
  print()
