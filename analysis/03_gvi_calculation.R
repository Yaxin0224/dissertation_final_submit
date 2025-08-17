#!/usr/bin/env Rscript
# ============================================
# City Look Dissertation v2
# 03_gvi_calculation.R - Green View Index (GVI) calculation
# 
# Using DeepLabv3+ deep learning model for semantic segmentation
# Compute vegetation proportion in street-view images
# ============================================

# Clear environment
rm(list = ls())
gc()

# ============================================
# 1. Load required packages
# ============================================

# R packages
library(tidyverse)
library(here)
library(reticulate)  # Python interface
library(magick)      # image processing
library(logger)      # logging
library(tictoc)      # timing
library(progress)    # progress bar
library(reticulate)
use_python("/opt/anaconda3/envs/pytorch/bin/python", required = TRUE)


# Set project path
setwd(here())

# Configure logging
log_appender(appender_file(here("logs", paste0("gvi_calculation_", 
                                               format(Sys.Date(), "%Y%m%d"), ".log"))))
log_threshold(INFO)

# ============================================
# 2. Python environment setup
# ============================================

cat("Setting up Python environment...\n")

# Use conda environment (adjust per your system)
# use_condaenv("pytorch", required = TRUE)
# Or use virtualenv
# use_virtualenv("~/venv/pytorch", required = TRUE)

# Import Python modules
torch <- import("torch")
torchvision <- import("torchvision")
PIL <- import("PIL")
np <- import("numpy")
cv2 <- import("cv2")

# Check CUDA availability
cuda_available <- torch$cuda$is_available()
device <- ifelse(cuda_available, "cuda", "cpu")
cat(sprintf("Using device: %s\n", device))
if(cuda_available) {
  cat(sprintf("CUDA device: %s\n", torch$cuda$get_device_name(0L)))
}

# ============================================
# 3. Configuration parameters
# ============================================

# Test parameters
TEST_LSOA <- "E01000882"  # replace with actual LSOA code
TEST_IMAGES <- 20         # number of test images

# Model parameters
MODEL_NAME <- "deeplabv3_resnet101"
BATCH_SIZE <- ifelse(cuda_available, 8L, 2L)  # Batch size 8 on GPU, 2 on CPU

# Vegetation classes (COCO/ADE20K datasets)
# DeepLabv3 classes on COCO
COCO_VEGETATION_CLASSES <- c(
  "potted plant" = 63  # only plant class in COCO
)

# If using ADE20K pretrained model, more vegetation classes
ADE20K_VEGETATION_CLASSES <- c(
  "tree" = 4,
  "grass" = 9,
  "plant" = 17,
  "flower" = 66,
  "bush" = 46
)

# Output path
OUTPUT_DIR <- here("output", "gvi_results")
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

# ============================================
# 4. Load DeepLabv3+ model
# ============================================

cat("\nLoading DeepLabv3+ model...\n")

# Load pretrained model
model <- torchvision$models$segmentation$deeplabv3_resnet101(
  pretrained = TRUE,
  progress = TRUE
)

# Set to evaluation mode
model$eval()

# Move to device
if(cuda_available) {
  model <- model$cuda()
}

# Image preprocessing
preprocess <- torchvision$transforms$Compose(list(
  torchvision$transforms$ToTensor(),
  torchvision$transforms$Normalize(
    mean = c(0.485, 0.456, 0.406),
    std = c(0.229, 0.224, 0.225)
  )
))

cat("Model loaded!\n")

# ============================================
# 5. Helper functions
# ============================================

#' Load and preprocess image
#' @param image_path path to the image file
#' @return preprocessed tensor
load_and_preprocess_image <- function(image_path) {
  # Load image with PIL
  image <- PIL$Image$open(image_path)$convert("RGB")
  
  # Get original size
  original_size <- c(image$size[[1]], image$size[[2]])  # width, height
  
  # Preprocess
  input_tensor <- preprocess(image)
  
  # Add batch dimension
  input_batch <- input_tensor$unsqueeze(0L)
  
  return(list(
    tensor = input_batch,
    original_size = original_size,
    pil_image = image
  ))
}

#' Use color thresholding as a fallback method
#' @param image_path path to the image file
#' @return GVI score
calculate_gvi_color_threshold <- function(image_path) {
  # Read image
  img <- image_read(image_path)
  
  # Convert to array
  img_array <- as.integer(image_data(img))
  
  # Get RGB channels
  if(length(dim(img_array)) == 3) {
    R <- img_array[1,,]
    G <- img_array[2,,]
    B <- img_array[3,,]
  } else if(length(dim(img_array)) == 4) {
    R <- img_array[1,,,1]
    G <- img_array[2,,,1]
    B <- img_array[3,,,1]
  }
  
  # Vegetation detection condition (based on literature)
  # G > R and G > B
  vegetation_mask <- (G > R) & (G > B) & (G > 30)
  
  # Calculate GVI
  total_pixels <- length(vegetation_mask)
  vegetation_pixels <- sum(vegetation_mask)
  gvi <- vegetation_pixels / total_pixels * 100
  
  return(list(
    gvi = gvi,
    vegetation_pixels = vegetation_pixels,
    total_pixels = total_pixels
  ))
}

#' Process single image for GVI
#' @param image_path path to the image
#' @param save_visualization whether to save visualization result
#' @return GVI result
process_single_image <- function(image_path, save_visualization = FALSE) {
  
  tryCatch({
    # Load image
    img_data <- load_and_preprocess_image(image_path)
    
    # Move to device
    if(cuda_available) {
      input_batch <- img_data$tensor$cuda()
    } else {
      input_batch <- img_data$tensor
    }
    
    # Inference (no gradient calculation)
    with(torch$no_grad(), {
      output <- model(input_batch)$'out'$squeeze(0L)
    })
    
    # Get predicted classes
    predictions <- output$argmax(0L)
    
    # Move back to CPU and convert to numpy
    predictions_np <- predictions$cpu()$numpy()
    
    # Calculate vegetation pixels (note: COCO vegetation classes are limited)
    # Here we use a simplified approach; may need to switch to ADE20K model
    vegetation_mask <- predictions_np == 63L  # potted plant class
    
    # If no vegetation detected, try color thresholding
    vegetation_pixels <- sum(vegetation_mask)
    
    if(vegetation_pixels < 100) {  # too few vegetation pixels, may be model limitation
      # Use color thresholding as fallback
      color_result <- calculate_gvi_color_threshold(image_path)
      
      result <- list(
        image_path = image_path,
        method = "color_threshold",
        gvi = color_result$gvi,
        vegetation_pixels = color_result$vegetation_pixels,
        total_pixels = color_result$total_pixels,
        processing_time = NA
      )
    } else {
      # Use deep learning result
      total_pixels <- length(predictions_np)
      gvi <- vegetation_pixels / total_pixels * 100
      
      result <- list(
        image_path = image_path,
        method = "deeplab",
        gvi = gvi,
        vegetation_pixels = vegetation_pixels,
        total_pixels = total_pixels,
        processing_time = NA
      )
    }
    
    # Save visualization (if requested)
    if(save_visualization) {
      save_segmentation_visualization(
        image_path, 
        predictions_np, 
        result$gvi,
        output_dir = OUTPUT_DIR
      )
    }
    
    return(result)
    
  }, error = function(e) {
    log_error(paste("Failed to process image:", image_path, "-", e$message))
    return(list(
      image_path = image_path,
      method = "error",
      gvi = NA,
      vegetation_pixels = NA,
      total_pixels = NA,
      processing_time = NA,
      error = e$message
    ))
  })
}

#' Save segmentation visualization result
save_segmentation_visualization <- function(image_path, predictions, gvi, output_dir) {
  # Create visualization
  # Simplified here; could be made more elaborate
  
  basename <- tools::file_path_sans_ext(basename(image_path))
  output_path <- file.path(output_dir, paste0(basename, "_segmentation.png"))
  
  # Save info file
  info_path <- file.path(output_dir, paste0(basename, "_info.txt"))
  writeLines(c(
    paste("Image:", basename(image_path)),
    paste("GVI:", round(gvi, 2), "%"),
    paste("Method:", ifelse(sum(predictions == 63) > 100, "DeepLab", "Color threshold"))
  ), info_path)
}

# ============================================
# 6. Main processing workflow
# ============================================

cat("\nStarting GVI calculation test...\n")
cat(paste("Test LSOA:", TEST_LSOA, "\n"))

# Get LSOA images directory
lsoa_dir <- here("data", "raw", "mapillary_images", TEST_LSOA)

if(!dir.exists(lsoa_dir)) {
  stop(paste("LSOA directory does not exist:", lsoa_dir))
}

# Get list of images
image_files <- list.files(lsoa_dir, pattern = "\\.jpg$", full.names = TRUE)
cat(paste("Found", length(image_files), "images\n"))

# Select test images
if(length(image_files) > TEST_IMAGES) {
  # Randomly select
  set.seed(123)
  test_images <- sample(image_files, TEST_IMAGES)
} else {
  test_images <- image_files
}

cat(paste("\nProcessing", length(test_images), "test images...\n"))

# Create progress bar
pb <- progress_bar$new(
  format = "Processing [:bar] :percent :elapsed",
  total = length(test_images),
  clear = FALSE
)

# Process images
results <- list()
tic()

for(i in seq_along(test_images)) {
  pb$tick()
  
  # Process single image
  result <- process_single_image(
    test_images[i], 
    save_visualization = (i <= 5)  # only save visualizations for the first 5 images
  )
  
  results[[i]] <- result
}

processing_time <- toc()

# ============================================
# 7. Results aggregation and analysis
# ============================================

cat("\n\nSummarizing results...\n")

# Convert to dataframe
results_df <- bind_rows(results) %>%
  mutate(
    lsoa_code = TEST_LSOA,
    image_id = basename(image_path) %>% str_remove("\\.jpg$")
  )

# Basic statistics
summary_stats <- results_df %>%
  filter(!is.na(gvi)) %>%
  summarise(
    n_images = n(),
    mean_gvi = mean(gvi, na.rm = TRUE),
    median_gvi = median(gvi, na.rm = TRUE),
    sd_gvi = sd(gvi, na.rm = TRUE),
    min_gvi = min(gvi, na.rm = TRUE),
    max_gvi = max(gvi, na.rm = TRUE),
    deeplab_count = sum(method == "deeplab", na.rm = TRUE),
    color_threshold_count = sum(method == "color_threshold", na.rm = TRUE),
    error_count = sum(method == "error", na.rm = TRUE)
  )

# Print results
cat("\n========== GVI Calculation Results ==========\n")
cat(sprintf("LSOA: %s\n", TEST_LSOA))
cat(sprintf("Number of processed images: %d\n", summary_stats$n_images))
cat(sprintf("Mean GVI: %.2f%%\n", summary_stats$mean_gvi))
cat(sprintf("Median GVI: %.2f%%\n", summary_stats$median_gvi))
cat(sprintf("Standard deviation: %.2f\n", summary_stats$sd_gvi))
cat(sprintf("GVI range: %.2f%% - %.2f%%\n", summary_stats$min_gvi, summary_stats$max_gvi))
cat(sprintf("\nMethod usage:\n"))
cat(sprintf("  DeepLab: %d\n", summary_stats$deeplab_count))
cat(sprintf("  Color threshold: %d\n", summary_stats$color_threshold_count))
cat(sprintf("  Errors: %d\n", summary_stats$error_count))
cat("==================================\n")

# ============================================
# 8. Save results
# ============================================

# Save detailed results
output_file <- file.path(OUTPUT_DIR, paste0("gvi_test_", TEST_LSOA, "_", 
                                            format(Sys.Date(), "%Y%m%d"), ".csv"))
write_csv(results_df, output_file)
cat(paste("\nResults saved to:", output_file, "\n"))

# Save summary statistics
summary_file <- file.path(OUTPUT_DIR, paste0("gvi_summary_", TEST_LSOA, "_", 
                                             format(Sys.Date(), "%Y%m%d"), ".csv"))
write_csv(summary_stats, summary_file)

# ============================================
# 9. Create visualizations
# ============================================

library(ggplot2)

# GVI distribution histogram
p1 <- results_df %>%
  filter(!is.na(gvi)) %>%
  ggplot(aes(x = gvi)) +
  geom_histogram(bins = 20, fill = "forestgreen", alpha = 0.7) +
  geom_vline(aes(xintercept = mean(gvi)), color = "red", linetype = "dashed", size = 1) +
  labs(
    title = paste("GVI Distribution -", TEST_LSOA),
    subtitle = paste("Mean GVI:", round(summary_stats$mean_gvi, 2), "%"),
    x = "Green View Index (%)",
    y = "Count"
  ) +
  theme_minimal()

# Save plot
ggsave(file.path(OUTPUT_DIR, paste0("gvi_distribution_", TEST_LSOA, ".png")),
       p1, width = 8, height = 6, dpi = 300)

# ============================================
# 10. Recommendations and next steps
# ============================================

cat("\n========== Test complete ==========\n")
cat("Next steps:\n")
cat("1. Check visualizations in output/gvi_results/\n")
cat("2. Validate the accuracy of the GVI calculation\n")
cat("3. If satisfied, extend to all 20 LSOAs\n")
cat("4. Consider whether to use an ADE20K pretrained model (more vegetation classes)\n")

# Check if improvements are needed
if(summary_stats$color_threshold_count > summary_stats$deeplab_count) {
  cat("\n⚠️ WARNING: Majority of images used color thresholding instead of deep learning\n")
  cat("   May need a model that includes more vegetation classes\n")
}

cat("\nCompletion time:", format(Sys.time()), "\n")

# ============================================
# Appendix: Batch processing function (for future use)
# ============================================

#' Batch process all LSOAs
#' @param lsoa_codes vector of LSOA codes
#' @param images_per_lsoa number of images to process per LSOA
#' @param parallel whether to use parallel processing
process_all_lsoas <- function(lsoa_codes, images_per_lsoa = NULL, parallel = TRUE) {
  # This function can be implemented after testing succeeds
  # Include parallel processing, batch inference, progress monitoring, etc.
  cat("Batch processing function to be implemented...\n")
}

# ============================================
# END OF SCRIPT
# ============================================
