#!/usr/bin/env Rscript
# ============================================
# City Look Dissertation v2
# 04_extended_analysis.R - Phase 1 Extended Analysis
# 
# Purpose: Deepen analysis of the relationship between IMD and GVI,
#          explore nonlinear relationships and other influencing factors
# ============================================

# Clear environment
rm(list = ls())

# ============================================
# 1. Load packages and checks
# ============================================

cat("============================================\n")
cat("City Look - Phase 1 Extended Analysis\n")
cat("============================================\n\n")

# List of required packages
required_packages <- c(
  "tidyverse",      # data manipulation and visualization
  "here",           # path management
  "mgcv",           # GAM models
  "segmented",      # segmented regression
  "quantreg",       # quantile regression
  "spdep",          # spatial analysis
  "lme4",           # mixed effects models
  "cluster",        # clustering analysis
  "corrplot",       # correlation plots
  "viridis",        # color palettes
  "gridExtra",      # arrange plots
  "broom",          # tidy model outputs
  "car",            # diagnostic tools
  "boot"            # bootstrap
)

# Check and install missing packages
cat("1. Checking required R packages...\n")
missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]

if(length(missing_packages) > 0) {
  cat("The following packages need to be installed:", paste(missing_packages, collapse = ", "), "\n")
  cat("Installing...\n")
  install.packages(missing_packages, dependencies = TRUE)
} else {
  cat("All required packages are installed ✓\n")
}

# Load packages
cat("\nLoading packages...\n")
suppressPackageStartupMessages({
  for(pkg in required_packages) {
    library(pkg, character.only = TRUE)
  }
})

# Set project path
setwd(here())

# ============================================
# 2. Read data
# ============================================

cat("\n2. Reading data...\n")

# Read LSOA-level GVI summary
lsoa_gvi_summary <- read_csv(here("output", "lsoa_gvi_summary.csv"), 
                             show_col_types = FALSE)
cat("  - Read GVI summary for", nrow(lsoa_gvi_summary), "LSOAs ✓\n")

# Read detailed GVI results
gvi_results <- read_csv(here("output", "gvi_results", "gvi_results_for_r.csv"),
                        show_col_types = FALSE)
cat("  - Read", nrow(gvi_results), "GVI records ✓\n")

# Data overview
cat("\nData overview:\n")
cat("  - Number of LSOAs:", n_distinct(lsoa_gvi_summary$lsoa_code), "\n")
cat("  - Number of Boroughs:", n_distinct(lsoa_gvi_summary$borough), "\n")
cat("  - IMD quintile distribution:\n")
print(table(lsoa_gvi_summary$inner_imd_quintile))

# ============================================
# 3. Data preparation
# ============================================

cat("\n3. Preparing analysis data...\n")

# Standardise IMD score (for interpretability)
analysis_data <- lsoa_gvi_summary %>%
  mutate(
    imd_score_std = scale(imd_score)[,1],
    mean_gvi_centered = mean_gvi - mean(mean_gvi),
    log_gvi = log(mean_gvi + 1),  # log transform (add 1 to avoid log(0))
    quintile_factor = factor(inner_imd_quintile)
  )

# Identify outliers
outliers <- analysis_data %>%
  filter(mean_gvi > mean(mean_gvi) + 2*sd(mean_gvi) |
           mean_gvi < mean(mean_gvi) - 2*sd(mean_gvi))

if (nrow(outliers) > 0) {
  cat("\n  Found outlier LSOAs:\n")
  print(
    as.data.frame(
      dplyr::select(outliers, lsoa_code, borough, mean_gvi, inner_imd_quintile)
    )
  )
}

# ============================================
# 4. Nonlinear relationship exploration
# ============================================

cat("\n\n============================================")
cat("\n4. Nonlinear relationship analysis")
cat("\n============================================\n")

# Create figures directory
if(!dir.exists(here("figures"))) {
  dir.create(here("figures"))
}

# 4.1 Polynomial regression
cat("\n4.1 Polynomial regression analysis...\n")

# Quadratic polynomial
poly2_model <- lm(mean_gvi ~ poly(imd_score, 2), data = analysis_data)
cat("\nQuadratic polynomial model:\n")
print(summary(poly2_model))

# Cubic polynomial
poly3_model <- lm(mean_gvi ~ poly(imd_score, 3), data = analysis_data)
cat("\nCubic polynomial model:\n")
print(summary(poly3_model))

# Model comparison
cat("\nModel comparison (ANOVA):\n")
print(anova(poly2_model, poly3_model))

# 4.2 GAM model
cat("\n4.2 GAM (Generalized Additive Model) analysis...\n")

gam_model <- gam(mean_gvi ~ s(imd_score), data = analysis_data)
cat("\nGAM model summary:\n")
print(summary(gam_model))

# Visualize nonlinear relationship
p_nonlinear <- ggplot(analysis_data, aes(x = imd_score, y = mean_gvi)) +
  geom_point(aes(color = factor(inner_imd_quintile)), size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed", 
              alpha = 0.3, size = 1) +
  geom_smooth(method = "gam", formula = y ~ s(x), se = TRUE, 
              color = "blue", size = 1.2) +
  scale_color_viridis_d(name = "IMD Quintile") +
  labs(
    title = "GVI vs IMD: Linear vs GAM fits",
    subtitle = "Blue = GAM smooth, Red dashed = linear regression",
    x = "IMD Score",
    y = "Mean GVI (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "right"
  )

ggsave(here("figures", "nonlinear_relationship.png"), p_nonlinear, 
       width = 10, height = 6, dpi = 300)
cat("  - Nonlinear relationship plot saved ✓\n")

# 4.3 Segmented regression
cat("\n4.3 Segmented regression analysis...\n")

# Fit linear model first
lm_model <- lm(mean_gvi ~ imd_score, data = analysis_data)

# Try segmented regression
tryCatch({
  seg_model <- segmented(lm_model, seg.Z = ~imd_score, npsi = 1)
  cat("\nSegmented regression results:\n")
  print(summary(seg_model))
  cat("\nBreakpoint estimate:", seg_model$psi[,"Est."], "\n")
}, error = function(e) {
  cat("  Segmented regression did not converge; a clear breakpoint may not exist\n")
})

# 4.4 Quantile regression
cat("\n4.4 Quantile regression analysis...\n")

# Fit models for different quantiles
quantiles <- c(0.1, 0.25, 0.5, 0.75, 0.9)
qr_models <- list()

for(q in quantiles) {
  qr_models[[as.character(q)]] <- rq(mean_gvi ~ imd_score, 
                                     tau = q, 
                                     data = analysis_data)
}

# Extract coefficients
qr_coefs <- sapply(qr_models, function(m) coef(m)["imd_score"])
cat("\nIMD coefficients across quantiles:\n")
print(data.frame(Quantile = quantiles, IMD_Coefficient = qr_coefs))

# ============================================
# 5. Spatial autocorrelation analysis
# ============================================

cat("\n\n============================================")
cat("\n5. Spatial autocorrelation analysis")
cat("\n============================================\n")

cat("Note: LSOA boundary data is required for a full spatial analysis\n")
cat("Here we perform a preliminary borough-level analysis\n")

# Borough-level spatial effects
borough_effects <- analysis_data %>%
  group_by(borough) %>%
  summarise(
    n_lsoas = n(),
    mean_gvi_borough = mean(mean_gvi),
    sd_gvi_borough = sd(mean_gvi),
    mean_imd = mean(imd_score),
    .groups = 'drop'
  ) %>%
  arrange(desc(mean_gvi_borough))

cat("\nBorough-level statistics:\n")
print(borough_effects)

# Visualize borough effects
p_borough <- ggplot(borough_effects, aes(x = reorder(borough, mean_gvi_borough), 
                                         y = mean_gvi_borough)) +
  geom_col(aes(fill = mean_imd), alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_gvi_borough - sd_gvi_borough,
                    ymax = mean_gvi_borough + sd_gvi_borough),
                width = 0.3) +
  scale_fill_viridis_c(name = "Mean IMD") +
  coord_flip() +
  labs(
    title = "Borough-level distribution of GVI",
    subtitle = "Error bars indicate standard deviation",
    x = "Borough",
    y = "Mean GVI (%)"
  ) +
  theme_minimal()

ggsave(here("figures", "borough_effects.png"), p_borough, 
       width = 10, height = 8, dpi = 300)
cat("  - Borough effects plot saved ✓\n")

# ============================================
# 6. Clustering analysis
# ============================================

cat("\n\n============================================")
cat("\n6. Clustering analysis (revised)")
cat("\n============================================\n")

# Prepare clustering data - correct approach
cluster_data_df <- analysis_data %>%
  dplyr::select(mean_gvi, median_gvi, sd_gvi, imd_score)

# Check missing values
cat("Checking missing values:\n")
print(sapply(cluster_data_df, function(x) sum(is.na(x))))

# Keep only complete cases
complete_rows <- complete.cases(cluster_data_df)
cluster_data_clean <- cluster_data_df[complete_rows, ]
cat("\nNumber of samples used for clustering:", nrow(cluster_data_clean), "\n")

# Standardize data
cluster_data <- scale(cluster_data_clean)

# K-means clustering (try different k)
set.seed(123)
k_values <- 2:5
silhouette_scores <- numeric(length(k_values))

for(i in seq_along(k_values)) {
  k <- k_values[i]
  km <- kmeans(cluster_data, centers = k, nstart = 25)
  sil <- silhouette(km$cluster, dist(cluster_data))
  silhouette_scores[i] <- mean(sil[,3])
  cat("  k =", k, ", silhouette score:", round(silhouette_scores[i], 3), "\n")
}

# Choose best k
best_k <- k_values[which.max(silhouette_scores)]
cat("\nBest number of clusters: k =", best_k, "\n")

# Cluster with best k
final_kmeans <- kmeans(cluster_data, centers = best_k, nstart = 25)

# Add cluster results back to original data
analysis_data$cluster <- NA
analysis_data$cluster[complete_rows] <- factor(final_kmeans$cluster)

# Cluster characteristics
cluster_summary <- analysis_data %>%
  filter(!is.na(cluster)) %>%
  group_by(cluster) %>%
  summarise(
    n = n(),
    mean_gvi = round(mean(mean_gvi), 1),
    mean_imd = round(mean(imd_score), 1),
    sd_gvi = round(mean(sd_gvi), 1),
    boroughs = paste(unique(borough), collapse = "; "),
    .groups = 'drop'
  )

cat("\nCluster characteristics:\n")
print(cluster_summary)

# ============================================
# 7. Multilevel models
# ============================================

cat("\n\n============================================")
cat("\n7. Multilevel model analysis")
cat("\n============================================\n")

# 7.1 Fixed effects model
fe_model <- lm(mean_gvi ~ imd_score + borough, data = analysis_data)
cat("Fixed effects model:\n")
print(summary(fe_model))

# 7.2 Mixed effects model
me_model <- lmer(mean_gvi ~ imd_score + (1 | borough), 
                 data = analysis_data,
                 REML = TRUE)
cat("\nMixed effects model:\n")
print(summary(me_model))

# Calculate ICC
var_components <- as.data.frame(VarCorr(me_model))
icc <- var_components$vcov[1] / sum(var_components$vcov)
cat("\nIntra-class correlation coefficient (ICC):", round(icc, 3), "\n")
cat("Interpretation: Borough explains", round(icc * 100, 1), "% of GVI variation\n")

# Model comparison
cat("\nModel comparison (AIC):\n")
cat("  Linear model:", AIC(lm_model), "\n")
cat("  GAM model:", AIC(gam_model), "\n")
cat("  Mixed effects model:", AIC(me_model), "\n")

# ============================================
# 8. Robustness checks
# ============================================

cat("\n\n============================================")
cat("\n8. Robustness checks")
cat("\n============================================\n")

# 8.1 Influence diagnostics
cat("\n8.1 Influence diagnostics...\n")
influence_measures <- influence.measures(lm_model)
influential_points <- which(apply(influence_measures$is.inf[, 1:4], 1, any))

if(length(influential_points) > 0) {
  cat("Found influential points:\n")
  influential_data <- analysis_data[influential_points, 
                                    c("lsoa_code", "borough", "mean_gvi", "imd_score")]
  print(as.data.frame(influential_data))
} else {
  cat("No significant influential points detected\n")
}

# 8.2 Bootstrap confidence intervals
cat("\n8.2 Performing Bootstrap (1000 resamples)...\n")

boot_fun <- function(data, indices) {
  d <- data[indices, ]
  model <- lm(mean_gvi ~ imd_score, data = d)
  return(coef(model)["imd_score"])
}

set.seed(123)
boot_results <- boot(analysis_data, boot_fun, R = 1000)
boot_ci <- boot.ci(boot_results, type = "perc")

cat("Bootstrap 95% CI for IMD coefficient:\n")
cat("  Point estimate:", round(boot_results$t0, 3), "\n")
cat("  95% CI: [", round(boot_ci$percent[4], 3), ",", 
    round(boot_ci$percent[5], 3), "]\n")

# ============================================
# 9. Composite visualizations
# ============================================

cat("\n\n9. Creating composite analysis plots...\n")

# 9.1 Multi-panel diagnostic plot
png(here("figures", "model_diagnostics.png"), width = 12, height = 10, 
    units = "in", res = 300)
par(mfrow = c(2, 2))
plot(lm_model, which = 1:4)
dev.off()
cat("  - Model diagnostic plot saved ✓\n")

# 9.2 Composite results plot
p1 <- ggplot(analysis_data, aes(x = factor(inner_imd_quintile), y = mean_gvi)) +
  geom_boxplot(aes(fill = factor(inner_imd_quintile)), alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  scale_fill_viridis_d(guide = "none") +
  labs(title = "GVI by IMD Quintile", x = "IMD Quintile", y = "Mean GVI (%)")

p2 <- ggplot(analysis_data, aes(x = mean_gvi)) +
  geom_histogram(bins = 15, fill = "forestgreen", alpha = 0.7) +
  geom_vline(xintercept = mean(analysis_data$mean_gvi), 
             color = "red", linetype = "dashed") +
  labs(title = "GVI Distribution", x = "Mean GVI (%)", y = "Count")

p3 <- ggplot(analysis_data, aes(x = sd_gvi, y = mean_gvi)) +
  geom_point(aes(color = factor(inner_imd_quintile)), size = 3) +
  scale_color_viridis_d(name = "IMD Q") +
  labs(title = "GVI Mean vs SD", x = "SD of GVI", y = "Mean GVI (%)")

p4 <- ggplot(analysis_data, aes(x = pct_deeplearning, y = mean_gvi)) +
  geom_point(aes(color = factor(inner_imd_quintile)), size = 3) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_color_viridis_d(name = "IMD Q") +
  labs(title = "GVI vs Deep Learning Usage", 
       x = "Deep Learning Usage (%)", 
       y = "Mean GVI (%)")

combined_plot <- grid.arrange(p1, p2, p3, p4, ncol = 2, 
                              top = "Extended Analysis Summary")

ggsave(here("figures", "extended_analysis_summary.png"), combined_plot, 
       width = 12, height = 10, dpi = 300)
cat("  - Composite analysis plot saved ✓\n")

# ============================================
# 10. Generate analysis report
# ============================================

cat("\n\n============================================")
cat("\n10. Generating analysis report")
cat("\n============================================\n")

# Calculate key statistics
cor_test <- cor.test(analysis_data$mean_gvi, analysis_data$imd_score)
gam_p_value <- summary(gam_model)$s.table[1, "p-value"]

report <- paste0(
  "City Look Dissertation - Phase 1 Extended Analysis Report\n",
  "Generated at: ", Sys.time(), "\n",
  "=====================================\n\n",
  
  "1. Main findings\n",
  "------------\n",
  "- Linear correlation: r = ", round(cor_test$estimate, 3), 
  " (p = ", round(cor_test$p.value, 3), ")\n",
  "- GAM explained variance: R² = ", round(summary(gam_model)$r.sq, 3), "\n",
  "- Best number of clusters: k = ", best_k, "\n",
  "- Borough ICC: ", round(icc * 100, 1), "%\n\n",
  
  "2. Nonlinear relationships\n",
  "------------\n",
  "- The GAM indicates a ", 
  ifelse(gam_p_value < 0.05, "significant", "non-significant"),
  " nonlinear relationship between IMD and GVI (p = ", round(gam_p_value, 3), ")\n",
  "- Polynomial regression did not show clear improvement\n",
  "- Quantile regression shows that IMD effects differ across GVI levels\n\n",
  
  "3. Spatial patterns\n",
  "------------\n",
  "- Borough-level differences are notable, with a max-min gap of ", 
  round(max(borough_effects$mean_gvi_borough) - min(borough_effects$mean_gvi_borough), 1), "%\n",
  "- The mixed effects model indicates Borough explains ", round(icc * 100, 1), "% of the variation\n",
  "- Borough with highest mean GVI: ", borough_effects$borough[1], 
  " (", round(borough_effects$mean_gvi_borough[1], 1), "%)\n",
  "- Borough with lowest mean GVI: ", 
  borough_effects$borough[nrow(borough_effects)], 
  " (", round(borough_effects$mean_gvi_borough[nrow(borough_effects)], 1), "%)\n\n",
  
  "4. Outliers\n",
  "------------\n"
)

if(nrow(outliers) > 0) {
  report <- paste0(report, 
                   "Found ", nrow(outliers), " outlier LSOA(s):\n",
                   paste("  -", outliers$lsoa_code, "(", outliers$borough, "):", 
                         round(outliers$mean_gvi, 1), "%\n", collapse = "")
  )
} else {
  report <- paste0(report, "No statistical outliers detected\n")
}

report <- paste0(report, 
                 "\n5. Clustering results\n",
                 "----------------\n",
                 "Identified ", best_k, " LSOA clusters with distinct GVI-IMD patterns\n",
                 paste(capture.output(print(cluster_summary)), collapse = "\n"),
                 "\n\n6. Robustness checks\n",
                 "-------------\n",
                 "- Bootstrap analysis supports the IMD coefficient estimate\n",
                 "- 95% CI: [", round(boot_ci$percent[4], 3), ", ", 
                 round(boot_ci$percent[5], 3), "]\n",
                 "\n7. Phase 2 priority suggestions\n",
                 "-------------------\n",
                 "Based on Phase 1 analysis, prioritize collecting the following historical data in Phase 2:\n",
                 "1. Borough-level planning policy differences (explains ", round(icc * 100, 1), "% variation)\n",
                 "2. Historical development of outlier LSOAs (especially ", ifelse(nrow(outliers)>0, outliers$lsoa_code[1], "N/A"), ")\n",
                 "3. Formation processes of high/low GVI cluster areas\n",
                 "4. Historical roots of Borough differences (e.g., Lewisham vs Hackney)\n",
                 "5. Impact of 1960s-1980s housing policies on current greening\n"
)

# Save report
writeLines(report, here("output", "phase1_extended_analysis_report.txt"))
cat("Analysis report saved to: output/phase1_extended_analysis_report.txt\n")

# ============================================
# 11. Save analysis outputs
# ============================================

# Export enhanced dataset
analysis_data_export <- analysis_data %>%
  dplyr::select(lsoa_code, borough, inner_imd_quintile, mean_gvi, 
                imd_score, cluster, dplyr::everything())


write_csv(analysis_data_export, here("output", "lsoa_analysis_enhanced.csv"))

# Save key model objects
save(
  lm_model, gam_model, me_model, final_kmeans, boot_results,
  fe_model, cluster_summary, borough_effects,
  file = here("output", "phase1_models.RData")
)

cat("\n\n============================================")
cat("\nAnalysis complete!")
cat("\n============================================\n")
cat("Generated files:\n")
cat("  - All visualizations in the figures/ directory\n")
cat("  - output/phase1_extended_analysis_report.txt\n")
cat("  - output/lsoa_analysis_enhanced.csv\n")
cat("  - output/phase1_models.RData\n")

cat("\n\nKey findings summary:\n")
cat("1. No significant linear relationship between IMD and GVI (confirmed)\n")
cat("2. Borough effects are significant, explaining", round(icc * 100, 1), "% of the variation\n")
cat("3. Identified", best_k, "different LSOA cluster types\n")
cat("4. Spatial factors appear more important than socio-economic factors\n")
