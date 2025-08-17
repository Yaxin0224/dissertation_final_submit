#!/usr/bin/env Rscript
# ============================================
# City Look Dissertation v2
# 05_cluster_interpretation.R - In-depth cluster interpretation
# 
# Purpose: Interpret the meaning of k=2 clustering and provide direction for Phase 2
# ============================================

# Clear environment
rm(list = ls())

# Load required packages
library(tidyverse)
library(here)
library(viridis)
library(gridExtra)
library(ggrepel)  # for improved labels

select <- dplyr::select
everything <- dplyr::everything


# Set paths
setwd(here())

cat("============================================\n")
cat("Cluster results in-depth analysis\n")
cat("============================================\n\n")

# ============================================
# 1. Load data and models
# ============================================

cat("1. Loading data and models...\n")

# Read enhanced analysis data
analysis_data <- read_csv(here("output", "lsoa_analysis_enhanced.csv"),
                          show_col_types = FALSE)

# Load model objects
load(here("output", "phase1_models.RData"))

cat("  - Data loaded ✓\n")

# ============================================
# 2. Cluster feature analysis
# ============================================

cat("\n2. Analyze characteristics of the two clusters...\n")

# Detailed cluster comparison
cluster_detailed <- analysis_data %>%
  filter(!is.na(cluster)) %>%
  group_by(cluster) %>%
  summarise(
    n = n(),
    # GVI statistics
    mean_gvi = mean(mean_gvi),
    median_gvi = median(median_gvi),
    sd_gvi = mean(sd_gvi),
    gvi_range = paste0(round(min(mean_gvi), 1), "-", round(max(mean_gvi), 1)),
    
    # IMD statistics
    mean_imd = mean(imd_score),
    imd_range = paste0(round(min(imd_score), 1), "-", round(max(imd_score), 1)),
    
    # Borough distribution
    n_boroughs = n_distinct(borough),
    main_boroughs = paste(names(sort(table(borough), decreasing = TRUE)[1:3]), 
                          collapse = ", "),
    
    # Method usage
    mean_dl_usage = mean(pct_deeplearning),
    
    .groups = 'drop'
  ) %>%
  mutate(across(where(is.numeric), ~round(., 2)))

cat("\nDetailed cluster comparison:\n")
print(cluster_detailed)

# Name clusters
cluster_names <- analysis_data %>%
  filter(!is.na(cluster)) %>%
  group_by(cluster) %>%
  summarise(mean_gvi = mean(mean_gvi)) %>%
  arrange(desc(mean_gvi)) %>%
  mutate(cluster_name = c("High GVI Cluster", "Low GVI Cluster"))

# ============================================
# 3. Enhanced cluster visualizations
# ============================================

cat("\n3. Creating enhanced cluster visualizations...\n")

# Merge cluster names
analysis_data <- analysis_data %>%
  left_join(cluster_names %>% select(cluster, cluster_name), by = "cluster")

# 3.1 Enhanced cluster scatter plot
p_cluster_enhanced <- ggplot(analysis_data %>% filter(!is.na(cluster)), 
                             aes(x = imd_score, y = mean_gvi)) +
  geom_point(aes(color = cluster_name, size = sd_gvi), alpha = 0.7) +
  geom_text_repel(aes(label = lsoa_code), size = 2.5, max.overlaps = 15) +
  stat_ellipse(aes(color = cluster_name), level = 0.95, size = 1.2) +
  scale_color_manual(values = c("High GVI Cluster" = "#2E8B57", 
                                "Low GVI Cluster" = "#DC143C"),
                     name = "Cluster Type") +
  scale_size_continuous(name = "GVI Variability\n(SD)", range = c(3, 8)) +
  labs(
    title = "LSOA Clustering: Two Distinct Green Visibility Patterns",
    subtitle = "Ellipses show 95% confidence regions for each cluster",
    x = "IMD Score (Higher = More Deprived)",
    y = "Mean Green View Index (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "right"
  )

ggsave(here("figures", "cluster_enhanced_scatter.png"), p_cluster_enhanced,
       width = 12, height = 8, dpi = 300)

# 3.2 Borough representation in clusters
borough_cluster <- analysis_data %>%
  filter(!is.na(cluster)) %>%
  count(borough, cluster_name) %>%
  group_by(borough) %>%
  mutate(pct = n / sum(n) * 100)

p_borough_cluster <- ggplot(borough_cluster, 
                            aes(x = reorder(borough, pct), y = pct, 
                                fill = cluster_name)) +
  geom_col(position = "stack") +
  coord_flip() +
  scale_fill_manual(values = c("High GVI Cluster" = "#2E8B57", 
                               "Low GVI Cluster" = "#DC143C"),
                    name = "Cluster Type") +
  labs(
    title = "Borough Representation in Each Cluster",
    x = "Borough",
    y = "Percentage of LSOAs"
  ) +
  theme_minimal()

ggsave(here("figures", "borough_cluster_distribution.png"), p_borough_cluster,
       width = 10, height = 8, dpi = 300)

# 3.3 Prepare radar chart features for clusters
cat("\n  Preparing cluster feature comparison...\n")

# Standardize variables for radar chart
radar_data <- analysis_data %>%
  filter(!is.na(cluster)) %>%
  group_by(cluster_name) %>%
  summarise(
    `Green Visibility` = mean(mean_gvi),
    `Deprivation` = mean(imd_score),
    `GVI Variability` = mean(sd_gvi),
    `DL Method Use` = mean(pct_deeplearning),
    `Sample Size` = mean(n_images, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  pivot_longer(-cluster_name, names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  mutate(value_scaled = (value - min(value)) / (max(value) - min(value)) * 100)

# ============================================
# 4. Outliers and borderline case analysis
# ============================================

cat("\n4. Analyze outliers and borderline cases...\n")

# Identify extreme cases in each cluster
extreme_cases <- analysis_data %>%
  filter(!is.na(cluster)) %>%
  group_by(cluster) %>%
  mutate(
    gvi_zscore = (mean_gvi - mean(mean_gvi)) / sd(mean_gvi),
    is_extreme = abs(gvi_zscore) > 1.5
  ) %>%
  filter(is_extreme) %>%
  select(lsoa_code, borough, cluster_name, mean_gvi, imd_score, gvi_zscore)

cat("\nExtreme cases (z-score > 1.5):\n")
print(as.data.frame(extreme_cases))

# Focus on a particularly high-GVI LSOA
high_gvi_lsoa <- analysis_data %>%
  filter(lsoa_code == "E01003134")

cat("\n\nHigh-GVI LSOA details (E01003134 - Lambeth):\n")
cat("  - Mean GVI:", round(high_gvi_lsoa$mean_gvi, 1), "%\n")
cat("  - IMD Score:", round(high_gvi_lsoa$imd_score, 1), "\n")
cat("  - IMD Quintile:", high_gvi_lsoa$inner_imd_quintile, "\n")
cat("  - Cluster:", high_gvi_lsoa$cluster_name, "\n")

# ============================================
# 5. Cluster boundary / transition analysis
# ============================================

cat("\n5. Analyze 'borderline' LSOAs between clusters...\n")

# Compute cluster centers
cluster_centers <- analysis_data %>%
  filter(!is.na(cluster)) %>%
  group_by(cluster) %>%
  summarise(
    center_gvi = mean(mean_gvi),
    center_imd = mean(imd_score),
    .groups = 'drop'
  )

# Compute distances to cluster centers
analysis_data <- analysis_data %>%
  mutate(
    dist_to_cluster1 = sqrt((mean_gvi - cluster_centers$center_gvi[1])^2 + 
                              (imd_score - cluster_centers$center_imd[1])^2),
    dist_to_cluster2 = sqrt((mean_gvi - cluster_centers$center_gvi[2])^2 + 
                              (imd_score - cluster_centers$center_imd[2])^2),
    dist_ratio = pmin(dist_to_cluster1, dist_to_cluster2) / 
      pmax(dist_to_cluster1, dist_to_cluster2)
  )

# Identify boundary cases (distance ratio close to 1)
boundary_cases <- analysis_data %>%
  filter(!is.na(cluster), dist_ratio > 0.8) %>%
  select(lsoa_code, borough, cluster_name, mean_gvi, imd_score, dist_ratio) %>%
  arrange(desc(dist_ratio))

cat("\nBoundary cases (LSOAs that may switch clusters):\n")
print(as.data.frame(boundary_cases))

# ============================================
# 6. Policy implications based on clusters
# ============================================

cat("\n\n6. Policy implications based on clusters...\n")

# Generate policy summaries for each cluster
policy_implications <- list(
  high_gvi = analysis_data %>%
    filter(cluster_name == "High GVI Cluster", !is.na(cluster_name)) %>%
    summarise(
      n = n(),
      mean_gvi = mean(mean_gvi),
      mean_imd = mean(imd_score),
      boroughs = paste(unique(borough), collapse = ", ")
    ),
  
  low_gvi = analysis_data %>%
    filter(cluster_name == "Low GVI Cluster", !is.na(cluster_name)) %>%
    summarise(
      n = n(),
      mean_gvi = mean(mean_gvi),
      mean_imd = mean(imd_score),
      boroughs = paste(unique(borough), collapse = ", ")
    )
)

# ============================================
# 7. Generate cluster analysis report
# ============================================

cat("\n7. Generating cluster analysis report...\n")

report <- paste0(
  "City Look Dissertation - Cluster In-depth Analysis Report\n",
  "Generated at: ", Sys.time(), "\n",
  "=====================================\n\n",
  
  "1. Cluster overview\n",
  "------------\n",
  "Using K-means clustering (k=2), two distinct types of LSOAs were identified:\n\n",
  
  "High GVI cluster (n=", policy_implications$high_gvi$n, "):\n",
  "  - Mean GVI: ", round(policy_implications$high_gvi$mean_gvi, 1), "%\n",
  "  - Mean IMD: ", round(policy_implications$high_gvi$mean_imd, 1), "\n",
  "  - Main Boroughs: ", policy_implications$high_gvi$boroughs, "\n\n",
  
  "Low GVI cluster (n=", policy_implications$low_gvi$n, "):\n",
  "  - Mean GVI: ", round(policy_implications$low_gvi$mean_gvi, 1), "%\n",
  "  - Mean IMD: ", round(policy_implications$low_gvi$mean_imd, 1), "\n",
  "  - Main Boroughs: ", policy_implications$low_gvi$boroughs, "\n\n",
  
  "2. Key findings\n",
  "------------\n",
  "- The difference in GVI between the two clusters (", 
  round(policy_implications$high_gvi$mean_gvi - policy_implications$low_gvi$mean_gvi, 1),
  "%) is much larger than the IMD difference (",
  round(abs(policy_implications$high_gvi$mean_imd - policy_implications$low_gvi$mean_imd), 1),
  " points)\n",
  "- This suggests that greenness levels are driven mainly by factors other than deprivation\n",
  "- Strong clustering effects at the Borough level are evident\n\n",
  
  "3. Notable outlier\n",
  "------------\n",
  "E01003134 (Lambeth): GVI = ", round(high_gvi_lsoa$mean_gvi, 1), "%, which is much higher than other LSOAs\n",
  "Field investigation needed to understand:\n",
  "  - Proximity to large parks or green spaces?\n",
  "  - Special historical protection policies?\n",
  "  - Distinctive street design?\n\n",
  
  "4. Phase 2 data collection recommendations\n",
  "------------------------\n",
  "Based on cluster analysis, prioritize collecting:\n\n",
  
  "A. Success factors in the High GVI cluster:\n",
  "  - Historical planning documents (especially for Lewisham)\n",
  "  - Protection policies and greening standards\n",
  "  - Community engagement and maintenance mechanisms\n\n",
  
  "B. Constraints in the Low GVI cluster:\n",
  "  - History of high-density development\n",
  "  - Priority given to transport infrastructure\n",
  "  - Land-use conflicts\n\n",
  
  "C. Transformation potential of boundary cases:\n"
)

if(nrow(boundary_cases) > 0) {
  report <- paste0(report,
                   "  The following LSOAs show transformation potential:\n",
                   paste("  -", boundary_cases$lsoa_code, "(", boundary_cases$borough, ")\n", 
                         collapse = "")
  )
}

report <- paste0(report,
                 "\n5. Policy recommendations\n",
                 "------------\n",
                 "- For Low GVI cluster: implement targeted greening interventions\n",
                 "- Learn from High GVI cluster best practices\n",
                 "- Prioritize upgrading boundary-case LSOAs\n",
                 "- Establish a Borough-level knowledge-sharing mechanism for greening\n"
)

# Save report
writeLines(report, here("output", "cluster_analysis_report.txt"))
cat("  - Cluster analysis report saved ✓\n")

# ============================================
# 8. Create composite visualization panel
# ============================================

cat("\n8. Creating composite visualizations...\n")

# Prepare four subplots
p1 <- p_cluster_enhanced

p2 <- ggplot(analysis_data %>% filter(!is.na(cluster)), 
             aes(x = cluster_name, y = mean_gvi)) +
  geom_boxplot(aes(fill = cluster_name), alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  scale_fill_manual(values = c("High GVI Cluster" = "#2E8B57", 
                               "Low GVI Cluster" = "#DC143C"),
                    guide = "none") +
  labs(title = "GVI Distribution by Cluster", 
       x = "Cluster", y = "Mean GVI (%)") +
  theme_minimal()

p3 <- ggplot(analysis_data %>% filter(!is.na(cluster)), 
             aes(x = cluster_name, y = imd_score)) +
  geom_boxplot(aes(fill = cluster_name), alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  scale_fill_manual(values = c("High GVI Cluster" = "#2E8B57", 
                               "Low GVI Cluster" = "#DC143C"),
                    guide = "none") +
  labs(title = "IMD Distribution by Cluster", 
       x = "Cluster", y = "IMD Score") +
  theme_minimal()

p4 <- p_borough_cluster

# Combine plots
combined <- grid.arrange(
  p2, p3, p4,
  ncol = 3,
  top = "Cluster Analysis Summary"
)

ggsave(here("figures", "cluster_summary_panel.png"), combined,
       width = 18, height = 6, dpi = 300)

cat("  - Composite visualization saved ✓\n")

# ============================================
# 9. Export key results
# ============================================

# Save cluster membership list
cluster_membership <- analysis_data %>%
  filter(!is.na(cluster)) %>%
  select(lsoa_code, lsoa_name, borough, cluster, cluster_name, 
         mean_gvi, imd_score, inner_imd_quintile) %>%
  arrange(cluster, desc(mean_gvi))

write_csv(cluster_membership, here("output", "cluster_membership.csv"))

cat("\n\n============================================")
cat("\nCluster analysis complete!\n")
cat("============================================\n")

cat("\nKey insights:\n")
cat("1. Two clearly distinct types of LSOAs were identified\n")
cat("2. Greenness differences are not primarily driven by deprivation\n")
cat("3. Borough and spatial factors may be more important\n")
cat("4. There are clear opportunities for policy intervention\n")

cat("\nGenerated files:\n")
cat("- figures/cluster_enhanced_scatter.png\n")
cat("- figures/borough_cluster_distribution.png\n")
cat("- figures/cluster_summary_panel.png\n")
cat("- output/cluster_analysis_report.txt\n")
cat("- output/cluster_membership.csv\n")
