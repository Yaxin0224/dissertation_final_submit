#!/usr/bin/env Rscript
# ============================================
# City Look Dissertation v2
# 03.2_gvi_analysis_simple.R - Simplified GVI analysis
# 
# Does not rely on external RData files; directly analyzes Python-generated results
# ============================================

# Clear environment
rm(list = ls())

# Load packages
library(tidyverse)
library(here)
library(ggplot2)
library(viridis)

# Set project path
setwd(here())

# ============================================
# 1. Read data
# ============================================

cat("1. Reading data...\n")

# Read Python-generated GVI results
gvi_results <- read_csv(here("output", "gvi_results", "gvi_results_for_r.csv"))
cat(paste("  - Read", nrow(gvi_results), "GVI records\n"))

# Read LSOA information
selected_lsoas <- read_csv(here("data", "processed", "selected_lsoas.csv"))
cat(paste("  - Read", nrow(selected_lsoas), "LSOA records\n"))

# ============================================
# 2. Data aggregation
# ============================================

cat("\n2. Calculating LSOA-level statistics...\n")

# LSOA-level summary
lsoa_gvi_summary <- gvi_results %>%
  group_by(lsoa_code) %>%
  summarise(
    n_images = n(),
    mean_gvi = mean(gvi, na.rm = TRUE),
    median_gvi = median(gvi, na.rm = TRUE),
    sd_gvi = sd(gvi, na.rm = TRUE),
    min_gvi = min(gvi, na.rm = TRUE),
    max_gvi = max(gvi, na.rm = TRUE),
    q25_gvi = quantile(gvi, 0.25, na.rm = TRUE),
    q75_gvi = quantile(gvi, 0.75, na.rm = TRUE),
    # Method usage statistics
    n_deeplearning = sum(method == "deeplearning"),
    n_color = sum(method == "color_threshold"),
    pct_deeplearning = round(n_deeplearning / n() * 100, 1),
    .groups = 'drop'
  )

# Join with LSOA information
lsoa_gvi_complete <- lsoa_gvi_summary %>%
  left_join(selected_lsoas, by = "lsoa_code") %>%
  arrange(inner_imd_quintile, lsoa_code)

# Save summary results
write_csv(lsoa_gvi_complete, here("output", "lsoa_gvi_summary.csv"))
cat("  - LSOA summary results saved\n")

# ============================================
# 3. Descriptive statistics
# ============================================

cat("\n3. Overall statistics:\n")

# Overall stats
cat(paste("  - Number of LSOAs processed:", n_distinct(gvi_results$lsoa_code), "\n"))
cat(paste("  - Number of images processed:", nrow(gvi_results), "\n"))
cat(paste("  - Mean GVI:", round(mean(gvi_results$gvi), 2), "%\n"))
cat(paste("  - Median GVI:", round(median(gvi_results$gvi), 2), "%\n"))
cat(paste("  - Standard deviation:", round(sd(gvi_results$gvi), 2), "\n"))
cat(paste("  - GVI range:", round(min(gvi_results$gvi), 2), "-", 
          round(max(gvi_results$gvi), 2), "%\n"))

# Method usage statistics
method_stats <- gvi_results %>%
  count(method) %>%
  mutate(percentage = round(n / sum(n) * 100, 1))

cat("\n  Method usage:\n")
print(method_stats)

# IMD quintile statistics
cat("\n4. IMD quintile statistics:\n")
imd_stats <- lsoa_gvi_complete %>%
  group_by(inner_imd_quintile) %>%
  summarise(
    n_lsoas = n(),
    mean_gvi = round(mean(mean_gvi), 2),
    sd_gvi = round(sd(mean_gvi), 2),
    median_gvi = round(median(median_gvi), 2),
    .groups = 'drop'
  ) %>%
  mutate(
    quintile_label = paste0("Q", inner_imd_quintile, 
                            ifelse(inner_imd_quintile == 1, " (Least deprived)", 
                                   ifelse(inner_imd_quintile == 5, " (Most deprived)", "")))
  )

print(imd_stats)

# ============================================
# 4. Statistical tests
# ============================================

cat("\n5. Statistical tests:\n")

# Correlation test
if("imd_score" %in% names(lsoa_gvi_complete)) {
  cor_test <- cor.test(lsoa_gvi_complete$mean_gvi, 
                       lsoa_gvi_complete$imd_score,
                       method = "pearson")
  
  cat(paste("  - Pearson correlation: r =", round(cor_test$estimate, 3), 
            ", p =", format(cor_test$p.value, scientific = FALSE), "\n"))
}

# Kruskal-Wallis test
kw_test <- kruskal.test(mean_gvi ~ as.factor(inner_imd_quintile), 
                        data = lsoa_gvi_complete)

cat(paste("  - Kruskal-Wallis test: χ² =", round(kw_test$statistic, 2), 
          ", p =", format(kw_test$p.value, scientific = FALSE), "\n"))

# ============================================
# 5. Visualizations
# ============================================

cat("\n6. Creating visualizations...\n")

# Create figures directory
if(!dir.exists(here("figures"))) {
  dir.create(here("figures"))
}

# 5.1 Boxplot: GVI by IMD Quintile
p1 <- lsoa_gvi_complete %>%
  ggplot(aes(x = factor(inner_imd_quintile), y = mean_gvi)) +
  geom_boxplot(aes(fill = factor(inner_imd_quintile)), 
               alpha = 0.7, outlier.shape = 21) +
  geom_jitter(width = 0.2, alpha = 0.5, size = 3) +
  scale_fill_viridis_d(option = "viridis") +
  scale_x_discrete(labels = c("Q1\nLeast deprived", "Q2", "Q3", "Q4", "Q5\nMost deprived")) +
  labs(
    title = "Green View Index by IMD Quintile",
    subtitle = paste("Inner London,", nrow(lsoa_gvi_complete), "LSOAs"),
    x = "IMD Quintile",
    y = "Mean GVI (%)",
    caption = paste("Kruskal-Wallis test: p =", 
                    format(kw_test$p.value, digits = 3))
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

ggsave(here("figures", "gvi_by_imd_quintile.png"), p1, 
       width = 10, height = 7, dpi = 300)
cat("  - Boxplot saved\n")

# 5.2 Bar chart: Mean GVI by LSOA
p2 <- lsoa_gvi_complete %>%
  arrange(mean_gvi) %>%
  mutate(lsoa_label = paste0(lsoa_code, " (Q", inner_imd_quintile, ")")) %>%
  ggplot(aes(x = reorder(lsoa_label, mean_gvi), y = mean_gvi)) +
  geom_col(aes(fill = factor(inner_imd_quintile)), alpha = 0.8) +
  geom_text(aes(label = round(mean_gvi, 1)), hjust = -0.2, size = 3) +
  scale_fill_viridis_d(option = "viridis", name = "IMD Quintile") +
  coord_flip() +
  labs(
    title = "Mean GVI by LSOA",
    subtitle = "Ordered by GVI value",
    x = "LSOA (IMD Quintile)",
    y = "Mean GVI (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.y = element_text(size = 8)
  )

ggsave(here("figures", "gvi_by_lsoa_ranked.png"), p2, 
       width = 10, height = 10, dpi = 300)
cat("  - LSOA ranked plot saved\n")

# 5.3 Method usage distribution
p3 <- lsoa_gvi_complete %>%
  select(lsoa_code, inner_imd_quintile, n_deeplearning, n_color, pct_deeplearning) %>%
  ggplot(aes(x = reorder(lsoa_code, pct_deeplearning), y = pct_deeplearning)) +
  geom_col(aes(fill = factor(inner_imd_quintile)), alpha = 0.8) +
  geom_hline(yintercept = 95, linetype = "dashed", color = "red") +
  scale_fill_viridis_d(option = "viridis", name = "IMD Quintile") +
  coord_flip() +
  labs(
    title = "Deep Learning Method Usage by LSOA",
    subtitle = "Percentage of images processed with SegFormer",
    x = "LSOA",
    y = "Deep Learning Usage (%)",
    caption = "Red line indicates 95% threshold"
  ) +
  theme_minimal()

ggsave(here("figures", "method_usage_by_lsoa.png"), p3, 
       width = 10, height = 8, dpi = 300)
cat("  - Method usage plot saved\n")

# 5.4 GVI distribution histogram
p4 <- gvi_results %>%
  ggplot(aes(x = gvi)) +
  geom_histogram(bins = 50, fill = "forestgreen", alpha = 0.7) +
  geom_vline(aes(xintercept = mean(gvi)), 
             color = "red", linetype = "dashed", size = 1) +
  facet_wrap(~method, ncol = 1, scales = "free_y") +
  labs(
    title = "Distribution of GVI Values",
    subtitle = paste("All images (n =", nrow(gvi_results), ")"),
    x = "GVI (%)",
    y = "Count",
    caption = "Red line shows mean value"
  ) +
  theme_minimal()

ggsave(here("figures", "gvi_distribution_by_method.png"), p4, 
       width = 10, height = 8, dpi = 300)
cat("  - GVI distribution plot saved\n")

# ============================================
# 6. Generate summary table
# ============================================

cat("\n7. Generating summary table...\n")

# Create summary table
summary_table <- lsoa_gvi_complete %>%
  mutate(
    Borough = str_replace_all(borough, " and ", " & "),
    `IMD Q` = inner_imd_quintile,
    `Images` = n_images,
    `Mean GVI` = paste0(round(mean_gvi, 1), "%"),
    `SD` = round(sd_gvi, 1),
    `DL%` = paste0(pct_deeplearning, "%")
  ) %>%
  select(LSOA = lsoa_code, Borough, `IMD Q`, Images, `Mean GVI`, SD, `DL%`)

# Save table
write_csv(summary_table, here("output", "gvi_summary_table.csv"))
cat("  - Summary table saved\n")

# Print first 10 rows
cat("\nLSOA GVI summary (first 10):\n")
print(summary_table %>% head(10))

# ============================================
# 7. Done
# ============================================

cat("\n========== GVI analysis complete ==========\n")
cat("Generated files:\n")
cat("  - output/lsoa_gvi_summary.csv - detailed summary data\n")
cat("  - output/gvi_summary_table.csv - concise summary table\n")
cat("  - figures/gvi_*.png - visualization charts\n")
cat("\nKey findings:\n")
cat(paste("  - Overall mean GVI:", round(mean(lsoa_gvi_complete$mean_gvi), 2), "%\n"))
cat(paste("  - Deep learning usage rate:", 
          round(sum(gvi_results$method == "deeplearning") / nrow(gvi_results) * 100, 1), 
          "%\n"))
cat(paste("  - IMD quintile difference significance: p =", 
          format(kw_test$p.value, digits = 3), "\n"))

# Problem LSOAs alert
problem_lsoas <- lsoa_gvi_complete %>%
  filter(pct_deeplearning < 90) %>%
  pull(lsoa_code)

if(length(problem_lsoas) > 0) {
  cat("\nNote: the following LSOAs have deep learning usage less than 90%:\n")
  for(lsoa in problem_lsoas) {
    lsoa_info <- lsoa_gvi_complete %>% 
      filter(lsoa_code == lsoa) %>%
      slice(1)
    cat(paste("  -", lsoa, ":", lsoa_info$pct_deeplearning, "%\n"))
  }
}

cat("\nNext steps suggestions:\n")
cat("  1. Check visualizations in the figures/ directory\n")
cat("  2. Run spatial autocorrelation analysis\n")
cat("  3. Perform regression analysis to explore determinants\n")

# Return key results
invisible(list(
  lsoa_summary = lsoa_gvi_complete,
  imd_stats = imd_stats,
  kw_test = kw_test,
  method_stats = method_stats
))
