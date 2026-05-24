# ============================================
# Power Calculations — Anganwadi Nutrition RCT
# Outcome: Weight-for-age Z-score (WAZ)
# M&E Framework: ICDS Nutrition Counselling
# ============================================

library(pwr)
library(tidyverse)

# ============================================
# 1. BASIC POWER CALCULATION
# ============================================

# Parameters
# Baseline mean WAZ: -1.6, SD: 1.2 (NFHS-5, 2019-21)
# Minimum detectable effect: 0.15 SD
# Standard 80% power, 5% significance

effect_size <- 0.15      # in SD units
alpha <- 0.05            
power <- 0.80            

result <- pwr.t.test(
  d = effect_size,
  sig.level = alpha,
  power = power,
  type = "two.sample",
  alternative = "two.sided"
)

print(result)
cat("Sample needed per arm:", ceiling(result$n), "\n")
cat("Total sample:", ceiling(result$n) * 2, "\n")

# ============================================
# 2. DESIGN EFFECT FOR CLUSTERING
# ============================================

# Cluster randomisation at Anganwadi centre level
# ICC = 0.05 (typical for child nutrition outcomes)
# Cluster size = 20 children per centre

icc <- 0.05
cluster_size <- 20

# Design effect: DEFF = 1 + (m-1) * ICC
deff <- 1 + (cluster_size - 1) * icc
cat("Design effect:", deff, "\n")

# Adjusted sample
n_adjusted <- ceiling(result$n * deff)
cat("Adjusted sample per arm:", n_adjusted, "\n")
cat("Adjusted total sample:", n_adjusted * 2, "\n")
cat("Clusters per arm:", ceiling(n_adjusted / cluster_size), "\n")
cat("Total clusters:", ceiling(n_adjusted / cluster_size) * 2, "\n")

# Note: ignoring clustering would underpower the study by nearly half

# ============================================
# 3. POWER CURVE
# ============================================

sample_sizes <- seq(200, 2000, by = 50)

power_values <- sapply(sample_sizes, function(n) {
  pwr.t.test(
    n = n / deff,
    d = effect_size,
    sig.level = alpha,
    type = "two.sample",
    alternative = "two.sided"
  )$power
})

power_df <- tibble(
  sample_size = sample_sizes,
  power = power_values
)

ggplot(power_df, aes(x = sample_size, y = power)) +
  geom_line(color = "#2E86AB", linewidth = 1.2) +
  geom_hline(yintercept = 0.80, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 2726, linetype = "dashed", color = "darkgreen") +
  annotate("text", x = 2726, y = 0.3,
           label = "N = 2,726\n(recommended)",
           hjust = -0.1, size = 3.5) +
  annotate("text", x = 200, y = 0.82,
           label = "80% power threshold",
           hjust = 0, size = 3.5, color = "red") +
  labs(
    title = "Power Curve: Anganwadi Nutrition RCT",
    subtitle = "Effect size = 0.15 SD, ICC = 0.05, Cluster size = 20",
    x = "Total Sample Size",
    y = "Statistical Power"
  ) +
  theme_minimal()

ggsave("outputs/power_curve.png", width = 8, height = 6)
