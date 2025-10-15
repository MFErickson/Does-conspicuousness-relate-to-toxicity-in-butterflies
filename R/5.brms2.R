# fits brms model for MeanAbsDiff
# Last update
# Wed Oct 15 19:07:45 2025 ------------------------------


# packages ----
library(ape)
library(brms)
library(ggplot2)
library(performance)
library(dplyr)

# Import data ----
tree <- read.nexus("data/AA154_secondary_only_strategyA.tre")
dt <- read.csv("output/conspicuousness.csv")


# 1. Compute dorsal-ventral differences per background

# Select dorsal columns ending in 1,2,3
dorsal_bg_cols <- grep("^Dorsal\\..*[123]$", names(dt), value = TRUE)

# Create difference columns
for(dcol in dorsal_bg_cols){
  vcol <- sub("Dorsal", "Ventral", dcol)
  if(vcol %in% names(dt)){
    diff_col <- paste0("Diff_", sub("Dorsal\\.", "", dcol))
    dt[[diff_col]] <- dt[[dcol]] - dt[[vcol]]
  }
}

# Compute Mean Absolute Difference per row
dt_summary <- dt %>%
  rowwise() %>%
  mutate(MeanAbsDiff = mean(abs(c_across(starts_with("Diff_"))), na.rm = TRUE)) %>%
  ungroup()

# Remove rows where MeanAbsDiff is NA
dt_summary <- dt_summary %>%
  filter(!is.na(MeanAbsDiff))

# Keep only relevant columns
dt_summary_clean <- dt_summary[, c("Key", "MeanAbsDiff", "N.daphnia", "deaths", "Phylo")]


# Check tree and prune

tree_pruned <- drop.tip(tree, setdiff(tree$tip.label, unique(dt_summary_clean$Phylo)))

# Filter data to align with pruned tree
dt_summary_clean <- dt_summary_clean[dt_summary_clean$Phylo %in% tree_pruned$tip.label, ]

# Create phylogenetic covariance matrix
phylo_cov <- vcv(tree_pruned, corr = TRUE)

# Scale predictor
dt_summary_clean$MeanAbsDiff_scaled <- scale(dt_summary_clean$MeanAbsDiff)


# Fit Bayesian phylogenetic model

set.seed(1)
brms_model <- brm(
  deaths | trials(N.daphnia) ~ MeanAbsDiff_scaled + 
    (1 | gr(Phylo, cov = phylo_cov)),
  data = dt_summary_clean,
  family = binomial(),
  data2 = list(phylo_cov = phylo_cov),
  control = list(adapt_delta = 0.99),
  chains = 4,
  iter = 5000,
  warmup = 1000,
  thin = 10
)

# Summary

summary(brms_model)
prior_summary(brms_model)

# Save output
output_file <- "output/brms-result2.txt"
sink(output_file)
print(summary(brms_model))
sink()

# Posterior predictive check
pp_check(brms_model)

# Bayesian R2
bayes_R2(brms_model)
r2_results <- performance::r2(brms_model)
print(r2_results)

# Plot residuals
residuals <- resid(brms_model)
plot(residuals[, 1], residuals[, 2], xlab = "Fitted Values", ylab = "Residuals",
     main = "Residual Plot")
abline(h = 0, col = "red", lty = 2)
boxplot(residuals, main = "Boxplot of Residuals")
