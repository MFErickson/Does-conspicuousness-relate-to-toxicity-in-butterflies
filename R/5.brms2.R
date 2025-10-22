# fits brms model for MeanAbsDiff
# Last update
# Tue Oct 21 19:29:33 2025 ------------------------------


# packages ----
library(ape)
library(brms)
library(ggplot2)
library(performance)
library(dplyr)
library(cowplot)

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

# Calculating the effect in biologically meaningful way ------

# Get mean and SD of the variable before scaling
MeanAbsDiff <- attr(dt_summary_clean$MeanAbsDiff_scaled, "scaled:center")
sd_MeanAbsDiff <- attr(dt_summary_clean$MeanAbsDiff_scaled, "scaled:scale")

cat("Mean detection time (ms):", MeanAbsDiff, "\n")
cat("Standard deviation (ms):", sd_MeanAbsDiff, "\n")

# Extract the fixed-effect estimates
est_MeanAbsDiff <- fixef(brms_model)["MeanAbsDiff_scaled", "Estimate"]
intercept <- fixef(brms_model)["Intercept", "Estimate"]

cat("Log-odds effect of 1 SD (scaled) increase:", est_MeanAbsDiff, "\n")

# Convert log-odds to probabilities using inverse logit
inv_logit <- function(x) 1 / (1 + exp(-x))

p_mean <- inv_logit(intercept)                         # mean detection time (scaled = 0)
p_plus1 <- inv_logit(intercept + est_MeanAbsDiff)     # +1 SD (scaled = +1)
p_minus1 <- inv_logit(intercept - est_MeanAbsDiff)    # -1 SD (scaled = -1)

# Print
cat("Probability at mean detection time:", round(p_mean, 3), "\n")
cat("Probability at +1 SD (", round(MeanAbsDiff + sd_MeanAbsDiff, 1), " ms):", round(p_plus1, 3), "\n")
cat("Probability at -1 SD (", round(MeanAbsDiff - sd_MeanAbsDiff, 1), " ms):", round(p_minus1, 3), "\n")


### Plot model-------------

# Generate a prediction grid
pred_grid <- dt_summary_clean %>%
  # Keep only MeanAbsDiff and scale it like in the model
  mutate(MeanAbsDiff_scaled = (MeanAbsDiff - mean(MeanAbsDiff, na.rm = TRUE)) / sd(MeanAbsDiff, na.rm = TRUE)) %>%
  summarise(
    MeanAbsDiff_min = min(MeanAbsDiff, na.rm = TRUE),
    MeanAbsDiff_max = max(MeanAbsDiff, na.rm = TRUE)
  )

# Create a sequence across the observed range
newdata <- data.frame(
  MeanAbsDiff_scaled = seq(
    (pred_grid$MeanAbsDiff_min - mean(dt_summary_clean$MeanAbsDiff)) / sd(dt_summary_clean$MeanAbsDiff),
    (pred_grid$MeanAbsDiff_max - mean(dt_summary_clean$MeanAbsDiff)) / sd(dt_summary_clean$MeanAbsDiff),
    length.out = 100
  ),
  N.daphnia = 1
)

#Get predicted probabilities from the brms model
pred_matrix <- posterior_epred(brms_model, newdata = newdata, re_formula = NA)
pred_mean <- apply(pred_matrix, 2, mean)
pred_lower <- apply(pred_matrix, 2, quantile, probs = 0.025)
pred_upper <- apply(pred_matrix, 2, quantile, probs = 0.975)

# Convert scaled back to milliseconds
MeanAbsDiff_mean <- attr(dt_summary_clean$MeanAbsDiff_scaled, "scaled:center")
MeanAbsDiff_sd <- attr(dt_summary_clean$MeanAbsDiff_scaled, "scaled:scale")

plot_df <- data.frame(
  MeanAbsDiff_ms = MeanAbsDiff_mean + newdata$MeanAbsDiff_scaled * MeanAbsDiff_sd,
  pred_mean = pred_mean,
  pred_lower = pred_lower,
  pred_upper = pred_upper
)

# Add raw data points
raw_data <- dt_summary_clean %>%
  mutate(Mortality = deaths / N.daphnia)

# Plot
p3 <- ggplot(plot_df, aes(x = MeanAbsDiff_ms, y = pred_mean)) +
  geom_ribbon(aes(ymin = pred_lower, ymax = pred_upper), fill = "#9e9ac8", alpha = 0.2) +
  geom_line(color = "#9e9ac8", size = 1) +
  geom_point(data = raw_data, aes(x = MeanAbsDiff, y = Mortality),
             color = "#8856a7", alpha = 0.7, size = 3) +
  labs(
    x = "Mean difference in detection time (ms)",
    y = NULL
    ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
    )

p3


ggsave("output/p3.png",
       p3,
       width = 5,
       height = 4,
       dpi = 300
)

# Plot grid
#Keep p1 and p2 on work environment from script 4
g1 <- plot_grid(
  p1, p2, p3,
  labels = c(" (a)", " (b)", " (c)"),
  label_fontface = "italic", ncol = 3
)
g1

ggsave("output/g1.png",
       g1,
       width = 15,
       height = 5,
       dpi = 300
)
