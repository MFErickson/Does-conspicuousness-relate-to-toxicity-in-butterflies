# fits brms model
#XXXXXXXX
#last update
# Tue Oct 21 14:06:00 2025 ------------------------------

# packages ----
library(ape)
library(caper)
library(phylolm)
library(MCMCglmm)
library(brms)
library(ggplot2)
library(performance)
library(missForest)
library(dplyr)

# Import data
tree <- read.nexus("data/AA154_secondary_only_strategyA.tre")
dt <- read.csv("output/conspicuousness.csv")

#filter columns
dt <- dt[, c("Key","Ventral.time", "Dorsal.time", "N.daphnia", "deaths", "Phylo")]

# Replace NA in Dorsal.time with the mean of the non-missing values 
dt$Dorsal.time[is.na(dt$Dorsal.time)] <- mean(dt$Dorsal.time, na.rm = TRUE)


# Check if species are in the tree and prune the tree ----
tree_pruned <- drop.tip(tree, setdiff(tree$tip.label, unique(dt$Phylo)))

# Check tree structure
is.binary(tree_pruned)
is.rooted(tree_pruned)

# Filter data to align with the pruned tree
dt_filtered <- dt[dt$Phylo %in% tree_pruned$tip.label, ]

# Create phylogenetic covariance matrix
phylo_cov <- vcv(tree_pruned, corr = TRUE)

# Fit the Bayesian model for Dorsal & Ventral Conspicuoussness
set.seed(1)

# Scale variables
dt_filtered$Dorsal.time_scaled <- scale(dt_filtered$Dorsal.time)
dt_filtered$Ventral.time_scaled <- scale(dt_filtered$Ventral.time)

# Define and run the model ----
brms_model <- brm(
  deaths | trials(N.daphnia) ~ Dorsal.time_scaled + Ventral.time_scaled + 
    (1 | gr(Phylo, cov = phylo_cov)),
  data = dt_filtered,
  family = binomial(),
  data2 = list(phylo_cov = phylo_cov),
  control = list(adapt_delta = 0.99),
  chains = 4,
  iter = 5000,
  warmup = 1000,
  thin = 10
)


# summary
summary(brms_model)

# Model diagnostics
summary(brms_model)
prior_summary(brms_model)

# Save output
output_file <- "output/brms-result.txt"
sink(output_file)
print(summary(brms_model))
sink()

# Model checks
pp_check(brms_model)
bayes_R2(brms_model)
r2_results <- performance::r2(brms_model)
print(r2_results)

# Plot residuals
residuals <- resid(brms_model)
plot(residuals[, 1], residuals[, 2], xlab = "Fitted Values", ylab = "Residuals",
     main = "Residual Plot")
abline(h = 0, col = "red", lty = 2)
boxplot(residuals, main = "Boxplot of Residuals")

# Calculating the effect in biologically meaning way ------

#Get mean and SD of the variable before scaling
mean_dorsal <- attr(dt_filtered$Dorsal.time_scaled, "scaled:center")
sd_dorsal   <- attr(dt_filtered$Dorsal.time_scaled, "scaled:scale")

cat("Mean detection time (ms):", mean_dorsal, "\n")
cat("Standard deviation (ms):", sd_dorsal, "\n")

# extract the fixed-effect estimates
est_dorsal <- fixef(brms_model)["Dorsal.time_scaled", "Estimate"]
intercept  <- fixef(brms_model)["Intercept", "Estimate"]

cat("Log-odds effect of 1 SD (scaled) increase:", est_dorsal, "\n")

#  Convert log-odds to probabilities using inverse logit 
inv_logit <- function(x) 1 / (1 + exp(-x))

p_mean <- inv_logit(intercept) # mean detection time (scaled = 0)
p_plus1 <- inv_logit(intercept + est_dorsal) # +1 SD (scaled = +1)
p_minus1 <- inv_logit(intercept - est_dorsal) # -1 SD (scaled = -1)

# Print
cat("Probability at mean detection time:", round(p_mean, 3), "\n")
cat("Probability at +1 SD (", round(mean_dorsal + sd_dorsal, 1), " ms):", round(p_plus1, 3), "\n")
cat("Probability at -1 SD (", round(mean_dorsal - sd_dorsal, 1), " ms):", round(p_minus1, 3), "\n")

### Plot model-------------

#Needs to be done in order

#Plot Dorsal -------------
# Generate a prediction grid
pred_grid_dorsal <- dt_filtered %>%
  mutate(Dorsal.time_scaled = (Dorsal.time - mean(Dorsal.time, na.rm = TRUE)) / sd(Dorsal.time, na.rm = TRUE)) %>%
  summarise(
    Dorsal_min = min(Dorsal.time, na.rm = TRUE),
    Dorsal_max = max(Dorsal.time, na.rm = TRUE)
  )

# Create a sequence across the observed range
newdata_dorsal <- data.frame(
  Dorsal.time_scaled = seq(
    (pred_grid_dorsal$Dorsal_min - mean(dt_filtered$Dorsal.time)) / sd(dt_filtered$Dorsal.time),
    (pred_grid_dorsal$Dorsal_max - mean(dt_filtered$Dorsal.time)) / sd(dt_filtered$Dorsal.time),
    length.out = 100
  ),
  Ventral.time_scaled = 0,  # keep Ventral at mean
  N.daphnia = 1
)

# Get predicted probabilities from the brms model
pred_matrix_dorsal <- posterior_epred(brms_model, newdata = newdata_dorsal, re_formula = NA)
pred_mean <- apply(pred_matrix_dorsal, 2, mean)
pred_lower <- apply(pred_matrix_dorsal, 2, quantile, probs = 0.025)
pred_upper <- apply(pred_matrix_dorsal, 2, quantile, probs = 0.975)

# Convert scaled back to milliseconds
Dorsal_mean <- mean(dt_filtered$Dorsal.time)
Dorsal_sd   <- sd(dt_filtered$Dorsal.time)
plot_df <- data.frame(
  Dorsal_ms = Dorsal_mean + newdata_dorsal$Dorsal.time_scaled * Dorsal_sd,
  pred_mean = pred_mean,
  pred_lower = pred_lower,
  pred_upper = pred_upper
)

# Add raw data points
raw_data <- dt_filtered %>%
  mutate(Mortality = deaths / N.daphnia)

# Plot
p1 <- ggplot(plot_df, aes(x = Dorsal_ms, y = pred_mean)) +
  geom_ribbon(aes(ymin = pred_lower, ymax = pred_upper), fill = "#9e9ac8", alpha = 0.2) +
  geom_line(color = "#9e9ac8", size = 1) +
  geom_point(data = raw_data, aes(x = Dorsal.time, y = Mortality),
             color = "#8856a7", alpha = 0.7, size = 4) +
  labs(
    x = "Dorsal detection time (ms)",
    y = expression(paste("Predicted probability of ", italic("Daphnia"), " death")),
  ) +
  theme_classic(base_size = 14)

p1

ggsave("output/p1.png",
       p1,
       width = 5,
       height = 4,
       dpi = 300
)

# Plot ventral -----------
# Generate a prediction grid
pred_grid_Ventral <- dt_filtered %>%
  mutate(Ventral.time_scaled = (Ventral.time - mean(Ventral.time, na.rm = TRUE)) / sd(Ventral.time, na.rm = TRUE)) %>%
  summarise(
    Ventral_min = min(Ventral.time, na.rm = TRUE),
    Ventral_max = max(Ventral.time, na.rm = TRUE)
  )

# Create a sequence across the observed range
newdata_Ventral <- data.frame(
  Ventral.time_scaled = seq(
    (pred_grid_Ventral$Ventral_min - mean(dt_filtered$Ventral.time)) / sd(dt_filtered$Ventral.time),
    (pred_grid_Ventral$Ventral_max - mean(dt_filtered$Ventral.time)) / sd(dt_filtered$Ventral.time),
    length.out = 100
  ),
  Dorsal.time_scaled = 0,  # set Dorsal to mean
  N.daphnia = 1
)


# Get predicted probabilities from the brms model
pred_matrix_Ventral <- posterior_epred(brms_model, newdata = newdata_Ventral, re_formula = NA)
pred_mean <- apply(pred_matrix_Ventral, 2, mean)
pred_lower <- apply(pred_matrix_Ventral, 2, quantile, probs = 0.025)
pred_upper <- apply(pred_matrix_Ventral, 2, quantile, probs = 0.975)

# Convert scaled back to milliseconds
Ventral_mean <- mean(dt_filtered$Ventral.time)
Ventral_sd   <- sd(dt_filtered$Ventral.time)
plot_df <- data.frame(
  Ventral_ms = Ventral_mean + newdata_Ventral$Ventral.time_scaled * Ventral_sd,
  pred_mean = pred_mean,
  pred_lower = pred_lower,
  pred_upper = pred_upper
)

# Add raw data points
raw_data <- dt_filtered %>%
  mutate(Mortality = deaths / N.daphnia)

# Plot
p2 <- ggplot(plot_df, aes(x = Ventral_ms, y = pred_mean)) +
  geom_ribbon(aes(ymin = pred_lower, ymax = pred_upper), fill = "#9e9ac8", alpha = 0.2) +
  geom_line(color = "#9e9ac8", size = 1) +
  geom_point(data = raw_data, aes(x = Ventral.time, y = Mortality),
             color = "#8856a7", alpha = 0.7, size = 4) +
  labs(
    x = "Ventral detection time (ms)",
    y = NULL) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

p2

ggsave("output/p2.png",
       p2,
       width = 5,
       height = 4,
       dpi = 300
)

