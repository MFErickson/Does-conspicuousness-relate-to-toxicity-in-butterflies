# fits brms model
#Marilia F Erickson
#last update
# Fri Jun 20 16:30:08 2025 ------------------------------



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

# Define and run the model ----
dt_filtered$Dorsal.time_scaled <- scale(dt_filtered$Dorsal.time)
dt_filtered$Ventral.time_scaled <- scale(dt_filtered$Ventral.time)

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
