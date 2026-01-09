# Produces phylogeny figure
#XXXXXXXX
#last update
# Fri Sep 26 08:52:52 2025 ------------------------------



# Load packages---------------------

library(dplyr)
library(tibble)
library(brms)
library(ape)
library(ggtree)
library(ggplot2)

#Must run the 4.brms script and leave objects in environment before running this script


# 1. Extract posterior effects and collapse Phylo -------------------------

posterior <- as_draws_df(brms_model)

phylo_cols <- grep("^r_Phylo\\[", colnames(posterior), value = TRUE)
phylo_means <- colMeans(posterior[, phylo_cols])
species_names <- gsub("^r_Phylo\\[|,Intercept\\]$", "", phylo_cols)

phylo_effects <- tibble(
  Phylo = species_names,
  phylo_mean = phylo_means
)

# Join with dt and join phylogeny into genus
phylo_results <- dt %>%
  left_join(phylo_effects, by = "Phylo")

phylo_results_collapsed <- phylo_results %>%
  mutate(
    Phylo_collapsed = case_when(
      Phylo == "DanausR_X_Nymphalidae_Danainae_Danaini_Danaus_plexippus" ~
        "GCA0049599151_X_Nymphalidae_Danainae_Danaini_Danaus_chrysippus",
      Phylo == "DL02Q800_X_Hesperiidae_Hesperiinae_Taractrocerini_Ocybadistes_flavovittata_X_ME" ~
        "BN000549_127_Hesperiidae_Hesperiinae_Taractrocerini_Ocybadistes_walkeri",
      TRUE ~ Phylo
    )
  )

phylo_results_collapsed_summary <- phylo_results_collapsed %>%
  group_by(Phylo_collapsed) %>%
  summarise(
    phylo_mean = mean(phylo_mean, na.rm = TRUE),
    Key = first(Key),  # manter Key para extrair gênero
    .groups = "drop"
  ) %>%
  mutate(Genus = sapply(strsplit(Key, " "), `[`, 1))


# 2. Build genus tree ----------------------------

tree <- read.nexus("data/AA154_secondary_only_strategyA.tre")

tree_pruned <- drop.tip(tree,
                        setdiff(tree$tip.label, phylo_results_collapsed_summary$Phylo_collapsed)
)

tree_pruned$tip.label <- phylo_results_collapsed_summary$Phylo_collapsed[
  match(tree_pruned$tip.label, phylo_results_collapsed_summary$Phylo_collapsed)
]

# 3. Prepare data and plot --------------------

tree_data <- fortify(tree_pruned)

tip_data <- tree_data %>%
  filter(isTip) %>%
  left_join(
    phylo_results_collapsed_summary %>% rename(label = Phylo_collapsed),
    by = "label"
  )

p <- ggplot(tree_data, aes(x = x, y = y)) +
  geom_tree() +
  geom_point(
    data = tip_data,
    aes(color = phylo_mean),
    size = 3
  ) +
  scale_color_gradient2(
    low = "blue", mid = "white", high = "red", midpoint = 0,
    name = "Mean Posterior\nPhylogenetic\nEffect in Toxicity"
  ) +
  geom_tiplab(
    data = tip_data,
    aes(label = Genus),
    hjust = 0,
    offset = 0.8,
    size = 3,
    fontface = "italic"
  ) +
  theme_tree2() +
  theme(
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank()
  ) +
  xlim(0, 110)

# Display
print(p)

# Save
ggsave(
  "output/phylo_toxicity_tree_genus.png",
  plot = p,
  width = 9,
  height = 5,
  dpi = 300
)

