# chi2 for dorsal side of butterflies are were more stable across backgrounds on defended species
#Marilia F Erickson
#last update
# Fri Jun 20 16:35:56 2025 ------------------------------


#packages ----
library(ggplot2)

# Load data
dt <- read.csv("output/conspicuousness.csv")


# Categorize defense status
dt$Defense_Status <- ifelse(dt$Daphnia.mortality.p > 0.2, "defended", "not defended")

# Create contingency table
chi_table <- table(dt$Defense_Status, dt$Dorsal.Class)

# Perform Chi-square test ----
chi_result <- chisq.test(chi_table)
print(chi_result)

# Convert contingency table to a dataframe for plotting
chi_df <- as.data.frame(chi_table)
colnames(chi_df) <- c("Defense_Status", "Dorsal.Class", "Freq")

# Plot ----
p3 <- ggplot(chi_df, aes(x = Dorsal.Class, y = Freq, fill = Defense_Status)) +
  geom_bar(stat = "identity", position = "stack") +  # Use stacked bars for raw counts
  geom_text(aes(label = Freq), position = position_stack(vjust = 0.5), size = 5, color = "white") +  # Add count labels
  scale_fill_manual(values = c("defended" = "#8856a7", "not defended" = "#1c9099")) +
  labs(x = "Dorsal Class", y = "Count", fill = "Defense Status") +
  theme_classic(base_size = 16)
p3

