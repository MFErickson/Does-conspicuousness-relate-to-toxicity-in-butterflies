# chi2 for defended butterflies are more conspicuous on ventral side
#XXXXXXXX
#last update
# Fri Jun 20 16:34:10 2025 ------------------------------

#packages ----

library(ggplot2)

#import data
dt <- read.csv("output/conspicuousness.csv")

# Exclude rows where Time.difference == 0
dt <- dt[dt$Time.difference != 0, ]

# Classify as 'dorsal' or 'ventral' based on Time.difference
dt$cons_side <- ifelse(dt$Time.difference > 0, "dorsal", "ventral")

# Classify as 'defended' or 'not defended'
dt$Defense_Status <- ifelse(dt$Daphnia.mortality.p > 0.2, "defended", "not defended")

# Create a contingency table
chi_table <- table(dt$Defense_Status, dt$cons_side)

# Perform Chi-Square Test ----
chi_result <- chisq.test(chi_table)

# View results
chi_result


### PLot ----


# Convert original data to a proper format before making the table
dt_filtered <- dt[dt$Time.difference != 0, ]  # Exclude neutral cases
chi_table <- table(dt_filtered$Defense_Status, dt_filtered$cons_side)

# Convert table to a data frame with proper column names
chi_df <- as.data.frame(chi_table)
colnames(chi_df) <- c("Defense_Status", "cons_side", "Freq")  # Rename for clarity

# Now plot
p1 <- ggplot(chi_df, aes(x = cons_side, y = Freq, fill = Defense_Status)) +
  geom_bar(stat = "identity", position = "stack") +  # Keep stacked bars (not normalized proportions)
  geom_text(aes(label = Freq), position = position_stack(vjust = 0.5), size = 5, color = "white") +  # Add counts inside bars
  scale_fill_manual(values = c("defended" = "#8856a7", "not defended" = "#1c9099")) +
  labs(x = "Most Conpicuous side", y = "Count", fill = "Defense Status") +
  theme_classic(base_size = 16)
p1
