# Plots data from brms analyses
#Marilia F Erickson
#last update
# Fri Jun 20 16:33:00 2025 ------------------------------


#packages ----
library(ggplot2)
library(cowplot)

dt <- read.csv("output/conspicuousness.csv")

dt$Ventral.time <- (dt$Ventral.time/1000)
dt$Dorsal.time <- (dt$Dorsal.time/1000)

# plots ----
p1 <- ggplot(dt, aes(x = Dorsal.time, y = Daphnia.mortality.p)) +
  geom_point(size = 2, alpha = 0.6, color = "#8856a7") +  # Scatter points in orange
  geom_smooth(method = "lm", se = TRUE, linetype = "dashed", color = "black", fill = "pink", alpha = 0.3) +  # Dashed regression line in black, CI in pink
  scale_x_continuous(limits = c(2.4, 10)) +
  scale_y_continuous(limits = c(0, 0.75)) +
  labs(y ="Death proportion", x = "Dorsal Detection Time (Secs)") +
  theme_classic(base_size = 16)
p1


p2 <- ggplot(dt, aes(x = Ventral.time, y = Daphnia.mortality.p)) +
  geom_point(size = 2, alpha = 0.7, color = "#8856a7") +  # Scatter points in orange
  geom_smooth(method = "lm", se = TRUE, linetype = "dashed", color = "black", fill = "pink", alpha = 0.3) +  # Dashed regression line in black, CI in pink
  scale_x_continuous(limits = c(2.4, 10)) +
  scale_y_continuous(limits = c(0, 0.75)) +
  labs(y = "Death proportion", x = "Ventral Detection Time (Secs)") +
  theme_classic(base_size = 16)

p2

g1 <- plot_grid(p1,p2, labels= "auto")
g1



ggsave("output/figures/g1.png",
       g1,
       width = 10,
       height = 5,
       dpi = 300
       )
