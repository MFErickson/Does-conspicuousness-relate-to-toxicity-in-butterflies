# Plots data from brms analyses
#XXXXXXXX
#last update
# Thu Sep 25 17:47:39 2025 ------------------------------


#packages ----
library(ggplot2)
library(cowplot)

dt <- read.csv("output/conspicuousness.csv")

# re-create MeanAbsDiff column
dorsal_bg_cols <- grep("^Dorsal\\..*[123]$", names(dt), value = TRUE)

for(dcol in dorsal_bg_cols){
  vcol <- sub("Dorsal", "Ventral", dcol)
  if(vcol %in% names(dt)){
    diff_col <- paste0("Diff_", sub("Dorsal\\.", "", dcol))
    dt[[diff_col]] <- dt[[dcol]] - dt[[vcol]]
  }
}

dt_summary <- dt %>%
  rowwise() %>%
  mutate(MeanAbsDiff = mean(abs(c_across(starts_with("Diff_"))), na.rm = TRUE)) %>%
  ungroup()

dt_summary <- dt_summary %>%
  filter(!is.na(MeanAbsDiff))

#Scale time variables
dt$Ventral.time <- (dt$Ventral.time/1000)
dt$Dorsal.time <- (dt$Dorsal.time/1000)

# plots ----
p1 <- ggplot(dt, aes(x = Dorsal.time, y = Daphnia.mortality.p)) +
  geom_point(size = 2, alpha = 0.6, color = "#8856a7") +  # Scatter points in orange
  #geom_smooth(method = "lm", se = TRUE, linetype = "dashed", color = "black", fill = "pink", alpha = 0.3) +  # Dashed regression line in black, CI in pink
  scale_x_continuous(limits = c(2.4, 10)) +
  scale_y_continuous(limits = c(0, 0.75)) +
  labs(y ="Death proportion", x = "Dorsal Detection Time (Secs)") +
  theme_classic(base_size = 16)
p1


p2 <- ggplot(dt, aes(x = Ventral.time, y = Daphnia.mortality.p)) +
  geom_point(size = 2, alpha = 0.7, color = "#8856a7") +  # Scatter points in orange
  #geom_smooth(method = "lm", se = TRUE, linetype = "dashed", color = "black", fill = "pink", alpha = 0.3) +  # Dashed regression line in black, CI in pink
  scale_x_continuous(limits = c(2.4, 10)) +
  scale_y_continuous(limits = c(0, 0.75)) +
  labs(y = "Death proportion", x = "Ventral Detection Time (Secs)") +
  theme_classic(base_size = 16)

p2



p3 <- ggplot(dt_summary, aes(x = MeanAbsDiff, y = Daphnia.mortality.p)) +
  geom_point(size = 2, alpha = 0.7, color = "#8856a7") +  # Scatter points in orange
  #geom_smooth(method = "lm", se = TRUE, linetype = "dashed", color = "black", fill = "pink", alpha = 0.3) +  # Dashed regression line in black, CI in pink
  #scale_x_continuous(limits = c(2.4, 10)) +
  #scale_y_continuous(limits = c(0, 0.75)) +
  labs(y = "Death proportion", x = "Mean detection time difference (Secs)") +
  theme_classic(base_size = 16)

p3

g1 <- plot_grid(
  p1, p2, p3,
  labels = c("(a)", "(b)", "(c)"),
  label_fontface = "italic", ncol = 3
)
g1



ggsave("output/g-raw.png",
       g1,
       width = 14,
       height = 4,
       dpi = 300
       )
