# Are the 3 backgrounds of each type actually similar?
#Jim Mclean
#last update
# Fri Jun 20 16:19:59 2025 ------------------------------

#packages ----
library(colorspace)
library(jpeg)
library(Durga)
library(JUtils)
source("R/0.functions.R")

GetColFn <- function(range) {
  # pal <- diverge_hcl(6, palette = "Tofino")
  # vec <- seq(min(range), max(range), length.out = length(pal))
  # maxDiff <- max(abs(range))
  # function(x) {
  #   alpha <- sqrt(1 - abs(x) / maxDiff) / 4
  #   col <- pal[findInterval(x, vec, all.inside = TRUE)]
  #   sapply(seq_along(col), function(i) adjustcolor(col[i], alpha[i]))
  # }

  pal <- diverge_hcl(12, palette = "Tofino")
  sd <- sd(range)
  function(x) {
    ci <- round(2 * x / sd) + 6
    pal[ci]
  }
}


df <- read.csv("data/score.csv")
# Remove duplicate species 
df <- df[df$butterflyUrl != "3455 (1).png", ]
# Only outcomes, ie not misses
outcomes <- df[df$score != "miss", ]
outcomes$bg <- sub(".jpg", "", outcomes$backgroundUrl)
bgs <- sort(unique(outcomes$bg))

# # Order background types into ascending average time ----
# agg <- aggregate(list(Time = outcomes$time), by = list(bgType = outcomes$bgType), FUN = mean)
# agg <- agg[order(agg$Time), ]
# groups <- bgs[order(match(sub("[1-9]", "", bgs), agg$bgType))]
# d <- DurgaDiff(time ~ bg, outcomes, groups = groups, R = NA)

# # "Star" plot
# par(mar = c(1, 1, 1, 1))
# plot(NULL, xlim = c(-1, 1), ylim = c(-1, 1), asp = 1, axes = FALSE, xlab = "", ylab = "")
# bgs <- sort(d$group.names)
# nbg <- length(bgs)
# pts <- complex(modulus = 1, argument = (0:(nbg - 1)) * 2 * pi / nbg) 
# points(pts, pch = 16)
# text(pts * 1.1, labels = seq_along(bgs), xpd = NA)
# gd <- d$group.differences
# colFn <- GetColFn(sapply(gd, function(g) g$t0))
# maxDiff <- max(sapply(gd, function(g) abs(g$t0)))
# for (i in seq_len(nbg)) {
#   for (j in seq_len(nbg)) {
#     jj <- j %% nbg + 1
#     # Find the group difference
#     for (k in seq_len(length(gd))) {
#       if (gd[[k]]$groups[1] == bgs[i] && gd[[k]]$groups[2] == bgs[jj]) {
#         col <- colFn(gd[[k]]$t0)
#         lwd <- 1 + (1 - abs(gd[[k]]$t0) / maxDiff)
#         lwd = 5
#         lines(pts[c(i, jj)], col = col, lwd = lwd)
#         break
#       }
#     }
#   }
# }
# legend("topright", legend = 1:12, fill = diverge_hcl(12, palette = "Tofino"))

# JPlotToPNG("output/backgrounds.png", {
#   par(mar = c(2, 4, 0, 0))
#   p <- DurgaPlot(d, ef.size = FALSE, axes = T, 
#                  group.colour = rep(RColorBrewer::brewer.pal(10, "Set3"), each = 3))
# }, width = 2400, height = 600)
# 
# 
# groups <- agg$bgType
# bgtypeD <- DurgaDiff(time ~ bgType, outcomes, groups = groups, contrasts = NULL, R = NA)
# 
# JPlotToPNG("output/background-types.png", {
#   par(mar = c(2, 4, 1, 0))
#   DurgaPlot(bgtypeD, ef.size = FALSE, points = F, axes = T, violin.shape = "full", group.colour = "Set3")
# }, width = 900, height = 500, res = 90)



############################################################################

# Is there a relationship between complexity (estimated as compressibility) and conspicuousness ----
#(mean conspicuousness for a background image)? Answer is YES!
# There is a positive relationship between image complexity and crypsis (R^2 =
# 0.35).
#
# Note there is only a very small negative relationship between mean butterfly
# detection time and butterfly image complexity.

# Determine the "compressibility" of an image
bgComplexity <- function(url) {
  LZComplexity(file.path("images/backgrounds", url))
}


JPlotToPNG("output/background-complexity.png", {
  pb <- JBuildProgressBar(numItems = length(unique(outcomes$bg)) + 1, title = "Calculating compressibility...")
  agg <- aggregate(list(Time = outcomes$time), by = list(Background = outcomes$bg), FUN = mean)
  compl <- sapply(paste0(agg$Background, ".jpg"), function(bg) { pb(); bgComplexity(bg) })
  pb(close = TRUE, printElapsed = TRUE)  
  
  agg$Secs <- agg$Time / 1000
  compPc <- compl * 100
  par(mar = c(5, 4, 1, 1) + 0.1)
  plot(agg$Secs ~ compPc, ylim = extendrange(agg$Secs),
       pch = 16, cex = 1.5, col = rep(RColorBrewer::brewer.pal(10, "Set3"), each = 3),
       xlab = "Image complexity (%)", ylab = "Mean time to detection (sec)")
  text(compPc, agg$Secs, agg$Background, pos = 1)
  
  l <- lm(Secs ~ compPc, data = agg)
  abline(l, col = "brown")
  newx <- seq(par("usr")[1], par("usr")[2], length.out = 1000)
  conf_interval <- predict(l, newdata = data.frame(compPc = newx), interval = "confidence", level = 0.95)
  polygon(c(newx, rev(newx)), c(conf_interval[,2], rev(conf_interval[,3])), 
          col = adjustcolor("brown", 0.05), border = adjustcolor("brown", 0.2))
  
  cat("\nIs there a relationship between background image complexity\n  (estimated as 1/compressibility) and conspicuousness?\n")
  print(summary(l))
  JReportToFile("output/background-complexity.txt", {
    cat("Background images compressibility ratio (%) summary:\n")
    print(summary(compl * 100))
    cat("Mean background detection times summary (secs):\n")
    print(summary(agg$Secs))
    print(sd(agg$Secs))
    cat("\nIs there a relationship between background image complexity\n  (estimated as 1/compressibility) and conspicuousness?\n")
    print(summary(l))
  })
}, width = 900, res = 110)
