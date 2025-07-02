# Visualise the method we used to classify images as cryptic or conspicuous.
#
# The analysis is based on the premise that conspicuous butterflies should be
# conspicuous across many backgrounds, whereas cryptic butterflies are more
# likely to be specialists. Therefore, we expect cryptic butterflies to exhibit
# a large range of detection times across different backgrounds, whereas
# conspicuous butterflies should have a smaller range of detection times, i.e.
# they should always be fairly conspicuous.

#XXXXXXXX
#last update
# Fri Jun 20 16:22:40 2025 ------------------------------

#packages ----
library(JUtils)
source("R/0.functions.R")

df <- read.csv("data/score.csv")
scores <- df[df$butterflyUrl != "3455 (1).png", ] # Remove duplicate

# Get butterfly classifications
cls <- ButtClassify(scores)
cls$Time.sec <- cls$mostCrypticTime / 1000


# A function that can be useful for data exploration - 
# adds some summary statistics from a linear model to a plot ----
.drawLMTex <- function(l, side = 1, adj = .99, firstLine = 0, lineInc = 1, bonferroniFactor = 1, ...) {
  sl <- summary(l)
  t <- bquote(F[.(sl$fstatistic[2])][","][.(sl$fstatistic[3])] ~ "=" ~ .(signif(sl$fstatistic, 2)))
  # White background behind text
  rect(par("usr")[2] - 1.2 * strwidth(t), par("usr")[3] + .01, par("usr")[2] - .01, par("usr")[3] + 4 * strheight(t), col = "white", border = NA)
  line <- firstLine
  mtext(bquote(R^2 ~ "=" ~ .(round(sl$r.squared, 2))), side, line, adj = adj, ...)
  line <- line + lineInc
  mtext(t, side, line, adj = adj, padj = 0.2, ...)
  line <- line + lineInc
  pVal <- sl$coefficients[2,4] * bonferroniFactor
  mtext(bquote(p ~ "=" ~ .(signif(pVal, 2))), side, line, adj = adj, ...)
  box()
}

# Fits a linear model to the points then adds it to the current plot ----
.addlm <- function(x, y, col, level = 0.95, drawText = TRUE, adj = 0.99, ...) {
  l <- lm(y ~ x)
  newx <- seq(min(x), max(x), length.out = 1000)
  conf_interval <- predict(l, newdata = data.frame(x = newx), interval = "confidence", level = level)
  polygon(c(newx, rev(newx)), c(conf_interval[,2], rev(conf_interval[,3])), 
          col = adjustcolor(col, 0.1), border = adjustcolor(col, 0.3))
  lines(newx, conf_interval[,1], col = col)
  if (drawText) {
    .drawLMTex(l, firstLine = -3.5, cex = 0.66, col = col, adj = adj)
  }
  l
}

# Function to create the plot ----
plotClassification <- function(cls) {
  par(mar = c(4.5, 4, 0, 1) + 0.1)
  
  # Colour points by group
  isCryptic <- cls$Class == "Cryptic"
  plot(cls$Time.sec, cls$Time.range.pc, xlab = "Most cryptic detection time (sec)", ylab = "Time range (%)",
       pch = 21, bg = adjustcolor(ifelse(isCryptic, 3, 2), 0.3))
  mtext("Generalists", 2, line = 3, col = adjustcolor(1, 0.8), font = 3, adj = 0.03)
  mtext("Specialists", 2, line = 3, col = adjustcolor(1, 0.8), font = 3, adj = 0.97)
  
  # Estimate cutoff
  cutoff <- mean(c(max(cls$Time.sec[!isCryptic]), min(cls$Time.sec[isCryptic])))
  abline(v = cutoff, col = adjustcolor(1, 0.3), lty = 2)
  cat(sprintf("Conspicuous/cryptic cutoff = %g\n", cutoff))
  
  # Fit linear models to the 2 clusters
  .addlm(cls$Time.sec[!isCryptic], cls$Time.range.pc[!isCryptic], col = 2, adj = 0.7, drawText = FALSE)
  .addlm(cls$Time.sec[isCryptic], cls$Time.range.pc[isCryptic], col = 3, drawText = FALSE)
  
  legend("topleft", c("Conspicuous", "Cryptic"), pch = 21, pt.bg = c(2, 3))
}

# Plot to a PNG ----
JPlotToPNG("output/conspicuous-specificity.png", plotClassification(cls), width = 900, res = 120)
JPlotToPNG("output/conspicuous-specificity-small.png", plotClassification(cls), width = 600, res = 90)

# Are conspicuous butterflies more generalists than cryptic ones?
crypticSpecialists <- function(cls) {
  d <- DurgaDiff(Time.range.pc ~ Class, cls, effect.type = "cohens d")
  par(mar = c(5, 4, 0, 1) + 0.1)
  DurgaPlot(d, left.ylab = "Time range (%)")
  mtext("Generalists", 2, line = 3, col = adjustcolor(1, 0.8), font = 3, adj = 0)
  mtext("Specialists", 2, line = 3, col = adjustcolor(1, 0.8), font = 3, adj = 1)
}

JPlotToPNG("output/conspicuous-generalists.png", crypticSpecialists(cls), width = 900, res = 120)
JPlotToPNG("output/conspicuous-generalists-small.png", crypticSpecialists(cls), width = 600, res = 90)
