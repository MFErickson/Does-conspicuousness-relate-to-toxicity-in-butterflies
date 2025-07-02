#Generates butterfly image plots
#XXXXXXXX
#last update
# Fri Jun 20 16:05:54 2025 ------------------------------

#Packages----
library(JUtils)
library(Durga)
library(readxl)
source("R/0.functions.R")


comb <- read.csv("output/conspicuousness.csv")
# Order on difference decreasing since time is crypticness, not conspicuousness
comb <- comb[order(comb$Time.difference, na.last = FALSE, decreasing = TRUE), ]

# Is dorsal more conspicuous than ventral?----
JPlotToPNG("output/dorsal-vs-ventral.png", {
  d <- DurgaDiff(comb, groups = c("Ventral" = "Ventral.time", "Dorsal" = "Dorsal.time"), na.rm = TRUE)
  par(mar = c(4, 4, 1, 1) + 0.1)
  DurgaPlot(d, left.ylab = "time")
  cat("Mean dorsal/ventral time comparison:\n")
  print(d)
}, width = 900, res = 120)


# Dorsal - ventral time - hypothesised that sexual signals are dorsal, anti-predator are ventral----
JPlotToJPEG("output/dorsal-ventral.jpg", {
  
  cols <- RColorBrewer::brewer.pal(6, "Dark2")
  # Subset
  drawSubset <- function(ss, padToLength, sd, tdy = 0) {
    par(mar = c(2, 2, 2, 2))
    
    ss <- ss[rev(seq_len(nrow(ss))), ]
    du <- ifelse(is.na(ss$Dorsal.URL), NA, file.path("images/butterflies", ss$Dorsal.URL))
    length(du) <- max(length(du), padToLength)
    vu <- ifelse(is.na(ss$Ventral.URL), NA, file.path("images/butterflies", ss$Ventral.URL))
    length(vu) <- max(length(vu), padToLength)
    visuallyCmpConspMethods(vu, du, drawConnections = FALSE, scale = 0.003)
    col <- sapply(ss$Time.difference, function(d) ifelse(is.na(d), 1,
                                                         adjustcolor(ifelse(d >= 0, cols[2], cols[1]), 
                                                                     ifelse(abs(d) > sd, 1, 0.6))))
    font <- ifelse(abs(ss$Time.difference) > 2 * sd, 2, 1)
    label <- ifelse(is.na(ss$Time.difference), 
                    sprintf("%s\n", buttLabel(ss)),
                    sprintf("%s\n%g sec", buttLabel(ss), signif(ss$Time.difference / 1000, 2)))
    # Stagger text heights
    xs <- seq_len(nrow(ss))
    sh <- strheight("m") * 1.5
    text(xs, 1.5 + sh * (-0.5 + xs %% 2) + tdy, label, cex = 1, col = col, font = font, xpd = NA)
  }

  s1 <- tail(which(comb$Time.difference < 0 & !is.na(comb$Time.difference)), 15)
  s2 <- head(which(comb$Time.difference < 0 & !is.na(comb$Time.difference)), 15)
  s3 <- tail(which(comb$Time.difference >= 0 & !is.na(comb$Time.difference)), 15)
  s4 <- head(which(comb$Time.difference >= 0 & !is.na(comb$Time.difference)), 15)
  s5 <- is.na(comb$Time.difference)
  sd <- sd(comb$Time.difference, na.rm = TRUE)
  layout(matrix(1:5), heights = c(3, 3, 3, 3, 2))
  h <- 0.55
  drawSubset(comb[s1, ], 15, sd, tdy = 0.03)
  abline(h = h, col = adjustcolor(1, 0.2), xpd = NA)
  drawSubset(comb[s2, ], 15, sd, tdy = 0.03)
  abline(h = h, col = adjustcolor(1, 0.2), xpd = NA)
  drawSubset(comb[s3, ], 15, sd, tdy = 0.03)
  abline(h = h, col = adjustcolor(1, 0.2), xpd = NA)
  drawSubset(comb[s4, ], 15, sd, tdy = 0.03)
  abline(h = h, col = adjustcolor(1, 0.2), xpd = NA)
  drawSubset(comb[s5, ], 15, sd, tdy = 0.5)
  
  x <- 13
  mtext("Dorsal more conspicuous", side = 1, line = -3, at = x, adj = 0, col = cols[1])
  mtext("Ventral more conspicuous", side = 1, line = -1, at = x, adj = 0, col = cols[2])
}, width = 2400, aspectRatio = 1.414, res = 180)

# CLS_COL <- c("#db4804", "#2e0101")
CLS_COL <- c("#B32B00", "#140000")

# Ventral ordered by time----
JPlotToPNG("output/conspicuous-butts-ventral.png", {
  ventral <- comb[!duplicated(comb$Ventral.photo) & !is.na(comb$Ventral.photo), ]
  ventral <- ventral[order(ventral$Ventral.time, decreasing = TRUE), ]
  label <- paste(ventral$Binomial, ifelse(is.na(ventral$Sex), "", " "))
  col <- ifelse(ventral$Ventral.Class == "Conspicuous", CLS_COL[1], CLS_COL[2])
  caption <- sprintf("%.1f s, %s%% mortality", ventral$Ventral.time / 1000, signif(100 * ventral$Daphnia.mortality.p, 2))
  font <- ifelse(ventral$Daphnia.mortality.p > 0.2, 2, 1)
  
  caption <- sapply(seq_len(nrow(ventral)), function(i) {
    t <- sprintf("%.1f s,", ventral$Ventral.time[i] / 1000)
    m <- sprintf("%s%% mortality", signif(100 * ventral$Daphnia.mortality.p[i], 2))
    if (ventral$Daphnia.mortality.p[i] > 0.2) {
      as.expression(bquote(.(t) ~ bold(.(m))))
    } else {
      as.expression(bquote(.(t) ~ .(m)))
    }
  })
  
  sc <- plotAll(file.path("images/butterflies", ventral$Ventral.URL),
          label,
          caption,
          scale = 0.0028, # Make scale appear the same as dorsal
          nr = 8,
          mar = c(3.5, 0, 2, 0),
          cap.cex = 1.35, # No butterflies were ventrally sexually dimorphic, so don't show sex
          txt.col = col)
}, width = 2000, aspectRatio = 1.1, res = 110)

# Dorsal ordered by time----
JPlotToPNG("output/conspicuous-butts-dorsal.png", {
  dorsal <- comb[!duplicated(comb$Dorsal.photo) & !is.na(comb$Dorsal.photo), ]
  dorsal <- dorsal[order(dorsal$Dorsal.time, decreasing = TRUE), ]
  label <- paste(dorsal$Binomial, ifelse(is.na(dorsal$Sex), "", " "))
  col <- ifelse(dorsal$Dorsal.Class == "Conspicuous", CLS_COL[1], CLS_COL[2])
  caption <- sapply(seq_len(nrow(dorsal)), function(i) {
    t <- sprintf("%.1f s,", dorsal$Dorsal.time[i] / 1000)
    m <- sprintf("%s%% mortality", signif(100 * dorsal$Daphnia.mortality.p[i], 2))
    if (dorsal$Daphnia.mortality.p[i] > 0.2) {
      as.expression(bquote(.(t) ~ bold(.(m))))
    } else {
      as.expression(bquote(.(t) ~ .(m)))
    }
  })
  
  sc <- plotAll(file.path("images/butterflies", dorsal$Dorsal.URL),
                label,
                caption,
                scale = 0.0030, # Make scale appear the same as ventral
                nr = 8,
                mar = c(3.5, 0, 1, 0),
                cap.cex = 1.35,
                sexSymbol = dorsal$Sex,
                txt.col = col)
}, width = 2000, aspect = 1.1, res = 110)
