#Functions to use in analyses script
#Jim Mclean
#last update
# Fri Jun 20 16:08:31 2025 ------------------------------

#packages----
library(readxl)
library(png)
library(JUtils)

# Plot all images in a directory on a grid with a constant scale----
plotAllInDir <- function(dir) {
  bf <- list.files(dir, "*.png", full.names = T)
  # bf <- head(bf, 12)
  buts <- sapply(bf, function(fn) readPNG(fn, native = FALSE))
  # Count no. of non-transparent pixels
  notTrans <- sapply(buts, function(but) { sum(but[,,4] > 0.5) })
  buts <- buts[order(notTrans)]
  bf <- bf[order(notTrans)]
  notTrans <- sort(notTrans)
  
  # Scale them all to fit in
  widths <- sapply(buts, function(but) dim(but)[2])
  heights <- sapply(buts, function(but) dim(but)[1])
  maxSz <- max(c(widths, heights))
  scale <- 1 / maxSz
  par(mar = c(0, 0, 0, 0))
  nr <- ceiling(sqrt(length(bf)))
  nc <- ceiling(length(bf) / nr)
  plot(NULL, xlim = c(0.5, nc + 0.5), ylim = c(0.5, nr + 0.5), axes = FALSE, xlab = "", ylab = "", asp = 1)
  for (i in seq_along(buts) ) {
    cx <- (i - 1) %% nc + 1
    cy <- (i - 1) %/% nc + 1
    #cat(sprintf("%d: %s %d x %d\n", i, bf[i], widths[i], heights[i]))
    # text(cx, cy, i)
    JPlotRaster(buts[[i]], cx, cy, width = widths[i] * scale)
    # w <- widths[i] * scale
    # h <- heights[i] * scale
    #rect(cx - w / 2, cy - h / 2, cx + w / 2, cy + h / 2)
    text(cx, cy - 0.45, notTrans[i])
    text(cx, cy + 0.45, basename(bf[i]))
  }
}

plotAll <- function(files, label = basename(files[i]), caption, cap.cex = 1, nr = NA, 
                    scale = NA, mar = c(0, 0.5, 0, 0.5), sexSymbol = NA, txt.col = 1) {
  # Extend txt.col to correct length 
  txt.col <- rep_len(txt.col, length(files))

  # Prepare sex images
  if (length(sexSymbol) == length(files)) {
    symb <- suppressWarnings(list(female = readPNG("data/female.png"), 
                                  male = readPNG("data/male.png")))
  }
  buts <- sapply(files, function(fn) readPNG(fn, native = FALSE))
  
  # Scale them all to fit in
  widths <- sapply(buts, function(but) dim(but)[2])
  heights <- sapply(buts, function(but) dim(but)[1])
  maxSz <- max(c(widths, heights))
  if (is.na(scale))
    scale <- 1 / maxSz
  par(mar = mar)
  if (is.na(nr)) nr <- ceiling(sqrt(length(files))) # Default to square layout
  nc <- ceiling(length(files) / nr)
  plot(NULL, xlim = c(1, nc), ylim = c(1, nr), xaxs = 'i', axes = FALSE, xlab = "", ylab = "", asp = 1)
  for (i in seq_along(buts) ) {
    # Adjust i to leave a gap at the end instead of the beginning of the sequence
    ai <- i + nc - 1 - (length(buts) - 1) %% nc
    cx <- nc - (ai - 1) %% nc
    cy <- (ai - 1) %/% nc + 1
    # cat(sprintf("%d: %g, %g\n", i, cx, cy))
    JPlotRaster(buts[[i]], cx, cy, width = widths[i] * scale, xpd = NA)
    tx <- cx
    ty <- cy - 0.45
    text(tx, ty, label[i], cex = cap.cex, xpd = NA, col = txt.col[i])
    # Draw a sex symbol. Do it this very complicated way because if we just draw
    # a text symbol, it is so thin as to be almost invisible
    if (length(sexSymbol) == length(files) && !is.na(sexSymbol[i])) {
      w <- strwidth(label[i]) * cap.cex
      h <- strheight(label[i]) * cap.cex
      JPlotRaster(symb[[sexSymbol[i]]], tx + w / 2, ty, width = 1.1 * strwidth("m"))
    }
    text(cx, cy - 0.45 - 1.5 * strheight("x") * cap.cex, caption[i], cex = cap.cex, xpd = NA, col = txt.col[i])
  }
  invisible(scale)
}

# Creates an image of ordered butterflies.----
# 
# ... List of ordered vectors of butterfly image URLs
visuallyCmpConspMethods <- function(..., drawConnections = TRUE, lineDy = 0.07, scale = NA) {
  orders <- list(...)
  
  allButs <- unique(unlist(orders))
  buts <- sapply(allButs, function(fn) if (is.na(fn)) { NULL } else { readPNG(fn, native = FALSE) })
  widths <- sapply(buts, function(but) if (is.null(but)) { NA } else { dim(but)[2] })
  heights <- sapply(buts, function(but) if (is.null(but)) { NA } else { dim(but)[1] })
  maxSz <- max(c(widths, heights), na.rm = TRUE)
  if (is.na(scale))
    scale <- 1 / maxSz
  
  plot(NULL, xlim = c(0.5, length(orders[[1]]) + 0.5), ylim = c(0.8, length(orders) + 0.2), 
       xaxs = 'i', yaxs = 'i', axes = FALSE, xlab = "", ylab = "")
  for (ri in seq_along(orders)) {
    r <- orders[[ri]]
    for (bi in seq_along(r)) {
      if (!is.na(r[bi])) {
        png <- buts[[r[bi]]]
        width <- dim(png)[2]
        JPlotRaster(png, bi, ri, width = width * scale, xpd = NA)
      }
    }
  }
  
  # Draw connecting lines
  if (drawConnections) {
    for (i in seq_len(length(orders) - 1)) {
      o1 <- orders[[i]]
      o2 <- orders[[i + 1]]
      for (x1 in seq_along(o1)) {
        x2 <- which(o2 == o1[x1])
        if (length(x2) > 0)
          segments(x1, i + lineDy, x2, i + 1 - lineDy, col = x1 %% 3 + 2)
      }
      # calculate Pearson's r
      rank1 <- seq_along(o1)
      rank2 <- match(o2, o1)
      r <- cor(rank1, rank2, method = "spearman")
      if (!is.na(r))
        text(0.6, i + 0.5, sprintf("Spearman's ρ = %g", r), pos = 4, cex = 1.5)
    }
  }
  
  invisible(scale)
}

buildLeaderBoard <- function(outcomes) {
  leaderBoard <- aggregate(list(Time = outcomes$time), by = list(sessionId = outcomes$sessionId), FUN = sum)
  leaderBoard <- merge(leaderBoard, 
                       aggregate(list(Found = outcomes$score), by = list(sessionId = outcomes$sessionId), 
                                 FUN = function(x) sum(x == "hit")))
  leaderBoard <- leaderBoard[order(leaderBoard$Found, leaderBoard$Time, decreasing = c(TRUE, FALSE)), ]
  leaderBoard[, c("Found", "Time")]
}


# Read species data for each image URL----
readSpeciesInfo <- function(photoInfoFile = "game_photo_legend.xlsx") {
  # Read photo information
  info <- read_xlsx(photoInfoFile)
  
  # Construct URL from Photo ID. This is a little bit complicated because the
  # files weren't named consistently
  info$URL <- sapply(info$Photo, function(pid) {
    # Look for a photo with the appropriate photo id in its name
    lf <- list.files("images/butterflies", pattern = paste0("^", pid, "[^0-9]"))
    if (length(lf) == 1)
      lf[[1]]
    else
      NA
  })
  bad <- which(is.na(info$URL))
  if (any(bad)) {
    warning(sprintf("Unable to locate images for photo IDs: %s", paste(info$Photo[bad], collapse = ", ")))
  }
  
  # Check binomial name format
  badNames <- grep("^[A-Z][a-z]+ [a-z]+$", info$Binomial, invert = TRUE, value = TRUE)
  if (length(badNames) > 0) {
    warning(sprintf("Improper binomial names in %s: %s\n", photoInfoFile, paste(unique(badNames), collapse = ", ")))
    info$Binomial <- JCapitalise(info$Binomial)
  }
  
  info  
}

# Incorporate species data into conspicuousness measurements for each image URL----
incorporateSpecies <- function(con, photoInfoFile = "game_photo_legend.xlsx") {
  info <- readSpeciesInfo(photoInfoFile)
  m <- merge(con, info)
  bad <- con$URL[!con$URL %in% m$URL]
  if (length(bad) > 0) {
    warning(sprintf("No species defined for URL(s) %s", paste(bad, collapse = ", ")))
  }
  m
}

# Given a data frame of outcomes, 
#returns a data frame of conspicuousness for each butterfly using several methods. ----
# @returns
# data frame with columns:
# MeanTime - mean time for all hits and escapes for each butterfly
# ConspicRatio - mean ratio of hits (1) to escapes (0)
# TimeConspicuousness - 1 - meanTime / max(outcomes$time)
# BgConspicTime - TimeConspicuousness on most cryptic background type for each butterfly
# BgConspicRatio - ConspicRatio on the most cryptic background type
# mostCrypticTime - mean time on the most cryptic background for the butterfly
calculateConspicuousness <- function(scores) {
  outcomes <- scores[scores$score != "miss", ]
  
  # Calculate conspicuousness as mean time taken to find the butterfly (or max time in case of an escape)
  agg <- aggregate(list(MeanTime = outcomes$time,
                        ConspicRatio = outcomes$conspicuousRatio),
                   by = list(URL = outcomes$butterflyUrl),
                   FUN = mean)
  agg$TimeConspicuousness <- 1 - agg$MeanTime / max(outcomes$time)
  
  # Also calculate conspicuousness on least conspicuous background type
  magg <- aggregate(list(ComboTime = outcomes$time,
                         ComboConsp = outcomes$conspicuousRatio), 
                    by = list(Combo = outcomes$combo, URL = outcomes$butterflyUrl, bgType = outcomes$bgType), 
                    FUN = mean)
  # Conspicuousness on most cryptic background type using timed conspicuousness
  magg$comboTimeConspicuousness <- 1 - magg$ComboTime / max(outcomes$time)
  minTimeCon <- sapply(unique(magg$URL), function(butt) { min(magg$comboTimeConspicuousness[magg$URL == butt]) })
  
  # Also calculate conspicuousness on least conspicuous background
  bmagg <- aggregate(list(BgTime = outcomes$time), 
                     by = list(URL = outcomes$butterflyUrl, bg = outcomes$backgroundUrl), 
                     FUN = mean)
  # Most cryptic background
  crypticBg <- sapply(unique(magg$URL), function(butt) { butTm <- bmagg[bmagg$URL == butt, ]; butTm$bg[which.max(butTm$BgTime)] })
  # Crypsis on most cryptic background using time
  maxBgTime <- sapply(unique(magg$URL), function(butt) { max(bmagg$BgTime[bmagg$URL == butt]) })
  
  # Conspicuousness on most cryptic background using hit:escape conspicuousness
  minRatioCon <- sapply(unique(magg$URL), function(butt) { min(magg$ComboConsp[magg$URL == butt]) })
  
  agg <- merge(agg, data.frame(URL = names(minTimeCon),
                               BgConspicTime = minTimeCon, 
                               BgConspicRatio = minRatioCon, 
                               mostCrypticTime = maxBgTime,
                               mostCrypticBG = crypticBg))
  
  agg
}

# Given a data frame of outcomes, 
# returns a data frame of conspicuousness for each butterfly single method. ----
#The result is the same as the
# @returns
# data frame with columns:
calculateLongestBgTime <- function(scores) {
  outcomes <- scores[scores$score != "miss", ]
  
  # Calculate average time on least conspicuous background
  bmagg <- aggregate(list(BgTime = outcomes$time), 
                     by = list(URL = outcomes$butterflyUrl, bg = outcomes$backgroundUrl), 
                     FUN = mean)
  # Crypsis on most cryptic background using time
  maxBgTime <- sapply(unique(bmagg$URL), function(butt) { max(bmagg$BgTime[bmagg$URL == butt]) })
  
  data.frame(URL = names(maxBgTime), mostCrypticTime = maxBgTime)
}


# Bootstrap CI of conspicuousness measures ----
# for a single measure of conspicuousness for each butterfly, average detection time on the most cryptic
# background.
#
# Returns data frame with row per butterfly, columns "URL", "Time",
# "Time.lower", "Time.upper", "Rank", "Rank.lower", "Rank.upper".
bootstrapCI <- function(scores, R = 1000, CI = 0.95) {
  
  con <- calculateLongestBgTime(scores)
  
  bootcrypsis <- matrix(ncol = nrow(con), nrow = R + 1)
  bootrank <- matrix(ncol = nrow(con), nrow = R + 1)
  
  # Include actual data in bootstrap
  crypsis <- con$mostCrypticTime
  bootcrypsis[R + 1, ] <- crypsis
  bootrank[R + 1, ] <- rank(crypsis)
  
  pb <- JBuildProgressBar("text", R, title = "Bootstrapping CIs")
  
  for (i in seq_len(R)) {
    bootData <- scores[sample(nrow(scores), replace = TRUE), ]
    cc <- calculateLongestBgTime(bootData)
    bcrypsis <- cc$mostCrypticTime
    bootcrypsis[i, ] <- bcrypsis
    bootrank[i, ] <- rank(bcrypsis)
    pb()
  }
  
  pb(close = TRUE, printElapsed = TRUE)
  
  CI2 <- (1 - CI) / 2
  rankCI <- apply(bootrank, 2, quantile, c(CI2, 1 - CI2))
  crypsisCI <- apply(bootcrypsis, 2, quantile, c(CI2, 1 - CI2))
  
  # JPlotDensities(apply(bootcrypsis, 1, density), lineColours = NA, fillColours = adjustcolor(1, 0.05))
  # JPlotDensities(apply(bootcrypsis, 1, density), lineColours = NA, fillColours = adjustcolor(rainbow(10), 0.05))
  
  df <- data.frame(con$URL, crypsis, t(crypsisCI), rank(crypsis), t(rankCI))
  names(df) <- c("URL", "Time", "Time.lower", "Time.upper", "Rank", "Rank.lower", "Rank.upper")
  #df <- df[order(df$Time), ]
  df
}


# Returns the "complexity" of a JPG image as the ratio of compressed to uncompressed size----
LZComplexity <- function(path) {
  # Read raw data. Don't use image data directly because it is already compressed
  if (grepl("\\.png$|\\.PNG$", path)) {
    img <- readPNG(path)
  } else {
    img <- readJPEG(path)
  }
  raw <- serialize(img, NULL)
  origSz <- length(raw)

  # We want the highest compression possible, hence xz
  txt.gz <- memCompress(raw, "xz")
  newSz <- length(txt.gz)
  newSz / origSz
}

ImageEntropy <- function(path) {
  # Read raw data. Don't use image data directly because it is already compressed
  if (grepl("\\.png$|\\.PNG$", path)) {
    img <- readPNG(path)
  } else {
    img <- readJPEG(path)
  }
  raw <- as.integer(serialize(img, NULL))
  freqs <- table(raw) / length(raw)
  -sum(freqs * log2(freqs))
}

# Given butterfly information data frame, returns a label describing the
# butterfly as "species [sex]".
buttLabel <- function(info) {
  paste(info$Binomial, ifelse(is.na(info$Sex), "",
         ifelse(tolower(info$Sex) == "female", "\u2640", "\u2642")))
}



# Classify butterfly images as cryptic or conspicuous----
#
# Plots each image in a scatterplot, with crypticness (mean detection time on
# the most cryptic background) on the x axis, and normalised range of detection
# times across backgrounds on the y axis. Range is normalised by dividing by
# crypticness, since that is a hard upper limit to the possible values of the
# range.
#
# Returns data frame with row per butterfly, columns "URL", "Class"
# ("Conspicuous" or "Cryptic"), "Time.range.pc", which is (bg_maxtime -
# bg_mintime) / bg_maxtime, where bg_maxtime is the maximum mean time on a
# background
ButtClassify <- function(scores) {
  outcomes <- scores[scores$score != "miss", ]

  # Calculate average time on each background
  bmagg <- aggregate(list(BgTime = outcomes$time), 
                     by = list(URL = outcomes$butterflyUrl, bg = outcomes$backgroundUrl), 
                     FUN = mean)
  buts <- reshape(bmagg, direction = "wide", idvar = "URL", timevar = "bg")
  mbb <- buts[, -1]
  
  # Most cryptic background for each butterfly 
  max.means <- apply(mbb, 1, max)
  # Range in times across backgrounds for each butterfly
  rng.means <- apply(mbb, 1, function(x) diff(range(x)))
  # Express range as a proportion of the possible range, given the maximum. We
  # do this because otherwise x and y are related to each other, since if e.g.
  # most cryptic time is 5 secs, the range can't be greater than 5 secs.
  rng.prop.means <- rng.means / max.means

  # Ensure we always get the same clusters with the same data
  set.seed(1)
  
  # Cluster into conspicuous and cryptic
  dat <- data.frame(x = max.means, y = 100 * rng.prop.means)
  clusters <- kmeans(dat, 2, iter.max = 100)
  # Work out which cluster is which. Conspicuous is short times, cryptic is long times
  consp <- clusters$cluster[which.min(max.means)]
  cryp <- clusters$cluster[which.max(max.means)]
  cutoff <- mean(c(max(max.means[clusters$cluster == consp]),
                   min(max.means[clusters$cluster == cryp])))

  data.frame(URL = buts$URL, Class = ifelse(clusters$cluster == consp, "Conspicuous", "Cryptic"), mostCrypticTime = max.means, Time.range.pc = dat$y)
}
