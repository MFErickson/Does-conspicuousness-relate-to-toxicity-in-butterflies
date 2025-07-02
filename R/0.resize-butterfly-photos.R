# Resizes butterfly photos so they all have similar numbers of non-transparent pixels
#XXXXXXXX
#last update
# Fri Jun 20 16:13:24 2025 ------------------------------

#packages----
library(imager)
library(png)
library(JUtils)
source("R/functions.R")


# Check and correct butterfly images sizes----

# Number of desired non-transparent pixels in each butterfly image
TARGET_COUNT <- 30000



# Scale and save a single image
scaleAndSaveImage <- function(inFile, scale, outFile) {
  x <- load.image(inFile)
  
  # Scale; the height is determined automatically so that
  # the aspect ratio is preserved
  y <- imresize(x, scale, interpolation = 6)
  
  # show the scaled image
  #display(y)
  
  save.image(y, outFile)
}

# Scale all PNG images to a target number of non-transparent pixels
scaleAll <- function(indir, outdir, targetPixels) {
  bf <- list.files(indir, "*.png", full.names = T)
  buts <- sapply(bf, function(fn) readPNG(fn, native = FALSE))
  # Count no. of non-transparent pixels
  notTrans <- sapply(buts, function(but) { sum(but[,,4] > 0.5) })
  
  for (i in seq_along(bf)) {
    print(i)
    scaleAndSaveImage(bf[i], sqrt(targetPixels) / sqrt(notTrans[i]), file.path(outdir, basename(bf[i])))
  }
}

compareNoTransPixels <- function(..., proportion = FALSE) {
  
  dirs <- list(...)
  # Count no. of non-transparent pixels
  countPix <- function(dir) {
    bf <- list.files(dir, "*.png", full.names = T)
    buts <- sapply(bf, function(fn) {
      png <- readPNG(fn, native = FALSE)
      np <- sum(png[,,4] > 0.5)
      if (proportion) {
        np / length(png[, , 4])
      } else {
        np
      }
    })
  }
  
  ld <- lapply(dirs, function(dir) density(countPix(dir)))
  xlab <- if (proportion) { "Proportion non-transparent pixels" } else { "Non-transparent pixels" }
  JPlotDensities(ld, legendLabels = unlist(dirs), xlab = xlab)
}


# Read all the images in Big butts and resize them into Good butts 
scaleAll("../images/Big butts", "../images/Good butts", TARGET_COUNT)

#compareNoTransPixels("../findbutterfly/images/Big butts", "images/Good butts")
compareNoTransPixels("images/butterflies")
abline(v = TARGET_COUNT)

JPlotToPNG("butts-original.png", plotAllInDir("../images/Big butts"), width = 2000, aspect = 1)
JPlotToPNG("butts-scaled.png", plotAllInDir("../images/Good butts"), width = 2000, aspect = 1)

