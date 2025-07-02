# Curates data frame

#XXXXXXXX
#last update
# Fri Jun 20 16:18:26 2025 ------------------------------

#packages----
library(png)
library(jpeg)
library(lubridate)
library(Durga)
library(JUtils)
source("R/0.functions.R")

# This script assumes that the game data has already been downloaded by running the script download-data.R
if (!file.exists("data/score.csv")) stop("You must run download-data.R before running this script")

# Read downloaded game data ----
scores <- read.csv("data/score.csv")
sessions <- read.csv("data/session.csv")

# Remove duplicate species 
scores <- scores[scores$butterflyUrl != "3455 (1).png", ]

# Only outcomes, ie not misses
outcomes <- scores[scores$score != "miss", ]


# Report summary stats----

cat(sprintf("After cleaning, %d sessions from %d users with %d scores: %s escapes, %d misses and %d hits\n\n",
            nrow(sessions), length(unique(sessions$userId)), nrow(scores),
            sum(scores$score == "escape"), sum(scores$score == "miss"), sum(scores$score == "hit")))


# Calculate conspicuous scores
con <- calculateConspicuousness(scores)

# Report correlations between different methods for measuring conspicuousness----
# cat(sprintf("Comparison of methods for quantifying conspicuousness,\n"))
# cat(sprintf("  correlation between mean detection time across all \n  backgrounds and mean detection time in most cryptic background:\n"))
# r <- cor(con$TimeConspicuousness, con$BgConspicTime, method = "spearman")
# cat(sprintf("  Spearman's ρ = %g (rank correlation)\n", r))
# r <- cor(con$TimeConspicuousness, con$BgConspicTime, method = "pearson")
# cat(sprintf("  Pearson correlation = %g\n\n", r))

# # Visually compare two methods for quantifying conspicuousness
# JPlotToPNG("output/consp-methods.png", {
#   visuallyCmpConspMethods(file.path("images/butterflies", con$URL[order(con$mostCrypticTime, decreasing = TRUE)]),
#                           file.path("images/butterflies", con$URL[order(con$TimeConspicuousness )]))
#   text(0.5, 2.2, "Mean time (across all backgrounds)", adj = 0, cex = 2, xpd = NA)
#   text(0.5, 0.8, "Most cryptic background", adj = 0, cex = 2, xpd = NA)
# }, width = 10000, height = 700)

# # Visually compare two methods for quantifying conspicuousness
# JPlotToPNG("output/bg-types.png", {
#   visuallyCmpConspMethods(file.path("images/butterflies", con$URL[order(con$mostCrypticTime, decreasing = TRUE)]),
#                           file.path("images/butterflies", con$URL[order(con$BgConspicTime)]))
#   text(0.5, 2.2, "Most cryptic background type (original method)", adj = 0, cex = 2, xpd = NA)
#   text(0.5, 0.8, "Most cryptic individual background (new method)", adj = 0, cex = 2, xpd = NA)
# }, width = 10000, height = 700)


# Build a spreadsheet----

# Get species, sex, angle info for each image, but only keep a single measure of conspicuousness
# CONSP_COL <- "BgConspicTime"
CONSP_COL <- "mostCrypticTime"
info <- incorporateSpecies(con[, c("URL", CONSP_COL, "mostCrypticBG")], "data/game_photo_legend.xlsx")
# Remove species without toxicity data
info <- info[!info$Binomial %in% c("Catopyrops florinda", "Neptis praslini"), ]


#### ??? We didn't get toxicity data for 2 species
info <- info[!info$Binomial %in% c("Catopyrops florinda", "Neptis praslini"), ]
cat(sprintf("Total species %d, %d dorsal images, %d ventral images, %d images\n", 
            length(unique(info$Binomial)), 
            length(unique(info$URL[info$Side == 'dorsal'])), 
            length(unique(info$URL[info$Side == 'ventral'])),
            length(unique(info$URL))))

# Calculate 95% CIs
ci <- bootstrapCI(scores, R = 100)
ci$Time <- NULL # Remove since it duplicates column in info
info <- merge(info, ci)
# Classify into cryptic or conspicuous
cls <- ButtClassify(scores)
info <- merge(info, cls)
# Reorder columns
info <- info[, c("Binomial", "Sex", "Photo", "URL", "Side", CONSP_COL, "mostCrypticBG",
                 "Time.lower", "Time.upper", "Rank", "Rank.lower", "Rank.upper", "Class", "Time.range.pc")]
# Split into dorsal and ventral then recombine
dorsal <- info[info$Side == "dorsal", ]
dorsal$Side <- NULL
names(dorsal) <- c("Binomial", "Sex", "Dorsal.photo", "Dorsal.URL", "Dorsal.time", "Dorsal.cryptic.BG", "Dorsal.Time.lower", "Dorsal.Time.upper", "Dorsal.Rank", "Dorsal.Rank.lower", "Dorsal.Rank.upper", "Dorsal.Class", "Dorsal.Time.range.pc")
ventral <- info[info$Side == "ventral", ]
ventral$Side <- NULL
names(ventral) <- c("Binomial", "Sex", "Ventral.photo", "Ventral.URL", "Ventral.time", "Ventral.cryptic.BG", "Ventral.Time.lower", "Ventral.Time.upper", "Ventral.Rank", "Ventral.Rank.lower", "Ventral.Rank.upper", "Ventral.Class", "Ventral.Time.range.pc")
comb <- merge(dorsal, ventral, all = TRUE)

# Sometimes there's one ventral for 2 dorsal images
reused <- numeric(0)
for (missing in which(is.na(comb$Ventral.photo))) {
  miss <- comb[missing,]
  vi <- which(comb$Binomial == miss$Binomial & is.na(comb$Dorsal.photo) & !is.na(comb$Ventral.photo))
  #cat(sprintf("%d <- %d\n", missing, vi))
  cols <- c("Ventral.photo", "Ventral.URL", "Ventral.time", "Ventral.cryptic.BG", "Ventral.Time.lower", "Ventral.Time.upper", "Ventral.Rank", "Ventral.Rank.lower", "Ventral.Rank.upper", "Ventral.Class", "Ventral.Time.range.pc")
  comb[missing, cols] <- comb[vi, cols]
  
  reused <- c(reused, vi)
}
comb <- comb[-unique(reused), ]

# Calculate difference in conspicuousness between the two sides
comb$Time.difference <- comb$Dorsal.time - comb$Ventral.time
# Put this new column near the start
nc <- ncol(comb)
comb <- comb[, c(1, 2, nc, 3:(nc - 1))]

# Add mean time on each background
timeOnBackgrounds <- function(scores, side = c("Dorsal", "Ventral")) {
  scores$bg <- sub(".jpg", "", scores$backgroundUrl)
  # Calculate mean time on each background
  agg <- aggregate(setNames(list(scores$time), side),
                   by = list(URL = scores$butterflyUrl, BG = scores$bg),
                   FUN = mean)
  # Long to wide
  w <- reshape(agg, direction = "wide", idvar = "URL", timevar = "BG", v.names = side)
  names(w)[1] <- paste0(side, ".URL")
  w
}
comb <- merge(merge(comb, timeOnBackgrounds(outcomes[outcomes$butterflyUrl %in% info$URL[info$Side == "dorsal"], ], "Dorsal"), all.x = TRUE), 
              merge(comb, timeOnBackgrounds(outcomes[outcomes$butterflyUrl %in% info$URL[info$Side == "ventral"], ], "Ventral"), all.x = TRUE))
# Shift Dorsal.URL column
nc <- ncol(comb)
comb <- comb[, c(2:5, 1, 6:nc)]


# Incorporate toxicity data ----
toxicity <- read.csv("data/death.dat.csv")[, 1:5]
names(toxicity) <- c("Key","N.daphnia", "deaths", "Daphnia.mortality.p","Phylo")

# Check species names against other data
toxicity$Binomial <- sub("_.*", "", toxicity$Key)
cat(sprintf("In toxicity but not in comb: %s\n", JToSentence(toxicity$Binomial[!toxicity$Binomial %in% comb$Binomial])))
cat(sprintf("In comb but not in toxicity: %s\n", JToSentence(comb$Binomial[!comb$Binomial %in% toxicity$Binomial])))

# Create a join key in the conspicuousness data
comb$Key <- ifelse(is.na(comb$Sex), comb$Binomial, paste(comb$Binomial, JCapitalise(comb$Sex), sep = "_"))
cat(sprintf("Key in toxicity but not in comb: %s\n", JToSentence(setdiff(toxicity$Key, comb$Key))))
cat(sprintf("Key in comb but not in toxicity: %s\n", JToSentence(setdiff(comb$Key, toxicity$Key))))

comb <- merge(comb, toxicity[, c("Key","N.daphnia", "deaths", "Daphnia.mortality.p", "Phylo")], by = "Key", all.x = TRUE)
# Scrap Key column
#comb$Key <- NULL
# Shift Daphnia.mortality.p column
nc <- ncol(comb)
comb <- comb[, c(1:3, nc, 4:(nc-1))]



# Write out the result ----
write.csv(comb, "output/conspicuousness.csv", row.names = FALSE)


# Image conspicuousness ----

# Plot to show mean butterfly conspicuousness
JPlotToPNG("output/conspicuous-butts.png", {
  info <- info[order(info[[CONSP_COL]], decreasing = TRUE), ]
  plotAll(file.path("images/butterflies", info$URL),
          nr = 10,
          buttLabel(info), cap.cex = 1.22,
          sprintf("Time = %.1f s", info[[CONSP_COL]] / 1000))
}, width = 2500, aspect = 1.35)



# Dorsal vs ventralcomparisons ----

source("R/0.dorsal-ventral.R")

