#Analyses effect of number of plays of the same player on data
#XXXXXXXX
#last update
# Tue Oct 21 15:41:27 2025 ------------------------------

#load packages
library(JUtils)
library(lme4)
library(lmerTest)
# Do players get better at the game if they play repeatedly?


# Read downloaded game data
scores <- read.csv("data/score.csv")
sessions <- read.csv("data/session.csv")

# Remove duplicate species 
scores <- scores[scores$butterflyUrl != "3455 (1).png", ]

# Only outcomes, ie not misses
outcomes <- scores[scores$score != "miss", ]


# Determine game number for each user, i.e. first game played is 1, second is 2 etc.
sessions$userGameCount <- NA
for (u in unique(sessions$userId)) {
  userSess <- which(sessions$userId == u)
  sessions$userGameCount[userSess] <- seq_along(userSess)
}

# Add session info to outcomes
xx <- merge(outcomes, sessions[, c("userId", "sessionId", "userGameCount")])
xx$firstGame <- xx$userGameCount == 1
xx$user.n.games <- sapply(xx$userId, function(u) max(xx$userGameCount[xx$userId == u]))


# Do people get faster at locating butterflies each time they play the game?
# Only analyse people who played multiple games since poor players may have
# selected themselves out of the game, in which case we would detect an effect
# of better players playing later games, rather than players improving in later
# games
JPlotToPNG("output/player-learning.png", {
  par(mar = c(5, 4, 1, 1) + 0.1)
  # plot(time ~ userGameCount, xx[xx$user.n.games > 1 & xx$user.n.games < 15, ], pch = 16, col = adjustcolor(4, 0.1),
  plot(time ~ userGameCount, xx[xx$user.n.games > 1, ], pch = 16, col = adjustcolor(4, 0.1),
       xlab = "Player's game number", ylab = "Detection time (ms)")
  l <- lmer(time ~ userGameCount + (1|userId), xx[xx$user.n.games > 1, ])
  p <- predict(l, newdata = data.frame(userGameCount = seq(1, max(xx$userGameCount)), userId = xx$userId[1]), type = "response")
  lines(p, col = 2)
  sl <- summary(l)
  print(sl)
  rsq <- r.squaredGLMM(l)
  print(rsq)
  cat(sprintf("Yes, players' detection time decreased by around %g milliseconds (%g secs) per game (p = %g),\n  but it only explains %g%% of the total variation (marginal, i.e. fixed effects only r^2 = %g)\n",
              -coefficients(sl)[2, 1], -coefficients(sl)[2, 1] / 1000, coefficients(sl)[2, 5], 100 * rsq[1], rsq[1]))
  
}, width = 1200, res = 140)

