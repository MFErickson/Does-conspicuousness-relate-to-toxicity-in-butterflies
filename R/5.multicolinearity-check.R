# Check multicolinearity of brms predictors
#Marilia F Erickson
#last update

# Fri Jun 20 16:26:21 2025 ------------------------------


#package ----
library(car)

#import data 
dt <- read.csv("output/conspicuousness.csv")

#Fit a linear model with the two predictors ----

vif_model <- lm(Daphnia.mortality.p ~ Dorsal.time + Ventral.time, data = dt)
  
#Calculate and return VIF values
vif_values <- vif(vif_model)

summary(vif_model)

summary(vif_values)


