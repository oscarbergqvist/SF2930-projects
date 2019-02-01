# load packages
library("car")
library("dplyr")
library("tidyverse")
library("corrplot")

setwd("/Users/Jessika/Documents/GitHub/SF2930-projects/project-1")

# load data for men
men <- read.csv("bodyfatmen.csv")

#create response variable using Siris Equation
men$bfm <- c(495/(men$density)-450)

#transform weight from pounds to kg
men$weight <- c(453.6*men$weight)
men$height <- c(2.54*men$height)

#fit model to data
model_men <- lm(bfm ~ age + weight + height + neck + chest + abdomen + hip + thigh + knee + ankle + biceps + forearm + wrist, data = men)
model_men <- lm(bfm ~ ., data = men)
anova(model_men)
summary(model_men)
plot(model_men)

# visualize correlation between different explanatory variables
men %>%
  dplyr::select(bfm,age,weight,height,neck,chest,abdomen,hip,thigh,knee,ankle,biceps,forearm,wrist) %>%
  cor %>%
  corrplot.mixed()

#Jämför exempel 3, 4 från övning 3 (script finns på Canvas)


#Spectral Decmoposition (ORIGIANAL DATASET)

X <- data.matrix(bodyfat, rownames.force = NA)
X_prim <- t(X)
X_corr <- X_prim %*% X
eigen <- eigen(X_corr, symmetric = TRUE, only.values = FALSE, EISPACK = FALSE)

lambda_max <- max(eigen$values)
lambda_min <- min(eigen$values)

k <- lambda_max %/% lambda_min
