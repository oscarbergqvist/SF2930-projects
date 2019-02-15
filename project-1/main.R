# load packages
library("car")
library("dplyr")
library("tidyverse")
library("corrplot")

setwd("/Users/Jessika/Documents/GitHub/SF2930-projects/project-1")

# load data for men
men <- read.csv("bodyfatmen.csv")

#transform weight from pounds to kg
men$weight <- c(0.001*453.6*men$weight)
men$height <- c(2.54*0.01*men$height)
men$neck <- c(0.01*men$neck)
men$chest <- c(0.01*men$chest)
men$abdomen <- c(0.01*men$abdomen)
men$hip <- c(0.01*men$hip)
men$thigh <- c(0.01*men$thigh)
men$knee <- c(0.01*men$knee)
men$ankle <- c(0.01*men$ankle)
men$biceps <- c(0.01*men$biceps)
men$forearm <- c(0.01*men$forearm)
men$wrist <- c(0.01*men$wrist)

#fit model to data
model_men <- lm(density ~ ., data = men)
anova(model_men)
summary(model_men)
plot(model_men)

#visualize correlation between different explanatory variables
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
