# load packages
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
anova(model_men)
summary(model_men)
plot(model_men)

# visualize correlation between different explanatory variables
men %>%
  dplyr::select(bfm,age,weight,height,neck,chest,abdomen,hip,thigh,knee,ankle,biceps,forearm,wrist) %>%
  cor %>%
  corrplot.mixed()

#Jämför exempel 3, 4 från övning 3 (script finns på Canvas)