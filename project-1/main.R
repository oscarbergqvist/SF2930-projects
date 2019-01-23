# load packages
library("dplyr")
library("tidyverse")

setwd("/Users/Jessika/Documents/GitHub/SF2930-projects/project-1")

# load data for women
women <- read.csv("bodyfatwomen.csv")

model_women <- lm(DEXfat ~ age + waistcirc + hipcirc + elbowbreadth + kneebreadth + anthro3a + anthro3b + anthro3c + anthro4, data = women)
anova(model_women)

# load data for men
men <- read.csv("bodyfatmen.csv")

#create response variable using Siris Equation
men$bfm <- c(495/(men$density)-450)

#fit model to data
model_men <- lm(bfm ~ age + weight + height + neck + chest + abdomen + hip + thigh + knee + ankle + biceps + forearm + wrist, data = men)
anova(model_men)
