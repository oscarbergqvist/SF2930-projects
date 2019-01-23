# load packages
library("dplyr")
library("tidyverse")

# load data for women
setwd("/Users/Jessika/Documents/GitHub/SF2930-projects/project-1")
women <- read.csv("bodyfatwomen.csv")
view(women)

model_women <- lm(DEXfat ~ age + waistcirc + hipcirc + elbowbreadth + kneebreadth + anthro3a + anthro3b + anthro3c + anthro4, data = women)

# load data for men
men <- read.csv("bodyfatmen.csv")
view(men)

mean_density <- mean(men$density)
