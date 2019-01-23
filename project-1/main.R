# load packages
library("dplyr")
library("tidyverse")

# load data for women
setwd("/Users/Jessika/Documents/GitHub/SF2930-projects/project-1")
women <- read.csv("bodyfatwomen.csv")
view(women)

# load data for men
men <- read.csv("bodyfatmen.csv")

