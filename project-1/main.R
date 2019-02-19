# load packages
library("car")
library("dplyr")
library("tidyverse")
library("corrplot")
library("leaps")
library("MASS")

setwd("/Users/Jessika/Documents/GitHub/SF2930-projects/project-1")
setwd("C:/Users/NextLevel/Desktop/SF2930-projects/project-1")

# load data for men, create test and training datasets
men <- read.csv("bodyfatmen.csv")

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

train <- sample_frac(men, 0.8)
sid <- as.numeric(rownames(train))
test <- men[-sid,]
write.csv(train, file = "train.csv")
write.csv(test, file = "test.csv")

men <- read.csv("train.csv")

#fit linear model to data
model_men <- lm(density ~ ., data = men)
anova(model_men)
summary(model_men)
plot(model_men)

# Residual analysis on the full model
n = nrow(model_men$model)
p = ncol(model_men$model) - 1
h_ii = lm.influence(model_men)$hat

res = residuals(model_men)
MS_res = sum(res^2)/(n-p) 
res_std   = res / sqrt(MS_res)
res_stud  = res / sqrt(MS_res*(1-h_ii))
res_press = res / (1 - h_ii)
S_sq = ((n-p)*MS_res - res^2/(1-h_ii)) / (n-p-1)
res_rstud = res / sqrt(S_sq*(1-h_ii))
res_studlib = studres(model_men)  
# Why the difference between library stud and calculated stud residual


plo#Remove row 39, which is deemed to be an outler
men <- men[-c(30, 102),]

#fit linear model to data AGAIN
model_men <- lm(density ~ ., data = men)
anova(model_men)
summary(model_men)
plot(model_men)
studres(model_men)

#visualize correlation between different explanatory variables
men %>%
  dplyr::select(age,weight,height,neck,chest,abdomen,hip,thigh,knee,ankle,biceps,forearm,wrist) %>%
  cor %>%
  corrplot.mixed()

#Spectral Decmoposition (ORIGIANAL DATASET)
X <- data.matrix(men, rownames.force = NA)
X_prim <- t(X)
X_corr <- X_prim %*% X
eigen <- eigen(X_corr, symmetric = TRUE, only.values = FALSE, EISPACK = FALSE)

lambda_max <- max(eigen$values)
lambda_min <- min(eigen$values)

k <- lambda_max %/% lambda_min

# NU KAN VI KONSTATERA ATT: PROBLEM MED MULTIKOLINJÄRITET, OCH MÅNGA REGRESSORS HAR INVERKAN PÅ VARIABELN (DENSITY)

#Model selection mha leaps
#Vi kan använda oss av leaps (istället för backward/forward selection) pga antalet modeller är förhållandevis lågt 2^K
leaps = regsubsets(density ~ ., data = men, nbest=10)
plot(leaps, scale="adjr2")
