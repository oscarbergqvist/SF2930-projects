library("car")
library("dplyr")
library("tidyverse")
library("corrplot")
library("leaps")
library("MASS")
library("glmnet")

setwd("/Users/Jessika/Documents/GitHub/SF2930-projects/project-1")
setwd("C:/Users/NextLevel/Desktop/SF2930-projects/project-1")

# # load data for men, rescale variables to SI units, create test and training datasets
# men <- read.csv("bodyfatmen.csv")
# 
# men$weight <- c(0.001*453.6*men$weight)
# men$height <- c(2.54*0.01*men$height)
# men$neck <- c(0.01*men$neck)
# men$chest <- c(0.01*men$chest)
# men$abdomen <- c(0.01*men$abdomen)
# men$hip <- c(0.01*men$hip)
# men$thigh <- c(0.01*men$thigh)
# men$knee <- c(0.01*men$knee)
# men$ankle <- c(0.01*men$ankle)
# men$biceps <- c(0.01*men$biceps)
# men$forearm <- c(0.01*men$forearm)
# men$wrist <- c(0.01*men$wrist)
# sid <- as.numeric(rownames(train))
# train <- sample_frac(men, 0.8)
# test <- men[-sid,]
# write.csv(train, file = "train.csv")
# write.csv(test, file = "test.csv")
    
# Read data and fit linear model to data
men <- read.csv("train.csv")
men$X <- NULL
model_men <- lm(density ~ ., data = men)
anova(model_men)
summary(model_men)
plot(model_men)

### Residual analysis on the full model ###
n = nrow(model_men$model)
p = ncol(model_men$model) - 1
h_ii = lm.influence(model_men)$hat
hat_mean <- 2*p/n

# Residuals
res = residuals(model_men)
MS_res = sum(res^2)/(n-p) 
res_std   = res / sqrt(MS_res)
res_stud  = res / sqrt(MS_res*(1-h_ii))
res_press = res / (1 - h_ii)
S_sq = ((n-p)*MS_res - res^2/(1-h_ii)) / (n-p-1)
res_rstud = res / sqrt(S_sq*(1-h_ii))
res_studlib = studres(model_men)

plot(res)
plot(res_stud)
plot(res_studlib)
plot(res_rstud)
plot(res_press)

jmf <- res - res_press
plot(jmf)

### Investigating points
# Leverage points
leverage <- which(h_ii > hat_mean)

# Influential points using COVRATIO
influential <- which(covratio(model_men) > 1+3*p/n | covratio(model_men) < 1-3*p/n)

# Influential + leverage points
lev_infl <- intersect(leverage, influential)

# Using this set of observation, manually look at each one to determine which are to be removed
men <- men[-c(30, 124, 178),]

# Fit linear model to reduced model
model_men <- lm(density ~ ., data = men)
anova(model_men)
summary(model_men)
plot(model_men)
studres(model_men)

### MULTICOLLINEARITY ###

# Visualize correlation between different explanatory variables
men %>%
  dplyr::select(age,weight,height,neck,chest,abdomen,hip,thigh,knee,ankle,biceps,forearm,wrist) %>%
  cor %>%
  corrplot.mixed()

# Spectral Decmoposition
X <- data.matrix(men[c(-1)], rownames.force = NA)
X_prim <- t(X)
X_corr <- X_prim %*% X
eigen <- eigen(X_corr, symmetric = TRUE, only.values = FALSE, EISPACK = FALSE)
lambda_max <- max(eigen$values)
lambda_min <- min(eigen$values)
k <- lambda_max %/% lambda_min

# Calculate VIF (if VIF_j > 10, we have a problem)
vif <- vif(model_men)

### Model selection ###

# Multikolinjäritet + model selection mha LEAPS
y <- data.matrix(men[c(1)])
x <- data.matrix(men[c(-1)])

leaps <- leaps(x, y, method="Cp")
plot(rowSums(leaps$which,2), leaps$Cp, xlim = c(0,14), ylim = c(0,14))
abline(0, 1)
text(rowSums(leaps$which,2), leaps$Cp, 1:length(rowSums(leaps$which,2))) # Kandidater: nr. 32, 48

# Vi prövar nr 32
model_men_Cp <- lm(density ~ weight + abdomen + biceps + wrist, data = men)
anova(model_men_Cp)
summary(model_men_Cp)
plot(model_men_Cp)
studres(model_men_Cp)

# Multikolinjäritet + model selection mha LASSO
y <- as.matrix(men[c(1)])
x <- as.matrix(men[c(-1)])

# Välj optimala lambda genom att iterera 100 ggr över 10-fold modeller
lambdas = NULL
for (i in 1:5)
{
  fit <- cv.glmnet(x, y, alpha = 1, nfolds = 10) 
  errors = data.frame(fit$lambda, fit$cvm)
  lambdas <- rbind(lambdas, errors)
  print("*")
}
# take mean cvm for each lambda
lambdas <- aggregate(lambdas[, 2], list(lambdas$fit.lambda), mean)

# select the best one
bestindex = which(lambdas[2]==min(lambdas[2]))
bestlambda = lambdas[bestindex,1]

# and now run glmnet once more with it
lasso <- glmnet(x, y, alpha = 1, lambda=bestlambda)
pfit <- predict.glmnet(lasso, x, s = 0, type="response")
plot(pfit,y)

r2_lasso <- lasso$dev.ratio

model_men <- fit 
### Hur ska vi jämföra våra två modeller?