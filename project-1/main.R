library("car")
library("dplyr")
library("tidyverse")
library("corrplot")
library("leaps")
library("MASS")
library("glmnet")

setwd("/Users/Jessika/Documents/GitHub/SF2930-projects/project-1")
setwd("C:/Users/NextLevel/Desktop/SF2930-projects/project-1")
setwd("C:/Users/Oscar Bergqvist/Desktop/SF2930-projects/project-1")

SI <- function(men){
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
  return(men)
}

res <- function(model_men) {
  n = nrow(model_men$model)
  p = ncol(model_men$model) - 1
  h_ii = lm.influence(model_men)$hat
  hat_mean <- 2*p/n
  res = residuals(model_men)
  MS_res = sum(res^2)/(n-p) 
  res_std   = res / sqrt(MS_res)
  res_stud  = res / sqrt(MS_res*(1-h_ii))
  res_press = res / (1 - h_ii)
  S_sq = ((n-p)*MS_res - res^2/(1-h_ii)) / (n-p-1)
  res_rstud = res / sqrt(S_sq*(1-h_ii))
  return(list("res"=res,
              "res_std"=res_std,
              "res_stud"=res_stud,
              "res_rstud"=res_rstud,
              "res_press"=res_press))
}

# Read data 
men <- read.csv("train.csv")
men_test <- read.csv("test.csv")
men$X <- NULL
men_test$X <- NULL

# Transform to SI
men = SI(men)
men_test = SI(men_test)

# Initial model
model_men <- lm(density ~ ., data = men)
anova(model_men)
summary(model_men)
plot(model_men)

### Residual analysis on the full model ###
res <- res(model_men)
plot(res$res)
plot(res$res_stud)
plot(res$res_rstud)
plot(res$res_press)
jmf <- res$res - res$res_press
plot(jmf)

### Investigating points ###

# Leverage points
n = nrow(model_men$model)
p = ncol(model_men$model) - 1
h_ii = lm.influence(model_men)$hat
hat_mean <- 2*p/n

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

leaps <- leaps(x, y, method="adjr2", nbest = 1)

fold_width <- floor(nrow(men)/10)
adjR_sq_models <- NULL
for(model_ind in 1:13){
  model <- leaps$which[model_ind,]
  adjR_sq <- NULL
  for(fold_ind in 0:9){
    val_ind <- (fold_width*fold_ind+1):(fold_width*fold_ind + fold_width)
    subset_vec <- rep(TRUE, nrow(men)) 
    subset_vec[val_ind] <- FALSE
    model_cv <- lm(formula(men[c(TRUE, model)]), data = men, subset = subset_vec)
    pfit <- predict(model_cv, newdata = men[val_ind,])
    y_val = men[val_ind,"density"]
    SS_res <- sum((y_val-pfit)^2)
    SS_R <- sum((pfit-mean(y_val))^2)
    SS_T <- SS_R + SS_res
    adjR_sq <- c(adjR_sq, SS_R / SS_T)
  }
  adjR_sq_models <- c(adjR_sq_models, mean(adjR_sq)) 
}
rm(SS_res, SS_R, SS_T, adjR_sq, y_val, subset_vec, model_cv,
   pfit, model, fold_width, fold_ind, model_ind, val_ind)

best_model <- leaps$which[which.max(adjR_sq_models),]
best_model <- lm(formula(men[c(TRUE, best_model)]), data = men)


# CP 
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

y_test <- as.matrix(men_test[c(1)])
x_test <- as.matrix(men_test[c(-1)])

pfit <- predict.glmnet(lasso, x_test, s=0, type="response")
SS_res <- sum((y_test-pfit)^2)
SS_R <- sum((pfit-mean(y_test))^2)
SS_T <- SS_R + SS_res
R_sq_lasso <- SS_R / SS_T

pfit <- predict(model_men_Cp, as.data.frame(x_test))
SS_res <- sum((y_test-pfit)^2)
SS_R <- sum((pfit-mean(y_test))^2)
SS_T <- SS_R + SS_res
R_sq_bestsub <- SS_R / SS_T

