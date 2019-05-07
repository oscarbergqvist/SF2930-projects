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

res_fun <- function(model_men) {
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

R_sq_fun <- function(fit, x, y){
  SS_res <- sum((y-fit)^2)
  SS_R <- sum((fit-mean(y))^2)
  SS_T <- SS_R + SS_res
  return(SS_R / SS_T)
}

#### BUILDING AN INITIAL MODEL ####

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


### OUTLIERS, LEVERAGE and INFLUENTIAL POINTS ###

### Residual analysis on the full model ###
res <- res_fun(model_men)
plot(res$res)
plot(res$res_stud)
plot(res$res_rstud)
plot(res$res_press)
jmf <- res$res - res$res_press
plot(jmf)
which(jmf>0.002)  #149, 178

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
#30, 53, 124, 143, 144, 149, 158, 168, 178

# Using this set of observation, manually look at each one to determine which are to be removed
men <- men[-c(30, 124, 178),]

# Fit linear model to reduced model
model_men <- lm(density ~ ., data = men)
anova(model_men)
summary(model_men)
plot(model_men)

res <- res_fun(model_men)
plot(res$res)
plot(res$res_stud)
plot(res$res_rstud)
plot(res$res_press)
jmf <- res$res - res$res_press
plot(jmf)
which(jmf>0.002)

### MULTICOLLINEARITY ###

# Visualize correlation between different explanatory variables
# men %>%
#   dplyr::select(age,weight,height,neck,chest,abdomen,hip,thigh,knee,ankle,biceps,forearm,wrist) %>%
#   cor %>%
#   corrplot.mixed()

# Spectral Decmoposition
X <- data.matrix(men[c(-1)], rownames.force = NA)
X <- scale(X)
X_prim <- t(X)
X_corr <- X_prim %*% X
eigen <- eigen(X_corr, symmetric = TRUE, only.values = FALSE, EISPACK = FALSE)
lambda_max <- max(eigen$values)
lambda_min <- min(eigen$values)
k <- lambda_max %/% lambda_min

# Calculate VIF (if VIF_j > 10, we have a problem)
vif <- vif(model_men)


### MODEL SELECTION ###

# Model selection using best subset selection and 
# 10-fold cross validation
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


# Model selection using LASSO and 
# 10-fold cross validation
y <- as.matrix(men[c(1)])
x <- as.matrix(men[c(-1)])
fit_lasso <- cv.glmnet(x, y, alpha = 1, nfolds = 10)
coef(fit_lasso, fit_lasso$lambda.1se)
coef(fit_lasso, fit_lasso$lambda.min)
rsq_lasso = 1-fit_lasso$cvm/as.numeric(var(y))
rsq_lasso = rsq_lasso[which(fit_lasso$lambda==fit_lasso$lambda.1se)]


##### MODEL VALIDATION #####

### Test errors of the lasso and best subset models 
y_test <- as.matrix(men_test[c(1)])
x_test <- as.matrix(men_test[c(-1)])
pfit <- predict.glmnet(fit_lasso$glmnet.fit, x_test, s=fit_lasso$lambda.1se, type="response")
R_sq_lasso <- R_sq_fun(pfit, x, y)
pfit <- predict(best_model, as.data.frame(x_test))
R_sq_bestsub <- R_sq_fun(pfit, x, y)


## Bootstrapping to find mean and sd of R-sqr of model fit
men_all = rbind(men, men_test)
y_all <- as.matrix(men_all[c(1)])
x_all <- as.matrix(men_all[c(-1)])
R_sq_bestsub_vec = NULL
R_sq_lasso_vec = NULL
for(i in 1:5000){
  sample_vec =  sample(nrow(men_all), 
                       nrow(men_all), 
                       replace = TRUE) 
  x_sample = x_all[sample_vec,]
  y_sample = y_all[sample_vec]
  
  pfit <- predict.glmnet(fit_lasso$glmnet.fit, 
                         x_sample, 
                         s=fit_lasso$lambda.1se, 
                         type="response")
  R_sq_lasso_vec <- c(R_sq_lasso_vec, 
                      R_sq_fun(pfit, x_sample, y_sample))
  
  pfit <- predict(best_model, as.data.frame(x_sample))
  R_sq_bestsub_vec <- c(R_sq_bestsub_vec, 
                        R_sq_fun(pfit, x_sample, y_sample))
}
rm(x_sample, y_sample, pfit)

R_sq_lasso_bootstrap <- mean(R_sq_lasso_vec)
R_sq_lasso_sd_bootstrap <- sd(R_sq_lasso_vec)
R_sq_bestsub_boot <- mean(R_sq_bestsub_vec)
R_sq_bestsub_sd_boot <- sd(R_sq_bestsub_vec)

hist(R_sq_lasso_vec)
hist(R_sq_bestsub_vec)
