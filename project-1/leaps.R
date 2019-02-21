library("car")
library("dplyr")
library("tidyverse")
library("corrplot")
library("leaps")
library("MASS")
library("glmnet")

men <- read.csv("train.csv")
men$X <- NULL
men <- men[-c(30, 124, 178),]

# Multikolinjäritet + model selection mha LEAPS
y <- data.matrix(men[c(1)])
x <- data.matrix(men[c(-1)])

leaps <- leaps(x, y, method="Cp")
#plot(rowSums(leaps$which,2), leaps$Cp, xlim = c(3,6), ylim = c(3,6))
#abline(0, 1)
#text(rowSums(leaps$which,2), leaps$Cp, 1:length(rowSums(leaps$which,2))) # Kandidat: nr. 32

# Vi prövar nr 32
model_men <- lm(density ~ abdomen + biceps + wrist, data = men)
#anova(model_men)
#summary(model_men)
#plot(model_men)

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

# plot(res)
# plot(res_stud)
# plot(res_studlib)
# plot(res_rstud)
# plot(res_press)

jmf <- res - res_press
#plot(jmf)

### Investigating points
# Leverage points
leverage <- which(h_ii > hat_mean)

# Influential points using COVRATIO
influential <- which(covratio(model_men) > 1+3*p/n | covratio(model_men) < 1-3*p/n)

# Influential + leverage points
lev_infl <- intersect(leverage, influential)

# MULTICOLLINEARITY
# Scale to unity

# center
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
}
x <- data.matrix(men[c(3,7,12,14)])
x <- center_colmeans(x)

# scale
x <- scale(x, center = FALSE, scale = TRUE)

x_prim <- t(x)
x_corr <- x_prim %*% x
eigen <- eigen(x_corr, symmetric = TRUE, only.values = FALSE, EISPACK = FALSE)
lambda_max <- max(eigen$values)
lambda_min <- min(eigen$values)
k <- lambda_max %/% lambda_min

vif <- vif(model_men)
