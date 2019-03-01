setwd("C:/Users/Oscar Bergqvist/Desktop/SF2930-regression")
source("multiplot.R")

library(ggplot2)
library(foreach)
library(xlsx)
library(glmnet)
library(bestglm)
library(dplyr)

######################### Section 1: Read data #########################

# This is where your GLM data is read form tractors.csv into a table in R 
# Note that the folder in which you have tractors.csv must be set as the working directory 
###### You do not need to change anything in this section. The data will be sotred in a table named glmdata

glmdata <- read.table("Tractors.csv", header=TRUE, sep=";", dec="," )

### GROUP SELECTION ### 

riskdata <- within(glmdata, {
  risk <- ClaimCost / Duration
  Duration   <- NULL        
  ClaimCost  <- NULL
  NoOfClaims <- NULL
  RiskYear <- NULL
})
riskdata <- riskdata[which(riskdata$risk > 0),]

plot(riskdata$Weight, riskdata$risk, xaxp = c(0, 15000, 30), xlab = "Weight", ylab = "Risk")
plot(riskdata$VehicleAge, riskdata$risk, xlim = c(1,30), xaxp = c(0, 30, 30), xlab = "Age", ylab = "Risk")





######################### Section 2: Create groups & aggregate data #########################

# Now you need to modify your data so that you can perform a GLM analysis 

# First, any continuous variable needs to be grouped into discrete groups 
# The code below groups the variable weight, from you table glmdata, into six groups, and stores this in a new column, weight_group 
# It also groups the variable VehicleAge into four groups
###### This is only an example. You need to create your own groups, with breaks that suit your data
###### You might also want to group other variables from glmdata, in a similar manner

glmdata$weight_group <- cut(glmdata$Weight, 
                       breaks = c(-Inf, 500, 2000, 5000, 6500, Inf), 
                       labels = c("01_<499kg", "02_500-1999kg", "03_2000-4999kg", "04_5000-6499kg", "05_>6500kg"), 
                       right = FALSE)

glmdata$age_group <- cut(glmdata$VehicleAge, 
                            breaks = c(-Inf, 3, 6, 15, Inf), 
                            labels = c("01_<3years", "02_3-5years", "03_6-14years", "04_>15years"), 
                            right = FALSE)

  
# Secondly, we want to aggregate the data.
# That is, instead of having one row per tractor, we want one row for each existing combination of variables 
# This code aggregates columns 6-8 of glmdata, by three variables: weight_group, Climate, and ActivityCode 
# Tha aggregated data is stored in a new table, glmdata2 
##### You need to consider if there are any other variables you want to aggregate by, and modify the code accordingly 

glmdata2 <- aggregate(glmdata[,6:8],by=list(weight_group = glmdata$weight_group, 
                                            Climate = glmdata$Climate,
                                            ActivityCode = glmdata$ActivityCode,
                                            age_group = glmdata$age_group), FUN=sum, na.rm=TRUE)



### GROUP SELECTION VERIFICATION ### 

riskdata2 <- within(glmdata2, {
  risk <- ClaimCost / Duration
  Duration   <- NULL        
  ClaimCost  <- NULL
  NoOfClaims <- NULL
})
riskdata2 <- riskdata2[which(riskdata2$risk > 0),]

plot(riskdata2$weight_group, riskdata2$risk, xlab = "Weight", ylab = "Risk")
plot(riskdata2$Climate, riskdata2$risk)
plot(riskdata2$ActivityCode, riskdata2$risk)
plot(riskdata2$age_group, riskdata2$risk, xlab = "Age", ylab = "Risk")



# We then do some preparation for the output the GLM function will give.
# This piece of code creates a new table, glmdata3, with a row per variable and group, and with data on the total duration corresponding to this group.
##### You need ot modify the code to take into account any changes in variables you're using 

glmdata3 <-
  data.frame(rating.factor =
               c(rep("Weight", nlevels(glmdata2$weight_group)),
                 rep("Climate", nlevels(glmdata2$Climate)),
                 rep("ActivityCode", nlevels(glmdata2$ActivityCode)),
                 rep("Age", nlevels(glmdata2$age_group))),
             class =
               c(levels(glmdata2$weight_group),
                 levels(glmdata2$Climate),
                 levels(glmdata2$ActivityCode),
                 levels(glmdata2$age_group)),
             stringsAsFactors = FALSE)

new.cols <-
  foreach (rating.factor = c("weight_group", "Climate", "ActivityCode", "age_group"),
           .combine = rbind) %do%
           {
             nclaims <- tapply(glmdata2$NoOfClaims, glmdata2[[rating.factor]], sum)
             sums <- tapply(glmdata2$Duration, glmdata2[[rating.factor]], sum)
             n.levels <- nlevels(glmdata2[[rating.factor]])
             contrasts(glmdata2[[rating.factor]]) <-
               contr.treatment(n.levels)[rank(-sums, ties.method = "first"), ]
             data.frame(duration = sums, n.claims = nclaims)
           }
glmdata3 <- cbind(glmdata3, new.cols)
rm(new.cols)

############## LASSO ################
# x <- model.matrix(NoOfClaims ~ weight_group + Climate + ActivityCode + age_group, 
#                   glmdata2)
# y <- glmdata2$NoOfClaims
# cv.out <- cv.glmnet(x, y, offset = log(glmdata2$Duration),
#                     alpha=1,
#                     family="poisson",
#                     type.measure = "deviance")
# plot(cv.out)
# lasso_coef <- coef(cv.out, s=cv.out$lambda.1se)
# 
# # Lasso with glmnet doesn't work using Gamma("log") family


####### ALL SUBSETS BESTGLM #########

# CLAIM FREQ 
bestglm_data_freq <- within(glmdata2, {
  Duration   <- NULL        
  ClaimCost  <- NULL
  y    <- NoOfClaims    
  NoOfClaims  <- NULL         
})

bestglm_data_freq <- bestglm_data_freq[, c("weight_group", "Climate","ActivityCode","age_group","y")]

bestglm_freq <-
  bestglm(Xy = bestglm_data_freq,
          offset(log(glmdata2$Duration)),
          family = poisson,
          IC = "AIC",                 
          method = "exhaustive")

rels_bestmodel_freq = exp(coef(bestglm_freq$BestModel)[-1])

glmdata3$rels.frequency <-   c(ifelse(rep(bestglm_freq$BestModels$weight_group[1], nlevels(glmdata2$weight_group)), 
                                      c(1, rels_bestmodel_freq[select_vars(names(rels_bestmodel_freq), starts_with("weight_group", ignore.case = TRUE))]), 
                                      rep(1, nlevels(glmdata2$weight_group))), 
                               ifelse(rep(bestglm_freq$BestModels$Climate[1], nlevels(glmdata2$Climate)), 
                                      c(1, rels_bestmodel_freq[select_vars(names(rels_bestmodel_freq), starts_with("Climate", ignore.case = TRUE))]),
                                      rep(1, nlevels(glmdata2$Climate))),
                               ifelse(rep(bestglm_freq$BestModels$ActivityCode[1], nlevels(glmdata2$ActivityCode)), 
                                      c(1, rels_bestmodel_freq[select_vars(names(rels_bestmodel_freq), starts_with("ActivityCode", ignore.case = TRUE))]), 
                                      rep(1, nlevels(glmdata2$ActivityCode))), 
                               ifelse(rep(bestglm_freq$BestModels$age_group[1], nlevels(glmdata2$age_group)), 
                                      c(1, rels_bestmodel_freq[select_vars(names(rels_bestmodel_freq), starts_with("Age", ignore.case = TRUE))]), 
                                      rep(1, nlevels(glmdata2$age_group))))


#CLAIM SEVERITY
bestglm_data_sev <- within(glmdata2, {
  Duration   <- NULL        
  y    <-  glmdata2$ClaimCost/glmdata2$NoOfClaims    
  NoOfClaims <- NULL
  ClaimCost <- NULL       
})
bestglm_data_sev <- bestglm_data_sev[which(bestglm_data_sev$y>0), c("weight_group", "Climate","ActivityCode","age_group","y")]

bestglm_sev <-
  bestglm(Xy = bestglm_data_sev,
          family = Gamma("log"),
          #weights = glmdata2$NoOfClaims[which(glmdata2$y>0)],
          IC = "AIC",                 
          method = "exhaustive")

rels_bestmodel_sev = exp(coef(bestglm_sev$BestModel)[-1])

glmdata3$rels.severity <-   c(ifelse(rep(bestglm_sev$BestModels$weight_group[1], nlevels(glmdata2$weight_group)), 
                                     c(1, rels_bestmodel_sev[select_vars(names(rels_bestmodel_sev), 
                                                                         starts_with("weight_group", ignore.case = TRUE))]), 
                                     rep(1, nlevels(glmdata2$weight_group))), 
                              ifelse(rep(bestglm_sev$BestModels$Climate[1], nlevels(glmdata2$Climate)), 
                                     c(1, rels_bestmodel_sev[select_vars(names(rels_bestmodel_sev), 
                                                                         starts_with("Climate", ignore.case = TRUE))]),
                                     rep(1, nlevels(glmdata2$Climate))),
                              ifelse(rep(bestglm_sev$BestModels$ActivityCode[1], nlevels(glmdata2$ActivityCode)), 
                                     c(1, rels_bestmodel_sev[select_vars(names(rels_bestmodel_sev), 
                                                                         starts_with("ActivityCode", ignore.case = TRUE))]), 
                                     rep(1, nlevels(glmdata2$ActivityCode))), 
                              ifelse(rep(bestglm_sev$BestModels$age_group[1], nlevels(glmdata2$age_group)), 
                                     c(1, rels_bestmodel_sev[select_vars(names(rels_bestmodel_sev), 
                                                                         starts_with("Age", ignore.case = TRUE))]), 
                                     rep(1, nlevels(glmdata2$age_group))))


## RISK
glmdata3$rels.risk <- with(glmdata3, rels.frequency*rels.severity)


## BASE LEVEL

baselevel <- sum(glmdata$ClaimCost[which(glmdata$RiskYear == '2016')]) 
premium = baselevel / 0.9

gamma_total = NULL
for (i in 1:nrow(glmdata2)){
  gamma_age <- glmdata3$rels.risk[which(glmdata3$class == glmdata2[i, "age_group"])]
  gamma_climate <- glmdata3$rels.risk[which(glmdata3$class == glmdata2[i, "Climate"])]
  gamma_weight <- glmdata3$rels.risk[which(glmdata3$class == glmdata2[i, "weight_group"])]
  gamma_activity <- glmdata3$rels.risk[which(glmdata3$class == glmdata2[i, "ActivityCode"])]
  gamma_total <- c(gamma_total, gamma_age * gamma_climate * gamma_activity * gamma_weight)
}
glmdata2$yearly_cost <- gamma_total * premium / sum(gamma_total)


######################### Section 4: Plotting #########################

# In this section, the results from the GLM are plotted.

# First, long variable names need to be cut, to fit into the plots.
# This row of code cuts away everything except for the first letter for variable names belonging to activity codes.
##### If you have long variable names, modify here to cut them.
glmdata3[glmdata3$rating.factor == "ActivityCode",2] <- substr(glmdata3$class,1,1)[10:20]  


# Then the results are plotted. This code plots the GLM factors for frequency, severity, and total risk, for the three variables Weight, Climate, and Activity code.
##### If you have changed what variables are included in your model, add, remove, or modify sections of this code to plot them. 
##### This is also where you can make changes to change the look of your plots, if you would like to.

p1 <- ggplot(subset(glmdata3, rating.factor=="Weight"), aes(x=class, y=rels.frequency)) + 
      geom_point(colour="blue") + geom_line(aes(group=1), colour="blue") + ggtitle("Weight: frequency factors") +
      geom_text(aes(label=paste(round(rels.frequency,2))), nudge_y=1) +theme(axis.text.x = element_text(angle = 30, hjust = 1))

p2 <- ggplot(subset(glmdata3, rating.factor=="Weight"), aes(x=class, y=rels.severity)) + 
      geom_point(colour="blue") + geom_line(aes(group=1), colour="blue") + ggtitle("Weight: severity factors") +
      geom_text(aes(label=paste(round(rels.severity,2))), nudge_y=0.5)+theme(axis.text.x = element_text(angle = 30, hjust = 1))

p3 <- ggplot(subset(glmdata3, rating.factor=="Weight"), aes(x=class, y=rels.risk)) + 
      geom_point(colour="blue") + geom_line(aes(group=1), colour="blue") + ggtitle("Weight: risk factors") +
      geom_text(aes(label=paste(round(rels.risk,2))), nudge_y=1.6)+theme(axis.text.x = element_text(angle = 30, hjust = 1))

p4 <- ggplot(subset(glmdata3, rating.factor=="Climate"), aes(x=class, y=rels.frequency)) + 
  geom_point(colour="blue") + geom_line(aes(group=1), colour="blue") + ggtitle("Climate: frequency factors") +
  geom_text(aes(label=paste(round(rels.frequency,2))), nudge_y=0.05)

p5 <- ggplot(subset(glmdata3, rating.factor=="Climate"), aes(x=class, y=rels.severity)) + 
  geom_point(colour="blue") + geom_line(aes(group=1), colour="blue") + ggtitle("Climate: severity factors") +
  geom_text(aes(label=paste(round(rels.severity,2))), nudge_y=0.1)

p6 <- ggplot(subset(glmdata3, rating.factor=="Climate"), aes(x=class, y=rels.risk)) + 
  geom_point(colour="blue") + geom_line(aes(group=1), colour="blue") + ggtitle("Climate: risk factors") +
  geom_text(aes(label=paste(round(rels.risk,2))), nudge_y=0.1)

p7 <- ggplot(subset(glmdata3, rating.factor=="ActivityCode"), aes(x=class, y=rels.frequency)) + 
  geom_point(colour="blue") + geom_line(aes(group=1), colour="blue") + ggtitle("ActivityCode: frequency factors") +
  geom_text(aes(label=paste(round(rels.frequency,2))), nudge_y=0.5) 

p8 <- ggplot(subset(glmdata3, rating.factor=="ActivityCode"), aes(x=class, y=rels.severity)) + 
  geom_point(colour="blue") + geom_line(aes(group=1), colour="blue") + ggtitle("ActivityCode: severity factors") +
  geom_text(aes(label=paste(round(rels.severity,2))), nudge_y=0.5)

p9 <- ggplot(subset(glmdata3, rating.factor=="ActivityCode"), aes(x=class, y=rels.risk)) + 
  geom_point(colour="blue") + geom_line(aes(group=1), colour="blue") + ggtitle("ActivityCode: risk factors") +
  geom_text(aes(label=paste(round(rels.risk,2))), nudge_y=0.5)

p10 <- ggplot(subset(glmdata3, rating.factor=="Age"), aes(x=class, y=rels.frequency)) + 
  geom_point(colour="blue") + geom_line(aes(group=1), colour="blue") + ggtitle("Age: frequency factors") +
  geom_text(aes(label=paste(round(rels.frequency,2))), nudge_y=0.5) 

p11 <- ggplot(subset(glmdata3, rating.factor=="Age"), aes(x=class, y=rels.severity)) + 
  geom_point(colour="blue") + geom_line(aes(group=1), colour="blue") + ggtitle("Age: severity factors") +
  geom_text(aes(label=paste(round(rels.severity,2))), nudge_y=0.5)

p12 <- ggplot(subset(glmdata3, rating.factor=="Age"), aes(x=class, y=rels.risk)) + 
  geom_point(colour="blue") + geom_line(aes(group=1), colour="blue") + ggtitle("Age: risk factors") +
  geom_text(aes(label=paste(round(rels.risk,2))), nudge_y=0.5)



multiplot(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12, cols=4)



######################### Section 5: Export factors to Excel #########################

#As a last step, the risk factors are exported to excel. 
# The dopcument will be saved in the folder set as your working directory.

write.xlsx(glmdata3, "glmfactors.xlsx")


