# Clear console
cat("\014")
dev.off()

# Remove all variables
rm(list=ls(all=TRUE)) 

# Set working directory
setwd('C:\\Users\\Md Arafat H Khan\\Dropbox\\Academics - University of Texas at Dallas\\STAT 6340.18s Statistical and Machine Learning - Spring 2018\\Mini Projects\\Project 03')
# Load Data
bankruptcy = read.csv("bankruptcy.csv")

#Install if the package doesn't exist 
if (!require(ISLR)) install.packages('ISLR')
library(ISLR) 
if (!require(MASS)) install.packages('MASS')
library(MASS)
if (!require(e1071)) install.packages('e1071')
library(e1071)
if (!require(caret)) install.packages('caret')
library(caret)
if (!require(lattice)) install.packages('lattice')
library(lattice)
if (!require(class)) install.packages('class')
library(class)
if (!require(ggplot2)) install.packages('ggplot2')
library(ggplot2)
if (!require(pROC)) install.packages('pROC')
library(pROC)
source("decisionplot.R")

######## Exploratory Data Analysis ###############
head(bankruptcy)
# 4 Variables and one response. So we get rig of the extra columns
bankruptcy = bankruptcy[1:5]
str(bankruptcy)
summary(bankruptcy)
pairs(bankruptcy)
cor(bankruptcy)
boxplot(bankruptcy$X1~bankruptcy$Group,data=bankruptcy, 
main="Boxplot for X1 = (cash flow)/(total debt) vs Group", 
xlab="Group", ylab="X1")
boxplot(bankruptcy$X2~bankruptcy$Group,data=bankruptcy, 
main="Boxplot for X2 =(net income)/(total assets) vs Group", 
xlab="Group", ylab="X2")
boxplot(bankruptcy$X3~bankruptcy$Group,data=bankruptcy, 
main="Boxplot for X3 = =(current assets)/(current liabilities) vs Group", 
xlab="Group", ylab="X3")
boxplot(bankruptcy$X4~bankruptcy$Group,data=bankruptcy, 
main="Boxplot for X4 =(current assets)/(net sales) vs Group", 
xlab="Group", ylab="X4")
############## Logistic Regression ################
# Building different models
logit.model1234 <- glm(Group ~ X1 + X2 + X3 + X4, family = binomial, data = bankruptcy)
logit.model123 <- glm(Group ~ X1 + X2 + X3 , family = binomial, data = bankruptcy)
# Comparing models
anova(logit.model123, logit.model1234, test = "Chisq")
# Decision: Pr(>Chi) 0.3007 > 0.05 - Accept the null hypothesis.
# Thus our model reduced to logit.model123
# Building new model
logit.model12 <- glm(Group ~ X1 + X2 , family = binomial, data = bankruptcy)
# Comparing models
anova(logit.model12, logit.model123, test = "Chisq")
# Decision: Pr(>Chi) 0.0001784 < 0.05 - Reject the null hypothesis.
# Building new model
logit.model13 <- glm(Group ~ X1 + X3 , family = binomial, data = bankruptcy)
# Comparing models
anova(logit.model13, logit.model123, test = "Chisq")
# Decision: Pr(>Chi) 0.7277 < 0.05 - Accept the null hypothesis.
# Thus our model reduced to logit.model13
# Building new model
logit.model23 <- glm(Group ~ X2 + X3 , family = binomial, data = bankruptcy)
# Comparing models
anova(logit.model23, logit.model123, test = "Chisq")
# Decision: Pr(>Chi) 0.1282 > 0.05 - Reject the null hypothesis.
anova(logit.model13, logit.model1234, test = "Chisq")
# Decision: Pr(>Chi) 0.5509 > 0.05 - Accept the null hypothesis.
################ ROC-confusion matrix, sensitivity, specificity, and overall misclassification rate #########################
# Test and Train Data
bank.test <- subset(bankruptcy, (X3 > 4.15) | (X3<1.15))
bank.train <- subset(bankruptcy, (X3 < 4.15)& (X3>1.15))
# Fit final model
fit.model <- glm(Group ~ X1 + X3, family = binomial, data = bank.train)
# Test Error Rate  
lr.prob <- predict(fit.model, bank.test, type = "response")
lr.pred <- ifelse(lr.prob >= 0.5, "1", "0")
# Confusion Matrix, Sensitiviti, specificity, misclassification rate
confusionMatrix(as.factor(lr.pred),as.factor(bank.test[, "Group"]))
# ROC Curve
roc.lr <- roc(bank.test[, "Group"], lr.prob, levels = c("0", "1"))
plot(roc.lr, legacy.axes = T)
# Area under the curve AUC
roc.lr
# Decision Boundary plot
model <- glm(Group ~., data = bankruptcy[1:46, c("X1", "X3", "Group")], family=binomial(link='logit'))
class(model) <- c("lr", class(model))
predict.lr <- function(object, newdata, ...)
predict.glm(object, newdata, type = "response") > .5
model
decisionplot(model, bankruptcy[1:46, c("X1", "X3", "Group")], class = "Group", main = "Logistic Regression")

# Equation for the Decision Boundary
model

################ Analysis with all predictors #########################
# Test and Train Data
bank.test <- subset(bankruptcy, (X3 > 4.15) | (X3<1.15))
bank.train <- subset(bankruptcy, (X3 < 4.15)& (X3>1.15))
# Fit final model
fit.model <- glm(Group ~ X1 + X2 + X3 + X4, family = binomial, data = bank.train)
# Test Error Rate  
lr.prob <- predict(fit.model, bank.test, type = "response")
lr.pred <- ifelse(lr.prob >= 0.5, "1", "0")
# Confusion Matrix, Sensitiviti, specificity, misclassification rate
confusionMatrix(as.factor(lr.pred),as.factor(bank.test[, "Group"]))
# ROC Curve
roc.lr <- roc(bank.test[, "Group"], lr.prob, levels = c("0", "1"))
plot(roc.lr, legacy.axes = T)
# Area under the curve AUC
roc.lr
# Decision Boundary Equation
model <- glm(Group ~., data = bankruptcy[1:46, c("X1", "X2", "X3", "X4", "Group")], family=binomial(link='logit'))
class(model) <- c("lr", class(model))
predict.lr <- function(object, newdata, ...)
predict.glm(object, newdata, type = "response") > .5
model
# This is a hyper plane of dimension four nested in a 5 dimensional space and so we cannot plot it in R



################ LDA #######################
fit.model <- lda(Group ~ X1 + X3, data = bank.train)
fit.model
# Test Error Rate  
lr.prob <- predict(fit.model, bank.test, type = "response")
lr.pred <- ifelse(lr.prob >= 0.5, "1", "0")
# Confusion Matrix, Sensitiviti, specificity, misclassification rate
confusionMatrix(as.factor(lr.pred),as.factor(bank.test[, "Group"]))
# ROC Curve
roc.lr <- roc(bank.test[, "Group"], lr.prob, levels = c("0", "1"))
plot(roc.lr, legacy.axes = T)
# Area under the curve AUC
roc.lr
decisionplot(model, bankruptcy[1:46, c("X1", "X3", "Group")], class = "Group", main = "LDA")


################ QDA #######################
fit.model <- qda(Group ~ X1 + X3, data = bank.train)
fit.model
# Test Error Rate  
lr.pred <- ifelse(lr.prob >= 0.5, "1", "0")
# Confusion Matrix, Sensitiviti, specificity, misclassification rate
confusionMatrix(as.factor(lr.pred),as.factor(bank.test[, "Group"]))
# ROC Curve
roc.lr <- roc(bank.test[, "Group"], lr.prob, levels = c("0", "1"))
plot(roc.lr, legacy.axes = T)
# Area under the curve AUC
roc.lr
