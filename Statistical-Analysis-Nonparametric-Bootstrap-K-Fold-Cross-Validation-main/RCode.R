# Clear console
cat("\014")
dev.off()

# Remove all variables
rm(list=ls(all=TRUE)) 

# Set working directory
setwd('<FOLDERPATH>')
# Load Data
gpaData = read.csv("gpa.csv")

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
if (!require(boot)) install.packages('boot')
library(boot)
if (!require(lmtest)) install.packages('lmtest')
library(lmtest)
if (!require(hmeasure)) install.packages('hmeasure')
library(hmeasure)
if (!require(leaps)) install.packages('leaps')
library(leaps)

########### scatterplot of gpa against act and the strength of linear relationship ######################
str(gpaData)
plot(gpaData$act,gpaData$gpa)
cor(gpaData)

########### A point estimate of population correlation between gpa and act, 
########### bootstrap estimates of bias and standard error of the point estimate, and 
########### 95% confidence interval computed using percentile bootstrap.
# Perform bootstrap
set.seed(1)
# Parameter of interest: Correlation
correlation.fn <- function(x, indices) {
result <- cor(x[indices,1],x[indices,2])
return(result)
}

correlation.boot <- boot(gpaData, correlation.fn, R = 1000)
correlation.boot

# Get a 95% confidence interval for Correlation
boot.ci(correlation.boot, type = "perc")
# Let's verify the percentile bootstrap interval
sort(correlation.boot$t)[c(25, 975)]

########### model diagnostics to verify the model assumptions ######################
lsm.fit <- lm(gpa ~ act, data = gpaData)
lsm.fit
summary(lsm.fit)
confint(lsm.fit)
length((lsm.fit$residuals))
sum(lsm.fit$residuals)/length((lsm.fit$residuals))
qqnorm(lsm.fit$residuals)
qqline(lsm.fit$residuals)
shapiro.test(residuals(lsm.fit))
plot(residuals(lsm.fit),type="l")
abline(h=0)
bptest(lsm.fit)

########### nonparametric bootstrap ######################
fit.fn <- function(data, index) {
result <- coef(lm(gpaData$gpa ~ gpaData$act , data = data, subset = index))
return(result)
}

# Check that it works
n <- nrow(Auto)
fit.fn(gpaData, 1:n)
# Estimates and SEs from LM fit
summary(lm(mpg ~ poly(horsepower, 2), data = Auto))$coef
# Perform bootstrap 
set.seed(1)
reg.boot <- boot(gpaData, fit.fn, R = 1000)
reg.boot
boot.ci(reg.boot, type = "perc")


########### OJ Dataset ######################
storeData <- OJ
str(storeData)
View(OJ)
summary(storeData)
str(storeData)
storeData$Purchase <- ifelse (storeData$Purchase == 'MM',1,0)
storeData$StoreID <- as.factor(storeData$StoreID)
storeData$Purchase <- as.factor(storeData$Purchase)
storeData$STORE <- as.integer(storeData$STORE)

glm.fit1 <- glm(storeData$Purchase ~  storeData$StoreID + storeData$Store7 + storeData$STORE,family=binomial)
summary(glm.fit1)

glm.fit2 <- glm(storeData$Purchase ~  storeData$Store7 + storeData$STORE,family=binomial)
summary(glm.fit2)

glm.fit3 <- glm(storeData$Purchase ~  storeData$StoreID + storeData$STORE,family=binomial)
summary(glm.fit3)

glm.fit4 <- glm(storeData$Purchase ~  storeData$StoreID + storeData$Store7,family=binomial)
summary(glm.fit4)


########### logistic regression ######################
str(storeData)
drops <- c("STORE","Store7")
storeData <- storeData[ , !(names(storeData) %in% drops)]
str(storeData)

logistic.fit1 = glm(storeData$Purchase ~., family = binomial, data = storeData)
summary(logistic.fit1)

logistic.fit2 = glm(storeData$Purchase ~PriceCH+ PriceMM+ DiscMM+ LoyalCH + PctDiscMM, family = binomial, data = storeData)
summary(logistic.fit2)

anova(logistic.fit1,logistic.fit2,test='Chisq')

keeps <- c("Purchase","PriceCH","PriceMM", "DiscMM", "LoyalCH", "PctDiscMM")
storeData <- storeData[keeps]


# Test Error Rate  
lr.prob <- predict(logistic.fit2, storeData, type = "response")
lr.pred <- ifelse(lr.prob >= 0.5, "1", "0")
# Confusion Matrix, Sensitiviti, specificity, misclassification rate
confusionMatrix(as.factor(lr.pred),storeData$Purchase)
# ROC Curve
roc.lr <- roc(storeData$Purchase, lr.prob, levels = c("0", "1"))
roc.lr
plot(roc.lr, legacy.axes = T)
str(storeData)
logistic.fit2 = glm(Purchase ~., family = binomial, data = storeData)

cv.err <- cv.glm(storeData, logistic.fit2, ,K=10)
cv.err$delta[1]


########### LDA ######################
lda.fit = lda(Purchase ~ ., data = storeData) 
lda.fit
# Test Error Rate  
lr.pred = predict(lda.fit, storeData) 
# Confusion Matrix, Sensitiviti, specificity, misclassification rate
confusionMatrix(as.factor(lr.pred$class),storeData$Purchase)
# ROC Curve
roc.lr <- roc(storeData$Purchase, lr.pred$posterior[,"0"], levels = c("1", "0"))
roc.lr

plot(roc.lr, legacy.axes = T)
str(storeData)

cv.lda <-
function (data, model=Purchase~., yname="Purchase", K=10, seed=123) {
n <- nrow(data)
set.seed(seed)
datay=data[,yname] #response variable
library(MASS)
#partition the data into K subsets
f <- ceiling(n/K)
s <- sample(rep(1:K, f), n)  
#generate indices 1:10 and sample n of them  
# K fold cross-validated error

CV=NULL

for (i in 1:K) { #i=1
test.index <- seq_len(n)[(s == i)] #test data
train.index <- seq_len(n)[(s != i)] #training data

#model with training data
lda.fit=lda(model, data=data[train.index,])
#observed test set y
lda.y <- data[test.index, yname]
#predicted test set y
lda.predy=predict(lda.fit, data[test.index,])$class

#observed - predicted on test data
error= mean(lda.y!=lda.predy)
#error rates 
CV=c(CV,error)
}
#Output
list(call = model, K = K, 
lda_error_rate = mean(CV), seed = seed)  
}
er_lda=cv.lda(data=storeData,model=Purchase~., yname="Purchase", K=10, seed=123)
er_lda$lda_error_rate



########### QDA ######################
qda.fit = qda(Purchase ~ ., data = storeData) 
qda.fit
# Test Error Rate  
lr.pred = predict(qda.fit, storeData) 
# Confusion Matrix, Sensitiviti, specificity, misclassification rate
confusionMatrix(as.factor(lr.pred$class),storeData$Purchase)
# ROC Curve
roc.lr <- roc(storeData$Purchase, lr.pred$posterior[,"0"], levels = c("1", "0"))
roc.lr

plot(roc.lr, legacy.axes = T)
str(storeData)

cv.qda <-
function (data, model=Purchase~., yname="Purchase", K=10, seed=123) {
n <- nrow(data)
set.seed(seed)
datay=data[,yname] #response variable
library(MASS)
#partition the data into K subsets
f <- ceiling(n/K)
s <- sample(rep(1:K, f), n)  
#generate indices 1:10 and sample n of them  
# K fold cross-validated error

CV=NULL

for (i in 1:K) { #i=1
test.index <- seq_len(n)[(s == i)] #test data
train.index <- seq_len(n)[(s != i)] #training data

#model with training data
qda.fit=qda(model, data=data[train.index,])
#observed test set y
qda.y <- data[test.index, yname]
#predicted test set y
qda.predy=predict(qda.fit, data[test.index,])$class

#observed - predicted on test data
error= mean(qda.y!=qda.predy)
#error rates 
CV=c(CV,error)
}
#Output
list(call = model, K = K, 
qda_error_rate = mean(CV), seed = seed)  
}

er_qda=cv.qda(data=storeData,model=Purchase~., yname="Purchase", K=10, seed=123)
er_qda$qda_error_rate

########### Comparison Logistic Regression, LDA, QDA ######################

cv.knn<- function (dataY, dataX, kn=1, K=10, seed=123) {
n <- nrow(dataX)
set.seed(seed)
library(class)

f <- ceiling(n/K)
s <- sample(rep(1:K, f), n)  
dataX=scale(dataX)
CV=NULL;PvsO=NULL

for (i in 1:K) { 
test.index <- seq_len(n)[(s == i)] #test data
train.index <- seq_len(n)[(s != i)] #training data

train.X <- dataX[train.index,]
test.X <- dataX[test.index,]
train.y <- dataY[train.index]
test.y <- dataY[test.index]
#predicted test set y
knn.pred=knn(train.X, test.X, train.y, k=kn) 
#observed - predicted on test data 
error= mean(knn.pred!=test.y) 
#error rates 
CV=c(CV,mean(error))
predvsobs=data.frame(knn.pred,test.y)
PvsO=rbind(PvsO,predvsobs)
} 

#Output
list(k = K,
knn_error_rate = mean(CV), confusion=table(PvsO[,1],PvsO[,2]), seed=seed)
}

cv.error=NULL
for (i in 1:100) {
cv.error[i] <- cv.knn(dataY=storeData$Purchase, dataX=storeData[,c(2,3,4,5,6)], kn=i, 
K=10, seed=123)$knn_error_rate

}
kopt=which(cv.error==min(cv.error))
print(kopt)

er_knn=cv.knn(dataY=storeData$Purchase, dataX=storeData[,c(2,3,4,5,6)], kn=kopt, K=10, seed=1)
er_knn
MisclassRate = (er_knn$confusion[2]+er_knn$confusion[3])/(er_knn$confusion[1]+er_knn$confusion[4]+er_knn$confusion[2]+er_knn$confusion[3])
MisclassRate
Sensitivity = (er_knn$confusion[1])/(er_knn$confusion[1]+er_knn$confusion[2])
Sensitivity
specificity = (er_knn$confusion[4])/(er_knn$confusion[3]+er_knn$confusion[4])
specificity

# train k-NN classifier
knn.fit <- knn(train=storeData[,c(2,3,4,5,6)], test=storeData[,c(2,3,4,5,6)],
cl=storeData$Purchase, k=kopt, prob=TRUE, use.all=TRUE)
lr.prob <- attr(knn.fit,"prob")
# ROC Curve
roc.lr <- roc(storeData$Purchase, lr.prob, levels = c("0", "1"))
roc.lr
plot(roc.lr, legacy.axes = T)

############### Auto dataset - Exploratory Analysis ################
str(Auto)
plot(Auto)
str(Auto)
drops <- c("name")
Auto <- Auto[ , !(names(Auto) %in% drops)]
str(Auto)
cor(Auto)
sum(is.na(Auto)) 
Auto <- na.omit(Auto)
str(Auto)
dim(Auto)
View(Auto)
str(Auto)
summary(Auto)
hist(Auto$mpg)
pairs(Auto)

######### multiple linear regression model #####################
# Multiple Linear Regression 
lm.fit1 <- lm(mpg ~., data=Auto)
summary(lm.fit1) # show results
confint(lm.fit1, level=0.95) # CIs for model parameters 
str(Auto)
drops <- c("name")
Auto <- Auto[ , !(names(Auto) %in% drops)]
str(Auto)
lm.fit1 <- lm(mpg ~ ., data=Auto)
summary(lm.fit1)
confint(lm.fit1, level=0.95) # CIs for model parameters 
lm.fit2 <- lm(mpg ~ displacement + weight + year + origin, data=Auto)
summary(lm.fit2)
lm.fit3 <- lm(mpg ~ weight + year + origin, data=Auto)
summary(lm.fit3)
anova(lm.fit2,lm.fit1,test='Chisq')
anova(lm.fit2,lm.fit3,test='Chisq')

######### best-subset selection #####################
str(Auto)
# Total number of predictors in the data
totpred <- ncol(Auto) - 1


hist(Auto$mpg)
# ---------------------- #
# Best Subset Selection #
# -----------------------# 

fit.full <- regsubsets(mpg ~ ., Auto, nvmax = totpred)

par(mfrow = c(2, 2))
plot(fit.full, scale = "r2")
plot(fit.full, scale = "adjr2")
plot(fit.full, scale = "Cp")
plot(fit.full, scale = "bic")
fit.summary <- summary(fit.full)
# Gives the best model for each size
fit.summary

names(fit.summary)
fit.summary$rsq
fit.summary$bic
# Plot model fit measures for best model of each size against size

par(mfrow = c(2, 2))

# RSS
plot(fit.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
indexrss= which.min(fit.summary$rss)
points(indexrss, fit.summary$rss[indexrss], col = "red", cex = 2, pch = 20)

# Adjusted R^2
plot(fit.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
indexadjr2 = which.max(fit.summary$adjr2)
points(indexadjr2, fit.summary$adjr2[indexadjr2], col = "red", cex = 2, pch = 20)

# CP
plot(fit.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
indexcp= which.min(fit.summary$cp)
points(indexcp, fit.summary$cp[indexcp], col = "red", cex = 2, pch = 20)

# BIC
plot(fit.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
indexbic= which.min(fit.summary$bic)
points(indexbic, fit.summary$bic[indexbic], col = "red", cex = 2, pch = 20)
# ?plot.regsubsets


######### forward stepwise selection #####################
# ---------------------- #
#    Forward Selection   #
# -----------------------# 

fit.fwd <- regsubsets(mpg ~ ., Auto, method = "forward")

par(mfrow = c(2, 2))
plot(fit.fwd, scale = "r2")
plot(fit.fwd, scale = "adjr2")
plot(fit.fwd, scale = "Cp")
plot(fit.fwd, scale = "bic")
fit.summary <- summary(fit.fwd)
# Gives the best model for each size
fit.summary

names(fit.summary)
fit.summary$rsq
fit.summary$bic
# Plot model fit measures for best model of each size against size

par(mfrow = c(2, 2))

# RSS
plot(fit.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
indexrss= which.min(fit.summary$rss)
points(indexrss, fit.summary$rss[indexrss], col = "red", cex = 2, pch = 20)

# Adjusted R^2
plot(fit.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
indexadjr2 = which.max(fit.summary$adjr2)
points(indexadjr2, fit.summary$adjr2[indexadjr2], col = "red", cex = 2, pch = 20)

# CP
plot(fit.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
indexcp= which.min(fit.summary$cp)
points(indexcp, fit.summary$cp[indexcp], col = "red", cex = 2, pch = 20)

# BIC
plot(fit.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
indexbic= which.min(fit.summary$bic)
points(indexbic, fit.summary$bic[indexbic], col = "red", cex = 2, pch = 20)
# ?plot.regsubsets


######### backward stepwise selection #####################
# ---------------------- #
#    Backward Selection   #
# -----------------------# 

fit.bwd <- regsubsets(mpg ~ ., Auto, method = "backward")

par(mfrow = c(2, 2))
plot(fit.bwd, scale = "r2")
plot(fit.bwd, scale = "adjr2")
plot(fit.bwd, scale = "Cp")
plot(fit.bwd, scale = "bic")
fit.summary <- summary(fit.bwd)
# Gives the best model for each size
fit.summary

names(fit.summary)
fit.summary$rsq
fit.summary$bic
# Plot model fit measures for best model of each size against size

par(mfrow = c(2, 2))

# RSS
plot(fit.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
indexrss= which.min(fit.summary$rss)
points(indexrss, fit.summary$rss[indexrss], col = "red", cex = 2, pch = 20)

# Adjusted R^2
plot(fit.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
indexadjr2 = which.max(fit.summary$adjr2)
points(indexadjr2, fit.summary$adjr2[indexadjr2], col = "red", cex = 2, pch = 20)

# CP
plot(fit.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
indexcp= which.min(fit.summary$cp)
points(indexcp, fit.summary$cp[indexcp], col = "red", cex = 2, pch = 20)

# BIC
plot(fit.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
indexbic= which.min(fit.summary$bic)
points(indexbic, fit.summary$bic[indexbic], col = "red", cex = 2, pch = 20)
# ?plot.regsubsets

######### Comparison - 
######### 1. linear regression model using the least squares method.
######### 2. best-subset selection to find the best model.
######### 3. forward stepwise selection.
######### 4. backward stepwise selection.

fitlog <- lm(log(mpg)~cylinders+displacement+horsepower+weight+year+origin,  data=Auto)
summary(fitlog)             
par(mfrow = c(1, 1))

library(lmtest)
bptest(fitlog)
sum(fitlog$residuals)
qqnorm(fitlog$residuals)
