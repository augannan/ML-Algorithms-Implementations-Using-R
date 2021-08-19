##################### LDA-QDA ########################
#Install if the package doesn't exist 
if (!require(ISLR)) install.packages('ISLR')
library(ISLR) 
if (!require(MASS)) install.packages('MASS')
library(MASS)

# Training Data and Test Data
testDataFronEachGroup = 5;
group1 = admission[admission$Group == 1, ]
group2 = admission[admission$Group == 2, ]
group3 = admission[admission$Group == 3, ]
trainingDatagroup1 = group1[1:(nrow(group1)-5),]
trainingDatagroup2 = group2[1:(nrow(group2)-5),]
trainingDatagroup3 = group3[1:(nrow(group3)-5),]
testDatagroup1 = group1[(nrow(group1)-testDataFronEachGroup+1):nrow(group1),]
testDatagroup2 = group2[(nrow(group2)-testDataFronEachGroup+1):nrow(group2),]
testDatagroup3 = group3[(nrow(group3)-testDataFronEachGroup+1):nrow(group3),]
trainingData = rbind(trainingDatagroup1, trainingDatagroup2, trainingDatagroup3)
testData = rbind(testDatagroup1, testDatagroup2, testDatagroup3)

############ LDA ############
lda.fit = lda(Group~GPA+GMAT, data=trainingData)
lda.fit
plot(lda.fit)
lda.pred = predict(lda.fit, trainingData)
class(lda.pred)
table(lda.pred$class,trainingData$Group)
source("decisionplot.R")
plot(trainingData$GPA,trainingData$GMAT)
decisionplot(lda.fit, trainingData, class = "Group", main = "LDA")
legend("topleft", pch=c(3,2,4), col=c("green", "red","blue"), c("Group 1", "Group 2", "Group 3"))
1-mean(lda.pred$class == trainingData$Group)
lda.pred = predict(lda.fit, testData)
class(lda.pred)
table(lda.pred$class,testData$Group)
source("decisionplot.R")
plot(testData$GPA,testData$GMAT)
decisionplot(lda.fit, testData, class = "Group", main = "LDA")
legend("topleft", pch=c(3,2,4), col=c("green", "red","blue"), c("Group 1", "Group 2", "Group 3"))
1-mean(lda.pred$class == testData$Group)

##################
trainingData<- trainingData[1:3]
testData<- testData[1:3]
## Loading and taking a glance at the dataset
admission.predictors <- trainingData[1:2]
admission.decision <- trainingData[,3]
our.lda.fit <- s6340.lda(as.factor(admission.decision), admission.predictors)
########### Equation for decision boundary ###############
our.lda.fit$disc
library(MASS)
lda.model <- lda(Group ~ ., data = trainingData)
lda.model
our.lda.fit$disc
lda.pred <- predict(our.lda.fit, trainingData)


############# QDA ####################
qda.fit = qda(Group~GPA+GMAT, data=trainingData)
qda.fit
qda.pred = predict(qda.fit, trainingData)
class(qda.pred)
table(qda.pred$class,trainingData$Group)
source("decisionplot.R")
plot(trainingData$GPA,trainingData$GMAT)
decisionplot(qda.fit, trainingData, class = "Group", main = "qda")
legend("topleft", pch=c(3,2,4), col=c("green", "red","blue"), c("Group 1", "Group 2", "Group 3"))
1-mean(qda.pred$class == trainingData$Group)
qda.pred = predict(qda.fit, testData)
class(qda.pred)
table(qda.pred$class,testData$Group)
source("decisionplot.R")
plot(testData$GPA,testData$GMAT)
decisionplot(qda.fit, testData, class = "Group", main = "qda")
legend("topleft", pch=c(3,2,4), col=c("green", "red","blue"), c("Group 1", "Group 2", "Group 3"))
1-mean(qda.pred$class == testData$Group)


our.qda.fit <- s6340.qda(as.factor(admission.decision), admission.predictors)
our.qda.fit
########### Equation for decision boundary ###############
our.qda.fit$quad.coef.matrix #Matrix for quadratic terms
our.qda.fit$linear.coef.vector #vector for linear terms
our.qda.fit$cutoff #cutoff value
