# Clear console
cat("\014")
dev.off()

# Remove all variables
rm(list=ls(all=TRUE)) 

# Set working directory
setwd('FolderPath')
# Load Data
admission = read.csv("admission.csv")

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
source("decisionplot.R")
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

## Loading and taking a glance at the dataset
admission.training.predictors <- trainingData[1:2]
admission.training.decision <- trainingData[,3]
admission.training <- trainingData[1:3]
admission.test.predictors <- testData[1:2]
admission.test.decision <- testData[,3]
admission.test <- testData[1:3]

admission.training[["Group"]] = factor(admission.training[["Group"]])
admission.test[["Group"]] = factor(admission.test[["Group"]])
admission.training[2] = lapply(admission.training[2]/250, as.numeric)
admission.test[2] = lapply(admission.test[2]/250, as.numeric)


########## Plotting Decision Boundary #########
plot(admission.training[,1:2], col = admission.training[,3])
knn.model <- knn3(Group ~ ., data=admission.training, k = 10)
decisionplot(knn.model, admission.training, class = "Group")

############### CREATE RESPONSE AND PREDICTOR OBJECTS #################
# cbind Takes a sequence of vector, matrix or data-frame arguments and combine by columns or rows, respectively.
trainingData.x=admission.training[1:2]
trainingData.y=admission.training[,3]
testData.x=admission.test[1:2]
testData.y=admission.test[,3]

############### INITIALIZE K AND TRAINING AND TESTING ERROR RATE #################
k=c(seq(1,length(trainingData.y)))
n=length(k)
# Training error rate
trainingData.error.rate = numeric(length = n)
# Testing error rate
testData.error.rate = numeric(length = n)
# names: Functions to get or set the names of an object.
names(trainingData.error.rate) = names(testData.error.rate) = k

############### APPLY KNN METHOD #################
# knn(train, test, cl, k = 1, l = 0, prob = FALSE, use.all = TRUE)
# -- Arguments --
# train: matrix or data frame of training set cases.
# test:matrix or data frame of test set cases. A vector will be interpreted as a row vector for a single case.
# cl: factor of true classifications of training set
# k: number of neighbours considered.
# l: minimum vote for definite decision, otherwise doubt. (More precisely, less than k-l dissenting votes are allowed, even if k is increased by ties.)
# prob: If this is true, the proportion of the votes for the winning class are returned as attribute prob.
# use.all: controls handling of ties. If true, all distances equal to the kth largest are included. If false, a random selection of distances equal to the kth is chosen to use exactly k neighbours.
for (i in seq(along = k)) {
# Set the seed of R's random number generator, which is useful for creating simulations or random objects that can be reproduced.
set.seed(1)
# k-nearest neighbour classification for training set.
mod.trainingData = knn3(Group ~ ., data=admission.training, k = k[i])
# Training error rate
train_predict <- predict(mod.trainingData, newdata = trainingData.x)
trainingData.error.rate[i] = 1 - sum(train_predict == trainingData.y)/length(trainingData.y)
set.seed(1)
# k-nearest neighbour classification for test set.
test_predict <- predict(mod.trainingData, newdata = testData.x)
# Test error rate
testData.error.rate[i] = 1 - sum(test_predict == testData.y)/length(testData.y)
}
# Create objects with error rate
result = data.frame(k, trainingData.error.rate, testData.error.rate)

# Plot training and test error rates with respect to k
plot(k, trainingData.error.rate, xlab = "Number of nearest neighbors", ylab = "Error rate", 
main="Plot of training and test error rates against k", 
type = "b", ylim = range(c(trainingData.error.rate, testData.error.rate)), col = "black", pch = 20)
lines(k, testData.error.rate, type="b", col="red", pch = 1)
legend(40,0.05, lty = 1, col = c("black", "red"), pch=c(20,1),legend = c("Training Error Rate", "Test Error Rate"),
cex=1)

# Get optimal value of k. We used the least flexible model (i.e the highest k)
opt=result[testData.error.rate == min(result$testData.error.rate), ]


####### Decision boundary for optimal k with training data #######

# Set up the grid
n.grid = 50
x1.grid = seq(min(trainingData.x[, 1]), max(trainingData.x[, 1]), length.out = n.grid)
x2.grid = seq(min(trainingData.x[, 2]), max(trainingData.x[, 2]), length.out = n.grid)
grid = expand.grid(x1.grid, x2.grid)

# Apply knn for optimal k on grid
k.opt = max(opt$k)
k.opt


########## Plotting Decision Boundary #########
plot(admission.training[,1:2], col = admission.training[,3])
knn.model <- knn3(Group ~ ., data=admission.training, k = k.opt)
decisionplot(knn.model, admission.training, class = "Group")
legend("topleft", pch=c(3,2,4), col=c("green", "red","blue"), c("Group 1", "Group 2", "Group 3"))

plot(admission.test[,1:2], col = admission.test[,3])
knn.model <- knn3(Group ~ ., data=admission.test, k = k.opt)
decisionplot(knn.model, admission.test, class = "Group")
legend("topleft", pch=c(3,2,4), col=c("green", "red","blue"), c("Group 1", "Group 2", "Group 3"))

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

knn_fit <- train(Group ~., data = admission.training, method = "knn",
trControl=trctrl,
preProcess = c("center", "scale"),
tuneLength = 10)

knn_fit
test_pred <- predict(knn_fit, newdata = admission.test)
test_pred
confusionMatrix(test_pred, admission.test$Group )
train_pred <- predict(knn_fit, newdata = admission.training)
train_pred
confusionMatrix(train_pred, admission.training$Group )
