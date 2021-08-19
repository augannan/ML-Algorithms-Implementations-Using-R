# Clear console
cat("\014")
dev.off()

# Remove all variables
rm(list=ls(all=TRUE)) 

# Set working directory
setwd('<FolderPath>')
# Load Data

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
if (!require(glmnet)) install.packages('glmnet')
library(glmnet)
if (!require(tree)) install.packages('tree')
library(tree)
if (!require(randomForest)) install.packages('randomForest')
library(randomForest)
if (!require(gbm)) install.packages('gbm')
library(gbm)

####################################
str(Caravan)
standardized.X <- scale(Caravan[,-86], center=TRUE, scale=TRUE)
train.X <- standardized.X[1001:5822, ]
test.X=standardized.X[1:1000,]
train.Y <- Caravan$Purchase[1001:5822]
test.Y <- Caravan$Purchase[1:1000]
train <- cbind(train.X,train.Y)
test <- cbind(test.X,test.Y)
colnames(train)[colnames(train)=="train.Y"] <- "Purchase"
colnames(test)[colnames(test)=="test.Y"] <- "Purchase"

test <- as.data.frame(test)
train <- as.data.frame(train)

train$Purchase <- as.factor(train$Purchase)
test$Purchase <- as.factor(test$Purchase)
tree.fit<- tree(Purchase ~ ., train)
tree.fit
summary(tree.fit)
plot(tree.fit)
tree.fit
summary(tree.fit)
plot(tree.fit)
text(tree.fit, pretty = 0, cex = 0.7)

par(xpd = NA)
plot(NA, NA, type = "n", xlim = range(train$PPERSAUT), ylim = range(train$PBRAND), xlab = "PPERSAUT", ylab = "PBRAND")
lines(x = c(0.866083, 0.866083), y = c(-1, 3.3), lwd = 1)
text(x = 0, y = 1, labels = c("R1"), col = "red")
lines(x = c(0.866083, 1.7), y = c(0.35773, 0.35773), lwd = 1)
text(x = 1.3, y = -0.5, labels = c("R2"), col = "red")
text(x = 1.3, y = 2, labels = c("R3"), col = "red")

par(xpd = NA)
plot(NA, NA, type = "n", xlim = c(0.35773, max(train$PBRAND)), ylim = range(train$MOPLLAAG), xlab = "PBRAND", ylab = "MOPLLAAG")
lines(x = c(0.35773, max(train$PBRAND)), c(-0.0315404, -0.0315404), lwd = 1)
text(x = 1.8, y = -1, labels = c("R4"), col = "red")
text(x = 1.8, y = 1, labels = c("R5"), col = "red")

tree.pred <- predict(tree.fit, as.data.frame(test.X), type = "class")
table(tree.pred, test.Y)




cv.caravan <- cv.tree(tree.fit, FUN = prune.misclass)
cv.caravan

cv.caravan$size[which.min(cv.caravan$dev)]

prune.caravan <- prune.misclass(tree.fit, best = 4)
plot(prune.caravan)
prune.caravan
summary(prune.caravan)
plot(prune.caravan)
text(prune.caravan, pretty = 0, cex = 0.7)

tree.pred <- predict(prune.caravan, as.data.frame(test.X), type = "class")
table(tree.pred, test.Y)

######################

bag.caravan <- randomForest(Purchase ~ ., data = train, mtry = 85, ntree = 1000, importance = TRUE)
bag.caravan
yhat.bag <- predict(bag.caravan, newdata = test.X)
yhat.bag <- predict(bag.caravan, newdata = test.X,type = "class")
table(yhat.bag, test.Y)
importance(bag.caravan)
varImpPlot(bag.caravan)

####################
str(Caravan)

bag.caravan <- randomForest(Purchase ~ ., data = train, mtry = 9, ntree = 1000, importance = TRUE)
bag.caravan
yhat.bag <- predict(bag.caravan, newdata = test.X)
yhat.bag <- predict(bag.caravan, newdata = test.X,type = "class")
table(yhat.bag, test.Y)
importance(bag.caravan)
varImpPlot(bag.caravan)

###########################
train$Purchase <- ifelse(Caravan$Purchase[1001:5822] == "Yes", 1, 0)
boost.caravan <- gbm(Purchase ~ ., train, distribution = "bernoulli", n.trees = 1000, interaction.depth = 1, shrinkage = 0.01, verbose = F)
boost.caravan
summary(boost.caravan)
yhat.boost <- predict(boost.caravan, newdata = as.data.frame(test.X), type="response",n.trees=1000)
z.hat <- ifelse(yhat.boost >=0.5, 1, 0) 
table(z.hat,test$Purchase)
varImpPlot(boost.caravan)

##########################

cv.knn<- function (dataY, dataX, kn=1, K=10, seed=70030) {
n <- nrow(dataX)
set.seed(seed)
library(class)

f <- ceiling(n/K)
s <- sample(rep(1:K, f), n)  

CV=NULL;PvsO=NULL

for (i in 1:K) { 
test.index <- seq_len(n)[(s == i)] #test data
train.index <- seq_len(n)[(s != i)] #training data

training.X <- dataX[train.index,]
testing.X <- dataX[test.index,]
training.y <- dataY[train.index]
testing.y <- dataY[test.index]
#predicted test set y
knn.pred=knn(training.X, testing.X, training.y, k=kn) 
#observed - predicted on test data 
error= mean(knn.pred!=testing.y) 
#error rates 
CV=c(CV,mean(error))
predvsobs=data.frame(knn.pred,testing.y)
PvsO=rbind(PvsO,predvsobs)
} 

list(k = K,
knn_error_rate = mean(CV), confusion=table(PvsO[,1],PvsO[,2]), seed=seed)
}

cv.error=NULL
for (i in 1:100) {
cv.error[i] <- cv.knn(dataY=test.Y, dataX=as.matrix(test.X,drop=FALSE), kn=i,K=10, seed=123)$knn_error_rate 
} 
X <- standardized.X
Y <- Caravan[,86]
knn.fit = knn(train.X, test.X, train.Y, k =10, prob = T) 
knn.prob = attr(knn.fit, "prob") 
knn.prob = ifelse(knn.fit == "1", knn.prob, 1 - knn.prob) 
table(knn.fit, test.Y)
k.opt=which(cv.error==min(cv.error))
k.opt
min(cv.error)
####################
grid <- 10^seq(10,-2,length=100)
y <- train.Y
x <- model.matrix(Purchase ~.,train)[,-1]
ridge.mod <- glmnet(x, y, family = "binomial", alpha = 0, lambda = grid,thresh=1e-12)
plot(ridge.mod, xvar = "lambda")
xnew <- model.matrix(Purchase~., test)[,-1]
ynew <- test.Y
set.seed(1)
cv.ridge <- cv.glmnet(xnew, as.factor(ynew), alpha = 0, family="binomial")
cv.ridge
cv.ridge$lambda.min
ridge.modeltrain <- glmnet(x, y, alpha = 0, family = "binomial",lambda = cv.ridge$lambda.min)
x.test <- model.matrix(Purchase ~., test)[,-1]
probabilities <- predict(ridge.modeltrain,s=cv.ridge$lambda.min,newx = x.test,type='response')
predicted.classes <- ifelse(probabilities > 0.5, "Yes", "No")
observed.classes <- Caravan$Purchase[1:1000]
mean(predicted.classes == observed.classes)
1-mean(predicted.classes == observed.classes)
table(test.Y,predicted.classes)
