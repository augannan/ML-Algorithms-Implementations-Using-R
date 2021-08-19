############### Preparing the Data #####################
str(Caravan)
ntestSampless = 1000
trainIndex = (ntestSampless+1):nrow(Caravan)
testIndex = 1:ntestSampless

standardized.X <- scale(Caravan[,-length(Caravan)], center=TRUE, scale=TRUE)

train.X <- standardized.X[trainIndex, ]
test.X=standardized.X[testIndex,]
train.Y <- ifelse(Caravan$Purchase[trainIndex] == "Yes", 1, -1)
test.Y <- ifelse(Caravan$Purchase[testIndex] == "Yes", 1, -1)
train <- cbind(train.X,train.Y)
test <- cbind(test.X,test.Y)
colnames(train)[colnames(train)=="train.Y"] <- "Purchase"
colnames(test)[colnames(test)=="test.Y"] <- "Purchase"
test <- as.data.frame(test)
train <- as.data.frame(train)

train$Purchase <- as.factor(train$Purchase)
test$Purchase <- as.factor(test$Purchase)

str(train)
str(test)

head(train, 5)[, c(1:3, (length(Caravan)-3):length(Caravan))]
tail(test, 5)[, c(1:3, (length(Caravan)-3):length(Caravan))]

############### Support vector classifier to the training data with cost 
############### parameter chosen optimally using
############### 10-fold cross-validation. #####################

set.seed(1)
# Linear Kernel
tune.out.linear = tune(svm, Purchase~ ., data = train, kernel = "linear", ranges = list(cost = c(0.00001, 0.0001, 0.001, 0.01, 0.1, 0.5, 1, 1.5, 2, 5)))
tune.out.linear
data.frame(cost = tune.out.linear$performances$cost, CV.Error.Rate = tune.out.linear$performances$error, 
misclass = round(tune.out.linear$performances$error * nrow(train)))
# Get results for the best model
bestmod.linear = tune.out.linear$best.model
summary(bestmod.linear)
## Performance of support vector classifier on test data
# Predict the test responses using the best model
ypred.linear.train = predict(bestmod.linear, train)
table(predict = ypred.linear.train, truth = train$Purchase)

ypred.linear.test = predict(bestmod.linear, test)
table(predict = ypred.linear.test, truth = test$Purchase)


# Polynomial Kernel
set.seed(1)
tune.out.poly = tune(svm, Purchase ~ ., data = train, kernel = "polynomial", degree = 2, ranges = list(cost = c(0.00001, 0.0001, 0.001, 0.01, 0.1, 0.5, 1, 1.5, 2, 5)))
summary(tune.out.poly)
tune.out.poly
data.frame(cost = tune.out.poly$performances$cost, CV.Error.Rate = tune.out.poly$performances$error,
misclass = round(tune.out.poly$performances$error * nrow(train)))
# Get results for the best model
bestmod.poly = tune.out.poly$best.model
summary(bestmod.poly)
## Performance of support vector classifier on test data
# Predict the test responses using the best model
ypred.poly.train = predict(bestmod.poly, train)
table(predict = ypred.poly.train, truth = train$Purchase)
ypred.poly.test = predict(bestmod.poly, test)
table(predict = ypred.poly.test, truth = test$Purchase)


#Radial Kernel
set.seed(1)
tune.out.radial = tune(svm, Purchase ~ ., data = train, kernel = "radial", 
ranges = list(cost = c(0.00001, 0.0001, 0.001, 0.01, 0.1, 0.5, 1, 1.5, 2, 5), gamma = c(0.01, 0.1, 1, 5)))
tune.out.radial
summary(tune.out.radial)
data.frame(cost = tune.out.radial$performances$cost, gamma= tune.out.radial$performances$gamma, CV.Error.Rate =
tune.out.radial$performances$error, misclass = round(tune.out.radial$performances$error * nrow(train)))
# Get results for the best model
bestmod.radial = tune.out.radial$best.model
summary(bestmod.radial)
## Performance of support vector classifier on test data
# Predict the test responses using the best model
ypred.radial.train = predict(bestmod.radial, train)
table(predict = ypred.radial.train, truth = train$Purchase)
ypred.radial.test = predict(bestmod.radial, test)
table(predict = ypred.radial.test, truth = test$Purchase)
##########################################
