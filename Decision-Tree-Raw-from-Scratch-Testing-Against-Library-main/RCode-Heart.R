# Clear console
cat("\014")
dev.off()

# Remove all variables
rm(list=ls(all=TRUE)) 

# Set working directory
setwd('C:\\Users\\Md Arafat H Khan\\Dropbox\\Academics - University of Texas at Dallas\\STAT 6340.18s Statistical and Machine Learning - Spring 2018\\Mini Projects\\Project 06')
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

####################################
Heart <- read.csv('heart.csv')
str(Heart)
Heart <- Heart[,c("Ca","Thal","AHD")]
str(Heart)
Heart <- na.omit(Heart)
str(Heart)
Heart$Ca <- as.factor(Heart$Ca)
str(Heart)
summary(Heart)

tree.fit <- tree(AHD~., data= Heart)
tree.fit
summary(tree.fit)
plot(tree.fit)
text(tree.fit, pretty = 0, cex = 0.7)

Region = Heart$AHD
n =length(Region)
Y = sum(Region == "Yes")/length(Region)
N = sum(Region == "No")/length(Region)
D1 = -2*(Y*n * log(Y) + N * n * log(N))
Result = c(n,D1,N,Y)
print(Result)

Region = Heart[Heart$Thal =="normal",]$AHD
n =length(Region)
Y = sum(Region == "Yes")/length(Region)
N = sum(Region == "No")/length(Region)
D2 = -2*(Y*n * log(Y) + N * n * log(N))
Result = c(n,D2,N,Y)
print(Result)


Region = Heart[Heart$Thal =="fixed",]$AHD
n =length(Region)
Y = sum(Region == "Yes")/length(Region)
N = sum(Region == "No")/length(Region)
D3 = -2*(Y*n * log(Y) + N * n * log(N))
Result = c(n,D3,N,Y)
print(Result)

Region = Heart[Heart$Thal =="reversable",]$AHD
n =length(Region)
Y = sum(Region == "Yes")/length(Region)
N = sum(Region == "No")/length(Region)
D4 = -2*(Y*n * log(Y) + N * n * log(N))
Result = c(n,D4,N,Y)
print(Result)

Region = Heart[Heart$Ca =="0",]$AHD
n =length(Region)
Y = sum(Region == "Yes")/length(Region)
N = sum(Region == "No")/length(Region)
D5 = -2*(Y*n * log(Y) + N * n * log(N))
Result = c(n,D5,N,Y)
print(Result)

Region = Heart[Heart$Ca =="1",]$AHD
n =length(Region)
Y = sum(Region == "Yes")/length(Region)
N = sum(Region == "No")/length(Region)
D6 = -2*(Y*n * log(Y) + N * n * log(N))
Result = c(n,D6,N,Y)
print(Result)

Region = Heart[Heart$Ca =="2",]$AHD
n =length(Region)
Y = sum(Region == "Yes")/length(Region)
N = sum(Region == "No")/length(Region)
D7 = -2*(Y*n * log(Y) + N * n * log(N))
Result = c(n,D7,N,Y)
print(Result)

Region = Heart[Heart$Ca =="3",]$AHD
n =length(Region)
Y = sum(Region == "Yes")/length(Region)
N = sum(Region == "No")/length(Region)
D8 = -2*(Y*n * log(Y) + N * n * log(N))
Result = c(n,D8,N,Y)
print(Result)


D2+D3+D4
D5+D6+D7+D8

Region = Heart[Heart$Thal =="reversable" | Heart$Thal =="fixed" ,]$AHD
n =length(Region)
Y = sum(Region == "Yes")/length(Region)
N = sum(Region == "No")/length(Region)
D = -2*(Y*n * log(Y) + N * n * log(N))
Result = c(n,D,N,Y)
print(Result)


##############################

Region = Heart[Heart$Ca =="0" & Heart$Thal =="normal",]$AHD
n =length(Region)
Y = sum(Region == "Yes")/length(Region)
N = sum(Region == "No")/length(Region)
D5 = -2*(Y*n * log(Y) + N * n * log(N))
Result = c(n,D5,N,Y)
print(Result)

Region = Heart[Heart$Ca =="1" & Heart$Thal =="normal",]$AHD
n =length(Region)
Y = sum(Region == "Yes")/length(Region)
N = sum(Region == "No")/length(Region)
D6 = -2*(Y*n * log(Y) + N * n * log(N))
Result = c(n,D6,N,Y)
print(Result)

Region = Heart[Heart$Ca =="2" & Heart$Thal =="normal",]$AHD
n =length(Region)
Y = sum(Region == "Yes")/length(Region)
N = sum(Region == "No")/length(Region)
D7 = -2*(Y*n * log(Y) + N * n * log(N))
Result = c(n,D7,N,Y)
print(Result)

Region = Heart[Heart$Ca =="3" & Heart$Thal =="normal",]$AHD
n =length(Region)
Y = sum(Region == "Yes")/length(Region)
N = sum(Region == "No")/length(Region)
D8 = -2*(Y*n * log(Y) + N * n * log(N))
Result = c(n,D8,N,Y)
print(Result)

D5
D6+D7+D8

Region = Heart[Heart$Ca !="0" & Heart$Thal =="normal",]$AHD
n =length(Region)
Y = sum(Region == "Yes")/length(Region)
N = sum(Region == "No")/length(Region)
D8 = -2*(Y*n * log(Y) + N * n * log(N))
Result = c(n,D8,N,Y)
print(Result)



##############################

Region = Heart[Heart$Ca =="0" & Heart$Thal !="normal",]$AHD
n =length(Region)
Y = sum(Region == "Yes")/length(Region)
N = sum(Region == "No")/length(Region)
D5 = -2*(Y*n * log(Y) + N * n * log(N))
Result = c(n,D5,N,Y)
print(Result)

Region = Heart[Heart$Ca =="1" & Heart$Thal !="normal",]$AHD
n =length(Region)
Y = sum(Region == "Yes")/length(Region)
N = sum(Region == "No")/length(Region)
D6 = -2*(Y*n * log(Y) + N * n * log(N))
Result = c(n,D6,N,Y)
print(Result)

Region = Heart[Heart$Ca =="2" & Heart$Thal !="normal",]$AHD
n =length(Region)
Y = sum(Region == "Yes")/length(Region)
N = sum(Region == "No")/length(Region)
D7 = -2*(Y*n * log(Y) + N * n * log(N))
Result = c(n,D7,N,Y)
print(Result)

Region = Heart[Heart$Ca =="3" & Heart$Thal !="normal",]$AHD
n =length(Region)
Y = sum(Region == "Yes")/length(Region)
N = sum(Region == "No")/length(Region)
D8 = -2*(Y*n * log(Y) + N * n * log(N))
Result = c(n,D8,N,Y)
print(Result)

D5
D6+D7+D8

Region = Heart[Heart$Ca !="0" & Heart$Thal !="normal",]$AHD
n =length(Region)
Y = sum(Region == "Yes")/length(Region)
N = sum(Region == "No")/length(Region)
D8 = -2*(Y*n * log(Y) + N * n * log(N))
Result = c(n,D8,N,Y)
print(Result)

###################################

Region = Heart[Heart$Ca =="1" & Heart$Thal !="normal",]$AHD
n =length(Region)
Y = sum(Region == "Yes")/length(Region)
N = sum(Region == "No")/length(Region)
D8 = -2*(Y*n * log(Y) + N * n * log(N))
Result = c(n,D8,N,Y)
print(Result)

Region = Heart[Heart$Ca =="2" & Heart$Thal !="normal",]$AHD
n =length(Region)
Y = sum(Region == "Yes")/length(Region)
N = sum(Region == "No")/length(Region)
D8 = -2*(Y*n * log(Y) + N * n * log(N))
Result = c(n,D8,N,Y)
print(Result)

Region = Heart[Heart$Ca =="3" & Heart$Thal !="normal",]$AHD
n =length(Region)
Y = sum(Region == "Yes")/length(Region)
N = sum(Region == "No")/length(Region)
D8 = -2*(Y*n * log(Y) + N * n * log(N))
Result = c(n,D8,N,Y)
print(Result)



Region = Heart[Heart$Ca !="0" & Heart$Ca !="2" & Heart$Thal !="normal",]$AHD
n =length(Region)
Y = sum(Region == "Yes")/length(Region)
N = sum(Region == "No")/length(Region)
D8 = -2*(Y*n * log(Y) + N * n * log(N))
Result = c(n,D8,N,Y)
print(Result)





Region = Heart[Heart$Ca =="2" & Heart$Thal !="normal",]$AHD
n =length(Region)
Y = sum(Region == "Yes")/length(Region)
N = sum(Region == "No")/length(Region)
D8 = -2*(Y*n * log(Y) + N * n * log(N))
Result = c(n,D8,N,Y)
print(Result)
