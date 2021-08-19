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

####################################
str(Heart)
Ht = na.omit(Hitters)
# Root node
Ht$Salary = log(Ht$Salary)
tree.fit <- tree(Salary~ Years + Hits, data= Ht)
tree.fit
summary(tree.fit)
plot(tree.fit)
text(tree.fit, pretty = 0, cex = 0.7)

############ Q3(a) #################
sum((Ht$Salary-mean(Ht$Salary))^2)

# Consider Years
s = seq(min(Ht$Years),max(Ht$Years),by = 0.01)
R = 0*s
n = length(s)
for (i in 1:n) {
C1 = Ht[Ht$Years<s[i],]$Salary
C2 = Ht[Ht$Years>=s[i],]$Salary

R[i] = sum((C1 - mean(C1))^2)+sum((C2 - mean(C2))^2)
}
mean(s[R==min(R)])
min(R)
C1 = Ht[Ht$Years<mean(s[R==min(R)]),]$Salary
C2 = Ht[Ht$Years>=mean(s[R==min(R)]),]$Salary
sum((C1 - mean(C1))^2)
sum((C2 - mean(C2))^2)
# Consider Hits
s = seq(min(Ht$Hits),max(Ht$Hits),by = 0.01)
R = 0*s
n = length(s)
for (i in 1:n) {
C1 = Ht[Ht$Hits<s[i],]$Salary
C2 = Ht[Ht$Hits>=s[i],]$Salary
R[i] = sum((C1 - mean(C1))^2)+sum((C2 - mean(C2))^2)
}
mean(s[R==min(R)])
min(R)


# Decision: Years at 4.505 with 115.0585, Hits 117.505 with 160.9715
# 115.0585<160.9715, so we should split it with year at 4.505

# Let us now split the data into two parts
DL = Ht[Ht$Years<4.505,]
DU = Ht[Ht$Years>=4.505,]
nrow(DL)
nrow(DU)
# Now, split the data for year < 4.505

# Consider Years
s = seq(min(DL$Years),max(DL$Years),by = 0.5)
R = 0*s
n = length(s)
for (i in 1:n) {
C1 = DL[DL$Years<s[i],]$Salary
C2 = DL[DL$Years>=s[i],]$Salary
R[i] = sum((C1 - mean(C1))^2)+sum((C2 - mean(C2))^2)
}
mean(s[R==min(R)])
min(R)
C1 = DL[DL$Years<mean(s[R==min(R)]),]$Salary
C2 = DL[DL$Years>=mean(s[R==min(R)]),]$Salary
sum((C1 - mean(C1))^2)
sum((C2 - mean(C2))^2)
length(C1)
length(C2)


# Consider Hits
s = seq(min(DL$Hits),max(DL$Hits),by = 0.01)
R = 0*s
n = length(s)
for (i in 1:n) {
C1 = DL[DL$Hits<s[i],]$Salary
C2 = DL[DL$Hits>=s[i],]$Salary
R[i] = sum((C1 - mean(C1))^2)+sum((C2 - mean(C2))^2)
}
mean(s[R==min(R)])
min(R)
C1 = DL[DL$Hits<mean(s[R==min(R)]),]$Salary
C2 = DL[DL$Hits>=mean(s[R==min(R)]),]$Salary
sum((C1 - mean(C1))^2)
sum((C2 - mean(C2))^2)
length(C1)
length(C2)


# Decision: Years at 3.505 with 33.14307, Hits 15.505 with 33.01459
# We see that 15 Hits is too marginal for splitting while both RSS are almost same ~ 33
# So, we will split with year again at 3.505

# Now, split the data for year > 4.505

# Consider Years
s = seq(min(DU$Years),max(DU$Years),by = 0.01)
R = 0*s
n = length(s)
for (i in 1:n) {
C1 = DU[DU$Years<s[i],]$Salary
C2 = DU[DU$Years>=s[i],]$Salary
R[i] = sum((C1 - mean(C1))^2)+sum((C2 - mean(C2))^2)
}
mean(s[R==min(R)])
min(R)
# Consider Hits
s = seq(min(DU$Hits),max(DU$Hits),by = 0.01)
R = 0*s
n = length(s)
for (i in 1:n) {
C1 = DU[DU$Hits<s[i],]$Salary
C2 = DU[DU$Hits>=s[i],]$Salary
R[i] = sum((C1 - mean(C1))^2)+sum((C2 - mean(C2))^2)
}
mean(s[R==min(R)])
min(R)
C1 = DU[DU$Hits<mean(s[R==min(R)]),]$Salary
C2 = DU[DU$Hits>=mean(s[R==min(R)]),]$Salary
sum((C1 - mean(C1))^2)
sum((C2 - mean(C2))^2)
length(C1)
length(C2)

# Decision: Years at 6.505 with 69.87702, Hits 117.505 with 48.97678
# 48.97678<69.87702, so we should split it with Hits at 117.505

# Now, we have the following regions
nrow(Ht[Ht$Years<3.505,]) +
nrow(Ht[Ht$Years>=3.505 & Ht$Years<4.505,]) + 
nrow(Ht[Ht$Years>=4.505 & Ht$Hits<117.505,]) + 
nrow(Ht[Ht$Years>=4.505 & Ht$Hits>=117.505,])

# Now we have four rectangles
R1 = Ht[Ht$Years<3.505,]
R2 = Ht[Ht$Years>=3.505 & Ht$Years<4.505,]
R3 = Ht[Ht$Years>=4.505 & Ht$Hits<117.505,]
R4 = Ht[Ht$Years>=4.505 & Ht$Hits>=117.505,]

# Let us try to split R1
nrow(R1)

# Consider Years
s = seq(min(R1$Years),max(R1$Years),by = 0.01)
R = 0*s
n = length(s)
for (i in 1:n) {
C1 = R1[R1$Years<s[i],]$Salary
C2 = R1[R1$Years>=s[i],]$Salary
R[i] = sum((C1 - mean(C1))^2)+sum((C2 - mean(C2))^2)
}
mean(s[R==min(R)])
min(R)
C1 = R1[R1$Years<mean(s[R==min(R)]),]$Salary
C2 = R1[R1$Years>=mean(s[R==min(R)]),]$Salary
sum((C1 - mean(C1))^2)
sum((C2 - mean(C2))^2)
length(C1)
length(C2)
# Consider Hits
s = seq(min(R1$Hits),max(R1$Hits),by = 1)
R = 0*s
n = length(s)
for (i in 1:n) {
C1 = R1[R1$Hits<s[i],]$Salary
C2 = R1[R1$Hits>=s[i],]$Salary
R[i] = sum((C1 - mean(C1))^2)+sum((C2 - mean(C2))^2)
}
mean(s[R==min(R)])
min(R)
C1 = R1[R1$Hits<mean(s[R==min(R)]),]$Salary
C2 = R1[R1$Hits>=mean(s[R==min(R)]),]$Salary
sum((C1 - mean(C1))^2)
sum((C2 - mean(C2))^2)
length(C1)
length(C2)
SS = abs(R-19)
mean(s[SS==min(SS)])
C1 = R1[R1$Hits<mean(s[SS==min(SS)]),]$Salary
C2 = R1[R1$Hits>=mean(s[SS==min(SS)]),]$Salary
sum((C1 - mean(C1))^2)
sum((C2 - mean(C2))^2)
length(C1)
length(C2)
# Let us try to split R2
nrow(R2)

# Consider Years
s = seq(min(R2$Years),max(R2$Years),by = 0.01)
R = 0*s
n = length(s)
for (i in 1:n) {
C1 = R2[R2$Years<s[i],]$Salary
C2 = R2[R2$Years>=s[i],]$Salary
R[i] = sum((C1 - mean(C1))^2)+sum((C2 - mean(C2))^2)
}
mean(s[R==min(R)])
min(R)
C1 = R2[R2$Years<mean(s[R==min(R)]),]$Salary
C2 = R2[R2$Years>=mean(s[R==min(R)]),]$Salary
sum((C1 - mean(C1))^2)
sum((C2 - mean(C2))^2)
length(C1)
length(C2)
# Consider Hits
s = seq(min(R1$Hits),max(R1$Hits),by = 1)
R = 0*s
n = length(s)
for (i in 1:n) {
C1 = R2[R2$Hits<s[i],]$Salary
C2 = R2[R2$Hits>=s[i],]$Salary
R[i] = sum((C1 - mean(C1))^2)+sum((C2 - mean(C2))^2)
}
mean(s[R==min(R)])
min(R)
C1 = R2[R2$Hits<mean(s[R==min(R)]),]$Salary
C2 = R2[R2$Hits>=mean(s[R==min(R)]),]$Salary
sum((C1 - mean(C1))^2)
sum((C2 - mean(C2))^2)
length(C1)
length(C2)

# Let us try to split R3
nrow(R3)

# Consider Years
s = seq(min(R3$Years),max(R3$Years),by = 0.01)
R = 0*s
n = length(s)
for (i in 1:n) {
C1 = R3[R3$Years<s[i],]$Salary
C2 = R3[R3$Years>=s[i],]$Salary
R[i] = sum((C1 - mean(C1))^2)+sum((C2 - mean(C2))^2)
}
mean(s[R==min(R)])
min(R)
C1 = R3[R3$Years<mean(s[R==min(R)]),]$Salary
C2 = R3[R3$Years>=mean(s[R==min(R)]),]$Salary
sum((C1 - mean(C1))^2)
sum((C2 - mean(C2))^2)
length(C1)
length(C2)
# Consider Hits
s = seq(min(R3$Hits),max(R3$Hits),by = 1)
R = 0*s
n = length(s)
for (i in 1:n) {
C1 = R3[R3$Hits<s[i],]$Salary
C2 = R3[R3$Hits>=s[i],]$Salary
R[i] = sum((C1 - mean(C1))^2)+sum((C2 - mean(C2))^2)
}
mean(s[R==min(R)])
min(R)
C1 = R3[R3$Hits<mean(s[R==min(R)]),]$Salary
C2 = R3[R3$Hits>=mean(s[R==min(R)]),]$Salary
sum((C1 - mean(C1))^2)
sum((C2 - mean(C2))^2)
length(C1)
length(C2)



# Let us try to split R3
nrow(R4)

# Consider Years
s = seq(min(R4$Years),max(R4$Years),by = 0.01)
R = 0*s
n = length(s)
for (i in 1:n) {
C1 = R4[R4$Years<s[i],]$Salary
C2 = R4[R4$Years>=s[i],]$Salary
R[i] = sum((C1 - mean(C1))^2)+sum((C2 - mean(C2))^2)
}
mean(s[R==min(R)])
min(R)
C1 = R4[R4$Years<mean(s[R==min(R)]),]$Salary
C2 = R4[R4$Years>=mean(s[R==min(R)]),]$Salary
sum((C1 - mean(C1))^2)
sum((C2 - mean(C2))^2)
length(C1)
length(C2)
# Consider Hits
s = seq(min(R4$Hits),max(R4$Hits),by = 1)
R = 0*s
n = length(s)
for (i in 1:n) {
C1 = R4[R4$Hits<s[i],]$Salary
C2 = R4[R4$Hits>=s[i],]$Salary
R[i] = sum((C1 - mean(C1))^2)+sum((C2 - mean(C2))^2)
}
mean(s[R==min(R)])
min(R)
C1 = R4[R4$Hits<mean(s[R==min(R)]),]$Salary
C2 = R4[R4$Hits>=mean(s[R==min(R)]),]$Salary
sum((C1 - mean(C1))^2)
sum((C2 - mean(C2))^2)
length(C1)
length(C2)


R5 = R3[R3$Years>=6.505,]
# Consider Years
s = seq(min(R5$Years),max(R5$Years),by = 0.01)
R = 0*s
n = length(s)
for (i in 1:n) {
C1 = R5[R5$Years<s[i],]$Salary
C2 = R5[R5$Years>=s[i],]$Salary
R[i] = sum((C1 - mean(C1))^2)+sum((C2 - mean(C2))^2)
}
mean(s[R==min(R)])
min(R)
C1 = R5[R5$Years<mean(s[R==min(R)]),]$Salary
C2 = R5[R5$Years>=mean(s[R==min(R)]),]$Salary
sum((C1 - mean(C1))^2)
sum((C2 - mean(C2))^2)
length(C1)
length(C2)
# Consider Hits
s = seq(min(R5$Hits),max(R3$Hits),by = 1)
R = 0*s
n = length(s)
for (i in 1:n) {
C1 = R5[R5$Hits<s[i],]$Salary
C2 = R5[R5$Hits>=s[i],]$Salary
R[i] = sum((C1 - mean(C1))^2)+sum((C2 - mean(C2))^2)
}
mean(s[R==min(R)])
min(R)
C1 = R5[R5$Hits<mean(s[R==min(R)]),]$Salary
C2 = R5[R5$Hits>=mean(s[R==min(R)]),]$Salary
sum((C1 - mean(C1))^2)
sum((C2 - mean(C2))^2)
length(C1)
length(C2)


###################
R1 = Ht[Ht$Years<3.505,]
R2 = Ht[Ht$Years>=3.505 & Ht$Years<4.505,]
R3 = Ht[Ht$Years>=4.505 & Ht$Hits<117.505,]
R4 = Ht[Ht$Years>=4.505 & Ht$Hits>=117.505,]

R8 = R1[R1$Hits<114.5,]
R9 = R1[R1$Hits>=114.5,]

R10 = R2[R2$Hits<106.5,]
R11 = R2[R2$Hits>=106.5,]

R12 = R3[R1$Years<6.505,]
R13 = R3[R1$Years>=6.505,]

R14 = R4[R1$Years<12.505,]
R15 = R4[R1$Years>=12.505,]




# Consider Years
s = seq(min(R8$Years),max(R8$Years),by = 0.01)
R = 0*s
n = length(s)
for (i in 1:n) {
C1 = R8[R8$Years<s[i],]$Salary
C2 = R8[R8$Years>=s[i],]$Salary
R[i] = sum((C1 - mean(C1))^2)+sum((C2 - mean(C2))^2)
}
mean(s[R==min(R)])
min(R)
C1 = R8[R8$Years<mean(s[R==min(R)]),]$Salary
C2 = R8[R8$Years>=mean(s[R==min(R)]),]$Salary
sum((C1 - mean(C1))^2)
sum((C2 - mean(C2))^2)
length(C1)
length(C2)
# Consider Hits
s = seq(min(R8$Hits),max(R8$Hits),by = 1)
R = 0*s
n = length(s)
for (i in 1:n) {
C1 = R8[R8$Hits<s[i],]$Salary
C2 = R8[R8$Hits>=s[i],]$Salary
R[i] = sum((C1 - mean(C1))^2)+sum((C2 - mean(C2))^2)
}
mean(s[R==min(R)])
min(R)
C1 = R8[R8$Hits<mean(s[R==min(R)]),]$Salary
C2 = R8[R8$Hits>=mean(s[R==min(R)]),]$Salary
sum((C1 - mean(C1))^2)
sum((C2 - mean(C2))^2)
length(C1)
length(C2)

SS = abs(R-14)
mean(s[SS==min(SS)])

C1 = R8[R8$Hits<mean(s[SS==min(SS)]),]$Salary
C2 = R8[R8$Hits>=mean(s[SS==min(SS)]),]$Salary
sum((C1 - mean(C1))^2)
sum((C2 - mean(C2))^2)
length(C1)
length(C2)





# Consider Years
s = seq(min(R9$Years),max(R9$Years),by = 0.01)
R = 0*s
n = length(s)
for (i in 1:n) {
C1 = R9[R9$Years<s[i],]$Salary
C2 = R9[R9$Years>=s[i],]$Salary
R[i] = sum((C1 - mean(C1))^2)+sum((C2 - mean(C2))^2)
}
mean(s[R==min(R)])
min(R)
C1 = R9[R9$Years<mean(s[R==min(R)]),]$Salary
C2 = R9[R9$Years>=mean(s[R==min(R)]),]$Salary
sum((C1 - mean(C1))^2)
sum((C2 - mean(C2))^2)
length(C1)
length(C2)
# Consider Hits
s = seq(min(R9$Hits),max(R9$Hits),by = 1)
R = 0*s
n = length(s)
for (i in 1:n) {
C1 = R9[R9$Hits<s[i],]$Salary
C2 = R9[R9$Hits>=s[i],]$Salary
R[i] = sum((C1 - mean(C1))^2)+sum((C2 - mean(C2))^2)
}
mean(s[R==min(R)])
min(R)
C1 = R9[R9$Hits<mean(s[R==min(R)]),]$Salary
C2 = R9[R9$Hits>=mean(s[R==min(R)]),]$Salary
sum((C1 - mean(C1))^2)
sum((C2 - mean(C2))^2)
length(C1)
length(C2)




###################
R1 = Ht[Ht$Years<3.505,]
R2 = Ht[Ht$Years>=3.505 & Ht$Years<4.505,]
R3 = Ht[Ht$Years>=4.505 & Ht$Hits<117.505,]
R4 = Ht[Ht$Years>=4.505 & Ht$Hits>=117.505,]

R8 = R1[R1$Hits<114.5,]
R9 = R1[R1$Hits>=114.5,]

R10 = R2[R2$Hits<106.5,]
R11 = R2[R2$Hits>=106.5,]

R12 = R3[R3$Years<6.505,]
R13 = R3[R3$Years>=6.505,]

R14 = R4[R4$Years<12.505,]
R15 = R4[R4$Years>=12.505,]



# Consider Years
s = seq(min(R13$Years),max(R13$Years),by = 0.01)
R = 0*s
n = length(s)
for (i in 1:n) {
C1 = R13[R13$Years<s[i],]$Salary
C2 = R13[R13$Years>=s[i],]$Salary
R[i] = sum((C1 - mean(C1))^2)+sum((C2 - mean(C2))^2)
}
mean(s[R==min(R)])
min(R)
C1 = R13[R13$Years<mean(s[R==min(R)]),]$Salary
C2 = R13[R13$Years>=mean(s[R==min(R)]),]$Salary
sum((C1 - mean(C1))^2)
sum((C2 - mean(C2))^2)
length(C1)
length(C2)
# Consider Hits
s = seq(min(R13$Hits),max(R13$Hits),by = 1)
R = 0*s
n = length(s)
for (i in 1:n) {
C1 = R13[R13$Hits<s[i],]$Salary
C2 = R13[R13$Hits>=s[i],]$Salary
R[i] = sum((C1 - mean(C1))^2)+sum((C2 - mean(C2))^2)
}
mean(s[R==min(R)])
min(R)
C1 = R13[R13$Hits<mean(s[R==min(R)]),]$Salary
C2 = R13[R13$Hits>=mean(s[R==min(R)]),]$Salary
sum((C1 - mean(C1))^2)
sum((C2 - mean(C2))^2)
length(C1)
length(C2)







# Consider Years
s = seq(min(R15$Years),max(R15$Years),by = 0.01)
R = 0*s
n = length(s)
for (i in 1:n) {
C1 = R15[R15$Years<s[i],]$Salary
C2 = R15[R15$Years>=s[i],]$Salary
R[i] = sum((C1 - mean(C1))^2)+sum((C2 - mean(C2))^2)
}
mean(s[R==min(R)])
min(R)
C1 = R15[R15$Years<mean(s[R==min(R)]),]$Salary
C2 = R15[R15$Years>=mean(s[R==min(R)]),]$Salary
sum((C1 - mean(C1))^2)
sum((C2 - mean(C2))^2)
length(C1)
length(C2)
# Consider Hits
s = seq(min(R15$Hits),max(R15$Hits),by = 1)
R = 0*s
n = length(s)
for (i in 1:n) {
C1 = R15[R15$Hits<s[i],]$Salary
C2 = R15[R15$Hits>=s[i],]$Salary
R[i] = sum((C1 - mean(C1))^2)+sum((C2 - mean(C2))^2)
}
mean(s[R==min(R)])
min(R)
C1 = R15[R15$Hits<mean(s[R==min(R)]),]$Salary
C2 = R15[R15$Hits>=mean(s[R==min(R)]),]$Salary
sum((C1 - mean(C1))^2)
sum((C2 - mean(C2))^2)
length(C1)
length(C2)


mean(Ht$Salary)
mean(Ht[Ht$Years<4.505,]$Salary)
mean(Ht[Ht$Years<3.505,]$Salary)
mean(Ht[Ht$Years<3.505 & Ht$Hits<117.505,]$Salary)
mean(Ht[Ht$Years<3.505 & Ht$Hits<41,]$Salary)
mean(Ht[Ht$Years<3.505 & Ht$Hits<117.505 & Ht$Hits>=41,]$Salary)
mean(Ht[Ht$Years<3.505 & Ht$Hits>=117.505,]$Salary)
mean(Ht[Ht$Years<4.505 & Ht$Years>=3.505,]$Salary)
mean(Ht[Ht$Years<4.505 & Ht$Years>=3.505 & Ht$Hits<106.5,]$Salary)
mean(Ht[Ht$Years<4.505 & Ht$Years>=3.505 & Ht$Hits>=106.5,]$Salary)


mean(Ht[Ht$Years>=4.505,]$Salary)
mean(Ht[Ht$Years>=4.505 & Ht$Hits<117.5,]$Salary)
mean(Ht[Ht$Years>=4.505 & Ht$Hits<117.5 & Ht$Years<6.505,]$Salary)
mean(Ht[Ht$Years>=4.505 & Ht$Hits<117.5 & Ht$Years>=6.505 & Ht$Hits<51,]$Salary)
mean(Ht[Ht$Years>=4.505 & Ht$Hits<117.5 & Ht$Years>=6.505 & Ht$Hits>=51,]$Salary)

mean(Ht[Ht$Years>=4.505 & Ht$Hits>=117.5,]$Salary)
mean(Ht[Ht$Years>=4.505 & Ht$Hits>=117.5 & Ht$Years<12.505,]$Salary)
mean(Ht[Ht$Years>=4.505 & Ht$Hits>=117.5 & Ht$Years>=12.505,]$Salary)
mean(Ht[Ht$Years>=4.505 & Ht$Hits>=117.5 & Ht$Years>=12.505 & Ht$Years<14.505,]$Salary)
mean(Ht[Ht$Years>=4.505 & Ht$Hits>=117.5 & Ht$Years>=12.505 & Ht$Years>=14.505,]$Salary)
