#### Perform an exploratory analysis of data ####
# Understand variables
# Find patterns in data
# Suggest modeling stategies

# Clear console
cat("\014")

# Remove all variables
rm(list=ls(all=TRUE)) 

# Set working directory
setwd('FolderPath')
# Load Data
pcancer = read.csv("prostate_cancer.csv")

#Install if the package doesn't exist 
if (!require(DataExplorer)) install.packages('DataExplorer')
library(DataExplorer)


# Get high level overview of data
str(pcancer)
summary(pcancer)
pcancer[, 'vesinv'] <- as.factor(pcancer[, 'vesinv'])

# Do we need to modify the data frame? Yes, subject is not necessary
if (!require(dplyr)) install.packages('dplyr')
library(dplyr)
pcancer = select(pcancer,-subject)
summary(pcancer)

plot_str(pcancer, "fontSize" = 60)

class(pcancer)
View(pcancer)
head(pcancer)
str(pcancer)


############## UNIVARIATE ANALYSIS ##################
######## PSA Level ##############
par(mfrow=c(3,1))

boxplot(pcancer$psa,
        main="PSA level Central Tendency and Spread with Box Plot",
        xlab = "Serum prostate-specific antigen level (mg/ml)",
        col="gray",
        border="black",
        horizontal=TRUE
)
hist(pcancer$psa,main="PSA level Central Tendency and Spread with Histogram",xlab = "Serum prostate-specific antigen level (mg/ml)")
plot(density(pcancer$psa),main="PSA level Central Tendency and Spread with Density Plot")

######## Cancer Volume ##############
par(mfrow=c(3,1))

boxplot(pcancer$cancervol,
        main="Cancer Volume Central Tendency and Spread with Box Plot",
        xlab = "Estimate of prostate cancer volume (cc)",
        col="gray",
        border="black",
        horizontal=TRUE
)
hist(pcancer$cancervol,main="Cancer Volume Central Tendency and Spread with Histogram",xlab = "Estimate of prostate cancer volume (cc)")
plot(density(pcancer$cancervol),main="Cancer Volume Central Tendency and Spread with Density Plot")

######## Weight ##############
par(mfrow=c(3,1))

boxplot(pcancer$weight,
        main="Weight Central Tendency and Spread with Box Plot",
        xlab = "prostate weight (gm)",
        col="gray",
        border="black",
        horizontal=TRUE
)
hist(pcancer$weight,main="Weight Central Tendency and Spread with Histogram",xlab = "prostate weight (gm)")
plot(density(pcancer$weight),main="Weight Central Tendency and Spread with Density Plot")

######## Age ##############
par(mfrow=c(3,1))

boxplot(pcancer$age,
        main="Age Central Tendency and Spread with Box Plot",
        xlab = "Age of patient (years)",
        col="gray",
        border="black",
        horizontal=TRUE
)
hist(pcancer$age,main="Age Central Tendency and Spread with Histogram",xlab = "Age of patient (years)")
plot(density(pcancer$age),main="Age Central Tendency and Spread with Density Plot")

######## Benign prostatic hyperplasia ##############
par(mfrow=c(3,1))

boxplot(pcancer$benpros,
        main="Benign prostatic hyperplasia Central Tendency and Spread with Box Plot",
        xlab = "Amount of benign prostatic hyperplasia (cm^2)",
        col="gray",
        border="black",
        horizontal=TRUE
)
hist(pcancer$benpros,main="Benign prostatic hyperplasia Central Tendency and Spread with Histogram",xlab = "Amount of benign prostatic hyperplasia (cm^2)")
plot(density(pcancer$benpros),main="Benign prostatic hyperplasia Central Tendency and Spread with Density Plot")

######## Capsular penetration ##############
par(mfrow=c(3,1))

boxplot(pcancer$capspen,
        main="Capsular penetration Central Tendency and Spread with Box Plot",
        xlab = "Degree of capsular penetration (cm)",
        col="gray",
        border="black",
        horizontal=TRUE
)
hist(pcancer$capspen,main="Capsular penetration Central Tendency and Spread with Histogram",xlab = "Degree of capsular penetration (cm)")
plot(density(pcancer$capspen),main="Capsular penetration Central Tendency and Spread with Density Plot")

######## Gleason score ##############
par(mfrow=c(3,1))

boxplot(pcancer$gleason,
        main="Gleason score Central Tendency and Spread with Box Plot",
        xlab = "Pathologically determined grade of disease (6, 7 or 8)",
        col="gray",
        border="black",
        horizontal=TRUE
)
hist(pcancer$gleason,main="Gleason score Central Tendency and Spread with Histogram",xlab = "Pathologically determined grade of disease (6, 7 or 8)")
plot(density(pcancer$gleason),main="Gleason score Central Tendency and Spread with Density Plot")

########### CATEGORICAL DATA #############
par(mfrow=c(1,1))
vesinv2 <- table(pcancer$vesinv)
barplot(vesinv2[order(vesinv2, decreasing = TRUE)],horiz = TRUE,
        main = 'Seminal vesicle invasion count with Bar plot',
        ylab = 'Presence (1) or absence (0)',
        xlab = 'Number of records')


################## MULTIVARIATE ANALYSIS #######################


pcancer <- pcancer[-c(which.max(pcancer$weight)),] 
str(pcancer)

plot(pcancer)
pcancer2 = select(pcancer,-vesinv)
cor(pcancer2)


############### Transformation ##########################

######## PSA Level ##############
psa2 = log(pcancer$psa,exp(1))
psa2 = psa2 - mean(psa2)
par(mfrow=c(3,1))

boxplot(psa2,
        main="PSA Level Central Tendency and Spread with Box Plot",
        xlab = "Serum prostate-specific antigen level (mg/ml)",
        col="gray",
        border="black",
        horizontal=TRUE
)
hist(psa2,main="PSA Level Central Tendency and Spread with Histogram",
     xlab = "Serum prostate-specific antigen level (mg/ml)")
plot(density(psa2),main="PSA Level Central Tendency and Spread with Density Plot")

######## Cancer Volume ##############
cancer2 = log(pcancer$cancervol,exp(1))
cancer2 = cancer2 - mean(cancer2)
par(mfrow=c(3,1))

boxplot(cancer2,
        main="Cancer Volume Central Tendency and Spread with Box Plot",
        xlab = "Estimate of prostate cancer volume (cc)",
        col="gray",
        border="black",
        horizontal=TRUE
)
hist(cancer2,main="Cancer Volume Central Tendency and Spread with Histogram",
     xlab = "Estimate of prostate cancer volume (cc)")
plot(density(cancer2),main="Cancer Volume Central Tendency and Spread with Density Plot")
plot(cancer2)

############### Q(3) ####################
fit1 <- lm(psa2 ~ cancer2)
summary(fit1)
par(mfrow=c(1,1))
plot(cancer2,psa2,main='Plot of transformed PSA Level against Cancer Volume',
     xlab = 'Cancer Volume after logarithmic transformation',
     ylab = 'PSA Level after logarithmic transformation',)
abline(fit1)

###
fit2 <- lm(psa2 ~ pcancer$weight)
fit2
summary(fit2)
par(mfrow=c(1,1))
plot(pcancer$weight,psa2,main='Plot of transformed PSA Level against prostate weight',
     xlab = 'Prostate weight (gm)',
     ylab = 'PSA Level after logarithmic transformation')
abline(fit2)
###
fit3 <- lm(psa2 ~ pcancer$age)
fit3
summary(fit3)
par(mfrow=c(1,1))
plot(pcancer$age,psa2,main='Plot of transformed PSA Level against Age',
     xlab = 'Age of patient (years)',
     ylab = 'PSA Level after logarithmic transformation')
abline(fit3)


###
fit4 <- lm(psa2 ~ pcancer$benpros)
fit4
summary(fit4)
par(mfrow=c(1,1))
plot(pcancer$benpros,psa2,main='Plot of transformed PSA Level against Benign prostatic	hyperplasia',
     xlab = 'Amount of benign prostatic hyperplasia (cm^2)',
     ylab = 'PSA Level after logarithmic transformation')
abline(fit4)

###
fit5 <- lm(psa2 ~ pcancer$capspen)
fit5
summary(fit5)
par(mfrow=c(1,1))
plot(pcancer$benpros,psa2,main='Plot of transformed PSA Level against Capsular penetration',
     xlab = 'Degree of capsular penetration (cm)',
     ylab = 'PSA Level after logarithmic transformation')
abline(fit5)

###
fit6 <- lm(psa2 ~ pcancer$gleason)
fit6
summary(fit6)
par(mfrow=c(1,1))
plot(pcancer$gleason,psa2,main='Plot of transformed PSA Level against Gleason score',
     xlab = 'Pathologically determined grade of disease (6, 7 or 8)',
     ylab = 'PSA Level after logarithmic transformation')
abline(fit6)

###
fit7 <- lm(psa2 ~ pcancer$vesinv)
fit7
summary(fit7)
par(mfrow=c(1,1))
plot(pcancer$vesinv,psa2,main='Plot of transformed PSA Level against Seminal vesicle invasion',
     xlab = 'Presence (1) or absence (0) of seminal vesicle invasion',
     ylab = 'PSA Level after logarithmic transformation')
abline(fit7)


################ Mult Lin Reg (d) ####################
summary(pcancer)
str(pcancer)
fitm <- lm(psa2 ~ cancer2+pcancer$weight+pcancer$age+pcancer$benpros+pcancer$vesinv+pcancer$capspen+pcancer$gleason)
print(fitm)
summary(fitm)
plot(fitm)

fitcancer2 <- lm(psa2 ~ pcancer$weight+pcancer$age+pcancer$benpros+pcancer$vesinv+pcancer$capspen+pcancer$gleason)
anova(fitcancer2,fitm)

fitage <- lm(psa2 ~ cancer2+pcancer$weight+pcancer$benpros+pcancer$vesinv+pcancer$capspen+pcancer$gleason)
anova(fitage,fitm)

fittemp <- lm(psa2 ~ cancer2+pcancer$weight+pcancer$benpros+pcancer$vesinv+pcancer$capspen+pcancer$gleason)

fitweight <- lm(psa2 ~ cancer2+pcancer$benpros+pcancer$vesinv+pcancer$capspen+pcancer$gleason)
anova(fitweight,fittemp)

fittemp <- lm(psa2 ~ cancer2+pcancer$weight+pcancer$benpros+pcancer$vesinv+pcancer$capspen+pcancer$gleason)

fitbenpros <- lm(psa2 ~ cancer2+pcancer$weight+pcancer$vesinv+pcancer$capspen+pcancer$gleason)
anova(fitbenpros,fittemp)


fittemp <- lm(psa2 ~ cancer2+pcancer$weight+pcancer$vesinv+pcancer$capspen+pcancer$gleason)

fitcapspen <- lm(psa2 ~ cancer2+pcancer$weight+pcancer$vesinv+pcancer$gleason)
anova(fitcapspen,fittemp)

fittemp <- lm(psa2 ~ cancer2+pcancer$weight+pcancer$vesinv+pcancer$gleason)

fitgleason <- lm(psa2 ~ cancer2+pcancer$weight+pcancer$vesinv)
anova(fitgleason,fittemp)

fittemp <- lm(psa2 ~ cancer2+pcancer$weight+pcancer$vesinv+pcancer$gleason)

fitvesinv <- lm(psa2 ~ cancer2+pcancer$weight+pcancer$gleason)
anova(fitvesinv,fittemp)


####### product terms #########
fitprod <- lm(psa2 ~cancer2+
              pcancer$weight+
              pcancer$vesinv+
              pcancer$gleason+
              cancer2:pcancer$weight+
                cancer2*pcancer$vesinv+
                cancer2*pcancer$gleason+
                pcancer$weight*pcancer$vesinv+
                pcancer$weight*pcancer$gleason+
                pcancer$vesinv*pcancer$gleason)
print(fitprod)
summary(fitprod)


fitprodtemp <- lm(psa2 ~cancer2+
                    pcancer$weight+
                    pcancer$vesinv+
                    pcancer$gleason+
                    cancer2:pcancer$weight+
                    cancer2*pcancer$vesinv+
                    cancer2*pcancer$gleason+
                    pcancer$weight*pcancer$vesinv+
                    pcancer$weight*pcancer$gleason+
                    pcancer$vesinv*pcancer$gleason)
summary(fitprodtemp)
fitprodvw <- lm(psa2 ~cancer2+
                  pcancer$weight+
                  pcancer$vesinv+
                  pcancer$gleason+
                  cancer2*pcancer$vesinv+
                  cancer2*pcancer$gleason+
                  pcancer$weight*pcancer$vesinv+
                  pcancer$weight*pcancer$gleason+
                  pcancer$vesinv*pcancer$gleason)
summary(fitprodvw)
anova(fitprodtemp,fitprodvw)



fitprodtemp <- lm(psa2 ~cancer2+
                    pcancer$weight+
                    pcancer$vesinv+
                    pcancer$gleason+
                    cancer2*pcancer$vesinv+
                    cancer2*pcancer$gleason+
                    pcancer$weight*pcancer$vesinv+
                    pcancer$weight*pcancer$gleason+
                    pcancer$vesinv*pcancer$gleason)
summary(fitprodtemp)
fitprodvg <- lm(psa2 ~cancer2+
                  pcancer$weight+
                  pcancer$vesinv+
                  pcancer$gleason+
                  cancer2*pcancer$vesinv+
                  pcancer$weight*pcancer$vesinv+
                  pcancer$weight*pcancer$gleason+
                  pcancer$vesinv*pcancer$gleason)
summary(fitprodvg)
anova(fitprodtemp,fitprodvg)




fitprodtemp <- lm(psa2 ~cancer2+
                    pcancer$weight+
                    pcancer$vesinv+
                    pcancer$gleason+
                    cancer2*pcancer$vesinv+
                    pcancer$weight*pcancer$vesinv+
                    pcancer$weight*pcancer$gleason+
                    pcancer$vesinv*pcancer$gleason)
summary(fitprodtemp)
fitprodvs <- lm(psa2 ~cancer2+
                  pcancer$weight+
                  pcancer$vesinv+
                  pcancer$gleason+
                  pcancer$weight*pcancer$vesinv+
                  pcancer$weight*pcancer$gleason+
                  pcancer$vesinv*pcancer$gleason)
summary(fitprodvs)
anova(fitprodtemp,fitprodvs)



fitprodtemp <- lm(psa2 ~cancer2+
                    pcancer$weight+
                    pcancer$vesinv+
                    pcancer$gleason+
                    pcancer$weight*pcancer$vesinv+
                    pcancer$weight*pcancer$gleason+
                    pcancer$vesinv*pcancer$gleason)
summary(fitprodtemp)
fitprodwg <- lm(psa2 ~cancer2+
                  pcancer$weight+
                  pcancer$vesinv+
                  pcancer$gleason+
                  pcancer$weight*pcancer$vesinv+
                  pcancer$vesinv*pcancer$gleason)
summary(fitprodvs)
anova(fitprodtemp,fitprodwg)



fitprodtemp <- lm(psa2 ~cancer2+
                    pcancer$weight+
                    pcancer$vesinv+
                    pcancer$gleason+
                    pcancer$weight*pcancer$vesinv+
                    pcancer$vesinv*pcancer$gleason)
summary(fitprodtemp)
fitprodws <- lm(psa2 ~cancer2+
                  pcancer$weight+
                  pcancer$vesinv+
                  pcancer$gleason+
                  pcancer$vesinv*pcancer$gleason)
summary(fitprodvs)
anova(fitprodtemp,fitprodws)


fitprodtemp <- lm(psa2 ~cancer2+
                    pcancer$weight+
                    pcancer$vesinv+
                    pcancer$gleason+
                    pcancer$weight*pcancer$vesinv+
                    pcancer$vesinv*pcancer$gleason)
summary(fitprodtemp)
fitprodws <- lm(psa2 ~cancer2+
                  pcancer$weight+
                  pcancer$vesinv+
                  pcancer$gleason+
                  pcancer$weight*pcancer$vesinv)
summary(fitprodvs)
anova(fitprodtemp,fitprodws)


fitprodtemp <- lm(psa2 ~cancer2+
                    pcancer$weight+
                    pcancer$vesinv+
                    pcancer$gleason+
                    pcancer$weight*pcancer$vesinv)
summary(fitprodtemp)
fitprodws <- lm(psa2 ~cancer2+
                  pcancer$weight+
                  pcancer$vesinv+
                  pcancer$gleason)
summary(fitprodvs)
anova(fitprodws,fitprodtemp)

####### Assumptions ######
fitFinal <- lm(psa2 ~cancer2+
                    pcancer$weight+
                    pcancer$vesinv+
                    pcancer$gleason+
                    pcancer$weight:pcancer$vesinv)
summary(fitFinal)
plot(fitted(fitFinal),resid(fitFinal),main = 'Residual Plot')
abline(h=0)
plot(fitFinal)
shapiro.test(residuals(fitFinal))
if (!require(lmtest)) install.packages('lmtest')
library(lmtest)
bptest(fitFinal)
summary(fitFinal)



############################

table(pcancer$vesinv)



#################################
v <- cancer2
w <- pcancer$weight
s1 <- pcancer$vesinv
g <- pcancer$gleason
fitFinal <- lm(psa2 ~ v+w+s+g+w:s)

summary(pcancer)
summary(fitFinal)
str(fitFinal)
predict(fitFinal,data.frame(v = log(7.0591)-7.0591,
                            w = 41.27,
                            s1="0",
                            g=6.885),
                            se.fit=T,interval="prediction")



par(mfrow=c(1,2))
boxplot(pcancer$psa~pcancer$vesinv,
        main="PSA level Central Tendency with Box Plot",
        xlab = "Serum prostate-specific antigen level (mg/ml)",
        col="gray",
        border="black",
        horizontal=FALSE
)

if (!require(sm)) install.packages('sm')
library(sm)
sm.density.compare(pcancer$psa,pcancer$vesinv, xlab="PSA Level with Seminal vesicle invasion")
# Add a legend (the color numbers start from 2 and go up)
legend("topright", levels(pcancer$vesinv), fill=2+(0:nlevels(pcancer$vesinv)))
title(main="Distributions of PSA Level - Seminal vesicle
      invasion Presence (1) or absence (0)")

par(mfrow=c(1,2))
boxplot(pcancer$psa~pcancer$gleason,
        main="PSA level Central Tendency with Box Plot",
        col="gray",
        border="black",
        horizontal=FALSE
)

if (!require(sm)) install.packages('sm')
library(sm)
sm.density.compare(pcancer$psa,pcancer$vesinv, xlab="PSA Level with Seminal vesicle invasion")
# Add a legend (the color numbers start from 2 and go up)
legend("topright", levels(pcancer$vesinv), fill=2+(0:nlevels(pcancer$vesinv)))
title(main="Distributions of PSA Level - Seminal vesicle
invasion Presence (1) or absence (0)")

