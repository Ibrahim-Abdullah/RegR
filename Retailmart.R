# cwj, 2015-03-17, replicating Chapter 6 using R
setwd("C:/users/Charlie/Documents/Ashesi/BigData/Ashesi-2015/2015-03-17-Logistic-Regression")
list.files()
y = read.csv("retailmart-csv.csv",header=TRUE)
# take categorical variables and recode as pairs of binaries
Male = rep(0,1000)
table(y$Implied.Gender)
Male[y$Implied.Gender=="M"] = 1
Female = rep(0,1000)
Female[y$Implied.Gender=="F"] = 1
Home = rep(0,1000)
Home[y$Home.Apt..PO.Box=="H"] = 1
Apt = rep(0,1000)
Apt[y$Home.Apt..PO.Box=="A"] = 1
# paste these four variables in front of the others in new frame
y2 = data.frame(Male=Male,Female=Female,Home=Home,Apt=Apt,y[,3:18])
# this is how to do the logistic regression
model = glm(PREGNANT ~ ., family=poisson, data=y2)
summary(model)
# doing linear models (regressions) in R with lm function
help(lm)
lmodel = lm(PREGNANT ~ ., data=y2)
summary(lmodel)
round(lmodel$coefficients,2)
# now bring in the set of 1000 new customers
z = read.csv("test-set-csv.csv",header=TRUE)
# do the predictions based on the learned linear model
lmodel = lm(PREGNANT ~ ., data=y2)
zfit = predict(lmodel, newdata=z)
length(zfit)
round(zfit[1:5],2) # check out first five to verify same as book
range(zfit) # we confirm range from -.35 to +1.25
# to generate ROC curves, use appropriate library
library(ROCR)
# store the do the linear prediction, and actual responses
lpred = prediction(zfit,z$PREGNANT)
# construct the performance stats using true pass rate on y
# axis and false positive rate on x axis (typical ROC curve)
lperf = performance(lpred,"tpr","fpr")
plot(lperf)
#
# now, we continue to do the logistic regression
glmodel = glm(PREGNANT ~ .,data=y2,family="binomial")
summary(glmodel)
glfit = predict(glmodel,z,type="response")
# we notice that the scores when actually pregnant are nearer 1
hist(glfit[z$PREGNANT==1])
# and the scores when actually not pregnant are nearer 0
hist(glfit[z$PREGNANT==0])
# bundle together fit and actual response in predictor object
glpred = prediction(glfit,z$PREGNANT)
# likewise, construct the performance stats for the ROC curve
glperf = performance(glpred,"tpr","fpr")
# Now we can show both ROC's on the same axes
# linear model in black
plot(lperf,xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i",lwd=2)
plot(glperf,add=TRUE,col=2,lwd=2) # logistic model in red
title(main="ROC Curves",
sub="Linear Model Black, Logistic Model Red")
