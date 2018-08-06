#mlr with the daily data from 2011 and 2012
data = read.csv("C:/Users/Mallory/Documents/SMU/Doing Data Science/project 2/dayview.csv")
head(data)
is.data.frame(data)
data.lm=lm(cnt~season+yr+mnth+holiday+weekday+workingday+weathersit+
             temp+atemp+hum+windspeed, data = data)
summary(data.lm)
plot(data.lm)
#variance covariancec matrix
vcov(data.lm)
#confidence intervals for the model
confint(data.lm, level=0.95)

#resiuduals plot 
data.res=resid(data.lm)
data.res
plot(data$cnt, data.res, ylab='Residuals', xlab='Count of Bikes Shared', main='Residuals Plot')

#histograms
historgram <- hist(data.res, breaks = 10, density = 10,
                   col = "lightgray", xlab = "Count of Bikes Shared", main = "Histogram of Residuals with Normal Curve") 
xfit <- seq(min(data.res), max(data.res), length = 40) 
yfit <- dnorm(xfit, mean = mean(data.res), sd = sd(data.res))

#additional diagnostic plots (QQ and leverage)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(data.lm)

#Stepwise Regression
library(MASS)
step1 <- stepAIC(data.lm, direction="both")
step1$anova # display results
summary(step1)

# Forward Regression
step2 <- stepAIC(data.lm, direction="forward")
step2$anova # display results
summary(step2)

#Backward Regression
step3 <- stepAIC(data.lm, direction="backward")
step3$anova # display results
summary(step3)

#getting the CI and PI for a new point that is not in original data set
newx=data$cnt
newx=sort(newx)
prd_c=predict(data.lm, newdata= data.frame(cnt = newx), interval=c("confidence"), 
              type = c("response"), level=0.95) 
prd_c
newpoint <- data.frame(holiday=NA, cnt=1000)
predict(data.lm, newpoint, interval="confidence", level = 0.95)
