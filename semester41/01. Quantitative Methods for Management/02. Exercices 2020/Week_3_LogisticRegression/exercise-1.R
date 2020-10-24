# Week 3 - Logistical regression
#
#
library(dplyr)
library(car)
library(lmtest)
library(pROC)
library(ROCR)
library(lubridate)
library(pscl)
#
#
# Exercise 1 - Profiling
#
profiling <- as.data.frame(read.csv("Profiling.csv"))
attach(profiling)
#
# a.
#
plot(mobile)
#
# b.
#
reg1 <- glm(mobile ~ income + hours + where, family = binomial(link= "logit"))
summary(reg1)
beta_where <- coefficients(reg1)[4]
mod_where <- exp(beta_where)
#
# c.
#
vif_reg1 <- vif(reg1)
corr_reg1 <- cor(profiling)
#
# d.
#
reg2 <- glm(mobile ~ income + hours, family = binomial(link= "logit"))
summary(reg2)
lrtest(reg1, reg2)
#
# e.
#
profiling_test <- read.csv("Profiling-Test.csv")
profiling_test

pred <- predict(reg1, type="response", newdata = profiling_test)

accuracy_pred <- function(pred, c){
  pred.bin <- ifelse(pred >= c, TRUE, FALSE)
  accuracy <- table(pred.bin, profiling_test[,"mobile"])
  sum(diag(accuracy)/sum(accuracy))
}

acc_c1 <- accuracy_pred(pred, 1/2)
acc_c2 <- accuracy_pred(pred, 2/3)
#
# f.
#
o.exp <- exp(reg1$coefficients[1]+reg1$coefficients[2]*profiling_test[1,2]
             +reg1$coefficients[3]*profiling_test[1,3]+reg1$coefficients[4]*profiling_test[1,4])
o.exp/(1+o.exp)
#
# g.
#
f1 <- roc(mobile~income)
f2 <- roc(mobile~hours)
f3 <- roc(mobile~where)

plot(f1)
lines(f2, col="green")
lines(f3, col="red")
#
# h.
#
confint(reg1)
