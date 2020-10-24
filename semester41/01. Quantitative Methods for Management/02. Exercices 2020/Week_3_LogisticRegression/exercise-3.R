# Week 3
#
# Exercise 3
#
library(dplyr)
library(car)
library(lmtest)
library(pROC)
library(ROCR)
library(lubridate)
library(pscl)

data <- read.csv("admission.csv")
attach(data)
#
#
# a.
plot(GPA, admit)
#
# b.
#
set.seed(1)
id <- sample(c(1,2), size=nrow(data), replace=TRUE, prob=c(.75,.25))
train <- data[id==1, ]
test <- data[id==2, ]
#
# c.
#
model1 <- glm(admit~GPA+as.factor(rank), family=binomial, data=train)
summary(model1)
#
# d.
#
GPArange <- seq(from = min(GPA), to = max(GPA), by = 0.01)

logits_rank1 <- model1$coefficients[1]+model1$coefficients[2]*GPArange
logits_rank2 <- model1$coefficients[1]+model1$coefficients[2]*GPArange + model1$coefficients[3]
logits_rank3 <- model1$coefficients[1]+model1$coefficients[2]*GPArange + model1$coefficients[4]
logits_rank4 <- model1$coefficients[1]+model1$coefficients[2]*GPArange + model1$coefficients[5]

probs_rank1 <- exp(logits_rank1)/(1+exp(logits_rank1))
probs_rank2 <- exp(logits_rank2)/(1+exp(logits_rank2))
probs_rank3 <- exp(logits_rank3)/(1+exp(logits_rank3))
probs_rank4 <- exp(logits_rank4)/(1+exp(logits_rank4))

plot(GPA, admit)
lines(GPArange, probs_rank1, col="blue")
lines(GPArange, probs_rank2, col="green")
lines(GPArange, probs_rank3, col="red")
lines(GPArange, probs_rank4, col="purple")
#
# e.
#
pred <- predict(model1, type="response", newdata = test)
pred.bin <- ifelse(pred >= 0.5, 1, 0)
accuracy <- table(pred.bin, test[, "admit"])
sum(diag(accuracy))/sum(accuracy)

pR2(model1)
#
# f.
#
prediction <- prediction(pred, test$admit)
perf <- performance(prediction, measure="tpr", x.measure="fpr")
plot(perf)

auc <- performance(prediction,measure="auc")
auc <- auc@y.values[1]
auc

model2 <- glm(admit~as.factor(rank), family=binomial, data=train)
model3 <- glm(admit~GPA, family=binomial, data=train)

lrtest(model1, model2)
lrtest(model1, model3)

detach(data)