# Week 4
#
# Exercise 2
library(dplyr)
library(car)
library(lmtest)
library(pROC)
library(ROCR)
library(lubridate)
library(pscl)

data <- read.csv("ship3.csv")
attach(data)
#
# a.
#
plot(SellingPrice, Crisis)
#
# b.
#
mod1 <- glm(Crisis ~ SellingPrice, family = binomial(link="logit"))
summary(mod1)
exp_beta <- exp(mod1$coefficients[2])
# Increasing the selling price increases the odds of crisis by a factor of exp_beta = 1.082924
#
# c.
#
pR2(mod1)
1 - pchisq(mod1$deviance, mod1$df.residual)
#
# d.
#
SPrange <- seq(from = min(SellingPrice), to = max(SellingPrice), by=1)
logits <- mod1$coefficients[1]+mod1$coefficients[2]*SPrange
probs <- exp(logits)/(1+exp(logits))
plot(SellingPrice, Crisis, pch=19, cex=0.5)
lines(SPrange, probs, col="blue")
#
# e.
#
set.seed(1)
id <- sample(seq(1,2), size = nrow(data), replace = TRUE, prob=c(0.75,0.25))
trainset <- data[id==1, ]
testset <- data[id==2, ]

train.model <- glm(Crisis~SellingPrice, family=binomial, data = trainset)
pred <- predict(train.model, type="response", newdata=testset)
pred.bin <- ifelse(pred>=0.5, 1,0)
accuracy <- table(pred.bin, testset[, "Crisis"])
correct <- sum(diag(accuracy))/sum(accuracy)



detach(data)
