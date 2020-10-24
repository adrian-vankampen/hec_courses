# Week 4
#
# Exercise 2
#
library(dplyr)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(corrplot)
#
setwd(
  "~/HEC/4e année (MScM-BA)/1st semester/01. Quantitative Methods for Management/02. Exercices 2020/Week_5_Clustering"
)
#
data1 <- read.csv("Cars.csv")
attach(data1)

summary(data1)

par(mfrow = c(2,3))
for(i in 2:6){
  boxplot(data1[, i], main = names(data1[i]), type = "l")
}
par(mfrow = c(1,1))
