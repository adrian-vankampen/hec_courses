# Week 4
#
# Exercise 2
#
library(ggcorrplot)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(corrplot)
#
setwd(
  "~/HEC/4e année (MScM-BA)/1st semester/01. Quantitative Methods for Management/02. Exercices 2020/Week_4_FactorAnalysis"
)
#
data3 <- read.csv("ship3.csv")
attach(data3)

data3.ss <- data3[-c(1,6)]
data3.ss

corrplot(cor(data3.ss))

pca3 <- PCA(data3.ss)
pca3$eig                # K-G rule => 2 factors

fviz_pca_biplot(pca3,
                geom.ind = "point",
                col.ind = as.factor(Crisis),
                addEllipses = TRUE,
                )

#
# c.
#
fact.anal3 <- factanal(data3.ss, factors=1)
print(fact.anal3, digits=2)

detach(data3)