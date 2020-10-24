# Week 4
#
# Exercise 1
#
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(corrplot)
#
setwd("~/HEC/4e année (MScM-BA)/1st semester/01. Quantitative Methods for Management/02. Exercices 2020/Week_4_FactorAnalysis")
#
data1 <- read.csv("Vehicles.csv")
attach(data1)
#
# a.
#
corr <- cor(as.matrix(data1[-1]))

pca1 <- PCA(data1[-1])
summary(pca1)
#
# b.
#
# Kaiser-Gutman : components with eigenvalue > 1
#
pca1$eig
#
# -> only components 1 and 2
#
# c.
#
plot(pca1, choix="var")
fviz_pca_biplot(pca1, 
                fill.ind = type, 
                geom.ind = "point", 
                col.ind = "black", 
                pointshape = 21, 
                pointsize = 2,     #
                addEllipses = TRUE # Adds ellipses
                )
#
# d.
#
factanal(data1[-1], factors = 2) # Explains 71% of variance... p-value is very low, so let's try more...
factanal(data1[-1], factors = 3) # and more...
factanal(data1[-1], factors = 4) # and still more...
unrot <- factanal(data1[-1], factors = 5) # Finally a p-value of 0.172, explains 82% of variance
print(unrot)

rot <- factanal(data1[-1], factors = 5, rotation = "varimax") # -> it doesn't change anything
print(rot)

promax <- factanal(data1[-1], factors=5, rotation = "promax") # -> it explains less of the variance !
print(promax)

detach(data1)
