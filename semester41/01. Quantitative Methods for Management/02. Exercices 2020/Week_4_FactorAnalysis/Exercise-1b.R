# Week 4
#
# Exercise 1
#
library(FactoMineR)
library(ggplot2)
library(corrplot)
library(factoextra)
#
#
data <- read.csv("Vehicles.csv")
#
# a.
#

corr <- cor(data[-1])
corr

data.pca <- prcomp(data[-1])
mypca <- PCA(data[-1])

summary(data.pca)
summary(mypca)
#
# b.
#
eig.val <- get_eigenvalue(mypca)
# Kaiser-Guttman rule = Components with eigenvalues greater than 1 should be retained.
round(eig.val, digits=2)
# We keep only dimension 1 and 2 !
#
# c.
#
fviz_pca_var(mypca,
             col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
#
# d.
#
n.factors <- 5

fit <- factanal(data[-1], n.factors, rotation="none")
print(fit, digits=2)

fitrot <- factanal(data[-1], n.factors, rotation="varimax")
print(fitrot, digits=2)
#
# e.
#
fitpromax <- factanal(data[-1], n.factors, rotation="promax")
print(fitpromax, digits=2)

