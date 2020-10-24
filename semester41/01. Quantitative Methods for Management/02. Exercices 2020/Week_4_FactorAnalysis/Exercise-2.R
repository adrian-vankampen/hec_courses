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
data2 <- read.csv("Service.csv")
attach(data2)
#
# a.
#
cormat <- cor(as.matrix(data2))
ggcorrplot(cormat)
corrplot(cormat, method = "color")
#
# b.
#
pca2 <- PCA(data2)
summary(pca2)

pca2$eig           # -> according to the K-G rule : 3 factors



fact_analyze <- function(data,
                         alpha = 0.05,
                         nfact = 2,
                         rotation = "none") {
  fact_anal_it <- factanal(data, factors = nfact, rotation = rotation)
  while (fact_anal_it$PVAL[1] < alpha) {
    nfact <- nfact + 1
    if (nfact > length(data))
      return(fact_anal_it)
    fact_anal_it <- factanal(data2, factors = nfact, rotation)
  }
  return(fact_anal_it)
}

unrot.analysis.chisq <- fact_analyze(data2, alpha = 0.1) # Explains 88.7 % of variance
print(unrot.analysis.chisq) # 10 factors ! But it's a bit too much, so based on K-G rule, 3 factors

unrot.analysis.KG <- factanal(data2, factors=3) # -> already explains 78.1% of variance
print(unrot.analysis.KG)
#
# d.
#
# Using Chi-square test
#
rot.analysis.chisq <- fact_analyze(data2, rotation = "varimax") # No change
print(rot.analysis.chisq)

promax.analysis.chisq <- fact_analyze(data2, rotation = "promax") # Also no change
print(promax.analysis.chisq)
#
# Using Kaiser-Gutman's rule
#
rot.analysis.KG <- factanal(data2, factors=3, rotation = "varimax") # No change
print(rot.analysis.KG)

promax.analysis.KG <- factanal(data2, factors=3, rotation = "promax") # Explains 65.1 %, which is less
print(promax.analysis.KG)
#
# e.
#
uniq <- unrot.analysis.KG$uniquenesses
uniq[order(-uniq)[1:3]]               # V11, V12 and V1 are the most unique

com <- 1-uniq
com[order(-com)[1:3]]                 # V25, V16 and V26 are the least unique (the most common)

detach(data2)
