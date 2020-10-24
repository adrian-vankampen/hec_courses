#-------------------------------------------------------------------------------
#============= WEEK 2 - EXERCISE 1 =============================================
#-------------------------------------------------------------------------------
setwd("~/GitHub/QMM2020/Week_2")

df_MC <- as.data.frame(read.csv("McCracken.csv"))

# a.

plot(df_MC$PurchaseVol ~ Age)
plot(df_MC$PurchaseVol ~ FamilyIncome)
plot(df_MC$PurchaseVol ~ FamilySize)

corMat <- as.data.frame(round(cor(df_MC),3))

# b.

cor1 <- cor.test(df_MC$PurchaseVol, df_MC$Age)
cor2 <- cor.test(df_MC$PurchaseVol, df_MC$FamilyIncome)
cor3 <- cor.test(df_MC$PurchaseVol, df_MC$FamilySize)

# c.
nullModel <- lm(PurchaseVol ~ 1, data = df_MC)
fullModel <- lm(PurchaseVol ~ Age + FamilyIncome + FamilySize, data = df_MC)
bestModelF_p <- step(nullModel, scope = list(lower = nullModel, upper = fullModel), direction = "forward")

# d.

summary(bestModelF_p)

# R-squared is 39.55 %, the adjusted R-squared is 34.06 %.

# e.

res1 <- resid(bestModelF_p)
plot(res1, type = "h")
resMean <- mean(res1)
print(round(resMean, 3))

# f.

MSE <- anova(bestModelF_p)["Residuals", "Mean Sq"]
print(round(MSE))

# g.

predictedValues <- predict(bestModelF_p)
plot(predictedValues, res1)

# h.

ci <- confint(bestModelF_p, level = 0.9)
lower <- round(ci["FamilyIncome", "5 %"], 5)
upper <- round(ci["FamilyIncome", "95 %"], 5)

# i.

Answer_i <- "The higher the family size, the more children, thus the lower the age. Family size is then negatively correlated with age."