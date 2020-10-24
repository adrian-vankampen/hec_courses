#-----------------------
### Exercise 2
#-----------------------

# Libraries
library(tidyverse)
library(caret)
library(car)
library(devtools)

# Data

USComp <- read.csv("01. USComp.csv")
attach(USComp)

#A

Answer_A <- "From Exercise 1, we know that Assets, Earnings per share, and Total return in 1997 do not seem to correlate well with Profits."

#B

fit <- lm(Profits ~ Employees + Assets + Market_Value + Revenues + Stockholders_Equity + Total_Return_to_Investors_1997, data = USComp)
summary(fit)

#C


car::vif(fit)

fit_norev <- lm(Profits ~ Employees + Assets + Market_Value + Stockholders_Equity + Total_Return_to_Investors_1997)
summary(fit_norev)

car::vif(fit_norev)

fit_opti <- lm(Profits ~ Market_Value + Stockholders_Equity)
summary(fit_opti)

## Alternative to C
mynull <- lm(Profits ~ 1)

# Backward stepwise selection (using AIC)

step(fit, scope=list(lower=mynull, upper=fit, direction="backward"))

# Forward stepwise selection (using AIC)

step(mynull, scope=list(lower=mynull, upper=fit, direction="forward"))
