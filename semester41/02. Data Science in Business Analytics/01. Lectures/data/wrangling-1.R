#-----------------------
### Exercise 1
#-----------------------


USComp <- read.csv("01. USComp.csv")
attach(USComp)
n <- 50

#A

plot(Employees, Profits)

Answer_A <- "The plot does not seem to show any correlation between profits and number of employees, or perhaps a slight positive correlation."

#B

corr <- cor(Employees, Profits)

t_value <- corr / sqrt((1-corr^2)/(n-2))

alpha_t <- pt(t_value, df=48, lower.tail=FALSE)*2

Answer_B <- "The correlation is not statistically significative to a 5% size (alpha_t = 0.0094 < 0.05)."

#C

lm.out <- lm(Profits ~ Employees)

summary(lm.out)
b0 <- coef(summary(lm.out))["(Intercept)","Estimate"]
b1 <- coef(summary(lm.out))["Employees","Estimate"]
s1 <- coef(summary(lm.out))["Employees", "Std. Error"]

abline(lm.out)

Answer_C_1 <- "Profits_i = 2557 + 0.0047*Employees_i + epsilon_i"
Answer_C_2 <- "A company with 0 employees would make 2557 units of profit according to this model."
Answer_C_3 <- "Every additional employee that the company hires adds 0.0047 units of profit according to this model."
Answer_C_4 <- "The slope coefficient is significant at alpha = 1% (showed by the ** in the summary)"

#D

Profits_predicted <- c(coef(summary(lm.out))["(Intercept)","Estimate"] + coef(summary(lm.out))["Employees","Estimate"]*Employees)
Profits_squared_errors <- c((Profits - Profits_predicted)^2)
SSE <- sum(Profits_squared_errors)

Profits_mean <- mean(Profits)
Profits_squared_dif <- c((Profits - Profits_mean)^2)
SST <- sum(Profits_squared_dif)
SSR1 <- SST - SSE

Profits_squared_regression <- c((Profits_predicted - Profits_mean)^2)
SSR2 <- sum(Profits_squared_regression)

R_squared <- SSR2 / SST
Unexplained <- SSE/SST
Answer_D <- "Using SSR, SSE and SST, we can see that there is a large part of differences in profits that the model does not explain (86.8%)"

#E

Mean_employees <- mean(Employees)
Mean_profits_predicted <- b0 + b1*Mean_employees

CI_Profits_lower <- Mean_profits_predicted - Mean_employees*(1.96*s1)/sqrt(n)
CI_Profits_upper <- Mean_profits_predicted + Mean_employees*(1.96*s1)/sqrt(n)

#F

Answer_F <- "No, this indicator does not seem to be a good indicator for profits. We can try to regress Profits on each type of other data that we have:"

LM_Assets <- lm(Profits ~ Assets)
plot(Assets, Profits)
abline(LM_Assets)

Answer_F_2 <- "Assets does not seem to correlate with profits either."

LM_earningsps <- lm(Profits ~ Earnings_per_share)
plot(Earnings_per_share, Profits)
abline(LM_earningsps)

summary(LM_earningsps)

Answer_F_3 <- "Earnings per share neither..."

LM_marketv <- lm(Profits ~ Market_Value)
plot(Market_Value, Profits)
abline(LM_marketv)

summary(LM_marketv)

Answer_F_4 <- "Market value however seems to correlate pretty well with profits, which seems logical."


LM_revenue <- lm(Profits ~ Revenues)
plot(Revenues, Profits)
abline(LM_revenue)

summary(LM_revenue)

Answer_F_5 <- "As do revenues..."

LM_stockeq <- lm(Profits ~ Stockholders_Equity)
plot(Stockholders_Equity, Profits)
abline(LM_stockeq)

summary(LM_stockeq)

Answer_F_6 <- "and stockholder equity"


LM_return97 <- lm(Profits ~ Total_Return_to_Investors_1997)
plot(Total_Return_to_Investors_1997, Profits)
abline(LM_return97)

summary(LM_return97)

Answer_F_7 <- "Finally, the 1997 record for return to investors does not seem to be a good indicator."