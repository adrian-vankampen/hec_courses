#---------------------------------------------------------------
###--------------DATA CASE--------------------------------------
#---------------------------------------------------------------

setwd("C:/Users/Adrian van Kampen/Documents/GitHub/QMM2020/data_cases/Data Case-20200921")

source("step_1.R")
library(dplyr)
ship1_ss1 <- ship1[c(1,2,3)]

# a) Plot the age of the ship against the Selling price

ggplot(data = ship1_ss1, mapping = aes(x=VesselAge, y=SellingPrice)) + geom_point()

# b) Fit a third-order linear regression model to explain price with the age of the ship.

reg1 <- lm(SellingPrice ~ VesselAge)
summary(reg1)

plot(VesselAge, SellingPrice)
abline(reg1)

# c) Compute estimated price for vessels aged 0 to 35 and report relative annual variation of estimated prices.

years<- format(as.Date(Date), format = "%Y")
datapriceyears <- cbind(years, ship1_ss1)[-2]

datapriceyears <- filter(datapriceyears, VesselAge<=35)
datapriceyears <- filter(datapriceyears, VesselAge>=0)

annual_mean_price <- aggregate(SellingPrice, by=list(years), FUN=mean)
plot(annual_mean_price, type= "l")

# d)

df_ship1 <- as.data.frame.matrix(ship1)

exponential.model <- lm(log(SellingPrice) ~ VesselAge)
summary(exponential.model)

# e)

ship2 <- as.data.frame(read.csv("ship2.csv"))

exponential.model2 <- lm(log(ship2$SellingPrice) ~ ship2$VesselAge)
summary(exponential.model2)

plot(ship2)

years2<- format(as.Date(ship2$Date), format = "%Y")
ship2 <- cbind(years2, ship2)

annual_mean_price2 <- aggregate(ship2$SellingPrice, by=list(ship2$years2), FUN=mean)
plot(annual_mean_price, type= "l")

# f) Compute the log of the prices and the log of estimations

ship2 <- cbind(ship2, log(ship2$SellingPrice))
colnames(ship2)[7] <- "LogPrices"

ggplot(data = ship2, mapping = aes(x = Date, y = LogPrices)) + geom_point()
