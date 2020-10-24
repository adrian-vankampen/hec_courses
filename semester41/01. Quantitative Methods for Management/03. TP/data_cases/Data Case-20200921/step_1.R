#---------------------------------------------------------------
###--------------DATA CASE--------------------------------------
#---------------------------------------------------------------

setwd("C:/Users/Adrian van Kampen/Documents/GitHub/QMM2020/data_cases/Data Case-20200921")

# Libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)

#Options
options(stringsAsFactors = FALSE)

ship1 <- read.csv("ship1.csv")
attach(ship1)

ship1["Date"] <- as.Date(Date)


# a) Compute mean and standard error of each variable (except Date)

std <- function(x) sd(x)/sqrt(length(x))

avg_price = mean(SellingPrice)
se_price =std(SellingPrice)

avg_age = mean(VesselAge)
se_age = std(VesselAge)

avg_dwt = mean(Dwt)
se_dwt = std(Dwt)

avg_freight = mean(Freight)
se_freight = std(Freight)

# b) Plot Freight over time


ggplot(data = ship1, mapping = aes(x = as.Date(Date), y = Freight)) + geom_line()

# c) Study the evolution of annual means of each variables (except Date... duh)
first_year <- as.numeric(format(as.Date(min(Date), format="%d/%m/%Y"),"%Y"))
last_year <- as.numeric(format(as.Date(max(Date), format="%d/%m/%Y"),"%Y"))
  
years<- format(as.Date(Date), format = "%Y")
datayears <- cbind(years, ship1)

annualmeantable <- aggregate(datayears, by=list(years), FUN=mean)
annualmeantable <- annualmeantable[-c(2,3)]
colnames(annualmeantable)[1] <- "Year"

ggplot(data = annualmeantable, mapping = aes(x=Year, y=SellingPrice)) + geom_point()
ggplot(data = annualmeantable, mapping = aes(x=Year, y=VesselAge)) + geom_point()
ggplot(data = annualmeantable, mapping = aes(x=Year, y=Dwt)) + geom_point()
ggplot(data = annualmeantable, mapping = aes(x=Year, y=Freight)) + geom_point()


# d) Calculate the correlation matrix of the variables

cor_matrix <- round(cor(ship1[, c(2,3,4,5)]),2)
print(cor_matrix)