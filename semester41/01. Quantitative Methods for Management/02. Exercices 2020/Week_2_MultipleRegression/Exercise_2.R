#-------------------------------------------------------------------------------
#============= WEEK 2 - EXERCISE 2 =============================================
#-------------------------------------------------------------------------------
setwd("~/GitHub/QMM2020/Week_2")

df_RE <- as.data.frame(read.csv("RealEstate.csv"))

# a.

reg1 <- lm(df_RE$Price ~ df_RE$Type + df_RE$Square.feet)
lm1 <- summary(reg1)

# R automatically converts chr type variables to booleans IF there are only 2 possible values.
# Thus, if a house is a family home, it is worth 3629.50 £ more.
# Every square feet of house area is worht 90.37 £

# b.

# b2 is significant at alpha = 1 %, thus it is also significant at alpha = 5 %.
# b1, on the other hand, is not significant.

# c.
reg2 <- lm(df_RE$Price ~ df_RE$Type + df_RE$Square.feet + df_RE$Type*df_RE$Square.feet)
lm2 <- summary(reg2)

# d. Single-family home of 1750 sq. ft.
sq.ft <- 1750L
isFamily <- TRUE
price1 <- coef(reg1)[1] + coef(reg1)[2]*isFamily + coef(reg1)[3]*sq.ft
price2 <- coef(reg2)[1] + coef(reg2)[2]*isFamily + coef(reg2)[3]*sq.ft + coef(reg2)[4]*isFamily*sq.ft

# e. Polynomial model of degree 4
set.seed(18)
q <- seq(from=0, to=4999, by=1)

reg3 <- lm(df_RE$Price ~ df_RE$Square.feet + I(df_RE$Square.feet^2) + I(df_RE$Square.feet^3) + I(df_RE$Square.feet^4))
lm3 <- summary(reg3)

price3 <- coef(reg3)[1] + coef(reg3)[2]*sq.ft + coef(reg3)[3]*I(sq.ft^2) + coef(reg3)[4]*I(sq.ft^3) + coef(reg3)[5]*I(sq.ft^4)
pred3 <- coef(reg3)[1] + coef(reg3)[2]*q + coef(reg3)[3]*I(q^2) + coef(reg3)[4]*I(q^3) + coef(reg3)[5]*I(q^4)
plot(x=q, y=pred3, xlab="Square feet", ylab="Price")

# f.

reg0 <- lm(df_RE$Price ~ 1)
reg4 <- step(reg3, scope = list(lower=reg0, upper=reg3), direction="backward")

price4 <- coef(reg4)[1] + coef(reg4)[2]*I(sq.ft^4)

pred4 <- coef(reg4)[1] + coef(reg4)[2]*I(q^4)
plot(x=q, y=pred4, xlab="Square Feet", ylab="Price", type="h")

# g.

# Square feet and meters are perfectly correlated, thus it would be useless to add it to the regression.

# h.

df_RE$Square.meters <- df_RE$Square.feet / 10.7639
regm <- lm(df_RE$Price ~ df_RE$Type + df_RE$Square.meters)
lmm <- summary(regm)

coefm_theo <- coef(reg1)[3] * 10.7639
coefm <- coef(regm)[3]
print(list(c(coefm_theo, coefm), "They are exactly the same, duh!"))