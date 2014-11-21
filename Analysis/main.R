source('loadData.R')
source('config.R')
source('utils.R')
data <- loadData(file, colnames, propertyToTake)


t <- data[data$version=="1.3",]
t <- t[t$age >= 0,]
years <- t$year
sSum <- lapply(years, ">=", t$year)
tSum <- lapply(years, ">", t$year)

t$S <- t$amount
#t$S <- unlist(lapply(sSum,mySum, t$amount))
t$T <- unlist(lapply(tSum,mySum, t$amount))
class(t$S)
y <- lm(S~1+I(T)+I(T^2), data=t)
print(summary(y)$coefficients)
a <-summary(y)$coefficients["(Intercept)", "Estimate"]
b <-summary(y)$coefficients["I(T)", "Estimate"]
c <-summary(y)$coefficients["I(T^2)", "Estimate"]


m1 <- (-b + sqrt(b^2-4*c*a))/(2*c)
m2 <- (-b - sqrt(b^2-4*c*a))/(2*c)
ifelse (m1 >m2, m <- m1, m <- m2)
p <- a / m
q <- b + p

t$Sest <- estimateS(p,q,m,11)
plot(t$age,t$S)
lines(t$age, t$Sest)

#move ploting 
#plot(pData[pData$version=="1a",]$age,pData[pData$version=="1a",]$percentage)
