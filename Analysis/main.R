source('loadData.R')
source('config.R')
source('utils.R')
data <- loadData(file, colnames, propertyToTake)


t <- data[data$version=="1.3",]
t <- t[t$age >= 0,]
years <- t$year
sSum <- lapply(years, ">=", t$year)
tSum <- lapply(years, ">", t$year)

t$S <- unlist(lapply(sSum,mySum, t$amount))
t$T <- unlist(lapply(tSum,mySum, t$amount))
class(t$S)
y <- lm(S~1+I(T)+I(T^2), data=t)
print(y)


#move ploting 
#plot(pData[pData$version=="1a",]$age,pData[pData$version=="1a",]$percentage)
