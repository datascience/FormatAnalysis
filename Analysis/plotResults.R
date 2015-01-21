
library(minpack.lm)

plotResults <- function(pData, modelData, propertyToTake) {
  
  if (is.na(propertyToTake)) {
    unig <- aggregate(.~mime, FUN=length, data=pData)
  }else {
    unig <- aggregate(as.formula(paste(".~", paste(c("mime", propertyToTake), collapse="+"))), 
                      FUN=length, data=pData)
  }
  
  for (i in (1:nrow(unig))) {
    if (is.na(propertyToTake)) {
      X <- pData[pData$mime == unig[i,]$mime,]$age
      Yper <- pData[pData$mime == unig[i,]$mime,]$percentage
      Yavg <- pData[pData$mime == unig[i,]$mime,]$average
      title <- paste(unig[i,]$mime," ", sep="")
#       model <- estimate(X,Y)
#       if (!is.na(modelData)) {
#         model <- modelData[modelData$mime==unig[i,]$mime,]$model
#       }
    }else {
      data <- merge(pData, unig[i,][names(pData) %in% c("mime",propertyToTake)], by=c("mime",propertyToTake))
      X <- data$age
      Yper <- data$percentage
      Yavg <- data$average
      
#       X <- pData[pData[["mime"]]==unig[i,]$mime & pData[[propertyToTake]]==unig[i,][[propertyToTake]],]$age
#       Yper <- pData[pData[["mime"]]==unig[i,]$mime & pData[[propertyToTake]]==unig[i,][[propertyToTake]],]$percentage
#       Yavg <- pData[pData[["mime"]]==unig[i,]$mime & pData[[propertyToTake]]==unig[i,][[propertyToTake]],]$average
      
        title <- paste(unig[i,][names(unig) %in% c("mime",propertyToTake)], collapse=" ")
#       model <- NA
#       if (!is.na(modelData)) {
#         model <- modelData[modelData[["mime"]]==unig[i,]$mime & modelData[[propertyTotake]]==unig[i,][[propertyToTake]],]$model
#       }
    }
    model <- estimateBass(X,Yavg)
    drawPlot(X,Yper, Yavg, title, model)
  }
  
}


drawPlot <- function(X,Yper, Yavg, title, model) {
  plot(X,Yper, main=title, xlim=c(0,20), xlab="age", ylab="percentage")
  points(X,Yavg, pch=19)
#   lo <- loess(Yper ~ X)
#   X1 <- seq(min(X),max(X), (max(X) - min(X))/1000)
#   lines(X1, predict(lo, X1),col='red')
  if (!is.na(model)) {
    f <- data.frame(x=seq(0,20, len=200))
    #print(model)
    #f <- data.frame(x=seq(min(X),max(X), len=200))
    f$y <- predict(model, newdata=f)
    lines(f$x,f$y)
  }
}

estimate <- function(X,Y) {
  f <- data.frame(x=X, y=Y)
  fit <- lm(y ~ 1 + I(x) + I(x^2) + I(x^3) + I(x^4) , data=f)
  return (fit)
}

estimateBass <- function(X,Y) {
  f <- data.frame(x=X, y=Y)
  fit <- nlsLM(y ~ m*((p+q)^2/p)*((exp(-(p+q)*x))/(1+(q/p)*exp(-(p+q)*x))^2), data=f, start=list(p=0.03,q=0.3,m=1))
  return (fit)
}
