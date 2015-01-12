
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
      X <- pData[pData[["mime"]]==unig[i,]$mime & pData[[propertyToTake]]==unig[i,][[propertyToTake]],]$age
      Yper <- pData[pData[["mime"]]==unig[i,]$mime & pData[[propertyToTake]]==unig[i,][[propertyToTake]],]$percentage
      Yavg <- pData[pData[["mime"]]==unig[i,]$mime & pData[[propertyToTake]]==unig[i,][[propertyToTake]],]$average
      title <- paste(unig[i,]$mime, unig[i,][[propertyToTake]], sep=" ")
#       model <- NA
#       if (!is.na(modelData)) {
#         model <- modelData[modelData[["mime"]]==unig[i,]$mime & modelData[[propertyTotake]]==unig[i,][[propertyToTake]],]$model
#       }
    }
    model <- estimate(X,Yavg)
    drawPlot(X,Yper, Yavg, title, model)
  }
  
}


drawPlot <- function(X,Yper, Yavg, title, model) {
  plot(X,Yper, main=title)
  points(X,Yavg, pch=19)
#   lo <- loess(Yper ~ X)
#   X1 <- seq(min(X),max(X), (max(X) - min(X))/1000)
#   lines(X1, predict(lo, X1),col='red')
  if (!is.na(model)) {
    f <- data.frame(x=seq(min(X),max(X), len=200))
    f$y <- predict(model, newdata=f)
    lines(f$x,f$y)
  }
}

estimate <- function(X,Y) {
  f <- data.frame(x=X, y=Y)
  fit <- lm(y ~ 1 + I(x) + I(x^2) + I(x^3) + I(x^4) , data=f)
  return (fit)
}

