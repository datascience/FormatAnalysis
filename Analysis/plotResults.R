
library(minpack.lm)
library(propagate)

plotResults <- function(pData, propertyToTake, start, end) {
 
  options( warn = -1 )
  pData <- pData[pData$year >= start & pData$year<=end,]
 
  
  if (is.na(propertyToTake)) {
    unig <- aggregate(.~mime, FUN=length, data=pData)
  }else {
    unig <- aggregate(as.formula(paste(".~", paste(c("mime", propertyToTake), collapse="+"))), 
                      FUN=length, data=pData)
  }
  modelEstimates <- data.frame(properties=character(), p=character(), q=character(), m=character(), residual=character(), stringsAsFactors=FALSE)
  for (i in (1:nrow(unig))) {
    if (is.na(propertyToTake)) {
      X <- pData[pData$mime == unig[i,]$mime,]$age
      Yper <- pData[pData$mime == unig[i,]$mime,]$percentage
      Yavg <- pData[pData$mime == unig[i,]$mime,]$average
      year <- pData[pData$mime == unig[i,]$mime,]$year
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
      year <- data$year
      
#       X <- pData[pData[["mime"]]==unig[i,]$mime & pData[[propertyToTake]]==unig[i,][[propertyToTake]],]$age
#       Yper <- pData[pData[["mime"]]==unig[i,]$mime & pData[[propertyToTake]]==unig[i,][[propertyToTake]],]$percentage
#       Yavg <- pData[pData[["mime"]]==unig[i,]$mime & pData[[propertyToTake]]==unig[i,][[propertyToTake]],]$average
      
        title <- paste(unig[i,][names(unig) %in% c("mime",propertyToTake)], collapse=" ")
#       model <- NA
#       if (!is.na(modelData)) {
#         model <- modelData[modelData[["mime"]]==unig[i,]$mime & modelData[[propertyTotake]]==unig[i,][[propertyToTake]],]$model
#       }
    }
    print("estimating")
    model <- estimateBass(X,Yavg)
    modelEstimates[i,]$properties <- title 
    modelEstimates[i,]$p <- coef(model)["p"]
    modelEstimates[i,]$q <- coef(model)["q"]
    modelEstimates[i,]$m <- coef(model)["m"]
    modelEstimates[i,]$residual <- sum(residuals(model)^2)
    releaseYear <- year[1]-X[1]
    drawPlot(X,Yper, Yavg, title, model, releaseYear)
  }
  return (modelEstimates)

options( warn = 0 )
}


drawPlot <- function(X,Yper, Yavg, title, model, releaseYear) {
  #plot(X,Yper, main=title, xlim=c(0,20), ylim=c(0,max(Yper)+0.1*max(Yper)),xlab="age", ylab="adoption")
  #points(X,Yavg, pch=19)
#   lo <- loess(Yper ~ X)
#   X1 <- seq(min(X),max(X), (max(X) - min(X))/1000)
#   lines(X1, predict(lo, X1),col='red')
  if (!is.na(model)) {
    f <- data.frame(x=seq(0,20, len=200))
    #print(model)
    #f <- data.frame(x=seq(min(X),max(X), len=200))
    temp <- predictNLS(model, newdata=f, interval="prediction", alpha=0.01)
    #print(temp$summary[,1])
    #f$y <- predict(model, newdata=f)
    f$y <- temp$summary[,1]
    f$lwr <- temp$summary[,5]
    f$upr <- temp$summary[,6]
    plot(NULL, main=title, xlim=c(0,20), ylim=c(min(f$lwr)-0.1*abs(min(f$lwr)), max(f$upr)+0.1*max(f$upr)),xlab="age", ylab="adoption")
    polygon(c(f$x,rev(f$x)), c(f$lwr,rev(f$upr)), col='grey96', border=NA)
    points(X,Yper, pch=17)
    points(X,Yavg, pch=19)
    lines(f$x,f$y)
    lines(f$x,f$lwr, lty=2)
    lines(f$x,f$upr, lty=2)
    title <- gsub("/","-",title)
    
    f <- data.frame(x=seq(0,20))
    temp <- predictNLS(model, newdata=f, interval="prediction", alpha=0.01)
    f$year <- f$x + releaseYear
    f$y <- temp$summary[,1]
    f$lwr <- temp$summary[,5]
    f$upr <- temp$summary[,6]
    write.table(f, file=paste("output data/", paste(title,"_predictions.csv"), sep=""), quote=FALSE, sep="\t", col.names=TRUE, row.names=FALSE)
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
