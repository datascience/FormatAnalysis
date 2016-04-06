library(minpack.lm)
library(propagate)

estimateModelParameters <- function(pData, propertyToTake, start, end) {

  options( warn = -1 )
  pData <- pData[pData$year >= start & pData$year<=end,]
  
  #extract unique values 
  unig <- unique(pData$ID)
  

  modelEstimates <- data.frame(matrix(vector(),0,16+length(propertyToTake), 
                                      dimnames = list(c(),c("ID", "name", propertyToTake, "release.year", 
                                                            "ages", "percentages", "averages",
                                                            "p", "q", "m", "prediction", "residual", "interval", 
                                                            "model", "upper", "lower", "derv"))))

  k <- 1
  for (i in unig) {
    data <- pData[pData$ID==i,]
    print(paste("Estimating and fitting the model to ", data[1,]$name))
    X <- data$age
    Yper <- data$percentage
    Yavg <- data$average
    year <- data$year
    
    #model estimation
    pInterval <- c(0.01, 0.03, 0.05, 0.07, 0.1, 0.2, 0.3)
    qInterval <- c(0.01, 0.05, 0.1, 0.3, 0.5, 0.7, 1.0)
    mInterval <- c(0.01, 0.1, 1, 5, 10, 50, 100)
    model <- NA
    tempRes <- Inf
    for (iP in pInterval) {
      for (iQ in qInterval) {
        for (iM in mInterval) {
          #print(paste(iP,iQ,iM, sep = " "))
          t <- try(mod <- estimateBass(X,Yavg, iP, iQ, iM))
          if("try-error" %in% class(t)) {
            print("error in estimating the model")
            next
          }
          
          p <- as.numeric(coef(mod)["p"])
          q <- as.numeric(coef(mod)["q"])
          m <- as.numeric(coef(mod)["m"])
          
          if ((m*(p+q)^2)/(4*q) <= 1) {
            sumRes <- sum(resid(mod)^2)
            if (sumRes < tempRes) {
              model <- mod
              tempRes <- sumRes
            }
          }
          
        }
      }
    }
    
    
    modelEstimates[k,]$ID <- data[1,]$ID
    modelEstimates[k,]$name <- data[1,]$name
    for (prop in propertyToTake) {
      modelEstimates[k,][[prop]] <- data[1,][[prop]]
    }
    modelEstimates[k,]$release.year <- data[1,]$release.year
    modelEstimates[k,]$ages <- list(X)
    modelEstimates[k,]$percentages <- list(Yper)
    modelEstimates[k,]$averages <- list(Yavg)
    
    if (is.na(model)) {
    print("Model is NA")
      modelEstimates[k,]$p <- NA
      modelEstimates[k,]$q <- NA
      modelEstimates[k,]$m <- NA
      modelEstimates[k,]$prediction <- NA
      modelEstimates[k,]$residual <- NA
      modelEstimates[k,]$interval <- NA
      modelEstimates[k,]$model <- NA
      modelEstimates[k,]$upper <- NA
      modelEstimates[k,]$lower <- NA
      modelEstimates[k,]$derv <- NA
    } 
    else {
      
    
      #print(summary(model))
      f <- data.frame(x=seq(0,30, len=200))
      #t <- try(temp <- predictNLS(model, newdata=f, interval='none'))
      t <- try(temp <- predictNLS(model, newdata=f, interval="confidence", alpha=0.05))
      if("try-error" %in% class(t)) {
        print("error in predictNLS 1")
        modelEstimates[k,]$p <- NA
        modelEstimates[k,]$q <- NA
        modelEstimates[k,]$m <- NA
        modelEstimates[k,]$prediction <- NA
        modelEstimates[k,]$residual <- NA
        modelEstimates[k,]$interval <- NA
        modelEstimates[k,]$model <- NA
        modelEstimates[k,]$upper <- NA
        modelEstimates[k,]$lower <- NA
        modelEstimates[k,]$derv <- NA
        k <- k + 1
        next
      } 
      
      r <- data.frame(x=X)
      t <- try(tempRes <- predictNLS(model, newdata=r, interval="confidence", alpha=0.05))
      if("try-error" %in% class(t)) {
        print("error in predictNLS 2")
      } 
      residual <- Yper - tempRes$summary[,1] 
      
  
      modelEstimates[k,]$p <- coef(model)["p"]
      p <- as.numeric(modelEstimates[k,]$p)
      modelEstimates[k,]$q <- coef(model)["q"]
      q <- as.numeric(modelEstimates[k,]$q)
      modelEstimates[k,]$m <- coef(model)["m"]
      m <- as.numeric(modelEstimates[k,]$m)
      modelEstimates[k,]$prediction <- list(tempRes$summary[,1])
      modelEstimates[k,]$residual <- list(residual)
      modelEstimates[k,]$interval <- list(f$x)
      modelEstimates[k,]$model <- list(temp$summary[,1])
      modelEstimates[k,]$upper <- list(temp$summary[,5])
      modelEstimates[k,]$lower <- list(temp$summary[,6])
      f$derv <- ((m/p)*(p+q)^3*exp(-(p+q)*f$x)*((q/p)*exp(-(p+q)*f$x)-1))/(((q/p)*exp(-(p+q)*f$x)+1)^3)
      modelEstimates[k,]$derv <- list(f$derv)
    }
    
    k <- k + 1
  
  }
  options( warn = 0 )
  return (modelEstimates)
}


# not used 
estimate <- function(X,Y) {
  f <- data.frame(x=X, y=Y)
  fit <- lm(y ~ 1 + I(x) + I(x^2) + I(x^3) + I(x^4) , data=f)
  return (fit)
}

estimateBass <- function(X,Y, sP, sQ, sM) {
  f <- data.frame(x=X, y=Y)
  fit <- nlsLM(y ~ m*((p+q)^2/p)*((exp(-(p+q)*x))/(1+(q/p)*exp(-(p+q)*x))^2), data=f, start=list(p=sP,q=sQ,m=sM))
  return (fit)
}