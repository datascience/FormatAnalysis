library(minpack.lm)
library(propagate)

estimateModelParameters <- function(pData, propertyToTake, start, end, intervalType, alphaInterval, useMovingAverage) {

  options( warn = -1 )
  #pData <- pData[pData$year >= start & pData$year<=end,]
  
  #extract unique values 
  unig <- unique(pData$ID)
  

  modelEstimates <- data.frame(matrix(vector(),0,6 + length(propertyToTake), 
                                      dimnames = list(c(),c("ID", "name", propertyToTake, "release.year", 
                                                            "ages", "percentages", "averages"))))

  k <- 1
  for (i in unig) {
    data <- pData[pData$ID==i,]
    print(paste("Estimating and fitting the model to ", data[1,]$name))
    
    #take the data needed for doing the estimation
    X <- data[data$year >= start & data$year<=end,]$age
    Xper <- data[!(data$year >= start & data$year<=end),]$age
    Yper <- data[data$year >= start & data$year<=end,]$percentage
    Yavg <- data[data$year >= start & data$year<=end,]$average
    year <- data[data$year >= start & data$year<=end,]$year
    
    if (useMovingAverage == TRUE) {
      Y <- Yavg
    } else {
      Y <- Yper
    }
    
    # save common data 
    modelEstimates[k,][["ID"]] <- data[1,"ID"]
    modelEstimates[k,][["name"]] <- data[1,"name"]
    for (prop in propertyToTake) {
      modelEstimates[k,][[prop]] <- data[1,][[prop]]
    }
    modelEstimates[k,][["release.year"]] <- data[1,][["release.year"]]
    modelEstimates[k,][["ages"]] <- list(data$age)
    modelEstimates[k,][["percentages"]] <- list(data$percentage)
    modelEstimates[k,][["averages"]] <- list(data$average)
    
    if (nrow(data[data$year==end+1,])==0) {
      modelEstimates[k,"real-next"] <- NA
    }else {
      modelEstimates[k,"real-next"] <- data[data$year==end+1,]$percentage 
    }
    
    modelEstimates <- estimateLinear1Model(modelEstimates, X, Y, Yper, k, Xper, intervalType, alphaInterval)
    
    modelEstimates <- estimateLinear2Model(modelEstimates, X, Y, Yper, k, Xper, intervalType, alphaInterval)
    
    modelEstimates <- estimateLinear3Model(modelEstimates, X, Y, Yper, k, Xper, intervalType, alphaInterval)
    
    modelEstimates <- estimateBassModel(modelEstimates, X, Y, Yper, k, Xper, intervalType, alphaInterval)
    
    
    
    
    k <- k + 1
  
  }
  options( warn = 0 )
  return (modelEstimates)
}


estimateLinear1Model <- function(modelEstimates, X, Y, Yreal, k, predYear, intervalType, alphaInterval) {
  t <- try(model <- estimateLinear1(X,Y))
  if("try-error" %in% class(t)) {
    print("error in estimating the linear1 model")
  }
  f <- data.frame(x=seq(0,30, len=200))
  temp <- predict(model, newdata=f, interval=intervalType, alpha=alphaInterval)
  r <- data.frame(x=X)
  tempRes <- predict(model, newdata=r, interval=intervalType, alpha=alphaInterval)
  r <- data.frame(x=predYear)
  predRes <- predict(model, newdata=r, interval="prediction", alpha=alphaInterval)
  
  residual <- Yreal - tempRes[,1]
  modelEstimates[k,"a.linear1"] <- coef(model)["(Intercept)"]
  modelEstimates[k,"b.linear1"] <- coef(model)["I(x)"]
  b <- as.numeric(modelEstimates[k,"b.linear1"])

  #This part is a hack that should be improved eventaully
  modelEstimates[k,"prediction.linear1"] <- list(tempRes[,1])
  modelEstimates[k,][["prediction.linear1"]] <- list(tempRes[,1])
  modelEstimates[k,"residual.linear1"] <- list(residual)
  modelEstimates[k,][["residual.linear1"]] <- list(residual)
  modelEstimates[k,"interval.linear1"] <- list(f$x)
  modelEstimates[k,][["interval.linear1"]] <- list(f$x)
  modelEstimates[k,"model.linear1"] <- list(temp[,1])
  modelEstimates[k,][["model.linear1"]] <- list(temp[,1])
  modelEstimates[k,"upper.linear1"] <- list(temp[,3])
  modelEstimates[k,][["upper.linear1"]] <- list(temp[,3])
  modelEstimates[k,"lower.linear1"] <- list(temp[,2])
  modelEstimates[k,][["lower.linear1"]] <- list(temp[,2])
  f$derv <-rep(b,200)
  modelEstimates[k,"derv.linear1"] <- list(f$derv)
  modelEstimates[k,][["derv.linear1"]] <- list(f$derv)
  modelEstimates[k,"prediction-next.linear1"] <- list(predRes[,1])
  modelEstimates[k,][["prediction-next.linear1"]] <- list(predRes[,1])
  modelEstimates[k,"predictionLower-next.linear1"] <- list(predRes[,2])
  modelEstimates[k,][["predictionLower-next.linear1"]] <- list(predRes[,2])
  modelEstimates[k,"predictionUpper-next.linear1"] <- list(predRes[,3])
  modelEstimates[k,][["predictionUpper-next.linear1"]] <- list(predRes[,3])
  modelEstimates[k,"MSE.linear1"] <- calculateMSE(residual)
  modelEstimates[k,"R2.linear1"] <- calculateR2(residual, Yreal)
  return (modelEstimates)
  
}

estimateLinear2Model <- function(modelEstimates, X, Y, Yreal, k, predYear, intervalType, alphaInterval) {
  
  #Linear2 model estimation
  t <- try(model <- estimateLinear2(X,Y))
  if("try-error" %in% class(t)) {
    print("error in estimating the linear2 model")
  }
  f <- data.frame(x=seq(0,30, len=200))
  temp <- predict(model, newdata=f, interval=intervalType, alpha=alphaInterval)
  r <- data.frame(x=X)
  tempRes <- predict(model, newdata=r, interval=intervalType, alpha=alphaInterval)
  r <- data.frame(x=predYear)
  predRes <- predict(model, newdata=r, interval="prediction", alpha=alphaInterval)
  
  residual <- Yreal - tempRes[,1]
  modelEstimates[k,"a.linear2"] <- coef(model)["(Intercept)"]
  a <- as.numeric(modelEstimates[k,"a.linear2"])
  modelEstimates[k,"b.linear2"] <- coef(model)["I(x)"]
  b <- as.numeric(modelEstimates[k,"b.linear2"])
  modelEstimates[k,"c.linear2"] <- coef(model)["I(x^2)"]
  c <- as.numeric(modelEstimates[k,"c.linear2"])
  modelEstimates[k,"prediction.linear2"] <- list(tempRes[,1])
  modelEstimates[k,][["prediction.linear2"]] <- list(tempRes[,1])
  modelEstimates[k,"residual.linear2"] <- list(residual)
  modelEstimates[k,][["residual.linear2"]] <- list(residual)
  modelEstimates[k,"interval.linear2"] <- list(f$x)
  modelEstimates[k,][["interval.linear2"]] <- list(f$x)
  modelEstimates[k,"model.linear2"] <- list(temp[,1])
  modelEstimates[k,][["model.linear2"]] <- list(temp[,1])
  modelEstimates[k,"upper.linear2"] <- list(temp[,3])
  modelEstimates[k,][["upper.linear2"]] <- list(temp[,3])
  modelEstimates[k,"lower.linear2"] <- list(temp[,2])
  modelEstimates[k,][["lower.linear2"]] <- list(temp[,2])
  f$derv <- b + c*f$x
  modelEstimates[k,"derv.linear2"] <- list(f$derv)
  modelEstimates[k,][["derv.linear2"]] <- list(f$derv)
  modelEstimates[k,"prediction-next.linear2"] <- list(predRes[,1])
  modelEstimates[k,][["prediction-next.linear2"]] <- list(predRes[,1])
  modelEstimates[k,"predictionLower-next.linear2"] <- list(predRes[,2])
  modelEstimates[k,][["predictionLower-next.linear2"]] <- list(predRes[,2])
  modelEstimates[k,"predictionUpper-next.linear2"] <- list(predRes[,3])
  modelEstimates[k,][["predictionUpper-next.linear2"]] <- list(predRes[,3])
  modelEstimates[k,"MSE.linear2"] <- calculateMSE(residual)
  modelEstimates[k,"R2.linear2"] <- calculateR2(residual, Yreal)
  return (modelEstimates)
  
}


estimateLinear3Model <- function(modelEstimates, X, Y, Yreal, k, predYear, intervalType, alphaInterval) {
  
  #Linear2 model estimation
  t <- try(model <- estimateLinear3(X,Y))
  if("try-error" %in% class(t)) {
    print("error in estimating the linear3 model")
  }
  f <- data.frame(x=seq(0,30, len=200))
  temp <- predict(model, newdata=f, interval=intervalType, alpha=alphaInterval)
  r <- data.frame(x=X)
  tempRes <- predict(model, newdata=r, interval=intervalType, alpha=alphaInterval)
  r <- data.frame(x=predYear)
  predRes <- predict(model, newdata=r, interval="prediction", alpha=alphaInterval)
  
  residual <- Yreal - tempRes[,1]
  modelEstimates[k,"a.linear3"] <- coef(model)["(Intercept)"]
  a <- as.numeric(modelEstimates[k,"a.linear3"])
  modelEstimates[k,"b.linear3"] <- coef(model)["I(x)"]
  b <- as.numeric(modelEstimates[k,"b.linear3"])
  modelEstimates[k,"c.linear3"] <- coef(model)["I(x^2)"]
  c <- as.numeric(modelEstimates[k,"c.linear3"])
  modelEstimates[k,"d.linear3"] <- coef(model)["I(x^3)"]
  d <- as.numeric(modelEstimates[k,"d.linear3"])
  modelEstimates[k,"prediction.linear3"] <- list(tempRes[,1])
  modelEstimates[k,][["prediction.linear3"]] <- list(tempRes[,1])
  modelEstimates[k,"residual.linear3"] <- list(residual)
  modelEstimates[k,][["residual.linear3"]] <- list(residual)
  modelEstimates[k,"interval.linear3"] <- list(f$x)
  modelEstimates[k,][["interval.linear3"]] <- list(f$x)
  modelEstimates[k,"model.linear3"] <- list(temp[,1])
  modelEstimates[k,][["model.linear3"]] <- list(temp[,1])
  modelEstimates[k,"upper.linear3"] <- list(temp[,3])
  modelEstimates[k,][["upper.linear3"]] <- list(temp[,3])
  modelEstimates[k,"lower.linear3"] <- list(temp[,2])
  modelEstimates[k,][["lower.linear3"]] <- list(temp[,2])
  f$derv <- b + c*f$x + d*(f$x)^2
  modelEstimates[k,"derv.linear3"] <- list(f$derv)
  modelEstimates[k,][["derv.linear3"]] <- list(f$derv)
  modelEstimates[k,"prediction-next.linear3"] <- list(predRes[,1])
  modelEstimates[k,][["prediction-next.linear3"]] <- list(predRes[,1])
  modelEstimates[k,"predictionLower-next.linear3"] <- list(predRes[,2])
  modelEstimates[k,][["predictionLower-next.linear3"]] <- list(predRes[,2])
  modelEstimates[k,"predictionUpper-next.linear3"] <- list(predRes[,3])
  modelEstimates[k,][["predictionUpper-next.linear3"]] <- list(predRes[,3])
  modelEstimates[k,"MSE.linear3"] <- calculateMSE(residual)
  modelEstimates[k,"R2.linear3"] <- calculateR2(residual, Yreal)
  return (modelEstimates)
  
}



estimateBassModel <- function(modelEstimates, X, Y, Yreal, k, predYear, intervalType, alphaInterval) {
  
  #Bass model estimation
  pInterval <- c(0.01, 0.03, 0.05, 0.07, 0.1, 0.2, 0.3)
  qInterval <- c(0.01, 0.05, 0.1, 0.3, 0.5, 0.7, 1.0)
  mInterval <- c(0.01, 0.1, 1, 5, 10, 50, 100)
  model <- NA
  tempRes <- Inf
  for (iP in pInterval) {
    for (iQ in qInterval) {
      for (iM in mInterval) {
        #print(paste(iP,iQ,iM, sep = " "))
        t <- try(mod <- estimateBass(X,Y, iP, iQ, iM))
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
  
  if (is.na(model)) {
    print("Model is NA")
    modelEstimates[k,"p.bass"] <- NA
    modelEstimates[k,"q.bass"] <- NA
    modelEstimates[k,"m.bass"] <- NA
    modelEstimates[k,"prediction.bass"] <- NA
    modelEstimates[k,"residual.bass"] <- NA
    modelEstimates[k,"interval.bass"] <- NA
    modelEstimates[k,"model.bass"] <- NA
    modelEstimates[k,"upper.bass"] <- NA
    modelEstimates[k,"lower.bass"] <- NA
    modelEstimates[k,"derv.bass"] <- NA
    modelEstimates[k,"prediction-next.bass"] <- NA
    modelEstimates[k,"predictionLower-next.bass"] <- NA
    modelEstimates[k,"predictionUpper-next.bass"] <- NA
    modelEstimates[k,"MSE.bass"] <- NA
    modelEstimates[k,"R2.bass"] <- NA
  }  else {
    
    
    #print(summary(model))
    f <- data.frame(x=seq(0,30, len=200))
    #t <- try(temp <- predictNLS(model, newdata=f, interval='none'))
    t <- try(temp <- predictNLS(model, newdata=f, interval=intervalType, alpha=alphaInterval))
    if("try-error" %in% class(t)) {
      print("error in predictNLS 1")
      modelEstimates[k,"p.bass"] <- NA
      modelEstimates[k,"q.bass"] <- NA
      modelEstimates[k,"m.bass"] <- NA
      modelEstimates[k,"prediction.bass"] <- NA
      modelEstimates[k,"residual.bass"] <- NA
      modelEstimates[k,"interval.bass"] <- NA
      modelEstimates[k,"model.bass"] <- NA
      modelEstimates[k,"upper.bass"] <- NA
      modelEstimates[k,"lower.bass"] <- NA
      modelEstimates[k,"derv.bass"] <- NA
      modelEstimates[k,"prediction-next.bass"] <- NA
      modelEstimates[k,"predictionLower-next.bass"] <- NA
      modelEstimates[k,"predictionUpper-next.bass"] <- NA
      modelEstimates[k,"MSE.bass"] <- NA
      modelEstimates[k,"R2.bass"] <- NA
    } else {
      
      r <- data.frame(x=X)
      t <- try(tempRes <- predictNLS(model, newdata=r, interval=intervalType, alpha=alphaInterval))
      if("try-error" %in% class(t)) {
        print("error in predictNLS 2")
      } 
      r <- data.frame(x=predYear)
      t <- try(predRes <- predictNLS(model, newdata=r, interval="prediction", alpha=alphaInterval))
      if("try-error" %in% class(t)) {
        print("error in predictNLS 2")
      }
      
      residual <- Yreal - tempRes$summary[,1] 
      
      
      modelEstimates[k,"p.bass"] <- coef(model)["p"]
      p <- as.numeric(modelEstimates[k,"p.bass"])
      modelEstimates[k,"q.bass"] <- coef(model)["q"]
      q <- as.numeric(modelEstimates[k,"q.bass"])
      modelEstimates[k,"m.bass"] <- coef(model)["m"]
      m <- as.numeric(modelEstimates[k,"m.bass"])
      modelEstimates[k,"prediction.bass"] <- list(tempRes$summary[,1])
      modelEstimates[k,][["prediction.bass"]] <- list(tempRes$summary[,1])
      modelEstimates[k,"residual.bass"] <- list(residual)
      modelEstimates[k,][["residual.bass"]] <- list(residual)
      modelEstimates[k,"interval.bass"] <- list(f$x)
      modelEstimates[k,][["interval.bass"]] <- list(f$x)
      modelEstimates[k,"model.bass"] <- list(temp$summary[,1])
      modelEstimates[k,][["model.bass"]] <- list(temp$summary[,1])
      modelEstimates[k,"upper.bass"] <- list(temp$summary[,6])
      modelEstimates[k,][["upper.bass"]] <- list(temp$summary[,6])
      modelEstimates[k,"lower.bass"] <- list(temp$summary[,5])
      modelEstimates[k,][["lower.bass"]] <- list(temp$summary[,5])
      f$derv <- ((m/p)*(p+q)^3*exp(-(p+q)*f$x)*((q/p)*exp(-(p+q)*f$x)-1))/(((q/p)*exp(-(p+q)*f$x)+1)^3)
      modelEstimates[k,"derv.bass"] <- list(f$derv)
      modelEstimates[k,][["derv.bass"]] <- list(f$derv)
      modelEstimates[k,"prediction-next.bass"] <- list(predRes$summary[,1][1])
      modelEstimates[k,][["prediction-next.bass"]] <- list(predRes$summary[,1][1])
      modelEstimates[k,"predictionLower-next.bass"] <- list(predRes$summary[,5][1])
      modelEstimates[k,][["predictionLower-next.bass"]] <- list(predRes$summary[,5][1])
      modelEstimates[k,"predictionUpper-next.bass"] <- list(predRes$summary[,6][1])
      modelEstimates[k,][["predictionUpper-next.bass"]] <- list(predRes$summary[,6][1])
      modelEstimates[k,"MSE.bass"] <- calculateMSE(residual)
      modelEstimates[k,"R2.bass"] <- calculateR2(residual, Yreal)
    }
  }
    
  return (modelEstimates)
  
}





estimateLinear1 <- function(X,Y) {
  f <- data.frame(x=X, y=Y)
  fit <- lm(y ~ I(x), data=f)
  return (fit)
}

estimateLinear2 <- function(X,Y) {
  f <- data.frame(x=X, y=Y)
  fit <- lm(y ~ 1 + I(x) + I(x^2), data=f)
  return (fit)
}

estimateLinear3 <- function(X,Y) {
  f <- data.frame(x=X, y=Y)
  fit <- lm(y ~ 1 + I(x) + I(x^2) + 1/I(x^3), data=f)
  return (fit)
}

estimateBass <- function(X,Y, sP, sQ, sM) {
  f <- data.frame(x=X, y=Y)
  fit <- nlsLM(y ~ m*((p+q)^2/p)*((exp(-(p+q)*x))/(1+(q/p)*exp(-(p+q)*x))^2), data=f, start=list(p=sP,q=sQ,m=sM),lower = c(0,0,0))
  return (fit)
}

calculateMSE <- function(resid) {
  mse <- sum(resid^2) / length(resid)
  return (mse)
}

calculateR2 <- function(resid, Y) {
  RSS <- sum(resid^2)
  m <- mean(Y)
  TSS <- sum((Y - m)^2)
  r2 <- 1 - RSS/TSS
  return (r2)
}