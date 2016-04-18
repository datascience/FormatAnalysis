library(minpack.lm)
library(propagate)
require(MASS)
estimateModelParameters <- function(pData, propertyToTake, start, end, predictionYears, intervalType, alphaInterval, useMovingAverage, useTotal) {

  options( warn = -1 )
  #pData <- pData[pData$year >= start & pData$year<=end,]
  
  #extract unique values 
  unig <- unique(pData$ID)
  

  modelEstimates <- data.frame(matrix(vector(),0,7 + length(propertyToTake), 
                                      dimnames = list(c(),c("ID", "name", propertyToTake, "release.year", 
                                                            "ages", "percentages", "averages", "predictionAge"))))

  k <- 1
  for (i in unig) {
    data <- pData[pData$ID==i,]
    print(paste("Estimating and fitting the model to ", data[1,]$name))
    
    #take the data needed for doing the estimation
    X <- data[data$year >= start & data$year<=end,]$age
    Xper <- NA
   
#      if (!is.na(predictionYears)) {
#       Xper <- predictionYears - data[1,]$release.year
#       Xper[Xper < 0] <- 0 
#     } 
    Yper <- NA
    Yavg <- NA
    year <- NA
    if (useTotal==TRUE) {
      Yper <- data[data$year >= start & data$year<=end,]$percentageYear
      Yavg <- data[data$year >= start & data$year<=end,]$averageYear
      year <- data[data$year >= start & data$year<=end,]$year
    }else {
      Yper <- data[data$year >= start & data$year<=end,]$percentage
      Yavg <- data[data$year >= start & data$year<=end,]$average
      year <- data[data$year >= start & data$year<=end,]$year
    }
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
    modelEstimates[k,][["percentages"]] <- list(Yper)
    modelEstimates[k,][["averages"]] <- list(Yavg)
    
#     if (is.na(Xper)) {
#       modelEstimates[k,"predictionAge"] <- NA
#       modelEstimates[k,"real-next"] <- NA
#     } else {
#       modelEstimates[k,][["predictionAge"]] <- list(Xper)
#       RV <- c()
#       for (j in 1:length(Xper)) {
#         temp <- data[data$year=Xper[j],]$percentage
#         if (length(temp)>0) {
#           RV[j] <- temp
#         } else {
#           RV[j] <- NA
#         }
#       }
#       modelEstimates[k,][["real-next"]] <- list(RV)
#     }
    
   # modelEstimates <- estimateLinear1Model(modelEstimates, X, Y, Yper, k, Xper, intervalType, alphaInterval)
    
  #  modelEstimates <- estimateLinear2Model(modelEstimates, X, Y, Yper, k, Xper, intervalType, alphaInterval)
    
   # modelEstimates <- estimateLinear3Model(modelEstimates, X, Y, Yper, k, Xper, intervalType, alphaInterval)
    
    modelEstimates <- estimateBassModel(modelEstimates, X, Y, Yper, k, Xper, intervalType, alphaInterval)
    
    
    
    
    k <- k + 1
  
  }
  options( warn = 0 )
  return (modelEstimates)
}




estimateBassModel <- function(modelEstimates, X, Y, Yreal, k, predYear, intervalType, alphaInterval) {
  
  #Bass model estimation
  #pInterval <- c(0.01, 0.03, 0.05, 0.07, 0.1, 0.2, 0.3)
  pInterval <- seq(0.00, 0.5, 0.01)
  #qInterval <- c(0.01, 0.05, 0.1, 0.3, 0.5, 0.7, 1.0)
  qInterval <- seq(0.00, 1.0, 0.05)
  mInterval <- c(0.000, 0.01, 0.1, 1, 10, 100, 1000, 10000, 30000)
  #mInterval <- seq(0,10000,100)
  model <- NA
  tempRes <- Inf
  for (iP in pInterval) {
    for (iQ in qInterval) {
      for (iM in mInterval) {
        print(paste(iP,iQ,iM, sep = " "))
        t <- try(mod <- estimateBass(X,Y, iP, iQ, iM))
        if("try-error" %in% class(t)) {
          print("error in estimating the model")
          next
        }
        
        p <- as.numeric(coef(mod)["p"])
        q <- as.numeric(coef(mod)["q"])
        m <- as.numeric(coef(mod)["m"])
        if (p>1 | p<0 | q<0 | m<0) {
          next
        }
        if ((m*(p+q)^2)/(4*q) <= 1000) {
          sumRes <- sum(resid(mod)^2)
          if (sumRes < tempRes) {
            model <- mod
            tempRes <- sumRes
          }
        }
        
        #if p is above 1 do not use the model
        
        
#         sumRes <- sum(resid(mod)^2)
#         if (sumRes < tempRes) {
#           model <- mod
#           tempRes <- sumRes
#         }
        
      }
    }
  }
  
  if (is.na(model)) {
    print("Model is NA")
    modelEstimates[k,"p.bass"] <- NA
    modelEstimates[k,"q.bass"] <- NA
    modelEstimates[k,"m.bass"] <- NA
    modelEstimates[k,"qprat.bass"] <-NA
    modelEstimates[k,"prediction.bass"] <- NA
    modelEstimates[k,"residual.bass"] <- NA
    modelEstimates[k,"interval.bass"] <- NA
    modelEstimates[k,"model.bass"] <- NA
    modelEstimates[k,"upper.bass"] <- NA
    modelEstimates[k,"lower.bass"] <- NA
    modelEstimates[k,"derv.bass"] <- NA
#     modelEstimates[k,"prediction-next.bass"] <- NA
#     modelEstimates[k,"predictionLower-next.bass"] <- NA
#     modelEstimates[k,"predictionUpper-next.bass"] <- NA
    modelEstimates[k,"MSE.bass"] <- NA
    modelEstimates[k,"R2.bass"] <- NA
    modelEstimates[k,"pLow.bass"] <- NA
    modelEstimates[k,"pUpr.bass"] <- NA
    modelEstimates[k,"qLow.bass"] <- NA
    modelEstimates[k,"qUpr.bass"] <- NA
    modelEstimates[k,"mLow.bass"] <- NA
    modelEstimates[k,"mUpr.bass"] <- NA
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
      modelEstimates[k,"qprat.bass"] <-NA
      modelEstimates[k,"prediction.bass"] <- NA
      modelEstimates[k,"residual.bass"] <- NA
      modelEstimates[k,"interval.bass"] <- NA
      modelEstimates[k,"model.bass"] <- NA
      modelEstimates[k,"upper.bass"] <- NA
      modelEstimates[k,"lower.bass"] <- NA
      modelEstimates[k,"derv.bass"] <- NA
#       modelEstimates[k,"prediction-next.bass"] <- NA
#       modelEstimates[k,"predictionLower-next.bass"] <- NA
#       modelEstimates[k,"predictionUpper-next.bass"] <- NA
      modelEstimates[k,"MSE.bass"] <- NA
      modelEstimates[k,"R2.bass"] <- NA
      modelEstimates[k,"pLow.bass"] <- NA
      modelEstimates[k,"pUpr.bass"] <- NA
      modelEstimates[k,"qLow.bass"] <- NA
      modelEstimates[k,"qUpr.bass"] <- NA
      modelEstimates[k,"mLow.bass"] <- NA
      modelEstimates[k,"mUpr.bass"] <- NA
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
      
      t <- try (cInt <- confint(model, level = 0.95))
      if ("try-error" %in% class(t)) {
        print("error in predictNLS 2")
        pL <- NA
        pU <- NA
        qL <- NA
        qU <- NA
        mL <- NA
        mU <- NA
      }else {
        pL <- cInt["p",1]
        pU <- cInt["p",2]
        qL <- cInt["q",1]
        qU <- cInt["q",2]
        mL <- cInt["m",1]
        mU <- cInt["m",2]
        
      }
        
      modelEstimates[k,"p.bass"] <- coef(model)["p"]
      p <- as.numeric(modelEstimates[k,"p.bass"])
      modelEstimates[k,"pLow.bass"] <- pL
      modelEstimates[k,"pUpr.bass"] <- pU
      modelEstimates[k,"q.bass"] <- coef(model)["q"]
      q <- as.numeric(modelEstimates[k,"q.bass"])
      modelEstimates[k,"qLow.bass"] <- qL
      modelEstimates[k,"qUpr.bass"] <- qU
      modelEstimates[k,"m.bass"] <- coef(model)["m"]
      m <- as.numeric(modelEstimates[k,"m.bass"])
      modelEstimates[k,"mLow.bass"] <- mL
      modelEstimates[k,"mUpr.bass"] <- mU
      modelEstimates[k,"qprat.bass"] <-as.numeric(q/p)
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
#       modelEstimates[k,"prediction-next.bass"] <- list(predRes$summary[,1][1])
#       modelEstimates[k,][["prediction-next.bass"]] <- list(predRes$summary[,1][1])
#       modelEstimates[k,"predictionLower-next.bass"] <- list(predRes$summary[,5][1])
#       modelEstimates[k,][["predictionLower-next.bass"]] <- list(predRes$summary[,5][1])
#       modelEstimates[k,"predictionUpper-next.bass"] <- list(predRes$summary[,6][1])
#       modelEstimates[k,][["predictionUpper-next.bass"]] <- list(predRes$summary[,6][1])
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
  fit <- nlsLM(y ~ m*((p+q)^2/p)*((exp(-(p+q)*x))/(1+(q/p)*exp(-(p+q)*x))^2), data=f, start=list(p=sP,q=sQ,m=sM))
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