
estimateModel <- function(pData, propertyToTake) {
  
  if (is.na(propertyToTake)) {
    unig <- aggregate(.~mime, FUN=length, data=pData)
    models <- data.frame(mime=character(), model=lm(), stringsAsFactors=FALSE)
  }else {
    unig <- aggregate(as.formula(paste(".~", paste(c("mime", propertyToTake), collapse="+"))), 
                      FUN=length, data=pData)
    models <- data.frame(mime=c(), model=lm(), stringsAsFactors=FALSE)
    models[[propertyToTake]] <- character()
  }
  for (i in (1:nrow(unig))) {
    if (is.na(propertyToTake)) {
      X <- pData[pData$mime == unig[i,]$mime,]$age
      Y <- pData[pData$mime == unig[i,]$mime,]$percentage
      models[i,]$mime <- unig[i,]$mime
    }else {
      X <- pData[pData[["mime"]]==unig[i,]$mime & pData[[propertyToTake]]==unig[i,][[propertyToTake]],]$age
      Y <- pData[pData[["mime"]]==unig[i,]$mime & pData[[propertyToTake]]==unig[i,][[propertyToTake]],]$percentage
      models[i,]$mime <- unig[i,]$mime
      models[i,][[propertyToTake]] <- unig[i,][[propertyToTake]]
    }
    m <- estimate(X,Y)
    models[i,]$model <- m
    
  }
  
  return (models)
}


estimate <- function(X,Y) {
  fit <- lm(Y ~ 1 + I(X) + I(X^2) + I(X^3) + I(X^4) + I(X^5), data=X)
  return (fit)
}