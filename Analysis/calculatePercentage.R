
calculatePercentage <- function(pData, propertyToTake) {
  
  
  aggreg <- aggregate(amount~year , FUN=sum, data=pData) 
  aggreg <- setNames(aggreg, c("year", "total"))
  
  pData <- merge(pData, aggreg, by="year")
  pData$percentage <- pData$amount / pData$total
  pData <- pData[!(names(pData)=="total")]
  rm(aggreg)
  
  if (is.na(propertyToTake)) {
    pData <- pData[c("mime", "year", "amount", "age", "percentage")]
  }
  else {
    pData <- pData[c("mime", propertyToTake, "year", "amount", "age", "percentage")]
  }

  return (pData)
}