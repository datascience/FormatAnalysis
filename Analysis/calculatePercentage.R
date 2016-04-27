
calculatePercentage <- function(pData, propertyToTake) {
  
  # calculate percentages inside of the market
  aggreg <- aggregate(amount~year , FUN=sum, data=pData) 
  aggreg <- setNames(aggreg, c("year", "total"))
  
  pData <- merge(pData, aggreg, by="year")
  pData$percentage <- (pData$amount / pData$total)*1000
  pData <- pData[!(names(pData)=="total")]
  rm(aggreg)

  pData$average <- c(1:nrow(pData))
  
  for (i in (1:nrow(pData))) {
    if (!is.na(pData[i,]$age)) {
      age<- pData[i,]$age
      data <- merge(pData, pData[i,][names(pData) %in% propertyToTake], by=propertyToTake)
      avrg1 <- average(age,data, "percentage")
      pData[i,]$average <- avrg1
    } else {
      pData[i,]$average <- NA
    }
    
  }
  
  pData <- pData[c("ID", "name", propertyToTake, "year", "amount", "release.year", "age", 
                   "percentage", "average")]

  #remove those where release year was not available
  pData <- pData[pData$age>=0 & !is.na(pData$release.year) & !is.na(pData$age),]
  
  return (pData)
}


average <- function(age, data, cn) {
  
  perc1 <- data[data$age==age-1,cn]
  perc2 <- data[data$age==age,cn]
  perc3 <- data[data$age==age+1,cn]

  if (length(perc1)==0 & length(perc3)==0) {
    return (perc2)
  }
  if (length(perc1)==0) {
    return ((perc2+perc3)/2)
  }
  if (length(perc3)==0) {
    return ((perc1+perc2)/2)
  }
  return ((perc1+perc2+perc3)/3)
}