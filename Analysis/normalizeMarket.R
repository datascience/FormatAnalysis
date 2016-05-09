
normalizeMarket <- function(pData, propertyToTake, multiplicationFactor) {
  
  # calculate percentages inside of the market
  # and multiply the values with the multiplication factor 
  aggreg <- aggregate(amount~year , FUN=sum, data=pData) 
  aggreg <- setNames(aggreg, c("year", "total"))
  
  pData <- merge(pData, aggreg, by="year")
  pData$adoptionRate <- (pData$amount / pData$total)*multiplicationFactor
  pData <- pData[!(names(pData)=="total")]
  rm(aggreg)

  # calculate average values 
  pData$smoothedAdoptionRate <- c(1:nrow(pData))
  
  for (i in (1:nrow(pData))) {
    if (!is.na(pData[i,]$age)) {
      age<- pData[i,]$age
      data <- merge(pData, pData[i,][names(pData) %in% propertyToTake], by=propertyToTake)
      avrg1 <- average(age,data)
      pData[i,]$smoothedAdoptionRate <- avrg1
    } else {
      pData[i,]$average <- NA
    }
    
  }
  
  # take the needed columns from the data frame 
  pData <- pData[c("ID", "name", propertyToTake, "year", "amount", "release.year", "age", 
                   "adoptionRate", "smoothedAdoptionRate")]

  # remove those where release year was not available as for those the model can not be estimated
  pData <- pData[pData$age>=0 & !is.na(pData$release.year) & !is.na(pData$age),]
  
  return (pData)
}


average <- function(age, data) {
  perc1 <- data[data$age==age-1,]$adoptionRate
  perc2 <- data[data$age==age,]$adoptionRate
  perc3 <- data[data$age==age+1,]$adoptionRate
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