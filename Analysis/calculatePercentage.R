
calculatePercentage <- function(pData, propertyToTake) {
  
  
  aggreg <- aggregate(amount~year , FUN=sum, data=pData) 
  aggreg <- setNames(aggreg, c("year", "total"))
  
  pData <- merge(pData, aggreg, by="year")
  pData$percentage <- pData$amount / pData$total
  pData <- pData[!(names(pData)=="total")]
  rm(aggreg)
  

  aggreg <- aggregate(as.formula(paste("amount~", paste(propertyToTake, collapse="+"))) , FUN=sum, data=pData) 
  aggreg <- setNames(aggreg, c(propertyToTake, "total"))
  pData <- merge(pData, aggreg, by=propertyToTake)

  pData$average <- c(1:nrow(pData))
  
  for (i in (1:nrow(pData))) {
    age<- pData[i,]$age
    data <- merge(pData, pData[i,][names(pData) %in% propertyToTake], by=propertyToTake)
      
    avrg <- average(age,data)
    pData[i,]$average <- avrg
  }
  
  pData <- pData[c("ID", "name", propertyToTake, "year", "amount", "release.year", "age", 
                   "percentage", "average")]

  return (pData)
}


average <- function(age, data) {
  
  perc1 <- data[data$age==age-1,]$percentage
  perc2 <- data[data$age==age,]$percentage
  perc3 <- data[data$age==age+1,]$percentage
#   print(perc1)
#   print(perc2)
#   print(perc3)
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