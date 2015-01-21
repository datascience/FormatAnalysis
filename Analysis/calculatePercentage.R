
calculatePercentage <- function(pData, propertyToTake) {
  
  
  aggreg <- aggregate(amount~year , FUN=sum, data=pData) 
  aggreg <- setNames(aggreg, c("year", "total"))
  
  pData <- merge(pData, aggreg, by="year")
  pData$percentage <- pData$amount / pData$total
  pData <- pData[!(names(pData)=="total")]
  rm(aggreg)
  
  if (is.na(propertyToTake)) {
    aggreg <- aggregate(amount~mime , FUN=sum, data=pData) 
    aggreg <- setNames(aggreg, c("mime", "total"))
    pData <- merge(pData, aggreg, by="mime")
  }else {
    aggreg <- aggregate(as.formula(paste("amount~", paste(c("mime", propertyToTake), collapse="+"))) , FUN=sum, data=pData) 
    aggreg <- setNames(aggreg, c("mime", propertyToTake, "total"))
    pData <- merge(pData, aggreg, by=c("mime",propertyToTake))
  }
  #pData$percentage2 <- pData$amount / pData$total
  #pData <- pData[!(names(pData)=="total")]
  #rm(aggreg)
  #print(pData)
  pData$average <- c(1:nrow(pData))
  #pData$average2 <- c(1:nrow(pData))
  for (i in (1:nrow(pData))) {
    age<- pData[i,]$age
    if (is.na(propertyToTake)) {
      data <- merge(pData, pData[i,][names(pData) %in% c("mime")], by="mime")
      
#       data <- data.frame(age=pData[pData$mime==pData[i,]$mime,]$age, 
#                          percentage=pData[pData$mime==pData[i,]$mime,]$percentage)
#       data2 <- data.frame(age=pData[pData$mime==pData[i,]$mime,]$age, 
#                          percentage=pData[pData$mime==pData[i,]$mime,]$percentage2)
    } else {
      data <- merge(pData, pData[i,][names(pData) %in% c("mime",propertyToTake)], by=c("mime",propertyToTake))
      
#       data <- data.frame(age=pData[pData$mime==pData[i,]$mime & pData[[propertyToTake]]==
#                                      pData[i,][[propertyToTake]],]$age,
#                          percentage=pData[pData$mime==pData[i,]$mime & pData[[propertyToTake]]==pData[i,][[propertyToTake]],]$percentage)
#       data2 <- data.frame(age=pData[pData$mime==pData[i,]$mime & pData[[propertyToTake]]==
#                                      pData[i,][[propertyToTake]],]$age,
#                          percentage=pData[pData$mime==pData[i,]$mime & pData[[propertyToTake]]==pData[i,][[propertyToTake]],]$percentage2)
    }
    avrg <- average(age,data)
#     avrg2 <- average(age,data2) 
    pData[i,]$average <- avrg
#     pData[i,]$average2 <- avrg2
  }
  
  if (is.na(propertyToTake)) {
    pData <- pData[c("mime", "year", "amount", "age", "percentage", "average")]
#     pData <- pData[c("mime", "year", "amount", "age", "percentage", 
#                      "average", "percentage2", "average2")]
  }
  else {
    pData <- pData[c("mime", propertyToTake, "year", "amount", "age", 
                     "percentage", "average")]
#     pData <- pData[c("mime", propertyToTake, "year", "amount", "age", 
#                      "percentage", "average", "percentage2", "average2")]
  }

  return (pData)
}


average <- function(age, data) {
  
#   print (age)
#   print (data)
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