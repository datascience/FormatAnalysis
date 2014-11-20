source('utils.R')

loadData <- function(file, colNames, propertyToTake) {

  options( warn = -1 )
  
  rawData <- loadDataFromFile(file, colNames)


  mimeTika <- mime(rawData[["tika"]])

  propertyTika <- property(rawData[["tika"]], propertyToTake)
  mimeDroid <- mime(rawData[["droid"]])
  propertyDroid <- property(rawData[["droid"]], propertyToTake)

  
  pData <- data.frame(mimeTika, propertyTika, mimeDroid, propertyDroid, rawData$year, rawData$amount)
  colnames(pData) <- c("mimeTika", paste(propertyToTake,"Tika", sep=""), "mimeDroid", 
                     paste(propertyToTake,"Droid", sep=""), "year", "amount")
  
  rm(rawData)
  rm(mimeTika)
  rm(propertyTika)
  rm(mimeDroid)
  rm(propertyDroid)
  
  pData <- pData[pData$mimeTika %in% formats | pData$mimeDroid %in% formats, ]
  pData$mimeTika <- as.character(pData$mimeTika)
  pData[[paste(propertyToTake,"Tika", sep="")]] <- as.character(pData[[paste(propertyToTake,"Tika", sep="")]])
  pData$mimeDroid <- as.character(pData$mimeDroid)
  pData[[paste(propertyToTake,"Droid", sep="")]] <- as.character(pData[[paste(propertyToTake,"Droid", sep="")]])


  pData$mime <- mapply(resolveConflicts, pData$mimeTika, pData$mimeDroid) 
  pData[[propertyToTake]] <- mapply(resolveConflictsProperty, pData[[paste(propertyToTake,"Tika", sep="")]], 
                                  pData[[paste(propertyToTake,"Droid", sep="")]])

  pData <- pData[!(names(pData) %in% c("mimeTika", "mimeDroid", paste(propertyToTake, "Tika", sep=""), 
                                   paste(propertyToTake, "Droid", sep="")))]

  pData <- pData[!is.na(pData[[propertyToTake]]), ]

  pData <- aggregate(amount~mime+version+year, FUN=sum, data=pData)
 
  aggreg <- aggregate(amount~year , FUN=sum, data=pData) 
  aggreg <- setNames(aggreg, c("year", "total"))

  pData <- merge(pData,releases, by=c("mime", propertyToTake))
  pData$age <- pData$year - pData$releaseYear
  pData <- pData[!(names(pData)=="releaseYear")]

  pData <- merge(pData, aggreg, by="year")
  pData$percentage <- pData$amount / pData$total
  pData <- pData[!(names(pData)=="total")]

  pData <- pData[c("mime", propertyToTake, "year", "amount", "age", "percentage")]

  rm(aggreg)
  
  options( warn = 0 )
  return (pData)
}

           

