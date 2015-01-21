
calculateAge <- function(pData, releaseYears, propertyToTake) {

  options( warn = -1 )
  
  if (is.na(propertyToTake)) {
    
    releases <- aggregate(releaseYear~mime, FUN=min, data=releaseYears)
  
    pData <- merge(pData,releases, by=c("mime"))
    rm(releases)
    pData$age <- pData$year - pData$releaseYear
    pData <- pData[!(names(pData)=="releaseYear")]
  
  } else {
    nam <- names(releaseYears)
    nam <- nam[nam != "releaseYear"]
    pData <- merge(pData,releaseYears, by=nam)
    pData$age <- pData$year - pData$releaseYear
    pData <- pData[!(names(pData)=="releaseYear")]
  }
  pData <- pData[pData$age>=0,]
  options(warn=0)
  return (pData)
}