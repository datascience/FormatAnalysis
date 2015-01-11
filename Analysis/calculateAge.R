
calculateAge <- function(pData, releaseYears, propertyToTake) {

  if (is.na(propertyToTake)) {
    releases <- aggregate(releaseYear~mime, FUN=min, data=releaseYears)
  
    pData <- merge(pData,releases, by=c("mime"))
    rm(releases)
    pData$age <- pData$year - pData$releaseYear
    pData <- pData[!(names(pData)=="releaseYear")]
  
  } else {
    pData <- merge(pData,releaseYears, by=c("mime", propertyToTake))
    pData$age <- pData$year - pData$releaseYear
    pData <- pData[!(names(pData)=="releaseYear")]
  }
  pData <- pData[pData$age>=0,]
  return (pData)
}