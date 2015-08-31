
calculateAge <- function(pData, gData, pTake) {

  options( warn = -1 )

  nam <- names(gData)
  nam <- nam[!(nam %in% c("release.year", "name", "comments", "ID"))]
  pData <- merge(pData,gData, by=nam)
  pData$age <- pData$year - as.numeric(pData$release.year)
  pData <- pData[!(names(pData)=="comments")]
  #pData <- pData[!(names(pData)=="releaseYear")]

  pData <- pData[pData$age>=0,]
  options(warn=0)
  return (pData)
}