source('loadData.R')
source('utils.R')
source('config.R')


rawData <- loadData(file, colNames)

mimeF <- mime(rawData[[col]])
propertyF <- property(rawData[[col]], propertyToTake)

pData <- data.frame(mimeF, propertyF, rawData$year, rawData$amount)
colnames(pData) <- c("mime", propertyToTake, "year", "amount")
pData <- pData[pData$mime %in% formats, ]
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

#move ploting 
plot(pData[pData$version=="1.3",]$age,pData[pData$version=="1.3",]$percentage)
           

