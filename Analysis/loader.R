source('loadData.R')
source('utils.R')
source('config.R')


rawData <- loadData(file, colNames)

print(dim(rawData))

mimeF <- mime(rawData[[col]])
propertyF <- property(rawData[[col]], propertyToTake)

pData <- data.frame(mimeF, propertyF, rawData$YEAR, rawData$AMOUNT)
print(dim(pData))
colnames(pData) <- c("MIME", propertyToTake, "YEAR", "AMOUNT")
pData <- pData[pData$MIME %in% formats, ]
pData <- pData[!is.na(pData[[propertyToTake]]), ]

pData <- aggregate(YEAR, FUN=sum, pData)
