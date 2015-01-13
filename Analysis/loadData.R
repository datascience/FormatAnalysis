#loading the data

source('utils.R')

loadData <- function(file, colNames, propertyToTake, resolveConflictsMime, resolveConflictsProperty, unificationRules) {

  options( warn = -1 )
  
  #load data from a file into a data frame
  rawData <- loadDataFromFile(file, colNames)

  
  #extrcating mime and property into seprate columns and 
  #unifying everything into one data frame (pData)
  pData <- data.frame(rawData$year, rawData$amount)
  colnames(pData) <- c("year", "amount");
  for (i in colNames[1:(length(colNames)-2)]) {
    tmp <- paste("mime",i,sep="")
    pData[[tmp]] <- mime(rawData[[i]]) 
    if (!is.na(propertyToTake)) {
      tmp <- paste(propertyToTake,i,sep="")
      pData[[tmp]] <- property(rawData[[i]], propertyToTake)
    }
  }
  
  #remove raw data from memory
  rm(rawData)
  
  #filter according to mime type 
  pData <- pData[apply(pData[,grep("mime", names(pData))], 1, function(row) any(row %in% fileData$mime)), ]
  
  #filter according to property 
  if (!is.na(propertyToTake)) {
    pData <- pData[apply(pData[,grep(propertyToTake, names(pData))], 1, function(row) any(row %in% fileData[[propertyToTake]])), ]
  }
  
  # resolving conflicts
  pData$mime <- apply(pData[,grep("mime", names(pData))], 1, resolveConflictsMime)
  if (!is.na(propertyToTake)) {
    pData[[propertyToTake]] <- apply(pData[,grep(propertyToTake, names(pData))], 1, resoresolveConflictsProperty)
  }
  
  
  pData <- pData[,names(pData) %in% c("mime", propertyToTake, "year", "amount")]

  #filter once more just to be sure we have removed all unwanted elements
  pData <- pData[!is.na(pData$mime) & pData$mime %in% fileData$mime,]
  if (!is.na(propertyToTake)) {
    pData <- pData[!is.na(pData[[propertyToTake]]) & pData[[propertyToTake]] %in% fileData[[propertyToTake]], ]
  }

  #unification 
  if (!is.na(unificationRules)) {
    for (i in c(1:nrow(unificationRules))) {
      element <- unificationRules[i,]$element
      from <- unificationRules[i,]$from
      to <- unificationRules[i,]$to
      pData[pData[[element]]==from,][[element]] <- to
    }
  }

  #aggregate if there are some repeating entries
  if (is.na(propertyToTake)) {
    pData <- aggregate(amount~mime+year, FUN=sum, data=pData)
  }else {
    pData <- aggregate(as.formula(paste("amount~", paste(c("mime", propertyToTake, "year"), collapse="+"))), 
                       FUN=sum, data=pData)    
  }
  options(warn=0)
  return (pData)
  
}

           

