#loading the data

source('utils.R')
library(plyr)
library(ggplot2)

prepareData <- function(file, colNames, groupData, propertyToTake, resolveConflicts, afterResolution, conflictCategory) {

  options( warn = -1 )
  
  #load data from a file into a data frame
  print("Loading raw data")
  rawData <- loadDataFromFile(file, colNames)
  print("Raw data loaded, starting processing")
  
  #extrcating mime and property into seprate columns and 
  #unifying everything into one data frame (pData)
  print("Extracting properties as separate values")
  pData <- data.frame(rawData$year, rawData$amount)
  colnames(pData) <- c("year", "amount");
  for (i in colNames[1:(length(colNames)-2)]) {
    for (prop in propertyToTake)   {
        tmp <- paste(prop,i,sep="")
        pData[[tmp]] <- property(rawData[[i]], prop)
    }
  }
  
  #remove raw data from memory
  rm(rawData)
  
  aggregatePerYear <- aggregate(amount ~ year, data=pData, FUN = sum)
  
  #filter according to selected properties
  for (prop in propertyToTake) {
    print (paste("Filtering according to: ", prop))
    tempNames <- grep(prop, names(pData)) 
    tempV <- unique(unlist(groupData[[prop]]))
    if (TRUE %in% ("-" %in% tempV)) {
      print("TRUE IS")
      for (i in tempNames) {
        pData[is.na(pData[,i]),i] <- "-"
      }
    } else {
      pData <- pData[apply(pData[,tempNames], 1, function(row) any(row %in% tempV)), ]
    }
  }
  # remove if there are amounts equal to NA 
  pData <- pData[!is.na(pData$amount),]
  recordConflicts(pData, propertyToTake, 1)
  
  numConBeforeUnify <- countConflicts(pData, propertyToTake, 1)
  sizeOfTheSegmentBefore <- sum(pData$amount)
  
  #unify all equal properties
  for (prop in propertyToTake) {
    print (paste("Unifying according to: ", prop))
    tempNames <- grep(prop, names(pData))
    pData[,tempNames] <- apply(pData[,tempNames], c(1,2), function(value) unifyValues(value, groupData[[prop]]))
    groupData[[prop]] <- sapply(groupData[[prop]], "[",1)
  }
  
  recordConflicts(pData, propertyToTake, 2)
  numConBefore <- countConflicts(pData, propertyToTake, 1)
    
    
  # resolving conflicts
  print("Resolving conflicts")
  if (!is.na(resolveConflicts)) {
    for (prop in propertyToTake) {
        pData[[prop]] <- apply(pData[,c(grep(prop, names(pData)),which(names(pData)=="amount"))], 
                               1, function(row) resolveConflicts(row,prop))
    }
  }
  
  afterResolution()
  numConAfter <- countConflicts(pData, propertyToTake, 2)
  recordConflicts(pData, propertyToTake, 3)

  
  print ("Final processing, merging with market data and adding age")
  # filter once more to remove those elements that do not belong to the selected group
  # conflict reduction could have resolved some entries to not needed values
  for (prop in propertyToTake) {
    if (!("-" %in% groupData[[prop]])) {
      pData <- pData[pData[[prop]] %in% groupData[[prop]],]
    }
    pData[[prop]] <- as.character(pData[[prop]])
  }
  
 # pData[["category"]] <- apply(pData, 1, function(row) conflictCategory(row,propertyToTake))
  nam <- names(groupData)
  nam <- nam[!(nam %in% c("release.year", "name", "comments", "ID"))]
  pData <- merge(pData,groupData, by=nam)
  

#   boxDF <- ddply(pData, "name", transform, percent = amount / sum(amount)*100)
#   boxDF <- aggregate(percent~name+category, data=boxDF, FUN=sum)
#   
#   png(filename=paste(path, "/statistics/elements.png", sep=""), width = 800, height = 680)
#   elPlot <- ggplot(boxDF, aes(x=name, y=percent, fill=category)) + geom_bar(stat="identity") + coord_flip() +
#     scale_fill_brewer(palette = "Accent") + scale_y_continuous(expand=c(0,0))
#   print(elPlot)
#   dev.off()
  recordConflicts(pData, propertyToTake, 4)
  
  #discard columns that are not needed anymore
  pData <- pData[,names(pData) %in% c(propertyToTake, "year", "amount", "release.year", "name", "ID")]


  #aggregate if there are some duplicate entries
  pData <- aggregate(as.formula(paste("amount~", paste(c(propertyToTake, "year", "amount", "release.year", "name","ID"), collapse="+"))), 
                       FUN=sum, data=pData)  
  
  sizeOfTheSegmentAfter <- sum(pData$amount)
  
  conflictRules <- read.table(paste(path, "/statistics/rulesUsed.tsv", sep=""), header=TRUE, sep="\t", stringsAsFactors=FALSE)
  numOfRules <- nrow(conflictRules)
  
  percOfToWA <- (sizeOfTheSegmentBefore / 2572553360) * 100
  percOfToWAAft <- (sizeOfTheSegmentAfter / 2572553360) * 100
  percOfConfBeforeUnify <- (numConBeforeUnify / sizeOfTheSegmentBefore) * 100
  percOfConf <- (numConBefore / sizeOfTheSegmentBefore) * 100
  percOfUNresCon <- (numConAfter / sizeOfTheSegmentBefore) * 100
  marketStatsDF <- data.frame(property=c("% of total WA", "% of conflicts before merging", "% of conflicts after merging", 
                                         "% of unresolved conflicts", "number of custom rules", 
                                         "% of total WA after cr"), 
                              value=c(percOfToWA, percOfConfBeforeUnify, percOfConf, percOfUNresCon, numOfRules, percOfToWAAft))
  write.table(marketStatsDF, file=paste(path, "/marketStats.tsv", sep=""), 
              quote=FALSE, sep="\t", col.names=TRUE, row.names=FALSE)
  
  #calculate age
  pData$age <- pData$year - as.numeric(pData$release.year)
  # this is moved to calculatePercentages
  #pData <- pData[pData$age>=0,]
  
  options(warn=0)
  print("Data loading finished")
  return (pData)

  
}

           

