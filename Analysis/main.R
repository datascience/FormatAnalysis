source('config.R')
source('loadData.R')


path <- paste("output data/", name, sep="")
dir.create(path)

groupData <- read.table(paste("input data/", groupFile, sep=""), header=TRUE, sep="\t", stringsAsFactors=FALSE)
groupData <- read.table(paste("input data/", groupFile, sep=""), header=TRUE, 
                        colClasses=rep("character", each=ncol(groupData)), sep="\t", stringsAsFactors=FALSE)


# calculate properties to take  
nam <- names(groupData)
propertyToTake <- nam[!(nam %in% c("ID", "name", "release.year", "comments"))]

source('discardNotComplete.R')
groupData <- discardNotComplete(groupData, propertyToTake)
write.table(groupData, file=paste(path, "/market.tsv", sep=""), quote=FALSE, sep="\t", 
            col.names=TRUE, row.names=FALSE)

#make a list from multiple mime and other property values
for (prop in propertyToTake) {
  groupData[[prop]] <- strsplit(groupData[[prop]], split = " ")
}

# load raw data, filter and reduce conflicts and save the resulting dataset to a file  
data <- loadData(fileName, colNames, groupData, propertyToTake, resolveConflicts, afterConflictsResolution)
write.table(data, file=paste(path,"/cleaned_data.tsv",sep=""), quote=FALSE, 
            sep="\t", col.names=TRUE, row.names=FALSE)

for (prop in propertyToTake) {
  groupData[[prop]] <- sapply(groupData[[prop]], "[",1)
}

# calculate age 
source('calculateAge.R')
data2 <- calculateAge(data, groupData, propertyToTake)

# calculate percentage and moving average of each value
source('calculatePercentage.R')
data3 <- calculatePercentage(data2,propertyToTake)
write.table(data3, file=paste(path, "/adoption.tsv", sep=""), 
            quote=FALSE, sep="\t", col.names=TRUE, row.names=FALSE)

# estimate the model and plot the curves
source('estimateModelParameters.R')
estimates <- estimateModelParameters(data3, propertyToTake, start, end)
#write.table(estimates, file=paste("output data/", paste(name,"_estimates.csv", sep=""), sep=""), 
#            quote=FALSE, sep="\t", col.names=TRUE, row.names=FALSE)

source('plotResults.R')
plotResults(estimates, includeRateOfChange, includeInterval, includePoints)
