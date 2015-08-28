source('config.R')
source('loadData.R')
#source('utils.R')


dir.create(paste("output data/",name,sep=""))

groupData <- read.table(paste("input data/", groupFile, sep=""), header=TRUE, sep="\t", stringsAsFactors=FALSE)
groupData <- read.table(paste("input data/", groupFile, sep=""), header=TRUE, colClasses=rep("character", each=ncol(groupData)), sep="\t", stringsAsFactors=FALSE)
#releases <- read.table(paste("input data/", releaseFile, sep=""), header=TRUE, sep="\t", stringsAsFactors=FALSE)


# calculate properties to take  
nam <- names(groupData)
propertyToTake <- nam[!(nam %in% c("ID", "name", "release.year", "comments"))]

#make a list from multiple mime and other property values
for (prop in propertyToTake) {
  groupData[[prop]] <- strsplit(groupData[[prop]], split = " ")
}

# load raw data, filter and reduce conflicts and save the resulting dataset to a file  
data <- loadData(fileName, colNames, groupData, propertyToTake, resolveConflicts)
write.table(data, file=paste("output data/", paste(name,"_filtered.csv"), sep=""), quote=FALSE, sep="\t", col.names=TRUE, row.names=FALSE)

# calculate age 
source('calculateAge.R')
data2 <- calculateAge(data, releases, propertyToTake)

# calculate percentage and moving average of each value
source('calculatePercentage.R')
data3 <- calculatePercentage(data2,propertyToTake)
write.table(data3, file=paste("output data/", paste(name,"_adoption.csv", sep=""), sep=""), quote=FALSE, sep="\t", col.names=TRUE, row.names=FALSE)

# estimate the model and plot the curves
source('plotResults.R')
estimates <- plotResults(data3, propertyToTake, start, end)
write.table(estimates, file=paste("output data/", paste(name,"_estimates.csv", sep=""), sep=""), quote=FALSE, sep="\t", col.names=TRUE, row.names=FALSE)
