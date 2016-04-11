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
  groupData[[prop]] <- strsplit(groupData[[prop]], split = ",")
}

# load raw data, filter and reduce conflicts and save the resulting dataset to a file  
data <- loadData(fileName, colNames, groupData, propertyToTake, resolveConflicts, 
                 afterConflictsResolution, conflictCategory)
write.table(data, file=paste(path,"/cleaned_data.tsv",sep=""), quote=FALSE, 
            sep="\t", col.names=TRUE, row.names=FALSE)

# for (prop in propertyToTake) {
#   groupData[[prop]] <- sapply(groupData[[prop]], "[",1)
# }

# # calculate age 
# source('calculateAge.R')
# data2 <- calculateAge(data, groupData, propertyToTake)

# calculate percentage and moving average of each value
source('calculatePercentage.R')
data2 <- calculatePercentage(data,propertyToTake)
write.table(data2, file=paste(path, "/adoption.tsv", sep=""), 
            quote=FALSE, sep="\t", col.names=TRUE, row.names=FALSE)

# estimate the model and plot the curves
source('estimateModelParameters.R')
estimates <- estimateModelParameters(data2, propertyToTake, start, end, intervalType, alphaInterval, useMovingAverage)
estimatesPrint <- estimates[,names(estimates) %in% c("name", "a.linear1", "b.linear1", "MSE.linear1", "R2.linear1",
                                                     "a.linear2", "b.linear2", "c.linear2", "MSE.linear2", "R2.linear2",
                                                     "a.linear3", "b.linear3", "c.linear3", "d.linear3", "MSE.linear3", "R2.linear3",
                                                     "p.bass", "q.bass", "m.bass", "MSE.bass", "R2.bass")]
predictionsPrint <- estimates[,names(estimates) %in% c("name", "real-next", "prediction-next.linear1", "predictionLower-next.linear1", 
                                                       "predictionUpper-next.linear1", "prediction-next.linear2", 
                                                       "predictionLower-next.linear2", "predictionUpper-next.linear2",
                                                       "prediction-next.linear3", "predictionLower-next.linear3", 
                                                       "predictionUpper-next.linear3", 
                                                       "prediction-next.bass", "predictionLower-next.bass", 
                                                       "predictionUpper-next.bass")]
write.table(estimatesPrint, file=paste(path, "/estimates.csv", sep=""), 
            quote=FALSE, sep="\t", col.names=TRUE, row.names=FALSE)
write.table(predictionsPrint, file=paste(path, "/predictions.csv", sep=""), 
            quote=FALSE, sep="\t", col.names=TRUE, row.names=FALSE)

source('plotResults.R')
plotResults(estimates, c("bass","linear1","linear2","linear3"), includeRateOfChange, includeInterval, includePoints)

