# this script is used to gather all selected models from 
# experiments 

experiments <- c("IMAGES")

allEstimaes <- data.frame(market=character(), name=character(), pStart=numeric(), qStart=numeric(), mStart=numeric(), 
                          p=numeric(), q=numeric(), m=numeric(), qprat=numeric(), release.year=numeric(), 
                          ages=numeric(), RMSEreal=numeric())

for (i in 1:length(experiments)) {
 
  experimentName <- experiments[i]
  path <- paste("output data/", experimentName, sep="")
  
  # load needed data 
  pathMarketElements <- paste(path, "/market elements/", sep="")
  bestModelEstimates <- readRDS(paste(pathMarketElements, "bestModelEstimates.rds",sep=""))
  dataShares <- read.table(paste(path, "/adoptionRates.tsv", sep=""), header=TRUE, sep="\t", stringsAsFactors=FALSE)
  
  # pick estimated values for selected models and plot them 
  chosenModels <- read.table(paste(pathMarketElements, "selectedModels.tsv", sep=""), header=TRUE, sep="\t", stringsAsFactors=FALSE)
  if (TRUE %in% is.na(chosenModels$modelID)) {
    message(paste("Please select models for ", experimentName, " experiment!\n", sep=""))
    next  
  }
  estimatesFinal <- merge(bestModelEstimates,chosenModels, by=c("ID", "modelID", "name"))
  estimatesFinal <- estimatesFinal[,names(estimatesFinal) %in% c("name", "pStart", "qStart", "mStart", 
                                                                 "p", "q", "m", "qprat", "release.year", "ages", "RMSEreal")]
  estimatesFinal$market <- experimentName
  allEstimaes <- rbind(allEstimates, estimatesFinal)
}

allEstimaes$TTP <-  (log(allEstimaes$q) - log(allEstimates$p))/(allEstimaes$p+allEstimaes$q)
allEstimaes$peak <- (allEstimates$m*(allEstimates$p+allEstimates$q)^2)/(4*q)
allEstimaes$numPoints <- length(unlist(allEstimaes$ages))
allEstimates$rmsePeak <- allEstimaes$rmse / allEstimates$peak

write.table(allEstimates, file=paste("output data/", "allExperiments.tsv", sep=""), 
            quote=FALSE, sep="\t", col.names=TRUE, row.names=FALSE)






