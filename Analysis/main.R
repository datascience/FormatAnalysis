source('config.R')
source('prepareData.R')



if (length(experiments)==length(marketFiles)) {
  dir.create("output data")
  for (i in 1:length(experiments)) {
    experimentName <- experiments[i]
    path <- paste("output data/", experimentName, "/", sep="")
    #if the folder already exists delete all the content 
    if (file.exists(path)) {
      unlink(path, recursive = TRUE)
    }
    # create expeirment folder 
    dir.create(path)
    
    marketFile <- marketFiles[i]

    # save the config parameters from the experiment 
    cpdf <- data.frame(parameter=c("experiment name", "file", "start year", "end year", "moving average used", "prediction years", "multiplication factor"), 
                       value=c(experimentName, marketFile, start, end, useMovingAverage, paste(predictionYears, collapse=","), multiplicationFactor))
    write.table(cpdf, file=paste(path, "experimentConfig.tsv", sep=""), quote=FALSE, sep="\t", 
                col.names=FALSE, row.names=FALSE)
    
    
    # load market data 
    # load it twice to see which columns it has
    marketData <- read.table(paste("input data/", marketFile, sep=""), header=TRUE, sep="\t", stringsAsFactors=FALSE)
    marketData <- read.table(paste("input data/", marketFile, sep=""), header=TRUE, 
                            colClasses=rep("character", each=ncol(marketData)), sep="\t", stringsAsFactors=FALSE)
    

    # calculate properties to take TODO explain exclusion from the tsv file 
    nam <- names(marketData)
    propertyToTake <- nam[!(nam %in% c("ID", "name", "release.year", "comments"))]  
    
    # discard those elements from the market where properties, according to which filtering should
    # be done, are missing
    source('discardNotComplete.R')
    marketData <- discardNotComplete(marketData, propertyToTake)
    write.table(marketData, file=paste(path, "market.tsv", sep=""), quote=FALSE, sep="\t", 
                col.names=TRUE, row.names=FALSE)
    
    # make a list from multiple mime and other property values
    for (prop in propertyToTake) {
      marketData[[prop]] <- strsplit(marketData[[prop]], split = ",")
    }
    
    # load raw data, filter according to properties, reduce conflicts calculate age and save the resulting dataset to a file  
    dataCleaned <- prepareData(fileName, colNames, marketData, propertyToTake, resolveConflicts, 
                     afterConflictsResolution, conflictCategory)
    write.table(dataCleaned, file=paste(path,"cleanedData.tsv",sep=""), quote=FALSE, 
                sep="\t", col.names=TRUE, row.names=FALSE)
    
    # calculate percentage and moving average of each value
    source('normalizeMarket.R')
    dataShares <- normalizeMarket(dataCleaned,propertyToTake, multiplicationFactor)
    write.table(dataShares, file=paste(path, "adoptionRates.tsv", sep=""), 
                quote=FALSE, sep="\t", col.names=TRUE, row.names=FALSE)
    
    
    # estimating models, plotting results and making predictions 
    source('estimateModelParameters.R')
    source('plotResults.R')
    
    # create folder where all models will be saved 
    pathMarketElements <- paste(path, "market elements/", sep="")
    dir.create(pathMarketElements)
    
    # for each element estimate models 
    allModelEstimates <- estimateModelParameters(dataShares, start, end, useMovingAverage, multiplicationFactor, pathMarketElements)
    saveRDS(allModelEstimates, file=paste(pathMarketElements, "allModelEstimates.rds", sep=""))
    bestModelEstimates <- calculateBestModelValues(dataShares, allModelEstimates, pathMarketElements)
    saveRDS(bestModelEstimates, file=paste(pathMarketElements, "bestModelEstimates.rds", sep="")) 
    # plot bets models
    graphAllModels(dataShares, bestModelEstimates, pathMarketElements)
  }
  
  #Ask user to select wanted models manually 
  readline(prompt = "Please for each experiment and each market element in that experiment select one model. 
           This is easily done by filling out selectedModels.tsv file. Press Enter when ready to proceed")
  
  for (i in 1:length(experiments)) {
    experimentName <- experiments[i]
    path <- paste("output data/", experimentName, "/", sep="")
    
    # load needed data 
    pathMarketElements <- paste(path, "market elements/", sep="")
    bestModelEstimates <- readRDS(paste(pathMarketElements, "bestModelEstimates.rds",sep=""))
    dataShares <- read.table(paste(path, "adoptionRates.tsv", sep=""), header=TRUE, sep="\t", stringsAsFactors=FALSE)
    
    # pick estimated values for selected models and plot them 
    chosenModels <- read.table(paste(pathMarketElements, "selectedModels.tsv", sep=""), header=TRUE, sep="\t", stringsAsFactors=FALSE)
    estimatesFinal <- merge(bestModelEstimates,chosenModels, by=c("ID", "modelID", "name"))
    plotResults(estimatesFinal, "separated", FALSE, TRUE, TRUE, path, experimentName)    

    # make predictions and plot prediction results 
    predictions <- makePredictions(dataShares, estimatesFinal, chosenModels, predictionYears, path, pathMarketElements)
    plotResults(predictions, "separated", FALSE, TRUE, TRUE, paste(path,"/prediction",sep="", experimentName))
    
  }
  
  syncFolder <- "/Users/kresimir/Dropbox/Work/Projects/BenchmarkDP/formatanalysis/figures/201605 figures/20160506-1200/model figures/"
  for (i in 1:length(experiments)) {
    experimentName <- experiments[i]
    path <- paste("output data/", experimentName, "/", sep="")
    print(path)
    dataShares <- read.table(paste(path, "adoptionRates.tsv", sep=""), header=TRUE, sep="\t", stringsAsFactors=FALSE)
    pathElements <- paste("output data/", experimentName, "/market elements", sep="")
    
    for (name in unique(dataShares$name) ) {
      pathFrom <- paste(pathElements, "/", name, "/graphs", sep="")
      print(pathFrom)
      file.copy(from=list.files(pathFrom, full.names = TRUE) , to=syncFolder, 
                overwrite = TRUE, recursive = TRUE, 
                copy.mode = TRUE)
    }
    
    pathAllGraphs <- paste(path, "graphs", sep="")
    print(pathAllGraphs)
    file.copy(from=list.files(pathAllGraphs, full.names = TRUE) , to=syncFolder, 
              overwrite = TRUE, recursive = TRUE, 
              copy.mode = TRUE)
    
  }
  
  
} else {
  message("The number of experiments and market files does not match! Check experiments and marketFiles variables in the config.R file")
}








