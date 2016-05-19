source('config.R')
source('prepareData.R')



if (length(experiments)==length(marketFiles)) {
  dir.create("output data")
  
  for (i in 1:length(experiments)) {
    experimentName <- experiments[i]
    marketFile <- marketFiles[i]
    path <- paste("output data/", experimentName, "/", sep="")
    
    # if experiment folder does not exsits create it 
    if (!file.exists(path)){
      dir.create(path)
    }
    print(experimentName)
    
    # check for the experiment configuration 
    pathConfig <- paste(path, "experimentConfig.tsv", sep="")
    if (file.exists(pathConfig)) {
      cpdf <- read.table(pathConfig, header=TRUE, sep="\t", stringsAsFactors=FALSE)
      print(path)
      print(pathConfig)
      if (cpdf[cpdf$parameter=="file",]$value!=marketFile | cpdf[cpdf$parameter=="start year",]$value!=start |
          cpdf[cpdf$parameter=="end year",]$value!=end | cpdf[cpdf$parameter=="moving average used",]$value!=useMovingAverage |
          cpdf[cpdf$parameter=="multiplication factor",]$value!=multiplicationFactor) {
        # delete everything that exists 
        unlink(path, recursive = TRUE)
        dir.create(path)
        # save config parameters from the experiment 
        cpdf <- data.frame(parameter=c("experiment name", "file", "start year", "end year", "moving average used", "prediction years", "multiplication factor"), 
                           value=c(experimentName, marketFile, start, end, useMovingAverage, paste(predictionYears, collapse=","), multiplicationFactor))
        write.table(cpdf, file=pathConfig, quote=FALSE, sep="\t", 
                    col.names=TRUE, row.names=FALSE)
      }
    } else {
      # if experimentConfig.tsv does not exsist delete everything else
      unlink(path, recursive = TRUE)
      dir.create(path)
      cpdf <- data.frame(parameter=c("experiment name", "file", "start year", "end year", "moving average used", "prediction years", "multiplication factor"), 
                         value=c(experimentName, marketFile, start, end, useMovingAverage, paste(predictionYears, collapse=","), multiplicationFactor))
      write.table(cpdf, file=pathConfig, quote=FALSE, sep="\t", 
                  col.names=TRUE, row.names=FALSE)
    }
    rm(pathConfig)
    
    # load market data 
    # load it twice to see which columns it has
    marketData <- read.table(paste("input data/", marketFile, sep=""), header=TRUE, sep="\t", stringsAsFactors=FALSE)
    marketData <- read.table(paste("input data/", marketFile, sep=""), header=TRUE, 
                             colClasses=rep("character", each=ncol(marketData)), sep="\t", stringsAsFactors=FALSE)
    
    # calculate properties to take TODO explain exclusion from the tsv file 
    nam <- names(marketData)
    propertyToTake <- nam[!(nam %in% c("ID", "name", "release.year", "comments"))]
    
    # check if dataCleaned.tsv exsists and if yes load it 
    # otherwise prepare data 
    dataCleaned <- NA
    pathDataCleaned <- paste(path,"cleanedData.tsv",sep="")
    if (file.exists(pathDataCleaned)) {
      dataCleaned <- read.table(pathDataCleaned, header=TRUE, sep="\t", stringsAsFactors=FALSE)
      cat("Cleaned data loaded from the disk")
    } else {
      # discard those elements from the market where properties, according to which filtering should
      # be done, are missing
      source('discardNotComplete.R')
      marketData <- discardNotComplete(marketData, propertyToTake)
      write.table(marketData, file=paste(path, "market.tsv", sep=""), quote=FALSE, sep="\t", 
                  col.names=TRUE, row.names=FALSE)
      
      # make a list from multiple mime and other property values
      for (prop in propertyToTake) {
        marketData[[prop]] <- trimws(marketData[[prop]])
        marketData[[prop]] <- strsplit(marketData[[prop]], split = ",")
      }
      
      # load raw data, filter according to properties, reduce conflicts calculate age and save the resulting dataset to a file  
      source("conflictResolution.R")
      dataCleaned <- prepareData(fileName, colNames, marketData, propertyToTake, resolveConflicts, 
                                 afterConflictsResolution, conflictCategory)
      
      write.table(dataCleaned, file=paste(path,"cleanedData.tsv",sep=""), quote=FALSE, 
                  sep="\t", col.names=TRUE, row.names=FALSE)
    }
    rm(pathDataCleaned)
    
    pathDataShares <- paste(path, "adoptionRatse.tsv", sep="")
    dataShares <- NA
    if (file.exists(pathDataShares)) {
      dataShares <- read.table(pathDataShares, header=TRUE, sep="\t", stringsAsFactors=FALSE) 
    } else {
      # calculate percentage and moving average of each value
      source('normalizeMarket.R')
      dataShares <- normalizeMarket(dataCleaned,propertyToTake, multiplicationFactor)
      write.table(dataShares, file=paste(path, "adoptionRates.tsv", sep=""), 
                  quote=FALSE, sep="\t", col.names=TRUE, row.names=FALSE)
    }
    rm(pathDataShares)
    
    # estimating models, plotting results and making predictions 
    source('estimateModelParameters.R')
    source('plotResults.R')
    
    # create folder where all models will be saved 
    pathMarketElements <- paste(path, "market elements/", sep="")
    if (!file.exists(pathMarketElements)){
      dir.create(pathMarketElements)
    } 
    
    pathAllModelEstimates <- paste(pathMarketElements, "allModelEstimates.rds", sep="")
    allModelEstimates <- NA
    if (file.exists(pathAllModelEstimates)){
      allModelEstimates <- readRDS(pathAllModelEstimates)
    } else {
      # for each element estimate models 
      allModelEstimates <- estimateModelParameters(dataShares, start, end, useMovingAverage, multiplicationFactor, pathMarketElements)
      saveRDS(allModelEstimates, file=paste(pathMarketElements, "allModelEstimates.rds", sep=""))
    }
    rm(pathAllModelEstimates)
    
    pathBestModelEstimates <- paste(pathMarketElements, "bestModelEstimates.rds", sep="")
    bestModelEstimates <- NA
    if (file.exists(pathBestModelEstimates)) {
      bestModelEstimates <- readRDS(pathBestModelEstimates)
    } else {
      bestModelEstimates <- calculateBestModelValues(dataShares, allModelEstimates, pathMarketElements)
      saveRDS(bestModelEstimates, file=paste(pathMarketElements, "bestModelEstimates.rds", sep="")) 
    }
    
    fileGraphsDone <- paste(pathMarketElements, "graphsDone.txt", sep="")
    # plot best models
    if (!file.exists(fileGraphsDone)) {
      graphAllModels(dataShares, bestModelEstimates, pathMarketElements, experimentName)
      file.create(fileGraphsDone)
        # sync graphs to the selected folder 
        for (name in unique(dataShares$name) ) {
          pathFrom <- paste(pathMarketElements, name, "/graphs", sep="")
          print(pathFrom)
          file.copy(from=list.files(pathFrom, full.names = TRUE) , to=syncFolder, 
                    overwrite = TRUE, recursive = TRUE, 
                    copy.mode = TRUE)
        }
    }
    
  }
  
  
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
    } else {
      if (!dir.exists(paste(path, "/graphs", sep=""))) {
        estimatesFinal <- merge(bestModelEstimates,chosenModels, by=c("ID", "modelID", "name"))
        plotResults(estimatesFinal, "selected", TRUE, FALSE, FALSE, path, experimentName)    
      }
      if (!dir.exists(paste(path, "/prediction", sep=""))) {
        # make predictions and plot prediction results 
        predictions <- makePredictions(dataShares, estimatesFinal, chosenModels, predictionYears, path, pathMarketElements)
        plotResults(predictions, "predicted", FALSE, TRUE, TRUE, paste(path,"/prediction",sep=""), experimentName)
      }
    }
  }
  
  source("estimateModelParameters.R")
  # do the cross validation on selected models
  for (i in 1:length(experiments)) {
    experimentName <- experiments[i]
    path <- paste("output data/", experimentName, "/", sep="")
    print(path)
    if (!dir.exists(paste(path,"cross-validation", sep=""))) {
      
      # load needed data 
      pathMarketElements <- paste(path, "market elements/", sep="")
      bestModelEstimates <- readRDS(paste(pathMarketElements, "bestModelEstimates.rds",sep=""))
      dataShares <- read.table(paste(path, "adoptionRates.tsv", sep=""), header=TRUE, sep="\t", stringsAsFactors=FALSE)
      
      # pick estimated values for selected models and plot them
      if (file.exists(paste(pathMarketElements, "selectedModels.tsv", sep=""))) {
        chosenModels <- read.table(paste(pathMarketElements, "selectedModels.tsv", sep=""), header=TRUE, sep="\t", stringsAsFactors=FALSE)
        estimatesFinal <- merge(bestModelEstimates,chosenModels, by=c("ID", "modelID", "name"))
        print(chosenModels)
      }
      
      crossValidate(dataShares, estimatesFinal, path, experimentName)        
      
    }
  }
  
#   syncFolder <- "/Users/kresimir/Dropbox/Work/Projects/BenchmarkDP/formatanalysis/figures/201605 figures/20160506-1200/model figures/"
#   for (i in 1:length(experiments)) {
#     experimentName <- experiments[i]
#     path <- paste("output data/", experimentName, "/", sep="")
#     print(path)
#     dataShares <- read.table(paste(path, "adoptionRates.tsv", sep=""), header=TRUE, sep="\t", stringsAsFactors=FALSE)
#     pathElements <- paste("output data/", experimentName, "/market elements", sep="")
#     
#     for (name in unique(dataShares$name) ) {
#       pathFrom <- paste(pathElements, "/", name, "/graphs", sep="")
#       print(pathFrom)
#       file.copy(from=list.files(pathFrom, full.names = TRUE) , to=syncFolder, 
#                 overwrite = TRUE, recursive = TRUE, 
#                 copy.mode = TRUE)
#     }
#     
#     pathAllGraphs <- paste(path, "graphs", sep="")
#     print(pathAllGraphs)
#     file.copy(from=list.files(pathAllGraphs, full.names = TRUE) , to=syncFolder, 
#               overwrite = TRUE, recursive = TRUE, 
#               copy.mode = TRUE)
#     
#   }
#   
  
} else {
  message("The number of experiments and market files does not match! Check experiments and marketFiles variables in the config.R file")
}








