# script for preparing stuff for the web application

experiments <- c("ARCHIVE", "AUDIO", "BMPS", "DISTILLER", "DOCUMENTS",
                 "FLASH", "GIFS", "HTML", "IMAGES", "PDFS", "VIDEO")

allExperiments <- NA

for (i in 1:length(experiments)) {
  experimentName <- experiments[i]
  path <- paste("output data/", experimentName, sep="")
  
  pathGraphElements <- paste(path, "/graphs/", sep="")
  bestModelEstimates <- readRDS(paste(pathGraphElements, "estimatesFinal.rds",sep=""))
  
  pathPredictionElements <- paste(path, "/prediction/", sep="")
  predictionEstimates <- readRDS(paste(pathPredictionElements, "predictionEstimates.rds",sep=""))
  
  colnames(predictionEstimates)[which(names(predictionEstimates) == "upper")] <- "upperPrediction"
  colnames(predictionEstimates)[which(names(predictionEstimates) == "lower")] <- "lowerPrediction"
  
  predictionEstimates <- predictionEstimates[,names(predictionEstimates) %in% c("ID", "modelID", "name", "upperPrediction", 
                                                                                "lowerPrediction", "predictedValues", 
                                                                                "predictedLow", "predictedHigh", "yearsToPredict")]
  
  bestModelEstimates <- bestModelEstimates[,!(names(bestModelEstimates) %in% c("predictedValues", "predictedLow", "predictedHigh", 
                                                                                "yearsToPredict"))]
  
  bestModelEstimates <- merge(bestModelEstimates, predictionEstimates, by = c("ID", "modelID", "name"))
  
  bestModelEstimates$market <- experimentName
  
  if (i==1) {
    allExperiments <- bestModelEstimates
  } else {
    allExperiments <- rbind(allExperiments,bestModelEstimates)
  }
  
}
allExperiments$ID <- 1:nrow(allExperiments)
saveRDS(allExperiments, paste("output data/allExperimentsCombined.rds"))


  