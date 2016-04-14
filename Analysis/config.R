
#linux
#fileName <- "/home/kresimir/Projects/FormatAnalysis/fmts-cleaned.tsv"
#mac
fileName <- "/Users/kresimir/Projects/Format Analysis/fmts-cleaned.tsv"
#test
#fileName <- "/home/kresimir/Projects/FormatAnalysis/test.tsv"

colNames <- c("server", "tika", "droid", "year", "amount")

#market definition
name <- "DISTILLER"
groupFile <- "Format markets - DISTILLER.tsv"

#years used to estimate the model
start <- 1994
end <- 2010

#type of interval calculated: "none", "confidence" or "prediction"
intervalType <- "confidence"
#alpha for the interval 
alphaInterval <- 0.05

#years for which predictions should be done
predictionYears <- c(2011)

#if a three year moving average should be used 
useMovingAverage <- FALSE

#plot settings
includeInterval <- TRUE
includeRateOfChange <- FALSE
includePoints <- TRUE
includeResidual <- TRUE

# load conflict resolution functions and assign them to variables to be used later
source('conflictResolution.R')
resolveConflicts <- conflictResolution
afterConflictsResolution <- afterResolution
conflictCategory <- conflictCategoryDetermination
