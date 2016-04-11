
#linux
#fileName <- "/home/kresimir/Projects/FormatAnalysis/fmts-cleaned.tsv"
#mac
fileName <- "/Users/kresimir/Projects/Format Analysis/fmts-cleaned.tsv"
#test
#fileName <- "/home/kresimir/Projects/FormatAnalysis/test.tsv"

colNames <- c("server", "tika", "droid", "year", "amount")

name <- "PDF - formats predictions"
groupFile <- "Format markets - PDF versions.tsv"
start <- 1994
end <- 2009

#type of interval calculated: "none", "confidence" or "prediction"
intervalType <- "prediction"
#alpha for the interval 
alphaInterval <- 0.05

#if a three year moving average should be used 
useMovingAverage <- FALSE

#plot settings
includeInterval <- TRUE
includeRateOfChange <- FALSE
includePoints <- TRUE
includeResidual <- TRUE

source('conflictResolution.R')
resolveConflicts <- conflictResolution
afterConflictsResolution <- afterResolution
conflictCategory <- conflictCategoryDetermination
