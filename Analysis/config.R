
#linux
#fileName <- "/home/kresimir/Projects/FormatAnalysis/fmts-cleaned.tsv"
#mac
fileName <- "/Users/kresimir/Projects/Format Analysis/fmts-cleaned.tsv"
#test
#fileName <- "/home/kresimir/Projects/FormatAnalysis/test.tsv"

#sync folder for gathering all plots 
syncFolder <- "/Users/kresimir/Dropbox/Work/Projects/BenchmarkDP/formatanalysis/figures/201605 figures/201605090800/models/"

colNames <- c("server", "tika", "droid", "year", "amount")

# market definition
experiments <- c("IMAGES")
marketFiles <- c("Format markets - IMAGES.tsv")

#years used to estimate the model
start <- 1994
end <- 2010


#years for which predictions should be done
predictionYears <- c(2009, 2010, 2011)

#if a three year moving average should be used 
useMovingAverage <- TRUE

# multiplication factor for normalizing markets sizes
multiplicationFactor <- 1000 


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
