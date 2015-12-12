
#linux
#fileName <- "/home/kresimir/Projects/FormatAnalysis/fmts-cleaned.tsv"
#mac
fileName <- "/Users/kresimir/Projects/Format Analysis/fmts-cleaned.tsv"
#test
#fileName <- "/home/kresimir/Projects/FormatAnalysis/test.tsv"

colNames <- c("server", "tika", "droid", "year", "amount")

name <- "IMAGES"
groupFile <- "Format markets - IMAGES.tsv"
start <- 1994
end <- 2010

#plot settings
includeInterval <- FALSE
includeRateOfChange <- TRUE
includePoints <- TRUE
includeResidual <- TRUE

source('conflictResolution.R')
resolveConflicts <- conflictResolution
afterConflictsResolution <- afterResolution
conflictCategory <- conflictCategoryDetermination
