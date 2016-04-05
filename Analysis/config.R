
#linux
#fileName <- "/home/kresimir/Projects/FormatAnalysis/fmts-cleaned.tsv"
#mac
fileName <- "/Users/kresimir/Projects/Format Analysis/fmts-cleaned.tsv"
#test
#fileName <- "/home/kresimir/Projects/FormatAnalysis/test.tsv"

colNames <- c("server", "tika", "droid", "year", "amount")

name <- "PDFS1"
groupFile <- "Format markets - PDF versions.tsv"
start <- 1994
end <- 2010

#plot settings
includeInterval <- TRUE
includeRateOfChange <- FALSE
includePoints <- TRUE
includeResidual <- TRUE

source('conflictResolution.R')
resolveConflicts <- conflictResolution
afterConflictsResolution <- afterResolution
conflictCategory <- conflictCategoryDetermination
