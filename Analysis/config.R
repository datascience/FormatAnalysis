
fileName <- "/home/kresimir/Projects/FormatAnalysis/fmts-cleaned.tsv"
#fileName <- "/home/kresimir/Projects/FormatAnalysis/test.tsv"

colNames <- c("server", "tika", "droid", "year", "amount")

name <- "PDF"
groupFile <- "Format markets - PDF versions.tsv"
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