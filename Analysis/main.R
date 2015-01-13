source('loadData.R')
source('utils.R')



file <- "/home/kresimir/Projects/BenchmarkDP/fmts-cleaned.tsv"
colNames <- c("server", "tika", "droid", "year", "amount")

fileData <- read.table("input data/PDFS_software.txt", header=TRUE, sep="\t", colClasses=c("character", "character"), stringsAsFactors=FALSE) 
releases <- read.table("input data/release_years.txt", header=TRUE, sep="\t", stringsAsFactors=FALSE)

source('conflictResolution.R')
resolveConflictsMime <- resolveConflictsMimeDefault
resoresolveConflictsProperty <- resolveConflictsPropertyDefault

propertyToTake <- names(fileData)[2]

data <- loadData(file, colNames, propertyToTake, resolveConflictsMime, resolveConflictsProperty)

source('calculateAge.R')
data2 <- calculateAge(data, releases, propertyToTake)

source('calculatePercentage.R')
data3 <- calculatePercentage(data2,propertyToTake)

source('estimateModel.R')
models <- estimateModel(data3,propertyToTake)

source('plotResults.R')
plotResults(data3, NA, propertyToTake)
