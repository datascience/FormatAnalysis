source('loadData.R')
source('utils.R')



file <- "/home/kresimir/Projects/FormatAnalysis/fmts-cleaned.tsv"
colNames <- c("server", "tika", "droid", "year", "amount")

fileData <- read.table("input data/IMAGES.txt", header=TRUE, sep="\t", colClasses=c("character"), stringsAsFactors=FALSE) 
releases <- read.table("input data/release_years_formats.txt", header=TRUE, sep="\t", stringsAsFactors=FALSE)
unification <- read.table("input data/unification_rules.txt", header=TRUE, sep="\t", stringsAsFactors=FALSE)
unification <- NA
source('conflictResolution.R')
resolveConflictsMime <- resolveConflictsMimeDefault
resoresolveConflictsProperty <- resolveConflictsPropertyDefault

nam <- names(fileData)
if (length(nam)==1) {
  propertyToTake <- NA
} else {
  propertyToTake <- nam[2:length(nam)]
}
data <- loadData(file, colNames, propertyToTake, resolveConflictsMime, resolveConflictsProperty, unification)

source('calculateAge.R')
data2 <- calculateAge(data, releases, propertyToTake)

source('calculatePercentage.R')
data3 <- calculatePercentage(data2,propertyToTake)

#source('estimateModel.R')
#models <- estimateModel(data3,propertyToTake)

source('plotResults.R')
estimates <- plotResults(data3, NA, propertyToTake)
write.table(estimates, file="images_estimates.txt", quote=FALSE, sep="\t", col.names=TRUE)
