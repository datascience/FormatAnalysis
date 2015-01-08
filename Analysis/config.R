# config file with all configurations 

file <- "/home/kresimir/Projects/FormatAnalysis/fmts-cleaned.tsv"
#file <- "/home/kresimir/Projects/FormatAnalysis/test.tsv"


colNames <- c("server", "tika", "droid", "year", "amount")



fileData <- read.table("input data/PDFS_software.txt", header=TRUE, sep="\t", stringsAsFactors=FALSE) 

releases <- read.table("input data/release_years_software.txt", header=TRUE, sep="\t", stringsAsFactors=FALSE)

source('conflictResolution.R')
resolveConflictsMime <- resolveConflictsMimeDefault
resoresolveConflictsProperty <- resolveConflictsPropertyDefault
