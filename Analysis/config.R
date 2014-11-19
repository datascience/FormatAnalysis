# config file with all configurations 

file <- "/home/kresimir/Projects/FormatAnalysis/fmts-cleaned.tsv"
#file <- "/home/kresimir/Projects/FormatAnalysis/test.tsv"

colNames <- c("server", "tika", "droid", "year", "amount")

col <- "tika"
propertyToTake <- "version"

formats <- c("application/pdf")

releases <- read.table("release_years.txt", header=TRUE, sep="\t", stringsAsFactors=FALSE)
