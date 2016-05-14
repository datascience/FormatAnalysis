# this script is used to gather all cross-validation results from 
# experiments 

experiments <- c("PDFS", "DISTILLER", "DOCUMENTS", "IMAGES", "HTML")
type <- c("VERSION", "TOOL", "FORMAT", "FORMAT", "VERSION")

allCVResults <- NA

for (i in 1:length(experiments)) {
  
  experimentName <- experiments[i]
  exType <- type[i]
  path <- paste("output data/", experimentName, sep="")
  pathCVFile <- paste(path, "/cross-validation/", experimentName, "-joinedResults.tsv", sep="")
  cvTemp <- read.table(pathCVFile, header=TRUE, sep="\t", stringsAsFactors=FALSE)
  if (i==1) {
    allCVResults <- cvTemp
  } else {
    allCVResults <- merge(x=allCVResults, y=cvTemp, by="age", all=TRUE)
  }
}

write.table(allCVResults, file=paste("output data/", "allCVResults.tsv", sep=""), 
            quote=FALSE, sep="\t", col.names=TRUE, row.names=FALSE)


dfAllCV <- data.frame(product=character(39), total=numeric(39), positive=numeric(39))
dfAllCV$product <- names(allCVResults[,!(names(allCVResults)=="age")])
dfAllCV$total <- apply(allCVResults[,!(names(allCVResults)=="age")], 2, function(x) sum(!is.na(x)))
dfAllCV$positive <- apply(allCVResults[,!(names(allCVResults)=="age")], 2, function(x) length(x[x==TRUE & !is.na(x)]))
dfAllCV$ratio <- dfAllCV$positive/dfAllCV$total


library(ggplot2)
plot <- ggplot(dfAllCV, aes(x=total, y=ratio)) + geom_point()
png(filename = "output data/CVresults.png", width = 1800, height = 900, res=300)
print(plot)
dev.off()




