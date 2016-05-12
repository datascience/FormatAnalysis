# this script is used to gather all selected models from 
# experiments 

experiments <- c("PDFS", "DISTILLER", "DOCUMENTS", "IMAGES", "HTML")
type <- c("VERSION", "TOOL", "FORMAT", "FORMAT", "VERSION")

allEstimates <- data.frame(market=character(), type=character(), name=character(), pStart=numeric(), qStart=numeric(), mStart=numeric(), 
                          p=numeric(), q=numeric(), m=numeric(), qprat=numeric(), release.year=numeric(), 
                          ages=numeric(), RMSEreal=numeric())

for (i in 1:length(experiments)) {
 
  experimentName <- experiments[i]
  exType <- type[i]
  path <- paste("output data/", experimentName, sep="")
  
  # load needed data 
  pathMarketElements <- paste(path, "/market elements/", sep="")
  bestModelEstimates <- readRDS(paste(pathMarketElements, "bestModelEstimates.rds",sep=""))
  dataShares <- read.table(paste(path, "/adoptionRates.tsv", sep=""), header=TRUE, sep="\t", stringsAsFactors=FALSE)
  
  # pick estimated values for selected models and plot them 
  chosenModels <- read.table(paste(pathMarketElements, "selectedModels.tsv", sep=""), header=TRUE, sep="\t", stringsAsFactors=FALSE)
  if (TRUE %in% is.na(chosenModels$modelID)) {
    message(paste("Please select models for ", experimentName, " experiment!\n", sep=""))
    next  
  }
  estimatesFinal <- merge(bestModelEstimates,chosenModels, by=c("ID", "modelID", "name"))
  estimatesFinal <- estimatesFinal[,names(estimatesFinal) %in% c("name", "pStart", "qStart", "mStart", 
                                                                 "p", "q", "m", "qprat", "release.year", "ages", "RMSEreal")]
  estimatesFinal$market <- experimentName
  estimatesFinal$type <- exType 
  allEstimates <- rbind(allEstimates, estimatesFinal)
}

allEstimates$TTP <-  (log(allEstimates$q) - log(allEstimates$p))/(allEstimates$p+allEstimates$q)
allEstimates$peak <- (allEstimates$m*(allEstimates$p+allEstimates$q)^2)/(4*allEstimates$q)
allEstimates$numPoints <- unlist(lapply(allEstimates$ages, length))
allEstimates$rmsePeak <- allEstimates$RMSEreal / allEstimates$peak

# allEstimates <- allEstimates[,names(allEstimates) %in% c("market", "name", "pStart", "qStart", "mStart", 
#                                                         "p", "q", "m", "qprat", "release.year", "numPoints", "TTP", 
#                                                         "peak", "RMSEreal", "rmsePeak")]

allEstimates <- allEstimates[,c("market", "name", "type", "pStart", "qStart", "mStart", 
                                "p", "q", "m", "qprat", "release.year", "numPoints", "TTP", 
                                "peak", "RMSEreal", "rmsePeak")]

write.table(allEstimates, file=paste("output data/", "allExperiments.tsv", sep=""), 
            quote=FALSE, sep="\t", col.names=TRUE, row.names=FALSE)


library(ggplot2)

scDF <- allEstimates[,c("market", "type", "p", "q")]

scaterPlotMarket <- ggplot(scDF, aes(x=p, y=q, color=market)) + geom_point()
png(filename = "output data/scaterMarket.png", width = 1880, height = 1600, res=300)
print(scaterPlotMarket)
dev.off()

scaterPlotType <- ggplot(scDF, aes(x=p, y=q, color=type)) + geom_point()
png(filename = "output data/scaterType.png", width = 1880, height = 1600, res=300)
print(scaterPlotType)
dev.off()

scDF2 <- allEstimates[,c("type", "release.year", "TTP", "peak")]
miY <- min(scDF2$release.year)
maY <- max(scDF2$release.year)
scDF2$release.year <- as.numeric(scDF2$release.year)
number_ticks <- function(n) {function(limits) pretty(limits, n)}
scaterPlotPeak <- ggplot(scDF2, aes(x=release.year, y=TTP, color=type, size=peak)) + geom_point() +
  labs(x="release year", y="time to peak", size="peak (max=1000)") + 
  scale_x_continuous(breaks = number_ticks(10))
png(filename = "output data/scaterPeak.png", width = 1880, height = 1200, res=300)
print(scaterPlotPeak)
dev.off()

scaterPlotMarket <- ggplot(scDF[scDF$type=="FORMAT",], aes(x=p, y=q, color=market)) + geom_point()
png(filename = "output data/scaterFormatMarket.png", width = 1880, height = 1600, res=300)
print(scaterPlotMarket)
dev.off()

scaterPlotMarket <- ggplot(scDF[scDF$type=="VERSION",], aes(x=p, y=q, color=market)) + geom_point()
png(filename = "output data/scaterVersionMarket.png", width = 1880, height = 1600, res=300)
print(scaterPlotMarket)
dev.off()

scaterPlotMarket <- ggplot(scDF[scDF$type=="TOOL",], aes(x=p, y=q, color=market)) + geom_point()
png(filename = "output data/scaterToolMarket.png", width = 1880, height = 1600, res=300)
print(scaterPlotMarket)
dev.off()



scDF3 <- allEstimates[,c("type", "market", "release.year", "TTP", "peak", "qprat")]
scDF3 <- scDF3[scDF3$market %in% c("DISTILLER","PDFS","HTML"),]
scDF3 <- scDF3[!(scDF3$market=="DISTILLER" & scDF3$release.year==1993),]
scDF3 <- scDF3[!(scDF3$market=="PDFS" & scDF3$release.year==1993),]
scDF3$release.year <- as.numeric(scDF3$release.year)
scDF3$qprat <- as.numeric(scDF3$qprat)
scaterPlotQPRat <- ggplot(scDF3, aes(x=release.year, y=qprat, color=market)) + geom_point() +
  geom_smooth(method = "lm", se=FALSE, linetype=3)
png(filename = "output data/scaterQPRats.png", width = 1880, height = 1600, res=300)
print(scaterPlotQPRat)
dev.off()

scaterPlotQPRat2 <- ggplot(scDF3, aes(x=release.year, y=qprat, color=market)) + geom_point() +
  geom_smooth(method = "lm", formula = y~exp(x),  se=FALSE, linetype=3)
png(filename = "output data/scaterQPRatsExp.png", width = 1880, height = 1600, res=300)
print(scaterPlotQPRat2)
dev.off()
