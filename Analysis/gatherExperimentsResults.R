# this script is used to gather all selected models from 
# experiments 

#experiments <- c("PDFS", "DISTILLER", "DOCUMENTS", "IMAGES", "HTML")
#type <- c("VERSION", "TOOL", "FORMAT", "FORMAT", "VERSION")

experiments <- c("ARCHIVE-2", "AUDIO-2", "BMPS-2", "DISTILLER-2", "DOCUMENTS-2",
                 "FLASH-2", "GIFS-2", "HTML-2", "IMAGES-2", "PDFS-2", "VIDEO-2")
type <- c("VERSION", "FORMAT", "VERSION", "TOOL", "FORMAT", "VERSION", "VERSION", 
          "VERSION", "FORMAT", "VERSION", "FORMAT")

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
library(plyr)

# scatter for evrything colored by type and by market 
# also the same thing but filtered according to rmsePeak>0.3 or numPoints<7

# function for finding convex hull 
findHull <- function(df) df[chull(df$q, df$p),]

scDF <- allEstimates[,c("market", "type", "p", "q", "numPoints", "rmsePeak")]

hulls1 <- ddply(scDF, "market", findHull)
scatterPlotMarket <- ggplot(scDF, aes(x=q, y=p, color=market, fill=market)) + geom_point() + 
  geom_polygon(data=hulls1, alpha=0.3, color=NA)
png(filename = "output data/scatterAllMarket.png", width = 1800, height = 900, res=300)
print(scatterPlotMarket)
dev.off()

hulls2 <- ddply(scDF, "type", findHull)
scatterPlotType <- ggplot(scDF, aes(x=q, y=p, color=type, fill=type)) + geom_point() +
  geom_polygon(data=hulls2, alpha=0.3, color=NA)
png(filename = "output data/scatterAllType.png", width = 1800, height = 900, res=300)
print(scatterPlotType)
dev.off()

#filtering 
scDF <- scDF[!(scDF$rmsePeak>0.3 | scDF$numPoints<7),]

hulls3 <- ddply(scDF, "market", findHull)
scatterPlotMarketFiltered <- ggplot(scDF, aes(x=q, y=p, color=market, fill=market)) + geom_point() +
  geom_polygon(data=hulls3, alpha=0.3, color=NA)
png(filename = "output data/scatterAllMarketFiltered.png", width = 1800, height = 900, res=300)
print(scatterPlotMarketFiltered)
dev.off()

hulls4 <- ddply(scDF, "type", findHull)
scatterPlotTypeFiltered <- ggplot(scDF, aes(x=q, y=p, color=type, fill=type)) + geom_point() +
  geom_polygon(data=hulls3, alpha=0.3, color=NA)
png(filename = "output data/scatterAllTypeFiltered.png", width = 1800, height = 900, res=300)
print(scatterPlotTypeFiltered)
dev.off()



# bubble chart of  release.year against time to peak with peak for bubble maginitute  
scDF2 <- allEstimates[,c("type", "release.year", "TTP", "peak")]
miY <- min(scDF2$release.year)
maY <- max(scDF2$release.year)
scDF2$release.year <- as.numeric(scDF2$release.year)
number_ticks <- function(n) {function(limits) pretty(limits, n)}
scatterPlotPeak <- ggplot(scDF2, aes(x=release.year, y=TTP, color=type, size=peak)) + geom_point() +
  labs(x="release year", y="time to peak", size="peak (max=1000)") + 
  scale_x_continuous(breaks = number_ticks(10))
png(filename = "output data/bubblePeak.png", width = 1800, height = 900, res=300)
print(scatterPlotPeak)
dev.off()

# scatter plots filtered according to type
scatterPlotFormatMarket <- ggplot(scDF[scDF$type=="FORMAT",], aes(x=q, y=p, color=market)) + geom_point()
png(filename = "output data/scatterFormatMarket.png", width = 1800, height = 900, res=300)
print(scatterPlotFormatMarket)
dev.off()

scatterPlotVersionMarket <- ggplot(scDF[scDF$type=="VERSION",], aes(x=q, y=p, color=market)) + geom_point()
png(filename = "output data/scatterVersionMarket.png", width = 1800, height = 900, res=300)
print(scatterPlotVersionMarket)
dev.off()

scaterPlotToolMarket <- ggplot(scDF[scDF$type=="TOOL",], aes(x=q, y=p, color=market)) + geom_point()
png(filename = "output data/scatterToolMarket.png", width = 1800, height = 900, res=300)
print(scaterPlotToolMarket)
dev.off()


# scatter plots for qp-ratio with regression (linear and exponentail) lines added to them 

scDF3 <- allEstimates[,c("type", "market", "release.year", "TTP", "peak", "qprat")]
scDF3 <- scDF3[scDF3$market %in% c("DISTILLER","PDFS","HTML"),]
scDF3 <- scDF3[!(scDF3$market=="DISTILLER" & scDF3$release.year==1993),]
scDF3 <- scDF3[!(scDF3$market=="PDFS" & scDF3$release.year==1993),]
scDF3$release.year <- as.numeric(scDF3$release.year)
scDF3$qprat <- as.numeric(scDF3$qprat)
scatterPlotQPRat <- ggplot(scDF3, aes(x=release.year, y=qprat, color=market)) + geom_point() +
  geom_smooth(method = "lm", se=FALSE, linetype=3)
png(filename = "output data/scatterPlotQPRatsLin.png", width = 1800, height = 900, res=300)
print(scatterPlotQPRat)
dev.off()

scatterPlotQPRat2 <- ggplot(scDF3, aes(x=release.year, y=qprat, color=market)) + geom_point() +
  geom_smooth(method = "nls", formula = y~exp(a*x-b), method.args=list(start=c(a=1,b=1995)),  se=FALSE, linetype=3)
png(filename = "output data/scatterQPRatsExp.png", width = 1800, height = 900, res=300)
print(scatterPlotQPRat2)
dev.off()


#geom_point(data=dfCenters, aes(x=m, y=qprat), color=I("black"))

scDF4 <- allEstimates[,c("type", "m", "qprat", "rmsePeak")]
scDF4 <- scDF4[scDF4$rmsePeak<0.5 & scDF4$qprat<10000 & scDF4$m<1000,]
scDF4$m <- log10(scDF4$m)
kmeansRes <- kmeans(scDF4[,c("m","qprat")],3, iter.max=100000 )
scDF4$clusters <- as.factor(unlist(kmeansRes["cluster"]))
centers <- kmeansRes["centers"]
print(centers)
dfCenters <- data.frame(centers)
colnames(dfCenters) <- c("m", "qprat")
dfCenters$m <- as.numeric(dfCenters$m)
dfCenters$qprat <- as.numeric(dfCenters$qprat)
scatterPlotMQPRatio <- ggplot(scDF4, aes(x=m, y=qprat, color=clusters)) + geom_point() + 
    stat_ellipse()
png(filename = "output data/scatterPlotMQPRatio.png", width = 1800, height = 900, res=300)
print(scatterPlotMQPRatio)
dev.off()


scDF5 <- allEstimates[,c("market", "type", "p", "q", "m", "qprat", "numPoints", "rmsePeak")]
scDF5100 <- scDF5[scDF5$m<100 & scDF5$rmsePeak<0.5 & scDF5$qprat<10000,]
kmeansRes <- kmeans(scDF5100[,c("p","q")],3,iter.max=100000 , nstart=10 )
scDF5100$clusters <- as.factor(unlist(kmeansRes["cluster"]))
scatterPlotM100 <- ggplot(scDF5100, aes(x=q, y=p, color=clusters)) + geom_point() + 
  stat_ellipse(geom = "polygon", alpha=0.2, aes(fill=clusters), level = 0.9)
png(filename = "output data/scatterPlotM100.png", width = 1800, height = 900, res=300)
print(scatterPlotM100)
dev.off()

scDF52000 <- scDF5[scDF5$m>100 & scDF5$m<2000 & scDF5$rmsePeak<0.5 & scDF5$qprat<10000,]
kmeansRes <- kmeans(scDF52000[,c("p","q")],3,iter.max=100000, nstart = 10)
scDF52000$clusters <- as.factor(unlist(kmeansRes["cluster"]))
scatterPlotM2000 <- ggplot(scDF52000, aes(x=q, y=p, color=clusters)) + geom_point() + 
  stat_ellipse(geom = "polygon", alpha=0.2, aes(fill=clusters), level = 0.9)
png(filename = "output data/scatterPlotM2000.png", width = 1800, height = 900, res=300)
print(scatterPlotM2000)
dev.off()

scDF520000 <- scDF5[scDF5$m>2000 & scDF5$m<20000 & scDF5$rmsePeak<0.5 & scDF5$qprat<10000,]
kmeansRes <- kmeans(scDF520000[,c("p","q")],3,iter.max=100000, nstart = 10 )
scDF520000$clusters <- as.factor(unlist(kmeansRes["cluster"]))
scatterPlotM20000 <- ggplot(scDF520000, aes(x=q, y=p, color=clusters)) + geom_point() + 
  stat_ellipse(geom = "polygon", alpha=0.2, aes(fill=clusters), level = 0.9) 
  #scale_x_continuous(limits = c(-0.2,1.1*max(scDF520000$q))) +
  #scale_y_continuous(limits = c(-0.2,1.1*max(scDF520000$p))) +
png(filename = "output data/scatterPlotM20000.png", width = 1800, height = 900, res=300)
print(scatterPlotM20000)
dev.off()

scDF6 <- allEstimates[,c("market", "type", "p", "q", "m", "qprat", "numPoints", "rmsePeak")]
scDF6 <- scDF6[scDF6$rmsePeak<0.5 & scDF6$qprat<10000,]
scDF6$sizeGroup <- NA
scDF6[scDF6$m<100,]$sizeGroup <- "1-small"
scDF6[scDF6$m>100 & scDF6$m<2000,]$sizeGroup <- "2-medium"
scDF6[scDF6$m>2000 & scDF6$m<20000,]$sizeGroup <- "3-large"
kmeansRes <- kmeans(scDF6[,c("p","q", "m")],6,iter.max=100000 , nstart=10 )
scDF6$m <- log10(scDF6$m)
#scDF6$p <- log10(scDF6$p)
scDF6$clusters <- as.factor(unlist(kmeansRes["cluster"]))
scatterPlotPQM <- ggplot(scDF6, aes(x=q, y=p, shape=sizeGroup)) + geom_point() +
  scale_shape_manual(values = c(4, 1, 15)) + labs(shape="Market potential")
  #stat_ellipse(geom = "polygon", alpha=0.2, aes(fill=clusters), level = 0.9) 
png(filename = "output data/scatterPlotPQM.png", width = 1800, height = 900, res=300)
print(scatterPlotPQM)
dev.off()

scDF7 <- allEstimates[,c("market", "type", "p", "q", "m", "qprat", "numPoints", "rmsePeak")]
scDF7 <- scDF7[scDF7$rmsePeak<0.5 & scDF7$qprat<10000 & scDF7$type=="FORMAT",]
scDF7$sizeGroup <- NA
scDF7[scDF7$m<100,]$sizeGroup <- "1-small"
scDF7[scDF7$m>100 & scDF7$m<2000,]$sizeGroup <- "2-medium"
scDF7[scDF7$m>2000 & scDF7$m<20000,]$sizeGroup <- "3-large"
kmeansRes <- kmeans(scDF7[,c("p","q","m")],4,iter.max=100000 , nstart=10 )
scDF7$m <- log10(scDF7$m)
#scDF7$p <- log10(scDF6$p)
scDF7$clusters <- as.factor(unlist(kmeansRes["cluster"]))
scatterPlotPQM <- ggplot(scDF7, aes(x=q, y=p, color=clusters, size=sizeGroup)) + geom_point()  
#stat_ellipse(geom = "polygon", alpha=0.2, aes(fill=clusters), level = 0.9) 
png(filename = "output data/scatterPlotPQClusterFormats.png", width = 1800, height = 900, res=300)
print(scatterPlotPQM)
dev.off()



