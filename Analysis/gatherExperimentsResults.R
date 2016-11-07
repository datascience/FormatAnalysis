# this script is used to gather all selected models from
# experiments

#experiments <- c("PDFS", "DISTILLER", "DOCUMENTS", "IMAGES", "HTML")
#type <- c("VERSION", "TOOL", "FORMAT", "FORMAT", "VERSION")

market <- c(
  "ARCHIVE",
  "AUDIO",
  "BMPS",
  "DISTILLER",
  "DOCUMENTS",
  "FLASH",
  "GIFS",
  "HTML",
  "IMAGES",
  "PDFS",
  "VIDEO"
)
experiments <-
  c(
    "ARCHIVE",
    "AUDIO",
    "BMPS",
    "DISTILLER",
    "DOCUMENTS",
    "FLASH",
    "GIFS",
    "HTML",
    "IMAGES",
    "PDFS",
    "VIDEO"
  )
type <-
  c(
    "VERSION",
    "FORMAT",
    "VERSION",
    "TOOL",
    "FORMAT",
    "VERSION",
    "VERSION",
    "VERSION",
    "FORMAT",
    "VERSION",
    "FORMAT"
  )

allEstimates <-
  data.frame(
    market = character(),
    type = character(),
    name = character(),
    pStart = numeric(),
    qStart = numeric(),
    mStart = numeric(),
    p = numeric(),
    q = numeric(),
    m = numeric(),
    qprat = numeric(),
    release.year = numeric(),
    ages = numeric(),
    RMSEreal = numeric()
  )


dropValues <-
  data.frame(
    name = character(),
    mime = character(),
    percent2005 = numeric(),
    percent2010 = numeric(),
    absoluteDrop = numeric(),
    relativeDrop=numeric(),
    stringsAsFactors = FALSE
  )
dVIndex <- 1

for (i in 1:length(experiments)) {
  experimentName <- experiments[i]
  exType <- type[i]
  path <- paste("output data/", experimentName, sep = "")
  
  # load needed data
  pathMarketElements <- paste(path, "/market elements/", sep = "")
  bestModelEstimates <-
    readRDS(paste(pathMarketElements, "bestModelEstimates.rds", sep = ""))
  dataShares <-
    read.table(
      paste(path, "/adoptionRates.tsv", sep = ""),
      header = TRUE,
      sep = "\t",
      stringsAsFactors = FALSE
    )
  if (exType=="FORMAT") {
    for (n in unique(dataShares$name)) {
      dropValues[dVIndex,]$mime <- unique(dataShares[dataShares$name==n,]$mime)[1]
      dropValues[dVIndex,]$name <- n
      if (nrow(dataShares[dataShares$name==n & dataShares$year==2005,])>0) {
        dropValues[dVIndex,]$percent2005 <- dataShares[dataShares$name==n & dataShares$year==2005,]$adoptionRate / 1000;
      } else {
        dropValues[dVIndex,]$percent2005 <- NA 
      }
      if (nrow(dataShares[dataShares$name==n & dataShares$year==2010,])>0) {
        dropValues[dVIndex,]$percent2010 <- dataShares[dataShares$name==n & dataShares$year==2010,]$adoptionRate / 1000;
      } else {
        dropValues[dVIndex,]$percent2010 <- NA
      }
      dropValues[dVIndex,]$absoluteDrop <- dropValues[dVIndex,]$percent2005 - dropValues[dVIndex,]$percent2010;
      dropValues[dVIndex,]$relativeDrop <- (dropValues[dVIndex,]$percent2005 - dropValues[dVIndex,]$percent2010) / dropValues[dVIndex,]$percent2005;
      dVIndex <- dVIndex + 1
    } 
  }
  
  # pick estimated values for selected models and plot them
  chosenModels <-
    read.table(
      paste(pathMarketElements, "selectedModels.tsv", sep = ""),
      header = TRUE,
      sep = "\t",
      stringsAsFactors = FALSE
    )
  if (TRUE %in% is.na(chosenModels$modelID)) {
    message(paste(
      "Please select models for ",
      experimentName,
      " experiment!\n",
      sep = ""
    ))
    next
  }
  estimatesFinal <-
    merge(bestModelEstimates,
          chosenModels,
          by = c("ID", "modelID", "name"))
  estimatesFinal <-
    estimatesFinal[, names(estimatesFinal) %in% c(
      "name",
      "pStart",
      "qStart",
      "mStart",
      "p",
      "q",
      "m",
      "qprat",
      "release.year",
      "ages",
      "RMSEreal",
      "qualityFit"
    )]
  #estimatesFinal$market <- experimentName
  estimatesFinal$market <- market[i]
  estimatesFinal$type <- exType
  allEstimates <- rbind(allEstimates, estimatesFinal)
}

allEstimates$TTP <-
  (log(allEstimates$q) - log(allEstimates$p)) / (allEstimates$p + allEstimates$q)
allEstimates$T1 <-
  (-1 / (allEstimates$p + allEstimates$q)) * log((2 + sqrt(3)) * allEstimates$p /
                                                   allEstimates$q)
allEstimates$T2 <-
  (-1 / (allEstimates$p + allEstimates$q)) * log((1 / (2 + sqrt(3))) * allEstimates$p /
                                                   allEstimates$q)
allEstimates$TTdif <- allEstimates$T2 - allEstimates$T1
allEstimates$peak <-
  (allEstimates$m * (allEstimates$p + allEstimates$q) ^ 2) / (4 * allEstimates$q)
allEstimates$numPoints <- unlist(lapply(allEstimates$ages, length))
allEstimates$rmsePeak <- allEstimates$RMSEreal / allEstimates$peak

compound <- 0.99
allEstimates$lifeTime <-
  -(log((1 - compound) / (1 + allEstimates$q / allEstimates$p)) / (allEstimates$p +
                                                                     allEstimates$q))

allEstimates <-
  allEstimates[, c(
    "market",
    "name",
    "type",
    "pStart",
    "qStart",
    "mStart",
    "p",
    "q",
    "m",
    "qprat",
    "release.year",
    "numPoints",
    "TTP",
    "peak",
    "RMSEreal",
    "rmsePeak",
    "qualityFit",
    "T1",
    "T2",
    "TTdif",
    "lifeTime"
  )]

pathDir <- "output data/ALL EXPERIMENTS/"
dir.create(pathDir)
write.table(
  allEstimates,
  file = paste(pathDir, "allExperiments.tsv", sep = ""),
  quote = FALSE,
  sep = "\t",
  col.names = TRUE,
  row.names = FALSE
)


library(ggplot2)
library(plyr)


# # scatter plot q-p for all the GOOD and EXCELLENT fits.
# # groupping is done by market
# findHull <- function(df) df[chull(df$q, df$p),]
# scDF <- allEstimates[,c("market", "type", "p", "q", "numPoints", "rmsePeak", "qualityFit", "TTP")]
# scDF <- scDF[scDF$qualityFit=="GOOD" | scDF$qualityFit=="EXCELLENT",]
# scDF <- scDF[scDF$TTP>0,]
# hulls <- ddply(scDF, "market", findHull)
# scatterPlotMarket <- ggplot(scDF, aes(x=q, y=p, color=market, fill=market)) + geom_point() +
#   geom_polygon(data=hulls, alpha=0.3, color=NA)
# png(filename = paste(pathDir, "qpAllMarket.png", sep=""), width = 1800, height = 900, res=300)
# print(scatterPlotMarket)
# dev.off()


# scatter plot q-p for all the GOOD and EXCELLENT fits.
# groupping is done by type
findHull <- function(df)
  df[chull(df$q, df$p), ]
scDF <- allEstimates
#scDF <- allEstimates[,c("market", "type", "p", "q", "numPoints", "rmsePeak", "qualityFit", "TTP")]
scDF <-
  scDF[scDF$qualityFit == "GOOD" | scDF$qualityFit == "EXCELLENT", ]
scDF <- scDF[scDF$numPoints >= 6,]
#scDF <- scDF[scDF$qualityFit=="EXCELLENT",]
scDF <- scDF[scDF$TTP > 0, ]

hulls <- ddply(scDF, "type", findHull)
scatterPlotMarket <-
  ggplot(scDF, aes(
    x = q,
    y = p,
    color = type,
    fill = type,
    shape = type
  )) + geom_point(size=2) +
  geom_polygon(data = hulls,
               alpha = 0.2,
               color = NA) + labs(color = "",
                                  fill = "",
                                  shape = "") +
  scale_shape_manual(values =
                       c(15,17,19)) +
  theme(
    legend.position = "bottom",
    plot.margin = unit(c(1, 2, 2.0, 0), "mm"),
    legend.margin = unit(c(-3.0, 0, 0, 0), "mm")
  )
png(
  filename = paste(pathDir, "QPAllType.png", sep = ""),
  width = 1800,
  height = 1200,
  res = 300
)
print(scatterPlotMarket)
dev.off()

print(mean(scDF$p))
print(mean(scDF$q))

# bubble chart of release.year against time to peak with peak for bubble maginitute
scDF2 <- allEstimates
#scDF2 <- scDF2[scDF2$release.year>1990,]
#scDF2 <- scDF2[scDF2$numPoints>8,]
scDF2 <-
  scDF2[scDF2$qualityFit == "GOOD" | scDF2$qualityFit == "EXCELLENT", ]
#scDF2 <- scDF2[scDF2$qualityFit=="EXCELLENT",]
scDF2 <- scDF2[scDF2$TTP > 0, ]
#scDF2 <- scDF2[scDF2$type=="FORMAT",]
#scDF2 <- scDF2[scDF2$lifeTime < 100, ]
miY <- as.numeric(min(scDF2$release.year))
maY <- as.numeric(max(scDF2$release.year))
myTicks <- seq(miY, maY, 5)
scDF2$release.year <- as.numeric(scDF2$release.year)
scatterPlotPeak <-
  ggplot(scDF2, aes(
    x = release.year,
    y = TTP,
    color = type,
    size = peak
  )) + geom_point() +
  labs(x = "release year",
       y = "time to peak",
       color = "type:",
       size = "peak (max=1000):") +
  scale_x_continuous(breaks = myTicks) + scale_color_manual(values = c("#41b6c4", "#2c7fb8",
                                                                       "#253494")) + theme(
                                                                         legend.position = "bottom",
                                                                         legend.background = element_blank(),
                                                                         legend.key = element_blank(),
                                                                         plot.margin = unit(c(0, 2, -1.0, 1.0), "mm"),
                                                                         legend.box = "horizontal"
                                                                       )
png(
  filename = paste(pathDir, "TTPOverTime.png", sep = ""),
  width = 1800,
  height = 900,
  res = 300
)
print(scatterPlotPeak)
dev.off()
corelationTTPRel <- cor(scDF2$release.year, scDF2$TTP)
print(paste("Corelation release year time to peak ", corelationTTPRel))


# c("#41b6c4", "#2c7fb8","#253494")
scatterPlotPeak2 <-
  ggplot(scDF2, aes(
    x = release.year,
    y = TTP,
    color = type,
    size = peak
  )) + geom_point() + 
  geom_smooth(method = "lm",
              se = FALSE,
              linetype = 3, size=0.5) +
  labs(x = "release year",
       y = "time to peak",
       color = "type:",
       size = "peak (max=1000):") +
  scale_x_continuous(breaks = myTicks) + scale_color_manual(values = c("#41b6c4", "#2c7fb8","#253494")) + theme(
                                                                         legend.position = "bottom",
                                                                         legend.background = element_blank(),
                                                                         legend.key = element_blank(),
                                                                         plot.margin = unit(c(0, 2, -1.0, 1.0), "mm"),
                                                                         legend.box = "horizontal"
                                                                       )
png(
  filename = paste(pathDir, "TTPOverTimeLM.png", sep = ""),
  width = 1800,
  height = 900,
  res = 300
)
print(scatterPlotPeak2)
dev.off()







# scatter plots for qp-ratio with regression (linear and exponentail) lines added to them

scDF3 <- allEstimates
scDF3 <-
  scDF3[scDF3$qualityFit == "GOOD" | scDF3$qualityFit == "EXCELLENT", ]
#scDF3 <- scDF3[scDf3$TTP>0,]
scDF3 <- scDF3[scDF3$qprat < 1000, ]
scDF3 <- scDF3[scDF3$market %in% c("PDFS", "HTML", "FLASH"), ]
scDF3[scDF3$market == "PDFS", ]$market <- "PDF"
#scDF3 <- scDF3[scDF3$qualityFit=="EXCELLENT",]
#scDF3 <- scDF3[scDF3$market %in% c("DISTILLER","PDFS","HTML"),]
#scDF3 <- scDF3[!(scDF3$market=="DISTILLER" & scDF3$release.year==1993),]
#scDF3 <- scDF3[!(scDF3$market=="PDFS" & scDF3$release.year==1993),]
scDF3$release.year <- as.numeric(scDF3$release.year)
scDF3$qprat <- as.numeric(scDF3$qprat)
scatterPlotQPRat <-
  ggplot(scDF3, aes(x = release.year, y = qprat, color = market)) + geom_point() +
  geom_smooth(method = "lm",
              se = FALSE,
              linetype = 3) + scale_x_continuous(limits = c(1992, 2006))
# png(filename = paste(pathDir,"scatterPlotQPRatsLin.png", sep=""), width = 1800, height = 900, res=300)
# print(scatterPlotQPRat)
# dev.off()

scatterPlotQPRat2 <-
  ggplot(scDF3,
         aes(
           x = release.year,
           y = qprat,
           color = market,
           shape = market
         )) + geom_point(size=2) +
  geom_smooth(
    method = "nls",
    formula = y ~ exp(a * x - b),
    method.args = list(start = c(a = 1, b = 1995)),
    se = FALSE,
    linetype = 3,
    size = 0.5
  ) +
  scale_y_continuous(limits = c(0, 250)) +  scale_shape_manual(values =
                                                                 c(15,16,17,18)) +
  labs(
    x = "release year",
    y = "q/p ratio",
    color = "",
    shape = ""
  ) +
  theme(
    legend.position = "bottom",
    plot.margin = unit(c(1, 2, 2.0, 0), "mm"),
    legend.margin = unit(c(-3.0, 0, 0, 0), "mm")
  ) + scale_x_continuous(limits = c(1992, 2005))
png(
  filename = paste(pathDir, "QPRatsExponential.png", sep = ""),
  width = 1800,
  height = 900,
  res = 300
)
print(scatterPlotQPRat2)
dev.off()




# scatter plot defining market potential
scDF4 <- allEstimates
#scDF4 <- scDF4[scDF4$type=="FORMAT",]
#scDF4 <- scDF4[scDF4$rmsePeak<0.5 & scDF4$qprat<10000,]
scDF4 <-
  scDF4[scDF4$qualityFit == "GOOD" | scDF4$qualityFit == "EXCELLENT", ]
#scDF4 <- scDF4[scDF4$qualityFit=="EXCELLENT",]
scDF4 <- scDF4[scDF4$TTP > 0, ]
scDF4$sizeGroup <- NA
scDF4[scDF4$m < 100, ]$sizeGroup <- "small"
scDF4[scDF4$m > 100 & scDF4$m < 2000, ]$sizeGroup <- "medium"
scDF4[scDF4$m > 2000, ]$sizeGroup <- "large"
scDF4$m <- log10(scDF4$m)
scatterPlotPQM <-
  ggplot(scDF4, aes(x = q, y = p, shape = sizeGroup)) + geom_point() +
  scale_shape_manual(values = c(15, 1, 4)) + labs(shape = "Market potential") +
  scale_x_continuous(limits = c(0, 2.0)) + scale_y_continuous(limits = c(0, 0.2)) +
  theme(legend.position = c(0.88, 0.75))
png(
  filename = paste(pathDir, "PQMarketPotential.png", sep = ""),
  width = 1800,
  height = 900,
  res = 300
)
print(scatterPlotPQM)
dev.off()



scDF5 <- allEstimates
scDF35 <-
  scDF5[scDF5$qualityFit == "EXCELLENT", ]
#scDF3 <- scDF3[scDf3$TTP>0,]
scDF5 <- scDF5[scDF5$qprat < 1000, ]
scDF5 <- scDF5[scDF5$type == "VERSION", ]
#scDF3 <- scDF3[scDF3$qualityFit=="EXCELLENT",]
#scDF3 <- scDF3[scDF3$market %in% c("DISTILLER","PDFS","HTML"),]
#scDF3 <- scDF3[!(scDF3$market=="DISTILLER" & scDF3$release.year==1993),]
#scDF3 <- scDF3[!(scDF3$market=="PDFS" & scDF3$release.year==1993),]
scDF5$release.year <- as.numeric(scDF5$release.year)
scDF5$qprat <- as.numeric(scDF5$qprat)
scatterPlotQPRat <-
  ggplot(scDF5, aes(x = release.year, y = qprat)) + geom_point() +
#  geom_smooth(method = "lm",
#              se = FALSE,
#              linetype = 3) + 
  scale_x_continuous(limits = c(1992, 2006))
png(filename = paste(pathDir,"scatterPlotQPRatsReleaseYearVersion.png", sep=""), width = 1800, height = 900, res=300)
 print(scatterPlotQPRat)
dev.off()


dropValuesAbsolute <- dropValues[order(dropValues$absoluteDrop, na.last = TRUE, decreasing = TRUE),]
write.table(
  dropValuesAbsolute,
  file = paste(pathDir, "dropValuesAbsolute.tsv", sep = ""),
  quote = FALSE,
  sep = "\t",
  col.names = TRUE,
  row.names = FALSE
)

dropValuesRelative <- dropValues[order(dropValues$relativeDrop, na.last = TRUE, decreasing = TRUE),]
write.table(
  dropValuesRelative,
  file = paste(pathDir, "dropValuesRelative.tsv", sep = ""),
  quote = FALSE,
  sep = "\t",
  col.names = TRUE,
  row.names = FALSE
)


