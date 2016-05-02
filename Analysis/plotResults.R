library(ggplot2)
library(gridExtra)




# TODO a function which takes all the best values and produces graphs for them
graphAllModels <- function(marketShare, modelValues, path) {
  uniq <- unique(marketShare$ID)
  
  for (i in uniq) {
    data <- modelValues[modelValues$ID==i,]
    name <- data[1,]$name
    data$name <- paste(data$name, data$modelID, sep = "-")
    
    pathEl <- paste(path, name, sep="")
    
    plotResults(data, "combined", FALSE, TRUE, TRUE, pathEl)
    
  }
}

# TODO a function which takes a data frame with all the values for every element and plots specific 
# the function also takes a path where all the models should be plotted 
plotResults <- function(pData,  plotType="separated", includeRateOfChange, includeInterval, includePoints, path) {
  
  options( warn = -1 )
  pathGraph <- paste(path, "/graphs/", sep="")
  print(pathGraph)
  dir.create(pathGraph)
  file.remove(file.path(pathGraph, list.files(pathGraph)))
  
  
  dfClusterAge <- data.frame(title=as.character(), interval=as.numeric(), model=as.numeric())
  dfClusterYear <- data.frame(title=as.character(), interval=as.numeric(), model=as.numeric())
  
  for (i in (1:nrow(pData))) {
    
    
    
    title <- pData[i,"name"] 
    releaseYear <- as.numeric(pData[i,"release.year"])
    ages <- unlist(pData[i,"ages"])
    percentages <- unlist(pData[i,"percentages"])
    averages <- unlist(pData[i,"averages"])
    interval <- unlist(pData[i,"interval"])
    model <- unlist(pData[i,"model"])
    upper <- unlist(pData[i,"upper"])
    lower <- unlist(pData[i,"lower"])
    derv <- unlist(pData[i,"derv"])
    residual <- unlist(pData[i,"residual"])
    prediction <- unlist(pData[i,"prediction"])
    
    p <- round(pData[i,"p"], 5)
    pStart <- round(pData[i,"pStart"], 5)
    pLwr <- round(pData[i,"pLwr"], 5)
    pUpr <- round(pData[i,"pUpr"], 5)
    q <- round(pData[i,"q"], 5)
    qStart <- round(pData[i,"qStart"], 5)
    qLwr <- round(pData[i,"qLwr"], 5)
    qUpr <- round(pData[i,"qUpr"], 5)
    m <- round(pData[i,"m"], 5)
    mStart <- round(pData[i,"mStart"], 5)
    mLwr <- round(pData[i,"mLwr"], 5)
    mUpr <- round(pData[i,"mUpr"], 5)
    qpRat <- round(pData[i,"qprat"], 5)
    
    
    yUpperLimit <- max(1.1*max(model), 1.1*max(percentages))
    
    if (is.na(model)) {
      next
    }
    
    variables <- c("pStart", "p", "pLwr","pUpr", "qStart", "q", "qLwr", "qUpr", "mStart", "m", "mLwr", "mUpr", "p-q ratio")
    values <- c(pStart, p, pLwr, pUpr, qStart, q, qLwr, qUpr, mStart, m, mLwr, mUpr, qpRat)
    info <- data.frame(variable=variables, value=values)
    
    
    dfModel <- data.frame(interval, model, upper, lower)
    dfPoints <- data.frame(ages,percentages)
    
    
    # table with all the info
    tbl <- tableGrob(info)
    
    dfClusterTemp <- data.frame(title=rep(title,length(interval)), interval=interval, model=model)
    dfClusterAge <- rbind(dfClusterAge, dfClusterTemp)
    dfClusterTemp <- data.frame(title=rep(title,length(interval)), interval=interval+releaseYear, model=model)
    dfClusterYear <- rbind(dfClusterYear, dfClusterTemp)
    
    #adding elements to the main plot
    modelPlot <- ggplot() 
    if (includeInterval) {
      modelPlot <- modelPlot + geom_ribbon(data=dfModel, aes(x=interval, ymin=lower, ymax=upper), 
                                           alpha=0.2) + coord_cartesian(ylim=c(0,yUpperLimit))
    }
    modelPlot <- modelPlot + geom_line(data=dfModel, aes(x=interval, y=model))
    if (includePoints) {
      modelPlot <- modelPlot + geom_point(data=dfPoints, aes(x=ages, y=percentages))
    }
    modelPlot <- modelPlot + labs(x="age", y="number of adoptions")
    
    # change plot 
    dfChange <- data.frame(interval, derv)
    changePlot <- ggplot(dfChange, aes(x=interval, y=derv)) +
      geom_area(fill="gray", alpha=0.3) +
      geom_line() + labs(x="age", y="change rate")
    
    # residual plot 
    dfResidual <- data.frame(prediction, residual)
    residualPlot <- ggplot(dfResidual, aes(x=prediction, y=residual)) + geom_point() + labs(x="fitted value", y="residual")
    
    if (plotType=="combined") {
      
      lay <- rbind(c(1,3), c(2,3))
      png(filename=paste(pathGraph, title, ".png", sep=""))
      print(grid.arrange(modelPlot, residualPlot, tbl, layout_matrix=lay))
      dev.off()
      
    }else {
      if (includeRateOfChange) {
        png(filename=paste(pathGraph, title, ".png", sep=""))
        print(grid.arrange(modelPlot, changePlot))
        dev.off()
      }else {
        png(filename=paste(pathGraph, title, ".png", sep=""))
        print(modelPlot)
        dev.off()
      }
      
      png(filename=paste(pathGraph, title, "-residual.png", sep=""))
      print(residualPlot)
      dev.off()
    }
    
  }
  
  png(filename=paste(pathGraph, "cluster-ages.png", sep=""))
  clusterPlot <- ggplot(dfClusterAge, aes(x=interval, y=model, colour=title)) + geom_line() +
    theme(legend.position=c(1,1), legend.justification=c(1,1), legend.background=element_rect(fill="white"),
          legend.text=element_text(size=10), legend.title=element_blank(),
          legend.key=element_blank()) + labs(x="age", y="number of adoptions")
  print(clusterPlot)
  dev.off()
  
  png(filename=paste(pathGraph, "cluster-years.png", sep=""))
  clusterPlot <- ggplot(dfClusterYear, aes(x=interval, y=model, colour=title)) + geom_line() +
    theme(legend.position=c(1,1), legend.justification=c(1,1), legend.background=element_rect(fill="white"),
          legend.text=element_text(size=10), legend.title=element_blank(),
          legend.key=element_blank()) + labs(x="harvest year", y="number of adoptions")
  print(clusterPlot)
  dev.off()

}


