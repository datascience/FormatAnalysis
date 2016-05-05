library(ggplot2)
library(gridExtra)
library(grid)




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
  
  dfClusterParameters <- data.frame(title=as.character(), p=as.numeric(), plwr=as.numeric(), pupr=as.numeric(),
                                    q=as.numeric(), qlwr=as.numeric(), qupr=as.numeric(),
                                    m=as.numeric(), mlwr=as.numeric(), mupr=as.numeric())
  
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
    pValue <- unlist(pData[i,"pValue"])
    qValue <- unlist(pData[i,"qValue"])
    
    p <- signif(pData[i,"p"], 4)
    pStart <- signif(pData[i,"pStart"], 3)
    pLwr <- signif(pData[i,"pLwr"], 4)
    pUpr <- signif(pData[i,"pUpr"], 4)
    q <- signif(pData[i,"q"], 4)
    qStart <- signif(pData[i,"qStart"], 3)
    qLwr <- signif(pData[i,"qLwr"], 4)
    qUpr <- signif(pData[i,"qUpr"], 4)
    m <- signif(pData[i,"m"], 4)
    mStart <- signif(pData[i,"mStart"], 3)
    mLwr <- signif(pData[i,"mLwr"], 4)
    mUpr <- signif(pData[i,"mUpr"], 4)
    qpRat <- round(pData[i,"qprat"], 2)
    mse <- signif(pData[i,"MSE"], 4)
    msereal <- signif(pData[i,"MSEreal"], 4)
    
    pBound <- signif(100*(pUpr-p)/p,3)
    qBound <- signif(100*(pUpr-p)/q,3)
    mBound <- signif(100*(pUpr-p)/m,3)
    
    
    yUpperLimit <- max(1.1*max(model), 1.1*max(percentages))
    
    if (is.na(model)) {
      next
    }
    
    variables <- c("pStart", "p", "pLwr","pUpr", "pBound", "qStart", "q", "qLwr", "qUpr", "qBound", "mStart", "m", 
                   "mLwr", "mUpr", "mBound", "q-p ratio", "mse", "mse-real")
    values <- c(pStart, p, pLwr, pUpr, pBound, qStart, q, qLwr, qUpr, qBound, mStart, m, mLwr, mUpr, mBound, qpRat, mse, msereal)
    info <- data.frame(variable=variables, value=values)
    
    
    dfModel <- data.frame(interval, model, upper, lower)
    dfPoints <- data.frame(ages,percentages)
    
    dfComponentsPlot <- data.frame(interval,model,pValue, qValue)
    
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
    
    # components plot 
    componentsPlot<- ggplot(dfComponentsPlot, aes(x = interval)) + geom_area(aes(y=pValue), fill="gray75") + 
      geom_line(aes(y=model, colour="col1", linetype="col1"), size=1) + 
      geom_line(aes(y=pValue, colour="col2", linetype="col2"), size=1) + 
      geom_line(aes(y=qValue, color="col3", linetype="col3"), size=1) +
      scale_y_continuous(expand = c(0,0), limits=c(0,1.1*max(model))) + 
      scale_x_continuous(expand = c(0,0)) +
      scale_color_manual(name="", values=c("col1"="black", "col2"="red", "col3"="blue"),
                         labels=c("New adoptions", paste("External influence\n(p=", p, ")", sep=""),
                                  paste("Internal influence\n(q=",q,")", sep=""))) + 
      scale_linetype_manual(name="", values = c("col1"="solid", "col2"="dashed", "col3"="dashed"),
                            labels=c("New adoptions", paste("External influence\n(p=",p,")", sep=""),
                                     paste("Internal influence\n(q=",q,")", sep=""))) +
      labs(x="Age", y="Number of adoptions") +
      theme(axis.title=element_text(face="italic",size=16),
            legend.position=c(0.7,0.6), legend.background=element_blank(),
            legend.text=element_text(face="bold", size=16), 
            legend.key=element_blank(), legend.key.width=unit(3.5,"line"), 
            legend.key.height=unit(3,"line"))
    
    # residual plot 
    dfResidual <- data.frame(prediction, residual)
    residualPlot <- ggplot(dfResidual, aes(x=prediction, y=residual)) + geom_point() + labs(x="fitted value", y="residual")
    
    if (plotType=="combined") {
      
      lay <- rbind(c(1,3), c(2,3))
      png(filename=paste(pathGraph, title, ".png", sep=""))
      print(grid.arrange(modelPlot, residualPlot, tbl, layout_matrix=lay))
      dev.off()
      
      png(filename=paste(pathGraph, title, "components.png", sep=""))
      print(componentsPlot)
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


