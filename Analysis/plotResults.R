library(ggplot2)
library(gridExtra)
library(grid)




# TODO a function which takes all the best values and produces graphs for them
graphAllModels <- function(marketShare, modelValues, path, experimentName) {
  uniq <- unique(marketShare$ID)
  
  for (i in uniq) {
    data <- modelValues[modelValues$ID==i,]
    name <- data[1,]$name
    #data$name <- paste(data$name, data$modelID, sep = "-")
    
    pathEl <- paste(path, name, sep="")
    
    plotResults(data, "toSelect", FALSE, TRUE, TRUE, pathEl, experimentName)
    
  }
}


plotResults <- function(pData,  plotType="toSelect", includeRateOfChange, includeInterval, includePoints, path, experimentName) {
  
  options( warn = -1 )
  
  pathGraph <- paste(path, "/graphs/", sep="")
  
  timeStamp <- format(Sys.time(), "%y%m%d%H%M")
  
  graphFileName <- paste(pathGraph, experimentName, "_", timeStamp, "_", plotType, sep="")
  
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
    modelID <- pData[i,"modelID"]
    releaseYear <- as.numeric(pData[i,"release.year"])
    ages <- unlist(pData[i,"ages"])
    percentages <- unlist(pData[i,"adoptionRate"])
    averages <- unlist(pData[i,"smoothedAdoptionRate"])
    interval <- unlist(pData[i,"interval"])
    model <- unlist(pData[i,"model"])
    upper <- unlist(pData[i,"upper"])
    lower <- unlist(pData[i,"lower"])
    derv <- unlist(pData[i,"derv"])
    residual <- unlist(pData[i,"residual"])
    residualAverage <- unlist(pData[i,"residualAverage"])
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
    rmse <- signif(pData[i,"RMSE"], 4)
    rmsereal <- signif(pData[i,"RMSEreal"], 4)
    
    pBound <- signif(100*(pUpr-p)/p,3)
    qBound <- signif(100*(pUpr-p)/q,3)
    mBound <- signif(100*(pUpr-p)/m,3)
    
    
    yUpperLimit <- max(1.1*max(model), 1.1*max(percentages))
    
    if (is.na(model)) {
      next
    }
    
    variables <- c("pStart", "p", "pLwr","pUpr", "pBound", "qStart", "q", "qLwr", "qUpr", "qBound", "mStart", "m", 
                   "mLwr", "mUpr", "mBound", "q-p ratio", "rmse", "rmse-real")
    values <- c(pStart, p, pLwr, pUpr, pBound, qStart, q, qLwr, qUpr, qBound, mStart, m, mLwr, mUpr, mBound, qpRat, rmse, rmsereal)
    info <- data.frame(variable=variables, value=values)
    
    
    dfModel <- data.frame(interval, model, upper, lower)
    dfPoints <- data.frame(ages,percentages)
    
    dfComponentsPlot <- data.frame(interval,model,pValue, qValue)
    
    # table with all the info
    tbl <- tableGrob(info)
    
    dfClusterTemp <- data.frame(title=rep(title,length(interval)), modelID=rep(modelID,length(interval)), interval=interval, model=model)
    dfClusterAge <- rbind(dfClusterAge, dfClusterTemp)
    dfClusterTemp <- data.frame(title=rep(title,length(interval)), modelID=rep(modelID,length(interval)), interval=interval+releaseYear, model=model)
    dfClusterYear <- rbind(dfClusterYear, dfClusterTemp)
    
    
    
    #adding elements to the main plot
    modelPlot <- ggplot() 
    if (includeInterval) {
      modelPlot <- modelPlot + geom_ribbon(data=dfModel, aes(x=interval, ymin=lower, ymax=upper), 
                                           alpha=0.2) + coord_cartesian(ylim=c(0,yUpperLimit))
    }
    modelPlot <- modelPlot + geom_line(data=dfModel, aes(x=interval, y=model)) +
      scale_x_continuous(expand = c(0,0))
    if (includePoints) {
      modelPlot <- modelPlot + geom_point(data=dfPoints, aes(x=ages, y=percentages))
    }
    modelPlot <- modelPlot + labs(x="age", y="rate of adoptions")
    
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
      scale_color_manual(name="", values=c("col1"="black", "col2"="red", "col3"="green"),
                         labels=c("New adoptions", paste("External influence\n(p=", p, ")", sep=""),
                                  paste("Internal influence\n(q=",q,")", sep=""))) + 
      scale_linetype_manual(name="", values = c("col1"="solid", "col2"="dashed", "col3"="dashed"),
                            labels=c("New adoptions", paste("External influence\n(p=",p,")", sep=""),
                                     paste("Internal influence\n(q=",q,")", sep=""))) +
      labs(x="Age", y="Rate of adoptions") +
      theme(axis.title=element_text(face="italic",size=16),
            legend.position=c(0.7,0.6), legend.background=element_blank(),
            legend.text=element_text(face="bold", size=16), 
            legend.key=element_blank(), legend.key.width=unit(3.5,"line"), 
            legend.key.height=unit(3,"line"))
    
    # residual plot 
    dfResidual <- data.frame(prediction, residual)
    residualPlot <- ggplot(dfResidual, aes(x=prediction, y=residual)) + geom_point() + labs(x="fitted value", y="residual")
    
    # average residual plot 
    dfResidual <- data.frame(prediction, residualAverage)
    residualAveragePlot <- ggplot(dfResidual, aes(x=prediction, y=residualAverage)) + geom_point() + labs(x="fitted value", y="residual(average)")

    
    
    if (plotType=="toSelect") {
      
      lay <- rbind(c(1,1,4), c(2,3,4))
      png(filename=paste(graphFileName, "_", title,"-", modelID, ".png", sep=""), width = 720, height = 480)
      print(grid.arrange(modelPlot, residualPlot, residualAveragePlot, tbl, layout_matrix=lay))
      dev.off()
      
#       png(filename=paste(pathGraph, timeStamp, "-", title, "-", modelID, "-components.png", sep=""))
#       print(componentsPlot)
#       dev.off()
      
    }else {
      if (includeRateOfChange) {
        png(filename=paste(graphFileName, "_", title, ".png", sep=""))
        print(grid.arrange(modelPlot, changePlot))
        dev.off()
      }else {
        png(filename=paste(graphFileName, "_", title, ".png", sep=""))
        print(modelPlot)
        dev.off()
      }
      
      png(filename=paste(graphFileName, "_", title, "_residual.png", sep=""))
      print(residualPlot)
      dev.off()
      
      png(filename=paste(graphFileName, "_", title, "_averageResidual.png", sep=""))
      print(residualAveragePlot)
      dev.off()
      
      
      
      
    }
    
  }
  
  if (nrow(dfClusterAge)>0) {
    allModelsName <- NA
    if (plotType=="toSelect") {
      allModelsName <- paste(graphFileName, "_", title, sep="")
    } else {
      allModelsName <- graphFileName
    }
    
  png(filename=paste(allModelsName, "_allmodels-ages.png", sep=""))
  clusterPlot <- ggplot(dfClusterAge, aes(x=interval, y=model, colour=paste(title,"-",modelID,sep=""))) + geom_line() +
    scale_fill_brewer(palette="Dark2") +
    theme(legend.position=c(1,1), legend.justification=c(1,1), legend.background=element_rect(fill="white"),
          legend.text=element_text(size=10), legend.title=element_blank(),
          legend.key=element_blank()) + labs(x="age", y="rate of adoptions")
  print(clusterPlot)
  dev.off()
  
  png(filename=paste(allModelsName,  "_allmodels-years.png", sep=""))
  clusterPlot <- ggplot(dfClusterYear, aes(x=interval, y=model, colour=paste(title,"-",modelID,sep=""))) + geom_line() +
    scale_fill_brewer(palette="Dark2") +
    theme(legend.position=c(1,1), legend.justification=c(1,1), legend.background=element_rect(fill="white"),
          legend.text=element_text(size=10), legend.title=element_blank(),
          legend.key=element_blank()) + labs(x="harvest year", y="rate of adoptions")
  print(clusterPlot)
  dev.off()
    
  }
  

}


