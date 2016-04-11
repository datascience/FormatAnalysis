library(ggplot2)
library(gridExtra)
plotResults <- function(pData, modelTypes, includeRateOfChange, includeInterval, includePoints) {
  
  options( warn = -1 )
  pathGraph <- paste(path, "/graphs/", sep="")
  dir.create(pathGraph)
  
  
  for (mType in modelTypes) {
    
    dfCluster <- data.frame(title=as.character(), interval=as.numeric(), model=as.numeric())
    
    for (i in (1:nrow(pData))) {
      
      
      
      title <- pData[i,"name"] 
      ages <- unlist(pData[i,"ages"])
      percentages <- unlist(pData[i,"percentages"])
      averages <- unlist(pData[i,"averages"])
      interval <- unlist(pData[i,paste("interval.",mType, sep="")])
      model <- unlist(pData[i,paste("model.",mType, sep="")])
      upper <- unlist(pData[i,paste("upper.",mType, sep="")])
      lower <- unlist(pData[i,paste("lower.",mType, sep="")])
      derv <- unlist(pData[i,paste("derv.",mType, sep="")])
      residual <- unlist(pData[i,paste("residual.",mType,sep="")])
      prediction <- unlist(pData[i,paste("prediction.",mType,sep="")])
      
      if (is.na(model)) {
        next
      }
      
      dfModel <- data.frame(interval, model, upper, lower)
      dfPoints <- data.frame(ages,percentages)
      
      modelPlot <- ggplot() 
      
      dfClusterTemp <- data.frame(title=rep(title,length(interval)), interval=interval, model=model)
      dfCluster <- rbind(dfCluster, dfClusterTemp)
      
      if (includeInterval) {
        modelPlot <- modelPlot + geom_ribbon(data=dfModel, aes(x=interval, ymin=lower, ymax=upper), alpha=0.2) 
      }
      
      modelPlot <- modelPlot + geom_line(data=dfModel, aes(x=interval, y=model)) 
      
      if (includePoints) {
        modelPlot <- modelPlot + geom_point(data=dfPoints, aes(x=ages, y=percentages))
      }
      modelPlot <- modelPlot + labs(x="age", y="% of the market share")
      
      dfChange <- data.frame(interval, derv)
      changePlot <- ggplot(dfChange, aes(x=interval, y=derv)) +
        geom_area(fill="gray", alpha=0.3) +
        geom_line() + labs(x="age", y="change rate")
      
      if (includeRateOfChange) {
        png(filename=paste(pathGraph, title, "-", mType, ".png", sep=""))
        print(grid.arrange(modelPlot, changePlot))
        dev.off()
      }else {
        png(filename=paste(pathGraph, title, "-", mType, ".png", sep=""))
        print(modelPlot)
        dev.off()
      }
      
      if (includeResidual) {
        dfResidual <- data.frame(prediction, residual)
        residualPlot <- ggplot(dfResidual, aes(x=prediction, y=residual)) + geom_point() + labs(x="fitted value", y="residual")
        png(filename=paste(pathGraph, title, "-", mType, "-residual.png", sep=""))
        print(residualPlot)
        dev.off()
      }
    }
    
    png(filename=paste(pathGraph, "cluster-", mType,".png", sep=""))
    clusterPlot <- ggplot(dfCluster, aes(x=interval, y=model, colour=title)) + geom_line() +
      theme(legend.position=c(1,1), legend.justification=c(1,1),legend.background=element_rect(fill="white"),
            legend.text=element_text(size=10), legend.title=element_blank(),
            legend.key=element_blank()) + labs(x="age", y="% of the market share")
    print(clusterPlot)
    dev.off()
    
  }

}


