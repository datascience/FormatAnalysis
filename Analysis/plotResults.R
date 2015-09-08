
plotResults <- function(pData, includeRateOfChange, includeInterval, includePoints) {
  
  options( warn = -1 )
  pathGraph <- paste(path, "/graphs/", sep="")
  dir.create(pathGraph)
  
  for (i in (1:nrow(pData))) {
    
    title <- pData[i,]$name 
    ages <- unlist(pData[i,]$ages)
    percentages <- unlist(pData[i,]$percentages)
    averages <- unlist(pData[i,]$averages)
    interval <- unlist(pData[i,]$interval)
    model <- unlist(pData[i,]$model)
    upper <- unlist(pData[i,]$upper)
    lower <- unlist(pData[i,]$lower)
    derv <- unlist(pData[i,]$derv)
    residual <- unlist(pData[i,]$residual)
    prediction <- unlist(pData[i,]$prediction)
    
    png(filename=paste(pathGraph, title, ".png", sep=""))
    
    if (includeRateOfChange & includeResidual) { 
      layout(matrix(c(1, 2, 3), 3, 1, byrow = TRUE), heights = c(1.5, 1, 1.5))
      par(mar=c(2,4,2,1))
    } 
    
    if (includeInterval) {
      plot(NULL, main=title, xlim=c(0,30), ylim=c(min(lower)-0.1*abs(min(lower)), 
                                                  max(upper)+0.1*max(upper)),xlab="age", ylab="adoption")
      polygon(c(interval,rev(interval)), c(lower,rev(upper)), col='grey96', border=NA)
      lines(interval,lower, lty=2)
      lines(interval,upper, lty=2)
    } else {
      plot(NULL, main=title, xlim=c(0,30), ylim=c(min(model)-0.1*min(model),max(model)+0.1*max(model)), ylab="adoption", xlab="")
    }
    
    if (includePoints) {
      points(ages,percentages, pch=17)
      #points(ages,averages, pch=19)
    }
    lines(interval,model)
    
    if (includeRateOfChange) {
      par(mar=c(4,4,2,1))
      plot(NULL, xlim=c(0,30), ylim=c(min(derv)-0.1*abs(min(derv)),max(derv)+0.1*max(derv)),xlab="age", ylab="rate of change")
      polygon(c(interval,rev(interval)), c(rep(0,length(interval)),rev(derv)), col='grey96', border=NA)
      lines(interval,derv)
    }
    
    if (includeResidual) {
      plot(prediction,residual, pch=19)
      abline(0,0,untf=FALSE)
    }
    
    dev.off()
    
  }
  
  #plot the cluster 
  print ("Plotting the cluster")
  png(filename=paste(pathGraph, "cluster.png", sep=""))
  plot(NULL, main="cluster", xlim=c(0,30), ylim=c(0,1.2), xlab="age", ylab="adoption")
  for (i in (1:nrow(pData))) {
    interval <- unlist(pData[i,]$interval)
    model <- unlist(pData[i,]$model)
    lines(interval,model, col=i)
  }
  legend('topright', pData$name, lty=c(1,1), col=1:nrow(pData))
  dev.off()
}


