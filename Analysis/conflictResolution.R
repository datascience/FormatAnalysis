library(plyr)
library(ggplot2)

rules <- read.table("input data/Format markets - conflict_rules.tsv", header=TRUE, sep="\t", 
                    colClasses=c("character","character","character"), stringsAsFactors=FALSE)
print (paste("Loaded ", nrow(rules), " rules.", sep=""))

rulesCounter <- rules
rulesCounter$used <- 0
type <- c("no conflict", "special rule used", "tika & droid agree", "only one tool returned value", "conflict not resolved")
num <- c(0,0,0,0,0)
cResolution <- data.frame(prop=character(), type=character(),amount=numeric(), stringsAsFactors = FALSE)
cResolution$amount <- as.numeric(cResolution$amount)

conflictResolution <- function(row, prop) {
  
  if (!(prop %in% unique(cResolution$prop))) {
    cResolution[nrow(cResolution)+1,] <<- c(prop, "custom rule", 0)
    cResolution[nrow(cResolution)+1,] <<- c(prop, "no conflict", 0)
    cResolution[nrow(cResolution)+1,] <<- c(prop, "tikadroid", 0)
    cResolution[nrow(cResolution)+1,] <<- c(prop, "one tool", 0)
    cResolution[nrow(cResolution)+1,] <<- c(prop, "not resolved", 0)
  }
  
  amount <- as.numeric(row[4])
  row <- row[1:3]
  row[is.na(row)] <- ""
  
  #if entry matches certain rule 
  test <- rules[rules$property==prop & rules$server==row[1] & rules$tika==row[2] & rules$droid==row[3],]
  if (nrow(test)>0) {
    #take the first one
    temp <- test[1,]$resolveTo
    count <- rulesCounter[rulesCounter$ID == test[1,]$ID,]$used
    count <- count + amount
    # warning this referes to the global variable rulesCounter!!!
    rulesCounter[rulesCounter$ID == test[1,]$ID,]$used <<- count
    cResolution[cResolution$prop==prop & cResolution$type=="custom rule",]$amount <<- 
      as.numeric(cResolution[cResolution$prop==prop & cResolution$type=="custom rule",]$amount) + amount
    return (temp)
  }
    
  #if all three agree . not a conflict but needs to be recorded for the stats
  if (row[1]==row[2] & row[2]==row[3]) {
    cResolution[cResolution$prop==prop & cResolution$type=="no conflict",]$amount <<- 
      as.numeric(cResolution[cResolution$prop==prop & cResolution$type=="no conflict",]$amount) + amount
    #cResolution[1,]$num <<- cResolution[1,]$num + amount
    return (row[1])
  }
  
  #if tika and droid agree then pick that value
  if (row[2]==row[3] & !is.na(row[2]) & row[2]!="application/octet-stream" & row[2]!=" ") {
    cResolution[cResolution$prop==prop & cResolution$type=="tikadroid",]$amount <<- 
      as.numeric(cResolution[cResolution$prop==prop & cResolution$type=="tikadroid",]$amount) + amount
    #cResolution[nrow(cResolution)+1,] <<- c(prop, "tika=droid", amount)
    #cResolution[3,]$num <<- cResolution[3,]$num + amount
    return(row[2])
  }
  
  #if only one tool managed to return a value pick that value
  test <- row[!is.na(row) & row!=" " & row!="" & row!="application/octet-stream"]
  if (length(test)==1) {
    cResolution[cResolution$prop==prop & cResolution$type=="one tool",]$amount <<- 
      as.numeric(cResolution[cResolution$prop==prop & cResolution$type=="one tool",]$amount) + amount
    #cResolution[nrow(cResolution)+1,] <<- c(prop, "one tool", amount)
    #cResolution[4,]$num <<- cResolution[4,]$num + amount
    return (test[1])
  }
  
  
  #if nothing works return NA
  cResolution[cResolution$prop==prop & cResolution$type=="not resolved",]$amount <<- 
    as.numeric(cResolution[cResolution$prop==prop & cResolution$type=="not resolved",]$amount) + amount
  #cResolution[nrow(cResolution)+1,] <<- c(prop, "not resolved", amount)
  #cResolution[5,]$num <<- cResolution[5,]$num + amount
  return (NA)
}

afterResolution <-function() {
  
  rulesCounter <<- rulesCounter[rulesCounter$used > 0,]
  rulesCounter <<- rulesCounter[order(-rulesCounter$used),]
  write.table(rulesCounter, file= paste(path, "/statistics/rulesUsed.tsv", sep=""), 
              quote=FALSE, sep="\t", col.names=TRUE, row.names=FALSE)
  write.table(cResolution, file= paste(path, "/statistics/conflictsStats.tsv", sep=""), 
              quote=FALSE, sep="\t", col.names=TRUE, row.names=FALSE)
  
  cResolution$amount<<- as.numeric(cResolution$amount)
  print (cResolution)
  #cResolution <<- aggregate(amount~prop+type, FUN=sum, data=cResolution)

  tempCR <- cResolution
  
  tempCR[tempCR$type=="custom rule" | tempCR$type=="tikadroid" | tempCR$type=="one tool",]$type <- "resolved"
  tempCR <- ddply(tempCR, "prop", transform, percent = amount / sum(amount)*100)
  tempCR <- aggregate(percent~prop+type, data=tempCR, FUN=sum)
  
  png(filename=paste(path,"/statistics/conflicts.png", sep=""),width = 800, height = 680)
  plot1 <- ggplot(tempCR, aes(x=type, y=percent)) + geom_bar(aes(fill=prop),position = "dodge", stat="identity") + 
    scale_y_sqrt(expand=c(0,0),breaks=c(0.25,0.5,1,5,10,25,50,75,100),limits=c(0,100)) +
    scale_fill_brewer(palette = "Set2")
  print(plot1)
  dev.off()

  tempCR <- cResolution
  
  tempCR<- tempCR[tempCR$type %in% c("custom rule", "tikadroid", "one tool"), ]
  tempCR <- ddply(tempCR, "prop", transform, percent = amount / sum(amount)*100)
  tempCR <- aggregate(percent~prop+type, data=tempCR, FUN=sum)
  
  png(filename=paste(path,"/statistics/conflictsRules.png", sep=""),width = 800, height = 680)
  plot2 <- ggplot(tempCR, aes(x=type, y=percent)) + geom_bar(aes(fill=prop),position = "dodge", stat="identity") + 
    scale_y_sqrt(expand=c(0,0),breaks=c(0.25,0.5,1,5,10,25,50,75,100),limits=c(0,100)) +
    scale_fill_brewer(palette = "Set2")
  print(plot2)
  dev.off()
  
}

conflictCategoryDetermination <- function(row, propertyToTake) {
  category <- 1
  for (prop in propertyToTake) {
    if (length(unique(row[grep(prop, names(row))]))==1 & category<=1) {
      category <- 1
      next
    }
    if (length(unique(c(row[paste(prop,"tika", sep="")], row[paste(prop,"droid", sep="")], row[prop])))==1 & category<=2) {
      category <- 2
      next
    }  
    category <- 3
  }
  if (category==1) {
    return ("no conflict")
  }
  if (category==2) {
    return ("tika=droid")
  }
  if (category==3) {
    return ("conflict resolved")
  }  
}
