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
  for(p in unique(cResolution$prop)) {
    tempCR <- cResolution[cResolution$prop==p,]
    sA <- sum(tempCR$amount)
    tcS <- c("no conflict", "resolved", "not resolved")
    ncS <- c(tempCR[tempCR$type=="no conflict",]$amount/sA, (tempCR[tempCR$type=="custom rule",]$amount+
                                                              tempCR[tempCR$type=="tikadroid",]$amount+
                                                              tempCR[tempCR$type=="one tool",]$amount)/sA, 
             tempCR[tempCR$type=="not resolved",]$amount/sA)
    print (ncS)
    cS <- data.frame(tcS, ncS)
    png(filename=paste(path,"/statistics/conflicts-", p, ".png", sep=""))
    pie(cS$ncS, labels=cS$tcS, main="Amount of conflicts")
    dev.off()

    sR <- tempCR[tempCR$type=="custom rule",]$amount+tempCR[tempCR$type=="tikadroid",]$amount+tempCR[tempCR$type=="one tool",]$amount
    trS <- c("custom rule used", "tika & droid agree", "only one tool returned value")
    nrS <- c(tempCR[tempCR$type=="custom rule",]$amount/sR, tempCR[tempCR$type=="tikadroid",]$amount/sR, 
             tempCR[tempCR$type=="one tool",]$amount/sR)
    rS <- data.frame(trS, nrS)
    
    png(filename=paste(path,"/statistics/conflitRules-",p,".png", sep=""))
    pie(rS$nrS, labels=rS$trS, main="Ratio of conflict rules used")
    dev.off()
  }
  
}
