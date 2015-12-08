rules <- read.table("input data/Format markets - conflict_rules.tsv", header=TRUE, sep="\t", 
                    colClasses=c("character","character","character"), stringsAsFactors=FALSE)
print (paste("Loaded ", nrow(rules), " rules.", sep=""))

rulesCounter <- rules
rulesCounter$used <- 0
type <- c("no conflict", "special rule used", "tika & droid agree", "only one tool returned value", "conflict not resolved")
num <- c(0,0,0,0,0)
cResolution <- data.frame(type,num)


conflictResolution <- function(row, prop) {
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
    cResolution[2,]$num <<- cResolution[2,]$num + amount  
    return (temp)
  }
    
  #if all three agree . not a conflict but needs to be recorded for the stats
  if (row[1]==row[2] & row[2]==row[3]) {
    cResolution[1,]$num <<- cResolution[1,]$num + amount
  }
  
  #if tika and droid agree then pick that value
  if (row[2]==row[3] & !is.na(row[2]) & row[2]!="application/octet-stream" & row[2]!=" ") {
    cResolution[3,]$num <<- cResolution[3,]$num + amount
    return(row[2])
  }
  
  #if only one tool managed to return a value pick that value
  test <- row[!is.na(row) & row!=" " & row!="" & row!="application/octet-stream"]
  if (length(test)==1) {
    cResolution[4,]$num <<- cResolution[4,]$num + amount
    return (test[1])
  }
  
  
  #if nothing works return NA
  cResolution[5,]$num <<- cResolution[5,]$num + amount
  return (NA)
}

afterResolution <-function() {
  
  rulesCounter <<- rulesCounter[rulesCounter$used > 0,]
  rulesCounter <<- rulesCounter[order(-rulesCounter$used),]
  write.table(rulesCounter, file= paste(path, "/statistics/rulesUsed.tsv", sep=""), 
              quote=FALSE, sep="\t", col.names=TRUE, row.names=FALSE)
  write.table(cResolution, file= paste(path, "/statistics/conflictsStats.tsv", sep=""), 
              quote=FALSE, sep="\t", col.names=TRUE, row.names=FALSE)
  
  sA <- sum(cResolution$num)
  tcS <- c("no conflict", "resolved", "not resolved")
  ncS <- c(cResolution[1,]$num/sA, (cResolution[2,]$num+cResolution[3,]$num+cResolution[4,]$num)/sA, cResolution[5,]$num/sA)
  cS <- data.frame(tcS, ncS)
  
  sR <- cResolution[2,]$num+cResolution[3,]$num+cResolution[4,]$num
  trS <- c("special rule used", "tika & droid agree", "only one tool returned value")
  nrS <- c(cResolution[2,]$num/sR, cResolution[3,]$num/sR, cResolution[4,]$num/sR)
  rS <- data.frame(trS, nrS)
  
  png(filename=paste(path,"/statistics/conflicts.png", sep=""))
  pie(cS$ncS, labels=cS$tcS, main="Amount of conflicts")
  dev.off()
  
  png(filename=paste(path,"/statistics/conflitRules.png", sep=""))
  pie(rS$nrS, labels=rS$trS, main="Ratio of conflict rules used")
  dev.off()
  
}
