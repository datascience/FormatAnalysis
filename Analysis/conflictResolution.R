
rules <- read.table("input data/Format markets - conflict_rules.tsv", header=TRUE, sep="\t", colClasses=c("character","character","character"), stringsAsFactors=FALSE)
print (paste("Loaded ", nrow(rules), " rules.", sep=""))

rulesCounter <- rules
rulesCounter$used <- 0

conflictResolution <- function(row, prop) {
  
  #if entry matches certain rule 
  test <- rules[rules$property==prop & rules$server==row[1] & rules$tika==row[2] & rules$droid==row[3],]
  if (nrow(test)>0) {
    #take the first one
    temp <- test[1,]$resolveTo
    #count <- rulesCounter[rulesCounter$ID == test[1,]$ID,]$used
    #cout <- count + 1
    #rulesCounter[rulesCounter$ID == test[1,]$ID,]$used <- count
    return (temp)
  }
    
  #if only one tool managed to return a value pick that value
  test <- row[!is.na(row) & row!=" " & row!="application/octet-stream"]
  if (length(test)==1) {
    return (test[1])
  }
  
  #if tika and droid agree then pick that value
  if (row[2]==row[3] & !is.na(row[2]) & row[2]!="application/octet-stream" & row[2]!=" ") {
    return(row[2])
  }
  
  #if nothing works return NA 
  return (NA)
}

afterResolution <-function() {
  write.table(rulesCounter, file= paste(path, "/statistics/rulesUsed.tsv", sep=""), 
              quote=FALSE, sep="\t", col.names=TRUE, row.names=FALSE)
}
