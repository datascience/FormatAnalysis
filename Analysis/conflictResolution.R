
rules <- read.table("input data/conflicts_rules.csv", header=TRUE, sep="\t", colClasses=c("character","character","character"), stringsAsFactors=FALSE)

conflictResolution <- function(row, prop) {
  
  #if only one tool managed to return a value pick that value
  test <- row[!is.na(row) & row!=" " & row!="application/octet-stream"]
  if (length(test)==1) {
    return (test[1])
  }
  
  #if tika and droid agree then pick that value
  if (row[2]==row[3] & !is.na(row[2]) & row[2]!="application/octet-stream" & row[2]!=" ") {
    return(row[2])
  }
  
  return (NA)
}
