mime <- function(x) {
  values <- strsplit(x, split=";")
  c <- sapply(values, "[", 1) 
  return (c)
}

property <- function(x, prop) {
  print (prop)
  if (prop=="mime" | prop=="Mime") {
    print ("returning mime")
    return (mime(x))
  } 
  values <- strsplit(x, split="; ")
  c <- unlist(sapply(values, extractProperty, prop))
  return (c)
}

extractProperty <- function(x, property) {
  x  <- unlist(x)
  c <- unlist(strsplit(x[grep(property, x, fixed=TRUE)], split="="))[2]
  c[grep("\"*\"",c)] <- substring(c[grep("\"*\"",c)], 2, nchar(c[grep("\"*\"",c)])-1)
  c[is.null(c)] <- NA
  return (c)
}



loadDataFromFile <- function(x, names) {
  txt <- readLines(x)
  splitTxt <- strsplit(txt, split="\t")
  processed <- lapply(splitTxt, processLine)
  data <- matrix(unlist(processed), nrow=length(processed), byrow=TRUE)
  colnames(data) <- names
  #colnames(data) <- c("server", "tika", "droid", "year", "amount")
  data <- as.data.frame(data, stringAsFactors=FALSE)
  data$server <- as.character(data$server)
  data$tika <- as.character(data$tika)
  data$droid <- as.character(data$droid)
  data$year <- as.integer(as.character(data$year))
  data$amount <- as.integer(as.character(data$amount))
  return(data)
}

processLine <- function(x) {
  out <- character(5)
  out[1] <- x[1]
  out[2] <- x[2]
  out[3] <- x[3]
  out[4] <- x[4]
  out[5] <- x[5]
  out
}


mySum <- function(x,y) {
  return (sum(y[x]))
}

estimateS <- function(p,q,m,num) {
  c <- c(1:num) 
  sum <- 0
  for (i in 1:num) {
    if (i==1) {
      c[i] <- p*m
      sum <- c[i]
    } else {
      c[i] <- p*m +(q-p)*sum-(q/m)*sum*sum
      sum <- sum + c[i]
    } 
  }
  return (c)
}


unifyValues <- function(row, groupProps) {
  bo <- row
  print(row)
  if (!is.na(row) & row!=" "){
    tmp <- unlist(lapply(groupProps, function(x) row %in% unlist(x)))
    poz <- min(which(tmp==TRUE))
    if (is.finite(poz)) {
      tmpList <- unlist(groupProps[poz])
      boOld <- bo
      bo <- tmpList[1]
      print (paste(c(boOld," changed to ",bo)))
    } 
  }
  return (bo)
}


