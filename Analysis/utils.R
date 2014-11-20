mime <- function(x) {
  values <- strsplit(x, split=";")
  c <- sapply(values, "[", 1) 
  c
}

property <- function(x, prop) {
  values <- strsplit(x, split="; ")
  c <- unlist(sapply(values, extractProperty, prop))
  c
}

extractProperty <- function(x, property) {
  x  <- unlist(x)
  c <- unlist(strsplit(x[grep(property, x, fixed=TRUE)], split="="))[2]
  c[is.null(c)] <- NA
  c
}

resolveConflicts <- function(x,y) {
  if (x==y) {
    return (x)
  } else if (x=="application/octet-stream" & y!="application/octet-stream") {
    return (y)
  } else if (x!="application/octet-stream" & y=="application/octet-stream") {
    return (x)
  }
  return (NA)
}

resolveConflictsProperty <- function(x,y) {
  if (is.na(x) & is.na(y)) {
   return (NA) 
  }else if (is.na(x) & !is.na(y)) {
    return (y)
  }else if (!is.na(x) & is.na(y)) {
    return (x)
  } else if (x==y) {
    return (x)
  }
  return (NA)
}

loadDataFromFile <- function(x, names) {
  txt <- readLines(x)
  splitTxt <- strsplit(txt, split="\t")
  processed <- lapply(splitTxt, processLine)
  data <- matrix(unlist(processed), nrow=length(processed), byrow=TRUE)
  colnames(data) <- c("server", "tika", "droid", "year", "amount")
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