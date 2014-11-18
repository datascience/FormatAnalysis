loadData <- function(x, names) {
  source('processLine.R')
  txt <- readLines(x)
  splitTxt <- strsplit(txt, split="\t")
  processed <- lapply(splitTxt, processLine)
  data <- matrix(unlist(processed), nrow=length(processed), byrow=TRUE)
  colnames(data) <- names
  data <- as.data.frame(data, stringAsFactors=FALSE)
  data$SERVER <- as.character(data$SERVER)
  data$TIKA <- as.character(data$TIKA)
  data$DROID <- as.character(data$DROID)
  data$YEAR <- as.integer(as.character(data$YEAR))
  data$AMOUNT <- as.integer(data$AMOUNT)
  data
}