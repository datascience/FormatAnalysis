loadData <- function(x, names) {
  source('processLine.R')
  txt <- readLines(x)
  splitTxt <- strsplit(txt, split="\t")
  processed <- lapply(splitTxt, processLine)
  data <- matrix(unlist(processed), nrow=length(processed), byrow=TRUE)
  colnames(data) <- names
  data <- as.data.frame(data, stringAsFactors=FALSE)
  data$server <- as.character(data$server)
  data$tika <- as.character(data$tika)
  data$droid <- as.character(data$droid)
  data$year <- as.integer(as.character(data$year))
  data$amount <- as.integer(as.character(data$amount))
  return(data)
}