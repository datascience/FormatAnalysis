discardNotComplete <- function(data, properties) {
  num1 <- nrow(data)
  for (prop in properties) {
    data <- data[!(is.na(data[[prop]])) | data[[prop]]=="" | is.null(data[[prop]]) 
                 | length(data[[prop]])==0,]
  }
  
  #data <- data[!(data$release.year==""),]
  num2 <- nrow(data)
  dif <- abs(num2 - num1)
  if (dif>0) {
    print (paste("Discarded ", dif, " entries from the market definition due to the incomplete values"))
  }
  return (data)
}