mime <- function(x) {
  c <- c(length(x))
  for (i in 1:length(x)) {
    values <- unlist(strsplit(x[i], split=";"))
    c[i] <- values[1]
  }
  c
}

property <- function(x, prop) {
  c <- c(length(x))
  for (i in 1:length(x)) {
    values <- unlist(strsplit(x[i], split=";"))
    found = FALSE
    for (j in 1:length(values)) {
      if (length(grep(prop,values[j]))>0) {
        values2 <- unlist(strsplit(values[j], split="="))
        c[i] <- values2[2]
        found = TRUE
        break
      }
    }
    if (found==FALSE) {
      c[i] <- NA
    }
  }
  c
}