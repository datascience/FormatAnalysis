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
