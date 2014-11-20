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