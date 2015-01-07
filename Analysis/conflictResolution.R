resolveConflictsMimeDefault <- function(row) {
  x <- row[2]
  y <- row[3]
  if (is.na(x) & !is.na(y)) {
    return (y)
  }else if (!is.na(x) & is.na(y)) {
    return (x)
  }else if (is.na(x) & is.na(y)) {
    return (NA)
  }else if (x==y) {
    return (x)
  } else if (x=="application/octet-stream" & y!="application/octet-stream") {
    return (y)
  } else if (x!="application/octet-stream" & y=="application/octet-stream") {
    return (x)
  }else if (x!=y) {
    return (x)
  }
  return (NA)
}

resolveConflictsPropertyDefault <- function(row) {
  x <- row[2]
  y <- row[3]
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