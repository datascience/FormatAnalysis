
rules <- read.table("input data/conflicts_rules.txt", header=TRUE, sep="\t", stringsAsFactors=FALSE)

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
    value <- rules[rules$element=="mime" & ((rules$value1==x & rules$value2==y) | 
                                              (rules$value1==y & rules$value2==x)),]$value2
    if (!is.na(value)) {
      return (value)
    }else {
      return (x)
    }
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