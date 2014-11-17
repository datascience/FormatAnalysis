source('loadData.R')
source('utils.R')
file <- "/home/kresimir/Projects/FormatAnalysis/fmts-cleaned.tsv"
file2 <- "/home/kresimir/Projects/FormatAnalysis/test.tsv"
data <- loadData(file)
print(dim(data))
mimeF <- mime(data$DROID)
print("done with calculating mime")
w <- (mimeF == "application/pdf")
mimeF <- mimeF[w]
propertyF <- property(data[w,]$DROID,"version")
yearF <- data[w,]$YEAR
amountF <- data[w,]$AMOUNT
data3 <- data.frame(MIME=mimeF, VERSION=propertyF, YEAR=yearF, AMOUNT=amountF)
