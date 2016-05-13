library(ggplot2)

df <- read.table(paste("input data/UKWAPerYear.tsv", sep=""), header=TRUE, sep="\t", stringsAsFactors=FALSE)

print(sum(as.numeric(df$amount)))
archPlot <- ggplot(df, aes(x=year, y=amount)) + geom_bar(stat="identity") +
  scale_y_continuous(label=function(x){paste(x/1000000,"M", sep="")}, expand = c(0,0)) +
  theme(axis.title=element_blank(),
        legend.text=element_text(face="bold", size=16), axis.text=element_text(size = 20))
  
png(filename="output data/ukwa.png", width = 1200, height = 600, res=200)
print(archPlot)
dev.off()
