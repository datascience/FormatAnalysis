library(ggplot2)

df <- read.table(paste("input data/UKWAPerYear.tsv", sep=""), header=TRUE, sep="\t", stringsAsFactors=FALSE)
df$amount <- df$amount / 1000000

archPlot <- ggplot(df, aes(x=year, y=amount)) + geom_bar(stat="identity") +
  scale_y_continuous(label=function(x){paste(x,"M", sep="")}, expand = c(0,0)) +
  theme(axis.title=element_blank(),
        legend.text=element_text(face="bold", size=16), axis.text=element_text(size = 20))
  
png(filename="output data/ukwa.png")
print(archPlot)
dev.off()

ggsave("output data/ukwa2.png" , archPlot)