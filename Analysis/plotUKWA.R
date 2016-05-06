
df <- read.table(paste("input data/UKWAPerYear.tsv", sep=""), header=TRUE, sep="\t", stringsAsFactors=FALSE)

archPlot <- ggplot(df, aes(x=year, y=amount)) + geom_bar(stat="identity") +
  theme(axis.title=element_text(face="italic",size=16),
        legend.text=element_text(face="bold", size=16), axis.text=element_text(size = 20))
  
png(filename="output data/ukwa.png")
print(archPlot)
dev.off()

ggsave("output data/ukwa2.png" , archPlot)