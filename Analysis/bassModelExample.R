library(ggplot2)
library(grid)
library(gridExtra)

path <- paste("output data/")



#output four main patterns of the Bass model
#png(filename=paste(path, "bass1.png", sep=""))
#layout(matrix(c(1, 2, 3,4), 2, 2, byrow = TRUE), heights = c(1.5, 1.5, 1.5, 1.5))
#par(mar=c(0.3,0.3,0.3,0.3), font=2)


# First model
#originals 
#p <- 0.004
#q <- 0.2
#m <- 10

p <- 0.0490543312
q <- 0.0626791133
m <- 16763.90076

x <- seq(0,30,len=200)
y <- m*((p+q)^2/p)*((exp(-(p+q)*x))/(((q/p)*exp(-(p+q)*x) + 1 )^2))
q1 <- 0
yp <- p*m*(1-(1-exp(-(p+q)*x))/((q/p)*exp(-(p+q)*x)+1))
yq <- y - yp
# plot(NULL, main ="Bass", xlim=c(0,50), ylim=c(0,max(y)+0.1*max(y)), 
#      xaxt='n', yaxt='n', ann=FALSE, xaxs="i", yaxs="i")
# lines(x,y,lwd=3)
# polygon(c(x,rev(x)), c(rep(0,length(x)),rev(yp)), col='gray85', border=NA)
# lines(x,yp,lwd=3)
# text(40,0.4, labels=expression(paste(frac(q,p),"=50")), cex=2)
#lines(x,yq)

df <- data.frame(x, y, yp, yq)
bassGraph1<- ggplot(df, aes(x = x)) + geom_area(aes(y=yp), fill="gray75") + 
  geom_line(aes(y=y), size=1) + 
  geom_line(aes(y=yp), size=1, linetype=2, color="red") + 
  geom_line(aes(y=yq), size=1, linetype=2, color="green") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits=c(0,1.1*max(df$y))) +
  annotate(geom="text", x=max(df$x)-0.2*max(df$x), y=max(df$y)- 0.2*max(df$y), label=paste("frac(q,p)==", round(q/p, digits = 1), sep=""), color="black", parse=TRUE, size=7) +
  theme(axis.ticks=element_blank(), axis.text=element_blank(), axis.title=element_blank(), 
        plot.margin=unit(c(0.0,0.0,0.0,0.0), "mm"))



#expression(paste(frac(q,p),as.character(q/p)))



#Second model 
# originals
#p <- 0.1
#q <- 0.9
#m <- 10

p <- 0.0073622453
q <- 0.1843805408
m <- 12823.87403
x <- seq(0,30,len=200)
y <- m*((p+q)^2/p)*((exp(-(p+q)*x))/(((q/p)*exp(-(p+q)*x) + 1 )^2))
q1 <- 0
yp <- p*m*(1-(1-exp(-(p+q)*x))/((q/p)*exp(-(p+q)*x)+1))
yq <- y - yp
# plot(NULL, main ="Bass", xlim=c(0,10), ylim=c(0,max(y)+0.1*max(y)),
#      xaxt='n', yaxt='n', ann=FALSE, xaxs="i", yaxs="i")
# lines(x,y,lwd=3)
# polygon(c(x,rev(x)), c(rep(0,length(x)),rev(yp)), col='gray85', border=NA)
# lines(x,yp,lwd=3)
# text(8,2.1, labels=expression(paste(frac(q,p),"=9")), cex=2)
# #lines(x,yq)
df <- data.frame(x, y, yp, yq)
bassGraph2<- ggplot(df, aes(x = x)) + geom_area(aes(y=yp), fill="gray75") + 
  geom_line(aes(y=y), size=1) + 
  geom_line(aes(y=yp), size=1, linetype=2, color="red") + 
  geom_line(aes(y=yq), size=1, linetype=2, color="green") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits=c(0,1.1*max(df$y))) +
  annotate(geom="text", x=0.2*max(df$x), y=0.8*max(df$y), label=paste("frac(q,p)==", round(q/p),sep=""), color="black",
           parse=TRUE, size=7) + 
  theme(axis.ticks=element_blank(), axis.text=element_blank(), axis.title=element_blank(), 
        plot.margin=unit(c(0.0,0.0,0.0,0.0), "mm"))


#Third model 
#originals
#p <- 0.0001
#q <- 0.06
#m <- 10
p <- 0.013595435
q <- 0.1415115735
m <- 21283.91046
x <- seq(0,30,len=200)
y <- m*((p+q)^2/p)*((exp(-(p+q)*x))/(((q/p)*exp(-(p+q)*x) + 1 )^2))
q1 <- 0
yp <- p*m*(1-(1-exp(-(p+q)*x))/((q/p)*exp(-(p+q)*x)+1))
yq <- y - yp
# plot(NULL, main ="Bass", xlim=c(0,200), ylim=c(0,max(y)+0.1*max(y)),
#      xaxt='n', yaxt='n', ann=FALSE, xaxs="i", yaxs="i")
# lines(x,y,lwd=3)
# polygon(c(x,rev(x)), c(rep(0,length(x)),rev(yp)), col='gray85', border=NA)
# lines(x,yp,lwd=3)
# text(175,0.1, labels=expression(paste(frac(q,p),"=600")), cex=2)
# #lines(x,yq)
df <- data.frame(x, y, yp, yq)
bassGraph3<- ggplot(df, aes(x = x)) + geom_area(aes(y=yp), fill="gray75") + 
  geom_line(aes(y=y), size=1) + 
  geom_line(aes(y=yp), size=1, linetype=2, color="red") + 
  geom_line(aes(y=yq), size=1, linetype=2, color="green") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits=c(0,1.1*max(df$y))) +
  annotate(geom="text", x=0.15*max(df$x), y=0.8*max(df$y), label=paste("frac(q,p)==", round(q/p), sep=""), color="black", parse=TRUE, size=7) +
  theme(axis.ticks=element_blank(), axis.text=element_blank(), axis.title=element_blank(), 
        plot.margin=unit(c(0.0,0.0,0.0,0.0), "mm"))


#Fourth model 
# original 
#p <- 0.2
#q <- 0.3
#m <- 10

p <- 0.0002193225
q <- 0.3718790702
m <- 1948.277493

x <- seq(0,30,len=200)
y <- m*((p+q)^2/p)*((exp(-(p+q)*x))/(((q/p)*exp(-(p+q)*x) + 1 )^2))
q1 <- 0
yp <- p*m*(1-(1-exp(-(p+q)*x))/((q/p)*exp(-(p+q)*x)+1))
yq <- y - yp
# plot(NULL, main ="Bass", xlim=c(0,15), ylim=c(0,max(y)+0.1*max(y)),
#      xaxt='n', yaxt='n', ann=FALSE, xaxs="i", yaxs="i")
# lines(x,y,lwd=3)
# polygon(c(x,rev(x)), c(rep(0,length(x)),rev(yp)), col='gray85', border=NA)
# lines(x,yp,lwd=3)
# text(12,1.3, labels=expression(paste(frac(q,p),"=1.5")), cex=2)
# #lines(x,yq)
df <- data.frame(x, y, yp, yq)
bassGraph4<- ggplot(df, aes(x = x)) + geom_area(aes(y=yp), fill="gray75") + 
  geom_line(aes(y=y), size=1) + 
  geom_line(aes(y=yp), size=1, linetype=2, color="red") + 
  geom_line(aes(y=yq), size=1, linetype=2, color="green") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits=c(0,1.1*max(df$y))) +
  annotate(geom="text", x=0.3*max(df$x), y=0.8*max(df$y), label=paste("frac(q,p)==", round(q/p), sep=""), color="black", parse=TRUE, size=7) +
  theme(axis.ticks=element_blank(), axis.text=element_blank(), axis.title=element_blank(), 
        plot.margin=unit(c(0.0,0.0,0.0,0.0), "mm"))

png(filename=paste(path, "bass1.png", sep=""), width=1800, height=1200, res = 300)
print(grid.arrange(bassGraph1, bassGraph3, bassGraph2, bassGraph4, ncol = 2, nrow = 2))


dev.off()







#only one figure 

# original
# p <- 0.1
# q <- 0.9
# m <- 10
# x <- seq(0,10,len=200)


p <- 0.013595435
q <- 0.1415115735
m <- 21283.91046
x <- seq(0,60,len=200)
y <- m*((p+q)^2/p)*((exp(-(p+q)*x))/(((q/p)*exp(-(p+q)*x) + 1 )^2))
q1 <- 0
yp <- p*m*(1-(1-exp(-(p+q)*x))/((q/p)*exp(-(p+q)*x)+1))
yq <- y - yp
derv <- ((m/p)*(p+q)^3*exp(-(p+q)*x)*((q/p)*exp(-(p+q)*x)-1))/(((q/p)*exp(-(p+q)*x)+1)^3)
df <- data.frame(x, y, yp, yq, derv)

T1 <- -(1/(p+q))*log((2+sqrt(3))*(p/q))
TP <- -(1/(p+q))*log(p/q)
T2 <- -(1/(p+q))*log((1/(2+sqrt(3)))*(p/q))

lSize = 0.4

bassGraph<- ggplot(df, aes(x = x)) + geom_area(aes(y=yp), fill="gray75") + 
  geom_line(aes(y=y, colour="col1", linetype="col1"), size=0.5) + 
  geom_line(aes(y=yp, colour="col2", linetype="col2"), size=0.5) + 
  geom_line(aes(y=yq, color="col3", linetype="col3"), size=0.5) +
  scale_y_continuous(expand = c(0,0), limits = c(0,1.1*max(y))) + 
  scale_x_continuous(expand = c(0,0) , limits=c(0,60)) +
  scale_color_manual(name="", values = c("col1"="black", "col2"="red", "col3"="green"),
                     labels=c("New adoptions","External influence\n(p=0.014)",
                              "Internal influence\n(q=0.14)")) + 
  scale_linetype_manual(name="", values = c("col1"="solid", "col2"="dashed", "col3"="longdash"),
                        labels=c("New adoptions","External influence\n(p=0.014)",
                                 "Internal influence\n(q=0.14)")) +
  labs(x=NULL, y="Adoption rate") + 
  geom_vline(xintercept = T1, linetype=2, color="gray50", size=lSize) +
  geom_vline(xintercept = TP, linetype=2, color="gray50", size=lSize) + 
  geom_vline(xintercept = T2, linetype=2, color="gray50", size=lSize) +
  geom_hline(yintercept = 0) +
  theme(axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_text(face="italic",size=10),
        legend.position=c(0.7,0.6), legend.background=element_blank(),
        legend.text=element_text(face="bold", size=8), 
        legend.key=element_blank(), legend.key.width=unit(1.5,"line"), 
        legend.key.height=unit(1.5,"line") , plot.margin=unit(c(0,2,-0.5,1.5),"mm"))

bassDerv <- ggplot(df, aes(x=x, y=derv)) +
  geom_area(fill="gray", alpha=0.3) +
  geom_line(aes(linetype="line1")) + labs(x="Time", y="Change rate") + scale_x_continuous(expand = c(0,0)) +
  scale_linetype_manual(name="", values= c("line1"="dotted"), labels=c("Change rate")) +
  scale_y_continuous(expand = c(0,0), limits = c(1.1*min(derv), 1.1*max(derv))) + 
  geom_vline(xintercept = T1, linetype=2, color="gray50", size=lSize) +
  geom_vline(xintercept = TP, linetype=2, color="gray50", size=lSize) + 
  geom_vline(xintercept = T2, linetype=2, color="gray50", size=lSize) +
  #geom_hline(yintercept = 1.1*min(derv)) +
  theme(axis.ticks=element_blank(), axis.text=element_blank(), axis.title=element_text(face="italic",size=10),
        legend.position=c(0.7,0.8), legend.background=element_blank(),
        legend.text=element_text(face="bold", size=8), 
        legend.key=element_blank(), legend.key.width=unit(1.5,"line"), 
        legend.key.height=unit(1.5,"line"),
        plot.margin=unit(c(-0.5,2,1,1.5),"mm"))

png(filename=paste(path, "bass3.png", sep=""), width=1800, height=1200, res=300)
print(grid.arrange(bassGraph, bassDerv))
#print(bassGraph)
dev.off()
  

