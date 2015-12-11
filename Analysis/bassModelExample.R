
library(ggplot2)
library(grid)
path <- paste("output data/")

png(filename=paste(path, "bass1.png", sep=""))
layout(matrix(c(1, 2, 3,4), 2, 2, byrow = TRUE), heights = c(1.5, 1.5, 1.5, 1.5))
par(mar=c(0.3,0.3,0.3,0.3), font=2)

p <- 0.004
q <- 0.2
m <- 10
x <- seq(0,50,len=200)
y <- m*((p+q)^2/p)*((exp(-(p+q)*x))/(((q/p)*exp(-(p+q)*x) + 1 )^2))
q1 <- 0
yp <- p*m*(1-(1-exp(-(p+q)*x))/((q/p)*exp(-(p+q)*x)+1))
yq <- y - yp
plot(NULL, main ="Bass", xlim=c(0,50), ylim=c(0,max(y)+0.1*max(y)), 
     xaxt='n', yaxt='n', ann=FALSE, xaxs="i", yaxs="i")
lines(x,y,lwd=3)
polygon(c(x,rev(x)), c(rep(0,length(x)),rev(yp)), col='gray85', border=NA)
lines(x,yp,lwd=3)
text(40,0.4, labels=expression(paste(frac(q,p),"=50")), cex=2)
#lines(x,yq)


p <- 0.1
q <- 0.9
m <- 10
x <- seq(0,10,len=200)
y <- m*((p+q)^2/p)*((exp(-(p+q)*x))/(((q/p)*exp(-(p+q)*x) + 1 )^2))
q1 <- 0
yp <- p*m*(1-(1-exp(-(p+q)*x))/((q/p)*exp(-(p+q)*x)+1))
yq <- y - yp
plot(NULL, main ="Bass", xlim=c(0,10), ylim=c(0,max(y)+0.1*max(y)),
     xaxt='n', yaxt='n', ann=FALSE, xaxs="i", yaxs="i")
lines(x,y,lwd=3)
polygon(c(x,rev(x)), c(rep(0,length(x)),rev(yp)), col='gray85', border=NA)
lines(x,yp,lwd=3)
text(8,2.1, labels=expression(paste(frac(q,p),"=9")), cex=2)
#lines(x,yq)

p <- 0.0001
q <- 0.06
m <- 10
x <- seq(0,200,len=200)
y <- m*((p+q)^2/p)*((exp(-(p+q)*x))/(((q/p)*exp(-(p+q)*x) + 1 )^2))
q1 <- 0
yp <- p*m*(1-(1-exp(-(p+q)*x))/((q/p)*exp(-(p+q)*x)+1))
yq <- y - yp
plot(NULL, main ="Bass", xlim=c(0,200), ylim=c(0,max(y)+0.1*max(y)),
     xaxt='n', yaxt='n', ann=FALSE, xaxs="i", yaxs="i")
lines(x,y,lwd=3)
polygon(c(x,rev(x)), c(rep(0,length(x)),rev(yp)), col='gray85', border=NA)
lines(x,yp,lwd=3)
text(175,0.1, labels=expression(paste(frac(q,p),"=600")), cex=2)
#lines(x,yq)

p <- 0.2
q <- 0.3
m <- 10
x <- seq(0,15,len=200)
y <- m*((p+q)^2/p)*((exp(-(p+q)*x))/(((q/p)*exp(-(p+q)*x) + 1 )^2))
q1 <- 0
yp <- p*m*(1-(1-exp(-(p+q)*x))/((q/p)*exp(-(p+q)*x)+1))
yq <- y - yp
plot(NULL, main ="Bass", xlim=c(0,15), ylim=c(0,max(y)+0.1*max(y)),
     xaxt='n', yaxt='n', ann=FALSE, xaxs="i", yaxs="i")
lines(x,y,lwd=3)
polygon(c(x,rev(x)), c(rep(0,length(x)),rev(yp)), col='gray85', border=NA)
lines(x,yp,lwd=3)
text(12,1.3, labels=expression(paste(frac(q,p),"=1.5")), cex=2)
#lines(x,yq)
dev.off()

#only one figure 

#png(filename=paste(path, "bass2.png", sep=""))
p <- 0.1
q <- 0.9
m <- 10
x <- seq(0,10,len=200)
y <- m*((p+q)^2/p)*((exp(-(p+q)*x))/(((q/p)*exp(-(p+q)*x) + 1 )^2))
q1 <- 0
yp <- p*m*(1-(1-exp(-(p+q)*x))/((q/p)*exp(-(p+q)*x)+1))
yq <- y - yp

df <- data.frame(x, y, yp, yq)

png(filename=paste(path, "bass3.png", sep=""), width = 800, height = 680)
ggplot(df, aes(x = x)) + geom_area(aes(y=yp), fill="gray75") + 
  geom_line(aes(y=y, colour="col1", linetype="col1"), size=1) + 
  geom_line(aes(y=yp, colour="col2", linetype="col2"), size=1) + 
  geom_line(aes(y=yq, color="col3", linetype="col3"), size=1) +
  scale_y_continuous(expand = c(0,0), limits=c(0,3)) + 
  scale_x_continuous(expand = c(0,0)) +
  scale_color_manual(name="", values = c("col1"="black", "col2"="red", "col3"="blue"),
                     labels=c("Total new adopters","Internal influence\n(q=0.9)",
                              "External influence\n(p=0.1)")) + 
  scale_linetype_manual(name="", values = c("col1"="solid", "col2"="dashed", "col3"="dashed"),
                        labels=c("Total new adopters","Internal influence\n(q=0.9)",
                                 "External influence\n(p=0.1)")) +
  labs(x="Time", y="Adopters") +
  theme(axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_text(face="italic",size=16),
        legend.position=c(0.7,0.6), legend.background=element_blank(),
        legend.text=element_text(face="bold", size=16), 
        legend.key=element_blank(), legend.key.width=unit(3.5,"line"), 
        legend.key.height=unit(3,"line"))

#ggsave(paste(path,"bass3.png",sep=""), width=4, height=3, unit="in", dpi=300)
dev.off()
  
  
# #layout(matrix(c(1, 2), 2, 1, byrow = TRUE))
# par(fig=c(0,1,0,1), oma=c(0,0,0,0), mar=c(5.5,2.5,2.0,2.0), font=2, mgp=c(0.8,1,0))
# plot(NULL, xlim=c(0,10), ylim=c(0,max(y)+0.1*max(y)),
#      xaxt='n', yaxt='n', xlab="Time", ylab="New adopters", xaxs="i", yaxs="i", 
#      cex.lab = 1.5)
# lines(x,y, lwd=3)
# polygon(c(x,rev(x)), c(rep(0,length(x)),rev(yp)), col='gray85', border=NA)
# lines(x,yp, lty=3, col="red", lwd=3)
# lines(x,yq, lty=2, col="blue", lwd=3)
# text(4,2.7, labels="New adopters", adj = c(0,0), cex = 1.5)
# arrows(3.9,2.75,x[50]+0.1,y[50]+0.001,length=0.15,angle=30, lwd = 2)
# text(7.0,0.5, labels="Innovators", adj = c(0,0), cex = 1.5)
# arrows(7.9,0.45,x[65]-1,yp[65],length=0.15,angle=30, lwd = 2)
# text(5.5,1.8, labels="Imitators", adj = c(0,0), cex = 1.5)
# arrows(6.2,1.75,x[70]-1,yq[70],length=0.15,angle=30, lwd = 2)
# par(xpd=TRUE)
# legend(-0.5, -0.3, c("External influence (p=0.1)  ", "Internal influence (q=0.9)"), 
#        col=c("blue", "red"), lwd=c(3,3), lty=c(2,3), xpd=TRUE, horiz = TRUE, 
#        inset = c(0,0), bty = "n", cex = 1.2)
# dev.off()
