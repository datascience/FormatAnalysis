path <- paste("output data/")

png(filename=paste(path, "bass.png", sep=""))
# layout(matrix(c(1, 2, 3,4), 2, 2, byrow = TRUE), heights = c(1.5, 1.5, 1.5, 1.5))
# par(mar=c(0.3,0.3,0.3,0.3), font=2)
# 
# p <- 0.004
# q <- 0.2
# m <- 10
# x <- seq(0,50,len=200)
# y <- m*((p+q)^2/p)*((exp(-(p+q)*x))/(((q/p)*exp(-(p+q)*x) + 1 )^2))
# q1 <- 0
# yp <- p*m*(1-(1-exp(-(p+q)*x))/((q/p)*exp(-(p+q)*x)+1))
# yq <- y - yp
# plot(NULL, main ="Bass", xlim=c(0,50), ylim=c(0,max(y)+0.1*max(y)), 
#      xaxt='n', yaxt='n', ann=FALSE, xaxs="i", yaxs="i")
# lines(x,y,lwd=3)
# polygon(c(x,rev(x)), c(rep(0,length(x)),rev(yp)), col='gray85', border=NA)
# lines(x,yp,lwd=3)
# text(40,0.4, labels=expression(paste(frac(q,p),"=50")), cex=2)
# #lines(x,yq)
# 
# 
# p <- 0.1
# q <- 0.9
# m <- 10
# x <- seq(0,10,len=200)
# y <- m*((p+q)^2/p)*((exp(-(p+q)*x))/(((q/p)*exp(-(p+q)*x) + 1 )^2))
# q1 <- 0
# yp <- p*m*(1-(1-exp(-(p+q)*x))/((q/p)*exp(-(p+q)*x)+1))
# yq <- y - yp
# plot(NULL, main ="Bass", xlim=c(0,10), ylim=c(0,max(y)+0.1*max(y)),
#      xaxt='n', yaxt='n', ann=FALSE, xaxs="i", yaxs="i")
# lines(x,y,lwd=3)
# polygon(c(x,rev(x)), c(rep(0,length(x)),rev(yp)), col='gray85', border=NA)
# lines(x,yp,lwd=3)
# text(8,2.1, labels=expression(paste(frac(q,p),"=9")), cex=2)
# #lines(x,yq)
# 
# p <- 0.0001
# q <- 0.06
# m <- 10
# x <- seq(0,200,len=200)
# y <- m*((p+q)^2/p)*((exp(-(p+q)*x))/(((q/p)*exp(-(p+q)*x) + 1 )^2))
# q1 <- 0
# yp <- p*m*(1-(1-exp(-(p+q)*x))/((q/p)*exp(-(p+q)*x)+1))
# yq <- y - yp
# plot(NULL, main ="Bass", xlim=c(0,200), ylim=c(0,max(y)+0.1*max(y)),
#      xaxt='n', yaxt='n', ann=FALSE, xaxs="i", yaxs="i")
# lines(x,y,lwd=3)
# polygon(c(x,rev(x)), c(rep(0,length(x)),rev(yp)), col='gray85', border=NA)
# lines(x,yp,lwd=3)
# text(175,0.1, labels=expression(paste(frac(q,p),"=600")), cex=2)
# #lines(x,yq)
# 
# p <- 0.2
# q <- 0.3
# m <- 10
# x <- seq(0,15,len=200)
# y <- m*((p+q)^2/p)*((exp(-(p+q)*x))/(((q/p)*exp(-(p+q)*x) + 1 )^2))
# q1 <- 0
# yp <- p*m*(1-(1-exp(-(p+q)*x))/((q/p)*exp(-(p+q)*x)+1))
# yq <- y - yp
# plot(NULL, main ="Bass", xlim=c(0,15), ylim=c(0,max(y)+0.1*max(y)),
#      xaxt='n', yaxt='n', ann=FALSE, xaxs="i", yaxs="i")
# lines(x,y,lwd=3)
# polygon(c(x,rev(x)), c(rep(0,length(x)),rev(yp)), col='gray85', border=NA)
# lines(x,yp,lwd=3)
# text(12,1.3, labels=expression(paste(frac(q,p),"=1.5")), cex=2)
# #lines(x,yq)


#only one figure 

p <- 0.1
q <- 0.9
m <- 10
x <- seq(0,10,len=200)
y <- m*((p+q)^2/p)*((exp(-(p+q)*x))/(((q/p)*exp(-(p+q)*x) + 1 )^2))
q1 <- 0
yp <- p*m*(1-(1-exp(-(p+q)*x))/((q/p)*exp(-(p+q)*x)+1))
yq <- y - yp

#layout(matrix(c(1, 2), 2, 1, byrow = TRUE))
par(fig=c(0,1,0,1), oma=c(0,0,0,0), mar=c(5.5,2.5,2.0,2.0), font=2, mgp=c(0.8,1,0))
plot(NULL, xlim=c(0,10), ylim=c(0,max(y)+0.1*max(y)),
     xaxt='n', yaxt='n', xlab="Time", ylab="New adopters", xaxs="i", yaxs="i", 
     cex.lab = 2)
lines(x,y, lwd=3)
polygon(c(x,rev(x)), c(rep(0,length(x)),rev(yp)), col='gray85', border=NA)
lines(x,yp, lty=3, col="red", lwd=3)
lines(x,yq, lty=2, col="blue", lwd=3)
text(4,2.7, labels="New adopters", adj = c(0,0), cex = 1.5)
arrows(3.9,2.75,x[50]+0.1,y[50]+0.001,length=0.15,angle=30, lwd = 2)
text(6,0.5, labels="p = 0.1", adj = c(0,0), cex = 1.5)
arrows(7.9,0.45,x[70],yp[70],length=0.15,angle=30, lwd = 2)
text(5.5,1.8, labels="q = 0.9", adj = c(0,0), cex = 1.5)
arrows(5.9,1.75,x[70],yq[70],length=0.15,angle=30, lwd = 2)
par(xpd=TRUE)
legend(1.5, -0.3, c("External influence", "Internal influence"), 
       col=c("blue", "red"), lwd=c(3,3), lty=c(2,3), xpd=TRUE, horiz = TRUE, 
       inset = c(0,0), bty = "n")
dev.off()
