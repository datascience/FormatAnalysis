path <- paste("output data/")

png(filename=paste(path, "bass.png", sep=""))
layout(matrix(c(1, 2, 3,4), 2, 2, byrow = TRUE), heights = c(1.5, 1.5, 1.5, 1.5))
par(mar=c(3.3,3.3,0.3,0.3), font=2)

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
lines(x,y)
polygon(c(x,rev(x)), c(rep(0,length(x)),rev(yp)), col='gray85', border=NA)
lines(x,yp)
text(40,0.4, labels=expression(paste(frac(q,p),"=50")))
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
lines(x,y)
polygon(c(x,rev(x)), c(rep(0,length(x)),rev(yp)), col='gray85', border=NA)
lines(x,yp)
text(8,2.1, labels=expression(paste(frac(q,p),"=9")))
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
lines(x,y)
polygon(c(x,rev(x)), c(rep(0,length(x)),rev(yp)), col='gray85', border=NA)
lines(x,yp)
text(175,0.1, labels=expression(paste(frac(q,p),"=600")))
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
      ann=FALSE, xaxs="i", yaxs="i")
lines(x,y)
polygon(c(x,rev(x)), c(rep(0,length(x)),rev(yp)), col='gray85', border=NA)
lines(x,yp)
text(12,1.3, labels=expression(paste(frac(q,p),"=1.5")))
#lines(x,yq)

dev.off()
