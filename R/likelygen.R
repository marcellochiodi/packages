############################################
#
# free code by Marcello Chiodi (Feb. 2012)
# you can reuse it, quoting the author 
# didactical purpose only
#
# source the code of this file in R and then use draw1() or draw1(x)
# if x is a sample
# library(rpanel)
#
likelygen<- function(panel) {# f2
# hist(panel$y, breaks=20, freq=FALSE, main="",col=grey(.99))
# draw.sing=FALSE

z=panel$y
ymax=2*max(density(z)$y)

n=length(z)
hh=panel$h
eps=diff(range(z))/(2*n)
ry  =c(min(z)-eps,max(z)+eps)
x=seq(min(ry),max(ry),length.out=1000)

if (panel$ind==1){
#d=density(panel$y,bw=hh)
plot(z,array(0,n),ylim=c(0,ymax),xlim=(ry),xlab="x",ylab="f(x)",main=paste("h=",round(hh,2)))

lines(density(z,bw=hh),col=2,lwd=2)
abline(h=0,lty=3)

# drawing single components of the kernel
if (panel$draw.sing){
abline(v=z,lty=3)
for (i in 1:n){
lines(x,dnorm(x,mean=z[i],sd=hh)/n,col=3)
segments(z[i],0,z[i],dnorm(z[i],mean=z[i],sd=hh)/n,col=1)
}


}
}
else if (panel$ind==2)
{
plot(z,array(0,n),ylim=c(0,ymax),xlim=(ry),xlab="x",ylab="f(x)",main=paste("h=",as.character(round(hh,2))
))
lines(density(z,bw=hh,kernel="rect"),col=2,lwd=2)
abline(h=0,lty=3)


# drawing single components of the kernel
if (panel$draw.sing){
abline(v=z,lty=3)
for (i in 1:n){
lines(x,dunif(x,min=z[i]-sqrt(3)*hh,max=z[i]+sqrt(3)*hh)/n,col=3)
segments(z[i],0,z[i],dunif(z[i],min=z[i]-sqrt(3)*hh,max=z[i]+sqrt(3)*hh)/(n),col=1)
}
}
}
else
{
hist(z, breaks=20, freq=FALSE,col=grey(.8)
,xlab="x",ylab="f(x)",main=paste("h=",as.character(round(hh,2))))
lines(density(z,bw=hh),col=1,lwd=2)
lines(density(z,bw=hh,kernel="epan"),col=2,lwd=2)
lines(density(z,bw=hh,kernel="rect"),col=3,lwd=2)
lines(density(z,bw=hh,kernel="trian"),col=4,lwd=2)
}
#points(x=panel$y,y=0)
panel
}
