cvlikely=function(x,hmin=min(abs(diff(sort(x))))/2,hmax=sd(x)*2,nh=20,plot.cv=TRUE,plot.dens=TRUE){
n=length(x)

hvec=seq(hmin,hmax,length.out=nh)
lvec=hvec*0
mvec=lvec
jh=0
for (h in hvec){
l=0
m=0
jh=jh+1
d0=density(x,bw=h)
m=sum((d0$y)^2)*abs(diff(d0$x)[1])

for (i in 1:n){
d=density(x[-i],from=x[i],bw=h,to=x[i],n=1)
l=l-log(d$y)
m=m-2*d$y/n
}
lvec[jh]=l
mvec[jh]=m
}

hh=bw.nrd0(x)
if(plot.cv){
dev.new()
lp=cbind(hvec,lvec)[is.finite(lvec),]
plot(lp[,1],scale(lp[,2]),type="l",
main=paste("n=",as.character(n)))

lp=cbind(hvec,mvec)[is.finite(mvec),]
lines(lp[,1],scale(lp[,2]),col=2)
abline(v=hh,col=3)
}

if(plot.dens){
dev.new()
h=hvec[which.min(lvec)]
d1=density(x,bw=h) # likelihood CV
plot(d1,type="l")
h=hvec[which.min(mvec)]
d2=density(x,bw=h) # Least squares CV
lines(d2,col=2)
d2=density(x,bw=hh) # R default (Silverman's rule)
lines(d2,col=3)

}

return(list(hvec=hvec,lvec=lvec,mvec=mvec))
}

