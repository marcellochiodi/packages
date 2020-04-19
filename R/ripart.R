ripart=function(x,...){
f=(1:length(x))/length(x)
par(mfrow=c(1,2))
plot(sort(x),f,type="S",...,ylab="F(x)")
t=MLA.freq(x)
plot(t$x,t$freq,type="h",ylim=c(0,max(t$freq)),...,yl="f(x)")
return(list(x=sort(x),F.emp=f))
}
