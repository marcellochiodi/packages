rnormal.mix2<-function(n=100,mean=c(2,8),sd=c(1,2),x0=c(0,0),p=0.5){
ind<-2-as.numeric(runif(n)<p)
return(x0[ind]+rnorm(n,mean=mean[ind],sd=sd[ind]))}
