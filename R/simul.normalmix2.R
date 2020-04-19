simul.normalmix2<-function(n=400,
			  ncamp=100,
			  ngrid=100,
			  mean=c(3,8),sd=c(1,2),p=0.5,
			  x0=c(0,0),
			  hs=.5, hmet="fixed",
			  kernel="gaussian",
			  plot=TRUE,plot.band=TRUE,tpause=0.5,ikey=FALSE,newwindow=TRUE)

{ eps=0.00001
  k<-length(mean)
  xgrid<-seq(x0[1]+qnorm(eps,mean=mean[1],sd=sd[1]),x0[k]+qnorm(1-eps,mean=mean[k],sd=sd[k]),length=ngrid+2)
  xgrid<-xgrid[2:(ngrid+1)]
  d.true<-dnormal.mix2(xgrid,mean=mean,sd=sd,x0=x0,p=p)
  mat<-matrix(NA,ncamp,ngrid)
  h<-numeric(0)

  for (i in 1:ncamp){
    xx<-rnormal.mix2(n,mean=mean,sd=sd,x0=x0,p=p)
    h[i]=switch(hmet,"fixed"=hs,"cv"=bw.bcv(xx),"silv"=bw.nrd0(xx))    
    kni<-switch(kernel,"gaussian"=density(xx,bw=h[i],n=ngrid,from=min(xgrid),to=max(xgrid),kernel="gaussian"),
                    "rectangular"=density(xx,bw=h[i],n=ngrid,from=min(xgrid),to=max(xgrid),kernel="rectangular"))
    mat[i,]<-kni$y
		    }
m=apply(mat,2,mean)
s=apply(mat,2,sd)
me=apply(mat,2,median)
q1=apply(mat,2,quantile,probs=0.05)
q2=apply(mat,2,quantile,probs=0.95)
	
  if (plot==TRUE){
# graphics.off() 	
	options(windowsBuffered=(tpause==0))
if (newwindow)    dev.new()
     plot(xgrid,d.true,col=3,lwd=3,type="l",xlab="x",ylab=expression(f(x)))
      for (i in 1:ncamp){
         lines(xgrid,mat[i,],col=1)
         Sys.sleep(tpause)
	if(ikey)key.press()
	  }
titleh=switch(hmet,"fixed"=paste(" fixed h=",hs),"cv"="Cross Valid.","silv"="Silverman")    

title(main=paste("n=",n, "; num.camp=",ncamp,"; h ",titleh),
      sub=paste("M1= ",mean[1],"; M2= ",mean[2],"; S1= ",sd[1],"; S2= ",sd[2],"; p1= ",p) )


      lines(xgrid,d.true,col=3,lwd=3)
if(plot.band){
      lines(xgrid,m,col=2,lwd=3)
      lines(xgrid,me,col=4,lwd=3)
      lines(xgrid,m+s,col=2,lwd=2)
      lines(xgrid,m-s,col=2,lwd=2)
      lines(xgrid,q1,col=4,lwd=3)
      lines(xgrid,q2,col=4,lwd=3)
     lines(xgrid,d.true,col=3,lwd=3,type="l")
	}


}
  return(list(n=n,m=m,me=me,s=s,ncamp=ncamp,
	  mat=mat,
	  h=h,
	  true=d.true,
	  xgrid=xgrid))
}
