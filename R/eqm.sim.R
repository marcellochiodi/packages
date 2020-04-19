eqm.sim=function(hs,n=100,ncamp=100,ngrid=100,mean=c(5,8),sd=c(1,2),x0=c(0,0),p=0.5){
  print("call eqm.sim")
	      f=simul.normalmix2(n=n,ncamp=ncamp,mean=mean,sd=sd,x0=x0,p=p,
	      hs=hs,hmet="fixed",tpause=0,ikey=F,plot=FALSE)
	      eqm=sum((f$m-f$true)^2+f$s^2)*mean(diff(f$xgrid))
	      return(eqm)
}

