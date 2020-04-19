eqm.sim.cv=function(n=100,ncamp=100,ngrid=100,
                    mean=c(5,8),sd=c(1,2),x0=c(0,0),p=0.5){
  print("call eqm.sim.cv")
	      f=simul.normalmix2(n=n,ncamp=ncamp,mean=mean,sd=sd,x0=x0,p=p,hmet="cv",tpause=0,ikey=F,plot=FALSE)
	      eqm	=sum((f$m-f$true)^2+f$s^2)
	      IE	=colMeans((t(f$mat)-f$true)^2)
	      return(list(eqm=eqm,h=f$h,IE=IE))
}
