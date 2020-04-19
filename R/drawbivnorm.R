



#rgl.snapshot("rgl1.png")

#' @title drawbivnorm
#'
#' @description Interactive visualization ov bivariate normal density
#' @details Interactive didactic example of bivariate normal density
#' (centered on the origin)
#' trough an interactive panel which controls a 3d plot 
#' allowing \code{rho} and \code{sigma2} to vary through sliders. 
#' The other scale parameter is fixed to one. 
#' 
#'
#' @param rho correlation between components
#' @param sigma2 standard deviation of 
#' the second variable (the first is fixed to one)
#'
#' @return An interactive 3d plot
#' @export
#'
#' @examples \dontrun{drawbivnorm()}	
#' 
#' 
drawbivnorm<-function(rho=0,sigma2=1){
  bg3d("white")
  par3d(windowRect = c(20, 30, 800, 800))
  view3d(15,25)
  
  
  
  panel2 <- rp.control(title="bandwidth",rho=rho,sigma2=sigma2)
  rp.slider(panel2, sigma2, 0.1, 10, title="sigma2", showvalue=TRUE,bandwith)
  rp.slider(panel2, rho, -0.99, 0.99, showvalue=TRUE,bandwith)
  # rp.button(panel2,reset,"reset values")
  # rgl.snapshot( filename, fmt="png", top=TRUE )
  #   rgl.snapshot( "kernelbiv1.png", fmt="png", top=TRUE )
}

dens.biv= function(x,y,s1=1,s2=1,m1=0,m2=0,r=0) {
z1=(x-m1)/s1
z2=(y-m2)/s2
exp(-0.5*(z1*z1+z2*z2-2*r*z2*z1)/(1-r*r))/(2*pi*s1*s2*sqrt(1-r*r))
}
##################################


bivnorm.plot=function(s1=1,s2=1,r=0,ngrid=101,
                      col1="black",col2="green",alpha1=0.2,alpha2=0.3) {
x=seq(-5,5,length.out=ngrid)
y=x
dens= outer(x,y,"dens.biv",r=r,s2=s2)

n=length(x)
#zz =dens$z/max(dens$z)
zz =dens/max(dens)

rgl.surface(x,y,4*zz,fog=F,col=col1,alpha=alpha1,back="lines",front="lines")
title3d(main=paste("r=",as.character(round(r,2))),col=1)

#rgl.surface(x,y,4*zz, col=rgb(zz,0,1-zz),alpha=alpha2,fog=FALSE,pixmap="rgb",lit=FALSE,shin=0.0) 
axes3d()

rgl.surface(x,y,0*zz, col=rgb(zz,0,1-zz) ,alpha=alpha2,fog=FALSE,pixmap="rgb",lit=FALSE,shin=0.0)
}



bandwith<- function(panel2) {# f2
  
  rgl.bringtotop()
rgl.clear()

bivnorm.plot(r=panel2$rho,s2=panel2$sigma2,ngrid=81)
panel2
}

reset=function(panel){
panel$sigma2=1
panel$rho=0
}





