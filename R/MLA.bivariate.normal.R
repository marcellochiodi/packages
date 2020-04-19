#' @title Plts of 2d  contour ellipse of a bivariate normal distribution,
#'  optionally with random points  
#'
#' @description  \code{MLA.bivariate.normal} plots a bivariate normal distribution
#' with 2d-ellipse contours at different levels,
#'  optionally with random points  
#' @details The complete representations draws ellipses together 
#' with points, coloured according to the level of density. Also principal
#' axes can be drawn.
#'
#' @param rho a scalar: correlation of components
#' @param mu a two-elements vector of means
#' @param s a two-elements vector of standard deviation
#' @param lev a vector of levels for the 2d contour ellipses
#' @param vec.col a vector of colurs numbers for the 2d contour ellipses
#' @param axes if \code{TRUE} principal axes will be drawn 
#' (no axes drawn at the present moment)
#' @param npoints number of points drawn 
#' from the bivariate normal distribution and 
#' coloured according to vec.col
#' @param ... graphical parameters for points drawing
#'
#' @return only plotting is done
#' @export
#'
#' @examples
MLA.bivariate.normal=function(rho=.5,mu=c(0,0),
                  s=c(1,1),lev=c(0.25,0.50,0.75),vec.col=1:(length(lev)+1),
                 axes=FALSE,npoints=0,...){

nlev=length(lev)
m1=matrix(c(s[1]^2,rho*s[1]*s[2],rho*s[1]*s[2],s[2]^2),2,2)
fac=3.5
smax=max(s[1],s[2])
xlim=c(mu[1]-fac*smax,mu[1]+fac*smax)
ylim=c(mu[2]-fac*smax,mu[2]+fac*smax)
v=eigen(m1)$vectors
plot(1, type="n", xlim=xlim, ylim=ylim)
if(npoints>0){
  x=rmvnorm(npoints,mean=mu,sigma=m1)  
  
  x2  =mahala.x(x,mu,m1,qplot=FALSE)
  q   =pchisq(x2,df=2)
  qcol=1
  for(i in 1:nlev) qcol=qcol+(q>lev[i])
  
    points(x,col=vec.col[qcol],...)

}
else
{
  
}
lwd=1+(npoints>10000)
for(i in 1:nlev) lines(ellipse(rho,center=mu,scale=s,level=lev[i]), col=vec.col[i],lwd=lwd)
#for(i in 1:nlev) lines(ellipse(m1,center=mu,level=lev[i]), col=1)
if (axes)(return("no axes drawn at the present moment"))
if (axes){
  beta1=v[2,1]/v[1,1]
  abline(mu[2]-beta1*mu[1],beta1,col="blue",lty=3)
  beta1=v[2,2]/v[1,2]
  abline(mu[2]-beta1*mu[1],beta1,col="blue",lty=3)
  beta1=m1[1,2]/m1[1,1]
  abline(mu[2]-beta1*mu[1],beta1,col="red",lty=2)
  beta1=m1[1,2]/m1[2,2]
  abline(mu[1]/beta1-mu[2],1/beta1,col="green",lty=2)
  
}
 }

