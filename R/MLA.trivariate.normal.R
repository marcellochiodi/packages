#' @title Plots a trivariate normal distribution
#' with 3d-ellipse contours,
#'  optionally with random points  
#'
#' @description  \code{MLA.trivariate.normal} plots a trivariate normal distribution
#' with 3d-ellipse contours at different levels,
#'  optionally with random points  
#' @details The complete representations draws ellipses together 
#' with points, coloured according to the level of density. Also principal
#' axes can be drawn.
#' @param sigma a 3x3 variance-covariance matrix
#' @param mu a vector of three mean values
#' @param lev a vector of levels for the 3d contour ellipses
#' @param vec.col a vector of colurs numbers for the 3d contour ellipses
#' @param grid.ell if TRUE a mesh is plotted on the ellipses surfaces
#' @param axes if TRUE principal axes of the ellipses are drawn
#' @param ext extension, in standardized units, of principal axes
#' @param zplane.cond if TRUE a horizontal plane is plotted at level z.val
#' @param z.val value at which a horizontal plane is plotted
#' @param npoints number of points drawn 
#' from the trivariate normal distribution and 
#' coloured according to vec.col
#' @param ... graphical parameters for points drawing
#'
#' @return a 3d plot of a trivariate normal. According to the options used, 
#' a sample is drawn and plotted using the same colour used for the nearest external ellipse. 
#' (points inside a most internal black ellipse will be black )
#' @export
#'
#' @examples \donttest{
#' MLA.trivariate.normal(npoints=1000)
#' MLA.trivariate.normal(sigma=matrix(c(1,0.0,0.0,0.0,1,0.8,0.0,0.8,1),3,3),npoints=100000,size=1)
#' MLA.trivariate.normal(sigma=matrix(c(1,0.0,0.0,0.0,1,0.8,0.0,0.8,1),3,3),lev=0.5, npoints=1000)
#' }
MLA.trivariate.normal=function(sigma=matrix(c(1,0.5,0.5,0.5,1,0.5,0.5,0.5,1),3,3),mu=c(0,0,0),lev=c(0.25,0.50,0.75),vec.col=1:(length(lev)+1),
                  grid.ell=TRUE,axes=TRUE,ext=4,
                  zplane.cond=TRUE,z.val=1,npoints=0,...){

  nlev=length(lev)
alpha1=0.4
alpha2=0.2
alpha3=0.1
v=eigen(sigma)$vectors
if(npoints>0){
  x=rmvnorm(npoints,mean=mu,sigma=sigma)  
  
  x2  =mahala.x(x,mu,sigma,qplot=FALSE)
  q   =pchisq(x2,df=3)
  qcol=1
  for(i in 1:nlev) qcol=qcol+(q>lev[i])
  
  points3d(x,col=vec.col[qcol],...)
  
}

for(i in 1:nlev)plot3d(ellipse3d(sigma,center=mu,level=lev[i]),alpha=alpha3,fog=FALSE,pixmap="rgb",col=vec.col[i],add=TRUE)
   if (grid.ell) plot3d(ellipse3d(sigma,center=mu,level=lev[nlev]),back="lines",front="lines",col=vec.col[nlev],add=TRUE)
  if (axes){
    abclines3d(mu[1],mu[2],mu[3],v[,1],col="blue",lwd=3)
    abclines3d(mu[1],mu[2],mu[3],v[,2],col="red",lwd=3)
    abclines3d(mu[1],mu[2],mu[3],v[,3],col="green",lwd=3)
  }
  
  if(zplane.cond)planes3d(a=0,0,1,d=z.val,alpha=alpha2,col="blue")
  axes3d(box=TRUE)
}


"%^%" <- function(x, n)  with(eigen(x), vectors %*% (values^n * t(vectors)))
