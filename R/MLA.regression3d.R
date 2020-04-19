#' @title Visualization of a multiple linear regression with two regressors
#' @description Visualization of a multiple linear regression with confidence
#' region for expected values   \code{E(y)=beta0+beta1*x1+beta2*x2} 
#' and for future observations  input is a matrix with \code{x1,x2,y}


#' @param data.obs A matrix of observed data set with 3 columns, \code{x1,x2,y}
#'
#' @param ext proportion of extension of ranges of \code{x1,x2} 
#' @param newplot logical. if \code{newplot=TRUE} a new 3d device is opened
#' @param points logical; if \code{points=TRUE} points are plotted
#' @param conf.exp logical; if \code{ conf.exp=TRUE} confidence surfaces for expected values are drawn 
#' @param conf.sing logical; if \code{conf.sing=TRUE} confidence surfaces for future  observations are drawn
#' @param level numerical; confidence level for confidence surfaces 
#' @param ell logical; if \code{ ell=TRUE}  ellipses for the conjoint  distribution of \code{x1,x2} are drawn at a prob. level given by \code{liv.ell}.
#' @param liv.ell numerical; prob. level for the \code{x1,x2}ellipses
#' @param ngrid integer. Number of grid points for each axis  in \code{x1,x2} plane to compute confidence surfaces
#' @param rotation a rotation of the final 3d view is performed
#' @param col.obs color of observed points
#' @param col.fit color of fitted points
#'
#' @return no value is returned: only 3d plotting is performed
#' @export
#'
#' @examples \donttest{
#' data("antropometric")
#' MLA.regression3d(antropometric[101:300,c(7,9,8)],conf.exp=FALSE,level=0.99)
#' open3d()
#' MLA.regression3d(antropometric[101:200,c(7,9,8)],level=0.99)
#' open3d()
#' MLA.regression3d(antropometric[101:200,c(7,13,8)],level=0.99,ell=TRUE)}
#' 
#' 
#' 




MLA.regression3d=function(data.obs,ext=0.3,newplot=TRUE,points=TRUE,
                      conf.exp=TRUE,conf.sing=FALSE,level=0.95, 
                      ell=FALSE,liv.ell=0.2,ngrid=50,rotation=FALSE, col.obs=1,col.fit=2){

#plot3d(data.obs[,1:3])
if (newplot) p3d=plot3d(data.obs[,1:3],type="n")
if(points) points3d(data.obs[,1:3],size=3,col=col.obs)
dat=as.data.frame(complete(data.obs))
old.names=names(dat)
names(dat)=c("x1","x2","y")
p1=lm(y~x1+x2,data=dat)

if(points) points3d(dat$x1,dat$x2,p1$fitted,col=col.fit)


#r1=diff(range(dat$x1))*ext
#r2=diff(range(dat$x2))*ext

#teo=as.data.frame(xy.grid(c(min(dat$x1)-r1,max(dat$x1)+r1),
#c(min(dat$x2)-r2,max(dat$x2)+r2),nx=ngrid))


teo=as.data.frame(xy.grid(
  enlarged.range(dat$x1,ext),enlarged.range(dat$x2,ext),nx=ngrid))

names(teo)=c("x1","x2")
pr1=predict.lm(p1,newdata=teo,se.fit=TRUE,level=level,interval="confidence")

coefs=coef(p1)
planes3d(coefs[2],coefs[3],-1,coefs[1],col=4,alpha=0.4)
#surface3d(unique(teo[,1]),unique(teo[,2]),pr1$fit[,1],alpha=0.8,color="red",fog=F,fron="line",back="line",col="black")

scal.col=(pr1$se.fit/max(pr1$se.fit))^2


if(conf.exp) {
surface3d(unique(teo[,1 ]),unique(teo[,2]),pr1$fit[,2],alpha=0.3,col=rgb(scal.col,0,0))
surface3d(unique(teo[,1 ]),unique(teo[,2]),pr1$fit[,3],alpha=0.3,col=rgb(scal.col,0,0))
}
if(conf.sing) {
pr1=predict.lm(p1,newdata=teo,se.fit=TRUE,level=level,interval="prediction")
surface3d(unique(teo[,1 ]),unique(teo[,2]),pr1$fit[,2],alpha=0.2,col=rgb(0,scal.col,0))
surface3d(unique(teo[,1 ]),unique(teo[,2]),pr1$fit[,3],alpha=0.2,col=rgb(0,scal.col,0))
}
if (ell){
difference=pr1$fit[,1]-pr1$fit[,2]
liv=min(difference)+diff(range(difference))*liv.ell
surface3d(unique(teo[,1 ]),unique(teo[,2]),pr1$fit[,3],alpha=1,col=rgb(difference<liv,0,0))
}
if(rotation) play3d(spin3d(axis=c(0,0,1), rpm=2), duration=30)

}
