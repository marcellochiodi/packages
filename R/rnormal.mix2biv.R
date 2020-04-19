#' @title Generates bivariate vectors from a mixture
#'  of two bivariate normal 
#'  distributions
#' @description Generates bivariate vectors from a mixture
#'  of two bivariate normal 
#'  distributions with mixing proportion p and (1-p)
#'  
#' @param n number of bivariate vectors to generate 
#' @param mean two vectors of means (one for each component)
#' @param sd two vectors of standard deviations (one for each component)
#' @param r two values of correlation (one for each component)
#' @param p weight parameter
#'
#' @return a nx2 matrix of random bivariate vectors
#' @export
#'
#' @examples \donttest{
#' plot(rnormal.mix2biv(10000,r=c(0,-0.8),p=0.2),pch=".")}
#' 
rnormal.mix2biv<-function(n=100,mean=rbind(c(0,0),c(2,2)),sd=rbind(c(1,1),c(1,1)),r=c(0,0),p=0.5){
ind<-2-as.numeric(runif(n)<p)
vx=sd[1,1]
vy=sd[1,2]
sig1=rbind(c(vx^2,vx*vy*r[1]),c(vx*vy*r[1],vy^2))
vx=sd[2,1]
vy=sd[2,2]
sig2=rbind(c(vx^2,vx*vy*r[2]),c(vx*vy*r[2],vy^2))
x=rmvnorm.mixt(n, props=c(p,1-p), mus=mean,  Sigmas=rbind(sig1,sig2))
return(x)
}
