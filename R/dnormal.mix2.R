
# NORMALMIX2 DENSITY
#' Title
#'
#' @param x 
#' @param mean 
#' @param sd 
#' @param x0 
#' @param p 
#'
#' @return
#' @export
#'
#' @examples
dnormal.mix2<-function(x,mean=c(2,8),sd=c(1,2),x0=c(0,0),p=0.5){
return(p*dnorm(x-x0[1],mean=mean[1],sd=sd[1])+(1-p)*dnorm(x-x0[2],mean=mean[2],sd=sd[2]))}

