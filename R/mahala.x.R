#' @title Computes the Mahalanobis distances of data points from  a point. 
#' @description Computes the Mahalanobis distances of data points
#'  from  a given point or from the centroid. 
#'  The covariance matrix  can be given as input or computed on data
#' @details Not computationally efficient. Someday it will be
#' joined with Mahala.factor. I do not remember why I wrote 
#' two different routines...
#' 

#' @param x input data matrix
#' @param mu vector of center from which distances are computed
#' @param sigma given covariance matrix
#' @param qplot logical. if TRUE plots the final qq-plot of distances
#'
#' @return Mahalanobis distances
#' @export
#'
#' @examples
mahala.x=function(x,mu=apply(x,2,mean),sigma=cov(x),qplot=FALSE){
  x   =scale(x,center=mu,scale=FALSE)
  z   =x%*%(sigma%^%(-0.5))
  n   =nrow(x)
  k   =ncol(x)
  dist=rowSums(z*z)
  if(qplot) qqplot(qchisq(ppoints(n), df = k),dist)
  return(dist)
}
