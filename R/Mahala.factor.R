#' @title Mahala.factor
#' 
#' @description Computes the Mahalanobis distances of dat points from their centroids.
#' Uses the spectral decomposition of Variance matrix for numerical stability
#' @param x a matrix with observed data
#' @param eps minimum scaled value of eigenvalues
#' @param plot logical. If \code{plot=TRUE} a qq plot of distances vs. 
#' theoretical chi-square distribution is plotted
#' @return The vector of Mahalanobis disctances from centroid
#' @export
#'
#' @examples
Mahala.factor=function(x,eps=0.0000001,plot=FALSE){
  #    x=matrix(rnorm(10000),1000,10)
  x=scale(as.data.frame(x))
  eig=eigen(var(x))
  check=sum(eig$values)/k
  m=apply(x,2,mean)
  n=nrow(x)
  k=ncol(x)
  ind=which((eig$values/check)>eps)
  dist=array(0,n)
  for (i in 1:n){
    dev=x[i,]-m
    dist[i]=sum((crossprod(dev,eig$vectors[,ind]))^2/eig$values[ind])
  }
  if(plot) qqplot(qchisq(ppoints(n), df = k),dist)
  return(dist)
  # qqplot(qchisq(ppoints(1000), df = 3)
}
