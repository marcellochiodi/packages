#' @title Histogram, non parametric density, normal density
#' @details The function plots the histogram of the distribution of the input variable
#' together with a non parametric estimate of the density and
#'   a normal density. Possibly \code{x} is a vector of 
#'   \code{lm$residuals}. 
#' @description  Join to MLA.plot.feq some day...
#' @param z input numerical variable 
#' @param nc number of classes for histogram
#' @param std if \code{std=TRUE} then variable 
#' is standardized before plotting
#'
#' @return plots the histogram of the distribution of the input variable
#' together with a non parametric estimate of the density and
#'   a normal density.
#' @export
#'
#' @examples MLA.plot.norm(rnorm(100))
#' MLA.plot.norm(rgamma(1000,shape=2))
MLA.plot.norm<-function(z,nc=50,std=FALSE){
  # plots histogram, non parametric density, normal density
    hist(z,nc=nc,probability  = TRUE)
    lines(density(z),col=4)
    if(std)z=scale(z)
    m   =mean(z)
    s   =sd(z)
    zx=seq(-4*s+m,4*s+m,by=0.05)
    lines(zx,dnorm(zx,mean=m,sd=s),col=2)
    ## future: chi square
}