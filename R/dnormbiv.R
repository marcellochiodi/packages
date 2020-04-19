#' Title
#'
#' @param x 
#' @param y 
#' @param s1 
#' @param s2 
#' @param m1 
#' @param m2 
#' @param r 
#'
#' @return
#' @export
#'
#' @examples
dnormbiv= function(x,y,s1=1,s2=1,m1=0,m2=0,r=0) {
z1=(x-m1)/s1
z2=(y-m2)/s2
exp(-0.5*(z1*z1+z2*z2-2*r*z2*z1)/(1-r*r))/(2*pi*s1*s2*sqrt(1-r*r))
}
