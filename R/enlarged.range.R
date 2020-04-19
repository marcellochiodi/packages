#' @title Enlarge the range of a variable
#'
#' @param x a numerical variable
#' @param eps increment of the range
#'
#' @return the range of x enlarged of a proportion of \code{eps/2} on each side
#' @export
#'
#' @examples
enlarged.range<-function(x,eps=0.05){
  r=range(x)
  r1=r[1]
  r2=r[2]
  r.eps=diff(r)*0.5*eps
  return(c(r1-r.eps,r2+r.eps))
  }