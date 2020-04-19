#' @title Density plot of a grouped variable
#'
#' @param x a numeric vector
#' @param ind a grouping variable (same length of \code{x})
#' @param ... plotting options
#'
#' @return A plot with the density of  \code{x} grouped by  \code{ind}
#' @export
#'
#' @examples \donttest{
#' data(children.rid)
#' MLA.explor.plot2D(children.rid$gestazione,children.rid$lunghezza)
#' MLA.group.density.plot(children.rid$lunghezza,children.rid$gestazione)
#' }
MLA.group.density.plot=function(x,ind,...){
  group   =sort(unique(ind))
  k       =length(group)
  ## check density limits for x and y
  ymax=0
  xmin=Inf
  xmax=-Inf
  for (i in 1:k) {
    z=density(x[ind==group[i]])
       ymax=max(ymax, max(z$y))
       xmax=max(xmax, max(z$x))
       xmin=min(xmin, min(z$x))
  } 
       
       
       plot(density(x[ind==group[1]]),type="l",ylim=c(0,ymax),xlim=c(xmin,xmax),...)
  if(k>1){
    for(i in 2:k)lines(density(x[ind==group[i]]),col=i)
  }
}
