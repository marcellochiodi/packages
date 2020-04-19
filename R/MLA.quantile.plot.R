#' @title Plot quantile lines 
#'
#' @param y a numerical variable
#' @param x a grouping variable 
#' @param seg logical. If \code{seg=TRUE }vertical segments are drawn
#' @param probs vector of probabilities for which quantiles are computed.
#' Default values \code{seq(0, 1, 0.25) }
#'
#' @return A plot with quantiles plot
#' @export
#'
#' @examples \donttest{
#' data(children.rid)
#' MLA.explor.plot2D(children.rid$gestazione,children.rid$lunghezza)
#' MLA.quantile.plot(children.rid$lunghezza,children.rid$gestazione)
#' }

MLA.quantile.plot=function(y,x,seg=TRUE, probs = seq(0, 1, 0.25)){
  quantile.day    =tapply(y,x,quantile,probs=probs)
  n               =length(quantile.day)
  k               =length(probs)
  graf            = matrix(0,n,5)
  for(i in 1:n){
    graf[i,]       =quantile.day[[i]]
  }
  veccol          =c(3,2,1,2,3)
  x   =as.numeric(names(quantile.day))
  i=3
  plot(x,graf[,i],col=veccol[i],type="l",ylim=c(min(graf[,1]),max(graf[,5])))
  for (i in c(1,2,4,5)){
    lines(x,graf[,i],col=veccol[i])
  }
  if (seg)segments(x,graf[,1],x,graf[,5],col=8)
}
