#' @title MLA.pca3d
#' @description 3d Principal Components Analysis 
#'
#' @param data.obs A matrix of observed data set with least 3 columns.
#' @param ind.var Vector of integers (at least 3 elements): which variables must be used?
#' @param ind.comp Vector of integers ( 3 elements): which components must be displayed?
#' @param standardize If \code{TRUE} (strongly suggested) variables will be standardized before PCA
#'  
#' @param segments If \code{segments=TRUE} Segments starting from each point, 
#'  orthogonal to the first principal axis, will be drawn
#' 
#' @param animation If \code{animation=TRUE} the final visualization will be animated 
#' @param lim.max Maximum absolute standardized limit
#' @param princ.axes  If \code{princ.axes=TRUE} principal axes will be drawn
#' @param color.points Color for points
#' @param xy If \code{xy=TRUE} projection on a xy plan will be drawn
#' @param xz If \code{xz=TRUE} projection on a xz plan will be drawn
#' @param yz If \code{yz=TRUE} projection on a yz plan will be drawn
#' @param marg.col color for marginal projection
#' @param ellipse If \code{ellipse=TRUE} 3d contour ellipses will be drawn at probability given by \code{levels}
#' @param levels probability levels a which 3d contour ellipses will be drawn
#' @param lim.stand  If \code{lim.stand=TRUE} fixed standardized limits 
#' of  \code{lim.max=TRUE} will be used for x,y,z 
#' @param only3vars use only three variables
#'
#' @return  3d plots with many options
#' @export
#'
#' @examples \donttest{
#' data(antropometric)
#' MLA.pca3d(antropometric[,7:13])
#' }
MLA.pca3d=function(data.obs,ind.var=1:3,ind.comp=1:3,standardize=TRUE,lim.max=4,segments=FALSE,animation=FALSE,
                   princ.axes=TRUE,color.points=1, xy=TRUE,xz=TRUE,yz=TRUE,marg.col="lightcyan",
                   ellipse=TRUE,levels=c(0.25,0.50,0.75),
                   lim.stand=TRUE,only3vars=TRUE){
    p=ncol(data.obs)
    if(standardize) data.obs=scale(data.obs)
    rid=data.obs[,ind.var]
    n=nrow(rid)
    autov=eigen(cov(rid))
    acp1=princomp(rid)
    sigma=cov(rid)
    print(summary(acp1))
    q1=outer(acp1$scores[,1],acp1$loadings[,1])
    if(lim.stand){
      xlim=c(-lim.max,lim.max)  
      ylim=c(-lim.max,lim.max)  
      zlim=c(-lim.max,lim.max)  
    }
    else
    {
      xlim=range(rid[,1])
      ylim=range(rid[,2])
      zlim=range(rid[,3])
    }
      plot3d(rid,xlim=xlim,ylim=ylim,zlim=zlim,col=color.points)
    
    if (princ.axes){
        abclines3d(0,0,0,autov$vectors[,1],color="red")
        abclines3d(0,0,0,autov$vectors[,2],color="green")
        abclines3d(0,0,0,autov$vectors[,3],color="blue")
    }
    if (only3vars) totvar=sum(diag(sigma)) else  totvar=(n-1)*sum(apply(data.obs,2,var))/n
    perc=100*sum(autov$values)/totvar
        title3d(main= paste(c("eigenvalues= ",round(autov$values,2)," var.=",round(perc,2), "%"),collapse =" "))
    
    q1=outer(acp1$scores[,1],acp1$loadings[,1])
    points3d(q1,col="yellow")
    
    
    if(xy)points3d(cbind(rid[,1:2],array(min(zlim),n) )  ,col=marg.col)
#   if(xy)show2d(plot(rid[,1:2]) , ignoreExtent = FALSE,expand = 1)
    if(xz)points3d(cbind(rid[,1],array(min(ylim),n),rid[,3] )  ,col=marg.col)
    if(yz)points3d(cbind(array(min(xlim),n),rid[,2:3] )  ,col=marg.col)
    
    if(ellipse) for(i in 1:length(levels)) plot3d(ellipse3d(sigma,level=levels[i]),alpha=.3,col=i+1,type="wire",add=TRUE)
     
    
    if(segments) for(i in 1:n) lines3d(rbind(q1[i,],rid[i,]),color="red") #wrong!!!!!!!!!!!!
    
    if(animation)play3d(spin3d(axis=c(1,1,0), rpm=4), duration=20)
}
