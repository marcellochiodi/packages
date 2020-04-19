#' @title 3d double contour plot of a  kernel density estimator compared 
#' with normal contour ellipses
#' @description 3d contour plot of a  kernel density estimator of a 3d data set, 
#' compared with normal contour ellipses
#'
#' @param data.obs A matrix of observed data set with least 3 columns.
#' @param ind.var Vector of integers (3 elements): which variables must be used?
#' @param standardize If \code{TRUE}  variables will be standardized before PCA
#' @param animation If \code{animation=TRUE} the final visualization will be animated 
#' @param n.rounds number of animated rotations (10 seconds each)
#' @param princ.axes  If \code{princ.axes=TRUE} principal axes will be drawn
#' @param color.points color for observed points
#' @param xy If \code{xy=TRUE} projection on a xy plan will be drawn
#' @param xz If \code{xz=TRUE} projection on a xz plan will be drawn
#' @param yz If \code{yz=TRUE} projection on a yz plan will be drawn
#' @param marg.col color for marginal projection
#' @param ellipse If \code{ellipse=TRUE} 3d contour ellipses will be drawn at probability given by \code{levels}
#' @param levels probability levels a which 3d contour ellipses will be drawn
#'
#' @return A 3d interactive double plot  
#' @export
#'
#' @examples \donttest{
#' data(antropometric)
#' MLA.contour3d(antropometric[,7:9],
#' standardize=FALSE,levels=c(.2,.4,.6,.8),
#' princ.axes=TRUE,ellipse=TRUE,animation=FALSE)
#' }
#' 

MLA.contour3d=function(data.obs,ind.var=1:3,standardize=TRUE,animation=FALSE, n.rounds=1,
                   princ.axes=TRUE,color.points=1, xy=TRUE,xz=TRUE,yz=TRUE,marg.col="lightcyan",
                   ellipse=FALSE,levels=c(0.25,0.50,0.75)){
    only3vars=TRUE
    lim.stand=FALSE
    lim.max=4
    p=ncol(data.obs)
    if(standardize) data.obs=scale(data.obs)
    rid=data.obs[,ind.var]
    m1=mean(rid[,1])
    m2=mean(rid[,2])
    m3=mean(rid[,3])
    n=nrow(rid)
    autov=eigen(cov(rid))
    acp1=princomp(rid)
    sigma=cov(rid)
    print(summary(acp1))
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
        abclines3d(m1,m2,m3,autov$vectors[,1],color="red")
        abclines3d(m1,m2,m3,autov$vectors[,2],color="green")
        abclines3d(m1,m2,m3,autov$vectors[,3],color="blue")
    }
    if (only3vars) totvar=sum(diag(sigma)) else  totvar=(n-1)*sum(apply(data.obs,2,var))/n
    perc=100*sum(autov$values)/totvar
      title3d(main= paste(c("eigenvalues= ",round(autov$values,2)," var.=",round(perc,2), "%"),collapse =" "))
    
    
    if(xy)points3d(cbind(rid[,1:2],array(min(zlim),n) )  ,col=marg.col)
#   if(xy)show2d(plot(rid[,1:2]) , ignoreExtent = FALSE,expand = 1)
    if(xz)points3d(cbind(rid[,1],array(min(ylim),n),rid[,3] )  ,col=marg.col)
    if(yz)points3d(cbind(array(min(xlim),n),rid[,2:3] )  ,col=marg.col)
    
    if(ellipse) for(i in 1:length(levels)) plot3d(ellipse3d(sigma,centre=c(m1,m2,m3),level=levels[i]),alpha=.3,col=i+1,type="wire",add=TRUE)
      open3d()
      
      fhat <- kde(rid)
      plot(fhat, drawpoints=TRUE,cont=100*levels,
           alpha=0.3,colors=6-(1:length(levels)),display="persp")
      
      
      if (princ.axes){
        abclines3d(m1,m2,m3,autov$vectors[,1],color="red")
        abclines3d(m1,m2,m3,autov$vectors[,2],color="green")
        abclines3d(m1,m2,m3,autov$vectors[,3],color="blue")
      }
      
      open3d()
    mfrow3d(1,2,sharedMouse = TRUE)
    r3dDefaults$windowRect <- c(50,50, 1000, 700) 
    
    plot(fhat, drawpoints=TRUE,cont=100*levels,
         alpha=0.3,colors=6-(1:length(levels)),display="persp")
    
    
    if (princ.axes){
      abclines3d(m1,m2,m3,autov$vectors[,1],color="red")
      abclines3d(m1,m2,m3,autov$vectors[,2],color="green")
      abclines3d(m1,m2,m3,autov$vectors[,3],color="blue")
    }
    plot3d(rid,xlim=xlim,ylim=ylim,zlim=zlim,col=color.points)
    if(ellipse) for(i in 1:length(levels)) plot3d(ellipse3d(sigma,centre=c(m1,m2,m3),level=levels[i]),alpha=.3,col=i+1,type="wire",add=TRUE)
    
    if (princ.axes){
      abclines3d(m1,m2,m3,autov$vectors[,1],color="red")
      abclines3d(m1,m2,m3,autov$vectors[,2],color="green")
      abclines3d(m1,m2,m3,autov$vectors[,3],color="blue")
    }
    
    
    sec.round=10
    if(animation) play3d(spin3d(axis=c(1,1,0), rpm=60/sec.round), duration=n.rounds*sec.round)
}
