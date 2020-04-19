#' @title dimensions
#' 
#' @description Some didactical presentation about the curse of dimensionality
#'
#' @param n integer: number of points
#' @param k integer: number of classes
#' @param distribution can be only "normal" or "uniform"
#'
#' @return plots 
#' @export
#'
#' @examples
dimensions=function(n=100,k=5,distribution="uniform"){

x=matrix(switch(distribution,uniform=runif(3*n),normal=rnorm(3*n)),n,3)


plot(x[,1],array(0,n),
	   main=paste("d=1;   n= ",n, "; k= ",k,"; Expected obs. for cell= ",n/k),
	   sub=paste("points from a ",distribution, "distribution"),
	   col="red")
grid(nx=k,ny=0)

dev.new()
plot(x[,1],x[,2],
main=paste("d=2;   n= ",n, "; k= ",k,"; Expected obs. for cell= ",n/k^2),
	   sub=paste("points from a ",distribution, "distribution"),
	   col="red")
grid(nx=k,ny=k)



plot3d(x[,1],x[,2],x[,3],col="red",size=1.5,type="s")
title3d(paste("d=3;   n= ",n, "; k= ",k,"; Expected obs. for cell= ",n/k^3))
title3d(sub=paste("points from a ",distribution, "distribution"))
grid3d(side=c("x","y","z"),n=k)
grid3d(side=c("x+","y+","z+"),n=k)
}



