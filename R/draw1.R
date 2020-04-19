#' @title draw1
#'
#' @description Interactive didactic example of kernel univariate density estimation
#' @details Interactive didactic example of kernel univariate density estimation,
#' trough an interactive panel which controls a plot. It is only for didactical purposes so 
#' x should not be large. The option \code{draw.sing} is only for didactic purposes.
#' Useful only with moderate values of \code{n}. If \code{newwindow=TRUE} a new window is opened at the first call, 
#' to avoid problems with RStudio plot window
#' 
#' \code{x} x can be any numerical vector, observed or drawn from some univariate distribution
#' @param x a numerical vector. The default is a size \code{n} 
#' sample from a normal standard distribution
#' @param n default size of \code{x}
#' @param hmin minimum size of bandwidth 
#' @param hmax maximum size of bandwidth 
#' @param ind integer (1,2 or 3) used for the default choice in the first panel.
#' @param draw.sing logical. When \code{TRUE} single kernel components are drawn in green
#' @param newwindow logical. When \code{TRUE}  a new window is opened at the first call, 
#' to avoid problems with RStudio plot window
#' @return An interactive panel is  drawn
#' @export
#'
#' @examples
draw1=function(x=rnorm(n),n=25,hmin=hmax/1000,hmax=diff(range(x)),ind=1,
               draw.sing=FALSE, newwindow=TRUE){
if (newwindow) dev.new()
      h 	=((hmin)+(hmax))/2
			n   =length(x)
pannello <- rp.control(title="Kernel options",
#			size=c(400,300),
			y = x, 
			h 	=((hmin)+(hmax))/2,
			ind	=ind,
			draw.sing=draw.sing
			)
#Sys.sleep(2.)
rp.slider(pannello, h, hmin, hmax, likelygen,log=TRUE, title="h=bandwidth")
rp.radiogroup(pannello, ind,
       c(1,2,3),   labels=c("Normal kernel","Uniform kernel","Comparison of 4 kernels"),initval=1 ,
       action = likelygen, title = "Plot type")
rp.checkbox(pannello,draw.sing , action = likelygen, title ="Draw individual components")
#rp.do(pannello, likelygen)
return(x)
}
