
#' @title Plot the means of a zero-one response variable as a function
#'  of a numerical grouping variable
#' @name myplot.grouped.bin
#' @param y a zero-one response variable (or however a 
#' variable with only two distinct values)
#' @param group a numerical grpouping variable
#' @param rid a factor to adjust the size of the circles
#' @param ksd multiple of the standard deviation to plot vertical segments
#' @param lines0  if \code{lines0=TRUE} a line connecting means is drawn
#' @param interval if \code{interval=TRUE} plot vertical segments are drawn
#' @param ylim0 y range of the plot
#' @param fac.range factor of enlargement for y-axis range
#' @param line.color color of the lines
#' @param back.color background color
#' @param ... other graphical parameters 
#'
#' @return a list with \code{y} means, grouping values, sizes 
#' @export
#'
#' @examples \donttest{
#' data(students.test)
#' myplot.grouped.bin(students.test$imm_test,students.test$VOTO_B_100)}
myplot.grouped.bin=function(y,group,rid=5,ksd=2,lines0=TRUE,interval=TRUE,ylim0=range(y),fac.range=0.5,
                          line.color=1,back.color=8,...){
  ## first version: y is a 0/1 variable
  ##
  uy=sort(unique(y))
  if(length(uy)!=2)return("ERROR: y is not a dichotomous variable")
  y=as.numeric(y==uy[2])
  mgen    =mean(y)
  my      =tapply(y,group,mean)
  mx      =as.numeric(names(my))
  mn      =tapply(y,group,length)
  if(missing(ylim0)) {
    r   =range(my)
    d   =diff(r)*fac.range
    m0  =r[1]-d
    m1  =r[2]+d
    ry  =c(max(0,m0),min(1,m1))
  }
  else 
  {
    ry =ylim0
  }
  fac     =sqrt(max(mn))/rid
  plot(mx,my,col=line.color,pch=21,bg=back.color,cex=sqrt(mn)/fac,ylim=ry,...)
  if (lines0) lines(mx,my,col=line.color,...)
  abline(h=mgen)
  sd      =sqrt(my*(1-my)/mn)
  k       =ksd
  if(interval)segments(x0=mx,y0=my-k*sd,y1=my+k*sd,col=line.color,...)
  return(list(m=my,x=mx,n=mn ))
}
