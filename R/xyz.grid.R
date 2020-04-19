
#' Title
#'
#' @param rangex 
#' @param rangey 
#' @param rangez 
#' @param nx 
#' @param ny 
#' @param nz 
#'
#' @return
#' @export
#'
#' @examples
xyz.grid=function(rangex,rangey,rangez,nx,ny=nx,nz=nx){
  expand.grid(seq(rangex[1],rangex[2],length=nx ),seq(rangey[1],rangey[2],length=ny ),seq(rangez[1],rangez[2],length=nz ))
}
