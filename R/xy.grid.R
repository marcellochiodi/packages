
#' Title
#'
#' @param rangex 
#' @param rangey 
#' @param nx 
#' @param ny 
#'
#' @return
#' @export
#'
#' @examples
xy.grid=function(rangex,rangey,nx,ny=nx){	
  return(expand.grid(seq(rangex[1],rangex[2],length=nx ),seq(rangey[1],rangey[2],length=ny )))
}
