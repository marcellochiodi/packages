#' @title Gives the three directions of the intersection line of two planes
#'
#' @param plane1 a vector of three parameters \code{a,b,c} of a plane of equation
#' \code{a x + b y + c z = k} 
#' @param plane2  plane1 a vector of three parameters \code{a,b,c} of a plane of equation
#' \code{a x + b y + c z = k} 
#'
#' @return A vector of the three direction of the intersection line
#' @export
#'
#' @examples
  parametric.line3d=function(plane1,plane2){
  if((length(plane1)==3)&(length(plane2)==3)){
    a=plane1[2]*plane2[3]-plane1[3]*plane2[2]
    b=plane1[3]*plane2[1]-plane1[1]*plane2[3]
    c=plane1[1]*plane2[2]-plane1[2]*plane2[1]
    return(c(a,b,c))
  }
  else
  {
    print("error: parametric.line3d needs two 3-elements vectors")  
    return(c(0,0,0))
  }
}
