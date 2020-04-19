#' data.sample
#'
#' @param x a data frame or a matrix
#' @param k number of row to draw
#'
#' @return a random sample of \code{k} rows of the input data frame
#' @export
#'
#' @examples
data.sample <-
function(x,k=1){x=as.data.frame(x)
return(x[sample(nrow(x),k),])
}
