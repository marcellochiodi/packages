#' Title
#'
#' @param data a data.frame
#' @param var the name of a variable
#'
#' @return a data.frame ordered acccording to var
#' @export
#'
#' @examples
order.var <-
function(data,var="V1"){data[order(as.vector(data[[var]])),]}
