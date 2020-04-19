#' @title complete
#'
#' @param x a dataframe 
#'
#' @return a dataframe without the rows containing missing values
#' 
#' @export
#'
#' @examples \dontrun{
#' complete("a data frame")}
complete <-
function(x){subset(x,complete.cases(x))}
