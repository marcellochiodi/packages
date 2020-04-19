
#' Title
#'
#' @param ikey 
#'
#' @return
#' @export
#'
#' @examples
key.press   <- function(ikey=TRUE){
                    msg="press enter --->"
                    if(ikey){
                    cat(msg)
                    scan()
                    }
                    }
########################################################################

