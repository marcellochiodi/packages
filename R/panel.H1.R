#' @title Panel plot called by MLA.explor.pairs
#'
#' @param x input data
#' @param ... other parameters to pass
#'
#' @return a panel plot
#' @export
#'
#' @examples
panel.H1 <-
function(x, ...)
     {
         usr <- par("usr"); on.exit(par(usr))
         par(usr = c(usr[1:2], 0, 1.5) )
         h <- hist(x, plot = FALSE)
         breaks <- h$breaks; nB <- length(breaks)
         y <- h$density ; y<-y/max(y)
         rect(breaks[-nB], 0, breaks[-1], y, col="yellow", ...)
x<-sort(x)
f<-dnorm(x,mean(x),sqrt(var(x)))

lines(x,f/max(f))

     }
