# added to package 22-04-2014
#' Title
#'
#' @param x 
#' @param y 
#' @param h 
#' @param r 
#' @param n 
#' @param lims 
#'
#' @return
#' @export
#'
#' @examples
kde2dr=function (x, y, h, r=cor(x,y),n = 41, lims = c(range(x), range(y))) 
{
    nx <- length(x)
    if (length(y) != nx) 
        stop("data vectors must be the same length")
    if (any(!is.finite(x)) || any(!is.finite(y))) 
        stop("missing or infinite values in the data are not allowed")
    if (any(!is.finite(lims))) 
        stop("only finite values are allowed in 'lims'")
    n <- rep(n, length.out = 2L)
    gx <- seq.int(lims[1L], lims[2L], length.out = n[1L])
    gy <- seq.int(lims[3L], lims[4L], length.out = n[2L])
    h <- if (missing(h)) 
        c(bandwidth.nrd(x), bandwidth.nrd(y))
    else rep(h, length.out = 2L)
    h <- h/4
    r1=sqrt(1-r*r)
    ax <- outer(gx, x, "-")/h[1L]
    ay <- outer(gy, y, "-")/h[2L]
    z <- tcrossprod(matrix(dnorm(ax), , nx), matrix(dnorm(r*ax+r1*ay), 
        , nx))/(nx * h[1L] * h[2L])
    list(x = gx, y = gy, z = z)
}

