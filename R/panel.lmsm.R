#' @title Panel plot called by MLA.explor.pairs
#'
#' @param x input data
#' @param y input data
#' @param col color of points
#' @param bg background color of points
#' @param pch graphical par
#' @param cex graphical par
#' @param col.smooth color of smoothing line
#' @param span smoothing
#' @param iter number of iterations in smoothing
#' @param ... other parameters to pass
#'
#' @return a panel plot
#' @export
#'
#' @examples
panel.lmsm <-
function (x, y, col = par("col"), bg = NA, pch = par("pch"),
    cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...)
{
    points(x, y, pch = pch, col = col, bg = bg, cex = cex)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok))
        lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
            col = col.smooth, ...)
lines(x, predict(lm(y~x)),col="blue")
}
