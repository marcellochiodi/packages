\name{kde2dr}
\alias{kde2dr}
\title{A 2-d normal kernel estimator with correlated components}
\description{A simple and quick 2-d normal kernel estimator with correlated components, used for didactical purposes.}

\usage{
kde2dr(x, y, h, r=cor(x,y),n = 41, lims = c(range(x), range(y)))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{x-values of kernel points of length \code{n} (\code{n}=\code{length(xkern)}).}
  \item{y}{y-values of kernel points of length \code{n}.}
  \item{h}{bandwidths: a length 2 numerical vector.}
  \item{r}{correlation for bivariate kernel components. Default to the empirical correlation cor(x,y).}
  \item{n}{number of grid points for each direction where kernel density will be computed. Default =41}
  \item{lims}{limits of the grids.}

  }
\details{A standard bivariate normal kernel estimator with correlated components will be computed.}
\value{grid values and estimated densities.
}
\references{
Venables, W. N. and Ripley, B. D. (2002) \emph{Modern Applied Statistics with S}. 
Fourth edition. Springer.
Wand, M.P and Jones, M.C. (1995). \emph{Kernel Smoothing}. London: Chapman & Hall/CRC. 
}
\note{Personal modification from \code{MASS::kde2d}	}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\keyword{kernel}

\examples{\donttest{
data(antropometric)
attach(antropometric)
par(mfrow=c(2,2))
plot(ALTEZZA,TORACECM)
f1 <- kde2dr(ALTEZZA,TORACECM,  n = 50)
f2 <- kde2dr(ALTEZZA,TORACECM,r=0, n = 50 )
persp(f1$x,f1$y,f1$z)
image(f1)
image(f2)
mfrow3d(1,2,sharedMouse=TRUE)
persp3d(f1$x,f1$y,f1$z,alpha=.3,col="blue")
title3d(main= "normal kernel with independent components")
persp3d(f2$x,f2$y,f2$z,alpha=.3,col="red")
title3d(main= "normal kernel with correlated components")
open3d()
persp3d(f1$x,f1$y,f1$z,alpha=.3,col="blue")
persp3d(f2$x,f2$y,f2$z,alpha=.3,col="red",add=TRUE)
}}