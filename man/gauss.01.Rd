\name{gauss.01}
\alias{gauss.01}
\title{Returns gaussian quadrature points and weights}
\description{Read gaussian coeff. in -1,1 and converts in 0-1 quadrature formulas; returns points and weights for quadrature with 2^igauss points}
\usage{
gauss.01(igauss)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{igauss}{the points of quadrature will be \code{2^igauss}  }
 }
\details{returns points and weights for quadrature with \code{2^igauss} points
}
\value{points and weights for quadrature with 2^igauss points}
\author{Marcello Chiodi}


\keyword{gaussian}
\keyword{quadrature}
\examples{
gauss.01(3)
}