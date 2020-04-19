\name{cvlikely}
\alias{cvlikely}
\title{Computation of a cross validation likelihood for density estimation
}
\description{computation of a cross validation likelihood and MSE. Teaching material for the course of \emph{Non parametric statitical methods}
}
\usage{
cvlikely(x,hmin=min(abs(diff(sort(x))))/2,hmax=sd(x)*2,nh=20,plot.cv=TRUE,plot.dens=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numericvector;}
  \item{hmin}{the minimum possible bandwidth}
  \item{hmax}{the maximum possible bandwidth}
  \item{nh}{the number of values of bandwith to use}
  \item{plot.cv}{If \code{TRUE} a plot of the cv MSE and of minus cv likelihood against the values of the bandwidth will be plotted}
  \item{plot.dens}{If \code{TRUE} a plot of the kernel density estimates of \code{x} will be drawn, using the best bandwidths}
%%     ~~Describe \code{x} here~~

}
\details{
nh, the number of values of the bandwith, is quite high (50,100) then the plot of the cross validation MSE and of minus likelihood against \code{h} will be quite good
}
\value{
Plots, according  to the values of \code{plot.cv} and \code{plot.dens}.
A list, with elements:
\item{hvec}{the vectors of bandwiths used}
\item{lvec}{the vectors of cross validation minus  likelihood}
\item{mvec}{the vectors of cross validation estimated MSE}
}
\author{Marcello Chiodi
%%  ~~who you are~~
}
\examples{
\dontrun{x=rnorm(n)
d=cvlikely(x,nh=40,hmin=0.25*1.06*n^(-.2),plot.dens=FALSE)
}}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\keyword{cross validation}
