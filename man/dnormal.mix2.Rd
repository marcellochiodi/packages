\name{dnormal.mix2}
\alias{dnormal.mix2}
\alias{rnormal.mix2}
\title{Mixture of two univariate normal distributions. Density and random numbers generation
}
\description{generate from a mixture of two normal distribution; compute the density.
Teaching material for the course of \emph{Non parametric statistical methods}}
\usage{
dnormal.mix2(x,mean=c(2,8),sd=c(1,2),x0=c(0,0),p=0.5)
rnormal.mix2(n=100,mean=c(2,8),sd=c(1,2),x0=c(0,0),p=0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{x}{points where the density must be computed}
\item{n}{number of points to be generated from the mixture}

\item{mean}{2-elements vector with the means of the two component of the mixture}
\item{sd}{2-elements vector with the standard deviations of the two component of the mixture}
\item{x0}{2-elements vector with the minimum values of the two component of the mixture}
\item{p}{the mixing proportion of the two component of the mixture (weight of the first component)}
}

\details{
the density is computed as usual: f(x)=p*f1(x-x0[2]+(1-p)*f2(x-x0[2])).
the random number generation is straighforward}
\value{
The density in \code{x} or a random vector of length \code{n}
}
\author{Marcello Chiodi
%%  ~~who you are~~
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{mixture}
\examples{\dontrun{
n=1000
plot(density(rnormal.mix2(n)))
}}