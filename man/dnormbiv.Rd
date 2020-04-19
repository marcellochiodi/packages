\name{dnormbiv}
\alias{dnormbiv}
\title{Density of a bivariate normal distribution
}
\description{Computes the density of a bivariate nromal distribution (very basic, only for few exercises)}
\usage{
dnormbiv(x,y,s1=1,s2=1,m1=0,m2=0,r=0) 
}
\arguments{
\item{x}{x-coordinates of the points where the density must be computed}
\item{y}{y-coordinates of the points where the density must be computed}
\item{s1}{Standard deviation of x}
\item{s2}{Standard deviation of y}

\item{m1}{Mean of x}
\item{m2}{mean of y}
\item{r}{Correlation between x and y}
}

\details{
the density is computed as usual}
\value{
The density in \code{x,y}
}
\author{Marcello Chiodi
%%  ~~who you are~~
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{normal}
\examples{\dontrun{
dnormbiv(0:2,0:2,s1=1,s2=1,m1=0,m2=0,r=0) 
}}