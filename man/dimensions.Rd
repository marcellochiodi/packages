\name{dimensions}
\alias{dimensions}
\title{Points in 1,2,3 dimensions}
\description{Introductory graphics for the problem of  "curse of dimensionality". Teaching material for the course of \emph{Non parametric statistical methods}}
\usage{
dimensions(n=100,k=5,distribution="uniform")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{number of random points}
  \item{k}{number of classes for each dimension}
  \item{distribution}{distribution from which \code{n} points are drawn: can be \code{uniform} or \code{normal}
 }}
\details{Teaching tool with 3 plots, one dimension, 2d and 3d. Introductory for the lesson on the curse of dimensionality
}
\value{Three plots are drawn}
\author{Marcello Chiodi}

\examples{\dontrun{
dimensions(1000)
dimensions(1000,distribution="normal")
}}
\keyword{dimensionality}
