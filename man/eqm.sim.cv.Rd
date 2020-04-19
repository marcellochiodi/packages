\name{eqm.sim.cv}
\alias{eqm.sim.cv}
\alias{eqm.sim}
\title{Simulated MISE for CV and fixed bandwidth kernel (an exercise)
}
\description{Teaching tool to compute by simulation mean squared error for cross validation kernel density estimator. Teaching material for the course of \emph{Non parametric statitical methods 
}}
\usage{
eqm.sim.cv(n=100,ncamp=100,ngrid=100,mean=c(5,8),sd=c(1,2),x0=c(0,0),p=0.5)
eqm.sim(hs,n=100,ncamp=100,ngrid=100,mean=c(5,8),sd=c(1,2),x0=c(0,0),p=0.5)
}
\arguments{
\item{n}{size of each sample from a normal mixture}
\item{ncamp}{number of samples}
\item{ngrid}{number of points on the \code{x-}axes where density mustt be computed (default=100)}

\item{mean}{2-elements vector with the means of the two component of the mixture}
\item{sd}{2-elements vector with the standard deviations of the two component of the mixture}
\item{x0}{2-elements vector with the minimum values of the two component of the mixture}
\item{p}{the mixing proportion of the two component of the mixture (weight of the first component)}
\item{hs}{bandwidth}

}
\value{
simulated MISE}
\author{Marcello Chiodi
%%  ~~who you are~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
\dontrun{
e200=eqm.sim.cv(n=200)
e500=eqm.sim.cv(n=500)
str(e200)
str(e500)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{missing}
