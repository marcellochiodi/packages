\name{simul.normalmix2}
\alias{simul.normalmix2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Simulation and kernel density estimation from a normal mixture
%%  ~~function to do ... ~~
}
\description{
Simulation and kernel density estimation from a normal mixture. 
Simulate sampling distribution of different kernel estimators and computes simulated bias and variance. Useful when teaching non parametric statistical methods
}
\usage{
simul.normalmix2(n=400,
			  ncamp=100,
			  ngrid=100,
			  mean=c(3,8),sd=c(1,2),p=0.5,
			  x0=c(0,0),
			  hs=.5, hmet="fixed",
			  kernel="gaussian",
			  plot=TRUE,plot.band=TRUE,tpause=0.5,ikey=FALSE,
			  newwindow = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{sample size}
  \item{ncamp}{number of generated samples}
  \item{ngrid}{number of x points in which the kernel density will be evaluated}
  \item{mean}{2-elements vector: population means of  mixture components}
  \item{sd}{2-elements vector: population standard deviation of  mixture components}
  \item{p}{a numerical scalar: mixing proportion of mixture components}
  \item{x0}{minimum value for each mixture components (can be left to default valuec(0,0))}
  \item{hs}{bandwidth}
  \item{hmet}{bandwidth selection method}
  \item{kernel}{kernel type}
  \item{plot}{logical: TRUE if plot are desired}
  \item{plot.band}{logical: TRUE if empirical simulated summaries must plotted}
  \item{tpause}{time pause between subsequent samples}
  \item{ikey}{logical: if TRUE each simulation will be launched after a key pressure}
  \item{newwindow}{logical: if TRUE a new window is open before drawing}
%%     ~~Describe \code{x} here~~
}

\value{
a list with summaries of the simulation

In the plot 

black lines are simulated densities

red lines are mean-sd,mean,mean+sd

blu lines are 5th,50th and 95th percentiles

the green line is the true density}
\author{
Marcello Chiodi}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\examples{\donttest{
simul.normalmix2(n=100,ncamp=250,ngrid=100,tpause = 0)   }}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{simulation}
\keyword{kernel density}% __ONLY ONE__ keyword per line
