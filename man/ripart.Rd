\name{ripart}
\alias{ripart}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Computes and plots empirical distribution function 
%%  ~~function to do ... ~~
}
\description{Computes and plots empirical distribution function 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ripart(x,...)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a numeric vector}
  \item{...}{
other arguments}
}
\details{
Plots the Distribution function and the frequency function for a numerical vector}
\value{
The plots of the distribution function and the frequency function of \code{x}
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{x }{Sorted x}
  \item{F.emp}{Empirical distribution function}
%% ...
}
\author{
marcello chiodi}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\keyword{plot}
\keyword{distribution}% __ONLY ONE__ keyword per line
\examples{\dontrun{
ripart(trunc(10*runif(100))) 
}}