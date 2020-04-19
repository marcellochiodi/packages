\name{xyz.grid}
\alias{xyz.grid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compute a 3d grid of points}
\description{
Compute a 3d grid of points}
\usage{
xyz.grid(rangex, rangey, rangez, nx, ny = nx, nz = nx)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rangex}{
%%     ~~Describe \code{rangex} here~~
}
  \item{rangey}{
%%     ~~Describe \code{rangey} here~~
}
  \item{rangez}{
%%     ~~Describe \code{rangez} here~~
}
  \item{nx}{
%%     ~~Describe \code{nx} here~~
}
  \item{ny}{
%%     ~~Describe \code{ny} here~~
}
  \item{nz}{
%%     ~~Describe \code{nz} here~~
}
}
\value{
a 3d  regular grid of points, \code{nx x ny x nz} rows and 3 columns}
\examples{
\dontrun{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (rangex, rangey, rangez, nx, ny = nx, nz = nx) 
expand.grid(seq(rangex[1], rangex[2], length = nx), seq(rangey[1], 
    rangey[2], length = ny), seq(rangez[1], rangez[2], length = nz))
}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{3d grid}% use one of  RShowDoc("KEYWORDS")
