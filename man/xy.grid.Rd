\name{xy.grid}
\alias{xy.grid}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{Compute a 2d grid of points}
 \description{Compute a 2d grid of points}
 \usage{
xy.grid(rangex, rangey, nx, ny = nx)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rangex}{
%%     ~~Describe \code{rangex} here~~
}
  \item{rangey}{
%%     ~~Describe \code{rangey} here~~
}
  \item{nx}{
%%     ~~Describe \code{nx} here~~
}
  \item{ny}{
%%     ~~Describe \code{ny} here~~
}
}
\value{
a grid matrix of \code{nx x ny} rows and 2 columns}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (rangex, rangey, nx, ny = nx) 
expand.grid(seq(rangex[1], rangex[2], length = nx), seq(rangey[1], 
    rangey[2], length = ny))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{2d grid}% use one of  RShowDoc("KEYWORDS")
