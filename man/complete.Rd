\name{complete}
\alias{complete}
\title{Select complete records from a data frame
}
\description{Select complete records from a data frame, that is, drops the records that contains
at least one missing observation
}
\usage{
complete(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a data frame
%%     ~~Describe \code{x} here~~
}
}
\value{Returns a data frame with the same number of columns of x,
 without missing observations 
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Marcello Chiodi
%%  ~~who you are~~
}
\examples{
 \dontrun{complete(x)=function(x){subset(x,complete.cases(x))}}
}
\keyword{missing}
