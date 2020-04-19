\name{data.sample}
\alias{data.sample}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Draw a sample of records from a data frame
%%  ~~function to do ... ~~
}
\description{Draw a random sample of distinct records from a data frame of any length
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
data.sample(x, k = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the data frame (the population of records)
%%     ~~Describe \code{x} here~~
}
  \item{k}{number of distinct records to sample from x
%%     ~~Describe \code{k} here~~
}
}
\value{a data frame with the same structure of \code{x} but with \code{k} rows
}
\author{Marcello Chiodi
%%  ~~who you are~~
}

\examples{
\dontrun{## The function is currently defined as
function (x, k = 1) 
{
    x = as.data.frame(x)
    return(x[sample(nrow(x), k), ])
  }
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{sampling}
