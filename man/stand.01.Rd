\name{stand.01}
\alias{stand.01}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Standardize a variable in 0-1
}
\description{
Standardize a variable in 0-1}
\usage{
stand.01(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a vector}
}
\value{
\code{(x-min(x))/diff(range(x))}
}
\examples{
\dontrun{
x=rnorm(20)
stand.01(x)
}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{scale}
