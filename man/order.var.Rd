\name{order.var}
\alias{order.var}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Order a data frame according to the value of a variable
}
\description{
Order a data frame according to the value of a variable}
\usage{
order.var(data, var = "V1")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
a data  frame}
  \item{var}{
the name of a variable}
}
\details{
no check is made on input}
\value{
a data matrix reordered according \code{V1}}
\author{
marcello chiodi}
\note{
no check is made on input}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\examples{
\dontrun{
data=as.data.frame(matrix(runif(100),20,5))
order.var(data)
# gives data ordered according to the first variable
}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{order}
