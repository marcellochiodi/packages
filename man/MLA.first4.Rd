\name{MLA.first4}
\alias{MLA.first4}
\title{Computes the first four moments of the variables of a data frame}

\description{Computes the first four moments of the variables of a data frame
together with kurtosis indeces and standard deviations). 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
MLA.first4(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numeric matrix or a data frame
%%     ~~Describe \code{x} here~~
}
}
\details{For matrices or data.frames use apply
%%  ~~ If necessary, more details than the description above ~~
}
\value{a list with the following components:
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{\code{m1}}{The first moment (the arithmetic mean)}
 \item{\code{m2}}{The second central moment (the population variance)}
  \item{\code{m3}}{The third central moment }
  \item{\code{m4}}{The fourth central moment }
  \item{\code{s}}{The standard deviation}
  \item{\code{g1}}{gamma1 skewness index (0 for symmetrical distributions)}
  \item{\code{b2}}{gamma2 kurtosis index (3 for the normal distribution)}
%% ...
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
\donttest{
data(children.rid)
MLA.first4(children.rid[,1:3])
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{moments}
\keyword{descriptive}% __ONLY ONE__ keyword per line
