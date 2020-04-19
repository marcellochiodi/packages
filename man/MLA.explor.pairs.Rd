\name{MLA.explor.pairs}
\alias{MLA.explor.pairs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Matrix plot with something more than usual pairs()}

\description{A matrix plot with custom functions panel.lmsm (off diagonal) and panel.H1 (diagonal)
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
MLA.explor.pairs(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a data matrix
%%     ~~Describe \code{x} here~~
}
  \item{\dots}{other arguments 
%%     ~~Describe \code{\dots} here~~
}
}
\value{
No return value, only plotting}

\author{Marcello Chiodi
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, ...) 
{
    pairs(x, ..., upper.panel = panel.lmsm, lower.panel = panel.lmsm, 
        diag.panel = panel.H1)
  }
\donttest{
data(children.rid)
MLA.explor.pairs(children.rid[1:2000,c(1,2,3,6)], pch=".")
}  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
