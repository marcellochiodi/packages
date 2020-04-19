\name{key.press}
\alias{key.press}
\title{Pause an execution until a key is pressed
}
\description{key.press to pause an execution
}
\usage{
key.press(ikey=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{ikey}{a logical value}
}
\author{Marcello Chiodi
%%  ~~who you are~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
\donttest{
x=runif(5)
plot(x)
key.press()
lines(x)
}
}
% 