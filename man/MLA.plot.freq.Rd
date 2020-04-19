\name{MLA.plot.freq}
\alias{MLA.plot.freq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plots an histogram and a kernel density}

\description{Plots an histogram of the frequency distribution of 
a numerical variable together with a smooth kernel density estimates
}
\usage{MLA.plot.freq(x, nclass = 20, adj = 0.7,
colbar = "yellow", colborder = "blue", col = "red")}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the numerical variable, a vector
}
  \item{nclass}{number of classes of the histogram
}
  \item{adj}{factor for the bandwidth
%%     ~~Describe \code{adj} here~~
}
  \item{colbar}{color of the histograms bars
%%     ~~Describe \code{colbar} here~~
}
  \item{colborder}{color of the borders of the bars
%%     ~~Describe \code{colborder} here~~
}
  \item{col}{color of the kernel density
%%     ~~Describe \code{col} here~~
}
}
\details{the parametr adj adjust the bandwidth: the real optimal bandwidth computed by 
density will be multiplied by adj, so that a value of adj greater than one will give 
smoother density 
%%  ~~ If necessary, more details than the description above ~~
}
\value{a plot with the histogram of the frequency distribution of x together with a kernel
density estimation
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Marcello Chiodi
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\examples{
MLA.plot.freq(rnorm(1000))
MLA.plot.freq(rgamma(1000,shape=2))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{histogram}
\keyword{density}% __ONLY ONE__ keyword per line
