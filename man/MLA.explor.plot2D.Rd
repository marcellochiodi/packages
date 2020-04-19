\name{MLA.explor.plot2D}
\alias{MLA.explor.plot2D}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Explorative plot of two variables
}
\description{Plots two numeric variables, together with a linear regression and 
a non parametric regression, with a standard default choice of colors and symbols
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
MLA.explor.plot2D(x, y, lwd1 = 2, lwd2 = 3, col1 = "blue", col2 = "red", smooth = 2/3,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{x variable: assumed to be an esplicative variable (horizontal axes)
}
  \item{y}{y variable: assumed to be a dependent variable (vertical axes)}
  \item{lwd1}{The regression line width 
}
  \item{lwd2}{The non parametric regression line width 
}
  \item{col1}{The regression line color
}
  \item{col2}{The non parametric regression line color

}
  \item{smooth}{degree smothness of the non parametric regression (performed by lowess)
}
  \item{\dots}{other arguments }
  
}
\details{
The default values should give a simple colored graph, with fundamental different colors 
used for different elements}
\value{A plot, with the linear regression, computed with \code{lm},  together with the 
non parametric regression, computed by \code{lowess}
}
\author{
Marcello Chiodi}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\examples{
\donttest{
        x=seq(0,1,len=100)
        y=rnorm(100,m=x)
        MLA.explor.plot2D(x,y)
        data(antropometric)
        MLA.explor.plot2D(antropometric$PESOKG,antropometric$ALTEZZA)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{plot}
\keyword{smoothing}% __ONLY ONE__ keyword per line
