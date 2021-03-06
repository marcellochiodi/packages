\name{draw1}
\alias{draw1}
\title{Interactive tool for basic kernel density estimation}
\description{Interactive tool for basic kernel density estimation. A teaching tool with the basic graphic concept of kernel estimation. Teaching material for the course of \emph{Non parametric statitical methods}}
\usage{
draw1(x=rnorm(n),n=25,hmin=hmax/1000,hmax=diff(range(x)),ind=1,
               draw.sing=FALSE,newwindow=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numeric vector (the default is normal random sample)}
  \item{n}{the length of the normal random sample)}
  \item{hmin}{the minimum possible bandwidth}
  \item{hmax}{the maximum possible bandwidth}
  \item{ind}{the default option button}
  \item{draw.sing}{if \code{TRUE} (the default) then the single components of the kernel are drawn}
  \item{newwindow}{logical. When \code{TRUE}  a new window is opened at the first call,to avoid problems with RStudio plot window}
 }
\details{Teaching tool with the basic graphic concept of kernel estimation of the input vector \code{x}. A panel appears for three the interactive options for the graphic routine \code{\link{likelygen}}; the first is a slider for the interactive choice of the bandwidth \code{h}; the second is the choice among a normal kernel, a uniform kernel and a comparison of 4 kernel; the third is a button to draw individual kernel components
}
\value{The vector \code{x} is returned; a panel is displayed, and a graphic plot is visible as soon as some action is made on the option panel}
\author{Marcello Chiodi}


\keyword{kernel}
\examples{\donttest{
draw1(runif(20))
}}
