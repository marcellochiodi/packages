\name{MLANP}
\alias{MLANP}
\docType{package}
\title{Tools for teaching Non Parametric Statistics and Linear Models and other related topics. Lectures of Marcello Chiodi, University of Palermo. 
}
\description{Mainly for my lessons. Tools for teaching some topic on Linear Models and other  topics related to Non Parametric Statistics. Lectures of Marcello Chiodi, University of Palermo. Two different courses: Non parametric statistcs; Linear models and related Topics.
The strange name has been obtained joining \code{MLA} the italian acronym of \code{Modelli Lineari ed Altro (Linear Models and Other things)} with \code{NP=Non Parametric statistical methods}... I am afraid that it will remain unchanged at least for this year 2020 (the COVID year...)}
\details{
\tabular{ll}{
Package: \tab MLANP\cr
Type: \tab Package\cr
Version: \tab 1.4.1\cr
Date: \tab 2020-04-19\cr
License: \tab GPL (>=2) \cr
Depends: \tab R (>= 2.14.0), rgl,ks,mvtnorm,rpanel,ellipse,MASS \cr
}
}
\author{
Marcello Chiodi

Maintainer: Marcello Chiodi<marcello.chiodi@unipa.it>
}

\note{The package is intended for the use in my lessons, but can be used for any related topic. No care is put on computational issues. Code often is written to explain some topics (as cross validations arguments, where condensed formulas are not used)
}
\examples{
\donttest{

example(rnormal.mix2biv)
example(MLA.explor.pairs)
example(MLA.explor.plot2D)
example(MLA.summ.mat)

example(MLA.pca3d)
example(MLA.trivariate.normal)
example(MLA.contour3d)

example(simul.normalmix2)


}
}