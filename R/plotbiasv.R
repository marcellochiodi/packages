#' @title  Plot of comparison of bias vs variance (lesson exercise)
#'
#' @param x output of simul.normalmix2
#' @param ... other arguments to pass
#'
#' @return a plot of comparison of bias vs variance
#' @export
#'
#' @examples \donttest{
#' d1=simul.normalmix2(n=1000,ncamp=500,ngrid=200,tpause = 0)
#' plotbiasv(d1)}
plotbiasv= function(x,...){
f=x
par(mfrow=c(1,2))
bias=(f$m-f$true)
v=f$s^2
plot(diff(diff(f$true)),bias[-c(1,100)])
l=lm(bias[-c(1,100)]~diff(diff(f$true)))
abline(l)
plot(f$true,v)
l=lm(v ~  f$true)
abline(l)
}

