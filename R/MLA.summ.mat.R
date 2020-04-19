#' @title A \emph{personal} summary of a numerical variable   
#' @aliases summ
#' @name summ
#' @description a \emph{personal} summary of a numerical variable. 
#' \emph{Not computationally optimized}
#' @param x a numerical array
#'
#' @return A list of summary statistics
#' \itemize{
#' \item n number of observations
#' \item minimum, maximum
#' \item mean, mean square deviation
#' \item median, mean absolute deviatons from median and from mean
#' \item gamma1,gamma2: skewness and kurtosis indeces (0 for the normal,gamma2=beta2-3)
#' } 

#' @export
#'
#' @examples \donttest{summ(rnorm(1000))}
summ=function(x){
  n=length(x)
  xmin=min(x)
  xmax=max(x)
  
  m    =mean(x)
  sm  =sqrt(mean((x-m)^2))
  med =median(x)
  s0  =mean(abs(x-med))
  s1  =mean(abs(x-m))
  b1  =(mean((x-m)^3))/sm^3
  b2  =(mean((x-m)^4))/sm^4
  return(list(n=n,min=xmin,max=xmax,mean=m,median=med,stdev=sm,s0=s0,s1=s1,gamma1=b1,gamma2=b2-3))
}
#' @title MLA.summ.mat
#' @description a \emph{personal} summary of the variables of a matrix or a data.frame
#' \emph{Not computationally optimized}

#' @param data a matrix or a data.frame
#'
#' @return a matrix with \code{nrow(data)}: 
#'         each row contains a summary output given by \code{summ}
#' @export
#'
#' @examples \donttest{
#' data(antropometric)
#' print(round(MLA.summ.mat(antropometric[,7:13]),2))}
MLA.summ.mat=function(data){
  k=ncol(data)
  ris=numeric(0)
  for (i in 1:k) ris=rbind(ris,unlist(summ(data[,i])))
  return(ris)
}
