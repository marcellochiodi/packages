#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
MLA.first4 <-
function(x){
      k     =ncol(x)
      n     =nrow(x)
      m1    =numeric(0)
      m2    =numeric(0)
      m3    =numeric(0)
      m4    =numeric(0)
      s    =numeric(0)
      gamma1    =numeric(0)
      gamma2    =numeric(0)
      se.gamma1    =numeric(0)
      se.gamma2    =numeric(0)
## add.asymptotic standard errors sqrt(6/n) for b1 and  sqrt(6/n) for b1 
## and sqrt(24/n)    
      
      for(j in 1:k){
      y     =x[,j]
      
      m1    =c(m1,mean(y))
      m2    =c(m2,mean((y-m1[j])^2))
      m3    =c(m3,mean((y-m1[j])^3))
      m4    =c(m4,mean((y-m1[j])^4))
      s     =c(s,sqrt(m2[j]))
      gamma1    =c(gamma1,m3[j]/(m2[j]^1.5))
      gamma2    =c(gamma2,m4[j]/(m2[j]^2))
      se.gamma1 =c(se.gamma1,sqrt(6/n) )
      se.gamma2 =c(se.gamma1,sqrt(24/n))
      }
      return(list(moments=cbind(n,m1,m2,m3,m4),s=s, b=cbind(gamma1,gamma2),
                  se=cbind(se.gamma1,se.gamma2)))
}
