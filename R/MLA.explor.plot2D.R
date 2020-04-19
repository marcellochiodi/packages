MLA.explor.plot2D <-
function(x,y,lwd1=2,lwd2=3,
                         col1="blue", col2="red",
                            smooth=2/3,...){
    plot(x,y,...)
    abline(lm(y~x),lwd=lwd1,col=col1)
    lines(lowess(y~x,f=smooth), lwd=lwd2, col=col2)
}

