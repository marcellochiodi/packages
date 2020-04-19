  MLA.plot.freq <-
function(x,nclass=20,adj=.7,colbar="yellow",colborder="blue",col="red"){
        hist(x,nclass=nclass,probability=TRUE,col=colbar,border=colborder)
        lines(density(x,adj=adj),col=col)}
