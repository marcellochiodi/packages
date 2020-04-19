
#' @title Returns the list of objects according to 
#' increasing memory size
#' 
#' @description \code{allsizes()} returns the list of objects according to increasing memory size
#' @return No value is returned
#' @export
#'
#' @examples 
#' \dontrun{allsizes()}
allsizes=function(){
  ##  print the size of all objects in the global environment 
  ## sorted by size. compute also total memory
  ## print only, wwithout return values
  lista   =ls(globalenv())
  ris     =numeric(0)
  n       =length(lista)
  for(j in 1:length(lista)){
    obj=lista[[j]]
    ris =c(ris,eval(parse(text=paste("object.size(",obj,")",sep=""))))
    names(ris)[j]=obj
  }
  print(sort(ris))
  z       =sum(ris)
  class(z)="object_size"
  cat("total sizes: ")
  print(z,units="Mb")
}
