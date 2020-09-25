#' @export
diffr.ppn.var<-function(sample,cond,N=NULL)
{
  dat$y.value<-(eval(parse(text=paste("dat$y.value",cond,sep="")))*1)
  diffr.var(dat)
}
