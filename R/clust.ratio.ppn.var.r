#' @export
clust.ratio.ppn.var<-function(sample,cond,N=NULL)
{
  dat$y.value<-(eval(parse(text=paste("dat$y.value",cond,sep="")))*1)
  clust.ratio.var(dat)
}
