#' @export
ratio.ppn.var<-function(sample,cond,N=NULL)
{
  pdat<-dat
  pdat$y.value<-(eval(parse(text=paste("dat$y.value",cond,sep="")))*1)
  ((1-dat$n/dat$N)/dat$n)*(est.syy(dat)-2*rat*est.sxy(dat)+rat^2*est.sxx(dat))
}
