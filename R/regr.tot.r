#' @export
regr.tot<-function(dat)
{
  regr(dat)*dat$N
}

regr.ppn<-function(dat,cond)
{
  dat$y.value<-eval(parse(text=paste("dat$y.value",cond,sep="")))*1
  regr(dat)
}