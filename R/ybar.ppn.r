#' @export
ybar.ppn<-function(dat,cond)
{
  dat$y.value<-eval(parse(text=paste("dat$y.value",cond,sep="")))*1
  ybar(dat)
}
