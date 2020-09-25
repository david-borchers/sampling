#' @export
est.beta<-function(dat,mdf=1,n=dat$n)
{
  est.sxy(dat,mdf=mdf,n=n)/est.sxx(dat,mdf=mdf,n=n)
}
# Variance estimation 
# add estimate to 'samples.population' object for all samples
add.var.est<-function(dat,type="ybar",cond=NULL)
{
  dat$var.est<-var.est(dat,type=type,cond=cond)
  if(length(dat$var.est)>1) class(dat)<-c("samples.population","finite.sample.dbn", "sample")
  else class(dat)<-c("sample")
  dat
}