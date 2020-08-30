add.point.est<-function(dat,type="ybar",cond=NULL)
{
  dat$point.est<-point.est(dat,type=type,cond=cond)
  if(length(dat$point.est)>1) class(dat)<-c("samples.population","finite.sample.dbn", "sample")
  else class(dat)<-c("sample")
  dat
}
