add.ci.est<-function(dat,level=0.95)
{
  if(!is.finite.sample.dbn(dat) & !is.sample(dat)) stop("Argument 'dat' must be of class 'sample' or 'finite.sample.dbn'")
  if(is.null(dat$point.est) | is.null(dat$var.est)) {
    cat("\nSamples.frame 'dat' must have columns '$point est'  and '$var.est'.")
    cat("\nThese can be added using functions 'add.point.est' and 'add.var.est'.")
    stop("")
  }
  sig<-(1-level)/2
  dat$ci.lower<-dat$point.est+qt(sig,df=dat$n)*sqrt(dat$var.est)
  dat$ci.upper<-dat$point.est+qt((1-sig),df=dat$n)*sqrt(dat$var.est)
  if(length(dat$ci.lower)>1) class(dat)<-c("samples.population","finite.sample.dbn", "sample")
  else class(dat)<-c("sample")
  dat
}
