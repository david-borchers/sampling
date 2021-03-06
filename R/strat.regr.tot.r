#' @export
strat.regr.tot<-function(dat,by.stratum=FALSE)
{
  est<-0
  if(by.stratum) stratum.est<-rep(0,dat$M)
  for(i in 1:dat$M) {
    if(by.stratum) stratum.est[i]<-ybar.tot(dat[[i]])
    est<-as.vector(outer(est,regr.tot(dat[[i]]),"+"))
  }
  if(by.stratum) list(est=est,est.by.stratum=stratum.est,N=dat$N, n=dat$n)
  else est
}