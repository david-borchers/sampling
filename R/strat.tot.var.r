strat.tot.var<-function(dat,by.stratum=FALSE)
{
  # now some pretty cryptic code: need to add all combinations of stratum i
  # estimates and stratum (i-1) estimates to give full set of estimates. 
  # Code below does this, two strata at a time.
  est<-0
  if(by.stratum) stratum.est<-rep(0,dat$M)
  for(i in 1:dat$M) {
    if(by.stratum) stratum.est[i]<-ybar.tot.var(dat[[i]])
    est<-as.vector(outer(est,ybar.tot.var(dat[[i]]),"+"))
  }
  if(by.stratum) list(est=est,est.by.stratum=stratum.est,N=dat$N, n=dat$n)
  else est
}