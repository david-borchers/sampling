clust.var<-function(dat)
{
  ((1-dat$m/dat$M)*est.syy(dat,n=dat$m)/dat$m)/mean(dat$N)
}