strat.ratio.mean<-function(dat,by.stratum=FALSE)
{
  est<-strat.ratio.tot(dat,by.stratum=by.stratum)
  if(by.stratum) {
    est$est<-est$est/sum(dat$N)
    est$est.by.stratum<-est$est.by.stratum/dat$N
  } else est<-est/sum(dat$N)
  est
}
