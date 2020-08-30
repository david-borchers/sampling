strat.regr.ppn<-function(dat,cond,by.stratum=FALSE)
{
  for(i in 1:dat$M) dat[[i]]$y.value<-(eval(parse(text=paste("dat[[i]]$y.value",cond,sep="")))*1)
  est<-strat.regr.tot(dat,by.stratum=by.stratum)
  if(by.stratum) {
    est$est<-est$est/sum(dat$N)
    est$est.by.stratum<-est$est.by.stratum/dat$N
  } else est<-est/sum(dat$N)
  est
}
