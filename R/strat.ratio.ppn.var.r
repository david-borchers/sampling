strat.ratio.ppn.var<-function(dat,cond,by.stratum=FALSE)
{
  for(i in 1:dat$M) dat[[i]]$y.value<-(eval(parse(text=paste("dat[[i]]$y.value",cond,sep="")))*1)
  est<-strat.ratio.tot.var(dat,by.stratum=by.stratum)
  if(by.stratum) {
    est$est<-est$est/sum(dat$N)^2
    est$est.by.stratum<-est$est.by.stratum/dat$N^2
  } else est<-est/sum(dat$N)^2
  est
}