diffr.tot<-function(dat)
{
  diffr(dat)*dat$N
}

diffr.ppn<-function(dat,cond)
{
  dat$y.value<-eval(parse(text=paste("dat$y.value",cond,sep="")))*1
  diffr(dat)
}