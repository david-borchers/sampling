ybar.ppn.var<-function(dat,cond)
{
  if(dat$n<2) stop("Can't estimate variance with <2 samples")
  pdat<-dat
  pdat$y.value<-(eval(parse(text=paste("dat$y.value",cond,sep="")))*1)
  (1-dat$n/dat$N)*est.syy(pdat)/dat$n
}