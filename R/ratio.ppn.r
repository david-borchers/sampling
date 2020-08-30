ratio.ppn<-function(dat,cond)
{
  mu.x<-dat$mu.x
  xbar<-apply(dat$x.value,1,mean)
  apply((eval(parse(text=paste("dat$y.value",cond,sep="")))*1)*mu.x/xbar,1,mean)
}