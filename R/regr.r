regr<-function(dat)
{
  if(is.null(dat$x.value)) stop("Can't use regression estimator without an auxiliary variable") 
  if(is.null(dat$mu.x)) stop("Can't use regression estimator without population mean of the auxiliary variable") 
  mu.x<-dat$mu.x
  xbar<-apply(dat$x.value,1,mean)
  ybar<-apply(dat$y.value,1,mean)
  ybar+est.beta(dat,mdf=2)*(mu.x-xbar)
}