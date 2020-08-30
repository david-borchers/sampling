diffr<-function(dat)
{
  if(is.null(dat$x.value)) stop("Can't use difference estimator without an auxiliary variable") 
  if(is.null(dat$mu.x)) stop("Can't use difference estimator without population mean of the auxiliary variable") 
  mu.x<-dat$mu.x
  xbar<-apply(dat$x.value,1,mean)
  ybar<-apply(dat$y.value,1,mean)
  ybar+(mu.x-xbar)
}