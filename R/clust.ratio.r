clust.ratio<-function(dat)
{
  if(is.null(dat$x.value)) stop("Can't use cluster ratio estimator without auxiliary variables N") 
  if(is.null(dat$mu.x)) stop("Can't use cluster ratio estimator without population mean of the auxiliary variable N") 
  mu.x<-dat$mu.x
  xbar<-apply(dat$x.value,1,mean)
  apply((dat$y.value)/xbar,1,mean)
}