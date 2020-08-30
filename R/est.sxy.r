est.sxy<-function(dat,mdf=1,n=dat$n)
{
  #  if(!is.subsample(dat)) stop("Argument 'dat' must be of class 'subsample'")
  if(is.null(dat$x.value)) stop("No auxiliary (x) data in 'dat'")
  if(is.null(dat$y.value)) stop("No response (y) data in 'dat'")
  if(n-mdf<1) stop("Sample size too small to estimate Sxy")
  xy<-dat$x.value*dat$y.value
  meanx<-apply(dat$x.value,1,mean)
  meany<-apply(dat$y.value,1,mean)
  (apply(xy,1,sum)-n*meanx*meany)/(n-mdf)
}
