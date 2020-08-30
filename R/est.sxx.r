# S^2 functions:
est.sxx<-function(dat,mdf=1,n=dat$n)
{
  #  if(!is.subsample(dat)) stop("Argument 'dat' must be of class 'subsample'")
  if(is.null(dat$x.value)) stop("No auxiliary (x) data in 'dat'")
  if(n-mdf<1) stop("Sample size too small to estimate Sxy")
  x<-dat$x.value
  meanx<-apply(x,1,mean)
  (apply(x*x,1,sum)-n*meanx^2)/(n-mdf)
}