#' @export
est.syy<-function(dat,mdf=1,n=dat$n)
{
  #  if(!is.subsample(dat)) stop("Argument 'dat' must be of class 'subsample'")
  if(is.null(dat$y.value)) stop("No response (y) data in 'dat'")
  if(n-mdf<1) stop("Sample size too small to estimate Sxy")
  y<-dat$y.value
  meany<-apply(y,1,mean)
  (apply(y*y,1,sum)-n*meany^2)/(n-mdf)
}