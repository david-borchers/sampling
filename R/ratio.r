#' @export
ratio<-function(dat)
{
  if(is.null(dat$x.value)) stop("Can't use ratio estimator without an auxiliary variable") 
  if(is.null(dat$mu.x)) stop("Can't use ratio estimator without population mean of the auxiliary variable") 
  mu.x<-dat$mu.x
  xbar<-apply(dat$x.value,1,mean)
  apply((dat$y.value)*mu.x/xbar,1,mean)
}