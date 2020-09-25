#' @export
clust.ratio.var<-function(dat)
{
  rat<-apply(dat$y.value,1,mean)/apply(dat$x.value,1,mean)
  ((1-dat$m/dat$M)/dat$m)*(est.syy(dat,n=dat$m)-2*rat*est.sxy(dat,n=dat$m)+rat^2*est.sxx(dat,n=dat$m))/dat$mu.x^2
}