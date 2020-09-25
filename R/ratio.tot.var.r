#' @export
ratio.tot.var<-function(dat)
{
  rat<-apply(dat$y.value,1,mean)/apply(dat$x.value,1,mean)
  dat$N^2*((1-dat$n/dat$N)/dat$n)*(est.syy(dat)-2*rat*est.sxy(dat)+rat^2*est.sxx(dat))
}