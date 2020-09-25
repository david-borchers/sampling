#' @export
diffr.var<-function(dat)
{
  ((1-dat$n/dat$N)/dat$n)*(est.syy(dat)-est.sxy(dat)/est.sxx(dat))
}