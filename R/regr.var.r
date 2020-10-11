#' @export
regr.var<-function(dat)
{
  ((1-dat$n/dat$N)/dat$n)*(est.syy(dat,mdf=2)-est.sxy(dat,mdf=2)^2/est.sxx(dat,mdf=2))
}