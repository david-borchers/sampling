#' @export
regr.tot.var<-function(dat)
{
  dat$N^2*regr.var(dat)
}