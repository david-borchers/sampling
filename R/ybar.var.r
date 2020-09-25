#' @export
ybar.var<-function(dat)
{
  (1-dat$n/dat$N)*est.syy(dat)/dat$n
}