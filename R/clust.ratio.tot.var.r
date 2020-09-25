#' @export
clust.ratio.tot.var<-function(dat)
{
  dat$mu.x^2*clust.ratio.var(dat)
}