#' @export
clust.mean<-function(dat)
{
  dat$M*apply(dat$y.value,1,mean)/sum(dat$N)
}