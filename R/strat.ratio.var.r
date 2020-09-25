#' @export
strat.ratio.var<-function(dat,by.stratum=FALSE)
{
  est<-strat.ratio.tot.var(dat,by.stratum=by.stratum)
  if(by.stratum) {
    est$est<-est$est/sum(dat$N)^2
    est$est.by.stratum<-est$est.by.stratum/dat$N^2
  } else est<-est/sum(dat$N)^2
  est
}