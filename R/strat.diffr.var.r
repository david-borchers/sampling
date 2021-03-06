#' @export
strat.diffr.var<-function(dat,by.stratum=FALSE)
{
  est<-strat.diffr.tot.var(dat,by.stratum=by.stratum)
  if(by.stratum) {
    est$est<-est$est/sum(dat$N)^2
    est$est.by.stratum<-est$est.by.stratum/dat$N^2
  } else est<-est/sum(dat$N)^2
  est
}