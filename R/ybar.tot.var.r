ybar.tot.var<-function(dat)
{
  dat$N^2*(1-dat$n/dat$N)*est.syy(dat)/dat$n
}
