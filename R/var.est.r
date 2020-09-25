#' @export
# return estimate from "sample" object
var.est<-function(dat,type="ybar",cond=NULL,by.stratum=FALSE)
{
  if(!is.samples.population(dat) & !is.sample(dat)) stop("Argument 'dat' must be of class 'sample' or 'samples.population'.")
  if(is.samples.population(dat) & by.stratum) {
    by.stratum<-FALSE
    warning("Can generate results by stratum only for a single sample: 'by.stratum' set to FALSE.")
  }
  if(substring(type,1,4)=="ybar" & dat$M>1) stop("Use of type 'ybar' on multilevel sample not supported") 
  if(substring(type,1,5)=="ratio" & dat$M>1) stop("Use of type 'ratio' on multilevel sample not supported") 
  if(substring(type,1,5)!="clust" & dat$M>dat$m) stop("Only type 'clust' or 'clust.r' can be used on cluster sample") 
  switch(type,
         ybar = strat.var(dat),
         ybar.tot = strat.tot.var(dat),
         ybar.ppn = strat.ppn.var(dat,cond),
         strat.mean = strat.var(dat,by.stratum=by.stratum),
         strat.tot = strat.tot.var(dat,by.stratum=by.stratum),
         strat.ppn = strat.ppn.var(dat,cond,by.stratum=by.stratum),
         ratio.mean = strat.ratio.var(dat),
         ratio.tot = strat.ratio.tot.var(dat),
         ratio.ppn = strat.ratio.ppn.var(dat,cond),
         strat.ratio = strat.ratio.var(dat,by.stratum=by.stratum),
         strat.ratio.tot = strat.ratio.tot.var(dat,by.stratum=by.stratum),
         strat.ratio.ppn = strat.ratio.ppn.var(dat,cond,by.stratum=by.stratum),
         regr.mean = strat.regr.var(dat),
         regr.tot = strat.regr.tot.var(dat),
         regr.ppn = strat.regr.ppn.var(dat,cond),
         strat.regr.mean = strat.regr.var(dat,by.stratum=by.stratum),
         strat.regr.tot = strat.regr.tot.var(dat,by.stratum=by.stratum),
         strat.regr.ppn = strat.regr.ppn.var(dat,cond,by.stratum=by.stratum),
         diffr.mean = strat.diffr.var(dat),
         diffr.tot = strat.diffr.tot.var(dat),
         diffr.ppn = strat.diffr.ppn.var(dat,cond),
         strat.diffr.mean = strat.diffr.var(dat,by.stratum=by.stratum),
         strat.diffr.tot = strat.diffr.tot.var(dat,by.stratum=by.stratum),
         strat.diffr.ppn = strat.diffr.ppn.var(dat,cond,by.stratum=by.stratum),
         clust.mean = clust.var(dat),
         clust.tot = clust.tot.var(dat),
         clust.ppn = clust.ppn.var(dat,cond),
         clust.r.mean = clust.ratio.var(dat),
         clust.r.tot = clust.ratio.tot.var(dat),
         clust.r.ppn = clust.ratio.ppn.var(dat,cond),
  )
}
