#' @export
point.est<-function(dat,type="ybar",cond=NULL,by.stratum=FALSE)
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
         ybar = strat.mean(dat),
         ybar.tot = strat.tot(dat),
         ybar.ppn = strat.ppn(dat,cond),
         strat.mean = strat.mean(dat,by.stratum=by.stratum),
         strat.tot = strat.tot(dat,by.stratum=by.stratum),
         strat.ppn = strat.ppn(dat,cond,by.stratum=by.stratum),
         ratio.mean = strat.ratio.mean(dat),
         ratio.tot = strat.ratio.tot(dat),
         ratio.ppn = strat.ratio.ppn(dat,cond),
         strat.ratio.mean = strat.ratio.mean(dat,by.stratum=by.stratum),
         strat.ratio.tot = strat.ratio.tot(dat,by.stratum=by.stratum),
         strat.ratio.ppn = strat.ratio.ppn(dat,cond,by.stratum=by.stratum),
         regr.mean = strat.regr.mean(dat),
         regr.tot = strat.regr.tot(dat),
         regr.ppn = strat.regr.ppn(dat,cond),
         strat.regr.mean = strat.regr.mean(dat,by.stratum=by.stratum),
         strat.regr.tot = strat.regr.tot(dat,by.stratum=by.stratum),
         strat.regr.ppn = strat.regr.ppn(dat,cond,by.stratum=by.stratum),
         diffr.mean = strat.diffr.mean(dat),
         diffr.tot = strat.diffr.tot(dat),
         diffr.ppn = strat.diffr.ppn(dat,cond),
         strat.diffr.mean = strat.diffr.mean(dat,by.stratum=by.stratum),
         strat.diffr.tot = strat.diffr.tot(dat,by.stratum=by.stratum),
         strat.diffr.ppn = strat.diffr.ppn(dat,cond,by.stratum=by.stratum),
         clust.mean = clust.mean(dat),
         clust.tot = clust.tot(dat),
         clust.ppn = clust.ppn(dat,cond),
         clust.r.mean = clust.ratio(dat),
         clust.r.tot = clust.ratio.tot(dat),
         clust.r.ppn = clust.ratio.ppn(dat,cond),
  )
}