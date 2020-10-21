#' @export
ci.est<-function(dat,type="ybar",level=0.95)
{
  if(!is.sample(dat)) stop("Argument 'dat' must be of class 'sample'")
  if(substring(type,1,4)=="ybar" & dat$M>1) stop("Use of type 'ybar' on multilevel sample not supported") 
  if(substring(type,1,4)=="ratio" & dat$M>1) stop("Use of type 'ratio' on multilevel sample not supported") 
  switch(type,
         ybar = t.dbn.ci(strat.mean(dat),strat.var(dat),level=level,df=dat$n-1),
         ybar.tot = t.dbn.ci(strat.tot(dat),strat.tot.var(dat),level=level,df=dat$n-1),
         ybar.ppn = t.dbn.ci(strat.ppn(dat),strat.tot.ppn(dat),level=level,df=dat$n-1),
         strat.mean = t.dbn.ci(strat.mean(dat),strat.var(dat),level=level,df=dat$n-1),
         strat.tot = t.dbn.ci(strat.tot(dat),strat.tot.var(dat),level=level,df=dat$n-1),
         strat.ppn = t.dbn.ci(strat.ppn(dat),strat.tot.ppn(dat),level=level,df=dat$n-1), 
         ratio.mean = t.dbn.ci(strat.ratio.mean(dat),strat.ratio.var(dat),level=level,df=dat$n-1),
         ratio.tot = t.dbn.ci(strat.ratio.tot(dat),strat.ratio.tot.var(dat),level=level,df=dat$n-1),
         ratio.ppn = t.dbn.ci(strat.ratio.ppn(dat),strat.ratio.tot.ppn(dat),level=level,df=dat$n-1),
         strat.ratio.mean = t.dbn.ci(strat.ratio.mean(dat),strat.ratio.var(dat),level=level,df=dat$n-1),
         strat.ratio.tot = t.dbn.ci(strat.ratio.tot(dat),strat.ratio.tot.var(dat),level=level,df=dat$n-1),
         strat.ratio.ppn = t.dbn.ci(strat.ratio.ppn(dat),strat.ratio.tot.ppn(dat),level=level,df=dat$n-1),
         regr.mean = t.dbn.ci(strat.regr.mean(dat),strat.regr.var(dat),level=level,df=dat$n-1),
         regr.tot = t.dbn.ci(strat.regr.tot(dat),strat.regr.tot.var(dat),level=level,df=dat$n-1),
         regr.ppn = t.dbn.ci(strat.regr.ppn(dat,cond),strat.regr.ppn.var(dat),level=level,df=dat$n-1),
         strat.regr.mean = t.dbn.ci(strat.regr.mean(dat),strat.regr.var(dat),level=level,df=dat$n-1),
         strat.regr.tot = t.dbn.ci(strat.regr.tot(dat),strat.regr.tot.var(dat),level=level,df=dat$n-1),
         strat.regr.ppn = t.dbn.ci(strat.regr.ppn(dat,cond),strat.regr.ppn.var(dat),level=level,df=dat$n-1),
         diffr.mean = t.dbn.ci(strat.diffr.mean(dat),strat.diffr.var(dat),level=level,df=dat$n-1),
         diffr.tot = t.dbn.ci(strat.diffr.tot(dat),strat.diffr.tot.var(dat),level=level,df=dat$n-1),
         diffr.ppn = t.dbn.ci(strat.diffr.ppn(dat,cond),strat.diffr.ppn.var(dat),level=level,df=dat$n-1),
         strat.diffr.mean = t.dbn.ci(strat.diffr.mean(dat),strat.diffr.var(dat),level=level,df=dat$n-1),
         strat.diffr.tot = t.dbn.ci(strat.diffr.tot(dat),strat.diffr.tot.var(dat),level=level,df=dat$n-1),
         strat.diffr.ppn = t.dbn.ci(strat.diffr.ppn(dat,cond),strat.diffr.ppn.var(dat),level=level,df=dat$n-1),
         clust.mean = t.dbn.ci(clust.mean(dat),clust.var(dat),level=level,df=dat$m),
         clust.tot = t.dbn.ci(clust.tot(dat),clust.tot.var(dat),level=level,df=dat$m),
         clust.ppn = t.dbn.ci(clust.ppn(dat,cond),clust.ppn.var(dat),level=level,df=dat$m),
         clust.r.mean = t.dbn.ci(clust.ratio(dat),clust.ratio.var(dat),level=level,df=dat$m),
         clust.r.tot = t.dbn.ci(clust.ratio.tot(dat),clust.ratio.tot.var(dat),level=level,df=dat$m),
         clust.r.ppn = t.dbn.ci(clust.ratio.ppn(dat,cond),clust.ratio.ppn.var(dat),level=level,df=dat$m),
  )
}