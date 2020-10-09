#' @export
ci.est<-function(dat,type="ybar",level=0.95)
{
  if(!is.sample(dat)) stop("Argument 'dat' must be of class 'sample'")
  if(substring(type,1,4)=="ybar" & dat$M>1) stop("Use of type 'ybar' on multilevel sample not supported") 
  if(substring(type,1,4)=="ratio" & dat$M>1) stop("Use of type 'ratio' on multilevel sample not supported") 
  switch(type,
         ybar = t.dbn.ci(strat.mean(dat),strat.var(dat),level=level,df=dat$n),
         ybar.tot = t.dbn.ci(strat.tot(dat),strat.tot.var(dat),level=level,df=dat$n),
         ybar.ppn = t.dbn.ci(strat.ppn(dat),strat.tot.ppn(dat),level=level,df=dat$n),
         strat.mean = t.dbn.ci(strat.mean(dat),strat.var(dat),level=level,df=dat$n),
         strat.tot = t.dbn.ci(strat.tot(dat),strat.tot.var(dat),level=level,df=dat$n),
         strat.ppn = t.dbn.ci(strat.ppn(dat),strat.tot.ppn(dat),level=level,df=dat$n), 
         ratio.mean = t.dbn.ci(strat.ratio.mean(dat),strat.ratio.var(dat),level=level,df=dat$n),
         ratio.tot = t.dbn.ci(strat.ratio.tot(dat),strat.ratio.tot.var(dat),level=level,df=dat$n),
         ratio.ppn = t.dbn.ci(strat.ratio.ppn(dat),strat.ratio.tot.ppn(dat),level=level,df=dat$n),
         strat.ratio.mean = t.dbn.ci(strat.ratio(dat),strat.ratio.var(dat),level=level,df=dat$n),
         strat.ratio.tot = t.dbn.ci(strat.ratio.tot(dat),strat.ratio.tot.var(dat),level=level,df=dat$n),
         strat.ratio.ppn = t.dbn.ci(strat.ratio.ppn(dat),strat.ratio.tot.ppn(dat),level=level,df=dat$n),
         regr.mean = t.dbn.ci(strat.regr.mean(dat),strat.regr.var(dat),level=level,df=dat$n),
         regr.tot = t.dbn.ci(strat.regr.tot(dat),strat.regr.tot.var(dat),level=level,df=dat$n),
         regr.ppn = t.dbn.ci(strat.regr.ppn(dat,cond),strat.regr.ppn.var(dat),level=level,df=dat$n),
         strat.regr.mean = t.dbn.ci(strat.regr.mean(dat),strat.regr.var(dat),level=level,df=dat$n),
         strat.regr.tot = t.dbn.ci(strat.regr.tot(dat),strat.regr.tot.var(dat),level=level,df=dat$n),
         strat.regr.ppn = t.dbn.ci(strat.regr.ppn(dat,cond),strat.regr.ppn.var(dat),level=level,df=dat$n),
         diffr.mean = t.dbn.ci(strat.diffr.mean(dat),strat.diffr.var(dat),level=level,df=dat$n),
         diffr.tot = t.dbn.ci(strat.diffr.tot(dat),strat.diffr.tot.var(dat),level=level,df=dat$n),
         diffr.ppn = t.dbn.ci(strat.diffr.ppn(dat,cond),strat.diffr.ppn.var(dat),level=level,df=dat$n),
         strat.diffr.mean = t.dbn.ci(strat.diffr.mean(dat),strat.diffr.var(dat),level=level,df=dat$n),
         strat.diffr.tot = t.dbn.ci(strat.diffr.tot(dat),strat.diffr.tot.var(dat),level=level,df=dat$n),
         strat.diffr.ppn = t.dbn.ci(strat.diffr.ppn(dat,cond),strat.diffr.ppn.var(dat),level=level,df=dat$n),
         clust.mean = t.dbn.ci(clust.mean(dat),clust.var(dat),level=level,df=dat$m),
         clust.tot = t.dbn.ci(clust.tot(dat),clust.tot.var(dat),level=level,df=dat$m),
         clust.ppn = t.dbn.ci(clust.ppn(dat,cond),clust.ppn.var(dat),level=level,df=dat$m),
         clust.r.mean = t.dbn.ci(clust.ratio(dat),clust.ratio.var(dat),level=level,df=dat$m),
         clust.r.tot = t.dbn.ci(clust.ratio.tot(dat),clust.ratio.tot.var(dat),level=level,df=dat$m),
         clust.r.ppn = t.dbn.ci(clust.ratio.ppn(dat,cond),clust.ratio.ppn.var(dat),level=level,df=dat$m),
  )
}