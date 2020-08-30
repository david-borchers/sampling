summary.finite.sample.dbn<-function(dat,type="point.est",plot=FALSE,true.stat=NULL)
{
  if(!is.finite.sample.dbn(dat)) stop("Argument 'dat' must be of class 'finite.sample.dbn'")
  valid.type<-c("point.est","var.est","ci.est",)
  if(!is.element(type,valid.type)) {
    cat("\nInvalid value for argument 'type'")
    cat("\nValid types are",valid.type,"\n")
    stop("")
  }
  if(type=="ci.est") {
    if(is.null(true.stat)) warning("To get coverage probabilites, you need to specify 'true.stat'")
    if(!is.element("ci.lower",names(dat)) | !is.element("ci.upper",names(dat))) {
      cat("Argument 'dat' must have elements 'ci.lower' and 'ci.upper'.\n")
      stop("Run 'add.ci.est' to add them.'")
    }
    keep<-(!is.nan(dat$ci.lower)&!is.nan(dat$ci.upper))
    lower<-dat$ci.lower[keep]; upper<-dat$ci.upper[keep]
    nsamp<-length(lower)
    c.width<-mean(upper-lower)
    c.prob<-sum(lower<=true.stat & true.stat<=upper)*100/nsamp
    cat("\nFinite Sampling Distribution Summary")
    cat("\n=========================================")
    cat("\n            Finite population sample size: ",nsamp)
    if(!is.null(true.stat)) cat("\n Confidence interval coverage probability: ",format(c.prob))
    cat("\n        Average confidence interval width: ",format(c.width),"\n")
    if(plot) {
      plot.finite.sample.dbn(dat,type="ci.est",true.stat=true.stat)
    }
  } else {
    if(!is.element(type,names(dat))) {
      cat("\n'dat' does not have a column named ",type,"\n")
      if(type=="point.est") stop("Run 'add.point.est' to generate this column.'")
      if(type=="var.est") stop("Run 'add.var.est' to generate this column.'")
    }
    stat<-dat[[type]]
    nsamp<-length(stat)
    mean.stat<-mean(stat)
    var.stat<-var(stat)*(nsamp-1)/nsamp
    cat("\nFinite Sampling Distribution Summary")
    cat("\n====================================")
    cat("\n   Finite population sample size: ",nsamp)
    cat("\n               Mean of statistic: ",format(mean.stat))
    cat("\n           Variance of statistic: ",format(var.stat))
    cat("\n Standard deviation of statistic: ",format(sqrt(var.stat)),"\n")
    if(plot) {
      plot(dat,type=type)
      #      plot(dat$units,stat,xlab="Sample",ylab="Statistic",main="Individual samples")
    }
  }
}