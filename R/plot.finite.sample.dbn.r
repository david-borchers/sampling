plot.finite.sample.dbn<-function(dat, type="point.est", show.mean=TRUE, freq=TRUE, true.stat=NULL, ...)
{
  if(!is.finite.sample.dbn(dat)) stop("Argument 'dat' must be of class 'finite.sample.dbn'")
  valid.type<-c("point.est","var.est","ci.est",)
  if(!is.element(type,valid.type)) {
    cat("\nInvalid value for argument 'type'")
    cat("\nValid types are",valid.type,"\n")
    stop("")
  }
  if(type=="ci.est") {
    if(is.null(true.stat)) warning("To show coverage, you need to specify 'true.stat'")
    if(!is.element("ci.lower",names(dat)) | !is.element("ci.upper",names(dat))) {
      cat("Argument 'dat' must have elements 'ci.lower' and 'ci.upper'.\n")
      stop("Run 'add.ci.est' to add them.'")
    }
    keep<-(!is.nan(dat$ci.lower)&!is.nan(dat$ci.upper))
    lower<-dat$ci.lower[keep]; upper<-dat$ci.upper[keep]
    nsamp<-length(lower)
    xlim<-range(lower,upper)
    ylim<-c(0,nsamp+1)
    y0<-y1<-1:nsamp
    plot(xlim,ylim,type="n",xlab="CIs (lines)",ylab="Sample number",main="Confidence interval coverage")
    segments(lower,y0,upper,y1)
    if(!is.null(true.stat)) {
      lines(rep(true.stat,2),c(1,nsamp),col="blue")
      text(true.stat,0.5,label=as.character(true.stat),col="blue")
      text(true.stat,(nsamp+0.5),label="True statistic value",col="blue")
    }
  } else {
    if(!is.element(type,names(dat))) {
      cat("\n'dat' does not have a column named ",type,"\n")
      if(type=="point.est") stop("Run 'add.point.est' to generate this column.")
      if(type=="var.est") stop("Run 'add.var.est' to generate this column.")
    }
    stat<-dat[[type]]
    dbn.hist<-hist(stat,plot=FALSE,freq=freq,...)
    if(freq) maxy<-max(dbn.hist$counts) else maxy<-max(dbn.hist$density)
    breaks<-dbn.hist$breaks
    hist(stat,freq=freq,xlab="Estimator",main="Sampling distribution",...)
    if(show.mean) {
      mn<-mean(stat)
      lines(rep(mn,2),c(0,maxy),col="blue",lty=2)
      text(rep(mn,2),c((-0.02*maxy), (1.02*maxy)), col="blue", label=c(as.character(format(mn)),"Expecation"))
    }
  } # end if(type!="ci.est")
}
