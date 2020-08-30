plot.sample<-function(dat)
{
  if(!is.sample(dat)) stop("Argument 'dat' must be of class 'sample'")
  nplots<-dat$m
  if(dat$M==1) type<-"Simple random"
  else if(dat$M==dat$m) type<-"Stratified random"
  else if(dat$M>dat$m) type<-"Cluster"
  if(type=="Cluster") {
    windows(height=5,width=5)
    plot(1:length(dat$y.value),dat$y.value,cex=dat$x.value/2,xlab="Cluster size",ylab="Cluster total",main="Cluster sample (dot size proportional to cluster size)")
  }
  if(type=="Stratified random" | type=="Simple random") {
    for(i in 1:dat$M) {
      windows(height=5,width=5)
      main=paste(type,"sample")
      if(type=="Stratified random") main<-paste(main,": stratum number ",as.character(i),sep="")
      if(!is.null(dat[[i]]$x.value)) plot(dat[[i]]$x.value,dat[[i]]$y.value,xlab="auxiliary variable (x)",ylab="response (y)",main=main)
      else plot(1:length(dat[[i]]$y.value),dat[[i]]$y.value,xlab="unit number",ylab="response (y)",main=main)
    }
  }
}