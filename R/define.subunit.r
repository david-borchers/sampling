define.subunit<-function(dat,aux.name,breaks=NULL,type="stratum") 
{
  if(is.null(aux.name)) stop("You must specify 'aux.name'")
  x.col<-which(names(dat)==aux.name)
  X<-dat[[x.col]]
  nobreaks<-FALSE
  if(is.null(breaks)) {
    nobreaks<-TRUE
    #   need code below because hist() gets clever with breaks, adding and subtracting bits
    ux<-sort(unique(X))
    lo<-c((min(X)-1),sort(unique(X))[1:length(ux)-1])
    breaks<-c(lo+(ux-lo)/2,max(X)+1)
  }
  h<-hist(X,breaks=breaks,plot=FALSE)
  N<-h$counts
  if(min(X)<min(breaks) | max(breaks)<max(X)) stop("'breaks' does not span range of auxiliary variable")
  M<-length(breaks)-1
  subunit<-X*0
  if(length(X==breaks[1])>0) subunit[X==breaks[1]]<-1
  for(i in 1:M) {
    ntodo<-length(subunit[subunit<1])
    if(ntodo>0) subunit[subunit<1]<-i*(rep(breaks[i],ntodo)<X[subunit<1] & X[subunit<1]<=rep(breaks[i+1],ntodo))
  }
  dat$subunit<-subunit
  header<-switch(type,
                 stratum = "                     Stratification summary",
                 cluster = "                         Cluster summary"
  )
  if(nobreaks) {
    cat("\n",header)
    cat("\n=====================================================================")
    for(i in 1:M) {
      cat("\n ",type,":",i,":  N=",N[i],", auxiliary variable value:",ux[i])
      if(i!=M) cat("\n")
    } 
    cat("\n=====================================================================\n\n")
  } else {
    cat("\n",header)
    cat("\n=====================================================================")
    for(i in 1:M) {
      if(i==1) lower<-"<=" else lower<-"<"
      cat("\n",type,":",i,":  N=:",N[i],"   Bounds:",breaks[i],lower,aux.name,"<=",breaks[i+1])
      if(i!=M) cat("\n")
    } 
    cat("\n=====================================================================\n\n")
  }
  dat
} 
