#' @title Take sample(s) from population data frame
#' @description 
#' This function generates a single sample (of class 'sample') or a multiple sample (of class 'samples.population') 
#' containing all possible samples without replacement, from a given population.
#' 
#' @usage take.sample(dat,y.name,n=0,m=0,type="srs",aux.name=NULL,take.all=FALSE)
#' @param dat Data frame containing the population to be sampled.
#' @param y.name Name of the response variable (must be a column name of 'dat')
#' @param n Primary sample unit sample size. If it is a vector, it is assumed to be the sample sizes of 
#' within each stratum. For example, to take a stratified sample with 5 primary sample units from stratum 
#' 1, 3 primary sample units from stratum 2 and 6 primary sample units from stratum 3, you would have 
#' 'n=c(5,3,6). Has no effect in the case of cluster sampling.
#' @param m Secondary sample unit size. In cluster sampling, it is the number of clusters to be sampled. 
#' Has no effect in the case of stratified sampling.
#' @param type Type of sampling. Valid types are 
#' \itemize{
#'   \item{"srs"}{for simple random sampling,} 
#'   \item{"strat"}{for stratified simple random sampling,}  
#'   \item{"clust"}{for cluster sampling.}
#' }
#' @param aux.name Name of the auxiliary variable, if this is to be included in the sample (must be a 
#' column name of 'dat'). Not used in cluster sampling.
#' @param take.all if FALSE, a single sample is generated; if TRUE all possible samples without 
#' replacement are generated

#' @details 
#' Units are sampled without replacement. In the case of cluster sampling, secondary units are 
#' sampled with equal probability; in the case of simple random sampling, primary units are sampled with 
#' equal probability, and in the case of stratified simple random sampling, primary units are sampled with 
#' equal probability within strata.
#' 
#' @return
#' The function returns an object of class `sample? when take.all==FALSE and of class 
#' 'samples.population' when take.all==TRUE. The object has the following components:
#' \itemize{ 
#'   \item{M}{Number of secondary units in population}
#'   \item{m}{Number of secondary units sampled}
#'   \item{N}{Number of primary units in population; if stratified, this is a vector of length M.}
#'   \item{n}{Number of primary units in sample; if stratified, this is a vector of length M.}
#'   \item{y.value}{For cluster sampling, this contains a vector with the sum of the y values in the sampled 
#'   subunits. For srs and stratified rs, it is NULL.}
#'   \item{x.value}{For cluster sampling, this contains a vector with the sizes (Nj) of the sampled 
#'   subunits. For srs and stratified rs, it is NULL.}
#'   \item{subunit1 ... subunitm}{Objects of class 'subsample' or 'subsamples.population' containing the primary 
#'   unit sampled data. In the case of cluster sampling, this is NULL. Each subunit contains the 
#'   following components:}
#'   \itemize{
#'     \item{N}{Number of primary units in the subunit}
#'     \item{n}{Number of primary units sampled in the subunit}
#'     \item{mu.x}{Population mean of the auxiliary variable in the subunit. If there is no auxiliary 
#'     variable, it is NULL.}
#'     \item{x.value}{Sampled auxiliary variable (x) values in the subunit. This is a matrix with one row per 
#'     sample. If there is no auxiliary variable, it is NULL.}
#'     \item{y.value}{Sampled response (y) values in the subunit. This is a matrix with one row per sample.}
#'     \item{units}{Indices of the primary units in the sample. This is a character vector, with one element 
#'     per sample. Each element is of the form "(i1,i2,...,in)", where i1 is the index of the first unit, i2 
#'     the index of the second unit, and so on.}
#'   }
#' }
#' 
#' @seealso \code{\link{define.subunit}}
#' @examples
#'   data(ackroyd) # get ackroyd data
#'   
#'   # simple random sample of sales, of size 4
#'   samp<-take.sample(ackroyd,y.name="sales",n=4)
#'   summary(samp) # summarize sample data
#'   
#'   # All simple random samples of sales, of size 4
#'   all.samp<-take.sample(ackroyd,y.name="sales",n=4,take.all=TRUE) 
#'   summary(all.samp) # summarize sample data
#'   
#'   # All simple random samples of sales, of size 4, with auxiliary variable "mplyees"
#'   all.samp<-take.sample(ackroyd,y.name="sales",n=4,aux.name="mplyees",take.all=TRUE) 
#'   summary(all.samp) # summarize sample data
#'   
#'   # All stratified random samples of sales, of size 4, with auxiliary variable "mplyees"
#'   # First need to define strata:
#'   strat.ackroyd<-define.subunit(ackroyd,aux.name="nature",type="strat")
#'   # Then take samples:
#'   all.strat.samp<-take.sample(strat.ackroyd,type="strat", y.name="sales",n=c(2,2),aux.name="mplyees",take.all=TRUE) 
#' summary(all.strat.samp) # summarize sample data
#' 
#' @export
take.sample<-function(dat,y.name,n=0,m=0,type="srs",aux.name=NULL,take.all=FALSE)
{
  # local function:
  get.samp<-function(index,vec) vec[index]
  
  nsamp<-1
  
  # some checks:
  valid.type=c("srs","strat","clust")
  if(!is.element(type,valid.type)) {
    cat("\nInvalid 'type'; valid types are: ",valid.type,"\n")
    stop("'type' must be one of these")
  }
  valid.y.name<-names(dat)
  if(!is.element(y.name,valid.y.name)) {
    cat("\nInvalid 'y.name'; valid names are: ",valid.y.name,"\n")
    stop("'y.name' must be one of these")
  }
  valid.x.name<-names(dat)[-which(names(dat)==y.name)]
  if(!is.null(aux.name)) {
    if(!is.element(aux.name,valid.x.name)) {
      cat("\nInvalid 'aux.name'; valid names are: ",valid.x.name,"\n")
      stop("'aux.name' must be one of these")
    }}
  
  # response:
  y.col<-which(names(dat)==y.name)
  Y<-dat[[y.col]]
  N<-length(Y)
  
  # stratified sampling:
  if(type=="strat") {
    if(n==0) stop("You must specify 'n'")
    if(!is.element("subunit",names(dat)[-which(names(dat)==y.name)])) 
      stop("No stratification variable - you can add one using function define.subunit()")
    if(!is.null(aux.name)) {
      x.col<-which(names(dat)==aux.name)
      X<-dat[[x.col]]
      if(length(X)!=N) stop("Variables 'y.name. and 'aux.name' have different lengths")
    }
    s.name<-sort(unique(dat$subunit))
    M<-length(s.name)
    if(length(n)!=M) {
      cat("\nThere are M=",M," strata, but 'n' is of length ",length(n),".\n")
      stop("'n' must be a vector of length M")
    }
    N<-rep(0,M)
    for(i in 1:M) N[i]<-sum(dat$subunit==s.name[i])
    if(any(N<1)) stop("At least one subunit is empty - you should re-stratify.")
    if(sum(n>N)) stop("Sample size (n) is bigger than population size (N) in at lest one subunit")
    if(take.all) nsamp<-exp(lgamma(N+1)-lgamma(n+1)-lgamma(N-n+1))
    samp<-as.list(NULL)
    for(i in 1:M) {
      if(take.all) comb<-t(combinations(N[i],n[i]))
      else comb<-matrix(sample(1:N[i],size=n[i]),nrow=1)
      sample.index<-apply(comb,1,vec2char)
      samp[[s.name[i]]]<-list(N=0,n=0,mu.x=NULL,units="",x.value=NULL,y.value=NULL)
      samp[[s.name[i]]]$N<-N[i]
      samp[[s.name[i]]]$n<-n[i]
      samp[[s.name[i]]]$units<-sample.index
      ys<-t(apply(comb,1,get.samp,Y[dat$subunit==s.name[i]]))
      if(dim(comb)[2]==1) ys<-matrix(ys,ncol=1) # because 1-D matrix becomes vector above!
      colnames(ys)<-paste(y.name,as.character(1:n[i]),sep=".")
      samp[[s.name[i]]]$y.value<-ys
      if(is.null(aux.name)) {
        aux<-NULL
        mean.aux<-NULL
      } else {
        aux<-t(apply(comb,1,get.samp,X[dat$subunit==s.name[i]]))
        if(dim(comb)[2]==1) aux<-matrix(aux,ncol=1) # because 1-D matrix becomes vector above!
        colnames(aux)<-paste(aux.name,as.character(1:n[i]),sep=".")
        samp[[s.name[i]]]$x.value<-aux
        samp[[s.name[i]]]$mu.x<-mean(samp[[s.name[i]]]$x.value)
      }
      class(samp[[s.name[i]]])<-c("subsamples.population", "subsample")
    }
    samp$M<-M
    samp$m<-M
    samp$N<-N
    samp$n<-n
    samp$nsamp<-nsamp
    names(samp)[1:M]<-paste("subunit",as.character(c(1:M)),sep="")
  }
  
  # cluster sampling:
  if(type=="clust") {
    if(m==0) stop("You must specify 'm'")
    if(!is.null(aux.name)) Warning("Variable 'aux.name' is not used in cluster sampling")
    if(!is.element("subunit",names(dat)[-which(names(dat)==y.name)])) 
      stop("No cluster variable defined - you can add one using function define.subunit()")
    X<-dat$subunit
    if(length(X)!=N) stop("Variables 'y.name'. and subunit have different lengths")
    s.name<-sort(unique(dat$subunit))
    M<-length(s.name)
    if(length(m)!=1) stop("'m' must be a scalar<",M,"\n")
    samp<-as.list(NULL)
    Y.tot<-rep(0,M)
    #   get totals in each cluster and store them
    N<-rep(0,M)
    for(i in 1:M) {
      Y.tot[i]<-sum(Y[dat$subunit==s.name[i]])
      N[i]<-sum(dat$subunit==s.name[i])
    }
    if(any(N<1)) stop("At least one subunit is empty - you should re-define  clusters.")
    if(sum(m>M)) stop("Cluster sample size (m) is bigger than population size (M)")
    if(take.all) {
      nsamp<-exp(lgamma(M+1)-lgamma(m+1)-lgamma(M-m+1))
      comb<-t(combinations(M,m))
    } else {
      nsamp<-1
      comb<-matrix(sample(1:M,size=m),nrow=1)
    }
    sample.index<-apply(comb,1,vec2char)
    ys<-t(apply(comb,1,get.samp,Y.tot))
    if(dim(comb)[2]==1) ys<-matrix(ys,ncol=1) # because 1-D matrix becomes vector above!
    colnames(ys)<-paste(y.name,as.character(1:m),sep=".")
    xs<-t(apply(comb,1,get.samp,N))
    if(dim(comb)[2]==1) ys<-matrix(ys,ncol=1) # because 1-D matrix becomes vector above!
    colnames(xs)<-paste("N",as.character(1:m),sep=".")
    samp$units<-sample.index
    samp$mu.x<-mean(N)
    samp$y.value<-ys
    samp$x.value<-xs
    class(samp)<-c("samples.population", "sample")
    samp$M<-M
    samp$m<-m
    samp$N<-N
    samp$n<-N
    samp$nsamp<-nsamp
  }
  
  # simple random sampling:
  if(type=="srs") {
    if(length(n)>1) stop("With srs, 'n' must contain only one value.")
    if(take.all) {
      nsamp<-exp(lgamma(N+1)-lgamma(n+1)-lgamma(N-n+1))
      comb<-t(combinations(N,n))
    } else {
      nsamp<-1
      comb<-matrix(sample(1:N,size=n),nrow=1)
    }
    sample.index<-apply(comb,1,vec2char)
    ys<-t(apply(comb,1,get.samp,Y))
    if(dim(comb)[2]==1) ys<-matrix(ys,ncol=1) # because 1-D matrix becomes vector above!
    colnames(ys)<-paste(y.name,as.character(1:n),sep=".")
    if(is.null(aux.name)) {
      aux<-NULL
      mean.aux<-NULL
    } else {
      x.col<-which(names(dat)==aux.name)
      X<-dat[[x.col]]
      if(length(X)!=N) stop("Variables 'y.name. and 'aux.name' have different lengths")
      mean.aux<-mean(X)
      aux<-t(apply(comb,1,get.samp,X))
      if(dim(comb)[2]==1) aux<-matrix(aux,ncol=1) # because 1-D matrix becomes vector above!
      colnames(aux)<-paste(aux.name,as.character(1:n),sep=".")
    }
    stratum<-list(mu.x=mean.aux,units=sample.index,y.value=ys,x.value=aux,N=N,n=n)
    class(stratum)<-c("subsamples.population", "subsample")
    samp<-list(subunit1=stratum,M=1,m=1,N=N,n=n,nsamp=nsamp)
  }
  cat("\nA total of ",vec2char(round(nsamp),"*")," samples was generated\n")
  if(take.all) class(samp)<-c("samples.population", "sample")
  else class(samp)<-c("sample")
  samp
}
