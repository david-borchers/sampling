#' @export
finite.sample.size<-function(dat,n=NULL,m=NULL)
{
  if(is.null(n) & is.null(m)) stop("You must specify either 'n' or 'm'.")
  if(!is.null(n) & !is.null(m)) stop("You can't specify both 'n' and 'm'.")
  if(!is.data.frame(dat)) stop("Argument 'dat' must be a dataframe.")
  if(!is.null(n)) {
    if(length(n)>1) {
      if(!is.element("subunit",names(dat))) {
        cat("\nArgument 'n' is a vector, but argument 'dat' does not have a $subunit column.\n")
        stop("Can only do stratified sampling if 'dat' has a $subunit column.\n")
      }
      type=" stratified random"
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
      nsamp<-round(exp(lgamma(N+1)-lgamma(n+1)-lgamma(N-n+1)))
    } else {
      type=" simple random"
      N<-length(dat[[1]])
      if(n>N) stop("Sample size (n) is bigger than population size (N).")
      nsamp<-round(exp(lgamma(N+1)-lgamma(n+1)-lgamma(N-n+1)))
    }
  } else {
    if(!is.element("subunit",names(dat))) {
      cat("\nArgument 'm' is used (implying cluster sampling),\n")
      cat(" but argument 'dat' does not have a $subunit column.\n")
      stop("Can only do cluster sampling if 'dat' has a $subunit column.\n")
    }
    type=" cluster"
    s.name<-sort(unique(dat$subunit))
    M<-length(s.name)
    if(m>M) stop("Sample size (m) is bigger than number of clusters (M).")
    nsamp<-round(exp(lgamma(M+1)-lgamma(m+1)-lgamma(M-m+1)))
    n<-m
  }  
  cat("There are ",prod(nsamp),type," samples of size ",vec2char(n)," that can be taken.\n")
}