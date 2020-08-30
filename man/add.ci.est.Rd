\name{add.ci.est}
\alias{add.ci.est}
\title{Calculates confidence interval estimates from samples.}
\description{
  This function calculates confidence interval estimates for population mean, total or proportion from %%@
    an object of class 'sample' or 'samples.population' and them it to the sample in columns named %%@
    "ci.lower" and "ci.upper".}
\usage{
  add.ci.est(dat,type="ybar",level=0.95)}
\arguments{
  \item{dat}{Object of class 'sample' or 'samples.population'.}
  \item{level}{Confidence level; 0.95 (95\%) by default.}
}
\details{
  Returns an approximate confidence interval estimate of the appropriate type, assuming that the %%@
    statistic has a t-distribution with degrees of freedom equal to sample size. Elements "point.est" %%@
    and "var.est" are used to construct the confidence interval(s), so they must already be in 'dat'. %%@
    They can be added to 'dat' using \code{\link{add.point.est}} and \code{\link{add.var.est}}
}
\value{
  The function returns an object identical to 'dat' except that elements $ci.lower and $ci.upper, %%@
    being the lower and upper (100*level) confidence interval bounds from the sample, have been added.}
\seealso{
  \code{\link{ci.est}}, \code{\link{var.est}}, \code{\link{add.var.est}}, %%@
    \code{\link{point.est}}, \code{\link{add.point.est}}
}
\examples{
  data(barnett) # get barnett data
  
  # All samples total weight of class from srs:
  samp.dbn<-take.sample(barnett,y.name="weight",n=23,take.all=TRUE)
  # estimate total
  samp.dbn<-add.point.est(samp.dbn,type="ybar.tot")
  samp.dbn<-add.var.est(samp.dbn,type="ybar.tot")
  samp.dbn<-add.ci.est(samp.dbn)
  
  # proportion of class with weight more than 15, from srs:
  samp.dbn<-take.sample(barnett,y.name="weight",n=23,take.all=TRUE)
  # estimate total
  samp.dbn<-add.point.est(samp.dbn,type="ybar.ppn",cond=">15")
  samp.dbn<-add.var.est(samp.dbn,type="ybar.ppn",cond=">15")
  samp.dbn<-add.ci.est(samp.dbn)
  
  # Stratified mean
  # Define 4 strata based on weight, with 10 lb interval widths:
  strat.barnett<-define.subunit(barnett,aux.name="weight",breaks=c(0,10,20,30,40),type="strat")
  # take stratified sample
  samp.dbn<-take.sample(strat.barnett,y.name="height",type="strat",n=c(2,9,7,3),take.all=TRUE)
  # estimate mean height using stratified estimator
  samp.dbn<-add.point.est(samp.dbn,type="strat.mean")
  samp.dbn<-add.var.est(samp.dbn,type="strat.mean")
  samp.ci<-add.var.est(samp.dbn)
  
  # Cluster mean
  # Define 5 clusters based on weight:
  clust.barnett<-define.subunit(barnett,aux.name="weight",
                                breaks=c(0,10,15,20,25,30,40),type="clust")
  # take cluster sample of size 3
  samp.dbn<-take.sample(clust.barnett,y.name="height",type="clust",m=3,take.all=TRUE)
  # estimate mean height using cluster estimator
  samp.dbn<-add.point.est(samp.dbn,type="clust.mean")
  samp.dbn<-add.var.est(samp.dbn,type="clust.mean")
  samp.dbn<-add.ci.est(samp.dbn)
  
  # Ratio estimator
  # take srs of size 6, with weight as auxiliary variable:
  samp.dbn<-take.sample(barnett,y.name="height",aux.name="weight",type="srs",n=23,take.all=TRUE)
  # estimate mean height using cluster estimator
  samp.dbn<-add.point.est(samp.dbn,type="ratio.mean")
  samp.dbn<-add.var.est(samp.dbn,type="ratio.mean")
  samp.dbn<-add.ci.est(samp.dbn)
  
}
\keyword{}
