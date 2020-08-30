\name{add.var.est}
\alias{add.var.est}
\title{Adds variance estimates to samples.}
\description{
  This function calculates variance estimates of population mean, total or proportion from an object of %%@
    class 'sample' or 'samples.population' and adds it to the sample in a column named "var.est".}

\usage{
  add.var.est(dat,type="ybar",cond=NULL)}
\arguments{
  \item{dat}{Object of class 'sample' or 'samples.population'.}
  \item{type}{Type of variance estimate. Valid types are 
              \itemize{
                \item{ybar}{sample mean}
                \item{ybar.tot}{sample total}
                \item{ybar.ppn}{sample proportion}
                \item{strat.mean}{stratified sample mean}
                \item{strat.tot}{stratified sample totatl}
                \item{strat.ppn}{stratified sample proportion}
                \item{ratio.mean}{ratio estimate of mean}
                \item{ratio.tot}{ratio estimate of toatal}
                \item{ratio.ppn}{ratio estimate of proportion}
                \item{strat.ratio}{stratified ratio estimate of mean}
                \item{strat.ratio.tot}{stratified ratio estimate of total}
                \item{strat.ratio.ppn}{stratified ratio estimate of proportion}
                \item{regr.mean}{regression estimate of mean}
                \item{regr.tot}{regression estimate of total}
                \item{regr.ppn}{regression estimate of proportion}
                \item{strat.regr}{stratified regression estimate of mean}
                \item{strat.regr.tot}{stratified regression estimate of total}
                \item{strat.regr.ppn}{stratified regression estimate of proportion}
                \item{diffr.mean}{difference estimate of mean}
                \item{diffr.tot}{difference estimate of total}
                \item{diffr.ppn}{difference estimate of proportion}
                \item{strat.diffr}{stratified difference estimate of mean}
                \item{strat.diffr.tot}{stratified difference estimate of total}
                \item{strat.diffr.ppn}{stratified difference estimate of proportion}
                \item{clust.mean}{cluster estimate of mean from mean of cluster totals}
                \item{clust.tot}{cluster estimate of mean from total of cluster totals}
                \item{clust.ppn}{cluster estimate of proportion from mean of cluster totals}
                \item{clust.r.mean}{cluster ratio estimate of mean}
                \item{clust.r.tot}{cluster ratio estimate of total}
                \item{clust.r.ppn}{cluster ratio estimate of proportion}
              }
  }
  \item{cond}{Condition to use when calculation proportions. Character of form "<logical %%@
              operator><value>", where <value> is a valid value for the response variable (y) and <logical operator> %%@
                is one of the following:
                \itemize{
                  \item{"<"}{if you want the proportion of response values less than <value>}
                  \item{">"}{if you want the proportion of response values greater than <value>}
                  \item{"<="}{if you want the proportion of response values less than or equal to <value>}
                  \item{">="}{if you want the proportion of response values greater than or equal to <value>}
                  \item{"=="}{if you want the proportion of response values equal to <value>}
                  \item{"!="}{if you want the proportion of response values not equal to <value>}
                }
  } 
}
\details{
  Returns object identical to 'dat' but with a new element called "var.est" containing the variance %%@
    estimate(s) of the appropriate type.}
\value{
  Returns data frame identical to 'dat' but with a new column called "var.est" containing the %%@
    variance estimate(s) of the appropriate type.
}
\seealso{
  \code{\link{var.est}}, \code{\link{point.est}}, \code{\link{add.point.est}}, \code{\link{ci.est}}, %%@
    \code{\link{add.ci.est}}
}
\examples{
  data(barnett) # get barnett data
  
  # All samples total weight of class from srs:
  samp.dbn<-take.sample(barnett,y.name="weight",n=23,take.all=TRUE)
  # estimate total
  samp.dbn<-add.point.est(samp.dbn,type="ybar.tot")
  samp.dbn<-add.var.est(samp.dbn,type="ybar.tot")
  
  # proportion of class with weight more than 15, from srs:
  samp.dbn<-take.sample(barnett,y.name="weight",n=23,take.all=TRUE)
  # estimate total
  samp.dbn<-add.point.est(samp.dbn,type="ybar.ppn",cond=">15")
  samp.dbn<-add.var.est(samp.dbn,type="ybar.ppn",cond=">15")
  
  # Stratified mean
  # Define 4 strata based on weight, with 10 lb interval widths:
  strat.barnett<-define.subunit(barnett,aux.name="weight",breaks=c(0,10,20,30,40),type="strat")
  # take stratified sample
  samp.dbn<-take.sample(strat.barnett,y.name="height",type="strat",n=c(2,9,7,3),take.all=TRUE)
  # estimate mean height using stratified estimator
  samp.dbn<-add.point.est(samp.dbn,type="strat.mean")
  samp.dbn<-add.var.est(samp.dbn,type="strat.mean")
  
  # Cluster mean
  # Define 5 clusters based on weight:
  clust.barnett<-define.subunit(barnett,aux.name="weight",
                                breaks=c(0,10,15,20,25,30,40),type="clust")
  # take cluster sample of size 3
  samp.dbn<-take.sample(clust.barnett,y.name="height",type="clust",m=3,take.all=TRUE)
  # estimate mean height using cluster estimator
  samp.dbn<-add.point.est(samp.dbn,type="clust.mean")
  samp.dbn<-add.var.est(samp.dbn,type="clust.mean")
  
  # Ratio estimator
  # take srs of size 6, with weight as auxiliary variable:
  samp.dbn<-take.sample(barnett,y.name="height",aux.name="weight",type="srs",n=23,take.all=TRUE)
  # estimate mean height using cluster estimator
  samp.dbn<-add.point.est(samp.dbn,type="ratio.mean")
  samp.dbn<-add.var.est(samp.dbn,type="ratio.mean")
  
}
\keyword{}