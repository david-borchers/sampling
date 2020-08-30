\name{var.est}
\alias{var.est}
\title{Calculates variance estimates from samples.}
\description{
  This function calculates variance estimates of population mean, total or %%@
    proportion from an object of class 'sample' or 'samples.population'.}

\usage{
  var.est(dat,type="ybar",cond=NULL,by.stratum=FALSE)}
\arguments{
  \item{dat}{Object of class 'sample' or 'samples.population'.}
  \item{type}{Type of variance estimate. Valid types are 
              \itemize{
                \item{"ybar":}{sample mean}
                \item{"ybar.tot":}{sample total}
                \item{"ybar.ppn":}{sample proportion}
                \item{"strat.mean":}{stratified sample mean}
                \item{"strat.tot":}{stratified sample total}
                \item{"strat.ppn":}{stratified sample proportion}
                \item{"ratio.mean":}{ratio estimate of mean}
                \item{"ratio.tot":}{ratio estimate of total}
                \item{"ratio.ppn":}{ratio estimate of proportion}
                \item{"strat.ratio":}{stratified ratio estimate of mean}
                \item{"strat.ratio.tot":}{stratified ratio estimate of total}
                \item{"strat.ratio.ppn":}{stratified ratio estimate of proportion}
                \item{"regr.mean":}{regression estimate of mean}
                \item{"regr.tot":}{regression estimate of total}
                \item{"regr.ppn":}{regression estimate of proportion}
                \item{"strat.regr":}{stratified regression estimate of mean}
                \item{"strat.regr.tot":}{stratified regression estimate of total}
                \item{"strat.regr.ppn":}{stratified regression estimate of proportion}
                \item{"diffr.mean":}{difference estimate of mean}
                \item{"diffr.tot":}{difference estimate of total}
                \item{"diffr.ppn":}{difference estimate of proportion}
                \item{"strat.diffr":}{stratified difference estimate of mean}
                \item{"strat.diffr.tot":}{stratified difference estimate of total}
                \item{"strat.diffr.ppn":}{stratified difference estimate of proportion}
                \item{"clust.mean":}{cluster estimate of mean from mean of cluster totals}
                \item{"clust.tot":}{cluster estimate of mean from total of cluster totals}
                \item{"clust.ppn":}{cluster estimate of proportion from mean of cluster %%@
                                      totals}
                \item{"clust.r.mean":}{cluster ratio estimate of mean}
                \item{"clust.r.tot":}{cluster ratio estimate of total}
                \item{"clust.r.ppn":}{cluster ratio estimate of proportion}
              }
  }
  \item{cond}{Condition to use when calculation proportions. Character of form %%@
                "<logical operator><value>", where <value> is a valid value for the response %%@
                variable (y) and <logical operator> is one of the following:
                \itemize{
                  \item{"<"}{if you want the proportion of response values less than <value>}
                  \item{">"}{if you want the proportion of response values greater than <value>}
                  \item{"<="}{if you want the proportion of response values less than or equal to %%@
                                <value>}
                  \item{">="}{if you want the proportion of response values greater than or equal %%@
                                to <value>}
                  \item{"=="}{if you want the proportion of response values equal to <value>}
                  \item{"!="}{if you want the proportion of response values not equal to <value>}
                }
  } 
  \item{by.stratum}{If TRUE, returns point estimates by stratum as well as over %%@
                      all strata. If the object is of class 'samples.population' (containing the set %%@
                                                                                    of all possible samples), the argument 'by.stratum' is set to FALSE irrespective %%@
                      of what value was passed to the function. Estimates by stratum can therefore %%@
                      only be obtained for a single sample.}
}
\details{
  Returns variance estimates of the appropriate type.}
\value{
  If 'by.stratum' is FALSE, the function returns a scalar, being the point %%@
    estimate from the sample.
  If 'by.stratum' is TRUE, the function returns a list with the following %%@
    components:
    \itemize{
      \item{"est"}{the point estimate from the sample,}
      \item{"est.by.stratum"}{a vector containing the point estimates from each %%@
                                stratum,}
      \item{"N"}{a vector containing the number of primary units in each %%@
                   stratum,}
      \item{"n"}{a vector containing the number of primary units sampled in each %%@
                   stratum.}
    }
}
\seealso{
  \code{\link{add.var.est}}, \code{\link{point.est}}, %%@
    \code{\link{add.point.est}}, \code{\link{ci.est}}, \code{\link{add.ci.est}}
}
\examples{
  data(barnett) # get barnett data
  
  # total weight of class from srs:
  samp<-take.sample(barnett,y.name="weight",n=6)
  # estimate total
  var.est(samp,type="ybar.tot")
  
  # proportion of class with weight more than 15, from srs:
  samp<-take.sample(barnett,y.name="weight",n=6)
  # estimate total
  var.est(samp,type="ybar.ppn",cond=">15")
  
  # Stratified mean
  # Define 4 strata based on weight, with 10 lb interval widths:
  %%@
    strat.barnett<-define.subunit(barnett,aux.name="weight",breaks=c(0,10,20,30,40),%%@
                                    type="strat")
  # take stratified sample
  samp<-take.sample(strat.barnett,y.name="height",type="strat",n=c(2,5,4,2))
  # estimate mean height using stratified estimator
  var.est(samp,type="strat.mean")
  # now estimate by stratum too:
  var.est(samp,type="strat.mean",by.stratum=TRUE)
  
  # Cluster mean
  # Define 4 clusters based on weight, with 10 lb interval widths:
  %%@
    clust.barnett<-define.subunit(barnett,aux.name="weight",breaks=c(0,10,20,30,40),%%@
                                    type="clust")
  # take cluster sample of size 2
  samp<-take.sample(clust.barnett,y.name="height",type="clust",m=2)
  # estimate mean height using cluster estimator
  var.est(samp,type="clust.mean")
  
  # Ratio estimator
  # take srs of size 6, with weight as auxiliary variable:
  %%@
    samp<-take.sample(clust.barnett,y.name="height",aux.name="weight",type="srs",n=6%%@
    )
  # estimate mean height using cluster estimator
  var.est(samp,type="ratio.mean")
  
  
}
\keyword{}
