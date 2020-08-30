\name{ci.est}
                   \alias{ci.est}
                   \title{Calculates confidence interval estimates from samples.}
                   \description{
                   This function calculates confidence interval estimates of population mean, total or proportion from an %%@
                   object of class 'sample' or 'samples.population'.}                   
                   \usage{
                   ci.est(dat,type="ybar",level=0.95)}
                   \arguments{
                   \item{dat}{Object of class 'sample' or 'samples.population'.}
                   \item{type}{Type of confidence interval estimate. Valid types are 
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
                   \item{strat.ratio.mean}{stratified ratio estimate of mean}
                   \item{strat.ratio.tot}{stratified ratio estimate of total}
                   \item{strat.ratio.ppn}{stratified ratio estimate of proportion}
                   \item{regr.mean}{regression estimate of mean}
                   \item{regr.tot}{regression estimate of total}
                   \item{regr.ppn}{regression estimate of proportion}
                   \item{strat.regr.mean}{stratified regression estimate of mean}
                   \item{strat.regr.tot}{stratified regression estimate of total}
                   \item{strat.regr.ppn}{stratified regression estimate of proportion}
                   \item{diffr.mean}{difference estimate of mean}
                   \item{diffr.tot}{difference estimate of total}
                   \item{diffr.ppn}{difference estimate of proportion}
                   \item{strat.diffr.mean}{stratified difference estimate of mean}
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
                   \item{level}{Confidence level; 0.95 (95\%) by default.}
                   }
                    \details{
                   Returns an approximate confidence interval estimate of the appropriate type, assuming that the %%@
                   statistic has a t-distribution with degrees of freedom equal to sample size.}
                   \value{
                   The function returns a list with elements $ci.lower and $ci.upper, being the lower and upper %%@
                   (100*level) confidence interval bounds from the sample.}
                   \seealso{
                   \code{\link{add.ci.est}}, \code{\link{var.est}}, \code{\link{add.var.est}}, %%@
                   \code{\link{point.est}}, \code{\link{add.point.est}}
                   }
                   \examples{
                   data(barnett) # get barnett data
                   
                   # total weight of class from srs:
                   samp<-take.sample(barnett,y.name="weight",n=6)
                   # estimate total
                   ci.est(samp,type="ybar.tot")
                   
                   # proportion of class with weight more than 15 from srs:
                   samp<-take.sample(barnett,y.name="weight",n=6)
                   # estimate total
                   ci.est(samp,type="ybar.ppn",cond=">15")
                   
                   # Stratified mean
                   # Define 4 strata based on weight, with 10 lb interval widths:
                   strat.barnett<-define.subunit(barnett,aux.name="weight",breaks=c(0,10,20,30,40),type="strat")
                   # take stratified sample
                   samp<-take.sample(strat.barnett,y.name="height",type="strat",n=c(2,5,4,2))
                   # estimate mean height using stratified estimator
                   ci.est(samp,type="strat.mean")
                   
                   # Cluster mean
                   # Define 4 clusters based on weight, with 10 lb interval widths:
                   clust.barnett<-define.subunit(barnett,aux.name="weight",breaks=c(0,10,20,30,40),type="clust")
                   # take cluster sample of size 2
                   samp<-take.sample(clust.barnett,y.name="height",type="clust",m=2)
                   # estimate mean height using cluster estimator
                   ci.est(samp,type="clust.mean")
                   
                   # Ratio estimator
                   # take srs of size 6, with weight as auxiliary variable:
                   samp<-take.sample(clust.barnett,y.name="height",aux.name="weight",type="srs",n=6)
                   # estimate mean height using cluster estimator
                   ci.est(samp,type="ratio.mean")
                   
                   
                   }
\keyword{}
                   
