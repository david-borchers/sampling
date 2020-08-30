\name{summary.sample}
\alias{summary.sample}
\title{Summarizes objects of class 'sample'.}
\description{
  This function generates a summary of a sample in an object of class 'sample'.
}
\usage{
  summary.sample(dat)
  summary(dat)}
\arguments{
  \item{dat}{Object of class 'sample.}
  }
             \details{
             Summarises an object of class 'sample'. 
             }
             \value{
             The function prints a summary.
             }
             \seealso{\code{summary.finite.sample.dbn}}
             \examples{
             data(barnett) # get barnett data
             
             # total weight of class from srs:
             samp<-take.sample(barnett,y.name="weight",n=6)
             # estimate total
             summary(samp)
             
             # proportion of class with weight more than 15 from srs:
             samp<-take.sample(barnett,y.name="weight",n=6)
             # estimate total
             summary(samp)
             
             # Stratified mean
             # Define 4 strata based on weight, with 10 lb interval widths:
             strat.barnett<-define.subunit(barnett,aux.name="weight",
             breaks=c(0,10,20,30,40),type="strat")
             # take stratified sample
             samp<-take.sample(strat.barnett,y.name="height",type="strat",
             n=c(2,5,4,2))
             # estimate mean height using stratified estimator
             summary(samp)
             
             # Cluster mean
             # Define 4 clusters based on weight, with 10 lb interval widths:
             clust.barnett<-define.subunit(barnett,aux.name="weight",
             breaks=c(0,10,20,30,40),type="clust")
             # take cluster sample of size 2
             samp<-take.sample(clust.barnett,y.name="height",type="clust",m=2)
             # estimate mean height using cluster estimator
             summary(samp)
             
             # Ratio estimator
             # take srs of size 6, with weight as auxiliary variable:
             samp<-take.sample(clust.barnett,y.name="height",aux.name="weight",
             type="srs",n=6)
             # estimate mean height using cluster estimator
             summary(samp)
             }
\keyword{}

             
            