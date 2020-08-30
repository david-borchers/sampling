\name{plot.sample}
                   \alias{plot.sample}
                   \title{Plots an object of class 'sample'.}
                   \description{
                   This function generates a plot of a sample in an object of class 'sample'.}
                   
                   \usage{
                   plot.sample(dat)
                   plot(dat)}
                   \arguments{
                   \item{dat}{Object of class 'sample.}
                   }
  \details{
    Generates a plot for an object of class 'sample'. }
  \value{
    The function plots a sample.
  }
  \seealso{\code{plot.finite.sample.dbn}}
\examples{
  data(barnett) # get barnett data
  
  # total weight of class from srs:
  samp<-take.sample(barnett,y.name="weight",n=6)
  # estimate total
  plot(samp)
  
  # proportion of class with weight more than 15 from srs:
  samp<-take.sample(barnett,y.name="weight",n=6)
  # estimate total
  plot(samp)
  
  # Stratified mean
  # Define 4 strata based on weight, with 10 lb interval widths:
  strat.barnett<-define.subunit(barnett,aux.name="weight",
                                breaks=c(0,10,20,30,40),type="strat")
  # take stratified sample
  samp<-take.sample(strat.barnett,y.name="height",type="strat",
                    n=c(2,5,4,2))
  # estimate mean height using stratified estimator
  plot(samp)
  
  # Cluster mean
  # Define 4 clusters based on weight, with 10 lb interval widths:
  clust.barnett<-define.subunit(barnett,aux.name="weight",
                                breaks=c(0,10,20,30,40),type="clust")
  # take cluster sample of size 3
  samp<-take.sample(clust.barnett,y.name="height",type="clust",m=3)
  # estimate mean height using cluster estimator
  plot(samp)
  
  # Ratio estimator
  # take srs of size 6, with weight as auxiliary variable:
  samp<-take.sample(clust.barnett,y.name="height",aux.name="weight",
                    type="srs",n=6)
  # estimate mean height using cluster estimator
  plot(samp)
}
\keyword{}
