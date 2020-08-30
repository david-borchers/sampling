\name{summary.finite.sample.dbn}
                           \alias{summary.finite.sample.dbn}
                           \title{Summarizes objects of class 'samples.population'.}
                           \description{
                           This function generates a summary of the samples in an object of class %%@
                           'samples.population'.}
                           
                           \usage{
                           summary.finite.sample.dbn(dat,type="point.est",plot=FALSE,true.stat=NULL)
                           summary(dat,type="point.est",plot=FALSE,true.stat=NULL)}
                           \arguments{
                           \item{dat}{Object of class 'samples.population.}
      \item{type}{Type of statistic to summarize. Valid types are 
                  \itemize{
                    \item{"point.est"}{for summary of point estimates in object,} 
                    \item{"var.est"}{for summary of variance estimates in object,}  
                    \item{"ci.est"}{for summary of confidence interval estimates in object.}
                  }
      }
      \item{plot}{If TRUE, a plot of the statistics is created.}
      \item{true.stat}{The value of the corresponding statistic in the population. %%@
                         Only used if type=="ci.est".}
                           }
  \details{
    Summarises point estimate, variance estimate or confidence interval %%@
      estimates of an object of class 'samples.population'. Use to investigate %%@
      properties of the sampling distribution of the point estimate, variance %%@
      estimate or confidence interval estimate. }
  \value{
    The function prints a summary
  }
  \seealso{\code{summary.sample}}
\examples{
  data(barnett) # get barnett data
  
  # All samples total weight of class from srs:
  samp.dbn<-take.sample(barnett,y.name="weight",n=23,take.all=TRUE)
  # estimate total
  samp.dbn<-add.point.est(samp.dbn,type="ybar.tot")
  samp.dbn<-add.var.est(samp.dbn,type="ybar.tot")
  samp.dbn<-add.ci.est(samp.dbn)
  summary(samp.dbn)
  summary(samp.dbn,type="var.est")
  summary(samp.dbn,type="ci.est",true.stat=sum(barnett$weight))
  
  # proportion of class with weight more than 15, from srs:
  samp.dbn<-take.sample(barnett,y.name="weight",n=23,take.all=TRUE)
  # estimate total
  samp.dbn<-add.point.est(samp.dbn,type="ybar.ppn",cond=">15")
  samp.dbn<-add.var.est(samp.dbn,type="ybar.ppn",cond=">15")
  samp.dbn<-add.ci.est(samp.dbn)
  summary(samp.dbn)
  summary(samp.dbn,type="var.est")
  summary(samp.dbn,type="ci.est",true.stat=
            (sum(barnett$weight>15)/length(barnett$weight)))
  
  # Stratified mean
  # Define 4 strata based on weight, with 10 lb interval widths:
  strat.barnett<-define.subunit(barnett,aux.name="weight",
                                breaks=c(0,10,20,30,40),type="strat")
  # take stratified sample
  samp.dbn<-take.sample(strat.barnett,y.name="height",type="strat",
                        n=c(2,9,7,3),take.all=TRUE)
  # estimate mean height using stratified estimator
  samp.dbn<-add.point.est(samp.dbn,type="strat.mean")
  samp.dbn<-add.var.est(samp.dbn,type="strat.mean")
  samp.dbn<-add.ci.est(samp.dbn)
  summary(samp.dbn)
  summary(samp.dbn,type="var.est")
  summary(samp.dbn,type="ci.est",true.stat=mean(barnett$height))
  
  # Cluster mean
  # Define 5 clusters based on weight:
  clust.barnett<-define.subunit(barnett,aux.name="weight",
                                breaks=c(0,10,15,20,25,30,40),type="clust")
  # take cluster sample of size 3
  samp.dbn<-take.sample(clust.barnett,y.name="height",type="clust",m=3,
                        take.all=TRUE)
  # estimate mean height using cluster estimator
  samp.dbn<-add.point.est(samp.dbn,type="clust.mean")
  samp.dbn<-add.var.est(samp.dbn,type="clust.mean")
  samp.dbn<-add.ci.est(samp.dbn)
  summary(samp.dbn)
  summary(samp.dbn,type="var.est")
  summary(samp.dbn,type="ci.est",true.stat=mean(barnett$height))
  
  # Ratio estimator
  # take srs of size 6, with weight as auxiliary variable:
  samp.dbn<-take.sample(barnett,y.name="height",aux.name="weight",
                        type="srs",n=23,take.all=TRUE)
  # estimate mean height using cluster estimator
  samp.dbn<-add.point.est(samp.dbn,type="ratio.mean")
  samp.dbn<-add.var.est(samp.dbn,type="ratio.mean")
  samp.dbn<-add.ci.est(samp.dbn)
  summary(samp.dbn)
  summary(samp.dbn,type="var.est")
  summary(samp.dbn,type="ci.est",true.stat=mean(barnett$height))
  
}
\keyword{}
