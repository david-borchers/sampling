 \name{plot.finite.sample.dbn}
                   \alias{plot.finite.sample.dbn}
                   \title{Generates plot for object of class 'samples.population'.}
                   \description{
                   This function generates a plot of the samples in an object of class 'samples.population'.}
                   
                   \usage{
                   plot.finite.sample.dbn(dat,type="point.est",show.mean=TRUE,freq=TRUE,true.stat=NULL, ...)
                   plot(dat,type="point.est",show.mean=TRUE,freq=TRUE,true.stat=NULL, ...)}
                   \arguments{
                   \item{dat}{Object of class 'samples.population'.}
                   \item{type}{Type of statistic to plot. Valid types are 
                   \itemize{
                   \item{"point.est"}{for plot of point estimates in object,} 
                   \item{"var.est"}{for plot of variance estimates in object,}  
                   \item{"ci.est"}{for plot of confidence interval estimates in object.}
                   }
                   }
                   \item{freq}{If TRUE, the histogram shows frequencies, else it shows density.}
                   \item{show.mean}{If TRUE and type=="point.est" or type=="var.est", the mean of the sampling %%@
                   distribution is shown on the histogram.}
                   \item{true.stat}{The value of the corresponding statistic in the population. Only used if %%@
                   type=="ci.est".}
                   \item{...}{Other arguments to be passed to the function 'hist'.}
                   }
                   \details{
                   Plots point estimate, variance estimate or confidence interval estimates of an object of class %%@
                   'samples.population'. Use to investigate properties of the sampling distribution of the point %%@
                   estimate, variance estimate or confidence interval estimate. }
                   \value{
                   The function creates a plot.
                   }
                   \seealso{\code{plot.sample}}
                   \examples{
                   data(barnett) # get barnett data
                   
                   # All samples total weight of class from srs:
                   samp.dbn<-take.sample(barnett,y.name="weight",n=23,take.all=TRUE)
                   # estimate total
                   samp.dbn<-add.point.est(samp.dbn,type="ybar.tot")
                   samp.dbn<-add.var.est(samp.dbn,type="ybar.tot")
                   samp.dbn<-add.ci.est(samp.dbn)
                   plot(samp.dbn)
                   plot(samp.dbn,type="var.est")
                   plot(samp.dbn,type="ci.est",true.stat=sum(barnett$weight))
                   
                   # proportion of class with weight more than 15, from srs:
                   samp.dbn<-take.sample(barnett,y.name="weight",n=23,take.all=TRUE)
                   # estimate total
                   samp.dbn<-add.point.est(samp.dbn,type="ybar.ppn",cond=">15")
                   samp.dbn<-add.var.est(samp.dbn,type="ybar.ppn",cond=">15")
                   samp.dbn<-add.ci.est(samp.dbn)
                   plot(samp.dbn)
                   plot(samp.dbn,type="var.est")
                   plot(samp.dbn,type="ci.est",true.stat=
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
                   plot(samp.dbn)
                   plot(samp.dbn,type="var.est")
                   plot(samp.dbn,type="ci.est",true.stat=mean(barnett$height))
                   
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
                   plot(samp.dbn)
                   plot(samp.dbn,type="var.est")
                   plot(samp.dbn,type="ci.est",true.stat=mean(barnett$height))
                   
                   # Ratio estimator
                   # take srs of size 6, with weight as auxiliary variable:
                   samp.dbn<-take.sample(barnett,y.name="height",aux.name="weight",type="srs",
                   n=23,take.all=TRUE)
                   # estimate mean height using cluster estimator
                   samp.dbn<-add.point.est(samp.dbn,type="ratio.mean")
                   samp.dbn<-add.var.est(samp.dbn,type="ratio.mean")
                   samp.dbn<-add.ci.est(samp.dbn)
                   plot(samp.dbn)
                   plot(samp.dbn,type="var.est")
                   plot(samp.dbn,type="ci.est",true.stat=mean(barnett$height))
                   
                   }
                   \keyword{}
                   