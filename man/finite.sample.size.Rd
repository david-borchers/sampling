 \name{finite.sample.size}
                   \alias{finite.sample.size}
                   \title{Calculates the number of samples of given size that can be taken without replacement.}
                   \description{
                   This function calculates the number of samples of size n than can be taken from a population of size N %%@
                   without replacement. Does this for simple random, stratified or cluster sampling..}
                   
                   \usage{
                   finite.sample.size(dat,n=NULL,m=NULL)}
                   \arguments{
                   \item{dat}{Data frame containing the population to be sampled (one element per row).}
                   \item{n}{Sample size. If a vector, it must he the same length as the number of strata (levels of the %%@
                   column called "subunit" in dat) and its elements are the sample sizes in each stratum. Only one of 'n' %%@
                   and 'm' should be specified.}
                   \item{m}{Number of clusters to be sampled in cluster sampling. Only one of 'n' and 'm' should be %%@
                   specified.}
                   }
                   \details{
                   Calculates the number of possible samples that can be taken. In the case of stratified sampling, this %%@
                   is the product of the number that can be taken in each stratum.}
                   \value{
                   The function prints the number of possible samples. }
                   \seealso{
                   \code{\link{define.subunit}}
                   }
                   \examples{
                   data(st03) # get 2003 Sampling Theory class data
                   data(worms) # get worms data
                   
                   # srs:
                   finite.sample.size(st03,4)
                   finite.sample.size(worms,4)
                   
                   # Stratified random samples:
                   # Define strata based on year of study:
                   strat.st03<-define.subunit(st03,aux.name="yr.study",type="strat")
                   finite.sample.size(strat.st03,n=c(3,3,1))
                   
                   # Cluster samples:
                   # Define strata based on year of study:
                   clust.st03<-define.subunit(st03,aux.name="yr.study",type="clust")
                   finite.sample.size(clust.st03,m=2)
                   
                   }
 \keyword{}
                   
                  