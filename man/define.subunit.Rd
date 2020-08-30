 \name{define.subunit}
                   \alias{define.subunit}
                   \title{Defines clusters or strata in a population.}
                   \description{
                   This function adds a column specifying subunits (strata or clusters) to a population data frame.}
                   
                   \usage{
                   define.subunit(dat,aux.name,breaks=NULL,type="stratum")}
                   \arguments{
                   \item{dat}{Data frame containing the population. Must be a column name of 'dat'}
                   \item{aux.name}{Name of the auxiliary variable that is to be used to define the subunits.}
                   \item{breaks}{A vector containing the cutpoints to be used to allocate auxiliary variable values to %%@
                   subunits. For example if the auxiliary variable takes on values between 1.1 and 22.5 say, and you %%@
                   want to define two strata, one containing primary units with auxiliary variable values between 1 and %%@
                   10 (inclusve), the other containing primary units with auxiliary variable values greater than 10, %%@
                   you could specify breaks=c(0,10,23). The numbers in breaks must be increasing; the first must be %%@
                   lower than the smallest auxiliary variable value and the last greater than or equal to the largest %%@
                   auxiliary variable value. Auxiliary variable values falling on a break point go into the interval to %%@
                   its left (except that values equal to the smallest break point go into the first interval).} 
                   \item{type}{Type of subunit. Valid types are 
                   \itemize{
                   \item{"stratum"}{to define strata,} 
                   \item{"cluster"}{do define clusters.}
                   }
                   }
                   }
                   \details{
                   A new column called "subunit" is added to the population data frame. It contains the subunit %%@
                   number into which the primary unit of that row falls. It is used when taking stratified or cluster %%@
                   samples, and must therefore have been added before such samples are taken.}
                   \value{
                   The function returns a data frame identical to 'dat', except that a column named "subunit" has %%@
                   been added. }
                   \seealso{
                   \code{\link{take.sample}}
                   }
                   \examples{
                   data(barnett) # get barnett data
                   
                   # Define 4 strata based on height, with 10 lb interval widths:
                   strat.barnett<-define.subunit(barnett,aux.name="weight",breaks=c(0,10,20,30,40),type="strat")
                   
                   # Define 3 clusters based on weight, with 15 lb interval widths:
                   clust.barnett<-define.subunit(barnett,aux.name="weight",breaks=c(0,15,30,45),type="clust")
                   
                   }
\keyword{}





































                   