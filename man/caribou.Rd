\name{caribou}
\alias{caribou}
\title{Animal counts from aerial survey of caribou}
\description{
  A data frame with each row corresponding to a transect line flown by the survey plane. 
  The observers in the plane search out to a distance of half a mile either side of the plane.
  The survey was stratified in to two strata, and stips 1 mile wide chosen by simple random 
  sampling separately in each stratum.
  Columns are as follows:
    \itemize{
      \item{"transect"}{Transect number (1 to 15)}
      \item{"length"}{Length, in miles, of the transect line}
      \item{"stratum"}{The survey stratum number}
      \item{``count''}{The number of caribou counted from the transect line}
      \item{``area''}{The surface area of the stratum. Note that this number is repeated for every row of data in the stratum.}
      \item{``N''}{The number of strips in each stratum. Note that this number is repeated for every row of data in the stratum.}
      }
    }
\usage{data(caribou)}
\format{A data frame}
\source{Valkenburg and Harbo (1979)}
\references{Valkenburg, P. and Harbo, S. 1979. Refinement of the aerial photo–direct count–extrapolation caribou
census technique. Final research report W–17–11. Alaska department of fish and game wildlife conservation division,
Fairbanks, Alaska.}
\keyword{datasets}
                   
                   
                   
                   