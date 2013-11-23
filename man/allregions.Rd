\name{allregions}
\alias{allregions}
\alias{ohi.global.regions.all}
\alias{ohi.global.regions.eez}
\alias{ohi.global.regions.highseas}
\title{Ocean Health Index: Global regions}
\usage{
  allregions(d = NULL, scope = "all")
}
\arguments{
  \item{d}{The data frame to join with. If NULL, returns
  all regions.}

  \item{scope}{'all' for all global regions, or 'eez' for
  EEZ/country regions.}
}
\value{
  Returns a data.frame containing a left join of regions.
}
\description{
  Functions to join with global regions
}
\examples{
d <- data.frame(id=1:50, status=runif(50, 0, 1))
allregions(d) # returns status=NA for all but the first 50 regions
ohi.load('regions_details')
}
\seealso{
  merge
}
\keyword{ohi}

