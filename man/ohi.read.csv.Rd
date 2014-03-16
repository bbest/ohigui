\name{ohi.read.csv}
\alias{ohi.load}
\alias{ohi.loadbin}
\alias{ohi.read.csv}
\alias{ohi.save}
\alias{ohi.save.pressures}
\alias{ohi.save.resilience}
\alias{ohi.save.results}
\alias{ohi.save.status}
\alias{ohi.save.trend}
\alias{ohi.savebin}
\alias{ohi.savetxt}
\alias{ohi.write.csv}
\title{Ocean Health Index: Data file format}
\usage{
ohi.read.csv(file, na.strings = "", row.names = NULL, ...)
}
\arguments{
  \item{file}{Full path to input/output file.}

  \item{x}{A data frame with data to write.}

  \item{digits}{Use to restrict ASCII representation of
  doubles.}

  \item{row.names}{Do not use them by default}

  \item{na.strings}{Use blanks for NA.}

  \item{na}{Use blanks for NA.}

  \item{dir}{The directory to use.}

  \item{method}{The data file format method.}

  \item{envir}{Environment in which to assign name}

  \item{name}{The variable to which data will be assigned,
  and used for the filename -- e.g., name = 'regions' will
  look for 'regions.csv', etc.}

  \item{xdim}{The name of a dimension, and expects
  get(dimension) to return a valid data frame with 2
  columns: region_code, and dimension -- e.g.,
  ('region_code', 'status').}

  \item{...}{Arguments passed onto read.csv, write.csv,
  load, save.}
}
\value{
Returns a data.frame with the input data.
}
\description{
Simple read/write utility functions for the CSV and RData
data file format.
}
\examples{
\dontrun{
  d <- ohi.read.csv('data/regions.csv')
  names(d)
  head(d)

  d$label <- toupper(d$label)
  ohi.write.csv(d, 'data/regions.veryloud.csv')

  status <- data.frame(region_code=d$region_code, status=rnorm(nrow(d)))
  ohi.save.status()
}
}
\seealso{
\code{\link{read.csv}}, \code{\link{write.csv}}
}
\keyword{ohi}

