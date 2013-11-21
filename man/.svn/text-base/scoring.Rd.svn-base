\name{scoring}
\alias{score.rescale}
\alias{score.max}
\alias{score.clamp}
\title{Ocean Health Index: Scoring}
\description{Scoring functions}
\usage{
score.rescale(x, xlim=NULL, method='linear', ...)
score.max(x, p=0.0, ...)
score.clamp(x, xlim=c(0,1), ...)
}
\arguments{
  \item{x}{A numeric vector of data.}
  \item{xlim}{The scoring range. If null, derives range from data.}
  \item{p}{A percentage buffer to add to the maximum value.}
  \item{method}{Only 'linear' is supported.}
  \item{...}{Arguments for min, max, pmin, pmax.}
}
\value{Returns scores.}
\seealso{min, max, pmin, pmax}
\examples{
score.max(c(0.5, 1, 2))
score.max(c(0.5, 1, 2), p=0.25)
score.rescale(c(0.5, 1, 2))
score.clamp(c(-0.5, 1, 2))
score.clamp(c(-0.5, 1, 2), xlim=c(-1, 1))
}
\keyword{ohi}


