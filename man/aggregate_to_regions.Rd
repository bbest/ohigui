\name{aggregate_to_region}
\alias{aggregate_by_country}
\alias{aggregate_by_country_year}
\alias{aggregate_weighted}
\title{Check and Assemble Layers}
\description{Check all the input layers as defined by layers_navigation.csv, found in dir.layers and assembled out to a consolidated layers_data.csv for easy data extraction.}
\usage{
  aggregate_by_country(df, col.value='value', col.country='country_id')
  aggregate_weighted(df, w, col.value='value', col.country='country_id', col.weight='weight')
  aggregate_by_country_year(df, col.value='value', col.country='country_id')
}
\arguments{
  \item{df}{Input data frame.}
  \item{col.value}{Column in data frame containing the value to be aggregated.}
  \item{col.country}{Column in data frame containing the country_id.}
  \item{col.weight}{Column in data frame containing the weight.}
}
\value{These functions aggregate to region by either country, country and year, or just a weight. These are mostly only used for aggregating a goal's status or trend calculations to region.}
\examples{
  \dontrun{
    aggregate_by_country(df, col.value='value', col.country='country_id')
    aggregate_weighted(df, w, col.value='value', col.country='country_id', col.weight='weight')
    aggregate_by_country_year(df, col.value='value', col.country='country_id')
  }
}
\keyword{layers_navigation}
