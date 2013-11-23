\name{assemble.layers_data}
\alias{assemble.layers_data}
\alias{check.layers_navigation}
\title{Assemble Layers}
\usage{
  assemble.layers_data(layers_navigation.csv,
    layers_data.csv, layers_id_fields, verbose = T,
    msg.indent = "  ")
}
\arguments{
  \item{layers_navigation.csv}{Full path to the
  layers_navigation.csv file.}

  \item{dir.layers}{Full path to the directory containing
  the layers files.}

  \item{layers_data.csv}{Combined data table output of all
  layers dir.layers based on descriptions in
  layers_navigation.}

  \item{layers_id_fields}{Character vector of unique
  identifiers typically spatial (eg region_id, country_id,
  saup_id).}
}
\value{
  All of these parameters should be defined in config.R.

  The check.layers_navigation() function iterates through
  the layers_navigation.csv and checks for the existence of
  all the input files and looks for matching fields. Any
  unused fields should be dealt with before moving onto
  using assemble.layers_data.

  The assemble.layers_data() function reads in all data
  layers and combines them into a single data table
  layers_data.csv.
}
\description{
  Check all the input layers as defined by
  layers_navigation.csv, found in dir.layers and assembled
  out to a consolidated layers_data.csv for easy data
  extraction.
}
\examples{
\dontrun{
  config.R = '~/ohi/scenarios/global_2012_nature/conf/config.R'
  source(config.R)
  check.layers_navigation(layers_navigation.csv, dir.layers, layers_id_fields)
  assemble.layers_data(layers_navigation.csv, dir.layers, layers_data.csv, layers_id_fields)
}
}
\keyword{layers_navigation}

