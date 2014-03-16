\name{launchApp}
\alias{launchApp}
\title{Launch the browser application}
\usage{
launchApp(scenario = list(conf = ohicore::conf.Global2013.www2013, layers =
  ohicore::layers.Global2013.www2013, scores =
  ohicore::scores.Global2013.www2013, spatial =
  system.file("extdata/spatial.www2013", package = "ohicore"), dir =
  path.expand("~/myohi/scenario.Global2013.www2013")), debug = F, ...)
}
\arguments{
  \item{config.R}{path to configuration file}

  \item{...}{arguments passed to \code{shiny::runApp}}
}
\description{
This function loads the specified configuration with
\code{\link{load.config}} and launches the web browser
using \code{\link{shiny::runApp}}. [TODO: specify needed
config.R format in seperate help file.]
}
\examples{
\dontrun{
# for now, must make objects global
conf   <<-
layers = layers.Global2013.www2013
scores = scores.Global2013.www2013
launchApp(conf.Global2013.www2013, layers, scores)
}
}
\keyword{app}

