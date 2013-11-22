\name{launchApp}
\alias{launchApp}
\title{Launch the browser application}
\usage{
  launchApp(config.R, ...)
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
launchApp('/usr/local/ohi/src/toolbox/scenarios/global_2012_nature/conf/config.R')
}
\keyword{app}

