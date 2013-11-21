\name{launchApp}
\alias{launchApp}
\title{Ocean Health Index: Launch Application}
\description{Launch a graphical user interface to inspect and do basic calculations of OHI.}
\usage{
launchApp(config.R)
}
\arguments{
    \item{config.R}{Full path to the configuration file. See Details below.}
}
\value{Opens an application in your local browser using the Shiny R package to interface between the web browser and R.}
\examples{
\dontrun{
  launchApp('~/ohi/scenarios/global_2012_nature/conf/config.R')
}
}
\keyword{ohi}
