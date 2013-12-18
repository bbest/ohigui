##' Ocean Health Index - Graphical User Interface
##' 
##' The ohigui is an interface through the web browser for exploring and calculating the Ocean Health Index.
##' 
##' @name ohigui-package
##' @aliases ohigui
##' @docType package
##' @author Ben Best \email{bbest@@nceas.ucsb.edu}
NULL

.onAttach <- function(library, pkgname)
{
  info <-utils::packageDescription(pkgname)
  package <- info$Package
  version <- info$Version
  date <- info$Date
  packageStartupMessage(
    sprintf(
      paste0(
        "%s %s (%s)\n", 
        "Type 'launchApp()' to see the default application launch in a web browser.\n",
        "To stop the application from running, use Ctrl+C or Esc.\n"), package, version, date))
}