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
    paste("\n\n",paste(package, version, paste("(",date, ")", sep=""), "\n\n"), 
          "Type 'demo(package='ohigui')' to see a list of demos for this package.\n\n",
          "The raw code for the demos can be found by typing:\n",
          "\t system.file('demo', package='ohigui')\n\n",
          "To access the help files type:\n",
          "\t help('ohigui-package')\n\n",
          "Note: ohigui uses the 'Shiny' package for web browser interactivity.\n"
    )
  )
}