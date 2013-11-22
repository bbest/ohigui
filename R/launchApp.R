#' Launch the browser application
#'
#' This function loads the specified configuration with \code{\link{load.config}} 
#' and launches the web browser using \code{\link{shiny::runApp}}.
#' [TODO: specify needed config.R format in seperate help file.]
#'
#' @param config.R path to configuration file
#' @param ... arguments passed to \code{shiny::runApp}
#' @keywords app
#' @export
#' @examples
#' launchApp('/usr/local/ohi/src/toolbox/scenarios/global_2012_nature/conf/config.R')
launchApp = function(config.R, ...){  
  load.config(config.R)
  shiny::runApp(system.file('shiny_app', package='ohigui'), ...)
}