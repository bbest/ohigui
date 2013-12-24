#' Launch the browser application
#'
#' This function loads the specified configuration with \code{\link{load.config}} 
#' and launches the web browser using \code{\link{shiny::runApp}}.
#' [TODO: specify needed config.R format in seperate help file.]
#'
#' @param config.R path to configuration file
#' @param ... arguments passed to \code{shiny::runApp}
#' @keywords app
#' @examples
#' \dontrun{
#' # for now, must make objects global
#' conf   <<- 
#' layers = layers.Global2013.www2013
#' scores = scores.Global2013.www2013
#' launchApp(conf.Global2013.www2013, layers, scores)
#' }
#' @import ohicore
#' @export
launchApp = function(scenario = list(conf   = ohicore::conf.Global2013.www2013, 
                                     layers = ohicore::layers.Global2013.www2013, 
                                     scores = ohicore::scores.Global2013.www2013,
                                     spatial = system.file('extdata/spatial.www2013', package='ohicore'),
                                     #spatial = system.file('inst/extdata/spatial.www2013', package='ohicore'),
                                     dir    = path.expand('~/myohi/scenario.Global2013.www2013')), debug=F, ...){
  
# load_all('~/Code/ohicore'); load_all('~/Code/ohigui')
# if (!require(devtools)) install.packages('devtools'); require(devtools); install_github('rCharts','bbest'); install_github('ohicore','bbest'); install_github('ohigui','bbest')
# load_all('~/Code/rCharts'); load_all('~/Code/ohicore'); setwd('~/Code/ohigui'); load_all(); ohigui::launchApp()
# unloadNamespace('ohigui'); load_all('~/Code/ohicore'); load_all('~/Code/ohigui'); launchApp()
  
  # HACK: make objects global in scope
  conf         <<- scenario$conf
  layers       <<- scenario$layers
  scores       <<- scenario$scores
  dir_spatial   <<- scenario$spatial
  dir_scenario <<- scenario$dir 
  
  dir_app = system.file('shiny_app', package='ohigui')

  # HACK: update paths for devtools load_all() mode
  if (!file.exists(dir_app   )) dir_app    =   system.file('inst/shiny_app'             , package='ohigui')
  if (!file.exists(dir_spatial)) dir_spatial <<- system.file('inst/extdata/spatial.www2013', package='ohicore')
  
  if (debug) options(shiny.trace=TRUE)
  
  shiny::runApp(appDir=dir_app, ...)
}
