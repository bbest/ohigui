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
#' @export
launchApp = function(scenario = list(conf   = ohicore::conf.Global2013.www2013, 
                                     layers = ohicore::layers.Global2013.www2013, 
                                     scores = ohicore::scores.Global2013.www2013,
                                     #shapes = system.file('extdata/shapes.www2013', package='ohicore'),
                                     shapes = system.file('inst/extdata/shapes.www2013', package='ohicore'),
                                     dir    = path.expand('~/myohi/scenario.Global2013.www2013')), ...){
  
# load_all('~/Code/ohicore'); load_all('~/Code/ohigui')
  
  # HACK: make objects global in scope
  conf         <<- scenario$conf
  layers       <<- scenario$layers
  scores       <<- scenario$scores
  dir_shapes   <<- scenario$shapes
  dir_scenario <<- scenario$dir 
  
  #shiny::runApp(appDir=system.file('shiny_app', package='ohigui'), ...)
  #shiny::runApp(appDir=system.file('inst/shiny_app', package='ohigui'))
  shiny::runApp(appDir=system.file('inst/shiny_app', package='ohigui'), ...)
}