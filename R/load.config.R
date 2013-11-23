#' Load the configuration file
#'
#' This function loads the specified configuration file into the global namespace. 
#' [TODO: specify needed config.R format in seperate help file.]
#'
#' @param config.R path to configuration file
#' @keywords app
#' @export
#' @examples
#' \dontrun{
#' load.config('/usr/local/ohi/src/toolbox/scenarios/global_2012_nature/conf/config.R')
#' }
load.config = function(config.R){
  
  # read configuration
  if (!file.exists(config.R)) stop(cat('The file config.R does not exist here: ',config.R))
  config.R <<- config.R # make global
  source(config.R)  
  
  # check for existence of files and folders set in config.R
  dirs  = c('dir.data','dir.conf','dir.scenario','dir.scenarios','dir.results')
  files = c('functions.R','goals.csv','layers_navigation.csv','pressures_matrix.csv','resilience_matrix.csv','resilience_weights.csv')
  for (v in c(dirs,files)){
    if (!exists(v)) stop(sprintf('Path variable %s not set in config.R (%s)',v,config.R))
    p = get(v)
    if (is.na(file.info(p)$isdir)) stop(sprintf('Path %s=%s does not exist on filesystem as set in config.R (%s)',v,p,config.R))
  }
  
  # TODO: check for existence of required variables like resilience_components, pressures_components 
}
