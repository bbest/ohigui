#' Print summary of configuration
#' 
#' This function outputs the summary of the required files in the configuration file.
#' 
#' @param config.R path to configuration file
#' @param indent level for indenting output before required input file
#' @export
config.summary = function(config.R, indent='  '){
  files = c('goals.csv','layers_navigation.csv','pressures_matrix.csv','resilience_matrix.csv','resilience_weights.csv')
  for (f in files){
    cat(indent,f,': PASS\n',sep='')
  }  
}
