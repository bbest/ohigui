##' Assemble Layers
##' 
##' Check all the input layers as defined by layers_navigation.csv, found in
##' dir.layers and assembled out to a consolidated layers_data.csv for easy
##' data extraction.
##' 
##' 
##' @aliases check.layers_navigation assemble.layers_data
##' @param layers_navigation.csv Full path to the layers_navigation.csv file.
##' @param dir.layers Full path to the directory containing the layers files.
##' @param layers_data.csv Combined data table output of all layers dir.layers
##' based on descriptions in layers_navigation.
##' @param layers_id_fields Character vector of unique identifiers typically
##' spatial (eg region_id, country_id, saup_id).
##' @return All of these parameters should be defined in config.R.
##' 
##' The check.layers_navigation() function iterates through the
##' layers_navigation.csv and checks for the existence of all the input files
##' and looks for matching fields. Any unused fields should be dealt with
##' before moving onto using assemble.layers_data.
##' 
##' The assemble.layers_data() function reads in all data layers and combines
##' them into a single data table layers_data.csv.
##' @keywords layers_navigation
##' @examples
##' 
##' \dontrun{
##'   config.R = '~/ohi/scenarios/global_2012_nature/conf/config.R'
##'   source(config.R)
##'   check.layers_navigation(layers_navigation.csv, dir.layers, layers_id_fields)
##'   assemble.layers_data(layers_navigation.csv, dir.layers, layers_data.csv, layers_id_fields)
##' }
assemble.layers_data = function(layers_navigation.csv, layers_data.csv, layers_id_fields, verbose=T, msg.indent='  '){
  #library(plyr)
  
  # some checks
  stopifnot(file.exists(layers_navigation.csv))
  stopifnot(file.exists(dirname(layers_navigation.csv)))
  stopifnot(is.vector(layers_id_fields))
  
  # initialize
  ln = read.csv(layers_navigation.csv, na.strings='')
  ld = data.frame(layer     = character(),
                  id_num    = numeric(),
                  id_chr    = character(),
                  category  = character(),
                  year      = numeric(),
                  value_num = numeric(),
                  value_chr = character())
  flds_ld  = names(ld)
  flds_std = names(ld)[-1] # except layer
  
  # iterate layers
  for (i in 1:nrow(ln)){ # i=1
    
    # identify layer and read in file
    if (verbose) cat(sprintf('%s%s\n', msg.indent, ln$layer[i],'\n'))
    path = file.path(ln$directory[i], ln$filename[i])
    d = read.csv(path)
    names(d) = tolower(names(d))
    
    # rename to standardized field names
    flds = ln[i, flds_std]
    flds = flds[, !is.na(flds), drop=F]
    d = plyr::rename(d, setNames(names(flds), flds))
    d$layer = ln$layer[i]
    
    # bind to master
    flds_miss = setdiff(flds_ld, names(d))
    dm = setNames(as.data.frame(matrix(rep(NA, nrow(d)*length(flds_miss)), ncol=length(flds_miss))), flds_miss)
    #ld = tryCatch(rbind(ld, cbind(d, dm)), error=browser())
    ld = rbind(ld, cbind(d, dm))
  }
  
  # write out
  ld = ld[,flds_ld]
  write.csv(ld, layers_data.csv, row.names=F, na='')      
}