##' Check Layers
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
check.layers_navigation = function(layers_navigation.csv, layers_id_fields, verbose=T, msg.indent='  '){
  # for each layer listed in layers_navigation.csv, check for file in dir.layers,
  # and update layers_navigation.csv with information about the file's existence and identified fields for assembling into layers_data.csv
  
  # read in
  ln = read.csv(layers_navigation.csv)
  
  # initialize fields to populate
  ln$file_exists  = F
  ln$id_num       = NA
  ln$id_chr       = NA
  ln$category     = NA
  ln$year         = NA
  ln$value_num    = NA
  ln$value_chr    = NA
  ln$flds_unused  = NA
  ln$flds_missing = NA
  ln$rows_duplicated = NA
  ln$val_min      = NA
  ln$val_max      = NA
  ln$val_0to1     = NA
  ln$unique_ids   = NA
  
  for (i in 1:nrow(ln)){ # i=1
    
    # identify layer and read in file
    if (verbose) cat(sprintf('%s%s\n', msg.indent, ln$layer[i],'\n'))
    path = file.path(ln$directory[i], ln$filename[i])
    if (!file.exists(path)) {
      next
    }
    ln$file_exists[i] = T
    d = read.csv(path)
    names(d) = tolower(names(d))
    
    # get field types
    fld_types = sapply(as.list(d), class)
    
    # id field
    idx.ids = which(names(d) %in% layers_id_fields)
    if (length(idx.ids)>0){
      # if more than one id field, then presume lookup table and get the id field entirely unique rows
      if (length(idx.ids)>1){
        fld_id = names(d)[lapply(as.list(d[,idx.ids]), anyDuplicated)==0]
      } else {
        fld_id = names(d)[idx.ids]
      }
      
      # assign id field based on type
      if (fld_types[fld_id]=='character'){
        ln$id_chr[i] = fld_id
      } else {
        ln$id_num[i] = fld_id
      }
      
      # assign metadata check
      ln$unique_ids   = length(unique(d[[fld_id]]))
    }
    
    # units field
    fld_units = tolower(chartr('/ ','..', ln$units[i])) # translate slash or space to a single dot
    if (!fld_units %in% names(d)){
      ln$flds_missing[i] = paste(ln$flds_missing[i], fld_units)
    } else {
      if (fld_types[fld_units]=='character'){
        ln$value_chr[i] = fld_units
      } else {
        ln$value_num[i] = fld_units
        
        # add metadata checks
        ln$val_min[i]  = min(d[[fld_units]], na.rm=T)
        ln$val_max[i]  = max(d[[fld_units]], na.rm=T)
        ln$val_0to1[i] = ln$val_min[i] >=0 & ln$val_max[i]<=1
      }
    }
    
    # year
    if ('year' %in% names(d)) ln$year[i] = 'year'
    
    # get other fields not assigned
    flds_assigned = as.vector(na.omit(t(ln[i,
                                           c('id_num','id_chr','category','year','value_num','value_chr')])))
    flds_other = setdiff(names(d), flds_assigned)
    
    # category - presume last remaining unidentified field
    if (length(flds_other>0)) ln$category[i] = flds_other[1]
    
    # check for any duplicated rows in layer, which forces the dcast to use length as the input value (BIG PROBLEM)
    n.rows.duplicated = sum(duplicated(d[,na.omit(unlist(ln[i, c('id_num','id_chr','category','year')]))]))
    if (n.rows.duplicated>0) ln$rows_duplicated[i] = n.rows.duplicated
    
    # still unassigned?    
    if (length(flds_other)>1){
      ln$flds_unused[i] = paste(flds_other[-1], collapse=',')
    }
  }
  files.missing = subset(ln, file_exists==F)
  if (nrow(files.missing)>0) warning(paste(c('Missing files...', sprintf('    %s: %s/%s', files.missing$layer, files.missing$directory, files.missing$filename)), collapse='\n'))
  flds.missing = subset(ln, file_exists==T & !is.na(flds_missing))
  if (nrow(flds.missing)>0) warning(paste(c('Missing fields...', sprintf('    %s: %s', flds.missing$layer, flds.missing$flds_missing)), collapse='\n'))
  flds.unused = subset(ln, file_exists==T & !is.na(flds_unused))
  if (nrow(flds.unused)>0) warning(paste(c('Unused fields...', sprintf('    %s: %s', flds.unused$layer, flds.unused$flds_unused)), collapse='\n'))  
  rows.duplicated = subset(ln, file_exists==T & !is.na(rows_duplicated))
  if (nrow(rows.duplicated)>0) warning(paste(c('Rows duplicated...', sprintf('    %s: %s', rows.duplicated$layer, rows.duplicated$rows_duplicated)), collapse='\n'))
  write.csv(ln, layers_navigation.csv, row.names=F, na='')
}