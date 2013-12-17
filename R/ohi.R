#
# ohi.R
# 
# Author: Ben Best <bbest@nceas.ucsb.edu>, Darren Hardy <hardy@nceas.ucsb.edu>
# Created: 2011-02-23
# $Id$
# 
# Copyright (c) 2011-2013 NCEAS (UCSB). All rights reserved.
#
# TODO: check as package to drop inst path in inst/shiny_app/server.R:
#       source(system.file('inst/scripts/load_data.r',package='ohi'), local=T, echo=F)

# ohigui:
#   library(ohigui); launchApp('~/ohi_tbx/scenarios/global_2013a/conf/config_2013a.R')


# DEVTOOLS (https://github.com/hadley/devtools/wiki):
#   ldohi = function(){load_all('/usr/local/ohi/src/R/ohi')}; ldohi()
#   library(devtools); la = function() {load_all('/usr/local/ohi/src/R/ohi'); launchApp('~/ohi_tbx/scenarios/global_2012_nature/conf/config.R')}; la()
#   document('/usr/local/ohi/src/R/ohi'); dev_help('ohi.model.pressures.matrix')
# TODO:
#   update setup.sh -> /usr/local/ohi/sbin/svn-changelog to use NEWS style vs current ChangeLog
#
# library(devtools); la = function() {load_all('/usr/local/ohi/src/R/ohi')}; la()
# R -e "install.packages('/usr/local/ohi/src/R/ohi_0.9.12.tar.gz', repos=NULL, type='source')"
# launchApp('/usr/local/ohi/src/toolbox/scenarios/global_2012_nature/conf/config.R')
# R -e "ohi::launchApp('/usr/local/ohi/src/toolbox/scenarios/global_2012_nature/conf/config.R')"

# options(gsubfn.engine = "R") # for sqldf
# library(sqldf)
# options(sqldf.driver = 'SQLite')
# options(sqldf.verbose = F)
#library(tools)

stopifnot(getRversion() >= '2.10')
# see DESCRIPTION and NAMESPACE files for dependencies and imports:
#   require(reshape2)
#   require(markdown)
#   require(pander)
#   require(knitr)
#   require(shiny)

# constants ---------------------------------------------------------------
ohi.DEBUG <- FALSE

# report ------------------------------------------------------------------

#opts_chunk$set(dependson='init',echo=FALSE,cache=TRUE,fig.width=8,fig.height=5)
#options(markdown.HTML.options=c('hard_wrap','use_xhtml','smartypants')) # exclude 'base64_images'

create.report = function(regions_goals.csv, regions_goals_dimensions.csv, report.out, 
              options = c('ck_Equations'=T,'ck_Flowers'=T,'ck_Histograms'=T,
                          'ck_Maps'=T,'ck_Paths'=T,'ck_Tables'=T)){
  
  options = c('ck_Equations'=T,'ck_Flowers'=T,'ck_Histograms'=T,
              'ck_Maps'=T,'ck_Paths'=T,'ck_Tables'=T)
  
  # tools::file_ext() == 'html'
  
  f = sprintf('%s/report.%s', dir.results, c('Rmd','md','html'))
  knitr::knit(f[1],f[2])
  knitr::markdownToHTML(f[2], f[3]); shell.exec(f[2]); shell.exec(f[3])
  
  # pandoc -s --toc pressures_README_modified.md -o pressures_README_modified.html # shell.exec(f[3])
}

# # DEBUG: run report
# config.R = '~/ohi_tbx/scenarios/global_2012_nature/conf/config.R'
# source(config.R)
# report.out=sprintf('%s/%s.html', dir.results, 'report')
# opts = c('ck_Equations'=T,'ck_Flowers'=T,'ck_Histograms'=T,
#          'ck_Maps'=T,'ck_Paths'=T,'ck_Tables'=T)
# create.report(regions_goals.csv, regions_goals_dimensions.csv, report.out, opts)



# calculation helper functions ----

na.to.F = function(x){ ifelse(is.na(x), F, x)} 

contrast = function(x, y, by=intersect(names(x), names(y))[1], by.x=by, by.y=by, on=intersect(names(x), names(y))[2], on.x=on, on.y=on, skip.y.na=T, drop.mutual.na=T, precision=2, verbosity=1){
  #   m = head(m)
  #   status_model_curref_lim = dbGetQuery(pg, "SELECT * FROM global_li.status_model_curref WHERE ref_base_value <> 0 AND ref_adj_value <> 0")
  #   contrast(x=m,                     , by.x=c('country_id','sector','metric'), on.x=c('year_cur','year_ref','base_cur'      ,'base_ref'      ,'adj_cur'      ,'adj_ref'),
  #            y=status_model_curref_lim, by.y=c('iso3166'   ,'sector','metric'), on.y=c('cur_year','ref_year','cur_base_value','ref_base_value','cur_adj_value','ref_adj_value'),
  #            precision=4)
  # x = contrast(x=b, y=a, by.x=c('metric','sector','country_id','year'), by.y=c('metric','sector','iso3166','year'), on.x=c('adj_value'), skip.y.na=F)
  # ck.LIV = contrast(s_liv, s_liv.a, by.x='region_id', by.y='id', on.x='score', on.y='status', precision=4)
  # by.y=NA; on.x=NA; on.y=NA; skip.y.na=T; drop.mutual.na=T; precision=2; verbosity=1
  # x=b; y=a; by.x=c('metric','sector','country_id','year'); by.y=c('metric','sector','iso3166','year'); on.x=c('adj_value'); skip.y.na=F
  # x=s_liv; y=s_liv.a; by.x='region_id'; by.y='id'; on.x='score'; on.y='status'; precision=4
  # x=head(b); by.x=c('metric','sector','country_id','year'); on.x=c('value','base_value','base_whence','adj_value');
  # y=head(a); by.y=c('metric','sector','iso3166'   ,'year'); skip.y.na=F
  
  # get col names and check for existence
  
  if (identical(by.y, NA)) by.y=by.x
  if (identical(on.x, NA)) on.x=setdiff(names(x), by.x)
  if (identical(on.y, NA)) on.y=on.x  
  stopifnot(by.x %in% names(x))
  stopifnot(by.y %in% names(y))
  stopifnot(on.x %in% names(x))
  stopifnot(on.y %in% names(y))
  
  # load comparison data
  r = merge(x[,c(by.x, on.x)], 
            setNames(y[,c(by.y, on.y)],
                     c(by.x, sprintf('%s.y', on.x))) , all=T)
  on.y = sprintf('%s.y', on.x)
  r = r[do.call(order, r[,by.x, drop=F]),]
  
  # calculate differences
  for (f in on.x){ # f = on.x[1]    
    u = r[[f]]
    v = r[[sprintf('%s.y',f)]]
    if (is.numeric(u) & is.numeric(v)){
      r[[sprintf('%s.dif',f)]]   = u - v
      r[[sprintf('%s.equ',f)]]   = is.na(v) | (!is.na(v) & !is.na(u) & round(v, precision) == round(u, precision))
    } else {      
      u = as.character(u)
      v = as.character(u)
      r[[sprintf('%s.dif',f)]]   = ifelse(u!=v, sprintf("'%s'->'%s'",u,v), NA)
      r[[sprintf('%s.equ',f)]]   = is.na(v) | (!is.na(v) & !is.na(u) & u==v)      
    }
    if (skip.y.na==F){
      r[[sprintf('%s.notNA',f)]] = !is.na(u) | (is.na(v) & is.na(u))  
    }
  }
  
  # align columns
  sfx = c('y','dif','equ')
  if (skip.y.na==F) sfx = c(sfx,'notNA')
  r = r[, c(by.x, as.vector(t(cbind(on.x, matrix(sapply(sfx, function(f) sprintf('%s.%s', on.x, f)), nrow=length(on.x))))))]
  
  if (drop.mutual.na){
    idx.y.notin.x = which(rowSums(is.na(r[,on.x,drop=F]))==length(on.x))
    idx.x.notin.y = which(rowSums(is.na(r[,on.y,drop=F]))==length(on.y))
    idx = intersect(idx.y.notin.x, idx.x.notin.y)
    if (length(idx) >0){
      cat('dropping mutual NAs:', length(idx),'/',nrow(r),'\n')
      r = r[-idx,]
    }
  }
  
  # print to summary to console
  if (verbosity > 0 ){
    # report overall mismatches
    idx.y.notin.x = which(rowSums(is.na(r[,on.x,drop=F]))==length(on.x))
    idx.x.notin.y = which(rowSums(is.na(r[,on.y,drop=F]))==length(on.y))
    if (length(idx.y.notin.x)>0){
      cat('all y in x FAIL!:', length(idx.y.notin.x),'/',nrow(y),'\n')
      print(head(r[idx.y.notin.x, c(by.x, on.y)], row.names=F))
      if (length(idx.y.notin.x) > 6) cat('...\n')
    } else {
      cat('all y in x success:', nrow(y),'\n')
    }
    if (length(idx.x.notin.y)>0){
      cat('all x in y FAIL!:', length(idx.x.notin.y),'/',nrow(x),'\n')
      print(head(r[idx.x.notin.y, c(by.x, on.x)], row.names=F))
      if (length(idx.x.notin.y) > 6) cat('...\n')
    } else {
      cat('all x in y success:', nrow(x),'\n')
    }
    
    # report individual fields
    sfx = c('equ')
    if (skip.y.na==F) sfx = c(sfx,'notNA')
    flds = as.vector(t(sapply(sfx, function(f) sprintf('%s.%s', on.x, f))))
    names(flds) = as.vector(t(matrix(rep(on.x, length(sfx)), ncol=length(sfx))))
    for (i in 1:length(flds)){ # i = 1
      f = flds[i]
      col = names(flds[i])
      v = r[[f]]
      nF = sum(v==F)
      if(nF>0){
        cat(f, ' FAIL! on', nF, '/', nrow(r) ,'\n')
        print(head(r[v==F, c(by.x, col, sprintf('%s.%s', col, c('y','dif')))], 6), row.names=F)
        if (nF > 6) cat('...\n')
        cat('\n')
      } else {
        cat(f, 'success\n\n')
      }
    }
  }
  
  # return r
  return(r)
}

load.regions.countries = function(){
  # layers_data.csv=layers_data.csv
  library(plyr)     # rename()
  library(reshape2) # dcast()

  ## from calc_ohi.R:
  #   ohi.load('georegions_countries', dir='/usr/local/ohi/src/model/global2012/doc/regions') # -> cntry_georegions
  #   ohi.load('regions_countries'   , dir='/usr/local/ohi/src/model/global2012/doc/regions') # -> cntry_rgn
  #   ohi.load('regions_georegions'  , dir='/usr/local/ohi/src/model/global2012/doc/regions') # -> rgn_georegions
  #   ohi.load('countries'           , dir='/usr/local/ohi/src/model/global2012/doc/regions') # -> cntry
  # TODO: check that field regions_georegions.n is not needed b/c skipped in rgn_georegions
  # TODO: add rgn_georegions.r*_labels like cntry_georegions.r*_labels
  
  cat(sprintf('\nLoading regions and countries, load.regions.countries(), using:\n  layers_data.csv = %s\n', layers_data.csv))
  
  # get layers_data
  layers_data = read.csv(layers_data.csv, na.strings='')
  
  # 2013: transform layers and globally assign
#  browser(); sort(unique(as.character(layers_data$layer)))
  cntry_georegions <<- rename(dcast(subset(layers_data, layer=='cnk_cntry_georegions'), 
                                          id_chr ~ category, value.var='value_num'), c('id_chr'='country_id'))                       
#   cntry_georegions <<- merge(rename(dcast(subset(layers_data, layer=='cnk_cntry_georegions'), 
#                                       id_chr ~ category, value.var='value_num'), c('id_chr'='country_id')),
#                              rename(dcast(subset(layers_data, layer=='ctk_cntry_georegion_labels'), 
#                                           id_chr ~ category, value.var='value_chr'), c('id_chr'='country_id','r0'='r0_label','r1'='r1_label','r2'='r2_label')), all.x=T)

#   # 2012: transform layers and globally assign
#   cntry_georegions <<- rename(dcast(subset(layers_data, layer=='cnk_cntry_georegions'), 
#                                           id_chr ~ category, value.var='value_num'), c('id_chr'='country_id'))
#   #                          rename(dcast(subset(layers_data, layer=='ctk_cntry_georegion_labels'), 
#   #                                        id_chr ~ category, value.var='value_chr'), c('id_chr'='country_id','r0'='r0_label','r1'='r1_label','r2'='r2_label')), all.x=T)
  
  
  rgn_georegions   <<- rename(dcast(subset(layers_data, layer=='rnk_rgn_georegions'), 
                                    id_num ~ category, value.var='value_num'), c('id_num'='region_id'))

  flds = c('id_num'='rgn_id','category'='type','value_chr'='label')
  rgn_global   <<- rename(subset(layers_data, layer=='rnk_rgn_global'), flds)[,flds] #; head(rgn_global)  
  
   cntry_rgn        <<- rename(subset(layers_data, layer=='cn_cntry_rgn'), 
                               c('id_chr'='country_id','value_num'='region_id'))[, c('country_id','region_id')]
   
#   cntry            <<- merge(rename(subset(layers_data, layer=='ct_cntry_label'),
#                                     c('id_chr'='country_id','value_chr'='country_label')),
#                              rename(subset(layers_data, layer=='cn_cntry_area_land'),
#                                     c('id_chr'='country_id','value_num'='country_area_km2')), all.x=T)[, c('country_id','country_label','country_area_km2')]

  # TODO: check validity of using country's land area (ie cntry.country_area_km2) vs EEZ area in aggregate functions!?
}


# calculations ------------------------------------------------------------

calc.S.T = function(){
  for (g in ohi.goal.subgoal.all){
    cat(g,'\n')
    
    # load goal-specific data if exists
    if (file.exists(sprintf('data/layers_data_%s.csv',g))){ 
      cat('   yes\n')
      #     ohi.load(sprintf('layers_data_%s',g))
      #     d = get(sprintf('layers_data_%s',g))
    } else {
      cat('   no\n')
      #    d = layers_data   
    }
  }
}




calc.Resilience = function(){
  # copied from '/Volumes/local_edit/src/toolbox/code/calc_resilience.R' on 2013-09-02
  
  # intro -------------------------------------------------------------------
  # Calculate resilience using ohi R functions:
  #
  #   r = ohi.model.resilience.matrix(b, w.layers=NA), where
  #       b         = a boolean value matrix [region_id x layer] which is TRUE if the given region_id should include layer, and FALSE otherwise.
  #       w.layers  = the weighting vector of the form [layer]. Each rank weight must be a real number >= 0, or NA for even weighting.
  #
  #   R = ohi.model.resilience(r, t, w=NA, gamma=0.5), where
  #       r = the resilience value matrix [region_id x layer]
  #       t = the typing vector t[layer] where values are from ohi.resilience.category
  #       w = the weighting matrix of the form [region_id x layer]. Each rank weight must be a real number >= 0, or NA for even weighting
  #
  # TODO: calculate avg of goals with subgoals
  # TODO: use config.R's resilience_weight.csv
  #
  # see: https://projects.nceas.ucsb.edu/ohi/issues/70
  #      /var/data/ohi/model/GL-NCEAS-Resilience_Matrix/
  #         report.R for all
  #         report2.R for data flow model
  #         report3.R computes AO model
  #         report4.R computes HAB model
  #         report5.R computes CW model (like AO but with fewer layers)
  #      /var/data/ohi/model/GL-NCEAS-Pressures_Matrix/report{5-7}.R - basic HAB component calc and compare example
  
  
  # libraries and paths -----------------------------------------------
  library(reshape2)  
  debug = TRUE # for saving and later comparing with results
  options(warn=0)
  
  # read data -----------------------------------------------------
  
  # ensure variables set from config.R
  stopifnot(exists('layers_navigation.csv'))
  
  # load data
  layers_navigation  = read.csv(layers_navigation.csv, na.strings=c('','NA'))
  layers_data        = read.csv(layers_data.csv, na.strings=c('','NA'))
  resilience_matrix  = read.csv(resilience_matrix.csv, na.strings='')  
  resilience_matrix  = within(resilience_matrix, {component[is.na(component)] = ''})
  resilience_weights = read.csv(resilience_weights.csv, na.strings='')  
  
#   # get unique region_ids for analysis, and setup initial pressures data.frame for column binding results
#   load.regions.countries()
#   region_ids = rgn_global$rgn_id
#   d_region_ids = data.frame(region_id=region_ids)
#   D = data.frame(rgn_id=region_ids)
  
#   ohi.load('resilience_layers')
#   ohi.load('resilience_component_aggregation')
#   ohi.load('layers_data', dir='/usr/local/ohi/src/model/global2012/doc')
#   ohi.load('layers_data_extra_for_pressures') # just using np_product_weights_byregion [region_id, category, value], generated by prep_pressures_layers.R
#   layers_data = rbind(layers_data, layers_data_extra_for_pressures)
    
  # get resilience layers
  rm.data = resilience_matrix[!names(resilience_matrix) %in% c('goal','component','component_name')]
  r.layers = sort(names(rm.data)); r.layers
  r.layers.missing = r.layers[!r.layers %in% layers_data$layer]
  if (length(r.layers.missing)>0){
    warning('MISSING resilience_matrix.csv layers from layers_data*.csv: ', paste(r.layers.missing, collapse=', '))  
    r.layers = setdiff(r.layers, r.layers.missing) 
  }
  
  # confirm that all the resilience layers in the layers_data range from 0 to 1
  r.vals =  ddply(subset(layers_data, layer %in% r.layers), .(layer), summarize,
                  val_min = min(value_num, na.rm=T),
                  val_max = max(value_num, na.rm=T),
                  val_ok  = val_min >= 0 & val_max <= 1)
  if (any(!r.vals$val_ok)){
    print(subset(r.vals, val_ok==F, row.names=F))
    stop('Some resilience matrix layers are outside the require range [0-1].')
  }
  
  # confirm that all layers in resilience_matrix have a weight assigned in resilience_matrix
  resilience_weights.missing = r.layers[!r.layers %in% resilience_weights$layer]
  if (length(r.layers.missing)>0){
    warning('MISSING resilience_weights.csv layers from resilience_matrix.csv: ', paste(resilience_weights.missing, collapse=', '))  
  }
  
  
#   d.p = rename(dcast(layers_data, id_num ~ layer, value.var='value_num', subset=.(layer %in% p.layers)),
#                c('id_num'='region_id')); head(d.p)  
  
  # # run once: update layers_navigation.csv to show layers used
  # ln = read.csv(layers_navigation.csv, na.strings='')
  # resilience_matrix = read.csv(resilience_matrix.csv, na.strings='')
  # r.layers = sort(names(resilience_matrix)[!names(resilience_matrix) %in% c('goal','component')])
  # ln[ln$layer %in% r.layers, 'used_nature2012'] = 'resilience_matrix'; ln[ln$layer %in% r.layers, 1:5]
  # write.csv(ln, layers_navigation.csv, row.names=F, na='')
  # # check that all layers were in original ftp
  # layers_navigation_ftp2012.csv = '/usr/local/ohi/src/toolbox/scenarios/global_2012_nature/conf/layers_navigation_ftp2012.csv'
  # ln.ftp2012 = read.csv(layers_navigation_ftp2012.csv, na.strings='')
  # r.layers %in% ln.ftp2012$layer_id
  
  # setup data frame for binding columns of resilience scores by goal
  #resilience = data.frame(region_id=ohi.global.regions.eez, row.names=ohi.global.regions.eez)
  load.regions.countries() # loads rgn_global
  D = data.frame(rgn_id=rgn_global$rgn_id); head(D)
  
  # w.layers: weighting vector [layer]
  weights = setNames(resilience_weights$weight, resilience_weights$layer)
  
  # t: typing vector [layer] 
  types = setNames(resilience_weights$type, resilience_weights$layer)  
  
  # iterate goals -----------------------------------------------------------
  #for (g in ohi.goal.subgoal.unique){ # g='AO'
  #DEBUG
  #single.component.goals = c('AO','SPP','CW','FIS','MAR','ECO','LIV','ICO','LSP','TR')
  #goals = c(single.component.goals, 'HAB','CS','CP','NP')
  for (g in unique(resilience_matrix$goal)){ # g='AO'  
    
    if (debug) cat(sprintf('goal: %s\n', g))    
    r.g = subset(resilience_matrix, goal==g)
    
    # DEBUG
    #if (g=='HAB') browser()
    
    if (nrow(r.g)==1){
      # simple single component goal
      
      # extract relavant resilience layers to goal
      r.g.layers = na.omit(as.character(r.g[!names(r.g) %in% c('goal','component','component_name')]))
      
      # extract layers data specific to layers used by goal and merge with layer weights
      r.d = rename(subset(layers_data, 
                          layer %in% r.g.layers & id_num %in% rgn_global$rgn_id, 
                          c(layer, id_num, value_num)),
                   c('id_num'='region_id','value_num'='value')); head(r.d)
      #r.d = merge(r.d, resilience_weights, by='layer');  head(r.d)
      
      # r: resilience value matrix [region_id x layer]
      r <- acast(region_id ~ layer, data=r.d)
      names(dimnames(r)) <- c('region_id', 'layer')
      
      # b: boolean value matrix [region_id x layer]
      b <- ifelse(!is.na(r),T,F); head(b)
      
      # w: weighting matrix [region_id x layer]
      w <- ohi.model.resilience.matrix(b, weights[dimnames(b)[[2]]]); head(w)
      
      # R: resilience score [region_id]
      R = ohi.model.resilience(r, types[dimnames(b)[[2]]], w)
      
      # assign to resilience matrix    
      D = merge(D, setNames(data.frame(as.integer(names(R)), R), c('rgn_id',g)))
    } else {
      # TODO: get agg by...
      #     agg.layer = subset(resilience_component_aggregation, goal==g, layer_id, drop=T)
      #     agg.by    = subset(resilience_component_aggregation, goal==g, aggregate_by, drop=T)
      #     if (identical(agg.by, character(0))) agg.by = ''      
      stopifnot(g %in% names(resilience_components))
      #if (!resilience_components[[g]][['layer']] %in% layers_data$layer) browser()
      stopifnot(resilience_components[[g]][['layer']] %in% layers_data$layer)
      
      # get layer for evaluating components
      flds.agg = c('id_num'='region_id','category'='category','value_num'='value')      
      lyr_agg = rename(subset(layers_data, layer==resilience_components[[g]][['layer']]),
                      flds.agg)[,flds.agg]
      if (min(lyr_agg$value, na.rm=T)<=0){
        cat(sprintf('\nResilience TODO: check OK for regions to have value==0 in lyr_agg here for scenario 201%da goal %s:\n', s, g))
        print(table(subset(lyr_agg, value==0, c(region_id, category))), row.names=F)
      }
      #stopifnot(min(lyr_agg$value, na.rm=T)>0) # ensure value of aggregation layer is always non-zero
      # TODO: explore effects of using a 0 in pressures, eg 2012 NP:
      #               category
      #     region_id corals fish_oil ornamentals seaweeds shells sponges
      #           104      1        1           0        1      1       1
      #           183      1        0           0        0      1       0
      #           231      1        1           1        0      1       1
      #           244      1        1           0        1      1       1
      #           245      0        0           0        1      1       1
      #           248      0        0           0        1      1       1
      #           249      0        0           0        1      1       1
      #           250      0        0           0        1      1       1
      
      # check that all components are in lyr_agg
      #browser()
      cond.1 = sub('(.*)( only|, with |, without )(.*)', '\\1', r.g$component)
      cond.2 = sub('(.*)( only|, with |, without )(.*)', '\\3', r.g$component)
      component_categories = unique(na.omit(c(ifelse(nchar(cond.1)>0, cond.1, NA),
                                              ifelse(nchar(cond.2)>0, cond.2, NA))))      
      if (!all(component_categories %in% unique(lyr_agg$category))){
        cat(sprintf('Based on the following components for %s:\n  %s', g, paste(r.g$component, collapse='\n  ')))
        stop(sprintf('The following component categories for %s are not in the aggregation layer %s categories (%s): %s', g, resilience_components[[g]][['layer']], 
                     paste(unique(lyr_agg$category), collapse=', '),
                     paste(component_categories[!component_categories %in% lyr_agg$category], collapse=', ')))
      }
      #stopifnot(component_categories %in% lyr_agg$category)
      #print(component_categories)
      #head(lyr_agg)
      #(.*) only      
    }      
    
    if (nrow(r.g) > 1 && resilience_components[[g]][['level']]=='region_id'){
      # multiple components within goal, selecting component row for layers to use, and calculating Resilience per region_id            
      
      # check for valid component conditions
      cond = with(r.g, 
                  data.frame(
                    component    = component,
                    cond.default = ifelse(component=='', TRUE, NA),
                    cond.only    = grepl('(.*) only',          component),
                    cond.with    = grepl('(.*), with (.*)',    component),
                    cond.without = grepl('(.*), without (.*)', component)))
      
      # ensure only one TRUE per condition
      stopifnot(all.equal(apply(cond[,-1], 1, sum, na.rm=T), rep(1,length(r.g$component))))
      # break down condition into individual components needed for later evaluation
      cond = cbind(cond,
                   cond.only.1     = ifelse(cond$cond.only==TRUE, gsub("(.*) only",             "\\1", r.g$component), NA),
                   cond.with.1     = ifelse(cond$cond.with==TRUE, gsub("(.*), with (.*)",       "\\1", r.g$component), NA),
                   cond.with.2     = ifelse(cond$cond.with==TRUE, gsub("(.*), with (.*)",       "\\2", r.g$component), NA),
                   cond.without.1  = ifelse(cond$cond.without==TRUE, gsub("(.*), without (.*)", "\\1", r.g$component), NA),
                   cond.without.2  = ifelse(cond$cond.without==TRUE, gsub("(.*), without (.*)", "\\2", r.g$component), NA))            
      
      
#       # DEBUG
#       habitats=c('corals'             = 'coral',
#                  'mangroves'         = 'mangrove',
#                  'salt_marshes'      = 'saltmarsh',
#                  'seagrasses'        = 'seagrass',
#                  'sea_ice_edge'      = 'seaice_edge',
#                  'sea_ice_shoreline' = 'seaice_shoreline')
#       hp = read.csv('/Volumes/data_edit/model/GL-NCEAS-Habitats_v2013a/data/hab_habitat_presence.csv', na.strings='')
#       head(hp)
#       table(hp$habitat)
#       #   coral    mangrove   saltmarsh    seagrass seaice_edge soft_bottom 
#       #     119          97          21         108          35         222
#       
      
      # iterate regions
      for (id in D$rgn_id){ # id=12
        # get components in given region
        components = subset(lyr_agg, region_id==id, category, drop=T)
        
        if (length(components)==0) next
        # ?: CS default '' needs components or ok if 0 when having default?
        
        # get condition for region "to see what condition my condition was in"
        cond.components = cond[,c('cond.default','cond.only.1','cond.with.1','cond.with.2','cond.without.1','cond.without.2')]
        components.in.cond = as.data.frame(apply(cond.components, c(1,2), function(x) x %in% components), row.names=cond[['component']])
        
        # TODO: for HAB, seems wrong that for regions to qualify for "* only" component conditions, they can only have that component, even if other like sea_ice_edge included
        components.in.cond[['cond.only.1']] = ifelse(components.in.cond[['cond.only.1']]==T & length(components)==1, T, F)      
        components.in.cond[['cond.without.2']] = !components.in.cond[['cond.without.2']] # invert without predicate
        components.in.cond[is.na(cond.components)] = NA
        # assign condition to default if default ('') row exists and no other condition found to be True
        if ('' %in% rownames(components.in.cond)){
          if(!any(apply(components.in.cond[''!=rownames(components.in.cond),], 1, function(x) all(x==T,na.rm=T)))){
            components.in.cond['','cond.default'] = TRUE
          }        
        }
        # get condition based on which is true
        condition = rownames(components.in.cond)[apply(components.in.cond, 1, function(x) all(x==T,na.rm=T))]
        if (identical(condition, character(0))) condition = NA
        
        # TODO: XXX/HACK for HAB, seems wrong that regions 112, 164 WITH corals, seagrasses and WITHOUT soft_bottom were getting assigned habitats-bd-soft-bottom-without-corals. So doing manual hack here to get equivalent global results.
        #if (g=='HAB' & id %in% c(112, 164)) condition = 'soft_bottom, with corals'

        layers <- na.omit(as.character(subset(resilience_matrix, goal==g & component==condition)[,c(-1,-2)]))
        
        # extract layers data specific to layers used by goal
        flds = c('layer'='layer','id_num'='region_id','value_num'='value')
        r.d = na.omit(rename(subset(layers_data, layer %in% layers & id_num==id), flds)[,flds]); head(r.d)
        #resilience_data = merge(resilience_data, resilience_weights)
        
        if (nrow(r.d)==0) next # eg for g=CP, id=162 (Antarctica), condition='sea_ice_shoreline only'
        
        # r: resilience value matrix [region_id x layer]
        r <- acast(region_id ~ layer, data=r.d)
        names(dimnames(r)) <- c('region_id', 'layer')
        
        # b: boolean value matrix [region_id x layer]
        b <- ifelse(!is.na(r),T,F)
                
        # w: weighting matrix [region_id x layer]
        w <- ohi.model.resilience.matrix(b, weights[dimnames(b)[[2]]])
        
        # R: resilience score [region_id]
        R = ohi.model.resilience(r, types[dimnames(b)[[2]]], w)
        
        # assign to resilience matrix
        if (!g %in% names(D)) D[[g]] = NA
        D[D$rgn_id==id,g] = R                
      }    
    }
    
    if (nrow(r.g) > 1 && resilience_components[[g]][['level']]=='region_id-category'){
      # multiple components within goal, calculating Resilience per category, and averaging up to region_id (for NP only)
      
      # iterate regions
      for (id in D$rgn_id){ # id=3
        
        # get components in given region
        components = subset(lyr_agg, region_id==id, category, drop=T)      
        if (length(components)==0) next
        
        # iterate components
        R.id.k = numeric()
        for (k in components){ # k=components[1]
          
          
          # extract relavant resilience layers to goal
          layers <- na.omit(as.character(subset(resilience_matrix, goal==g & component==k)[,c(-1,-2)]))
          
          # extract layers data specific to layers used by goal
          flds = c('layer'='layer','id_num'='region_id','value_num'='value')
          r.d = na.omit(rename(subset(layers_data, layer %in% layers & id_num==id), flds)[,flds]); head(r.d)
          #resilience_data = merge(resilience_data, resilience_weights)
          
          if (nrow(r.d)==0) next # eg for g=CP, id=162 (Antarctica), condition='sea_ice_shoreline only'
          
          # r: resilience value matrix [region_id x layer]
          r <- acast(region_id ~ layer, data=r.d)
          names(dimnames(r)) <- c('region_id', 'layer')
          
          # b: boolean value matrix [region_id x layer]
          b <- ifelse(!is.na(r),T,F)
          
          # w: weighting matrix [region_id x layer]
          w <- ohi.model.resilience.matrix(b, weights[dimnames(b)[[2]]])
          
          # R: resilience score [region_id]
          R = ohi.model.resilience(r, types[dimnames(b)[[2]]], w)
          R.id.k = c(R.id.k, setNames(R, k))                  
        }  
        
        # assign to resilience matrix
        if (!g %in% names(D)) D[[g]] = NA
        D[D$rgn_id==id,g] = weighted.mean(R.id.k, subset(lyr_agg, region_id==id, value, drop=T))    
      }      
    }
  } # end for g in...
  
  return(D)
  
}



# lookup old <-> new goal names -------------------------------------------
# for backward compatability, have lookups between new goal/subgoal and old goal-component naming, with and without dashes

# ftmp <- tempfile()
# cat(file=ftmp,
#     "goal.new,goal.old,component.old
# AO,artisanal-fishing,all
# BD,biodiversity,
# CS,carbon-storage,all
# CW,clean-waters,
# FP,food-provision,combo
# LE,livelihoods,
# NP,natural-products,combo
# CP,safe-coastlines,all
# SP,sense-of-place,
# TR,tourism-and-recreation,all
# HAB,biodiversity,habitats
# SPP,biodiversity,species
# FIS,food-provision,fishing
# MAR,food-provision,mariculture
# LIV,livelihoods,livelihood
# ECO,livelihoods,economy
# ICO,sense-of-place,iconic-species
# LSP,sense-of-place,lasting-special-places")
# ohi.old.goal.component = read.csv(ftmp)
# dump(ohi.old.goal.component)
ohi.old.goal.component <-
  structure(list(goal.new = c("AO", "BD", "CS", "CW", "FP", "LE", 
                              "NP", "CP", "SP", "TR", "HAB", "SPP", "FIS", "MAR", "LIV", "ECO", 
                              "ICO", "LSP"), 
                 goal.old = c("artisanal-fishing", "biodiversity", "carbon-storage", "clean-waters", "food-provision", "livelihoods", 
                              "natural-products", "safe-coastlines", "sense-of-place", "tourism-and-recreation", 
                                                          "biodiversity", "biodiversity", "food-provision", "food-provision", 
                                                          "livelihoods", "livelihoods", "sense-of-place", "sense-of-place"
                              ), 
                 component.old = c("all", "", "all", "", "combo", "", "combo", 
                                                   "all", "", "all", "habitats", "species", "fishing", "mariculture", 
                                                   "livelihood", "economy", "iconic-species", "lasting-special-places"
                              )
                 ), 
            .Names = c("goal.new", "goal.old", "component.old"), class = "data.frame")

ohi.goal.to.old.goal.component = function(g, with.dashes=F){
  # convert current 2-letter goal or 3-letter subgoal code to old style long name goal and component
  stopifnot(g %in% ohi.old.goal.component[['goal.new']])
  v = as.character(ohi.old.goal.component[ohi.old.goal.component[['goal.new']]==g, c('goal.old','component.old')])
  if (with.dashes==F){ v = gsub('-','', v) }
  names(v) = c('goal','component')
  return(v)
}
ohi.old.goal.component.to.goal = function(goal.old, component.old=''){
  # convert old style long name goal and component to current 2-letter goal or 3-letter subgoal code
  g.old = gsub('-','', goal.old)
  c.old = gsub('-','', component.old)
  if (g.old %in% c('artisanalfishing', 'carbonstorage', 'safecoastlines', 'tourismandrecreation') & c.old=='') {
    c.old = 'all'
  } else if (g.old %in% c('foodprovision', 'naturalproducts') &  c.old=='') {
    c.old = 'combo'
  }
  stopifnot(g.old %in% gsub('-','',ohi.old.goal.component[['goal.old']]) & c.old %in% gsub('-','',ohi.old.goal.component[['component.old']]))
  #v = as.character(subset(ohi.old.goal.component, gsub('-','', goal.old)==g & gsub('-','', component.old)==c, c(goal.new)))
  v = as.character(ohi.old.goal.component[gsub('-','', ohi.old.goal.component[['goal.old']])==g.old & gsub('-','', ohi.old.goal.component[['component.old']])==c.old, c('goal.new')])
  names(v) = c('goal')
  return(v)
}


# environment -------------------------------------------------------------

ohi.markdown.css = system.file('shiny_app', 'markdown.css', package='ohi')

ohi.options <- function() {
    double.digits <- 15 # <- floor(log10(.Machine$double.base^.Machine$double.digits)) 
    options(digits=double.digits)
    options(stringsAsFactors=FALSE) # to prevent factors
    options(width=120) # for outputting wide columns
    options(rstudio.markdownToHTML = 
              function(inputFile, outputFile) {      
                # example: eg /var/data/ohi/model/GL-NCEAS-Pressures_Matrix/report9.Rmd
                # see: http://www.rstudio.com/ide/docs/authoring/markdown_custom_rendering
                # original: '/Applications/RStudio.app/Contents/Resources/resources/markdown.css'
                markdownToHTML(inputFile, outputFile, stylesheet=ohi.markdown.css)   
              })
    options()
}

.onLoad <- function(libname, pkgname) {
    options(ohi.options())
}

.onAttach <- function(libname, pkgname){
  packageStartupMessage(sprintf('Loading %s (version %s)', pkgname, as.character(packageVersion(pkgname))))  
}

# require.or.install.packages = function(packages){
#   for (p in packages){ # p='raster'
#     if (!p %in% installed.packages()[,'Package']){
#       install.packages(p, dependencies=T, verbose=F)
#     }
#     suppressPackageStartupMessages(require(p, character.only=T))
#   }
# }


# style -------------------------------------------------------------------

md.table <- function(d, justify='right', split.tables=120, ...){
  # see use of ohi.markdown.css with .onLoad options above
  # render markdown tables from input table that are compatible with knitr to HTML for use in Rmarkdown and Knit HTML in RStudio
  # also see styling of CSS tables, per http://www.rstudio.com/ide/docs/authoring/markdown_custom_rendering
  #suppressPackageStartupMessages(require(pander))
  
  s = pander::pandoc.table.return(d, style='grid', justify=justify, split.tables=split.tables)  
  s = gsub('\n[\\+\\|]','\n',s)
  s = gsub('[\\+\\|]\n','\n',s)
  s = gsub('\n[^=][-\\+\\|]+\n','\n',s)
  s = gsub('\\=','-',s)
  s = gsub('\\+','|',s)
  #S = strsplit(s, '\n')[[1]]
  #s = paste(S[c(4,5,seq(6,length(S)-2,by=2))], collapse='\n')
  cat(s)
}

# set knitr defaults for processing chunks in R markdown
library(knitr)
opts_chunk$set(dependson='init',echo=FALSE,cache=TRUE,message=FALSE,fig.width=8,fig.height=5,warning=F)
#options(warn = 0)

# region functions --------------------------------------------------------

# global regional stuff
ohi.global.regions.max <- 186
ohi.global.regions.eez <- 1:172 # include ATA, exclude deleted, disputed, and highseas
ohi.global.regions.eez.noATA <- setdiff(ohi.global.regions.eez, c(162)) # remove ATA (ID=162)
ohi.global.regions.highseas <- 175:185
ohi.global.regions.all <- 1:ohi.global.regions.max

allregions <- function(d=NULL, scope='all') {
    region.ids <- switch(scope,
                         all=ohi.global.regions.all,
                         eez=ohi.global.regions.eez,
                         eez.noATA=ohi.global.regions.eez.noATA,
                         highseas=ohi.global.regions.highseas)

    stopifnot(is.null(region.ids) == FALSE)
    
    # left join with every region
    if (is.null(d)) {
        data.frame(id=region.ids)
    } else {
        merge(allregions(NULL, scope), d, all.x=T)
    }
}

# figures -----------------------------------------------------------------

# wrapper function to generate an aster plot to serve as a legend
aster.legend <- function(labels, ...) {
  aster(lengths=rep(1, length(labels)), labels=labels,
        plot.outline=FALSE, bty="o", ...)
  text(x=par("usr")[1]+0.25, y=par("usr")[4]-0.1, labels="Legend", font=4)
}
