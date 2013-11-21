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
ohi.version <- package_version("0.9.12")
ohi.DEBUG <- FALSE

###
# load the *MANUAL* order of the goals and subgoals:
# Refs #40: TR is before LE as per Nature2012 Figure 1.
#
# ohi.goal.all is the list of only the goals
ohi.goal.all <- c('FP','AO','NP','CS','CP','TR','LE','SP','CW','BD')
# ohi.subgoal.all is the list of only the subgoals, or the goal if only 1 subgoal
ohi.subgoal.all <- c('FIS','MAR','LIV','ECO','ICO','LSP','HAB','SPP')
# ohi.goal.subgoal.unique is the list of only the subgoals, or the goal if only 1 subgoal
ohi.goal.subgoal.unique <- c('FIS','MAR','AO','NP','CS','CP','TR','LIV','ECO','ICO','LSP','CW','HAB','SPP')
# ohi.goal.subgoal.all is the cross product of all goals and subgoals
ohi.goal.subgoal.all <- c('FIS','FP','MAR','AO','NP','CS','CP','TR','LIV','LE','ECO','ICO','SP','LSP','CW','HAB','BD','SPP')
ohi.subgoal.parent <- c('FIS'='FP', 'MAR'='FP', 'LIV'='LE', 'ECO'='LE', 'ICO'='SP', 'LSP'='SP', 'HAB'='BD', 'SPP'='BD')

# Labels
ohi.model.keys <- c('r'='region', 'g'='goal', 'd'='dimension', 'c'='component', 'l'='layer')
ohi.model.labels <- c('x'='Status', 't'='Trend', 'p'='Pressures', 'r'='Resilience', 'xF'='Likely Future Status')
ohi.model.dimensions <- make.names(tolower(ohi.model.labels))
# Refs #40: Order here is meaningless -- use alpha sort by goal/subgoal
ohi.goal.labels <- c(
    'AO'='Artisanal Fishing Opportunities',
    'BD'='Biodiversity',
    'CP'='Coastal Protection',
    'CS'='Carbon Storage',
    'CW'='Clean Waters',
    'FP'='Food Provision',
    'LE'='Coastal Livelihoods and Economies',
    'NP'='Natural Products',
    'SP'='Sense of Place',
    'TR'='Tourism and Recreation',
    'ECO'='Economies',
    'FIS'='Fisheries',
    'HAB'='Habitats',
    'ICO'='Iconic Species',
    'LIV'='Livelihoods',
    'LSP'='Lasting Special Places',
    'MAR'='Mariculture',
    'SPP'='Species'
)

# schemes as list of column_name=aster_label
ohi.valuesets <- c('unweighted'='Global',
                   'preservationist'='Preservationist',
                   'extractive'='Extractive',
                   'nonextractive'='Non-extractive',
                   'extreme'='Extractive Extreme')

ohi.labels <- c(ohi.goal.labels, ohi.model.labels, ohi.valuesets)

ohi.pressure.category <- list('environmental'=c('po','hd','fp','sp','cc'), 'social'=c('ss'))


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

aggregate_by_country = function(df, col.value='value', col.country='country_id', lyrs.dat.csv=layers_data.csv){  
  # debug: df = cn; col.value='status'; col.country='country_id'  
  
  library(sqldf)
  
  #rgn_georegions = setNames(rgn_georegions, c('region_id', 'n', 'r0', 'r1', 'r2'))
  load.regions.countries(lyrs.dat.csv) # makes: cntry_georegions
  
  # get data joined to georegions
  q = sprintf("SELECT d.%s AS country_id, c.country_area_km2, d.%s AS value, g.r0, g.r1, g.r2
              FROM df AS d
              JOIN cntry_georegions AS g USING (country_id)
              JOIN cntry AS c USING (country_id)              
              WHERE value IS NOT NULL
              ORDER BY country_id", col.country, col.value)
  d = sqldf(q)
  
  # aggregate into regional area-weighted averages based on actual data
  t_regionals = sqldf(
    "SELECT * FROM (
    -- aggregate into r0 (world) georegion
    SELECT r0, SUM(value * country_area_km2)/SUM(country_area_km2) AS r0_mean, COUNT(*) AS r0_n
    FROM d
    GROUP BY r0
  ) AS t0 JOIN (
    -- aggregate into r1 (continent) georegions
    SELECT r0, r1, SUM(value * country_area_km2)/SUM(country_area_km2) AS r1_mean, COUNT(*) AS r1_n
    FROM d
    GROUP BY r0, r1
  ) AS t1 USING (r0) JOIN (
    -- aggregate into r2 (regional) georegions
    SELECT r0, r1, r2, SUM(value * country_area_km2)/SUM(country_area_km2) AS r2_mean, COUNT(*) AS r2_n
    FROM d
    GROUP BY r0, r1, r2
  ) AS t2 USING (r0, r1)")
  
  # calculate OHI region area weighted average of values using available data
  t_actuals = sqldf("SELECT * FROM  (    
                    -- first find actuals for regions with data for only a single country
                    SELECT  region_id, 
                    MIN(d.value) AS score, 1 AS n    -- note this means MIN == d.value
                    FROM d
                    JOIN cntry_rgn AS r USING (country_id)
                    GROUP BY region_id
                    HAVING COUNT(*) = 1
                    UNION
                    -- now aggregate (with weighted average by area) regions with data
                    -- for more than one country
                    SELECT  region_id, 
                    SUM(d.value * d.country_area_km2)/SUM(d.country_area_km2) AS score, COUNT(*) AS n
                    FROM d
                    JOIN cntry_rgn AS r USING (country_id)
                    GROUP BY region_id
                    HAVING COUNT(*) > 1
  ) ORDER BY region_id")
  
  # merge the results so that available data is used when present,
  # otherwise use r2, r1, or r0 geomeans, in that order
  t_scores = sqldf(
    "SELECT r.region_id, 
    CASE  WHEN d.score IS NOT NULL     THEN d.score
    WHEN g.r2_mean IS NOT NULL   THEN g.r2_mean
    WHEN g.r1_mean IS NOT NULL   THEN g.r1_mean
    ELSE g.r0_mean
    END AS score,
    CAST(
    CASE WHEN d.score IS NOT NULL THEN 'actual'
    ELSE 'georegion'
    END AS VARCHAR(80)) AS source
    FROM rgn_georegions r
    LEFT JOIN t_actuals d USING (region_id)
    LEFT JOIN t_regionals g USING (r0, r1, r2)
    WHERE (d.region_id IS NOT NULL OR g.r0 IS NOT NULL) -- must have *some* data
    ORDER BY r.region_id")
  
  # return
  df.out = setNames(t_scores[,c('region_id','score')], c('region_id', col.value))
  attr(df.out, 'source') = t_scores[,'source']
  return(df.out)
}

aggregate_by_country_weighted = function(df, w, col.value='value', col.country='country_id', col.weight='weight', ld.csv=layers_data.csv){
  library(sqldf)
  
  # eg LIV in calc.LE:
  #  a = aggregate_weighted(df=subset(s, component='livelihood'),
  #                          w=subset(cy, year==workforce_year & !is.na(workforce), c(country_id,workforce)), 
  #                          col.value='score', col.country='country_id', col.weight='workforce') # ABW workforce==NA
  
  # set common names to rgn_georegions
  #rgn_georegions = setNames(rgn_georegions, c('region_id', 'n', 'r0', 'r1', 'r2'))
  load.regions.countries() # makes: cntry_georegions  
  
  # standardize names, first limiting to only used fields
  w  =  w[,c(col.country,col.weight)]
  df = df[,c(col.country,col.value)]
  names(w)[names(w)==col.weight] = 'w'
  names(df)[names(df)==col.value] = 'value'
  names(df)[names(df)==col.country] = 'country_id'
  
  # get data joined to georegions
  q = sprintf("SELECT d.country_id, d.value, w.w, g.r0, g.r1, g.r2
              FROM df AS d
              JOIN cntry_georegions AS g USING (country_id)
              JOIN w USING (country_id)
              WHERE value IS NOT NULL
              ORDER BY country_id")
  d = sqldf(q)
  
  # aggregate into regional area-weighted averages based on actual data
  t_regionals = sqldf(
    "SELECT * FROM (
    -- aggregate into r0 (world) georegion
    SELECT r0, SUM(value * w)/SUM(w) AS r0_mean, COUNT(*) AS r0_n
    FROM d
    GROUP BY r0
  ) AS t0 JOIN (
    -- aggregate into r1 (continent) georegions
    SELECT r0, r1, SUM(value * w)/SUM(w) AS r1_mean, COUNT(*) AS r1_n
    FROM d
    GROUP BY r0, r1
  ) AS t1 USING (r0) JOIN (
    -- aggregate into r2 (regional) georegions
    SELECT r0, r1, r2, SUM(value * w)/SUM(w) AS r2_mean, COUNT(*) AS r2_n
    FROM d
    GROUP BY r0, r1, r2
  ) AS t2 USING (r0, r1) 
    ORDER BY r0, r1, r2")  
  
  # TODO: generalize aggregate_*() functions to accept country, year, weights with code above, and use same code below.
  
  # calculate OHI region area weighted average of values using available data
  t_actuals = sqldf(
    "SELECT * FROM  (    
    -- first find actuals for regions with data for only a single country
    SELECT  region_id, 
    MIN(d.value) AS score, 1 AS n    -- note this means MIN == d.value
    FROM d
    JOIN cntry_rgn AS r USING (country_id)
    GROUP BY region_id
    HAVING COUNT(*) = 1
    UNION
    -- now aggregate (with weighted average by area) regions with data
    -- for more than one country
    SELECT  region_id, 
    SUM(d.value * d.w)/SUM(d.w) AS score, COUNT(*) AS n
    FROM d
    JOIN cntry_rgn AS r USING (country_id)
    GROUP BY region_id
    HAVING COUNT(*) > 1
  ) ORDER BY region_id")
  
  # merge the results so that available data is used when present,
  #   otherwise use r2, r1, or r0 geomeans, in that order
  t_scores = sqldf(
    "SELECT r.region_id, 
    CASE  WHEN d.score IS NOT NULL     THEN d.score
    WHEN g.r2_mean IS NOT NULL   THEN g.r2_mean
    WHEN g.r1_mean IS NOT NULL   THEN g.r1_mean
    ELSE g.r0_mean
    END AS score,
    CAST(CASE WHEN d.score IS NOT NULL THEN 'actual'
    ELSE 'georegion'
    END AS VARCHAR(80)) AS source
    FROM rgn_georegions r
    LEFT JOIN t_actuals d USING (region_id)
    LEFT JOIN t_regionals g USING (r0, r1, r2)
    WHERE (d.region_id IS NOT NULL OR g.r0 IS NOT NULL) -- must have *some* data
    ORDER BY r.region_id")
  
  # return
  df.out = setNames(t_scores[,c('region_id','score')], c('region_id', col.value))
  attr(df.out, 'source') = t_scores[,'source']
  return(df.out)
}

aggregate_by_country_year = function(df, col.value='value', col.country='country_id'){
  library(sqldf)
  
  # debug: df = cny_k = subset(cnky, product=='fish_oil'); col.value='Xp'; col.country='country_id'
  #rgn_georegions = setNames(rgn_georegions, c('region_id', 'n', 'r0', 'r1', 'r2'))
  load.regions.countries(layers_data.csv) # makes: cntry_georegions  
  
  # get data joined to georegions
  q = sprintf("SELECT d.%s AS country_id, d.year, c.country_area_km2, d.%s AS value, g.r0, g.r1, g.r2
              FROM df AS d
              JOIN cntry_georegions AS g USING (country_id)
              JOIN cntry AS c USING (country_id)              
              WHERE value IS NOT NULL
              ORDER BY country_id", col.country, col.value)
  d = sqldf(q)
  
  # aggregate into regional area-weighted averages based on actual data
  t_regionals = sqldf(
    "SELECT * FROM (
    -- aggregate into r0 (world) georegion
    SELECT year, r0, SUM(value * country_area_km2)/SUM(country_area_km2) AS r0_mean, COUNT(*) AS r0_n
    FROM d
    GROUP BY year, r0
  ) AS t0 JOIN (
    -- aggregate into r1 (continent) georegions
    SELECT year, r0, r1, SUM(value * country_area_km2)/SUM(country_area_km2) AS r1_mean, COUNT(*) AS r1_n
    FROM d
    GROUP BY year, r0, r1
  ) AS t1 USING (year, r0) JOIN (
    -- aggregate into r2 (regional) georegions
    SELECT year, r0, r1, r2, SUM(value * country_area_km2)/SUM(country_area_km2) AS r2_mean, COUNT(*) AS r2_n
    FROM d
    GROUP BY year, r0, r1, r2
  ) AS t2 USING (year, r0, r1)")
  
  # calculate OHI region area weighted average of values using available data
  t_actuals = sqldf("SELECT * FROM  (    
                    -- first find actuals for regions with data for only a single country
                    SELECT d.year, region_id, 
                    MIN(d.value) AS score, 1 AS n    -- note this means MIN == d.value
                    FROM d
                    JOIN cntry_rgn AS r USING (country_id)
                    GROUP BY region_id, d.year
                    HAVING COUNT(*) = 1
                    UNION
                    -- now aggregate (with weighted average by area) regions with data
                    -- for more than one country
                    SELECT d.year, region_id, 
                    SUM(d.value * d.country_area_km2)/SUM(d.country_area_km2) AS score, COUNT(*) AS n
                    FROM d
                    JOIN cntry_rgn AS r USING (country_id)
                    GROUP BY region_id, d.year
                    HAVING COUNT(*) > 1
  ) ORDER BY region_id")
  
  # merge the results so that available data is used when present,
  # otherwise use r2, r1, or r0 geomeans, in that order
  t_scores = sqldf(
    "SELECT d.year, r.region_id, 
    CASE  WHEN d.score IS NOT NULL     THEN d.score
    WHEN g.r2_mean IS NOT NULL   THEN g.r2_mean
    WHEN g.r1_mean IS NOT NULL   THEN g.r1_mean
    ELSE g.r0_mean
    END AS score,
    CAST(
    CASE WHEN d.score IS NOT NULL THEN 'actual'
    ELSE 'georegion'
    END AS VARCHAR(80)) AS source
    FROM rgn_georegions r
    LEFT JOIN t_actuals d USING (region_id)
    LEFT JOIN t_regionals g USING (year, r0, r1, r2)
    WHERE (d.region_id IS NOT NULL OR g.r0 IS NOT NULL) -- must have *some* data
    ORDER BY d.year, r.region_id")
  
  # return
  df.out = setNames(t_scores[,c('year','region_id','score')], c('year', 'region_id', col.value))
  attr(df.out, 'source') = t_scores[,'source']
  return(df.out)
}

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


calc.Pressures = function(){
  # TODO: use regions from layers and load.regions.countries()
  
  # copied from '/Volumes/local_edit/src/toolbox/code/calc_pressures.R' on 2013-09-02
  #
  # intro -------------------------------------------------------------------
  # Calculate pressures using ohi R functions:
  #
  #   p = ohi.model.pressures.matrix(alpha, beta), where
  #       alpha = weighting matrix of the form [category x pressure]
  #       beta  = aggregation matrix of the form [region_id x category] to collapse across each category
  #
  #   P = ohi.model.pressures(p, w, GAMMA=0.5), where
  #       p = the pressures value matrix [region_id x pressure]
  #       w = rank weighting matrix of the form [region_id x pressure]
  #
  # TODO: CS habitat extent column names
  # TODO: add w_max_p=3 as argument to ohi.model.pressures to allow for modification
  # TODO: Are these regions the only ones that fail? 2, 24, 67, 79, 114, and 172
  #       If so, I suspect that it's a problem with a regions -> country mapping or some other aggregation method related to the regions definitions. 
  #       These regions are either a collection of islands (2, 67, 114), or other small islands (24, 79, 172).
  # TODO: automate ingestion of other layers and merge with prep_pressures_layers.R
  # TODO: compare alpha.all when setting NAs to 0
  #       alpha.all[is.na(alpha.all)] = 0
  # TODO: For now, warn of missing layers from pressures_matrix.csv and remove. Later, stop with error.  
  #
  # see: /var/data/ohi/model/GL-NCEAS-Pressures_Matrix/README_EXAMPLES.html
  #      /var/data/ohi/model/GL-NCEAS-Pressures_Matrix/report{5-7}.R - basic HAB component calc and compare example
  
  # libraries and paths -----------------------------------------------
  require(reshape2)
  require(plyr)
  options(gsubfn.engine = "R") # for sqldf
  library(sqldf)
  options(sqldf.driver = 'SQLite')
  options(sqldf.verbose = F)
  
  debug = TRUE # for saving and later comparing pressure components
  
  # read data -----------------------------------------------------
  
  # ensure variables set from config.R
  stopifnot(exists('layers_navigation.csv'))
    
  # load data
  layers_navigation = read.csv(layers_navigation.csv, na.strings=c('','NA'))
  layers_data = read.csv(layers_data.csv, na.strings=c('','NA'))
  pressures_matrix = read.csv(pressures_matrix.csv, na.strings='')
  
  # get unique region_ids for analysis, and setup initial pressures data.frame for column binding results
  load.regions.countries()
  region_ids = rgn_global$rgn_id
  d_region_ids = data.frame(region_id=region_ids)
  D = data.frame(rgn_id=region_ids)
  
  # ingest layers data ----------------------------------------------------------
  
  # read in pressures matrix
  alpha.all = pressures_matrix
  
  # cast pressures layer data
  p.layers = sort(names(alpha.all)[!names(alpha.all) %in% c('goal','component','component_name')])
  d.p = rename(dcast(layers_data, id_num ~ layer, value.var='value_num', subset=.(layer %in% p.layers)),
               c('id_num'='region_id')); head(d.p)

  # check for any duplicates in layers, which forces the dcast to use length as the input value (BIG PROBLEM)
  # TODO: add this to check.layers_navigation()
  stopifnot(ddply(subset(layers_data, layer %in% p.layers), .(layer), summarize, duplicated = !anyDuplicated(id_num)>0 )[,'duplicated'])  
  
  # # run once: update layers_navigation.csv to show layers used
  # ln = read.csv(layers_navigation.csv, na.strings='')
  # stopifnot(p.layers %in% ln$layer)
  # ln[ln$layer %in% p.layers, 'used_nature2012'] = 'pressures_matrix'; ln[ln$layer %in% p.layers, 1:5]
  # write.csv(ln, layers_navigation.csv, row.names=F, na='')
  
  # handle still missing layers
  p.layers.missing = p.layers[!p.layers %in% layers_data$layer]
  if (length(p.layers.missing)>0){
    warning('MISSING pressures_matrix.csv stressors from layers_data*.csv: ', paste(p.layers.missing, collapse=', '))  
    # MISSING pressures_matrix.csv stressors from layers_data*.csv: cc_slr, hd_shrimp_pct
    p.layers = setdiff(p.layers, p.layers.missing) 
    alpha.all = alpha.all[, !names(alpha.all) %in% p.layers.missing]
  }

  # confirm that all the resilience layers in the layers_data range from 0 to 1
  p.vals =  ddply(subset(layers_data, layer %in% p.layers), .(layer), summarize,
                  val_min = min(value_num, na.rm=T),
                  val_max = max(value_num, na.rm=T),
                  val_ok  = val_min >= 0 & val_max <= 1)
  if (any(!p.vals$val_ok)){
    lyrs.notok = subset(p.vals, val_ok==F, layer, drop=T)
    print('ORIGINAL problematic layers...')
    print(subset(p.vals, val_ok==F), row.names=F)    
    #stop('Some pressures matrix layers are outside the require range [0-1].')
    warning('Some pressures matrix layers are outside the require range [0-1]. So quick HACK rescaling 0 to 1.') # DEBUG
    #DEBUG HACKY KLUDGE to get reasonable pressures outputs
    for (lyr in lyrs.notok){ # lyr = lyrs.notok[1]
      d = subset(layers_data, layer==lyr); summary(d)
      d = within(d, {
        value_num = ( value_num - min(value_num, na.rm=T) ) / ( max(value_num, na.rm=T) - min(value_num, na.rm=T) ) }); summary(d)
      layers_data = rbind(subset(layers_data, layer != lyr),
                          d)
    }
    p.vals =  ddply(subset(layers_data, layer %in% lyrs.notok), .(layer), summarize,
                    val_min = min(value_num, na.rm=T),
                    val_max = max(value_num, na.rm=T),
                    val_ok  = val_min >= 0 & val_max <= 1)
    print('HACK RESCALED layers...')
    print(p.vals, row.names=F)    
  }
  
  
  # iterate goals -----------------------------------------------------------
  # TODO: generate ohi.goal.subgoal.unique from goals.csv
  for (g in ohi.goal.subgoal.unique){ # g=ohi.goal.subgoal.unique[1]   # g='NP' # g='LIV' # g='FIS'  # g='CP'
    #for (g in c('LIV')){  # DEBUG
    
    if (debug) cat(sprintf('goal: %s\n', g))
    
    # reset components for so when debug==TRUE and saving, is per goal
    P = w = p = alpha = beta = NA
    
    # p: pressures value matrix [region_id x pressure]
    p <- matrix(as.matrix(d.p[match(region_ids, d.p$region_id), p.layers]), 
                nrow=length(region_ids), ncol=length(p.layers), 
                dimnames = list(region_id=region_ids, pressure=p.layers)); dim(p)
    
    # components
    p.components = alpha.all$component[alpha.all$goal==g]
    
    if (length(p.components)==1){
      cat('  no components\n')
      
      # pressure weighting matrix applied to all regions
      w <- matrix(rep(unlist(alpha.all[alpha.all$goal==g, p.layers]), length(region_ids)*length(p.layers)), byrow=T, 
                  nrow=length(region_ids), ncol=length(p.layers), 
                  dimnames = list(region_id=region_ids, pressure=p.layers))        
      
      # debug spot check Ascension Island in middle of N Atlantic which should have low CW pressures, but is at max of 1
#       if (g=='CW') browser()
#       p['85', names(na.omit(w[1,]))]
#       w['85', names(na.omit(w[1,]))]
  #   po_chemicals_3nm   po_nutrients_3nm       po_pathogens           po_trash             ss_wgi 
  # 0.0671219881369822 0.0000000000000000 0.2426123407727320 0.8285002918469240 0.2233172297507260
  # 
  # po_chemicals_3nm po_nutrients_3nm     po_pathogens         po_trash           ss_wgi 
  #                3                3                3                3                1      
  # If the w(g, i, j) is NoData or 0, the weighted pressure pw(g, i, j) is NoData.
#   p.k = ( 3 * 0.0671219881369822 + 3 * 0 + 3 * 0.2426123407727320 + 3 * 0.8285002918469240 ) / 3; p.k # 0.379411540252213
#   
#   p.i = c( 3 * 0.0671219881369822 / 3, 3 * 0.2426123407727320 / 3, 3 * 0.8285002918469240 / 3 )
#   w.max = max(c(3, 3, 3))
#   p.e = sum(w.max * p.i) / (w.max * 3); p.e # 0.828500291846924 
#   
#       
#       sum( p.w * p.k ) / p.k * 3
#   p.worst = max(c(3 * 0.0671219881369822 / 3, 3 * 0.2426123407727320 / 3, 3 * 0.8285002918469240 / 3 )); p.worst # 0.828500291846924
#   p.env = ( 3 * 0.0671219881369822 + 3 * 0 + 3 * 0.2426123407727320 + 3 * 0.8285002918469240 ) / ( 3 *3); p.env # 0.379411540252213
#       
#   p.soc = 0.2233172297507260
#   gamma = 0.5
#   pressure = gamma * p.env + 0.5 * p.soc; pressure
#   pressure = 0.6116586149
#   p.soc = 0.2233172297507260
#   p.env = pressure - 0.5 * p.soc = 0.500000000024637
  
      # calculate pressures per region
       P = ohi.model.pressures(p, w, GAMMA=0.5)
# if (g=='CW'){
#   P = ohi.model.pressures(p, w, GAMMA=0.5, browse=T)
# } else {
#   P = ohi.model.pressures(p, w, GAMMA=0.5)
# }
      
    } else { 
      if (debug) cat(' ',length(p.components),'components:', paste(p.components,collapse=', '), '\n')
      
      # alpha [component x pressure]: pressure rank matrix applied to all categories
      alpha <- matrix(as.matrix(alpha.all[alpha.all$goal==g, p.layers]), 
                      nrow=length(p.components), ncol=length(p.layers), 
                      dimnames = list(category=p.components, pressure=p.layers))
      
      # get data layer for determining the weights by region, which could be from layers_data or layers_data_bycountry
      stopifnot(g %in% names(pressures_components))
      #     if (!agg$layer_id %in% get(agg$layers_data)$layer_id & debug==TRUE){
      #       if (debug) cat('  skipping: pressures_component_aggregation.layer_id not yet configured\n')
      #       next()
      #     } else if (!agg$layer_id %in% get(agg$layers_data)$layer_id & debug==FALSE){
      #       stopifnot(agg$layer_id %in% get(agg$layers_data)$layer_id)
      #     }        
      #d_w = subset(get(agg$layers_data), layer_id==agg$layer_id)
      d_w = rename(subset(layers_data, layer==pressures_components[[g]][['layer']], c(id_num,category,value_num)),
                   c('id_num'='region_id','value_num'='value'))
      
      # ensure that all components are in the aggregation layer category
      if (!all(p.components %in% unique(d_w$category))){
        stop(sprintf('The following components for %s are not in the aggregation layer %s categories (%s): %s', g, pressures_components[[g]][['layer']], 
                     paste(unique(d_w$category), collapse=', '),
                     paste(p.components[!p.components %in% d_w$category], collapse=', ')))
      }
#       cond.1 = sub('(.*)( only|, with |, without )(.*)', '\\1', r.g$component)
#       cond.2 = sub('(.*)( only|, with |, without )(.*)', '\\3', r.g$component)
#       component_categories = unique(na.omit(c(ifelse(nchar(cond.1)>0, cond.1, NA),
#                                               ifelse(nchar(cond.2)>0, cond.2, NA))))      
#       if (!all(component_categories %in% unique(lyr_agg$category))){
#         cat(sprintf('Based on the following components for %s:\n  %s', g, paste(r.g$component, collapse='\n  ')))
#         stop(sprintf('The following component categories for %s are not in the aggregation layer %s categories (%s): %s', g, resilience_components[[g]][['layer']], 
#                      paste(unique(lyr_agg$category), collapse=', '),
#                      paste(component_categories[!component_categories %in% lyr_agg$category], collapse=', ')))
#       }
      
      # based on sequence of aggregation
      if (pressures_components[[g]][['level']]=='region_id-category'){
        # eg NP: calculate a pressure by region_id (like a subgoal pressure per category), Then aggregate using pressures_component_aggregation:layer_id.
        if (debug) cat(sprintf("  scoring pressures seperately by region and category, like a subgoal (pressures_calc_level=='region_id-category')\n"))
        
        # get pressure per category
        if (exists('krp')) rm(krp)
        for (k in p.components){ # k = p.components[1]
          
          # w [region_id x pressure]: pressure weighting matrix applied to all regions
          w <- matrix(rep(unlist(alpha.all[alpha.all$goal==g & alpha.all$component==k, p.layers]), length(region_ids)*length(p.layers)), byrow=T, 
                      nrow=length(region_ids), ncol=length(p.layers), 
                      dimnames = list(region_id=region_ids, pressure=p.layers))
          
          # calculate pressures per region, category
          rp.k = data.frame(category=k, region_id=dimnames(p)$region_id, 
                            p=ohi.model.pressures(p, w, GAMMA=0.5))
          if (exists('krp')){
            krp = rbind(krp, rp.k)
          } else {
            krp = rp.k
          }
        }
        
        # join region, category, pressure to weighting matrix
        #browser()
        krpw = sqldf("
                     SELECT CAST(region_id AS INTEGER) AS region_id, category, krp.p, d_w.value AS w 
                     FROM krp 
                     INNER JOIN d_w USING (region_id, category) 
                     ORDER BY CAST(region_id AS INTEGER), category")
        #krpwp = sqldf("SELECT region_id, SUM(w*p)/COUNT(category) AS p FROM d_region_ids LEFT JOIN krpw USING (region_id) GROUP BY region_id")
        krpwp = sqldf("SELECT region_id, SUM(w*p)/SUM(w) AS p FROM d_region_ids LEFT JOIN krpw USING (region_id) GROUP BY region_id")
        P = krpwp$p
        names(P) = krpwp$region_id      
        
      } else if (pressures_components[[g]][['level']]=='region_id'){
        # most goals like this: collapse weights across categories first, then calculate pressures per region
        if (debug) cat(sprintf("  aggregating across categories to region (pressures_calc_level=='region_id')\n"))
        
        # cast and get sum of categories per region
        #if (g=='CS') browser()
        if (!is.na(subset(layers_navigation, layer==pressures_components[[g]][['layer']], id_chr, drop=T))){
          #if (agg$layers_data == 'layers_data_bycountry'){ # OLD, before agg moved to config.R
          # this condition seems to no onger apply, since all but NP (handled above if level is 'region_id-category')
          stop('surprise, layers_data_bycountry used')
          if (debug) cat(sprintf("  using layers_data='layers_data_bycountry'\n"))
          d_w_r = sqldf(paste("SELECT DISTINCT region_id, category, country_id, country_area_km2 value FROM d_w JOIN regions_countries_areas USING(country_id) WHERE region_id IN (",paste(region_ids, collapse=','),")"))        
          m_w = dcast(d_w_r, region_id ~ category, sum)  # function(x) sum(x, na.rm=T)>0)
        } else { # presume layers_data == 'layers_data'    
          if (debug) cat(sprintf("  using layers_data='layers_data'\n"))
          # for CS: matrix of weights by category based on proportion of regional total for all categories
          #browser()
          m_w = dcast(subset(d_w, region_id %in% region_ids), region_id ~ category, sum, margins=c('category'))
          m_w = cbind(m_w[,'region_id',drop=F], m_w[,2:(ncol(m_w)-1)] / m_w[,'(all)'])  #print(summary(m_w))        
        }      
        
        # beta [region_id x category]: aggregation matrix 
        beta = matrix(as.matrix(m_w[,-1]), 
                      nrow=nrow(m_w), ncol=ncol(m_w)-1, 
                      dimnames = list(region_id=m_w$region_id, category=names(m_w)[-1]))
        
        # for LIV/ECO, limit beta columns to alpha rows
        beta = beta[, rownames(alpha)]
        
        # calculate weighting matrix
        if (debug) cat(sprintf("  ohi.model.pressures.matrix(alpha, beta, calc='avg')\n"))
        w = ohi.model.pressures.matrix(alpha, beta, calc='avg')
        # TODO: test calc type of calculation, whether avg (default), mean (diff't from avg?) or presence (results in 1 or 0)
        
        # append missing regions with NA
        region_ids.missing = setdiff(region_ids, dimnames(w)$region_id)
        pressures.missing = setdiff(p.layers, dimnames(w)$pressure)
        w = matrix(rbind(cbind(w, 
                               matrix(0, nrow=nrow(w), ncol=length(pressures.missing))), 
                         matrix(0, nrow=length(region_ids.missing), ncol=ncol(w)+length(pressures.missing))),
                   nrow=nrow(w)+length(region_ids.missing), ncol=ncol(w)+length(pressures.missing),
                   dimnames = list('region_id'=c(dimnames(w)$region_id, region_ids.missing), 
                                   'pressure'=c(dimnames(w)$pressure, pressures.missing)))[as.character(region_ids), p.layers]    
        
        # check matrices
        stopifnot(all(dimnames(w)$pressure == dimnames(w)$pressure))
        stopifnot(!is.null(dimnames(w)$region_id))
        stopifnot(all(dimnames(p)$region_id == dimnames(w)$region_id))
        
        # calculate pressures per region
        P = ohi.model.pressures(p, w, GAMMA=0.5)
        
      } else {
        stop(sprintf("pressures_component_aggregation.csv : pressures_calc_level of '%s' not handled. Must be either 'region_id' or 'region_id-category'.", agg$aggregation_sequence))
      }    
    } # end if (length(p.components)==1)
    
#     # contrast
#     P.tbx = data.frame(goal.subgoal=g, id=names(P), pressures=P*100)
#     P.ans = subset(results_global_data, goal.subgoal==g, c(goal.subgoal, id, pressures))
#     cat('Compare x=P.tbx with y=P.ans...\n')
#     ck.P = contrast(x=P.tbx, y=P.ans, by=c('goal.subgoal','id'), on='pressures', drop.mutual.na=T, precision=2, verbosity=1)
    # notice that some Nature 2012 answer region_ids are consistently NA: 110, 114, 79
    
#     # save individual goal pressure components for later comparison if debug==TRUE
#     if (debug==TRUE){
#       fn = sprintf('data/debug/pressures_%s.RData', g)
#       if (!file.exists(dirname(fn))){ dir.create((dirname(fn))) }
#       save(P, w, p, alpha, beta, file=fn)      
#     }  
    
    # bind to results
    D = merge(D, setNames(data.frame(names(P),
                                     P), c('rgn_id',g)), all.x=T)
    
  }
  return(D)
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
    options(stringsAsFactors=FALSE)
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
  packageStartupMessage(paste('Loading OHI options (version ', as.character(ohi.version), ')', sep=''))  
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


ohi.model.goal <- function(id, status, trend, resilience, pressure, 
                             DISCOUNT = 1.0, BETA = 0.67, default.trend = 0.0) {
    #' Goal-level computation function to goal score ("component indicators for
    #' public goals") based on status, trend, resilience, pressure
    #'
    #' Parameters:
    #' @param id is the subregion identifier
    #' @param status (x)
    #' @param trend (t)
    #' @param resilience (r)
    #' @param pressure (p)
    #'
    #' Constants:
    #' @param DISCOUNT is the discount factor (i.e., df = 1 - rate)
    #' @param BETA is the trend dampening constant, aka "beta"
    #'
    #' Flags:
    #'
    #'
    #' Returns:
    #' @return data table with status (x), trend (t), resilience (r), 
    #' pressure (p), plus additional columns for future status (xF)
    #' and the goal score (score).

    # verify parameters
    if (getOption('debug', FALSE)) {
        stopifnot(BETA >= 0 && BETA <= 1)
        stopifnot(DISCOUNT >= 0)
    }

    # Simplify symbols based on math writeup
    d <- data.frame(id=id, x=status, t=trend, r=resilience, p=pressure)

    # replace a trend of NA with a default (0)
    if (!is.null(default.trend) && is.numeric(default.trend) && any(is.na(d$t))) {
        d$t[is.na(d$t)] <- default.trend
    }

    # enforce domains
    if (getOption('debug', FALSE)) {
        stopifnot(min(d$x, na.rm=T) >= 0  && max(d$x, na.rm=T) <= 1)   #  [0, 1]
        stopifnot(min(d$t, na.rm=T) >= -1 && max(d$t, na.rm=T) <= 1)   # [-1, 1]
        stopifnot(min(d$r, na.rm=T) >= 0  && max(d$r, na.rm=T) <= 1)   #  [0, 1]
        stopifnot(min(d$p, na.rm=T) >= 0  && max(d$p, na.rm=T) <= 1)   #  [0, 1]
    }

    # compute "future" status, using all dimensions
    d$xF <- with(d, (DISCOUNT * (1 + (BETA * t) + ((1-BETA) * (r - p)))) * x)
    # clamp status domain to [0, 1]
    d$xF <- with(d, score.clamp(xF))
    # compute score using formula for individual goal component indicator
    d$score <- with(d, (x + xF)/2)
    # return results
    d
}

ohi.model.pressures <- function(p, w, GAMMA=0.5, browse=F) {
    #' Computation of pressure
    #'
    #' The weighting matrix and the pressure scores matrix are of the form
    #' [region_id] x [pressure]
    #'
    #' The pressure names must be of the form "category"_"pressure". 
    #' Use "ss" to denote the social category.
    #'    
    #' Parameters:
    #' @param p is the pressures value matrix [region_id x pressure]
    #' @param w is the weighting matrix of the form [region_id x pressure]
    #'
    #' @return pressures scores as a named vector.
    
    # verify parameters
    if (getOption('debug', FALSE)) {
        stopifnot(is.array(w) && is.array(p))
        stopifnot(min(p, na.rm=T) >= 0  && max(p, na.rm=T) <= 1)   #  [0, 1]
        stopifnot(min(w, na.rm=T) >= 0  && max(w, na.rm=T) <= 3)   #  [0, 3]
    }
    
    # normalize dimension handles
    stopifnot(all(names(dimnames(p)) == c('region_id', 'pressure')))
    stopifnot(all(names(dimnames(w)) == c('region_id', 'pressure')))
    stopifnot(all(dimnames(w)$region_id %in% dimnames(p)$region_id))
    stopifnot(all(dimnames(w)$pressure %in% dimnames(p)$pressure))
    
    # align
    w <- with(dimnames(p), w[region_id,pressure])
    
    # create tree for hierarchy of pressures categories
    stopifnot(all(grepl('_', dimnames(p)$pressure)))
    pcat <- data.frame(pressure=unique(dimnames(p)$pressure))
    pcat <- within(pcat, {
      category <- gsub('^([a-z]+)_.*$', '\\1', tolower(pressure))
    })
    # all.cats <- unlist(ohi.pressure.category, use.names=F)
    # stopifnot(all(pcat$category %in% all.cats))
        
    # Step 1: apply rank weights
    w <- ifelse(w == 0, NA, w) # exclude any 0 or NoData weights
    p_w <- p * w
    p_w <- merge(melt(p_w), pcat, all.x=T, by='pressure')
    p_w <- acast(p_w, region_id ~ category ~ pressure)

    # Step 2: rescale and save the max rank weight per category for later
    p_w_max <- array(NA, 
                     dim=c(dim(p_w)[[1]], length(ohi.pressure.category$environmental)), 
                     dimnames=list(region_id=dimnames(p)[[1]], 
                                   category=ohi.pressure.category$environmental))
    p_k <- p_w_max
    for (k in dimnames(p_k)$category) { # k='po'
      j <- grep(paste('^', k, '_', sep=''), dimnames(w)[[2]], value=T)
      wj <- w[,j, drop=F]
      pj <- p[,j, drop=F]
      
      p_w_max[,k] <- apply.byrow(wj, function(x) { 
          if (all(is.na(x))) {
            NA
          } else {
            max(x, na.rm=T)
          }
        })
      
      # Eq (8) from Nature 2012
      p_k[,k] <- apply.byrow(pj * wj, function(x) { # Refs #26 - fix problem when all stressors within a category are NA
          if (all(is.na(x))) {
            NA
          } else {
            sum(x, na.rm=T)
          }
        })  # sum over all pressures
      
    
# DEBUG
if (browse & k=='po') browser(); range(p_k[,k])      
    
      p_k[,k] <- score.rescale(p_k[,k], xlim=c(0,3)) # rescale from rank weight max
      p_k[,k] <- score.clamp(p_k[,k]) # clamp to [0,1]
      # BB quick fix, since example of Ascension 2012a (id 85) was getting a po of 1. NOPE: getting value > 1.
      #p_k[,k] <- score.rescale(p_k[,k], xlim=c(0,1)) # rescale from rank weight max      
    }
    
    # Step 3: compute environmental pressures score using weights from max ranks
    k <- ohi.pressure.category$environmental
    p_e <- rep(NA, nrow(p_k))
    for (i in 1:nrow(p_k)) {
      # Eq (9) from Nature 2012
      p_e[i] <- sum(p_k[i,k] * p_w_max[i,k], na.rm=T) / sum(ifelse(is.na(p_k[i,k]),NA,1) * p_w_max[i,k], na.rm=T)
    }
    names(p_e) <- dimnames(p_k)$region_id
    
    # Step 4: compute social pressures score using unweighted pressures
    # Eq (10) from Nature 2012
    stopifnot(length(ohi.pressure.category$social) == 1) # supports only a single social category
    k <- ohi.pressure.category$social[[1]]
    j <- grep(paste('^', k, '_', sep=''), dimnames(p)[[2]], value=T)
    p_s <- score.clamp(apply.byrow(p[,j, drop=F], mean, na.rm=T))
    names(p_s) <- rownames(p)
    stopifnot(!all(is.nan(p_s)))
    
    # Step 5: apply gamma to environmental and social to yield score per region
    p_x <- (GAMMA * p_e) + ((1-GAMMA) * p_s)
    p_x
}

ohi.model.pressures.matrix <- function(alpha, beta, calc='avg') {
  #' Parameters:
  #' @param alpha weighting matrix of the form [category x pressure]
  #' @param beta aggregation matrix of the form [region_id x category] to collapse across each category
  #' @param calc type of calculation, whether avg (default), mean (diff't from avg?) or presence (results in 1 or 0)

  w <- matrix(NA, nrow=dim(beta)[[1]], ncol=dim(alpha)[[2]], 
              dimnames=list(region_id=dimnames(beta)[[1]], pressure=dimnames(alpha)[[2]]))
  for (i in dimnames(w)$region_id) { # i=dimnames(w)$region_id[1]
    for (j in dimnames(w)$pressure) { # j=dimnames(w)$pressure[1]
      if (calc=='avg'){
        w[i,j] <- sum(t(alpha)[j,] * beta[i,], na.rm=T) / sum(beta[i,], na.rm=T)        
      } else if (calc=='mean') {
        # eg HAB (see /var/data/ohi/model/GL-NCEAS-Pressures_Matrix/report7.R)
        # TODO: check whether bug in HAB calculation or intentional since beta [region_id x category] only ever 1 or NA
        w[i,j] <- mean(t(alpha)[j,] * beta[i,], na.rm=T)
      } else if (calc=='presence') {
        w[i,j] <- mean(t(alpha)[j,] * beta[i,], na.rm=T)
      } else {
        stop("ohi.model.pressures.matrix() calc argument not one of required: 'avg','mean','presence'")
      }
    }
  }
  # convert NaN to NA (which happens when dividing by 0, ie no category in given region_id)
  w[is.nan(w)] <- NA
  w
}

ohi.model.resilience.matrix <- function(b, w.layers=NA) {
  stopifnot(all(names(dimnames(b)) == c('region_id', 'layer')))
  if (missing(w.layers)) {
    w.layers <- rep(1, dim(b)[[2]])
    names(w.layers) <- dimnames(b)$layer
  } else {
    stopifnot(is.vector(w.layers))
  }
  stopifnot(all(dimnames(b)$layer %in% names(w.layers)))
  
  # calculate to preserve dimensionality and NoData values
  ifelse(b,1,NA) * matrix(w.layers[dimnames(b)$layer], nrow=nrow(b), ncol=ncol(b), byrow=T)
}

ohi.resilience.category <- c('environmental', 'regulatory', 'social')

ohi.model.resilience <- function(r, t, w=NA, gamma=0.5) {
    stopifnot(all(is.matrix(r), is.vector(t)))
    stopifnot(all(t %in% ohi.resilience.category))
    stopifnot(all(names(dimnames(r)) == c('region_id', 'layer')))    
    stopifnot(all(dimnames(r)$layer %in% names(t)))
    
    if (missing(w)) {
      w <- rep(1, dim(r)[[2]])
      names(w) <- dimnames(r)$layer
      w <- ohi.model.resilience.matrix(!is.na(r), w)
    } else {
      stopifnot(is.matrix(w))
      stopifnot(all(names(dimnames(w)) == c('region_id', 'layer')))
    }
    
    # verify parameters
    if (getOption('debug', FALSE)) {
        stopifnot(min(r, na.rm=T) >= 0  && max(r, na.rm=T) <= 1)   #  [0, 1]
        stopifnot(min(w, na.rm=T) >= 0  && max(w, na.rm=T) <= 2)   #  [0, 2]
    }
    
    # normalize dimension handles
    stopifnot(all(dimnames(w)$layer %in% dimnames(r)$layer))
    
    # align
    t <- t[dimnames(r)$layer]
    w <- w[dimnames(r)$region_id, dimnames(r)$layer, drop=F]
    stopifnot(all(dimnames(r)$layer == dimnames(w)$layer))
    stopifnot(all(dimnames(r)$layer == names(t)))
    
    # compute by category
    for (k in ohi.resilience.category) {
      l <- paste('r', k, sep='_')
      if (k %in% t) {
        l.r <- r[,names(t)[t == k], drop=F]
        l.mask <- ifelse(!is.na(l.r), 1, NA)
        l.w <- w[,dimnames(l.r)$layer, drop=F]
        l.score <- apply.byrow(l.r*l.w, sum, na.rm=T) / apply.byrow(l.mask*l.w, sum, na.rm=T)
        assign(l, l.score)
      } else {
        assign(l, rep(NA, nrow(r)))
      }
    }
    
    # compute
    scores.e <- apply.byrow(cbind(get('r_environmental'), get('r_regulatory')), mean, na.rm=T)
    scores.s <- get('r_social')
    scores <- apply.byrow(cbind(scores.e, scores.s), weighted.mean, w=c(gamma,1-gamma), na.rm=T)
    names(scores) <- dimnames(r)$region_id
    scores
}


# data file functions -------------------------------------------------

ohi.read.csv <- function(file, na.strings='', row.names=NULL, ...) {
    read.csv(file, na.strings=na.strings, row.names=row.names, ...)
}

ohi.write.csv <- function(x, file, digits=NULL, row.names=F, na='', ...) {
    if (!is.data.frame(x)) {
        d <- as.data.frame(x)
    } else {
        d <- x # deep copy 
    }

    if (is.null(digits)) {
        digits <- getOption('digits', 12)
    }
    for (i in 1:ncol(d)) {
        if (typeof(d[,i]) == "double") {
            #
            # strip out any extra precision using round+signif
            #
            # > options(digits=16)
            # > x <- as.double(-0.123456789987654321e-16)
            # > x
            # [1] -1.234567899876543e-17
            # > signif(x, 15)
            # [1] -1.23456789987654e-17
            # > round(x, 15)
            # [1] 0
            # > round(signif(x, 15), 15)
            # [1] 0

            # > x <- as.double(0.123456789987654321)
            # > x
            # [1] 0.1234567899876543
            # > signif(x, 15)
            # [1] 0.123456789987654
            # > round(x, 15)
            # [1] 0.123456789987654
            # > round(signif(x, 15), 15)
            # [1] 0.123456789987654
            # > 

            d[,i] <- round(signif(d[,i], digits), digits) 
        }
    }
    # write clean CSV files such that real numbers x digits
    write.csv(d, file, na=na, row.names=row.names, ...)
}


# save data 
ohi.save <- function(name, dir='data', method=c('RData', 'csv'), ...) {
    if (getOption('debug', FALSE)) {
        stopifnot(is.character(name) && exists(name))
        stopifnot(is.vector(method))
        stopifnot(any(c('RData','csv') %in% method))
        stopifnot(file.exists(dir))
    }
    
    if ('RData' %in% method) {
        fn <- paste(file.path(dir, name), 'RData', sep='.')
        vcat('saving', name, 'as RData file', fn, '...\n')
        save(list=name, file=fn, ...)            
    }
    if ('csv' %in% method) {
        fn <- paste(file.path(dir, name), 'csv', sep='.')
        vcat('saving', name, 'as CSV file', fn, '...\n')
        ohi.write.csv(get(name), file=fn, ...)
    }
    invisible()
}

# save results in the form of (region_code, dimension) tuples
ohi.save.results <- function(xdim, dir) {
    stopifnot(is.character(xdim) && xdim %in% ohi.model.dimensions && exists(xdim))
    stopifnot(is.data.frame(get(xdim)))
    ohi.save(xdim, dir=dir, method='csv')
}

ohi.save.status <- function(dir='..') { ohi.save.results('status', dir) }
ohi.save.trend <- function(dir='..') { ohi.save.results('trend', dir) }
ohi.save.pressures <- function(dir='..') { ohi.save.results('pressures', dir) }
ohi.save.resilience <- function(dir='..') { ohi.save.results('resilience', dir) }

ohi.savebin <- function(name, dir='data', ...) {
    ohi.save(name, dir, method='RData', ...)
}

ohi.savetxt <- function(name, dir='data', ...) {
    ohi.save(name, dir, method='csv', ...)
}

# load data
ohi.load <- function(name, dir='data', method=c('RData', 'csv'), envir=.GlobalEnv) {
    if (getOption('debug', FALSE)) {
        stopifnot(is.character(name))
        stopifnot(is.vector(method))
        stopifnot(any(c('RData','csv') %in% method))
        stopifnot(file.exists(dir))
    }

    # load data using as many methods as provided until success
    for (m in method) {
        if ('RData' == m) {
            fn <- paste(file.path(dir, name), 'RData', sep='.')
            if (file.exists(fn)) {
                vcat('loading', name, 'from RData file', fn, '...\n')
                load(fn, envir=envir)
                invisible(name)
            }
        } else if ('csv' == m) {
            fn <- paste(file.path(dir, name), 'csv', sep='.')
            if (file.exists(fn)) {
                vcat('loading', name, 'from CSV file', fn, '...\n')
                assign(name, ohi.read.csv(fn), envir=envir)
                invisible(name)
            } else {
                warning(paste("File not found:", fn))
            }
        } else {
            warning(paste("Unknown load method:", m))
        }
        
    }
    invisible()
}

ohi.loadbin <- function(name, dir='data', envir=.GlobalEnv) {
    ohi.load(name, dir, method='RData', envir)
}


# scoring functions -------------------------------------------------------
#
# > score.max(c(0.5, 1, 2))
# [1] 0.25 0.50 1.00
# > score.rescale(c(0.5, 1, 2))
# [1] 0.00 0.33 1.00
# > score.clamp(c(-0.5, 1, 2))
# [1] 0.00 1.00 1.00
#
score.rescale <- function(x, xlim=NULL, method='linear', ...) {
    if (is.null(xlim)) {
        xlim <- c(min(x, ...), max(x, ...))
    }
    if (getOption('debug', FALSE)) {
        stopifnot(method == 'linear')
        stopifnot(length(xlim) == 2)
        stopifnot(xlim[1] == min(xlim))
        stopifnot(xlim[2] == max(xlim))
    }

    (x - xlim[1])/(xlim[2]-xlim[1])
}

score.max <- function(x, p=0.0, ...) {
    score.rescale(x, xlim=c(0, (max(x, ...)*(1.0 + p))))
}

score.clamp <- function(x, xlim=c(0,1), ...) {
    if (getOption('debug', FALSE)) {
        stopifnot(length(xlim) == 2)
        stopifnot(xlim[1] == min(xlim))
        stopifnot(xlim[2] == max(xlim))
    }

    # x must be first since its class is used in mismatches
    pmin(pmax(x, xlim[1]), xlim[2]) 
}

if (getOption('debug', FALSE) && getOption('unit.test', FALSE)) {
    stopifnot(score.rescale(c(-0.5, 0.0, 0.5, 1.0, 1.5)) == c(0.00, 0.25, 0.5, 0.75, 1.00))
    stopifnot(score.max(c(-0.5, 0.0, 0.5, 1.0, 1.5)) == c(-1/3, 0, 1/3, 2/3, 1))
    stopifnot(score.clamp(c(-0.5, 0.0, 0.5, 1.0, 1.5)) == c(0, 0, 0.5, 1, 1))
}

# shiny app ------------------------------------------------------------------
launchApp = function(config.R){
  # library(devtools); la = function() {load_all('/usr/local/ohi/src/R/ohi')}; la()
  # R -e "install.packages('/usr/local/ohi/src/R/ohi_0.9.12.tar.gz', repos=NULL, type='source')"
  # launchApp('/usr/local/ohi/src/toolbox/scenarios/global_2012_nature/conf/config.R')
  # R -e "ohi::launchApp('/usr/local/ohi/src/toolbox/scenarios/global_2012_nature/conf/config.R')"
  
  load.config(config.R)
  dir.app = system.file('shiny_app', package='ohi')
  shiny::runApp(dir.app)
}

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

config.summary = function(config.R, indent='  '){
  files = c('goals.csv','layers_navigation.csv','pressures_matrix.csv','resilience_matrix.csv','resilience_weights.csv')
  for (f in files){
    cat(indent,f,': PASS\n',sep='')
  }  
}


# extras ------------------------------------------------------------------

if (getOption('ohi.extras', TRUE)) {
    # backward compatability
    goals <- ohi.goal.all
    goals_subgoals <- ohi.goal.subgoal.all
    goal_subgoals <- ohi.goal.subgoal.unique
    ohi.global.regions.noATA <- ohi.global.regions.eez.noATA
    ohi.global.regions <- ohi.global.regions.eez
    schemes <- ohi.valuesets
    subgoals <- ohi.subgoal.all
    ohi.casestudies <- c('CC'='California Current', 'BR'='Brazil', 'MB'='Mid-Atlantic Bight', 'GL'='Global', 'FJ'='Fiji')

    ###
    # utility functions
    vcat <- function(...) {
        if (getOption('verbose', FALSE) || getOption('debug', FALSE)) {
            cat(...)
        }
    }

    pkey <- function(..., sep='_') { 
        # make a composite key using character strings
        paste(..., sep=sep) 
    }

    mangle <- function(s, sep='') {
        # convert text into legal identifier
        # stopifnot(mode(s) == 'character' && all(nchar(s)>0))
        gsub('[.]+', sep, make.names(s, allow_ = FALSE))
    }

    starts.with <- function(x, s) { substr(s, 1, nchar(x)) == x }
    ends.with <- function(x, s) { substr(s, nchar(s)-nchar(x)+1, nchar(s)) == x }

    BY.ROW <- 1; BY.COL <- 2

    apply.byrow <- function(x, f, ...) { apply(x, BY.ROW, f, ...) }
    apply.bycol <- function(x, f, ...) { apply(x, BY.COL, f, ...) }


    ###
    # save results in the form of (id, dimension(s)) tuples
    ohi.write.results <- function(x, dir='.', scope='eez', ...) {
        stopifnot(is.data.frame(x))
        results <- x
        names(results) <- tolower(names(results))

        stopifnot('id' %in% tolower(names(results)))
        results <- allregions(results, scope)

        stopifnot(any(c('value', ohi.model.dimensions) %in% names(results)))
        ohi.write.csv(results, file.path(dir, '_results_by_region.csv'), ...)
    }

    ###
    # locate a file in using fs keys
    ohi.find.file <- function(name, type = 'stable', ext = '') {
        fn <- ifelse(nchar(ext) > 0, paste(name, ext, sep='.'), name)
        p <- as.character(
        switch (type,
                model = Sys.getenv("OHI_MODELDIR"),
                stable = Sys.getenv("OHI_STABLEDIR"),
                ingest = Sys.getenv("OHI_INGESTDIR"),
                raw = Sys.getenv("OHI_RAWDIR"),
                tmp = Sys.getenv("OHI_RUNDIR"),
                lib = Sys.getenv("OHI_DISK_LIB")
                )
        )
        file.path(p, fn)
    }

    ###
    # scenario runs where "A" decreases and "B" increases, 
    # or vice versa with inverse flag
    #
    # scenario.to.beta() >- 1.0
    # scenario.to.beta('A10') -> 0.9
    # scenario.to.beta('B10') -> 1.1
    # scenario.to.beta('B10', trend=T) -> 1.02
    # scenario.to.beta('A10', inverse=T) -> 1.1
    # scenario.to.beta('B10', inverse=T) -> 0.9
    # scenario.to.beta('B10', inverse=T, trend=T) -> 0.98
    # scenario.to.beta('B300') -> 4.0
    #
    scenario.to.beta <- function(s = NA, inverse=F, trend=F) {
        if (is.na(s) || nchar(s) < 2) {
            1.0
        } else {
            x <- as.numeric(substring(s, 2))/100.0 # X50 -> 0.5
            if (trend) {
                x <- x/5
            }
            if (toupper(substring(s, 1, 1)) == 'A') {
                if (inverse) { 1.0 + x } else { 1.0 - x }
            } else {
                if (inverse) { 1.0 - x } else { 1.0 + x }
            }
        }
    }

    # build out scenario cases
    scenarios.cases <- function(k, scenarios=c(NA, 'A', 'B', 'C', 'D', 'E')) {
        # scenarios must have a default (NA)
        stopifnot(sum(is.na(scenarios)) == 1)
        
        cases <- data.frame(scenario=scenarios)
        cases$path[is.na(scenarios)] <- 'cases/default'
        cases$path[!is.na(scenarios)] <- 
            paste('cases/Scenario_', 
                  cases$scenario[!is.na(scenarios)], sep='')
        cases
    }

    # generate delta of global scores between default and new case
    scenarios.compare <- function(cases, i, out.dir='.', f.diff=NULL) {
        if (is.null(f.diff)) {
            f.diff <- function(a, b) { 
                ifelse(a==0 & b!=0,Inf,((b - a) / a))
            }
        }

        # read data
        scorefn <- 'results_global_scores.csv'
        dir.1 <- cases$path[is.na(cases$scenario)]
        dir.2 <- cases$path[[i]]
        fn.1 <- file.path(dir.1, scorefn)
        fn.2 <- file.path(dir.2, scorefn)
        d.1 <- ohi.read.csv(fn.1)
        d.2 <- ohi.read.csv(fn.2)

        # load into matrix
        goals <- names(d.1)[-(1:3)]
        m.1 <- as.matrix(d.1[,goals])
        m.2 <- as.matrix(d.2[,goals])

        # run comparison
        m.diff <- f.diff(m.1, m.2)

        # write results
        d.diff <- as.data.frame(cbind(d.1[,1:3], m.diff))
        fn.diff <- paste(basename(dir.2), '.csv', sep='')
        write.csv(d.diff, file.path(out.dir, fn.diff), row.names=F)
        fn.diff
    }

    ###
    # load zonal stats data from either GRASS or ArcGIS format
    zstats.load <- function(fn, csv.format='GRASS', extract='mean') {
        stopifnot(any(c('GRASS','ArcGIS') %in% c(csv.format)))
        
        if (csv.format == 'GRASS') {
            d <- ohi.read.csv(fn, na.strings=c('*','-nan'))
            names(d) <- tolower(names(d))
            r <- data.frame(id=d[,'zone'], value=d[,tolower(extract)], n=d[,'non_null_cells'])
        } else if (csv.format == 'ArcGIS') {
            d <- ohi.read.csv(fn, na.strings='')
            names(d) <- tolower(names(d))
            r <- data.frame(id=d[,'value'], value=d[,extract], n=d[,'count'])
        }
        invisible(return(r))
    }

}


# figures -----------------------------------------------------------------


aster = function (lengths, widths, labels, disk=0.5, max.length,
                   center=NULL, main=NULL, fill.col=NULL, plot.outline=TRUE,
                   label.offset=0.15, xlim=c(-1.2, 1.2), ylim=c(-1.2, 1.2), uin=NULL,
                   tol=0.04, cex=1, bty="n", lty=1, 
                   label.col='black', label.font=3, label.cex=NULL, ...) {

  # Custom R function to generate something akin to a rose plot in which
  # the width and length of each petal are directly specified by the user.
  # Or to put it differently, this is somewhat like a pie chart in which
  # the radius of each wedge is allowed to vary (along with the angular
  # width, as pie charts do). As an additional enhancement, one can
  # specify a central disk of arbitrary radius (from 0 to 1, assuming that
  # the plot itself is scaled to the unit circle), in which case the petal
  # heights are always measured from the edge of the disk rather than the
  # center of the circle; if desired, text can be added in the center.
  #
  # Although this kind of plot may already be well known in some circles
  # (no pun intended), I haven't seen it clearly defined or labeled
  # anywhere, so I'm anointing it an 'aster' plot because its component
  # parts are reminiscent of composite flower morphology.
  #
  # As coded below, 'lengths' dictates how far out each petal extends,
  # 'widths' dictates the (angular) width of each petal, and 'disk' gives
  # the relative radius of a central donut hole. If no widths are
  # provided, all petals will have equal widths. Additional function
  # arguments can also control whether petals are labeled, whether the
  # petal lengths are rescaled to the maximum score or to a user-input
  # score, whether spokes delineating each petal are extended to an outer
  # circle, and more. I also wrote a quick convenience wrapper for
  # creating a legend plot.
  #
  # Note that the function here is a repurposed and very heavily modified
  # version of the windrose() function contained in the 'circular'
  # package, although sufficiently rewritten so as not to depend on any
  # functionality in that package.
  #
  # Example invocations appear below.
  #
  # Jim Regetz
  # NCEAS
  # Created on 13-Sept-2011
  #
  # Mods by Ben Best and Darren Hardy
  # December 2011
  #  - fix blank hairlines between circles and polygons in pedals
  #  - accepts more labeling and title options
  #  - accepts data frames for lengths
  #
  # Example plots...
  #
  # # generate some fake data
  # set.seed(1)
  # scores <- sample(1:10)
  # weights <- sample(1:10)
  # labels <- paste(LETTERS[1:10], "X", sep="")
  # 
  # # do some plots
  # png(file="aster-plots.png", height=600, width=600)
  # par(mfrow=c(2,2), xpd=NA)
  # aster(lengths=scores, widths=weights, disk=0, main="Example 1",
  #     plot.outline=FALSE)
  # aster(lengths=scores, widths=weights, labels=labels, main="Example 2",
  #     lty=2, fill.col="gray", plot.outline=FALSE)
  # aster.legend(labels=labels, widths=weights)
  # aster(lengths=scores, widths=weights, disk=0.5, main="Example 3",
  #     center="Hello world")
  # dev.off()
  # main aster function definition
  
  if (is.data.frame(lengths)) {
    lengths <- as.numeric(lengths)
  }
  n.petals <- length(lengths)
  if (missing(widths)) {
    widths <- rep(1, n.petals)
  }
  if (missing(max.length)) {
    max.length <- max(lengths)
  }
  if (missing(labels)) {
    labels <- names(lengths)
  }
  if (missing(label.cex)) {
    label.cex <- 0.7 * cex
  }  
  
  # determine radius of each petal
  if (disk < 0 || 1 < disk) {
    stop("disk radius must be between 0 and 1")
  }
  radii <- disk + (1-disk) * lengths/max.length
  
  # define inner function for drawing circles
  # (from original windrose function)
  circles <- function(rad, sector=c(0, 2 * pi), lty=2,
                      col="white", border=NA, fill=FALSE) {
    values <- seq(sector[1], sector[2], by=(sector[2] - sector[1])/360)
    x <- rad * cos(values)
    y <- rad * sin(values)
    if (fill) {
      polygon(x, y, xpd=FALSE, lty=lty, col=col, border=border)
    }
    lines(x, y, col=1, lty=lty)
  }
  
  # lots of low-level positional details
  # (from original windrose function)
  op <- par(mar=c(1, 1, 2, 1))
  mai <- par("mai")
  on.exit(par(op))
  midx <- 0.5 * (xlim[2] + xlim[1])
  xlim <- midx + (1 + tol) * 0.5 * c(-1, 1) * (xlim[2] - xlim[1])
  midy <- 0.5 * (ylim[2] + ylim[1])
  ylim <- midy + (1 + tol) * 0.5 * c(-1, 1) * (ylim[2] - ylim[1])
  oldpin <- par("pin") - c(mai[2] + mai[4], mai[1] + mai[3])
  xuin <- oxuin <- oldpin[1]/diff(xlim)
  yuin <- oyuin <- oldpin[2]/diff(ylim)
  if (is.null(uin)) {
    if (yuin > xuin) {
      xuin <- yuin
    } else {
      yuin <- xuin
    }
  } else {
    if (length(uin) == 1)
      uin <- uin * c(1, 1)
    if (any(c(xuin, yuin) < uin))
      stop("uin is too large to fit plot in")
    xuin <- uin[1]
    yuin <- uin[2]
  }
  xlim <- midx + oxuin/xuin * c(-1, 1) * diff(xlim) * 0.5
  ylim <- midy + oyuin/yuin * c(-1, 1) * diff(ylim) * 0.5
  
  # generate breaks (petal boundaries) based on the widths
  breaks <- (2*pi*c(0, cumsum(widths))/sum(widths))[-(n.petals+1)]
  breaks <- c(breaks, 2 * pi)
  plot(c(-1.2, 1.2), c(-1.2, 1.2), xlab="", ylab="", main="",
       xaxt="n", yaxt="n", pch=" ", xlim=xlim, ylim=ylim,
       bty=bty, ...)
  title(main=main, ...)
  
  # plot full petal outlines
  if (plot.outline) {
    # note: go to n.petals not n.breaks because we the last break is
    # the same as the first
    for (i in 1:n.petals) {
      lines(c(0, cos(breaks[i])), c(0, sin(breaks[i])), lty=lty)
    }
    circles(1, lty=lty)
  }
  # plot the petals themselves
  if (is.null(fill.col)) {
    fill.col <- rainbow(n.petals)
  }
  fill.col <- rep(fill.col, length.out=n.petals)
  for (i in 1:n.petals) {
    w1 <- breaks[i]
    w2 <- breaks[i + 1]
    rad <- radii[i]
    xx <- rad * c(0, cos(w1), cos(w2), 0)
    yy <- rad * c(0, sin(w1), sin(w2), 0)
    polygon(xx, yy, xpd=FALSE, col=fill.col[i], border=fill.col[i])
    lines(xx[1:2], yy[1:2])
    lines(xx[3:4], yy[3:4])
    circles(rad=rad, sector=c(w1, w2), fill=TRUE,
            lty=1, col=fill.col[i], border=fill.col[i])
  }
  # plot petal labels, if given
  if (!is.null(labels)) {
    if (plot.outline) {
      height <- label.offset + rep(1, n.petals)
    } else {
      height <- label.offset + radii
    }
    mids <- breaks[1:n.petals] + diff(breaks)/2
    for (i in 1:n.petals) {
      text(height[i] * cos(mids[i]), height[i] * sin(mids[i]),
           labels=labels[i], cex=label.cex, 
           font=label.font, col=label.col)
    }
  }
  
  # add disk, if desired, with optional text in the middle
  if (0 < disk) {
    circles(disk, fill=TRUE, lty=1)
  }
  if (!is.null(center)) {
    text(0, 0, labels=center, font=2, cex=2.2*cex)
  }
  invisible(NULL)
}


# wrapper function to generate an aster plot to serve as a legend
aster.legend <- function(labels, ...) {
  aster(lengths=rep(1, length(labels)), labels=labels,
        plot.outline=FALSE, bty="o", ...)
  text(x=par("usr")[1]+0.25, y=par("usr")[4]-0.1, labels="Legend", font=4)
}
