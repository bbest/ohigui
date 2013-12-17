# if (!require(devtools)) install.packages('devtools'); require(devtools); install_github('rCharts','bbest'); install_github('ohicore','bbest'); install_github('ohigui','bbest')

# setwd('~/Code/ohigui'); load_all(); shiny::runApp('~/Code/rCharts/inst/apps/leaflet_chloropleth')

require(shiny); require(RJSONIO); require(rCharts); require(RColorBrewer)
options(stringsAsFactors = F)


# Call the recover function when an error occurs: error=recover|browser|NULL
options(error=NULL)

# port data from ohicore package for example sake
# install_githbub('ohicore','bbest')
# require(ohicore)
# layers = layers.Global2013.www2013
# vars_rgn = as.character(sort(subset(layers$meta, fld_id_num=='rgn_id' & is.na(fld_category)  & is.na(fld_year) & is.na(fld_val_chr), layer, drop=T)))
# variable_data = plyr::rename(SelectLayersData(layers, layers=vars_rgn, narrow=T), c('id_num'='rgn_id'))


#library(ohi)
