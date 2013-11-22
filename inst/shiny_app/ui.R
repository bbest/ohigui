# OHI Toolbox Application - shiny user interface
# To run at command line:
#   R -e "ohi::launchApp('~/ohi_tbx/scenarios/global_2012_nature/conf/config.R')"
#
# TODO: 
#   TODO: more sophisticated configure files and parser here: http://tolstoy.newcastle.edu.au/R/e6/help/09/05/14975.html / http://docs.python.org/2/library/configparser.html
#   conditional execution, isolate() -- https://groups.google.com/forum/?fromgroups=#!topic/shiny-discuss/l8ZcauoOtuw
#   toggle buttons -- http://www.larentis.eu/bootstrap_toggle_buttons/ - 
#
#   pass parameters via URL: https://groups.google.com/forum/#!searchin/shiny-discuss/URL$20parameters/shiny-discuss/8ZpNyzPKaTg/mhpVUJ0rM2kJ
#     Shiny Persisting Input in URL: require(shiny); runGist("6e77383b48a044191771") # link URL elements to input elements bi-directionally.
#   Map: 
#     dynamic select -- 1) index / goal / goal-dimension / variable, 2) options
#     select region dropdown
#     http://rstudio.github.com/shiny/tutorial/#dynamic-ui
#     https://gist.github.com/wch/4211337
#     fix coral_extent: all yellow legend values 0 to 0.875 to 42022.75. Could use R code with ColorBrewer and Jenks breaks to get hist
#     change projection from Leaflet default "Google Mercator" EPSG:3857 to EPSG4326  (see https://github.com/kartena/Proj4Leaflet)&
#     make local tilecache so don't need internet and at alternate projection (http://blog.thematicmapping.org/2012/07/using-custom-projections-with-tilecache.html)
#     rainbow legend colors
#     see: D3 + Leaflet (http://bost.ocks.org/mike/leaflet/)
#     Cartograms with d3 & TopoJSON: http://prag.ma/code/d3-cartogram
#     unique map coloring: http://bl.ocks.org/jasondavies/4188334
#     line simplification: http://bost.ocks.org/mike/simplify/
#     topojson monster filesize reduction from geojson: https://github.com/mbostock/topojson
#     create Shiny drop-down for field selection
#      merge region label from region_details
#     
#   Paths:
#     d3.js and shiny -- https://groups.google.com/forum/?fromgroups=#!topic/shiny-discuss/tSUCaJr4OFE
#                        http://glimmer.rstudio.com/timelyportfolio/shiny-d3-showreel/
#                        http://glimmer.rstudio.com/timelyportfolio/shiny-d3-plot/
#                        http://timelyportfolio.blogspot.com/search/label/d3 or http://timelyportfolio.blogspot.com/search/label/shiny
#                        http://lamages.blogspot.co.uk/2013/02/first-steps-of-using-googlevis-on-shiny.html
#     helpText("Note: while the data view will show only the specified",
#              "number of observations, the summary will still be based",
#              "on the full dataset."),
#     submitButton(text)
#     div(tags$button(type = "submit", class = "btn btn-primary", text))
#     
#     fileInput(inputId='files', label='File data', multiple=T),
#     helpText("Note: while the data view will show only the specified",
#              "number of observations, the summary will still be based",
#              "on the full dataset."),
#     verbatimTextOutput('conf_out'))),
#     file inputs - https://groups.google.com/forum/?fromgroups=#!topic/shiny-discuss/FWy0le9xVBE
#       https://gist.github.com/4050398
#     styling inputs
#     http://viget.com/inspire/custom-file-inputs-with-a-bit-of-jquery
#     http://duckranger.com/2012/06/pretty-file-input-field-in-bootstrap/ 
#       http://www.randomout.com/2012/07/hacking-more-visually-appealing-file.html
#     upload        http://blueimp.github.com/jQuery-File-Upload/
#     customHeaderPanel --- , div(class = "span12", style = "padding: 10px 0px;", h1(title)) )}
#  Configure:
#     tables
#       - shinySlickgrid
#         https://github.com/wch/shiny-slickgrid
#
#       - spreadsheet-like editable data frame input
#         https://groups.google.com/forum/#!topic/shiny-discuss/F8aAtv85ZGs
#         https://groups.google.com/forum/#!topic/shiny-discuss/IX5qGlunVP0/discussion
#         https://groups.google.com/forum/?fromgroups#!searchin/shiny-discuss/d3|sort:date/shiny-discuss/F8aAtv85ZGs/a_QwvpcJDFQJ
#         install_github('shinyExt',username='killeveritt')
#         devtools::install_github("shiny-incubator", "rstudio")
#         https://github.com/yannrichet/try-shiny/tree/master/handsontable
#         runGitHub("try-shiny","yannrichet",subdir="handsontable")
#
#       - JQuery DataTable plugin - javascript binding issue?
#         https://groups.google.com/forum/?fromgroups=#!topic/shiny-discuss/K8LiWIM1ras

# http://timelyportfolio.blogspot.com/search/label/d3 - d3, Shiny best examples
# http://www.codecademy.com/tracks/jquery
# http://www.codecademy.com/tracks/javascript

# get list of layers
layers_navigation = read.csv(layers_navigation.csv, na.strings='')
# TODO: expand list to alternate scenario ids: region_id, rgn_id, country_id, cntry_id, saup_id.
# TODO: expand list to goal/dimension/...
varChoices = sort(subset(layers_navigation, id_num==layers_id_fields[1] & !is.na(value_num) & is.na(category) & is.na(year), layer, drop=T))
#browser()
names(varChoices) = varChoices

smax = 1 # max for goals slider inputs

#---- customize for leaflet map

customHeaderPanel <- function(title,windowTitle=title){
  tagList(tags$head(
      tags$title(windowTitle),
      tags$link(rel="stylesheet", type="text/css", href="/css/tree.css")))}

reactiveMap <- function (outputId){
  tagList(  
    singleton(tags$head(tags$link(rel="stylesheet", type="text/css", href="css/leaflet.css"   ))),
    singleton(tags$head(tags$link(rel="stylesheet", type="text/css", href="css/ohi_map.css"))),
    
    tags$body(
      tags$script(src="js/leaflet-src.js",  type='text/javascript'),
      tags$script(src="data/ohiRegions.js", type='text/javascript'),
      tags$div(id=outputId, class="shiny-map-output"),
      tags$script(src="js/shinymap.js",     type='text/javascript')   # note: must call shinymap after div tag with id already set
    )
  )    
}

#---- define ui
shinyUI(bootstrapPage(div(class='container-fluid',             # alternate to: pageWithSidebar
  div(class= "row-fluid", customHeaderPanel("OHI App")), # alternate to: headerPanel  
  div(class = "row-fluid", tabsetPanel(id='tabsetFunction',    # alternate to: mainPanel                                                                             
    tabPanel('Data', value='data', conditionalPanel(condition="input.tabsetFunction == 'data'",
      sidebarPanel(id='data-sidebar',
        selectInput(inputId='var', label='Choose a variable:', choices=varChoices)
        # TODO: use Select2 combo boxes and search field, see https://github.com/mostly-harmless/select2shiny
      ),  
      mainPanel(id='data-main',
        tabsetPanel(id='tabsetMap',
          tabPanel('Map',       value='data-map',
                   reactiveMap(outputId = "map")),                   
                    
          tabPanel('Histogram', value='data-histogram', plotOutput('histogram')),
          tabPanel('Summary',   value='data-summary',   verbatimTextOutput('summary')), 
          tabPanel('Table',     value='data-table',     tableOutput('table'))))))
                                       ,  
    tabPanel('Goals', value='goals',        
       sidebarPanel(id='goal-sidbar', 
        strong('Food Provision:'),
          sliderInput("MAR","Mariculture:"                 , min=0, max=smax, value=0.5, step=0.1),
          sliderInput("FIS","Fisheries:"                   , min=0, max=smax, value=0.5, step=0.1),br(),     
        sliderInput("AO",strong("Artisanal Opportunity:")  , min=0, max=smax, value=1, step=0.1),br(),
        sliderInput("NP",strong("Natural Products:")       , min=0, max=smax, value=1, step=0.1),br(),
        sliderInput("CS",strong("Carbon storage:")         , min=0, max=smax, value=1, step=0.1),br(),
        sliderInput("CP",strong("Coastal protection:")     , min=0, max=smax, value=1, step=0.1),br(),       
        sliderInput("TR",strong("Tourism & Recreation:")   , min=0, max=smax, value=1, step=0.1),br(),
        strong('Coastal Livelihoods & Economies:'),
          sliderInput("LIV","Livelihoods:"                 , min=0, max=smax, value=0.5, step=0.1),
          sliderInput("ECO","Economies:"                   , min=0, max=smax, value=0.5, step=0.1),br(),       
        strong('Sense of Place'),
          sliderInput("ICO","Iconic species:"              , min=0, max=smax, value=0.5, step=0.1),
          sliderInput("LSP","Lasting special places:"      , min=0, max=smax, value=0.5, step=0.1),br(),
        sliderInput("CW",strong("Clean waters:")           , min=0, max=smax, value=1, step=0.1),br(),
        strong('Biodiversity'),
          sliderInput("HAB","Habitats:"                    , min=0, max=smax, value=0.5, step=0.1),
          sliderInput("SPP","Species:"                     , min=0, max=smax, value=0.5, step=0.1)
       ),
       mainPanel(id='goal-main', style='height:850px', plotOutput('aster'))),
     
    tabPanel('Paths', value='paths', 
      includeHTML('tree_body.html')),
    
    tabPanel('Calculate', value='configure',
             uiOutput('sel_scenario_dir'), # generates dir_conf              
             verbatimTextOutput(outputId="txt_conf_summary"),
             actionButton('btn_calc','Calculate'),
             verbatimTextOutput(outputId="txt_calc_summary")),
             
    tabPanel('Report', value='report', 
             uiOutput('sel_compare'), # generates dir_conf              
             br('Include:'),
             checkboxInput('ck_Equations' , 'Equations' , value = FALSE),
             checkboxInput('ck_Flowers'   , 'Flowers'   , value = FALSE),
             checkboxInput('ck_Histograms', 'Histograms', value = FALSE),
             checkboxInput('ck_Maps'      , 'Maps'      , value = FALSE),
             checkboxInput('ck_Paths'     , 'Paths'     , value = FALSE),
             checkboxInput('ck_Tables'    , 'Tables'    , value = FALSE),
             actionButton('btn_report','Generate Report')) 
  )) # end tabsetFunction
    
)))