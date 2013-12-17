# see global.R

# get list of layers
varChoices = as.character(sort(subset(layers$meta, fld_id_num=='rgn_id' & is.na(fld_category)  & is.na(fld_year) & is.na(fld_val_chr), layer, drop=T)))

# add dir for regions
addResourcePath('shapes', path.expand(dir_shapes))

# defaults
smax = 1 # max for goals slider inputs

#---- customize for leaflet map
customHeaderPanel <- function(title,windowTitle=title){
  tagList(tags$head(
    tags$title(windowTitle),
    tags$link(rel="stylesheet", type="text/css", href="/css/tree.css"),
    tags$script(src='shapes/regions_gcs.js')))}      # assume regions geojson variable set by shapes/regions_gcs.js
      
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
          tabPanel('Map',       value='data-map',       mapOutput('map_container')),                                       
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