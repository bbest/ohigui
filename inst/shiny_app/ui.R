# see global.R

#---- customize for leaflet map
customHeaderPanel <- function(title,windowTitle=title){
  tagList(tags$head(
    tags$title(windowTitle),
    tags$link(rel="stylesheet", type="text/css", href="/css/tree.css"),
    tags$script(src='spatial/regions_gcs.js')))}      # assume regions geojson variable set by spatial/regions_gcs.js
      
#---- define ui
shinyUI(bootstrapPage(div(class='container-fluid',             # alternate to: pageWithSidebar
  div(class= "row-fluid", customHeaderPanel("OHI App")), # alternate to: headerPanel  
  div(class = "row-fluid", tabsetPanel(id='tabsetFunction',    # alternate to: mainPanel                                                                             
    tabPanel('Data', value='data', conditionalPanel(condition="input.tabsetFunction == 'data'",
      sidebarPanel(id='data-sidebar',                   
        selectInput(inputId='varType', label='1. Choose variable type:', choices=c('Input Layer'='Layer', 'Output Score'='Score'), selected='Input Layer'),
        conditionalPanel(condition="input.varType == 'Layer'",
          selectInput(inputId='sel_layer_target' , label='2. Choose target (goal, pressures, resilience or spatial):', 
                      choices=sel_layer_target_choices), #, selected=names(sel_layer_target_choices)[1]),
          selectInput(inputId='sel_layer', label='3. Choose layer:', choices=GetLayerChoices(layer_targets, input$sel_layer_target) )),          
        conditionalPanel(condition="input.varType == 'Score'",
          #selectInput(inputId='varScore'    , label='Choose goal-dimension:', choices=varScores, selected='Index - score'),
          selectInput(inputId='sel_score_target'   , label='2. Choose target (index or goal):' , choices=sel_score_target_choices   , selected='Index'),        
          selectInput(inputId='sel_score_dimension', label='3. Choose dimension:'              , choices=sel_score_dimension_choices, selected='score')),
        p(textOutput('var_description')),
        verbatimTextOutput(outputId="var_details")),
        # TODO: use Select2 combo boxes and search field, see https://github.com/mostly-harmless/select2shiny
                                                    
                                                    
      mainPanel(id='data-main',
        tabsetPanel(id='tabsetMap',
          tabPanel('Map',       value='data-map',       mapOutput('map_container')),                                       
          tabPanel('Histogram', value='data-histogram', plotOutput('histogram')),
          #tabPanel('Summary',   value='data-summary',   verbatimTextOutput('summary')),                     
          tabPanel('Table',     value='data-table',     dataTableOutput('table'), style='overflow:auto; height:850px'))))),
                                       
    tabPanel('Goals', value='goals', 
       sidebarPanel(id='goal-sidbar', style='overflow:auto; height:850px; width:200px',                     
        strong('Food Provision:'),
          sliderInput("MAR","Mariculture:"                 , min=0, max=smax, value=0.5, step=0.1),br(),
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
       mainPanel(id='goal-main', style='overflow:auto; height:850px',
         plotOutput('aster', ))),
     
    tabPanel('Paths', value='paths', 
      includeHTML('tree_body.html')),
    
    tabPanel('Calculate', value='configure',
      p('Scenario path exists:', verbatimTextOutput(outputId='dir_scenario_exists')),
      conditionalPanel(condition='output.dir_scenario_exists == "FALSE"',
        textInput('dir_scenario', 'Scenario directory to output (default)', value=dir_scenario),
        actionButton('btn_write','Write to disk')),
      conditionalPanel(condition='output.dir_scenario_exists == "TRUE"',
             #uiOutput('sel_scenario_dir'), # generates dir_conf              
             #verbatimTextOutput(outputId="txt_conf_summary"),
             p('Scenario path', verbatimTextOutput(outputId="show_dir_scenario")),
             actionButton('btn_calc','Calculate'),
             verbatimTextOutput(outputId="txt_calc_summary"))),
             
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