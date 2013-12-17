# initialize ----
# presume config.R already sourced by ohi::launchApp(config.R) and set needed variables

# TODO: get all layers to include those with other ids
#layers_navigation = read.csv(layers_navigation.csv, na.strings='')
#layers_data = read.csv(layers_data.csv, na.strings='')
layers_data = ohicore::SelectLayersData(layers)

# get goals for aster, all and specific to weights
goals.all = conf$goals # read.csv(goals.csv)
goals.all = goals.all[order(goals.all$order), 'goal']

# get colors for aster, based on 10 colors, but extended to all goals. subselect for goals.wts
cols.goals.all = colorRampPalette(RColorBrewer::brewer.pal(10, 'Spectral'), space='Lab')(length(goals.all))
names(cols.goals.all) = goals.all

# get results for aster
#regions_goals = read.csv(regions_goals.csv)

# helper functions ----
get_wts = function(input){
  #return rescaled goal weights so sums to 1
  wts = c(MAR=input$MAR,
          FIS=input$FIS,    
          AO=input$AO,
          NP=input$NP,
          CS=input$CS,
          CP=input$CP,
          TR=input$TR,
          LIV=input$LIV,
          ECO=input$ECO,
          ICO=input$ICO,
          LSP=input$LSP,
          CW=input$CW,
          HAB=input$HAB,
          SPP=input$SPP)

  # rescale so sums to 1
  wts = wts / sum(wts)
  return(wts)
}

captilize <- function(s) { # capitalize first letter
  paste(toupper(substr(s, 1, 1)), substr(s, 2, nchar(s)), sep='')
}

# reactiveValues ----
values = reactiveValues()
#values$dirs_scenarios <- grep('^[^\\.]', basename(list.dirs(path=dir.scenarios, recursive=F)), value=T)
if (!exists('dir.scenarios')) dir.scenarios = system.file('extdata', package='ohicore')
values$dirs_scenarios <- grep('^conf\\.', basename(list.dirs(path=dir.scenarios, recursive=F)), value=T)

# shinyServer ----
# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output, session) {

  # Observe: values$dirs_scenarios ----
  # monitor filesystem every 5 seconds for folders in dir.conf
  observe({
    invalidateLater(5 * 1000, session)  # 5 seconds, in milliseconds
    values$dirs_scenarios <- grep('^[^\\.]', basename(list.dirs(path=dir.scenarios, recursive=F)), value=T)
  })
  
  # Layers: layer_data() ----
  layer_data <- reactive({
    subset(layers_data, layer==input$var)
#     read.csv.sql(layers_data.csv, 
#                  sprintf("SELECT * FROM file WHERE layer ='%s'", input$var))
  })
  
  # Layers: map ----
  output$map <- reactive({ input$var })
  
  # Layers: histogram ----
  output$histogram <- renderPlot({
    library(ggplot2)
    
    # # DEBUG
    # config.R = '/usr/local/ohi/src/toolbox/scenarios/global_2012_nature/conf/config.R'
    # config.check(config.R)
    # layers_data = read.csv(layers_data.csv, na.strings='')    
    # input_var = 'alien_species'
    
    # get input data
    d = layer_data()
    input_var = input$var
    bin.width = diff(range(d[,'value_num']))/30
    
    # Histogram overlaid with kernel density curve
    p = ggplot(d, aes(x=value_num))
    p = p + geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                           binwidth=bin.width,
                           colour="black", fill="white") +
      geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot
    print(p)
  })  
  
  # Layers: summary ----
  output$summary <- renderPrint({
    summary(layer_data()[,'value_num'], digits=3)
  })
  
  # Layers: table ----
  output$table <- renderTable({
    flds = subset(layers_navigation, layer==input$var)[, c('id_num','id_chr','category','year','value_num','value_chr')]
    flds = flds[, !is.na(flds), drop=F]
    ld = plyr::rename(layer_data()[,names(flds)], setNames(flds, names(flds)))
  }, include.rownames=F)

  # Goals: aster ----
  output$aster <- renderPlot(width=800, height=850, {
    
    # get rescaled weights from slider inputs
    wts = get_wts(input)
    goals.wts = names(wts)
    cols.wts = cols.goals.all[goals.wts]
    
    # get data from results, so far assuming first line of results/regions_goals.csv
    # TODO: add dropdowns for: 1) different regions, 2) different schemes (ie weights)
    x = subset(scores, dimension=='score' & region_id==0)
    scores.wts  = x$score[match(names(wts), as.character(x$goal))] # regions_goals[1, goals.wts]
    index.score = weighted.mean(scores.wts, wts)
    
    # plot aster
    aster(lengths=scores.wts,
          max.length=100,
          widths=wts,
          disk=0.4,
          main='Global',
          fill.col=cols.wts,
          center=round(index.score),
          labels=paste(goals.wts, round(scores.wts), sep='\n'),
          label.cex=1.0,
          label.offset=0.1,
          cex=2.2,
          cex.main=2.5)
    
  })
  
  # Calculate: sel_scenario_dir ----
  output$sel_scenario_dir <- renderUI({
    selectInput("dir_scenario", sprintf("Select scenario (from %s):",dir.scenarios), 
                values$dirs_scenarios, selected=basename(dir.scenario))
  })
   
  # Calculate: txt_conf_summary ----
  output$txt_conf_summary <- renderPrint({
    cat('Scenario:', input$dir_scenario,'\n')
    if (is.null(input$dir_scenario)){
      config.R = file.path(dir.scenario, 'conf','config.R')  
    } else {
      config.R = file.path(dir.scenarios, input$dir_scenario, 'conf','config.R')  
    }    
    #config.check(config.R)
    config.summary(config.R)
  })
   
   # Calculate: txt_calc_summary ----
   output$txt_calc_summary <- renderText({
     #cat('input$btn_conf_calc:',input$btn_conf_calc,'\n')
     
     if (input$btn_calc == 0 & length(input$dir_scenario)>0){
       return('')
     } else {
       # outiside if and inside isolat()?
       return(c('Scenario:',input$dir_scenario,'\n',
                'TODO: integrate with execution of sequence of functions and display calculated summary.\n'))     
     }
   })
  
  # Report: sel_compare ----
  output$sel_compare <- renderUI({
    selectInput("dir_compare", "Compare with scenario:", 
                c('[None]',setdiff(values$dirs_scenarios, basename(dir.scenario))), 
                selected='[None]')
  })
  
  
})