# see global.R

# get goals for aster, all and specific to weights
goals.all = conf$goals # read.csv(goals.csv)
goals.all = goals.all[order(goals.all$order), 'goal']

# get colors for aster, based on 10 colors, but extended to all goals. subselect for goals.wts
cols.goals.all = colorRampPalette(RColorBrewer::brewer.pal(10, 'Spectral'), space='Lab')(length(goals.all))
names(cols.goals.all) = goals.all

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
dir_scenarios = dirname(dir_scenario)
values$dirs_scenario <- grep('^scenario\\.', list.dirs(path=dir_scenarios, recursive=F), value=T)

# shinyServer ----
# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output, session) {

  # Observe: values$dirs_scenarios ----
  # monitor filesystem every 5 seconds for folders in dir.conf
  observe({
    invalidateLater(5 * 1000, session)  # 5 seconds, in milliseconds
    values$dirs_scenario <- grep('^[^\\.]', basename(list.dirs(path=dir_scenarios, recursive=F)), value=T)
  })
  
  # Layers: get_var_data() ----
  get_var_data <- reactive({
    plyr::rename(ohicore::SelectLayersData(layers, layers=input$var, narrow=T), c('id_num'='rgn_id'))
  })
  
  # Layers: map ----
  output$map_container <- renderMap({
    plotMap(var=input$var, var_data=get_var_data())
    }, html_sub = c('"features": "#! regions !#",' = '"features": regions,'))
  
  # Layers: histogram ----
  output$histogram <- renderPlot({
    library(ggplot2)
    
    # get input data
    d = get_var_data()
    input_var = input$var
    bin.width = diff(range(d[,'val_num']))/30
    
    # Histogram overlaid with kernel density curve
    p = ggplot(d, aes(x=val_num))
    p = p + geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                           binwidth=bin.width,
                           colour="black", fill="white") +
      geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot
    print(p)
  })  
  
  # Layers: summary ----
  output$summary <- renderPrint({
    summary(get_var_data()[,'val_num'], digits=3)
  })
  
  # Layers: table ----
  output$table <- renderTable({
    #flds = subset(layers_navigation, layer==input$var)[, c('id_num','id_chr','category','year','value_num','value_chr')]
    d = layers$data[[input$var]]
    d = d[,names(d)!='layer']
    d
    #flds = flds[, !is.na(flds), drop=F]
    #ld = plyr::rename(get_var_data()[,names(flds)], setNames(flds, names(flds)))
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
    selectInput("dir_scenario", sprintf("Select scenario (from %s):", dir_scenarios), 
                values$dirs_scenario, selected=basename(dir_scenario))
  })
   
  # Calculate: txt_conf_summary ----
  output$txt_conf_summary <- renderPrint({
    cat('Scenario:', input$dir_scenario,'\n')
    if (is.null(input$dir_scenario)){
      config.R = file.path(dir_scenarios, 'conf','config.R')  
    } else {
      config.R = file.path(dir_scenarios, input$dir_scenario, 'conf','config.R')  
    }    
    #config.check(config.R)
    #config.summary(config.R)
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
                c('[None]',setdiff(values$dirs_scenario, basename(dir_scenarios))), 
                selected='[None]')
  })
  
  
})