# see global.R

rgn_names = rbind(rename(SelectLayersData(layers, layers=conf$config$layer_region_labels, narrow=T), c('id_num'='rgn_id', 'val_chr'='rgn_name'))[,c('rgn_id','rgn_name')],
                  data.frame(rgn_id=0, rgn_name='GLOBAL'))

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
  
  # Layers: get_var() ----
  get_var <- reactive({
    v = list()
    if (input$varType == 'Layer'){
      
      v$name = input$varLayer
      
      v$data = plyr::rename(ohicore::SelectLayersData(layers, layers=input$varLayer, narrow=T), c('id_num'='rgn_id'))
      
      v$description = 'coming soon'
      
      v$details = ''
      m = subset(layers$meta, layer==input$varLayer)      
      for (f in names(m)){
        v$details = paste0(v$details, sprintf('%s: %s\n', f, as.character(m[[f]])))
      }
      
      
    } else if (input$varType == 'Score') {
      
      v$name = input$varScore
      g = strsplit(v$name, ' - ')[[1]][1]
      m = strsplit(v$name, ' - ')[[1]][2]
      attr(v$name, 'goal') = g
      attr(v$name, 'dimension') = m

      v$data = plyr::rename(subset(scores, goal==g & dimension==m, c(region_id, score)), c('region_id'='rgn_id', 'score'='val_num'))
      
      v$description = paste(g,' : ',
                            ifelse(g=='Index',
                             'The overall Index represents the weighted average of all goal scores.',
                             as.character(subset(conf$goals, goal == g, description, drop=T))))
      v$details = ''      
    }
    v$summary = sprintf('%s\n\n  min: %s\n  mean: %s\n  max: %s\n\n', v$name, min(v$data$val_num, na.rm=T), mean(v$data$val_num, na.rm=T), max(v$data$val_num, na.rm=T))
    
    return(v)
  })

  # Data: info
  output$var_description = renderText({ get_var()$description })
  
  output$var_details = renderPrint({ v = get_var(); cat(v$summary, '\n\n', v$details) })
  
  # Data: Map ----
  output$map_container <- renderMap({
      plotMap(v=get_var())
    }, html_sub = c('"features": "#! regions !#",' = '"features": regions,'))
  
  
  # Layers: histogram ----
  output$histogram <- renderPlot({
    library(ggplot2)
    
    # get input data
    v = get_var()
    bin.width = diff(range(v$data[,'val_num'], na.rm=T))/30
    
    # Histogram overlaid with kernel density curve
    p = ggplot(na.omit(v$data), aes(x=val_num))
    p = p + geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                           binwidth=bin.width,
                           colour="black", fill="white") +
      geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot
    print(p)
  })  
  
#   # Layers: summary ----
#   output$summary <- renderPrint({
#     summary(get_var()$data[,'val_num'], digits=3)
#   })
  
  # Layers: table ----
  output$table <- renderDataTable({
    d = rename(get_var()$data, c('val_num'='value'))
    
    # HACK: assuming has rgn_id in layer
    d = merge(d, rgn_names, all.x=T)
    d = cbind(d[,c('rgn_id','rgn_name')], d[,!names(d) %in% c('layer','rgn_id','rgn_name'), drop=F])
    d
  })

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
  
  #output$dir_scenario_exists = file.exists(dir_scenario)
  
  # Calculate: write ----
  output$show_dir_scenario = renderText({cat(dir_scenario)})
  observe({
    input$btn_write
    output$dir_scenario_exists = renderText({file.exists(input$dir_scenario)})
    output$show_dir_scenario   = renderText({input$dir_scenario})
    if (input$btn_write == 0){
      state_btn_write <<- 0
      #output$dir_scenario_exists = as.character()
    } else if (input$btn_write != state_btn_write){
      #output$dir_scenario_exists = as.character(file.exists(dir_scenario))
      isolate({
        state_btn_write <<- input$btn_write
        dir_scenario <<- input$dir_scenario
        ohicore::WriteScenario(
          scenario = list(
            conf = conf, 
            layers = layers, 
            scores = scores,
            shapes = dir_shapes,
            dir    = dir_scenario))
      })
      # = file.exists(dir_scenario)
    }
  })

#   # Calculate: sel_scenario_dir ----
#   output$sel_scenario_dir <- renderUI({
#     selectInput("dir_scenario", sprintf("Select scenario (from %s):", dir_scenarios), 
#                 values$dirs_scenario, selected=basename(dir_scenario))
#   })
   
#   # Calculate: txt_conf_summary ----
#   output$txt_conf_summary <- renderPrint({
#     cat('Scenario:', input$dir_scenario,'\n')
#     if (is.null(input$dir_scenario)){
#       config.R = file.path(dir_scenarios, 'conf','config.R')  
#     } else {
#       config.R = file.path(dir_scenarios, input$dir_scenario, 'conf','config.R')  
#     }    
#     #config.check(config.R)
#     #config.summary(config.R)
#   })
   
   # Calculate: txt_calc_summary ----
    observe({
      input$btn_calc      
      if (input$btn_calc == 0){
        state_btn_calc <<- 0
        output$txt_calc_summary <- renderText({''})
      } else if (input$btn_calc != state_btn_calc){
        isolate({
          state_btn_calc <<- input$btn_calc
          output$txt_calc_summary <- renderText({
            
            # TODO: hardcoded for now, in future source scenario.R
            CheckLayers(file.path(dir_scenario, 'layers.csv'), file.path(dir_scenario, 'layers'), c('rgn_id','cntry_key','saup_id'))
            
            source(file.path(dir_scenario, 'scenario.R'))
            layers <<- scenario$layers
            conf   <<- scenario$conf
            
            #wd = "/Users/bbest/myohi/scenario.Global2013.www2013"
            #conf   = Conf(file.path(wd, "conf"))
            #CheckLayers(file.path(wd, 'layers.csv'), file.path(wd, 'layers'), c('rgn_id','cntry_key','saup_id'))
            #layers = Layers(file.path(wd, "layers.csv"), file.path(wd, "layers"))
            #scores = CalculateAll(scenario$conf, scenario$layers, debug=F)

            scores <<- ohicore::CalculateAll(scenario$conf, scenario$layers, debug=F)
            write.csv(scores, file.path(dir_scenario, 'scores.csv'), na='', row.names=F)
            sprintf('Scores calculated and output to: %s', file.path(dir_scenario, 'scores.csv'))
          })
        })
        
      }
    })  

#    output$txt_calc_summary <- renderText({
#      #cat('input$btn_conf_calc:',input$btn_conf_calc,'\n')
#      
#      if (input$btn_calc == 0 & length(input$dir_scenario)>0){
#        return('')
#        btn_calc_counter <<- 0
#      } else {
#        btn_calc_counter = btn_calc_counter + 1
#        # outiside if and inside isolat()?
#        return(c('Scenario:',input$dir_scenario,'\n',
#                 'btn_calc_counter:',as.character(btn_calc_counter),
#                 'TODO: integrate with execution of sequence of functions and display calculated summary.\n'))     
#      }
#    })
  
  # Report: sel_compare ----
  output$sel_compare <- renderUI({
    selectInput("dir_compare", "Compare with scenario:", 
                c('[None]',setdiff(values$dirs_scenario, basename(dir_scenarios))), 
                selected='[None]')
  })
  
  
})