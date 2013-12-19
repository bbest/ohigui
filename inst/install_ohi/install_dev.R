if (!require(devtools)){
  install.packages('devtools')
  require(devtools)
} 

github.user = 'bbest'
install_github('rCharts', github.user)
install_github('ohicore', github.user)
install_github('ohigui' , github.user)

require(ohigui)
launchApp()