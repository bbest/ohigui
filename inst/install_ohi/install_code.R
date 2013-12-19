if (!require(devtools)){
  install.packages('devtools')
  require(devtools)
} 

# for production version, use github.user = 'ohi-science'
# for development version, use github.user = 'bbest'
github.user = 'ohi-science'
install_github('rCharts', github.user)
install_github('ohicore', github.user)
install_github('ohigui' , github.user)

require(ohigui)
launchApp()