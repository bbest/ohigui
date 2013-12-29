# Install dependencies
if (!require(devtools)){
  install.packages('devtools')
  require(devtools)
} 
install_github('ramnathv/rCharts')

# For production version, use github.user = 'ohi-science'
# For development version, use github.user = 'bbest'
github.user = 'ohi-science'
install_github('ohicore', github.user)
install_github('ohigui' , github.user)

# Launch application
ohigui::launchApp()

# Go to Calculate tab, Write button 
# to create a shortcut on your filesystem
# for launching the app