if (!require(devtools)) install.packages('devtools')
require(devtools)

install_github('bbest/rCharts')
install_github('bbest/ohicore')
install_github('bbest/ohigui')

require(ohigui)
launchApp()