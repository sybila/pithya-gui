chooseCRANmirror(graphics=FALSE, ind=39)
source("config.R")
shiny::runApp('../biodivineGUI/', launch.browser=FALSE, host="127.0.0.1", port=8080)