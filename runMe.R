source("config.R")
chooseCRANmirror(graphics=FALSE, ind=39)
shiny::runApp('../biodivineGUI/', launch.browser=FALSE, host="127.0.0.1", port=8080)