chooseCRANmirror(graphics=FALSE, ind=19)
source("config.R")
shiny::runApp('./', launch.browser=FALSE, host="127.0.0.1", port=8090)
