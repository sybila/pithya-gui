
# Load and install (if needed) all app dependencies using the packman manager.
if (!require("pacman")) install.packages("pacman", quiet = F)
pacman::p_load(methods, shiny, shinyBS, shinyAce, shinyjs, pracma, stringr, data.table, rjson, parallel)