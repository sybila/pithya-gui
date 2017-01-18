### This is a global configuration file. Include this in all other files!

# Load and install (if needed) all app dependencies using the packman manager.
if (!require("pacman")) install.packages("pacman", quiet = F)
pacman::p_load(methods, shiny, shinyBS, shinyAce, shinyjs, pracma, stringr, data.table, rjson, parallel)

source("global.R")

# disable scientific notation when printing numbers
options(scipen=999)	

# allow file uploads of up to 1GB
options(shiny.maxRequestSize=1000*1024^2)
