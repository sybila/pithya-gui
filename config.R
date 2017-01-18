### This is a global configuration file. Include this in all other files!

source("dependencies.R")    # load dependencies
source("global.R")

# disable scientific notation when printing numbers
options(scipen=999)	

# allow file uploads of up to 1GB
options(shiny.maxRequestSize=1000*1024^2)
