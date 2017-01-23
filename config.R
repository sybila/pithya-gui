### This is a global configuration file. Include this in all other files!

# Load and install (if needed) all app dependencies using the packman manager.
if (!require("pacman")) install.packages("pacman", quiet = F)
pacman::p_load(methods, shiny, shinyBS, shinyAce, shinyjs, pracma, stringr, data.table, rjson, parallel, matrixcalc)

source("global.R")
source("util.R")

# disable scientific notation when printing numbers
options(scipen=999)	

# allow file uploads of up to 1GB
options(shiny.maxRequestSize=1000*1024^2)

defaultModel = "//repressilator_2D//model_indep.with_comments.bio"
defaultProperty = "//repressilator_2D//properties.with_comments.ctl"

corePath = "..//biodivine-ctl//build//install//biodivine-ctl//bin//"

shinyAppPort = "9999"

emptyVarName = "(none)"

# TODO change this on release version
debug <- function(...) { print(paste0(...)) }

# UI settings
scale_granularity <- 0.01