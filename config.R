### This is a global configuration file. Include this in all other files!

require("parallel")
if (!require("methods")) {install.packages("methods", quiet = F); library(methods)}
if (!require("shiny")) {install.packages("shiny", quiet = F); library(shiny)}
if (!require("shinyBS")) {install.packages("shinyBS", quiet = F); library(shinyBS)}
if (!require("shinyjs")) {install.packages("shinyjs", quiet = F); library(shinyjs)}
if (!require("shinyAce")) {install.packages("shinyAce", quiet = F);
library(shinyAce)}
if (!require("pracma")) {install.packages("pracma", quiet = F); library(pracma)}
if (!require("stringr")) {install.packages("stringr", quiet = F); library(stringr)}
if (!require("data.table")) {install.packages("data.table", quiet = F);
library(data.table)}
if (!require("rjson")) {install.packages("rjson", quiet = F); library(rjson)}
if (!require("matrixcalc")) {install.packages("matrixcalc", quiet = F);
library(matrixcalc)}

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