### This is a global configuration file. Include this in all other files!

require("parallel")
if (!require("methods")) {install.packages("methods", quiet = F); library(methods)}
if (!require("shiny")) {install.packages("shiny", quiet = F); library(shiny)}
if (!require("shinyBS")) {install.packages("shinyBS", quiet = F); library(shinyBS)}
if (!require("shinyjs")) {install.packages("shinyjs", quiet = F); library(shinyjs)}
if (!require("shinyAce")) {install.packages("shinyAce", quiet = F); library(shinyAce)}
if (!require("pracma")) {install.packages("pracma", quiet = F); library(pracma)}
if (!require("stringr")) {install.packages("stringr", quiet = F); library(stringr)}
if (!require("data.table")) {install.packages("data.table", quiet = F); library(data.table)}
if (!require("rjson")) {install.packages("rjson", quiet = F); library(rjson)}
if (!require("matrixcalc")) {install.packages("matrixcalc", quiet = F); library(matrixcalc)}
#if (!require("V8")) {install.packages("V8", quiet = F); library(V8)}

source("global.R")
source("util.R")

# disable scientific notation when printing numbers
options(scipen=999)	

# allow file uploads of up to 1GB
options(shiny.maxRequestSize=1000*1024^2)

# Examples data
defaultModel = "//repressilator_2D//model_indep.bio"
defaultProperty = "//repressilator_2D//properties.ctl"

# Examples list
examples_list <- list(
  "Select example"   = "none",
  "Repressilator 2D" = "rep2D",
  "Repressilator 3D" = "rep3D",
  "G1/S cell cycle transition" = "tcbb"
)
examples_data <- list(
  "rep2D" = list(code  = "rep2D",
                 model = "example//repressilator_2D//model_indep.bio",
                 prop  = "example//repressilator_2D//properties.ctl",
                 ps_res= "example//repressilator_2D//model_indep.PS_results.json",
                 aa_res= "example//repressilator_2D//model_indep.AA_results.json"),
  "rep3D" = list(code  = "rep3D",
                 model = "example//repressilator_3D//model_indep.bio",
                 prop  = "example//repressilator_3D//property.ctl",
                 ps_res= "",
                 aa_res= ""),
  "tcbb"  = list(code  = "tcbb",
                 model = "example//tcbb_model//model_indep.2P.bio",
                 prop  = "example//tcbb_model//properties.ctl",
                 ps_res= "example//tcbb_model//model_indep.2P.properties.results.json",
                 aa_res= "")
)

corePath = "..//biodivine-ctl//build//install//biodivine-ctl//bin//"

shinyAppPort = "9999"

emptyVarName = "(none)"

# TODO change this on release version
debug <- function(...) { print(paste0(...)) }

# UI settings
scale_granularity <- 0.01