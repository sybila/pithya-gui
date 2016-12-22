chooseCRANmirror(graphics=FALSE, ind=39)
library(methods)
if(!require(shiny,quietly = T)) {install.packages("shiny", dependencies=T,quiet = T); library(shiny,quietly = T)}
if(!require(shinyBS,quietly = T)) {install.packages("shinyBS", dependencies=T,quiet = T); library(shinyBS,quietly = T)}
if(!require(shinyjs,quietly = T)) {install.packages("shinyjs", dependencies=T,quiet = T); library(shinyjs,quietly = T)}
if(!require(pracma,quietly = T)) {install.packages("pracma", dependencies=T,quiet = T); library(pracma,quietly = T)}
if(!require(stringr,quietly = T)) {install.packages("stringr", dependencies=T,quiet = T); library(stringr,quietly = T)}
if(!require(data.table,quietly = T)) {install.packages("data.table", dependencies=T,quiet = T); library(data.table,quietly = T)}
if(!require(rjson,quietly = T)) {install.packages("rjson", dependencies=T,quiet = T); library(rjson,quietly = T)}
#if(!require(shinythemes,quietly=T)) install.packages("shinythemes",quiet=T); library(shinythemes,quietly=T)
#if(!require(ggplot2,quietly = T)) install.packages("ggplot2",quiet = T); library(ggplot2,quietly = T)
shiny::runApp('../GUI/',launch.browser=TRUE,host="127.0.0.1",port=8080)