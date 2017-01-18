source("config.R")          # global configuration

# Icons
icons <- list(
    remove = icon("remove", lib = "glyphicon"),
    move = icon("move", lib = "glyphicon"),
    barcode = icon("barcode", lib = "glyphicon"),
    picture = icon("picture", lib="glyphicon"),
    bug = icon("bug")
)

# Utility functions

# wrap element inside a tooltip div
tooltip <- function(tooltip, ...) {
    tags$div(title=tooltip, ...) 
}

# show element only if advanced switch is turned on
advanced <- function(...) {
    conditionalPanel(condition = "input.advanced == true", ...)
}