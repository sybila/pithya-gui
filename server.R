# in linux install: r-base-dev

source("config.R")          # global configuration
source("tooltips.R")        # texts

# Utility files
source("bio.R")             # .bio file parser and utilities
source("result.R")          # result .json parser

# Separate tab servers
source("editor/server.R")
source("explorer/server.R")
source("result/server.R")

shinyServer(function(input,output,session) {

    mySession <- list(shiny=session, pithya=list(
        # TODO remove sample file
        approximatedModel = reactiveValues(file = NULL, model = NULL, outdated = FALSE),
        #synthesisResult = reactiveValues(file = NULL, result = NULL, outdated = TRUE),
        synthesisResult = reactiveValues(
            file = NULL,#"example/repressilator_2D/model_dep.results.json", 
            result = parseResultFile("example/repressilator_2D/model_dep.results.json"), 
            outdated = FALSE
        ),
        sessionDir = tempdir(),
        examplesDir = "example//",
        nextId = createCounter(1)
    ))

    # Parse .bio model when approximation changes
    observeEvent(mySession$pithya$approximatedModel$file, {
        file <- mySession$pithya$approximatedModel$file
        if (!is.null(file)) {
            tryCatch({
                mySession$pithya$approximatedModel$model <- parseBioFile(file)
            }, error = function(e) {
                debug("[.bio parser] parsing error: ", e)
                showNotification("[INTERNAL ERROR] Model parsing failed")
                mySession$pithya$approximatedModel$file <- NULL
                mySession$pithya$approximatedModel$model <- NULL
            })        
        } else {
            mySession$pithya$approximatedModel$model <- NULL
        }
    })

    # Parse result .json files when synthesis finished
    observeEvent(mySession$pithya$synthesisResult$file, {
        file <- mySession$pithya$synthesisResult$file
        if (!is.null(file)) {
            tryCatch({
                mySession$pithya$synthesisResult$result <- parseResultFile(file)
            }, error = function(e) {
                debug("[result parser] parsing error: ", e)
                showNotification("[INTERNAL ERROR] Result parsing failed")
                mySession$pithya$synthesisResult$file <- NULL
                mySession$pithya$synthesisResult$result <- NULL
            })
        } else {
            mySession$pithya$synthesisResult$result <- NULL         
        }
    })

    editorServer(input, mySession, output)
    explorerServer(input, mySession, output)
    resultServer(input, mySession, output)

})