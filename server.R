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
    
    ## Just for case Windows would need to define specific temporary directory
    # session$onSessionEnded(function() {unlink(paste0(mytempdir()),recursive = T)})

    mySession <- list(shiny=session, pithya=list(
        approximatedModel = reactiveValues(file = NULL, model = NULL, outdated = FALSE),
        synthesisResult = reactiveValues(file = NULL, result = NULL, outdated = FALSE),
        TSanalysisResult = reactiveValues(file = NULL, result = NULL, outdated = FALSE),
        sessionDir = mytempdir(),
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
          ### Working solution for re-loading of fileInput
          #mySession$shiny$sendCustomMessage("loader_reset","ps_file")
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
    
    # Parse result .json files when TS analysis finished
    observeEvent(mySession$pithya$TSanalysisResult$file, {
      file <- mySession$pithya$TSanalysisResult$file
      if (!is.null(file)) {
        tryCatch({
          mySession$pithya$TSanalysisResult$result <- parseResultFile(file)
          ### Working solution for re-loading of fileInput
          #mySession$shiny$sendCustomMessage("loader_reset","ps_file")
        }, error = function(e) {
          debug("[result parser] parsing error: ", e)
          showNotification("[INTERNAL ERROR] Result parsing failed")
          mySession$pithya$TSanalysisResult$file <- NULL
          mySession$pithya$TSanalysisResult$result <- NULL
        })
      } else {
        mySession$pithya$TSanalysisResult$result <- NULL         
      }
    })
    
    # output$btn_test_export <- downloadHandler(
    #     filename = "report.html",
    #     content = function(file) {
    #         tempReport <- file.path(tempdir(), "report.Rmd")
    #         file.create(tempReport)
    #         
    #         params <- list(all = mySession)
    #         
    #         rmarkdown::render(tempReport, output_file = file,
    #                           params = params,
    #                           envir = new.env(parent = globalenv())
    #         )
    #     }
    # )
    
    # observeEvent(input$btn_test_import, {
    #     tempImage <- file.path(tempdir(), "image.Rmd")
    #     loading(tempImage)
    # })
    # observeEvent(input$btn_test_export, {
    #     tempImage <- file.path(tempdir(), "image.Rmd")
    #     saving(tempImage)
    #     # neco <- toJSON(reactiveValuesToList(mySession$shiny$input))
    #     # inp <- fromJSON(neco)
    #     # mySession$shiny$sendInputMessage("arrows_number",35)
    #     # updateSliderInput(mySession$shiny,"arrows_number",value = 35)
    # })

    editorServer(input, mySession, output)
    explorerServer(input, mySession, output)
    resultServer(input, mySession, output)

})