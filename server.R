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
        TCanalysisResult = reactiveValues(file = NULL, result = NULL, outdated = FALSE),
        importedResult = reactiveValues(file = NULL, result = NULL, outdated = FALSE),
        currentResult = reactiveValues(file = NULL, result = NULL, loaded = NULL),
        selectedExample = reactiveValues(code = NULL, model_new = FALSE, prop_new = FALSE),
        sessionDir = mytempdir(),
        examplesDir = "example//",
        nextId = createCounter(1)
    ))
    
    # Response to change of selected example - which is a modal window with Cancel and Ok buttons
    observeEvent(c(input$select_example), {
      if(!is.null(input$select_example) && input$select_example != "none" && 
         (is.null(mySession$pithya$selectedExample$code) || input$select_example != mySession$pithya$selectedExample$code)) {
        showModal(modalDialog(title = "Change of the example",
                              "All changes done in model and properties editors as well as all plots will be lost.",
                              footer = tagList(
                                actionButton("example_change_canceled", "Cancel"),
                                actionButton("example_change_approved", "Ok")
                              )
        ))
      } else if(!is.null(input$select_example) && input$select_example == "none" && 
                !is.null(mySession$pithya$selectedExample$code) && input$select_example != mySession$pithya$selectedExample$code) {
        updateSelectInput(session, "select_example", choices = examples_list, selected = mySession$pithya$selectedExample$code)
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    # Response to click on Cancel button when selecting new example
    observeEvent(input$example_change_canceled, {
      removeModal()
      updateSelectInput(session, "select_example", choices = examples_list, selected = mySession$pithya$selectedExample$code)
    })
    
    # This cares for Ace editors reload after change of tab to Editor tab.
    observeEvent(input$dimensions, {
      if(input$dimensions == Editor_label)
      js$shinyAce_resize()
    })
    
    # Response to click on Ok button when selecting new example
    observeEvent(input$example_change_approved, {
      removeModal()
      mySession$pithya$selectedExample$code <- input$select_example
      mySession$pithya$selectedExample$model_new <- TRUE
      mySession$pithya$selectedExample$prop_new <- TRUE
      example <- examples_data[[mySession$pithya$selectedExample$code]]
      # model loader
      updateAceEditor(session, "model_input_area", value = paste(readLines(example$model), collapse = "\n"))
      # property loader
      if(nchar(example$prop) != 0)
        updateAceEditor(session, "prop_input_area", value = paste(readLines(example$prop), collapse = "\n"))
      else
        updateAceEditor(session, "prop_input_area", value = "")
      
      # discard any shown results
      mySession$pithya$currentResult$file <- NULL
      mySession$pithya$currentResult$result <- NULL
      mySession$pithya$currentResult$loaded <- NULL
      # discard any shown explorations
      mySession$pithya$approximatedModel$file <- NULL
      mySession$pithya$approximatedModel$model <- NULL
      mySession$pithya$approximatedModel$outdated <- FALSE
      # PS results loader
      if(nchar(example$ps_res) != 0) {
        tryCatch({
          mySession$pithya$synthesisResult$file <- example$ps_res
          mySession$pithya$synthesisResult$result <- parseResultFile(example$ps_res)
          mySession$pithya$synthesisResult$outdated <- FALSE
          debug("example PS results imported successfully")
        }, error = function(e) {
          showNotification(paste0("Invalid example PS result file to import: ", e))
        })
      } else {
        mySession$pithya$synthesisResult$file <- NULL
        mySession$pithya$synthesisResult$result <- NULL
        mySession$pithya$synthesisResult$outdated <- TRUE
      }
      # AA results loader
      if(nchar(example$aa_res) != 0) {
        tryCatch({
          mySession$pithya$TCanalysisResult$file <- example$aa_res
          mySession$pithya$TCanalysisResult$result <- parseResultFile(example$aa_res)
          mySession$pithya$TCanalysisResult$outdated <- FALSE
          debug("example AA results imported successfully")
        }, error = function(e) {
          showNotification(paste0("Invalid example AA result file to import: ", e))
        })
      } else {
        debug("no example AA results")
        mySession$pithya$TCanalysisResult$file <- NULL
        mySession$pithya$TCanalysisResult$result <- NULL
        mySession$pithya$TCanalysisResult$outdated <- TRUE
      }
    })
    # Reaction of selected example when upload/reload model file from outside
    # selection is discarded, as it no longer show selected model
    observeEvent(c(input$model_file, input$reset_model), {
      if (!is.null(input$model_file) && !is.null(input$reset_model)) {
        mySession$pithya$selectedExample$code <- "none"
        updateSelectInput(session, "select_example", choices = examples_list, selected = mySession$pithya$selectedExample$code)
      }
    })
    # Reaction of selected example when upload/reload property file from outside
    # selection is discarded, as it no longer show property of selected model
    observeEvent(c(input$prop_file, input$reset_prop), {
      if (!is.null(input$prop_file) && !is.null(input$reset_prop)) {
        mySession$pithya$selectedExample$code <- "none"
        updateSelectInput(session, "select_example", choices = examples_list, selected = mySession$pithya$selectedExample$code)
      }
    })
      

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
          showNotification("[INTERNAL ERROR] PS esult parsing failed")
          mySession$pithya$synthesisResult$file <- NULL
          mySession$pithya$synthesisResult$result <- NULL
        })
      } else {
        mySession$pithya$synthesisResult$result <- NULL         
      }
    })
    
    # Parse result .json files when attractor analysis finished
    observeEvent(mySession$pithya$TCanalysisResult$file, {
      file <- mySession$pithya$TCanalysisResult$file
      if (!is.null(file)) {
        tryCatch({
          mySession$pithya$TCanalysisResult$result <- parseResultFile(file)
          ### Working solution for re-loading of fileInput
          #mySession$shiny$sendCustomMessage("loader_reset","ps_file")
        }, error = function(e) {
          debug("[result parser] parsing error: ", e)
          showNotification("[INTERNAL ERROR] AA result parsing failed")
          mySession$pithya$TCanalysisResult$file <- NULL
          mySession$pithya$TCanalysisResult$result <- NULL
        })
      } else {
        mySession$pithya$TCanalysisResult$result <- NULL         
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